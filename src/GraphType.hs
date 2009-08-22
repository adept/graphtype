-- | TODO: top-level docs
module Main where

import Parse (parseFiles)
import OptionParser

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Text.Dot
import Data.List
import Data.Maybe
import Control.Monad

main = do
  (Mode depth output, root, files) <- getOpts
  types <- parseFiles files
  let graph = buildGraph types depth root
  writeFile output graph

type DeclName = String
type Graph = String

-- | Builds dependency graph starting with datatype declaration `root'.
-- Recursively expands all user-defined `types' referenced from `root', up to `depth'
-- TODO: use depth
buildGraph :: [Decl]    -- ^ All declarations found in source files
           -> Maybe Int -- ^ Recursion depth
           -> DeclName  -- ^ Start from this declaration
           -> Graph     -- ^ Graph definition in DOT syntax
buildGraph types depth root =
  showDot $ do
    -- Allow links that end on cluster boundaries
    attribute("compound", "true")
    -- Try harder to route edges around clusters
    attribute("remincross", "true")
    -- Try harder to route edges around clusters
    -- attribute("rankdir", "LR")
    -- Add topmost declaration and proceed with links going from it
    (danglingLinks,clusters) <- addDecl root [] types
    addLinks danglingLinks clusters types

-- Each declaration is transformed to the cluster on the output graph.
-- Elements of the cluster are graph nodes, one for each constructor in the declaration.
-- Those nodes have shape "record".
-- Since there are "records" with "fields" in dot syntax, and "records" with "fields" in Haskell syntax,
-- there bound to be some misunderstanding. Unless said otherwise, from now on records and fields
-- are those from dot syntax.

type Links = [DanglingLink]
-- | Information about dangling link that should be added to graph.
-- We would want to create links while in the middle of constructing a complex record node, which is not possible.
-- Thus, all outgoing links are scheduled in the list of dangling links and resolved in the breadth-first manner.
-- Initially, each dangling link is specified via target declaration name. When this declaration is added to graph,
-- it is possible to find out cluster id and (some) node id corresponding to that declaration and actually build a link.
--
-- We have to have some target node id because dot does not allow links to clusters themselves. We choose first node
-- within a cluster as our target.
data DanglingLink =
  DL { linkTarget::DeclName -- ^ destination declaration to link to
     , createLink::(ClusterId -> NodeId -> Dot ()) -- ^ function used to create link once proper destination cluster is determined
     }
type ClusterId = NodeId


type Port = String
-- | Helper constructor for dangling links.
-- Notice the 'head" attribute - without it the edge would not stop at the cluster boundary
mkDL :: DeclName -> Port -> NodeId -> DanglingLink
mkDL target sourcePort sourceNode =
  DL target (\cluster targetNode -> edge' sourceNode (Just sourcePort) targetNode Nothing [("lhead",show cluster)])

-- | Information about clusters already added to the graph:
-- (Declaration name, (cluster id for this declaration, Id of the first node in this cluster))
type Clusters = [(DeclName, (NodeId, NodeId))]

-- | Add dangling `links' to the graph, adding new clusters as needed
addLinks :: Links -- ^ Links to be added to the graph
         -> Clusters -- ^ Clusters already present in graph
         -> [Decl]  -- ^ All declarations parsed from source files
         -> Dot ()
addLinks [] clusters types = return ()
addLinks links@((DL target mkLink):rest) clusters types =
  case lookup target clusters of
    Just (destCluster, destNode) -> do
      -- Target cluster is already in the graph. Just add link to it and proceed
      mkLink destCluster destNode
      addLinks rest clusters types
    Nothing -> do
       -- Target cluster is absent. Add it and re-try linking.
      (danglingLinks, clusters') <- addDecl target clusters types
      addLinks (links++danglingLinks) clusters' types


-- | Each "record" node in the dot file could be decomposed into several fields.
-- Each field represents a Haskell record field, Haskell datatype component or Haskell type declaration
data Field = F { fieldName::Maybe Name -- ^ name of the Haskell record field, empty otherwise
               , fieldPort::Maybe Port -- ^ dot-specific ID of the field, for anchoring originating links. Empty when field has some unknown type
               , typeName::DeclName    -- ^ user-friendly name of the Haskell type
               , fieldLink::[Maybe (NodeId -> DanglingLink)] -- ^ As soon as DOT record is finished, its node id is substituted here to
                                                             -- obtain a dangling link to target declaration. Empty when field has some unknown type
               }

addDecl :: DeclName -- Name of the declaration we have to add
        -> Clusters -- Declarations already added to graph
        -> [Decl]   -- All known declarations
        -> Dot (Links,Clusters) -- ( Links, dangling from this declaration, Updated list of clusters )
addDecl root clusters decls = do
  ( clusterId, (destNode, danglingLinks) ) <- cluster $ do
    let (Just d) = findDecl root decls
    let declType = getDeclType d

    attribute ( "label", unwords [ declType, getName d ] )

    if declType == "type"
       then do
         -- For simple type declaration, create a single record depicting type.
         -- Collect and outgoing links.
         let (TypeDecl _ _ _ t) = d
         let fs = type2fields 0 t
         fields2horizRecord fs
       else do
         -- For data/newtype declaration, create a single record for each constructor.
         -- Collect and outgoing links.
         (constructorNodes, links) <- liftM unzip $ sequence $ umap addConstructor d
         return (head constructorNodes, concat links)
  return ( danglingLinks, (root, (clusterId, destNode)):clusters )
  where
    -- TODO: add InfixConDecl
    -- FIXME: remove duplication
    fields2horizRecord fs = mkRecord ( mkLabel fs ) fs
    fields2vertRecord header fs = mkRecord ( header <//> mkLabel fs ) fs

    mkRecord label fs = do
      rId <- record label
      let links = [ mkLink rId | (F _ _ _ mkLinks) <- fs, Just mkLink <- mkLinks ]
      return (rId, links)

    mkLabel fs = wrap $ toLabel $ map mkComponent fs
      where
        wrap = case fs of
                [_] -> id
                _   -> block
        mkComponent field | fieldName field == Nothing = (fromMaybe "" $ fieldPort field) ++ typeName field
                          | otherwise = let fn = fromName $ fromJust $ fieldName field
                                            t = typeName field
                                            text = case head t of
                                                     '{' -> block $ fn ++ " :: | " ++ block t
                                                     _   -> fn ++ " :: " ++ t
                                            p = fromMaybe "" $ fieldPort field
                                            in p ++ text

    addConstructor (ConDecl nm types) = do
      let fs = concat $ zipWith type2fields [0..] types
      fields2vertRecord ("ConDecl " ++ fromName nm) fs
    addConstructor (RecDecl nm types) = do
      let fs = zipWith rectype2field [0..] types
      fields2vertRecord ("RecDecl " ++ fromName nm) fs

    rectype2field x (nms,t) =
      let fs = type2fields x t
          fName = concat $ intersperse ", " $ map prettyPrint nms
          fLabel = mkLabel fs -- toLabel $ map typeName fs
          in case fs of
               [f] -> f { fieldName=(Just $ name fName) }
               _   -> F { fieldName=(Just $ name fName)
                        , fieldPort=Nothing
                        , typeName=fLabel
                        , fieldLink = (concatMap fieldLink fs)
                        }

    type2fields x t = map (typeName2field x) referencedTypes
      where referencedTypes = [ prettyPrint qname | TyCon qname <- universeBi t ] -- TODO: process TyInfix as well

    typeName2field x nm =
      case findDecl nm decls of
        Just d  -> F Nothing (Just port) nm [Just (mkDL nm port)]
        Nothing -> F Nothing Nothing     nm [Nothing]
      where
        port = concat [ "<", nm, show x, "> " ]

    toLabel [] = ""
    toLabel fields = foldr1 (<||>) fields

umap f l = [ f x | x <- universeBi l ]

-- Graph nodes construction helpers
box label = node $ [ ("shape","box"),("label",label) ]
record label = node $ [ ("shape","record"),("label",block label) ]

-- Record label construnction helpers
infix <||>, <//>
a <||> b = concat [a, " | ", b]
a <//> b = concat [ a, " | { ", b, " }"]
block x = "{ " ++ x ++ " }"


-- Haskell AST manipulation helpers

findDecl nm decls = find ((==nm).getName) decls

getName (DataDecl _ _ _ nm _ _ _) = fromName nm
getName (TypeDecl _ nm _ _) = fromName nm

getDeclType (DataDecl _ DataType _ _ _ _ _) = "data"
getDeclType (DataDecl _ NewType _ _ _ _ _)  = "newtype"
getDeclType (TypeDecl _ _ _ _)              = "type"

fromName (Ident x) = x
fromName (Symbol x) = x

names ns = concat $ intersperse ", " $ map fromName ns