module Main where

import Parse (parseFiles)

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Text.Dot
import Data.List
import Data.Maybe
import Control.Monad

-- | Drawing depth
data Depth = Inf | Limit Int

-- TODO:
-- * Add CL options for:
-- ** Specifying input file(s)
-- ** Specifying drawing depth
-- ** Specifying root ADT
-- ** Output file name
main = do
  let files = [ "../example/Test01.hs", "../example/Test02.hs" ]
  types <- parseFiles files
  let graph = buildGraph types Inf "Organization"
  writeFile "output.dot" graph

-- | Builds dependency graph starting with datatype declaration `root'.
-- Recursively expands all user-defined `types' referenced from `root', up to `depth'
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

type DeclName = String
type Port = String

type Links = [DanglingLink]
-- | Information about dangling link that should be added to graph
data DanglingLink = DL { linkTarget::DeclName -- name of the declaration to link to
                       , createLink::(ClusterId -> NodeId -> Dot ()) -- function used to create link once declaration cluster is determined
                       }
type ClusterId = NodeId

mkDL :: DeclName -> Port -> NodeId -> DanglingLink
mkDL target sourcePort sourceNode = 
  DL target (\cluster targetNode -> edge' sourceNode (Just sourcePort) targetNode Nothing [("lhead",show cluster)])

-- | Information about clusters already added to the graph:
-- (Data declaration name, (cluster for this declaration, first node in this cluster))
-- We need info about first node because it is impossible to specify edge not ending on a node.
type Clusters = [(DeclName, (NodeId, NodeId))]

-- | Add `links' between clusters on graph, adding new clusters as needed
addLinks :: Links -> Clusters -> [Decl] -> Dot ()
addLinks [] clusters types = return ()
addLinks links@((DL target mkLink):rest) clusters types = 
  case lookup target clusters of
    Just (destCluster, destNode) -> do
      -- Target cluster is already in the graph. Just add link to it
      mkLink destCluster destNode
      -- Destination port is set to nothing because we really just want to get to the 
      -- edge of the destination cluster and that's it
      addLinks rest clusters types
    Nothing -> do
       -- Cluster for type 'decl' is absent. Add it and proceed with linking.
      (danglingLinks, clusters') <- addDecl target clusters types
      addLinks (links++danglingLinks) clusters' types

-- | Field of the record in dot file
data Field = F { fieldName::Maybe Name
               , fieldPort::Maybe Port
               , typeName::DeclName
               , createFieldLink::[Maybe (NodeId -> DanglingLink)] -- substitude record NodeId here and get a dangling link
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
               [f] -> F { fieldName=(Just $ name fName)
                        , fieldPort=fieldPort f
                        , typeName=typeName f
                        , createFieldLink = createFieldLink f
                        }
               _   -> F { fieldName=(Just $ name fName)
                        , fieldPort=Nothing
                        , typeName=fLabel
                        , createFieldLink = (concatMap createFieldLink fs)
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