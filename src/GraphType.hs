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
  DL target (\cluster targetNode -> edge' sourceNode (Just sourcePort) targetNode Nothing [("lheadz",show cluster)])

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
         let fs = type2fields t
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
      let links = [ mkLink rId | (F _ _ mkLinks) <- fs, Just mkLink <- mkLinks ]
      return (rId, links)

    mkLabel fs = block $ toLabel $ map mkComponent fs
      where mkComponent field | fieldName field == Nothing = typeName field
                              | otherwise = let (Just fn ) = fieldName field 
                                                t = typeName field
                                                in block $ fromName fn <||> t

    addConstructor (ConDecl nm types) = do
      let fs = concatMap type2fields types
      fields2vertRecord ("ConDecl " ++ fromName nm) fs
    addConstructor (RecDecl nm types) = do
      let fs = map rectype2field types
      fields2vertRecord ("RecDecl " ++ fromName nm) fs

    -- TODO: add names
    rectype2field (nms,t) = F (Just $ name fName) fLabel (concatMap createFieldLink fs)
      where fs = type2fields t
            fName = concat $ intersperse ", " $ map prettyPrint nms
            fLabel = toLabel $ map typeName fs

    type2fields t = map typeName2field referencedTypes
      where referencedTypes = [ prettyPrint qname | TyCon qname <- universeBi t ] -- TODO: process TyInfix as well
      
    typeName2field nm =
      case findDecl nm decls of
        Just d  -> F Nothing ( unwords [port, nm] ) [Just (mkDL nm port)]
        Nothing -> F Nothing nm [Nothing]
      where
        port = concat [ "<",nm,">" ]
                    
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