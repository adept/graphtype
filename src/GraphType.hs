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
    -- Add topmost declaration and proceed with links going from it
    (danglingLinks,clusters) <- addDecl root [] types
    addLinks danglingLinks clusters types

type DeclName = String
type Port = String

-- | Information about dangling link that should be added to graph:
-- (Source node, Port of source node, Name of the data declaration to link to)
type Links = [(NodeId,Port,DeclName)]

-- | Information about clusters already added to the graph:
-- (Data declaration name, (cluster for this declaration, first node in this cluster))
-- We need info about first node because it is impossible to specify edge not ending on a node.
type Clusters = [(DeclName, (NodeId, NodeId))]

-- | Add `links' between clusters on graph, adding new clusters as needed
addLinks :: Links -> Clusters -> [Decl] -> Dot ()
addLinks [] clusters types = return ()
addLinks links@((node,port,decl):rest) clusters types = 
  case lookup decl clusters of
    Just (destCluster, destNode) -> do
      -- Target cluster is already in the graph. Just add link to it
      edge' node (Just port) destNode Nothing [("lhead",show destCluster)] 
      -- Destination port is set to nothing because we really just want to get to the 
      -- edge of the destination cluster and that's it
      addLinks rest clusters types
    Nothing -> do
       -- Cluster for type 'decl' is absent. Add it and proceed with linking.
      (danglingLinks, clusters') <- addDecl decl clusters types
      addLinks (links++danglingLinks) clusters' types



addDecl :: DeclName -- Name of the declaration we have to add
        -> Clusters -- Declarations already added to graph
        -> [Decl]   -- All known declarations
        -> Dot (Links,Clusters) -- ( Links, dangling from this declaration, Updated list of clusters )
addDecl root clusters decls = do
  ( cluster_id, (dest_node, dangling_links) ) <- cluster $ do
    let (Just t) = findDecl root decls

    attribute ("label", getName t)

    (nodes, links) <- liftM unzip $ sequence $ umap addConstructor t
    return (head' nodes, concat links)
  return ( dangling_links, (root, (cluster_id, dest_node)):clusters ) 
  where
    -- FIXME: remove after specifying all cases in addConstructor
    head' [] = userNodeId (-1)
    head' ns = head ns
    -- FIXME: remove duplication
    addConstructor (ConDecl nm types) = do
      let recordFields = umap typeToField types
      nodeId <- record $ block $ ("ConDecl " ++ fromName nm) <//> ( block $ toLabel recordFields )
      return (nodeId, map (\(p,t) -> (nodeId,p,t)) $ catMaybes $ map snd recordFields)
    addConstructor (RecDecl nm types) = do
      let recordFields = umap typeToField $ map snd types
      nodeId <- record $ block $ ("RecDecl " ++ fromName nm) <//> ( block $ toLabel recordFields ) --block (foldr1 (<||>) $ map pp types)
      return (nodeId, map (\(p,t) -> (nodeId,p,t)) $ catMaybes $ map snd recordFields)

    typeToField (TyCon qname) = 
      case findDecl label decls of
                 Just t  -> ( port ++ " " ++ label, Just (port, label) ) -- FIXME
                 Nothing -> ( label, Nothing )
      where
        label = prettyPrint qname
        port = "<"++label++">"
    typeToField _ = ( "bogus" , Nothing )
                    
    toLabel [] = ""
    toLabel fields = foldr1 (<||>) $ map fst fields

umap f l = [ f x | x <- universeBi l ]
  
-- Graph nodes construction helpers
box label = node $ [ ("shape","box"),("label",label) ]
record label = node $ [ ("shape","record"),("label",label) ]

-- Record label construnction helpers
infix <||>, <//>
a <||> b = concat [a, " | ", b]
a <//> b = concat [ a, " | { ", b, " }"]
block x = "{ " ++ x ++ " }"


-- Haskell AST manipulation helpers

findDecl nm decls = find ((==nm).getName) decls

getName (DataDecl _ _ _ nm _ _ _) = fromName nm
getName (TypeDecl _ nm _ _) = fromName nm

fromName (Ident x) = x
fromName (Symbol x) = x

names ns = concat $ intersperse ", " $ map fromName ns