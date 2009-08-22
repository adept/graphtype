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

buildGraph types depth root = 
  showDot $ do
    attribute("compound", "true")
    (danglingLinks,clusters) <- addDataDecl root [] types
    addLinks danglingLinks clusters types

addLinks :: Links -> Clusters -> [Decl] -> Dot ()
addLinks [] clusters types = return ()
addLinks links@((node,port,decl):rest) clusters types = 
  case lookup decl clusters of
    -- We already have drawn this cluster. Just add link to it
    Just (destCluster, destNode) -> do
      edge' node (Just port) destNode Nothing [("lhead",show destCluster)] 
      -- destination port is set to nothing because we really just want to get to the 
      -- edge of the destination cluster and that's it
      addLinks rest clusters types
    -- Cluster for type 'decl' is absent. Add it.
    Nothing -> do
      (danglingLinks, clusters') <- addDataDecl decl clusters types
      addLinks (links++danglingLinks) clusters' types

type Clusters = [(String, (NodeId, NodeId))] -- Declaration name -> (cluster, node in cluster)
type Links = [(NodeId,String,String)]
addDataDecl :: String -> Clusters -> [Decl] -> Dot (Links,Clusters)  
addDataDecl root clusters types = do
  ( cluster_id, (dest_node, dangling_links) ) <- cluster $ do
    let (Just t) = findType root types

    attribute ("label", getName t)

    (nodes, links) <- liftM unzip $ sequence $ umap addConstructor t
    return (head' nodes, concat links)
  return ( dangling_links, (root, (cluster_id, dest_node)):clusters ) 
  where
    head' [] = userNodeId (-1)
    head' ns = head ns
    addConstructor (ConDecl nm types) = do
      let recordFields = umap typeToField types
      nodeId <- record $ block $ ("ConDecl " ++ fromName nm) <//> ( block $ toLabel recordFields )
      return (nodeId, map (\(p,t) -> (nodeId,p,t)) $ catMaybes $ map snd recordFields)
    addConstructor (RecDecl nm types) = do
      let recordFields = umap typeToField $ map snd types
      nodeId <- record $ block $ ("RecDecl " ++ fromName nm) <//> ( block $ toLabel recordFields ) --block (foldr1 (<||>) $ map pp types)
      return (nodeId, map (\(p,t) -> (nodeId,p,t)) $ catMaybes $ map snd recordFields)

    typeToField (TyCon qname) = 
      case findType label types of
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

findType nm types = find ((==nm).getName) types

getName (DataDecl _ _ _ nm _ _ _) = fromName nm
getName (TypeDecl _ nm _ _) = fromName nm

fromName (Ident x) = x
fromName (Symbol x) = x

names ns = concat $ intersperse ", " $ map fromName ns