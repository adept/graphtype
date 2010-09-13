-- | Produce dependency diagram from the set of *.hs files
--
-- Diagram will include specified top-level declaration and all user-defined types referencd from there (recursively).
--
-- User can choose to omit types and newtypes that do not contain anything other than library types - this could be
-- useful to unclutter really large diagrams
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
  (Mode output trim exts, root, files) <- getOpts
  types <- parseFiles exts files
  let trimmed = if trim 
                then doTrim types
                else types
  let graph = buildGraph trimmed root
  writeFile output graph

-- | Trim declarations, removing those types and newtypes that do not have references to other user-defined types
doTrim :: [Decl] -> [Decl]
doTrim types = if types' == types then types
                                  else doTrim types'
  where
    types' = types \\ (filter boring candidates)
    candidates = [ d | d <- types
                     , getDeclType d `elem` ["type", "newtype"] ]
    boring d = null $ catMaybes $ [ findDecl (prettyPrint qname) types | TyCon qname <- universeBi d ]

type DeclName = String
type Graph = String

-- | Builds dependency graph starting with datatype declaration `root'.
-- Recursively expands all user-defined `types' referenced from `root', up to `depth'
buildGraph :: [Decl]    -- ^ All declarations found in source files
           -> DeclName  -- ^ Start from this declaration
           -> Graph     -- ^ Graph definition in DOT syntax
buildGraph types root =
  showDot $ do
    -- Allow links that end on cluster boundaries
    attribute("compound", "true")
    -- Try harder to route edges around clusters
    attribute("remincross", "true")
    -- Try harder to route edges around clusters
    attribute("rankdir", "LR")
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
-- Notice the "lhead" attribute - without it the edge would not stop at the cluster boundary
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

-- | Add a single declaration to graph. As it was already said, each declaration is mapped to a DOT cluster
addDecl :: DeclName -- ^ Name of the declaration we are adding
        -> Clusters -- ^ Declarations already added to graph
        -> [Decl]   -- ^ All known declarations
        -> Dot (Links,Clusters) -- ^ ( Links dangling from this declaration, Updated list of clusters )
addDecl declName clusters decls = do
  ( clusterId, (firstNodeId, danglingLinks) ) <- mkCluster
  let clusters' = (declName, (clusterId, firstNodeId)):clusters
  return ( danglingLinks, clusters' )
  where
    -- Find declaration by name
    d = case findDecl declName decls of
          Just x -> x
          Nothing -> error $ "Could not find type " ++ declName ++ " in source files"

    -- Type, newtype or data
    declType = getDeclType d

    mkCluster = cluster $ do
      attribute ( "label", unwords [ declType, getName d ] )
      if declType == "type"
         then do
           -- For simple type declaration, convert all type components to DOT record fields
           let (TypeDecl _ _ _ t) = d
           let fs = type2fields 0 t
           -- Then, convert DOT fields to DOT record.
           -- Type components will be separated into different "cells" of the record, so that
           -- it would be possible to create outgoing links from any type component.
           mkRecord ( mkLabel fs ) fs
         else do
           -- For data/newtype declaration, create a single record for each constructor.
           (constructorNodes, links) <- liftM unzip $ sequence $ [ addConstructor x | x <- universeBi d ]
           -- Collect all outgoing links.
           return (head constructorNodes, concat links)

    mkRecord :: String -> [Field] -> Dot (NodeId, Links)
    mkRecord label fs = do
      -- Create DOT record node
      rId <- record label
      -- Instantiate all outgoing links
      let links = [ mkLink rId | (F _ _ _ links) <- fs, Just mkLink <- links ]
      return (rId, links)

    -- Produce label for record.
    -- Since label has both human-readable components and special markup that defines record shape,
    -- special care should be taken while combining information from separate fields:
    mkLabel :: [Field] -> String
    mkLabel fs = wrap $ toLabel $ map mkComponent fs
      where
        mkComponent field 
            -- If field is not named (body of type or component of data), then label is just "<port> type_name":
          | fieldName field == Nothing = mkPort field ++ typeName field
            -- If field is named, that we should take care to:
            -- 1)Preserve position of the topmost port
            -- 2)Enclose all complex declarations in {}
          | otherwise = let fn = fromName $ fromJust $ fieldName field -- Haskell field name
                            t = typeName field -- Haskell type
                            text = case head t of
                                     -- If the type is complex (Map Foo Bar), include complex description as DOT subfield
                                     '{' -> block $ fn ++ " :: | " ++ block t
                                     -- If the type is simple, just prepend field name
                                     _   -> fn ++ " :: " ++ t
                           -- Dont forget the port (if present)
                        in mkPort field ++ text

        mkPort f = fromMaybe "" $ fieldPort f

        toLabel [] = ""
        toLabel fields = foldr1 (<||>) fields

        -- When combining more that one field into label, enclose it in {}
        wrap = case fs of
                [_] -> id
                _   -> block

    -- TODO: add InfixConDecl
    addConstructor (ConDecl nm types) = do
      let fs = concat $ zipWith type2fields [0..] types
      fields2record ("constructor " ++ fromName nm) fs
    addConstructor (RecDecl nm types) = do
      let fs = zipWith rectype2field [0..] types
      fields2record ("record " ++ fromName nm) fs

    -- DOT records for Haskell data and Haskell record have "header" with the name of the constructor
    fields2record header fs = mkRecord ( header <//> mkLabel fs ) fs

    -- Collect all type constructors mentioned in type and convert them into DOT fields.
    -- `x' would be explained below
    type2fields x t = map (tyCon2field x) cons
      where cons = [ prettyPrint qname | TyCon qname <- universeBi t ] -- TODO: process TyInfix as well

    -- Convert type `typeName' into DOT field
    tyCon2field x typeName =
      case findDecl typeName decls of
        -- If this is a known (user-defined) type, allocate a port for link and add a dangling link to expanded type description
        Just d  -> F {fieldName=Nothing, fieldPort=Just port, typeName=typeName, fieldLink=[Just (mkDL typeName port)]}
        -- If this is a library type, just record its name
        Nothing -> F {fieldName=Nothing, fieldPort=Nothing, typeName=typeName, fieldLink=[Nothing]}
      where
        -- Allocate port. Port name is similar to type name, with sequential number X appended to distinguish between several
        -- components of the same type within a single declaration
        port = concat [ "<", typeName, show x, "> " ]

    -- Convert Haskell record field into DOT record field
    rectype2field x (nms,t) =
      let fs = type2fields x t -- first, conver all type components into fields
          fName = concat $ intersperse ", " $ map prettyPrint nms -- there might be more that one Haskell field name ("a,b::Int")
          fLabel = mkLabel fs  -- produce proper DOT description of the type
          in case fs of
               -- If it is a simple one-component type, just add a record name and be done with it
               [f] -> f { fieldName=(Just $ name fName) }
               -- If it is multi-component type, ...
               _   -> F { fieldName=(Just $ name fName) -- add record name, ...
                        , fieldPort=Nothing
                        , typeName=fLabel               -- save type description
                        , fieldLink = (concatMap fieldLink fs) -- collect all links from all type components
                        }


-----------------------------------
-- DOT record construnction helpers
-----------------------------------
record label = node $ [ ("shape","record"),("label",label) ]

infix <||>, <//>
-- | Append next subfield on the same level
a <||> b = concat [a, " | ", b]
-- | Start new sub-level
a <//> b = concat [ a, " | { ", b, " }"]
-- | Turn field into a block
block x = "{ " ++ x ++ " }"

-----------------------------------
-- Haskell AST manipulation helpers
-----------------------------------
-- | Find declaration by name
findDecl nm decls = find ((==nm).getName) decls

-- | Get declaration name
getName (DataDecl _ _ _ nm _ _ _) = fromName nm
getName (TypeDecl _ nm _ _) = fromName nm

-- | Get declaration .. ummm .. type. Pretty self-explanatory
getDeclType (DataDecl _ DataType _ _ _ _ _) = "data"
getDeclType (DataDecl _ NewType _ _ _ _ _)  = "newtype"
getDeclType (TypeDecl _ _ _ _)              = "type"

-- | Get name out of the Name datatype
fromName (Ident x) = x
fromName (Symbol x) = x