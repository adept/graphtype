module Main where

import Parse (parseFiles)

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Text.Dot
import Data.List

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


buildGraph types depth root = showDot $ do
  explainType types root


explainType types root = cluster $ do
  -- 't' stands for 'type'
  let t = findType types root

  attribute ("label", getName t)

  sequence_ [ explainConstructor c | c <- universeBi t ]
  where
    collect f t = foldr1 (<||>) $ [ f c | c <- universeBi t ]

    explainConstructor (ConDecl nm types) = record $ block $ ("ConDecl " ++ fromName nm) <//> collect explainType types
    explainConstructor (RecDecl nm types) = record $ block $ ("RecDecl " ++ fromName nm) <//> block (foldr1 (<||>) $ map pp types)
      where pp (nm,t) = concatMap prettyPrint nm ++ "::" ++ prettyPrint t

    explainType (TyCon qname) = prettyPrint qname

-- Graph nodes construction helpers
box label = node $ [ ("shape","box"),("label",label) ]
record label = node $ [ ("shape","record"),("label",label) ]

-- Record label construnction helpers
infix <||>, <//>
a <||> b = concat [a, " | ", b]
a <//> b = concat [ a, " | { ", b, " }"]
block x = "{ " ++ x ++ " }"


-- Haskell AST manipulation helpers

-- TODO: Mb process Map, [] etc in a special way here
findType types nm =
  case find ((==nm).getName) types of
    Just t -> t
    -- TODO: it might be better to just die here
    Nothing -> error $ "Failed to fetch definition of " ++ nm

getName (DataDecl _ _ _ nm _ _ _) = fromName nm
getName (TypeDecl _ nm _ _) = fromName nm

fromName (Ident x) = x
fromName (Symbol x) = x