module Main where

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Text.Dot
import Control.Monad
import System.Exit

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
  putStrLn types
  let graph = buildGraph types Inf "Organization"
  writeFile "output.dot" graph

parseFiles = liftM concat . mapM parseFile'
  where
    parseFile' fname = do
      res <- parseFile fname
      case res of
        ParseOk m -> return $ unlines $ showModule m
        ParseFailed srcLoc message -> do
          putStrLn $ unlines [ prettyPrint srcLoc
                             , message
                             ]
          exitFailure

showModule moduleDesc =
  [ show (getName x) | x <- universeBi moduleDesc, isDeclaration x]

isDeclaration (DataDecl _ _ _ nm _ _ _) = True
isDeclaration (TypeDecl _ _ _ _) = True
isDeclaration _ = False

getName (DataDecl _ _ _ nm _ _ _) = nm
getName (TypeDecl _ nm _ _) = nm

buildGraph = undefined