module Parse (parseFiles) where

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Control.Monad (liftM)
import System.Exit (exitFailure)

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
