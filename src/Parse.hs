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
        ParseOk m -> return $ collectDeclarations m
        ParseFailed srcLoc message -> do
          putStrLn $ unlines [ prettyPrint srcLoc
                             , message
                             ]
          exitFailure

collectDeclarations moduleDesc =
  [ x | x <- universeBi moduleDesc, isDeclaration x]
  where
    isDeclaration (DataDecl _ _ _ _ _ _ _) = True
    isDeclaration (TypeDecl _ _ _ _) = True
    isDeclaration _ = False

