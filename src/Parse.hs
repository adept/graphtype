-- | Parses specified *.hs files and returns a list of all data declarations from them
module Parse (parseFiles) where

import Language.Haskell.Exts
import Data.Generics.PlateData (universeBi)
import Control.Monad (liftM)
import System.Exit (exitFailure)

parseFiles :: Maybe [Extension] -> [FilePath] -> IO [Decl]
parseFiles exts = liftM concat . mapM parseFile'
  where
    parser = case exts of
      Nothing -> parseFile
      Just es -> \f -> parseFileWithMode defaultParseMode{ parseFilename = f, extensions = es} f
      
    parseFile' fname = do
      res <- parser fname
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

