module OptionParser
  (
   Mode(..),
   getOpts,
  )
where

import System (getArgs)    
import System.Exit
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Control.Monad (when)

data Flag 
     = Inf | Limit Int | Output FilePath | Help
       deriving (Eq,Show)

options :: [OptDescr Flag]
options =
  [ Option ['d']        ["depth"]   (OptArg getDepth "N") "Follow links up to this depth (default: infinite)",
    Option ['o']        ["output"]  (OptArg getOutput "file") "Name of the output file (default: output.dot)",
    Option []           ["help"]    (NoArg Help)    "Show this help" ]

getDepth Nothing = Inf
getDepth (Just s) = Limit ( read s )

getOutput Nothing = Output "output.dot"
getOutput (Just s) = Output s

data Mode = Mode { depth :: Maybe Int, output :: String }
defaultMode = Mode Nothing "output.dot"

update Inf        m = m { depth = Nothing }
update (Limit n)  m = m { depth = Just n }
update (Output f) m = m { output = f }

getOpts :: IO (Mode, String, [FilePath])
getOpts = getArgs >>= \argv ->
  case getOpt Permute options argv of
       (o, (root:files), []  ) -> do when (Help `elem` o) (do putStrLn usage 
                                                              exitWith ExitSuccess)
                                     return (foldr update defaultMode o, root, files)
       (_, _, errs)      -> ioError (userError (concat errs ++ usage))
 where header = "Usage: graphtype [OPTION...] type_name file1.hs file2.hs ..."
       usage  = usageInfo header options
