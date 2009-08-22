module OptionParser
  (
   Mode(..),
   getOpts,
  )
where

import System.Environment (getArgs)    
import System.Exit
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Control.Monad (when)

data Flag 
     = Output FilePath | Trim | Help
       deriving (Eq,Show)

options :: [OptDescr Flag]
options =
  [ Option ['o']        ["output"]  (OptArg getOutput "file") "Name of the output file (default: output.dot)",
    Option ['t']        ["trim"]    (NoArg Trim)    "Trim types/newtype that do not have references to other user-defined types",
    Option []           ["help"]    (NoArg Help)    "Show this help" ]

getOutput Nothing = Output "output.dot"
getOutput (Just s) = Output s

data Mode = Mode { output :: String, trim :: Bool }
defaultMode = Mode "output.dot" False

update (Output f) m = m { output = f }
update Trim m       = m { trim = True }

getOpts :: IO (Mode, String, [FilePath])
getOpts = getArgs >>= \argv ->
  case getOpt Permute options argv of
       (o, (root:files), []  ) -> do when (Help `elem` o) (do putStrLn usage 
                                                              exitWith ExitSuccess)
                                     return (foldr update defaultMode o, root, files)
       (_, _, errs)      -> ioError (userError (concat errs ++ usage))
 where header = "Usage: graphtype [OPTION...] type_name file1.hs file2.hs ..."
       usage  = usageInfo header options
