module Main where

import qualified System.Environment as Env
import qualified Language.Python.Common.Pretty as Pretty
import qualified Language.Python.Common.PrettyParseError ()
import qualified Language.Python.Common.PrettyAST ()
import qualified Language.Python.Version2 as Python2

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python2.parseModule code file of
       Left err -> putStrLn $ Pretty.prettyText err
       Right (pymodule, _) -> putStrLn $ Pretty.prettyText pymodule
