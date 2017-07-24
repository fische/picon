module Main where

import qualified System.Environment as Env (getArgs)

import qualified Language.Python.Version2 as Python2 (parseModule)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()
import Language.Cython.Context
import Language.Cython

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python2.parseModule code file of
      Left err -> putStrLn $ prettyText err
      Right (pymodule, _) ->
        let opts = Options{}
        in either print print $ cythonize opts pymodule
