module Main where

import qualified System.Environment as Env (getArgs)

import qualified Language.Python.Common.Pretty as Pretty (prettyText)
import qualified Language.Python.Version2 as Python2 (parseModule)
import Language.Python.Common.PrettyParseError ()

import qualified Language.Cython.AST as AST
import Language.Cython.PrettyAST ()

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python2.parseModule code file of
       Left err -> putStrLn $ Pretty.prettyText err
       Right (pymodule, _) -> putStrLn . Pretty.prettyText $ AST.cythonize pymodule
