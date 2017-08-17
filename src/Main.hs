module Main where

import qualified System.Environment as Env (getArgs)

import qualified Language.Python.Version3 as Python3 (parseModule)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()
import Language.Cython.PrettyAST ()
import Language.Cython.AST
import Language.Python.Common.SrcLocation (SrcSpan(..))

import Analyzable
import Cythonizable

import Control.Monad.State (State, evalState)

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python3.parseModule code file of
      Left err -> putStrLn $ prettyText err
      Right (pymodule, _) ->
        let analysis = analyze pymodule newContext
            cython =
              cythonize pymodule :: State Cythonizable.Context (Module SrcSpan)
        in putStrLn . prettyText . evalState cython $ fromAnalysis analysis
