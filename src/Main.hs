module Main where

import qualified System.Environment as Env (getArgs)

import qualified Language.Python.Version2 as Python2 (parseModule)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()

import Language.Cython.PrettyAST ()
import Language.Cython.AST
import Language.Cython.Context
import Control.Monad.State
import Control.Monad.Trans.Except

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python2.parseModule code file of
      Left err -> putStrLn $ prettyText err
      Right (pymodule, _) ->
        let tree = initCythonAST pymodule
            ctx = emptyContext
            results = evalState (runExceptT $ cythonize tree) ctx
        in putStrLn $ either prettyText prettyText results
