module Main where

import qualified System.Environment as Env (getArgs)

import qualified Language.Python.Version3 as Python3 (parseModule)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()

import PrettyScope ()
import Context
import Analyzable

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python3.parseModule code file of
      Left err -> putStrLn $ prettyText err
      Right (pymodule, _) ->
        putStrLn . prettyText . scope . analyze pymodule $ newContext
