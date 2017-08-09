module Main where

import qualified System.Environment as Env (getArgs)

import Control.Monad.State (evalState)
import Control.Monad.Trans.Except (runExceptT)

import qualified Language.Python.Version3 as Python3 (parseModule)
import qualified Language.Python.Common.AST as AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Cython.PrettyAST ()
import Language.Cython.AST
import Language.Cython.Error
import Language.Cython.Type

import Analysis.Analyzable
import Cythonizing.Cythonizable as Cythonizable
import Cythonizing.Context as CythonizingContext
import Analysis.Context as AnalysisContext
import Options

cythonize :: Options -> AST.Module SrcSpan ->
  Either (Error SrcSpan) (Module (Maybe CythonType, SrcSpan))
cythonize opts pymodule =
  let tree = fmap (\s -> (Nothing, s)) pymodule
      analysisCtx =
        AnalysisContext.newContext{AnalysisContext.options = opts}
      cythonizeCtx =
        CythonizingContext.newContext{CythonizingContext.options = opts}
      analyzeFromTree = evalState (runExceptT $ analyze tree) analysisCtx
      cythonizeFrom analysis =
        evalState (runExceptT $ Cythonizable.cythonize analysis) cythonizeCtx
  in either Left cythonizeFrom analyzeFromTree

main :: IO ()
main = do
  [file] <- Env.getArgs -- TODO Handle correctly args
  code <- readFile file
  case Python3.parseModule code file of
      Left err -> putStrLn $ prettyText err
      Right (pymodule, _) ->
        let opts = Options{}
        in putStrLn . either prettyText prettyText $
            Main.cythonize opts pymodule
