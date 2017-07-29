module Language.Cython where

import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Cython.Analyzable as Analyzable
import Language.Cython.Cythonizable as Cythonizable
import Language.Cython.Context
import Language.Cython.AST
import Language.Cython.Error
import Language.Cython.Annotation

cythonize :: Options -> AST.Module SrcSpan ->
  Either (Error SrcSpan) (Module (Maybe TypeAnnotation, SrcSpan))
cythonize opts pymodule =
  let tree = fmap (\s -> (Nothing, s)) pymodule
      analysisCtx = Analyzable.empty{Analyzable.options = opts}
      cythonizeCtx = Cythonizable.empty{Cythonizable.options = opts}
      analyzeFromTree = evalState (runExceptT $ analyze tree) analysisCtx
      cythonizeFrom analysis =
        evalState (runExceptT $ Cythonizable.cythonize analysis) cythonizeCtx
  in either Left cythonizeFrom analyzeFromTree
