module Language.Cython where

import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Cython.AST
import Language.Cython.Error
import Language.Cython.Context
import Language.Cython.Analyzable
import Language.Cython.Annotation

cythonize :: AnalysisContext -> AST.Module SrcSpan ->
  Either (Error SrcSpan) (Module (Type, SrcSpan))
cythonize ctx pymodule =
  let tree = fmap (\s -> (None, s)) pymodule
  in evalState (runExceptT $ analyze tree) ctx
