module Language.Cython where

import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Cython.Analyzable as Analyzable
import Language.Cython.Context
import Language.Cython.AST
import Language.Cython.Error
import Language.Cython.Annotation

-- TODO Use Context from Context module
cythonize :: Options -> AST.Module SrcSpan ->
  Either (Error SrcSpan) (Module (Maybe CythonAnnotation, SrcSpan))
cythonize opts pymodule =
  let tree = fmap (\s -> (Nothing, s)) pymodule
      analysisCtx = Analyzable.empty{options = opts}
      analysisResults = evalState (runExceptT $ analyze tree) analysisCtx
  in analysisResults
