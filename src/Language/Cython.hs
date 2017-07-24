module Language.Cython where

import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Cython.AST
import Language.Cython.Error
import Language.Cython.Analyzable
import Language.Cython.Annotation

-- TODO Use Context from Context module
cythonize :: Context -> AST.Module SrcSpan ->
  Either (Error SrcSpan) (Module (Maybe CythonAnnotation, SrcSpan))
cythonize ctx pymodule =
  let tree = fmap (\s -> (Nothing, s)) pymodule
  in evalState (runExceptT $ analyze tree) ctx
