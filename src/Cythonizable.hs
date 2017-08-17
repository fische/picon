{-# LANGUAGE FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}

module Cythonizable (
  Cythonizable(..),
  module Cythonizable.Context
) where

import Control.Monad.State

import qualified Language.Python.Common.AST as AST
import Language.Cython.AST
import Language.Python.Common.SrcLocation (SrcSpan(..))

import Cythonizable.Context

class Cythonizable p c where
  cythonize :: p -> State Context c

instance (Cythonizable p c) => Cythonizable [p] [c] where
  cythonize l = mapM cythonize l

instance Cythonizable (AST.Module SrcSpan) (Module SrcSpan) where
  cythonize (AST.Module stmts) = do
    rstmts <- cythonize stmts
    vars <- getLocalVariables
    let cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
    return $ Module (cdef:rstmts)

instance Cythonizable (AST.Statement SrcSpan) (Statement SrcSpan) where
  cythonize (AST.Fun name args r body annot) = do
    funCtx <- dropNextFunction $ AST.ident_string name
    let (returnType, retCtx) = runState getFunctionReturnType funCtx
        (vars, varsCtx) = runState getLocalVariables retCtx
        (cargs, argsCtx) = runState (cythonize args) varsCtx
        (cbody, _) = runState (cythonize body) argsCtx
        cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
    return $ Fun name cargs r returnType (cdef:cbody) annot
  cythonize s = return $ Statement s

instance Cythonizable (AST.Parameter SrcSpan) (Parameter SrcSpan) where
  cythonize (AST.Param ident py_annot dflt annot) = do
    typ <- getLocalVariableType $ AST.ident_string ident
    return $ Param typ ident py_annot dflt annot
  cythonize p = return $ Parameter p
