{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Cythonizable (
  Cythonizable(..),
  module Cythonizable.Context
) where

import Data.Bool

import Control.Monad.State

import qualified Language.Python.Common.AST as AST
import Language.Cython.AST
import Language.Python.Common.SrcLocation (SrcSpan(..))

import Cythonizable.Context

-- TODO Cythonize "import" to "cimport"

class Cythonizable p c where
  cythonize :: p -> State Context c

instance (Cythonizable p c) => Cythonizable [p] [c] where
  cythonize = mapM cythonize

instance Cythonizable (AST.Module SrcSpan) (Module SrcSpan) where
  cythonize (AST.Module stmts) = do
    rstmts <- cythonize stmts
    vars <- getLocalVariables
    let cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
    bool
      (return $ Module rstmts)
      (return $ Module (cdef:rstmts))
      (not $ null vars)

instance Cythonizable (AST.Statement SrcSpan) (Statement SrcSpan) where
  cythonize (AST.Import items annot) =
    return $ Import items annot
  cythonize (AST.FromImport m from annot) =
    return $ FromImport m from annot
  cythonize (AST.Fun name args r body annot) = do
    funCtx <- dropNextScope $ AST.ident_string name
    let (returnType, retCtx) = runState getFunctionReturnType funCtx
        (vars, varsCtx) = runState getLocalVariables retCtx
        (cargs, argsCtx) = runState (cythonize args) varsCtx
        (cbody, _) = runState (cythonize body) argsCtx
        cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
    bool
      (return $ Fun name cargs r returnType cbody annot)
      (return $ Fun name cargs r returnType (cdef:cbody) annot)
      (not $ null vars)
  cythonize (AST.Class name args body annot) = do
    classCtx <- dropNextScope $ AST.ident_string name
    let (vars, varsCtx) = runState getLocalVariables classCtx
        (cbody, _) = runState (cythonize body) varsCtx
        cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
    bool
      (return $ Class name args cbody annot)
      (return $ Class name args (cdef:cbody) annot)
      (not $ null vars)
  cythonize s = return $ Statement s

instance Cythonizable (AST.Parameter SrcSpan) (Parameter SrcSpan) where
  cythonize (AST.Param ident py_annot dflt annot) = do
    typ <- getLocalVariableType $ AST.ident_string ident
    return $ Param typ ident py_annot dflt annot
  cythonize p = return $ Parameter p
