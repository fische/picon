{-# LANGUAGE FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}

module Cythonizable (
  Cythonizable(..),
  module Cythonizable.Context
) where

import qualified Data.Map.Strict as Map

import Control.Monad.State

import qualified Language.Python.Common.AST as AST
import Language.Cython.AST
import Language.Python.Common.SrcLocation (SrcSpan(..))

import Cythonizable.Context

class Cythonizable p c where
  cythonize :: p -> State Context c

instance (Cythonizable p c) => Cythonizable [p] [c] where
  cythonize l = mapM cythonize l

instance {-# OVERLAPPING #-} Cythonizable (AST.Suite SrcSpan) (Suite SrcSpan)
  where
  cythonize [] = return []
  cythonize (hd:tl) = do
    rhd <- cythonize hd
    rtl <- cythonize tl
    return (rhd ++ rtl)

instance Cythonizable (AST.Module SrcSpan) (Module SrcSpan) where
  cythonize (AST.Module stmts) = do
    rstmts <- cythonize stmts
    vars <- getLocalVariables
    ctx <- get
    let cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
        ctypedefs = Map.foldrWithKey
          (\k v d ->
            (CTypeDef {
              typedef_ident = AST.Ident{
                AST.ident_string = k,
                AST.ident_annot = SpanEmpty
              },
              typedef_type = v,
              stmt_annot = SpanEmpty
            }:d))
          []
          (typedefs ctx)
    return $ Module (ctypedefs ++ (cdef:rstmts))

instance Cythonizable (AST.Statement SrcSpan) [Statement SrcSpan] where
  cythonize (AST.Fun name args r body annot) = do
    ctx <- get
    funCtx <- dropNextFunction $ AST.ident_string name
    let (returnType, retCtx) = runState getFunctionReturnType funCtx
        (vars, varsCtx) = runState getLocalVariables retCtx
        (cargs, argsCtx) = runState (cythonize args) varsCtx
        (cbody, bodyCtx) = runState (cythonize body) argsCtx
        cdef = CDefSuite {
          var_list = vars,
          stmt_annot = SpanEmpty
        }
        funName = getFunctionName funCtx
        cname = name{
          AST.ident_string = funName
        }
        ast = Fun cname cargs r returnType (cdef:cbody) annot
        assign = Assign {
          assign_to = [Expr AST.Var{
            AST.var_ident = AST.Ident{
              AST.ident_string = AST.ident_string name,
              AST.ident_annot = SpanEmpty
            },
            AST.expr_annot = SpanEmpty
          }],
          assign_expr = AddressOf{
            address_of = Expr AST.Var{
              AST.var_ident = AST.Ident{
                AST.ident_string = funName,
                AST.ident_annot = SpanEmpty
              },
              AST.expr_annot = SpanEmpty
            },
            expr_annot = SpanEmpty
          },
          stmt_annot = SpanEmpty
        }
    put ctx{
      typedefs = typedefs bodyCtx
    }
    return $ [ast, assign]
  cythonize s = return $ [Statement s]

instance Cythonizable (AST.Parameter SrcSpan) (AST.Parameter SrcSpan) where
  cythonize p = return p
