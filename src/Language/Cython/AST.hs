{-# LANGUAGE DeriveFunctor #-}

module Language.Cython.AST (
  Module(..),
  Suite,
  Statement(..),
  Handler(..),
  Parameter(..),
  Expr(..)
) where

import qualified Data.Map.Strict as Map

import qualified Language.Python.Common.AST as AST
import Language.Cython.Type

newtype Module annot = Module (Suite annot)
  deriving (Eq,Ord,Show,Functor)

type Suite annot = [Statement annot]

data Statement annot =
  Import {
    import_items :: [AST.ImportItem annot],
    stmt_annot :: annot
  } |
  FromImport {
    from_module :: AST.ImportRelative annot,
    from_items :: AST.FromItems annot,
    stmt_annot :: annot
  } |
  While {
    while_cond :: AST.Expr annot,
    while_body :: Suite annot,
    while_else :: Suite annot,
    stmt_annot :: annot
  } |
  For {
    for_targets :: [AST.Expr annot],
    for_generator :: AST.Expr annot,
    for_body :: Suite annot,
    for_else :: Suite annot,
    stmt_annot :: annot
  } |
  Fun {
    fun_name :: AST.Ident annot,
    fun_args :: [Parameter annot],
    fun_result_annotation :: Maybe (AST.Expr annot),
    fun_return :: CythonType,
    fun_body :: Suite annot,
    stmt_annot :: annot
  } |
  Class {
    class_name :: AST.Ident annot,
    class_args :: [AST.Argument annot],
    class_body :: Suite annot,
    stmt_annot :: annot
  } |
  Conditional {
    cond_guards :: [(AST.Expr annot, Suite annot)],
    cond_else :: Suite annot,
    stmt_annot :: annot
  } |
  Try {
    try_body :: Suite annot,
    try_excepts :: [Handler annot],
    try_else :: Suite annot,
    try_finally :: Suite annot,
    stmt_annot :: annot
  } |
  With {
    with_context :: [(AST.Expr annot, Maybe (AST.Expr annot))],
    with_body :: Suite annot,
    stmt_annot :: annot
  } |
  Decorated {
    decorated_decorators :: [AST.Decorator annot],
    decorated_def :: Statement annot,
    stmt_annot :: annot
  } |
  Assign {
    assign_to :: [Expr annot],
    assign_expr :: Expr annot,
    stmt_annot :: annot
  } |
  CDefSuite {
    var_list :: Map.Map String CythonType,
    stmt_annot :: annot
  } |
  Statement (AST.Statement annot)
  deriving (Eq,Ord,Show,Functor)

instance AST.Annotated Statement where
  annot stmt = stmt_annot stmt

data Handler annot =
  Handler {
    handler_clause :: AST.ExceptClause annot,
    handler_suite :: Suite annot,
    handler_annot :: annot
  }
  deriving (Eq,Ord,Show,Functor)

instance AST.Annotated Handler where
  annot handler = handler_annot handler

data Parameter annot =
  Param {
    param_type :: CythonType,
    param_name :: AST.Ident annot,
    param_py_annotation :: Maybe (AST.Expr annot),
    param_default :: Maybe (AST.Expr annot),
    param_annot :: annot
  } |
  Parameter (AST.Parameter annot)
  deriving (Eq,Ord,Show,Functor)

data Expr annot =
  AddressOf {
    address_of :: Expr annot,
    expr_annot :: annot
  } |
  Expr (AST.Expr annot)
  deriving (Eq,Ord,Show,Functor)

instance AST.Annotated Expr where
  annot expr = expr_annot expr
