{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Language.Cython.AST where

import Data.Data

import qualified Language.Python.Common.AST as AST

data Module annot =
  Module {
    module_body :: Suite annot,
    module_annot :: annot
  }
  deriving (Eq,Ord,Show,Typeable,Data,Functor)

instance AST.Annotated Module where
  annot m = module_annot m

data Suite annot =
  Suite {
    suite_stmts :: [Statement annot],
    suite_annot :: annot
  }
  deriving (Eq,Ord,Show,Typeable,Data,Functor)

instance AST.Annotated Suite where
  annot suite = suite_annot suite

data Statement annot =
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
    fun_args :: [AST.Parameter annot],
    fun_result_annotation :: Maybe (AST.Expr annot),
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
  CDef {
    var_name :: AST.Ident annot,
    var_value :: Maybe (AST.Expr annot),
    stmt_annot :: annot
  } |
  Statement (AST.Statement annot)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)

instance AST.Annotated Statement where
  annot stmt = stmt_annot stmt

data Handler annot =
  Handler {
    handler_clause :: AST.ExceptClause annot,
    handler_suite :: Suite annot,
    handler_annot :: annot
  }
  deriving (Eq,Ord,Show,Typeable,Data,Functor)

instance AST.Annotated Handler where
  annot handler = handler_annot handler
