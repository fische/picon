{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (Span)
import Data.Data

data CType = Int | Float | PythonObject deriving (Eq,Ord,Show,Typeable,Data)
data Annotation =
  Assign {
    cdef :: Bool,
    ctype :: CType
  } |
  Empty
  deriving (Eq,Ord,Show,Typeable,Data)

cythonizeModule :: (Span s) => AST.Module s -> AST.Module (Annotation, s)
cythonizeModule (AST.Module stmts) =
  AST.Module (cythonizeStatements stmts)

cythonizeStatements :: (Span s) => [AST.Statement s]
  -> [AST.Statement (Annotation, s)]
cythonizeStatements [] = []
cythonizeStatements (hd:tl) =
  (cythonizeStatement hd) : (cythonizeStatements tl)

-- TODO Handle mutliple vars
cythonizeStatement :: (Span s) => AST.Statement s
  -> AST.Statement (Annotation, s)
cythonizeStatement (AST.Assign [to] expr annot) =
  let cannot = Assign { cdef = True, ctype = (getExprType expr) }
  in AST.Assign [cythonizeExpr to] (cythonizeExpr expr) (cannot, annot)
cythonizeStatement st = fmap (\annot -> (Empty, annot)) st

cythonizeExpr :: (Span s) => AST.Expr s -> AST.Expr (Annotation, s)
cythonizeExpr = fmap (\annot -> (Empty, annot))

getExprType :: AST.Expr s -> CType
getExprType (AST.Int _ _ _) = Int
getExprType (AST.Float _ _ _) = Float
getExprType _ = PythonObject
