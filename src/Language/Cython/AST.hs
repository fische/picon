{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (Span)
import Data.Data

data CBasicType = Char | Short | Int | Long | LongLong | Float | Double
  deriving (Eq,Ord,Show,Typeable,Data)
data CType = BInt | Unsigned CBasicType | Signed CBasicType | Ptr CType
  deriving (Eq,Ord,Show,Typeable,Data)
data CythonType = CType CType | PythonObject
  deriving (Eq,Ord,Show,Typeable,Data)

data Annotation =
  Expr {
    ctype :: CythonType
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
  let cexpr = cythonizeExpr expr
      cto = fmap (\s -> (Expr $ getExprType cexpr, s)) to
      cannot = Empty
  in AST.Assign [cto] cexpr (cannot, annot)
cythonizeStatement st = fmap (\annot -> (Empty, annot)) st

-- TODO Handle complex numbers
cythonizeExpr :: (Span s) => AST.Expr s -> AST.Expr (Annotation, s)
cythonizeExpr (AST.Int val lit annot) =
  AST.Int val lit (Expr . CType $ Signed Int, annot)
cythonizeExpr (AST.LongInt val lit annot) =
  AST.LongInt val lit (Expr . CType $ Signed Long, annot)
cythonizeExpr (AST.Float val lit annot) =
  AST.Float val lit (Expr . CType $ Signed Double, annot)
cythonizeExpr (AST.Bool val annot) =
  AST.Bool val (Expr $ CType BInt, annot)
cythonizeExpr e = fmap (\annot -> (Expr PythonObject, annot)) e

getExprType :: (Span s) => AST.Expr (Annotation, s) -> CythonType
getExprType expr
  | cannot /= Empty = ctype cannot
  | otherwise = PythonObject
  where (cannot, _) = AST.expr_annot expr
