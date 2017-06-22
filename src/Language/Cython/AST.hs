{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

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
  Assign {
    cdef :: Bool,
    ctype :: CythonType
  } |
  Expr {
    ctype :: CythonType
  } |
  Empty
  deriving (Eq,Ord,Show,Typeable,Data)

class Cythonizable t where
  cythonize :: (Span annot) => t annot -> t (Annotation, annot)

instance Cythonizable AST.Module where
  cythonize (AST.Module stmts) =
    AST.Module (cythonizeSuite stmts)

cythonizeSuite :: (Span s) => [AST.Statement s]
  -> [AST.Statement (Annotation, s)]
cythonizeSuite [] = []
cythonizeSuite (hd:tl) =
  (cythonize hd) : (cythonizeSuite tl)

  -- TODO Handle mutliple vars
instance Cythonizable AST.Statement where
  cythonize (AST.Assign [to] expr annot) =
    let cexpr = cythonize expr
        cto = cythonize to
        cannot = Assign { cdef = True, ctype = (getExprType cexpr) }
    in AST.Assign [cto] cexpr (cannot, annot)
  cythonize st = fmap (\annot -> (Empty, annot)) st

  -- TODO Handle complex numbers
instance Cythonizable AST.Expr where
  cythonize (AST.Int val lit annot) =
    AST.Int val lit (Expr . CType $ Signed Int, annot)
  cythonize (AST.LongInt val lit annot) =
    AST.LongInt val lit (Expr . CType $ Signed Long, annot)
  cythonize (AST.Float val lit annot) =
    AST.Float val lit (Expr . CType $ Signed Double, annot)
  cythonize (AST.Bool val annot) =
    AST.Bool val (Expr $ CType BInt, annot)
  cythonize e = fmap (\annot -> (Expr PythonObject, annot)) e

getExprType :: (Span s) => AST.Expr (Annotation, s) -> CythonType
getExprType expr
  | cannot /= Empty = ctype cannot
  | otherwise = PythonObject
  where (cannot, _) = AST.expr_annot expr
