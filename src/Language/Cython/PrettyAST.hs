{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST where

import qualified Language.Python.Common.AST as AST
import Language.Cython.AST
import Language.Python.Common.Pretty
import Language.Python.Common.SrcLocation (Span)
import Language.Python.Common.PrettyAST ()

instance {-# OVERLAPS #-} Pretty (AST.Statement (Annotation, a)) where
  pretty (AST.Assign { AST.assign_to = [to], AST.assign_expr = expr, AST.stmt_annot = (Assign{ cdef = c, ctype = t }, _)})
    | c = text "cdef " <+> pretty t <+> pretty to <+> text " = "  <+> pretty expr
    | otherwise = pretty to <+> text " = " <+> pretty expr

instance Pretty (CBasicType) where
  pretty Char = text "char"
  pretty Short = text "short"
  pretty Int = text "int"
  pretty Long = text "long"
  pretty LongLong = text "long long"
  pretty Float = text "float"
  pretty Double = text "double"

instance Pretty (CType) where
  pretty BInt = text "bint"
  pretty (Signed t) = pretty t
  pretty (Unsigned t) = text "unsigned " <+> pretty t
  pretty (Ptr t) = pretty t <+> text "*"

instance Pretty (CythonType) where
  pretty (CType t) = pretty t
  pretty PythonObject = text "object"
