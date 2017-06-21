{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST where

import Language.Python.Common.PrettyAST ()
import Language.Python.Common.SrcLocation (Span)
import Language.Python.Common.Pretty
import qualified Language.Python.Common.AST as PAST
import qualified Language.Cython.AST as CAST

instance {-# OVERLAPS #-} (Span s) => Pretty (PAST.Expr (CAST.Annotation, s)) where
  pretty (PAST.Var { PAST.var_ident = i, PAST.expr_annot = (CAST.Expr ctype, _) }) = text "cdef " <+> pretty ctype <+> text " " <+> pretty i

instance Pretty (CAST.CBasicType) where
  pretty CAST.Char = text "char"
  pretty CAST.Short = text "short"
  pretty CAST.Int = text "int"
  pretty CAST.Long = text "long"
  pretty CAST.LongLong = text "long long"
  pretty CAST.Float = text "float"
  pretty CAST.Double = text "double"

instance Pretty (CAST.CType) where
  pretty CAST.BInt = text "bint"
  pretty (CAST.Signed t) = pretty t
  pretty (CAST.Unsigned t) = text "unsigned " <+> pretty t
  pretty (CAST.Ptr t) = pretty t <+> text "*"

instance Pretty (CAST.CythonType) where
  pretty (CAST.CType t) = pretty t
  pretty CAST.PythonObject = text "object"
