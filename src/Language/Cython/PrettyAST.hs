{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST where

import Language.Python.Common.PrettyAST ()
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Common.Pretty
import qualified Language.Python.Common.AST as PAST
import qualified Language.Cython.AST as CAST

instance {-# OVERLAPS #-} Pretty (PAST.Statement (CAST.Annotation, SrcSpan)) where
  pretty (PAST.Assign { PAST.assign_to = [to], PAST.assign_expr = expr, PAST.stmt_annot = (cannot, _)})
    | CAST.cdef cannot = text "cdef " <+> pretty (CAST.ctype cannot) <+> pretty to <+> text " = "  <+> pretty expr
    | otherwise = pretty to <+> text " = " <+> pretty expr

instance Pretty (CAST.CType) where
  pretty CAST.Int = text "int"
  pretty CAST.Float = text "float"
  pretty CAST.PythonObject = text "object"
