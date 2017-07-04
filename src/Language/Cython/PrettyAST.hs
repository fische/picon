{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST where

import Language.Cython.AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST ()
import qualified Language.Python.Common.AST as AST

instance {-# OVERLAPS #-} Pretty (AST.Statement (Annotation, s)) where
  pretty _ = text "test"

instance Pretty (CBasicType) where
  pretty Char = text "char"
  pretty Short = text "short"
  pretty Int = text "int"
  pretty Long = text "long"
  pretty LongLong = text "long long"
  pretty Float = text "float"
  pretty Double = text "double"

instance Pretty (CType) where
  pretty Void = text "void"
  pretty BInt = text "bint"
  pretty (Signed t) = pretty t
  pretty (Unsigned t) = text "unsigned " <+> pretty t
  pretty (Ptr t) = pretty t <+> text "*"

instance Pretty (CythonType) where
  pretty Unknown = text "object"
  pretty (CType t) = pretty t
  pretty String = text "str"
  pretty Bytes = text "bytes"
  pretty Unicode = text "unicode"
  pretty PythonObject = text "object"
