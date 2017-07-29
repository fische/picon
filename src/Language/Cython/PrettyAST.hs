{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST () where

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST ()
import Language.Cython.AST
import Language.Cython.Type
import Language.Cython.Annotation

indent :: Doc -> Doc
indent doc = nest 4 doc

instance Pretty (Module (Maybe TypeAnnotation, annot)) where
  pretty (Module suite _) = pretty suite

instance Pretty (Suite (Maybe TypeAnnotation, annot)) where
  pretty (Suite stmts _) = vcat $ map pretty stmts

optionalKeywordSuite :: String -> (Suite (Maybe TypeAnnotation, annot)) -> Doc
optionalKeywordSuite _ (Suite [] _) = empty
optionalKeywordSuite keyword stmts =
  text keyword <> colon $+$ indent (pretty stmts)

prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

prettyGuards :: [(AST.Expr (Maybe TypeAnnotation, annot),
  Suite (Maybe TypeAnnotation, annot))] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards) =
  text "elif" <+> pretty cond <> colon $+$ indent (pretty body) $+$
  prettyGuards guards

instance Pretty (Statement (Maybe TypeAnnotation, annot)) where
  pretty stmt@(While {}) =
    text "while" <+> pretty (while_cond stmt) <> colon $+$
    indent (pretty (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
  pretty stmt@(For {}) =
    text "for" <+> commaList (for_targets stmt) <+> text "in" <+> pretty (for_generator stmt) <> colon $+$
    indent (pretty (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
  pretty stmt@(Fun {}) =
    text "def" <+> pretty (fun_name stmt) <> parens (commaList (fun_args stmt)) <+>
    perhaps (fun_result_annotation stmt) (text "->") <+>
    pretty (fun_result_annotation stmt) <> colon $+$ indent (pretty (fun_body stmt))
  pretty stmt@(Class {}) =
    text "class" <+> pretty (class_name stmt) <> prettyOptionalList (class_args stmt) <>
    colon $+$ indent (pretty (class_body stmt))
  pretty (Conditional { cond_guards = guards, cond_else = optionalElse }) =
    case guards of
      (cond,body):xs ->
        text "if" <+> pretty cond <> colon $+$ indent (pretty body) $+$
        prettyGuards xs $+$
        optionalKeywordSuite "else" optionalElse
      [] -> error "Attempt to pretty print conditional statement with empty guards"
  pretty (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally}) =
    text "try" <> colon $+$ indent (pretty body) $+$
    prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$
    optionalKeywordSuite "finally" finally
  pretty (With { with_context = context, with_body = body }) =
    text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
    indent (pretty body)
  pretty (Decorated { decorated_decorators = decs, decorated_def = stmt}) =
    vcat (map pretty decs) $+$ pretty stmt
  pretty (CDef ident expr (typ, _)) = text "cdef" <+>
    maybe (text "object") pretty typ <+> pretty ident <+>
    maybe empty (\e -> text "=" <+> pretty e) expr
  pretty (Statement s) = pretty s

prettyWithContext :: (AST.Expr (Maybe TypeAnnotation, annot),
  Maybe (AST.Expr (Maybe TypeAnnotation, annot))) -> Doc
prettyWithContext (e, Nothing) = pretty e
prettyWithContext (e, Just as) = pretty e <+> text "as" <+> pretty as

prettyHandlers :: [Handler (Maybe TypeAnnotation, annot)] -> Doc
prettyHandlers = foldr (\next rec -> pretty next $+$ rec) empty

instance Pretty (Handler (Maybe TypeAnnotation, annot)) where
  pretty (Handler { handler_clause = exceptClause, handler_suite = suite }) =
    pretty exceptClause <> colon $+$ indent (pretty suite)

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
  pretty (CType t) = pretty t
  pretty String = text "str"
  pretty Bytes = text "bytes"
  pretty Unicode = text "unicode"
  pretty PythonObject = text "object"

instance Pretty (TypeAnnotation) where
  pretty (Const typ) = pretty typ
  pretty _ = text "object"
