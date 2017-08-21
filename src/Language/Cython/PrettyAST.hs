{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST () where

import qualified Data.Map.Strict as Map

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST ()
import Language.Cython.PrettyType ()
import Language.Cython.AST

indent :: Doc -> Doc
indent doc = nest 4 doc

prettySuite :: [Statement a] -> Doc
prettySuite stmts = vcat $ map pretty stmts

instance Pretty (Module annot) where
  pretty (Module stmts) = prettySuite stmts

optionalKeywordSuite :: String -> (Suite annot) -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts =
  text keyword <> colon $+$ indent (prettySuite stmts)

prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

prettyGuards :: [(AST.Expr annot, Suite annot)] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards) =
  text "elif" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
  prettyGuards guards

instance Pretty (Statement annot) where
  pretty stmt@(While {}) =
    text "while" <+> pretty (while_cond stmt) <> colon $+$
    indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
  pretty stmt@(For {}) =
    text "for" <+> commaList (for_targets stmt) <+> text "in" <+> pretty (for_generator stmt) <> colon $+$
    indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
  pretty stmt@(Fun {}) =
    text "cdef" <+> pretty (fun_return stmt) <+> pretty (fun_name stmt) <> parens (commaList (fun_args stmt)) <+>
    perhaps (fun_result_annotation stmt) (text "->") <+>
    pretty (fun_result_annotation stmt) <> colon $+$ indent (prettySuite (fun_body stmt))
  pretty stmt@(Class {}) =
    text "cdef class" <+> pretty (class_name stmt) <> prettyOptionalList (class_args stmt) <>
    colon $+$ indent (prettySuite (class_body stmt))
  pretty (Conditional { cond_guards = guards, cond_else = optionalElse }) =
    case guards of
      (cond,body):xs ->
        text "if" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
        prettyGuards xs $+$
        optionalKeywordSuite "else" optionalElse
      [] -> error "Attempt to pretty print conditional statement with empty guards"
  pretty (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally}) =
    text "try" <> colon $+$ indent (prettySuite body) $+$
    prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$
    optionalKeywordSuite "finally" finally
  pretty (With { with_context = context, with_body = body }) =
    text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
    indent (prettySuite body)
  pretty (Decorated { decorated_decorators = decs, decorated_def = stmt}) =
    vcat (map pretty decs) $+$ pretty stmt
  pretty (Assign { assign_to = p, assign_expr = e }) =
    commaList p <+> equals <+> pretty e
  pretty (CDefSuite vars _) = text "cdef:" $+$ indent (Map.foldrWithKey
    (\k v d -> d $+$ pretty v <+> pretty k) empty vars)
  pretty (Statement s) = pretty s

prettyWithContext :: (AST.Expr annot, Maybe (AST.Expr annot)) -> Doc
prettyWithContext (e, Nothing) = pretty e
prettyWithContext (e, Just as) = pretty e <+> text "as" <+> pretty as

prettyHandlers :: [Handler annot] -> Doc
prettyHandlers = foldr (\next rec -> pretty next $+$ rec) empty

instance Pretty (Handler annot) where
  pretty (Handler { handler_clause = exceptClause, handler_suite = suite }) =
    pretty exceptClause <> colon $+$ indent (prettySuite suite)

instance Pretty (Parameter annot) where
  pretty (Param { param_type = typ, param_name = ident, param_py_annotation = annot, param_default = def }) =
    pretty typ <+> pretty ident <> (maybe empty (\e -> colon <> pretty e <> space) annot) <>
    maybe empty (\e -> equals <> pretty e) def
  pretty (Parameter p) = pretty p

instance Pretty (Expr annot) where
  pretty (AddressOf expr _) = text "&(" <> pretty expr <> pretty ")"
  pretty (Expr expr) = pretty expr
