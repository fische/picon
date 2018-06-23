{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Cython.PrettyASTImplementation (
  Language.Cython.PrettyASTImplementation.pretty
) where

import qualified Data.Map.Strict as Map

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.Pretty as Pretty
import Language.Python.Common.PrettyAST ()
import Language.Cython.PrettyType ()
import Language.Cython.AST

pretty :: (PrettyImplementation a) => a -> String
pretty = render . prettyImplementation

class PrettyImplementation a where
  prettyImplementation :: a -> Doc

indent :: Doc -> Doc
indent doc = nest 4 doc

commaListImplementation :: PrettyImplementation a => [a] -> Doc
commaListImplementation = hsep . punctuate comma . map prettyImplementation

prettySuite :: [Statement a] -> Doc
prettySuite stmts = vcat $ map prettyImplementation stmts

instance PrettyImplementation (Module annot) where
  prettyImplementation (Module stmts) = prettySuite stmts

optionalKeywordSuite :: String -> (Suite annot) -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts =
  text keyword <> colon $+$ indent (prettySuite stmts)

prettyOptionalList :: PrettyImplementation a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaListImplementation list

prettyGuards :: [(AST.Expr annot, Suite annot)] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards) =
  text "elif" <+> prettyImplementation cond <> colon $+$ indent (prettySuite body) $+$
  prettyGuards guards

instance PrettyImplementation (Statement annot) where
  prettyImplementation (Import { import_items = items}) = text "cimport" <+> commaList items
  prettyImplementation stmt@(FromImport {})
    = text "from" <+> prettyImplementation (from_module stmt) <+> text "cimport" <+> prettyImplementation (from_items stmt)
  prettyImplementation stmt@(While {}) =
    text "while" <+> prettyImplementation (while_cond stmt) <> colon $+$
    indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
  prettyImplementation stmt@(For {}) =
    text "for" <+> commaList (for_targets stmt) <+> text "in" <+> prettyImplementation (for_generator stmt) <> colon $+$
    indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
  prettyImplementation stmt@(Fun {}) =
    text "cdef" <+> prettyImplementation (fun_return stmt) <+> prettyImplementation (fun_name stmt) <> parens (commaListImplementation (fun_args stmt)) <+>
    perhaps (fun_result_annotation stmt) (text "->") <+>
    prettyImplementation (fun_result_annotation stmt) <> colon $+$ indent (prettySuite (fun_body stmt))
  prettyImplementation stmt@(Class {}) =
    text "cdef class" <+> prettyImplementation (class_name stmt) <> prettyOptionalList (class_args stmt) <>
    colon $+$ indent (prettySuite (class_body stmt))
  prettyImplementation (Conditional { cond_guards = guards, cond_else = optionalElse }) =
    case guards of
      (cond,body):xs ->
        text "if" <+> prettyImplementation cond <> colon $+$ indent (prettySuite body) $+$
        prettyGuards xs $+$
        optionalKeywordSuite "else" optionalElse
      [] -> error "Attempt to prettyImplementation print conditional statement with empty guards"
  prettyImplementation (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally}) =
    text "try" <> colon $+$ indent (prettySuite body) $+$
    prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$
    optionalKeywordSuite "finally" finally
  prettyImplementation (With { with_context = context, with_body = body }) =
    text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
    indent (prettySuite body)
  prettyImplementation (Decorated { decorated_decorators = decs, decorated_def = stmt}) =
    vcat (map prettyImplementation decs) $+$ prettyImplementation stmt
  prettyImplementation (Assign { assign_to = p, assign_expr = e }) =
    commaListImplementation p <+> equals <+> prettyImplementation e
  prettyImplementation (CDefSuite vars _) = text "cdef:" $+$ indent (Map.foldrWithKey
    (\k v d -> d $+$ prettyImplementation v <+> prettyImplementation k) empty vars)
  prettyImplementation (Statement s) = prettyImplementation s

prettyWithContext :: (AST.Expr annot, Maybe (AST.Expr annot)) -> Doc
prettyWithContext (e, Nothing) = prettyImplementation e
prettyWithContext (e, Just as) = prettyImplementation e <+> text "as" <+> prettyImplementation as

prettyHandlers :: [Handler annot] -> Doc
prettyHandlers = foldr (\next rec -> prettyImplementation next $+$ rec) empty

instance PrettyImplementation (Handler annot) where
  prettyImplementation (Handler { handler_clause = exceptClause, handler_suite = suite }) =
    prettyImplementation exceptClause <> colon $+$ indent (prettySuite suite)

instance PrettyImplementation (Parameter annot) where
  prettyImplementation (Param { param_type = typ, param_name = ident, param_py_annotation = annot, param_default = def }) =
    prettyImplementation typ <+> prettyImplementation ident <> maybe empty (\e -> colon <> prettyImplementation e <> space) annot <>
    maybe empty (\e -> equals <> prettyImplementation e) def
  prettyImplementation (Parameter p) = prettyImplementation p

instance PrettyImplementation (Expr annot) where
  prettyImplementation (AddressOf expr _) = text "&(" <> prettyImplementation expr <> prettyImplementation ")"
  prettyImplementation (Expr expr) = prettyImplementation expr

instance {-# OVERLAPPABLE #-} (Pretty a) => PrettyImplementation a where
  prettyImplementation = Pretty.pretty
