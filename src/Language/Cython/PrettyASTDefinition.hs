{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Cython.PrettyASTDefinition (
  Language.Cython.PrettyASTDefinition.pretty
) where

import qualified Data.Map.Strict as Map

import Language.Python.Common.Pretty as Pretty
import Language.Python.Common.PrettyAST ()
import Language.Cython.PrettyType ()
import Language.Cython.AST

pretty :: (PrettyDefinition a) => a -> String
pretty = render . prettyDefinition

class PrettyDefinition a where
  prettyDefinition :: a -> Doc

indent :: Doc -> Doc
indent doc = nest 4 doc

commaListDefinition :: PrettyDefinition a => [a] -> Doc
commaListDefinition = hsep . punctuate comma . map prettyDefinition

prettySuite :: [Statement a] -> Doc
prettySuite stmts = vcat $ map prettyDefinition stmts

instance PrettyDefinition (Module annot) where
  prettyDefinition (Module stmts) = prettySuite stmts

prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

instance PrettyDefinition (Statement annot) where
  prettyDefinition (Import { import_items = items}) = text "cimport" <+> commaList items
  prettyDefinition stmt@(FromImport {})
    = text "from" <+> prettyDefinition (from_module stmt) <+> text "cimport" <+> prettyDefinition (from_items stmt)
  prettyDefinition stmt@(Fun {}) =
    text "cdef" <+> prettyDefinition (fun_return stmt) <+> prettyDefinition (fun_name stmt) <> parens (commaListDefinition (fun_args stmt)) <+>
    perhaps (fun_result_annotation stmt) (text "->") <+>
    prettyDefinition (fun_result_annotation stmt)
  prettyDefinition stmt@(Class {}) =
    text "cdef class" <+> prettyDefinition (class_name stmt) <> prettyOptionalList (class_args stmt) <>
    colon $+$ indent (prettySuite (class_body stmt))
  prettyDefinition (Decorated { decorated_decorators = decs, decorated_def = stmt}) =
    vcat (map prettyDefinition decs) $+$ prettyDefinition stmt
  prettyDefinition (CDefSuite vars _) = text "cdef:" $+$ indent (Map.foldrWithKey
    (\k v d -> d $+$ prettyDefinition v <+> prettyDefinition k) empty vars)
  prettyDefinition _ = empty

instance PrettyDefinition (Handler annot) where
  prettyDefinition (Handler { handler_clause = exceptClause, handler_suite = suite }) =
    prettyDefinition exceptClause <> colon $+$ indent (prettySuite suite)

instance PrettyDefinition (Parameter annot) where
  prettyDefinition (Param { param_type = typ, param_name = ident, param_py_annotation = annot, param_default = def }) =
    prettyDefinition typ <+> prettyDefinition ident <> maybe empty (\e -> colon <> prettyDefinition e <> space) annot <>
    maybe empty (\e -> equals <> prettyDefinition e) def
  prettyDefinition (Parameter p) = prettyDefinition p

instance {-# OVERLAPPABLE #-} (Pretty a) => PrettyDefinition a where
  prettyDefinition = Pretty.pretty
