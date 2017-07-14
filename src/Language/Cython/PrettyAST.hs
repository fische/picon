{-# LANGUAGE FlexibleInstances #-}

module Language.Cython.PrettyAST where

import qualified Language.Cython.Annotation as Annotation
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.AST

indent :: Doc -> Doc
indent doc = nest 4 doc

prettySuite :: [Statement (Annotation.Annotation, s)] -> Doc
prettySuite stmts = vcat $ map pretty stmts

optionalKeywordSuite :: String -> [Statement (Annotation.Annotation, s)] -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts = text keyword <> colon $+$ indent (prettySuite stmts)

prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

prettyGuards :: [(Expr (Annotation.Annotation, s),
  Suite (Annotation.Annotation, s))] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards)
   = text "elif" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
     prettyGuards guards

prettyWithContext :: (Expr (Annotation.Annotation, s),
  Maybe (Expr (Annotation.Annotation, s))) -> Doc
prettyWithContext (e, Nothing) = pretty e
prettyWithContext (e, Just as) = pretty e <+> text "as" <+> pretty as

prettyHandlers :: [Handler (Annotation.Annotation, s)] -> Doc
prettyHandlers = foldr (\next rec -> pretty next $+$ rec) empty

instance {-# OVERLAPS #-} Pretty (Statement (Annotation.Annotation, s)) where
  pretty (Import { import_items = items}) = text "import" <+> commaList items
  pretty stmt@(FromImport {})
     = text "from" <+> pretty (from_module stmt) <+> text "import" <+> pretty (from_items stmt)
  pretty stmt@(While {})
     = text "while" <+> pretty (while_cond stmt) <> colon $+$
       indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
  pretty stmt@(For {})
     = text "for" <+> commaList (for_targets stmt) <+> text "in" <+> pretty (for_generator stmt) <> colon $+$
       indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
  pretty stmt@(Fun {})
     = text "def" <+> pretty (fun_name stmt) <> parens (commaList (fun_args stmt)) <+>
       perhaps (fun_result_annotation stmt) (text "->") <+>
       pretty (fun_result_annotation stmt) <> colon $+$ indent (prettySuite (fun_body stmt))
  pretty stmt@(Class {})
     = text "class" <+> pretty (class_name stmt) <> prettyOptionalList (class_args stmt) <>
       colon $+$ indent (prettySuite (class_body stmt))
  pretty (Conditional { cond_guards = guards, cond_else = optionalElse })
     = case guards of
          (cond,body):xs ->
             text "if" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
             prettyGuards xs $+$
             optionalKeywordSuite "else" optionalElse
          [] -> error "Attempt to pretty print conditional statement with empty guards"
  -- TODO Handle when assign_to is an array with multiple elements
  pretty (Assign [to@Var{expr_annot = (cytype, _)}] expr (cdef@(Annotation.CDef True), _))
     = pretty cdef <+> pretty cytype <+> pretty to <+> equals <+> pretty expr
  pretty (Assign { assign_to = pattern, assign_expr = e })
     = equalsList pattern <+> equals <+> pretty e
  pretty (AugmentedAssign { aug_assign_to = to_expr, aug_assign_op = op, aug_assign_expr = e})
     = pretty to_expr <+> pretty op <+> pretty e
  pretty (Decorated { decorated_decorators = decs, decorated_def = stmt})
     = vcat (map pretty decs) $+$ pretty stmt
  pretty (Return { return_expr = e }) = text "return" <+> pretty e
  pretty (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally})
     = text "try" <> colon $+$ indent (prettySuite body) $+$
       prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$
       optionalKeywordSuite "finally" finally
  pretty (Raise { raise_expr = e })
     = text "raise" <+> pretty e
  pretty (With { with_context = context, with_body = body })
     = text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon $+$
       indent (prettySuite body)
  pretty Pass {} = text "pass"
  pretty Break {} = text "break"
  pretty Continue {} = text "continue"
  pretty (Delete { del_exprs = es }) = text "del" <+> commaList es
  pretty (StmtExpr { stmt_expr = e }) = pretty e
  pretty (Global { global_vars = idents }) = text "global" <+> commaList idents
  pretty (NonLocal { nonLocal_vars = idents }) = text "nonlocal" <+> commaList idents
  pretty (Assert { assert_exprs = es }) = text "assert" <+> commaList es
  pretty (Print { print_chevron = have_chevron, print_exprs = es, print_trailing_comma = trail_comma }) =
     text "print" <> (if have_chevron then text " >>" else empty) <+>
     hcat (punctuate comma (map pretty es)) <>
     if trail_comma then comma else empty
  pretty (Exec { exec_expr = e, exec_globals_locals = gls }) =
     text "exec" <+> pretty e <+>
     maybe empty (\ (globals, next) -> text "in" <+> pretty globals <+>
     maybe empty (\locals -> comma <+> pretty locals) next) gls

instance Pretty (Annotation.CBasicType) where
  pretty Annotation.Char = text "char"
  pretty Annotation.Short = text "short"
  pretty Annotation.Int = text "int"
  pretty Annotation.Long = text "long"
  pretty Annotation.LongLong = text "long long"
  pretty Annotation.Float = text "float"
  pretty Annotation.Double = text "double"

instance Pretty (Annotation.CType) where
  pretty Annotation.Void = text "void"
  pretty Annotation.BInt = text "bint"
  pretty (Annotation.Signed t) = pretty t
  pretty (Annotation.Unsigned t) = text "unsigned " <+> pretty t
  pretty (Annotation.Ptr t) = pretty t <+> text "*"

instance Pretty (Annotation.CythonType) where
  pretty Annotation.Unknown = text "object"
  pretty (Annotation.CType t) = pretty t
  pretty Annotation.String = text "str"
  pretty Annotation.Bytes = text "bytes"
  pretty Annotation.Unicode = text "unicode"
  pretty Annotation.PythonObject = text "object"

instance Pretty (Annotation.Annotation) where
  pretty (Annotation.CDef True) = text "cdef"
  pretty (Annotation.Type cytype) = pretty cytype
  pretty _ = empty
