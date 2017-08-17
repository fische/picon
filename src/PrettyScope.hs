{-# LANGUAGE FlexibleInstances #-}

module PrettyScope () where

import qualified Data.Map.Strict as Map

import Language.Python.Common.Pretty
import Language.Cython.PrettyType ()
import Language.Cython.Type
import Scope

instance Pretty Path where
  pretty Leaf = empty
  pretty (Node((ident, idx), Leaf)) = text ident <+> text "-" <+> pretty idx
  pretty (Node(_, p)) = pretty p

instance Pretty Type where
  pretty (Type t) = pretty t
  pretty (Either (t1, t2)) =
    text "(" <> pretty t1 <+> text "|" <+> pretty t2 <> text ")"
  pretty VarRef{ types = t } = pretty t
  pretty FuncRef{} = text "fun_ptr"

instance Pretty [Type] where
  pretty [] = pretty Void
  pretty l = foldr (\t d -> d <+> pretty t) empty l

instance Pretty (Map.Map String [Type]) where
  pretty m = Map.foldrWithKey
    (\k v d -> d $+$ text "<" <+> pretty v <+> text ">" <+> pretty k) empty m

instance Pretty [Scope] where
  pretty l = foldr (\s d -> d $+$ pretty s) empty l

instance Pretty (Map.Map String [Scope]) where
  pretty m = Map.foldr (\l d -> d $+$ pretty l) empty m

instance Pretty Scope where
  pretty (Module{ variables = v, functions = f }) = pretty v $+$ pretty f
  pretty (Function{ returnType = r, path = p, variables = v, functions = f }) =
    text "<" <+> pretty r <+> text ">" <+> pretty p <> text ":" $+$
    nest 4 (pretty v $+$ pretty f)
