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
  pretty ParamRef{ identifier = i } = text "param_ptr(" <> text i <> text ")"
  pretty FuncRef{} = text "fun_ptr"
  pretty ClassRef{} = text "class_ptr"

instance Pretty [Type] where
  pretty [] = pretty Void
  pretty l = foldr (\t d -> d <+> pretty t) empty l

instance Pretty (Map.Map String [Type]) where
  pretty = Map.foldrWithKey
    (\k v d -> d $+$ text "<" <+> pretty v <+> text ">" <+> pretty k) empty

instance Pretty [Scope] where
  pretty = foldr (\s d -> d $+$ pretty s) empty

instance Pretty (Map.Map String [Scope]) where
  pretty = Map.foldr (\l d -> d $+$ pretty l) empty

instance Pretty Scope where
  pretty Class{ path = p, variables = v, scopes = s } =
    pretty p <> text ":" $+$
    pretty v $+$ pretty s
  pretty Module{ variables = v, scopes = s } = pretty v $+$ pretty s
  pretty Function{ returnType = r, path = p, variables = v, scopes = s } =
    text "<" <+> pretty r <+> text ">" <+> pretty p <> text ":" $+$
    nest 4 (pretty v $+$ pretty s)
