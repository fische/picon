{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Annotation where

import qualified Data.Map.Strict as Map
import Data.Data

import qualified Language.Python.Common.AST as AST
import Language.Cython.Type

data Type =
  Locals (Map.Map String [Type]) |
  Const CythonType |
  Ref String |
  None
  deriving (Eq,Ord,Show,Typeable,Data)

getType :: (AST.Annotated a) =>
  a (Type, annot) -> Type
getType a = fst $ AST.annot a
