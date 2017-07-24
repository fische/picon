{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Annotation where

import qualified Data.Map.Strict as Map
import Data.Data

import Language.Cython.Type

data TypeAnnotation =
  Const CythonType |
  Ref String |
  Unknown
  deriving (Eq,Ord,Show,Typeable,Data)

data CythonAnnotation =
  Locals (Map.Map String [TypeAnnotation]) |
  Type TypeAnnotation
  deriving (Eq,Ord,Show,Typeable,Data)

getType :: CythonAnnotation -> TypeAnnotation
getType (Type t) = t
getType _ = Unknown
