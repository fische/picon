{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Annotation where

import Data.Data

data CBasicType =
  Char |
  Short |
  Int |
  Long |
  LongLong |
  Float |
  Double
  deriving (Eq,Ord,Show,Typeable,Data)

data CType =
  Void |
  BInt |
  Signed CBasicType |
  Unsigned CBasicType |
  Ptr CType
  deriving (Eq,Ord,Show,Typeable,Data)

data CythonType =
  Unknown |
  CType CType |
  String |
  Bytes |
  Unicode |
  PythonObject
  deriving (Eq,Ord,Show,Typeable,Data)

data Annotation =
  CDef Bool |
  Type CythonType |
  Empty
  deriving (Eq,Ord,Show,Typeable,Data)

getAnnotationType :: Annotation -> CythonType
getAnnotationType (Type typ) = typ
getAnnotationType _ = Unknown
