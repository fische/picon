{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Type where

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
  CType CType |
  String |
  Bytes |
  Unicode |
  PythonObject
  deriving (Eq,Ord,Show,Typeable,Data)
