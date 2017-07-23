{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Type where

import qualified Data.Map.Strict as Map
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
  PythonObject |
  Function {
    returnType :: CythonType,
    paramTypes :: Map.Map (Int, String) CythonType
  }
  deriving (Eq,Ord,Show,Typeable,Data)
