{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Type where

import Data.Data

import Control.Monad.Trans.Except

import Language.Cython.Error

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

mergeTypes' :: annot -> CythonType -> [CythonType] ->
  Except (Error annot) CythonType
mergeTypes' _ acc [] = return acc
mergeTypes' loc acc (hd:tl)
  | acc /= hd = throwE $ errNotSupported loc "Multi-typed variables"
  | otherwise = mergeTypes' loc hd tl

mergeTypes :: annot -> [CythonType] -> Except (Error annot) CythonType
mergeTypes _ [] = return $ CType Void
mergeTypes loc (hd:tl) = mergeTypes' loc hd tl
