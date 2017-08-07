{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Annotation (
  Ref(..),
  getRefIdentifier,
  isLocalRef,
  isNonLocalRef,
  isGlobalRef,
  TypeAnnotation(..),
  isRef,
  isConst,
  CythonAnnotation(..),
  getType
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Language.Cython.Type

data Ref =
  LocalRef String |
  NonLocalRef String |
  GlobalRef String
  deriving (Eq,Ord,Show,Typeable,Data)

getRefIdentifier :: Ref -> String
getRefIdentifier (LocalRef ident) = ident
getRefIdentifier (NonLocalRef ident) = ident
getRefIdentifier (GlobalRef ident) = ident

isLocalRef :: Ref -> Bool
isLocalRef (LocalRef _) = True
isLocalRef _ = False

isNonLocalRef :: Ref -> Bool
isNonLocalRef (NonLocalRef _) = True
isNonLocalRef _ = False

isGlobalRef :: Ref -> Bool
isGlobalRef (GlobalRef _) = True
isGlobalRef _ = False

data TypeAnnotation =
  Const CythonType |
  Ref Ref
  deriving (Eq,Ord,Show,Typeable,Data)

isRef :: TypeAnnotation -> Bool
isRef (Ref _) = True
isRef _ = False

isConst :: TypeAnnotation -> Bool
isConst (Const _) = True
isConst _ = False

data CythonAnnotation =
  Locals (Map.Map String [TypeAnnotation]) |
  Type TypeAnnotation
  deriving (Eq,Ord,Show,Typeable,Data)

getType :: CythonAnnotation -> TypeAnnotation
getType (Type t) = t
getType _ = (Const $ PythonObject)
