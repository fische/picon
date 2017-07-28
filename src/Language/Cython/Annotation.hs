{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Annotation (
  TypeAnnotation(..),
  getLocalRefIdentifier,
  getNonLocalRefIdentifier,
  getGlobalRefIdentifier,
  isLocalRef,
  isNonLocalRef,
  isGlobalRef,
  isConst,
  CythonAnnotation(..),
  getType
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Language.Cython.Type

data TypeAnnotation =
  Const CythonType |
  LocalRef String |
  NonLocalRef String |
  GlobalRef String |
  Unknown
  deriving (Eq,Ord,Show,Typeable,Data)

getLocalRefIdentifier :: TypeAnnotation -> Maybe String
getLocalRefIdentifier (LocalRef ident) = Just ident
getLocalRefIdentifier _ = Nothing

getNonLocalRefIdentifier :: TypeAnnotation -> Maybe String
getNonLocalRefIdentifier (NonLocalRef ident) = Just ident
getNonLocalRefIdentifier _ = Nothing

getGlobalRefIdentifier :: TypeAnnotation -> Maybe String
getGlobalRefIdentifier (GlobalRef ident) = Just ident
getGlobalRefIdentifier _ = Nothing

isLocalRef :: TypeAnnotation -> Bool
isLocalRef (LocalRef _) = True
isLocalRef _ = False

isNonLocalRef :: TypeAnnotation -> Bool
isNonLocalRef (NonLocalRef _) = True
isNonLocalRef _ = False

isGlobalRef :: TypeAnnotation -> Bool
isGlobalRef (GlobalRef _) = True
isGlobalRef _ = False

isConst :: TypeAnnotation -> Bool
isConst (Const _) = True
isConst _ = False

data CythonAnnotation =
  Locals (Map.Map String [TypeAnnotation]) |
  Type TypeAnnotation
  deriving (Eq,Ord,Show,Typeable,Data)

getType :: CythonAnnotation -> TypeAnnotation
getType (Type t) = t
getType _ = Unknown
