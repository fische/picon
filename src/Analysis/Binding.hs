{-# LANGUAGE DeriveDataTypeable #-}

module Analysis.Binding (
  Binding(..),
  isLocal,
  isNonLocal,
  isGlobal,
  getBindingRef
) where

import Data.Data

import Type

-- TODO NonLocal and Global should hold a reference instead of a copy
data Binding =
  Local { cytype :: [Type] } |
  NonLocal { cytype :: [Type] } |
  Global { cytype :: [Type] }
  deriving (Eq,Ord,Show,Typeable,Data)

isLocal :: Binding -> Bool
isLocal (Local _) = True
isLocal _ = False

isNonLocal :: Binding -> Bool
isNonLocal (NonLocal _) = True
isNonLocal _ = False

isGlobal :: Binding -> Bool
isGlobal (Global _) = True
isGlobal _ = False

getBindingRef :: String -> Binding -> Ref
getBindingRef ident (Local _) = LocalRef ident
getBindingRef ident (NonLocal _) = NonLocalRef ident
getBindingRef ident (Global _) = GlobalRef ident
