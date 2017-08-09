{-# LANGUAGE DeriveDataTypeable #-}

module Type (
  Ref(..),
  isLocalRef,
  isNonLocalRef,
  isGlobalRef,
  getRefIdentifier,
  getLocalRefTypes,
  resolveRefs,
  resolveTypes,
  Type(..),
  isRef,
  isConst
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Type
import Language.Cython.Error

data Ref =
  LocalRef String |
  NonLocalRef String |
  GlobalRef String
  deriving (Eq,Ord,Show,Typeable,Data)

isLocalRef :: Ref -> Bool
isLocalRef (LocalRef _) = True
isLocalRef _ = False

isNonLocalRef :: Ref -> Bool
isNonLocalRef (NonLocalRef _) = True
isNonLocalRef _ = False

isGlobalRef :: Ref -> Bool
isGlobalRef (GlobalRef _) = True
isGlobalRef _ = False

getRefIdentifier :: Ref -> String
getRefIdentifier (LocalRef ident) = ident
getRefIdentifier (NonLocalRef ident) = ident
getRefIdentifier (GlobalRef ident) = ident

getLocalRefTypes :: Map.Map String a -> Ref -> Maybe a
getLocalRefTypes toResolve (LocalRef ident) = Map.lookup ident toResolve
getLocalRefTypes _ _ = Nothing

-- TODO Handle cycling referencing
resolveRefs :: (Ref -> Maybe [Type]) -> String -> [Type] ->
  State (Map.Map Ref [Type]) [Type]
resolveRefs _ _ [] = return []
resolveRefs maybeResolve ident (hd@(Ref ref):tl) = do
  st <- get
  newHead <- maybe
    (return [hd])
    (\var ->
      let resolve = do
            resolved <- resolveRefs maybeResolve ident var
            put $ Map.insert ref resolved st
            return resolved
      in maybe resolve return $ Map.lookup ref st)
    (maybeResolve ref)
  newTail <- resolveRefs maybeResolve ident tl
  return (newHead ++ newTail)
resolveRefs maybeResolve ident (hd:tl) = do
  newTail <- resolveRefs maybeResolve ident tl
  return (hd:newTail)

resolveTypes :: annot -> (Ref -> Maybe [CythonType]) -> String -> [Type] ->
  Except (Error annot) [CythonType]
resolveTypes _ _ _ [] = return []
resolveTypes loc maybeResolve ident ((Ref ref):tl) = do
  let refToString (GlobalRef r) = "global variable " ++ r
      refToString (NonLocalRef r) = "nonlocal variable " ++ r
      refToString (LocalRef r) = "local variable " ++ r
  newHead <- maybe
    (throwE $ errReferenceNotFound loc (refToString ref))
    return
    (maybeResolve ref)
  newTail <- resolveTypes loc maybeResolve ident tl
  return (newHead ++ newTail)
resolveTypes loc maybeResolve ident ((Const hd):tl) = do
  newTail <- resolveTypes loc maybeResolve ident tl
  return (hd:newTail)

data Type =
  Const CythonType |
  Ref Ref
  deriving (Eq,Ord,Show,Typeable,Data)

isRef :: Type -> Bool
isRef (Ref _) = True
isRef _ = False

isConst :: Type -> Bool
isConst (Const _) = True
isConst _ = False
