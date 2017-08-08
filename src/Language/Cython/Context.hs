{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context (
  Binding(..),
  isLocal,
  isGlobal,
  isNonLocal,
  resolveTypes,
  resolveRefs,
  Options(..)
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Type
import Language.Cython.Annotation
import Language.Cython.Error

-- TODO NonLocal and Global should hold a reference instead of a copy
data Binding =
  Local { cytype :: [TypeAnnotation] } |
  NonLocal { cytype :: [TypeAnnotation] } |
  Global { cytype :: [TypeAnnotation] }
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

resolveTypes :: annot -> (Ref -> Maybe [CythonType]) -> String ->
  [TypeAnnotation] -> Except (Error annot) [CythonType]
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

-- TODO Handle cycling referencing
resolveRefs :: (Ref -> Maybe [TypeAnnotation]) -> String -> [TypeAnnotation] ->
  State (Map.Map Ref [TypeAnnotation]) [TypeAnnotation]
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

data Options =
  Options {}
  deriving (Eq,Ord,Show,Typeable,Data)
