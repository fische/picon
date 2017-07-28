{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context (
  ContextState,
  Language.Cython.Context.runState,
  Binding(..),
  isLocal,
  isGlobal,
  isNonLocal,
  mapWithKey,
  ResolverState,
  resolveRefs,
  Options(..)
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Annotation
import Language.Cython.Error

type ContextState ctx annot = ExceptT (Error annot) (State ctx)

runState :: ContextState ctx annot a -> ctx -> ContextState ctx annot (a, ctx)
runState s c = do
  let (newState, newCtx) = Control.Monad.State.runState (runExceptT s) c
  either throwE (\r -> return (r, newCtx)) newState

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

mapWithKey' :: (Monad m) => (k -> a -> m b) -> [(k, a)] -> m [(k, b)]
mapWithKey' _ [] = return []
mapWithKey' f ((k, v):tl) = do
  newValue <- f k v
  newTail <- mapWithKey' f tl
  return ((k, newValue) : newTail)

mapWithKey :: (Ord k, Monad m) => (k -> a -> m b) -> Map.Map k a ->
  m (Map.Map k b)
mapWithKey f m = do
  result <- mapWithKey' f $ Map.toList m
  return (Map.fromList result)

type ResolverState = State (Map.Map Ref [TypeAnnotation])

-- TODO Handle cycling referencing
resolveRefs :: (Ref -> Maybe [TypeAnnotation]) -> String -> [TypeAnnotation] ->
  ResolverState [TypeAnnotation]
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
      in maybe resolve return $ Map.lookup ref st) $
    maybeResolve ref
  newTail <- resolveRefs maybeResolve ident tl
  return (newHead ++ newTail)
resolveRefs maybeResolve ident (hd:tl) = do
  newTail <- resolveRefs maybeResolve ident tl
  return (hd:newTail)

data Options =
  Options {}
  deriving (Eq,Ord,Show,Typeable,Data)
