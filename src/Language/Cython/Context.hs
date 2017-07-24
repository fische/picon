{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context (
  ContextState,
  Language.Cython.Context.runState,
  Binding(..),
  resolve,
  isLocal,
  isGlobal,
  isNonLocal
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Data

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Annotation
import Language.Cython.Error

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

-- TODO Handle cycling referencing
resolveRef' :: Map.Map String Binding -> String -> [Type] ->
  State (Map.Map String [Type]) [Type]
resolveRef' _ _ [] = return []
resolveRef' toResolve k (hd@(Ref ident):tl)
  | maybe False isLocal var = do
    st <- get
    let resolved = Map.lookup ident st
        varToResolve = cytype $ fromJust var
    newHead <- maybe (resolveRef toResolve ident varToResolve) return resolved
    newTail <- resolveRef' toResolve k tl
    return (newHead ++ newTail)
  | otherwise = do
    newTail <- resolveRef' toResolve k tl
    return (hd:newTail)
  where var = Map.lookup ident toResolve
resolveRef' toResolve k (hd:tl) = do
  newTail <- resolveRef' toResolve k tl
  return (hd:newTail)

resolveRef :: Map.Map String Binding -> String -> [Type] ->
  State (Map.Map String [Type]) [Type]
resolveRef toResolve k v = do
  resolved <- resolveRef' toResolve k v
  st <- get
  put (Map.insert k resolved st)
  return resolved

resolve :: Map.Map String Binding -> Map.Map String Binding ->
  Map.Map String Binding
resolve toResolve scope =
  let resolveBinding k v = do
        resolved <- resolveRef toResolve k (cytype v)
        return v{ cytype = resolved }
  in evalState (mapWithKey resolveBinding scope) Map.empty

type ContextState ctx annot = ExceptT (Error annot) (State ctx)

runState :: ContextState ctx annot a -> ctx -> ContextState ctx annot (a, ctx)
runState s c = do
  let (newState, newCtx) = Control.Monad.State.runState (runExceptT s) c
  either throwE (\r -> return (r, newCtx)) newState
