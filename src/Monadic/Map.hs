module Monadic.Map (
  mapWithKeyM,
  alterM,
  foldrWithKeyM
) where

import qualified Data.Map.Strict as Map

mapWithKeyM' :: (Monad m, Ord k) => (k -> a -> m b) -> [(k, a)] -> m [(k, b)]
mapWithKeyM' _ [] = return []
mapWithKeyM' f ((k, v):tl) = do
  newValue <- f k v
  newTail <- mapWithKeyM' f tl
  return ((k, newValue) : newTail)

-- | mapWithKeyM is the monadic version of `Map.mapWithKey`.
mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m b) -> Map.Map k a ->
  m (Map.Map k b)
mapWithKeyM f m = do
  result <- mapWithKeyM' f $ Map.toList m
  return $ Map.fromList result

alterM' :: (Monad m, Ord k) => (Maybe a -> m (Maybe a)) -> k -> [(k, a)] ->
  m [(k, a)]
alterM' f k [] = do
  r <- f Nothing
  return $ maybe [] (\v -> [(k, v)]) r
alterM' f k (hd@(k1, v1):tl)
  | k == k1 = do
    a <- f (Just v1)
    return $ maybe tl (\v2 -> (k1, v2):tl) a
  | otherwise = do
    newTail <- alterM' f k tl
    return (hd:newTail)

-- | alterM is the monadic version of `Map.alter`.
alterM :: (Monad m, Ord k) => (Maybe a -> m (Maybe a)) -> k -> Map.Map k a ->
  m (Map.Map k a)
alterM f k m = do
  result <- alterM' f k $ Map.toList m
  return $ Map.fromList result

foldrWithKeyM' :: (Monad m) => (k -> a -> b -> m b) -> b -> [(k, a)] -> m b
foldrWithKeyM' _ acc [] = return acc
foldrWithKeyM' f acc ((k, v):tl) = do
  result <- f k v acc
  foldrWithKeyM' f result tl

-- | foldrWithKeyM is the monadic version of `Map.foldrWithKey`.
foldrWithKeyM :: (Monad m) => (k -> a -> b -> m b) -> b -> Map.Map k a -> m b
foldrWithKeyM f acc = foldrWithKeyM' f acc . Map.toList
