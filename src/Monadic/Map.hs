module Monadic.Map (
  mapWithKey
) where

import qualified Data.Map.Strict as Map

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
