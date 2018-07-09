module Data.Tree.Path (
  Path(..),
  update,
  updateM,
  get,
  findl,
  findr,
  findlWithPath,
  findrWithPath,
  findlM,
  findrM,
  findlWithPathM,
  findrWithPathM
) where

import           Data.Semigroup
import           Data.Tree
import           Data.Maybe

newtype Path = Path [Int]
  deriving (Eq,Ord,Show)

update :: (Tree a -> Tree a) -> Path -> Tree a -> Tree a
update f (Path []) t = f t
update f (Path (idx:l)) t =
  let forest = subForest t
      result = update f (Path l) $ forest !! idx
  in t{
    subForest = take idx forest ++ [result] ++ drop (idx + 1) forest
  }

updateM :: (Monad m) => (Tree a -> m (Tree a)) -> Path -> Tree a -> m (Tree a)
updateM f (Path []) t = f t
updateM f (Path (idx:l)) t = do
  let forest = subForest t
  result <- updateM f (Path l) $ forest !! idx
  return t{
    subForest = take idx forest ++ [result] ++ drop (idx + 1) forest
  }

get :: Path -> Tree a -> Tree a
get (Path []) t      = t
get (Path (idx:l)) t = get (Path l) $ subForest t !! idx

findl :: (a -> Maybe b) -> Path -> Tree a -> Maybe b
findl f (Path []) t = f $ rootLabel t
findl f (Path (idx:l)) t
  | isNothing found = findl f (Path l) $ subForest t !! idx
  | otherwise = found
  where found = f $ rootLabel t

findr :: (a -> Maybe b) -> Path -> Tree a -> Maybe b
findr f (Path []) t = f $ rootLabel t
findr f (Path (idx:l)) t
  | isNothing found = f $ rootLabel t
  | otherwise = found
  where found = findr f (Path l) $ subForest t !! idx

findlWithPath' :: (Path -> a -> Maybe b) -> Path -> Path -> Tree a -> Maybe b
findlWithPath' f pos (Path []) t = f pos $ rootLabel t
findlWithPath' f pos (Path (idx:l)) t
  | isNothing found = findlWithPath' f (pos <> Path [idx]) (Path l) $ subForest t !! idx
  | otherwise = found
  where found = f pos $ rootLabel t

findlWithPath :: (Path -> a -> Maybe b) -> Path -> Tree a -> Maybe b
findlWithPath f p@(Path (idx:_)) = findlWithPath' f (Path [idx]) p
findlWithPath f p = findlWithPath' f p p

findrWithPath' :: (Path -> a -> Maybe b) -> Path -> Path -> Tree a -> Maybe b
findrWithPath' f pos (Path []) t = f pos $ rootLabel t
findrWithPath' f pos (Path (idx:l)) t
  | isNothing found = f pos $ rootLabel t
  | otherwise = found
  where found = findrWithPath' f (pos <> Path [idx]) (Path l) $ subForest t !! idx

findrWithPath :: (Path -> a -> Maybe b) -> Path -> Tree a -> Maybe b
findrWithPath f p@(Path (idx:_)) = findrWithPath' f (Path [idx]) p
findrWithPath f p = findrWithPath' f p p

findlM :: (Monad m) => (a -> m (Maybe b)) -> Path -> Tree a -> m (Maybe b)
findlM f (Path []) t = f $ rootLabel t
findlM f (Path (idx:l)) t = do
  found <- f $ rootLabel t
  let notFound = findlM f (Path l) $ subForest t !! idx
  maybe notFound (return . Just) found

findrM :: (Monad m) => (a -> m (Maybe b)) -> Path -> Tree a -> m (Maybe b)
findrM f (Path []) t = f $ rootLabel t
findrM f (Path (idx:l)) t = do
  found <- findrM f (Path l) $ subForest t !! idx
  maybe (f $ rootLabel t) (return . Just) found

findlWithPathM' :: (Monad m) => (Path -> a -> m (Maybe b)) -> Path -> Path -> Tree a -> m (Maybe b)
findlWithPathM' f pos (Path []) t = f pos $ rootLabel t
findlWithPathM' f pos (Path (idx:l)) t = do
  found <- f pos $ rootLabel t
  let notFound = findlWithPathM' f (pos <> Path [idx]) (Path l) $ subForest t !! idx
  maybe notFound (return . Just) found

findlWithPathM :: (Monad m) => (Path -> a -> m (Maybe b)) -> Path -> Tree a -> m (Maybe b)
findlWithPathM f p@(Path (idx:_)) = findlWithPathM' f (Path [idx]) p
findlWithPathM f p = findlWithPathM' f p p

findrWithPathM' :: (Monad m) => (Path -> a -> m (Maybe b)) -> Path -> Path -> Tree a -> m (Maybe b)
findrWithPathM' f pos (Path []) t = f pos $ rootLabel t
findrWithPathM' f pos (Path (idx:l)) t = do
  found <- findrWithPathM' f (pos <> Path [idx]) (Path l) $ subForest t !! idx
  maybe (f pos $ rootLabel t) (return . Just) found

findrWithPathM :: (Monad m) => (Path -> a -> m (Maybe b)) -> Path -> Tree a -> m (Maybe b)
findrWithPathM f p@(Path (idx:_)) = findrWithPathM' f (Path [idx]) p
findrWithPathM f p = findrWithPathM' f p p

instance Monoid Path where
  mempty = Path []
  mappend = (<>)

instance Semigroup Path where
  (<>) (Path a) (Path b) = Path $ a ++ b
