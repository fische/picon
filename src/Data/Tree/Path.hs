module Data.Tree.Path (
  Path(..),
  update,
  updateM,
  get
) where

import           Data.Semigroup
import           Data.Tree

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

instance Monoid Path where
  mempty = Path []
  mappend = (<>)

instance Semigroup Path where
  (<>) (Path a) (Path b) = Path $ a ++ b
