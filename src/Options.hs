{-# LANGUAGE DeriveDataTypeable #-}

module Options (
  Options(..)
) where

import Data.Data

data Options =
  Options {}
  deriving (Eq,Ord,Show,Typeable,Data)
