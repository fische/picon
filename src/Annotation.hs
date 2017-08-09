{-# LANGUAGE DeriveDataTypeable #-}

module Annotation (
  CythonAnnotation(..)
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Type

data CythonAnnotation =
  Locals (Map.Map String [Type])
  deriving (Eq,Ord,Show,Typeable,Data)
