{-# LANGUAGE DeriveDataTypeable #-}

module Cythonizing.Context (
  Context(..),
  newContext,
  Cythonizing.Context.openBlock,
  Cythonizing.Context.openFunction,
  openModule,
  Cythonizing.Context.stash,
  Cythonizing.Context.unstash
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Control.Monad.State (get, put)
import Control.Monad.Trans.Except

import Language.Cython.Type
import Cythonizing.Scope as Scope
import Options
import State
import Type

data Context =
  Context {
    options :: Options,
    scope :: Scope
  }
  deriving (Eq,Ord,Show,Typeable,Data)

newContext :: Context
newContext = Context {
  options = Options{},
  scope = newScope
}

openBlock :: State Context annot Context
openBlock = do
  ctx <- get
  return ctx{
    scope = Scope.openBlock (scope ctx)
  }

openFunction :: State Context annot Context
openFunction = do
  ctx <- Cythonizing.Context.openBlock
  return ctx{
    scope = Scope.openFunction (scope ctx)
  }

openModule :: State Context annot Context
openModule = do
  ctx <- get
  return ctx{
    scope = newScope
  }

stash :: annot -> Map.Map String [Type] -> State Context annot ()
stash loc locals = do
  ctx <- get
  let result = runExcept $ Scope.stash loc (scope ctx) locals
  either throwE (\s -> put ctx{scope = s}) result

unstash :: annot -> String -> State Context annot [CythonType]
unstash loc ident = do
  ctx <- get
  either
    throwE
    (\(s, types) -> do
      put ctx{
        scope = s
      }
      return types)
    (runExcept $ Scope.unstash loc (scope ctx) ident)
