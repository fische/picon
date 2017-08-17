module Cythonizable.Context (
  Context(..),
  Cythonizable.Context.fromAnalysis,
  Cythonizable.Context.getLocalVariables,
  Cythonizable.Context.getFunctionReturnType,
  Cythonizable.Context.dropNextFunction
) where

import qualified Data.Map.Strict as Map

import Control.Monad.State

import qualified Analyzable.Context as Context

import Language.Cython.Type

import Scope

data Context =
  Context {
    scope :: Scope
  }

fromAnalysis :: Context.Context -> Context
fromAnalysis ctx = Context {
  scope = Context.scope ctx
}

getLocalVariables :: State Context (Map.Map String CythonType)
getLocalVariables = do
  ctx <- get
  return . Scope.getLocalVariables $ scope ctx

getFunctionReturnType :: State Context CythonType
getFunctionReturnType = do
  ctx <- get
  return . Scope.getFunctionReturnType $ scope ctx

dropNextFunction :: String -> State Context Context
dropNextFunction i = do
  ctx <- get
  let (newScope, fun) = Scope.dropNextFunction i (scope ctx)
  put ctx{
    scope = newScope
  }
  return ctx{
    scope = fun
  }
