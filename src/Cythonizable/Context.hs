module Cythonizable.Context (
  Context(..),
  Cythonizable.Context.fromAnalysis,
  Cythonizable.Context.getLocalVariableType,
  Cythonizable.Context.getLocalVariables,
  Cythonizable.Context.getFunctionReturnType,
  Cythonizable.Context.dropNextScope
) where

import qualified Data.Map.Strict as Map

import Control.Monad.State

import qualified Analyzable.Context as Context

import Language.Cython.Type

import Scope

data Context =
  Context {
    globalScope :: Scope,
    currentScope :: Scope
  }

fromAnalysis :: Context.Context -> Context
fromAnalysis ctx = Context {
  globalScope = Context.scope ctx,
  currentScope = Context.scope ctx
}

getLocalVariableType :: String -> State Context CythonType
getLocalVariableType ident = do
  ctx <- get
  return .
    Scope.getLocalVariableType (globalScope ctx) ident $
    currentScope ctx

getLocalVariables :: State Context (Map.Map String CythonType)
getLocalVariables = do
  ctx <- get
  return . Scope.getLocalVariables (globalScope ctx) $ currentScope ctx

getFunctionReturnType :: State Context CythonType
getFunctionReturnType = do
  ctx <- get
  return . Scope.getFunctionReturnType (globalScope ctx) $ currentScope ctx

dropNextScope :: String -> State Context Context
dropNextScope i = do
  ctx <- get
  let (newScope, fun) = Scope.dropNextScope i (currentScope ctx)
  put ctx{
    currentScope = newScope
  }
  return ctx{
    currentScope = fun
  }
