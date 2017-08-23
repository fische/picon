module Cythonizable.Context (
  Context(..),
  Cythonizable.Context.fromAnalysis,
  Cythonizable.Context.getLocalVariableType,
  Cythonizable.Context.getLocalVariables,
  Cythonizable.Context.getFunctionReturnType,
  Cythonizable.Context.dropNextScope
) where

import qualified Data.Map.Strict as Map

import Control.Monad.State as State

import qualified Analyzable.Context as Context
import Cythonizable.Scope as Scope

import Language.Cython.Type

-- | Context represents the scope context of a python program.
data Context =
  Context {
    -- | globalScope contains the whole scope tree of the python program.
    globalScope :: Scope,
    -- | currentScope contains the current scope.
    currentScope :: Scope
  }

-- | fromAnalysis creates a new `Context` from the `Context.Context` returned
-- by the analysis.
fromAnalysis :: Context.Context -> Context
fromAnalysis ctx = Context {
  globalScope = Context.scope ctx,
  currentScope = Context.scope ctx
}

-- | getLocalVariableType returns cython type of given variable identifier in
-- current scope.
getLocalVariableType :: String -> State Context CythonType
getLocalVariableType ident = do
  ctx <- State.get
  return .
    Scope.getLocalVariableType (globalScope ctx) ident $
    currentScope ctx

-- | getLocalVariables returns cython type of local variables declared in
-- current scope.
getLocalVariables :: State Context (Map.Map String CythonType)
getLocalVariables = do
  ctx <- State.get
  return . Scope.getLocalVariables (globalScope ctx) $ currentScope ctx

-- | getFunctionReturnType return complete return type of current function
-- scope.
getFunctionReturnType :: State Context CythonType
getFunctionReturnType = do
  ctx <- State.get
  return . Scope.getFunctionReturnType (globalScope ctx) $ currentScope ctx

-- | dropNextScope drops next scope with given identifier from context state
-- and returns context with dropped scope.
dropNextScope :: String -> State Context Context
dropNextScope i = do
  ctx <- State.get
  let (newScope, fun) = Scope.dropNextScope i (currentScope ctx)
  put ctx{
    currentScope = newScope
  }
  return ctx{
    currentScope = fun
  }
