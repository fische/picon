module Cythonizable.Context (
  Context(..),
  Cythonizable.Context.fromAnalysis,
  Cythonizable.Context.getLocalVariables,
  Cythonizable.Context.getFunctionReturnType,
  Cythonizable.Context.dropNextFunction,
  Cythonizable.Context.getFunctionName
) where

import qualified Data.Map.Strict as Map

import Control.Monad.State

import qualified Analyzable.Context as Context

import Language.Cython.Type

import Scope

data Context =
  Context {
    typedefs :: Map.Map String CythonType,
    scope :: Scope
  }

fromAnalysis :: Context.Context -> Context
fromAnalysis ctx = Context {
  typedefs = Map.empty,
  scope = Context.scope ctx
}

getLocalVariables :: State Context (Map.Map String CythonType)
getLocalVariables = do
  ctx <- get
  let (vars, newTypedefs) =
        runState (Scope.getLocalVariables $ scope ctx) $ typedefs ctx
  put ctx{
    typedefs = newTypedefs
  }
  return vars

getFunctionReturnType :: State Context CythonType
getFunctionReturnType = do
  ctx <- get
  let (ret, newTypedefs) =
        runState (Scope.getFunctionReturnType $ scope ctx) $ typedefs ctx
  put ctx{
    typedefs = newTypedefs
  }
  return ret

getFunctionName :: Context -> String
getFunctionName ctx = getMangledFunctionIdent . path $ scope ctx

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
