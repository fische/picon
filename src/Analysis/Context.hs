{-# LANGUAGE DeriveDataTypeable #-}

module Analysis.Context (
  Context(..),
  newContext,
  Analysis.Context.getIdentRef,
  Analysis.Context.addVarType,
  bindNonLocalVars,
  bindGlobalVars,
  Analysis.Context.openBlock,
  Analysis.Context.closeBlock,
  Analysis.Context.openFunction,
  Analysis.Context.closeFunction,
  Analysis.Context.openModule,
  Analysis.Context.closeModule
) where

import qualified Data.Map.Strict as Map
import Data.Data

import Control.Monad.State (get, put)
import Control.Monad.Trans.Except

import Language.Cython.Error

import State

import Analysis.Scope as Scope
import Type
import Options

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


-- Insert/Get/Update variable type
getIdentRef :: String -> State Context annot Ref
getIdentRef ident = do
  ctx <- get
  return $ Scope.getIdentRef (scope ctx) ident

addVarType :: String -> Type -> State Context annot ()
addVarType ident typ = do
  ctx <- get
  put $ ctx{
    scope = Scope.addVarType (scope ctx) ident typ
  }

bindVars :: (Scope -> String -> Except (Error annot) Scope) -> [String] ->
  Scope -> Except (Error annot) Scope
bindVars _ [] s = return s
bindVars f (ident:tl) s =
  let result = runExcept $ f s ident
  in either throwE (bindVars f tl) result

bindGlobalVars :: annot -> [String] -> State Context annot ()
bindGlobalVars loc idents = do
  ctx <- get
  let currScope = scope ctx
  if inGlobalScope currScope
    then
      throwE $ errNotAllowedInGlobalScope loc "Global bindings"
    else
      let result = runExcept $ bindVars (bindGlobalVar loc) idents currScope
      in either throwE (\s -> put ctx{scope = s}) result

bindNonLocalVars :: annot -> [String] -> State Context annot ()
bindNonLocalVars loc idents = do
  ctx <- get
  let currScope = scope ctx
  if inGlobalScope currScope
    then
      throwE $ errNotAllowedInGlobalScope loc "Nonlocal bindings"
    else
      let result = runExcept $ bindVars (bindNonLocalVar loc) idents currScope
      in either throwE (\s -> put ctx{scope = s}) result


-- Operations between scopes
openBlock :: State Context annot Context
openBlock = do
  ctx <- get
  return ctx{
    scope = Scope.openBlock (scope ctx)
  }

openFunction :: State Context annot Context
openFunction = do
  ctx <- Analysis.Context.openBlock
  let currScope = scope ctx
      mergedScope = fst $ mergeLocalScope currScope (localScope currScope)
  put ctx{
    scope = mergedScope
  }
  return ctx{
    scope = Scope.openFunction mergedScope
  }

openModule :: State Context annot Context
openModule = do
  ctx <- get
  return (ctx{
    scope = Scope.openModule (scope ctx)
  })

closeBlock :: Context -> State Context annot (Map.Map String [Type])
closeBlock innerCtx = do
  currCtx <- get
  let (newCurrScope, innerLocals) =
        Scope.closeBlock (scope currCtx) (scope innerCtx)
  put currCtx{
    scope = newCurrScope
  }
  return innerLocals

closeFunction :: Context ->
  State Context annot (Map.Map String [Type])
closeFunction innerCtx = do
  currCtx <- get
  let (newCurrScope, innerLocals) =
        Scope.closeFunction (scope currCtx) (scope innerCtx)
  put currCtx{
    scope = newCurrScope
  }
  return innerLocals

closeModule :: Context -> State Context annot (Map.Map String [Type])
closeModule innerCtx = return $ Scope.closeModule (scope innerCtx)
