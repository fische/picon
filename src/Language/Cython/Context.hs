{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context where

import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Data
import Control.Monad.State
import Language.Cython.Annotation

data Var =
  Local { ctype :: CythonType } |
  NonLocal { ctype :: CythonType } |
  Global { ctype :: CythonType }
  deriving (Eq,Ord,Show,Typeable,Data)

data Context =
  Context {
    inGlobalScope :: Bool,
    outerVars :: Map.Map String CythonType,
    localVars :: Map.Map String CythonType
  }
  deriving (Eq,Ord,Show,Typeable,Data)

emptyContext :: Context
emptyContext = Context {
  inGlobalScope = False,
  outerVars = Map.empty,
  localVars = Map.empty
}

getVarType :: String -> State Context CythonType
getVarType ident = do
  ctx <- get
  return (Map.findWithDefault
    (Map.findWithDefault Unknown ident (outerVars ctx)) ident (localVars ctx))

insertVar :: String -> CythonType -> State Context ()
insertVar ident typ = do
  ctx <- get
  put (ctx{ localVars = Map.insert ident typ (localVars ctx) })
  return ()

-- TODO Compare CTypes, change if needed and return it
assignVar :: String -> CythonType -> State Context Bool
assignVar ident typ = do
  ctx <- get
  let changeType _ old Unknown = old
      changeType _ _ new = new
      (oldValue, newscope) =
        Map.insertLookupWithKey changeType ident typ (localVars ctx)
  put (ctx{ localVars = newscope })
  return (isNothing oldValue)

newScope :: State Context Context
newScope = do
  ctx <- get
  let outer = Map.union (localVars ctx) (outerVars ctx)
  return (ctx{inGlobalScope = False, outerVars = outer, localVars = Map.empty})
