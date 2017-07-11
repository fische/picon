{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Maybe (isNothing, fromJust, maybe)
import Data.Data
import Language.Cython.Annotation

type ContextState = ExceptT String (State Context)

data Var =
  Local { cytype :: CythonType } |
  NonLocal { cytype :: CythonType } |
  Global { cytype :: CythonType }
  deriving (Eq,Ord,Show,Typeable,Data)

isLocal :: Var -> Bool
isLocal (Local _) = True
isLocal _ = False

isNonLocal :: Var -> Bool
isNonLocal (NonLocal _) = True
isNonLocal _ = False

isGlobal :: Var -> Bool
isGlobal (Global _) = True
isGlobal _ = False

data Context =
  Context {
    inGlobalScope :: Bool,
    globalVars :: Map.Map String CythonType,
    outerVars :: Map.Map String CythonType,
    localVars :: Map.Map String Var
  }
  deriving (Eq,Ord,Show,Typeable,Data)

emptyContext :: Context
emptyContext = Context {
  inGlobalScope = False,
  globalVars = Map.empty,
  outerVars = Map.empty,
  localVars = Map.empty
}

getVarType :: String -> ContextState CythonType
getVarType ident = do
  ctx <- get
  let err = "Identifier " ++ ident ++ " was not found"
      global = maybe (throwE err) return $ Map.lookup ident (globalVars ctx)
      outer = maybe global return $ Map.lookup ident (outerVars ctx)
  maybe outer (return . cytype) $ Map.lookup ident (localVars ctx)

insertVar :: String -> CythonType -> ContextState ()
insertVar ident typ = do
  ctx <- get
  put (ctx{ localVars = Map.insert ident (Local typ) (localVars ctx) })
  return ()

-- TODO Compare CTypes, change if needed and return it
mergeCythonType :: CythonType -> CythonType -> CythonType
mergeCythonType old new
  | new == Unknown = old
  | old == Unknown = old
  | new == old = old
  | otherwise = PythonObject

mergeVarType :: Var -> Var -> Var
mergeVarType old new = old{cytype = mergeCythonType (cytype old) (cytype new)}

assignVar :: String -> CythonType -> ContextState Bool
assignVar ident typ = do
  ctx <- get
  let insertIdent f = Map.insertLookupWithKey (const f) ident
      (rctx, def) = if inGlobalScope ctx
                      then
                        let scope = globalVars ctx
                            (old, vars) = insertIdent mergeCythonType typ scope
                        in (ctx{globalVars = vars}, isNothing old)
                      else
                        let scope = localVars ctx
                            var = Local typ
                            (old, vars) = insertIdent mergeVarType var scope
                        in (ctx{localVars = vars}, isNothing old)
  put rctx
  return def

bindGlobalVars' :: Context -> [String] -> ContextState Context
bindGlobalVars' ctx [] = return ctx
bindGlobalVars' ctx (ident:tl) = do
  let lVars = localVars ctx
      local = Map.lookup ident lVars
  if isNothing local
    then do
      let gVars = globalVars ctx
          typ = Map.findWithDefault Unknown ident gVars
      newCtx <- bindGlobalVars' (ctx{
        localVars = Map.insert ident (Global typ) lVars
      }) tl
      return newCtx
    else
      throwE $
        "Variable " ++ (show ident) ++ " has already been declared or bound"


bindGlobalVars :: [String] -> ContextState ()
bindGlobalVars idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ "Global bindings from the global scope are not allowed"
    else do
      newCtx <- bindGlobalVars' ctx idents
      put newCtx
      return ()

bindNonLocalVars' :: Context -> [String] -> ContextState Context
bindNonLocalVars' ctx [] = return ctx
bindNonLocalVars' ctx (ident:tl) = do
  let lVars = localVars ctx
      local = Map.lookup ident lVars
  if isNothing local
    then do
      let outer = Map.lookup ident (outerVars ctx)
      if isNothing outer
        then
          throwE $ "Variable " ++ (show ident) ++
            " has not been declared in an outer scope"
        else do
          newCtx <- bindNonLocalVars' (ctx{
            localVars = Map.insert ident (NonLocal $ fromJust outer) lVars
          }) tl
          return newCtx
    else
      throwE $
        "Variable " ++ (show ident) ++ " has already been declared or bound"


bindNonLocalVars :: [String] -> ContextState ()
bindNonLocalVars idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ "Nonlocal bindings from the global scope are not allowed"
    else do
      newCtx <- bindNonLocalVars' ctx idents
      put newCtx
      return ()

mergeLocalVars :: Context -> Context
mergeLocalVars ctx =
  let boundVars = Map.filter (not . isLocal) (localVars ctx)
      (boundGlobalVars, boundNonLocalVars) = Map.partition isGlobal boundVars
      boundNonLocals = fmap cytype boundNonLocalVars
      newOuter = Map.unionWith mergeCythonType (outerVars ctx) boundNonLocals
      boundGlobals = fmap cytype boundGlobalVars
      newGlobal = Map.unionWith mergeCythonType (globalVars ctx) boundGlobals
  in ctx{
    globalVars = newGlobal,
    outerVars = newOuter,
    localVars = Map.empty
  }

mergeCopiedContext :: Context -> ContextState Context
mergeCopiedContext copied = do
  ctx <- get
  let rctx = if inGlobalScope ctx
      then
        let copiedVars = globalVars copied
            currVars = globalVars ctx
        in ctx{
          globalVars = Map.intersectionWith mergeCythonType currVars copiedVars
        }
      else
        let merged = mergeLocalVars ctx
            copiedVars = localVars copied
            currVars = localVars ctx
        in merged{
          localVars = Map.intersectionWith mergeVarType currVars copiedVars
        }
  put rctx
  return rctx

mergeInnerContext :: Context -> ContextState Context
mergeInnerContext inner = do
  ctx <- get
  let mergedInner = mergeLocalVars inner
      mergedGlobals = globalVars mergedInner
      mergedOuter = outerVars mergedInner
      currLocals = localVars ctx
      (nonGlobals, boundGlobals) = Map.partition (not . isGlobal) currLocals
      intersectVarType old new = old{cytype = mergeCythonType (cytype old) new}
      newNonGlobals =
        Map.intersectionWith intersectVarType nonGlobals mergedOuter
      newBoundGlobals =
        Map.intersectionWith intersectVarType boundGlobals mergedGlobals
      newOuter = Map.difference mergedOuter (Map.filter isLocal newNonGlobals)
      rctx = ctx{
        globalVars = mergedGlobals,
        outerVars = Map.union newOuter (outerVars ctx),
        localVars = Map.union newNonGlobals newBoundGlobals
      }
  put rctx
  return rctx

copyContext :: ContextState Context
copyContext = do
  ctx <- get
  return ctx

openNewContext :: ContextState Context
openNewContext = do
  ctx <- get
  let locals = Map.filter isLocal (localVars ctx)
      outer = Map.union (fmap cytype locals) (outerVars ctx)
  return (ctx{inGlobalScope = False, outerVars = outer, localVars = Map.empty})
