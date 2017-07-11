{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Maybe (isNothing, maybe)
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
  -- TODO Handle default value as exception
  let local = Map.lookup ident (localVars ctx)
  return (maybe (Map.findWithDefault
    (Map.findWithDefault Unknown ident (globalVars ctx))
    ident (outerVars ctx)) cytype local)

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

bindGlobalVars' :: Context -> [String] -> Context
bindGlobalVars' ctx [] = ctx
bindGlobalVars' ctx (ident:tl) =
  -- TODO Raise exception when a local var exists
  let lVars = localVars ctx
      gVars = globalVars ctx
      local = Map.lookup ident lVars
      glob = Map.findWithDefault Unknown ident gVars
      dfltTyp = mergeCythonType glob Unknown
      typ = maybe dfltTyp (mergeCythonType glob . cytype) local
  in bindGlobalVars' (ctx{
    localVars = Map.insert ident (Global typ) lVars
  }) tl


bindGlobalVars :: [String] -> ContextState ()
bindGlobalVars idents = do
  ctx <- get
  put (bindGlobalVars' ctx idents)
  return ()

bindNonLocalVars' :: Context -> [String] -> Context
bindNonLocalVars' ctx [] = ctx
bindNonLocalVars' ctx (ident:tl) =
  -- TODO Raise exception when var not in outer
  -- TODO Raise exception when a local var exists
  let lVars = localVars ctx
      local = Map.findWithDefault (Local Unknown) ident lVars
      outer = Map.lookup ident (outerVars ctx)
      rctx = maybe ctx
        (\outerType -> let typ = mergeCythonType outerType (cytype local)
          in ctx{
            localVars = Map.insert ident (NonLocal typ) lVars
          })
        outer
  in bindNonLocalVars' rctx tl


bindNonLocalVars :: [String] -> ContextState ()
bindNonLocalVars idents = do
  ctx <- get
  put (bindNonLocalVars' ctx idents)
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
