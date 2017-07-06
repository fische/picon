{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context where

import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, maybe)
import Data.Data
import Control.Monad.State
import Language.Cython.Annotation

data Var =
  Local { cytype :: CythonType } |
  NonLocal { cytype :: CythonType } |
  Global { cytype :: CythonType }
  deriving (Eq,Ord,Show,Typeable,Data)

data Context =
  Context {
    inGlobalScope :: Bool,
    globalVars :: Map.Map String CythonType,
    outerVars :: Map.Map String Var,
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

getVarType :: String -> State Context CythonType
getVarType ident = do
  ctx <- get
  -- TODO Handle default value as exception
  return (cytype $ Map.findWithDefault
    (Map.findWithDefault (Global $
      Map.findWithDefault Unknown ident (globalVars ctx))
      ident (outerVars ctx))
    ident (localVars ctx))

insertVar :: String -> CythonType -> State Context ()
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

assignVar :: String -> CythonType -> State Context Bool
assignVar ident typ = do
  ctx <- get
  let insertIdent f v s = Map.insertLookupWithKey (const f) ident v s
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
  let lVars = localVars ctx
      gVars = globalVars ctx
      local = Map.lookup ident lVars
      glob = Map.findWithDefault Unknown ident gVars
      dfltTyp = mergeCythonType glob Unknown
      typ = maybe dfltTyp (\l -> mergeCythonType glob (cytype l)) local
  in bindGlobalVars' (ctx{
    globalVars = Map.insert ident typ gVars,
    localVars = Map.insert ident (Global typ) lVars
  }) tl


bindGlobalVars :: [String] -> State Context ()
bindGlobalVars idents = do
  ctx <- get
  put (bindGlobalVars' ctx idents)
  return ()

bindNonLocalVars' :: Context -> [String] -> Context
bindNonLocalVars' ctx [] = ctx
bindNonLocalVars' ctx (ident:tl) =
  -- TODO Raise exception when var not in outer
  let lVars = localVars ctx
      oVars = outerVars ctx
      local = Map.findWithDefault (Local Unknown) ident lVars
      outer = Map.lookup ident oVars
      rctx = maybe ctx
        (\oVar -> let typ = mergeCythonType (cytype oVar) (cytype local)
                  in ctx{
                    outerVars = Map.insert ident (oVar{cytype = typ}) oVars,
                    localVars = Map.insert ident (NonLocal typ) lVars
                  })
        outer
  in bindNonLocalVars' rctx tl


bindNonLocalVars :: [String] -> State Context ()
bindNonLocalVars idents = do
  ctx <- get
  put (bindNonLocalVars' ctx idents)
  return ()

copyScope :: State Context Context
copyScope = do
  ctx <- get
  return (ctx{inGlobalScope = False})

newScope :: State Context Context
newScope = do
  ctx <- get
  let outer = Map.union (localVars ctx) (outerVars ctx)
  return (ctx{inGlobalScope = False, outerVars = outer, localVars = Map.empty})
