{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Context where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Data

import Control.Monad.State
import Control.Monad.Trans.Except

import Language.Cython.Annotation
import Language.Cython.Error

type ContextState annot = ExceptT (Error annot) (State Context)

-- TODO NonLocal and Global should hold a reference instead of a copy
data Binding =
  Local { cytype :: [Type] } |
  NonLocal { cytype :: [Type] } |
  Global { cytype :: [Type] }
  deriving (Eq,Ord,Show,Typeable,Data)

isLocal :: Binding -> Bool
isLocal (Local _) = True
isLocal _ = False

isNonLocal :: Binding -> Bool
isNonLocal (NonLocal _) = True
isNonLocal _ = False

isGlobal :: Binding -> Bool
isGlobal (Global _) = True
isGlobal _ = False

data Context =
  Context {
    inGlobalScope :: Bool,
    globalVars :: Map.Map String [Type],
    outerVars :: Map.Map String [Type],
    localVars :: Map.Map String Binding
  }
  deriving (Eq,Ord,Show,Typeable,Data)

empty :: Context
empty = Context {
  inGlobalScope = False,
  globalVars = Map.empty,
  outerVars = Map.empty,
  localVars = Map.empty
}

copy :: ContextState annot Context
copy = do
  ctx <- get
  return ctx

openScope :: ContextState annot Context
openScope = do
  ctx <- get
  let (globals, outers) = Map.partition isGlobal (localVars ctx)
      updatedGlobal = Map.union (fmap cytype globals) (globalVars ctx)
      updatedOuter = Map.union (fmap cytype outers) (outerVars ctx)
  return (ctx{
    inGlobalScope = False,
    globalVars = updatedGlobal,
    outerVars = updatedOuter,
    localVars = Map.empty
  })

openModule :: ContextState annot Context
openModule = do
  ctx <- get
  return (ctx{
    inGlobalScope = True,
    globalVars = Map.empty,
    outerVars = Map.empty,
    localVars = Map.empty
  })

addVarType :: String -> Type -> ContextState annot ()
addVarType ident annot = do
  ctx <- get
  let insert _ old = (annot : old)
      insertBinding new old = old{ cytype = insert (cytype new) (cytype old) }
  put (if inGlobalScope ctx
    then
      let newGlobals = Map.insertWith insert ident [annot] (globalVars ctx)
      in ctx{globalVars = newGlobals}
    else
      let bound = Local [annot]
          oldLocals = localVars ctx
          newLocals = Map.insertWith insertBinding ident bound oldLocals
      in ctx{localVars = newLocals})

bindGlobalVars' :: annot -> Context -> [String] -> ContextState annot Context
bindGlobalVars' _ ctx [] = return ctx
bindGlobalVars' loc ctx (ident:tl) =
  let locals = localVars ctx
      found = Map.lookup ident locals
      -- TODO Find a more efficient way to insert the variable
      insert local =
        let globals = globalVars ctx
            typ = local ++ (Map.findWithDefault [] ident globals)
        in bindGlobalVars' loc (ctx{
          localVars = Map.insert ident (Global typ) locals
        }) tl
  in maybe
    (insert [])
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isNonLocal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          bindGlobalVars' loc ctx tl)
    found

bindGlobalVars :: annot -> [String] -> ContextState annot ()
bindGlobalVars loc idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ errNotAllowedInGlobalScope loc "Global bindings"
    else do
      newCtx <- bindGlobalVars' loc ctx idents
      put newCtx
      return ()

bindNonLocalVars' :: annot -> Context -> [String] -> ContextState annot Context
bindNonLocalVars' _ ctx [] = return ctx
bindNonLocalVars' loc ctx (ident:tl) =
  let locals = localVars ctx
      found = Map.lookup ident locals
      checkAndInsert local =
        let outers = outerVars ctx
            outer = Map.lookup ident outers
        in maybe
          (throwE $ errVarNotFound loc ident)
          (\var ->
            let typ = local ++ var
            in bindNonLocalVars' loc (ctx{
              localVars = Map.insert ident (NonLocal typ) locals
            }) tl)
          outer
  in maybe
    (checkAndInsert [])
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isGlobal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          bindNonLocalVars' loc ctx tl)
    found


bindNonLocalVars :: annot -> [String] -> ContextState annot ()
bindNonLocalVars loc idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ errNotAllowedInGlobalScope loc "Nonlocal bindings"
    else do
      newCtx <- bindNonLocalVars' loc ctx idents
      put newCtx
      return ()

updateLocalBindings :: Context -> Context
updateLocalBindings ctx =
  let (locals, bound) = Map.partition isLocal (localVars ctx)
      (globals, outers) = Map.partition isGlobal bound
      mergeBinding old new = old{ cytype = new }
      newBoundGlobals =
        Map.intersectionWith mergeBinding globals (globalVars ctx)
      newBoundOuters =
        Map.intersectionWith mergeBinding outers (outerVars ctx)
  in ctx{
    localVars = Map.unions [locals, newBoundGlobals, newBoundOuters]
  }

merge :: Map.Map String Binding -> Context -> ContextState annot ()
merge toMerge innerCtx = do
  currCtx <- get
  let innerBound = Map.filter (not . isLocal) toMerge
      (innerBoundGlobals, innerBoundOuters) = Map.partition isGlobal innerBound
      innerOuters =
        Map.union (fmap cytype innerBoundOuters) (outerVars innerCtx)

      currLocals = Map.filter isLocal (localVars currCtx)
      nonLocalOuters = Map.difference innerOuters currLocals

      newGlobals =
        Map.union (fmap cytype innerBoundGlobals) (globalVars innerCtx)
      newOuters =
        Map.union nonLocalOuters (outerVars currCtx)

  put $ updateLocalBindings currCtx{
    outerVars = newOuters,
    globalVars = newGlobals
  }

-- TODO Handle cycling referencing
resolveRef' :: Map.Map String Binding -> String -> [Type] ->
  State (Map.Map String [Type]) [Type]
resolveRef' _ _ [] = return []
resolveRef' toResolve k (hd@(Ref ident):tl)
  | maybe False isLocal var = do
    st <- get
    let resolved = Map.lookup ident st
        varToResolve = cytype $ fromJust var
    newHead <- maybe (resolveRef toResolve ident varToResolve) return resolved
    newTail <- resolveRef' toResolve k tl
    return (newHead ++ newTail)
  | otherwise = do
    newTail <- resolveRef' toResolve k tl
    return (hd:newTail)
  where var = Map.lookup ident toResolve
resolveRef' toResolve k (hd:tl) = do
  newTail <- resolveRef' toResolve k tl
  return (hd:newTail)

resolveRef :: Map.Map String Binding -> String -> [Type] ->
  State (Map.Map String [Type]) [Type]
resolveRef toResolve k v = do
  resolved <- resolveRef' toResolve k v
  st <- get
  put (Map.insert k resolved st)
  return resolved

resolve :: Map.Map String Binding -> Map.Map String Binding ->
  Map.Map String Binding
resolve toResolve scope =
  let resolveBinding k v = do
        resolved <- resolveRef toResolve k (cytype v)
        return v{ cytype = resolved }
  in evalState (mapWithKey resolveBinding scope) Map.empty

mergeCopy :: Context -> ContextState annot (Map.Map String [Type])
mergeCopy innerCtx = do
  currCtx <- get
  let resolveScope currScope innerScope =
        let inCurrCtx k _ = Map.member k currScope
            (outers, locals) =
              Map.partitionWithKey inCurrCtx innerScope
            resolvedLocals = resolve locals locals
            resolvedOuters = resolve resolvedLocals outers
        in (resolvedLocals, resolvedOuters)
  resolved <- (if inGlobalScope innerCtx
                then do
                  let (locals, outers) = resolveScope (globalVars currCtx)
                        (Local <$> globalVars innerCtx)
                  put currCtx{
                    globalVars = cytype <$> outers
                  }
                  return locals
                else do
                  let (locals, outers) = resolveScope (localVars currCtx)
                        (localVars innerCtx)
                  merge locals innerCtx{
                    localVars = Map.union locals outers
                  }
                  return locals)
  return . fmap cytype $ Map.filter isLocal resolved

mergeScope :: Context -> ContextState annot (Map.Map String [Type])
mergeScope innerCtx = do
  let resolved = resolve (localVars innerCtx) (localVars innerCtx)
  merge resolved innerCtx{
    localVars = resolved
  }
  return . fmap cytype $ Map.filter isLocal resolved

mergeModule :: Context -> ContextState annot (Map.Map String [Type])
mergeModule innerCtx =
  let globals = Local <$> globalVars innerCtx
      resolved = resolve globals globals
  in return . fmap cytype $ Map.filter isLocal resolved

mapWithKey' :: (Monad m) => (k -> a -> m b) -> [(k, a)] -> m [(k, b)]
mapWithKey' _ [] = return []
mapWithKey' f ((k, v):tl) = do
  newValue <- f k v
  newTail <- mapWithKey' f tl
  return ((k, newValue) : newTail)

mapWithKey :: (Ord k, Monad m) => (k -> a -> m b) -> Map.Map k a ->
  m (Map.Map k b)
mapWithKey f m = do
  result <- mapWithKey' f $ Map.toList m
  return (Map.fromList result)
