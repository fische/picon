{-# LANGUAGE DeriveDataTypeable #-}

module Analysis.Scope (
  Scope(..),
  newScope,
  getIdentRef,
  addVarType,
  bindGlobalVar,
  bindNonLocalVar,
  mergeLocalScope,
  openBlock,
  closeBlock,
  openFunction,
  closeFunction,
  openModule,
  closeModule
) where

import qualified Data.Map.Strict as Map
import Data.Data
import Data.Bool

import Control.Monad.State
import Control.Monad.Trans.Except
import Monadic.Map

import Language.Cython.Error
import Analysis.Binding
import Type

data Scope =
  Scope {
    inGlobalScope :: Bool,
    globalScope :: Map.Map String [Type],
    outerScope :: Map.Map String [Type],
    localScope :: Map.Map String Binding
  }
  deriving (Eq,Ord,Show,Typeable,Data)

newScope :: Scope
newScope = Scope {
  inGlobalScope = False,
  globalScope = Map.empty,
  outerScope = Map.empty,
  localScope = Map.empty
}

inScope :: Map.Map String a -> String -> b -> Bool
inScope scope ident _ = Map.member ident scope


-- Insert/Get/Update variable type
getIdentRef :: Scope -> String -> Ref
getIdentRef scope ident =
  let inLocalScope = Map.lookup ident (localScope scope)
      inOuterScope = Map.member ident (outerScope scope)
  in maybe
    (bool (GlobalRef ident) (NonLocalRef ident) inOuterScope)
    (getBindingRef ident)
    inLocalScope

addVarType :: Scope -> String -> Type -> Scope
addVarType scope ident typ =
  let insertBinding _ old = old{ cytype = (typ : (cytype old)) }
      binding = Local [typ]
      oldLocals = localScope scope
      newLocals = Map.insertWith insertBinding ident binding oldLocals
  in scope{localScope = newLocals}

bindGlobalVar :: annot -> Scope -> String -> Except (Error annot) Scope
bindGlobalVar loc scope ident =
  let locals = localScope scope
      found = Map.lookup ident locals
  in maybe
    (return scope{
      localScope = Map.insert ident (Global []) locals
    })
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isNonLocal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          return scope)
    found

bindNonLocalVar :: annot -> Scope -> String -> Except (Error annot) Scope
bindNonLocalVar loc scope ident =
  let locals = localScope scope
      found = Map.lookup ident locals
      checkAndInsert =
        let outer = Map.member ident (outerScope scope)
        in bool
          (throwE $ errVarNotFound loc ident)
          (return scope{
              localScope = Map.insert ident (NonLocal []) locals
          })
          outer
  in maybe
    checkAndInsert
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isGlobal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          return scope)
    found


-- Operations between scopes
mergeLocalScope :: Scope -> Map.Map String Binding ->
  (Scope, Map.Map String [Type])
mergeLocalScope scope localVars =
  let (locals, bindings) = Map.partition isLocal localVars
      (boundGlobals, boundNonLocals) = Map.partition isGlobal bindings

      newOuters =
        Map.unionWith (++) (fmap cytype boundNonLocals) (outerScope scope)
      newGlobals =
        Map.unionWith (++) (fmap cytype boundGlobals) (globalScope scope)

      -- Reset updates to bindings
      resetBindings = Map.map (\l -> l{cytype = []}) bindings

      -- Resolve references to variables from the local scope
      resolve ident binding = do
        resolvedTypes <- resolveRefs
          (fmap cytype . getLocalRefTypes locals) ident $ cytype binding
        st <- get
        put $ Map.insert (LocalRef ident) resolvedTypes st
        return resolvedTypes
      resolvedLocals = evalState (mapWithKey resolve locals) Map.empty
  in (scope{
    localScope = Map.union locals resetBindings,
    outerScope = newOuters,
    globalScope = newGlobals
  }, resolvedLocals)

updateToLocalRefs' :: (Ref -> Bool) -> (String -> Bool) -> [Type] -> [Type]
updateToLocalRefs' _ _ [] = []
updateToLocalRefs' isRightRefType isCurrLocalVar (hd@(Ref ref):tl)
  | changeToLocalRef =
      (Ref $ LocalRef refIdent):(updateToLocalRefs' isRightRefType isCurrLocalVar tl)
  | otherwise = hd:(updateToLocalRefs' isRightRefType isCurrLocalVar tl)
  where refIdent = getRefIdentifier ref
        changeToLocalRef = isRightRefType ref && isCurrLocalVar refIdent
updateToLocalRefs' isRightRefType isCurrLocalVar (hd:tl) =
  hd:(updateToLocalRefs' isRightRefType isCurrLocalVar tl)

updateToLocalRefs :: (Ref -> Bool) -> (String -> Bool) -> Scope -> Scope
updateToLocalRefs isRightRefType isCurrLocalVar scope =
  let update = updateToLocalRefs' isRightRefType isCurrLocalVar
      globals = Map.map update (globalScope scope)
      outers = Map.map update (outerScope scope)
  in scope{
    globalScope = globals,
    outerScope = outers
  }

resolveLocalRefs :: Map.Map String [Type] -> Scope -> Scope
resolveLocalRefs toResolve scope =
  let resolvedOuters = evalState (mapWithKey (resolveRefs
        (getLocalRefTypes toResolve)) (outerScope scope)) Map.empty
      resolvedGlobals = evalState (mapWithKey (resolveRefs
        (getLocalRefTypes toResolve)) (globalScope scope)) Map.empty
  in scope{
    globalScope = resolvedGlobals,
    outerScope = resolvedOuters
  }

openBlock :: Scope -> Scope
openBlock = id

openFunction :: Scope -> Scope
openFunction scope =
  let locals = localScope scope
      localGlobals = (locals, Map.empty)
      partition = Map.partition isGlobal locals
      (globals, outers) = bool partition localGlobals (inGlobalScope scope)
      updatedGlobal = Map.union (fmap cytype globals) (globalScope scope)
      updatedOuter = Map.union (fmap cytype outers) (outerScope scope)
  in scope{
    inGlobalScope = False,
    globalScope = Map.map (const []) updatedGlobal,
    outerScope = Map.map (const []) updatedOuter,
    localScope = Map.empty
  }

openModule :: Scope -> Scope
openModule scope =
  scope {
    inGlobalScope = True,
    globalScope = Map.empty,
    outerScope = Map.empty,
    localScope = Map.empty
  }

closeBlock :: Scope -> Scope -> (Scope, Map.Map String [Type])
closeBlock currScope innerScope =
  let currLocals = localScope currScope
      (newLocalScope, blockScope) =
        Map.partitionWithKey (inScope currLocals) (localScope innerScope)
      (mergedInner, toResolve) = mergeLocalScope innerScope blockScope
      resolvedInner = resolveLocalRefs toResolve mergedInner

      resolve ident binding = do
        resolvedTypes <- resolveRefs (getLocalRefTypes toResolve) ident $
          cytype binding
        return binding{cytype = resolvedTypes}
      resolvedLocalScope = evalState (mapWithKey resolve newLocalScope)
        Map.empty
  in (currScope{
    localScope = resolvedLocalScope,
    outerScope = (outerScope resolvedInner),
    globalScope = (globalScope resolvedInner)
  }, toResolve)

closeFunction :: Scope -> Scope ->
  (Scope, Map.Map String [Type])
closeFunction currScope innerScope =
  let (mergedInner, toResolve) =
        mergeLocalScope innerScope (localScope innerScope)
      resolvedInner = resolveLocalRefs toResolve mergedInner

      -- Update refs to current local variables
      currLocals = Map.filter isLocal $ localScope currScope
      isCurrentLocalVar k = Map.member k currLocals
      updatedInner =
        updateToLocalRefs
           (bool isNonLocalRef isGlobalRef (inGlobalScope currScope))
           isCurrentLocalVar resolvedInner

      joinTypeAnnots old new = new ++ old
      joinBindings old new =
        old{cytype = joinTypeAnnots (cytype old) (cytype new)}

      updatedCurrScope = bool
        -- Split updated variables in local and outer scope if not in global
        -- scope
        (let (updatedLocals, updatedOuters) =
                Map.partitionWithKey (inScope currLocals)
                  (outerScope updatedInner)

             newGlobals =
               Map.unionWith joinTypeAnnots (globalScope currScope)
                 (globalScope updatedInner)
             newOuters =
               Map.unionWith joinTypeAnnots (outerScope currScope)
                 updatedOuters

        in currScope{
          localScope = Map.unionWith joinBindings (localScope currScope)
            (Local <$> updatedLocals),
          outerScope = newOuters,
          globalScope = newGlobals
        })
        (currScope{
          localScope = Map.unionWith joinBindings (localScope currScope)
            (Local <$> (globalScope updatedInner))
        })
        (inGlobalScope currScope)

  in (updatedCurrScope, toResolve)

closeModule :: Scope -> (Map.Map String [Type])
closeModule innerScope =
  let globals = localScope innerScope
      resolve ident binding = do
        let toResolve = (fmap cytype . getLocalRefTypes globals)
        resolvedTypes <- resolveRefs toResolve ident $ cytype binding
        return binding{cytype = resolvedTypes}
      resolvedGlobals = evalState (mapWithKey resolve globals)
        Map.empty
  in fmap cytype resolvedGlobals
