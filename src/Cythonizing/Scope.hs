{-# LANGUAGE DeriveDataTypeable #-}

module Cythonizing.Scope (
  Scope(..),
  newScope,
  openBlock,
  openFunction,
  stash,
  unstash
) where

import qualified Data.Map.Strict as Map
import Data.Data
import Data.Bool

import Control.Monad.Trans.Except
import Monadic.Map

import Language.Cython.Type
import Language.Cython.Error
import Type

data Scope =
  Scope {
    inGlobalScope :: Bool,
    globalScope :: Map.Map String [CythonType],
    outerScope :: Map.Map String [CythonType],
    localStash :: Map.Map String [CythonType],
    localScope :: Map.Map String [CythonType]
  }
  deriving (Eq,Ord,Show,Typeable,Data)

newScope :: Scope
newScope = Scope {
  inGlobalScope = False,
  globalScope = Map.empty,
  outerScope = Map.empty,
  localStash = Map.empty,
  localScope = Map.empty
}

openBlock :: Scope -> Scope
openBlock = id

openFunction :: Scope -> Scope
openFunction scope =
  bool scope{
    inGlobalScope = False,
    localStash = Map.empty,
    localScope = Map.empty,
    outerScope =
      Map.unions [(localStash scope), (localScope scope), (outerScope scope)]
  } scope{
    inGlobalScope = False,
    localStash = Map.empty,
    localScope = Map.empty,
    globalScope =
      Map.unions [(localStash scope), (localScope scope), (globalScope scope)]
  } (inGlobalScope scope)

getRefTypes :: Scope -> Ref -> Maybe [CythonType]
getRefTypes scope (LocalRef ident) = Map.lookup ident (localScope scope)
getRefTypes scope (NonLocalRef ident) = Map.lookup ident (outerScope scope)
getRefTypes scope (GlobalRef ident) = Map.lookup ident (globalScope scope)

stash :: annot -> Scope -> Map.Map String [Type] -> Except (Error annot) Scope
stash loc scope locals =
  let resolved =
        runExcept (mapWithKey (resolveTypes loc (getRefTypes scope)) locals)
  in either throwE (\r -> return scope{localStash = r}) resolved

unstash :: annot -> Scope -> String ->
  Except (Error annot) (Scope, [CythonType])
unstash loc scope ident =
  let remove _ _ = Nothing
      stashed = (localStash scope)
      (old, newStash) = Map.updateLookupWithKey remove ident stashed
  in maybe
    (throwE $ errVarNotFound loc ident)
    (\types ->
      let s = scope{
            localScope = Map.insert ident types (localScope scope),
            localStash = newStash
          }
      in return (s, types))
    old
