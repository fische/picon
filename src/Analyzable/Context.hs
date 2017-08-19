module Analyzable.Context (
  Scope.Argument(..),
  Scope.Type(..),
  Scope.Path(..),
  Context(..),
  newContext,
  Analyzable.Context.getVariableReference,
  Analyzable.Context.assignVariable,
  Analyzable.Context.returnVariable,
  Analyzable.Context.exitBlock,
  Analyzable.Context.enterClass,
  Analyzable.Context.exitClass,
  Analyzable.Context.stashFunction,
  Analyzable.Context.call,
  Analyzable.Context.getReturnType,
  Analyzable.Context.addParameter,
  Analyzable.Context.enablePositionalParametersFlag,
  Analyzable.Context.disablePositionalParametersFlag,
  Analyzable.Context.callAllStashed
) where

import qualified Data.Map.Strict as Map
import Data.Bool

import Language.Cython.Type

import Analyzable.Scope as Scope

data Context =
  Context {
    positionalParameters :: Bool,
    position :: Path,
    scope :: Scope,
    functionsStash :: Map.Map Path (Context -> Context)
  }

newContext :: Context
newContext = Context {
  positionalParameters = True,
  position = Leaf,
  scope = newModule,
  functionsStash = Map.empty
}

getVariableReference :: String -> Context -> Type
getVariableReference ident ctx =
  Scope.getVariableReference ident (position ctx) (scope ctx)

assignVariable :: String -> Type -> Context -> Context
assignVariable ident typ ctx = ctx {
  scope = Scope.assignVariable ident typ (position ctx) (scope ctx)
}

returnVariable :: Type -> Context -> Context
returnVariable typ ctx = ctx {
  scope = Scope.returnVariable typ (position ctx) (scope ctx)
}

exitBlock :: Context -> Context -> Context
exitBlock curr block =
  block {
    scope = Scope.exitBlock (position curr) (scope curr) (scope block)
  }

enterClass :: String -> Context -> Context
enterClass i ctx =
  let (newScope, newPath) = Scope.addClass i (position ctx) $ scope ctx
  in ctx{
    scope = newScope,
    position = newPath
  }

exitClass :: Context -> Context -> Context
exitClass curr c =
  c {
    position = position curr
  }

stashFunction :: String -> (Context -> Context) -> Context -> Context
stashFunction ident parse ctx =
  let (newScope, newPath) =
        Scope.addFunction ident (position ctx) (scope ctx)
  in ctx {
    functionsStash = Map.insert newPath parse (functionsStash ctx),
    scope = newScope
  }

unstashFunction :: Path -> Context -> Context
unstashFunction p ctx =
  let del _ _ = Nothing
      (parse, stash) = Map.updateLookupWithKey del p (functionsStash ctx)
  in maybe ctx (\f ->
      let newCtx = f ctx {
            position = p,
            functionsStash = stash
          }
      in newCtx {
        position = position ctx
      }) parse

call :: Type -> Map.Map Argument Type -> Context -> Context
call t@FuncRef{ refering = p } args ctx =
  let newCtx = unstashFunction p ctx
  in newCtx{
    scope = Scope.call t args $ scope newCtx
  }
call VarRef{ types = (hd:_) } args ctx =
  Analyzable.Context.call hd args ctx
call _ _ _ = error "cannot call non-callable objects"

callAllStashed' :: [Path] -> Context -> Context
callAllStashed' [] ctx = ctx
callAllStashed' (hd:tl) ctx =
  let newCtx = unstashFunction hd ctx
      convert i [] m = Map.insert (Keyword i) (Type PythonObject) m
      convert i ((t@Type{}):_) m = Map.insert (Keyword i) t m
      convert i (_:l) m = convert i l m
      params = Scope.getParameters hd $ scope newCtx
      args = Map.foldrWithKey convert Map.empty params
  in callAllStashed' tl $ newCtx{
    scope = Scope.call (FuncRef hd) args $ scope newCtx
  }

callAllStashed :: Context -> Context
callAllStashed ctx =
  callAllStashed' (Map.keys $ functionsStash ctx) ctx

getReturnType :: Type -> Context -> Type
getReturnType t ctx = Scope.getReturnType t (scope ctx)

addParameter :: String -> Maybe Type -> Context -> Context
addParameter i t ctx =
  let param = bool (NonPositional i) (Positional i) $ positionalParameters ctx
  in ctx {
    scope = Scope.addParameter param t (position ctx) (scope ctx)
  }

enablePositionalParametersFlag :: Context -> Context
enablePositionalParametersFlag ctx = ctx{
  positionalParameters = True
}

disablePositionalParametersFlag :: Context -> Context
disablePositionalParametersFlag ctx = ctx{
  positionalParameters = False
}
