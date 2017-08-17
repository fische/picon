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
  Analyzable.Context.stashFunction,
  Analyzable.Context.call,
  Analyzable.Context.getReturnType,
  Analyzable.Context.addParameter,
  Analyzable.Context.enablePositionalParametersFlag,
  Analyzable.Context.disablePositionalParametersFlag
) where

import qualified Data.Map.Strict as Map
import Data.Bool

import Scope

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
            position = p
          }
      in newCtx {
        position = (position ctx),
        functionsStash = stash
      }) parse

call :: Type -> Map.Map Argument Type -> Context -> Context
call t@FuncRef{ refering = p } args ctx =
  let newCtx = unstashFunction p ctx
  in ctx{
    scope = Scope.call t args $ scope newCtx
  }
-- TODO Handle VarRef
call _ _ _ = error "cannot call non-callable objects"

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
