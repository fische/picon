module Context (
  Scope.Type(..),
  Scope.Path(..),
  Context(..),
  newContext,
  Context.getVariableReference,
  Context.assignVariable,
  Context.returnVariable,
  Context.exitBlock,
  Context.stashFunction,
  Context.call,
  Context.getReturnType
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Scope

data Context =
  Context {
    position :: Path,
    scope :: Scope,
    functionsStash :: Map.Map Path (Context -> Context)
  }

newContext :: Context
newContext = Context {
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

unstashFunction :: Path -> Context -> Maybe Context
unstashFunction p ctx =
  let del _ _ = Nothing
      (parse, stash) = Map.updateLookupWithKey del p (functionsStash ctx)
  in maybe Nothing (\f ->
      let newCtx = f ctx {
            position = p
          }
      in Just newCtx {
        position = (position ctx),
        functionsStash = stash
      }) parse

call :: Type -> Context -> Context
call t@FuncRef{ refering = p } ctx =
  fromMaybe ctx{
    scope = Scope.call t $ scope ctx
  } $ unstashFunction p ctx
-- TODO Handle VarRef
call _ _ = error "cannot call non-callable objects"

getReturnType :: Type -> Context -> Type
getReturnType t ctx = Scope.getReturnType t (scope ctx)
