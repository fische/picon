module Analyzable.Context (
  Context(..),
  newContext,
  addModule,
  Scope.Type(..),
  Scope.Path(..),
  Scope.Argument(..),
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
  Analyzable.Context.unstashAll,
  Analyzable.Context.getAttribute,
  Scope.getReferencePath
) where

import Control.Monad

import qualified Data.Map.Strict as Map
import Data.Bool
import Data.Maybe

import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()
import Language.Python.Common.ParseError
import qualified Language.Python.Common.AST as AST

import Analyzable.Scope as Scope

-- | Context represents the scope context of a python program.
data Context =
  Context {
    -- | positionalParameters is a flag indicating whether next parameters are
    -- positional parameters.
    positionalParameters :: Bool,
    -- | position is the position of the current scope in the program scope
    -- tree.
    position :: Path,
    -- | scope is the program scope tree.
    scope :: Scope,
    -- | functionsStash contains the path of functions' scope and their parsing
    -- function.
    functionsStash :: Map.Map Path (Context -> IO Context),
    -- | parseModule parses the python module at the given path.
    parseModule :: String -> IO (Either ParseError AST.ModuleSpan),
    -- | basePath is the path to the directory of the main module.
    basePath :: String
  }

-- | newContext initializes a new Context with an empty Module scope.
newContext :: String -> (String -> IO (Either ParseError AST.ModuleSpan)) ->
  Context
newContext base f = Context {
  positionalParameters = True,
  position = Leaf,
  scope = newProgram,
  functionsStash = Map.empty,
  basePath = base,
  parseModule = f
}

addModule :: String -> Context -> IO (Context, AST.ModuleSpan)
addModule p ctx = do
  parsed <- parseModule ctx p
  case parsed of
    Left err -> error $ prettyText err
    Right m -> do
      let (newScope, newPath) = Scope.add p (newModule m) Leaf $ scope ctx
      return (ctx{
        scope = newScope,
        position = newPath
      }, m)

-- | getVariableReference retrieves the variable reference or type from the
-- current scope.
getVariableReference :: String -> Context -> Type
getVariableReference ident ctx =
  Scope.getVariableReference ident (position ctx) (scope ctx)

-- | assignVariable assigns the given type to the given variable identifier in
-- the scope corresponding to the given path or in the current scope if no path
-- is provided.
assignVariable :: Maybe Path -> String -> Type -> Context -> Context
assignVariable p ident typ ctx =
  let pos = fromMaybe (position ctx) p
  in ctx {
    scope = Scope.assignVariable ident typ pos (scope ctx)
  }

-- | returnVariable adds a returnType to the current function scope.
returnVariable :: Type -> Context -> Context
returnVariable typ ctx = ctx {
  scope = Scope.returnVariable typ (position ctx) (scope ctx)
}

-- | exitBlock exits block from the current scope.
exitBlock :: Context -> Context -> Context
exitBlock curr block =
  block {
    scope = Scope.exitBlock (position curr) (scope curr) (scope block)
  }

-- | enterClass adds class with the given identifier to the current scope and
-- returns the new scope with the path of the class scope.
enterClass :: String -> Context -> Context
enterClass i ctx =
  let (newScope, newPath) = Scope.addClass i (position ctx) $ scope ctx
  in ctx{
    scope = newScope,
    position = newPath
  }

-- | exitClass changes class context position to current position.
exitClass :: Context -> Context -> Context
exitClass curr c =
  c {
    position = position curr
  }

-- | stashFunction adds a function scope to the current one with the given
-- identifier. It also stashes the given parsing function with the function
-- scope path.
stashFunction :: String -> (Context -> IO Context) -> Context -> Context
stashFunction ident parse ctx =
  let (newScope, newPath) =
        Scope.addFunction ident (position ctx) (scope ctx)
  in ctx {
    functionsStash = Map.insert newPath parse (functionsStash ctx),
    scope = newScope
  }

-- | unstashFunction removes function at given path from stash. If it was
-- actually in the stash, it calls parsing function with current context.
unstashFunction :: Path -> Context -> IO Context
unstashFunction p ctx =
  let del _ _ = Nothing
      (parse, stash) = Map.updateLookupWithKey del p (functionsStash ctx)
  in maybe (return ctx) (\f -> do
      newCtx <- f ctx {
            position = p,
            functionsStash = stash
          }
      return newCtx {
        position = position ctx
      }) parse

-- | call calls function reference in given type.
-- In the case of a `FuncRef`, it unstashes the function, if it is in stash,
-- and calls it.
-- In the case of a `ClassTypeRef`, it does not do anything.
call :: Type -> Map.Map Argument Type -> Context -> IO Context
call t@FuncRef{ refering = p } args ctx = do
  newCtx <- unstashFunction p ctx
  return newCtx{
    scope = Scope.call t args $ scope newCtx
  }
call VarRef{ types = (hd:_) } args ctx =
  Analyzable.Context.call hd args ctx
-- TODO Call __init__
call ClassTypeRef{} _ ctx = return ctx
call _ _ _ = error "cannot call non-callable objects"

unstashAll' :: [Path] -> Context -> IO Context
unstashAll' l ctx = foldM (flip unstashFunction) ctx l

-- | unstashAll removes all functions from the stash and parses them all.
unstashAll :: Context -> IO Context
unstashAll ctx =
  unstashAll' (Map.keys $ functionsStash ctx) ctx

-- | getReturnType retrieves return type after last function call.
getReturnType :: Type -> Context -> Type
getReturnType t ctx = Scope.getReturnType t (scope ctx)

-- | addParameter adds parameter to current function scope with the given
-- identifier and type.
addParameter :: String -> Maybe Type -> Context -> Context
addParameter i t ctx =
  let param = bool (NonPositional i) (Positional i) $ positionalParameters ctx
  in ctx {
    scope = Scope.addParameter param t (position ctx) (scope ctx)
  }

-- | enablePositionalParametersFlag sets `positionalParameters` flag to true.
enablePositionalParametersFlag :: Context -> Context
enablePositionalParametersFlag ctx = ctx{
  positionalParameters = True
}

-- | disablePositionalParametersFlag sets `positionalParameters` flag to false.
disablePositionalParametersFlag :: Context -> Context
disablePositionalParametersFlag ctx = ctx{
  positionalParameters = False
}

-- | getAttribute retrieves attribute type of given type.
getAttribute :: Context -> Type -> String -> Type
getAttribute ctx = Scope.getAttribute (scope ctx)
