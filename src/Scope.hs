module Scope (
  Type(..),
  Path(..),
  Scope(..),
  newModule,
  getVariableReference,
  exitBlock,
  enterFunction,
  assignVariable,
  returnVariable
) where

import qualified Data.Map.Strict as Map
import Data.Bool
import Data.Maybe
import Monadic.Map

import qualified Control.Monad.State as State
import Control.Applicative

import Language.Cython.Type

-- TODO Return, function & param references
data Type =
  Either (Type, Type) |
  Type CythonType |
  Ref {
    identifier :: String,
    refering :: Path,
    types :: [Type]
  }

data Path =
  Node ((String, Int), Path) |
  Leaf

append :: Path -> Path -> Path
append Leaf p2 = p2
append (Node (idx, p1)) p2 = Node (idx, (append p1 p2))

data Scope =
  Module {
    path :: Path,
    functions :: Map.Map String [Scope],
    variables :: Map.Map String [Type]
  } |
  Function {
    path :: Path,
    functions :: Map.Map String [Scope],
    variables :: Map.Map String [Type],
    returnType :: [Type]
  }



-- Primary scope operations
reverseIndex :: [Scope] -> Int -> Int
reverseIndex l idx
  | (length l) > idx = (length l) - idx - 1
  | otherwise = error "index is outside list range"

add' :: String -> Path -> Scope -> Maybe [Scope] ->
  State.State Path (Maybe [Scope])
add' i p s Nothing = do
  let newPath = append p $ Node ((i, 0), Leaf)
  State.put newPath
  return $ Just [s{
    path = newPath
  }]
add' i p s (Just l) = do
  let newPath = append p $ Node ((i, length l), Leaf)
  State.put newPath
  return $ Just (s{
    path = newPath
  }:l)

add :: String -> Scope -> Path -> Scope -> (Scope, Path)
add ident sub p s =
  let addToScope v = do
        new <- alterM (add' ident p sub) ident (functions v)
        return $ v {
          functions = new
        }
  in State.runState (updateM addToScope p s) Leaf

get :: Path -> Scope -> Scope
get Leaf s = s
get (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      found = Map.lookup ident (functions s)
  in maybe err (\l -> get p $ l !! (reverseIndex l idx)) found

update :: (Scope -> Scope) -> Path -> Scope -> Scope
update f Leaf s = f s
update f (Node ((ident, idx), p)) s =
  let err = error "next path level not found"
      updateScope l =
        let (hd, tl) = splitAt (reverseIndex l idx) l
        in Just $ (hd ++ ((update f p (head tl)):(tail tl)))
  in s {
    functions = Map.alter (maybe err updateScope) ident (functions s)
  }

updateM :: (Monad m) => (Scope -> m Scope) -> Path -> Scope -> m Scope
updateM f Leaf s = f s
updateM f (Node ((ident, idx), p)) s = do
  let err = error "next path level not found"
      updateScope l = do
        let (hd, tl) = splitAt (reverseIndex l idx) l
        v <- updateM f p $ head tl
        return . Just $ (hd ++ v:(tail tl))
  newFunctions <- alterM (maybe err updateScope) ident (functions s)
  return s {
    functions = newFunctions
  }

merge :: (Scope -> Scope -> Scope) -> Path -> Scope -> Scope -> Scope
merge f p s1 s2 = update (\v1 -> f v1 $ get p s2) p s1

mergeM :: (Monad m) => (Scope -> Scope -> m Scope) -> Path -> Scope -> Scope ->
  m Scope
mergeM f p s1 s2 = updateM (\v1 -> f v1 $ get p s2) p s1

getVariableReference' :: String -> Path -> Scope -> Maybe Type
getVariableReference' i Leaf s =
  let found = Map.lookup i (variables s)
      getHead [] = Just $ Ref{
        identifier = i,
        types = [],
        refering = path s
      }
      getHead (hd:_) = Just hd
  in maybe Nothing getHead found
getVariableReference' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let func = l !! (reverseIndex l idx)
            ref = bool Nothing (Just $ Ref{
              identifier = i,
              types = [],
              refering = path s
            }) $ Map.member i (functions s)
        in getVariableReference' i p func <|> ref
  in maybe err f $ Map.lookup ident (functions s)

getVariableReference :: String -> Path -> Scope -> Type
getVariableReference i p s =
  let err = error ("variable " ++ i ++ " was not found")
  in fromMaybe err $ getVariableReference' i p s

resolveType :: Scope -> Type -> Type
resolveType s t@Ref{identifier = i, refering = p} =
  let r = get p s
      err = error ("variable " ++ i ++ " was not found")
      var = Map.findWithDefault err i (variables r)
      getHead [] = error "variable referred has not been yet assigned"
      getHead (hd:_) = hd
  in t{
    types = (getHead var):(types t)
  }
resolveType _ t = t

resolveReferences' :: (Type -> Type) -> Scope -> Scope
resolveReferences' resolve s =
  s {
    functions = Map.map (map (resolveReferences' resolve)) $ functions s,
    variables = Map.map (map resolve) $ variables s,
    returnType = map resolve $ returnType s
  }

resolveReferences :: Path -> Scope -> Scope
resolveReferences p s =
  update (resolveReferences' (resolveType s)) p s



-- Operations between scopes
exitBlock' :: Scope -> Scope -> Scope
exitBlock' curr@Module{} block@Module{} =
  let addConditionalType k v =
        let found = Map.lookup k (variables curr)
        in maybe ((head v):v) (\l -> (Either (head l, head v)):v) found
  in curr {
    variables = Map.mapWithKey addConditionalType (variables block)
  }
exitBlock' _ _ =
  error "blocks's scope should use the same type than their outer scope"

exitBlock :: Path -> Scope -> Scope -> Scope
exitBlock = merge exitBlock'

enterFunction :: String -> Path -> Scope -> (Scope, Path)
enterFunction i = add i $ Function{
  path = Leaf,
  functions = Map.empty,
  variables = Map.empty,
  returnType = []
}



-- Top-level scope operations
newModule :: Scope
newModule = Module {
  path = Leaf,
  functions = Map.empty,
  variables = Map.empty
}

assignVariable' :: String -> Type -> Scope -> Scope
assignVariable' k t s =
  let appendVarType Nothing = Just [t]
      appendVarType (Just l) = Just (t:l)
  in s {
    variables = Map.alter appendVarType k (variables s)
  }

assignVariable :: String -> Type -> Path -> Scope -> Scope
assignVariable k t p s = update (assignVariable' k t) p s

returnVariable' :: Type -> Scope -> Scope
returnVariable' t s@Function{} =
  s {
    returnType = (t:(returnType s))
  }
returnVariable' _ _ = error "cannot return anywhere except in a function"

returnVariable :: CythonType -> Path -> Scope -> Scope
returnVariable t = update (returnVariable' (Type t))
