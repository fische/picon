module Scope (
  Type(..),
  Path(..),
  split,
  Scope(..),
  exists,
  add,
  get,
  update,
  updateM,
  merge,
  getReferenceType,
  resolveReferences,
  reverseIndex
) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Bool
import Monadic.Map

import qualified Control.Monad.State as State

import Language.Cython.Type
import Language.Python.Common.AST

-- TODO Handle builtins (print, etc)
-- TODO Handle special class methods
-- TODO Return references
-- TODO Hold members updates within class reference?
-- | Type represents a cython variable type.
data Type =
  Either (Type, Type) |
  Type CythonType |
  VarRef {
    identifier :: String,
    refering :: Path,
    types :: [Type]
  } |
  ParamRef {
    identifier :: String,
    refering :: Path
  } |
  FuncRef {
    refering :: Path
  } |
  -- | ClassTypeRef represents a reference to a class definition.
  ClassTypeRef {
    refering :: Path
  } |
  -- | ClassRef represents a reference to a class instance.
  ClassRef {
    refering :: Path
  } |
  -- | ModuleRef represents a reference to a module.
  ModuleRef {
    refering :: Path
  } |
  -- | ModuleVarRef represents a reference to a variable from a module.
  ModuleVarRef {
    identifier :: String,
    refering :: Path
  }
  deriving (Eq,Ord,Show)

-- | Path represents the path in the scope tree.
data Path =
  Node ((String, Int), Path) |
  Leaf
  deriving (Eq,Ord,Show)

append :: Path -> Path -> Path
append Leaf p2 = p2
append (Node (idx, p1)) p2 = Node (idx, append p1 p2)

sub :: Path -> Path -> Path
sub _ Leaf = error "p1 is not subpath of p2"
sub Leaf p = p
sub (Node(idx1, p1)) (Node(idx2, p2))
  | idx1 == idx2 = sub p1 p2
  | otherwise = error "p1 is not subpath of p2"

-- | split splits the last `node` from the path.
split :: Path -> (Path, Path)
split Leaf = error "this path can not be split"
split end@(Node(_, Leaf)) = (Leaf, end)
split (Node(ident, next)) =
  let (p, end) = split next
  in (Node(ident, p), end)

-- | Scope represents a python program through a scope tree.
data Scope =
  Program {
    scopes :: Map.Map String [Scope]
  } |
  Module {
    path :: Path,
    scopes :: Map.Map String [Scope],
    variables :: Map.Map String [Type],
    pymodule :: ModuleSpan
  } |
  Class {
    path :: Path,
    scopes :: Map.Map String [Scope],
    variables :: Map.Map String [Type]
  } |
  Function {
    path :: Path,
    scopes :: Map.Map String [Scope],
    variables :: Map.Map String [Type],
    returnType :: Maybe Type,
    parameterPosition :: [String],
    parameterType :: Map.Map String (Maybe Type)
  }
  deriving (Eq,Ord,Show)

-- | reverseIndex returns correct index given the given list is reversed.
reverseIndex :: [a] -> Int -> Int
reverseIndex l idx
  | length l > idx = length l - idx - 1
  | otherwise = error "index is outside list range"

exists :: Path -> Scope -> Bool
exists Leaf _ = True
exists (Node((ident, idx), p)) s =
  let found = Map.lookup ident (scopes s)
  in maybe False (\l -> exists p $ l !! reverseIndex l idx) found

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

-- | add adds given scope with the given identifier to the node.
-- It returns the new scope and the path of the scope that has been added.
add :: String -> Scope -> Path -> Scope -> (Scope, Path)
add ident body p s =
  let addToScope v = do
        new <- alterM (add' ident p body) ident (scopes v)
        return $ v {
          scopes = new
        }
  in State.runState (updateM addToScope p s) Leaf

-- | get retrieves scope node at given path in given scope tree.
get :: Path -> Scope -> Scope
get Leaf s = s
get (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      found = Map.lookup ident (scopes s)
  in maybe err (\l -> get p $ l !! reverseIndex l idx) found

-- | update updates node at given path with the given function.
update :: (Scope -> Scope) -> Path -> Scope -> Scope
update f Leaf s = f s
update f (Node ((ident, idx), p)) s =
  let err = error "next path level not found"
      updateScope l =
        let (hd, tl) = splitAt (reverseIndex l idx) l
        in Just (hd ++ (update f p (head tl):tail tl))
  in s {
    scopes = Map.alter (maybe err updateScope) ident (scopes s)
  }

-- | updateM is the monadic version of `update`.
updateM :: (Monad m) => (Scope -> m Scope) -> Path -> Scope -> m Scope
updateM f Leaf s = f s
updateM f (Node ((ident, idx), p)) s = do
  let err = error "next path level not found"
      updateScope l = do
        let (hd, tl) = splitAt (reverseIndex l idx) l
        v <- updateM f p $ head tl
        return . Just $ (hd ++ v:tail tl)
  newFunctions <- alterM (maybe err updateScope) ident (scopes s)
  return s {
    scopes = newFunctions
  }

-- | merge merges node at given path in both given scope with the given
-- function.
merge :: (Scope -> Scope -> Scope) -> Path -> Scope -> Scope -> Scope
merge f p s1 = update (f (get p s1)) p

-- | getReferenceType returns last assigned type in the given list of types.
getReferenceType :: String -> [Type] -> Type
getReferenceType i [] =
  error ("variable " ++ i ++ " referenced before assignement")
getReferenceType i (VarRef{ types = t }:_) = getReferenceType i t
getReferenceType _ (hd:_) = hd

checkReferencePath' :: String -> Path -> Scope -> Bool
checkReferencePath' _ Leaf _ = False
checkReferencePath' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let func = l !! reverseIndex l idx
        in checkReferencePath' i p func
  in Map.member i (variables s) || maybe err f (Map.lookup ident (scopes s))

checkReferencePath :: String -> Path -> Scope -> Maybe a
checkReferencePath i (Node((ident, idx), p)) s =
  let err = error ("binding to variable " ++ i ++ " has been overriden")
      f l = l !! reverseIndex l idx
      childScope = maybe err f (Map.lookup ident (scopes s))
  in bool Nothing err $ checkReferencePath' i p childScope
checkReferencePath _ Leaf _ = Nothing

resolveType :: Path -> Scope -> Type -> Type
resolveType funcPath s t@VarRef{identifier = i, refering = refPath} =
  let r = get refPath s
      err = error ("variable " ++ i ++ " was not found")
      var = Map.findWithDefault err i (variables r)
  in t{
    types = fromMaybe (getReferenceType i var:types t) $
              checkReferencePath i (sub refPath funcPath) r
  }
resolveType p s (Either(t1, t2)) =
  Either(resolveType p s t1, resolveType p s t2)
resolveType _ _ t = t

resolveReferences' :: (Type -> Type) -> Scope -> Scope
resolveReferences' resolve s@Function{} =
  s {
    variables = Map.map (map resolve) $ variables s,
    returnType = resolve <$> returnType s
  }
resolveReferences' resolve s =
  s {
    variables = Map.map (map resolve) $ variables s
  }

-- | resolveReferences resolves variable references at given path.
resolveReferences :: Path -> Scope -> Scope
resolveReferences p s =
  update (resolveReferences' (resolveType p s)) p s
