module Scope (
  Argument(..),
  Parameter(..),
  Type(..),
  Path(..),
  Scope(..),
  newModule,
  getVariableReference,
  exitBlock,
  addFunction,
  assignVariable,
  returnVariable,
  call,
  getReturnType,
  addParameter,
  getLocalVariableType,
  getLocalVariables,
  getFunctionReturnType,
  dropNextFunction
) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Bool
import Monadic.Map

import qualified Control.Monad.State as State
import Control.Applicative

import Language.Cython.Type

data Argument =
  Position Int |
  Keyword String
  deriving (Eq,Ord)

data Parameter =
  Positional String |
  NonPositional String

-- TODO Return references?
data Type =
  Either (Type, Type) |
  Type CythonType |
  VarRef {
    identifier :: String,
    refering :: Path,
    types :: [Type]
  } |
  FuncRef {
    refering :: Path
  } |
  ParamRef {
    identifier :: String,
    refering :: Path
  }

data Path =
  Node ((String, Int), Path) |
  Leaf
  deriving (Eq,Ord)

append :: Path -> Path -> Path
append Leaf p2 = p2
append (Node (idx, p1)) p2 = Node (idx, (append p1 p2))

sub :: Path -> Path -> Path
sub _ Leaf = error "p1 is not subpath of p2"
sub Leaf p = p
sub (Node(idx1, p1)) (Node(idx2, p2))
  | idx1 == idx2 = sub p1 p2
  | otherwise = error "p1 is not subpath of p2"

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
    returnType :: Maybe Type,
    parameterPosition :: [String],
    parameterType :: Map.Map String [Type]
  }



-- Primary scope operations
reverseIndex :: [a] -> Int -> Int
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
add ident body p s =
  let addToScope v = do
        new <- alterM (add' ident p body) ident (functions v)
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

getReferenceType :: String -> [Type] -> Type
getReferenceType i [] =
  error ("variable " ++ i ++ " referenced before assignement")
getReferenceType i (VarRef{ types = t }:_) = getReferenceType i t
getReferenceType _ (hd:_) = hd

getVariableReference' :: String -> Path -> Scope -> Maybe Type
getVariableReference' i Leaf s =
  let getHead [] = error ("variable " ++ i ++ " referenced before assignement")
      getHead (hd:_) = Just hd
  in maybe Nothing getHead $ Map.lookup i (variables s)
getVariableReference' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let func = l !! (reverseIndex l idx)
            ref = maybe Nothing (\t -> Just $ VarRef{
              identifier = i,
              types = [getReferenceType i t],
              refering = path s
            }) $ Map.lookup i (variables s)
        in getVariableReference' i p func <|> ref
  in maybe err f $ Map.lookup ident (functions s)

getVariableReference :: String -> Path -> Scope -> Type
getVariableReference i p s =
  let err = error ("variable " ++ i ++ " was not found")
  in fromMaybe err $ getVariableReference' i p s

checkReferencePath' :: String -> Path -> Scope -> Bool
checkReferencePath' _ Leaf _ = False
checkReferencePath' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let func = l !! (reverseIndex l idx)
        in checkReferencePath' i p func
  in Map.member i (variables s) || maybe err f (Map.lookup ident (functions s))

checkReferencePath :: String -> Path -> Scope -> Maybe a
checkReferencePath i (Node((ident, idx), p)) s =
  let err = error ("binding to variable " ++ i ++ " has been overriden")
      f l = l !! (reverseIndex l idx)
      childScope = maybe err f (Map.lookup ident (functions s))
  in bool Nothing err $ checkReferencePath' i p childScope
checkReferencePath _ Leaf _ = Nothing

resolveType :: Path -> Scope -> Type -> Type
resolveType funcPath s t@VarRef{identifier = i, refering = refPath} =
  let r = get refPath s
      err = error ("variable " ++ i ++ " was not found")
      var = Map.findWithDefault err i (variables r)
  in t{
    types = fromMaybe ((getReferenceType i var):(types t)) $
              checkReferencePath i (sub refPath funcPath) r
  }
resolveType p s (Either(t1, t2)) =
  Either(resolveType p s t1, resolveType p s t2)
resolveType _ _ t = t

resolveReferences' :: (Type -> Type) -> Scope -> Scope
resolveReferences' resolve s =
  s {
    variables = Map.map (map resolve) $ variables s,
    returnType = resolve <$> returnType s
  }

resolveReferences :: Path -> Scope -> Scope
resolveReferences p s =
  update (resolveReferences' (resolveType p s)) p s



-- Operations between scopes
exitBlock' :: Scope -> Scope -> Scope
exitBlock' curr@Module{} block@Module{} =
  let mergeHead [] [] = []
      mergeHead [] l2 = (Either (Type . CType $ Void, head l2)):l2
      mergeHead l1 [] = (head l1):l1
      mergeHead l1 l2
        | (length l1) == (length l2) = l1
        | otherwise = (Either (head l1, head l2)):l2
      addConditionalType k v =
        let found = Map.lookup k (variables curr)
        in maybe (mergeHead [] v) (\l -> mergeHead l v) found
  in curr {
    variables = Map.mapWithKey addConditionalType (variables block)
  }
exitBlock' _ _ =
  error "blocks's scope should use the same type than their outer scope"

exitBlock :: Path -> Scope -> Scope -> Scope
exitBlock = merge exitBlock'

addFunction :: String -> Path -> Scope -> (Scope, Path)
addFunction i p s =
  let (fScope, fPath) = add i Function{
        path = Leaf,
        functions = Map.empty,
        variables = Map.empty,
        returnType = Nothing,
        parameterPosition = [],
        parameterType = Map.empty
      } p s
      ref = FuncRef{
        refering = fPath
      }
  in (assignVariable i ref p fScope, fPath)



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
    returnType = maybe (Just t) (\r -> Just $ Either (t, r)) (returnType s)
  }
returnVariable' _ _ = error "cannot return anywhere except in a function"

returnVariable :: Type -> Path -> Scope -> Scope
returnVariable t = update (returnVariable' t)

addParameterType :: Argument -> Type -> Scope -> Scope
addParameterType (Keyword ident) t s =
  let err = error ("cannot find variable " ++ ident)
  in s {
      parameterType =
        Map.alter (maybe err (\l -> Just (t:l))) ident (parameterType s)
    }
addParameterType (Position idx) t s =
  let l = parameterPosition s
  in addParameterType (Keyword (l !! (reverseIndex l idx))) t s

call :: Type -> Map.Map Argument Type -> Scope -> Scope
call FuncRef{ refering = p } args s =
  resolveReferences p $
    update (\fun -> Map.foldrWithKey addParameterType fun args) p s
call (Either(t1, t2)) args s = call t2 args $ call t1 args s
call VarRef{ types = (hd:_) } args ctx = call hd args ctx
call _ _ _ = error "cannot call non-callable objects"

getReturnType' :: Type -> Type
getReturnType' (VarRef { types = [] }) = Type . CType $ Void
getReturnType' (VarRef { types = (hd:_) }) = hd
getReturnType' (Either(t1, t2)) = Either(getReturnType' t1, getReturnType' t2)
getReturnType' t = t

getReturnType :: Type -> Scope -> Type
getReturnType (FuncRef{ refering = p }) s =
  maybe (Type . CType $ Void) getReturnType' (returnType $ get p s)
getReturnType (VarRef{ types = (hd:_) }) s = getReturnType hd s
getReturnType t _ = t

addParameter' :: Parameter -> Maybe Type -> Scope -> Scope
addParameter' (Positional i) t s =
  let f Nothing = Just $ maybeToList t
      f (Just _) = error "parameter has already been added"
      params = Map.alter f i $ parameterType s
      ref = ParamRef{identifier = i, refering = path s}
  in s {
    variables = Map.insert i [ref] $ variables s,
    parameterPosition = i:(parameterPosition s),
    parameterType = params
  }
addParameter' (NonPositional i) t s =
  let f Nothing = Just $ maybeToList t
      f (Just _) = error "parameter has already been added"
      params = Map.alter f i $ parameterType s
      ref = ParamRef{identifier = i, refering = path s}
  in s {
    variables = Map.insert i [ref] $ variables s,
    parameterType = params
  }

addParameter :: Parameter -> Maybe Type -> Path -> Scope -> Scope
addParameter param t p s = update (addParameter' param t) p s



getCythonType' :: Scope -> Type -> CythonType
getCythonType' global (Either(t1, t2)) =
  mergeTypes [(getCythonType' global t1), (getCythonType' global t2)]
getCythonType' _ (Type t) = t
getCythonType' global (VarRef{ types = t }) =
  mergeTypes $ map (getCythonType' global) t
getCythonType' global (ParamRef{ identifier = i, refering = p }) =
  let s = get p global
      err = error ("can not find parameter " ++ i)
  in getCythonType global . Map.findWithDefault err i $ parameterType s
getCythonType' _ (FuncRef{}) =
  error "function pointers are not yet supported"

getCythonType :: Scope -> [Type] -> CythonType
getCythonType s t = mergeTypes (map (getCythonType' s) t)

getFunctionReturnType :: Scope -> Scope -> CythonType
getFunctionReturnType global (Function{returnType = r}) =
  maybe (CType Void) (getCythonType' global) r
getFunctionReturnType _ _ =
  error "cannot get return type of something else than a function"

getLocalVariableType :: Scope -> String -> Scope -> CythonType
getLocalVariableType global ident s =
  let err = error ("variable " ++ ident ++ " has not been declared")
  in maybe err (getCythonType global) $ Map.lookup ident (variables s)

getLocalVariables' :: Scope -> String -> [Type] -> Map.Map String CythonType ->
  Map.Map String CythonType
getLocalVariables' _ _ [FuncRef{}] acc = acc
getLocalVariables' global k v acc = Map.insert k (getCythonType global v) acc

getLocalVariables :: Scope -> Scope -> Map.Map String CythonType
getLocalVariables global Function{parameterType = p, variables = v} =
  Map.foldrWithKey (getLocalVariables' global) Map.empty $ Map.difference v p
getLocalVariables global Module{variables = v} =
  Map.foldrWithKey (getLocalVariables' global) Map.empty v

dropNextFunction' :: Maybe [Scope] -> State.State Scope (Maybe [Scope])
dropNextFunction' Nothing = error "no more function to drop"
dropNextFunction' (Just []) = error "no more function to drop"
dropNextFunction' (Just (hd:tl)) = do
  State.put hd
  return $ Just tl

dropNextFunction :: String -> Scope -> (Scope, Scope)
dropNextFunction i curr =
  let emptyFunc = Function{
        path = Leaf,
        functions = Map.empty,
        variables = Map.empty,
        returnType = Nothing,
        parameterPosition = [],
        parameterType = Map.empty
      }
      (newFunctions, dropped) =
        State.runState (alterM dropNextFunction' i $ functions curr) emptyFunc
  in (curr{
    functions = newFunctions
  }, dropped)