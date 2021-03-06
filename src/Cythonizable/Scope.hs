module Cythonizable.Scope (
  module Scope,
  getFunctionReturnType,
  getLocalVariableType,
  getLocalVariables,
  dropNextScope
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Bool
import Monadic.Map

import Control.Monad.State as State
import Control.Applicative

import Scope

import Language.Cython.Type

getCythonType' :: Scope -> Type -> State (Set.Set Type) (Maybe CythonType)
getCythonType' _ (Type t) = return (Just t)
getCythonType' global (Either(t1, t2)) = do
  ct1 <- getCythonType' global t1
  ct2 <- getCythonType' global t2
  let l = catMaybes [ct1, ct2]
  return $ bool
    (Just $ mergeTypes l)
    Nothing
    (null l)
getCythonType' global VarRef{ types = t }  = do
  ct <- mapM (getCythonType' global) t
  let l = catMaybes ct
  return $ bool
    (Just $ mergeTypes l)
    Nothing
    (null l)
getCythonType' global ref@ParamRef{ identifier = i, refering = p } = do
  set <- State.get
  if Set.member ref set
    then
      return Nothing
    else do
      put $ Set.insert ref set
      let s = Scope.get p global
          err = error ("can not find parameter " ++ i)
      maybe
        (return $ Just PythonObject)
        (\t -> do
          ct <- getCythonType' global t
          return $ ct <|> Just PythonObject)
        (Map.findWithDefault err i $ parameterType s)
getCythonType' _ ClassRef{ refering = p } =
  let getClassName Leaf = error "this path does not have any node"
      getClassName (Node((ident, _), Leaf)) = ident
      getClassName (Node(_, n)) = getClassName n
  in return . Just . UserDefined $ getClassName p
getCythonType' _ ClassTypeRef{} =
  error "class types are not supported"
getCythonType' _ FuncRef{} =
  error "function pointers are not yet supported"

getCythonType :: Scope -> [Type] -> CythonType
getCythonType s t =
  let l = evalState (mapM (getCythonType' s) t) Set.empty
  in mergeTypes $ catMaybes l

-- | getFunctionReturnType resolves return type of current function scope into
-- a cython type.
getFunctionReturnType :: Scope -> Scope -> CythonType
getFunctionReturnType global Function{returnType = r} =
  maybe (CType Void) (\t -> getCythonType global [t]) r
getFunctionReturnType _ _ =
  error "cannot get return type of something else than a function"

-- | getLocalVariableType resolves types of variable with the given identifier
-- in current scope.
getLocalVariableType :: Scope -> String -> Scope -> CythonType
getLocalVariableType global ident s =
  let err = error ("variable " ++ ident ++ " has not been declared")
  in maybe err (getCythonType global) $ Map.lookup ident (variables s)

getLocalVariables' :: Scope -> String -> [Type] -> Map.Map String CythonType ->
  Map.Map String CythonType
getLocalVariables' _ _ [FuncRef{}] acc = acc
getLocalVariables' _ _ [ClassTypeRef{}] acc = acc
getLocalVariables' global k v acc = Map.insert k (getCythonType global v) acc

-- | getLocalVariables resolves types of variables declared in current scope.
-- It does not include function parameters in the case of a function scope.
getLocalVariables :: Scope -> Scope -> Map.Map String CythonType
getLocalVariables global Function{parameterType = p, variables = v} =
  Map.foldrWithKey (getLocalVariables' global) Map.empty $ Map.difference v p
getLocalVariables global s =
  Map.foldrWithKey (getLocalVariables' global) Map.empty $ variables s

dropNextScope' :: Maybe [Scope] -> State Scope (Maybe [Scope])
dropNextScope' Nothing = error "no more function to drop"
dropNextScope' (Just []) = error "no more function to drop"
dropNextScope' (Just (hd:tl)) = do
  put hd
  return $ Just tl

-- | dropNextScope drops next scope with given identifier from the current
-- scope. It returns new current scope and the dropped scope.
dropNextScope :: String -> Scope -> (Scope, Scope)
dropNextScope i curr =
  let emptyFunc = Function{
        path = Leaf,
        scopes = Map.empty,
        variables = Map.empty,
        returnType = Nothing,
        parameterPosition = [],
        parameterType = Map.empty
      }
      (newFunctions, dropped) =
        runState (alterM dropNextScope' i $ scopes curr) emptyFunc
  in (curr{
    scopes = newFunctions
  }, dropped)
