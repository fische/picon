module Analyzable.Scope (
  module Scope,
  Argument(..),
  Parameter(..),
  newProgram,
  newModule,
  exitBlock,
  addFunction,
  addClass,
  getVariableReference,
  assignVariable,
  returnVariable,
  call,
  getReturnType,
  addParameter,
  getAttribute,
  getReferencePath
) where

import qualified Data.Map.Strict as Map

import Scope

import qualified Language.Python.Common.AST as AST
import Language.Cython.Type

-- | Argument represents function call arguments.
data Argument =
  Position Int |
  Keyword String
  deriving (Eq,Ord,Show)

-- | Parameter represents function definition parameters.
data Parameter =
  Positional String |
  NonPositional String
  deriving (Eq,Ord,Show)

-- newProgram creates an empty Program scope.
newProgram :: Scope
newProgram = Program {
  scopes = Map.empty
}

-- newModule creates an empty Module scope.
newModule :: AST.ModuleSpan -> Scope
newModule m = Module {
  path = Leaf,
  scopes = Map.empty,
  variables = Map.empty,
  pymodule = m
}

exitBlock' :: Scope -> Scope -> Scope
exitBlock' curr block
  | path curr == path block =
    let mergeHead [] [] = []
        mergeHead [] l2 = Either (Type . CType $ Void, head l2):l2
        mergeHead l1 [] = head l1:l1
        mergeHead l1 l2
          | length l1 == length l2 = l1
          | otherwise = Either (head l1, head l2):l2
        addConditionalType k v =
          let found = Map.lookup k (variables curr)
          in maybe (mergeHead [] v) (`mergeHead` v) found
    in block {
      variables = Map.mapWithKey addConditionalType (variables block)
    }
  | otherwise =
    error "blocks's scope should use the same type than their outer scope"

-- | exitBlock merges block scope with current scope. If a variable from
-- current scope has been modified in block scope, an `Either` type is appended
-- to the variable's types with these two possible types.
exitBlock :: Path -> Scope -> Scope -> Scope
exitBlock = merge exitBlock'

-- | addFunction adds an empty function scope at the given path and assigns a
-- `FuncRef` to the variable with the given identifier at the same path. It
-- returns the new scope and the new function scope path.
addFunction :: String -> Path -> Scope -> (Scope, Path)
addFunction i p s =
  let (fScope, fPath) = add i Function{
        path = Leaf,
        scopes = Map.empty,
        variables = Map.empty,
        returnType = Nothing,
        parameterPosition = [],
        parameterType = Map.empty
      } p s
      ref = FuncRef{
        refering = fPath
      }
  in (assignVariable i ref p fScope, fPath)

-- | addClass adds an empty class scope at the given path and assigns a
-- `ClassTypeRef` to the variable with the given identifier at the same path.
-- It returns the new scope and the new class scope path.
addClass :: String -> Path -> Scope -> (Scope, Path)
addClass i p s =
  let (fScope, fPath) = add i Class{
        path = Leaf,
        scopes = Map.empty,
        variables = Map.empty
      } p s
      ref = ClassTypeRef{
        refering = fPath
      }
  in (assignVariable i ref p fScope, fPath)

getReferenceFlag :: Scope -> Bool
getReferenceFlag Program{} = True
getReferenceFlag Module{} = True
getReferenceFlag Class{} = False
getReferenceFlag Function{} = True

getVariableReference' :: String -> Path -> Scope -> Either Bool Type
getVariableReference' i Leaf Program{} = error ("variable " ++ i ++ " referenced before assignement")
getVariableReference' i Leaf s =
  let getHead [] = error ("variable " ++ i ++ " referenced before assignement")
      getHead (hd:_) = Right hd
  in maybe (Left $ getReferenceFlag s) getHead $ Map.lookup i (variables s)
getVariableReference' i (Node((ident, idx), p)) s@Program{} =
  let err = error "next path level not found"
      f l = getVariableReference' i p (l !! reverseIndex l idx)
  in maybe err f $ Map.lookup ident (scopes s)
getVariableReference' i (Node((ident, idx), p)) s@Class{} =
  let err = error "next path level not found"
      f l = getVariableReference' i p (l !! reverseIndex l idx)
  in maybe err f $ Map.lookup ident (scopes s)
getVariableReference' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let getRef flag = maybe
              (Left $ flag || getReferenceFlag s)
              (\t ->
                let getType False = getReferenceType i t
                    getType True = VarRef{
                      identifier = i,
                      types = [getReferenceType i t],
                      refering = path s
                    }
                in Right $ getType flag)
              (Map.lookup i (variables s))
            result = getVariableReference' i p (l !! reverseIndex l idx)
        in either getRef Right result
  in maybe err f $ Map.lookup ident (scopes s)

-- | getVariableReference retrieves reference or type of variable.
getVariableReference :: String -> Path -> Scope -> Type
getVariableReference i p s =
  let err = error ("variable " ++ i ++ " was not found")
  in either err id $ getVariableReference' i p s

assignVariable' :: String -> Type -> Scope -> Scope
assignVariable' k t s =
  let appendVarType Nothing = Just [t]
      appendVarType (Just l) = Just (t:l)
  in s {
    variables = Map.alter appendVarType k (variables s)
  }

-- | assignVariable appends the given type to the variable at the given path.
assignVariable :: String -> Type -> Path -> Scope -> Scope
assignVariable k t = update (assignVariable' k t)

returnVariable' :: Type -> Scope -> Scope
returnVariable' t s@Function{} =
  s {
    returnType = maybe (Just t) (\r -> Just $ Either (t, r)) (returnType s)
  }
returnVariable' _ _ = error "cannot return anywhere except in a function"

-- | returnVariable adds given type to the function scope at given path.
returnVariable :: Type -> Path -> Scope -> Scope
returnVariable t = update (returnVariable' t)

addParameterType :: Argument -> Type -> Scope -> Scope
addParameterType (Keyword ident) t s =
  let err = error ("cannot find variable " ++ ident)
      addType Nothing = Just t
      addType (Just o) = Just (Either(o, t))
  in s {
      parameterType =
        Map.alter (maybe err (Just . addType)) ident (parameterType s)
    }
addParameterType (Position idx) t s =
  let l = parameterPosition s
  in addParameterType (Keyword (l !! reverseIndex l idx)) t s

-- TODO Handle call to class constructor
-- | call resolves variable references in the function scope.
call :: Type -> Map.Map Argument Type -> Scope -> Scope
call FuncRef{ refering = p } args s =
  resolveReferences p $
    update (\fun -> Map.foldrWithKey addParameterType fun args) p s
call (Either(t1, t2)) args s = call t2 args $ call t1 args s
call VarRef{ types = (hd:_) } args ctx = call hd args ctx
call _ _ _ = error "cannot call non-callable objects"

getReturnType' :: Type -> Type
getReturnType' VarRef{ types = [] } = Type $ CType Void
getReturnType' VarRef{ types = (hd:_) } = hd
getReturnType' (Either(t1, t2)) = Either(getReturnType' t1, getReturnType' t2)
getReturnType' t = t

-- | getReturnType gets return type from function scope produced by last call.
-- In the case of `ClassTypeRef`, it returns a `ClassRef` with the same
-- refering path.
getReturnType :: Type -> Scope -> Type
getReturnType FuncRef{ refering = p } s =
  maybe (Type . CType $ Void) getReturnType' . returnType $ get p s
getReturnType VarRef{ types = (hd:_) } s = getReturnType hd s
getReturnType ModuleVarRef{ identifier = i, refering = r } s =
  let t = getVariableReference i r s
  in getReturnType t s
getReturnType ClassTypeRef{ refering = p } _ = ClassRef{ refering = p }
getReturnType _ _ = error "cannot get return type of non callable objects"

addParameter' :: Parameter -> Maybe Type -> Path -> Scope -> Scope
addParameter' param t funcPath upper =
  update (\func ->
    let addType Nothing = Just t
        addType (Just _) = error "parameter has already been added"
        updateParams ident = Map.alter addType ident $ parameterType func
        ref ident =
          case upper of
            Class{path = p} -> [ClassRef{refering = p}]
            _ -> [ParamRef{identifier = ident, refering = path func}]
    in case param of
        (Positional ident) -> func {
          variables = Map.insert ident (ref ident) $ variables func,
          parameterPosition = ident:parameterPosition func,
          parameterType = updateParams ident
        }
        (NonPositional ident) -> func {
          variables = Map.insert ident (ref ident) $ variables func,
          parameterType = updateParams ident
        }) funcPath upper

-- | addParameter adds parameter to function scope at given path.
addParameter :: Parameter -> Maybe Type -> Path -> Scope -> Scope
addParameter param t p =
  let (upper, func) = split p
  in update (addParameter' param t func) upper

-- | getAttribute returns type of attribute from given type reference.
getAttribute :: Scope -> Type -> String -> Type
getAttribute _ VarRef{ identifier = i, types = [] } _ =
  error ("variable " ++ i ++ " referenced before assignement")
getAttribute s VarRef{ types = (hd:_) } attr = getAttribute s hd attr
getAttribute s ClassTypeRef{ refering = p } attr =
  let err = error ("attribute " ++ attr ++ " was not found")
      getType [] =
        error ("attribute " ++ attr ++ " referenced before assignement")
      getType (hd:_) = hd
  in maybe err getType . Map.lookup attr . variables $ get p s
getAttribute s ModuleRef{ refering = p } attr =
  let err = error ("variable " ++ attr ++ " was not found")
      getType [] =
        error ("variable " ++ attr ++ " referenced before assignement")
      getType (hd:_) = hd
  in maybe err getType . Map.lookup attr . variables $ get p s
getAttribute s ClassRef{ refering = p } attr =
  let err = error ("attribute " ++ attr ++ " was not found")
      getType [] =
        error ("attribute " ++ attr ++ " referenced before assignement")
      getType (hd:_) = hd
  in maybe err getType . Map.lookup attr . variables $ get p s
getAttribute s ModuleVarRef{ identifier = i, refering = p } attr =
  getAttribute s (getVariableReference i p s) attr
getAttribute _ _ _ =
  error "this expression has no attribute"

-- TODO Handle VarRef, ParamRef
-- | getReferencePath return class reference.
getReferencePath :: Type -> Path
getReferencePath ClassRef{ refering = p } = p
getReferencePath _ = error "type does not hold any reference path"
