module Analyzable.Scope (
  module Scope,
  Argument(..),
  Parameter(..),
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
  getParameters
) where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Control.Applicative

import Scope

import Language.Cython.Type

data Argument =
  Position Int |
  Keyword String
  deriving (Eq,Ord,Show)

data Parameter =
  Positional String |
  NonPositional String
  deriving (Eq,Ord,Show)

newModule :: Scope
newModule = Module {
  path = Leaf,
  scopes = Map.empty,
  variables = Map.empty
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

exitBlock :: Path -> Scope -> Scope -> Scope
exitBlock = merge exitBlock'

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

getVariableReference' :: String -> Path -> Scope -> Maybe Type
getVariableReference' i Leaf s =
  let getHead [] = error ("variable " ++ i ++ " referenced before assignement")
      getHead (hd:_) = Just hd
  in maybe Nothing getHead $ Map.lookup i (variables s)
getVariableReference' i (Node((ident, idx), p)) s =
  let err = error "next path level not found"
      f l =
        let func = l !! reverseIndex l idx
            ref = maybe Nothing (\t -> Just VarRef{
              identifier = i,
              types = [getReferenceType i t],
              refering = path s
            }) $ Map.lookup i (variables s)
        in getVariableReference' i p func <|> ref
  in maybe err f $ Map.lookup ident (scopes s)

getVariableReference :: String -> Path -> Scope -> Type
getVariableReference i p s =
  let err = error ("variable " ++ i ++ " was not found")
  in fromMaybe err $ getVariableReference' i p s

assignVariable' :: String -> Type -> Scope -> Scope
assignVariable' k t s =
  let appendVarType Nothing = Just [t]
      appendVarType (Just l) = Just (t:l)
  in s {
    variables = Map.alter appendVarType k (variables s)
  }

assignVariable :: String -> Type -> Path -> Scope -> Scope
assignVariable k t = update (assignVariable' k t)

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
  in addParameterType (Keyword (l !! reverseIndex l idx)) t s

-- TODO Handle call to class constructor
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

getReturnType :: Type -> Scope -> Type
getReturnType FuncRef{ refering = p } s =
  maybe (Type . CType $ Void) getReturnType' . returnType $ get p s
getReturnType VarRef{ types = (hd:_) } s = getReturnType hd s
getReturnType _ _ = error "cannot get return type of non callable objects"

addParameter' :: Parameter -> Maybe Type -> Scope -> Scope
addParameter' p t s =
  let f Nothing = Just $ maybeToList t
      f (Just _) = error "parameter has already been added"
      params ident = Map.alter f ident $ parameterType s
      ref ident = ParamRef{identifier = ident, refering = path s}
  in case p of
      (Positional ident) -> s {
        variables = Map.insert ident [ref ident] $ variables s,
        parameterPosition = ident:parameterPosition s,
        parameterType = params ident
      }
      (NonPositional ident) -> s {
        variables = Map.insert ident [ref ident] $ variables s,
        parameterType = params ident
      }

addParameter :: Parameter -> Maybe Type -> Path -> Scope -> Scope
addParameter param t = update (addParameter' param t)

getParameters :: Path -> Scope -> Map.Map String [Type]
getParameters p s = parameterType $ get p s
