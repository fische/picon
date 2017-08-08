{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Error where

import Data.Data

import Language.Python.Common.Pretty

data Error p =
  Error {
    annotation :: p,
    message :: String
  }
  deriving (Eq,Ord,Show,Typeable,Data)

instance (Pretty p) => Pretty (Error p) where
  pretty err = pretty (annotation err) <+> text (message err)

errVarNotFound :: p -> String -> Error p
errVarNotFound loc ident =
  Error loc $ "Variable " ++ ident ++ " was not found"

errVarAlreadyDeclared :: p -> String -> Error p
errVarAlreadyDeclared loc ident =
  Error loc $ "Variable " ++ ident ++ " has already been declared"

errVarAlreadyBound :: p -> String -> Error p
errVarAlreadyBound loc ident =
  Error loc $ "Variable " ++ ident
    ++ " can not be both globally and non locally bound"

errNotAllowedInGlobalScope :: p -> String -> Error p
errNotAllowedInGlobalScope loc action =
  Error loc $ action ++ " from the global scope are not allowed"

errVariableShouldHoldRef :: p -> String -> Error p
errVariableShouldHoldRef loc ident =
  Error loc $ "Node of variable " ++ ident ++ " should hold a reference"

errReferenceNotFound :: p -> String -> Error p
errReferenceNotFound loc ident =
  Error loc $ "Reference to " ++ ident ++ " was not found"

errNotSupported :: p -> String -> Error p
errNotSupported loc feature =
  Error loc $ feature ++ " is not supported for the moment"
