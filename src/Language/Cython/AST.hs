{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (Span)
import Data.Data

class Cythonizable p c where
  -- Convert Python AST (from Language.Python.Common.AST) to the AST here
  cythonize :: p -> c

newtype Module annot = Module [AST.Statement annot]
  deriving (Eq,Ord,Show,Typeable)

instance Cythonizable (AST.Module annot) (Module annot) where
  cythonize (AST.Module stmts) = Module stmts
