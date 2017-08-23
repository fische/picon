module Language.Cython.Type (
  CBasicType(..),
  CType(..),
  CythonType(..),
  mergeTypes
) where

data CBasicType =
  Char |
  Short |
  Int |
  Long |
  LongLong |
  Float |
  Double
  deriving (Eq,Ord,Show)

data CType =
  Void |
  BInt |
  Signed CBasicType |
  Unsigned CBasicType |
  Ptr CType
  deriving (Eq,Ord,Show)

-- TODO Handle typedefs
data CythonType =
  CType CType |
  String |
  Bytes |
  Unicode |
  PythonObject |
  UserDefined String
  deriving (Eq,Ord,Show)

-- TODO Handle typedefs
mergeTypes' :: CythonType -> [CythonType] -> CythonType
mergeTypes' acc [] = acc
mergeTypes' acc (hd:tl)
  | acc /= hd = error "Multi-typed variables are not yet supported"
  | otherwise = mergeTypes' hd tl

-- | mergeTypes merges given cython type into one single cython type.
mergeTypes :: [CythonType] -> CythonType
mergeTypes [] = CType Void
mergeTypes (hd:tl) = mergeTypes' hd tl
