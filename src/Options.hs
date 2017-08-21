module Options (
  Options(..)
) where

import Language.Python.Common.ParseError
import Language.Python.Common.AST

data Options =
  Options {
    targetDir :: Maybe FilePath,
    parser :: String -> IO (Either ParseError ModuleSpan)
  }
