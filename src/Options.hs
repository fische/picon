module Options (
  Options(..)
) where

import Language.Python.Common.ParseError
import Language.Python.Common.AST

data Options =
  Options {
    -- | targetDir represents the root directory in which cython code should be
    -- written.
    targetDir :: Maybe FilePath,
    -- | parser parses the python file at the given path and returns the
    -- corresponding AST.
    parser :: String -> IO (Either ParseError ModuleSpan)
  }
