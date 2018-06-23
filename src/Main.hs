module Main where

import System.FilePath.Posix
import System.Directory

import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))

import Control.Monad.State (evalState)

import Options.Applicative

import qualified Language.Python.Version2.Parser as Python2
import qualified Language.Python.Version3.Parser as Python3
import qualified Language.Python.Common.ParseError as ParseError
import Language.Python.Common.AST
import Language.Python.Common.Token
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Python.Common.PrettyParseError ()

import Language.Cython.AST as AST
import qualified Language.Cython.PrettyASTImplementation as PrettyASTImplementation
import qualified Language.Cython.PrettyASTDefinition as PrettyASTDefinition

import Analyzable
import Cythonizable
import Options
import Scope

parseModule ::
  (String -> String -> Either ParseError.ParseError (ModuleSpan, [Token])) ->
  String -> IO (Either ParseError.ParseError ModuleSpan)
parseModule parse p = do
  input <- readFile p
  return . fmap fst $ parse input p

opts :: Parser Options
opts =
  Options <$>
  optional (strOption
    (long "output"
      <> short 'o'
      <> metavar "TARGET_DIR"
      <> help "Target directory where to write translated files")) <*>
  (flag
    (Main.parseModule Python3.parseModule)
    (Main.parseModule Python2.parseModule)
    (short '2'
      <> help "Enable Python version 2 parsing") <|>
  flag'
    (Main.parseModule Python3.parseModule)
    (short '3'
      <> help "Enable Python version 3 parsing"))

file :: Parser String
file = strArgument (metavar "FILE")

allOpts :: Parser (Options, String)
allOpts = liftA2 (\o f -> (o, f)) opts file

cythonize' :: Scope -> String -> [Scope] -> AST.Module SrcSpan
cythonize' global _ [m] =
  let cython = Cythonizable.cythonize (pymodule m)
      ctx = Cythonizable.Context {
        globalScope = global,
        currentScope = m
      }
  in evalState cython ctx
cythonize' _ k _ = error $ "module " ++ k ++ " has not been loaded"

cythonize :: (Options, String) -> IO ()
cythonize (o, f) = do
  absolutePath <- makeAbsolute f
  (newCtx, m) <- addModule absolutePath $
    newContext (takeDirectory absolutePath) (parser o)
  analysis <- analyze m newCtx >>= unstashAll
  let global = scope analysis
      write (k, v) = do
                      let target = maybe k (replaceDirectory k) $ targetDir o
                          imp = replaceExtension target "pyx"
                          def = replaceExtension target "pxd"
                      writeFile def $ PrettyASTDefinition.pretty v
                      writeFile imp $ PrettyASTImplementation.pretty v
  mapM_ write . Map.assocs $ Map.mapWithKey (cythonize' global) $ scopes global

main :: IO ()
main =
  Main.cythonize =<< execParser p
  where p = info (allOpts <**> helper)
            (fullDesc
              <> progDesc "Convert python code from FILE to cython code"
              <> header "picon - a Python-to-Cython tool" )
