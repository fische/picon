module Main where

import System.FilePath.Posix

import Data.Semigroup ((<>))

import Control.Monad.State (State, evalState)

import Options.Applicative

import qualified Language.Python.Version2.Parser as Python2
import qualified Language.Python.Version3.Parser as Python3
import qualified Language.Python.Common.ParseError as ParseError
import Language.Python.Common.AST
import Language.Python.Common.Token
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyParseError ()

import Language.Cython.AST as AST
import Language.Cython.PrettyAST ()

import Analyzable
import Cythonizable
import Options

parseModule ::
  (String -> String -> Either ParseError.ParseError (ModuleSpan, [Token])) ->
  String -> IO (Either ParseError.ParseError ModuleSpan)
parseModule parse path = do
  input <- readFile path
  return . fmap fst $ parse input path

opts :: Parser Options
opts =
  Options <$>
  optional (strOption
    (long "output"
      <> short 'o'
      <> metavar "TARGET_DIR"
      <> help "Target directory where to write translated files")) <*>
  (flag
    (parseModule Python3.parseModule)
    (parseModule Python2.parseModule)
    (short '2'
      <> help "Enable Python version 2 parsing") <|>
  flag'
    (parseModule Python3.parseModule)
    (short '3'
      <> help "Enable Python version 3 parsing"))

file :: Parser String
file = strArgument (metavar "FILE")

allOpts :: Parser (Options, String)
allOpts = liftA2 (\o f -> (o, f)) opts file

cythonize :: (Options, String) -> IO ()
cythonize (o, f) = do
  parsed <- parser o f
  case parsed of
      Left err -> putStrLn $ prettyText err
      Right pymodule ->
        let analysis = unstashAll $ analyze pymodule newContext
            cython = Cythonizable.cythonize pymodule
              :: State Cythonizable.Context (AST.Module SrcSpan)
            path = replaceExtension (maybe f (replaceDirectory f) $
              targetDir o) "pyx"
        in writeFile path . prettyText . evalState cython $
            fromAnalysis analysis

main :: IO ()
main =
  Main.cythonize =<< execParser p
  where p = info (allOpts <**> helper)
            (fullDesc
              <> progDesc "Convert python code from FILE to cython code"
              <> header "picon - a Python-to-Cython tool" )
