module Main where

import Compiler
import Control.Monad
import Control.Monad.Except
import Data.Either
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import Interpreter
import Options.Applicative
import Parser
import System.IO
import Text.Megaparsec
import Types

newtype Filename = Filename (Maybe String)

main :: IO ()
main = do
  filename <-
    execParser
      (info (Filename <$> optional (strArgument (metavar "FILE" <> help ""))) idm)
  case filename of
    Filename Nothing -> forever repl
    Filename (Just a) -> file a

repl :: IO ()
repl = do
  putStr ">>> "
  hFlush stdout
  line <- getLine
  let ast = parse pLoxProgram "REPL" (T.pack line)
  case ast of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> do
      putStrLn "AST:"
      print ast
      let program = fromList $ compile ast
      putStrLn "\nInstructions:"
      mapM_ print program
      putStrLn "\nRunning:"
      ran <- runExceptT $ run program
      case ran of
        Right _ -> return ()
        Left x -> print x

file :: FilePath -> IO ()
file name = do
  str <- readFile name
  let ast = parse pLoxProgram name $ T.pack str
  case ast of
    Left err -> putStr $ errorBundlePretty err
    Right ast -> do
      putStrLn "AST:"
      print ast
      let program = fromList $ compile ast
      putStrLn "\nInstructions:"
      mapM_ print program
      putStr "\nRunning:"
      ran <- runExceptT $ run program
      case ran of
        Right _ -> return ()
        Left x -> print x
