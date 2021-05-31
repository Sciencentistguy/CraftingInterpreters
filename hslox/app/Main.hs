module Main where

import Control.Monad
import Data.Semigroup ((<>))
import Lexer
import Options.Applicative
import System.IO

newtype Filename = Filename (Maybe String)

main :: IO ()
main = do
  filename <-
    execParser
      (info (Filename <$> optional (strArgument (metavar "FILE" <> help ""))) idm)
  case filename of
    Filename Nothing -> forever repl
    Filename (Just a) -> file a

repl = do
  putStr ">>> "
  hFlush stdout
  line <- getLine
  let toks = collectedLex line
  print toks

file name = do
  str <- readFile name
  let toks = collectedLex str
  print toks
