module Lexer where

import Data.Char
import Debug.Trace

data TokenKind
  = LeftParenT
  | RightParenT
  | LeftBraceT
  | RightBraceT
  | CommaT
  | DotT
  | MinusT
  | PlusT
  | SemicolonT
  | SlashT
  | StarT
  | BangT
  | BangEqualT
  | EqualT
  | EqualEqualT
  | GreaterT
  | GreaterEqualT
  | LessT
  | LessEqualT
  | IdentifierT
  | StringT
  | NumberT
  | AndT
  | ClassT
  | ElseT
  | FalseT
  | ForT
  | FunT
  | IfT
  | NilT
  | OrT
  | PrintT
  | ReturnT
  | SuperT
  | ThisT
  | TrueT
  | VarT
  | WhileT
  | EofT
  | NullT
  deriving (Show)

data Token = Token
  { string :: Maybe String,
    kind :: TokenKind,
    line :: Int
  }
  deriving (Show)

type ErrorT = String

lexString :: String -> Int -> [Either ErrorT Token]
lexString [] _ = []
lexString (x : xs) line = case x of
  ' ' -> lexString xs line
  '\r' -> lexString xs line
  '\t' -> lexString xs line
  '\n' -> lexString xs (line + 1)
  '(' -> Right (Token Nothing LeftParenT line) : lexString xs line
  ')' -> Right (Token Nothing RightParenT line) : lexString xs line
  '{' -> Right (Token Nothing LeftBraceT line) : lexString xs line
  '}' -> Right (Token Nothing RightBraceT line) : lexString xs line
  ';' -> Right (Token Nothing SemicolonT line) : lexString xs line
  ',' -> Right (Token Nothing CommaT line) : lexString xs line
  '.' -> Right (Token Nothing DotT line) : lexString xs line
  '-' -> Right (Token Nothing MinusT line) : lexString xs line
  '+' -> Right (Token Nothing PlusT line) : lexString xs line
  '/' ->
    if nextChar =!= '/'
      then lexString (dropWhile (/= '\n') xs) line
      else Right (Token Nothing SlashT line) : lexString xs line
  '*' -> Right (Token Nothing StarT line) : lexString xs line
  '!' ->
    if nextChar =!= '='
      then Right (Token Nothing BangEqualT line) : lexString (tail xs) line
      else Right (Token Nothing BangT line) : lexString xs line
  '=' ->
    if nextChar =!= '='
      then Right (Token Nothing EqualEqualT line) : lexString (tail xs) line
      else Right (Token Nothing EqualT line) : lexString xs line
  '<' ->
    if nextChar =!= '='
      then Right (Token Nothing LessEqualT line) : lexString (tail xs) line
      else Right (Token Nothing LessT line) : lexString xs line
  '>' ->
    if nextChar =!= '='
      then Right (Token Nothing GreaterEqualT line) : lexString (tail xs) line
      else Right (Token Nothing GreaterT line) : lexString xs line
  '"' ->
    let contents = takeWhile (/= '"') xs
     in if contents == xs
          then [Left "Unterminated string"]
          else Right (Token (Just contents) StringT line) : lexString (drop (length contents + 1) xs) line
  x
    | isDigit x ->
      let contents = x : takeWhile (\x -> isDigit x || (x == '.')) xs
       in if last' contents =!= '.'
            then [Left "Number must not end in '.'"]
            else Right (Token (Just contents) NumberT line) : lexString (drop (length contents - 1) xs) line
  x
    | isValidIdenChar x ->
      let contents = x : takeWhile isValidIdenChar xs
          keywordT = case contents of
            "and" -> AndT
            "class" -> ClassT
            "else" -> ElseT
            "false" -> FalseT
            "for" -> ForT
            "fun" -> FunT
            "if" -> IfT
            "nil" -> NilT
            "or" -> OrT
            "print" -> PrintT
            "return" -> ReturnT
            "super" -> SuperT
            "this" -> ThisT
            "true" -> TrueT
            "var" -> VarT
            "while" -> WhileT
            _ -> IdentifierT
          xx = case keywordT of
            IdentifierT -> Just contents
            _ -> Nothing
       in Right (Token xx keywordT line) : lexString (drop (length contents -1) xs) line
  _ -> [Left $ "Unexpected character: '" ++ x : "'"]
  where
    nextChar = case xs of
      [] -> Nothing
      (x : _) -> Just x
    isAtEnd = null xs
    (=!=) :: Eq a => Maybe a -> a -> Bool
    Nothing =!= _ = False
    (Just a) =!= b = a == b
    last' [] = Nothing
    last' xs = Just $ last xs
    isValidIdenChar x = isAlphaNum x || (x == '_')

collectedLex :: String -> Either ErrorT [Token]
collectedLex s = sequenceA $ lexString s 0
