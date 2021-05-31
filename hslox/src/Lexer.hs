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
    kind :: TokenKind
  }
  deriving (Show)

type ErrorT = String

lexString :: String -> [Either ErrorT Token]
lexString [] = []
lexString (x : xs) = case x of
  x | isWhitespace x -> lexString xs
  '(' -> Right (Token Nothing LeftParenT) : lexString xs
  ')' -> Right (Token Nothing RightParenT) : lexString xs
  '{' -> Right (Token Nothing LeftBraceT) : lexString xs
  '}' -> Right (Token Nothing RightBraceT) : lexString xs
  ';' -> Right (Token Nothing SemicolonT) : lexString xs
  ',' -> Right (Token Nothing CommaT) : lexString xs
  '.' -> Right (Token Nothing DotT) : lexString xs
  '-' -> Right (Token Nothing MinusT) : lexString xs
  '+' -> Right (Token Nothing PlusT) : lexString xs
  '/' ->
    if nextChar =!= '/'
      then lexString $ dropWhile (/= '\n') xs
      else Right (Token Nothing SlashT) : lexString xs
  '*' -> Right (Token Nothing StarT) : lexString xs
  '!' ->
    if nextChar =!= '='
      then Right (Token Nothing BangEqualT) : lexString (tail xs)
      else Right (Token Nothing BangT) : lexString xs
  '=' ->
    if nextChar =!= '='
      then Right (Token Nothing EqualEqualT) : lexString (tail xs)
      else Right (Token Nothing EqualT) : lexString xs
  '<' ->
    if nextChar =!= '='
      then Right (Token Nothing LessEqualT) : lexString (tail xs)
      else Right (Token Nothing LessT) : lexString xs
  '>' ->
    if nextChar =!= '='
      then Right (Token Nothing GreaterEqualT) : lexString (tail xs)
      else Right (Token Nothing GreaterT) : lexString xs
  '"' ->
    let contents = takeWhile (/= '"') xs
     in if contents == xs
          then [Left "Unterminated string"]
          else Right (Token (Just contents) StringT) : lexString (drop (length contents + 1) xs)
  x
    | isDigit x ->
      let contents = x : takeWhile (\x -> isDigit x || (x == '.')) xs
       in if last' contents =!= '.'
            then [Left "Number must not end in '.'"]
            else Right (Token (Just contents) NumberT) : lexString (drop (length contents - 1) xs)
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
       in Right (Token xx keywordT) : lexString (drop (length contents -1) xs)
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
    isWhitespace ' ' = True
    isWhitespace '\r' = True
    isWhitespace '\t' = True
    isWhitespace '\n' = True
    isWhitespace _ = False

collectedLex :: String -> Either ErrorT [Token]
collectedLex s = sequenceA $ lexString s
