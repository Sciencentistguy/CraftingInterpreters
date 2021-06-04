module Lexer where

import Data.Char
import Debug.Trace

data TokenKind
  = AndT
  | BangEqualT
  | BangT
  | ClassT
  | CommaT
  | DotT
  | ElseT
  | EofT
  | EqualEqualT
  | EqualT
  | FalseT
  | ForT
  | FunT
  | GreaterEqualT
  | GreaterT
  | IdentifierT
  | IfT
  | LeftBraceT
  | LeftParenT
  | LessEqualT
  | LessT
  | MinusT
  | NilT
  | NullT
  | NumberT
  | OrT
  | PlusT
  | PrintT
  | ReturnT
  | RightBraceT
  | RightParenT
  | SemicolonT
  | SlashT
  | StarT
  | StringT
  | SuperT
  | ThisT
  | TrueT
  | VarT
  | WhileT
  deriving (Show)

data Token = Token
  { string :: Maybe String,
    kind :: TokenKind,
    line :: Int
  }
  deriving (Show)

data LexerError = LexerError String Int

instance Show LexerError where
  show (LexerError desc line) = "<Lexer> [Line " ++ show line ++ "] Error: " ++ desc ++ "."

lexString :: String -> Int -> [Either LexerError Token]
lexString [] _ = []
lexString (x : xs) line = case x of
  -- Skip whitespace
  ' ' -> lexString xs line
  '\r' -> lexString xs line
  '\t' -> lexString xs line
  -- Increment line number on '\n'
  '\n' -> lexString xs (line + 1)
  -- Simple tokens
  '(' -> Right (Token Nothing LeftParenT line) : lexString xs line
  ')' -> Right (Token Nothing RightParenT line) : lexString xs line
  '{' -> Right (Token Nothing LeftBraceT line) : lexString xs line
  '}' -> Right (Token Nothing RightBraceT line) : lexString xs line
  ';' -> Right (Token Nothing SemicolonT line) : lexString xs line
  ',' -> Right (Token Nothing CommaT line) : lexString xs line
  '.' -> Right (Token Nothing DotT line) : lexString xs line
  '-' -> Right (Token Nothing MinusT line) : lexString xs line
  '+' -> Right (Token Nothing PlusT line) : lexString xs line
  '*' -> Right (Token Nothing StarT line) : lexString xs line
  -- '/' can be the start of "//" comment
  '/' ->
    if nextChar =?= '/'
      then lexString (dropWhile (/= '\n') xs) line
      else Right (Token Nothing SlashT line) : lexString xs line
  -- Double tokens
  '!' ->
    if nextChar =?= '='
      then Right (Token Nothing BangEqualT line) : lexString (tail xs) line
      else Right (Token Nothing BangT line) : lexString xs line
  '=' ->
    if nextChar =?= '='
      then Right (Token Nothing EqualEqualT line) : lexString (tail xs) line
      else Right (Token Nothing EqualT line) : lexString xs line
  '<' ->
    if nextChar =?= '='
      then Right (Token Nothing LessEqualT line) : lexString (tail xs) line
      else Right (Token Nothing LessT line) : lexString xs line
  '>' ->
    if nextChar =?= '='
      then Right (Token Nothing GreaterEqualT line) : lexString (tail xs) line
      else Right (Token Nothing GreaterT line) : lexString xs line
  -- Strings (must be terminated)
  '"' ->
    let contents = takeWhile (/= '"') xs
     in if contents == xs
          then [Left $ LexerError "Unterminated string" line]
          else
            Right (Token (Just contents) StringT line) :
            lexString (drop (length contents + 1) xs) line
  -- Numbers (Cannot end with a '.')
  x
    | isDigit x ->
      let contents = x : takeWhile (\x -> isDigit x || (x == '.')) xs
       in if last' contents =?= '.'
            then [Left $ LexerError "Number must not end in '.'" line]
            else
              Right (Token (Just contents) NumberT line) :
              lexString (drop (length contents - 1) xs) line
  -- Idents and keywords
  -- (idents must start with an ascii letter or a '_' and then can include numbers)
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
          y = case keywordT of
            IdentifierT -> Just contents
            _ -> Nothing
       in Right (Token y keywordT line) : lexString (drop (length contents -1) xs) line
  _ -> [Left $ LexerError ("Unexpected character: '" ++ x : "'") line]
  where
    nextChar = case xs of
      [] -> Nothing
      (x : _) -> Just x
    last' [] = Nothing
    last' xs = Just $ last xs
    isValidIdenChar x = isAlphaNum x || (x == '_')
    (=?=) :: Eq a => Maybe a -> a -> Bool
    Nothing =?= _ = False
    (Just a) =?= b = a == b

collectedLex :: String -> Either LexerError [Token]
collectedLex s = sequenceA $ lexString s 0
