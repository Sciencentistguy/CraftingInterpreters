use crate::error::RcloxError;
use crate::Result;

/// The type (or "kind", depending on who you ask) of a token
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
    Null,
}

/// A token of input. This has a type, a reference to the string it represents,
/// and a line number
#[derive(Debug, PartialEq, Eq)]
pub struct Token<'source_code> {
    pub kind: TokenType,
    pub span: &'source_code str,
    pub line: usize,
}

impl Token<'_> {
    /// An "eof" token
    pub const EOF: Self = Self {
        kind: TokenType::Eof,
        span: "",
        line: 0,
    };

    /// A "null" token
    pub const NULL: Self = Self {
        kind: TokenType::Null,
        span: "",
        line: 0,
    };
}

/// The lexer state machine
#[derive(Debug)]
pub struct Lexer<'source_code> {
    source: &'source_code [u8],
    start: usize,
    current: usize,
    line: usize,
}

impl<'source_code> Lexer<'source_code> {
    /// Construct a new, empty, lexer
    pub fn new(source: &'source_code str) -> Self {
        Lexer {
            source: source.as_bytes(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    /// Lex a token from the input, advancing the lexer appropriately
    pub fn lex_token(&'_ mut self) -> Result<Token<'source_code>> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(Token::EOF);
        };

        let c = self.advance();
        match c {
            '(' => Ok(self.make_token(TokenType::LeftParen)),
            ')' => Ok(self.make_token(TokenType::RightParen)),
            '{' => Ok(self.make_token(TokenType::LeftBrace)),
            '}' => Ok(self.make_token(TokenType::RightBrace)),
            ';' => Ok(self.make_token(TokenType::Semicolon)),
            ',' => Ok(self.make_token(TokenType::Comma)),
            '.' => Ok(self.make_token(TokenType::Dot)),
            '-' => Ok(self.make_token(TokenType::Minus)),
            '+' => Ok(self.make_token(TokenType::Plus)),
            '/' => Ok(self.make_token(TokenType::Slash)),
            '*' => Ok(self.make_token(TokenType::Star)),
            '!' => {
                if self.advance_if_char_matches('=') {
                    Ok(self.make_token(TokenType::BangEqual))
                } else {
                    Ok(self.make_token(TokenType::Bang))
                }
            }
            '=' => {
                if self.advance_if_char_matches('=') {
                    Ok(self.make_token(TokenType::EqualEqual))
                } else {
                    Ok(self.make_token(TokenType::Equal))
                }
            }
            '<' => {
                if self.advance_if_char_matches('=') {
                    Ok(self.make_token(TokenType::LessEqual))
                } else {
                    Ok(self.make_token(TokenType::Less))
                }
            }
            '>' => {
                if self.advance_if_char_matches('=') {
                    Ok(self.make_token(TokenType::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenType::Greater))
                }
            }
            '"' => {
                while !self.is_at_end() && self.peek() != '"' {
                    if self.peek() == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                if self.is_at_end() {
                    return Err(RcloxError::Lexer("Unterminated string.".to_string()));
                }
                self.advance();
                Ok(self.make_token(TokenType::String))
            }
            '0'..='9' => {
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
                if self.peek() == '.' && self.peek_next().map(|x| x.is_ascii_digit()).unwrap_or(false) {
                    self.advance();
                    while self.peek().is_ascii_digit() {
                        self.advance();
                    }
                }
                Ok(self.make_token(TokenType::Number))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                while self.peek().is_alphanumeric() || self.peek() == '_' {
                    self.advance();
                }
                let mut tok = self.make_token(TokenType::Null);
                tok.kind = match tok.span {
                    "and" => TokenType::And,
                    "class" => TokenType::Class,
                    "else" => TokenType::Else,
                    "false" => TokenType::False,
                    "for" => TokenType::For,
                    "fun" => TokenType::Fun,
                    "if" => TokenType::If,
                    "nil" => TokenType::Nil,
                    "or" => TokenType::Or,
                    "print" => TokenType::Print,
                    "return" => TokenType::Return,
                    "super" => TokenType::Super,
                    "this" => TokenType::This,
                    "true" => TokenType::True,
                    "var" => TokenType::Var,
                    "while" => TokenType::While,

                    _ => TokenType::Identifier,
                };
                Ok(tok)
            }

            _ => Err(RcloxError::Lexer(format!("Unexpected Character '{}'", c))),
        }
    }

    /// Check whether the lexer has consumed all of its input
    #[inline]
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    /// Advance the lexer over whitespace characters
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }

                _ => return,
            };
        }
    }

    /// Advance the lexer if the current character matches a given character
    fn advance_if_char_matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    /// Peek the next character of the input
    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current + 1).map(|&x| x.into())
    }

    /// Peek the current character of the input
    fn peek(&self) -> char {
        if self.current == self.source.len() {
            return 0.into();
        }
        self.source[self.current].into()
    }

    /// Advance the lexer, and return the next character
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1].into()
    }

    /// Construct a token with the given type with a span and a line
    fn make_token(&'_ self, kind: TokenType) -> Token<'source_code> {
        Token {
            kind,
            span: std::str::from_utf8(&self.source[self.start..self.current])
                .expect("Invalid byte slice encontered while creating token"),
            line: self.line,
        }
    }
}
