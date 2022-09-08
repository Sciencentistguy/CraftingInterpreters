//! The lexer takes the original input string and produces a series of tokens. A token is the
//! smallest unit of the Lox language, for example all keywords, punctuation items such as  `(` or
//! `>=`, and string / number literals
//!
//! A string literal may contain any valid unicode. Non-asctii unicode is not allowed anywhere
//! else (other than comments, which are not lexed).
//!
//! A number literal may contain a decimal sectiom e.g. `1` *or* `1.0`

use bstr::{BStr, ByteSlice};

use crate::error::LoxError;

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a BStr,
    start: usize,
    current: usize,
    line: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Single-character tokens.
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
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
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

/// The smallest unit of parsing in the Lox language
#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub span: &'a str,
    pub line: usize,
}

impl Token<'_> {
    pub const NULL: Self = Self {
        kind: TokenKind::Null,
        span: "",
        line: usize::MAX,
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.as_bytes().as_bstr(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn with_line(source: &'a str, line: usize) -> Self {
        Self {
            source: source.as_bytes().as_bstr(),
            start: 0,
            current: 0,
            line,
        }
    }

    /// Produce the next token from the input.
    ///
    /// Returns `Ok(None)` when the input is exhausted.
    pub fn next_token(&mut self) -> Result<Token<'a>, LoxError> {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenKind::Eof));
        }

        let c = self.consume();

        match c {
            '(' => Ok(self.make_token(TokenKind::LeftParen)),
            ')' => Ok(self.make_token(TokenKind::RightParen)),
            '{' => Ok(self.make_token(TokenKind::LeftBrace)),
            '}' => Ok(self.make_token(TokenKind::RightBrace)),
            ';' => Ok(self.make_token(TokenKind::Semicolon)),
            ',' => Ok(self.make_token(TokenKind::Comma)),
            '.' => Ok(self.make_token(TokenKind::Dot)),
            '-' => Ok(self.make_token(TokenKind::Minus)),
            '+' => Ok(self.make_token(TokenKind::Plus)),
            '/' => Ok(self.make_token(TokenKind::Slash)),
            '*' => Ok(self.make_token(TokenKind::Star)),
            '!' => {
                if self.consume_if_matches('=') {
                    Ok(self.make_token(TokenKind::BangEqual))
                } else {
                    Ok(self.make_token(TokenKind::Bang))
                }
            }
            '=' => {
                if self.consume_if_matches('=') {
                    Ok(self.make_token(TokenKind::EqualEqual))
                } else {
                    Ok(self.make_token(TokenKind::Equal))
                }
            }
            '<' => {
                if self.consume_if_matches('=') {
                    Ok(self.make_token(TokenKind::LessEqual))
                } else {
                    Ok(self.make_token(TokenKind::Less))
                }
            }
            '>' => {
                if self.consume_if_matches('=') {
                    Ok(self.make_token(TokenKind::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenKind::Greater))
                }
            }
            '"' => {
                while self.peek() != '"' && !self.is_at_end() {
                    if self.peek() == '\n' {
                        self.line += 1;
                    }
                    self.consume();
                }
                if self.is_at_end() {
                    Err(LoxError::UnterminatedString)
                } else {
                    self.consume();
                    Ok(self.make_token(TokenKind::String))
                }
            }
            '0'..='9' => {
                while self.peek().is_ascii_digit() {
                    self.consume();
                }
                if self.peek() == '.'
                    && self
                        .peek_next()
                        .map(|x| x.is_ascii_digit())
                        .unwrap_or(false)
                {
                    self.consume();
                    while self.peek().is_ascii_digit() {
                        self.consume();
                    }
                }
                Ok(self.make_token(TokenKind::Number))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // char::is_ascii_alphanumeric + '_'
                while matches!(self.peek(), '0'..='9' | 'A'..='Z' | 'a'..='z' | '_') {
                    self.consume();
                }

                let mut token = self.make_token(TokenKind::Null);

                token.kind = match token.span {
                    "and" => TokenKind::And,
                    "class" => TokenKind::Class,
                    "else" => TokenKind::Else,
                    "false" => TokenKind::False,
                    "for" => TokenKind::For,
                    "fun" => TokenKind::Fun,
                    "if" => TokenKind::If,
                    "nil" => TokenKind::Nil,
                    "or" => TokenKind::Or,
                    "print" => TokenKind::Print,
                    "return" => TokenKind::Return,
                    "super" => TokenKind::Super,
                    "this" => TokenKind::This,
                    "true" => TokenKind::True,
                    "var" => TokenKind::Var,
                    "while" => TokenKind::While,
                    _ => TokenKind::Identifier,
                };

                Ok(token)
            }

            c => Err(LoxError::UnexpectedToken(c)),
        }
    }

    /// Advance the lexer past any whitespace, incrementing the line counter as appropriate.
    ///
    /// Whitespace is any of the following:
    /// - `' '`
    /// - `'\r'`
    /// - `'\t'`
    /// - `'\n`
    /// - a comment (from `"//"` until a newline)
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.consume();
                }
                '\n' => {
                    self.line += 1;
                    self.consume();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.consume();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    /// Consume a character from the input, advatnc the lexer, and return that character.
    ///
    /// `static char advance()`
    fn consume(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1] as _
    }

    /// Advance the lexer if the current character matches `expected`
    ///
    /// `static bool match(char expected)`
    fn consume_if_matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        let _ = self.consume();
        true
    }

    /// Get the current character without advancing the lexer.
    ///
    /// If the lexer has reached the end of the input, returns '\0'.
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current] as _
    }

    /// Get the character after the current one (if it exists) without advancing the lexer
    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current + 1).map(|&x| x as _)
    }

    /// Return `true` if the lexer has reached the end of its input
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    /// Produce a `&str` of the lexer's `start..current` ranage
    fn span(&'_ self) -> &'a str {
        self.source[self.start..self.current]
            .to_str()
            .expect("`start` and `current` should never not be on codepoint boundaries")
    }

    /// Emit a token from the lexer with the specified kind
    fn make_token(&'_ self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            span: self.span(),
            line: self.line,
        }
    }
}
