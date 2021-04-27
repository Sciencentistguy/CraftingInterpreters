use crate::Result;

pub struct Lexer<'source_code> {
    source: &'source_code [u8],
    start: usize,
    current: usize,
    line: usize,
}

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

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'source_code> {
    pub kind: TokenType,
    pub string: &'source_code str,
    pub line: usize,
}

impl<'source_code> Token<'source_code> {
    pub fn eof() -> Self {
        Self {
            kind: TokenType::Eof,
            string: "",
            line: 0,
        }
    }
    pub fn null() -> Self {
        Self {
            kind: TokenType::Null,
            string: "",
            line: 0,
        }
    }
}

impl<'source_code> Lexer<'source_code> {
    pub fn new(source: &'source_code str) -> Self {
        Lexer {
            source: source.as_bytes(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn lex_token(&'_ mut self) -> Result<Token<'source_code>> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(Token::eof());
        };

        let c = self.advance();
        use TokenType::*;

        match c {
            '(' => Ok(self.make_token(LeftParen)),
            ')' => Ok(self.make_token(RightParen)),
            '{' => Ok(self.make_token(LeftBrace)),
            '}' => Ok(self.make_token(RightBrace)),
            ';' => Ok(self.make_token(Semicolon)),
            ',' => Ok(self.make_token(Comma)),
            '.' => Ok(self.make_token(Dot)),
            '-' => Ok(self.make_token(Minus)),
            '+' => Ok(self.make_token(Plus)),
            '/' => Ok(self.make_token(Slash)),
            '*' => Ok(self.make_token(Star)),
            '!' => {
                if self.match_char('=') {
                    Ok(self.make_token(BangEqual))
                } else {
                    Ok(self.make_token(Bang))
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(self.make_token(EqualEqual))
                } else {
                    Ok(self.make_token(Equal))
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(self.make_token(LessEqual))
                } else {
                    Ok(self.make_token(Less))
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(self.make_token(GreaterEqual))
                } else {
                    Ok(self.make_token(Greater))
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
                    return Err("Unterminated string.".into());
                }
                self.advance();
                Ok(self.make_token(String))
            }
            '0'..='9' => {
                while self.peek().is_digit(10) {
                    self.advance();
                }
                if self.peek() == '.' && self.peek_next().map(|x| x.is_digit(10)).unwrap_or(false) {
                    self.advance();
                    while self.peek().is_digit(10) {
                        self.advance();
                    }
                }
                Ok(self.make_token(Number))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                while self.peek().is_alphanumeric() || self.peek() == '_' {
                    self.advance();
                }
                let mut tok = self.make_token(Identifier);
                tok.kind = match tok.string {
                    "and" => And,
                    "class" => Class,
                    "else" => Else,
                    "false" => False,
                    "for" => For,
                    "fun" => Fun,
                    "if" => If,
                    "nil" => Nil,
                    "or" => Or,
                    "print" => Print,
                    "return" => Return,
                    "super" => Super,
                    "this" => This,
                    "true" => True,
                    "var" => Var,
                    "while" => While,

                    _ => Identifier,
                };
                Ok(tok)
            }

            _ => return Err(format!("Unexpected Character '{}'", c).into()),
        }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
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

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current + 1).map(|&x| x.into())
    }

    fn peek(&self) -> char {
        if self.current == self.source.len() {
            return 0.into();
        }
        self.source[self.current].into()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1].into()
    }

    fn make_token(&'_ self, kind: TokenType) -> Token<'source_code> {
        Token {
            kind,
            string: std::str::from_utf8(&self.source[self.start..self.current])
                .expect("Invalid byte slice encontered while creating token"),
            line: self.line,
        }
    }
}
