pub struct Lexer<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: usize,
}

#[derive(Debug, PartialEq, Eq)]
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
}

pub struct Token<'a> {
    kind: TokenType,
    string: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    pub fn eof() -> Self {
        Self {
            kind: TokenType::Eof,
            string: "".into(),
            line: 0,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.as_bytes(),
            start: 0,
            current: 0,
            line: 0,
        }
    }
    pub fn lex(&mut self) {
        //let mut line = -1;
        loop {
            let token = match self.lex_token() {
                Ok(x) => x,
                Err(x) => {
                    println!("Error: {}", x);
                    break;
                }
            };
            println!(
                "{:04}\t{:02} '{}' ({:?})",
                token.line,
                token.string.len(),
                token.string,
                token.kind,
            );
            if token.kind == TokenType::Eof {
                break;
            }
        }
    }

    fn lex_token(&mut self) -> Result<Token, Box<dyn std::error::Error>> {
        if self.is_at_end() {
            return Ok(Token::eof());
        };

        self.skip_whitespace();
        self.start = self.current;

        let c = self.advance();
        use TokenType::*;

        match c {
            '(' => return Ok(self.make_token(LeftParen)),
            ')' => return Ok(self.make_token(RightParen)),
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
                if self.peek() == '.' && self.peek_next().is_digit(10) {
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
                Ok(self.make_token(self.identifier_type()))
            }

            _ => return Err(format!("Unexpected Character '{}'", c).into()),
        }
    }

    fn identifier_type(&self) -> TokenType {
        use TokenType::*;
        let c: char = self.source[self.start].into();
        match c {
            'a' => self.check_keyword(1, 2, "nd", And),
            'c' => self.check_keyword(1, 4, "lass", Class),
            'e' => self.check_keyword(1, 3, "lse", Else),
            'f' => {
                if self.current - self.start > 1 {
                    match char::from(self.source[self.start + 1]) {
                        'a' => self.check_keyword(2, 3, "lse", False),
                        'o' => self.check_keyword(2, 1, "r", For),
                        'u' => self.check_keyword(2, 1, "n", Fun),
                        _ => Identifier,
                    }
                } else {
                    Identifier
                }
            }
            'i' => self.check_keyword(1, 1, "f", If),
            'n' => self.check_keyword(1, 2, "il", Nil),
            'o' => self.check_keyword(1, 1, "r", Or),
            'p' => self.check_keyword(1, 4, "rint", Print),
            'r' => self.check_keyword(1, 5, "eturn", Return),
            's' => self.check_keyword(1, 4, "uper", Super),
            't' => {
                if self.current - self.start > 1 {
                    match char::from(self.source[self.start + 1]) {
                        'h' => self.check_keyword(2, 2, "is", This),
                        'r' => self.check_keyword(2, 2, "ue", True),
                        _ => Identifier,
                    }
                } else {
                    Identifier
                }
            }
            'v' => self.check_keyword(1, 2, "ar", Var),
            'w' => self.check_keyword(1, 4, "hile", While),
            _ => Identifier,
        }
    }

    fn check_keyword(&self, start: usize, length: usize, rest: &str, kind: TokenType) -> TokenType {
        if self.current - self.start == start + length
            && (std::str::from_utf8(&self.source[self.start + start..self.start + start + length])
                .unwrap()
                == rest)
        {
            kind
        } else {
            TokenType::Identifier
        }
    }

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
                    if self.peek_next() == '/' {
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

    fn peek_next(&self) -> char {
        self.source[self.current + 1].into()
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

    fn make_token(&self, kind: TokenType) -> Token {
        Token {
            kind,
            string: &std::str::from_utf8(&self.source[self.start..self.current])
                .expect("Invalid byte slice"),
            line: self.line,
        }
    }
}
