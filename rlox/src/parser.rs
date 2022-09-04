use crate::{
    chunk::Chunk,
    error::LoxError,
    lexer::{Lexer, Token, TokenKind},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    previous: Option<Token<'a>>,
    current: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            previous: None,
            current: None,
        }
    }

    #[allow(dead_code)]
    pub fn dump_lexer(self) -> Result<(), LoxError> {
        for token in self.lexer {
            let token = token?;
            println!("{:?}", token);
        }
        Ok(())
    }
    pub fn compile(self) -> Result<Chunk, LoxError> {
        self.dump_lexer()?;
        todo!();
    }
}
