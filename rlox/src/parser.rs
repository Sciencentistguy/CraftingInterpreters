use crate::{chunk::Chunk, error::LoxError, lexer::{Lexer, TokenKind}};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
        }
    }

    pub fn parse(&mut self) -> Result<Chunk, LoxError> {

        loop {
            let token = self.lexer.next_token()?;
            if token.kind == TokenKind::Eof {
                break;
            }
            println!("{:?}", token);
        }

        todo!("Compilation NYI, only lexing")
    }
}
