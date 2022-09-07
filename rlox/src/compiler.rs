use crate::{
    chunk::Chunk,
    error::LoxError,
    lexer::{Lexer, Token, TokenKind},
    opcode::Opcode,
    value::Value,
};

/// Disassemble the chunk once it is compiled
const DEBUG_PRINT_CODE: bool = true;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    chunk: Chunk,
    previous: Token<'a>,
    current: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            chunk: Chunk::default(),
            previous: Token::NULL,
            current: Token::NULL,
        }
    }

    /// Extract the completed chunk
    pub fn finalise(self) -> Chunk {
        self.chunk
    }

    /// Dumo the output of the lexer. Used for testing
    #[allow(dead_code)]
    pub fn dump_lexer(mut self) -> Result<(), LoxError> {
        loop {
            let token = self.lexer.next_token()?;
            if token.kind == TokenKind::Eof {
                break;
            }
            println!("{:?}", token);
        }
        Ok(())
    }

    /// Replace `self.previous` with `self.current`, and get a new token from the lexer for
    /// `self.current`
    fn advance(&mut self) -> Result<(), LoxError> {
        std::mem::swap(&mut self.current, &mut self.previous);

        let token = dbg!(self.lexer.next_token()?);

        self.current = token;

        Ok(())
    }

    /// Conditionally advance, if the current token is of the expected kind
    fn consume(&mut self, kind: TokenKind, msg: String) -> Result<(), LoxError> {
        if self.current.kind == kind {
            self.advance()?;
        } else {
            return Err(self.error(ErrorLocation::Current, msg));
        }

        Ok(())
    }

    /// Produce a LoxError with the given location and message
    fn error(&self, location: ErrorLocation, msg: String) -> LoxError {
        let line = match location {
            ErrorLocation::Previous => &self.previous,
            ErrorLocation::Current => &self.current,
        }
        .line;

        LoxError::SyntaxError { line, msg }
    }

    /// Get a mutable reference to the current chunk.
    ///
    /// Currently trivial, will become Not.
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// Emit an opcode to the current chunk
    fn emit(&mut self, opcode: Opcode) {
        let line = self.previous.line;
        self.current_chunk_mut().add_instruction(opcode, line);
    }

    /// Finalise the chunk, and print it if `DEBUG_PRINT_CODE` is true
    ///
    /// Called at the end of [`Self::compile`]
    fn end_compiler(&mut self) {
        self.emit(Opcode::Return);

        if DEBUG_PRINT_CODE {
            self.current_chunk_mut().disassemble("code");
        }
    }
}

impl<'a> Parser<'a> {
    /// The entry-point into the compiler
    pub fn compile(&mut self) -> Result<(), LoxError> {
        self.advance()?;

        self.expression()?;

        self.consume(TokenKind::Eof, "Expected end of expression".to_owned())?;

        self.end_compiler();

        Ok(())
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn number(&mut self) -> Result<(), LoxError> {
        let value = self.previous.span.parse()?;
        let value = Value::Number(value);

        self.emit(Opcode::Constant(value));

        Ok(())
    }

    fn grouping(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(
            TokenKind::RightParen,
            "Expected closing bracket after grouping".to_owned(),
        )?;

        Ok(())
    }

    fn unary(&mut self) -> Result<(), LoxError> {
        let kind = self.previous.kind;

        // compile the operand
        self.parse_precedence(Precedence::Unary)?;

        // Note: this call to emit uses self.current.line, but it *should* use the line of the `-`
        // token
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),

            _ => unreachable!("'-' is the only unary operator"),
        }

        Ok(())
    }

    /// Dispatch to the correct compilation method for the given precedence.
    ///
    /// This is the core of the Pratt parser.
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), LoxError> {
        dbg!(precedence);
        self.advance()?;
        let prefix_rule = parse_rule(self.previous.kind).prefix;

        if let Some(prefix_rule) = prefix_rule {
            prefix_rule.call(self)?;
        } else {
            return Err(self.error(ErrorLocation::Previous, "Expected expression".to_owned()));
        }

        while precedence <= parse_rule(self.current.kind).precedence {
            self.advance()?;
            let infix_rule = parse_rule(self.previous.kind).infix;
            // clox omits the null check, so unwrap
            infix_rule.unwrap().call(self)?;
        }

        Ok(())
    }

    fn binary(&mut self) -> Result<(), LoxError> {
        let kind = self.previous.kind;

        let rule = parse_rule(kind);
        let precedence = rule.precedence.next();
        self.parse_precedence(precedence)?;

        match kind {
            TokenKind::Minus => self.emit(Opcode::Subtract),
            TokenKind::Plus => self.emit(Opcode::Add),
            TokenKind::Slash => self.emit(Opcode::Divide),
            TokenKind::Star => self.emit(Opcode::Multiply),

            _ => unreachable!("All binary ops are covered"),
        }

        Ok(())
    }
}

/// Marker for which token (previous or current) to use when reporting an error.
enum ErrorLocation {
    Previous,
    Current,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    /// `'='`
    Assignment,
    /// `"or"`
    Or,
    /// `"and"`
    And,
    /// `'=='`, `'!='`
    Equality,
    /// `'<'`, `'>'`, `'<='`, `'>='`
    Comparison,
    /// `'+'` `'-'`
    Term,
    /// `'*'`, `'/'`
    Factor,
    /// `'!'`, `'-'`
    Unary,
    /// `'.'`, `'()'`
    Call,

    Primary,
}

impl Precedence {
    /// Returns the precedence aboce `self`.
    ///
    /// clox uses enum arithmetic: `(Precedence) (precedence + 1)`. This is a bit more explicit,
    /// and safer. It also avoids invalid states, which is good because that is UB in Rust.
    fn next(&self) -> Self {
        match self {
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,

            Precedence::None => unreachable!("None is not a valid precedence"),
            Precedence::Primary => unreachable!("Primary has no next precedence"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
    String,
    Variable,
    And,
    Or,
    Call,
}

impl ParseFn {
    /// Dispatch to the correct compilation method. A trick to avoid storing strust methods as
    /// function pointers, which is a pain in Rust.
    fn call(self, parser: &mut Parser) -> Result<(), LoxError> {
        match self {
            ParseFn::Grouping => parser.grouping(),
            ParseFn::Unary => parser.unary(),
            ParseFn::Binary => parser.binary(),
            ParseFn::Number => parser.number(),
            ParseFn::Literal => todo!(),
            ParseFn::String => todo!(),
            ParseFn::Variable => todo!(),
            ParseFn::And => todo!(),
            ParseFn::Or => todo!(),
            ParseFn::Call => todo!(),
        }
    }
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

/// The main Pratt parser lookup table.
///
/// Uses a cosnt fn rather than a static array because using a static array is horrible.
const fn parse_rule(token: TokenKind) -> ParseRule {
    match token {
        TokenKind::LeftParen => ParseRule {
            prefix: Some(ParseFn::Grouping),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::RightParen => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::LeftBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::RightBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Comma => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Dot => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Minus => ParseRule {
            prefix: Some(ParseFn::Unary),
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Term,
        },
        TokenKind::Plus => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Term,
        },
        TokenKind::Semicolon => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Slash => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Factor,
        },
        TokenKind::Star => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Factor,
        },
        TokenKind::Bang => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::BangEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::EqualEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Greater => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::GreaterEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Less => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::LessEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Identifier => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::String => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Number => ParseRule {
            prefix: Some(ParseFn::Number),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::And => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Class => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Else => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::False => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::For => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Fun => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::If => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Nil => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Or => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Print => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Return => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Super => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::This => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::True => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Var => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::While => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },

        // Should be an unreachable!(), alas cannot do that in const fn
        TokenKind::Eof => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Null => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}
