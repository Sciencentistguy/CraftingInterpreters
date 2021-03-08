use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::value::Value;
use crate::Result;

#[derive(PartialOrd, Ord, PartialEq, Eq)]
enum Precedence {
    Assignment = 0, // =
    Or = 1,         // OR
    And = 2,        // AND
    Equality = 3,   // == !=
    Comparison = 4, // < > <= >=
    Term = 5,       // + -
    Factor = 6,     // * /
    Unary = 7,      // ! -
    Call = 8,       // . ()
    Primary = 9,

    None = -1,
}

impl From<u8> for Precedence {
    fn from(i: u8) -> Self {
        match i {
            0 => Self::Assignment,
            1 => Self::Or,
            2 => Self::And,
            3 => Self::Equality,
            4 => Self::Comparison,
            5 => Self::Term,
            6 => Self::Factor,
            7 => Self::Unary,
            8 => Self::Call,
            9 => Self::Primary,
            _ => panic!("Invalid u8 passed to Precedence::from::<u8>()"),
        }
    }
}

impl Precedence {
    fn one_higher(&self) -> Self {
        use Precedence::*;
        match self {
            Assignment => Or, // =
            Or => And,        // OR
            And => Equality,  // AND
            Equality => Comparison,
            Comparison => Term, // < > <= >=
            Term => Factor,     // + -
            Factor => Unary,    // * /
            Unary => Call,      // ! -
            Call => Primary,    // . ()
            Primary => {
                panic!("Primary is the highest precedence, cannot use Precedence::one_higher()")
            }
            None => unreachable!(),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,

    None,
}

struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

pub struct Compiler<'source> {
    source: &'source str,
}

struct Parser<'source> {
    lexer: &'source mut Lexer<'source>,
    current: Token,
    previous: Token,
    chunk: &'source mut Chunk,
}

impl<'source> Parser<'source> {
    fn new(lexer: &'source mut Lexer<'source>, chunk: &'source mut Chunk) -> Self {
        Self {
            lexer,
            current: Token::null(),
            previous: Token::null(),
            chunk,
        }
    }

    fn advance(&mut self) -> Result<()> {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = match self.lexer.lex_token() {
            Ok(x) => x,
            Err(e) => return Err(self.error_at_current(e.to_string().as_str())),
        };
        Ok(())
    }

    fn consume(&mut self, kind: TokenType, message: &str) -> Result<()> {
        if self.current.kind == kind {
            self.advance()?;
            Ok(())
        } else {
            Err(self.error_at_current(message))
        }
    }

    #[inline]
    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.previous.line);
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Result<()> {
        let value = self.previous.string.parse()?;
        self.emit_constant(Value::Number(value))?;
        Ok(())
    }

    fn emit_constant(&mut self, value: Value) -> Result<()> {
        self.emit_byte(OpCode::Constant as u8);
        let c = self.make_constant(value)?;
        self.emit_byte(c);
        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> Result<u8> {
        self.chunk.add_constant(value)
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after expression.")
    }

    fn unary(&mut self) -> Result<()> {
        let kind = self.previous.kind;

        self.parse_precedence(Precedence::Unary)?;

        match kind {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let operator_kind = self.previous.kind;
        let rule = get_rule(operator_kind);

        self.parse_precedence(rule.precedence.one_higher())?;

        match operator_kind {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            _ => unreachable!(),
        };
        Ok(())
    }

    fn dispatch_parse_fn(&mut self, parse_fn: ParseFn) -> Result<()> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::None => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        let prefix_rule = get_rule(self.previous.kind).prefix;
        if prefix_rule == ParseFn::None {
            return Err(self.error_at_previous("Expected expression."));
        }
        self.dispatch_parse_fn(prefix_rule)?;
        while precedence <= get_rule(self.current.kind).precedence {
            self.advance()?;
            let infix_rule = get_rule(self.previous.kind).infix;
            self.dispatch_parse_fn(infix_rule)?;
        }
        Ok(())
    }

    fn error_at_current(&self, message: &str) -> Box<dyn std::error::Error> {
        error_at(&self.current, message)
    }

    fn error_at_previous(&self, message: &str) -> Box<dyn std::error::Error> {
        error_at(&self.previous, message)
    }
}

impl<'source> Compiler<'source> {
    pub fn new(source: &'source str) -> Self {
        Compiler { source }
    }

    pub fn compile(&mut self, chunk: &mut Chunk) -> Result<()> {
        let mut lexer = Lexer::new(self.source);
        let mut parser = Parser::new(&mut lexer, chunk);
        parser.advance()?;
        parser.expression()?;
        parser.consume(TokenType::Eof, "Expected end of expression")?;
        parser.emit_byte(OpCode::Return as u8); // endCompiler()
        crate::debug::disassemble_chunk(chunk, "code");
        Ok(())
    }
}

fn get_rule(kind: TokenType) -> ParseRule {
    match kind {
        TokenType::LeftParen => ParseRule {
            prefix: ParseFn::Grouping,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::RightParen => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::LeftBrace => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::RightBrace => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Comma => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Dot => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Minus => ParseRule {
            prefix: ParseFn::Unary,
            infix: ParseFn::Binary,
            precedence: Precedence::Term,
        },
        TokenType::Plus => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Term,
        },
        TokenType::Semicolon => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Slash => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Factor,
        },
        TokenType::Star => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Factor,
        },
        TokenType::Bang => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::BangEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Equal => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::EqualEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Greater => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::GreaterEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Less => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::LessEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Identifier => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::String => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Number => ParseRule {
            prefix: ParseFn::Number,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::And => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Class => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Else => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::False => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::For => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Fun => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::If => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Nil => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Or => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Print => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Return => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Super => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::This => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::True => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Var => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::While => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Eof => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::Null => unreachable!(),
    }
}

fn error_at(token: &Token, message: &str) -> Box<dyn std::error::Error> {
    let mut out = format!("[line {}] Error ", token.line);
    if token.kind == TokenType::Eof {
        out.push_str("at end");
    } else {
        out.push_str(format!("at '{}'", token.string).as_str());
    }
    out.push(':');
    out.push_str(message);
    out.into()
}
