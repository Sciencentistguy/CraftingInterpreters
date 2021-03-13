use std::rc::Rc;

use crate::chunk::Chunk;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::opcode::OpCode;
use crate::value::Value;
use crate::Result;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
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
    Literal,
    String,
    Variable,

    None,
}

struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

struct Local {
    name: Rc<String>,
    depth: usize,
}

struct Compiler {
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    parent: Option<Box<Self>>,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            locals: Vec::with_capacity(u8::MAX as usize),
            local_count: 0,
            scope_depth: 0,
            parent: None,
        }
    }

    fn fork(parent: Box<Self>) -> Self {
        let mut out = Compiler::new();
        out.parent = Some(parent);
        out
    }
}

pub struct CompilerDriver<'source> {
    source: &'source str,
    compiler: Box<Compiler>,
}

struct Parser<'source> {
    lexer: &'source mut Lexer<'source>,
    current: Token,
    previous: Token,
    chunk: &'source mut Chunk,
    compiler: &'source mut Compiler,
}

impl<'source> Parser<'source> {
    fn new(
        lexer: &'source mut Lexer<'source>,
        chunk: &'source mut Chunk,
        compiler: &'source mut Compiler,
    ) -> Self {
        Self {
            lexer,
            current: Token::null(),
            previous: Token::null(),
            chunk,
            compiler,
        }
    }

    fn advance(&mut self) -> Result<()> {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = match self.lexer.lex_token() {
            Ok(x) => x,
            Err(e) => return Err(format!("<Lexer> Error {}", e).into()),
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
        let c = self.make_constant(value);
        self.emit_byte(c as u8);
        Ok(())
    }

    #[inline]
    fn make_constant(&mut self, value: Value) -> i32 {
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
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
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
            TokenType::BangEqual => {
                self.emit_byte(OpCode::Equal as u8);
                self.emit_byte(OpCode::Not as u8);
            }
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => {
                self.emit_byte(OpCode::Less as u8);
                self.emit_byte(OpCode::Not as u8);
            }
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => {
                self.emit_byte(OpCode::Greater as u8);
                self.emit_byte(OpCode::Not as u8);
            }
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            _ => unreachable!(),
        };
        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        match self.previous.kind {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn string(&mut self) -> Result<()> {
        self.emit_constant(Value::String(Rc::new(
            self.previous.string.trim_matches('"').to_string(),
        )))
    }

    fn dispatch_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) -> Result<()> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
            ParseFn::String => self.string(),
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::None => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        let prefix_rule = get_rule(self.previous.kind).prefix;
        if prefix_rule == ParseFn::None {
            return Err(self.error_at_previous("Expected expression."));
        }

        let can_assign = precedence <= Precedence::Assignment;
        self.dispatch_parse_fn(prefix_rule, can_assign)?;

        while precedence <= get_rule(self.current.kind).precedence {
            self.advance()?;
            let infix_rule = get_rule(self.previous.kind).infix;
            self.dispatch_parse_fn(infix_rule, can_assign)?;
        }
        if can_assign && self.matches(TokenType::Equal)? {
            return Err(self.error_at_previous("Invalid assignment target"));
        }
        Ok(())
    }

    fn error_at_current(&self, message: &str) -> Box<dyn std::error::Error> {
        error_at(&self.current, message)
    }

    fn error_at_previous(&self, message: &str) -> Box<dyn std::error::Error> {
        error_at(&self.previous, message)
    }

    pub fn declaration(&mut self) -> Result<()> {
        if self.matches(TokenType::Var)? {
            self.variable_declaration()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<()> {
        if self.matches(TokenType::Print)? {
            self.print_statement()
        } else if self.matches(TokenType::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<()> {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration()?;
        }
        self.consume(TokenType::RightBrace, "Expected '}' after block.")
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    #[inline]
    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;
        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth as usize
                > self.compiler.scope_depth
        {
            self.emit_byte(OpCode::Pop as u8);
            self.compiler.local_count -= 1;
        }
    }

    pub fn matches(&mut self, kind: TokenType) -> Result<bool> {
        if !self.check(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    #[inline]
    fn check(&self, kind: TokenType) -> bool {
        self.current.kind == kind
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after value.")?;
        self.emit_byte(OpCode::Print as u8);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        self.emit_byte(OpCode::Pop as u8);
        Ok(())
    }

    fn parse_variable(&mut self, message: &str) -> Result<u8> {
        self.consume(TokenType::Identifier, message)?;
        self.declare_variable()?;
        if self.compiler.scope_depth > 0 {
            return Ok(0);
        }
        let name = self.previous.string.to_string();
        Ok(self.identifier_constant(name) as u8)
    }

    fn identifier_constant(&mut self, name: String) -> i32 {
        self.make_constant(Value::String(Rc::new(name)))
    }

    fn variable_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expected a varaible name.")?;
        if self.matches(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialised();
            return;
        }
        self.emit_byte(OpCode::DefineGlobal as u8);
        self.emit_byte(global);
    }

    fn mark_initialised(&mut self) {
        if self.compiler.local_count == 0 {
            panic!("No local declared, yet attempting to mark intialised")
        }
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }
        let name = self.previous.string.clone();
        for local in self.compiler.locals.iter().rev() {
            if local.depth != usize::MAX && local.depth < self.compiler.scope_depth {
                // local.depth != -1 is always true, unsigned type
                break;
            }
            if *local.name == name {
                return Err(format!(
                    "There is already a variable with name {} in this scope",
                    name
                )
                .into());
            }
        }
        self.add_local(name.as_str())
    }

    fn add_local(&mut self, name: &str) -> Result<()> {
        let depth = self.compiler.scope_depth;
        self.compiler.locals.push(Local {
            name: Rc::new(name.to_string()),
            depth: usize::MAX,
        });
        self.compiler.local_count += 1;
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let name = self.previous.string.to_string();
        self.named_variable(name, can_assign)
    }

    fn named_variable(&mut self, name: String, can_assign: bool) -> Result<()> {
        let get_op: OpCode;
        let set_op: OpCode;
        let mut arg = self.resolve_local(self.compiler, name.as_str())?;
        if arg != -1 {
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        } else {
            arg = self.identifier_constant(name);
            get_op = OpCode::GetGlobal;
            set_op = OpCode::SetGlobal;
        }

        if can_assign && self.matches(TokenType::Equal)? {
            self.expression()?;
            self.emit_byte(set_op as u8);
            self.emit_byte(arg as u8);
        } else {
            self.emit_byte(get_op as u8);
            self.emit_byte(arg as u8);
        }
        Ok(())
    }

    fn resolve_local(&self, compiler: &Compiler, name: &str) -> Result<i32> {
        for (i, local) in compiler.locals.iter().enumerate().rev() {
            if name == *local.name {
                if local.depth == usize::MAX {
                    return Err(self
                        .error_at_previous("Cannot read local variable in its own initialiser."));
                }
                return Ok(i as i32);
            }
        }
        Ok(-1)
    }
}

impl<'source> CompilerDriver<'source> {
    pub fn new(source: &'source str) -> Self {
        CompilerDriver {
            source,
            compiler: Box::new(Compiler::new()),
        }
    }

    pub fn compile(&mut self, chunk: &mut Chunk) -> Result<()> {
        println!("Starting compilation");
        let mut lexer = Lexer::new(self.source);
        let mut parser = Parser::new(&mut lexer, chunk, &mut self.compiler);
        parser.advance()?;
        while !parser.matches(TokenType::Eof)? {
            parser.declaration()?;
        }
        parser.emit_byte(OpCode::Return as u8); // endCompiler()
        crate::debug::disassemble_chunk(chunk, "code");
        println!("Finished compilation");
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
            prefix: ParseFn::Unary,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::BangEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::None,
        },
        TokenType::Equal => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::EqualEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Equality,
        },
        TokenType::Greater => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Comparison,
        },
        TokenType::GreaterEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Comparison,
        },
        TokenType::Less => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Comparison,
        },
        TokenType::LessEqual => ParseRule {
            prefix: ParseFn::None,
            infix: ParseFn::Binary,
            precedence: Precedence::Comparison,
        },
        TokenType::Identifier => ParseRule {
            prefix: ParseFn::Variable,
            infix: ParseFn::None,
            precedence: Precedence::None,
        },
        TokenType::String => ParseRule {
            prefix: ParseFn::String,
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
            prefix: ParseFn::Literal,
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
            prefix: ParseFn::Literal,
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
            prefix: ParseFn::Literal,
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
    let mut out = format!("<Compiler> [Line {}] Error ", token.line);
    if token.kind == TokenType::Eof {
        out.push_str("at end");
    } else {
        out.push_str(format!("at '{}'", token.string).as_str());
    }
    out.push(':');
    out.push_str(message);
    out.into()
}
