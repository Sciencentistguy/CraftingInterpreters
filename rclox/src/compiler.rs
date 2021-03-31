use std::rc::Rc;

use crate::chunk::Chunk;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::opcode::OpCode;
use crate::value::Value;
use crate::Result;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
#[rustfmt::skip]
enum Precedence {
    Assignment = 0, // =
    Or = 1, // OR
    And = 2, // AND
    Equality = 3, // == !=
    Comparison = 4, // < > <= >=
    Term = 5, // + -
    Factor = 6, // * /
    Unary = 7, // ! -
    Call = 8, // . ()
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
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
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
    And,
    Or,

    None,
}

struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

struct Local {
    name: Rc<String>,
    depth: Option<usize>,
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
    current: Token<'source>,
    previous: Token<'source>,
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
    fn emit_instruction(&mut self, instruction: OpCode) {
        self.chunk
            .write_instruction(instruction, self.previous.line);
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
        let c = self.make_constant(value);
        //self.emit_instruction(c as u8);
        self.emit_instruction(OpCode::Constant(c));
        Ok(())
    }

    #[inline]
    fn make_constant(&mut self, value: Value) -> usize {
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
            TokenType::Bang => self.emit_instruction(OpCode::Not),
            TokenType::Minus => self.emit_instruction(OpCode::Negate),
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
                self.emit_instruction(OpCode::Equal);
                self.emit_instruction(OpCode::Not);
            }
            TokenType::EqualEqual => self.emit_instruction(OpCode::Equal),
            TokenType::Greater => self.emit_instruction(OpCode::Greater),
            TokenType::GreaterEqual => {
                self.emit_instruction(OpCode::Less);
                self.emit_instruction(OpCode::Not);
            }
            TokenType::Less => self.emit_instruction(OpCode::Less),
            TokenType::LessEqual => {
                self.emit_instruction(OpCode::Greater);
                self.emit_instruction(OpCode::Not);
            }
            TokenType::Plus => self.emit_instruction(OpCode::Add),
            TokenType::Minus => self.emit_instruction(OpCode::Subtract),
            TokenType::Star => self.emit_instruction(OpCode::Multiply),
            TokenType::Slash => self.emit_instruction(OpCode::Divide),
            _ => unreachable!(),
        };
        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        match self.previous.kind {
            TokenType::False => self.emit_instruction(OpCode::False),
            TokenType::True => self.emit_instruction(OpCode::True),
            TokenType::Nil => self.emit_instruction(OpCode::Nil),
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
            ParseFn::And => self.and(),
            ParseFn::Or => self.or(),
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
        if can_assign && self.match_token(TokenType::Equal)? {
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
        if self.match_token(TokenType::Var)? {
            self.variable_declaration()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<()> {
        if self.match_token(TokenType::Print)? {
            self.print_statement()
        } else if self.match_token(TokenType::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else if self.match_token(TokenType::If)? {
            self.if_statement()
        } else if self.match_token(TokenType::While)? {
            self.while_statement()
        } else if self.match_token(TokenType::For)? {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<()> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after condition.")?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instruction(OpCode::Pop);
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::Jump(0));

        self.patch_jump(then_jump);
        self.emit_instruction(OpCode::Pop);

        if self.match_token(TokenType::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start = self.chunk.code.len();

        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after condition.")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0));

        self.emit_instruction(OpCode::Pop);
        self.statement()?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_instruction(OpCode::Pop);
        Ok(())
    }

    fn for_statement(&mut self) -> Result<()> {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expected'(' after 'for'.")?;

        // Initialiser
        if self.match_token(TokenType::Semicolon)? {
            // No initialiser
        } else if self.match_token(TokenType::Var)? {
            self.variable_declaration()?;
        } else {
            self.expression_statement()?;
        }

        // self.consume(TokenType::Semicolon, "Expected ';'.")?;

        let mut loop_start = self.chunk.code.len();

        let mut exit_jump = None;

        // Loop condition
        if !self.match_token(TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expected ';' after loop condition.")?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0)));
            self.emit_instruction(OpCode::Pop);
        }

        // self.consume(TokenType::Semicolon, "Expected ';'.")?;

        // Increment
        if !self.match_token(TokenType::RightParen)? {
            let body_jump = self.emit_jump(OpCode::Jump(0));
            let increment_start = self.chunk.code.len();
            self.expression()?;
            self.emit_instruction(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expected ')' after 'for' clauses.")?;

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement()?;
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_instruction(OpCode::Pop);
        }

        self.end_scope();

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.chunk.code.len() - loop_start + 1;

        self.emit_instruction(OpCode::Loop(offset));
    }

    /// Returns the index of the instruction emmited
    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        match instruction {
            OpCode::JumpIfFalse(_) => {
                self.emit_instruction(OpCode::JumpIfFalse(usize::MAX));
                self.chunk.code.len() - 2 // -2 because we have to set pc to the instruction before the one we want executed next
            }
            OpCode::Jump(_) => {
                self.emit_instruction(OpCode::Jump(usize::MAX));
                self.chunk.code.len() - 2 // -2 because we have to set pc to the instruction before the one we want executed next
            }
            _ => unreachable!("Invalid jump instruction"),
        }
    }

    /// Takes the index of the instruction to patch
    fn patch_jump(&mut self, offset: usize) {
        let distance = self.chunk.code.len() - offset;

        match self.chunk.code[offset] {
            OpCode::Jump(ref mut x) | OpCode::JumpIfFalse(ref mut x) => {
                assert_eq!(*x, usize::MAX);
                *x = distance;
            }
            _ => {
                unreachable!(
                    "Cannot patch a non-jump instruction '{:?}'",
                    self.chunk.code[offset]
                );
            }
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
            && self.compiler.locals[self.compiler.local_count - 1]
                .depth
                .unwrap() as usize
                > self.compiler.scope_depth
        {
            self.emit_instruction(OpCode::Pop);
            self.compiler.local_count -= 1;
        }
    }

    #[inline]
    pub fn match_token(&mut self, kind: TokenType) -> Result<bool> {
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
        self.emit_instruction(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        self.emit_instruction(OpCode::Pop);
        Ok(())
    }

    fn parse_variable(&mut self, message: &str) -> Result<usize> {
        self.consume(TokenType::Identifier, message)?;
        self.declare_variable()?;
        if self.compiler.scope_depth > 0 {
            return Ok(0);
        }
        let name = self.previous.string.to_string();
        Ok(self.identifier_constant(name))
    }

    fn identifier_constant(&mut self, name: String) -> usize {
        self.make_constant(Value::String(Rc::new(name)))
    }

    fn variable_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expected a varaible name.")?;
        if self.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_instruction(OpCode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialised();
            return;
        }
        self.emit_instruction(OpCode::DefineGlobal(global));
        //self.emit_instruction(global);
    }

    fn mark_initialised(&mut self) {
        if self.compiler.local_count == 0 {
            panic!("No local declared, yet attempting to mark intialised")
        }
        self.compiler.locals[self.compiler.local_count - 1].depth = Some(self.compiler.scope_depth);
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }
        let name = self.previous.string;
        for local in self.compiler.locals.iter().rev() {
            if local.depth.is_some() && local.depth.unwrap() < self.compiler.scope_depth {
                // local.depth != -1 is always true, unsigned type
                break;
            }
            if *local.name == name {
                return Err(self.error_at_previous(
                    format!(
                        "There is already a variable with name {} in this scope",
                        name
                    )
                    .as_str(),
                ));
            }
        }
        self.add_local(name)
    }

    fn add_local(&mut self, name: &str) -> Result<()> {
        let _depth = self.compiler.scope_depth;
        self.compiler.locals.push(Local {
            name: Rc::new(name.to_string()),
            depth: None,
        });
        self.compiler.local_count += 1;
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let name = self.previous.string.to_string();
        self.named_variable(name, can_assign)
    }

    fn named_variable(&mut self, name: String, can_assign: bool) -> Result<()> {
        let arg = self.resolve_local(self.compiler, name.as_str())?;
        let assignable = can_assign && self.match_token(TokenType::Equal)?;
        match arg {
            Some(x) if assignable => {
                self.expression()?;
                self.emit_instruction(OpCode::SetLocal(x))
            }
            Some(x) if !assignable => self.emit_instruction(OpCode::GetLocal(x)),
            None if assignable => {
                self.expression()?;
                let x = self.identifier_constant(name);
                self.emit_instruction(OpCode::SetGlobal(x))
            }
            None if !assignable => {
                let x = self.identifier_constant(name);
                self.emit_instruction(OpCode::GetGlobal(x))
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn resolve_local(&self, compiler: &Compiler, name: &str) -> Result<Option<usize>> {
        for (i, local) in compiler.locals.iter().enumerate().rev() {
            if name == *local.name {
                if local.depth.is_none() {
                    return Err(self
                        .error_at_previous("Cannot read local variable in its own initialiser."));
                }
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    fn and(&mut self) -> Result<()> {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instruction(OpCode::Pop);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self) -> Result<()> {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse(0));

        let end_jump = self.emit_jump(OpCode::Jump(0));

        self.patch_jump(else_jump);
        self.emit_instruction(OpCode::Pop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);

        Ok(())
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
        while !parser.match_token(TokenType::Eof)? {
            parser.declaration()?;
        }
        parser.emit_instruction(OpCode::Return); // endCompiler()
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
            precedence: Precedence::Equality,
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
            infix: ParseFn::And,
            precedence: Precedence::And,
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
            infix: ParseFn::Or,
            precedence: Precedence::Or,
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
        out.push_str("at end: ");
    } else {
        out.push_str(format!("at '{}': ", token.string).as_str());
    }
    out.push_str(message);
    out.into()
}
