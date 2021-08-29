use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::RcloxError;
use crate::instruction::Instruction;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::value::Value;
use crate::Result;

pub fn compile(source: &str, output_chunk: &mut Chunk) -> Result<()> {
    println!("Starting compilation");
    let lexer = Lexer::new(source);
    let mut targets = vec![Target::new()];
    let mut parser = Compiler::new(lexer, output_chunk, &mut targets);
    parser.advance()?;
    while !parser.advance_if_token_matches(TokenType::Eof)? {
        parser.declaration()?;
    }
    let last_line_num = parser.output_chunk.code.last().map(|x| x.line);
    parser.emit_instruction(Instruction::Return); // endCompiler()
    if let Some(x) = last_line_num {
        output_chunk.code.last_mut().unwrap().line = x + 1;
    }
    crate::debug::disassemble_chunk(output_chunk, "code");
    println!("Finished compilation.");
    Ok(())
}

#[rustfmt::skip]
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
#[repr(u8)]
enum Precedence {
    Assignment = 0, // =
    Or = 1,         // OR
    And = 2,        // AND
    Equality = 3,   // == !=
    Comparison = 4, // < > <= >=
    Term = 5,       // + -
    Factor = 6,     // *         /
    Unary = 7,      // ! -
    Call = 8,       // . ()
    Primary = 9,
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
                unreachable!(
                    "Primary is the highest precedence, cannot use Precedence::one_higher()"
                )
            }
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
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Option<Precedence>,
}

#[derive(Debug)]
struct LocalVariable {
    name: Rc<String>,
    depth: Option<usize>,
}

struct Target {
    locals: Vec<LocalVariable>,
    scope_depth: usize,
}

impl Target {
    fn new() -> Self {
        Target {
            locals: Vec::with_capacity(u8::MAX as usize),
            scope_depth: 0,
        }
    }
}

struct Compiler<'source> {
    lexer: Lexer<'source>,
    current_token: Token<'source>,
    previous_token: Token<'source>,
    output_chunk: &'source mut Chunk,
    targets: &'source mut Vec<Target>,
}

impl<'source> Compiler<'source> {
    fn new(
        lexer: Lexer<'source>,
        output_chunk: &'source mut Chunk,
        targets: &'source mut Vec<Target>,
    ) -> Self {
        Self {
            lexer,
            current_token: Token::null(),
            previous_token: Token::null(),
            output_chunk,
            targets,
        }
    }

    fn target(&self) -> &Target {
        self.targets.last().expect("Internal error: no target")
    }

    fn target_mut(&mut self) -> &mut Target {
        self.targets.last_mut().expect("Internal error: no target")
    }

    fn advance(&mut self) -> Result<()> {
        std::mem::swap(&mut self.previous_token, &mut self.current_token);
        self.current_token = self.lexer.lex_token()?;
        Ok(())
    }

    fn expect_token(&mut self, kind: TokenType, error_message: &'static str) -> Result<()> {
        if self.current_token.kind == kind {
            self.advance()?;
            Ok(())
        } else {
            Err(self.error_at_current(error_message))
        }
    }

    #[inline]
    fn emit_instruction(&mut self, instruction: Instruction) {
        self.output_chunk
            .write_instruction(instruction, self.previous_token.line);
    }

    fn expression(&mut self) -> Result<()> {
        self.compile_with_precedence(Some(Precedence::Assignment))
    }

    fn number(&mut self) -> Result<()> {
        let value = self
            .previous_token
            .string
            .parse()
            .expect("Invalid number token");
        self.emit_constant(Value::Number(value));
        Ok(())
    }

    fn emit_constant(&mut self, value: Value) {
        let c = self.make_constant(value);
        self.emit_instruction(Instruction::Constant(c));
    }

    #[inline]
    fn make_constant(&mut self, value: Value) -> usize {
        self.output_chunk.add_constant(value)
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::RightParen, "Expected ')' after expression.")
    }

    fn unary(&mut self) -> Result<()> {
        let kind = self.previous_token.kind;

        self.compile_with_precedence(Some(Precedence::Unary))?;

        match kind {
            TokenType::Bang => self.emit_instruction(Instruction::Not),
            TokenType::Minus => self.emit_instruction(Instruction::Negate),
            x => unreachable!("{:?} is not a unary operator", x),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let operator_kind = self.previous_token.kind;
        let rule = get_rule(operator_kind);

        self.compile_with_precedence(rule.precedence.map(|x| x.one_higher()))?;

        match operator_kind {
            TokenType::BangEqual => {
                self.emit_instruction(Instruction::Equal);
                self.emit_instruction(Instruction::Not);
            }
            TokenType::EqualEqual => self.emit_instruction(Instruction::Equal),
            TokenType::Greater => self.emit_instruction(Instruction::Greater),
            TokenType::GreaterEqual => {
                self.emit_instruction(Instruction::Less);
                self.emit_instruction(Instruction::Not);
            }
            TokenType::Less => self.emit_instruction(Instruction::Less),
            TokenType::LessEqual => {
                self.emit_instruction(Instruction::Greater);
                self.emit_instruction(Instruction::Not);
            }
            TokenType::Plus => self.emit_instruction(Instruction::Add),
            TokenType::Minus => self.emit_instruction(Instruction::Subtract),
            TokenType::Star => self.emit_instruction(Instruction::Multiply),
            TokenType::Slash => self.emit_instruction(Instruction::Divide),
            x => unreachable!("{:?} is not a binary operator", x),
        };
        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        match self.previous_token.kind {
            TokenType::False => self.emit_instruction(Instruction::False),
            TokenType::True => self.emit_instruction(Instruction::True),
            TokenType::Nil => self.emit_instruction(Instruction::Nil),
            x => unreachable!("{:?} is not a literal", x),
        }
        Ok(())
    }

    #[allow(clippy::unnecessary_wraps)]
    fn string(&mut self) -> Result<()> {
        self.emit_constant(Value::String(Rc::new(
            self.previous_token.string.trim_matches('"').to_string(),
        )));
        Ok(())
    }

    fn parse_with_parsefn(&mut self, parse_fn: ParseFn, can_assign: bool) -> Result<()> {
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
        }
    }

    fn compile_with_precedence(&mut self, precedence: Option<Precedence>) -> Result<()> {
        let precedence = precedence.expect("Precedence should not be none");

        self.advance()?;

        let prefix_parsefn = get_rule(self.previous_token.kind)
            .prefix
            .ok_or_else(|| self.error_at_previous("Expected expression."))?;

        let can_assign = precedence <= Precedence::Assignment;

        self.parse_with_parsefn(prefix_parsefn, can_assign)?;

        while get_rule(self.current_token.kind)
            .precedence
            .map(|x| precedence <= x)
            .unwrap_or(false)
        {
            self.advance()?;
            let infix_rule = get_rule(self.previous_token.kind)
                .infix
                .expect("should be unreachable");
            self.parse_with_parsefn(infix_rule, can_assign)?;
        }
        if can_assign && self.advance_if_token_matches(TokenType::Equal)? {
            return Err(self.error_at_previous("Invalid assignment target"));
        }
        Ok(())
    }

    fn error_at_current(&self, message: &str) -> RcloxError {
        error_at(&self.current_token, message)
    }

    fn error_at_previous(&self, message: &str) -> RcloxError {
        error_at(&self.previous_token, message)
    }

    pub fn declaration(&mut self) -> Result<()> {
        if self.advance_if_token_matches(TokenType::Var)? {
            self.variable_declaration()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<()> {
        if self.advance_if_token_matches(TokenType::Print)? {
            self.print_statement()
        } else if self.advance_if_token_matches(TokenType::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else if self.advance_if_token_matches(TokenType::If)? {
            self.if_statement()
        } else if self.advance_if_token_matches(TokenType::While)? {
            self.while_statement()
        } else if self.advance_if_token_matches(TokenType::For)? {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<()> {
        self.expect_token(TokenType::LeftParen, "Expected '(' after 'if'.")?;

        // compile condition
        self.expression()?;

        self.expect_token(TokenType::RightParen, "Expected ')' after condition.")?;

        // XXX: The 0 here is never read, and is only there because it has to be
        let then_jump_location = self.emit_jump_instruction(Instruction::JumpIfFalse(0));
        // TODO maybe have the jumps pop? instead of dancing around the pop instructions
        // like this
        self.emit_instruction(Instruction::Pop);

        // compile contents of if statement
        self.statement()?;

        // TODO: it should be possible to omit the second jump instruction if there is
        // no else block
        // XXX: The 0 here is never read, and is only there because it has to be
        let else_jump_location = self.emit_jump_instruction(Instruction::Jump(0));

        self.patch_jump_target(then_jump_location);

        self.emit_instruction(Instruction::Pop);

        if self.advance_if_token_matches(TokenType::Else)? {
            self.statement()?;
        }

        self.patch_jump_target(else_jump_location);

        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start_idx = self.output_chunk.code.len();

        self.expect_token(TokenType::LeftParen, "Expected '(' after 'while'.")?;

        // compile condition
        self.expression()?;
        self.expect_token(TokenType::RightParen, "Expected ')' after condition.")?;

        // XXX: The 0 here is never read, and is only there because it has to be
        let exit_jump_location = self.emit_jump_instruction(Instruction::JumpIfFalse(0));
        self.emit_instruction(Instruction::Pop);

        self.statement()?;

        self.emit_loop(loop_start_idx);

        self.patch_jump_target(exit_jump_location);
        self.emit_instruction(Instruction::Pop);
        Ok(())
    }

    fn for_statement(&mut self) -> Result<()> {
        self.begin_scope();

        self.expect_token(TokenType::LeftParen, "Expected'(' after 'for'.")?;

        // Initialiser
        if self.advance_if_token_matches(TokenType::Semicolon)? {
            // No initialiser
        } else if self.advance_if_token_matches(TokenType::Var)? {
            self.variable_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.output_chunk.code.len();

        let mut exit_jump = None;

        // Loop condition
        if !self.advance_if_token_matches(TokenType::Semicolon)? {
            // Compile the loop condition
            self.expression()?;

            self.expect_token(TokenType::Semicolon, "Expected ';' after loop condition.")?;

            // As the exit jump is only emitted if there is a loop condition, it is actually
            // more efficiont to use a `for (;;)` loop than a `while (true)`
            // loop. TODO: optimisation: special-case `while (true)` to do this
            // as well?
            exit_jump = Some(self.emit_jump_instruction(Instruction::JumpIfFalse(0)));
            self.emit_instruction(Instruction::Pop);
        }

        // Increment
        if !self.advance_if_token_matches(TokenType::RightParen)? {
            let body_jump = self.emit_jump_instruction(Instruction::Jump(0));
            let increment_start = self.output_chunk.code.len();
            self.expression()?;
            self.emit_instruction(Instruction::Pop);
            self.expect_token(TokenType::RightParen, "Expected ')' after 'for' clauses.")?;

            self.emit_loop(loop_start);

            // if there is an increment clause, we need to move the start of the loop so it
            // is jumped to every time
            loop_start = increment_start;
            self.patch_jump_target(body_jump);
        }

        self.statement()?;

        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump_target(exit_jump);
            self.emit_instruction(Instruction::Pop);
        }

        self.end_scope();

        Ok(())
    }

    fn emit_loop(&mut self, target_index: usize) {
        let offset = self.output_chunk.code.len() - target_index + 1;

        self.emit_instruction(Instruction::Loop(offset));
    }

    /// Returns the index of the jump instruction emitted
    fn emit_jump_instruction(&mut self, instruction: Instruction) -> usize {
        // TODO: remove the usize::MAX sentinel value, use the type system
        match instruction {
            Instruction::JumpIfFalse(_) => {
                self.emit_instruction(Instruction::JumpIfFalse(usize::MAX));
            }
            Instruction::Jump(_) => {
                self.emit_instruction(Instruction::Jump(usize::MAX));
            }
            _ => unreachable!("Invalid jump instruction"),
        };
        self.output_chunk.code.len() - 1
    }

    /// Takes the index of the jump instruction to patch. {anics if this is not
    /// a jump instruction
    fn patch_jump_target(&mut self, offset: usize) {
        // We have to subtract 1 here because we want to set the pc to the instruction
        // before the one we want executed next
        let distance = self.output_chunk.code.len() - offset - 1;

        match self.output_chunk[offset].instruction {
            Instruction::Jump(ref mut x) | Instruction::JumpIfFalse(ref mut x) => {
                // TODO: remove the usize::MAX sentinel value, use the type system
                assert_eq!(*x, usize::MAX);
                *x = distance;
            }
            _ => {
                unreachable!(
                    "Cannot patch a non-jump instruction '{:?}'",
                    self.output_chunk.code[offset]
                );
            }
        }
    }

    fn block(&mut self) -> Result<()> {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration()?;
        }
        self.expect_token(TokenType::RightBrace, "Expected '}' after block.")
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.target_mut().scope_depth += 1;
    }

    #[inline]
    fn end_scope(&mut self) {
        if cfg!(debug_asserts) {
            self.target_mut().scope_depth = self
                .target()
                .scope_depth
                .checked_sub(1)
                .expect("attempted to end the global scope");
        } else {
            self.target_mut().scope_depth -= 1;
        }

        while self
            .target()
            .locals
            .last()
            .map(|x| {
                x.depth
                    .expect("depth should be some after variable is marked as intialised")
            })
            .map(|x| x > self.target().scope_depth)
            .unwrap_or(false)
        {
            self.emit_instruction(Instruction::Pop);
            self.target_mut().locals.pop();
        }
    }

    #[inline]
    pub fn advance_if_token_matches(&mut self, kind: TokenType) -> Result<bool> {
        if !self.check(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    #[inline]
    fn check(&self, kind: TokenType) -> bool {
        self.current_token.kind == kind
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expected ';' after value.")?;
        self.emit_instruction(Instruction::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expected ';' after expression.")?;
        self.emit_instruction(Instruction::Pop);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<Option<usize>> {
        self.expect_token(TokenType::Identifier, "Expected a variable name.")?;
        self.declare_variable()?;
        if self.target().scope_depth > 0 {
            return Ok(None);
        }
        let name = self.previous_token.string.to_string();
        Ok(Some(self.identifier_constant(name)))
    }

    /// Add a constant with the value of `name`, used for storing the names of
    /// global variables.
    fn identifier_constant(&mut self, name: String) -> usize {
        self.make_constant(Value::String(Rc::new(name)))
    }

    fn variable_declaration(&mut self) -> Result<()> {
        // if scope depth is more than zero, then variable_name_id = None
        // else, it is the index in the constants table pointing to the name of the
        // variable
        let variable_name_id = self.parse_variable()?;
        if self.advance_if_token_matches(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_instruction(Instruction::Nil);
        }
        self.expect_token(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        self.define_variable(variable_name_id);
        Ok(())
    }

    fn define_variable(&mut self, variable_name_id: Option<usize>) {
        // if variable_name_id is None, this is a local variable.
        // if it is Some(x), x is the index of the name of the global variable in the
        // constants table
        //
        // The c version checks scope_depth twice, instead of using the value of
        // variable_name_id. This check is maintained with an assert, but should
        // be unnecessary
        match variable_name_id {
            Some(id) => self.emit_instruction(Instruction::DefineGlobal(id)),
            None => {
                // XXX: asserts are slow?
                assert!(self.target().scope_depth > 0);
                self.initialise_local_variable();
            }
        }
    }

    fn initialise_local_variable(&mut self) {
        self.target_mut()
            .locals
            .last_mut()
            .unwrap_or_else(|| unreachable!("No local declared, yet attempting to mark intialised"))
            .depth = Some(self.target().scope_depth);
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.target().scope_depth == 0 {
            return Ok(());
        }

        let variable_name = self.previous_token.string;

        // Check that a variable does not already exist with this name in the current
        // scope.
        for local_var in self.target().locals.iter().rev() {
            // If search depth is below current scope depth, we can stop searching, as we
            // have not found a conflict.
            if local_var
                .depth
                .expect("depth should be some after variable is marked as intialised")
                < self.target().scope_depth
            {
                break;
            }

            if local_var.name.as_str() == variable_name {
                return Err(self.error_at_previous(
                    format!(
                        "There is already a variable with name {} in this scope",
                        variable_name
                    )
                    .as_str(),
                ));
            }
        }

        self.add_uninitialised_local_variable(variable_name);

        Ok(())
    }

    fn add_uninitialised_local_variable(&mut self, name: &str) {
        //let depth = self.target().scope_depth;
        self.target_mut().locals.push(LocalVariable {
            name: Rc::new(name.to_string()),
            depth: None,
        });
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let name = self.previous_token.string.to_string();
        self.named_variable(name, can_assign)
    }

    fn named_variable(&mut self, name: String, can_assign: bool) -> Result<()> {
        let arg = self.resolve_local(self.target(), name.as_str())?;
        let assignable = can_assign && self.advance_if_token_matches(TokenType::Equal)?;
        match arg {
            Some(x) if assignable => {
                self.expression()?;
                self.emit_instruction(Instruction::SetLocal(x))
            }
            Some(x) if !assignable => self.emit_instruction(Instruction::GetLocal(x)),
            None if assignable => {
                self.expression()?;
                let x = self.identifier_constant(name);
                self.emit_instruction(Instruction::SetGlobal(x))
            }
            None if !assignable => {
                let x = self.identifier_constant(name);
                self.emit_instruction(Instruction::GetGlobal(x))
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn resolve_local(&self, target: &Target, name: &str) -> Result<Option<usize>> {
        for (i, local) in target.locals.iter().enumerate().rev() {
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
        let end_jump = self.emit_jump_instruction(Instruction::JumpIfFalse(0));
        self.emit_instruction(Instruction::Pop);
        self.compile_with_precedence(Some(Precedence::And))?;
        self.patch_jump_target(end_jump);
        Ok(())
    }

    fn or(&mut self) -> Result<()> {
        let else_jump = self.emit_jump_instruction(Instruction::JumpIfFalse(0));

        let end_jump = self.emit_jump_instruction(Instruction::Jump(0));

        self.patch_jump_target(else_jump);
        self.emit_instruction(Instruction::Pop);

        self.compile_with_precedence(Some(Precedence::Or))?;
        self.patch_jump_target(end_jump);

        Ok(())
    }
}

fn get_rule(kind: TokenType) -> ParseRule {
    match kind {
        TokenType::LeftParen => ParseRule {
            prefix: Some(ParseFn::Grouping),
            infix: None,
            precedence: None,
        },
        TokenType::RightParen => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::LeftBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::RightBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Comma => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Dot => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Minus => ParseRule {
            prefix: Some(ParseFn::Unary),
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Term),
        },
        TokenType::Plus => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Term),
        },
        TokenType::Semicolon => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Slash => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Factor),
        },
        TokenType::Star => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Factor),
        },
        TokenType::Bang => ParseRule {
            prefix: Some(ParseFn::Unary),
            infix: None,
            precedence: None,
        },
        TokenType::BangEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Equality),
        },
        TokenType::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::EqualEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Equality),
        },
        TokenType::Greater => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Comparison),
        },
        TokenType::GreaterEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Comparison),
        },
        TokenType::Less => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Comparison),
        },
        TokenType::LessEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Some(Precedence::Comparison),
        },
        TokenType::Identifier => ParseRule {
            prefix: Some(ParseFn::Variable),
            infix: None,
            precedence: None,
        },
        TokenType::String => ParseRule {
            prefix: Some(ParseFn::String),
            infix: None,
            precedence: None,
        },
        TokenType::Number => ParseRule {
            prefix: Some(ParseFn::Number),
            infix: None,
            precedence: None,
        },
        TokenType::And => ParseRule {
            prefix: None,
            infix: Some(ParseFn::And),
            precedence: Some(Precedence::And),
        },
        TokenType::Class => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Else => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::False => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: None,
        },
        TokenType::For => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Fun => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::If => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Nil => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: None,
        },
        TokenType::Or => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Or),
            precedence: Some(Precedence::Or),
        },
        TokenType::Print => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Return => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Super => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::This => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::True => ParseRule {
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: None,
        },
        TokenType::Var => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::While => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Eof => ParseRule {
            prefix: None,
            infix: None,
            precedence: None,
        },
        TokenType::Null => unreachable!(),
    }
}

fn error_at(token: &Token, message: &str) -> RcloxError {
    RcloxError::Compiler {
        string: token.string.to_owned(),
        line: token.line,
        message: message.to_owned(),
    }
}
