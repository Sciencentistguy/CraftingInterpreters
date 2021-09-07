//! The parser and compiler.
//!
//! See <https://craftinginterpreters.com/appendix-i.html> for the Lox grammar.

use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::RcloxError;
use crate::instruction::Instruction;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::value::LoxFunction;
use crate::value::Value;
use crate::vm::CallFrameCode;
use crate::Result;

/// Compile source code to a chunk.
pub fn compile(source: &str) -> Result<Vec<Target>> {
    println!("Starting compilation");
    let lexer = Lexer::new(source);
    let mut targets = vec![Target::new_script()];
    let mut compiler = Compiler::new(lexer, &mut targets);
    compiler.advance()?;
    while !compiler.advance_if_token_matches(TokenType::Eof)? {
        compiler.declaration()?;
    }

    let last_line_num = compiler.target().chunk().last().map(|x| x.line);

    // The script returns nothing, but the vm will still attempt to look at the
    // stack to find the return value, so insert a nil
    compiler.emit_instruction(Instruction::Nil);
    compiler.emit_instruction(Instruction::Return);

    if let Some(x) = last_line_num {
        compiler.target_mut().chunk_mut().last_mut().unwrap().line = x + 1;
    }
    for target in &targets {
        let name = match target.output {
            TargetOutput::Script(_) => "<program>".to_owned(),
            TargetOutput::Function(ref x) => format!("<fn {}>", x.name),
        };
        crate::debug::disassemble_chunk(target.chunk(), name.as_str());
    }
    println!("Finished compilation.");
    Ok(targets)
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
#[repr(u8)]
/// The order of precedence of the operations in Lox
///
/// See https://craftinginterpreters.com/appendix-i.html
enum Precedence {
    /// The precedence of `=`
    Assignment = 0,
    /// The precedence of `or`
    Or = 1,
    /// The precedence of `and`
    And = 2,
    /// The precedence of `==`, `!=`
    Equality = 3,
    /// The precedence of `<`, `>`, `<=`, `>=`
    Comparison = 4,
    /// The precedence of `+`, `binary -`
    Term = 5,
    /// The precedence of `*`, `/`
    Factor = 6,
    /// The precedence of `!`, `unary -`
    Unary = 7,
    /// The precedence of `.`, `()`
    Call = 8,
    /// The precedence of primaries
    Primary = 9,
}

impl Precedence {
    /// Get the next highest precedence. Panics if called on
    /// Precedence::Primary, as that is the highest precedence
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
/// Enum allowing dynamic dispatch of parsing functions
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

/// A row in the pratt parser table
///
/// See https://craftinginterpreters.com/compiling-expressions.html#a-pratt-parser
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Option<Precedence>,
}

#[derive(Debug)]
/// A local variable, with a (reference-counted) name, and depth
// This option exists to represent an interim state.
// TODO: remove it, use the type system better.
pub struct LocalVariable {
    name: Rc<String>,
    depth: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum TargetOutput {
    Script(Chunk),
    Function(LoxFunction),
}

impl TargetOutput {
    /// Clone this TargetOutput into a CallFrameCode, possibly allocating a new
    /// Rc
    pub fn to_callframe_code(&self) -> CallFrameCode {
        match self {
            Self::Script(c) => CallFrameCode::Script(c.clone()),
            Self::Function(f) => CallFrameCode::Function(Rc::new(f.clone())),
        }
    }

    /// Extract the inner chunk
    pub fn chunk(&self) -> &Chunk {
        match self {
            TargetOutput::Script(ref x) => x,
            TargetOutput::Function(ref x) => &x.chunk,
        }
    }

    /// Extract the inner chunk mutably
    fn chunk_mut(&mut self) -> &mut Chunk {
        match self {
            TargetOutput::Script(ref mut x) => x,
            TargetOutput::Function(ref mut x) => &mut x.chunk,
        }
    }

    /// Extract the inner function, panicking if `self` is a `Script(_)`
    fn as_function(&self) -> &LoxFunction {
        match self {
            TargetOutput::Script(_) => {
                unreachable!("called as_function on a TargetOutput::Script(_)")
            }
            TargetOutput::Function(x) => x,
        }
    }

    /// Extract the inner function mutably, panicking if `self` is a `Script(_)`
    fn as_function_mut(&mut self) -> &mut LoxFunction {
        match self {
            TargetOutput::Script(_) => {
                unreachable!("called as_function on a TargetOutput::Script(_)")
            }
            TargetOutput::Function(x) => x,
        }
    }
}

/// A set of local variables and a scope depth. Somewhat equivalent to a
/// `compiler` in the C version
#[derive(Debug)]
pub struct Target {
    pub locals: Vec<LocalVariable>,
    pub scope_depth: usize,
    pub output: TargetOutput,
}

impl Target {
    /// Creates a new `Target` with a `Script(_)` output.
    fn new_script() -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        locals.push(LocalVariable {
            name: Rc::new("".to_owned()),
            depth: Some(0),
        });
        // FIXME: A hack.
        // This is to use up the first stack slot, which is unused until methods are
        // added
        let mut c = Chunk::new();
        c.write_instruction(Instruction::Nil, 0);
        Self {
            locals,
            scope_depth: 0,
            output: TargetOutput::Script(c),
        }
    }

    /// Creates a new `Target` with a `Function(_)` output.
    fn new_function(name: String) -> Self {
        let locals = Vec::with_capacity(u8::MAX as usize);
        let function = LoxFunction::with_name(name);

        Self {
            locals,
            scope_depth: 0,
            output: TargetOutput::Function(function),
        }
    }

    /// Extract the inner chunk
    pub fn chunk(&self) -> &Chunk {
        match self.output {
            TargetOutput::Script(ref x) => x,
            TargetOutput::Function(ref x) => &x.chunk,
        }
    }

    /// Extract the inner chunk mutably
    fn chunk_mut(&mut self) -> &mut Chunk {
        match self.output {
            TargetOutput::Script(ref mut x) => x,
            TargetOutput::Function(ref mut x) => &mut x.chunk,
        }
    }
}

/// The compiler state machine
struct Compiler<'source> {
    lexer: Lexer<'source>,
    current_token: Token<'source>,
    previous_token: Token<'source>,
    targets: &'source mut Vec<Target>,
}

impl<'source> Compiler<'source> {
    /// Create a new, blank, `Compiler`
    fn new(lexer: Lexer<'source>, targets: &'source mut Vec<Target>) -> Self {
        Self {
            lexer,
            current_token: Token::null(),
            previous_token: Token::null(),
            targets,
        }
    }

    /// Returns a reference to the top of the target stack
    fn target(&self) -> &Target {
        self.targets.last().expect("Internal error: no target")
    }

    /// Returns a mutable reference to the top of the target stack
    fn target_mut(&mut self) -> &mut Target {
        self.targets.last_mut().expect("Internal error: no target")
    }

    /// Consume the next token from the Lexer, and advance `self.previous_token`
    /// and `self.current_token`
    fn advance(&mut self) -> Result<()> {
        std::mem::swap(&mut self.previous_token, &mut self.current_token);
        self.current_token = self.lexer.lex_token()?;
        Ok(())
    }

    /// Consume a token, erroring if it is not the expected type
    fn expect_token(&mut self, kind: TokenType, error_message: &'static str) -> Result<()> {
        if self.current_token.kind == kind {
            self.advance()?;
            Ok(())
        } else {
            Err(self.error_at_current(error_message))
        }
    }

    #[inline]
    /// Output an instruction to the chunk
    fn emit_instruction(&mut self, instruction: Instruction) {
        let previous_line = self.previous_token.line;
        self.target_mut()
            .chunk_mut()
            .write_instruction(instruction, previous_line);
    }

    /// Compile an expression
    fn expression(&mut self) -> Result<()> {
        self.compile_with_precedence(Some(Precedence::Assignment))
    }

    /// Compile a number literal
    fn number(&mut self) -> Result<()> {
        let value = self
            .previous_token
            .span
            .parse()
            .expect("Invalid number token");
        self.emit_instruction(Instruction::Constant(Value::Number(value)));
        Ok(())
    }

    /// Compile a grouping (brackets in operator precedence)
    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::RightParen, "Expected ')' after expression.")
    }

    /// Compile a unary expression
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

    /// Compile a binary expression
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

    /// Compile a literal expression
    fn literal(&mut self) -> Result<()> {
        match self.previous_token.kind {
            TokenType::False => self.emit_instruction(Instruction::False),
            TokenType::True => self.emit_instruction(Instruction::True),
            TokenType::Nil => self.emit_instruction(Instruction::Nil),
            x => unreachable!("{:?} is not a literal", x),
        }
        Ok(())
    }

    /// Compile a string literal
    fn string(&mut self) -> Result<()> {
        self.emit_instruction(Instruction::Constant(Value::String(Rc::new(
            self.previous_token.span.trim_matches('"').to_string(),
        ))));
        Ok(())
    }

    /// Dynamically dispatch the correct function to parse at the current
    /// precedence
    fn compile_with_parsefn(&mut self, parse_fn: ParseFn, can_assign: bool) -> Result<()> {
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
            ParseFn::Call => self.call(),
        }
    }

    /// Compile an expression of the given precedence
    fn compile_with_precedence(&mut self, precedence: Option<Precedence>) -> Result<()> {
        let precedence = precedence.expect("Precedence should not be none");

        self.advance()?;

        let prefix_parsefn = get_rule(self.previous_token.kind)
            .prefix
            .ok_or_else(|| self.error_at_previous("Expected expression."))?;

        let can_assign = precedence <= Precedence::Assignment;

        self.compile_with_parsefn(prefix_parsefn, can_assign)?;

        while get_rule(self.current_token.kind)
            .precedence
            .map(|x| precedence <= x)
            .unwrap_or(false)
        {
            self.advance()?;
            let infix_rule = get_rule(self.previous_token.kind)
                .infix
                .expect("should be unreachable");
            self.compile_with_parsefn(infix_rule, can_assign)?;
        }
        if can_assign && self.advance_if_token_matches(TokenType::Equal)? {
            return Err(self.error_at_previous("Invalid assignment target"));
        }
        Ok(())
    }

    /// Emit an error at the current token
    fn error_at_current(&self, message: &str) -> RcloxError {
        error_at(&self.current_token, message)
    }

    /// Emit an error at the previous token
    fn error_at_previous(&self, message: &str) -> RcloxError {
        error_at(&self.previous_token, message)
    }

    /// Compile a declaration
    pub fn declaration(&mut self) -> Result<()> {
        if self.advance_if_token_matches(TokenType::Var)? {
            self.variable_declaration()
        } else if self.advance_if_token_matches(TokenType::Fun)? {
            self.function_declaration()
        } else {
            self.statement()
        }
    }

    /// Compile a statement
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
        } else if self.advance_if_token_matches(TokenType::Return)? {
            self.return_statement()
        } else if self.advance_if_token_matches(TokenType::While)? {
            self.while_statement()
        } else if self.advance_if_token_matches(TokenType::For)? {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    /// Compile a return statement
    fn return_statement(&mut self) -> Result<()> {
        if matches!(self.target().output, TargetOutput::Script(_)) {
            return Err(self.error_at_previous("Cannot return from top-level code"));
        }

        if self.advance_if_token_matches(TokenType::Semicolon)? {
            self.emit_instruction(Instruction::Nil);
        } else {
            self.expression()?;
            self.expect_token(TokenType::Semicolon, "Expected `;` after return value.")?;
        }
        self.emit_instruction(Instruction::Return);
        Ok(())
    }

    /// Compile an if statement
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

    /// Compile a while loop
    fn while_statement(&mut self) -> Result<()> {
        let loop_start_idx = self.target().chunk().len();

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

    /// Compile a for loop
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

        let mut loop_start = self.target().chunk().len();

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
            let increment_start = self.target().chunk().len();
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

    /// Emit a loop instruction to jump to the given index
    fn emit_loop(&mut self, target_index: usize) {
        let offset = self.target().chunk().len() - target_index + 1;

        self.emit_instruction(Instruction::Loop(offset));
    }

    /// Emit a jump instruction.
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
        self.target().chunk().len() - 1
    }

    /// Patches in the correct destination to a jump instruction.  Takes the
    /// index of the jump instruction to patch. Panics if this is not a jump
    /// instruction
    fn patch_jump_target(&mut self, offset: usize) {
        // We have to subtract 1 here because we want to set the pc to the instruction
        // before the one we want executed next
        let distance = self.target().chunk().len() - offset - 1;

        match self.target_mut().chunk_mut()[offset].instruction {
            Instruction::Jump(ref mut x) | Instruction::JumpIfFalse(ref mut x) => {
                // TODO: remove the usize::MAX sentinel value, use the type system
                assert_eq!(*x, usize::MAX);
                *x = distance;
            }
            _ => {
                unreachable!(
                    "Cannot patch a non-jump instruction '{:?}'",
                    self.target().chunk()[offset]
                );
            }
        }
    }

    /// Parse a block
    fn block(&mut self) -> Result<()> {
        while !self.check_current_token_kind(TokenType::RightBrace)
            && !self.check_current_token_kind(TokenType::Eof)
        {
            self.declaration()?;
        }
        self.expect_token(TokenType::RightBrace, "Expected '}' after block.")
    }

    #[inline]
    /// Begin a new scope
    fn begin_scope(&mut self) {
        self.target_mut().scope_depth += 1;
    }

    #[inline]
    /// End the current scope, emitting the correct number of pop instructions
    /// to remove all the scope's locals from the stack
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
    /// Call `advance()` if the current token is the given kind
    pub fn advance_if_token_matches(&mut self, kind: TokenType) -> Result<bool> {
        if !self.check_current_token_kind(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    /// Check if the current token is the given kind
    #[inline]
    fn check_current_token_kind(&self, kind: TokenType) -> bool {
        self.current_token.kind == kind
    }

    /// Compile a print statement
    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expected ';' after value.")?;
        self.emit_instruction(Instruction::Print);
        Ok(())
    }

    /// Compile an expression statement
    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expected ';' after expression.")?;
        self.emit_instruction(Instruction::Pop);
        Ok(())
    }

    /// Parse a variable declaration
    ///
    /// Returns the name of the global variable, or `None` if this is a local
    fn parse_variable(&mut self) -> Result<Option<String>> {
        self.expect_token(TokenType::Identifier, "Expected a variable name.")?;
        self.declare_variable()?;
        if self.target().scope_depth > 0 {
            // This is a local
            return Ok(None);
        }
        let name = self.previous_token.span.to_string();
        Ok(Some(name))
    }

    /// Compile a variable declaration
    fn variable_declaration(&mut self) -> Result<()> {
        // if variable_name = None, this is a local variable
        let variable_name = self.parse_variable()?;
        if self.advance_if_token_matches(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_instruction(Instruction::Nil);
        }
        self.expect_token(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        self.define_variable(variable_name);
        Ok(())
    }

    /// Mark a variable as properly defined, either by emitting an
    /// `Instruction::DefineGlobal` or by setting its depth
    fn define_variable(&mut self, variable_name: Option<String>) {
        // if variable_name is None, this is a local variable.
        //
        // The c version checks scope_depth twice, instead of using the value of
        // variable_name_id. This check is maintained with an assert, but should
        // be unnecessary
        match variable_name {
            Some(name) => self.emit_instruction(Instruction::DefineGlobal(Rc::new(name))),
            None => {
                // XXX: asserts are slow?
                assert!(self.target().scope_depth > 0);
                self.initialise_local_variable();
            }
        }
    }

    /// Set the most recent LocalVariable to the correct depth, marking it as
    /// initialised.
    ///
    /// Does nothing if not in a local scope
    fn initialise_local_variable(&mut self) {
        if self.target().scope_depth == 0 {
            return;
        }
        let depth = self.target().scope_depth;
        let ptr = &mut self
            .target_mut()
            .locals
            .last_mut()
            .unwrap_or_else(|| {
                unreachable!("No local declared, yet attempting to mark initialised")
            })
            .depth;

        //assert!(
        //ptr.is_none(),
        //"Attempted to initialise a local variable more than once"
        //);
        if ptr.is_some() {
            eprintln!("Attempted to initialise a local variable more than once");
        }

        *ptr = Some(depth);
    }

    /// Declare a variable, checking that its name is not taken.
    fn declare_variable(&mut self) -> Result<()> {
        if self.target().scope_depth == 0 {
            // This is a global variable
            return Ok(());
        }

        let variable_name = self.previous_token.span;

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

    /// Add a local variable to the vector of locals.
    fn add_uninitialised_local_variable(&mut self, name: &str) {
        //let depth = self.target().scope_depth;
        self.target_mut().locals.push(LocalVariable {
            name: Rc::new(name.to_string()),
            depth: None,
        });
    }

    /// Compile a variable
    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let name = self.previous_token.span.to_string();
        self.named_variable(name, can_assign)
    }

    /// Emit the instructions to get the given variable
    fn named_variable(&mut self, name: String, can_assign: bool) -> Result<()> {
        // if stack_slot is None, this is a global variable.
        let stack_slot = self.get_stack_slot_from_variable_name(self.target(), name.as_str())?;

        let assignable = can_assign && self.advance_if_token_matches(TokenType::Equal)?;

        // XXX: Binary choice table means that each match arm is duplicated
        match stack_slot {
            Some(x) if assignable => {
                self.expression()?;
                self.emit_instruction(Instruction::SetLocal(x))
            }
            Some(x) if !assignable => self.emit_instruction(Instruction::GetLocal(x)),

            None if assignable => {
                self.expression()?;
                self.emit_instruction(Instruction::SetGlobal(Rc::new(name)))
            }
            None if !assignable => self.emit_instruction(Instruction::GetGlobal(Rc::new(name))),

            // XXX: use `core::hint::unreachable_unchecked()`?
            _ => unreachable!("this one's actually unreachable"),
        }
        Ok(())
    }

    /// Get a stack slot for a local variable
    fn get_stack_slot_from_variable_name(
        &self,
        target: &Target,
        name: &str,
    ) -> Result<Option<usize>> {
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

    /// Compile an and expression
    fn and(&mut self) -> Result<()> {
        let end_jump = self.emit_jump_instruction(Instruction::JumpIfFalse(0));
        self.emit_instruction(Instruction::Pop);
        self.compile_with_precedence(Some(Precedence::And))?;
        self.patch_jump_target(end_jump);
        Ok(())
    }

    /// Compile an or expression
    fn or(&mut self) -> Result<()> {
        let else_jump = self.emit_jump_instruction(Instruction::JumpIfFalse(0));

        let end_jump = self.emit_jump_instruction(Instruction::Jump(0));

        self.patch_jump_target(else_jump);
        self.emit_instruction(Instruction::Pop);

        self.compile_with_precedence(Some(Precedence::Or))?;
        self.patch_jump_target(end_jump);

        Ok(())
    }

    /// Compile a function declaration
    fn function_declaration(&mut self) -> Result<()> {
        let variable_name = self.parse_variable()?;
        // TODO: I can't see why this is necessary, define_variable calls it and it tripped my
        // sanity check assertion
        self.initialise_local_variable();
        self.function()?;
        self.define_variable(variable_name);
        Ok(())
    }

    /// Compile a function
    fn function(&mut self) -> Result<()> {
        // TODO this will need an argument once methods
        let mut target = Target::new_function(String::new());
        target.output.as_function_mut().name = self.previous_token.span.to_owned();
        self.targets.push(target);
        self.begin_scope();

        self.expect_token(TokenType::LeftParen, "Expected `(` after function name.")?;

        if !self.check_current_token_kind(TokenType::RightParen) {
            // compile parameters
            while {
                self.target_mut().output.as_function_mut().arity += 1;
                let variable_name = self.parse_variable()?;
                self.define_variable(variable_name);

                self.advance_if_token_matches(TokenType::Comma)?
            } {}
        }

        self.expect_token(
            TokenType::RightParen,
            "Expected `)` after function parameters.",
        )?;
        self.expect_token(TokenType::LeftBrace, "Expected `{` before function body.")?;

        self.block()?;

        self.emit_instruction(Instruction::Nil);
        self.emit_instruction(Instruction::Return);

        let target = self.targets.pop().unwrap();
        let function = match target.output {
            TargetOutput::Script(_) => unreachable!("Attempted to close global function"),
            TargetOutput::Function(x) => x,
        };
        self.emit_instruction(Instruction::Constant(Value::Function(Rc::new(function))));

        Ok(())
    }

    /// Compile a function call
    fn call(&mut self) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.emit_instruction(Instruction::Call(arg_count));
        Ok(())
    }

    /// Compile a comma-separated list of expression
    fn argument_list(&mut self) -> Result<usize> {
        let mut arg_count = 0;
        if !self.check_current_token_kind(TokenType::RightParen) {
            while {
                self.expression()?;
                arg_count += 1;
                self.advance_if_token_matches(TokenType::Comma)?
            } {}
        }
        self.expect_token(TokenType::RightParen, "Expected `)` after arguments.")?;
        Ok(arg_count)
    }
}

/// The pratt parser jump table.
///
/// This should be `const`, but https://github.com/rust-lang/rust/issues/51999
fn get_rule(kind: TokenType) -> ParseRule {
    match kind {
        TokenType::LeftParen => ParseRule {
            prefix: Some(ParseFn::Grouping),
            infix: Some(ParseFn::Call),
            precedence: Some(Precedence::Call),
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
        string: token.span.to_owned(),
        line: token.line,
        message: message.to_owned(),
    }
}
