use std::sync::Arc;

use string_interner::symbol::SymbolUsize;

use crate::{
    chunk::Chunk,
    error::LoxError,
    lexer::{Lexer, Token, TokenKind},
    opcode::Opcode,
    value::{FunctionKind, LoxFunction, Value},
    INTERNER,
};

/// Disassemble the chunk once it is compiled
const DEBUG_PRINT_CODE: bool = false;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    compilers: Vec<Compiler>,
}

struct Compiler {
    function: LoxFunction,
    function_kind: FunctionKind,
    locals: Vec<Local>,
    scope_depth: usize,
    upvalues: Vec<CompilerUpvalue>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            previous: Token::NULL,
            current: Token::NULL,
            compilers: vec![Compiler::new(0, FunctionKind::Script, None)],
        }
    }

    pub fn with_line(source: &'a str, line: usize) -> Self {
        Self {
            lexer: Lexer::with_line(source, line),
            previous: Token::NULL,
            current: Token::NULL,
            compilers: vec![Compiler::new(0, FunctionKind::Script, None)],
        }
    }

    /// Extract the completed chunk
    pub fn finalise(mut self) -> LoxFunction {
        self.compilers.pop().unwrap().function
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

        let token = self.lexer.next_token()?;

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
        &mut self.current_compiler_mut().function.chunk
    }

    /// Get a shared reference to the current chunk.
    ///
    /// Currently trivial, will become Not.
    fn current_chunk(&mut self) -> &Chunk {
        &self.current_compiler().function.chunk
    }

    fn current_compiler(&self) -> &Compiler {
        self.compilers.last().unwrap()
    }

    fn current_compiler_mut(&mut self) -> &mut Compiler {
        self.compilers.last_mut().unwrap()
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
        self.emit(Opcode::Nil);
        self.emit(Opcode::Return);

        if DEBUG_PRINT_CODE {
            let name = match INTERNER
                .lock()
                .resolve(self.current_compiler().function.name)
            {
                None | Some("") => "code".to_owned(),
                Some(name) => name.to_owned(),
            };

            println!("== {name} ==");
            self.current_chunk_mut().disassemble();
        }
    }

    fn matches(&mut self, kind: TokenKind) -> Result<bool, LoxError> {
        if !self.check(kind) {
            return Ok(false);
        }
        self.advance()?;
        Ok(true)
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn begin_scope(&mut self) {
        self.current_compiler_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_compiler_mut().scope_depth = self
            .current_compiler()
            .scope_depth
            .checked_sub(1)
            .expect("Internal error: went into negative scope");

        while self
            .current_compiler()
            .locals
            .last()
            .map(|x| x.depth.unwrap() > self.current_compiler().scope_depth)
            .unwrap_or(false)
        {
            if self.current_compiler().locals.last().unwrap().is_captured {
                self.emit(Opcode::CloseUpvalue);
            } else {
                self.emit(Opcode::Pop);
            }
            self.current_compiler_mut().locals.pop();
        }
    }
}

impl<'a> Parser<'a> {
    /// The entry-point into the compiler
    pub fn compile(&mut self) -> Result<(), LoxError> {
        self.advance()?;

        while !self.matches(TokenKind::Eof)? {
            self.declaration()?;
        }

        self.consume(TokenKind::Eof, "Expected end of expression".to_owned())?;

        self.end_compiler();

        Ok(())
    }

    fn declaration(&mut self) -> Result<(), LoxError> {
        if self.matches(TokenKind::Fun)? {
            self.function_declaration()?;
        } else if self.matches(TokenKind::Var)? {
            self.variable_declaration()?;
        } else {
            self.statement()?;
        }

        Ok(())
    }

    fn function_declaration(&mut self) -> Result<(), LoxError> {
        let location = self.parse_variable("Expected function name".to_owned())?;

        let name = self.previous.span;
        let name = INTERNER.lock().get_or_intern(name);

        if matches!(location, VariableLocation::Local) {
            self.current_compiler_mut().locals.last_mut().unwrap().depth =
                Some(self.current_compiler().scope_depth)
        }

        self.function(name)?;

        if let VariableLocation::Global(name) = location {
            self.emit(Opcode::DefineGlobal(name));
        }

        Ok(())
    }

    fn function(&mut self, name: SymbolUsize) -> Result<(), LoxError> {
        let new_compiler = Compiler::new(0, FunctionKind::Function, Some(name));
        self.compilers.push(new_compiler);

        self.begin_scope();

        self.consume(
            TokenKind::LeftParen,
            "Expected '(' after function name".to_owned(),
        )?;

        if !self.check(TokenKind::RightParen) {
            loop {
                self.current_compiler_mut().function.arity += 1;

                let location = self.parse_variable("Expected parameter name".to_owned())?;

                match location {
                    VariableLocation::Local => {
                        self.current_compiler_mut().locals.last_mut().unwrap().depth =
                            Some(self.current_compiler().scope_depth)
                    }
                    _ => unreachable!("Internal error: function parameters must be local"),
                }

                if !self.matches(TokenKind::Comma)? {
                    break;
                }
            }
        }

        self.consume(
            TokenKind::RightParen,
            "Expected ')' after function parameters".to_owned(),
        )?;

        self.consume(
            TokenKind::LeftBrace,
            "Expected '{' before function body".to_owned(),
        )?;

        self.block()?;

        self.end_compiler();

        let mut finished = self
            .compilers
            .pop()
            .expect("Internal error: no compiler to pop");

        finished.function.name = name;

        let Compiler {
            function, upvalues, ..
        } = finished;

        let function = Arc::new(function);

        self.emit(Opcode::Closure(Value::Function(function), upvalues));

        Ok(())
    }

    fn variable_declaration(&mut self) -> Result<(), LoxError> {
        let name = self.parse_variable("Expected a variable name after `var`".to_owned())?;

        if self.matches(TokenKind::Equal)? {
            self.expression()?;
        } else {
            self.emit(Opcode::Nil);
        }

        self.consume(
            TokenKind::Semicolon,
            "Expected ';' after variable declaration".to_owned(),
        )?;

        if let VariableLocation::Global(name) = name {
            self.emit(Opcode::DefineGlobal(name));
        } else {
            self.current_compiler_mut().locals.last_mut().unwrap().depth =
                Some(self.current_compiler().scope_depth);
        }

        Ok(())
    }

    fn parse_variable(&mut self, msg: String) -> Result<VariableLocation, LoxError> {
        self.consume(TokenKind::Identifier, msg)?;

        self.declare_variable()?;

        if self.current_compiler().scope_depth != 0 {
            return Ok(VariableLocation::Local);
        }

        let name = self.previous.span;
        let name = INTERNER.lock().get_or_intern(name);
        Ok(VariableLocation::Global(name))
    }

    fn declare_variable(&mut self) -> Result<(), LoxError> {
        if self.current_compiler().scope_depth == 0 {
            return Ok(());
        }

        let name = self.previous.span;
        let name = INTERNER.lock().get_or_intern(name);

        for local in self.current_compiler().locals.iter().rev() {
            if local.is_defined() && local.depth < Some(self.current_compiler().scope_depth) {
                break;
            }

            if local.name == name {
                return Err(self.error(
                    ErrorLocation::Previous,
                    "Already a variable with this name in this scope".to_owned(),
                ));
            }
        }

        self.add_local(name);

        Ok(())
    }

    fn add_local(&mut self, name: SymbolUsize) {
        self.current_compiler_mut().locals.push(Local {
            name,
            depth: None,
            is_captured: false,
        });
    }

    fn statement(&mut self) -> Result<(), LoxError> {
        if self.matches(TokenKind::Print)? {
            self.print_statement()?;
        } else if self.matches(TokenKind::For)? {
            self.for_statement()?;
        } else if self.matches(TokenKind::If)? {
            self.if_statement()?;
        } else if self.matches(TokenKind::Return)? {
            self.return_statement()?;
        } else if self.matches(TokenKind::While)? {
            self.while_statement()?;
        } else if self.matches(TokenKind::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.expression_statement()?;
        }

        Ok(())
    }

    fn return_statement(&mut self) -> Result<(), LoxError> {
        if matches!(self.current_compiler().function_kind, FunctionKind::Script) {
            return Err(self.error(
                ErrorLocation::Previous,
                "Cannot return from top-level code".to_owned(),
            ));
        }

        if self.matches(TokenKind::Semicolon)? {
            self.emit(Opcode::Nil);
            self.emit(Opcode::Return);
        } else {
            self.expression()?;
            self.consume(
                TokenKind::Semicolon,
                "Expected ';' after return value".to_owned(),
            )?;
            self.emit(Opcode::Return);
        }

        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), LoxError> {
        self.begin_scope();
        self.consume(TokenKind::LeftParen, "Expected '(' after 'for'".to_owned())?;

        // Initialiser clause:
        if self.matches(TokenKind::Semicolon)? {
            // No initialiser
        } else if self.matches(TokenKind::Var)? {
            self.variable_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let loop_start = self.current_chunk().code().len();

        let exit_jump = if !self.matches(TokenKind::Semicolon)? {
            self.expression()?;
            self.consume(
                TokenKind::Semicolon,
                "Expected ';' after loop condition".to_owned(),
            )?;

            // Jump out of the loop if the condition is false
            let exit_jump = self.emit_jump(Opcode::JumpIfFalse(usize::MAX));
            self.emit(Opcode::Pop);
            Some(exit_jump)
        } else {
            None
        };

        // The increment clause is executed at the end of each iteration. In clox, this is achieved
        // with a cursed loop-back-twice thing that needs a whole *diagram* to make sense.
        //
        // Here, just compile it as normal, move those instructions into a local vec, and put them
        // back at the end 🙂.
        //
        // One (1) heap-allocation at compile-time is a small price to pay for sanity.
        let increment_clause = if !self.matches(TokenKind::RightParen)? {
            let end = self.current_chunk().code().len();
            self.expression()?;
            self.emit(Opcode::Pop);
            self.consume(
                TokenKind::RightParen,
                "Expected ')' after for clauses".to_owned(),
            )?;

            let v = self.current_chunk().code()[end..].to_owned();

            self.current_chunk_mut().code_mut().truncate(end);

            Some(v)
        } else {
            None
        };

        self.statement()?;

        if let Some(increment_clause) = increment_clause {
            self.current_chunk_mut()
                .code_mut()
                .extend_from_slice(&increment_clause);
        }

        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit(Opcode::Pop);
        }

        self.end_scope();

        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), LoxError> {
        let loop_start = self.current_chunk().code().len();
        self.consume(
            TokenKind::LeftParen,
            "Expected '(' after `while`".to_owned(),
        )?;
        self.expression()?;
        self.consume(
            TokenKind::RightParen,
            "Expected ')' after condition".to_owned(),
        )?;

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse(usize::MAX));
        self.emit(Opcode::Pop);
        self.statement()?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(Opcode::Pop);

        Ok(())
    }

    fn emit_loop(&mut self, ptr: usize) {
        let offset = self.current_chunk_mut().code().len() - ptr + 1;
        self.emit(Opcode::Loop(offset));
    }

    fn and(&mut self) -> Result<(), LoxError> {
        let end_jump = self.emit_jump(Opcode::JumpIfFalse(usize::MAX));

        self.emit(Opcode::Pop);

        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump);

        Ok(())
    }

    fn or(&mut self) -> Result<(), LoxError> {
        let else_jump = self.emit_jump(Opcode::JumpIfFalse(usize::MAX));
        let end_jump = self.emit_jump(Opcode::Jump(usize::MAX));

        self.patch_jump(else_jump);
        self.emit(Opcode::Pop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);

        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), LoxError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after `if`".to_owned())?;
        self.expression()?;
        self.consume(
            TokenKind::RightParen,
            "Expected ')' after condition".to_owned(),
        )?;

        let then_jump = self.emit_jump(Opcode::JumpIfFalse(usize::MAX));

        self.emit(Opcode::Pop);

        self.statement()?;

        let else_jump = self.emit_jump(Opcode::Jump(usize::MAX));

        self.patch_jump(then_jump);

        if self.matches(TokenKind::Else)? {
            self.statement()?
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit(opcode);
        self.current_chunk_mut().code().len() - 1
    }

    fn patch_jump(&mut self, ptr: usize) {
        let offset = self.current_chunk_mut().code().len() - ptr - 1;

        let instruction = &mut self.current_chunk_mut().code_mut()[ptr];

        match instruction {
            Opcode::JumpIfFalse(x) => *x = offset,
            Opcode::Jump(x) => *x = offset,

            _ => unreachable!("Cannot patch non-jump instruction"),
        }
    }

    fn block(&mut self) -> Result<(), LoxError> {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration()?;
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after block".to_owned())?;

        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ';' after value".to_owned())?;
        self.emit(Opcode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ';' after value".to_owned())?;
        self.emit(Opcode::Pop);
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
            TokenKind::Bang => self.emit(Opcode::Not),

            _ => unreachable!("'-' is the only unary operator"),
        }

        Ok(())
    }

    /// Dispatch to the correct compilation method for the given precedence.
    ///
    /// This is the core of the Pratt parser.
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), LoxError> {
        self.advance()?;
        let prefix_rule = parse_rule(self.previous.kind).prefix;
        let can_assign = precedence.can_assign();

        if let Some(prefix_rule) = prefix_rule {
            prefix_rule.call(self, can_assign)?;
        } else {
            return Err(self.error(ErrorLocation::Previous, "Expected expression".to_owned()));
        }

        while precedence <= parse_rule(self.current.kind).precedence {
            self.advance()?;
            let infix_rule = parse_rule(self.previous.kind).infix;
            // clox omits the null check, so unwrap
            infix_rule.unwrap().call(self, can_assign)?;
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

            TokenKind::EqualEqual => self.emit(Opcode::Equal),
            TokenKind::BangEqual => {
                self.emit(Opcode::Equal);
                self.emit(Opcode::Not);
            }

            TokenKind::Greater => self.emit(Opcode::Greater),
            TokenKind::GreaterEqual => {
                self.emit(Opcode::Less);
                self.emit(Opcode::Not);
            }
            TokenKind::Less => self.emit(Opcode::Less),
            TokenKind::LessEqual => {
                self.emit(Opcode::Greater);
                self.emit(Opcode::Not);
            }

            _ => unreachable!("All binary ops are covered"),
        }

        Ok(())
    }

    fn literal(&mut self) -> Result<(), LoxError> {
        match self.previous.kind {
            TokenKind::False => self.emit(Opcode::False),
            TokenKind::True => self.emit(Opcode::True),
            TokenKind::Nil => self.emit(Opcode::Nil),

            _ => unreachable!("All literal tokens are covered"),
        }

        Ok(())
    }

    fn string(&mut self) -> Result<(), LoxError> {
        let span = self.previous.span;
        let span = &span[1..];
        let span = &span[..span.len() - 1];
        let value = Value::String(INTERNER.lock().get_or_intern(span));

        self.emit(Opcode::Constant(value));

        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        self.named_variable(can_assign)
    }

    fn named_variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        let name = self.previous.span;
        let name = INTERNER.lock().get_or_intern(name);

        let get_op;
        let set_op;

        if let Some(arg) = self.resolve_local(self.compilers.len() - 1, name)? {
            get_op = Opcode::GetLocal(arg);
            set_op = Opcode::SetLocal(arg);
        } else if let Some(arg) = self.resolve_upvalue(self.compilers.len() - 1, name)? {
            get_op = Opcode::GetUpvalue(arg);
            set_op = Opcode::SetUpvalue(arg);
        } else {
            get_op = Opcode::GetGlobal(name);
            set_op = Opcode::SetGlobal(name);
        }

        if can_assign && self.matches(TokenKind::Equal)? {
            self.expression()?;
            self.emit(set_op);
        } else {
            self.emit(get_op);
        }

        Ok(())
    }

    fn resolve_upvalue(
        &mut self,
        compiler_idx: usize,
        name: SymbolUsize,
    ) -> Result<Option<usize>, LoxError> {
        // First check if we're in the global scope
        if compiler_idx == 0 {
            return Ok(None);
        }

        // Then check in the parent scope
        if let Some(local) = self.resolve_local(compiler_idx - 1, name)? {
            self.compilers[compiler_idx - 1].locals[local].is_captured = true;
            return self.add_upvalue(compiler_idx, local, true);
        }

        if let Some(upvalue) = self.resolve_upvalue(compiler_idx - 1, name)? {
            self.compilers[compiler_idx - 1].locals[upvalue].is_captured = true;
            return self.add_upvalue(compiler_idx, upvalue, false);
        }

        // Upvalue not found. Lox is late-bound for globals, so this effectively means "hopefully a
        // global", or it will be a runtime error.
        Ok(None)
    }

    fn add_upvalue(
        &mut self,
        compiler_idx: usize,
        local: usize,
        is_local: bool,
    ) -> Result<Option<usize>, LoxError> {
        let upvalue = CompilerUpvalue {
            index: local,
            is_local,
        };

        for (i, uv) in self.compilers[compiler_idx].upvalues.iter().enumerate() {
            if uv == &upvalue {
                return Ok(Some(i));
            }
        }

        self.compilers[compiler_idx].upvalues.push(upvalue);

        Ok(Some(self.compilers[compiler_idx].upvalues.len() - 1))
    }

    fn resolve_local(
        &mut self,
        compiler_idx: usize,
        name: SymbolUsize,
    ) -> Result<Option<usize>, LoxError> {
        for (i, local) in self.compilers[compiler_idx].locals.iter().enumerate().rev() {
            if local.name == name {
                if local.depth.is_none() {
                    return Err(self.error(
                        ErrorLocation::Previous,
                        "Cannot read local variable in its own initializer".to_owned(),
                    ));
                }

                return Ok(Some(i));
            }
        }

        Ok(None)
    }

    fn call(&mut self) -> Result<(), LoxError> {
        let arg_count = self.argument_list()?;
        self.emit(Opcode::Call(arg_count));

        Ok(())
    }

    fn argument_list(&mut self) -> Result<usize, LoxError> {
        let mut arg_count = 0;

        if !self.check(TokenKind::RightParen) {
            loop {
                self.expression()?;

                /*
                 * if arg_count == 255 {
                 *     return Err(self.error(
                 *         ErrorLocation::Previous,
                 *         "Cannot have more than 255 arguments".to_owned(),
                 *     ));
                 * }
                 */

                arg_count += 1;

                if !self.matches(TokenKind::Comma)? {
                    break;
                }
            }
        }

        self.consume(
            TokenKind::RightParen,
            "Expected ')' after arguments".to_owned(),
        )?;

        Ok(arg_count)
    }
}

impl Compiler {
    fn new(scope_depth: usize, kind: FunctionKind, function_name: Option<SymbolUsize>) -> Self {
        let mut locals = Vec::new();
        let mut interner = INTERNER.lock();
        let name = interner.get_or_intern("");
        locals.push(Local {
            name,
            depth: Some(0),
            is_captured: false,
        });

        let name = function_name.unwrap_or(name);

        let function = LoxFunction::new(0, Chunk::default(), name);

        Self {
            locals,
            scope_depth,
            function,
            function_kind: kind,
            upvalues: Vec::new(),
        }
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

    fn can_assign(&self) -> bool {
        self <= &Self::Assignment
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
    /// Dispatch to the correct compilation method. A trick to avoid storing struct methods as
    /// function pointers, which is a pain in Rust.
    fn call(self, parser: &mut Parser, can_assign: bool) -> Result<(), LoxError> {
        match self {
            ParseFn::Grouping => parser.grouping(),
            ParseFn::Unary => parser.unary(),
            ParseFn::Binary => parser.binary(),
            ParseFn::Number => parser.number(),
            ParseFn::Literal => parser.literal(),
            ParseFn::String => parser.string(),
            ParseFn::Variable => parser.variable(can_assign),
            ParseFn::And => parser.and(),
            ParseFn::Or => parser.or(),
            ParseFn::Call => parser.call(),
        }
    }
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

struct Local {
    name: SymbolUsize,
    depth: Option<usize>,
    is_captured: bool,
}

impl Local {
    fn is_defined(&self) -> bool {
        self.depth.is_some()
    }
}

impl std::fmt::Debug for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = INTERNER.lock();
        let name = interner.resolve(self.name).unwrap();
        f.debug_struct("Local")
            .field("name", &name)
            .field("depth", &self.depth)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerUpvalue {
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug)]
enum VariableLocation {
    Local,
    Global(SymbolUsize),
}

/// The main Pratt parser lookup table.
///
/// Uses a cosnt fn rather than a static array because using a static array is horrible.
const fn parse_rule(token: TokenKind) -> ParseRule {
    match token {
        TokenKind::LeftParen => ParseRule {
            prefix: Some(ParseFn::Grouping),
            infix: Some(ParseFn::Call),
            precedence: Precedence::Call,
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
            prefix: Some(ParseFn::Unary),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::BangEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Equality,
        },
        TokenKind::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::EqualEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Equality,
        },
        TokenKind::Greater => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        TokenKind::GreaterEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        TokenKind::Less => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        TokenKind::LessEqual => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Binary),
            precedence: Precedence::Comparison,
        },
        TokenKind::Identifier => ParseRule {
            prefix: Some(ParseFn::Variable),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::String => ParseRule {
            prefix: Some(ParseFn::String),
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
            infix: Some(ParseFn::And),
            precedence: Precedence::And,
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
            prefix: Some(ParseFn::Literal),
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
            prefix: Some(ParseFn::Literal),
            infix: None,
            precedence: Precedence::None,
        },
        TokenKind::Or => ParseRule {
            prefix: None,
            infix: Some(ParseFn::Or),
            precedence: Precedence::Or,
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
            prefix: Some(ParseFn::Literal),
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
