#include "compiler.h"

#include <cstdlib>
#include <initializer_list>

#include "common.h"
#include "exception.h"
#include "lexer.h"
#include "token.h"
#include "upvalue.h"

CompilerDriver::Compiler::Compiler(FunctionType type) : function{}, functionType{type} {
    if (type == FunctionType::Script) {
        function.name = "<main>";
    }
    LocalVariable v{};
    v.name.lexeme = "";
    locals[localCount++] = v;
}

const Function& CompilerDriver::Compiler::getCurrentFunction() const {
    return function;
}

CompilerDriver::CompilerDriver(FunctionType type) : lexer{""}, parser{}, currentCompiler{std::make_unique<Compiler>(type)} {
}

Function CompilerDriver::compile() {
    if (lexer.isEmpty()) {
        throw CompilerException("Cannot compile \"\"");
    }
    advance();
    while (!match(TokenType::Eof)) {
        declaration();
    }
    auto fun = endCompiler();

    return fun;
}

void CompilerDriver::advance() {
    parser.previous = parser.current;
    while (true) {
        auto tmp = lexer.scanToken();
        parser.current = tmp;
        if (parser.current.getType() != TokenType::Error) {
            break;
        }
        errorAtCurrent(parser.current.getStart());
    }
}

void CompilerDriver::errorAtCurrent(const char* message) const {
    throw CompilerException(message, parser.current);
}

void CompilerDriver::errorAtPrevious(const char* message) const {
    throw CompilerException(message, parser.previous);
}

void CompilerDriver::consume(TokenType type, const char* message) {
    if (parser.current.getType() == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

void CompilerDriver::emitByte(uint8_t byte) {
    currentChunk().write(byte, parser.previous.getLine());
}

const Chunk& CompilerDriver::getChunk() const {
    return currentChunk();
}

void CompilerDriver::expression() {
    parsePrecedence(Precedence::Assignment);
}

void CompilerDriver::number(bool canAssign) {
    double value = std::strtod(parser.previous.getStart(), nullptr);
    emitConstant(value);
}

void CompilerDriver::emitConstant(const Value& value) {
    emitBytes(static_cast<uint8_t>(OpCode::Constant), makeConstant(value));
}

template<typename... Byte>
void CompilerDriver::emitBytes(Byte... bytes) {
    for (auto byte : {bytes...}) {
        emitByte(byte);
    }
}

uint8_t CompilerDriver::makeConstant(const Value& value) {
    int constant = currentChunk().addConstant(value);
    if (constant > UINT8_MAX) {
        errorAtPrevious("Too many constants in one chunk.");
    }
    return constant;
}

void CompilerDriver::grouping(bool canAssign) {
    expression();
    consume(TokenType::Right_paren, "Expected ')' after expression.");
}

void CompilerDriver::unary(bool canAssign) {
    TokenType opType{parser.previous.getType()};
    parsePrecedence(Precedence::Unary);

    switch (opType) {
        case TokenType::Minus:
            emitByte(OpCode::Negate);
            break;
        case TokenType::Bang:
            emitByte(OpCode::Not);
            break;
        default:
            return;
    }
}

void CompilerDriver::parsePrecedence(Precedence precedence) {
    advance();
    auto rule = getRule(parser.previous.getType());
    ParseFn prefixRule{rule.prefix};
    if (!prefixRule) {
        errorAtPrevious("Expected expression.");
    }

    bool canAssign = precedence <= Precedence::Assignment;
    (this->*prefixRule)(canAssign);

    while (precedence <= getRule(parser.current.getType()).precedence) {
        advance();
        auto rule3{getRule(parser.previous.getType())};
        auto pFunction{rule3.infix};
        (this->*pFunction)(canAssign);
    }
    if (canAssign && match(TokenType::Equal)) {
        errorAtPrevious("Invalid assignment target.");
    }
}

void CompilerDriver::binary(bool canAssign) {
    auto opType{parser.previous.getType()};
    ParseRule rule{getRule(opType)};
    parsePrecedence(rule.precedence + 1);
    switch (opType) {
        case TokenType::Plus:
            emitByte(OpCode::Add);
            break;
        case TokenType::Minus:
            emitByte(OpCode::Subtract);
            break;
        case TokenType::Star:
            emitByte(OpCode::Multiply);
            break;
        case TokenType::Slash:
            emitByte(OpCode::Divide);
            break;
        case TokenType::Bang_equal:
            emitBytes(OpCode::Equal, OpCode::Not);
            break;
        case TokenType::Equal_equal:
            emitByte(OpCode::Equal);
            break;
        case TokenType::Greater:
            emitByte(OpCode::Greater);
            break;
        case TokenType::Greater_equal:
            emitBytes(OpCode::Less, OpCode::Not);
            break;
        case TokenType::Less:
            emitByte(OpCode::Less);
            break;
        case TokenType::Less_equal:
            emitBytes(OpCode::Greater, OpCode::Not);
            break;
        default:
            return;  // unreachable
    }
}

const ParseRule& CompilerDriver::getRule(TokenType type) const {
    return rules.at(type);
}

void CompilerDriver::emitByte(OpCode byte) {
    emitByte(static_cast<uint8_t>(byte));
}

void CompilerDriver::literal(bool canAssign) {
    switch (parser.previous.getType()) {
        case TokenType::False:
            emitByte(OpCode::False);
            break;
        case TokenType::True:
            emitByte(OpCode::True);
            break;
        case TokenType::Nil:
            emitByte(OpCode::Nil);
            break;
        default:
            return;  // unreachable
    }
}

void CompilerDriver::string(bool canAssign) {
    emitConstant(std::string(parser.previous.getStart() + 1, parser.previous.getLength() - 2));
}

void CompilerDriver::declaration() {
    if (match(TokenType::Fun)) {
        functionDeclaration();
    } else if (match(TokenType::Var)) {
        variableDeclaration();
    } else {
        statement();
    }
}

void CompilerDriver::statement() {
    if (match(TokenType::Print)) {
        printStatement();
    } else if (match(TokenType::For)) {
        forStatement();
    } else if (match(TokenType::If)) {
        ifStatement();
    } else if (match(TokenType::Return)) {
        returnStatement();
    } else if (match(TokenType::While)) {
        whileStatement();
    } else if (match(TokenType::Left_brace)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

bool CompilerDriver::match(TokenType tokenType) {
    if (!check(tokenType)) {
        return false;
    }
    advance();
    return true;
}

bool CompilerDriver::check(TokenType tokenType) const {
    return parser.current.getType() == tokenType;
}

void CompilerDriver::printStatement() {
    expression();
    consume(TokenType::Semicolon, "Expected ';' after value.");
    emitByte(OpCode::Print);
}

void CompilerDriver::expressionStatement() {
    expression();
    consume(TokenType::Semicolon, "Expected ';' after expression.");
    emitByte(OpCode::Pop);
}

void CompilerDriver::variableDeclaration() {
    uint8_t global{parseVariable("Expected variable name.")};
    if (match(TokenType::Equal)) {
        expression();
    } else {
        emitByte(OpCode::Nil);
    }
    consume(TokenType::Semicolon, "Expected ';' after variable declaration.");
    defineVariable(global);
}

uint8_t CompilerDriver::parseVariable(const char* errorMessage) {
    consume(TokenType::Identifier, errorMessage);
    declareVariable();
    if (currentCompiler->scopeDepth > 0) {
        return 0;
    }
    return identifierConstant(parser.previous);
}

uint8_t CompilerDriver::identifierConstant(const Token& name) {
    return currentChunk().addConstant(name.getTokenStr());
}

void CompilerDriver::defineVariable(uint8_t global) {
    if (currentCompiler->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(static_cast<uint8_t>(OpCode::Define_global), global);
}

void CompilerDriver::setSource(const std::string& source) {
    lexer.setSource(source);
    currentCompiler = std::make_unique<Compiler>(FunctionType::Script);
}

void CompilerDriver::variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

void CompilerDriver::namedVariable(const Token& name, bool canAssign) {
    int arg{resolveLocal(*currentCompiler, name)};
    OpCode getOp;
    OpCode setOp;
    if (arg != -1) {
        getOp = OpCode::Get_local;
        setOp = OpCode::Set_local;
    } else if ((arg = resolveUpvalue(*currentCompiler, name)) != -1) {
        getOp = OpCode::Get_upvalue;
        setOp = OpCode::Set_upvalue;
    } else {
        arg = identifierConstant(name);
        getOp = OpCode::Get_global;
        setOp = OpCode::Set_global;
    }

    if (canAssign && match(TokenType::Equal)) {
        expression();
        emitBytes(static_cast<uint8_t>(setOp), static_cast<uint8_t>(arg));
    } else {
        emitBytes(static_cast<uint8_t>(getOp), static_cast<uint8_t>(arg));
    }
}

void CompilerDriver::block() {
    while (!check(TokenType::Right_brace) && !check(TokenType::Eof)) {
        declaration();
    }
    consume(TokenType::Right_brace, "Expected '}' after block.");
}

void CompilerDriver::beginScope() {
    ++currentCompiler->scopeDepth;
}

void CompilerDriver::endScope() {
    --currentCompiler->scopeDepth;
    while (currentCompiler->localCount > 0 && currentCompiler->locals[currentCompiler->localCount - 1].depth > currentCompiler->scopeDepth) {
        emitByte(OpCode::Pop);
        --currentCompiler->localCount;
    }
}

void CompilerDriver::declareVariable() {
    if (currentCompiler->scopeDepth == 0) {
        return;
    }
    for (int i = currentCompiler->localCount; i >= 0; --i) {
        const LocalVariable& local{currentCompiler->locals[i]};
        if (local.depth != -1 && local.depth < currentCompiler->scopeDepth) {
            break;
        }
        if (parser.previous == local.name) {
            errorAtPrevious("Variable with this name already declared in this scope.");
        }
    }
    addLocal(parser.previous);
}

void CompilerDriver::addLocal(const Token& name) {
    if (currentCompiler->localCount == MAX_LOCALS) {
        errorAtPrevious("Too many local variables in scope.");
    }
    auto& l = currentCompiler->locals[currentCompiler->localCount++];
    l.name = name;
    l.depth = -1;
}

int CompilerDriver::resolveLocal(const Compiler& compiler, const Token& name) {
    for (int i = compiler.localCount - 1; i >= 0; --i) {
        const LocalVariable& local = compiler.locals[i];
        if (name == local.name) {
            if (local.depth == -1) {
                errorAtPrevious("Cannot read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1;
}

void CompilerDriver::ifStatement() {
    consume(TokenType::Left_paren, "Expected '(' after 'if'.");
    expression();
    consume(TokenType::Right_paren, "Expected ')' after condition.");
    auto thenJump = emitJump(OpCode::Jump_if_false);
    emitByte(OpCode::Pop);

    statement();
    auto elseJump = emitJump(OpCode::Jump);
    patchJump(thenJump);
    emitByte(OpCode::Pop);
    if (match(TokenType::Else)) {
        statement();
    }
    patchJump(elseJump);
}

std::size_t CompilerDriver::emitJump(OpCode instruction) {
    constexpr uint8_t placeholder = 0xff;
    emitBytes(static_cast<uint8_t>(instruction), placeholder, placeholder);
    return currentChunk().getCount() - 2;
}

void CompilerDriver::patchJump(size_t offset) {
    auto jump = currentChunk().getCount() - offset - 2;
    if (jump > UINT16_MAX) {
        errorAtPrevious("Too much code to jump over.");
    }
    currentChunk().code[offset] = (jump >> 8u) & 0xffu;
    currentChunk().code[offset + 1] = jump & 0xffu;
}

void CompilerDriver::and_(bool canAssign) {
    auto endJump{emitJump(OpCode::Jump_if_false)};
    emitByte(OpCode::Pop);
    parsePrecedence(Precedence::And);
    patchJump(endJump);
}

void CompilerDriver::or_(bool canAssign) {
    auto elseJump{emitJump(OpCode::Jump_if_false)};
    auto endJump{emitJump(OpCode::Jump)};
    patchJump(elseJump);
    emitByte(OpCode::Pop);
    parsePrecedence(Precedence::Or);
    patchJump(endJump);
}

void CompilerDriver::whileStatement() {
    auto loopStart{currentChunk().getCount()};
    consume(TokenType::Left_paren, "Expected '(' after 'while'.");
    expression();
    consume(TokenType::Right_paren, "Expected ')' after condition.");

    auto exitJump{emitJump(OpCode::Jump_if_false)};
    emitByte(OpCode::Pop);
    statement();

    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OpCode::Pop);
}

void CompilerDriver::emitLoop(std::size_t loopStart) {
    emitByte(OpCode::Loop);
    auto offset{currentChunk().getCount() - loopStart + 2};
    if (offset > UINT16_MAX) {
        errorAtPrevious("Loop body too large.");
    }
    emitByte((offset >> 8u) & 0xff);
    emitByte(offset & 0xff);
}

void CompilerDriver::forStatement() {
    beginScope();
    consume(TokenType::Left_paren, "Expected '(' after 'for'.");
    if (match(TokenType::Semicolon)) {
    } else if (match(TokenType::Var)) {
        variableDeclaration();
    } else {
        expressionStatement();
    }

    auto loopStart{currentChunk().getCount()};

    auto exitJump{-1};
    if (!match(TokenType::Semicolon)) {
        expression();
        consume(TokenType::Semicolon, "Expected ';' after loop condition.");

        exitJump = emitJump(OpCode::Jump_if_false);
        emitByte(OpCode::Pop);
    }

    if (!match(TokenType::Right_paren)) {
        auto bodyJump{emitJump(OpCode::Jump)};
        auto incrementStart{currentChunk().getCount()};
        expression();
        emitByte(OpCode::Pop);
        consume(TokenType::Right_paren, "Expected ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OpCode::Pop);
    }
    endScope();
}

Chunk& CompilerDriver::currentChunk() {
    return currentCompiler->function.getChunk();
}

const Chunk& CompilerDriver::currentChunk() const {
    return currentCompiler->function.getChunk();
}

void CompilerDriver::functionDeclaration() {
    auto global{parseVariable("Expected function name after 'fun'.")};
    markInitialized();
    function(FunctionType::Script);
    defineVariable(global);
}

void CompilerDriver::markInitialized() {
    if (currentCompiler->scopeDepth == 0) {
        return;
    }
    currentCompiler->locals[currentCompiler->localCount - 1].depth = currentCompiler->scopeDepth;
}

void CompilerDriver::function(FunctionType type) {
    newCompiler(FunctionType::Function);
    beginScope();
    consume(TokenType::Left_paren, "Expected '(' after function name.");
    if (!check(TokenType::Right_paren)) {
        do {
            currentCompiler->function.arity++;
            if (currentCompiler->function.arity > 255) {
                errorAtCurrent("Cannot have more than 255 parameters.");
            }
            auto constant{parseVariable("Expected  parameter name.")};
            defineVariable(constant);
        } while (match(TokenType::Comma));
    }
    consume(TokenType::Right_paren, "Expected '(' after function parameters.");

    consume(TokenType::Left_brace, "Expected '{' before function body");
    block();

    const auto& f = endCompiler();
    endScope();
    auto comp = parentCompiler();

    emitBytes(static_cast<uint8_t>(OpCode::Closure), makeConstant(f));
    for (int i = 0; i < f.upvalueCount; ++i) {
        const auto& tmp = comp->upvalues[i];
        emitByte(tmp.isLocal ? 1 : 0);
        emitByte(tmp.index);
    }
}

void CompilerDriver::newCompiler(FunctionType type) {
    auto newCompiler{std::make_unique<Compiler>(type)};
    if (type != FunctionType::Script) {
        newCompiler->function.name = parser.previous.getTokenStr();
    }
    newCompiler->parent = std::move(currentCompiler);
    currentCompiler = std::move(newCompiler);
}

std::unique_ptr<CompilerDriver::Compiler> CompilerDriver::parentCompiler() {
    auto ret = std::move(currentCompiler);
    currentCompiler = std::move(ret->parent);
    return ret;
}

void CompilerDriver::call(bool canAssign) {
    auto argcount{argumentList()};
    emitBytes(static_cast<uint8_t>(OpCode::Call), argcount);
}

uint8_t CompilerDriver::argumentList() {
    uint8_t argCount{};
    if (!check(TokenType::Right_paren)) {
        do {
            expression();
            ++argCount;
        } while (match(TokenType::Comma));
    }
    consume(TokenType::Right_paren, "Expected ')' after arguments.");
    return argCount;
}

Function CompilerDriver::endCompiler() {
    emitByte(OpCode::Nil);
    emitByte(OpCode::Return);

    if constexpr (DEBUG) {
        const auto& fname{currentCompiler->function.getName()};
        currentChunk().disassemble(fname.empty() ? "<main>" : fname);
    }
    return currentCompiler->function;
}

void CompilerDriver::returnStatement() {
    if (currentCompiler->functionType == FunctionType::Script) {
        errorAtPrevious("Cannot return from top-level code.");
    }
    if (match(TokenType::Semicolon)) {
        emitBytes(OpCode::Nil, OpCode::Return);
    } else {
        expression();
        consume(TokenType::Semicolon, "Expected semicolon after return value.");
        emitByte(OpCode::Return);
    }
}

int CompilerDriver::resolveUpvalue(Compiler& compiler, const Token& name) {
    if (!compiler.parent) {
        return -1;
    }

    int local = resolveLocal(*compiler.parent, name);
    if (local != -1) {
        return addUpvalue(compiler, local, true);
    }

    int upvalue = resolveUpvalue(*compiler.parent, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, upvalue, false);
    }
    return -1;
}

int CompilerDriver::addUpvalue(Compiler& compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler.function.upvalueCount;

    for (int i = 0; i < upvalueCount; ++i) {
        Upvalue uv = compiler.upvalues[i];
        if (uv.index == index && uv.isLocal == isLocal) {
            return i;
        }
    }
    if (upvalueCount == UINT8_MAX) {
        errorAtPrevious("Too many closure variables in function.");
    }

    compiler.upvalues[upvalueCount].isLocal = isLocal;
    compiler.upvalues[upvalueCount].index = index;
    return compiler.function.upvalueCount++;
}
