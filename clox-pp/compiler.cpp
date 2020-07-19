#include "compiler.h"

#include <cstdlib>
#include <initializer_list>

#include "common.h"
#include "exception.h"
#include "lexer.h"
#include "token.h"

Compiler::Compiler(const std::string& source) : lexer{source}, parser{} {
}

void Compiler::compile() {
    if (lexer.isEmpty()) {
        throw CompilerException("Cannot compile \"\"");
    }
    advance();
    while (!match(TokenType::Eof)) {
        declaration();
    }
    emitByte(OpCode::Return);

    if constexpr (DEBUG) {
        chunk.disassemble("code");
    }
}

void Compiler::advance() {
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

void Compiler::errorAtCurrent(const char* message) const {
    throw CompilerException(message, parser.current);
}

void Compiler::errorAtPrevious(const char* message) const {
    throw CompilerException(message, parser.previous);
}

void Compiler::consume(TokenType type, const char* message) {
    if (parser.current.getType() == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

void Compiler::emitByte(uint8_t byte) {
    chunk.write(byte, parser.previous.getLine());
}

const Chunk& Compiler::getChunk() const {
    return chunk;
}

void Compiler::expression() {
    parsePrecedence(Precedence::Assignment);
}

void Compiler::number(bool canAssign) {
    double value = std::strtod(parser.previous.getStart(), nullptr);
    emitConstant(value);
}

void Compiler::emitConstant(const Value& value) {
    emitBytes(static_cast<uint8_t>(OpCode::Constant), makeConstant(value));
}

template<typename... Byte>
void Compiler::emitBytes(Byte... bytes) {
    for (auto byte : {bytes...}) {
        emitByte(byte);
    }
}

uint8_t Compiler::makeConstant(const Value& value) {
    int constant = chunk.addConstant(value);
    if (constant > UINT8_MAX) {
        errorAtPrevious("Too many constants in one chunk.");
        return 0;
    }
    return constant;
}

void Compiler::grouping(bool canAssign) {
    expression();
    consume(TokenType::Right_paren, "Expected ')' after expression.");
}

void Compiler::unary(bool canAssign) {
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

void Compiler::parsePrecedence(Precedence precedence) {
    advance();
    auto rule = getRule(parser.previous.getType());
    ParseFn prefixRule{rule.prefix};
    if (!prefixRule) {
        errorAtPrevious("Expected expression.");
        return;
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

void Compiler::binary(bool canAssign) {
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

const ParseRule& Compiler::getRule(TokenType type) const {
    return rules.at(type);
}

void Compiler::emitByte(OpCode byte) {
    emitByte(static_cast<uint8_t>(byte));
}

void Compiler::literal(bool canAssign) {
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

void Compiler::string(bool canAssign) {
    emitConstant(std::string(parser.previous.getStart() + 1, parser.previous.getLength() - 2));
}

void Compiler::declaration() {
    if (match(TokenType::Var)) {
        variableDeclaration();
    } else {
        statement();
    }
}

void Compiler::statement() {
    if (match(TokenType::Print)) {
        printStatement();
    } else {
        expressionStatement();
    }
}

bool Compiler::match(TokenType tokenType) {
    if (!check(tokenType)) {
        return false;
    }
    advance();
    return true;
}

bool Compiler::check(TokenType tokenType) const {
    return parser.current.getType() == tokenType;
}

void Compiler::printStatement() {
    expression();
    consume(TokenType::Semicolon, "Expected ';' after value.");
    emitByte(OpCode::Print);
}

void Compiler::expressionStatement() {
    expression();
    consume(TokenType::Semicolon, "Expected ';' after expression.");
    emitByte(OpCode::Pop);
}

void Compiler::variableDeclaration() {
    uint8_t global{parseVariable("Expected variable name.")};
    if (match(TokenType::Equal)) {
        expression();
    } else {
        emitByte(OpCode::Nil);
    }
    consume(TokenType::Semicolon, "Expected ';' after variable declaration.");
    defineVariable(global);
}

uint8_t Compiler::parseVariable(const char* errorMessage) {
    consume(TokenType::Identifier, errorMessage);
    return identifierConstant(parser.previous);
}

uint8_t Compiler::identifierConstant(const Token& name) {
    return chunk.addConstant(name.getTokenStr());
}

void Compiler::defineVariable(uint8_t global) {
    emitBytes(static_cast<uint8_t>(OpCode::Define_global), global);
}

void Compiler::setSource(const std::string& source) {
    lexer.setSource(source);
    chunk = Chunk();
}

void Compiler::variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

void Compiler::namedVariable(const Token& name, bool canAssign) {
    auto arg{identifierConstant(name)};

    if (canAssign && match(TokenType::Equal)) {
        expression();
        emitBytes(static_cast<uint8_t>(OpCode::Set_global), arg);
    } else {
        emitBytes(static_cast<uint8_t>(OpCode::Get_global), arg);
    }

}
