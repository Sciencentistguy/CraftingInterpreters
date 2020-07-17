#include "compiler.h"

#include <iostream>

#include "lexer.h"
#include "token.h"


Compiler::Compiler(const std::string& source) : lexer{source}, parser{} {
}

void Compiler::compile() {
    advance();
    expression();
    consume(TokenType::Eof, "Expected end of expression.");
    emitByte(OpCode::Return);
    if (parser.hadError) {
        throw CompilerException();
    }
    if (parser.panicMode) {
        throw CompilerException();
    }
    if constexpr(DEBUG) {
        chunk.disassemble("code");
    }
}

void Compiler::advance() {
    parser.previous = parser.current;
    while (true) {
        auto tmp = lexer.scanToken();
        parser.current= tmp;
        if (parser.current.getType() != TokenType::Error) {
            break;
        }
        errorAtCurrent(parser.current.getStart());
    }
}

void Compiler::errorAtCurrent(const char* message) {
    errorAt(parser.current, message);
}

void Compiler::errorAtPrevious(const char* message) {
    errorAt(parser.previous, message);
}

void Compiler::errorAt(const Token& token, const char* message) {
    std::cerr << "[line " << token.getLine() << "] Error ";
    switch (token.getType()) {
        case TokenType::Eof:
            std::cerr << "at end ";
            break;
        case TokenType::Error:
            break;
        default:
            std::cerr << ": ";
            std::cerr.write(token.getStart(), token.getLength());
            break;
    }
    std::cerr << ": " << message << '\n';
    parser.hadError = true;
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

void Compiler::number() {
    double value = std::strtod(parser.previous.getStart(), nullptr);
    emitConstant(value);
}

void Compiler::emitConstant(LoxNumber number) {
    emitBytes(static_cast<uint8_t>(OpCode::Constant), makeConstant(number));
}

template<typename... T>
void Compiler::emitBytes(T... bytes) {
    for (auto byte : {bytes...}) {
        emitByte(byte);
    }
}

uint8_t Compiler::makeConstant(LoxNumber number) {
    int constant = chunk.addConstant(number);
    if (constant > UINT8_MAX) {
        errorAtPrevious("Too many constants in one chunk.");
        return 0;
    }
    return constant;
}

void Compiler::grouping() {
    expression();
    consume(TokenType::Right_paren, "Expected ')' after expression.");
}

void Compiler::unary() {
    TokenType opType{parser.previous.getType()};
    parsePrecedence(Precedence::Unary);

    switch (opType) {
        case TokenType::Minus:
            emitByte(OpCode::Negate);
            break;
        default:
            return;
    }
}

void Compiler::parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule{getRule(parser.previous.getType()).prefix};
    if (!prefixRule) {
        errorAtPrevious("Expected expression.");
        return;
    }

    (this->*prefixRule)();

    while (precedence <= getRule(parser.current.getType()).precedence) {
        advance();
        auto infixRule{getRule(parser.previous.getType()).infix};
        (this->*infixRule)();
    }

}

void Compiler::binary() {
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
        default:
            return;  // unreachable
    }
}
const ParseRule& Compiler::getRule(TokenType type) const {
    return rules[static_cast<int>(type)];
}
