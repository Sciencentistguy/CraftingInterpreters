#include "compiler.h"

#include <iostream>

#include "exception.h"
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
        throw CompilerException("had error");
    }
    if (parser.panicMode) {
        throw CompilerException("panic!");
    }
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

void Compiler::errorAtCurrent(const char* message) {
    errorAt(parser.current, message);
}

void Compiler::errorAtPrevious(const char* message) {
    errorAt(parser.previous, message);
}

void Compiler::errorAt(const Token& token, const char* message) {
    throw CompilerException(message, token);
    //    switch (token.getType()) {
    //        case TokenType::Eof:
    //            std::cerr << "at end ";
    //            break;
    //        case TokenType::Error:
    //            break;
    //        default:
    //            std::cerr << ": ";
    //            std::cerr.write(token.getStart(), token.getLength());
    //            break;
    //    }
    //    std::cerr << ": " << message << '\n';
    //    parser.hadError = true;
    //    throw CompilerException(message);
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

void Compiler::emitConstant(Value number) {
    emitBytes(static_cast<uint8_t>(OpCode::Constant), makeConstant(number));
}

template<typename... T>
void Compiler::emitBytes(T... bytes) {
    for (auto byte : {bytes...}) {
        emitByte(byte);
    }
}

uint8_t Compiler::makeConstant(Value number) {
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

    (this->*prefixRule)();
    while (precedence <= getRule(parser.current.getType()).precedence) {
        advance();
        auto rule3{getRule(parser.previous.getType())};
        auto pFunction{rule3.infix};
        (this->*pFunction)();
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
void Compiler::literal() {
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
