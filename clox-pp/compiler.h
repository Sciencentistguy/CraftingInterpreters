#pragma once

#include <array>
#include <string>

#include "chunk.h"
#include "lexer.h"
#include "parser.h"

class CompilerException : public std::exception {};

class Compiler {
    Lexer lexer;
    Parser parser;
    Chunk chunk;

    void advance();
    void consume(TokenType type, const char* message);
    void errorAtCurrent(const char* message);
    void errorAtPrevious(const char* message);
    void errorAt(const Token& token, const char* message);

    [[nodiscard]] const ParseRule& getRule(TokenType type) const;

    void parsePrecedence(Precedence precedence);
    void expression();
    void grouping();
    void unary();
    void binary();
    void number();

    void emitByte(uint8_t byte);
    template<typename... T>
    void emitBytes(T... bytes);
    void emitConstant(LoxNumber number);

    uint8_t makeConstant(LoxNumber number);

    const std::array<ParseRule, 40> rules{
        ParseRule{&Compiler::grouping, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{&Compiler::unary, &Compiler::binary, Precedence::Term},
        ParseRule{nullptr, &Compiler::binary, Precedence::Term},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, &Compiler::binary, Precedence::Factor},
        ParseRule{nullptr, &Compiler::binary, Precedence::Factor},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{&Compiler::number, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
        ParseRule{nullptr, nullptr, Precedence::None},
    };

 public:
    explicit Compiler(const std::string& source);
    void compile();
    [[nodiscard]] const Chunk& getChunk() const;
};