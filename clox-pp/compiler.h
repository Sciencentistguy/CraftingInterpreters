#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <utility>

#include "chunk.h"
#include "lexer.h"
#include "opcode.h"
#include "parser.h"
#include "token.h"
#include "value.h"

class Compiler {
    Lexer lexer;
    Parser parser;
    Chunk chunk;

    void advance();
    void consume(TokenType type, const char* message);
    void errorAtCurrent(const char* message) const;
    void errorAtPrevious(const char* message) const;

    [[nodiscard]] const ParseRule& getRule(TokenType type) const;

    uint8_t identifierConstant(const Token& name);

    void defineVariable(uint8_t global);
    void variable(bool canAssign);
    void namedVariable(const Token& name, bool canAssign);

    void declaration();
    void variableDeclaration();

    void statement();
    void printStatement();
    void expressionStatement();

    void parsePrecedence(Precedence precedence);
    uint8_t parseVariable(const char* errorMessage);

    void expression();
    void grouping(bool canAssign);
    void unary(bool canAssign);
    void binary(bool canAssign);
    void literal(bool canAssign);
    void number(bool canAssign);
    void string(bool canAssign);

    void emitByte(OpCode byte);
    void emitByte(uint8_t byte);
    template<typename... Byte>
    void emitBytes(Byte... bytes);
    void emitConstant(const Value& value);

    bool match(TokenType tokenType);
    bool check(TokenType tokenType) const;

    uint8_t makeConstant(const Value& value);
    const std::unordered_map<TokenType, ParseRule> rules = {
        std::make_pair(TokenType::Left_paren, ParseRule(&Compiler::grouping, nullptr, Precedence::None)),
        std::make_pair(TokenType::Right_paren, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Left_brace, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Right_brace, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Comma, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Dot, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Minus, ParseRule{&Compiler::unary, &Compiler::binary, Precedence::Term}),
        std::make_pair(TokenType::Plus, ParseRule{nullptr, &Compiler::binary, Precedence::Term}),
        std::make_pair(TokenType::Semicolon, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Slash, ParseRule{nullptr, &Compiler::binary, Precedence::Factor}),
        std::make_pair(TokenType::Star, ParseRule{nullptr, &Compiler::binary, Precedence::Factor}),
        std::make_pair(TokenType::Bang, ParseRule{&Compiler::unary, nullptr, Precedence::None}),
        std::make_pair(TokenType::Bang_equal, ParseRule{nullptr, &Compiler::binary, Precedence::Equality}),
        std::make_pair(TokenType::Equal, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Equal_equal, ParseRule{nullptr, &Compiler::binary, Precedence::Equality}),
        std::make_pair(TokenType::Greater, ParseRule{nullptr, &Compiler::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Greater_equal, ParseRule{nullptr, &Compiler::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Less, ParseRule{nullptr, &Compiler::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Less_equal, ParseRule{nullptr, &Compiler::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Identifier, ParseRule{&Compiler::variable, nullptr, Precedence::None}),
        std::make_pair(TokenType::String, ParseRule{&Compiler::string, nullptr, Precedence::None}),
        std::make_pair(TokenType::Number, ParseRule{&Compiler::number, nullptr, Precedence::None}),
        std::make_pair(TokenType::And, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Class, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Else, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::False, ParseRule{&Compiler::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::For, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Fun, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::If, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Nil, ParseRule{&Compiler::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::Or, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Print, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Return, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Super, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::This, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::True, ParseRule{&Compiler::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::Var, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::While, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Error, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Eof, ParseRule{nullptr, nullptr, Precedence::None})};

 public:
    explicit Compiler(const std::string& source);
    void compile();
    [[nodiscard]] const Chunk& getChunk() const;
    void setSource(const std::string& source);
};