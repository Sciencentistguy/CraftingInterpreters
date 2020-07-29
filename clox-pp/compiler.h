#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <utility>

#include "chunk.h"
#include "function.h"
#include "lexer.h"
#include "opcode.h"
#include "parser.h"
#include "token.h"
#include "value.h"

constexpr int MAX_LOCALS{UINT8_MAX + 1};

struct LocalVariable {
    Token name;
    int depth;
};


class CompilerDriver {
    struct Compiler {
        std::unique_ptr<Compiler> parent{};
        std::array<LocalVariable, MAX_LOCALS> locals{};
        int localCount{};
        int scopeDepth{};

        Function function;
        FunctionType functionType{};
        explicit Compiler(FunctionType type);
        [[nodiscard]] const Function& getCurrentFunction() const;
    };

    Lexer lexer;
    Parser parser;
    std::unique_ptr<Compiler> currentCompiler;

    Chunk& currentChunk();
    const Chunk& currentChunk() const;

    void newCompiler(FunctionType type);
    void parentCompiler();

    void advance();
    void consume(TokenType type, const char* message);
    [[noreturn]] void errorAtCurrent(const char* message) const;
    [[noreturn]] void errorAtPrevious(const char* message) const;

    [[nodiscard]] const ParseRule& getRule(TokenType type) const;

    uint8_t identifierConstant(const Token& name);

    void defineVariable(uint8_t global);
    void declareVariable();
    void variable(bool canAssign);
    void namedVariable(const Token& name, bool canAssign);
    void addLocal(const Token& name);
    int resolveLocal(const Token& name);
    uint8_t argumentList();
    void markInitialized();

    void declaration();
    void functionDeclaration();
    void variableDeclaration();

    void statement();
    void forStatement();
    void printStatement();
    void whileStatement();
    void ifStatement();
    void expressionStatement();

    void parsePrecedence(Precedence precedence);
    uint8_t parseVariable(const char* errorMessage);

    void beginScope();
    void endScope();

    void expression();
    void block();
    void function(FunctionType type);
    void grouping(bool canAssign);
    void unary(bool canAssign);
    void binary(bool canAssign);
    void call(bool canAssign);
    void literal(bool canAssign);
    void number(bool canAssign);
    void string(bool canAssign);
    void and_(bool canAssign);
    void or_(bool canAssign);

    void emitByte(OpCode byte);
    void emitByte(uint8_t byte);
    template<typename... Byte>
    void emitBytes(Byte... bytes);
    void emitConstant(const Value& value);
    std::size_t emitJump(OpCode instruction);
    void emitLoop(std::size_t loopStart);
    void patchJump(size_t offset);

    bool match(TokenType tokenType);
    bool check(TokenType tokenType) const;

    Function endCompiler();

    uint8_t makeConstant(const Value& value);
    const std::unordered_map<TokenType, ParseRule> rules = {
        std::make_pair(TokenType::Left_paren, ParseRule(&CompilerDriver::grouping, &CompilerDriver::call, Precedence::Call)),
        std::make_pair(TokenType::Right_paren, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Left_brace, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Right_brace, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Comma, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Dot, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Minus, ParseRule{&CompilerDriver::unary, &CompilerDriver::binary, Precedence::Term}),
        std::make_pair(TokenType::Plus, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Term}),
        std::make_pair(TokenType::Semicolon, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Slash, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Factor}),
        std::make_pair(TokenType::Star, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Factor}),
        std::make_pair(TokenType::Bang, ParseRule{&CompilerDriver::unary, nullptr, Precedence::None}),
        std::make_pair(TokenType::Bang_equal, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Equality}),
        std::make_pair(TokenType::Equal, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Equal_equal, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Equality}),
        std::make_pair(TokenType::Greater, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Greater_equal, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Less, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Less_equal, ParseRule{nullptr, &CompilerDriver::binary, Precedence::Comparison}),
        std::make_pair(TokenType::Identifier, ParseRule{&CompilerDriver::variable, nullptr, Precedence::None}),
        std::make_pair(TokenType::String, ParseRule{&CompilerDriver::string, nullptr, Precedence::None}),
        std::make_pair(TokenType::Number, ParseRule{&CompilerDriver::number, nullptr, Precedence::None}),
        std::make_pair(TokenType::And, ParseRule{nullptr, &CompilerDriver::and_, Precedence::And}),
        std::make_pair(TokenType::Class, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Else, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::False, ParseRule{&CompilerDriver::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::For, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Fun, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::If, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Nil, ParseRule{&CompilerDriver::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::Or, ParseRule{nullptr, &CompilerDriver::or_, Precedence::Or}),
        std::make_pair(TokenType::Print, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Return, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Super, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::This, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::True, ParseRule{&CompilerDriver::literal, nullptr, Precedence::None}),
        std::make_pair(TokenType::Var, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::While, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Error, ParseRule{nullptr, nullptr, Precedence::None}),
        std::make_pair(TokenType::Eof, ParseRule{nullptr, nullptr, Precedence::None})};

 public:
    explicit CompilerDriver(FunctionType type);
    Function compile();
    [[nodiscard]] const Chunk& getChunk() const;
    void setSource(const std::string& source);
};