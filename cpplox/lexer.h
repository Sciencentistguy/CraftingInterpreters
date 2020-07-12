#pragma once

#include <any>
#include <string>
#include <vector>

#include <unordered_map>

#include "token.h"

const std::unordered_map<std::string, TokenType> keywords{
    {"and", TokenType::And},   {"class", TokenType::Class}, {"else", TokenType::Else},     {"false", TokenType::False},
    {"for", TokenType::For},   {"fun", TokenType::Fun},     {"if", TokenType::If},         {"nil", TokenType::Nil},
    {"or", TokenType::Or},     {"print", TokenType::Print}, {"return", TokenType::Return}, {"super", TokenType::Super},
    {"this", TokenType::This}, {"true", TokenType::True},   {"var", TokenType::Var},       {"while", TokenType::While}};

class Lexer {
    const std::string source;

    std::vector<Token> tokens;
    unsigned int start{0};
    unsigned int current{0};
    unsigned int line{1};

    bool isAtEnd() const;
    char peek() const;
    char peekNext() const;

    void scanToken();
    char advance();
    void addToken(const TokenType type);
    void addToken(const TokenType type, const std::any& literal);
    bool match(const char expected);
    void string();
    void number();
    void identifier();

 public:
    explicit Lexer(const std::string& source);
    std::vector<Token> scanTokens();
};
