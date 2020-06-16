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
    std::string source;
    std::vector<Token> tokens;
    int start{0};
    int current{0};
    int line{1};

    bool isAtEnd();
    void scanToken();
    char advance();
    void addToken(TokenType type);
    void addToken(TokenType type, std::any literal);
    bool match(char expected);
    char peek();
    char peekNext();
    void string();
    void number();
    void identifier();

 public:
    explicit Lexer(const std::string& source);
    std::vector<Token> scanTokens();
};
