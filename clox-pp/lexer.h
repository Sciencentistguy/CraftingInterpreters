#pragma once
#include <string>

#include "token.h"
enum class TokenType;
class Token;
class Lexer {
    std::string source;
    const char* start;
    const char* current;
    int line;

    bool isAtEnd() const;
    char advance();
    bool match(char c);
    void skipWhitespace();
    char peek() const;
    char peekNext() const;
    Token makeToken(TokenType type) const;
    Token errorToken(const char* errorMsg) const;
    TokenType identifierType() const;
    TokenType checkKeyword(int start, int length, const char* rest, TokenType type) const;

 public:
    Lexer(const std::string& source);
    Token scanToken();
    const char* getStart() const;
    const char* getCurrent() const;
    int getLine() const;
};
