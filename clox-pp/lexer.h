#pragma once
#include <string>

enum class TokenType;
class Token;

class Lexer {
    std::string source;
    const char* start;
    const char* current;
    int line;

    [[nodiscard]] bool isAtEnd() const;
    char advance();
    bool match(char c);
    void skipWhitespace();
    [[nodiscard]] char peek() const;
    [[nodiscard]] char peekNext() const;
    [[nodiscard]] Token makeToken(TokenType type) const;
    [[nodiscard]] Token errorToken(const char* errorMsg) const;
    [[nodiscard]] TokenType identifierType() const;
    TokenType checkKeyword(int start, int length, const char* rest, TokenType type) const;

 public:
    explicit Lexer(const std::string& source);
    Token scanToken();
    [[nodiscard]] const char* getStart() const;
    [[nodiscard]] const char* getCurrent() const;
    [[nodiscard]] int getLine() const;
};
