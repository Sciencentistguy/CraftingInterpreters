#pragma once
#include <any>
#include <string>

#include "main.h"
#include "token_type.h"

class Token {
    const TokenType type;
    const std::string lexeme;
    const std::any literal;
    const int line;

 public:
    Token(TokenType type, const std::string& lexeme, std::any, int line);

    friend std::ostream& operator<<(std::ostream& lhs, const Token& rhs);
    TokenType getType() const;
    const std::any& getLiteral() const;
    int getLine() const;
    const std::string& getLexeme() const;
};
