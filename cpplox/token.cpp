#include "token.h"

#include <iostream>
Token::Token(TokenType type, const std::string& lexeme, std::any literal, int line) : type{type}, lexeme{lexeme}, literal{literal}, line{line} {
}

std::ostream& operator<<(std::ostream& lhs, const Token& rhs) {
    std::string lit;
    if (rhs.literal.type() == typeid(int)) {
        lit = std::to_string(std::any_cast<int>(rhs.literal));
    } else if (rhs.literal.type() == typeid(std::string)) {
        lit = std::any_cast<std::string>(rhs.literal);
    } else if (rhs.literal.type() == typeid(double)) {
        lit = std::to_string(std::any_cast<double>(rhs.literal));
    }
    lhs << "<Token: type: '" << static_cast<int>(rhs.type) << "'; lexeme: '" << rhs.lexeme << "'; literal: '" << lit << "'>";
    return lhs;
}
