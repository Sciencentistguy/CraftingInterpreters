#include "token.h"

#include <iostream>
Token::Token(TokenType type, const std::string& lexeme, std::any literal, int line) : type{type}, lexeme{lexeme}, literal{literal}, line{line} {
}

std::ostream& operator<<(std::ostream& lhs, const Token& rhs) {
    lhs << "<Token: type: '" << static_cast<int>(rhs.type) << "'; lexeme: '" << rhs.lexeme << "'; literal: '" << stringify(rhs.getLiteral()) << "'>";
    return lhs;
}

TokenType Token::getType() const {
    return type;
}

const std::any& Token::getLiteral() const {
    return literal;
}

int Token::getLine() const {
    return line;
}

const std::string& Token::getLexeme() const {
    return lexeme;
}
