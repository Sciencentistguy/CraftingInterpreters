#include "token.h"

#include <cstring>

#include "lexer.h"

Token::Token(TokenType type, const Lexer& scanner) :
    type{type}, lexeme{scanner.getStart(), static_cast<std::size_t>((scanner.getCurrent() - scanner.getStart()))}, line{scanner.getLine()} {
}

TokenType Token::getType() const {
    return type;
}

const char* Token::getStart() const {
    return lexeme.data();
}

int Token::getLength() const {
    return lexeme.length();
}

int Token::getLine() const {
    return line;
}

Token::Token(const char* errorMsg, const Lexer& scanner) : type{TokenType::Error}, lexeme{errorMsg}, line{scanner.getLine()} {
}

const char* Token::getEnd() const {
    return getStart() + getLength();
}

std::string Token::getTokenStr() const {
    return std::string(lexeme);
}

bool operator==(const Token& lhs, const Token& rhs) {
    return lhs.lexeme == rhs.lexeme;
}
const std::string_view& Token::getLexeme() const {
    return lexeme;
}
