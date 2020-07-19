#include "token.h"

#include <cstring>

#include "lexer.h"

Token::Token(TokenType type, const Lexer& scanner) :
    type{type}, start{scanner.getStart()}, length{static_cast<int>(scanner.getCurrent() - scanner.getStart())}, line{scanner.getLine()} {
}

TokenType Token::getType() const {
    return type;
}

const char* Token::getStart() const {
    return start;
}

int Token::getLength() const {
    return length;
}

int Token::getLine() const {
    return line;
}

Token::Token(const char* errorMsg, const Lexer& scanner) :
    type{TokenType::Error}, start{errorMsg}, length{static_cast<int>(std::strlen(errorMsg))}, line{scanner.getLine()} {
}

const char* Token::getEnd() const {
    return start + length;
}

std::string Token::getTokenStr() const {
    return std::string(start, length);
}

