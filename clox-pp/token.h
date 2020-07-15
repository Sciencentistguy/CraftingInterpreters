#pragma once

#include <cstring>

#include "lexer.h"

enum class TokenType {
    Left_paren,
    Right_paren,
    Left_brace,
    Right_brace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    Bang_equal,
    Equal,
    Equal_equal,
    Greater,
    Greater_equal,
    Less,
    Less_equal,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof
};

class Lexer;

class Token {
    TokenType type;
    const char* start;
    int length;
    int line;

 public:
    Token(const char* errorMsg, const Lexer& scanner);
    Token(TokenType type, const Lexer& scanner);
    TokenType getType() const;
    const char* getStart() const;
    int getLength() const;
    int getLine() const;
};

// std::ostream& operator<<(std::ostream& os, TokenType rhs);