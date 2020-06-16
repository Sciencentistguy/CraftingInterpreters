#pragma once
#include <any>
#include <string>

#include "main.h"

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

    // One or two character tokens.
    Bang,
    Bang_equal,
    Equal,
    Equal_equal,
    Greater,
    Greater_equal,
    Less,
    Less_equal,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
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

    Eof
};

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
