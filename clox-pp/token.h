#pragma once

#include <string>
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
 public:
    TokenType type;
    std::string_view lexeme;
//    const char* start;
//    int length;
    int line;

    Token() = default;
    Token(const char* errorMsg, const Lexer& scanner);
    Token(TokenType type, const Lexer& scanner);
    [[nodiscard]] TokenType getType() const;
    [[nodiscard]] const char* getStart() const;
    [[nodiscard]] int getLength() const;
    [[nodiscard]] int getLine() const;
    [[nodiscard]] const char* getEnd() const;
    [[nodiscard]] std::string getTokenStr() const;
    const std::string_view& getLexeme() const;

    Token& operator=(const Token& rhs) = default;

    friend bool operator==(const Token& lhs, const Token& rhs);
};