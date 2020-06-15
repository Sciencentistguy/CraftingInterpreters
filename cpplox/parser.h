#pragma once

#include <any>
#include <vector>

#include "expression.h"
#include "main.h"
#include "token.h"

class Parser {
    class ParserException : std::runtime_error {
        Token token;
        std::string message;

     public:
        ParserException(const Token& token, const std::string& message);
        const char* what() const noexcept override;
    };

    const std::vector<Token> tokens;
    int current{0};

    bool match(const std::initializer_list<TokenType>& tokentypes);
    bool match(const TokenType type);
    bool check(const TokenType type);
    const Token& advance();
    bool isAtEnd();
    const Token& peek();
    const Token& previous();

    std::shared_ptr<Expression> expression();
    std::shared_ptr<Expression> equality();
    std::shared_ptr<Expression> comparison();
    std::shared_ptr<Expression> addition();
    std::shared_ptr<Expression> multiplication();
    std::shared_ptr<Expression> unary();
    std::shared_ptr<Expression> primary();

    Token consume(TokenType type, std::string message);
    void synchronise();

 public:
    Parser(const std::vector<Token>& tokens);
    std::shared_ptr<Expression> parse();
};
