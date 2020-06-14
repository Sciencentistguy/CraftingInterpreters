#pragma once

#include <vector>
#include <any>

#include "expression.h"
#include "token.h"
class Parser {
    const std::vector<Token> tokens;
    int current{0};

    bool match(const std::initializer_list<TokenType>& tokentypes);
    bool match(const TokenType type);
    bool check(const TokenType type);
    const Token& advance();
    bool isAtEnd();
    const Token& peek();
    const Token& previous();

    std::shared_ptr<Expression<std::any>> expression();
    std::shared_ptr<Expression<std::any>> equality();
    std::shared_ptr<Expression<std::any>> comparison();
    std::shared_ptr<Expression<std::any>> addition();
    std::shared_ptr<Expression<std::any>> multiplication();
    std::shared_ptr<Expression<std::any>> unary();
    std::shared_ptr<Expression<std::any>> primary();

 public:
    Parser(const std::vector<Token>& tokens);
};
