#include "parser.h"

#include "expression.h"
Parser::Parser(const std::vector<Token>& tokens) : tokens{tokens} {
}

bool Parser::match(const std::initializer_list<TokenType>& tokentypes) {
    for (const auto type : tokentypes) {
        if (check(type)) {
            advance();
            return true;
        }
    }
    return false;
}

bool Parser::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type) {
    if (isAtEnd()) {
        return false;
    }
    return peek().getType() == type;
}

const Token& Parser::advance() {
    if (!isAtEnd()) {
        current++;
    }
    return previous();
}

bool Parser::isAtEnd() {
    return peek().getType() == TokenType::Eof;
}

const Token& Parser::peek() {
    return tokens[current];
}

const Token& Parser::previous() {
    return tokens[current - 1];
}

std::shared_ptr<Expression<std::any>> Parser::expression() {
    return equality();
}

std::shared_ptr<Expression<std::any>> Parser::equality() {
    auto expr{comparison()};

    while (match({TokenType::Bang_equal, TokenType::Equal_equal})) {
        auto op{previous()};
        auto right{comparison()};
        expr = std::make_shared<Binary<std::any>>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression<std::any>> Parser::comparison() {
    auto expr{addition()};

    while (match({TokenType::Greater, TokenType::Greater_equal, TokenType::Less, TokenType::Less_equal})) {
        auto op{previous()};
        auto right{addition()};
        expr = std::make_shared<Binary<std::any>>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression<std::any>> Parser::addition() {
    auto expr = multiplication();

    while (match({TokenType::Minus, TokenType::Plus})) {
        auto op{previous()};
        auto right{multiplication()};
        expr = std::make_shared<Binary<std::any>>(op, expr, right);
    }
    return expr;
}
std::shared_ptr<Expression<std::any>> Parser::multiplication() {
    auto expr{unary()};
    while (match({TokenType::Slash, TokenType::Star})) {
        auto op{previous()};
        auto right{unary()};
        expr = std::make_shared<Binary<std::any>>(op, expr, right);
    }
}
std::shared_ptr<Expression<std::any>> Parser::unary() {
    if (match({TokenType::Bang, TokenType::Minus})) {
        auto op{previous()};
        auto right{unary()};
        return std::make_shared<Unary<std::any>>(op, right);
    }
    return primary();
}
std::shared_ptr<Expression<std::any>> Parser::primary() {
    if (match(TokenType::False)) {
        return std::make_shared<Literal<std::any>>(false);
    }
    if (match(TokenType::True)) {
        return std::make_shared<Literal<std::any>>(true);
    }
    if (match(TokenType::Nil)) {
        return std::make_shared<Literal<std::any>>();
    }

    if (match({TokenType::Number, TokenType::String})) {
        return std::make_shared<Literal<std::any>>(previous().getLiteral());
    }

    if (match(TokenType::Left_paren)) {
        auto expr{expression()};
//        consume(TokenType::Right_paren, "Expect ')'after expression()");
        return std::make_shared<Grouping<std::any>>(expr);
    }
    throw std::runtime_error("something's wrong");
}
