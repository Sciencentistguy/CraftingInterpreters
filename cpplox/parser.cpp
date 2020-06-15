#include "parser.h"

#include "expression.h"
Parser::Parser(const std::vector<Token>& tokens) : tokens{tokens} {
}

const char* Parser::ParserException::what() const noexcept {
    return message.c_str();
}

Parser::ParserException::ParserException(const Token& token, const std::string& message) : runtime_error{message}, token{token}, message{message} {
    error(token, message);
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

std::shared_ptr<Expression> Parser::expression() {
    return equality();
}

std::shared_ptr<Expression> Parser::equality() {
    auto expr{comparison()};

    while (match({TokenType::Bang_equal, TokenType::Equal_equal})) {
        auto op{previous()};
        auto right{comparison()};
        expr = std::make_shared<Binary>(op, expr, right);
        //        expr = std::make_shared<Binary>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::comparison() {
    auto expr{addition()};

    while (match({TokenType::Greater, TokenType::Greater_equal, TokenType::Less, TokenType::Less_equal})) {
        auto op{previous()};
        auto right{addition()};
        expr = std::make_shared<Binary>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::addition() {
    auto expr = multiplication();

    while (match({TokenType::Minus, TokenType::Plus})) {
        auto op{previous()};
        auto right{multiplication()};
        expr = std::make_shared<Binary>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::multiplication() {
    auto expr{unary()};
    while (match({TokenType::Slash, TokenType::Star})) {
        auto op{previous()};
        auto right{unary()};
        expr = std::make_shared<Binary>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::unary() {
    if (match({TokenType::Bang, TokenType::Minus})) {
        auto op{previous()};
        auto right{unary()};
        return std::make_shared<Unary>(op, right);
    }
    return primary();
}

std::shared_ptr<Expression> Parser::primary() {
    if (match(TokenType::False)) {
        return std::make_shared<Literal>(false);
    }
    if (match(TokenType::True)) {
        return std::make_shared<Literal>(true);
    }
    if (match(TokenType::Nil)) {
        return std::make_shared<Literal>(std::any());
    }

    if (match({TokenType::Number, TokenType::String})) {
        return std::make_shared<Literal>(previous().getLiteral());
    }

    if (match(TokenType::Left_paren)) {
        auto expr{expression()};
        consume(TokenType::Right_paren, "Expect ')'after expression()");
        return std::make_shared<Grouping>(expr);
    }
    throw std::runtime_error("something's wrong");
}

Token Parser::consume(TokenType type, std::string message) {
    if (check(type)) {
        return advance();
    }
    throw ParserException(peek(), message);
}
void Parser::synchronise() {
    advance();
    while (!isAtEnd()) {
        if (previous().getType() == TokenType::Semicolon) {
            return;
        }
        switch (peek().getType()) {
            case TokenType::Class:
            case TokenType::Fun:
            case TokenType::Var:
            case TokenType::For:
            case TokenType::If:
            case TokenType::While:
            case TokenType::Print:
            case TokenType::Return:
                return;

            default:
                break;
        }
        advance();
    }
}
std::shared_ptr<Expression> Parser::parse() {
    try {
        return expression();
    } catch (const ParserException& parserException) {
        return nullptr;
    }
}
