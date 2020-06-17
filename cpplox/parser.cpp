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

    if (match(TokenType::Identifier)) {
        return std::make_shared<Variable>(previous());
    }

    if (match(TokenType::Left_paren)) {
        auto expr{expression()};
        consume(TokenType::Right_paren, "Expect ')' after expression()");
        return std::make_shared<Grouping>(expr);
    }

    throw ParserException(peek(), "Not yet implemented");
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

std::vector<std::shared_ptr<Statement>> Parser::parse() {
    std::vector<std::shared_ptr<Statement>> statements;
    while (!isAtEnd()) {
        statements.push_back(declaration());
    }
    return statements;
}

std::shared_ptr<Statement> Parser::declaration() {
    try {
        if (match(TokenType::Var)) {
            return varDeclaration();
        }
        return statement();
    } catch (const ParserException&) {
        synchronise();
        return nullptr;
    }
}

std::shared_ptr<Statement> Parser::varDeclaration() {
    Token name{consume(TokenType::Identifier, "Expected variable name.")};
    std::shared_ptr<Expression> initialiser{nullptr};
    if (match(TokenType::Equal)) {
        initialiser = expression();
    }
    consume(TokenType::Semicolon, "Expected ';' after variable declaration.");
    return std::make_shared<VarStatement>(initialiser, name);
}

std::shared_ptr<Statement> Parser::statement() {
    if (match(TokenType::Print)) {
        return printStatement();
    }
    return expressionStatement();
}

std::shared_ptr<Statement> Parser::printStatement() {
    auto value = expression();
    consume(TokenType::Semicolon, "Except ';' after expression");
    return std::make_shared<PrintStatement>(value);
}

std::shared_ptr<Statement> Parser::expressionStatement() {
    auto expr = expression();
    consume(TokenType::Semicolon, "Expect ';' after expression");
    return std::make_shared<ExpressionStatement>(expr);
}
