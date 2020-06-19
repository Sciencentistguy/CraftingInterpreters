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

template<typename... TT>
bool Parser::match(const TT&... tokentypes) {
    for (const auto type : {tokentypes...}) {
        if (check(type)) {
            advance();
            return true;
        }
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
    return assignmentExpression();
}

std::shared_ptr<Expression> Parser::assignmentExpression() {
    auto expr{orExpression()};
    if (match(TokenType::Equal)) {
        auto equals{previous()};
        auto value{assignmentExpression()};

        if (std::dynamic_pointer_cast<VariableExpression>(expr) != nullptr) {
            auto name{dynamic_cast<VariableExpression&>(*expr).getName()};
            return std::make_shared<AssignExpression>(name, value);
        }
        error(equals, "Invalid assignment target");
    }
    return expr;
}

std::shared_ptr<Expression> Parser::orExpression() {
    auto expr{andExpression()};
    while (match(TokenType::Or)) {
        auto op{previous()};
        auto right{andExpression()};
        expr = std::make_shared<LogicalExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::andExpression() {
    auto expr{equalityExpression()};
    while (match(TokenType::And)) {
        auto op{previous()};
        auto right{equalityExpression()};
        expr = std::make_shared<LogicalExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::equalityExpression() {
    auto expr{comparisonExpression()};

    while (match(TokenType::Bang_equal, TokenType::Equal_equal)) {
        auto op{previous()};
        auto right{comparisonExpression()};
        expr = std::make_shared<BinaryExpression>(op, expr, right);
        //        expr = std::make_shared<BinaryExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::comparisonExpression() {
    auto expr{additionExpression()};

    while (match(TokenType::Greater, TokenType::Greater_equal, TokenType::Less, TokenType::Less_equal)) {
        auto op{previous()};
        auto right{additionExpression()};
        expr = std::make_shared<BinaryExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::additionExpression() {
    auto expr = multiplicationExpression();

    while (match(TokenType::Minus, TokenType::Plus)) {
        auto op{previous()};
        auto right{multiplicationExpression()};
        expr = std::make_shared<BinaryExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::multiplicationExpression() {
    auto expr{unaryExpression()};
    while (match(TokenType::Slash, TokenType::Star)) {
        auto op{previous()};
        auto right{unaryExpression()};
        expr = std::make_shared<BinaryExpression>(op, expr, right);
    }
    return expr;
}

std::shared_ptr<Expression> Parser::unaryExpression() {
    if (match(TokenType::Bang, TokenType::Minus)) {
        auto op{previous()};
        auto right{unaryExpression()};
        return std::make_shared<UnaryExpression>(op, right);
    }
    return callExpression();
}

std::shared_ptr<Expression> Parser::callExpression() {
    auto expr{primaryExpression()};
    while (true) {
        if (match(TokenType::Left_paren)) {
            expr = finishCall(expr);
        } else {
            break;
        }
    }
    return expr;
}

std::shared_ptr<Expression> Parser::finishCall(std::shared_ptr<Expression> callee) {
    std::vector<std::shared_ptr<Expression>> arguments{};
    if (!check(TokenType::Right_paren)) {
        do {
            if (arguments.size() >= 255) {
                error(peek(), "Cannot have more than 255 arguments");
            }
            arguments.push_back(expression());
        } while (match(TokenType::Comma));
    }
    Token paren{consume(TokenType::Right_paren, "Expected ')' after function arguments")};
    return std::make_shared<CallExpression>(paren, callee, arguments);
}

std::shared_ptr<Expression> Parser::primaryExpression() {
    if (match(TokenType::False)) {
        return std::make_shared<LiteralExpression>(false);
    }
    if (match(TokenType::True)) {
        return std::make_shared<LiteralExpression>(true);
    }
    if (match(TokenType::Nil)) {
        return std::make_shared<LiteralExpression>(std::any());
    }

    if (match(TokenType::Number, TokenType::String)) {
        return std::make_shared<LiteralExpression>(previous().getLiteral());
    }

    if (match(TokenType::Identifier)) {
        return std::make_shared<VariableExpression>(previous());
    }

    if (match(TokenType::Left_paren)) {
        auto expr{expression()};
        consume(TokenType::Right_paren, "Expect ')' after expression()");
        return std::make_shared<GroupingExpression>(expr);
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
        statements.push_back(declarationStatement());
    }
    return statements;
}

std::shared_ptr<Statement> Parser::declarationStatement() {
    try {
        if (match(TokenType::Fun)) {
            return functionDeclarationStatement("function?");
        }
        if (match(TokenType::Var)) {
            return variableDeclarationStatement();
        }
        return statement();
    } catch (const ParserException&) {
        synchronise();
        return nullptr;
    }
}

std::shared_ptr<Statement> Parser::variableDeclarationStatement() {
    Token name{consume(TokenType::Identifier, "Expected variable name.")};
    std::shared_ptr<Expression> initialiser{nullptr};
    if (match(TokenType::Equal)) {
        initialiser = expression();
    }
    consume(TokenType::Semicolon, "Expected ';' after variable declaration.");
    return std::make_shared<VarStatement>(initialiser, name);
}

std::shared_ptr<Statement> Parser::statement() {
    if (match(TokenType::For)) {
        return forStatement();
    }
    if (match(TokenType::If)) {
        return ifStatement();
    }
    if (match(TokenType::Print)) {
        return printStatement();
    }
    if (match(TokenType::Return)) {
        return returnStatement();
    }
    if (match(TokenType::While)) {
        return whileStatement();
    }
    if (match(TokenType::Left_brace)) {
        return std::make_shared<BlockStatement>(blockStatement());
    }
    return expressionStatement();
}

std::shared_ptr<Statement> Parser::forStatement() {
    consume(TokenType::Left_paren, "Expected '(' after 'for'.");

    std::shared_ptr<Statement> initializer;
    if (match(TokenType::Semicolon)) {
        initializer = nullptr;
    } else if (match(TokenType::Var)) {
        initializer = variableDeclarationStatement();
    } else {
        initializer = expressionStatement();
    }

    std::shared_ptr<Expression> condition{nullptr};
    if (!check(TokenType::Semicolon)) {
        condition = expression();
    }
    consume(TokenType::Semicolon, "Expected ';' after loop condition.");

    std::shared_ptr<Expression> increment{nullptr};
    if (!check(TokenType::Right_paren)) {
        increment = expression();
    }
    consume(TokenType::Right_paren, "Expected ')' after for clauses.");
    auto body{statement()};

    if (increment) {
        std::vector<std::shared_ptr<Statement>> vec{{body, std::make_shared<ExpressionStatement>(increment)}};
        body = std::make_shared<BlockStatement>(std::move(vec));
    }

    if (!condition) {
        condition = std::make_shared<LiteralExpression>(true);
    }
    body = std::make_shared<WhileStatement>(condition, body);

    if (initializer) {
        std::vector<std::shared_ptr<Statement>> vec{{initializer, body}};
        body = std::make_shared<BlockStatement>(vec);
    }

    return body;
}

std::shared_ptr<Statement> Parser::ifStatement() {
    consume(TokenType::Left_paren, "Expected '(' after 'if'.");
    auto condition{expression()};
    consume(TokenType::Right_paren, "Expected ')' after if condition.");
    std::shared_ptr<Statement> thenBranch{statement()};
    std::shared_ptr<Statement> elseBranch{nullptr};
    if (match(TokenType::Else)) {
        elseBranch = statement();
    }
    return std::make_shared<IfStatement>(condition, thenBranch, elseBranch);
}

std::shared_ptr<Statement> Parser::printStatement() {
    auto value = expression();
    consume(TokenType::Semicolon, "Excepted ';' after expression");
    return std::make_shared<PrintStatement>(value);
}

std::shared_ptr<Statement> Parser::returnStatement() {
    Token keyword{previous()};
    std::shared_ptr<Expression> value{nullptr};
    if (!check(TokenType::Semicolon)) {
        value = expression();
    }
    consume(TokenType::Semicolon, "Expected ';' after return statement.");
    return std::make_shared<ReturnStatement>(keyword, value);
}

std::shared_ptr<Statement> Parser::whileStatement() {
    consume(TokenType::Left_paren, "Expected '(' after 'while'.");
    auto condition{expression()};
    consume(TokenType::Right_paren, "Expected ')' after while condition.");
    auto body{statement()};
    return std::make_shared<WhileStatement>(condition, body);
}

std::shared_ptr<Statement> Parser::expressionStatement() {
    auto expr = expression();
    consume(TokenType::Semicolon, "Expected ';' after expression");
    return std::make_shared<ExpressionStatement>(expr);
}

std::vector<std::shared_ptr<Statement>> Parser::blockStatement() {
    std::vector<std::shared_ptr<Statement>> statements{};
    while (!check(TokenType::Right_brace) && !isAtEnd()) {
        statements.push_back(declarationStatement());
    }
    consume(TokenType::Right_brace, "Expected '}' after block.");
    return statements;
}

std::shared_ptr<Statement> Parser::functionDeclarationStatement(const std::string& kind) {
    Token name{consume(TokenType::Identifier, "Expected " + kind + " name.")};
    std::vector<Token> params{};
    advance();
    if (!check(TokenType::Right_paren)) {
        do {
            if (params.size() >= 255) {
                error(peek(), "Cannot have more than 255 parameters.");
            }
            params.push_back(consume(TokenType::Identifier, "Expected parameter name."));
        } while (match(TokenType::Comma));
    }
    consume(TokenType::Right_paren, "Exptected ')' after parameters.");
    consume(TokenType::Left_brace, "Expected '}' before " + kind + " body.");
    auto body{blockStatement()};
    return std::make_shared<FunctionStatement>(name, params, body);
}
