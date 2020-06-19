#pragma once

#include <any>
#include <vector>

#include "expression.h"
#include "main.h"
#include "statement.h"
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

    template<typename... TT>
    bool match(const TT&... tokentypes);

    bool check(const TokenType type);
    const Token& advance();
    bool isAtEnd();
    const Token& peek();
    const Token& previous();

    std::shared_ptr<Expression> expression();
    std::shared_ptr<Expression> assignmentExpression();
    std::shared_ptr<Expression> orExpression();
    std::shared_ptr<Expression> andExpression();
    std::shared_ptr<Expression> equalityExpression();
    std::shared_ptr<Expression> comparisonExpression();
    std::shared_ptr<Expression> additionExpression();
    std::shared_ptr<Expression> multiplicationExpression();
    std::shared_ptr<Expression> unaryExpression();
    std::shared_ptr<Expression> primaryExpression();

    std::shared_ptr<Statement> declarationStatement();
    std::shared_ptr<Statement> variableDeclarationStatement();

    std::shared_ptr<Statement> statement();
    std::shared_ptr<Statement> forStatement();
    std::shared_ptr<Statement> ifStatement();
    std::shared_ptr<Statement> printStatement();
    std::shared_ptr<Statement> whileStatement();
    std::vector<std::shared_ptr<Statement>> blockStatement();
    std::shared_ptr<Statement> expressionStatement();

    Token consume(TokenType type, std::string message);
    void synchronise();

 public:
    explicit Parser(const std::vector<Token>& tokens);
    std::vector<std::shared_ptr<Statement>> parse();
};
