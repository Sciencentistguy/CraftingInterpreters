#pragma once

#include <any>
#include <iostream>
#include <sstream>
#include <string>

#include "expression.h"
#include "main.h"
#include "token.h"
class Token;

class RuntimeError : public std::runtime_error {
    Token token;
    std::string message;

 public:
    RuntimeError(const std::string& message, const Token& token);
    const char* what() const noexcept override;
};

class Interpreter : public Visitor, public std::enable_shared_from_this<Interpreter> {
    std::any evaluate(std::shared_ptr<Expression> expr);
    bool isTruthy(const std::any& object);
    bool isEqual(const Token& token, const std::any& left, const std::any& right);
    void checkNumberOperand(const Token& token, const std::any& operand);
    void checkNumberOperand(const Token& token, const std::any& operand1, const std::any& operand2);

 public:
    std::any visitLiteralExpr(std::shared_ptr<Literal> expr) override;
    std::any visitAssignExpr(std::shared_ptr<Assign> expr) override;
    std::any visitBinaryExpr(std::shared_ptr<Binary> expr) override;
    std::any visitGroupingExpr(std::shared_ptr<Grouping> expr) override;
    std::any visitUnaryExpr(std::shared_ptr<Unary> expr) override;
    std::any visitVariableExpr(std::shared_ptr<Variable> expr) override;
    std::any visitLogicalExpr(std::shared_ptr<Logical> expr) override;
    std::any visitCallExpr(std::shared_ptr<Call> expr) override;
    std::any visitGetExpr(std::shared_ptr<Get> expr) override;
    std::any visitSetExpr(std::shared_ptr<Set> expr) override;
    std::any visitThisExpr(std::shared_ptr<This> expr) override;
    std::any visitSuperExpr(std::shared_ptr<Super> expr) override;

    void interpret(std::shared_ptr<Expression> expr);
};
