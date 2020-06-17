#pragma once

#include <any>
#include <iostream>
#include <sstream>
#include <string>

#include "expression.h"
#include "main.h"
#include "statement.h"
#include "token.h"
#include "environment.h"

class Environment;

class Token;

class RuntimeError : public std::runtime_error {
    std::string message;
    Token token;

 public:
    RuntimeError(const std::string& message, const Token& token);
    const char* what() const noexcept override;
};

class Interpreter : public Visitor, public StatementVisitor, public std::enable_shared_from_this<Interpreter> {
    std::unique_ptr<Environment> environment{std::make_unique<Environment>()};

    std::any evaluate(std::shared_ptr<Expression> expr);
    bool isTruthy(const std::any& object);
    bool isEqual(const Token& token, const std::any& left, const std::any& right);
    void checkNumberOperand(const Token& token, const std::any& operand);
    void checkNumberOperand(const Token& token, const std::any& operand1, const std::any& operand2);

    void execute(std::shared_ptr<Statement> statement);

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

    void visitExpressionStmt(const ExpressionStatement& stmt) override;
    void visitPrintStmt(const PrintStatement& stmt) override;
    void visitVarStmt(const VarStatement& stmt) override;
    void visitBlockStmt(const BlockStatement& stmt) override;
    void visitIfStmt(const IfStatement& stmt) override;
    void visitWhileStmt(const WhileStatement& stmt) override;
    void visitFunctionStmt(const FunctionStatement& stmt) override;
    void visitReturnStmt(const ReturnStatement& stmt) override;
    void visitClassStmt(const ClassStatement& stmt) override;

    void interpret(std::vector<std::shared_ptr<Statement>> statements);
};
