#pragma once

#include <any>
#include <chrono>
#include <iostream>
#include <sstream>
#include <string>

#include "environment.h"
#include "expression.h"
#include "loxcallable.h"
#include "main.h"
#include "statement.h"
#include "token.h"

class LoxFunction;

class Environment;

class Token;

class Interpreter : public ExpressionVisitor, public StatementVisitor, public std::enable_shared_from_this<Interpreter> {
    std::shared_ptr<Environment> globals{std::make_shared<Environment>()};
    std::shared_ptr<std::unordered_map<std::shared_ptr<Expression>, int>> locals{std::make_shared<std::unordered_map<std::shared_ptr<Expression>, int>>()};
    std::shared_ptr<Environment> environment = globals;

    friend class LoxFunction;
    std::any evaluate(std::shared_ptr<Expression> expr);
    bool isTruthy(const std::any& object) const;
    bool isEqual(const Token& token, const std::any& left, const std::any& right) const;
    void checkNumberOperand(const Token& token, const std::any& operand) const;
    void checkNumberOperand(const Token& token, const std::any& operand1, const std::any& operand2) const;
    std::any lookUpVariable(const Token& name, std::shared_ptr<Expression> expr) const;

    void execute(std::shared_ptr<Statement> statement);
    void executeBlock(const std::vector<std::shared_ptr<Statement>>& statements, std::shared_ptr<Environment> environment);

 public:
    Interpreter();

    std::any visitLiteralExpr(LiteralExpression& expr) override;
    std::any visitAssignExpr(AssignExpression& expr) override;
    std::any visitBinaryExpr(BinaryExpression& expr) override;
    std::any visitGroupingExpr(GroupingExpression& expr) override;
    std::any visitUnaryExpr(UnaryExpression& expr) override;
    std::any visitVariableExpr(VariableExpression& expr) override;
    std::any visitLogicalExpr(LogicalExpression& expr) override;
    std::any visitCallExpr(CallExpression& expr) override;
    std::any visitGetExpr(GetExpression& expr) override;
    std::any visitSetExpr(SetExpression& expr) override;
    std::any visitThisExpr(ThisExpression& expr) override;
    std::any visitSuperExpr(SuperExpression& expr) override;

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
    void resolve(std::shared_ptr<Expression> expr, int depth);
};
