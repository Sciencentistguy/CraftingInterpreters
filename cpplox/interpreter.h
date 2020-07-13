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
    std::shared_ptr<std::unordered_map<std::shared_ptr<const Expression>, int>> locals{std::make_shared<std::unordered_map<std::shared_ptr<const Expression>, int>>()};
    std::shared_ptr<Environment> environment = globals;

    friend class LoxFunction;
    std::any evaluate(const std::shared_ptr<Expression>& expr);
    bool isTruthy(const std::any& object) const;
    bool isEqual(const Token& token, const std::any& left, const std::any& right) const;
    void checkNumberOperand(const Token& token, const std::any& operand) const;
    void checkNumberOperand(const Token& token, const std::any& operand1, const std::any& operand2) const;
    std::any lookUpVariable(const Token& name, const std::shared_ptr<const Expression>& expr) const;

    void execute(const std::shared_ptr<Statement>& statement);
    void executeBlock(const std::vector<std::shared_ptr<Statement>>& statements, const std::shared_ptr<Environment>& environment);

 public:
    Interpreter();

    std::any visitLiteralExpr(const LiteralExpression& expr) override;
    std::any visitAssignExpr(const AssignExpression& expr) override;
    std::any visitBinaryExpr(const BinaryExpression& expr) override;
    std::any visitGroupingExpr(const GroupingExpression& expr) override;
    std::any visitUnaryExpr(const UnaryExpression& expr) override;
    std::any visitVariableExpr(const VariableExpression& expr) override;
    std::any visitLogicalExpr(const LogicalExpression& expr) override;
    std::any visitCallExpr(const CallExpression& expr) override;
    std::any visitGetExpr(const GetExpression& expr) override;
    std::any visitSetExpr(const SetExpression& expr) override;
    std::any visitThisExpr(const ThisExpression& expr) override;
    std::any visitSuperExpr(const SuperExpression& expr) override;

    void visitExpressionStmt(const ExpressionStatement& stmt) override;
    void visitPrintStmt(const PrintStatement& stmt) override;
    void visitVarStmt(const VarStatement& stmt) override;
    void visitBlockStmt(const BlockStatement& stmt) override;
    void visitIfStmt(const IfStatement& stmt) override;
    void visitWhileStmt(const WhileStatement& stmt) override;
    void visitFunctionStmt(const FunctionStatement& stmt) override;
    void visitReturnStmt(const ReturnStatement& stmt) override;
    void visitClassStmt(const ClassStatement& stmt) override;

    void interpret(const std::vector<std::shared_ptr<Statement>>& statements);
    void resolve(const std::shared_ptr<const Expression>& expr, int depth);
};
