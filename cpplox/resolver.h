#pragma once
#include <memory>
#include <vector>

#include "environment.h"
#include "expression.h"
#include "interpreter.h"
#include "statement.h"

class Resolver : public ExpressionVisitor, public StatementVisitor, public std::enable_shared_from_this<Resolver> {
    enum class FunctionType { None, Function, Constructor, Method };
    enum class ClassType { None, Class, SubClass };

    Interpreter& interpreter;
    std::vector<std::unordered_map<std::string, bool>> scopes{};
    FunctionType currentFunction{FunctionType::None};
    ClassType currentClass{ClassType::None};

    void beginScope();
    void endScope();
    void declare(const Token& name);
    void define(const Token& name);
    void resolveLocal(const std::shared_ptr<const Expression>& expr, const Token& name);
    void resolveFunction(const FunctionStatement& function, FunctionType type);

 public:
    explicit Resolver(Interpreter& interpreter);

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

    void resolve(const std::vector<std::shared_ptr<Statement>>& statements);
    void resolve(const std::shared_ptr<Statement>& statement);
    void resolve(const std::shared_ptr<Expression>& expression);
};
