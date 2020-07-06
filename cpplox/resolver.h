#pragma once
#include <memory>
#include <vector>

#include "environment.h"
#include "interpreter.h"

class Resolver : public ExpressionVisitor, public StatementVisitor, public std::enable_shared_from_this<Resolver> {
    enum class FunctionType { None, Function, Constructor, Method };
    enum class ClassType { None, Class };

    Interpreter& interpreter;
    std::vector<std::unordered_map<std::string, bool>> scopes{};
    FunctionType currentFunction{FunctionType::None};
    ClassType currentClass{ClassType::None};

    void beginScope();
    void endScope();
    void declare(const Token& name);
    void define(const Token& name);
    void resolveLocal(std::shared_ptr<Expression> expr, const Token& name);
    void resolveFunction(const FunctionStatement& function, FunctionType type);

 public:
    explicit Resolver(Interpreter& interpreter);

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

    void resolve(std::vector<std::shared_ptr<Statement>> statements);
    void resolve(std::shared_ptr<Statement> statement);
    void resolve(std::shared_ptr<Expression> expression);
};
