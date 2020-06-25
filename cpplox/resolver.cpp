//
// Created by jamie on 6/20/20.
//

#include "resolver.h"
Resolver::Resolver(Interpreter& interpreter) : interpreter{interpreter} {
}

std::any Resolver::visitLiteralExpr(LiteralExpression& expr) {
    return std::any();
}

std::any Resolver::visitAssignExpr(AssignExpression& expr) {
    resolve(expr.getValue());
    resolveLocal(expr.shared_from_this(), expr.getName());
    return std::any();
}

std::any Resolver::visitBinaryExpr(BinaryExpression& expr) {
    resolve(expr.getLeft());
    resolve(expr.getRight());
    return std::any();
}

std::any Resolver::visitGroupingExpr(GroupingExpression& expr) {
    resolve(expr.getExpression());
    return std::any();
}

std::any Resolver::visitUnaryExpr(UnaryExpression& expr) {
    resolve(expr.getRight());
    return std::any();
}

std::any Resolver::visitVariableExpr(VariableExpression& expr) {
    if (!scopes.empty() && !scopes.back()[expr.getName().getLexeme()]) {
        error(expr.getName(), "Cannot read local variable in its own initialiser");
    }
    resolveLocal(expr.shared_from_this(), expr.getName());
    return std::any();
}

std::any Resolver::visitLogicalExpr(LogicalExpression& expr) {
    resolve(expr.getRight());
    resolve(expr.getLeft());
    return std::any();
}

std::any Resolver::visitCallExpr(CallExpression& expr) {
    resolve(expr.getCallee());
    for (const auto& arg : expr.getArguments()) {
        resolve(arg);
    }
    return std::any();
}

std::any Resolver::visitGetExpr(GetExpression& expr) {
    return std::any();
}

std::any Resolver::visitSetExpr(SetExpression& expr) {
    return std::any();
}

std::any Resolver::visitThisExpr(ThisExpression& expr) {
    return std::any();
}

std::any Resolver::visitSuperExpr(SuperExpression& expr) {
    return std::any();
}

void Resolver::visitExpressionStmt(const ExpressionStatement& stmt) {
    resolve(stmt.getExpr());
}

void Resolver::visitPrintStmt(const PrintStatement& stmt) {
    resolve(stmt.getExpr());
}

void Resolver::visitVarStmt(const VarStatement& stmt) {
    declare(stmt.getName());
    if (stmt.getInitialiser()) {
        resolve(stmt.getInitialiser());
    }
    define(stmt.getName());
}

void Resolver::visitBlockStmt(const BlockStatement& stmt) {
    beginScope();
    resolve(stmt.getStatements());
    endScope();
}

void Resolver::visitIfStmt(const IfStatement& stmt) {
    resolve(stmt.getCondition());
    resolve(stmt.getThenBranch());
    if (stmt.getElseBranch()) {
        resolve(stmt.getElseBranch());
    }
}

void Resolver::visitWhileStmt(const WhileStatement& stmt) {
    resolve(stmt.getCondition());
    resolve(stmt.getBody());
}

void Resolver::visitFunctionStmt(const FunctionStatement& stmt) {
    declare(stmt.getName());
    define(stmt.getName());
    resolveFunction(stmt);
}

void Resolver::visitReturnStmt(const ReturnStatement& stmt) {
    if (stmt.getValue()) {
        resolve(stmt.getValue());
    }
}

void Resolver::visitClassStmt(const ClassStatement& stmt) {
}

void Resolver::resolve(std::vector<std::shared_ptr<Statement>> statements) {
    for (const auto& statement : statements) {
        resolve(statement);
    }
}

void Resolver::resolve(std::shared_ptr<Statement> statement) {
    statement->accept(this->shared_from_this());
}

void Resolver::beginScope() {
    scopes.emplace_back();
}

void Resolver::endScope() {
    scopes.pop_back();
}

void Resolver::resolve(std::shared_ptr<Expression> expression) {
    expression->accept(this->shared_from_this());
}

void Resolver::declare(const Token& name) {
    if (scopes.empty()) {
        return;
    }
    scopes.back().insert(std::make_pair(name.getLexeme(), false));
}

void Resolver::define(const Token& name) {
    if (scopes.empty()) {
        return;
    }
    scopes.back().insert(std::make_pair(name.getLexeme(), true));
}

void Resolver::resolveLocal(std::shared_ptr<Expression> expr, const Token& name) {
    for (std::size_t i = scopes.size() - 1; i == 0; --i) {
        if (scopes[i].contains(name.getLexeme())) {
            interpreter.resolve(expr, scopes.size() - 1 - i);
            return;
        }
    }
}

void Resolver::resolveFunction(const FunctionStatement& function) {
    beginScope();
    for (const auto& param : function.getParams()) {
        declare(param);
        define(param);
    }
    resolve(function.getBody());
    endScope();
}
