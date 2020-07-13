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
    if (!scopes.empty()) {
        auto scopeBackIt{scopes.back().find(expr.getName().getLexeme())};
        if (scopeBackIt != scopes.back().end()) {  // exists
            if (!scopeBackIt->second) {
                error(expr.getName(), "Cannot read local variable in its own initializer");
            }
        }
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
    resolve(expr.getObject());
    return std::any();
}

std::any Resolver::visitSetExpr(SetExpression& expr) {
    resolve(expr.getValue());
    resolve(expr.getObject());
    return std::any();
}

std::any Resolver::visitThisExpr(ThisExpression& expr) {
    if (currentClass == ClassType::None) {
        error(expr.getKeyword(), "Cannot use 'this' outside of a class.");
        return std::any();
    }
    resolveLocal(expr.shared_from_this(), expr.getKeyword());
    return std::any();
}

std::any Resolver::visitSuperExpr(SuperExpression& expr) {
    if (currentClass == ClassType::None) {
        error(expr.getKeyword(), "Cannot use 'super' outside of a class.");
    }
    if (currentClass == ClassType::Class) {
        error(expr.getKeyword(), "Cannot use 'super' in a class with no superclass.");
    }
    resolveLocal(expr.shared_from_this(), expr.getKeyword());
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
    if (stmt.getInitializer()) {
        resolve(stmt.getInitializer());
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
    resolveFunction(stmt, FunctionType::Function);
}

void Resolver::visitReturnStmt(const ReturnStatement& stmt) {
    if (currentFunction == FunctionType::None) {
        error(stmt.getName(), "Return statement not allowed outside function.");
    }
    if (stmt.getValue()) {
        if (currentFunction == FunctionType::Constructor) {
            error(stmt.getName(), "Cannot return a value from a constructor.");
        }
        resolve(stmt.getValue());
    }
}

void Resolver::visitClassStmt(const ClassStatement& stmt) {
    auto enclosingClass{currentClass};
    currentClass = ClassType::Class;

    declare(stmt.getName());
    define(stmt.getName());
    if (stmt.getSuperclass()) {
        if (stmt.getName().getLexeme() == stmt.getSuperclass()->getName().getLexeme()) {
            error(stmt.getSuperclass()->getName(), "A class cannot inherit from itself.");
        }
        currentClass = ClassType::SubClass;
        resolve(stmt.getSuperclass());

        beginScope();
        scopes.back().insert(std::make_pair("super", true));
    }

    beginScope();
    scopes.back().insert(std::make_pair("this", true));
    for (const auto& method : stmt.getMethods()) {
        resolveFunction(*method, method->getName().getLexeme() == "init" ? FunctionType::Constructor : FunctionType::Method);
    }
    endScope();
    if (stmt.getSuperclass()) {
        endScope();
    }

    currentClass = enclosingClass;
}

void Resolver::resolve(std::vector<std::shared_ptr<Statement>> statements) {
    for (const auto& statement : statements) {
        resolve(statement);
    }
}

void Resolver::resolve(std::shared_ptr<Statement> statement) {
    statement->accept(*this);
}

void Resolver::beginScope() {
    scopes.emplace_back();
}

void Resolver::endScope() {
    scopes.pop_back();
}

void Resolver::resolve(std::shared_ptr<Expression> expression) {
    expression->accept(*this);
}

void Resolver::declare(const Token& name) {
    if (scopes.empty()) {
        return;
    }
    if (scopes.back().contains(name.getLexeme())) {
        error(name, "Variable with this name already declared in this scope.");
    }
    scopes.back().insert(std::make_pair(name.getLexeme(), false));
}

void Resolver::define(const Token& name) {
    if (scopes.empty()) {
        return;
    }
    try {
        scopes.back().at(name.getLexeme()) = true;
    } catch (const std::out_of_range& e) {
        scopes.back().insert(std::make_pair(name.getLexeme(), true));
    }
}

void Resolver::resolveLocal(std::shared_ptr<Expression> expr, const Token& name) {
    for (int i = scopes.size() - 1; i >= 0; --i) {
        if (scopes[i].contains(name.getLexeme())) {
            interpreter.resolve(expr, scopes.size() - 1 - i);
            return;
        }
    }
}

void Resolver::resolveFunction(const FunctionStatement& function, FunctionType type) {
    auto enclosingFunction{currentFunction};
    currentFunction = type;

    beginScope();
    for (const auto& param : function.getParams()) {
        declare(param);
        define(param);
    }
    resolve(function.getBody());
    endScope();

    currentFunction = enclosingFunction;
}
