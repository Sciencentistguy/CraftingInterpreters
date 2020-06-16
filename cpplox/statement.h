#pragma once

#include <memory>

#include "expression.h"

class ExpressionStatement;
class PrintStatement;
class VarStatement;
class BlockStatement;
class IfStatement;
class WhileStatement;
class FunctionStatement;
class ReturnStatement;
class ClassStatement;

class StatementVisitor {
 public:
    virtual void visitExpressionStmt(const ExpressionStatement& stmt) = 0;
    virtual void visitPrintStmt(const PrintStatement& stmt) = 0;
    virtual void visitVarStmt(const VarStatement& stmt) = 0;
    virtual void visitBlockStmt(const BlockStatement& stmt) = 0;
    virtual void visitIfStmt(const IfStatement& stmt) = 0;
    virtual void visitWhileStmt(const WhileStatement& stmt) = 0;
    virtual void visitFunctionStmt(const FunctionStatement& stmt) = 0;
    virtual void visitReturnStmt(const ReturnStatement& stmt) = 0;
    virtual void visitClassStmt(const ClassStatement& stmt) = 0;
};

class Statement {
 public:
    virtual void accept(std::shared_ptr<StatementVisitor> visitor) = 0;
};

class ExpressionStatement : public Statement {
 public:
    std::shared_ptr<Expression> expr;

    explicit ExpressionStatement(const std::shared_ptr<Expression> expr);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class PrintStatement : public Statement {
 public:
    std::shared_ptr<Expression> expr;

    explicit PrintStatement(const std::shared_ptr<Expression>& expr);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class VarStatement : public Statement {
 public:
    std::shared_ptr<Expression> initialiser;
    Token name;

    VarStatement(const std::shared_ptr<Expression> initialiser, const Token& name);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class BlockStatement : public Statement {
 public:
    std::vector<std::shared_ptr<Statement>> statements;

    explicit BlockStatement(const std::vector<std::shared_ptr<Statement>>& statements);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class IfStatement : public Statement {
 public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> thenBranch;
    std::shared_ptr<Statement> elseBranch;

    IfStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> thenBranch, const std::shared_ptr<Statement> elseBranch);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class WhileStatement : public Statement {
 public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> body;

    WhileStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> body);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class FunctionStatement : public Statement {
 public:
    Token name;
    std::vector<Token> params;
    std::vector<std::shared_ptr<Statement>> body;

    FunctionStatement(const Token& name, const std::vector<Token>& params, const std::vector<std::shared_ptr<Statement>>& body);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class ReturnStatement : public Statement {
 public:
    Token name;
    std::shared_ptr<Expression> value;

    ReturnStatement(const Token& name, const std::shared_ptr<Expression> value);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};

class ClassStatement : public Statement {
 public:
    Token name;
    std::shared_ptr<Variable> superclass;
    std::vector<std::shared_ptr<FunctionStatement>> methods;

    ClassStatement(const Token& name, const std::shared_ptr<Variable> superclass, const std::vector<std::shared_ptr<FunctionStatement>>& methods);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
};
