#pragma once

#include <memory>

#include "expression.h"

class StatementVisitor;

class Statement {
 public:
    virtual void accept(StatementVisitor& visitor) = 0;
};

class ExpressionStatement : public Statement, public std::enable_shared_from_this<ExpressionStatement> {
    const std::shared_ptr<Expression> expr;

 public:
    explicit ExpressionStatement(const std::shared_ptr<Expression> expr);
    void accept(StatementVisitor& visitor) override;
    const std::shared_ptr<Expression>& getExpr() const;
};

class PrintStatement : public Statement {
    const std::shared_ptr<Expression> expr;

 public:
    explicit PrintStatement(const std::shared_ptr<Expression>& expr);
    void accept(StatementVisitor& visitor) override;
    std::shared_ptr<Expression> getExpr() const;
};

class VarStatement : public Statement {
    const std::shared_ptr<Expression> initializer;
    const Token name;

 public:
    VarStatement(const std::shared_ptr<Expression> initializer, const Token& name);
    void accept(StatementVisitor& visitor) override;
    const std::shared_ptr<Expression>& getInitializer() const;
    const Token& getName() const;
};

class BlockStatement : public Statement {
    const std::vector<std::shared_ptr<Statement>> statements;

 public:
    explicit BlockStatement(const std::vector<std::shared_ptr<Statement>>& statements);
    void accept(StatementVisitor& visitor) override;
    const std::vector<std::shared_ptr<Statement>>& getStatements() const;
};

class IfStatement : public Statement {
    const std::shared_ptr<Expression> condition;
    const std::shared_ptr<Statement> thenBranch;
    const std::shared_ptr<Statement> elseBranch;

 public:
    IfStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> thenBranch, const std::shared_ptr<Statement> elseBranch);
    void accept(StatementVisitor& visitor) override;
    const std::shared_ptr<Expression>& getCondition() const;
    const std::shared_ptr<Statement>& getThenBranch() const;
    const std::shared_ptr<Statement>& getElseBranch() const;
};

class WhileStatement : public Statement {
    const std::shared_ptr<Expression> condition;
    const std::shared_ptr<Statement> body;

 public:
    WhileStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> body);
    void accept(StatementVisitor& visitor) override;
    const std::shared_ptr<Expression>& getCondition() const;
    const std::shared_ptr<Statement>& getBody() const;
};

class FunctionStatement : public Statement, public std::enable_shared_from_this<FunctionStatement> {
    const Token name;
    const std::vector<Token> params;
    const std::vector<std::shared_ptr<Statement>> body;

 public:
    FunctionStatement(const Token& name, const std::vector<Token>& params, const std::vector<std::shared_ptr<Statement>>& body);
    void accept(StatementVisitor& visitor) override;
    const Token& getName() const;
    const std::vector<Token>& getParams() const;
    const std::vector<std::shared_ptr<Statement>>& getBody() const;
};

class ReturnStatement : public Statement {
    const Token name;
    const std::shared_ptr<Expression> value;

 public:
    ReturnStatement(const Token& name, const std::shared_ptr<Expression> value);
    void accept(StatementVisitor& visitor) override;
    const Token& getName() const;
    const std::shared_ptr<Expression>& getValue() const;
};

class ClassStatement : public Statement {
    const Token name;
    const std::shared_ptr<VariableExpression> superclass;
    const std::vector<std::shared_ptr<FunctionStatement>> methods;

 public:
    ClassStatement(const Token& name, const std::shared_ptr<VariableExpression> superclass, const std::vector<std::shared_ptr<FunctionStatement>>& methods);
    void accept(StatementVisitor& visitor) override;
    const std::shared_ptr<VariableExpression>& getSuperclass() const;
    const std::vector<std::shared_ptr<FunctionStatement>>& getMethods() const;
    const Token& getName() const;
};

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
