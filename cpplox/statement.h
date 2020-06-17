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
    std::shared_ptr<Expression> expr;

 public:
    explicit ExpressionStatement(const std::shared_ptr<Expression> expr);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<Expression>& getExpr() const;
};

class PrintStatement : public Statement {
    std::shared_ptr<Expression> expr;

 public:
    explicit PrintStatement(const std::shared_ptr<Expression>& expr);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<Expression>& getExpr() const;
};

class VarStatement : public Statement {
    std::shared_ptr<Expression> initialiser;
    Token name;

 public:
    VarStatement(const std::shared_ptr<Expression> initialiser, const Token& name);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<Expression>& getInitialiser() const;
    const Token& getName() const;
};

class BlockStatement : public Statement {
    std::vector<std::shared_ptr<Statement>> statements;

 public:
    explicit BlockStatement(const std::vector<std::shared_ptr<Statement>>& statements);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::vector<std::shared_ptr<Statement>>& getStatements() const;
};

class IfStatement : public Statement {
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> thenBranch;
    std::shared_ptr<Statement> elseBranch;

 public:
    IfStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> thenBranch, const std::shared_ptr<Statement> elseBranch);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<Expression>& getCondition() const;
    const std::shared_ptr<Statement>& getThenBranch() const;
    const std::shared_ptr<Statement>& getElseBranch() const;
};

class WhileStatement : public Statement {
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> body;

 public:
    WhileStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> body);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<Expression>& getCondition() const;
    const std::shared_ptr<Statement>& getBody() const;
};

class FunctionStatement : public Statement {
    Token name;
    std::vector<Token> params;
    std::vector<std::shared_ptr<Statement>> body;

 public:
    FunctionStatement(const Token& name, const std::vector<Token>& params, const std::vector<std::shared_ptr<Statement>>& body);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const Token& getName() const;
    const std::vector<Token>& getParams() const;
    const std::vector<std::shared_ptr<Statement>>& getBody() const;
};

class ReturnStatement : public Statement {
    Token name;
    std::shared_ptr<Expression> value;

 public:
    ReturnStatement(const Token& name, const std::shared_ptr<Expression> value);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const Token& getName() const;
    const std::shared_ptr<Expression>& getValue() const;
};

class ClassStatement : public Statement {
    Token name;
    std::shared_ptr<VariableExpression> superclass;
    std::vector<std::shared_ptr<FunctionStatement>> methods;

 public:
    ClassStatement(const Token& name, const std::shared_ptr<VariableExpression> superclass, const std::vector<std::shared_ptr<FunctionStatement>>& methods);
    void accept(std::shared_ptr<StatementVisitor> visitor) override;
    const std::shared_ptr<VariableExpression>& getSuperclass() const;
    const std::vector<std::shared_ptr<FunctionStatement>>& getMethods() const;
    const Token& getName() const;
};
