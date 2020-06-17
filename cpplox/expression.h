#pragma once

#include <memory>
#include <vector>

#include "token.h"

class LiteralExpression;

class AssignExpression;

class BinaryExpression;

class GroupingExpression;

class UnaryExpression;

class VariableExpression;

class LogicalExpression;

class CallExpression;

class GetExpression;

class SetExpression;

class ThisExpression;

class SuperExpression;

class Visitor {
 public:
    virtual std::any visitLiteralExpr(std::shared_ptr<LiteralExpression> expr) = 0;
    virtual std::any visitAssignExpr(std::shared_ptr<AssignExpression> expr) = 0;
    virtual std::any visitBinaryExpr(std::shared_ptr<BinaryExpression> expr) = 0;
    virtual std::any visitGroupingExpr(std::shared_ptr<GroupingExpression> expr) = 0;
    virtual std::any visitUnaryExpr(std::shared_ptr<UnaryExpression> expr) = 0;
    virtual std::any visitVariableExpr(std::shared_ptr<VariableExpression> expr) = 0;
    virtual std::any visitLogicalExpr(std::shared_ptr<LogicalExpression> expr) = 0;
    virtual std::any visitCallExpr(std::shared_ptr<CallExpression> expr) = 0;
    virtual std::any visitGetExpr(std::shared_ptr<GetExpression> expr) = 0;
    virtual std::any visitSetExpr(std::shared_ptr<SetExpression> expr) = 0;
    virtual std::any visitThisExpr(std::shared_ptr<ThisExpression> expr) = 0;
    virtual std::any visitSuperExpr(std::shared_ptr<SuperExpression> expr) = 0;
};

class Expression {
 public:
    virtual std::any accept(const std::shared_ptr<Visitor> visitor) = 0;
    //    virtual ~Expression() = default;
};

class LiteralExpression : public Expression, public std::enable_shared_from_this<LiteralExpression> {
    std::any value;

 public:
    explicit LiteralExpression(const std::any& value);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const std::any& getValue() const;
};

class AssignExpression : public Expression, public std::enable_shared_from_this<AssignExpression> {
    Token name;
    std::shared_ptr<Expression> value;

 public:
    AssignExpression(const Token& name, std::shared_ptr<Expression> value);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getName() const;
    const std::shared_ptr<Expression>& getValue() const;
};

class BinaryExpression : public Expression, public std::enable_shared_from_this<BinaryExpression> {
    Token op;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

 public:
    BinaryExpression(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
    std::any accept(const std::shared_ptr<Visitor> visitor) override;
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class GroupingExpression : public Expression, public std::enable_shared_from_this<GroupingExpression> {
    std::shared_ptr<Expression> expression;

 public:
    explicit GroupingExpression(const std::shared_ptr<Expression> expression);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const std::shared_ptr<Expression>& getExpression() const;
};

class UnaryExpression : public Expression, public std::enable_shared_from_this<UnaryExpression> {
    Token operation;
    std::shared_ptr<Expression> right;

 public:
    UnaryExpression(const Token& operation, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getOperation() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class VariableExpression : public Expression, public std::enable_shared_from_this<VariableExpression> {
    Token name;

 public:
    explicit VariableExpression(const Token& name);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getName() const;
};

class LogicalExpression : public Expression, public std::enable_shared_from_this<LogicalExpression> {
    Token op;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

 public:
    LogicalExpression(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class CallExpression : public Expression, public std::enable_shared_from_this<CallExpression> {
    Token paren;
    std::shared_ptr<Expression> callee;
    std::vector<std::shared_ptr<Expression>> arguments;

 public:
    CallExpression(const Token& paren, const std::shared_ptr<Expression> callee, const std::vector<Expression>& arguments_);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getParen() const;
    const std::shared_ptr<Expression>& getCallee() const;
    const std::vector<std::shared_ptr<Expression>>& getArguments() const;
};

class GetExpression : public Expression, public std::enable_shared_from_this<GetExpression> {
    std::shared_ptr<Expression> object;
    Token name;

 public:
    GetExpression(const std::shared_ptr<Expression> object, const Token& name);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const std::shared_ptr<Expression>& getObject() const;
    const Token& getName() const;
};

class SetExpression : public Expression, public std::enable_shared_from_this<SetExpression> {
    Token keyword;
    Token method;

 public:
    SetExpression(const Token& keyword, const Token& method);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
    const Token& getMethod() const;
};

class ThisExpression : public Expression, public std::enable_shared_from_this<ThisExpression> {
    Token keyword;

 public:
    explicit ThisExpression(const Token& keyword);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
};

class SuperExpression : public Expression, public std::enable_shared_from_this<SuperExpression> {
    Token keyword;
    Token method;

 public:
    SuperExpression(const Token& keyword, const Token& method);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
    const Token& getMethod() const;
};
