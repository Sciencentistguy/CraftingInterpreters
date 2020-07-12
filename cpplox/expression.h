#pragma once

#include <memory>
#include <vector>

#include "token.h"
class ExpressionVisitor;

class Expression {
 public:
    virtual std::any accept(const std::shared_ptr<ExpressionVisitor> visitor) = 0;
};

class LiteralExpression : public Expression, public std::enable_shared_from_this<LiteralExpression> {
    const std::any value;

 public:
    explicit LiteralExpression(const std::any& value);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const std::any& getValue() const;
};

class AssignExpression : public Expression, public std::enable_shared_from_this<AssignExpression> {
    const Token name;
    const std::shared_ptr<Expression> value;

 public:
    AssignExpression(const Token& name, std::shared_ptr<Expression> value);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getName() const;
    const std::shared_ptr<Expression>& getValue() const;
};

class BinaryExpression : public Expression, public std::enable_shared_from_this<BinaryExpression> {
    const Token op;
    const std::shared_ptr<Expression> left;
    const std::shared_ptr<Expression> right;

 public:
    BinaryExpression(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
    std::any accept(const std::shared_ptr<ExpressionVisitor> visitor) override;
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class GroupingExpression : public Expression, public std::enable_shared_from_this<GroupingExpression> {
    const std::shared_ptr<Expression> expression;

 public:
    explicit GroupingExpression(const std::shared_ptr<Expression> expression);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const std::shared_ptr<Expression>& getExpression() const;
};

class UnaryExpression : public Expression, public std::enable_shared_from_this<UnaryExpression> {
    const Token operation;
    const std::shared_ptr<Expression> right;

 public:
    UnaryExpression(const Token& operation, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getOperation() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class VariableExpression : public Expression, public std::enable_shared_from_this<VariableExpression> {
    const Token name;

 public:
    explicit VariableExpression(const Token& name);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getName() const;
};

class LogicalExpression : public Expression, public std::enable_shared_from_this<LogicalExpression> {
    const Token op;
    const std::shared_ptr<Expression> left;
    const std::shared_ptr<Expression> right;

 public:
    LogicalExpression(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class CallExpression : public Expression, public std::enable_shared_from_this<CallExpression> {
    const Token paren;
    const std::shared_ptr<Expression> callee;
    const std::vector<std::shared_ptr<Expression>> arguments;

 public:
    CallExpression(const Token& paren, const std::shared_ptr<Expression> callee, const std::vector<std::shared_ptr<Expression>>& arguments);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor) override;
    const Token& getParen() const;
    const std::shared_ptr<Expression>& getCallee() const;
    const std::vector<std::shared_ptr<Expression>>& getArguments() const;
};

class GetExpression : public Expression, public std::enable_shared_from_this<GetExpression> {
    const std::shared_ptr<Expression> object;
    const Token name;

 public:
    GetExpression(const std::shared_ptr<Expression> object, const Token& name);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const std::shared_ptr<Expression>& getObject() const;
    const Token& getName() const;
};

class SetExpression : public Expression, public std::enable_shared_from_this<SetExpression> {
    const Token name;
    const std::shared_ptr<Expression> object;
    const std::shared_ptr<Expression> value;

 public:
    SetExpression(const Token& name, const std::shared_ptr<Expression>& object, const std::shared_ptr<Expression>& value);

 public:
    const Token& getName() const;
    const std::shared_ptr<Expression>& getObject() const;
    const std::shared_ptr<Expression>& getValue() const;
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
};

class ThisExpression : public Expression, public std::enable_shared_from_this<ThisExpression> {
    const Token keyword;

 public:
    explicit ThisExpression(const Token& keyword);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getKeyword() const;
};

class SuperExpression : public Expression, public std::enable_shared_from_this<SuperExpression> {
    const Token keyword;
    const Token method;

 public:
    SuperExpression(const Token& keyword, const Token& method);
    std::any accept(std::shared_ptr<ExpressionVisitor> visitor);
    const Token& getKeyword() const;
    const Token& getMethod() const;
};

class ExpressionVisitor {
 public:
    virtual std::any visitLiteralExpr(LiteralExpression& expr) = 0;
    virtual std::any visitAssignExpr(AssignExpression& expr) = 0;
    virtual std::any visitBinaryExpr(BinaryExpression& expr) = 0;
    virtual std::any visitGroupingExpr(GroupingExpression& expr) = 0;
    virtual std::any visitUnaryExpr(UnaryExpression& expr) = 0;
    virtual std::any visitVariableExpr(VariableExpression& expr) = 0;
    virtual std::any visitLogicalExpr(LogicalExpression& expr) = 0;
    virtual std::any visitCallExpr(CallExpression& expr) = 0;
    virtual std::any visitGetExpr(GetExpression& expr) = 0;
    virtual std::any visitSetExpr(SetExpression& expr) = 0;
    virtual std::any visitThisExpr(ThisExpression& expr) = 0;
    virtual std::any visitSuperExpr(SuperExpression& expr) = 0;
};
