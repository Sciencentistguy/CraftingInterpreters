#pragma once

#include <memory>
#include <vector>

#include "token.h"

class Literal;

class Assign;

class Binary;

class Grouping;

class Unary;

class Variable;

class Logical;

class Call;

class Get;

class Set;

class This;

class Super;

class Visitor {
 public:
    virtual std::any visitLiteralExpr(std::shared_ptr<Literal> expr) = 0;
    virtual std::any visitAssignExpr(std::shared_ptr<Assign> expr) = 0;
    virtual std::any visitBinaryExpr(std::shared_ptr<Binary> expr) = 0;
    virtual std::any visitGroupingExpr(std::shared_ptr<Grouping> expr) = 0;
    virtual std::any visitUnaryExpr(std::shared_ptr<Unary> expr) = 0;
    virtual std::any visitVariableExpr(std::shared_ptr<Variable> expr) = 0;
    virtual std::any visitLogicalExpr(std::shared_ptr<Logical> expr) = 0;
    virtual std::any visitCallExpr(std::shared_ptr<Call> expr) = 0;
    virtual std::any visitGetExpr(std::shared_ptr<Get> expr) = 0;
    virtual std::any visitSetExpr(std::shared_ptr<Set> expr) = 0;
    virtual std::any visitThisExpr(std::shared_ptr<This> expr) = 0;
    virtual std::any visitSuperExpr(std::shared_ptr<Super> expr) = 0;
};

class Expression {
 public:
    virtual std::any accept(const std::shared_ptr<Visitor> visitor) = 0;
    //    virtual ~Expression() = default;
};

class Literal : public Expression, public std::enable_shared_from_this<Literal> {
    std::any value;

 public:
    explicit Literal(const std::any& value);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const std::any& getValue() const;
};

class Assign : public Expression, public std::enable_shared_from_this<Assign> {
    Token name;
    std::shared_ptr<Expression> value;

 public:
    Assign(const Token& name, std::shared_ptr<Expression> value);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getName() const;
    const std::shared_ptr<Expression>& getValue() const;
};

class Binary : public Expression, public std::enable_shared_from_this<Binary> {
    Token op;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

 public:
    Binary(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
    std::any accept(const std::shared_ptr<Visitor> visitor) override;
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class Grouping : public Expression, public std::enable_shared_from_this<Grouping> {
    std::shared_ptr<Expression> expression;

 public:
    explicit Grouping(const std::shared_ptr<Expression> expression);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const std::shared_ptr<Expression>& getExpression() const;
};

class Unary : public Expression, public std::enable_shared_from_this<Unary> {
    Token operation;
    std::shared_ptr<Expression> right;

 public:
    Unary(const Token& operation, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getOperation() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class Variable : public Expression, public std::enable_shared_from_this<Variable> {
    Token name;

 public:
    explicit Variable(const Token& name);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getName() const;
};

class Logical : public Expression, public std::enable_shared_from_this<Logical> {
    Token op;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

 public:
    Logical(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getOperator() const;
    const std::shared_ptr<Expression>& getLeft() const;
    const std::shared_ptr<Expression>& getRight() const;
};

class Call : public Expression, public std::enable_shared_from_this<Call> {
    Token paren;
    std::shared_ptr<Expression> callee;
    std::vector<std::shared_ptr<Expression>> arguments;

 public:
    Call(const Token& paren, const std::shared_ptr<Expression> callee, const std::vector<Expression>& arguments_);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getParen() const;
    const std::shared_ptr<Expression>& getCallee() const;
    const std::vector<std::shared_ptr<Expression>>& getArguments() const;
};

class Get : public Expression, public std::enable_shared_from_this<Get> {
    std::shared_ptr<Expression> object;
    Token name;

 public:
    Get(const std::shared_ptr<Expression> object, const Token& name);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const std::shared_ptr<Expression>& getObject() const;
    const Token& getName() const;
};

class Set : public Expression, public std::enable_shared_from_this<Set> {
    Token keyword;
    Token method;

 public:
    Set(const Token& keyword, const Token& method);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
    const Token& getMethod() const;
};

class This : public Expression, public std::enable_shared_from_this<This> {
    Token keyword;

 public:
    explicit This(const Token& keyword);
    std::any enable(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
};

class Super : public Expression, public std::enable_shared_from_this<Super> {
    Token keyword;
    Token method;

 public:
    Super(const Token& keyword, const Token& method);
    std::any accept(std::shared_ptr<Visitor> visitor);
    const Token& getKeyword() const;
    const Token& getMethod() const;
};
