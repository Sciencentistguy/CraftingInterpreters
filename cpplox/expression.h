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
 public:
    std::any value;

    explicit Literal(const std::any& value);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Assign : public Expression, public std::enable_shared_from_this<Assign> {
 public:
    Token name;
    std::shared_ptr<Expression> value;

    Assign(const Token& name, std::shared_ptr<Expression> value);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Binary : public Expression, public std::enable_shared_from_this<Binary> {
 public:
    Token operation;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

    Binary(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
    std::any accept(const std::shared_ptr<Visitor> visitor);
};

class Grouping : public Expression, public std::enable_shared_from_this<Grouping> {
 public:
    std::shared_ptr<Expression> expression;

    Grouping(const std::shared_ptr<Expression> expression);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Unary : public Expression, public std::enable_shared_from_this<Unary> {
 public:
    Token operation;
    std::shared_ptr<Expression> right;

    Unary(const Token& operation, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Variable : public Expression, public std::enable_shared_from_this<Variable> {
 public:
    Token name;

    Variable(const Token& name);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Logical : public Expression, public std::enable_shared_from_this<Logical> {
 public:
    Token operation;
    std::shared_ptr<Expression> left;
    std::shared_ptr<Expression> right;

    Logical(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right);
    std::any accept(std::shared_ptr<Visitor> visitor);
};

class Call : public Expression, public std::enable_shared_from_this<Call> {
 public:
    Token paren;
    std::shared_ptr<Expression> callee;
    std::vector<std::shared_ptr<Expression>> arguments;

    Call(const Token& paren, const std::shared_ptr<Expression> callee, std::vector<std::shared_ptr<Expression>> arguments);
    std::any enable(std::shared_ptr<Visitor> visitor);
};

class Get : public Expression, public std::enable_shared_from_this<Get> {
 public:
    std::shared_ptr<Expression> object;
    Token name;

    Get(const std::shared_ptr<Expression> object, const Token& name);
    std::any enable(std::shared_ptr<Visitor> visitor);
};

class Set : public Expression, public std::enable_shared_from_this<Set> {
 public:
    Token keyword;
    Token method;

    Set(const Token& keyword, const Token& method);
    std::any enable(std::shared_ptr<Visitor> visitor);
};

class This : public Expression, public std::enable_shared_from_this<This> {
 public:
    Token keyword;

    This(const Token& keyword);
    std::any enable(std::shared_ptr<Visitor> visitor);
};

class Super : public Expression, public std::enable_shared_from_this<Super> {
 public:
    Token keyword;
    Token method;

    Super(const Token& keyword, const Token& method);
    std::any accept(std::shared_ptr<Visitor> visitor);
};
