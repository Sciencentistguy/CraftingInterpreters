#pragma once

#include <memory>
#include <vector>

#include "token.h"

template<class T>
class Literal;

template<class T>
class Assign;

template<class T>
class Binary;

template<class T>
class Grouping;

template<class T>
class Unary;

template<class T>
class Variable;

template<class T>
class Logical;

template<class T>
class Call;

template<class T>
class Get;

template<class T>
class Set;

template<class T>
class This;

template<class T>
class Super;

template<class T>
class Visitor {
 public:
    virtual T visitLiteralExpr(std::shared_ptr<Literal<T>> expr) = 0;
    virtual T visitAssignExpr(std::shared_ptr<Assign<T>> expr) = 0;
    virtual T visitBinaryExpr(std::shared_ptr<Binary<T>> expr) = 0;
    virtual T visitGroupingExpr(std::shared_ptr<Grouping<T>> expr) = 0;
    virtual T visitUnaryExpr(std::shared_ptr<Unary<T>> expr) = 0;
    virtual T visitVariableExpr(std::shared_ptr<Variable<T>> expr) = 0;
    virtual T visitLogicalExpr(std::shared_ptr<Logical<T>> expr) = 0;
    virtual T visitCallExpr(std::shared_ptr<Call<T>> expr) = 0;
    virtual T visitGetExpr(std::shared_ptr<Get<T>> expr) = 0;
    virtual T visitSetExpr(std::shared_ptr<Set<T>> expr) = 0;
    virtual T visitThisExpr(std::shared_ptr<This<T>> expr) = 0;
    virtual T visitSuperExpr(std::shared_ptr<Super<T>> expr) = 0;
};

template<class T>
class Expression {
 public:
    virtual T accept(const std::shared_ptr<T> visitor) = 0;
    //    virtual ~Expression() = default;
};

template<class T>
class Literal : public Expression<T>, public std::enable_shared_from_this<Literal<T>> {
 public:
    T value;

    explicit Literal(const T& value);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Assign : public Expression<T>, public std::enable_shared_from_this<Assign<T>> {
 public:
    Token name;
    std::shared_ptr<Expression<T>> value;

    Assign(const Token& name, std::shared_ptr<Expression<T>> value);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Binary : public Expression<T>, public std::enable_shared_from_this<Binary<T>> {
 public:
    Token operation;
    std::shared_ptr<Expression<T>> left;
    std::shared_ptr<Expression<T>> right;

    Binary(const Token& operation, std::shared_ptr<Expression<T>> left, std::shared_ptr<Expression<T>> right);
    T accept(const std::shared_ptr<T> visitor) override;
};

template<class T>
class Grouping : public Expression<T>, public std::enable_shared_from_this<Grouping<T>> {
 public:
    std::shared_ptr<Expression<T>> expression;

    Grouping(const std::shared_ptr<Expression<T>> expression);
    T accept(std::shared_ptr<T> visitor) override;
};

template<class T>
class Unary : public Expression<T>, public std::enable_shared_from_this<Unary<T>> {
 public:
    Token operation;
    std::shared_ptr<Expression<T>> right;

    Unary(const Token& operation, const std::shared_ptr<Expression<T>> right);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Variable : public Expression<T>, public std::enable_shared_from_this<Variable<T>> {
 public:
    Token name;

    Variable(const Token& name);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Logical : public Expression<T>, public std::enable_shared_from_this<Logical<T>> {
 public:
    Token operation;
    std::shared_ptr<Expression<T>> left;
    std::shared_ptr<Expression<T>> right;

    Logical(const Token& operation, const std::shared_ptr<T> left, const std::shared_ptr<T> right);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Call : public Expression<T>, public std::enable_shared_from_this<Call<T>> {
 public:
    Token paren;
    std::shared_ptr<Expression<T>> callee;
    std::vector<std::shared_ptr<Expression<T>>> arguments;

    Call(const Token& paren, const std::shared_ptr<T> callee, std::vector<std::shared_ptr<Expression<T>>> arguments);
    T enable(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Get : public Expression<T>, public std::enable_shared_from_this<Get<T>> {
 public:
    std::shared_ptr<Expression<T>> object;
    Token name;

    Get(const std::shared_ptr<T> object, const Token& name);
    T enable(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Set : public Expression<T>, public std::enable_shared_from_this<Set<T>> {
 public:
    Token keyword;
    Token method;

    Set(const Token& keyword, const Token& method);
    T enable(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class This : public Expression<T>, public std::enable_shared_from_this<This<T>> {
 public:
    Token keyword;

    This(const Token& keyword);
    T enable(std::shared_ptr<Visitor<T>> visitor) override;
};

template<class T>
class Super : public Expression<T>, public std::enable_shared_from_this<Super<T>> {
 public:
    Token keyword;
    Token method;

    Super(const Token& keyword, const Token& method);
    T accept(std::shared_ptr<Visitor<T>> visitor) override;
};
