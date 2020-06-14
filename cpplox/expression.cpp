#include "expression.h"

template<class T>
Literal<T>::Literal(const T& value) : value{value} {
}

template<class T>
T Literal<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitLiteralExpr(this.shared_from_this());
}

template<class T>
Assign<T>::Assign(const Token& name, std::shared_ptr<Expression<T>> value) : name{name}, value{value} {
}

template<class T>
T Assign<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitAssignExpr(this->enable_shared_from_this());
}

template<class T>
Binary<T>::Binary(const Token& operation, std::shared_ptr<Expression<T>> left, std::shared_ptr<Expression<T>> right) :
    operation{operation}, left{left}, right{right} {
}

template<class T>
T Binary<T>::accept(const std::shared_ptr<T> visitor) {
    return visitor->visitBinaryExpr(this->shared_from_this());
}

template<class T>
Grouping<T>::Grouping(const std::shared_ptr<Expression<T>> expression) : expression{expression} {
}

template<class T>
T Grouping<T>::accept(std::shared_ptr<T> visitor) {
    return visitor->visitGroupingExpr(this->shared_from_this());
}

template<class T>
Unary<T>::Unary(const Token& operation, const std::shared_ptr<Expression<T>> right) : operation{operation}, right{right} {
}

template<class T>
T Unary<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitUnaryExpr(this->shared_from_this());
}

template<class T>
Variable<T>::Variable(const Token& name) : name{name} {
}

template<class T>
T Variable<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitVariableExpr(this->shared_from_this());
}

template<class T>
Logical<T>::Logical(const Token& operation, const std::shared_ptr<T> left, const std::shared_ptr<T> right) : operation{operation}, left{left}, right{right} {
}

template<class T>
T Logical<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitLogicalExpr(this->shared_from_this());
}

template<class T>
Call<T>::Call(const Token& paren, const std::shared_ptr<T> callee, std::vector<std::shared_ptr<Expression<T>>> arguments) :
    paren{paren}, callee{callee}, arguments{arguments} {
}

template<class T>
T Call<T>::enable(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitCallExpr(this->shared_from_this());
}

template<class T>
Get<T>::Get(const std::shared_ptr<T> object, const Token& name) : object{object}, name{name} {
}

template<class T>
T Get<T>::enable(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitGetExpr(this->shared_from_this());
}

template<class T>
Set<T>::Set(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}
template<class T>
T Set<T>::enable(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitSetExpr(this->shared_from_this());
}

template<class T>
This<T>::This(const Token& keyword) : keyword{keyword} {
}

template<class T>
T This<T>::enable(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitThisExpr(this->shared_from_this());
}

template<class T>
Super<T>::Super(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

template<class T>
T Super<T>::accept(std::shared_ptr<Visitor<T>> visitor) {
    return visitor->visitSuperExpr(this->shared_from_this());
}
