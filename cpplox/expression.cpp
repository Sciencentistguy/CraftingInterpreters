#include "expression.h"

Literal::Literal(const std::any& value) : value{value} {
}

std::any Literal::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitLiteralExpr(this->shared_from_this());
}

const std::any& Literal::getValue() const {
    return value;
}

Assign::Assign(const Token& name, std::shared_ptr<Expression> value) : name{name}, value{value} {
}

std::any Assign::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitAssignExpr(this->shared_from_this());
}

const Token& Assign::getName() const {
    return name;
}

const std::shared_ptr<Expression>& Assign::getValue() const {
    return value;
}

Binary::Binary(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right) : op{operation}, left{left}, right{right} {
}

std::any Binary::accept(const std::shared_ptr<Visitor> visitor) {
    return visitor->visitBinaryExpr(this->shared_from_this());
}

const Token& Binary::getOperator() const {
    return op;
}

const std::shared_ptr<Expression>& Binary::getLeft() const {
    return left;
}

const std::shared_ptr<Expression>& Binary::getRight() const {
    return right;
}

Grouping::Grouping(const std::shared_ptr<Expression> expression) : expression{expression} {
}

std::any Grouping::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitGroupingExpr(this->shared_from_this());
}

const std::shared_ptr<Expression>& Grouping::getExpression() const {
    return expression;
}

Unary::Unary(const Token& operation, const std::shared_ptr<Expression> right) : operation{operation}, right{right} {
}

std::any Unary::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitUnaryExpr(this->shared_from_this());
}

const Token& Unary::getOperation() const {
    return operation;
}

const std::shared_ptr<Expression>& Unary::getRight() const {
    return right;
}

Variable::Variable(const Token& name) : name{name} {
}

std::any Variable::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitVariableExpr(this->shared_from_this());
}

const Token& Variable::getName() const {
    return name;
}

Logical::Logical(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right) :
    op{operation}, left{left}, right{right} {
}

std::any Logical::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitLogicalExpr(this->shared_from_this());
}

const Token& Logical::getOperator() const {
    return op;
}

const std::shared_ptr<Expression>& Logical::getLeft() const {
    return left;
}

const std::shared_ptr<Expression>& Logical::getRight() const {
    return right;
}

Call::Call(const Token& paren, const std::shared_ptr<Expression> callee, const std::vector<Expression>& arguments_) : paren{paren}, callee{callee} {
    //    std::copy(arguments_.begin(), arguments_.end(), std::back_inserter(arguments));
    // todo this is somewhat broken
}

std::any Call::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitCallExpr(this->shared_from_this());
}

const Token& Call::getParen() const {
    return paren;
}

const std::shared_ptr<Expression>& Call::getCallee() const {
    return callee;
}

const std::vector<std::shared_ptr<Expression>>& Call::getArguments() const {
    return arguments;
}

Get::Get(const std::shared_ptr<Expression> object, const Token& name) : object{object}, name{name} {
}

std::any Get::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitGetExpr(this->shared_from_this());
}

const std::shared_ptr<Expression>& Get::getObject() const {
    return object;
}

const Token& Get::getName() const {
    return name;
}

Set::Set(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

std::any Set::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitSetExpr(this->shared_from_this());
}

const Token& Set::getKeyword() const {
    return keyword;
}

const Token& Set::getMethod() const {
    return method;
}

This::This(const Token& keyword) : keyword{keyword} {
}

std::any This::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitThisExpr(this->shared_from_this());
}

const Token& This::getKeyword() const {
    return keyword;
}

Super::Super(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

std::any Super::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitSuperExpr(this->shared_from_this());
}

const Token& Super::getKeyword() const {
    return keyword;
}

const Token& Super::getMethod() const {
    return method;
}
