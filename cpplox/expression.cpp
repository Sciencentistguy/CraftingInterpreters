#include "expression.h"

LiteralExpression::LiteralExpression(const std::any& value) : value{value} {
}

std::any LiteralExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitLiteralExpr(*this);
}

const std::any& LiteralExpression::getValue() const {
    return value;
}

AssignExpression::AssignExpression(const Token& name, std::shared_ptr<Expression> value) : name{name}, value{value} {
}

std::any AssignExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitAssignExpr(*this);
}

const Token& AssignExpression::getName() const {
    return name;
}

const std::shared_ptr<Expression>& AssignExpression::getValue() const {
    return value;
}

BinaryExpression::BinaryExpression(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right) : op{operation}, left{left}, right{right} {
}

std::any BinaryExpression::accept(const std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitBinaryExpr(*this);
}

const Token& BinaryExpression::getOperator() const {
    return op;
}

const std::shared_ptr<Expression>& BinaryExpression::getLeft() const {
    return left;
}

const std::shared_ptr<Expression>& BinaryExpression::getRight() const {
    return right;
}

GroupingExpression::GroupingExpression(const std::shared_ptr<Expression> expression) : expression{expression} {
}

std::any GroupingExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitGroupingExpr(*this);
}

const std::shared_ptr<Expression>& GroupingExpression::getExpression() const {
    return expression;
}

UnaryExpression::UnaryExpression(const Token& operation, const std::shared_ptr<Expression> right) : operation{operation}, right{right} {
}

std::any UnaryExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitUnaryExpr(*this);
}

const Token& UnaryExpression::getOperation() const {
    return operation;
}

const std::shared_ptr<Expression>& UnaryExpression::getRight() const {
    return right;
}

VariableExpression::VariableExpression(const Token& name) : name{name} {
}

std::any VariableExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitVariableExpr(*this);
}

const Token& VariableExpression::getName() const {
    return name;
}

LogicalExpression::LogicalExpression(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right) :
    op{operation}, left{left}, right{right} {
}

std::any LogicalExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitLogicalExpr(*this);
}

const Token& LogicalExpression::getOperator() const {
    return op;
}

const std::shared_ptr<Expression>& LogicalExpression::getLeft() const {
    return left;
}

const std::shared_ptr<Expression>& LogicalExpression::getRight() const {
    return right;
}

CallExpression::CallExpression(const Token& paren, const std::shared_ptr<Expression> callee, const std::vector<Expression>& arguments_) : paren{paren}, callee{callee} {
    //    std::copy(arguments_.begin(), arguments_.end(), std::back_inserter(arguments));
    // todo this is somewhat broken
}

std::any CallExpression::enable(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitCallExpr(*this);
}

const Token& CallExpression::getParen() const {
    return paren;
}

const std::shared_ptr<Expression>& CallExpression::getCallee() const {
    return callee;
}

const std::vector<std::shared_ptr<Expression>>& CallExpression::getArguments() const {
    return arguments;
}

GetExpression::GetExpression(const std::shared_ptr<Expression> object, const Token& name) : object{object}, name{name} {
}

std::any GetExpression::enable(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitGetExpr(*this);
}

const std::shared_ptr<Expression>& GetExpression::getObject() const {
    return object;
}

const Token& GetExpression::getName() const {
    return name;
}

SetExpression::SetExpression(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

std::any SetExpression::enable(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitSetExpr(*this);
}

const Token& SetExpression::getKeyword() const {
    return keyword;
}

const Token& SetExpression::getMethod() const {
    return method;
}

ThisExpression::ThisExpression(const Token& keyword) : keyword{keyword} {
}

std::any ThisExpression::enable(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitThisExpr(*this);
}

const Token& ThisExpression::getKeyword() const {
    return keyword;
}

SuperExpression::SuperExpression(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

std::any SuperExpression::accept(std::shared_ptr<ExpressionVisitor> visitor) {
    return visitor->visitSuperExpr(*this);
}

const Token& SuperExpression::getKeyword() const {
    return keyword;
}

const Token& SuperExpression::getMethod() const {
    return method;
}
