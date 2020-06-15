#include "expression.h"

Literal::Literal(const std::any& value) : value{value} {
}

std::any Literal::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitLiteralExpr(this->shared_from_this());
}

Assign::Assign(const Token& name, std::shared_ptr<Expression> value) : name{name}, value{value} {
}


std::any Assign::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitAssignExpr(this->shared_from_this());
}


Binary::Binary(const Token& operation, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right) :
    operation{operation}, left{left}, right{right} {
}


std::any Binary::accept(const std::shared_ptr<Visitor> visitor) {
    return visitor->visitBinaryExpr(this->shared_from_this());
}


Grouping::Grouping(const std::shared_ptr<Expression> expression) : expression{expression} {
}


std::any Grouping::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitGroupingExpr(this->shared_from_this());
}


Unary::Unary(const Token& operation, const std::shared_ptr<Expression> right) : operation{operation}, right{right} {
}


std::any Unary::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitUnaryExpr(this->shared_from_this());
}


Variable::Variable(const Token& name) : name{name} {
}


std::any Variable::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitVariableExpr(this->shared_from_this());
}


Logical::Logical(const Token& operation, const std::shared_ptr<Expression> left, const std::shared_ptr<Expression> right) : operation{operation}, left{left}, right{right} {
}


std::any Logical::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitLogicalExpr(this->shared_from_this());
}


Call::Call(const Token& paren, const std::shared_ptr<Expression> callee, std::vector<std::shared_ptr<Expression>> arguments) :
    paren{paren}, callee{callee}, arguments{arguments} {
}


std::any Call::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitCallExpr(this->shared_from_this());
}


Get::Get(const std::shared_ptr<Expression> object, const Token& name) : object{object}, name{name} {
}


std::any Get::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitGetExpr(this->shared_from_this());
}


Set::Set(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}

std::any Set::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitSetExpr(this->shared_from_this());
}


This::This(const Token& keyword) : keyword{keyword} {
}


std::any This::enable(std::shared_ptr<Visitor> visitor) {
    return visitor->visitThisExpr(this->shared_from_this());
}


Super::Super(const Token& keyword, const Token& method) : keyword{keyword}, method{method} {
}


std::any Super::accept(std::shared_ptr<Visitor> visitor) {
    return visitor->visitSuperExpr(this->shared_from_this());
}
