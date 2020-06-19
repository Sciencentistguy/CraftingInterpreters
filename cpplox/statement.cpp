#include "statement.h"

ExpressionStatement::ExpressionStatement(const std::shared_ptr<Expression> expr) : expr{expr} {
}

void ExpressionStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitExpressionStmt(this->shared_from_this());
}

const std::shared_ptr<Expression>& ExpressionStatement::getExpr() const {
    return expr;
}

PrintStatement::PrintStatement(const std::shared_ptr<Expression>& expr) : expr(expr) {
}

void PrintStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitPrintStmt(*this);
}

const std::shared_ptr<Expression>& PrintStatement::getExpr() const {
    return expr;
}

VarStatement::VarStatement(const std::shared_ptr<Expression> initialiser, const Token& name) : initialiser{initialiser}, name{name} {
}

void VarStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitVarStmt(*this);
}

const std::shared_ptr<Expression>& VarStatement::getInitialiser() const {
    return initialiser;
}

const Token& VarStatement::getName() const {
    return name;
}

BlockStatement::BlockStatement(const std::vector<std::shared_ptr<Statement>>& statements) : statements{statements} {
}

void BlockStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitBlockStmt(*this);
}
const std::vector<std::shared_ptr<Statement>>& BlockStatement::getStatements() const {
    return statements;
}

IfStatement::IfStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> thenBranch,
                         const std::shared_ptr<Statement> elseBranch) :
    condition{condition},
    thenBranch{thenBranch}, elseBranch{elseBranch} {
}

void IfStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitIfStmt(*this);
}

const std::shared_ptr<Expression>& IfStatement::getCondition() const {
    return condition;
}

const std::shared_ptr<Statement>& IfStatement::getThenBranch() const {
    return thenBranch;
}

const std::shared_ptr<Statement>& IfStatement::getElseBranch() const {
    return elseBranch;
}

WhileStatement::WhileStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> body) : condition{condition}, body{body} {
}

void WhileStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitWhileStmt(*this);
}

const std::shared_ptr<Expression>& WhileStatement::getCondition() const {
    return condition;
}

const std::shared_ptr<Statement>& WhileStatement::getBody() const {
    return body;
}

FunctionStatement::FunctionStatement(const Token& name, const std::vector<Token>& params, const std::vector<std::shared_ptr<Statement>>& body) :
    name{name}, params{params}, body{body} {
}

void FunctionStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitFunctionStmt(*this);
}

const Token& FunctionStatement::getName() const {
    return name;
}

const std::vector<Token>& FunctionStatement::getParams() const {
    return params;
}

const std::vector<std::shared_ptr<Statement>>& FunctionStatement::getBody() const {
    return body;
}

ReturnStatement::ReturnStatement(const Token& name, const std::shared_ptr<Expression> value) : name{name}, value{value} {
}

void ReturnStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitReturnStmt(*this);
}

const Token& ReturnStatement::getName() const {
    return name;
}

const std::shared_ptr<Expression>& ReturnStatement::getValue() const {
    return value;
}

ClassStatement::ClassStatement(const Token& name, const std::shared_ptr<VariableExpression> superclass,
                               const std::vector<std::shared_ptr<FunctionStatement>>& methods) :
    name{name},
    superclass{superclass}, methods{methods} {
}

void ClassStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitClassStmt(*this);
}

const std::shared_ptr<VariableExpression>& ClassStatement::getSuperclass() const {
    return superclass;
}

const std::vector<std::shared_ptr<FunctionStatement>>& ClassStatement::getMethods() const {
    return methods;
}

const Token& ClassStatement::getName() const {
    return name;
}
