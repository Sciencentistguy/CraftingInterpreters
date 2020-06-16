#include "statement.h"

ExpressionStatement::ExpressionStatement(const std::shared_ptr<Expression> expr) : expr{expr} {
}

void ExpressionStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitExpressionStmt(*this);
}

PrintStatement::PrintStatement(const std::shared_ptr<Expression>& expr) : expr(expr) {
}

void PrintStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitPrintStmt(*this);
}

VarStatement::VarStatement(const std::shared_ptr<Expression> initialiser, const Token& name) : initialiser{initialiser}, name{name} {
}

void VarStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitVarStmt(*this);
}

BlockStatement::BlockStatement(const std::vector<std::shared_ptr<Statement>>& statements) : statements{statements} {
}

void BlockStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitBlockStmt(*this);
}

IfStatement::IfStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> thenBranch,
                         const std::shared_ptr<Statement> elseBranch) :
    condition{condition},
    thenBranch{thenBranch}, elseBranch{elseBranch} {
}

void IfStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitIfStmt(*this);
}

WhileStatement::WhileStatement(const std::shared_ptr<Expression> condition, const std::shared_ptr<Statement> body) : condition{condition}, body{body} {
}

void WhileStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitWhileStmt(*this);
}

FunctionStatement::FunctionStatement(const Token& name, const std::vector<Token>& params, const std::vector<std::shared_ptr<Statement>>& body) :
    name{name}, params{params}, body{body} {
}

void FunctionStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitFunctionStmt(*this);
}

ReturnStatement::ReturnStatement(const Token& name, const std::shared_ptr<Expression> value) : name{name}, value{value} {
}

void ReturnStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitReturnStmt(*this);
}

ClassStatement::ClassStatement(const Token& name, const std::shared_ptr<Variable> superclass,
                               const std::vector<std::shared_ptr<FunctionStatement>>& methods) :
    name{name},
    superclass{superclass}, methods{methods} {
}

void ClassStatement::accept(std::shared_ptr<StatementVisitor> visitor) {
    visitor->visitClassStmt(*this);
}
