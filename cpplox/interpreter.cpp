#include "interpreter.h"

RuntimeError::RuntimeError(const std::string& message, const Token& token) : runtime_error{message}, message{message}, token{token} {
}

const char* RuntimeError::what() const noexcept {
    std::stringstream ss;
    ss << "[Runtime error Line " << token.getLine() << "] " << message << '\n';
    return ss.str().c_str();
}

std::any Interpreter::visitLiteralExpr(LiteralExpression& expr) {
    return expr.getValue();
}

std::any Interpreter::visitAssignExpr(AssignExpression& expr) {
    std::any value{evaluate(expr.getValue())};
    environment->assign(expr.getName(), value);
    return value;
}

std::any Interpreter::visitBinaryExpr(BinaryExpression& expr) {
    std::any left{evaluate(expr.getLeft())};
    std::any right{evaluate(expr.getRight())};

    switch (expr.getOperator().getType()) {
        // Arithmetic
        case TokenType::Minus:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) - std::any_cast<double>(right);
        case TokenType::Slash:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) / std::any_cast<double>(right);
        case TokenType::Star:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) * std::any_cast<double>(right);
        case TokenType::Plus:
            if (left.type() == typeid(double) && right.type() == typeid(double)) {
                return std::any_cast<double>(left) + std::any_cast<double>(right);
            }
            if (left.type() == typeid(std::string) && right.type() == typeid(std::string)) {
                return std::any_cast<std::string>(left) + std::any_cast<std::string>(right);
            }
            throw RuntimeError("You plussed wrong (cannot add a number to a string, this isn't javascript)", expr.getOperator());

        // Comparison
        case TokenType::Greater:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) > std::any_cast<double>(right);
        case TokenType::Greater_equal:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) >= std::any_cast<double>(right);
        case TokenType::Less:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) < std::any_cast<double>(right);
        case TokenType::Less_equal:
            checkNumberOperand(expr.getOperator(), left, right);
            return std::any_cast<double>(left) <= std::any_cast<double>(right);

        // Equality
        case TokenType::Equal_equal:
            return isEqual(expr.getOperator(), left, right);
        case TokenType::Bang_equal:
            return !isEqual(expr.getOperator(), left, right);

        default:
            break;
    }
    // Unreachable^TM
    return std::any();
}

std::any Interpreter::visitGroupingExpr(GroupingExpression& expr) {
    return evaluate(expr.getExpression());
}

std::any Interpreter::visitUnaryExpr(UnaryExpression& expr) {
    std::any right = evaluate(expr.getRight());
    switch (expr.getOperation().getType()) {
        case TokenType::Minus:
            return -std::any_cast<double>(right);

        case TokenType::Bang:
            return !(isTruthy(right));

        default:
            break;
    }
    // this should^TM be unreachable
    throw RuntimeError("You truthyed wrong (this really shouldn't happen)", expr.getOperation());
    return std::any();
}

std::any Interpreter::visitVariableExpr(VariableExpression& expr) {
    return environment->get(expr.getName());
}

std::any Interpreter::visitLogicalExpr(LogicalExpression& expr) {
    const auto& left{evaluate(expr.getLeft())};
    if (expr.getOperator().getType() == TokenType::Or) {
        if (isTruthy(left)) {
            return left;
        }
    } else {
        if (!isTruthy(left)) {
            return left;
        }
    }

    return evaluate(expr.getRight());
}

std::any Interpreter::visitCallExpr(CallExpression& expr) {
    return std::any();
}

std::any Interpreter::visitGetExpr(GetExpression& expr) {
    return std::any();
}

std::any Interpreter::visitSetExpr(SetExpression& expr) {
    return std::any();
}

std::any Interpreter::visitThisExpr(ThisExpression& expr) {
    return std::any();
}

std::any Interpreter::visitSuperExpr(SuperExpression& expr) {
    return std::any();
}

void Interpreter::execute(std::shared_ptr<Statement> statement) {
    statement->accept(this->shared_from_this());
}

bool Interpreter::isTruthy(const std::any& object) {
    if (!object.has_value()) {
        return false;
    }
    if (object.type() == typeid(bool)) {
        return std::any_cast<bool>(object);
    }
    return true;
}

bool Interpreter::isEqual(const Token& token, const std::any& left, const std::any& right) {
    if (!(left.has_value() || right.has_value())) {
        return true;
    }
    if (!left.has_value()) {
        return false;
    }

    if (left.type() == typeid(std::string) && right.type() == typeid(std::string)) {
        return std::any_cast<std::string>(left) == std::any_cast<std::string>(right);
    }
    if (left.type() == typeid(std::string) && right.type() == typeid(double)) {  // string == double is false
        return false;
    }
    if (left.type() == typeid(double) && right.type() == typeid(std::string)) {  // double == string is false
        return false;
    }
    if (left.type() == typeid(double) && right.type() == typeid(double)) {
        return std::any_cast<double>(left) == std::any_cast<double>(right);
    }
    throw RuntimeError("This is meant to not happen (isEqual type checking failed)", token);
    return false;
}

void Interpreter::checkNumberOperand(const Token& token, const std::any& operand) {
    if (operand.type() == typeid(double)) {
        return;
    }
    throw RuntimeError("Operand must be a number", token);
}

void Interpreter::checkNumberOperand(const Token& token, const std::any& operand1, const std::any& operand2) {
    if (operand1.type() == typeid(double) && operand2.type() == typeid(double)) {
        return;
    }
    throw RuntimeError("Operand must be a number", token);
}

std::any Interpreter::evaluate(std::shared_ptr<Expression> expr) {
    return expr->accept(this->shared_from_this());
}

void Interpreter::interpret(std::vector<std::shared_ptr<Statement>> statements) {
    try {
        for (const auto& statement : statements) {
            execute(statement);
        }
    } catch (const RuntimeError& rError) {
        runtimeError(rError);
    }
}

void Interpreter::visitExpressionStmt(const ExpressionStatement& stmt) {
    evaluate(stmt.getExpr());
}

void Interpreter::visitPrintStmt(const PrintStatement& stmt) {
    std::any value{evaluate(stmt.getExpr())};
    std::cout << stringify(value) << '\n';
}

void Interpreter::visitVarStmt(const VarStatement& stmt) {
    std::any value{};
    if (auto init = stmt.getInitialiser()) {
        value = evaluate(init);
    }
    environment->define(stmt.getName().getLexeme(), value);
}

void Interpreter::visitBlockStmt(const BlockStatement& stmt) {
    executeBlock(stmt.getStatements(), std::make_shared<Environment>(environment));
}

void Interpreter::visitIfStmt(const IfStatement& stmt) {
    if (isTruthy(evaluate(stmt.getCondition()))) {
        execute(stmt.getThenBranch());
    } else if (stmt.getElseBranch()) {
        execute(stmt.getElseBranch());
    }
}

void Interpreter::visitWhileStmt(const WhileStatement& stmt) {
    while (isTruthy(evaluate(stmt.getCondition()))) {
        execute(stmt.getBody());
    }
}

void Interpreter::visitFunctionStmt(const FunctionStatement& stmt) {
}

void Interpreter::visitReturnStmt(const ReturnStatement& stmt) {
}

void Interpreter::visitClassStmt(const ClassStatement& stmt) {
}

void Interpreter::executeBlock(std::vector<std::shared_ptr<Statement>> statements, std::shared_ptr<Environment> environment) {
    auto previousEnv = this->environment;
    try {
        this->environment = environment;
        for (const auto& statement : statements) {
            execute(statement);
        }
    } catch (std::runtime_error) {
    }
    this->environment = previousEnv;
    std::cout << "";
}
