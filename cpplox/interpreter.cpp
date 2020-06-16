#include "interpreter.h"

RuntimeError::RuntimeError(const std::string& message, const Token& token) : runtime_error{message}, message{message}, token{token} {
}

const char* RuntimeError::what() const noexcept {
    std::stringstream ss;
    ss << "[Runtime error Line " << token.getLine() << "] " << message << '\n';
    return ss.str().c_str();
}

std::any Interpreter::visitLiteralExpr(std::shared_ptr<Literal> expr) {
    return expr->getValue();
}

std::any Interpreter::visitAssignExpr(std::shared_ptr<Assign> expr) {
    return std::any();
}

std::any Interpreter::visitBinaryExpr(std::shared_ptr<Binary> expr) {
    std::any left{evaluate(expr->getLeft())};
    std::any right{evaluate(expr->getRight())};

    switch (expr->getOperator().getType()) {
        // Arithmetic
        case TokenType::Minus:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) - std::any_cast<double>(right);
        case TokenType::Slash:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) / std::any_cast<double>(right);
        case TokenType::Star:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) * std::any_cast<double>(right);
        case TokenType::Plus:
            if (left.type() == typeid(double) && right.type() == typeid(double)) {
                return std::any_cast<double>(left) + std::any_cast<double>(right);
            }
            if (left.type() == typeid(std::string) && right.type() == typeid(std::string)) {
                return std::any_cast<std::string>(left) + std::any_cast<std::string>(right);
            }
            throw RuntimeError("You plussed wrong (cannot add a number to a string, this isn't javascript)", expr->getOperator());

        // Comparison
        case TokenType::Greater:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) > std::any_cast<double>(right);
        case TokenType::Greater_equal:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) >= std::any_cast<double>(right);
        case TokenType::Less:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) < std::any_cast<double>(right);
        case TokenType::Less_equal:
            checkNumberOperand(expr->getOperator(), left, right);
            return std::any_cast<double>(left) <= std::any_cast<double>(right);

        // Equality
        case TokenType::Equal_equal:
            return isEqual(expr->getOperator(), left, right);
        case TokenType::Bang_equal:
            return !isEqual(expr->getOperator(), left, right);

        default:
            break;
    }
    // Unreachable^TM
    return std::any();
}

std::any Interpreter::visitGroupingExpr(std::shared_ptr<Grouping> expr) {
    return evaluate(expr->getExpression());
}

std::any Interpreter::visitUnaryExpr(std::shared_ptr<Unary> expr) {
    std::any right = evaluate(expr->getRight());
    switch (expr->getOperation().getType()) {
        case TokenType::Minus:
            return -std::any_cast<double>(right);

        case TokenType::Bang:
            return !(isTruthy(right));

        default:
            break;
    }
    // this should^TM be unreachable
    throw RuntimeError("You truthyed wrong (this really shouldn't happen)", expr->getOperation());
    return std::any();
}

std::any Interpreter::visitVariableExpr(std::shared_ptr<Variable> expr) {
    return std::any();
}

std::any Interpreter::visitLogicalExpr(std::shared_ptr<Logical> expr) {
    return std::any();
}

std::any Interpreter::visitCallExpr(std::shared_ptr<Call> expr) {
    return std::any();
}

std::any Interpreter::visitGetExpr(std::shared_ptr<Get> expr) {
    return std::any();
}

std::any Interpreter::visitSetExpr(std::shared_ptr<Set> expr) {
    return std::any();
}

std::any Interpreter::visitThisExpr(std::shared_ptr<This> expr) {
    return std::any();
}

std::any Interpreter::visitSuperExpr(std::shared_ptr<Super> expr) {
    return std::any();
}

std::any Interpreter::evaluate(std::shared_ptr<Expression> expr) {
    return expr->accept(this->shared_from_this());
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

void Interpreter::interpret(std::shared_ptr<Expression> expr) {
    try {
        const std::any& value{evaluate(expr)};
        std::cout << stringify(value) << '\n';
    } catch (const RuntimeError& rError) {
        runtimeError(rError);
    }
}
