#include "interpreter.h"

#include <cstring>

#include "loxclass.h"
#include "loxfunction.h"
#include "loxinstance.h"
#include "return.h"

RuntimeError::RuntimeError(const std::string& message, const Token& token) : runtime_error{message}, message{message}, token{token} {
}

const char* RuntimeError::what() const noexcept {
    std::stringstream ss;
    char* buf = new char[128];
    ss << "[Runtime error Line " << token.getLine() << "] " << message << '\n';
    std::strcpy(buf, ss.str().c_str());
    return buf;
}

std::any Interpreter::visitLiteralExpr(LiteralExpression& expr) {
    return expr.getValue();
}

std::any Interpreter::visitAssignExpr(AssignExpression& expr) {
    std::any value{evaluate(expr.getValue())};
    auto distanceIt = locals->find(expr.shared_from_this());
    if (distanceIt != locals->end()) {
        environment->assignAt(distanceIt->second, expr.getName(), value);
    } else {
        globals->assign(expr.getName(), value);
    }
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
    return lookUpVariable(expr.getName(), expr.shared_from_this());
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
    std::any callee{evaluate(expr.getCallee())};
    std::vector<std::any> arguments{};
    for (const auto& arg : expr.getArguments()) {
        arguments.push_back(evaluate(arg));
    }
    if (callee.type() == typeid(LoxBuiltinClock)) {
        auto function{std::any_cast<LoxBuiltinClock>(callee)};
        if (arguments.size() != function.arity()) {
            throw RuntimeError("Expected " + std::to_string(function.arity()) + " arguments but got " + std::to_string(arguments.size()) + ".",
                               expr.getParen());
        }
        return function(*this, arguments);
    }

    if (callee.type() == typeid(std::shared_ptr<LoxFunction>)) {
        auto& function{*std::any_cast<std::shared_ptr<LoxFunction>>(callee)};

        if (arguments.size() != function.arity()) {
            throw RuntimeError("Expected " + std::to_string(function.arity()) + " arguments but got " + std::to_string(arguments.size()) + ".",
                               expr.getParen());
        }

        return function(*this, arguments);
    }

    if (callee.type() == typeid(std::shared_ptr<LoxClass>)) {
        auto& function{*std::any_cast<std::shared_ptr<LoxClass>>(callee)};

        if (arguments.size() != function.arity()) {
            throw RuntimeError("Expected " + std::to_string(function.arity()) + " arguments but got " + std::to_string(arguments.size()) + ".",
                               expr.getParen());
        }

        return function(*this, arguments);
    }

    throw std::runtime_error(std::string("[visitCallExpr()] Unexpected std::any type '") + callee.type().name() + std::string("'."));
    return std::any();
}

std::any Interpreter::visitGetExpr(GetExpression& expr) {
    std::any obj{evaluate(expr.getObject())};
    if (obj.type() == typeid(std::shared_ptr<LoxInstance>)) {
        return std::any_cast<std::shared_ptr<LoxInstance>>(obj)->get(expr.getName());
    }
    throw RuntimeError("Cannot access property of non-instance type '" + std::string(obj.type().name()) + "'.", expr.getName());
    return std::any();
}

std::any Interpreter::visitSetExpr(SetExpression& expr) {
    std::any obj{evaluate(expr.getObject())};
    if (obj.type() != typeid(std::shared_ptr<LoxInstance>)) {
        throw RuntimeError("Cannot set property of non-instance type '" + std::string(obj.type().name()) + "'.", expr.getName());
    }
    std::any value{evaluate(expr.getValue())};
    std::any_cast<std::shared_ptr<LoxInstance>>(obj)->set(expr.getName(), value);
    return value;
}

std::any Interpreter::visitThisExpr(ThisExpression& expr) {
    return std::any();
}

std::any Interpreter::visitSuperExpr(SuperExpression& expr) {
    int distance = 0;
    if (auto super_iter = locals->find(expr.shared_from_this()); super_iter != locals->end()) {
        distance = super_iter->second;
    }
    auto env = *environment; //todo this is really hacky. getAt should really be const
    auto superclassany{environment->getAt(distance, "super")};
    if (!superclassany.has_value()) {
        throw RuntimeError("Superclass '" + expr.getKeyword().getLexeme() + "' doesn't exist", expr.getKeyword());
    }
    *(this->environment) = env;
    auto superclass{std::any_cast<std::shared_ptr<LoxClass>>(superclassany)};
    std::any objectany;
    try {
        objectany = environment->getAt(distance - 1, "this");
    } catch (const std::out_of_range& e) {
        std::cout << e.what();
    }
    if (!objectany.has_value()) {
        throw RuntimeError("'this' was never defined.", expr.getKeyword());
    }
    auto object{std::any_cast<std::shared_ptr<LoxInstance>>(objectany)};
    auto method{superclass->findMethod(expr.getMethod().getLexeme())};
    if (!method) {
        throw RuntimeError("Undefined property '" + expr.getMethod().getLexeme() + "'.", expr.getMethod());
    }
    return method->bind(object);
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
    } catch (const RuntimeError& e) {
        runtimeError(e);
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
    if (auto init = stmt.getInitializer()) {
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
    auto function = std::make_shared<LoxFunction>(stmt, environment, false);
    environment->define(stmt.getName().getLexeme(), function);
}

void Interpreter::visitReturnStmt(const ReturnStatement& stmt) {
    std::any value{};
    if (stmt.getValue()) {
        value = evaluate(stmt.getValue());
    }
    throw Return(value);
}

void Interpreter::visitClassStmt(const ClassStatement& stmt) {
    std::any superclass{};
    if (stmt.getSuperclass()) {
        superclass = evaluate(stmt.getSuperclass());
        if (!(superclass.type() == typeid(std::shared_ptr<LoxClass>))) {
            throw RuntimeError("Superclass must be a class", stmt.getSuperclass()->getName());
        }
    }
    environment->define(stmt.getName().getLexeme(), std::any());

    if (superclass.has_value()) {
        environment = std::make_shared<Environment>(environment);
        environment->define("super", superclass);
    }

    std::unordered_map<std::string, std::shared_ptr<LoxFunction>> methods{};
    for (const auto& method : stmt.getMethods()) {
        auto function{std::make_shared<LoxFunction>(*method, environment, method->getName().getLexeme() == "init")};
        methods.insert(std::make_pair(method->getName().getLexeme(), function));
    }
    auto cls{std::make_shared<LoxClass>(stmt.getName().getLexeme(), superclass.has_value() ? std::any_cast<std::shared_ptr<LoxClass>>(superclass) : nullptr,
                                        methods)};
    //    LoxClass cls{stmt.getName().getLexeme(), methods};
    if (superclass.has_value()) {
        environment = environment->getEnclosing();
    }
    environment->assign(stmt.getName(), cls);
}

void Interpreter::executeBlock(const std::vector<std::shared_ptr<Statement>>& statements, std::shared_ptr<Environment> environment) {
    auto previousEnv = this->environment;
    try {
        this->environment = environment;
        for (const auto& statement : statements) {
            execute(statement);
        }
    } catch (const RuntimeError& e) {
        runtimeError(e);
    } catch (const std::runtime_error& e) {
    } catch (const Return& e) {
        this->environment = previousEnv;
        throw;
    }
    this->environment = previousEnv;
}

Interpreter::Interpreter() {
    LoxBuiltinClock clock{};
    globals->define("clock", clock);
}

void Interpreter::resolve(std::shared_ptr<Expression> expr, int depth) {
    locals->insert(std::make_pair(expr, depth));
}

std::any Interpreter::lookUpVariable(const Token& name, std::shared_ptr<Expression> expr) {
    auto distanceIt = locals->find(expr);
    if (distanceIt != locals->end()) {  // exists
        return environment->getAt(distanceIt->second, name.getLexeme());
    } else {  // does not exist
        return globals->get(name);
    }
};
