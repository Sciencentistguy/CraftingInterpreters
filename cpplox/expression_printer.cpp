#include "expression_printer.h"

std::string ExpressionPrinter::print(std::shared_ptr<Expression> expression) {
    auto i{expression->accept(this->shared_from_this())};
    if (i.type() == typeid(int)) {
        return std::to_string(std::any_cast<int>(i));
    }
    if (i.type() == typeid(double)) {
        return std::to_string(std::any_cast<double>(i));
    }
    if (i.type() == typeid(std::string)) {
        return std::any_cast<std::string>(i);
    }
}

std::any ExpressionPrinter::visitLiteralExpr(std::shared_ptr<LiteralExpression> expr) {
    auto& value = expr->getValue();
    if (!value.has_value()) {
        return "nil";
    }
    if (value.type() == typeid(int)) {
        return std::any_cast<int>(value);
    }
    if (value.type() == typeid(double)) {
        return std::any_cast<double>(value);
    }
    if (value.type() == typeid(std::string)) {
        return std::any_cast<std::string>(value);
    }
}

std::any ExpressionPrinter::visitAssignExpr(std::shared_ptr<AssignExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitBinaryExpr(std::shared_ptr<BinaryExpression> expr) {
    return bracket(expr->getOperator().getLexeme(), {expr->getLeft(), expr->getRight()});
}

std::any ExpressionPrinter::visitGroupingExpr(std::shared_ptr<GroupingExpression> expr) {
    return bracket("group", expr->getExpression());
}

std::any ExpressionPrinter::visitUnaryExpr(std::shared_ptr<UnaryExpression> expr) {
    return bracket(expr->getOperation().getLexeme(), expr->getRight());
}

std::any ExpressionPrinter::visitVariableExpr(std::shared_ptr<VariableExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitLogicalExpr(std::shared_ptr<LogicalExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitCallExpr(std::shared_ptr<CallExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitGetExpr(std::shared_ptr<GetExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitSetExpr(std::shared_ptr<SetExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitThisExpr(std::shared_ptr<ThisExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::visitSuperExpr(std::shared_ptr<SuperExpression> expr) {
    return std::any();
}

std::any ExpressionPrinter::bracket(const std::string& name, std::initializer_list<std::shared_ptr<Expression>> exprs) {
    std::stringstream ss;
    ss << '(' << name;
    for (const auto& expr : exprs) {
        ss << ' ';
        auto a{expr->accept(this->shared_from_this())};
        std::string s;
        if (a.type() == typeid(int)) {
            s = std::to_string(std::any_cast<int>(a));
        }
        if (a.type() == typeid(double)) {
            s = std::to_string(std::any_cast<double>(a));
        }
        if (a.type() == typeid(std::string)) {
            s = std::any_cast<std::string>(a);
        }
        ss << s;
    }

    ss << ')';
    return ss.str();
}

std::any ExpressionPrinter::bracket(const std::string& name, std::shared_ptr<Expression> expr) {
    return bracket(name, {expr});
}

std::any ExpressionPrinter::bracket(const std::string& name) {
    return bracket(name, {});
}
