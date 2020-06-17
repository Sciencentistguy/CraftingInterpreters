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

std::any ExpressionPrinter::visitLiteralExpr(LiteralExpression& expr) {
    auto& value = expr.getValue();
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

std::any ExpressionPrinter::visitAssignExpr(AssignExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitBinaryExpr(BinaryExpression& expr) {
    return bracket(expr.getOperator().getLexeme(), {expr.getLeft(), expr.getRight()});
}

std::any ExpressionPrinter::visitGroupingExpr(GroupingExpression& expr) {
    return bracket("group", expr.getExpression());
}

std::any ExpressionPrinter::visitUnaryExpr(UnaryExpression& expr) {
    return bracket(expr.getOperation().getLexeme(), expr.getRight());
}

std::any ExpressionPrinter::visitVariableExpr(VariableExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitLogicalExpr(LogicalExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitCallExpr(CallExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitGetExpr(GetExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitSetExpr(SetExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitThisExpr(ThisExpression& expr) {
    return std::any();
}

std::any ExpressionPrinter::visitSuperExpr(SuperExpression& expr) {
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
