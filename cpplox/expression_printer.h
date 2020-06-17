#pragma once
#include <memory>
#include <sstream>
#include <string>

#include "expression.h"

class ExpressionPrinter : public ExpressionVisitor, public std::enable_shared_from_this<ExpressionPrinter> {
 public:
    ExpressionPrinter() = default;
    std::string print(std::shared_ptr<Expression> expression);
    std::any visitLiteralExpr(LiteralExpression& expr) override;
    std::any visitAssignExpr(AssignExpression& expr) override;
    std::any visitBinaryExpr(BinaryExpression& expr) override;
    std::any visitGroupingExpr(GroupingExpression& expr) override;
    std::any visitUnaryExpr(UnaryExpression& expr) override;
    std::any visitVariableExpr(VariableExpression& expr) override;
    std::any visitLogicalExpr(LogicalExpression& expr) override;
    std::any visitCallExpr(CallExpression& expr) override;
    std::any visitGetExpr(GetExpression& expr) override;
    std::any visitSetExpr(SetExpression& expr) override;
    std::any visitThisExpr(ThisExpression& expr) override;
    std::any visitSuperExpr(SuperExpression& expr) override;

    std::any bracket(const std::string& name);
    std::any bracket(const std::string& name, std::shared_ptr<Expression> expr);
    std::any bracket(const std::string& name, std::initializer_list<std::shared_ptr<Expression>> exprs);
};
