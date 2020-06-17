#pragma once
#include <memory>
#include <sstream>
#include <string>

#include "expression.h"

class ExpressionPrinter : public Visitor, public std::enable_shared_from_this<ExpressionPrinter> {
 public:
    ExpressionPrinter() = default;
    std::string print(std::shared_ptr<Expression> expression);
    std::any visitLiteralExpr(std::shared_ptr<LiteralExpression> expr) override;
    std::any visitAssignExpr(std::shared_ptr<AssignExpression> expr) override;
    std::any visitBinaryExpr(std::shared_ptr<BinaryExpression> expr) override;
    std::any visitGroupingExpr(std::shared_ptr<GroupingExpression> expr) override;
    std::any visitUnaryExpr(std::shared_ptr<UnaryExpression> expr) override;
    std::any visitVariableExpr(std::shared_ptr<VariableExpression> expr) override;
    std::any visitLogicalExpr(std::shared_ptr<LogicalExpression> expr) override;
    std::any visitCallExpr(std::shared_ptr<CallExpression> expr) override;
    std::any visitGetExpr(std::shared_ptr<GetExpression> expr) override;
    std::any visitSetExpr(std::shared_ptr<SetExpression> expr) override;
    std::any visitThisExpr(std::shared_ptr<ThisExpression> expr) override;
    std::any visitSuperExpr(std::shared_ptr<SuperExpression> expr) override;

    std::any bracket(const std::string& name);
    std::any bracket(const std::string& name, std::shared_ptr<Expression> expr);
    std::any bracket(const std::string& name, std::initializer_list<std::shared_ptr<Expression>> exprs);
};
