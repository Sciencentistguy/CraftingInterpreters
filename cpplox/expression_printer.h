#pragma once
#include <memory>
#include <string>
#include <sstream>

#include "expression.h"

class ExpressionPrinter : public Visitor, public std::enable_shared_from_this<ExpressionPrinter> {
 public:
    ExpressionPrinter() = default;
    std::string print(std::shared_ptr<Expression> expression);
    std::any visitLiteralExpr(std::shared_ptr<Literal> expr) override;
    std::any visitAssignExpr(std::shared_ptr<Assign> expr) override;
    std::any visitBinaryExpr(std::shared_ptr<Binary> expr) override;
    std::any visitGroupingExpr(std::shared_ptr<Grouping> expr) override;
    std::any visitUnaryExpr(std::shared_ptr<Unary> expr) override;
    std::any visitVariableExpr(std::shared_ptr<Variable> expr) override;
    std::any visitLogicalExpr(std::shared_ptr<Logical> expr) override;
    std::any visitCallExpr(std::shared_ptr<Call> expr) override;
    std::any visitGetExpr(std::shared_ptr<Get> expr) override;
    std::any visitSetExpr(std::shared_ptr<Set> expr) override;
    std::any visitThisExpr(std::shared_ptr<This> expr) override;
    std::any visitSuperExpr(std::shared_ptr<Super> expr) override;

    std::any bracket(const std::string& name);
    std::any bracket(const std::string& name, std::shared_ptr<Expression> expr);
    std::any bracket(const std::string& name, std::initializer_list<std::shared_ptr<Expression>> exprs);
};
