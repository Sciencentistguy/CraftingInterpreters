#pragma once

#include <ostream>

#include "loxcallable.h"

class LoxCallable;

class LoxFunction : public LoxCallable {
    FunctionStatement declaration;

 public:
    explicit LoxFunction(FunctionStatement declaration);

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) override;
    int arity() override;
    friend std::ostream& operator<<(std::ostream& os, const LoxFunction& function);
    std::string to_string();
};

class LoxBuiltinClock : public LoxCallable {
 public:
    int arity() override;

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& args) override;
};
