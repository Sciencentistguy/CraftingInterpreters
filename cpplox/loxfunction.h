#pragma once

#include <ostream>

#include "loxcallable.h"
#include "loxinstance.h"

class LoxCallable;

class LoxFunction : public LoxCallable {
    FunctionStatement declaration;
    std::shared_ptr<Environment> closure;
    bool isConstructor;

 public:
    LoxFunction(const FunctionStatement& declaration, const std::shared_ptr<Environment> closure, bool isInitializer);

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) override;
    int arity() override;

//    friend std::ostream& operator<<(std::ostream& os, const LoxFunction& function);
    std::shared_ptr<LoxFunction> bind(std::shared_ptr<LoxInstance> instance);

    std::string to_string();
};

class LoxBuiltinClock : public LoxCallable {
 public:
    int arity() override;

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& args) override;
};
