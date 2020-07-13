#pragma once

#include <ostream>

#include "loxcallable.h"
#include "loxinstance.h"
#include "statement.h"
#include "environment.h"

class LoxCallable;

class LoxFunction : public LoxCallable {
    FunctionStatement declaration;
    std::shared_ptr<Environment> closure;
    bool isConstructor;

 public:
    LoxFunction(const FunctionStatement& declaration, const std::shared_ptr<Environment> closure, bool isInitializer);

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) const override;
    size_t arity() const override;

    //    friend std::ostream& operator<<(std::ostream& os, const LoxFunction& function);
    std::shared_ptr<LoxFunction> bind(std::shared_ptr<LoxInstance> instance);

    std::string to_string() const;
};

class LoxBuiltinClock : public LoxCallable {
 public:
    size_t arity() const override;

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& args) const override;
};
