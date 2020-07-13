#pragma once

#include <any>
#include <vector>

class Interpreter;

class LoxCallable {
 public:
    virtual std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) const = 0;
    virtual size_t arity() const = 0;
};
