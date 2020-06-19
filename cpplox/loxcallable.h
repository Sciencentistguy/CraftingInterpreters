#pragma once

#include <any>
#include <vector>

#include "interpreter.h"
class Interpreter;

class LoxCallable {
 public:
    virtual std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) = 0;
    virtual int arity() = 0;
};
