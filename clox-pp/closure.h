#pragma once
#include <memory>
#include <vector>

#include "upvalue.h"

class Function;

class Closure {
    std::shared_ptr<Function> function;

 public:
    std::vector<RuntimeUpvalue> upvalues;

    explicit Closure(const std::shared_ptr<Function>& function);
    explicit Closure(const Function& function);

    [[nodiscard]] const Function& getFunction() const;
};
