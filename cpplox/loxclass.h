#pragma once

#include <string>

#include "loxcallable.h"

class LoxClass : public LoxCallable {
    const std::string name;
    std::shared_ptr<LoxClass> superclass;
    std::unordered_map<std::string, std::shared_ptr<LoxFunction>> methods;

 public:
    LoxClass(const std::string& name, std::shared_ptr<LoxClass> superclass, const std::unordered_map<std::string, std::shared_ptr<LoxFunction>> methods);
    std::string to_string() const;

    std::any operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) const override;
    size_t arity() const override;

    const std::string& getName() const;
    std::shared_ptr<LoxFunction> findMethod(std::string name) const;
};
