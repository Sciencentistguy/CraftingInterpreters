#pragma once
#include <any>
#include <string>

#include <unordered_map>

#include "interpreter.h"
#include "token.h"

class Environment {
    std::shared_ptr<Environment> enclosing;
    std::unordered_map<std::string, std::any> values;

 public:
    Environment();
    explicit Environment(const std::shared_ptr<Environment> enclosing);
    void define(const std::string& name, const std::any& value);
    void assign(const Token& name, const std::any& value);
    std::any get(const Token& name);

};
