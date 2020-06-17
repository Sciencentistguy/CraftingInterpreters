#pragma once
#include <any>
#include <string>

#include <unordered_map>

#include "interpreter.h"
#include "token.h"

class Environment {
    std::unordered_map<std::string, std::any> values;

 public:
    void define(const std::string& name, const std::any& value);
    std::any get(const Token& name);
};
