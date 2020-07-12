#pragma once
#include <any>
#include <memory>
#include <string>

#include <unordered_map>

#include "interpreter.h"
#include "token.h"

class Environment {
    std::shared_ptr<Environment> enclosing;
    std::unordered_map<std::string, std::any> values;
    Environment deep_copy() const;

 public:
    Environment();
    explicit Environment(const std::shared_ptr<Environment> enclosing);
    void define(const std::string& name, const std::any value);
    void assign(const Token& name, const std::any& value);
    std::any get(const Token& name) const;
    std::any getAt(int distance, std::string name) const;
    void assignAt(int distance, const Token& name, std::any value);
    Environment& ancestor(int distance);
    const Environment ancestor(int distance) const;
    const std::shared_ptr<Environment>& getEnclosing() const;
};
