#pragma once
#include <any>
#include <memory>
#include <string>

#include <unordered_map>

#include "token.h"

class Environment {
    std::shared_ptr<Environment> enclosing;
    std::unordered_map<std::string, std::any> values;

 public:
    Environment();
    explicit Environment(const std::shared_ptr<Environment> enclosing);

    void define(const std::string& name, const std::any value);
    void assign(const Token& name, const std::any& value);
    Environment& ancestor(int distance);
    void assignAt(int distance, const Token& name, std::any value);

    std::any get(const Token& name) const;
    const Environment& ancestor(int distance) const;
    std::any getAt(int distance, std::string name) const;

    const std::shared_ptr<Environment>& getEnclosing() const;
};
