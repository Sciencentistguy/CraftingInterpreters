#pragma once

#include "loxclass.h"
#include "token.h"

class LoxInstance : public std::enable_shared_from_this<LoxInstance> {
    LoxClass cls;
    std::unordered_map<std::string, std::any> fields{};

 public:
    LoxInstance(const LoxClass& cls);

    std::any get(const Token& name);
    void set(const Token& name, const std::any& value);

    std::string to_string() const;
};
