#pragma once

#include <string>

class Compiler {
    const std::string& source;
 public:
    explicit Compiler(const std::string& source);
    void compile();
};