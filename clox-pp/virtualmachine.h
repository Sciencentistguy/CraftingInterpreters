#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "chunk.h"
#include "compiler.h"
#include "value.h"

class VirtualMachine {
    Compiler compiler;
    Chunk chunk;
    std::vector<uint8_t>::iterator instruction_pointer;
    std::vector<Value> stack;
    std::unordered_map<std::string, Value> globals;

    [[nodiscard]] const Value& peek(int distance) const;

 public:
    explicit VirtualMachine(const std::string& source);

    void run();
    void interpret();
    void setSource(const std::string& source);
};