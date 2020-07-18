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

    const Value& peek(int distance);

 public:
    explicit VirtualMachine(const std::string& source);

    void run();
    void interpret();
};