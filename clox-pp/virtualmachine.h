#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "chunk.h"
#include "common.h"
#include "compiler.h"

enum class InterpretResult { Ok, Compile_Error, Runtime_Error };

class VirtualMachine {
    Compiler compiler;
    Chunk chunk;
    std::vector<uint8_t>::iterator instruction_pointer;
    std::vector<Value> stack;

    const Value& peek(int distance);
    template<typename Lambda>
    void binary_op(Lambda l);

 public:
    explicit VirtualMachine(const std::string& source);

    void run();
    void interpret();
};