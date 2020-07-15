#pragma once

#include <stack>
#include <vector>

#include "chunk.h"
#include "compiler.h"

enum class InterpretResult { Ok, Compile_Error, Runtime_Error };

class VirtualMachine {
    Compiler compiler;
    Chunk chunk;
    std::vector<uint8_t>::iterator instruction_pointer;
    std::vector<LoxNumber> stack;

 public:
    VirtualMachine(const Chunk& chunk);
    void setChunk(const Chunk& chunk);
    VirtualMachine();
    InterpretResult run();
    InterpretResult interpret(const std::string &source);
};