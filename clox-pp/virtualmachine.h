#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "chunk.h"
#include "common.h"
#include "compiler.h"

enum class InterpretResult {
    Ok,
    Compile_Error,
    Runtime_Error
};

class VirtualMachine {
    Compiler compiler;
    Chunk chunk;
    std::vector<uint8_t>::iterator instruction_pointer;
    std::vector<LoxNumber> stack;

 public:
    explicit VirtualMachine(const std::string& source);

    InterpretResult run();
    InterpretResult interpret();
};