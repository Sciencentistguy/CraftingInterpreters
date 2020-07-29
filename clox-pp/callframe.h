#pragma once

#include <span>
#include <vector>

#include "common.h"
#include "function.h"
#include "value.h"

class CallFrame {
 public:
    const Function* function;
    std::vector<uint8_t>::const_iterator instruction_pointer;
    std::array<Value, STACK_MAX>::iterator slots;

    [[nodiscard]] const Chunk& getChunk() const;
};