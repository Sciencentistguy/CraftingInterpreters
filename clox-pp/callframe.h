#pragma once

#include <vector>
#include <span>

#include "function.h"
#include "value.h"

class CallFrame {
 public:
    Function* function;
    std::vector<uint8_t>::const_iterator instruction_pointer;
    Value* slots;

    [[nodiscard]] const Chunk& getChunk() const;
};