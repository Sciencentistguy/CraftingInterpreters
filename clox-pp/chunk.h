#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "opcode.h"
#include "value.h"

class Chunk {
 public:
    std::vector<uint8_t> code;
    std::vector<int> lines;
    std::vector<Value> constants;

    void write(OpCode byte, int line);
    void write(unsigned char byte, int line);

    void disassemble(const std::string& name) const;
    void disasInstruction(OpCode instruction, size_t& offset) const;
    void disasInstruction(OpCode instruction) const;

    uint8_t addConstant(const Value& value);

    [[nodiscard]] size_t getCount() const;
    [[nodiscard]] size_t getCapacity() const;
};