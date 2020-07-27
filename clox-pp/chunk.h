#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <variant>
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

    void disassemble(const std::string_view& name) const;
    void disasInstruction(OpCode instruction, std::size_t& offset) const;
    void disasInstruction(OpCode instruction, long&& offset) const;

    uint8_t addConstant(const Value& value);

    [[nodiscard]] std::size_t getCount() const;
    [[nodiscard]] std::size_t getCapacity() const;
};