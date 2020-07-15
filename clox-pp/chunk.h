#pragma once
#include <cstdint>
#include <string>
#include <vector>

#include "common.h"
#include "opcode.h"

class Chunk {
 public:
    std::vector<uint8_t> code;
    std::vector<int> lines;
    std::vector<LoxNumber> constants;

    void write(OpCode byte, int line);
    void write(unsigned char byte, int line);

    void disassemble(const std::string& name) const;
    void disasInstruction(uint8_t instruction, size_t& offset) const;
    void disasInstruction(uint8_t instruction) const;

    size_t addConstant(LoxNumber number);

    size_t getCount() const;
    size_t getCapacity() const;
};