#include "chunk.h"

#include <iomanip>
#include <iostream>

void Chunk::write(OpCode byte, int line) {
    code.push_back(static_cast<uint8_t>(byte));
    lines.push_back(line);
}

void Chunk::write(unsigned char byte, int line) {
    code.push_back(byte);
    lines.push_back(line);
}

size_t Chunk::getCount() const {
    return code.size();
}

size_t Chunk::getCapacity() const {
    return code.capacity();
}

void Chunk::disassemble(const std::string& name) const {
    std::cout << "== " << name << " ==\n";
    std::cout << "codepoint\tline\topcode\t\tconstant\n";
    for (size_t offset = 0; offset < code.size();) {
        auto instruction{code[offset]};
        std::cout << std::setfill('0') << std::setw(4) << static_cast<int>(instruction) << "\t\t";
        if (lines[offset] == lines[offset-1]) {
            std::cout << "   |\t";
        } else {
            std::cout << std::setfill('0') << std::setw(4) << lines[offset] << '\t';
        }
        disasInstruction(instruction, offset);
    }
}

size_t Chunk::addConstant(LoxNumber number) {
    constants.push_back(number);
    return constants.size() - 1;
}

void Chunk::disasInstruction(uint8_t instruction, size_t& offset) const {
    switch (instruction) {
        case OpCode::Return:
            std::cout << "return\n";
            ++offset;
            break;
        case OpCode::Constant: {
            auto constant = constants[code[offset + 1]];
            std::cout << "constant\t" << constant << '\n';
            offset += 2;
            break;
        }
        case OpCode::Negate:
            std::cout << "negate\n";
            ++offset;
            break;
        default:
            std::cout << "Unknown instruction\t\t(" << instruction << ")\n";
            ++offset;
            break;
    }
}

void Chunk::disasInstruction(uint8_t instruction) const {
    size_t nop{};
    disasInstruction(instruction, nop);
}
