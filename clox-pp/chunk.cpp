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
    std::cout << "codepoint\tline\topcode\t\t\tconstant\n";
    for (size_t offset = 0; offset < code.size(); ++offset) {
        OpCode instruction{code[offset]};
        std::cout << std::setfill('0') << std::setw(4) << static_cast<int>(instruction) << "\t\t";
        if (lines[offset] == lines[offset - 1]) {
            std::cout << "   |\t";
        } else {
            std::cout << std::setfill('0') << std::setw(4) << lines[offset] << '\t';
        }
        disasInstruction(instruction, offset);
    }
    std::cout << "== end " << name << " ==\n";
}

uint8_t Chunk::addConstant(const Value& value) {
    constants.push_back(value);
    return constants.size() - 1;
}

void Chunk::disasInstruction(OpCode instruction, size_t& offset) const {
    switch (static_cast<OpCode>(instruction)) {
        case OpCode::Add:
            std::cout << "add\n";
            break;
        case OpCode::Subtract:
            std::cout << "subtract\n";
            break;
        case OpCode::Multiply:
            std::cout << "multiply\n";
            break;
        case OpCode::Divide:
            std::cout << "divide\n";
            break;
        case OpCode::Return:
            std::cout << "return\n";
            break;
        case OpCode::Constant: {
            auto constant = constants[code[offset + 1]];
            std::cout << "constant\t\t" << value_to_string(constant) << '\n';
            ++offset;
            break;
        }
        case OpCode::Negate:
            std::cout << "negate\n";
            break;
        case OpCode::Nil:
            std::cout << "nil\n";
            break;
        case OpCode::True:
            std::cout << "true\n";
            break;
        case OpCode::False:
            std::cout << "false\n";
            break;
        case OpCode::Not:
            std::cout << "not\n";
            break;
        case OpCode::Equal:
            std::cout << "equal\n";
            break;
        case OpCode::Greater:
            std::cout << "greater\n";
            break;
        case OpCode::Less:
            std::cout << "less\n";
            break;
        case OpCode::Print:
            std::cout << "print\n";
            break;
        case OpCode::Pop:
            std::cout << "pop\n";
            break;
        case OpCode::Define_global: {
            auto constant = constants[code[offset + 1]];
            std::cout << "define_global\t" << value_to_string(constant) << '\n';
            ++offset;
            break;
        }
        case OpCode::Get_global: {
            auto constant = constants[code[offset + 1]];
            std::cout << "get_global\t\t" << value_to_string(constant) << '\n';
            ++offset;
            break;
        }
        case OpCode::Set_global: {
            auto constant = constants[code[offset + 1]];
            std::cout << "set_global\t\t" << value_to_string(constant) << '\n';
            ++offset;
            break;
        }
    }
}

void Chunk::disasInstruction(OpCode instruction) const {
    size_t nop{};
    disasInstruction(instruction, nop);
}
