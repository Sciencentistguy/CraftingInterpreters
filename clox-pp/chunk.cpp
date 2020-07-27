#include "chunk.h"

#include <fmt/format.h>

void Chunk::write(OpCode byte, int line) {
    code.push_back(static_cast<uint8_t>(byte));
    lines.push_back(line);
}

void Chunk::write(unsigned char byte, int line) {
    code.push_back(byte);
    lines.push_back(line);
}

std::size_t Chunk::getCount() const {
    return code.size();
}

std::size_t Chunk::getCapacity() const {
    return code.capacity();
}

void Chunk::disassemble(const std::string_view& name) const {
    fmt::print("== {} ==\n", name);
    fmt::print("offset\tcodepoint\tline\topcode\t\t\tvalue\n");
    for (std::size_t offset = 0; offset < code.size();) {
        fmt::print("{:04d}\t", offset);
        OpCode instruction{code[offset]};
        fmt::print("{:04d}\t\t", instruction);
        if (lines[offset] == lines[offset - 1]) {
            fmt::print("   |\t");
        } else {
            fmt::print("{:04d}\t", lines[offset]);
        }
        disasInstruction(instruction, offset);
    }
    fmt::print("== end {} ==\n", name);
}

uint8_t Chunk::addConstant(const Value& value) {
    constants.push_back(value);
    return constants.size() - 1;
}



void Chunk::disasInstruction(OpCode instruction, std::size_t& offset) const {
    switch (static_cast<OpCode>(instruction)) {
        case OpCode::Add:
            fmt::print("add\n");
            ++offset;
            break;
        case OpCode::Subtract:
            fmt::print("subtract\n");
            ++offset;
            break;
        case OpCode::Multiply:
            fmt::print("multiply\n");
            ++offset;
            break;
        case OpCode::Divide:
            fmt::print("divide\n");
            ++offset;
            break;
        case OpCode::Return:
            fmt::print("return\n");
            ++offset;
            break;
        case OpCode::Constant: {
            auto constant = constants[code[++offset]];
            fmt::print("constant\t\t{}\n", value_to_string(constant));
            ++offset;
            break;
        }
        case OpCode::Negate:
            fmt::print("negate\n");
            ++offset;
            break;
        case OpCode::Nil:
            fmt::print("nil\n");
            ++offset;
            break;
        case OpCode::True:
            fmt::print("true\n");
            ++offset;
            break;
        case OpCode::False:
            fmt::print("false\n");
            ++offset;
            break;
        case OpCode::Not:
            fmt::print("not\n");
            ++offset;
            break;
        case OpCode::Equal:
            fmt::print("equal\n");
            ++offset;
            break;
        case OpCode::Greater:
            fmt::print("greater\n");
            ++offset;
            break;
        case OpCode::Less:
            fmt::print("less\n");
            ++offset;
            break;
        case OpCode::Print:
            fmt::print("print\n");
            ++offset;
            break;
        case OpCode::Pop:
            fmt::print("pop\n");
            ++offset;
            break;
        case OpCode::Define_global: {
            auto constant = constants[code[++offset]];
            fmt::print("define_global\t{}\n", value_to_string(constant));
            ++offset;
            break;
        }
        case OpCode::Get_global: {
            auto constant = constants[code[++offset]];
            fmt::print("get_global\t\t{}\n", value_to_string(constant));
            ++offset;
            break;
        }
        case OpCode::Set_global: {
            auto constant = constants[code[++offset]];
            fmt::print("set_global\t\t{}\n", value_to_string(constant));
            ++offset;
            break;
        }
        case OpCode::Get_local: {
            uint8_t slot{code[++offset]};
            fmt::print("get_local\t\t{:04d}\n", slot);
            ++offset;
            break;
        }
        case OpCode::Set_local: {
            uint8_t slot{code[++offset]};
            fmt::print("set_local\t\t{:04d}\n", slot);
            ++offset;
            break;
        }
        case OpCode::Jump_if_false: {
            auto jump{static_cast<uint16_t>(code[offset + 1] << 8u)};
            auto sign{1};
            jump |= code[offset + 2];
            fmt::print("jump_if_false\t{:04d} -> {:04d}\n", offset, offset + 3 + sign * jump);
            offset += 3;
            break;
        }
        case OpCode::Jump: {
            auto jump{static_cast<uint16_t>(code[offset + 1] << 8u)};
            auto sign{1};
            jump |= code[offset + 2];
            fmt::print("jump\t\t\t{:04d} -> {:04d}\n", offset, offset + 3 + sign * jump);
            offset += 3;
            break;
        }
        case OpCode::Loop: {
            auto jump{static_cast<uint16_t>(code[offset + 1] << 8u)};
            auto sign{-1};
            jump |= code[offset + 2];
            fmt::print("loop\t\t\t{:04d} -> {:04d}\n", offset, offset + 3 + sign * jump);
            offset += 3;
            break;
        }
    }
}

void Chunk::disasInstruction(OpCode instruction, long&& offset) const {
    std::size_t ofst = offset;
    disasInstruction(instruction, ofst);
}

