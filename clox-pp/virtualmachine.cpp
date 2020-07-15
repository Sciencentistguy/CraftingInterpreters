#include "virtualmachine.h"

#include <iostream>
VirtualMachine::VirtualMachine(const Chunk& chunk) : chunk{chunk} {
    instruction_pointer = this->chunk.code.begin();
}

InterpretResult VirtualMachine::run() {
    while (true) {
        uint8_t instruction{*instruction_pointer++};
        if constexpr (DEBUG) {
            std::cout << "Instruction: ";
            chunk.disasInstruction(*instruction_pointer);

            std::cout << "Stack: ";
            for (const auto i : stack) {
                std::cout << "[ " << i << "] ";
            }
            std::cout << '\n';
        }

        switch (instruction) {
            case OpCode::Return:
                std::cout << stack.back();
                stack.pop_back();
                return InterpretResult::Ok;
            case OpCode::Constant: {
                LoxNumber constant = chunk.constants[*instruction_pointer++];
                stack.push_back(constant);
                break;
            }
            case OpCode::Negate: {
                stack.back() = -stack.back();
                break;
            }
            case OpCode::Add: {
                double b = stack.back();
                stack.pop_back();
                double a = stack.back();
                stack.pop_back();
                stack.push_back(a + b);
                break;
            }
            case OpCode::Subtract: {
                double b = stack.back();
                stack.pop_back();
                double a = stack.back();
                stack.pop_back();
                stack.push_back(a - b);
                break;
            }
            case OpCode::Multiply: {
                double b = stack.back();
                stack.pop_back();
                double a = stack.back();
                stack.pop_back();
                stack.push_back(a * b);
                break;
            }
            case OpCode::Divide: {
                double b = stack.back();
                stack.pop_back();
                double a = stack.back();
                stack.pop_back();
                stack.push_back(a / b);
                break;
            }
        }
    }
}
InterpretResult VirtualMachine::interpret(const std::string& source) {
    compiler.compile(source);
    return InterpretResult::Runtime_Error;
}

VirtualMachine::VirtualMachine() {
}

void VirtualMachine::setChunk(const Chunk& chunk) {
    VirtualMachine::chunk = chunk;
}
