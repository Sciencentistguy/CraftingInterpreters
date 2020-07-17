#include "virtualmachine.h"

#include <iostream>

#include "opcode.h"

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
                std::cout << stack.back() << '\n';
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

InterpretResult VirtualMachine::interpret() {
    try {
        compiler.compile();
        chunk = compiler.getChunk();
    } catch (const CompilerException&) {
        return InterpretResult::Compile_Error;
    }
    instruction_pointer = chunk.code.begin();
    return run();
}

VirtualMachine::VirtualMachine(const std::string& source) : compiler{source} {
}
