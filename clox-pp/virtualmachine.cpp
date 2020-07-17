#include "virtualmachine.h"

#include <iostream>

#include "exception.h"
#include "opcode.h"
#include "value.h"

void VirtualMachine::run() {
    while (true) {
        OpCode instruction{*instruction_pointer++};
        if constexpr (DEBUG) {
            std::cout << "Instruction: ";
            chunk.disasInstruction(instruction);

            std::cout << "Stack: ";
            if (!stack.empty()) {
                for (const auto& i : stack) {
                    std::cout << "[" << value_to_string(i) << "] ";
                }
            }
            std::cout << '\n';
        }

        switch (static_cast<OpCode>(instruction)) {
            case OpCode::Return:
                if (stack.empty()) {
                    return;
                }
                std::cout << value_to_string(stack.back()) << '\n';
                stack.pop_back();
                return;
            case OpCode::Constant: {
                Value constant = chunk.constants[*instruction_pointer++];
                stack.push_back(constant);
                break;
            }
            case OpCode::Negate: {
                if (!value_is<double>(peek(0))) {
                    throw RuntimeException("Operand must be a number.");
                }
                stack.back() = -value_extract<double>(stack.back());
                break;
            }
            case OpCode::Add: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a + b);
                break;
            }

            case OpCode::Subtract: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a - b);
                break;
            }

            case OpCode::Multiply: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a * b);
                break;
            }

            case OpCode::Divide: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a / b);
                break;
            }

            case OpCode::Nil:
                stack.push_back(Nil());
                break;

            case OpCode::False:
                stack.push_back(false);
                break;

            case OpCode::True:
                stack.push_back(true);
                break;
            case OpCode::Not: {
                auto back{stack.back()};
                stack.pop_back();
                stack.push_back(isFalsey(back));
                break;
            }
            case OpCode::Equal: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a == b);
                break;
            }
            case OpCode::Greater: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a > b);
                break;
            }
            case OpCode::Less: {
                auto b{stack.back()};
                stack.pop_back();
                auto a{stack.back()};
                stack.pop_back();
                stack.push_back(a < b);
                break;
            }
        }
    }
}

void VirtualMachine::interpret() {
    compiler.compile();
    chunk = compiler.getChunk();
    instruction_pointer = chunk.code.begin();
    run();
}

VirtualMachine::VirtualMachine(const std::string& source) : compiler{source} {
}

const Value& VirtualMachine::peek(int distance) {
    return stack.rbegin()[distance];
}

