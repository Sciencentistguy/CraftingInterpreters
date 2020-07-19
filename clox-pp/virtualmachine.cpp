#include "virtualmachine.h"

#include <iostream>

#include "common.h"
#include "exception.h"
#include "opcode.h"
#include "value.h"

void VirtualMachine::run() {
    while (true) {
        OpCode instruction{*instruction_pointer++};
        if constexpr (DEBUG) {
            std::cout << "Stack: ";
            if (!stack.empty()) {
                for (const auto& i : stack) {
                    std::cout << "[" << value_to_string(i) << "] ";
                }
            }

            std::cout << "\nInstruction: ";
            chunk.disasInstruction(instruction);

            std::cout << '\n';
        }

        switch (static_cast<OpCode>(instruction)) {
            case OpCode::Return:
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

            case OpCode::Print:
                std::cout << value_to_string(stack.back()) << '\n';
                stack.pop_back();
                break;

            case OpCode::Pop:
                stack.pop_back();
                break;
            case OpCode::Define_global: {
                Value name = chunk.constants[*instruction_pointer++];
                globals[value_extract<std::string>(name)] = peek(0);
                stack.pop_back();
                break;
            }
            case OpCode::Get_global: {
                const auto& name = value_extract<std::string>(chunk.constants[*instruction_pointer++]);
                auto it{globals.find(name)};
                if (it == globals.end()) {  // not in map
                    using namespace std::string_literals;
                    throw RuntimeException("Undefined variable '"s + name + "'.");
                }
                stack.push_back(it->second);
                break;
            }
            case OpCode::Set_global: {
                const auto& name = value_extract<std::string>(chunk.constants[*instruction_pointer++]);
                auto it{globals.find(name)};
                if (it == globals.end()) {  // not in map
                    using namespace std::string_literals;
                    throw RuntimeException("Undefined variable '"s + name + "'.");
                }
                globals[name] = peek(0);
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

const Value& VirtualMachine::peek(int distance) const {
    return stack.rbegin()[distance];
}
void VirtualMachine::setSource(const std::string& source) {
    compiler.setSource(source);
    stack.clear();
}
