#include "virtualmachine.h"

#include <variant>

#include <fmt/core.h>

#include "common.h"
#include "exception.h"
#include "opcode.h"
#include "value.h"

void VirtualMachine::run() {
    CallFrame* frame = &frames[frameCount - 1];
    while (true) {
        OpCode instruction{*frame->instruction_pointer++};
        if constexpr (DEBUG) {
            fmt::print("/*\n");
            fmt::print("Stack: ");
            if (!stack.empty()) {
                for (auto stackIt = stack.cbegin(); stackIt != stack_top; ++stackIt) {
                    auto str = value_to_string(*stackIt);
                    fmt::print("[{}] ", str);
                }
            }
            fmt::print("\nInstruction: ");
            auto offset{std::distance(frame->getChunk().code.cbegin(), frame->instruction_pointer)};
            frame->getChunk().disasInstruction(instruction, offset - 1);
            fmt::print("*/\n");
        }

        switch (static_cast<OpCode>(instruction)) {
            case OpCode::Return: {
                auto result = pop();
                --frameCount;
                if (frameCount == 0) {
                    pop();
                    return;
                }
                stack_top = frame->slots;
                push(result);
                frame = &frames[frameCount-1];
            }
            case OpCode::Constant: {
                const Value& constant = frame->getChunk().constants[*frame->instruction_pointer++];
                push(constant);
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
                auto b{pop()};
                auto a{pop()};
                push(a + b);
                break;
            }

            case OpCode::Subtract: {
                auto b{pop()};
                auto a{pop()};
                push(a - b);
                break;
            }

            case OpCode::Multiply: {
                auto b{pop()};
                auto a{pop()};
                push(a * b);
                break;
            }

            case OpCode::Divide: {
                auto b{pop()};
                auto a{pop()};
                push(a / b);
                break;
            }

            case OpCode::Nil:
                push(Nil());
                break;

            case OpCode::False:
                push(false);
                break;

            case OpCode::True:
                push(true);
                break;

            case OpCode::Not: {
                auto back{pop()};
                push(isFalsey(back));
                break;
            }

            case OpCode::Equal: {
                auto b{pop()};
                auto a{pop()};
                push(a == b);
                break;
            }

            case OpCode::Greater: {
                auto b{pop()};
                auto a{pop()};
                push(a > b);
                break;
            }

            case OpCode::Less: {
                auto b{pop()};
                auto a{pop()};
                push(a < b);
                break;
            }

            case OpCode::Print:
                fmt::print("{}\n", value_to_string(pop()));
                break;

            case OpCode::Pop:
                pop();
                break;

            case OpCode::Define_global: {
                Value name = frame->getChunk().constants[*frame->instruction_pointer++];
                globals[value_extract<std::string>(name)] = pop();
                break;
            }

            case OpCode::Get_global: {
                auto v{frame->getChunk().constants[*frame->instruction_pointer++]};
                const auto& name = value_extract<std::string>(v);
                auto it{globals.find(name)};
                if (it == globals.end()) {  // not in map
                    using namespace std::string_literals;
                    throw RuntimeException("Undefined variable '"s + name + "'.");
                }
                push(it->second);
                break;
            }

            case OpCode::Set_global: {
                const auto& name = value_extract<std::string>(frame->getChunk().constants[*frame->instruction_pointer++]);
                auto it{globals.find(name)};
                if (it == globals.end()) {  // not in map
                    using namespace std::string_literals;
                    throw RuntimeException("Undefined variable '"s + name + "'.");
                }
                globals[name] = peek(0);
                break;
            }

            case OpCode::Get_local: {
                uint8_t slot{*frame->instruction_pointer++};
                push(frame->slots[slot]);
                break;
            }
            case OpCode::Set_local: {
                uint8_t slot{*frame->instruction_pointer++};
                frame->slots[slot] = peek(0);
                break;
            }
            case OpCode::Jump_if_false: {
                frame->instruction_pointer += 2;
                if (isFalsey(peek(0))) {
                    uint16_t offset{static_cast<uint16_t>(frame->instruction_pointer[-2] << 8u | frame->instruction_pointer[-1])};
                    frame->instruction_pointer += offset;
                }
                break;
            }
            case OpCode::Jump: {
                frame->instruction_pointer += 2;
                uint16_t offset{static_cast<uint16_t>(frame->instruction_pointer[-2] << 8u | frame->instruction_pointer[-1])};
                frame->instruction_pointer += offset;
                break;
            }
            case OpCode::Loop: {
                frame->instruction_pointer += 2;
                uint16_t offset{static_cast<uint16_t>(frame->instruction_pointer[-2] << 8u | frame->instruction_pointer[-1])};
                frame->instruction_pointer -= offset;
                break;
            }
            case OpCode::Call: {
                uint8_t argCount{*frame->instruction_pointer++};
                callValue(peek(argCount), argCount);
                frame = &frames[frameCount-1];
                break;
            }
        }
    }
}

void VirtualMachine::interpret() {
    auto function = compiler.compile();
    auto p = push(function);
    callValue(*p, 0);
    run();
}

VirtualMachine::VirtualMachine(const std::string& source) : compiler{FunctionType::Script} {
    setSource(source);
}

const Value& VirtualMachine::peek(int distance) const {
    return stack_top[-1 - distance];
}

void VirtualMachine::setSource(const std::string& source) {
    compiler.setSource(source);
    std::fill(stack.begin(), stack.end(), Value());
}

Value* VirtualMachine::push(const Value& value) {
    *(stack_top) = value;
    return stack_top++;
}

Value& VirtualMachine::pop() {
    --stack_top;
    return *stack_top;
}

void VirtualMachine::callValue(const Value& callee, uint8_t argCount) {
    if (value_is<Function>(callee)) {
        return call(value_extract<Function>(callee), argCount);
    }

    throw RuntimeException("Can only call functions and classes.");
}

void VirtualMachine::call(const Function& function, uint8_t argCount) {
    if (argCount != function.arity) {
        throw RuntimeException(fmt::format("Expected {} arguments but got {}.", function.arity, argCount));
    }
    if (frameCount == FRAMES_MAX) {
        throw RuntimeException("Stack overflow (frame limit reached).");
    }
    CallFrame& frame = frames[frameCount++];
    frame.function = &function;
    frame.instruction_pointer = function.chunk->code.begin();
    frame.slots = stack_top - argCount - 1;
    fmt::print("{}\n", value_to_string(function));
}
