#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "callframe.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "value.h"
class VirtualMachine {
    friend class RuntimeException;
    CompilerDriver compiler;
    std::array<CallFrame, FRAMES_MAX> frames{};
    int frameCount{};

    std::array<Value, STACK_MAX> stack;
    std::array<Value, STACK_MAX>::iterator stack_top{stack.begin()};
    std::unordered_map<std::string, Value> globals;

    void defineNative(std::string_view name, NativeFn function);

    [[nodiscard]] const Value& peek(int distance) const;
    Value* push(const Value& value);
    Value& pop();

    void callValue(const Value& callee, uint8_t argCount);
    void call(const Function& function, uint8_t argCount);

 public:
    explicit VirtualMachine(const std::string& source);

    void run();
    void interpret();
    void setSource(const std::string& source);
};