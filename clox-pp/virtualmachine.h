#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "callframe.h"
#include "chunk.h"
#include "compiler.h"
#include "value.h"

constexpr auto FRAMES_MAX{256};
constexpr auto STACK_MAX{FRAMES_MAX * UINT8_MAX};

class VirtualMachine {
    CompilerDriver compiler;
    std::array<CallFrame, FRAMES_MAX> frames{};
    int frameCount{};

    std::array<Value, STACK_MAX> stack;
    Value* stack_top{stack.data()};
    std::unordered_map<std::string, Value> globals;

    [[nodiscard]] const Value& peek(int distance) const;
    Value* push(const Value& value);
    Value& pop();


 public:
    explicit VirtualMachine(const std::string& source);

    void run();
    void interpret();
    void setSource(const std::string& source);
};