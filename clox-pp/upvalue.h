#pragma once

#include <cstdint>
#include <string>
#include <variant>

//#include "value.h"

struct Upvalue {
    uint8_t index;
    bool isLocal;
};
class RuntimeUpvalue;
class Nil;
class Function;
using NativeFn = double (*)();
class Closure;

using Value = std::variant<double, bool, Nil, std::string, Function, NativeFn, Closure, RuntimeUpvalue>;

class RuntimeUpvalue {
    Value* location;

 public:
    explicit RuntimeUpvalue(Value* location);
    [[nodiscard]] Value* getLocation() const;
};