#pragma once

#include <string>
#include <variant>

#include <fmt/core.h>

#include "closure.h"
#include "function.h"
#include "upvalue.h"

class Nil {};
class Function;
class Closure;
class RuntimeUpvalue;

using NativeFn = double (*)();
using Value = std::variant<double, bool, Nil, std::string, Function, NativeFn, Closure, RuntimeUpvalue>;

double clockNative();

bool isFalsey(const Value& value);

std::string value_to_string(const Value& v);

bool operator==(const Value& lhs, const Value& rhs);

Value operator+(const Value& lhs, const Value& rhs);

double operator-(const Value& lhs, const Value& rhs);

double operator*(const Value& lhs, const Value& rhs);

double operator/(const Value& lhs, const Value& rhs);

bool operator>(const Value& lhs, const Value& rhs);
bool operator<(const Value& lhs, const Value& rhs);
