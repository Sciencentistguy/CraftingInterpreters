#include "value.h"

#include <chrono>

#include <fmt/core.h>

#include "closure.h"
#include "exception.h"

bool isFalsey(const Value& value) {
    return std::holds_alternative<Nil>(value) || (std::holds_alternative<bool>(value) && !std::get<bool>(value));
}

std::string value_to_string(const Value& v) {
    if (std::holds_alternative<bool>(v)) {
        return std::get<bool>(v) ? "true" : "false";
    }
    if (std::holds_alternative<double>(v)) {
        return fmt::format("{:g}", std::get<double>(v));
    }
    if (std::holds_alternative<Nil>(v)) {
        return "nil";
    }
    if (std::holds_alternative<std::string>(v)) {
        return std::get<std::string>(v);
    }
    if (std::holds_alternative<Function>(v)) {
        const auto& fun{std::get<Function>(v)};
        if (fun.getName().empty()) {
            return fmt::format("<main>");
        }
        return fmt::format("<Fn {}>", fun.getName());
    }
    if (std::holds_alternative<Closure>(v)) {
        return fmt::format("<Closure {}>", std::get<Closure>(v).getFunction().name);
    }
    if (std::holds_alternative<NativeFn>(v)) {
        return "<Fn clock>";
    }
    if (std::holds_alternative<RuntimeUpvalue>(v)) {
        return "upvalue (unreachable)";
    }
    throw std::runtime_error("Unhandled alternative in value_to_string.");
}

bool operator==(const Value& lhs, const Value& rhs) {
    if (lhs.index() != rhs.index())
        return false;

    if (std::holds_alternative<bool>(rhs)) {
        return std::get<bool>(lhs) == std::get<bool>(rhs);
    }
    if (std::holds_alternative<double>(lhs)) {
        return std::get<double>(lhs) == std::get<double>(rhs);
    }
    if (std::holds_alternative<Nil>(lhs)) {
        return false;
    }
    if (std::holds_alternative<std::string>(lhs)) {
        return std::get<std::string>(lhs) == std::get<std::string>(rhs);
    }
    return false;
}

Value operator+(const Value& lhs, const Value& rhs) {
    if (std::holds_alternative<std::string>(lhs) && std::holds_alternative<std::string>(rhs)) {
        const auto& a = std::get<std::string>(lhs);
        const auto& b = std::get<std::string>(rhs);
        return a + b;
    }
    if (std::holds_alternative<double>(lhs) && std::holds_alternative<double>(rhs)) {
        const auto& a = std::get<double>(lhs);
        const auto& b = std::get<double>(rhs);
        return a + b;
    }
    throw RuntimeException("Operands to '+' must be either both numbers or both strings.");
}

double operator-(const Value& lhs, const Value& rhs) {
    if (!(std::holds_alternative<double>(lhs) && std::holds_alternative<double>(rhs))) {
        throw RuntimeException("Operands to '-' must be numbers.");
    }
    const auto& a = std::get<double>(lhs);
    const auto& b = std::get<double>(rhs);
    return a - b;
}

double operator*(const Value& lhs, const Value& rhs) {
    if (!(std::holds_alternative<double>(lhs) && std::holds_alternative<double>(rhs))) {
        throw RuntimeException("Operands to '*' must be numbers.");
    }
    const auto& a = std::get<double>(lhs);
    const auto& b = std::get<double>(rhs);
    return a * b;
}

double operator/(const Value& lhs, const Value& rhs) {
    if (!(std::holds_alternative<double>(lhs) && std::holds_alternative<double>(rhs))) {
        throw RuntimeException("Operands to '/' must be numbers.");
    }
    const auto& a = std::get<double>(lhs);
    const auto& b = std::get<double>(rhs);
    return a / b;
}

bool operator>(const Value& lhs, const Value& rhs) {
    if (!(std::holds_alternative<double>(lhs) && std::holds_alternative<double>(rhs))) {
        throw RuntimeException("Operands to '>' must be numbers.");
    }
    const auto& a = std::get<double>(lhs);
    const auto& b = std::get<double>(rhs);
    return a > b;
}

bool operator<(const Value& lhs, const Value& rhs) {
    return rhs > lhs;
}

double clockNative() {
    return static_cast<double>(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count());
}
