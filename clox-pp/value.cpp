#include "value.h"

#include "exception.h"
#include "virtualmachine.h"

template<typename T>
bool value_is(const Value& v) {
    return std::holds_alternative<T>(v);
}

template<typename T>
T value_extract(const Value& v) {
    return std::get<T>(v);
}

bool isFalsey(const Value& value) {
    return value_is<Nil>(value) || (value_is<bool>(value) && !value_extract<bool>(value));
}

std::string value_to_string(const Value& v) {
    if (value_is<bool>(v)) {
        return value_extract<bool>(v) ? "true" : "false";
    }
    if (value_is<double>(v)) {
        double d{value_extract<double>(v)};
        if (d == static_cast<int>(d)) {
            return std::to_string(static_cast<int>(d));
        } else {
            return std::to_string(d);
        }
    }
    if (value_is<Nil>(v)) {
        return "nil";
    }
    return "This should be unreachable.";
}

bool operator==(const Value& lhs, const Value& rhs) {
    if (lhs.index() != rhs.index())
        return false;

    if (value_is<bool>(rhs)) {
        return value_extract<bool>(lhs) == value_extract<bool>(rhs);
    }
    if (value_is<double>(lhs)) {
        return value_extract<double>(lhs) == value_extract<double>(rhs);
    }
    if (value_is<Nil>(lhs)) {
        return false;
    }
    return false;
}

// void VirtualMachine::binary_op(Lambda l) {
//    if (!value_is<double>(peek(0)) || !value_is<double>(peek(1))) {
//        throw RuntimeException("Operands to binary operation must be numbers.");
//    }
//    auto b = value_extract<double>(stack.back());
//    stack.pop_back();
//    auto a = value_extract<double>(stack.back());
//    stack.pop_back();
//    stack.push_back(l(a, b));
//}

double operator+(const Value& lhs, const Value& rhs) {
    if (!(value_is<double>(lhs) && value_is<double>(rhs))) {
        throw RuntimeException("Operands to + must be numbers.");
    }
    auto a = value_extract<double>(lhs);
    auto b = value_extract<double>(rhs);
    return a + b;
}

double operator-(const Value& lhs, const Value& rhs) {
    if (!(value_is<double>(lhs) && value_is<double>(rhs))) {
        throw RuntimeException("Operands to - must be numbers.");
    }
    auto a = value_extract<double>(lhs);
    auto b = value_extract<double>(rhs);
    return a - b;
}

double operator*(const Value& lhs, const Value& rhs) {
    if (!(value_is<double>(lhs) && value_is<double>(rhs))) {
        throw RuntimeException("Operands to / must be numbers.");
    }
    auto a = value_extract<double>(lhs);
    auto b = value_extract<double>(rhs);
    return a * b;
}

double operator/(const Value& lhs, const Value& rhs) {
    if (!(value_is<double>(lhs) && value_is<double>(rhs))) {
        throw RuntimeException("Operands to / must be numbers.");
    }
    auto a = value_extract<double>(lhs);
    auto b = value_extract<double>(rhs);
    return a / b;
}

bool operator>(const Value& lhs, const Value& rhs) {
    auto a = isFalsey(lhs);
    auto b = isFalsey(rhs);
    return a > b;
}

bool operator<(const Value& lhs, const Value& rhs) {
    return rhs > lhs;
}
