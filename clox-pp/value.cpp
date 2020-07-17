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
    if (value_is<std::string>(v)) {
        return value_extract<std::string>(v);
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
    if (value_is<std::string>(lhs)) {
        return value_extract<std::string>(lhs) == value_extract<std::string>(rhs);
    }
    return false;
}

Value operator+(const Value& lhs, const Value& rhs) {
    if (value_is<std::string>(lhs) && value_is<std::string>(rhs)) {
        auto a = value_extract<std::string>(lhs);
        auto b = value_extract<std::string>(rhs);
        return a + b;
    }
    if (value_is<double>(lhs) && value_is<double>(rhs)) {
        auto a = value_extract<double>(lhs);
        auto b = value_extract<double>(rhs);
        return a + b;
    }
    throw RuntimeException("Operands to + must be either both numbers or both strings.");
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
        throw RuntimeException("Operands to * must be numbers.");
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
