#pragma once

#include <string>
#include <variant>

// using Nil = std::nullptr_t;
class Nil {};

using Value = std::variant<double, bool, Nil>;

template<typename T>
bool value_is(const Value& v);

template<typename T>
T value_extract(const Value& v);

bool isFalsey(const Value& value);

std::string value_to_string(const Value& v);

bool operator==(const Value& lhs, const Value& rhs);

double operator+(const Value& lhs, const Value& rhs);

double operator-(const Value& lhs, const Value& rhs);

double operator*(const Value& lhs, const Value& rhs);

double operator/(const Value& lhs, const Value& rhs);

bool operator>(const Value& lhs, const Value& rhs);
bool operator<(const Value& lhs, const Value& rhs);
