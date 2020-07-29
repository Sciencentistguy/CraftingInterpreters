#pragma once

#include <string>
#include <variant>

#include "function.h"

class Nil {};
class Function;

using Value = std::variant<double, bool, Nil, std::string, Function>;

template<typename T>
bool value_is(const Value& v);

template<typename T>
T value_extract(const Value& v);

bool isFalsey(const Value& value);

std::string value_to_string(const Value& v);

bool operator==(const Value& lhs, const Value& rhs);

Value operator+(const Value& lhs, const Value& rhs);

double operator-(const Value& lhs, const Value& rhs);

double operator*(const Value& lhs, const Value& rhs);

double operator/(const Value& lhs, const Value& rhs);

bool operator>(const Value& lhs, const Value& rhs);
bool operator<(const Value& lhs, const Value& rhs);
