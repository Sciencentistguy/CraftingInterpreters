#pragma once

#include <stdexcept>
#include <string>

#include "token.h"

class RuntimeError : public std::runtime_error {
    const std::string message;
    const Token token;

 public:
    RuntimeError(const std::string& message, const Token& token);
    const char* what() const noexcept override;
};
