#pragma once

#include <stdexcept>
#include <string>

#include "token.h"

class RuntimeError : public std::runtime_error {
//    const std::string message;
    const Token token;
    char* message;

 public:
    RuntimeError(const std::string& err_msg, const Token& token);
    ~RuntimeError();

    const char* what() const noexcept override;
};
