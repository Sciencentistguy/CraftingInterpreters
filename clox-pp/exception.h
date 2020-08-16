#pragma once

#include <bits/exception.h>
#include <string>

class Token;
class VirtualMachine;

class CompilerException : public std::exception {
    std::string errorMsg;

 public:
    explicit CompilerException(const char* errorMsg);
    CompilerException(const char* errorMsg, const Token& token);
    [[nodiscard]] const char* what() const noexcept override;
};

class RuntimeException : public std::exception {
    std::string errorMsg;
    std::string stackTrace{};

 public:
    explicit RuntimeException(const std::string& errorMsg);
    RuntimeException(const std::string& errorMsg, const VirtualMachine& vm);
    [[nodiscard]] const char* what() const noexcept override;
};
