#pragma once
#include <stdexcept>
#include <string>
class Token;

class CompilerException : public std::exception {
    std::string errorMsg;

 public:
    explicit CompilerException(const char* errorMsg);
    CompilerException(const char* errorMsg, const Token& token);
    [[nodiscard]] const char* what() const noexcept override;
};

class RuntimeException : public std::exception {
    std::string errorMsg;

 public:
    explicit RuntimeException(const std::string& errorMsg);
    [[nodiscard]] const char* what() const noexcept override;
};
