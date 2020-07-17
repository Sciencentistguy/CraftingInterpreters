#pragma once
#include <stdexcept>
class Token;

class CompilerException : public std::exception {
    char* errorMsg;

 public:
    explicit CompilerException(const char* errorMsg);
    CompilerException(const char* errorMsg, const Token& token);
    [[nodiscard]] const char* what() const noexcept override;
    ~CompilerException() override;
};

class RuntimeException : public std::exception {
    const char* errorMsg;

 public:
    explicit RuntimeException(const char* errorMsg);
    [[nodiscard]] const char* what() const noexcept override;
};
