#include "exception.h"

#include <fmt/core.h>

#include "token.h"

CompilerException::CompilerException(const char* errorMsg) : errorMsg{errorMsg} {
}

const char* CompilerException::what() const noexcept {
    return errorMsg.c_str();
}

CompilerException::CompilerException(const char* errorMsg, const Token& token) {
    this->errorMsg = fmt::format(
        "[Error line {}]{}: {}\n", token.getLine(),
        [](const Token& token) {
            using namespace std::string_literals;
            switch (token.getType()) {
                case TokenType::Eof:
                    return " at end"s;
                case TokenType::Error:
                    break;
                default:
                    return fmt::format(": {}", token.getLexeme());
            }
            return ""s;
        }(token),
        errorMsg);
}

RuntimeException::RuntimeException(const std::string& errorMsg, const VirtualMachine& vm) {
    for (int i = vm.frameCount; i >= 0; --i) {
        const auto& frame = vm.frames[i];
        const auto& function = *frame.function;
        std::size_t instruction = frame.instruction_pointer - function.chunk->code.begin() - 1;
        stackTrace +=
            fmt::format("[line {}] in {}\n", function.chunk->lines[instruction], function.name.empty() ? "script" : fmt::format("{}()", function.name));
    }
}

const char* RuntimeException::what() const noexcept {
    return errorMsg.c_str();
}

RuntimeException::RuntimeException(const std::string& errorMsg) : errorMsg{errorMsg}, stackTrace{} {
}
