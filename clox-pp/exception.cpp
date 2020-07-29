#include "exception.h"

#include <sstream>

#include <fmt/core.h>

#include "chunk.h"
#include "token.h"

CompilerException::CompilerException(const char* errorMsg) : errorMsg{errorMsg} {
}

const char* CompilerException::what() const noexcept {
    return errorMsg.c_str();
}

CompilerException::CompilerException(const char* errorMsg, const Token& token) {
    std::stringstream ss;
    ss << "[Error line " << token.getLine() << ']';
    switch (token.getType()) {
        case TokenType::Eof:
            ss << " at end";
            break;
        case TokenType::Error:
            break;
        default:
            ss << ": ";
            ss.write(token.getStart(), token.getLength());
            break;
    }
    ss << ": " << errorMsg << '\n';
    this->errorMsg = ss.str();
}

RuntimeException::RuntimeException(const std::string& errorMsg, const VirtualMachine& vm) {
    for (int i = vm.frameCount; i >= 0; --i) {
        const auto& frame = vm.frames[i];
        const auto& function = *frame.function;
        std::size_t instruction = frame.instruction_pointer - function.chunk->code.begin() - 1;
        stackTrace = fmt::format("[line {}] in {}\n", function.chunk->lines[instruction], function.name.empty() ? "script" : fmt::format("{}()", function.name));
    }
}

const char* RuntimeException::what() const noexcept {
    return errorMsg.c_str();
}
RuntimeException::RuntimeException(const std::string& errorMsg) : errorMsg{}, stackTrace{} {
}
