#include "exception.h"

#include <sstream>

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

RuntimeException::RuntimeException(const std::string& errorMsg) : errorMsg{errorMsg} {
}

const char* RuntimeException::what() const noexcept {
    return errorMsg.c_str();
}
