#include "exception.h"

#include <cstring>
#include <sstream>

#include "token.h"

CompilerException::CompilerException(const char* errorMsg) {
    this->errorMsg = new char[std::strlen(errorMsg)+1];
    std::strcpy(this->errorMsg, errorMsg);
}

const char* CompilerException::what() const noexcept {
    return errorMsg;
}

CompilerException::CompilerException(const char* errorMsg, const Token& token){
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
    this->errorMsg = new char[ss.str().size() + 1];
    std::strcpy(this->errorMsg, ss.str().c_str());
}

CompilerException::~CompilerException() {
    delete [] errorMsg;
}

RuntimeException::RuntimeException(const char* errorMsg) : errorMsg{errorMsg} {
}

const char* RuntimeException::what() const noexcept {
    return errorMsg;
}
