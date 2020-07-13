#include "runtimeerror.h"

#include <cstring>
#include <sstream>

RuntimeError::RuntimeError(const std::string& message, const Token& token) : runtime_error{message}, message{message}, token{token} {
}

const char* RuntimeError::what() const noexcept {
    std::stringstream ss;
    ss << "[Runtime error Line " << token.getLine() << "] " << message << '\n';
    char* buf = new char[ss.str().length() + 2];
    std::strcpy(buf, ss.str().c_str());
    return buf;
}
