#include "runtimeerror.h"

#include <cstring>
#include <sstream>

RuntimeError::RuntimeError(const std::string& err_msg, const Token& token) : runtime_error{err_msg}, token{token} {
    std::stringstream ss;
    ss << "[Runtime error Line " << token.getLine() << "] " << err_msg << '\n';
    message = new char[ss.str().length() + 1];
    std::strcpy(message, ss.str().c_str());
}

const char* RuntimeError::what() const noexcept {
    return message;
}

RuntimeError::~RuntimeError() {
    delete[] message;
}
