#include "return.h"

#include <utility>

Return::Return(std::any  value) : value{std::move(value)} {
}

const char* Return::what() noexcept {
    return "Return exception. This shouldn't have terminated.";
}

const std::any& Return::getValue() const {
    return value;
}
