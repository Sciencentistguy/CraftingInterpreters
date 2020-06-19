//
// Created by jamie on 6/19/20.
//

#include "return.h"
Return::Return(const std::any& value) : value{value} {
}
const char* Return::what() noexcept {
    return "return exception. this shouldn't have terminated";
}
const std::any& Return::getValue() const {
    return value;
}
