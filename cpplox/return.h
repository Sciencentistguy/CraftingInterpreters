#pragma once
#include <any>

class Return {
    std::any value;

 public:
    explicit Return(const std::any& value);
    const char* what() noexcept;
    const std::any& getValue() const;
};
