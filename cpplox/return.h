#pragma once

#include <any>

class Return {
    std::any value;

 public:
    explicit Return(std::any  value);
    static const char* what() noexcept;
    [[nodiscard]] const std::any& getValue() const;
};
