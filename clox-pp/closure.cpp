#include "closure.h"

#include "function.h"

Closure::Closure(const std::shared_ptr<Function>& function) : Closure(*function) {
}

const Function& Closure::getFunction() const {
    return *function;
}

Closure::Closure(const Function& function) :
    function{std::make_shared<Function>(function)}, upvalues{static_cast<std::vector<RuntimeUpvalue*>::size_type>(function.upvalueCount),
                                                             RuntimeUpvalue(nullptr)} {
}
