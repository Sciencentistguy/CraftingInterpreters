#include "upvalue.h"

RuntimeUpvalue::RuntimeUpvalue(Value* location) : location{location} {
}

Value* RuntimeUpvalue::getLocation() const {
    return location;
}
