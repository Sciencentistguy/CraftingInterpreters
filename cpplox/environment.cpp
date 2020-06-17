#include "environment.h"

void Environment::define(const std::string& name, const std::any& value) {
    values.insert(std::make_pair(name, value));
}

std::any Environment::get(const Token& name) {
    const std::string& str = name.getLexeme();
    auto it = values.find(str);
    if (it != values.end()) {
        return it->second;
    }
    throw RuntimeError("Undefined variable '" + name.getLexeme() + "'.", name);
}
