#include "environment.h"

Environment::Environment() : enclosing{nullptr} {
}

Environment::Environment(const std::shared_ptr<Environment> enclosing) : enclosing{enclosing} {
}

void Environment::define(const std::string& name, const std::any value) {
    values.insert(std::make_pair(name, value));
}

void Environment::assign(const Token& name, const std::any& value) {
    if (values.contains(name.getLexeme())) {
        values[name.getLexeme()] = value;
        return;
    }
    if (enclosing) {
        enclosing->assign(name, value);
        return;
    }
    throw RuntimeError("Undefined variable '" + name.getLexeme() + "'", name);
}

std::any Environment::get(const Token& name) {
    const std::string& str = name.getLexeme();
    auto it = values.find(str);
    if (it != values.end()) {
        return it->second;
    }
    if (enclosing) {
        return enclosing->get(name);
    }

    throw RuntimeError("Undefined variable '" + name.getLexeme() + "'.", name);
}
