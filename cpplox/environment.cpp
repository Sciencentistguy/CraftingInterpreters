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

std::any Environment::getAt(int distance, std::string name) {
    auto anc = ancestor(distance);
    auto vals = anc.values;
    auto val{vals.at(name)};
    if (val.has_value()) {
        return val;
    } else {
        return val;
    }
}

Environment& Environment::ancestor(int distance) {
    Environment& env = *this;
    for (int i = 0; i < distance; ++i) {
        env = *env.enclosing;
    }
    return env;  // todo is this bad?
}

void Environment::assignAt(int distance, const Token& name, std::any value) {
    ancestor(distance).values.insert(std::make_pair(name.getLexeme(), value));
}

const std::shared_ptr<Environment>& Environment::getEnclosing() const {
    return enclosing;
}
