#include "environment.h"

#include "runtimeerror.h"

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

std::any Environment::get(const Token& name) const {
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

std::any Environment::getAt(int distance, std::string name) const {
    auto anc = ancestor(distance);
    auto vals = anc.values;
    auto valIt = vals.find(name);
    if (valIt == vals.end()) {
        return std::any();
    }
    auto val{*valIt};
    return val.second;
}

Environment& Environment::ancestor(int distance) {
    Environment* envp = this;
    for (int i = 0; i < distance; ++i) {
        envp = envp->enclosing.get();
    }
    return *envp;
}

const Environment& Environment::ancestor(int distance) const {
    const auto* envp = this;
    for (int i = 0; i < distance; ++i) {
        envp = envp->enclosing.get();
    }
    return *envp;
}

void Environment::assignAt(int distance, const Token& name, std::any value) {
    auto& env = ancestor(distance);
    env.values[name.getLexeme()] = value;
}

const std::shared_ptr<Environment>& Environment::getEnclosing() const {
    return enclosing;
}