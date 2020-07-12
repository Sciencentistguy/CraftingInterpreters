#include "loxclass.h"

#include <sstream>

#include "loxfunction.h"
#include "loxinstance.h"

LoxClass::LoxClass(const std::string& name, std::shared_ptr<LoxClass> superclass,
                   const std::unordered_map<std::string, std::shared_ptr<LoxFunction>> methods) :
    name{name},
    superclass{superclass}, methods{methods} {
}

std::string LoxClass::to_string() const {
    return "<class " + name + '>';
}

std::any LoxClass::operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) const {
    auto instance{std::make_shared<LoxInstance>(*this)};
    auto initializer = findMethod("init");
    if (initializer) {
        (*initializer->bind(instance))(interpreter, arguments);
    }
    return instance;
}

size_t LoxClass::arity() const {
    auto initializer{findMethod("init")};
    if (!initializer) {
        return 0;
    }
    return initializer->arity();
}

const std::string& LoxClass::getName() const {
    return name;
}

std::shared_ptr<LoxFunction> LoxClass::findMethod(std::string name) const {
    if (methods.contains(name)) {
        return methods.at(name);
    }
    if (superclass) {
        return superclass->findMethod(name);
    }
    return nullptr;
}
