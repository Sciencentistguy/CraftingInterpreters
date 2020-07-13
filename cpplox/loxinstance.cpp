#include "loxinstance.h"

#include "loxfunction.h"
#include "runtimeerror.h"

LoxInstance::LoxInstance(const LoxClass& cls) : cls{cls} {
}

std::string LoxInstance::to_string() const {
    return "<instance of " + cls.getName() + '>';
}

std::any LoxInstance::get(const Token& name) {
    if (fields.contains(name.getLexeme())) {
        return fields.at(name.getLexeme());
    }
    auto method = cls.findMethod(name.getLexeme());
    if (method) {
        return method->bind(this->shared_from_this());
    }

    throw RuntimeError(std::string("Undefined property '") + name.getLexeme() + "'.", name);
    return std::any();
}

void LoxInstance::set(const Token& name, const std::any& value) {
    fields.insert(std::make_pair(name.getLexeme(), value));
}
