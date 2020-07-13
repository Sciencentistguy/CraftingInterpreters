#include "loxfunction.h"

#include <chrono>

#include "return.h"
#include "interpreter.h"

LoxFunction::LoxFunction(const FunctionStatement& declaration, const std::shared_ptr<Environment>& closure, bool isInitializer) :
    declaration{declaration}, closure{closure}, isConstructor{isInitializer} {
}

std::any LoxFunction::operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) const {
    auto environment{std::make_shared<Environment>(closure)};
    for (unsigned int i = 0; i < declaration.getParams().size(); ++i) {
        environment->define(declaration.getParams()[i].getLexeme(), arguments[i]);
    }
    try {
        interpreter.executeBlock(declaration.getBody(), environment);
    } catch (const Return& r) {
        if (isConstructor) {
            return closure->getAt(0, "this");
        }
        return r.getValue();
    }
    if (isConstructor) {
        return closure->getAt(0, "this");
    }
    return std::any();
}

size_t LoxFunction::arity() const {
    return declaration.getParams().size();
}

std::string LoxFunction::to_string() const {
    return "<fn " + declaration.getName().getLexeme() + '>';
}

std::shared_ptr<LoxFunction> LoxFunction::bind(std::shared_ptr<LoxInstance> instance) {
    auto env{std::make_shared<Environment>(closure)};
    env->define("this", instance);
    return std::make_shared<LoxFunction>(declaration, env, isConstructor);
}

std::any LoxBuiltinClock::operator()(Interpreter& interpreter, const std::vector<std::any>& args) const {
    auto time = std::chrono::system_clock::now();
    auto since_epoch{time.time_since_epoch()};
    auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(since_epoch);
    double ret = millis.count();
    return ret;
}

size_t LoxBuiltinClock::arity() const {
    return 0;
}
