#include "loxfunction.h"
#include <chrono>

LoxFunction::LoxFunction(FunctionStatement declaration) : declaration{declaration} {

}

std::any LoxFunction::operator()(Interpreter& interpreter, const std::vector<std::any>& arguments) {
    auto environment{std::make_shared<Environment>(interpreter.globals)};
    for (int i = 0; i < declaration.getParams().size(); ++i) {
        environment->define(declaration.getParams()[i].getLexeme(), arguments[i]);
    }
    interpreter.executeBlock(declaration.getBody(), environment);
    return std::any();
}

int LoxFunction::arity() {
    return declaration.getParams().size();
}

std::ostream& operator<<(std::ostream& os, const LoxFunction& function) {
    os << "<fn " + function.declaration.getName().getLexeme() << '>';
    return os;
}

std::string LoxFunction::to_string() {
    return "<fn " + declaration.getName().getLexeme() + '>';
}

std::any LoxBuiltinClock::operator()(Interpreter& interpreter, const std::vector<std::any>& args) {
    auto time = std::chrono::system_clock::now();
    auto since_epoch{time.time_since_epoch()};
    auto millis = std::chrono::duration_cast<std::chrono::seconds>(since_epoch);
    return millis.count() / 1000;
}
int LoxBuiltinClock::arity() {
    return 0;
}
