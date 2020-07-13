#include "main.h"

#include <any>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#include "interpreter.h"
#include "lexer.h"
#include "loxclass.h"
#include "loxfunction.h"
#include "loxinstance.h"
#include "parser.h"
#include "resolver.h"
#include "token.h"

bool hadError = false;
bool hadRuntimeError = false;

std::shared_ptr<Interpreter> interpreter{std::make_shared<Interpreter>()};

int main(int argc, char* argv[]) {
    if (argc > 2) {
        std::cout << "Usage: 'cpplox [script]'\n";
        std::exit(64);
    }

    if (argc == 2) {
        runFile(argv[1]);
    } else {
        runPrompt();
    }
}

void error(int line, const std::string& message) {
    report(line, "", message);
}

void error(const Token& token, const std::string& message) {
    if (token.getType() == TokenType::Eof) {
        report(token.getLine(), " at end", message);
    } else {
        report(token.getLine(), " at '" + token.getLexeme() + "'", message);
    }
}

void report(int line, const std::string& where, const std::string& message) {
    std::cerr << "[Error line " << line << "] " << where << ": " << message << '\n';
    hadError = true;
}

void run(const std::string& str) {
    Lexer l{str};
    auto tokens = l.scanTokens();
    Parser p{tokens};
    auto statements = p.parse();

    if (hadError) {
        return;
    }

    auto r{std::make_shared<Resolver>(*interpreter)};
    r->resolve(statements);

    if (hadError) {
        return;
    }

    interpreter->interpret(statements);

    //    auto printer{std::make_shared<expressionprinter>()};
    //    std::cout << printer->print(expression) << '\n';

    //    for (const auto& token : tokens) {
    //        std::cout << token << '\n';
    //    }
}

std::string readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::binary);
    file.unsetf(std::ios::skipws);
    return std::string((std::istream_iterator<uint8_t>(file)), std::istream_iterator<uint8_t>());
}

void runFile(const std::string& path) {
    const std::string file{readFile(path)};
    run(file);
    if (hadError) {
        std::exit(65);
    }
    if (hadRuntimeError) {
        std::exit(70);
    }
}

void runPrompt() {
    while (true) {
        std::string str{};
        std::cout << std::flush;
        std::cout << ">>> ";
        std::getline(std::cin, str);
        if (str == "") {
            std::cout << '\n';
            return;
        }
        try {
            run(str);
        } catch (const std::exception& e) {
            std::cerr << e.what() << std::endl;
        }

        hadError = false;
    }
}

void runtimeError(const RuntimeError& error) {
    const char* msg = error.what();
    std::cerr << msg;
    delete[] msg;
    hadRuntimeError = true;
}

std::string stringify(const std::any& a) {
    if (!a.has_value()) {
        return "nil";
    }
    if (a.type() == typeid(long)) {
        return std::to_string(std::any_cast<long>(a));
    }
    if (a.type() == typeid(double)) {
        const double& val = std::any_cast<double>(a);
        if (val == static_cast<long>(val)) {
            return std::to_string(static_cast<long>(val));
        }
        return std::to_string(val);
    }
    if (a.type() == typeid(std::string)) {
        return std::any_cast<std::string>(a);
    }
    if (a.type() == typeid(bool)) {
        return std::any_cast<bool>(a) ? "true" : "false";
    }
    if (a.type() == typeid(std::shared_ptr<LoxFunction>)) {
        return std::any_cast<std::shared_ptr<LoxFunction>>(a)->to_string();
    }
    if (a.type() == typeid(LoxClass)) {
        return std::any_cast<LoxClass>(a).to_string();
    }
    if (a.type() == typeid(std::shared_ptr<LoxInstance>)) {
        return std::any_cast<std::shared_ptr<LoxInstance>>(a)->to_string();
    }

    throw std::runtime_error(std::string("[stringify()] Unexpected std::any type '") + a.type().name() + "'");
}
