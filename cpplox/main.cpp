#include "main.h"

#include <any>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#include "expression_printer.h"
#include "interpreter.h"
#include "lexer.h"
#include "parser.h"
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
    std::cerr << error.what();
    hadRuntimeError = true;
}

std::string stringify(const std::any& a) {
    if (!a.has_value()) {
        return "nil";
    }
    if (a.type() == typeid(double)) {
        const double& val = std::any_cast<double>(a);
        if (val == static_cast<int>(val)) {
            return std::to_string(static_cast<int>(val));
        }
        return std::to_string(val);
    }
    if (a.type() == typeid(std::string)) {
        return std::any_cast<std::string>(a);
    }
    if (a.type() == typeid(bool)) {
        return std::any_cast<bool>(a) ? "true" : "false";
    }
    throw std::runtime_error(std::string("Unexpected std::any type '") + a.type().name() + "'");
}
