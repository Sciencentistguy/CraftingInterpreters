#include "main.h"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#include "lexer.h"
#include "token.h"
#include "token_type.h"

bool hadError = false;

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

void report(int line, const std::string& where, const std::string message) {
    std::cerr << "[Error line " << line << "] " << where << ": " << message << '\n';
    hadError = true;
}

void run(const std::string& str) {
    Lexer s{str};
    auto tokens = s.scanTokens();

    for (const auto& token : tokens) {
        std::cout << token << '\n';
    }
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
}

void runPrompt() {
    while (true) {
        std::string str{};
        std::cout << ">>> ";
        std::getline(std::cin, str);
        if (str == "") {
            std::cout << '\n';
            return;
        }
        run(str);
        hadError = false;
    }
}
