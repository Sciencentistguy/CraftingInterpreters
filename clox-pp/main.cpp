#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

#include <fmt/core.h>

#include "exception.h"
#include "virtualmachine.h"

std::string readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::binary);
    file.unsetf(std::ios::skipws);
    return std::string((std::istream_iterator<uint8_t>(file)), std::istream_iterator<uint8_t>());
}

[[noreturn]] void repl() {
    std::string line{};
    VirtualMachine vm{""};
    while (true) {
        fmt::print(">>> ");
        std::getline(std::cin, line);
        vm.setSource(line);
        try {
            vm.interpret();
        } catch (const CompilerException& e) {
            fmt::print("Compiler error: {}\n", e.what());
        } catch (const RuntimeException& e) {
            fmt::print("Runtime error: {}\n", e.what());
        }
    }
}

void runFile(const std::string& path) {
    std::string source = readFile(path);
    VirtualMachine vm{source};
    try {
        vm.interpret();
    } catch (const CompilerException& e) {
        fmt::print("Compiler error: {}\n", e.what());
    } catch (const RuntimeException& e) {
        fmt::print("Runtime error: {}\n", e.what());
    }
}

int main(int argc, char** argv) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        std::cerr << "Usage: clox-pp <file>\n";
        std::exit(64);
    }
}
