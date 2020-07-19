#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

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
        std::cout << ">>> ";
        std::getline(std::cin, line);
        vm.setSource(line);
        try {
            vm.interpret();
        } catch (const CompilerException& e) {
            std::cout << "Compiler error: " << e.what() << '\n';
        } catch (const RuntimeException& e) {
            std::cout << "Runtime error: " << e.what() << '\n';
        }
    }
}

void runFile(const std::string& path) {
    std::string source = readFile(path);
    VirtualMachine vm{source};
    try {
        vm.interpret();
    } catch (const CompilerException& e) {
        std::cout << "Compiler error: " << e.what() << '\n';
    } catch (const RuntimeException& e) {
        std::cout << "Runtime error: " << e.what() << '\n';
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
