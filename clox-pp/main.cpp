#include <iostream>
#include <iterator>
#include <fstream>

#include "chunk.h"
#include "common.h"
#include "opcode.h"
#include "virtualmachine.h"

std::string readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::binary);
    file.unsetf(std::ios::skipws);
    return std::string((std::istream_iterator<uint8_t>(file)), std::istream_iterator<uint8_t>());
}

void repl() {
    VirtualMachine vm;
    std::string line{};
    while (true) {
        std::cout << ">>> ";
        std::getline(std::cin, line);
        vm.interpret(line);
    }
}

void runFile(std::string path) {
    std::string source = readFile(path);
    VirtualMachine vm{};
    auto result = vm.interpret(source);
    if (result == InterpretResult::Compile_Error) {
        std::exit(65);
    }
    if (result == InterpretResult::Runtime_Error) {
        std::exit(70);
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
