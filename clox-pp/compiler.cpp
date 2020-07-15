#include "compiler.h"

#include <cstdio>
#include <iomanip>
#include <iostream>

#include "lexer.h"

void Compiler::compile(std::string source) {
    Lexer l{source};
    int line = -1;
    while (true) {
        Token token{l.scanToken()};
        if (token.getLine() != line) {
            std::cout << std::setfill('0') << std::setw(4) << token.getLine() << ' ';
            line = token.getLine();
        } else {
            std::cout << "   | ";
        }
        std::cout << std::setfill('0') << std::setw(2) << static_cast<int>(token.getType());
        std::cout << " '";
        std::cout.write(token.getStart(), token.getLength());
        std::cout << "'\n";

        if (token.getType() == TokenType::Eof) {
            break;
        }
    }
}
