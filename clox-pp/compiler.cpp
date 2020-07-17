#include "compiler.h"

#include <iomanip>
#include <iostream>

#include "lexer.h"
#include "token.h"

Compiler::Compiler(const std::string& source) : source{source} {
}

void Compiler::compile() {
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
        if (token.getType() == TokenType::Error) {
            std::cout << " Error:";
        }
        std::cout << " '";
        std::cout.write(token.getStart(), token.getLength());
        std::cout << "'\n";

        if (token.getType() == TokenType::Eof) {
            break;
        }
    }
}
