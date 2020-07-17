#include "parser.h"

Precedence operator+(Precedence lhs, int rhs) {
    return static_cast<Precedence>(static_cast<int>(lhs) + rhs);
}
ParseRule::ParseRule(void (Compiler::*prefix)(), void (Compiler::*infix)(), Precedence precedence) : prefix(prefix), infix(infix), precedence(precedence) {
}
