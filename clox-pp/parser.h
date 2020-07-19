#pragma once

#include "token.h"

enum class Precedence {
    None,
    Assignment,  // =
    Or,          // or
    And,         // and
    Equality,    // == !=
    Comparison,  // < > <= >=
    Term,        // + -
    Factor,      // * /
    Unary,       // ! -
    Call,        // . ()
    Primary
};

Precedence operator+(Precedence lhs, int rhs);
class Compiler;

using ParseFn = void (Compiler::*)(bool canAssign);

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
    ParseRule(ParseFn prefix, ParseFn infix, Precedence precedence);
};

struct Parser {
    Token current{};
    Token previous{};
};