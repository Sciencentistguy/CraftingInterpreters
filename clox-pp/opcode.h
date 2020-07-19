#pragma once

enum class OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    Get_global,
    Define_global,
    Set_global,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Return,
};
