# Crafting Interpreters

This repo contains multiple (incomplete) implementations of a Lox interpreter. Lox is a language designed by Bob Nystrom for his book [Crafting Interpreters](https://craftinginterpreters.com/).

## cpplox

`cpplox` is a tree-walk interpreter for Lox, written in C++. This is the only of the four that is complete.

It has very bad performance, due not only to the design, but also to the fact that it uses far too many `std::shared_ptr` and other inefficient patterns.

## clox-pp

`clox-pp` is a C++ implementation of a bytecode interpreter, like `clox`.

## rclox

`rclox` is an implementation of a code-generating interpreter (somewhat similar to `clox`), in Rust

It uses a Rust `enum` for the instructions, rather than bytecode. This has the effect that jumps can have distance `usize` (usually 64 bits), rather than 16 bits.

## hslox

This one is in Haskell. The parser is implemented using [Megaparsec](https://github.com/mrkkrp/megaparsec), a parser-combinators library.

This is also WIP.

---

Much of the code in `cpplox` and `clox-pp` (and to a lesser extent `rclox`) is very similar to the code from the book, which is available [here](https://github.com/munificent/craftinginterpreters) under the MIT licence.

My code here is available under the terms of the Mozilla Public Licence, version 2.0
