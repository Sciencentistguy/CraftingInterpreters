cmake_minimum_required(VERSION 3.16)
project(clox_pp)

set(CMAKE_CXX_STANDARD 20)

add_executable(clox_pp main.cpp chunk.cpp chunk.h common.h opcode.h opcode.cpp)

target_compile_options(clox_pp PRIVATE -Wall -Wextra -pedantic -Wno-unused-parameter)
