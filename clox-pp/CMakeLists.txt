cmake_minimum_required(VERSION 3.16)
project(clox_pp)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(fmt REQUIRED)
include_directories(${fmt_INCLUDE_DIRS})

add_executable(clox_pp main.cpp chunk.cpp chunk.h common.h opcode.h opcode.cpp virtualmachine.cpp virtualmachine.h compiler.cpp compiler.h lexer.cpp lexer.h token.cpp token.h parser.cpp parser.h value.h exception.cpp exception.h value.cpp function.cpp function.h callframe.cpp callframe.h)

target_link_libraries(clox_pp fmt::fmt)

target_compile_options(clox_pp
        PRIVATE
            -Wall
            -Wextra
            -pedantic
            -Wno-unused-parameter
            -Werror
        )
