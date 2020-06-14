cmake_minimum_required(VERSION 3.17)
project(cpplox)

set(CMAKE_CXX_STANDARD 20)

include_directories(.)

add_executable(cpplox
        main.cpp main.h lexer.cpp lexer.h token.cpp token.h token_type.h expression.cpp expression.h)
