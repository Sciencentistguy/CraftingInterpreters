cmake_minimum_required(VERSION 3.16)
project(cpplox)


set(CMAKE_CXX_STANDARD 20)

include_directories(.)

add_executable(cpplox
        main.cpp main.h lexer.cpp lexer.h token.cpp token.h expression.cpp expression.h parser.cpp parser.h interpreter.cpp interpreter.h statement.cpp statement.h environment.cpp environment.h loxcallable.h loxfunction.cpp loxfunction.h return.cpp return.h resolver.cpp resolver.h loxclass.cpp loxclass.h loxinstance.cpp loxinstance.h runtimeerror.cpp runtimeerror.h)

target_compile_options(cpplox PRIVATE -Wall -Wextra -pedantic -Wno-unused-parameter)

install(TARGETS cpplox DESTINATION bin)
