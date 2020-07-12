#pragma once
#include <any>
#include <string>

class Token;
class RuntimeError;

void error(int line, const std::string& message);

void error(const Token& token, const std::string& message);

void report(int line, const std::string& where, const std::string& message);

std::string readFile(const std::string& filename);

void runFile(const std::string& path);

void runPrompt();

void run(const std::string& str);

void runtimeError(const RuntimeError& error);

std::string stringify(const std::any& a);
