#pragma once

#include <memory>
#include <string>
#include <vector>

#include "value.h"

class Chunk;
enum class FunctionType {
    Function,
    Script,
};

class Function {
 public:
    int arity;
    std::shared_ptr<Chunk> chunk;
    std::string name;

    Function();

    [[nodiscard]] int getArity() const;
    [[nodiscard]] const Chunk& getChunk() const;
    [[nodiscard]] Chunk& getChunk();
    [[nodiscard]] const std::string& getName() const;
//    virtual ~Function();
};

