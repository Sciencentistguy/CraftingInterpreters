#include "function.h"

#include "chunk.h"

Function::Function() : arity{0}, chunk{std::make_shared<Chunk>()}, name{} {
}

int Function::getArity() const {
    return arity;
}

Chunk& Function::getChunk() {
    return *chunk;
}

const Chunk& Function::getChunk() const {
    return *chunk;
}

const std::string& Function::getName() const {
    return name;
}
