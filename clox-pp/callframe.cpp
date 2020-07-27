#include "callframe.h"

const Chunk& CallFrame::getChunk() const {
    return function->getChunk();
}
