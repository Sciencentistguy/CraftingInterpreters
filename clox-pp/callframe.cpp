#include "callframe.h"

const Chunk& CallFrame::getChunk() const {
    return closure->getFunction().getChunk();
}
