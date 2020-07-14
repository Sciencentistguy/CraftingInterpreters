#include "common.h"

#include "chunk.h"
#include "opcode.h"
int main() {
    Chunk chunk;
    auto c = chunk.addConstant(1.5);
    chunk.write(OpCode::Constant, 1);
    chunk.write(c, 1);
    chunk.write(OpCode::Return, 1);
    chunk.disassemble("test");
}
