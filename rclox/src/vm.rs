use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::value::Value;

pub struct VM {
    chunk: Chunk,
    program_counter: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            program_counter: 0,
            stack: Vec::with_capacity(256),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), Box<dyn std::error::Error>> {
        self.chunk = chunk;
        self.program_counter = 0;
        self.run()
    }

    fn read_byte(&mut self) -> u8 {
        self.program_counter += 1;
        self.chunk.code[self.program_counter - 1]
    }

    #[inline]
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("Attempted to pop from an empty stack")
    }

    fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(&self.chunk, self.program_counter, false);

            let instruction = self.read_byte();
            match OpCode::fromu8(instruction) {
                Ok(x) => match x {
                    OpCode::Return => {
                        println!("{}", self.pop());
                        return Ok(());
                    }
                    OpCode::Constant => {
                        let constant = {
                            let index = self.read_byte() as usize;
                            self.chunk.constants[index].clone()
                        };
                        self.push(constant);
                    }
                    OpCode::Negate => {
                        let val = match self.pop() {
                            Value::Number(x) => Value::Number(-x),
                        };
                        self.push(val);
                    }
                    OpCode::Add => {
                        let b = self.pop();
                        let a = self.pop();
                        self.push(a + b);
                    }
                    OpCode::Subtract => {
                        let b = self.pop();
                        let a = self.pop();
                        self.push(a - b);
                    }
                    OpCode::Multiply => {
                        let b = self.pop();
                        let a = self.pop();
                        self.push(a * b);
                    }
                    OpCode::Divide=> {
                        let b = self.pop();
                        let a = self.pop();
                        self.push(a / b);
                    }
                },
                Err(_) => {
                    return Err(
                        format!("Attempted to run invalid instruction '{}'", instruction).into(),
                    )
                }
            }
        }
    }
}
