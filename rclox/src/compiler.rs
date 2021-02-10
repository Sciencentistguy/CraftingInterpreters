pub struct Compiler {
    source: String
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler {
            source
        }
    }
    pub fn compile(&self) {}
}
