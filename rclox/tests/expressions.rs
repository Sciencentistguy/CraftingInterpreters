use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn evaluate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print (5 - (3 - 1)) + -1;";
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["2"]);
    Ok(())
}
