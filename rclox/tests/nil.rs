use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print nil;"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["nil"]);
    Ok(())
}
