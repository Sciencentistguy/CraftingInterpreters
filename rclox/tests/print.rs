use rclox::vm::VM;

#[path ="macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn test_print_number_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 5;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["5"]);
    Ok(())
}

#[test]
fn test_print_string_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "Hello World";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["Hello World"]);
    Ok(())
}

#[test]
fn test_print_no_arg() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print ;"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at ';': Expected expression.",
        "'print' with no argument is ill-formed and should fail."
    );
    Ok(())
}

