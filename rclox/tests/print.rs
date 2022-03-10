use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn number_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 5;";
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["5"]);
    Ok(())
}

#[test]
fn string_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "Hello World";"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["Hello World"]);
    Ok(())
}

#[test]
fn nil() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print nil;";
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["nil"]);
    Ok(())
}

#[test]
fn bool() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "
    print true; 
    print false;
    ";
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["true", "false"]);
    Ok(())
}

#[test]
fn no_arg() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print ;"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at ';': Expected expression.",
        "'print' with no argument is ill-formed and should fail."
    );
    Ok(())
}
