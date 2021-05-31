use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn number_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 5;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["5"]);
    Ok(())
}

#[test]
fn string_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "Hello World";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["Hello World"]);
    Ok(())
}

#[test]
fn nil() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print nil;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["nil"]);
    Ok(())
}

#[test]
fn bool() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "
    print true; 
    print false;
    ";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["true", "false"]);
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
