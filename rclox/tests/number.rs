use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn literals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "
        print 123;
        print 987654;
        print 0;
        print -0;
        print 123.456;
        print -0.001;
    ";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["123", "987654", "0", "-0", "123.456", "-0.001"]);
    Ok(())
}

#[test]
fn decimal_point_at_eof() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "123.";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at '.': Expected ';' after expression.",
        "A decimal point at the end of the file is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn leading_dot() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = ".123";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at '.': Expected expression.",
        "A with a leading decimal is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn trailing_dot() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "123.;";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at '.': Expected ';' after expression.",
        "A decimal point at the end of the file is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn nan_equality() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print (0/0) == 0;
        print (0/0) != 1;
        print (0/0) == (0/0);
        print (0/0) != (0/0);
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "true", "false", "true"]);
    Ok(())
}
