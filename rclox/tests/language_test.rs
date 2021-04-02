use rclox::vm::VM;
use rclox::Result;

/// Assert that the compiler not only errors, but produces the correct error.
#[macro_export]
macro_rules! check_error_msg {
    ($invocation:expr, $expected:expr, $panic_msg: expr) => {{
        if let Err(e) = $invocation {
            println!("{}", e);
            assert_eq!(e.to_string(), $expected, "Incorrect error message.");
        } else {
            panic!($panic_msg);
        }
    }};
}

// Print

#[test]
fn test_print_number_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 5;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(*printed.first().unwrap(), 5.to_string());
    Ok(())
}

#[test]
fn test_print_string_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "Hello World";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(*printed.first().unwrap(), "Hello World".to_string());
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

// Number literals

#[test]
fn test_number_literals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 123;
    print 987654;
    print 0;
    print -0;
    print 123.456;
    print -0.001;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["123", "987654", "0", "0", "123.456", "-0.001"]);
    Ok(())
}

#[test]
fn test_number_decimal_at_eof() -> Result<()> {
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
fn test_number_leading_dot() -> Result<()> {
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
fn test_number_nan_equality() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print (0/0) == 0;
print (0/0) != 1;
print (0/0) == (0/0);
print (0/0) != (0/0);"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "true", "false", "true"]);
    Ok(())
}

// Operators

#[test]
fn test_operator_add() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 123 + 456;
    print "str" + "ing";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["579", "string"]);
    Ok(())
}
