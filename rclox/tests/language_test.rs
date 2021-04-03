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

#[test]
fn test_operator_add_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 7] = [
        "true + nil;",
        "true + 123;",
        r#"true + "s";"#,
        "nil + nil;",
        "1 + nil;",
        r#""s" + nil;"#,
        "true + true;",
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to + must be either both numbers or both strings",
            "Adding two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_divide() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 8 / 2;
    print 12.34 / 12.34;
    print 5.5 / 2.2;
    print 5 / 0;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["4", "1", "2.5", "inf"]);
    Ok(())
}

#[test]
fn test_operator_divide_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true / nil;",
        "true / 123;",
        r#"true / "s";"#,
        "nil / nil;",
        "1 / nil;",
        r#""s" / nil;"#,
        "true / true;",
        r#""s" / "s";"#,
        r#"1 / "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to / must be two numbers",
            "Dividing two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_equals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print nil==nil;
    print true == true;
    print true == false;
    print 1 == 1;
    print 1 == 2;
    print "str" == "str";
    print "str" == "ing";
    print nil == false;
    print false == 0;
    print 0 == "0";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["true", "true", "false", "true", "false", "true", "false", "false", "false", "false"]
    );
    Ok(())
}

#[test]
fn test_operator_multiply() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 5 * 3;
    print 12.34 * 0.3;
    print 2 * 0;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["15", "3.702", "0"]);
    Ok(())
}

#[test]
fn test_operator_multiply_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true * nil;",
        "true * 123;",
        r#"true * "s";"#,
        "nil * nil;",
        "1 * nil;",
        r#""s" * nil;"#,
        "true * true;",
        r#""s" * "s";"#,
        r#"1 * "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to * must be two numbers",
            "Multiplying two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_subtract() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 4 - 3;
    print 12.34 - 0.3;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["1", "12.04"]);
    Ok(())
}

#[test]
fn test_operator_subtract_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true - nil;",
        "true - 123;",
        r#"true - "s";"#,
        "nil - nil;",
        "1 - nil;",
        r#""s" - nil;"#,
        "true - true;",
        r#""s" - "s";"#,
        r#"1 - "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to - must be two numbers",
            "Subtracting two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_less_than_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true < nil;",
        "true < 123;",
        r#"true < "s";"#,
        "nil < nil;",
        "1 < nil;",
        r#""s" < nil;"#,
        "true < true;",
        r#""s" < "s";"#,
        r#"1 < "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to < must be two numbers",
            "Comparing two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_greater_than_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true > nil;",
        "true > 123;",
        r#"true > "s";"#,
        "nil > nil;",
        "1 > nil;",
        r#""s" > nil;"#,
        "true > true;",
        r#""s" > "s";"#,
        r#"1 > "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to > must be two numbers",
            "Comparing two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn test_operator_comparison() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#" print 1 < 2;
    print 2 < 2;
    print 2 < 1;

    print 1 <= 2;
    print 2 <= 2;
    print 2 <= 1;

    print 1 > 2;
    print 2 > 2;
    print 2 > 1;

    print 1 >= 2;
    print 2 >= 2;
    print 2 >= 1;

    print 0 < -0;
    print -0 < 0;
    print 0 > -0;
    print -0 > 0;
    print 0 <= -0;
    print -0 <= 0;
    print 0 >= -0;
    print -0 >= 0;"#;
    let printed = vm.interpret(PROGRAM)?;

    let expected = &[
        "true", "false", "false", "true", "true", "false", "false", "false", "true", "false",
        "true", "true", "false", "false", "false", "false", "true", "true", "true", "true",
    ];

    assert_eq!(printed, expected);
    Ok(())
}

#[test]
fn test_operator_negate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print -(3);
    print --(3);
    print ---(3);";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["-3", "3", "-3"]);
    Ok(())
}
