use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;

#[test]
fn literals() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "
        print 123;
        print 987654;
        print 0;
        print -0;
        print 123.456;
        print -0.001;
    ";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(
        vm.print_log,
        &["123", "987654", "0", "-0", "123.456", "-0.001"]
    );
    Ok(())
}

#[test]
fn decimal_point_at_eof() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "123.";

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 0]: Expected ';' after value",
        "A decimal point at the end of the file is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn leading_dot() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = ".123";

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 0]: Expected expression",
        "A with a leading decimal is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn trailing_dot() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "123.;";

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 0]: Expected ';' after value",
        "A decimal point at the end of the file is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn nan_equality() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        print (0/0) == 0;
        print (0/0) != 1;
        print (0/0) == (0/0);
        print (0/0) != (0/0);
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["false", "true", "false", "true"]);
    Ok(())
}
