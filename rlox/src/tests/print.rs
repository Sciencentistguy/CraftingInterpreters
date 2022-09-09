use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;
#[test]
fn number_literal() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "print 5;";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["5"]);
    Ok(())
}

#[test]
fn string_literal() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"print "Hello World";"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["Hello World"]);
    Ok(())
}

#[test]
fn nil() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "print nil;";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["Nil"]);
    Ok(())
}

#[test]
fn bool() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "
    print true; 
    print false;
    ";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["true", "false"]);
    Ok(())
}

#[test]
fn no_arg() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"print ;"#;

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 0]: Expected expression",
        "'print' with no argument is ill-formed and should fail."
    );
    Ok(())
}
