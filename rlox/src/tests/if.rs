use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;

#[test]
fn r#if() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Evaluate the 'then' expression if the condition is true.
        if (true) print "good"; // expect: good
        if (false) print "bad";

        // Allow block body.
        if (true) { print "block"; } // expect: block

        // Assignment in if condition.
        var a = false;
        if (a = true) print a; // expect: true
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["good", "block", "true"]);
    Ok(())
}

#[test]
fn r#else() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Evaluate the 'else' expression if the condition is false.
        if (true) print "good"; else print "bad"; // expect: good
        if (false) print "bad"; else print "good"; // expect: good

        // Allow block body.
        if (false) nil; else { print "block"; } // expect: block
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["good", "good", "block"]);
    Ok(())
}

#[test]
fn truth() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // False and nil are false.
        if (false) print "bad"; else print "false"; // expect: false
        if (nil) print "bad"; else print "nil"; // expect: nil

        // Everything else is true.
        if (true) print true; // expect: true
        if (0) print 0; // expect: 0
        if ("") print "empty"; // expect: empty
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["false", "nil", "true", "0", "empty"]);
    Ok(())
}

#[test]
fn dangling_else() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // A dangling else binds to the right-most if.
        if (true) if (false) print "bad"; else print "good"; // expect: good
        if (false) if (true) print "bad"; else print "bad";
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["good"]);
    Ok(())
}

#[test]
fn var_in_then() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // [line 2] Error at 'var': Expect expression.
        if (true) var foo;
    "#;

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 2]: Expected expression",
        "'Then' must be an expression"
    );
    Ok(())
}

#[test]
fn var_in_else() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // [line 2] Error at 'var': Expect expression.
        if (true) "ok"; else var foo;
    "#;

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 2]: Expected expression",
        "'Then' must be an expression"
    );
    Ok(())
}
