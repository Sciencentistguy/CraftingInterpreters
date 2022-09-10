use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;

#[test]
fn and() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Note: These tests implicitly depend on ints being truthy.

        // Return the first non-true argument.
        print false and 1; // expect: false
        print true and 1; // expect: 1
        print 1 and 2 and false; // expect: false

        // Return the last argument if all are true.
        print 1 and true; // expect: true
        print 1 and 2 and 3; // expect: 3

        // Short-circuit at the first false argument.
        var a = "before";
        var b = "before";
        (a = true) and
            (b = false) and
            (a = "bad");
        print a; // expect: true
        print b; // expect: false
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(
        vm.print_log,
        &["false", "1", "false", "true", "3", "true", "false"]
    );
    Ok(())
}

#[test]
fn or() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Note: These tests implicitly depend on ints being truthy.

        // Return the first true argument.
        print 1 or true; // expect: 1
        print false or 1; // expect: 1
        print false or false or true; // expect: true

        // Return the last argument if all are false.
        print false or false; // expect: false
        print false or false or false; // expect: false

        // Short-circuit at the first true argument.
        var a = "before";
        var b = "before";
        (a = false) or
            (b = true) or
            (a = "bad");
        print a; // expect: false
        print b; // expect: true
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(
        vm.print_log,
        &["1", "1", "true", "false", "false", "false", "true"]
    );
    Ok(())
}

#[test]
fn and_truth() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // False and nil are false.
        print false and "bad"; // expect: false
        print nil and "bad"; // expect: nil

        // Everything else is true.
        print true and "ok"; // expect: ok
        print 0 and "ok"; // expect: ok
        print "" and "ok"; // expect: ok
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["false", "Nil", "ok", "ok", "ok"]);
    Ok(())
}

#[test]
fn or_truth() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // False and nil are false.
        print false or "ok"; // expect: ok
        print nil or "ok"; // expect: ok

        // Everything else is true.
        print true or "ok"; // expect: true
        print 0 or "ok"; // expect: 0
        print "s" or "ok"; // expect: s
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["ok", "ok", "true", "0", "s"]);
    Ok(())
}
