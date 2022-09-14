use crate::virtual_machine::VirtualMachine;

use super::Result;

#[test]
fn empty() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        {} // By itself.

        // In a statement.
        if (true) {}
        if (false) {} else {}

        print "ok";
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}

#[test]
fn scope() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "outer";
        {
          var a = "inner";
          print a; //
        }
        print a;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["inner", "outer"]);
    Ok(())
}
