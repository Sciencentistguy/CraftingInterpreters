
use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;

#[test]
fn r#while() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Single-expression body.
        var c = 0;
        while (c < 3) print c = c + 1;
        // expect: 1
        // expect: 2
        // expect: 3

        // Block body.
        var a = 0;
        while (a < 3) {
          print a;
          a = a + 1;
        }
        // expect: 0
        // expect: 1
        // expect: 2

        // Statement bodies.
        while (false) if (true) 1; else 2;
        while (false) while (true) 1;
        // while (false) for (;;) 1;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["1", "2", "3", "0", "1", "2"]);
    Ok(())
}
