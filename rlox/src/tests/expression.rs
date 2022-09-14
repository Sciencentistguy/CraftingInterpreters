use crate::virtual_machine::VirtualMachine;

use super::Result;

#[test]
fn evaluate() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "print (5 - (3 - 1)) + -1;";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["2"]);
    Ok(())
}
