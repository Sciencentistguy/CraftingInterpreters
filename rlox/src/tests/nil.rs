use crate::virtual_machine::VirtualMachine;

use super::Result;

#[test]
fn literal() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"print nil;"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["Nil"]);
    Ok(())
}
