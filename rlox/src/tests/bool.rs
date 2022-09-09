use crate::virtual_machine::VirtualMachine;

use super::Result;

#[test]
fn equality() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        print true == true;
        print true == false;
        print false == true;
        print false == false;

        // Not equal to other types.
        print true == 1;
        print false == 0;
        print true == "true";
        print false == "false";
        print false == "";

        print true != true;
        print true != false;
        print false != true;
        print false != false;

        // Not equal to other types.
        print true != 1;
        print false != 0;
        print true != "true"; 
        print false != "false";
        print false != "";
    "#;
    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    let expected = &[
        "true", "false", "false", "true", "false", "false", "false", "false", "false", "false",
        "true", "true", "false", "true", "true", "true", "true", "true",
    ];

    assert_eq!(vm.print_log, expected);
    Ok(())
}

#[test]
fn not() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        print !true;
        print !false;
        print !!true;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["false", "true", "true"]);
    Ok(())
}
