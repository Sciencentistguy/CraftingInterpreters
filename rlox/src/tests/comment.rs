use crate::virtual_machine::VirtualMachine;

use super::Result;

#[test]
fn last_line() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        print "ok";
        //comment
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}

#[test]
fn only_line() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "//comment\n";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert!(vm.print_log.is_empty());
    Ok(())
}

#[test]
fn only() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "//comment";

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert!(vm.print_log.is_empty());
    Ok(())
}

#[test]
fn unicode() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        // Unicode characters are allowed in comments.
        //
        // Latin 1 Supplement: £§¶ÜÞ
        // Latin Extended-A: ĐĦŋœ
        // Latin Extended-B: ƂƢƩǁ
        // Other stuff: ឃᢆ᯽₪ℜ↩⊗┺░
        // Emoji: ☃☺♣

        print "ok";
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}
