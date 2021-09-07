use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn last_line() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print "ok";
        //comment
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}

#[test]
fn only_line() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "//comment\n";
    let printed = vm.interpret(PROGRAM)?;
    assert!(printed.is_empty());
    Ok(())
}

#[test]
fn only() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "//comment";
    let printed = vm.interpret(PROGRAM)?;
    assert!(printed.is_empty());
    Ok(())
}

#[test]
fn unicode() -> Result<()> {
    let mut vm = VM::new();
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
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}
