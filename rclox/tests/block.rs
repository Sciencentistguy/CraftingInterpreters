use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn empty() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        {} // By itself.

        // In a statement.
        if (true) {}
        if (false) {} else {}

        print "ok";
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}

#[test]
fn scope() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "outer";
        {
          var a = "inner";
          print a; //
        }
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["inner", "outer"]);
    Ok(())
}
