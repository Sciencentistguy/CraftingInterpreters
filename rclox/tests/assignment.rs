use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn associativity() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "a";
        var b = "b";
        var c = "c";

        // Assignment is right-associative.
        a = b = c;
        print a;
        print b;
        print c;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["c", "c", "c"]);
    Ok(())
}

#[test]
fn global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
    var a = "before";
        print a;

        a = "after";
        print a;

        print a = "arg";
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["before", "after", "arg", "arg"]);
    Ok(())
}

#[test]
fn syntax() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "before";
        var c = a = "var";
        print a;
        print c;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["var", "var"]);
    Ok(())
}

#[test]
fn local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "before";
        print a; // expect: before

        a = "after";
        print a; // expect: after

        print a = "arg"; // expect: arg
        print a; // expect: arg
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["before", "after", "arg", "arg"]);
    Ok(())
}

#[test]
fn grouping() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "a";
        (a) = "value";
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not groups."
    );
    Ok(())
}

#[test]
fn infix_operator() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "a";
        var b = "b";
        a + b = "value";
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 3] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}

#[test]
fn prefix_operator() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "a";
        !a = "value";
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}

#[test]
fn undefined_name() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        not_defined = "hello";
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Runtime> [Line 1] Error: Undefined variable not_defined",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}
