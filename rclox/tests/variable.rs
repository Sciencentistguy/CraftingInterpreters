use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn block() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "a";
        print a;

        var b = a + " b";
        print b;

        var c = a + " c";
        print c;

        var d = b + " d";
        print d; 
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["a", "a b", "a c", "a b d"]);
    Ok(())
}

#[test]
fn duplicate_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "value";
        var a = "other";
    }"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at 'a': There is already a variable with name a in this scope",
        "Defining a local variable with a name that is already in use is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn nested_block() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
        }
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["outer"]);
    Ok(())
}

#[test]
fn redeclare_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "1";
        var a;
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["nil"]);
    Ok(())
}

#[test]
fn redefine_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "1";
        var a = "2";
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["2"]);
    Ok(())
}

#[test]
fn reuse_in_different_blocks() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        {
            var a = "first";
            print a;
        }

        {
            var a = "second";
            print a;
        }
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["first", "second"]);
    Ok(())
}

#[test]
fn shadow_and_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
            var a = "inner";
            print a;
        }
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["outer", "inner"]);
    Ok(())
}

#[test]
fn shadow_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "global";
        {
            var a = "shadow";
            print a;
        }
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["shadow", "global"]);
    Ok(())
}

#[test]
fn shadow_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "local";
        {
            var a = "shadow";
            print a;
        }
        print a;
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["shadow", "local"]);
    Ok(())
}

#[test]
fn undefined_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print not_defined;";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Runtime> [Line 0] Error: Undefined variable 'not_defined'",
        "Referencing a global variable that has not been defined is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn undefined_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        print not_defined;
    }"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Runtime> [Line 1] Error: Undefined variable 'not_defined'",
        "Referencing a local variable that has not been defined is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn uninitialised() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a;
    print a;"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["nil"]);
    Ok(())
}

#[test]
fn unreached_undefined() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"if (false) print not_defined;
    print "ok";"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}

#[test]
fn keyword_as_ident() -> Result<()> {
    const PROGRAMS_WITH_ERRORS: [(&str, &str); 3] = [
        (
            r#"var false = "value";"#,
            "<Compiler> [Line 0] Error at 'false': Expected a variable name.",
        ),
        (
            r#"var this = "value";"#,
            "<Compiler> [Line 0] Error at 'this': Expected a variable name.",
        ),
        (
            r#"var class = "value";"#,
            "<Compiler> [Line 0] Error at 'class': Expected a variable name.",
        ),
    ];
    for (program, error_message) in PROGRAMS_WITH_ERRORS {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            *error_message,
            "Attempting to use a reserved word as an identifier is ill-formed and should fail."
        );
    }
    Ok(())
}

#[test]
fn global_in_initialiser() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "value";
        var a = a;
        print a;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["value"]);
    Ok(())
}

#[test]
fn local_in_initialiser() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var a = "outer";
        {
            var a = a;
        }
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 3] Error at 'a': Cannot read local variable in its own initialiser.",
        "Attempting to read a local variable in its own initialiser is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn collide_with_parameter() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        fun foo(a) {
            var a;
        }
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at 'a': There is already a variable with name a in this scope",
        "Variable name cannot shadow parameter."
    );
    Ok(())
}
