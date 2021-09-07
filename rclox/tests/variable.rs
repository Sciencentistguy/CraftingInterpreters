use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn test_variable_block() -> Result<()> {
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
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["a", "a b", "a c", "a b d"]);
    Ok(())
}

#[test]
fn test_variable_duplicate_local() -> Result<()> {
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
fn test_variable_nested_block() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
        }
    }"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["outer"]);
    Ok(())
}

#[test]
fn test_variable_redeclare_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "1";
    var a;
    print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["nil"]);
    Ok(())
}

#[test]
fn test_variable_redefine_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "1";
    var a = "2";
    print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["2"]);
    Ok(())
}

#[test]
fn test_variable_reuse_in_different_blocks() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "first";
        print a;
    }

    {
        var a = "second";
        print a;
    }"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["first", "second"]);
    Ok(())
}

#[test]
fn test_variable_shadow_and_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
            var a = "inner";
            print a;
        }
    }"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["outer", "inner"]);
    Ok(())
}

#[test]
fn test_variable_shadow_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "global";
    {
        var a = "shadow";
        print a;
    }
    print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["shadow", "global"]);
    Ok(())
}

#[test]
fn test_variable_shadow_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var a = "local";
        {
            var a = "shadow";
            print a;
        }
        print a;
    }"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["shadow", "local"]);
    Ok(())
}

#[test]
fn test_variable_undefined_global() -> Result<()> {
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
fn test_variable_undefined_local() -> Result<()> {
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
fn test_variable_uninitialised() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a;
    print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["nil"]);
    Ok(())
}

#[test]
fn test_variable_unreached_undefined() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"if (false) print not_defined;
    print "ok";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}

#[test]
fn test_variable_keyword_as_ident() -> Result<()> {
    const PROGRAMS: [(&str, &str); 3] = [
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
    for (program, error_message) in PROGRAMS.iter() {
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
fn test_variable_global_in_initialiser() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "value";
    var a = a;
    print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["value"]);
    Ok(())
}

#[test]
fn test_variable_local_in_initialiser() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "outer";
    {
        var a = a;
    }"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at 'a': Cannot read local variable in its own initialiser.",
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
