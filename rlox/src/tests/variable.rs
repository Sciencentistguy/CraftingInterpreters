use crate::{check_error_msg, virtual_machine::VirtualMachine};

use super::Result;

#[test]
fn block() -> Result<()> {
    let mut vm = VirtualMachine::new();
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

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["a", "a b", "a c", "a b d"]);
    Ok(())
}

#[test]
fn duplicate_local() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"{
        var a = "value";
        var a = "other";
    }"#;

    check_error_msg!(
    vm.reset(PROGRAM, 0),
        "Syntax error [Line 2]: Already a variable with this name in this scope",
        "Defining a local variable with a name that is already in use is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn nested_block() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
        }
    }"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["outer"]);
    Ok(())
}

#[test]
fn redeclare_global() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "1";
        var a;
        print a;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["Nil"]);
    Ok(())
}

#[test]
fn redefine_global() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "1";
        var a = "2";
        print a;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["2"]);
    Ok(())
}

#[test]
fn reuse_in_different_blocks() -> Result<()> {
    let mut vm = VirtualMachine::new();
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

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["first", "second"]);
    Ok(())
}

#[test]
fn shadow_and_local() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"{
        var a = "outer";
        {
            print a;
            var a = "inner";
            print a;
        }
    }"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["outer", "inner"]);
    Ok(())
}

#[test]
fn shadow_global() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "global";
        {
            var a = "shadow";
            print a;
        }
        print a;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["shadow", "global"]);
    Ok(())
}

#[test]
fn shadow_local() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"{
        var a = "local";
        {
            var a = "shadow";
            print a;
        }
        print a;
    }"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["shadow", "local"]);
    Ok(())
}

#[test]
fn undefined_global() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = "print not_defined;";

    vm.reset(PROGRAM, 0)?;

    check_error_msg!(
        vm.start(),
        "Runtime error: Variable 'not_defined' not defined",
        "Referencing a global variable that has not been defined is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn undefined_local() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"{
        print not_defined;
    }"#;

    vm.reset(PROGRAM, 0)?;

    check_error_msg!(
        vm.start(),
        "Runtime error: Variable 'not_defined' not defined",
        "Referencing a local variable that has not been defined is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn uninitialised() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"var a;
    print a;"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["Nil"]);
    Ok(())
}

#[test]
#[ignore = "NYI"]
fn unreached_undefined() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"if (false) print not_defined;
    print "ok";"#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["ok"]);
    Ok(())
}

#[test]
fn keyword_as_ident() -> Result<()> {
    const PROGRAMS_WITH_ERRORS: [(&str, &str); 3] = [
        (
            r#"var false = "value";"#,
            "Syntax error [Line 0]: Expected a variable name after `var`",
        ),
        (
            r#"var this = "value";"#,
            "Syntax error [Line 0]: Expected a variable name after `var`",
        ),
        (
            r#"var class = "value";"#,
            "Syntax error [Line 0]: Expected a variable name after `var`",
        ),
    ];
    for (program, error_message) in PROGRAMS_WITH_ERRORS {
        let mut vm = VirtualMachine::new();

        check_error_msg!(
            vm.reset(program, 0),
            *error_message,
            "Attempting to use a reserved word as an identifier is ill-formed and should fail."
        );
    }
    Ok(())
}

#[test]
fn global_in_initialiser() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "value";
        var a = a;
        print a;
    "#;

    vm.reset(PROGRAM, 0)?;
    vm.start()?;

    assert_eq!(vm.print_log, &["value"]);
    Ok(())
}

#[test]
fn local_in_initialiser() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        var a = "outer";
        {
            var a = a;
        }
    "#;

    check_error_msg!(
        vm.reset(PROGRAM, 0),
        "Syntax error [Line 3]: Cannot read local variable in its own initializer",
        "Attempting to read a local variable in its own initialiser is ill-formed and should fail."
    );
    Ok(())
}

#[test]
#[ignore = "NYI"]
fn collide_with_parameter() -> Result<()> {
    let mut vm = VirtualMachine::new();
    const PROGRAM: &str = r#"
        fun foo(a) {
            var a;
        }
    "#;

    vm.reset(PROGRAM, 0)?;

    check_error_msg!(
        vm.start(),
        "<Compiler> [Line 2] Error at 'a': There is already a variable with name a in this scope",
        "Variable name cannot shadow parameter."
    );
    Ok(())
}
