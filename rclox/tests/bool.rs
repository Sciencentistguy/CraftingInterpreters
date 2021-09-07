use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn equality() -> Result<()> {
    let mut vm = VM::new();
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
    let printed = vm.interpret(PROGRAM)?;

    let expected = &[
        "true", "false", "false", "true", "false", "false", "false", "false", "false", "false",
        "true", "true", "false", "true", "true", "true", "true", "true",
    ];

    assert_eq!(printed, expected);
    Ok(())
}

#[test]
fn not() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print !true;
        print !false;
        print !!true;
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "true", "true"]);
    Ok(())
}
