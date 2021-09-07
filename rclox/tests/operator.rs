use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn add() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print 123 + 456;
        print "str" + "ing";
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["579", "string"]);
    Ok(())
}

#[test]
fn add_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 7] = [
        "true + nil;",
        "true + 123;",
        r#"true + "s";"#,
        "nil + nil;",
        "1 + nil;",
        r#""s" + nil;"#,
        "true + true;",
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to + must be either both numbers or both strings",
            "Adding two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn divide() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print 8 / 2;
        print 12.34 / 12.34;
        print 5.5 / 2.2;
        print 5 / 0;
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["4", "1", "2.5", "inf"]);
    Ok(())
}

#[test]
fn divide_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true / nil;",
        "true / 123;",
        r#"true / "s";"#,
        "nil / nil;",
        "1 / nil;",
        r#""s" / nil;"#,
        "true / true;",
        r#""s" / "s";"#,
        r#"1 / "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to / must be two numbers",
            "Dividing two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn equals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print nil == nil;
        print true == true;
        print true == false;
        print 1 == 1;
        print 1 == 2;
        print "str" == "str";
        print "str" == "ing";
        print nil == false;
        print false == 0;
        print 0 == "0";
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["true", "true", "false", "true", "false", "true", "false", "false", "false", "false"]
    );
    Ok(())
}

#[test]
fn not_equals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print nil != nil; // expect: false

        print true != true; // expect: false
        print true != false; // expect: true

        print 1 != 1; // expect: false
        print 1 != 2; // expect: true

        print "str" != "str"; // expect: false
        print "str" != "ing"; // expect: true

        print nil != false; // expect: true
        print false != 0; // expect: true
        print 0 != "0"; // expect: true
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["false", "false", "true", "false", "true", "false", "true", "true", "true", "true"]
    );
    Ok(())
}

#[test]
fn multiply() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print 5 * 3;
        print 12.34 * 0.3;
        print 2 * 0;
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["15", "3.702", "0"]);
    Ok(())
}

#[test]
fn multiply_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true * nil;",
        "true * 123;",
        r#"true * "s";"#,
        "nil * nil;",
        "1 * nil;",
        r#""s" * nil;"#,
        "true * true;",
        r#""s" * "s";"#,
        r#"1 * "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to * must be two numbers",
            "Multiplying two values together of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn subtract() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print 4 - 3;
        print 12.34 - 0.3;
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["1", "12.04"]);
    Ok(())
}

#[test]
fn subtract_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true - nil;",
        "true - 123;",
        r#"true - "s";"#,
        "nil - nil;",
        "1 - nil;",
        r#""s" - nil;"#,
        "true - true;",
        r#""s" - "s";"#,
        r#"1 - "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to - must be two numbers",
            "Subtracting two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn less_than_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true < nil;",
        "true < 123;",
        r#"true < "s";"#,
        "nil < nil;",
        "1 < nil;",
        r#""s" < nil;"#,
        "true < true;",
        r#""s" < "s";"#,
        r#"1 < "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to < must be two numbers",
            "Comparing two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn greater_than_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 9] = [
        "true > nil;",
        "true > 123;",
        r#"true > "s";"#,
        "nil > nil;",
        "1 > nil;",
        r#""s" > nil;"#,
        "true > true;",
        r#""s" > "s";"#,
        r#"1 > "s";"#,
    ];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operands to > must be two numbers",
            "Comparing two values of incompatible types should be a runtime error."
        );
    }
    Ok(())
}

#[test]
fn comparison() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print 1 < 2;
        print 2 < 2;
        print 2 < 1;

        print 1 <= 2;
        print 2 <= 2;
        print 2 <= 1;

        print 1 > 2;
        print 2 > 2;
        print 2 > 1;

        print 1 >= 2;
        print 2 >= 2;
        print 2 >= 1;

        print 0 < -0;
        print -0 < 0;
        print 0 > -0;
        print -0 > 0;
        print 0 <= -0;
        print -0 <= 0;
        print 0 >= -0;
        print -0 >= 0;
    "#;
    let printed = vm.interpret(PROGRAM)?;

    let expected = &[
        "true", "false", "false", "true", "true", "false", "false", "false", "true", "false",
        "true", "true", "false", "false", "false", "false", "true", "true", "true", "true",
    ];

    assert_eq!(printed, expected);
    Ok(())
}

#[test]
fn negate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "
        print -(3);
        print --(3);
        print ---(3);
    ";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["-3", "3", "-3"]);
    Ok(())
}

#[test]
fn negate_invalid_operands() -> Result<()> {
    const PROGRAMS: [&str; 3] = [r#"-"s";"#, "-nil;", "-true;"];
    for program in PROGRAMS.iter() {
        let mut vm = VM::new();
        check_error_msg!(
            vm.interpret(program),
            "<Runtime> [Line 0] Error: Operand to unary negation must be a number.",
            "Negation of an invalid type is a runtime error."
        );
    }
    Ok(())
}

#[test]
#[ignore = "Requires classes"]
fn class() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        class Foo {}
        class Bar {}

        print Foo == Foo; // expect: true
        print Foo == Bar; // expect: false
        print Bar == Foo; // expect: false
        print Bar == Bar; // expect: true

        print Foo == "Foo"; // expect: false
        print Foo == nil;   // expect: false
        print Foo == 123;   // expect: false
        print Foo == true;  // expect: false
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["true", "false", "false", "true", "false", "false", "false", "false"]
    );
    Ok(())
}

#[test]
#[ignore = "Requires classes"]
fn equals_method() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        // Bound methods have identity equality.
        class Foo {
          method() {}
        }

        var foo = Foo();
        var fooMethod = foo.method;

        // Same bound method.
        print fooMethod == fooMethod; // expect: true

        // Different closurizations.
        print foo.method == foo.method; // expect: false
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["true", "false"]);
    Ok(())
}

#[test]
#[ignore = "Requires classes"]
fn not_class() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        class Bar {}
        print !Bar;      // expect: false
        print !Bar();    // expect: false
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "false"]);
    Ok(())
}

#[test]
fn not() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        print !true;     // expect: false
        print !false;    // expect: true
        print !!true;    // expect: true

        print !123;      // expect: false
        print !0;        // expect: false

        print !nil;     // expect: true

        print !"";       // expect: false
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["false", "true", "true", "false", "false", "true", "false"]
    );
    Ok(())
}

#[test]
fn not_fun() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        fun foo() {}
        print !foo;      // expect: false
    "#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false"]);
    Ok(())
}
