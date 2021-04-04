use rclox::vm::VM;
use rclox::Result;

/// Assert that the compiler not only errors, but produces the correct error.
#[macro_export]
macro_rules! check_error_msg {
    ($invocation:expr, $expected:expr, $panic_msg: expr) => {{
        if let Err(e) = $invocation {
            println!("{}", e);
            assert_eq!(e.to_string(), $expected, "Incorrect error message.");
        } else {
            panic!($panic_msg);
        }
    }};
}

// Print

#[test]
fn test_print_number_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 5;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["5"]);
    Ok(())
}

#[test]
fn test_print_string_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "Hello World";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["Hello World"]);
    Ok(())
}

#[test]
fn test_print_no_arg() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print ;"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at ';': Expected expression.",
        "'print' with no argument is ill-formed and should fail."
    );
    Ok(())
}

// Number literals

#[test]
fn test_number_literals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print 123;
    print 987654;
    print 0;
    print -0;
    print 123.456;
    print -0.001;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["123", "987654", "0", "0", "123.456", "-0.001"]);
    Ok(())
}

#[test]
fn test_number_decimal_at_eof() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "123.";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at '.': Expected ';' after expression.",
        "A decimal point at the end of the file is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn test_number_leading_dot() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = ".123";
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 0] Error at '.': Expected expression.",
        "A with a leading decimal is ill-formed and should fail."
    );
    Ok(())
}

#[test]
fn test_number_nan_equality() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print (0/0) == 0;
print (0/0) != 1;
print (0/0) == (0/0);
print (0/0) != (0/0);"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "true", "false", "true"]);
    Ok(())
}

// Operators

#[test]
fn test_operator_add() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 123 + 456;
    print "str" + "ing";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["579", "string"]);
    Ok(())
}

#[test]
fn test_operator_add_invalid_operands() -> Result<()> {
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
fn test_operator_divide() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 8 / 2;
    print 12.34 / 12.34;
    print 5.5 / 2.2;
    print 5 / 0;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["4", "1", "2.5", "inf"]);
    Ok(())
}

#[test]
fn test_operator_divide_invalid_operands() -> Result<()> {
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
fn test_operator_equals() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print nil==nil;
    print true == true;
    print true == false;
    print 1 == 1;
    print 1 == 2;
    print "str" == "str";
    print "str" == "ing";
    print nil == false;
    print false == 0;
    print 0 == "0";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(
        printed,
        &["true", "true", "false", "true", "false", "true", "false", "false", "false", "false"]
    );
    Ok(())
}

#[test]
fn test_operator_multiply() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 5 * 3;
    print 12.34 * 0.3;
    print 2 * 0;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["15", "3.702", "0"]);
    Ok(())
}

#[test]
fn test_operator_multiply_invalid_operands() -> Result<()> {
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
fn test_operator_subtract() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print 4 - 3;
    print 12.34 - 0.3;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["1", "12.04"]);
    Ok(())
}

#[test]
fn test_operator_subtract_invalid_operands() -> Result<()> {
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
fn test_operator_less_than_invalid_operands() -> Result<()> {
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
fn test_operator_greater_than_invalid_operands() -> Result<()> {
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
fn test_operator_comparison() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#" print 1 < 2;
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
    print -0 >= 0;"#;
    let printed = vm.interpret(PROGRAM)?;

    let expected = &[
        "true", "false", "false", "true", "true", "false", "false", "false", "true", "false",
        "true", "true", "false", "false", "false", "false", "true", "true", "true", "true",
    ];

    assert_eq!(printed, expected);
    Ok(())
}

#[test]
fn test_operator_negate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print -(3);
    print --(3);
    print ---(3);";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["-3", "3", "-3"]);
    Ok(())
}

//Variables

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
            "<Compiler> [Line 0] Error at 'false': Expected a varaible name.",
        ),
        (
            r#"var this = "value";"#,
            "<Compiler> [Line 0] Error at 'this': Expected a varaible name.",
        ),
        (
            r#"var class = "value";"#,
            "<Compiler> [Line 0] Error at 'class': Expected a varaible name.",
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

// Nil

#[test]
fn test_nil_literal() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print nil;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["nil"]);
    Ok(())
}

// Comments

#[test]
fn test_comment_last_line() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print "ok";
    //comment"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}

#[test]
fn test_comment_only_line() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "//comment\n";
    let printed = vm.interpret(PROGRAM)?;
    assert!(printed.is_empty());
    Ok(())
}

#[test]
fn test_comment_only() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "//comment";
    let printed = vm.interpret(PROGRAM)?;
    assert!(printed.is_empty());
    Ok(())
}

#[test]
fn test_comment_unicode() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"// Unicode characters are allowed in comments.
//
// Latin 1 Supplement: £§¶ÜÞ
// Latin Extended-A: ĐĦŋœ
// Latin Extended-B: ƂƢƩǁ
// Other stuff: ឃᢆ᯽₪ℜ↩⊗┺░
// Emoji: ☃☺♣

print "ok";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}

#[test]
fn test_assignment_associativity() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "a";
var b = "b";
var c = "c";

// Assignment is right-associative.
a = b = c;
print a;
print b;
print c;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["c", "c", "c"]);
    Ok(())
}

#[test]
fn test_assignment_global() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "before";
print a;

a = "after";
print a;

print a = "arg";
print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["before", "after", "arg", "arg"]);
    Ok(())
}

#[test]
fn test_assignment_syntax() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "before";
var c = a = "var";
print a;
print c;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["var", "var"]);
    Ok(())
}

#[test]
fn test_assignment_local() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
  var a = "before";
  print a; // expect: before

  a = "after";
  print a; // expect: after

  print a = "arg"; // expect: arg
  print a; // expect: arg
}"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["before", "after", "arg", "arg"]);
    Ok(())
}

#[test]
fn test_assignment_grouping() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "a";
    (a) = "value";"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 1] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not groups."
    );
    Ok(())
}

#[test]
fn test_assignment_infix_operator() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "a";
    var b = "b";
    a + b = "value";"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}

#[test]
fn test_assignment_prefix_operator() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "a";
    !a = "value";"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 1] Error at '=': Invalid assignment target",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}

#[test]
fn test_assignment_undefined_name() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"not_defined = "hello";"#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Runtime> [Line 0] Error: Undefined variable not_defined",
        "Assignemnt targets must be raw variable names, not expressions."
    );
    Ok(())
}

#[test]
fn test_block_empty() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{} // By itself.

// In a statement.
if (true) {}
if (false) {} else {}

print "ok";"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["ok"]);
    Ok(())
}

#[test]
fn test_block_scope() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"var a = "outer";
{
  var a = "inner";
  print a; //
}
print a;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["inner", "outer"]);
    Ok(())
}

#[test]
fn test_bool_equality() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print true == true;
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
fn test_bool_not() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"print !true;
    print !false;
    print !!true;"#;
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["false", "true", "true"]);
    Ok(())
}

#[test]
fn test_expression_evaluate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print (5 - (3 - 1)) + -1;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["2"]);
    Ok(())
}

#[test]
fn test_expression_lexer() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "(5 - (3 - 1)) + -1";
    let tokens = vm.lex(PROGRAM)?;
    use rclox::lexer::Token;
    use rclox::lexer::TokenType::*;
    #[rustfmt::skip]
    let expected = &[
        Token {kind: LeftParen, string: "(", line: 0},
        Token {kind: Number, string: "5", line: 0},
        Token {kind: Minus, string: "-", line: 0},
        Token {kind: LeftParen, string: "(", line: 0},
        Token {kind: Number, string: "3", line: 0},
        Token {kind: Minus, string: "-", line: 0},
        Token {kind: Number, string: "1", line: 0},
        Token {kind: RightParen, string: ")", line: 0},
        Token {kind: RightParen, string: ")", line: 0},
        Token {kind: Plus, string: "+", line: 0},
        Token {kind: Minus, string: "-", line: 0},
        Token {kind: Number, string: "1", line: 0},
        Token {kind: Eof, string: "", line: 0},
    ];
    println!("{:#?}", tokens);
    assert_eq!(tokens, expected);
    Ok(())
}
