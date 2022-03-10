use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn scope() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"{
        var i = "before";

        // New variable is in inner scope.
        for (var i = 0; i < 1; i = i + 1) {
            print i; // expect: 0

            // Loop body is in second inner scope.
            var i = -1;
            print i; // expect: -1
        }
    }

    {
        // New variable shadows outer variable.
        for (var i = 0; i > 0; i = i + 1) {}

        // Goes out of scope after loop.
        var i = "after";
        print i; // expect: after

        // Can reuse an existing variable.
        for (i = 0; i < 1; i = i + 1) {
            print i; // expect: 0
        }
    }"#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["0", "-1", "after", "0"]);
    Ok(())
}

#[test]
fn syntax() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        // Single-expression body.
        for (var c = 0; c < 3;) print c = c + 1;
        // expect: 1
        // expect: 2
        // expect: 3

        // Block body.
        for (var a = 0; a < 3; a = a + 1) {
          print a;
        }
        // expect: 0
        // expect: 1
        // expect: 2

        // No variable.
        var i = 0;
        for (; i < 2; i = i + 1) print i;
        // expect: 0
        // expect: 1

        // No increment.
        for (var i = 0; i < 2;) {
          print i;
          i = i + 1;
        }
        // expect: 0
        // expect: 1

        // Statement bodies.
        for (; false;) if (true) 1; else 2;
        for (; false;) while (true) 1;
        for (; false;) for (;;) 1;
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["1", "2", "3", "0", "1", "2", "0", "1", "0", "1"]);
    Ok(())
}

#[test]
#[ignore = "Not yet implemented"]
fn closure_in_body() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        var f1;
        var f2;
        var f3;

        for (var i = 1; i < 4; i = i + 1) {
          var j = i;
          fun f() {
            print i;
            print j;
          }

          if (j == 1) f1 = f;
          else if (j == 2) f2 = f;
          else f3 = f;
        }

        f1(); // expect: 4
              // expect: 1
        f2(); // expect: 4
              // expect: 2
        f3(); // expect: 4
              // expect: 3
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["4", "1", "4", "2", "4", "3"]);
    Ok(())
}

#[test]
fn fun_in_body() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
    // [line 2] Error at 'fun': Expect expression.
    for (;;) fun foo() {}
    "#;
    check_error_msg!(
        vm.interpret(PROGRAM),
        "<Compiler> [Line 2] Error at \'fun\': Expected expression.",
        "'function declaration' is not an expression."
    );
    Ok(())
}

#[test]
#[ignore = "Not yet implemented"]
fn return_closure() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        fun f() {
          for (;;) {
            var i = "i";
            fun g() { print i; }
            return g;
          }
        }

        var h = f();
        h(); // expect: i
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["i"]);
    Ok(())
}

#[test]
#[ignore = "Not yet implemented"]
fn return_inside() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"
        fun f() {
          for (;;) {
            var i = "i";
            return i;
          }
        }

        print f();
        // expect: i
    "#;
    vm.interpret(PROGRAM)?;
    assert_eq!(vm.print_log, &["i"]);
    Ok(())
}
