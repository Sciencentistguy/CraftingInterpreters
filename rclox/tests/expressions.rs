use rclox::vm::VM;

#[path = "macros.rs"]
mod macros;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn evaluate() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = "print (5 - (3 - 1)) + -1;";
    let printed = vm.interpret(PROGRAM)?;
    assert_eq!(printed, &["2"]);
    Ok(())
}

#[test]
fn lexer() -> Result<()> {
    let mut vm = VM::new();
    const PROGRAM: &str = r#"(5 - (3 - 1)) + -1"#;
    let tokens = vm.lex(PROGRAM)?;
    use rclox::lexer::Token;
    use rclox::lexer::TokenType::*;

    #[rustfmt::skip]
    let expected = &[
        Token {kind: LeftParen, span: "(", line: 0},
        Token {kind: Number, span: "5", line: 0},
        Token {kind: Minus, span: "-", line: 0},
        Token {kind: LeftParen, span: "(", line: 0},
        Token {kind: Number, span: "3", line: 0},
        Token {kind: Minus, span: "-", line: 0},
        Token {kind: Number, span: "1", line: 0},
        Token {kind: RightParen, span: ")", line: 0},
        Token {kind: RightParen, span: ")", line: 0},
        Token {kind: Plus, span: "+", line: 0},
        Token {kind: Minus, span: "-", line: 0},
        Token {kind: Number, span: "1", line: 0},
        Token {kind: Eof, span: "", line: 0},
    ];

    println!("{:#?}", tokens);
    assert_eq!(tokens, expected);
    Ok(())
}
