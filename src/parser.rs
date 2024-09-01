use crate::ast::{
    Assign, Block, Component, Elif, Expr, FuncCall, FuncDef, If, Value, Pair, Program, Stmt,
    VarDef,
};

peg::parser! {
    pub grammar primo_parser() for str {
        pub rule program() -> Program
            = __ c:(component() ** __) __
                { c }
        pub rule component() -> Component
            = func_def()
        pub rule func_def() -> Component
            = func_ident() _ i:ident() "(" _ args:(ident() ** (_ "," _)) _ ")" _ b:block()
                { Component::FuncDef(FuncDef { name:i, args: args, block: b }) }
        // TODO: if elif elseの定義のリファクタリング
        pub rule if_stmt() -> Stmt
            = if_ident() _ e:expr() _ b:block() elif_b:elif_branches()? else_b:else_branch()?
                { Stmt::If(If { condition: e, block: b, elif: elif_b, else_block: else_b }) }
        rule elif_branches() -> Vec<Elif>
            = elif_branch()+
        rule elif_branch() -> Elif
            = __ elif_b:elif()
                { elif_b }
        rule else_branch() -> Block
            = __ else_b:r#else()
                { else_b }
        rule elif() -> Elif
            = elif_ident() _ e:expr() _ b:block()
                { Elif { condition: e, block: b } }
        rule r#else() -> Block
            = else_ident() _ b:block()
                { b }
        pub rule loop_stmt() -> Stmt
            = loop_ident() _ b:block()
                { Stmt::Loop(b) }
        pub rule block() -> Vec<Stmt>
            = "{" __ s:(statement() ** __ ) __? "}"
                { s }
        pub rule statement() -> Stmt
            = var_def()
            / if_stmt()
            / loop_stmt()
            / next()
            / r#break()
            / r#return()
            / print_stmt()
            / assign()
            / expr_stmt()
        pub rule print_stmt() -> Stmt
            = print_ident() _ e:(expr() ** (_ "," __)) _ stmt_end()
                { Stmt::Print(e) }
        pub rule expr_stmt() -> Stmt
            = e:expr() _ stmt_end()
                { Stmt::Expr(e) }
        pub rule var_def() -> Stmt
            = let_ident() _ i:ident() _ "=" _ e:expr() _ stmt_end()
                { Stmt::VarDef(VarDef { name: i, value: e }) }
        pub rule assign() -> Stmt
            = i:ident() _ "=" _ e:expr() _ stmt_end()
                { Stmt::Assign(Assign { name: i, value: e }) }
        pub rule next() -> Stmt
            = next_ident() _ stmt_end()
                { Stmt::Next }
        pub rule r#break() -> Stmt
            = break_ident() _ stmt_end()
                { Stmt::Break }
        pub rule r#return() -> Stmt
            = return_ident() _ e:expr()? _ stmt_end()
                { Stmt::Return(e) }
        pub rule func_call() -> FuncCall
            = i:ident() "(" args:(expr() ** (_ "," _)) ")"
                { FuncCall { name: i, args: args } }
        pub rule expr() -> Expr
            = precedence! {
                l:(@) _ or_ident() _ r:@
                    { Expr::Or(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                l:(@) _ and_ident() _ r:@
                    { Expr::And(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                not_ident() _ e:expr()
                    { Expr::Not(Box::new(e)) }
                --
                l:(@) _ "==" _ r:@
                    { Expr::Eq(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                l:(@) _ "!=" _ r:@
                    { Expr::NotEq(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                --
                l:(@) _ "+" _ r:@
                    { Expr::Add(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                l:(@) _ "-" _ r:@
                    { Expr::Sub(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                --
                l:(@) _ "*" _ r:@
                    { Expr::Mul(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                l:(@) _ "/" _ r:@
                    { Expr::Div(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                l:(@) _ "%" _ r:@
                    { Expr::Mod(Pair { lhs: Box::new(l), rhs: Box::new(r) }) }
                --
                l:literal()
                    { Expr::Literal(l) }
                f:func_call()
                    { Expr::FuncCall(f) }
                "(" _ e:expr() _ ")"
                    { Expr::Expr(Box::new(e)) }
                i:ident()
                    { Expr::Ident(i) }
            }
        pub rule literal() -> Value
            = string() / r#bool() / integer()
        rule string() -> Value
            = "\"" c:$([^'"']*) "\""
                {
                    Value::Str(c.to_string()
                        .replace("\\n", "\n")
                        .replace("\\r", "\r")
                        .replace("\\t", "\t")
                        .replace("\\\\", "\\")
                    )
                }
        rule integer() -> Value
            = n:$("-"? ['0'..='9']+)
                { Value::Int(n.parse().unwrap()) }
        rule r#bool() -> Value
            = "true"
                { Value::Bool(true) }
            / "false"
                { Value::Bool(false) }
        // FIXME: 予約済みのキーワードが許可されてる
        pub rule ident() -> String
            = reserved_words()
                {? Err("this word is reserved") }
            / i:$(['a'..='z'|'A'..='Z'|'_']['a'..='z'|'A'..='Z'|'0'..='9'|'_']*)
                {? Ok(i.to_string()) }
        rule reserved_words()
            = func_ident()
            / if_ident()
            / elif_ident()
            / else_ident()
            / loop_ident()
            / let_ident()
            / or_ident()
            / and_ident()
            / not_ident()
            / print_ident()
            / next_ident()
            / break_ident()
            / return_ident()
        rule func_ident()
            = "fn"
        rule if_ident()
            = "if"
        rule elif_ident()
            = "elif"
        rule else_ident()
            = "else"
        rule loop_ident()
            = "loop"
        rule let_ident()
            = "let"
        rule or_ident()
            = "or"
        rule and_ident()
            = "and"
        rule not_ident()
            = "not"
        rule print_ident()
            = "print"
        rule next_ident()
            = "next"
        rule break_ident()
            = "break"
        rule return_ident()
            = "return"
        rule stmt_end()
            = ";"
        rule _()
            = quiet!{ [' ' | '\t']* }
        rule __()
            = quiet!{ [' ' | '\t' | '\n' | '\r']* }
    }
}

#[cfg(test)]
mod tests {
    use super::primo_parser;
    use crate::ast::{Assign, Block, Component, Elif, Expr, FuncCall, FuncDef, If, Value, Pair, Program, Stmt, VarDef};
    use rstest::rstest;

    #[rstest]
    #[case("", vec![])]
    #[case("fn main() {}", vec![
        Component::FuncDef(FuncDef {
            name: "main".to_string(),
            args: vec![],
            block: vec![]
        })
    ])]
    #[case("
        fn main() {}
        fn add() {}
        ", vec![
        Component::FuncDef(FuncDef {
            name: "main".to_string(),
            args: vec![],
            block: vec![]
        }),
        Component::FuncDef(FuncDef {
            name: "add".to_string(),
            args: vec![],
            block: vec![]
        })
    ])]
    fn program_parse(#[case] input: &str, #[case] excepted: Program) {
        let result = primo_parser::program(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    #[case(
        "fn main() {}",
        Component::FuncDef(FuncDef {
            name: "main".to_string(),
            args: vec![],
            block: vec![]
        })
    )]
    #[case(
        "fn main(a, b) {}",
        Component::FuncDef(FuncDef {
            name: "main".to_string(),
            args: vec![
                "a".to_string(),
                "b".to_string()
            ],
            block: vec![]
        })
    )]
    // NOTE: loop文同様ブロックの中身あるバージョンは書かない
    fn component_parse(#[case] input: &str, #[case] excepted: Component) {        
        let result = primo_parser::component(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    #[case("{}", vec![])]
    #[case(
        "{ let x = 0; x = 1; return x; }",
        vec![
            Stmt::VarDef(VarDef {
                name: "x".to_string(),
                value: Expr::Literal(Value::Int(0))
            }),
            Stmt::Assign(Assign {
                name: "x".to_string(),
                value: Expr::Literal(Value::Int(1))
            }),
            Stmt::Return(Some(Expr::Ident("x".to_string())))
        ]
    )]
    #[case(
        "{
            let x = 0;
            x = 1;
            return x;
        }",
        vec![
            Stmt::VarDef(VarDef {
                name: "x".to_string(),
                value: Expr::Literal(Value::Int(0))
            }),
            Stmt::Assign(Assign {
                name: "x".to_string(),
                value: Expr::Literal(Value::Int(1))
            }),
            Stmt::Return(Some(Expr::Ident("x".to_string())))
        ]
    )]
    fn block_parse(#[case] input: &str, #[case] excepted: Block) {
        let result = primo_parser::block(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    // case 1
    #[case("1;", Stmt::Expr(Expr::Literal(Value::Int(1))))]
    // case 2
    #[case(
        "print \"Hello World!\";",
        Stmt::Print(vec![
            Expr::Literal(Value::Str("Hello World!".to_string()))
        ])
    )]
    // case 3
    #[case(
        "print \"n:\", 10;",
        Stmt::Print(vec![
            Expr::Literal(Value::Str("n:".to_string())),
            Expr::Literal(Value::Int(10))
        ])
    )]
    // case 4
    #[case(
        "let x = 1;",
        Stmt::VarDef(VarDef {
            name: "x".to_string(),
            value: Expr::Literal(Value::Int(1)) 
        })
    )]
    // case 5
    #[case(
        "x = 1;",
        Stmt::Assign(Assign {
            name: "x".to_string(),
            value: Expr::Literal(Value::Int(1)) 
        })
    )]
    // case 6
    // NOTE: loop文のブロックあるバージョンは構文の定義上ブロックのテストがあるので要らないと判断
    #[case("loop {}", Stmt::Loop(vec![]))]
    // case 7
    #[case(
        "if false {}",
        Stmt::If(If {
            condition: Expr::Literal(Value::Bool(false)),
            block: vec![],
            elif: None,
            else_block: None
        })
    )]
    // case 8
    #[case(
        "if false {} else {}",
        Stmt::If(If {
            condition: Expr::Literal(Value::Bool(false)),
            block: vec![],
            elif: None,
            else_block: Some(vec![])
        })
    )]
    // case 9
    #[case(
        "if false {} elif true {}",
        Stmt::If(If {
            condition: Expr::Literal(Value::Bool(false)),
            block: vec![],
            elif: Some(vec![
                Elif {
                    condition: Expr::Literal(Value::Bool(true)),
                    block: vec![]
                }
            ]),
            else_block: None
        })
    )]
    // case 10
    #[case(
        "if false {} elif false {} elif true {}",
        Stmt::If(If {
            condition: Expr::Literal(Value::Bool(false)),
            block: vec![],
            elif: Some(vec![
                Elif {
                    condition: Expr::Literal(Value::Bool(false)),
                    block: vec![]
                },
                Elif {
                    condition: Expr::Literal(Value::Bool(true)),
                    block: vec![]
                }
            ]),
            else_block: None
        })
    )]
    // case 11
    #[case(
        "if false {} elif false {} elif true {} else {}",
        Stmt::If(If {
            condition: Expr::Literal(Value::Bool(false)),
            block: vec![],
            elif: Some(vec![
                Elif {
                    condition: Expr::Literal(Value::Bool(false)),
                    block: vec![]
                },
                Elif {
                    condition: Expr::Literal(Value::Bool(true)),
                    block: vec![]
                }
            ]),
            else_block: Some(vec![])
        })
    )]
    // case 12
    #[case("next;", Stmt::Next)]
    // case 13
    #[case("break;", Stmt::Break)]
    // case 14
    #[case("return;", Stmt::Return(None))]
    // case 15
    #[case("return 1;", Stmt::Return(Some(Expr::Literal(Value::Int(1)))))]
    fn statement_parse(#[case] input: &str, #[case] excepted: Stmt) {
        let result = primo_parser::statement(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    // case 1
    #[case(
        "hoge()",
        FuncCall {
            name: "hoge".to_string(),
            args: vec![]
        }
    )]
    // case 2
    #[case(
        "add(1, 2)",
        FuncCall {
            name: "add".to_string(),
            args: vec![
                Expr::Literal(Value::Int(1)),
                Expr::Literal(Value::Int(2))
            ]
        }
    )]
    // case 3
    #[case(
        "add(add(1, 2), 3)",
        FuncCall {
            name: "add".to_string(),
            args: vec![
                Expr::FuncCall(FuncCall {
                    name: "add".to_string(),
                    args: vec![
                        Expr::Literal(Value::Int(1)),
                        Expr::Literal(Value::Int(2)),
                    ]
                }),
                Expr::Literal(Value::Int(3))
            ]
        }
    )]
    fn func_call_parse(#[case] input: &str, #[case] excepted: FuncCall) {
        let result = primo_parser::func_call(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    // case 1
    #[case("a", Expr::Ident("a".to_string()))]
    // case 2
    #[case(
        "1--2",
        Expr::Sub(Pair {
            lhs: Box::new(Expr::Literal(Value::Int(1))),
            rhs: Box::new(Expr::Literal(Value::Int(-2)))
        }))]
    // case 3
    #[case(
        "a * 1 + b / 2",
        Expr::Add(Pair {
            lhs: Box::new(Expr::Mul(Pair {
                lhs: Box::new(Expr::Ident("a".to_string())),
                rhs: Box::new(Expr::Literal(Value::Int(1))),
            })),
            rhs: Box::new(Expr::Div(Pair {
                lhs: Box::new(Expr::Ident("b".to_string())),
                rhs: Box::new(Expr::Literal(Value::Int(2))),
            }))
        })
    )]
    // case 4
    #[case(
        "1 + a * 2 - b",
        Expr::Sub(Pair {
            lhs: Box::new(Expr::Add(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Mul(Pair {
                    lhs: Box::new(Expr::Ident("a".to_string())),
                    rhs: Box::new(Expr::Literal(Value::Int(2)))
                })),
            })),
            rhs: Box::new(Expr::Ident("b".to_string()))
        })
    )]
    // case 5
    #[case(
        "1 * 2 == 3 / 4",
        Expr::Eq(Pair {
            lhs: Box::new(Expr::Mul(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Literal(Value::Int(2)))
            })),
            rhs: Box::new(Expr::Div(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(3))),
                rhs: Box::new(Expr::Literal(Value::Int(4)))
            }))
        })
    )]
    // case 6
    #[case(
        "1 * (2 != 3) / 4",
        Expr::Div(Pair {
            lhs: Box::new(Expr::Mul(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Expr(Box::new(Expr::NotEq(Pair {
                    lhs: Box::new(Expr::Literal(Value::Int(2))),
                    rhs: Box::new(Expr::Literal(Value::Int(3)))
                }))))
            })),
            rhs: Box::new(Expr::Literal(Value::Int(4)))
        })
    )]
    // case 7
    #[case(
        "1 == 2 or 3 != 4",
        Expr::Or(Pair {
            lhs: Box::new(Expr::Eq(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Literal(Value::Int(2)))
            })),
            rhs: Box::new(Expr::NotEq(Pair {
                lhs: Box::new(Expr::Literal(Value::Int(3))),
                rhs: Box::new(Expr::Literal(Value::Int(4)))
            }))
        })
    )]
    // case 8
    #[case("not true", Expr::Not(Box::new(Expr::Literal(Value::Bool(true)))))]
    // case 9
    #[case(
        "not 1 == 2",
        Expr::Not(Box::new(Expr::Eq(Pair {
            lhs: Box::new(Expr::Literal(Value::Int(1))),
            rhs: Box::new(Expr::Literal(Value::Int(2)))
        })))
    )]
    fn expr_parse(#[case] input: &str, #[case] excepted: Expr) {
        let result = primo_parser::expr(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    // case 1
    #[case("\"\"", Value::Str("".to_string()))]
    // case 2
    #[case("\"Hello World!\"", Value::Str("Hello World!".to_string()))]
    // case 3
    #[case("\"\\t\\r\\n\\\\\"", Value::Str("\t\r\n\\".to_string()))]
    // case 4
    #[case("0", Value::Int(0))]
    // case 5
    #[case("10", Value::Int(10))]
    // case 6
    #[case("-100", Value::Int(-100))]
    // case 7
    #[case("true", Value::Bool(true))]
    // case 8
    #[case("false", Value::Bool(false))]
    fn literal_parse(#[case] input: &str, #[case] excepted: Value) {
        let result = primo_parser::literal(input);
        assert_eq!(result.unwrap(), excepted);
    }

    #[rstest]
    // case 1
    #[case("a")]
    // case 2
    #[case("_")]
    // case 3
    #[case("Abc")]
    // case 4
    #[case("abC")]
    // case 5
    #[case("_abc")]
    // case 6
    #[case("abc01")]
    // case 7
    #[case("_abc01")]
    // case 8
    #[case("_abc01_")]
    fn ident_parse(#[case] input: &str) {
        let result = primo_parser::ident(input);
        assert!(result.is_ok());
    }
}
