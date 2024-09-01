pub type Block = Vec<Stmt>;
pub type Program = Vec<Component>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pair<T> {
    pub lhs: T,
    pub rhs: T,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<String>,
    pub block: Block,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct If {
    pub condition: Expr,
    pub block: Block,
    pub elif: Option<Vec<Elif>>,
    pub else_block: Option<Block>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Elif {
    pub condition: Expr,
    pub block: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDef {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assign {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Component {
    FuncDef(FuncDef),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    VarDef(VarDef),
    Assign(Assign),
    If(If),
    Loop(Block),
    Next,
    Break,
    Return(Option<Expr>),
    Expr(Expr),
    Print(Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    // Logical
    Not(Box<Expr>),
    And(Pair<Box<Expr>>),
    Or(Pair<Box<Expr>>),
    // Compare
    Eq(Pair<Box<Expr>>),
    NotEq(Pair<Box<Expr>>),
    // Arithmetic
    Add(Pair<Box<Expr>>),
    Sub(Pair<Box<Expr>>),
    Mul(Pair<Box<Expr>>),
    Div(Pair<Box<Expr>>),
    Mod(Pair<Box<Expr>>),
    Expr(Box<Expr>),
    // Others
    FuncCall(FuncCall),
    Literal(Value),
    Ident(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Str(String),
}
