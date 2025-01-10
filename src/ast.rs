#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    Bool,
    String,
    Array(Box<Type>, usize),
    Unit,
    GenericArray, // Not for use, only for display
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
    Array(Vec<Literal>, usize), //TODO: Change to Expr rather than Literal? And move to Expr?
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Lit(Literal),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Par(Box<Expr>),
    Call(String, Arguments),
    IfThenElse(Box<Expr>, Block, Option<Block>),
    Block(Block),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
    // Integer operations
    Add, // And String concatenation
    Sub,
    Mul,
    Div,
    // Boolean only operations
    And,
    Or,
    // Comparison operations (Integer and String if not specified)
    Eq, // Work for all types
    Ne, // Work for all types
    Lt, // strictly less than
    Le, // less than or equal
    Gt, // strictly greater than
    Ge, // greater than or equal
    // Array operations
    Get, // Get element from array a[i]
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnOp {
    Bang, // Logical negation (!)
    Neg,  // Numeric negation (-)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Mutable(pub bool);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Mutable, String, Option<Type>, Option<Expr>),
    Assign(Expr, Expr),
    While(Expr, Block),
    Expr(Expr),
    Fn(FnDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub semi: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub mutable: Mutable,
    pub id: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameters(pub Vec<Parameter>);

#[derive(Debug, Clone, PartialEq)]
pub struct Arguments(pub Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct FnDeclaration {
    pub id: String,
    pub parameters: Parameters,
    pub ty: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prog(pub Vec<FnDeclaration>);
