#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any, // ("_") used for empty arrays, or the same as not giving the type in a let
    I32,
    Bool,
    String,
    Array(Box<Type>, usize),
    // Ref(Box<Type>),
    // MutRef(Box<Type>),
    Unit,
}
//TODO: References
// Things to not forget:
// - Think about lifetimes -> scope can't return a value from an inner scope
//                         -> On variable redefinition, a ref to the old one is still usable
//                            (So the initial variable should still be remembered somewhere, but not accessible anymore
//                             except from this reference)
//          -> This means that I need to generate `Scopes` which store variables as HashMap<String, usize>
//             which would store var_name -> scope_id to to be able to access the variable in the scope data
//             and I would also need an HashMap<usize, Var> to store the actual variables.
//             I can also store temporary ones there this way.
//
// This would be a good thing to be implemented at every step of the compiler, so that every part could be compatible with
// the reference system.
// May I implement a generic Scope object in 'common.rs'? Scope<T> system. That would be nice.

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub values: Vec<Expr>,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
    Array(Array),
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

#[derive(Debug, Clone)] // PartialEq is implemented manually
pub struct Block {
    pub statements: Vec<Statement>,
    pub semi: bool,
    // This will be set by the type checker so that the backend can know in advance the size of the return value
    pub return_type: Option<Type>,
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
