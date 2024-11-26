// Extra traits implemented for AST

use crate::ast::*;
use std::fmt;

// Utility functions/traits for your AST here. (Same order as ast.rs)

//? ------------ Helper method to get the string of the desired number of tabs ------------

const TAB: &str = "    "; // Can be "\t" or "    " (4 spaces, it looks better in the terminal)

fn get_tabs(nb_tabs: u16) -> String {
    let mut tabs = String::new();
    for _ in 0..nb_tabs {
        tabs.push_str(TAB);
    }
    tabs
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         'Type' Type                                           #
//?#                                                                                               #
//?#################################################################################################

/// Anything that can be converted to a Literal can also be converted to a Type.
impl<T: Into<Literal>> From<T> for Type {
    fn from(x: T) -> Self {
        let lit: Literal = x.into();
        match lit {
            Literal::Unit => Type::Unit,
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::I32,
            Literal::String(_) => Type::String,
            Literal::Array(arr, size) => Type::Array(Box::new(Type::from(arr[0].clone())), size),
            _ => unimplemented!("Type::from for {:?}", lit),
        }
    }
}

impl Type {
    //TODO: Optimize regex so that it is only compiled once and it can be centralized for both functions

    pub fn new(typename: &str) -> Self {
        // Array support
        // using regex to match the array type
        let r = regex::Regex::new(r"^\[(?P<type>.+);\s*(?P<size>\d+)\]$").unwrap();
        if let Some(caps) = r.captures(typename) {
            // We are considering an array type

            let size: usize = caps["size"].parse().unwrap();
            let inner_type = &caps["type"].trim();
            return Type::Array(Box::new(Type::new(inner_type)), size);
        }

        // Other types
        match typename {
            "i32" => Type::I32,
            "bool" => Type::Bool,
            "String" => Type::String,
            "()" => Type::Unit,
            _ => unimplemented!("Type::new for {:?}", typename),
        }
    }

    pub fn is_valid_typename(typename: &str) -> bool {
        // Array support
        // using regex to match the array type
        let r = regex::Regex::new(r"^\[(?P<type>.+);\s*(?P<size>\d+)\]$").unwrap();
        if let Some(caps) = r.captures(typename) {
            // We are considering an array type

            let usize_str = caps["size"].trim();
            if usize_str.parse::<usize>().is_err() {
                return false;
            }
            let inner_type = &caps["type"].trim();
            return Type::is_valid_typename(inner_type);
        }

        // Other types
        match typename {
            "i32" | "bool" | "String" | "()" => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Unit => write!(f, "()"),
            Type::Array(ty, size) => write!(f, "[{}; {}]", ty, size),
            Type::Blank => write!(f, "_"),
            _ => unimplemented!("Type::fmt for {:?}", self),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Literal Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl From<i32> for Literal {
    fn from(i: i32) -> Self {
        Literal::Int(i)
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Bool(b)
    }
}

impl From<()> for Literal {
    fn from(_: ()) -> Self {
        Literal::Unit
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}

impl From<Vec<Literal>> for Literal {
    fn from(a: Vec<Literal>) -> Self {
        let size = a.len();
        Literal::Array(a, size)
    }
}

impl From<syn::Lit> for Literal {
    fn from(lit: syn::Lit) -> Self {
        match lit {
            syn::Lit::Str(s) => Literal::String(s.value()),
            syn::Lit::Int(i) => Literal::Int(i.base10_parse().unwrap()),
            syn::Lit::Bool(b) => Literal::Bool(b.value),
            _ => unimplemented!("Literal::from for {:?}", lit),
        }
    }
}

impl From<Literal> for i32 {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Int(i) => i,
            _ => panic!("cannot get integer from {:?}", lit),
        }
    }
}

impl From<Literal> for bool {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Bool(b) => b,
            _ => panic!("cannot get bool from {:?}", lit),
        }
    }
}

impl From<Literal> for String {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::String(s) => s,
            _ => panic!("cannot get string from {:?}", lit),
        }
    }
}

impl From<Literal> for Vec<Literal> {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Array(arr, size) => arr,
            _ => panic!("cannot get array from {:?}", lit),
        }
    }
}

impl Literal {
    pub fn get_int(&self) -> i32 {
        match self {
            Literal::Int(i) => *i,
            _ => panic!("cannot get integer from {:?}", self),
        }
    }

    pub fn get_bool(&self) -> bool {
        match self {
            Literal::Bool(b) => *b,
            _ => panic!("cannot get bool from {:?}", self),
        }
    }

    pub fn get_string(&self) -> String {
        match self {
            Literal::String(s) => s.clone(),
            _ => panic!("cannot get string from {:?}", self),
        }
    }

    pub fn get_array(&self) -> Vec<Literal> {
        match self {
            Literal::Array(arr,size) => arr.clone(),
            _ => panic!("cannot get array from {:?}", self),
        }
    }

    // Is it really useful? Just in case...
    pub fn get_unit(&self) {
        match self {
            Literal::Unit => (),
            _ => panic!("cannot get unit from {:?}", self),
        }
    }

    // When it is needed to check if a literal is a unit type
    pub fn is_unit(&self) -> bool {
        match self {
            Literal::Unit => true,
            _ => false,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Int(i) => write!(f, "{}", i),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Array(arr, size) => {
                write!(f, "[")?;
                for index in 0..*size {
                    if index < *size - 1 {
                        write!(f, "{}, ", arr[index])?;
                    } else {
                        write!(f, "{}", arr[index])?;
                    }
                }
                write!(f, "]")
            }
            Literal::Unit => write!(f, "()"),
            _ => unimplemented!("Literal::fmt for {:?}", self),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          Expr Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl Expr {
    pub fn bin_op(o: BinOp, left: Expr, right: Expr) -> Self {
        Expr::BinOp(o, Box::new(left), Box::new(right))
    }

    pub fn un_op(o: UnOp, operand: Expr) -> Self {
        Expr::UnOp(o, Box::new(operand))
    }

    // Comparison parsing requires to identify whether the operator is a comparison operator
    pub fn is_comparison(&self) -> bool {
        match self {
            Expr::BinOp(op, _, _) => op.is_comparison(),
            _ => false,
        }
    }

    // When it is needed to check if an expression is a unit type
    // Will be moved to the type checker
    pub fn is_unit(&self) -> bool {
        match self {
            //TODO: Identifier can be of unit type?
            Expr::Lit(l) => l.is_unit(),
            Expr::Par(e) => e.is_unit(),
            //TODO: Call can be, but need to check return type. How to access function definition?
            Expr::IfThenElse(_, then_block, else_block) => {
                // Both blocks should be of the same type, but not verified yet. (Will be in the type checker)
                then_block.is_unit()
                    || match else_block {
                        Some(block) => block.is_unit(),
                        None => true,
                    }
            }
            Expr::Block(b) => b.is_unit(),
            _ => false,
        }
    }

    fn get_string_repr(&self, indent: u16) -> String {
        // Gets the string representation of the expression with the desired number of tabs
        let mut s = String::new();
        let tabs = get_tabs(indent);

        s.push_str(&tabs);

        let mut expr_repr = String::new();

        match self {
            Expr::Ident(id) => expr_repr.push_str(&format!("{}", id)),
            Expr::Lit(l) => expr_repr.push_str(&format!("{}", l)),
            Expr::BinOp(op, left, right) => {
                match op {
                    BinOp::Get => {
                        expr_repr.push_str(&format!("{}[{}]", left, right));
                    },
                    _ => {
                        expr_repr.push_str(&format!("{} {} {}", left, op, right))
                    }
                }
            },
            Expr::UnOp(op, operand) => expr_repr.push_str(&format!("{}{}", op, operand)),
            Expr::Par(e) => expr_repr.push_str(&format!("({})", e)),
            Expr::Call(id, args) => expr_repr.push_str(&format!("{}{}", id, args)),
            Expr::IfThenElse(cond, then_block, else_block) => {
                expr_repr.push_str(&format!("if {} {}", cond, then_block.get_string_repr(indent)));
                if let Some(else_block) = else_block {
                    expr_repr.push_str(&format!(" else {}", else_block.get_string_repr(indent)));
                }
            }
            Expr::Block(block) => expr_repr.push_str(&format!("{}", block.get_string_repr(indent + 1))),
            _ => unimplemented!("Expr::fmt for {:?}", self),
        }

        s.push_str(&expr_repr);

        s
    }
}

/// Anything that can be converted into a Literal (like i32, bool, etc) can also be converted into an Expr.
impl<T: Into<Literal>> From<T> for Expr {
    fn from(x: T) -> Self {
        Expr::Lit(x.into())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0))
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         BinOp Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl BinOp {
    // Expression priority according to https://doc.rust-lang.org/reference/expressions.html
    pub fn priority(&self) -> u8 {
        match self {
            // Integer operations
            BinOp::Add => 4,
            BinOp::Sub => 4,
            BinOp::Mul => 5,
            BinOp::Div => 5,
            // Boolean operations
            BinOp::And => 2,
            BinOp::Or => 1,
            // Comparison operations
            BinOp::Eq => 3,
            BinOp::Ne => 3,
            BinOp::Gt => 3,
            BinOp::Lt => 3,
            BinOp::Ge => 3,
            BinOp::Le => 3,
            // Array operations
            BinOp::Get => 6, // Highest priority
            _ => unimplemented!("BinOp::priority for {:?}", self),
        }
    }

    // Comparison parsing requires to identify whether the operator is a comparison operator
    pub fn is_comparison(&self) -> bool {
        match self {
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => true,
            _ => false,
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::Get => "[]", // Not made to be used, since the binary operation will be responsible of correctly displaying it
            _ => unimplemented!("BinOp::fmt for {:?}", self),
        };
        write!(f, "{}", s)
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          UnOp Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl UnOp {
    // Every unary operators have higher priorities than binary operators
    // Not really necessary as it is automatically handled by the parse_operand
    pub fn priority(&self) -> u8 {
        match self {
            UnOp::Neg => 7,
            UnOp::Bang => 7,
            _ => unimplemented!("UnOp::priority for {:?}", self),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Bang => write!(f, "!"),
            _ => unimplemented!("UnOp::fmt for {:?}", self),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Mutable Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl fmt::Display for Mutable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mutable(true) => write!(f, "mut "), // there is a space because mut is always followed by a variable name
            Mutable(false) => write!(f, ""),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                       Statement Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl Statement {
    // This function is used to check if a statement requires a semi colon at the end of its line or not
    // If the statement is the last one of a block, it ignores this function as no legal termination statement requires a semi colon
    pub fn requires_semi_colon(&self) -> bool {
        match self {
            Statement::Let(_, _, _, _) => true,
            Statement::Assign(_, _) => true,
            Statement::While(_, _) => false,
            Statement::Expr(_) => true,
            Statement::Fn(_) => false,
            _ => unimplemented!("Statement::requires_semi_colon for {:?}", self),
        }
    }

    // This function is used to check if a statement can be the last statement of a block
    pub fn can_terminate_block(&self) -> bool {
        match self {
            Statement::Let(_, _, _, _) => false, // Only 'let' statements cannot be the last statement of a block
            Statement::Assign(_, _) => true,
            Statement::While(_, _) => true,
            Statement::Expr(_) => true,
            Statement::Fn(_) => true,
            _ => unimplemented!("Statement::can_terminate_block for {:?}", self),
        }
    }

    fn get_string_repr(&self, indent: u16) -> String {
        // Gets the string representation of the statement with the desired number of tabs
        let mut s = String::new();
        let tabs = get_tabs(indent);

        let mut stmt_repr = String::new();

        match self {
            Statement::Let(m, id, ty, expr) => {
                stmt_repr.push_str(&tabs);
                stmt_repr.push_str(&format!("let {}{}", m, id));
                if let Some(t) = ty {
                    stmt_repr.push_str(&format!(": {}", t));
                }
                if let Some(e) = expr {
                    stmt_repr.push_str(&format!(" = {}", e));
                }
            }
            Statement::Assign(left, right) => {
                stmt_repr.push_str(&tabs);
                stmt_repr.push_str(&format!("{} = {}", left, right));
            }
            Statement::While(condition, block) => {
                stmt_repr.push_str(&format!("while {} {}", condition, block.get_string_repr(indent)));
            }
            Statement::Expr(expr) => {
                stmt_repr.push_str(&format!("{}", expr.get_string_repr(indent)));
            }
            Statement::Fn(fn_decl) => {
                stmt_repr.push_str(&format!("{}", fn_decl.get_string_repr(indent)));
            }
        }

        s.push_str(&stmt_repr);

        s
    }
}

impl From<Expr> for Statement {
    fn from(expr: Expr) -> Self {
        Statement::Expr(expr)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0))
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Block Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl From<Expr> for Block {
    fn from(expr: Expr) -> Self {
        let s = Statement::Expr(expr);
        Block::new(vec![s], false) // No semi colon from an expression
    }
}

impl From<Statement> for Block {
    fn from(stmt: Statement) -> Self {
        Block::new(vec![stmt], true)
    }
}

impl Block {
    pub fn new(statements: Vec<Statement>, semi: bool) -> Self {
        Block { statements, semi }
    }

    // When it is needed to check if a block is a unit type
    pub fn is_unit(&self) -> bool {
        self.statements.is_empty() || self.semi // A block is of unit type if it is empty or ends with a semi colon
    }

    fn get_string_repr(&self, indent: u16) -> String {
        // Gets the string representation of the block with the desired number of tabs

        let mut s = String::new();
        let tabs = get_tabs(indent);

        // Blocks start at the same indent level as the block itself, so no need to add a tab at the beginning
        // However, the last line of the block which close the brackets should be added tabs to correctly align it with the block

        let mut block_repr = String::new();

        block_repr.push_str(&"{\n");
        for index in 0..self.statements.len() {
            block_repr.push_str(&format!("{}", self.statements[index].get_string_repr(indent + 1)));
            if index < self.statements.len() - 1 || self.semi {
                block_repr.push_str(&";");
            }

            block_repr.push_str(&"\n");
        }
        block_repr.push_str(&tabs);
        block_repr.push_str(&"}");

        s.push_str(&block_repr);

        s
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0))
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                       Parameter Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl Parameter {
    pub fn new(mutable: Mutable, id: String, ty: Type) -> Self {
        Parameter { mutable, id, ty }
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}: {}", self.mutable, self.id, self.ty)
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                       Parameters Type                                         #
//?#                                                                                               #
//?#################################################################################################

impl Parameters {
    pub fn new(params: Vec<Parameter>) -> Self {
        Parameters(params)
    }
}

impl fmt::Display for Parameters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for index in 0..self.0.len() {
            if index < self.0.len() - 1 {
                write!(f, "{}, ", self.0[index])?;
            } else {
                write!(f, "{}", self.0[index])?;
            }
        }
        write!(f, ")")
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                       Arguments Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl Arguments {
    pub fn new(args: Vec<Expr>) -> Self {
        Arguments(args)
    }
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for index in 0..self.0.len() {
            if index < self.0.len() - 1 {
                write!(f, "{}, ", self.0[index])?;
            } else {
                write!(f, "{}", self.0[index])?;
            }
        }
        write!(f, ")")
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                     FnDeclaration Type                                        #
//?#                                                                                               #
//?#################################################################################################

impl FnDeclaration {
    pub fn new(id: String, parameters: Parameters, ty: Option<Type>, body: Block) -> Self {
        FnDeclaration {
            id,
            parameters,
            ty,
            body,
        }
    }

    fn get_string_repr(&self, indent: u16) -> String {
        // Gets the string representation of the function declaration with the desired number of tabs
        let mut s = String::new();
        let tabs = get_tabs(indent);

        s.push_str(&tabs);

        let mut fn_repr = String::new();

        fn_repr.push_str(&format!("fn {}{}", self.id, self.parameters));
        if let Some(t) = &self.ty {
            fn_repr.push_str(&format!(" -> {}", t));
        }
        fn_repr.push_str(&format!(" {}", self.body.get_string_repr(indent)));

        s.push_str(&fn_repr);

        s
    }
}

impl fmt::Display for FnDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0))
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Prog Type                                             #
//?#                                                                                               #
//?#################################################################################################

impl Prog {
    pub fn new(fns: Vec<FnDeclaration>) -> Self {
        Prog(fns)
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for fn_decl in &self.0 {
            write!(f, "{}\n\n", fn_decl)?; // One empty line between each function
        }
        write!(f, "")
    }
}
