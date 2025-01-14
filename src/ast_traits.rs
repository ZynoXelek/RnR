// Extra traits implemented for AST

use crate::ast::*;
use std::fmt;

// Utility functions/traits for your AST here. (Same order as ast.rs)

//? ------------ Helper method to get the string of the desired number of tabs ------------

const TAB: &str = "    "; // Can be "\t" or "    " (4 spaces, it looks better in the terminal) or "." for debug purposes

fn get_tabs(nb_tabs: usize) -> String {
    let mut tabs = String::new();
    for _ in 0..nb_tabs {
        tabs.push_str(TAB);
    }
    tabs
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Array Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl Array {
    pub fn new(expressions: Vec<Expr>) -> Self {
        Array {
            values: expressions.clone(),
            size: expressions.len(),
        }
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_values(&self) -> Vec<Expr> {
        self.values.clone()
    }

    pub fn get_value(&self, index: usize) -> Expr {
        self.values[index].clone()
    }

    pub fn as_tuple(&self) -> (Vec<Expr>, usize) {
        (self.values.clone(), self.size)
    }

    // To verify if an identifier is contained in the Array
    pub fn contains_identifier(&self) -> bool {
        for expr in &self.values {
            if expr.contains_identifier() {
                return true;
            }
        }
        false
    }

    pub fn modify(&mut self, index: usize, new_value: Expr) {
        self.values[index] = new_value;
    }

    pub fn modify_seq(&mut self, indexes: &[usize], new_value: Expr) -> Result<(), String> {
        if indexes.is_empty() {
            // Nothing to do here
            Ok(())
        } else if indexes.len() == 1 {
            self.modify(indexes[0], new_value);
            Ok(())
        } else {
            let index = indexes[0];
            let new_array = self.get_value(index).clone();

            match new_array {
                Expr::Lit(Literal::Array(mut array)) => {
                    array.modify_seq(&indexes[1..], new_value)?;
                    self.modify(index, Expr::Lit(Literal::Array(array)));
                    Ok(())
                }
                _ => Err("Cannot modify a non-array value".to_string()),
            }
        }
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let values = &self.values;
        let size = self.size;

        write!(f, "[")?;
        for index in 0..size {
            if index < size {
                write!(f, "{}, ", values[index])?;
            } else {
                write!(f, "{}", values[index])?;
            }
        }
        write!(f, "]")
    }
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
            _ => unimplemented!("Type::from for {:?}", lit),
        }
    }
}

impl Type {
    //TODO: Optimize regex so that it is only compiled once and it can be centralized for both functions

    pub fn contains_any(&self) -> bool {
        match self {
            Type::Any => true,
            Type::Array(ty, _) => ty.contains_any(),
            _ => false,
        }
    }

    pub fn equals(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::I32, Type::I32) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Array(ty1, size1), Type::Array(ty2, size2)) => size1 == size2 && ty1.equals(ty2),
            _ => false,
        }
    }

    pub fn new(typename: &str) -> Self {
        // Array support
        // using regex to match the array type
        let r = regex::Regex::new(r"^\[(?P<type>.+)\s*;\s*(?P<size>\d+)\]$").unwrap();
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
            "_" => Type::Any,
            _ => unimplemented!("Type::new for {:?}", typename),
        }
    }

    pub fn is_valid_typename(typename: &str) -> bool {
        // Array support
        // using regex to match the array type
        let r = regex::Regex::new(r"^\[(?P<type>.+)\s*;\s*(?P<size>\d+)\]$").unwrap();
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
            "i32" | "bool" | "String" | "()" | "_" => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "_"),
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Unit => write!(f, "()"),
            Type::Array(ty, size) => write!(f, "[{}; {}]", ty, size),
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

impl From<Array> for Literal {
    fn from(a: Array) -> Self {
        Literal::Array(a)
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

    // To verify if an identifier is contained in the Literal
    pub fn contains_identifier(&self) -> bool {
        match self {
            Literal::Array(arr) => arr.contains_identifier(),
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
            Literal::Array(arr) => write!(f, "{}", arr),
            Literal::Unit => write!(f, "()"),
            // _ => unimplemented!("Literal::fmt for {:?}", self),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          Expr Type                                            #
//?#                                                                                               #
//?#################################################################################################

impl Expr {
    pub fn get_empty_expr() -> Expr {
        Expr::Lit(Literal::Unit)
    }

    pub fn is_empty_expr(&self) -> bool {
        let empty = Expr::get_empty_expr();
        self == &empty
    }

    //? Helpers for creation

    pub fn bin_op(o: BinOp, left: Expr, right: Expr) -> Self {
        Expr::BinOp(o, Box::new(left), Box::new(right))
    }

    pub fn un_op(o: UnOp, operand: Expr) -> Self {
        Expr::UnOp(o, Box::new(operand))
    }

    pub fn par(e: Expr) -> Self {
        Expr::Par(Box::new(e))
    }

    pub fn if_then_else(cond: Expr, then_block: Block, else_block: Option<Block>) -> Self {
        Expr::IfThenElse(Box::new(cond), then_block, else_block)
    }

    // Comparison parsing requires to identify whether the operator is a comparison operator
    pub fn is_comparison(&self) -> bool {
        match self {
            Expr::BinOp(op, _, _) => op.is_comparison(),
            _ => false,
        }
    }

    // When it is needed to check if an expression is a unit type
    pub fn is_unit(&self) -> bool {
        match self {
            Expr::Lit(l) => l.is_unit(),
            Expr::Par(e) => e.is_unit(),
            Expr::IfThenElse(_, then_block, else_block) => {
                // Both blocks should be of the same type, but not verified yet. (Will be in the type checker)
                then_block.is_unit()
                    || match else_block {
                        Some(block) => block.is_unit(),
                        None => true,
                    }
            }
            Expr::Block(b) => b.is_unit(),
            _ => false, // The type checker verifies the other cases in its vm
        }
    }

    // To verify if an identifier is contained in the Expr
    pub fn contains_identifier(&self) -> bool {
        match self {
            Expr::Lit(lit) => lit.contains_identifier(),
            Expr::Ident(_) => true,
            Expr::BinOp(_, left, right) => {
                left.contains_identifier() || right.contains_identifier()
            }
            Expr::UnOp(_, operand) => operand.contains_identifier(),
            Expr::Par(e) => e.contains_identifier(),
            Expr::Call(_, _) => true, // A call requires an identifier to call the function,
            Expr::IfThenElse(cond, then_block, else_block) => {
                cond.contains_identifier()
                    || then_block.contains_identifier()
                    || match else_block {
                        Some(block) => block.contains_identifier(),
                        None => false,
                    }
            }
            Expr::Block(block) => block.contains_identifier(),
        }
    }

    // Function to extract a variable identifier from an expression
    // It can be used for direct Ident expressions 'a' but it is not very useful
    // However, it can be used for array in this kind of expressions: a[0][1]
    // This is read as BinOp(Get, BinOp(Get, Ident(a), 0), 1)
    pub fn extract_var_identifier(&self) -> Option<String> {
        match self {
            Expr::Ident(ident) => Some(ident.clone()),
            Expr::BinOp(BinOp::Get, left, _) => {
                let left = *left.clone();
                left.extract_var_identifier()
            }
            _ => None, // There is no unique identifier here
        }
    }

    // This function extracts the sequence of expression in an array access
    // For instance: a[1 + 1][0] ---> [1 + 1, 0]
    pub fn extract_get_sequence(&self) -> Vec<Expr> {
        let mut current_get_sequence = Vec::new();
        self._extract_get_sequence(&mut current_get_sequence)
    }

    fn _extract_get_sequence(&self, current_get_sequence: &mut Vec<Expr>) -> Vec<Expr> {
        match self {
            Expr::BinOp(BinOp::Get, left, right) => {
                current_get_sequence.push(*right.clone());
                let left = *left.clone();
                left._extract_get_sequence(current_get_sequence)
            }
            _ => current_get_sequence.clone(), // There is no unique identifier here
        }
    }

    // Made to check if an expression is an if-then-else_if version
    fn is_expr_if_then_else_if(&self) -> bool {
        match self {
            Expr::IfThenElse(cond, then_block, else_block) => match else_block {
                Some(else_block) => {
                    if else_block.statements.len() == 1 {
                        if let Statement::Expr(Expr::IfThenElse(cond, then_block, else_block)) =
                            &else_block.statements[0]
                        {
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                None => false,
            },
            _ => false,
        }
    }

    // This function is used to check whether an expression requires multiple lines to be displayed
    fn requires_multiple_lines(&self) -> bool {
        match self {
            Expr::Lit(_) => false,
            Expr::Ident(_) => false,
            Expr::BinOp(_, left, right) => {
                left.requires_multiple_lines() || right.requires_multiple_lines()
            }
            Expr::UnOp(_, operand) => operand.requires_multiple_lines(),
            Expr::Par(e) => e.requires_multiple_lines(),
            Expr::Call(_, args) => args.0.iter().any(|arg| arg.requires_multiple_lines()),
            Expr::IfThenElse(cond, then_block, else_block) => {
                self.is_expr_if_then_else_if() // Two conditions are too many to put on the same line
                    || cond.requires_multiple_lines()
                    || then_block.requires_multiple_lines()
                    || match else_block {
                        // If both then and else are 1 line only, we can put them on the same line
                        Some(block) => block.requires_multiple_lines(),
                        None => false,
                    }
            }
            Expr::Block(block) => block.requires_multiple_lines(),
        }
    }

    fn get_string_repr(&self, indent: usize) -> String {
        self._get_string_repr_with_param(indent, false, 0)
    }

    fn _get_string_repr_with_param(
        &self,
        indent: usize,
        force_multiline: bool,
        first_indent: usize,
    ) -> String {
        // Gets the string representation of the expression with the desired number of tabs
        let mut s = String::new();

        let tabs = get_tabs(first_indent);
        s.push_str(&tabs);

        let mut expr_repr = String::new();

        match self {
            Expr::Ident(id) => expr_repr.push_str(&format!("{}", id)),
            Expr::Lit(l) => expr_repr.push_str(&format!("{}", l)),
            Expr::BinOp(op, left, right) => match op {
                BinOp::Get => {
                    expr_repr.push_str(&format!(
                        "{}[{}]",
                        left.get_string_repr(indent),
                        right.get_string_repr(indent)
                    ));
                }
                _ => expr_repr.push_str(&format!(
                    "{} {} {}",
                    left.get_string_repr(indent),
                    op,
                    right.get_string_repr(indent)
                )),
            },
            Expr::UnOp(op, operand) => {
                expr_repr.push_str(&format!("{}{}", op, operand.get_string_repr(indent)))
            }
            Expr::Par(e) => expr_repr.push_str(&format!("({})", e.get_string_repr(indent))),
            Expr::Call(id, args) => {
                expr_repr.push_str(&format!("{}{}", id, args.get_string_repr(indent)))
            }
            Expr::IfThenElse(cond, then_block, else_block) => {
                let multiline = self.requires_multiple_lines() || force_multiline;

                expr_repr.push_str(&format!(
                    "if {} {}",
                    cond.get_string_repr(indent),
                    then_block._get_string_repr_with_param(indent, multiline)
                ));

                if let Some(else_block) = else_block {
                    // This display supports if-then-else_if versions

                    if else_block.statements.len() == 1 {
                        if let Statement::Expr(Expr::IfThenElse(cond, then_block, else_block)) =
                            &else_block.statements[0]
                        {
                            let second_if_expr = Expr::IfThenElse(
                                cond.clone(),
                                then_block.clone(),
                                else_block.clone(),
                            );
                            expr_repr.push_str(&format!(
                                " else {}",
                                second_if_expr._get_string_repr_with_param(indent, multiline, 0)
                            ));
                        } else {
                            expr_repr.push_str(&format!(
                                " else {}",
                                else_block._get_string_repr_with_param(indent, multiline)
                            ));
                        }
                    } else {
                        expr_repr.push_str(&format!(
                            " else {}",
                            else_block._get_string_repr_with_param(indent, multiline)
                        ));
                    }
                }
            }
            Expr::Block(block) => expr_repr.push_str(&format!("{}", block.get_string_repr(indent))), // _ => unimplemented!("Expr::fmt for {:?}", self),
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
            BinOp::Get => 8, // Highest priority (above unary operators as well)
                             // _ => unimplemented!("BinOp::priority for {:?}", self),
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
                                // _ => unimplemented!("BinOp::fmt for {:?}", self),
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
            // _ => unimplemented!("UnOp::priority for {:?}", self),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Bang => write!(f, "!"),
            // _ => unimplemented!("UnOp::fmt for {:?}", self),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Mutable Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl Mutable {
    pub fn new(b: bool) -> Self {
        Mutable(b)
    }
}

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
    pub fn get_empty_statement() -> Statement {
        Statement::Expr(Expr::get_empty_expr())
    }

    pub fn is_empty_statement(&self) -> bool {
        let empty = Statement::get_empty_statement();
        self == &empty
    }

    // To verify whether the statement contains an identifier or not
    pub fn contains_identifier(&self) -> bool {
        match self {
            Statement::Let(_, _, _, expr) => match expr {
                Some(e) => e.contains_identifier(),
                None => false,
            },
            Statement::Assign(left, right) => {
                let res = left.contains_identifier() || right.contains_identifier();
                if !res {
                    eprintln!(
                        "/!\\ WARNING: Assignment statement does not contain any identifier /!\\"
                    );
                }
                res
            }
            Statement::While(cond, block) => {
                cond.contains_identifier() || block.contains_identifier()
            }
            Statement::Expr(expr) => expr.contains_identifier(),
            Statement::Fn(fn_decl) => false, // The declaration of a function, similarly to a var, is not an identifier itself
        }
    }

    // This function is used to check if a statement requires a semi colon at the end of its line or not
    // If the statement is the last one of a block, it ignores this function and uses the next one
    pub fn requires_semi_colon(&self) -> bool {
        match self {
            Statement::Let(_, _, _, _) => true,
            Statement::Assign(_, _) => true,
            Statement::While(_, _) => false,
            Statement::Expr(expr) => match expr {
                Expr::IfThenElse(_, _, _) => false,
                Expr::Block(_) => false,
                _ => true,
            },
            Statement::Fn(_) => false,
            // _ => unimplemented!("Statement::requires_semi_colon for {:?}", self),
        }
    }

    // This function is used to check if a statement can be the last statement of a block without a semi colon
    pub fn can_terminate_block(&self) -> bool {
        match self {
            Statement::Let(_, _, _, _) => false, // Only 'let' statements cannot be the last statement of a block
            Statement::Assign(_, _) => true,
            Statement::While(_, _) => true,
            Statement::Expr(_) => true,
            Statement::Fn(_) => true,
            // _ => unimplemented!("Statement::can_terminate_block for {:?}", self),
        }
    }

    // This function is used to check whether a statement requires multiple lines to be displayed
    fn requires_multiple_lines(&self) -> bool {
        match self {
            Statement::Let(_, _, _, expr) => match expr {
                Some(e) => e.requires_multiple_lines(),
                None => false,
            },
            Statement::Assign(left, right) => {
                left.requires_multiple_lines() || right.requires_multiple_lines()
            }
            //In case we want to have some single line whiles: cond.requires_multiple_lines() || block.requires_multiple_lines(),
            Statement::While(cond, block) => true,
            Statement::Expr(expr) => expr.requires_multiple_lines(),
            // In case we want to have some single line func declaration: fn_decl.body.requires_multiple_lines(),
            Statement::Fn(fn_decl) => true,
        }
    }

    fn get_string_repr(&self, indent: usize) -> String {
        // Gets the string representation of the statement with the desired number of tabs
        let mut s = String::new();
        let tabs = get_tabs(indent);

        let mut stmt_repr = String::new();
        stmt_repr.push_str(&tabs);

        match self {
            Statement::Let(m, id, ty, expr) => {
                stmt_repr.push_str(&format!("let {}{}", m, id));
                if let Some(t) = ty {
                    stmt_repr.push_str(&format!(": {}", t));
                }
                if let Some(e) = expr {
                    stmt_repr.push_str(&format!(" = {}", e.get_string_repr(indent)));
                }
            }
            Statement::Assign(left, right) => {
                stmt_repr.push_str(&format!(
                    "{} = {}",
                    left.get_string_repr(indent),
                    right.get_string_repr(indent)
                ));
            }
            Statement::While(condition, block) => {
                let multiline = self.requires_multiple_lines();

                stmt_repr.push_str(&format!(
                    "while {} {}",
                    condition.get_string_repr(indent),
                    block._get_string_repr_with_param(indent, multiline)
                ));
            }
            Statement::Expr(expr) => {
                stmt_repr.push_str(&format!("{}", expr.get_string_repr(indent)));
            }
            Statement::Fn(fn_decl) => {
                stmt_repr.push_str(&format!("{}", fn_decl.get_string_repr(indent, 0)));
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

    pub fn get_empty_block() -> Block {
        Block::new(vec![], false)
    }

    // When it is needed to check if a block is a unit type
    pub fn is_unit(&self) -> bool {
        self.statements.is_empty() || self.semi // A block is of unit type if it is empty or ends with a semi colon
    }

    // To verify if an identifier is contained in the Block
    pub fn contains_identifier(&self) -> bool {
        for stmt in &self.statements {
            if stmt.contains_identifier() {
                return true;
            }
        }
        false
    }

    // This function is used to check whether a block requires multiple lines to be displayed
    fn requires_multiple_lines(&self) -> bool {
        // Unit type blocks will be displayed on multiple lines rather than a single line
        // Avoid { a = 2; }
        // For
        // {
        //     a = 2;
        // }
        if self.statements.len() > 1 || self.semi {
            true
        } else {
            match self.statements.first() {
                Some(stmt) => stmt.requires_multiple_lines(),
                None => false,
            }
        }
    }

    fn get_string_repr(&self, indent: usize) -> String {
        self._get_string_repr_with_param(indent, false)
    }

    fn _get_string_repr_with_param(&self, indent: usize, force_multiline: bool) -> String {
        // Gets the string representation of the block with the desired number of tabs

        let mut s = String::new();

        // Blocks start at the same indent level as the block itself, so no need to add a tab at the beginning
        // However, the last line of the block which close the brackets should be added tabs to correctly align it with the block

        let mut block_repr = String::new();

        let multiline = self.requires_multiple_lines() || force_multiline;

        block_repr.push_str(&"{");

        let indent = if multiline { indent } else { 0 };
        let inner_ident = if multiline { indent + 1 } else { 0 };

        if multiline {
            block_repr.push_str(&"\n");
        } else {
            block_repr.push_str(&" ");
        }

        let tabs = get_tabs(indent);

        let nb_stmts = self.statements.len();
        for index in 0..nb_stmts {
            let stmt = &self.statements[index];

            block_repr.push_str(&format!("{}", stmt.get_string_repr(inner_ident)));

            let last_statement = index == nb_stmts - 1;

            // Avoid adding a semi-colon to if, while, fn, and block statements
            let should_add_semi = (!last_statement || self.semi) && stmt.requires_semi_colon();

            if should_add_semi {
                block_repr.push_str(&";");
            }

            if !last_statement || multiline {
                block_repr.push_str(&"\n");
            } else {
                block_repr.push_str(&" ");
            }
        }
        if multiline {
            block_repr.push_str(&tabs);
        }
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

    fn get_string_repr(&self, indent: usize) -> String {
        // Gets the string representation of the arguments with the desired number of tabs
        let mut s = String::new();

        let mut args_repr = String::new();

        // TODO: Improve display for blocks in arguments

        args_repr.push_str(&format!("("));

        let nb_args = self.0.len();
        for index in 0..nb_args {
            if index < nb_args - 1 {
                args_repr.push_str(&format!("{}, ", self.0[index].get_string_repr(indent)));
            } else {
                args_repr.push_str(&format!("{}", self.0[index].get_string_repr(indent)));
            }
        }
        args_repr.push_str(&format!(")"));

        s.push_str(&args_repr);

        s
    }
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0))
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

    // Helper method to easily get the return type of the function
    pub fn get_return_type(&self) -> Type {
        if let Some(t) = &self.ty {
            t.clone()
        } else {
            Type::Unit
        }
    }

    // When we just want to display its signature, not the whole function
    pub fn get_signature_repr(&self, indent: usize) -> String {
        // Gets the string representation of the signature of the function declaration with the desired number of tabs
        let mut s = String::new();
        let tabs = get_tabs(indent);

        s.push_str(&tabs);

        let mut fn_repr = String::new();

        fn_repr.push_str(&format!("fn {}{}", self.id, self.parameters));
        if let Some(t) = &self.ty {
            fn_repr.push_str(&format!(" -> {}", t));
        }

        s.push_str(&fn_repr);

        s
    }

    fn get_string_repr(&self, indent: usize, signature_indent: usize) -> String {
        // Gets the string representation of the function declaration with the desired number of tabs

        let multiline = Statement::Fn(self.clone()).requires_multiple_lines();

        let mut fn_repr = self.get_signature_repr(signature_indent);
        fn_repr.push_str(&format!(
            " {}",
            self.body._get_string_repr_with_param(indent, multiline)
        ));

        fn_repr
    }
}

impl fmt::Display for FnDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_string_repr(0, 0))
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
