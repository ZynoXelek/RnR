use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Parameters, Prog, Statement, Type, UnOp,
};
use crate::vm::Val;
use crate::common::Optimize;
use crate::error::Error; //TODO: use custom error
use crate::intrinsics::*;

//?#################################################################################################
//?#                                                                                               #
//?#                                   Optimizer Scope Type                                        #
//?#                                                                                               #
//?#################################################################################################

pub type Uses = usize;

#[derive(Clone, Debug)]
pub struct ScopeFn {
    uses: Uses,
    fn_decl: FnDeclaration,
}

impl ScopeFn {
    pub fn new(uses: Uses, fn_decl: FnDeclaration) -> Self {
        Self { uses, fn_decl }
    }

    pub fn get_uses(&self) -> Uses {
        self.uses
    }

    pub fn get_fn_decl(&self) -> &FnDeclaration {
        &self.fn_decl
    }

    pub fn use_fn(&mut self) {
        self.uses += 1;
    }

    pub fn use_fn_multiple(&mut self, uses: Uses) {
        self.uses += uses;
    }

    pub fn revert_uses(&mut self, uses: Uses) {
        if self.uses > uses {
            self.uses -= uses;
        } else {
            self.uses = 0;
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    functions: HashMap<String, ScopeFn>, // Tracks the defined functions and their number of uses
    vars: HashMap<String, Uses>,      // Tracks the defined variables and their number of uses
}

impl Scope {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    //* Variables
    
    pub fn define_var(&mut self, var: String) {
        if let Some(uses) = self.vars.get_mut(&var) {
            *uses = 0; // We redefine the variable, it has therefore not be used yet.
        } else {
            self.vars.insert(var, 0);
        }
    }

    pub fn remove_var(&mut self, var: String) {
        self.vars.remove(&var);
    }

    pub fn use_var(&mut self, var: String) {
        if let Some(uses) = self.vars.get_mut(&var) {
            *uses += 1;
        } else {
            unreachable!("Variable {} is not defined", var); // Unreachable since the type checker will have already checked it.
        }
    }

    pub fn get_var_uses(&self, var: &str) -> Uses {
        if let Some(uses) = self.vars.get(var) {
            *uses
        } else {
            unreachable!("Variable {} is not defined", var); // Unreachable since the type checker will have already checked it.
        }
    }

    pub fn use_var_multiple(&mut self, var: String, uses: Uses) {
        if let Some(uses_var) = self.vars.get_mut(&var) {
            *uses_var += uses;
        } else {
            self.vars.insert(var, uses);
        }
    }

    pub fn revert_var_uses(&mut self, var: String, uses: Uses) {
        if let Some(uses_var) = self.vars.get_mut(&var) {
            if *uses_var > uses {
                *uses_var -= uses; // It is still more than 1
            } else {
                // The variable is not used anymore
                *uses_var = 0;
                // self.vars.remove(&var);
            }
        } else {
            unreachable!("Variable {} is not defined -> can't remove uses", var);
        }
    }

    //* Functions

    pub fn define_func(&mut self, fn_decl: FnDeclaration) {
        let fn_name = fn_decl.id.clone();
        if let Some(uses) = self.functions.get_mut(&fn_name) {
            unreachable!("Function {} is already defined", fn_name); // Unreachable since the type checker will have already checked it.
        } else {
            self.functions.insert(fn_name, ScopeFn::new(0, fn_decl));
        }
    }

    pub fn remove_func(&mut self, fn_name: String) {
        self.functions.remove(&fn_name);
    }

    pub fn use_func(&mut self, fn_name: String) {
        if let Some(scope_fn) = self.functions.get_mut(&fn_name) {
            scope_fn.use_fn();
        } else {
            unreachable!("Function {} is not defined", fn_name);
        }
    }

    pub fn get_func_uses(&self, fn_name: &str) -> Uses {
        if let Some(scope_fn) = self.functions.get(fn_name) {
            scope_fn.get_uses()
        } else {
            unreachable!("Function {} is not defined", fn_name);
        }
    }

    pub fn get_func_decl(&self, fn_name: &str) -> &FnDeclaration {
        if let Some(scope_fn) = self.functions.get(fn_name) {
            scope_fn.get_fn_decl()
        } else {
            unreachable!("Function {} is not defined", fn_name);
        }
    }

    pub fn use_func_multiple(&mut self, fn_name: String, uses: Uses) {
        if let Some(scope_fn) = self.functions.get_mut(&fn_name) {
            scope_fn.use_fn_multiple(uses);
        } else {
            // Only works for already defined functions
            unreachable!("Function {} is not defined -> can't infer function declaration", fn_name);
        }
    }

    pub fn revert_func_uses(&mut self, fn_name: String, uses: Uses) {
        if let Some(scope_fn) = self.functions.get_mut(&fn_name) {
            scope_fn.revert_uses(uses);
        } else {
            unreachable!("Function {} is not defined -> can't remove uses", fn_name);
        }
    }

    pub fn get_unused_vars(&self) -> Vec<String> {
        let mut unused_vars = vec![];

        for (var, uses) in self.vars.iter() {
            if *uses == 0 {
                unused_vars.push(var.clone());
            }
        }

        unused_vars
    }

    pub fn get_unused_funcs(&self) -> Vec<String> {
        let mut unused_funcs = vec![];

        for (fn_name, scope_fn) in self.functions.iter() {
            if scope_fn.get_uses() == 0 {
                unused_funcs.push(fn_name.clone());
            }
        }

        unused_funcs
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();

        s.push_str("Defined Variables and their uses:\n");
        for (var, uses) in self.vars.iter() {
            s.push_str(&format!("|\t{}: {}\n", var, uses));
        }

        s.push_str("Defined Functions and their uses:\n");
        for (fn_name, scope_fn) in self.functions.iter() {
            s.push_str(&format!("|\t{}: {}\n", fn_name, scope_fn.get_uses()));
        }

        write!(f, "{}", s)
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                 Var and Func usage tracking                                   #
//?#                                                                                               #
//?#################################################################################################

// Define private types for variable and function usage tracking

//? Helper methods to concatenate hashmaps of uses

fn concatenate_uses(uses1: &HashMap<String, Uses>, uses2: &HashMap<String, Uses>) -> HashMap<String, Uses> {
    let mut concatenated_uses = uses1.clone();

    for (var, uses) in uses2.iter() {
        if let Some(uses_self) = concatenated_uses.get_mut(var) {
            *uses_self += uses;
        } else {
            concatenated_uses.insert(var.clone(), *uses);
        }
    }

    concatenated_uses
}

//? Any expression can be described this way

// A block usage shows what is used from the outer block, on scope exit.
#[derive(Clone, Debug, PartialEq)]
struct ExprUsage {
    vars: HashMap<String, Uses>, // The list of the names of each variable used in the expr (used means read, not written)
    funcs: HashMap<String, Uses>, // The list of the names of each function called in the expr
    assigned_vars: Vec<String>, // The name of the outer variables assigned in the expr (for block cases)
}

//TODO: remove if unnecessary
// #[derive(Clone, Debug, PartialEq)]
// enum ExprUsage {
//     Basic(BasicUsage), // Ident, Lit
//     BinOp(Box<ExprUsage>, Box<ExprUsage>), // BinOp
//     UnOp(Box<ExprUsage>), // UnOp
//     Call(String, Vec<ExprUsage>), // Call: func name to get the 
//     IfThenElse(Box<ExprUsage>, Box<BlockUsage>, Option<Box<BlockUsage>>), // IfThenElse
//     Block(BlockUsage), // Block
// }

impl ExprUsage {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            assigned_vars: vec![],
        }
    }

    fn get_used_vars(&self) -> HashMap<String, Uses> {
        self.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<String, Uses> {
        self.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<String> {
        self.assigned_vars.clone()
    }

    fn add_var_use(&mut self, var: String) {
        if let Some(uses) = self.vars.get_mut(&var) {
            *uses += 1;
        } else {
            self.vars.insert(var, 1);
        }
    }

    fn add_func_use(&mut self, func: String) {
        if let Some(uses) = self.funcs.get_mut(&func) {
            *uses += 1;
        } else {
            self.funcs.insert(func, 1);
        }
    }

    fn add_assigned_var(&mut self, var: String) {
        // Avoid duplicates
        if !self.assigned_vars.contains(&var) {
            self.assigned_vars.push(var);
        }
    }

    fn concatenate(&mut self, other: ExprUsage) {
        self.vars = concatenate_uses(&self.vars, &other.vars);
        self.funcs = concatenate_uses(&self.funcs, &other.funcs);
        for var in other.assigned_vars {
            self.add_assigned_var(var);
        }
    }

    fn concatenates_vec(&mut self, other: Vec<ExprUsage>) {
        for usage in other {
            self.concatenate(usage);
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct OptExpr {
    expr: Expr,
    usage: ExprUsage,
}

impl OptExpr {
    fn new(expr: Expr, usage: ExprUsage) -> Self {
        Self { expr, usage }
    }

    fn as_tuple(self) -> (Expr, ExprUsage) {
        (self.expr, self.usage)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct OptBlock {
    block: Block,
    usage: ExprUsage,
}

impl OptBlock {
    fn new(block: Block, usage: ExprUsage) -> Self {
        Self { block, usage }
    }

    fn as_tuple(self) -> (Block, ExprUsage) {
        (self.block, self.usage)
    }
}

impl From<OptBlock> for OptExpr {
    fn from(opt_block: OptBlock) -> Self {
        Self::new(Expr::Block(opt_block.block), opt_block.usage)
    }
}

//? Single statement

#[derive(Clone, Debug, PartialEq)]
struct StmtUsage {
    vars: HashMap<String, Uses>, // The list of the names of each variable used in the statement (used means read, not written)
    funcs: HashMap<String, Uses>, // The list of the names of each function called in the statement
    assigned_vars: Vec<String>, // The name of the variables assigned in the statement
    modify_outer_scope: bool, // Whether the statement modifies a variable from an outer scope
    define_var: Option<String>, // The name of the variable defined in the statement if there is one
    define_func: Option<String>, // The name of the function defined in the statement if there is one
}

impl StmtUsage {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            assigned_vars: vec![],
            modify_outer_scope: false,
            define_var: None,
            define_func: None,
        }
    }

    fn get_used_vars(&self) -> HashMap<String, Uses> {
        self.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<String, Uses> {
        self.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<String> {
        self.assigned_vars.clone()
    }

    fn add_var_use(&mut self, var: String) {
        if let Some(uses) = self.vars.get_mut(&var) {
            *uses += 1;
        } else {
            self.vars.insert(var, 1);
        }
    }

    fn add_func_use(&mut self, func: String) {
        if let Some(uses) = self.funcs.get_mut(&func) {
            *uses += 1;
        } else {
            self.funcs.insert(func, 1);
        }
    }

    fn add_assigned_var(&mut self, var: String) {
        // Avoid duplicates
        if !self.assigned_vars.contains(&var) {
            self.assigned_vars.push(var);
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct OptStmt {
    stmt: Statement,
    usage: StmtUsage,
}

impl OptStmt {
    fn new(stmt: Statement, usage: StmtUsage) -> Self {
        Self { stmt, usage }
    }
}

//TODO: Remove if unnecessary
// //? This treats any possible case

// #[derive(Clone, Debug, PartialEq)]
// struct UsedVarsFuncs {
//     usage: StmtUsage,
//     modify_outer_scope: bool,
// }
// enum UsedVarsFuncs {
//     Stmt(StmtUsage),
//     Block(BlockUsage),
//     IfThenElse(StmtUsage, BlockUsage, Option<BlockUsage>),
//     While(StmtUsage, BlockUsage),
// }

// impl UsedVarsFuncs {

//     fn stmt(stmt_usage: StmtUsage) -> Self {
//         UsedVarsFuncs::Stmt(stmt_usage)
//     }

//     fn block(block_usage: BlockUsage) -> Self {
//         UsedVarsFuncs::Block(block_usage)
//     }

//     fn if_then_else(cond: StmtUsage, then_block: BlockUsage, else_block: Option<BlockUsage>) -> Self {
//         UsedVarsFuncs::IfThenElse(cond, then_block, else_block)
//     }

//     fn while_loop(cond: StmtUsage, block: BlockUsage) -> Self {
//         UsedVarsFuncs::While(cond, block)
//     }

//     fn get_used_vars(&self) -> HashMap<String, Uses> {
//         match self {
//             UsedVarsFuncs::Stmt(stmt_usage) => {
//                 stmt_usage.vars.clone()
//             }
//             UsedVarsFuncs::Block(block_usage) => {
//                 block_usage.vars.clone()
//             }
//             UsedVarsFuncs::While(cond, block) => {
//                 let vars_used = cond.vars.clone();
//                 concatenate_uses(&vars_used, &block.vars)
//             }
//             UsedVarsFuncs::IfThenElse(cond, then_block, else_block) => {
//                 let vars_used = cond.vars.clone();
//                 let mut vars_used = concatenate_uses(&vars_used, &then_block.vars);

//                 if let Some(else_block) = else_block {
//                     vars_used = concatenate_uses(&vars_used, &else_block.vars);
//                 }

//                 vars_used
//             }
//         }
//     }

//     fn get_used_funcs(&self) -> HashMap<String, Uses> {
//         match self {
//             UsedVarsFuncs::Stmt(stmt_usage) => {
//                 stmt_usage.funcs.clone()
//             }
//             UsedVarsFuncs::Block(block_usage) => {
//                 block_usage.funcs.clone()
//             }
//             UsedVarsFuncs::While(cond, block) => {
//                 let funcs_used = cond.funcs.clone();
//                 concatenate_uses(&funcs_used, &block.funcs)
//             }
//             UsedVarsFuncs::IfThenElse(cond, then_block, else_block) => {
//                 let funcs_used = cond.funcs.clone();
//                 let mut funcs_used = concatenate_uses(&funcs_used, &then_block.funcs);

//                 if let Some(else_block) = else_block {
//                     funcs_used = concatenate_uses(&funcs_used, &else_block.funcs);
//                 }

//                 funcs_used
//             }
//         }
//     }
// }

//?#################################################################################################
//?#                                                                                               #
//?#                                           Optimizer                                           #
//?#                                                                                               #
//?#################################################################################################

impl Optimize<Expr> for Expr {
    fn optimize(&self) -> Result<Expr, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_expr(self.clone())?;

        Ok(result.expr)
    }
}

impl Optimize<Block> for Block {
    fn optimize(&self) -> Result<Block, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_block(self.clone())?;

        Ok(result.block)
    }
}

impl Optimize<Prog> for Prog {
    fn optimize(&self) -> Result<Prog, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_prog(self.clone())?;

        Ok(result)
    }
}

#[derive(Clone, Debug)]
pub struct Optimizer {
    // The optimizer will generate an optimized version of the AST by erasing useless parts of code,
    // and simplifying expressions

    // The optimizer keeps tracks of the scopes to determine whether a function/var is used at some point.
    scopes: Vec<Scope>,
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            scopes: vec![],
        }
    }

    //? Scopes management

    fn add_scope(&mut self) {
        self.scopes.push(Scope::new());
    }
    
    fn remove_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_var(&mut self, var: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define_var(var);
        } else {
            unreachable!("No scope to define variable {}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn get_var_uses(&self, var: &str) -> Option<Uses> {
        for scope in self.scopes.iter().rev() {
            if let Some(_) = scope.vars.get(var) {
                return Some(scope.get_var_uses(var));
            }
        }

        None // Not defined
    }

    fn get_var_scope_id(&self, var: &str) -> Option<usize> {
        // This is not reversed!
        for (i, scope) in self.scopes.iter().enumerate() {
            if scope.vars.contains_key(var) {
                return Some(i);
            }
        }

        None // Not defined
    }

    fn use_var(&mut self, var: String) {
        // We look through the scopes in a reverse order to find the first definition of the variable.
        // We then increase the number of uses of this variable.

        for scope in self.scopes.iter_mut().rev() {
            if scope.vars.contains_key(&var) {
                scope.use_var(var);
                return;
            }
        }

        unreachable!("Trying to use an undefined variable: {}", var); // Unreachable since the type checker will have already checked it.
    }

    fn use_var_multiple(&mut self, var: String, uses: Uses) {
        // We look through the scopes in a reverse order to find the first definition of the variable.
        // We then increase the number of uses of this variable.

        for scope in self.scopes.iter_mut().rev() {
            if scope.vars.contains_key(&var) {
                scope.use_var_multiple(var, uses);
                return;
            }
        }

        unreachable!("Trying to use an undefined variable: {}", var); // Unreachable since the type checker will have already checked it.
    }

    fn revert_var_uses(&mut self, var: String, uses: Uses) {
        // We look through the scopes in a reverse order to find the first definition of the variable.
        // We then decrease the number of uses of this variable.

        for scope in self.scopes.iter_mut().rev() {
            if scope.vars.contains_key(&var) {
                scope.revert_var_uses(var, uses);
                return;
            }
        }

        unreachable!("Trying to revert uses of an undefined variable: {}", var); // Unreachable since the type checker will have already checked it.
    }

    fn define_func(&mut self, fn_decl: FnDeclaration) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define_func(fn_decl);
        } else {
            unreachable!("No scope to define function {}", fn_decl.id); // Unreachable since the type checker will have already checked it.
        }
    }

    fn get_func_uses(&self, fn_name: &str) -> Option<Uses> {
        for scope in self.scopes.iter().rev() {
            if let Some(_) = scope.functions.get(fn_name) {
                return Some(scope.get_func_uses(fn_name));
            }
        }

        None // Not defined
    }

    fn get_func_decl(&self, fn_name: &str) -> Option<&FnDeclaration> {
        for scope in self.scopes.iter().rev() {
            if let Some(_) = scope.functions.get(fn_name) {
                return Some(scope.get_func_decl(fn_name));
            }
        }

        None // Not defined
    }

    fn use_func(&mut self, fn_name: String) {
        // We look through the scopes in a reverse order to find the first definition of the function.
        // We then increase the number of uses of this function.

        for scope in self.scopes.iter_mut().rev() {
            if scope.functions.contains_key(&fn_name) {
                scope.use_func(fn_name);
                return;
            }
        }

        unreachable!("Trying to use an undefined function: {}", fn_name); // Unreachable since the type checker will have already checked it.
    }

    fn use_func_multiple(&mut self, fn_name: String, uses: Uses) {
        // We look through the scopes in a reverse order to find the first definition of the function.
        // We then increase the number of uses of this function.

        for scope in self.scopes.iter_mut().rev() {
            if scope.functions.contains_key(&fn_name) {
                scope.use_func_multiple(fn_name, uses);
                return;
            }
        }

        unreachable!("Trying to use an undefined function: {}", fn_name); // Unreachable since the type checker will have already checked it.
    }

    fn revert_func_uses(&mut self, fn_name: String, uses: Uses) {
        // We look through the scopes in a reverse order to find the first definition of the function.
        // We then decrease the number of uses of this function.

        for scope in self.scopes.iter_mut().rev() {
            if scope.functions.contains_key(&fn_name) {
                scope.revert_func_uses(fn_name, uses);
                return;
            }
        }

        unreachable!("Trying to revert uses of an undefined function: {}", fn_name); // Unreachable since the type checker will have already checked it.
    }

    fn use_used_vars_funcs_obj(&mut self, used_vars_funcs: StmtUsage) {

        let used_vars = used_vars_funcs.get_used_vars();
        let used_funcs = used_vars_funcs.get_used_funcs();

        for (var, uses) in used_vars.iter() {
            self.use_var_multiple(var.clone(), *uses);
        }

        for (func, uses) in used_funcs.iter() {
            self.use_func_multiple(func.clone(), *uses);
        }
    }

    fn revert_used_vars_func_obj(&mut self, used_vars_funcs: StmtUsage) {
        let used_vars = used_vars_funcs.get_used_vars();
        let used_funcs = used_vars_funcs.get_used_funcs();

        for (var, uses) in used_vars.iter() {
            self.revert_var_uses(var.clone(), *uses);
        }

        for (func, uses) in used_funcs.iter() {
            self.revert_func_uses(func.clone(), *uses);
        }
    }

    pub fn pretty_string_scopes(&self) -> String {
        let mut s = String::new();

        for (i, scope) in self.scopes.iter().enumerate() {
            s.push_str(&format!("Scope {}:\n", i));
            s.push_str(&format!("{}", scope));
            s.push_str("\n");
        }

        s
    }

    pub fn pretty_print_scopes(&self) {
        println!("{}", self.pretty_string_scopes());
    }

    //?###################################################################
    //?#                              Expressions                        #
    //?###################################################################

    fn optimize_unop(&mut self, unop: UnOp, operand: Expr) -> Result<OptExpr, Error> {
        // For now, the unop optimization only optimizes the operand, and returns the same unop if it is not a literal.

        let (operand_opt, operand_use) = self.optimize_expr(operand)?.as_tuple();

        // If the operand is the same unop for negations, we can remove them.
        if unop == UnOp::Neg {
            if let Expr::UnOp(UnOp::Neg, inner_inner) = operand_opt {
                return Ok(OptExpr::new(*inner_inner, operand_use));
            }
        } else if unop == UnOp::Bang {
            if let Expr::UnOp(UnOp::Bang, inner_inner) = operand_opt {
                return Ok(OptExpr::new(*inner_inner, operand_use));
            }
        }

        // If the operand is a literal, we can evaluate the operation, else we just return the unop with the optimized operand.
        match operand_opt {
            Expr::Lit(lit) => {
                // Use the vm to evaluate it
                let val = lit.into();
                let result = unop.eval(val);

                // Now we need to convert it as an OptExpr
                match result {
                    Ok(val) => {
                        let lit = Literal::from(val);
                        let lit_expr = Expr::Lit(lit);
                        Ok(OptExpr::new(lit_expr, operand_use)) // We keep the same operand use, which should therefore be a new() one.
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => {
                let expr = Expr::UnOp(unop, Box::new(operand_opt));
                Ok(OptExpr::new(expr, operand_use))
            },
        }
    }

    fn optimize_binop(&mut self, binop: BinOp, left: Expr, right: Expr) -> Result<OptExpr, Error> {
        // For now, the binop optimization only optimizes the two operands, and returns the same binop if they are not both literals.
        // Arrays get operations can be optimized if the index is a literal
        // Boolean operations may be short-circuited

        let (left_opt, left_use) = self.optimize_expr(left)?.as_tuple();
        let (right_opt, right_use) = self.optimize_expr(right)?.as_tuple();

        let mut complete_use = left_use.clone();
        complete_use.concatenate(right_use.clone());

        // Boolean short-circuiting
        if binop == BinOp::And {
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                let expr = Expr::Lit(Literal::Bool(false));
                return Ok(OptExpr::new(expr, left_use)); // We don't use the right operand
            }
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                return Ok(OptExpr::new(right_opt, right_use)); // We don't use the left operand
            }
        } else if binop == BinOp::Or {
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                let expr = Expr::Lit(Literal::Bool(true));
                return Ok(OptExpr::new(expr, left_use)); // We don't use the right operand
            }
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                return Ok(OptExpr::new(right_opt, right_use)); // We don't use the left operand
            }
        }

        //TODO: Improve optimization to support better optimizations
        // For instance, 3 + 4 + 5 + a => 12 + a
        // However, 3 + a + 4 + 5 => 12 + a as well (not supported yet)

        // If the operands are literals, we can evaluate the operation, else we just return the binop with the optimized operands.
        match (left_opt.clone(), right_opt.clone()) {
            (Expr::Lit(lit1), Expr::Lit(lit2)) => {
                // Use the vm to evaluate it
                let val1 = lit1.into();
                let val2 = lit2.into();
                let result = binop.eval(val1, val2);

                // Now we need to convert it as an Expr
                match result {
                    Ok(val) => {
                        let lit = Literal::from(val);
                        let expr = Expr::Lit(lit);
                        Ok(OptExpr::new(expr, complete_use)) // We keep the same operand use, which should therefore be a new() one.
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => {
                let expr = Expr::BinOp(binop, Box::new(left_opt), Box::new(right_opt));
                Ok(OptExpr::new(expr, complete_use))
            },
        }
    }

    fn optimize_par(&mut self, inner: Expr) -> Result<OptExpr, Error> {
        // For now, the Par optimization only optimizes the inner expression, and returns it without the parenthesis.

        let inner = self.optimize_expr(inner)?;

        Ok(inner)

        //? We may want to keep the outer parenthesis for the inner expression if it is an operation. In this case, uncomment the following lines:
        // // If the inner part is not an operation, we can remove the parenthesis.
        // // TODO: Check for priority of operations to remove useless parenthesis
        // let (inner_opt, inner_use) = inner.as_tuple();
        // match inner_opt {
        //     Expr::BinOp(_, _, _) | Expr::UnOp(_, _) => {
        //         let expr = Expr::Par(Box::new(inner_opt));
        //         Ok(OptExpr::new(expr, inner_use))
        //     }, // We keep the parenthesis.
        //     _ => Ok(inner), // We can remove the parenthesis.
        // }
    }

    fn optimize_call(&mut self, ident: String, args: Arguments) -> Result<OptExpr, Error> {
        // To optimize a function call, we optimize each argument and keep the same function call.

        let mut args_opt = vec![];
        let mut args_use = vec![];
        for arg in args.0.iter() {
            let (arg_opt, arg_use) = self.optimize_expr(arg.clone())?.as_tuple();
            args_opt.push(arg_opt);
            args_use.push(arg_use);
        }

        let final_expr = Expr::Call(ident.clone(), Arguments(args_opt));
        let mut final_use = ExprUsage::new();
        final_use.add_func_use(ident);
        final_use.concatenates_vec(args_use);
        Ok(OptExpr::new(final_expr, final_use))
    }

    fn optimize_ifthenelse(&mut self, cond: Expr, then_block: Block, else_block: Option<Block>) -> Result<OptExpr, Error> {
        // To optimize an if then else, we optimize the condition. If we can get a literal boolean, we can replace the if then else by the corresponding block.
        // We optimize the remaining block(s) as well.

        let (cond_opt, cond_use) = self.optimize_expr(cond)?.as_tuple();
        let mut final_use = cond_use.clone();

        // If the condition is a literal, we can evaluate it and return the corresponding block.
        if let Expr::Lit(Literal::Bool(b)) = cond_opt {
            if b {
                let expr_to_opti = Expr::Block(then_block);
                let (block_opt, block_use) = self.optimize_expr(expr_to_opti)?.as_tuple();
                final_use.concatenate(block_use);
                return Ok(OptExpr::new(block_opt, final_use));
            } else {
                if let Some(else_block) = else_block {
                    let expr_to_opti = Expr::Block(else_block);
                    let (block_opt, block_use) = self.optimize_expr(expr_to_opti)?.as_tuple();
                    final_use.concatenate(block_use);
                    return Ok(OptExpr::new(block_opt, final_use));
                } else {
                    return Ok(OptExpr::new(Expr::get_empty_expr(), cond_use));
                }
            }
        }
        
        // Else, we optimize both blocks and return the if then else with the optimized condition.
        let (then_block_opt, then_block_use) = self.optimize_block(then_block)?.as_tuple();
        let (else_block_opt, else_block_use) = match else_block {
            Some(block) => {
                let (block_opt, block_use) = self.optimize_block(block)?.as_tuple();
                (Some(block_opt), block_use)
            },
            None => (None, ExprUsage::new()),
        };

        let expr = Expr::IfThenElse(Box::new(cond_opt), then_block_opt, else_block_opt);
        final_use.concatenate(then_block_use);
        final_use.concatenate(else_block_use);

        Ok(OptExpr::new(expr, final_use))
    }

    fn post_block_opt_process(&self, block: Block) -> Expr {
        // If the block can be removed and replaced by a simple expression, it will be a block with a single statement without a semi-colon.
        // If the block is useless, it will be empty. Therefore, we can remove it.

        if block.statements.is_empty() {
            Expr::get_empty_expr() // We return an empty expression.
        } else if block.statements.len() == 1 && !block.semi {
            match block.statements[0].clone() {
                Statement::Expr(expr) => expr,
                _ => Expr::get_empty_expr(), // We return an empty expression.
            }
        } else {
            // Else, the block can not be optimized out, so we keep it.
            Expr::Block(block)
        }
    }

    pub fn optimize_expr(&mut self, expr: Expr) -> Result<OptExpr, Error> {

        match expr {
            Expr::Ident(ident) => {
                let mut expr_usage = ExprUsage::new();
                expr_usage.add_var_use(ident.clone());
                Ok(OptExpr::new(Expr::Ident(ident), expr_usage))
            },
            Expr::Lit(lit) => Ok(OptExpr::new(Expr::Lit(lit), ExprUsage::new())),
            Expr::BinOp(binop, left, right) => self.optimize_binop(binop, *left, *right),
            Expr::UnOp(unop, operand) => self.optimize_unop(unop, *operand),
            Expr::Par(inner) => self.optimize_par(*inner),
            Expr::Call(ident, args) => self.optimize_call(ident, args),
            Expr::IfThenElse(cond, then_block, else_block) => {
                self.optimize_ifthenelse(*cond, then_block, else_block)
            }
            Expr::Block(block) => {
                let (block_opt, block_use) = self.optimize_block(block)?.as_tuple();

                let b = self.post_block_opt_process(block_opt);
                Ok(OptExpr::new(b, block_use))
            }
        }
    }

    //?###################################################################
    //?#                              Statements                         #
    //?###################################################################

    // Creates a new optimizer from the current one, ready for a function call
    // It will have the same functions defined, but variables env reset and ready for checking the function
    fn from_function_call(&mut self, params: &Parameters) -> Optimizer {
        let mut optimizer = Optimizer::new();
        
        // Copy the functions
        let mut new_scopes = self.scopes.clone();
        for scope in new_scopes.iter_mut() {
            scope.vars.clear();
        }

        optimizer.scopes = new_scopes;
        
        // Define the parameters
        for param in params.0.iter() {
            optimizer.define_var(param.id.clone());
        }

        optimizer
    }

    fn optimize_fn_declaration(&mut self, fn_decl: FnDeclaration) -> Result<FnDeclaration, Error> {
        // To optimize a function declaration, we optimize the block.

        // We shall first define every required parameters.
        let mut optimizer = self.from_function_call(&fn_decl.parameters);

        // Main optimization goes through the modified optimization of a block
        let is_main = fn_decl.id == "main";

        // Then we can optimize its block
        let block_opt = optimizer._optimize_block(fn_decl.body, is_main)?;

        Ok(FnDeclaration {
            id: fn_decl.id,
            parameters: fn_decl.parameters,
            ty: fn_decl.ty,
            body: block_opt,
        })
    }

    fn optimize_statement(&mut self, statement: Statement, last_statement: bool) -> Result<OptStmt, Error> {
        // To optimize a statement, we optimize its different parts
        // If it is useless, we 'remove' it by returning an expr statement which is a literal unit.
        // Last statement allows to avoid removing an assign or an expression while it was the last statement of a block.

        match statement {
            Statement::Let(m, ident, ty, expr) => {
                // If there is an expr, we optimize it
                let expr_opt = match expr {
                    Some(e) => Some(self.optimize_expr(e)?),
                    None => None,
                };

                // The variable is not defined here, but in the scope optimization code

                Ok(Statement::Let(m, ident, ty, expr_opt))
            }
            Statement::Assign(ident, expr) => {
                // If there is an expr, we optimize it
                let expr_opt = self.optimize_expr(expr)?;

                Ok(Statement::Assign(ident, expr_opt))
            }
            Statement::Expr(expr) => {
                // - Expressions that are operations
                // - Expressions that are function calls which do not affect the program (no mutable references)

                let expr_opt = self.optimize_expr(expr)?;
                if last_statement {
                    Ok(Statement::Expr(expr_opt))
                } else {
                    match expr_opt.clone() {
                        Expr::Call(_, _) => {
                            let may_affect_prog: bool = false;

                            //TODO: Check if the function called has mutable references in its arguments. (need to implement references then)
                            // If not, it can be optimized out

                            if may_affect_prog {
                                Ok(Statement::Expr(expr_opt))
                            } else {
                                Ok(Statement::get_empty_statement()) // We return an empty statement.
                            }
                        }
                        Expr::IfThenElse(_, _, _) => Ok(Statement::Expr(expr_opt)),
                        Expr::Block(block) => {
                            // If we have a block, it will have been optimized.
                            // If the block can be removed and replaced by a simple expression, it will be a block with a single statement without a semi-colon.
                            // If the block is useless, it will be empty. Therefore, we can remove it.

                            if block.statements.is_empty() {
                                Ok(Statement::get_empty_statement()) // We return an empty statement.
                            } else if block.statements.len() == 1 && !block.semi {
                                match block.statements[0].clone() {
                                    Statement::Expr(expr) => Ok(Statement::Expr(expr)),
                                    _ => Ok(Statement::get_empty_statement()), // We return an empty statement.
                                }
                            } else {
                                // Else, the block can not be optimized out, so we keep it.
                                Ok(Statement::Expr(Expr::Block(block)))
                            }
                        }
                        _ => Ok(Statement::get_empty_statement()), // We return an empty statement.
                    }
                }
            }
            Statement::Fn(fn_decl) => {
                let opt_fn_decl = self.optimize_fn_declaration(fn_decl.clone())?;
                Ok(Statement::Fn(opt_fn_decl))
            },
            Statement::While(cond, block) => {
                // We optimize the block away if the condition can be evaluated to false.
                let cond_opt = self.optimize_expr(cond)?;

                if let Expr::Lit(Literal::Bool(false)) = cond_opt {
                    Ok(Statement::get_empty_statement())
                } else {
                    // Else, we optimize the block
                    let block_opt = self.optimize_block(block)?;
                    Ok(Statement::While(cond_opt, block_opt))
                }
            }
        }
    }

    //?###################################################################
    //?#                                Blocks                           #
    //?###################################################################

    fn define_block_functions(&mut self, block: &Block) {
        // This scans the whole block for function definitions 

        for stmt in block.statements.iter() {
            match stmt {
                Statement::Fn(decl) => {
                    self.define_func(decl.clone());
                }
                _ => (),
            }
        }
    }

    fn get_used_in_expr(&self, opt_expr: &Expr) -> UsedVarsFuncs {
        // This functions scans the given optimized expression to collect the used variables and functions.
        // It also returns whether the expression modifies a variable from an outer scope.

        // We shall go through the whole expression to determine which variables and functions are used.
        // Except if a mutable reference to an outer variable is used in a function call, it can not modify the outer scope
        // TODO: add support for the case described right above
        
        match opt_expr {
            Expr::Ident(ident) => {
                let mut stmt_usage = StmtUsage::new();
                stmt_usage.add_var_use(ident.clone());
                
                UsedVarsFuncs::stmt(stmt_usage)
            }
            Expr::Lit(_) => {
                UsedVarsFuncs::stmt(StmtUsage::new())
            }
            Expr::BinOp(_, left, right) => {
                let mut left_used = self.get_used_in_expr(left);
                let right_used = self.get_used_in_expr(right);

                left_used.concatenate(right_used);
                left_used
            }
            Expr::UnOp(_, inner) => {
                self.get_used_in_expr(inner)
            }
            Expr::Par(inner) => {
                self.get_used_in_expr(inner)
            }
            Expr::Call(ident, args) => {
                // A function call uses the function called obviously
                // Then, we process each argument
                let mut used_vars_funcs = UsedVarsFuncs::new();
                used_vars_funcs.add_func_use(ident.clone());

                for arg in args.0.iter() {
                    let arg_uses = self.get_used_in_expr(arg);
                    used_vars_funcs.concatenate(arg_uses);
                }

                // Moreover, it also uses the functions called inside the function body.
                // In the future, with mutable references, it will also use the mutable reference given as an argument
                // Yet it is already taken into account in the args processing

                // Propagating the function calls used in the function called
                let fn_decl = self.get_func_decl(ident).unwrap();
                let mut block_uses = UsedVarsFuncs::new();
                for stmt in fn_decl.body.statements.iter() {
                    let stmt_uses = self.get_used_vars_and_funcs(stmt);
                    block_uses.concatenate(stmt_uses);
                }
                // We only want to keep the functions used in the block
                block_uses.vars.clear();
                block_uses.modify_outer_scope = false;
                block_uses.define_var = None;
                block_uses.define_func = None;

                used_vars_funcs.concatenate(block_uses);

                used_vars_funcs
            }
            Expr::IfThenElse(cond, _, _) => {
                // The blocks will have already been processed the same way so we just have to consider the condition
                self.get_used_in_expr(cond)
            }
            Expr::Block(block) => {
                // The block have already been processed by the optimization
                UsedVarsFuncs::new()
            }
        }
    }

    fn get_used_vars_and_funcs(&self, opt_stmt: &Statement) -> UsedVarsFuncs {
        // This functions scans the given optimized statement to collect the used variables and functions.
        // It also returns whether the statement modifies a variable from an outer scope.

        // It is recursively built
        match opt_stmt {
            Statement::Let(_, ident, _, expr) => {
                let mut stmt_used = UsedVarsFuncs::new();
                stmt_used.define_var = Some(ident.clone());

                match expr {
                    Some(e) => {
                        let expr_used = self.get_used_in_expr(e);
                        stmt_used.concatenate(expr_used);
                    }
                    None => {}
                }

                stmt_used
            }
            Statement::Assign(ident_expr, expr) => {
                // An assignment modifies a variable. We need to check if the variable is defined in the current scope or an outer one.
                let ident = match ident_expr {
                    Expr::Ident(ident) => ident.clone(),
                    Expr::BinOp(BinOp::Get, left, _) => { // In case of an array access
                        match left.as_ref() {
                            Expr::Ident(ident) => ident.clone(),
                            _ => unreachable!("An assignment should have an identifier as left hand side (array case)"), // Unreachable thanks to type checker
                        }
                    }
                    _ => unreachable!("An assignment should have an identifier as left hand side"), // Unreachable thanks to type checker
                };

                let mut modify_outer_scope = false;
                let scope_id = self.get_var_scope_id(&ident);
                match scope_id {
                    Some(id) => {
                        if id == self.scopes.len() - 1 {
                            // The variable is defined in the current scope
                            // Nothing happens
                        } else {
                            // The variable is defined in an outer scope
                            modify_outer_scope = true;
                        }
                    }
                    None => {
                        unreachable!("Variable {} is not defined", ident); // Unreachable since the type checker will have already checked it.
                    }
                }

                let mut stmt_used = UsedVarsFuncs::new();
                stmt_used.assign_var = Some(ident.clone());
                stmt_used.modify_outer_scope = modify_outer_scope;

                // Then, we get the used variables and functions in the expression
                let used_vars_funcs = self.get_used_in_expr(expr);
                
                stmt_used.concatenate(used_vars_funcs);

                stmt_used
            }
            Statement::While(cond, block) => {
                // We don't need to bother about the block since it has already been processed the same way
                self.get_used_in_expr(cond)
            }
            Statement::Expr(expr) => {
                self.get_used_in_expr(expr)
            }
            Statement::Fn(decl) => {
                // A function declaration does not use anything, but defines the function
                let mut stmt_used = UsedVarsFuncs::new();
                stmt_used.define_func = Some(decl.id.clone());
                stmt_used
            }
        }
    }

    // This recursive function removes useless redundant let statements from before optimizing the block and remove every useless statement
    // For instance:
    // let var = {
    //     let c = 2; // Not detected as useless (Needs 3 iterations)
    //     let a = 1 - c; // Not detected as useless (Needs 2 iterations)
    //     let b = a + c; // Detected as useless (From the 1st iteration)
    //     let a = 3;
    //     let b: i32; // Detected as useless (From the 1st iteration)
    //     let b = a + 4;
    //     b
    // };
    fn optimize_block_redundant_vars(&mut self, init_statements: Vec<(Statement, UsedVarsFuncs)>) -> Vec<(Statement, UsedVarsFuncs)> {

        // Keep a copy of the scopes to go back to it in the recursive call
        let init_scopes = self.scopes.clone();

        // We proceed a first redundant removal
        let mut opt_statements: Vec<(Statement, UsedVarsFuncs)> = vec![];
        let nb_stmts = init_statements.len();

        for i in 0..nb_stmts {
            let (stmt, used) = init_statements[i].clone();

            // Update the used variables and functions. (No need to compute it again)
            self.use_used_vars_funcs_obj(used.clone());

            // If the statement is a let statement, we shall define the variable
            // However, if it already exists in this same scope, we shall first verify whether it is useful or not.
            // If it is not used, it can be removed
            if let Statement::Let(_, ident, _, _) = stmt.clone() {
                let current_scope = self.scopes.last().unwrap();
                if let Some(uses) = current_scope.vars.get(&ident) {
                    if *uses == 0 {
                        // The previously defined variable is not of use.
                        // We shall find and remove its definition from the already optimized statements
                        // We should start by the end of the list to remove the correct one
                        let nb_opt_stmts = opt_statements.len();
                        for (i, (stmt, used)) in opt_statements.clone().iter().rev().enumerate() {
                            if used.assign_var == Some(ident.clone()) {
                                // We have found an assignment statement, we shall remove it
                                opt_statements.remove(nb_opt_stmts - 1 - i);
                                // We shall revert the effect of this statement
                                self.revert_used_vars_func_obj(used.clone());
                            } else if used.define_var == Some(ident.clone()) {
                                // We have found the definition statement, it is the end
                                opt_statements.remove(nb_opt_stmts - 1 - i);
                                // We shall revert the effect of this statement
                                self.revert_used_vars_func_obj(used.clone());
                                break;
                            }
                        }
                    }
                }

                self.define_var(ident.clone());
            }

            opt_statements.push((stmt, used));
        }

        //? Debug
        // let initial_block = Block {
        //     statements: init_statements.iter().map(|(stmt, _)| stmt.clone()).collect(),
        //     semi: false, // we don't care
        // };
        // let resulting_block = Block {
        //     statements: opt_statements.iter().map(|(stmt, _)| stmt.clone()).collect(),
        //     semi: false, // we don't care
        // };
        // eprintln!("############################");
        // eprintln!("Recursive call result ->");
        // eprintln!("Initial block:\n{}", initial_block);
        // eprintln!("Resulting block:\n{}", resulting_block);
        // eprintln!("############################");

        // We shall compare it to the one that was given at first
        // If they are the same, we can stop the process
        if opt_statements == init_statements {
            return init_statements;
        } else {
            // recursive call
            self.scopes = init_scopes;
            return self.optimize_block_redundant_vars(opt_statements);
        }
    }

    pub fn optimize_block(&mut self, block: Block) -> Result<OptBlock, Error> {
        self._optimize_block(block, false)
    }

    fn _optimize_block(&mut self, block: Block, is_main: bool) -> Result<OptBlock, Error> {
        // Optimizing blocks should remove any useless statements, and simplify useful ones.
        // If a block does not affect the program, it should be removed.
        // If it returns a simple literal or variable or operation, and the previous statements do not affect the program or anything, it should be replaced by the return value.
        // In addition, if it respects the previous description, and does not return any value, it should simply be removed.

        // What are useless statements, block relative?
        // - Let statements that are not used.
        // - Function declarations that are not used.

        // First define the block scope
        self.add_scope();

        // Then scan the block for function definitions
        self.define_block_functions(&block);

        // This copy will be used to detect useless variables that are defined multiple times:
        // let a = 1; <--- This one
        // let a = 2;
        let mut optim_copy = self.clone();

        let mut opt_statements: Vec<(Statement, UsedVarsFuncs)> = vec![];
        let mut block_affects_outer = false;

        // First optimization of each statement
        let nb_stmts = block.statements.len();
        for i in 0..nb_stmts {
            let statement = block.statements[i].clone();
            let last_stmt = (i == nb_stmts - 1) && !block.semi; // To avoid optimizing out the return value of a block

            let statement_opt = self.optimize_statement(statement.clone(), last_stmt)?;

            // Skip the statement if it is useless
            if statement_opt.is_empty_statement() {
                continue;
            }

            // If the statement has not been optimized out yet, we can consider the variables/functions used.
            let modifications = self.get_used_vars_and_funcs(&statement_opt);

            // Updates the uses of variables and functions
            block_affects_outer = block_affects_outer || modifications.modify_outer_scope;
            self.use_used_vars_funcs_obj(modifications.clone());

            // If the statement is a let statement, we shall define the variable
            if let Statement::Let(_, ident, _, _) = statement_opt.clone() {
                self.define_var(ident.clone());
            }

            opt_statements.push((statement_opt, modifications));
        }

        // Check if the block is useless -> if it does not return any value + does not affect the outer scope
        // The main function can not be optimized out this way though
        if !is_main && block.semi && !block_affects_outer {
            // We remove the block scope
            self.remove_scope();

            // We return an empty block
            return Ok(Block {
                statements: vec![],
                semi: false,
            });
        }

        // The block is not useless, let's continue its optimization
        // We will start by removing any redundant definition of variables that are not used

        // Remove the redundant definition of variables that are useless
        // For instance:
        // {
        //      let a = 1; <--- This one
        //      let a = 2;
        //      a
        // }
        opt_statements = optim_copy.optimize_block_redundant_vars(opt_statements);

        // Get back the correct final scopes then
        self.scopes = optim_copy.scopes.clone();

        // Now that we have removed every redundant variable definition, and counted the number of times each local var and func is used in this block,
        // we shall remove the useless ones before exiting.
        // Once we have removed the first ones being used 0 times, we should process again if there are more to remove.
        // We stop iterating over the block once there is no remaining variable being used 0 times.
        // Finally, we remove the unused functions similarly.
        // TODO: manage mutable references inside functions call, which can therefore be a call to another function or anything

        //? --- Variable optimizations

        // Special case for the main function
        // -> In order to avoid optimizing out the main function as well since it does not return any value
        // we consider that each remaining variable is used at least once
        if is_main {
            let last_scope = self.scopes.last_mut().unwrap();
            for (_, uses) in last_scope.vars.iter_mut() {
                if uses.clone() == 0 {
                    *uses = 1;
                }
            }
        }

        let mut useless_vars = self.scopes.last().unwrap().get_unused_vars();
        
        while !useless_vars.is_empty() {
            let mut new_opt_statements: Vec<(Statement, UsedVarsFuncs)> = vec![];

            // We shall go through the statements in a reverse order to avoid removing the wrong definition of a var
            // For example:
            // let a = 1; <--- a is useful
            // let b = f(a);
            // let a = 0; <--- This one is not useful and this one should be removed
        
            for (stmt, used) in opt_statements.iter().rev() {
                if let Some(var_id) = &used.assign_var { // Assigning a value to a useless var
                    if useless_vars.contains(&var_id) {
                        // We do not add the statement to the final statements
                        // We revert the effect of this statement
                        self.revert_used_vars_func_obj(used.clone());
                    } else {
                        new_opt_statements.push((stmt.clone(), used.clone()));
                    }
                } else if let Some(var_id) = &used.define_var { // Defining a useless var
                    if useless_vars.contains(&var_id) {
                        // We do not add the statement to the final statements
                        // We revert the effect of this statement
                        self.revert_used_vars_func_obj(used.clone());
                        // We remove this variable from the current scope
                        self.scopes.last_mut().unwrap().remove_var(var_id.clone());

                        // We remove this variable from the useless variables
                        let useless_index = useless_vars.iter().position(|x| x == var_id).unwrap();
                        useless_vars.remove(useless_index);
                    } else {
                        new_opt_statements.push((stmt.clone(), used.clone()));
                    }
                } else {
                    new_opt_statements.push((stmt.clone(), used.clone()));
                }
            }
        
            // We shall reverse the order of statements to get back to the correct block order
            opt_statements = new_opt_statements;
            opt_statements.reverse();
            useless_vars = self.scopes.last().unwrap().get_unused_vars();
        }

        //? --- Function optimizations

        let mut useless_funcs = self.scopes.last().unwrap().get_unused_funcs();

        while !useless_funcs.is_empty() {

            // Here we don't have to bother about the order since a function with a given name can only be defined once in some scope

            let mut new_opt_statements: Vec<(Statement, UsedVarsFuncs)> = vec![];
        
            for (stmt, used) in opt_statements.iter() {
                if let Some(fn_id) = &used.define_func {
                    if useless_funcs.contains(&fn_id) {
                        // We do not add the statement to the final statements
                        // We revert the effect of this statement
                        self.revert_used_vars_func_obj(used.clone());
                        // We remove this function from the current scope
                        self.scopes.last_mut().unwrap().remove_func(fn_id.clone());
                    } else {
                        new_opt_statements.push((stmt.clone(), used.clone()));
                    }
                } else {
                    new_opt_statements.push((stmt.clone(), used.clone()));
                }
            }
        
            opt_statements = new_opt_statements;
            useless_funcs = self.scopes.last().unwrap().get_unused_funcs();
        }

        // Building the final block

        let opt_block = Block {
            statements: opt_statements.iter().map(|(stmt, _)| stmt.clone()).collect(),
            semi: block.semi,
        };

        // Remove the block scope
        self.remove_scope();

        Ok(opt_block)
    }

    //?###################################################################
    //?#                             Blocks                              #
    //?###################################################################

    pub fn optimize_prog(&mut self, prog: Prog) -> Result<Prog, Error> {
        // To optimize a program, we optimize each function declaration.

        // First define the initial scope
        self.add_scope();

        let mut opt_fn_decls: Vec<FnDeclaration> = vec![];

        // First define each function
        for fn_decl in prog.0.iter() {
            self.define_func(fn_decl.clone());

            if fn_decl.id == "main" {
                // The main function automatically has 1 use
                self.use_func("main".to_string());
            }
        }

        // Then optimize each of them and replace the previous definition
        for fn_decl in prog.0.iter() {
            let fn_decl_opt = self.optimize_fn_declaration(fn_decl.clone())?;
            let init_scope = self.scopes.last_mut().unwrap();
            init_scope.remove_func(fn_decl.id.clone());
            init_scope.define_func(fn_decl_opt.clone());

            opt_fn_decls.push(fn_decl_opt);
        }

        // At this point, each function has been completely optimized
        // If one of the given functions is not used, we can remove it from the program (except for main)
        let mut final_opt_fn_decls: Vec<FnDeclaration> = vec![];
        for fn_decl in opt_fn_decls.iter() {
            if fn_decl.id == "main" {
                final_opt_fn_decls.push(fn_decl.clone());
                continue;
            }

            if self.get_func_uses(&fn_decl.id).unwrap() > 0 {
                final_opt_fn_decls.push(fn_decl.clone());
            }
        }

        Ok(Prog(opt_fn_decls))
    }
}
