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

// Scope identifier to be able to correctly locate the variable or function in the scopes
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Sid {
    name: String, // The name of the var/func
    scope_id: usize, // The id of the scope where it is defined
}

impl Sid {
    pub fn new(name: String, scope_id: usize) -> Self {
        Self { name, scope_id }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_scope_id(&self) -> usize {
        self.scope_id
    }

    pub fn as_tuple(&self) -> (String, usize) {
        (self.name.clone(), self.scope_id)
    }
}

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

fn concatenate_uses(uses1: &HashMap<Sid, Uses>, uses2: &HashMap<Sid, Uses>) -> HashMap<Sid, Uses> {
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

fn subtract_uses(uses1: &HashMap<Sid, Uses>, uses2: &HashMap<Sid, Uses>) -> HashMap<Sid, Uses> {
    let mut subtracted_uses = uses1.clone();

    for (var, uses) in uses2.iter() {
        if let Some(uses_self) = subtracted_uses.get_mut(var) {
            if *uses_self > *uses {
                *uses_self -= uses;
            } else {
                *uses_self = 0;
            }
        }
    }

    subtracted_uses
}

//? Any expression can be described with the three following structs

#[derive(Clone, Debug, PartialEq)]
struct SimpleExprUsage {
    vars: HashMap<Sid, Uses>, // The list of the names of each variable used in the expr (used means read, not written)
    funcs: HashMap<Sid, Uses>, // The list of the names of each function called in the expr
    assigned_vars: Vec<Sid>, // The name of the outer variables assigned in the expr (for block cases)
}

impl SimpleExprUsage {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            assigned_vars: vec![],
        }
    }

    fn get_used_vars(&self) -> HashMap<Sid, Uses> {
        self.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<Sid, Uses> {
        self.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<Sid> {
        self.assigned_vars.clone()
    }

    fn add_var_use(&mut self, var: &Sid) {
        if let Some(uses) = self.vars.get_mut(var) {
            *uses += 1;
        } else {
            self.vars.insert(var.clone(), 1);
        }
    }

    fn add_var_uses(&mut self, var: &Sid, uses: Uses) {
        if let Some(uses_self) = self.vars.get_mut(var) {
            *uses_self += uses;
        } else {
            self.vars.insert(var.clone(), uses);
        }
    }

    fn remove_one_var_use(&mut self, var: &Sid) {
        if let Some(uses) = self.vars.get_mut(var) {
            if *uses > 0 {
                *uses -= 1;
            } else {
                // The variable is not used anymore
                *uses = 0;
                // self.vars.remove(&var);
            }
        }
    }

    fn remove_one_var_use_and_delete(&mut self, var: &Sid) {
        if let Some(uses) = self.vars.get_mut(var) {
            if *uses > 1 {
                *uses -= 1;
            } else {
                // The variable is not used anymore
                // *uses = 0;
                self.vars.remove(&var);
            }
        }
    }

    fn add_func_use(&mut self, func: &Sid) {
        if let Some(uses) = self.funcs.get_mut(func) {
            *uses += 1;
        } else {
            self.funcs.insert(func.clone(), 1);
        }
    }

    fn add_func_uses(&mut self, func: &Sid, uses: Uses) {
        if let Some(uses_self) = self.funcs.get_mut(func) {
            *uses_self += uses;
        } else {
            self.funcs.insert(func.clone(), uses);
        }
    }

    fn add_assigned_var(&mut self, var: &Sid) {
        // Avoid duplicates
        if !self.assigned_vars.contains(var) {
            self.assigned_vars.push(var.clone());
        }
    }

    fn concatenate(&mut self, other: SimpleExprUsage) {
        self.vars = concatenate_uses(&self.vars, &other.vars);
        self.funcs = concatenate_uses(&self.funcs, &other.funcs);
        for var in other.assigned_vars {
            self.add_assigned_var(&var);
        }
    }

    fn concatenate_vec(&mut self, other: Vec<SimpleExprUsage>) {
        for usage in other {
            self.concatenate(usage);
        }
    }

    fn get_subtracted_var_uses(&self, other: SimpleExprUsage) -> HashMap<Sid, Uses> {
        subtract_uses(&self.vars, &other.vars)
    }

    fn get_subtracted_func_uses(&self, other: SimpleExprUsage) -> HashMap<Sid, Uses> {
        subtract_uses(&self.funcs, &other.funcs)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct BlockUsage {
    return_usage: SimpleExprUsage,
    scope_id: usize,
    stmts_usage: Vec<StmtUsage>,
}

impl BlockUsage {
    fn new(return_usage: SimpleExprUsage, scope_id: usize, stmts_usage: Vec<StmtUsage>) -> Self {
        Self { return_usage, scope_id, stmts_usage }
    }
    
    fn new_empty(scope_id: usize) -> Self {
        Self {
            return_usage: SimpleExprUsage::new(),
            scope_id,
            stmts_usage: vec![],
        }
    }

    fn as_simple_usage(&self) -> SimpleExprUsage {
        self.return_usage.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
enum DetailedExprUsage {
    Generic(SimpleExprUsage), // Ident, Lit
    BinOp(Box<DetailedExprUsage>, Box<DetailedExprUsage>), // BinOp
    UnOp(Box<DetailedExprUsage>), // UnOp
    Par(Box<DetailedExprUsage>), // Par
    Call(Sid, Vec<DetailedExprUsage>, Box<DetailedExprUsage>), // Call: func sid to identify the function + call usage
    IfThenElse(Box<DetailedExprUsage>, BlockUsage, Option<BlockUsage>), // IfThenElse
    Block(BlockUsage), // Block
}

impl DetailedExprUsage {
    //? Constructors

    fn new_generic(g_use: SimpleExprUsage) -> Self {
        Self::Generic(g_use)
    }

    fn new_binop(left: DetailedExprUsage, right: DetailedExprUsage) -> Self {
        Self::BinOp(Box::new(left), Box::new(right))
    }

    fn new_unop(expr: DetailedExprUsage) -> Self {
        Self::UnOp(Box::new(expr))
    }

    fn new_par(expr: DetailedExprUsage) -> Self {
        Self::Par(Box::new(expr))
    }

    fn new_call(func: Sid, args: Vec<DetailedExprUsage>, call_use: DetailedExprUsage) -> Self {
        Self::Call(func, args, Box::new(call_use))
    }

    fn new_if_then_else(cond: DetailedExprUsage, then_block: BlockUsage, else_block: Option<BlockUsage>) -> Self {
        Self::IfThenElse(Box::new(cond), then_block, else_block)
    }

    fn new_block(block: BlockUsage) -> Self {
        Self::Block(block)
    }

    //? Simplification

    fn as_simple_usage(&self) -> SimpleExprUsage {
        match self {
            Self::Generic(g_use) => g_use.clone(),
            Self::BinOp(left, right) => {
                let mut g_use = left.as_simple_usage();
                g_use.concatenate(right.as_simple_usage());
                g_use
            }
            Self::UnOp(expr) => expr.as_simple_usage(),
            Self::Par(expr) => expr.as_simple_usage(),
            Self::Call(func, args, func_call) => {
                let mut g_use = func_call.as_simple_usage();
                for arg in args {
                    g_use.concatenate(arg.as_simple_usage());
                }
                g_use.add_func_use(func);
                g_use
            }
            Self::IfThenElse(cond, then_b, else_b) => {
                let mut g_use = cond.as_simple_usage();
                g_use.concatenate(then_b.as_simple_usage());
                if let Some(else_b) = else_b {
                    g_use.concatenate(else_b.as_simple_usage());
                }
                g_use
            }
            Self::Block(block) => block.as_simple_usage(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ExprUsage {
    summary: SimpleExprUsage,
    detailed: DetailedExprUsage,
}

impl ExprUsage {

    //? Constructors

    fn new(detailed: DetailedExprUsage) -> Self {
        let summary = detailed.as_simple_usage();
        Self { summary, detailed }
    }

    fn new_empty() -> Self {
        let summary = SimpleExprUsage::new();
        let detailed = DetailedExprUsage::new_generic(summary.clone());
        Self { summary, detailed }
    }

    fn new_simple(g_use: SimpleExprUsage) -> Self {
        let detailed = DetailedExprUsage::new_generic(g_use.clone());
        Self { summary: g_use, detailed }
    }

    fn new_unop(expr: ExprUsage) -> Self {
        let detailed = DetailedExprUsage::new_unop(expr.detailed);
        ExprUsage::new(detailed)
    }

    fn new_binop(left: ExprUsage, right: ExprUsage) -> Self {
        let detailed = DetailedExprUsage::new_binop(left.detailed, right.detailed);
        ExprUsage::new(detailed)
    }

    fn new_par(expr: ExprUsage) -> Self {
        let detailed = DetailedExprUsage::new_par(expr.detailed);
        ExprUsage::new(detailed)
    }

    fn new_call(func: Sid, args: Vec<ExprUsage>, call_use: ExprUsage) -> Self {
        let detailed = DetailedExprUsage::new_call(func, args.iter().map(|arg| arg.detailed.clone()).collect(), call_use.detailed);
        ExprUsage::new(detailed)
    }

    fn new_if_then_else(cond: ExprUsage, then_block: BlockUsage, else_block: Option<BlockUsage>) -> Self {
        let detailed = DetailedExprUsage::new_if_then_else(cond.detailed, then_block, else_block);
        ExprUsage::new(detailed)
    }

    fn new_block(block: BlockUsage) -> Self {
        let detailed = DetailedExprUsage::Block(block);
        ExprUsage::new(detailed)
    }

    //? getters

    fn get_used_vars(&self) -> HashMap<Sid, Uses> {
        self.summary.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<Sid, Uses> {
        self.summary.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<Sid> {
        self.summary.assigned_vars.clone()
    }
}

impl From<BlockUsage> for ExprUsage {
    fn from(block_usage: BlockUsage) -> Self {
        Self::new_block(block_usage)
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

    fn get_expr(&self) -> Expr {
        self.expr.clone()
    }

    fn get_usage(&self) -> ExprUsage {
        self.usage.clone()
    }

    fn as_tuple(&self) -> (Expr, ExprUsage) {
        (self.expr.clone(), self.usage.clone())
    }
}

//? Blocks

#[derive(Clone, Debug, PartialEq)]
struct OptBlock {
    block: Block,
    usage: BlockUsage,
}

impl OptBlock {
    fn new(block: Block, usage: BlockUsage) -> Self {
        Self { block, usage }
    }

    fn get_block(&self) -> Block {
        self.block.clone()
    }

    fn get_usage(&self) -> BlockUsage {
        self.usage.clone()
    }

    fn as_tuple(&self) -> (Block, BlockUsage) {
        (self.block.clone(), self.usage.clone())
    }
}

impl From<OptBlock> for OptExpr {
    fn from(opt_block: OptBlock) -> Self {
        Self::new(Expr::Block(opt_block.block), ExprUsage::from(opt_block.usage))
    }
}

//? Statements

#[derive(Clone, Debug, PartialEq)]
struct SimpleStmtUsage {
    vars: HashMap<Sid, Uses>, // The list of the names of each variable used in the statement (used means read, not written)
    funcs: HashMap<Sid, Uses>, // The list of the names of each function called in the statement
    assigned_vars: Vec<Sid>, // The name of the variables assigned in the statement
    modify_current_scope: bool, // Whether the statement modifies a variable from the current scope
    modify_outer_scope: bool, // Whether the statement modifies a variable from an outer scope
    defined_var: Option<Sid>, // The name of the variable defined in the statement if there is one
    defined_func: Option<Sid>, // The name of the function defined in the statement if there is one
}

impl SimpleStmtUsage {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            assigned_vars: vec![],
            modify_current_scope: false,
            modify_outer_scope: false,
            defined_var: None,
            defined_func: None,
        }
    }

    fn get_used_vars(&self) -> HashMap<Sid, Uses> {
        self.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<Sid, Uses> {
        self.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<Sid> {
        self.assigned_vars.clone()
    }

    fn add_var_use(&mut self, var: &Sid) {
        if let Some(uses) = self.vars.get_mut(var) {
            *uses += 1;
        } else {
            self.vars.insert(var.clone(), 1);
        }
    }

    fn add_func_use(&mut self, func: &Sid) {
        if let Some(uses) = self.funcs.get_mut(func) {
            *uses += 1;
        } else {
            self.funcs.insert(func.clone(), 1);
        }
    }

    fn add_assigned_var(&mut self, var: &Sid) {
        // Avoid duplicates
        if !self.assigned_vars.contains(var) {
            self.assigned_vars.push(var.clone());
        }
    }

    fn concatenate(&mut self, other: SimpleStmtUsage) {
        self.vars = concatenate_uses(&self.vars, &other.vars);
        self.funcs = concatenate_uses(&self.funcs, &other.funcs);
        for var in other.assigned_vars {
            self.add_assigned_var(&var);
        }

        self.modify_current_scope = self.modify_current_scope || other.modify_current_scope;
        self.modify_outer_scope = self.modify_outer_scope || other.modify_outer_scope;

        // For var and func definitions, there should never be a case where the two statements define one.
        if self.defined_var.is_none() {
            self.defined_var = other.defined_var;
        }
        if self.defined_func.is_none() {
            self.defined_func = other.defined_func;
        }
    }

    fn concatenate_vec(&mut self, other: Vec<SimpleStmtUsage>) {
        for usage in other {
            self.concatenate(usage);
        }
    }

    fn get_subtracted_var_uses(&self, other: &SimpleStmtUsage) -> HashMap<Sid, Uses> {
        subtract_uses(&self.vars, &other.vars)
    }

    fn get_subtracted_func_uses(&self, other: &SimpleStmtUsage) -> HashMap<Sid, Uses> {
        subtract_uses(&self.funcs, &other.funcs)
    }

    fn get_subtracted_simple_stmt_usage(&self, other: &SimpleStmtUsage) -> SimpleStmtUsage {
        // This is a helper method which define a new StmtUsage with the subtracted uses
        let vars = self.get_subtracted_var_uses(other);
        let funcs = self.get_subtracted_func_uses(other);

        SimpleStmtUsage {
            vars,
            funcs,
            // The rest is not of use
            assigned_vars: vec![],
            modify_current_scope: false,
            modify_outer_scope: false,
            defined_var: None,
            defined_func: None,
        }
    }
}

impl From<SimpleExprUsage> for SimpleStmtUsage {
    fn from(expr_usage: SimpleExprUsage) -> Self {
        Self {
            vars: expr_usage.vars,
            funcs: expr_usage.funcs,
            assigned_vars: expr_usage.assigned_vars,
            modify_current_scope: false,
            modify_outer_scope: false,
            defined_var: None,
            defined_func: None,
        }
    }
}

impl From<ExprUsage> for SimpleStmtUsage {
    fn from(expr_usage: ExprUsage) -> Self {
        expr_usage.summary.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
enum DetailedStmtUsage {
    Let(Sid, Option<ExprUsage>), // Let: var sid + expr usage
    Assign(Sid, ExprUsage, ExprUsage), // Assign: sid + expr usage + expr usage (left is an expr usage as well for arrays)
    Expr(ExprUsage), // Expr: expr usage
    Fn(Sid), // Fn: no usage but function definition
    While(ExprUsage, BlockUsage), // While: expr usage + block usage
}

impl DetailedStmtUsage {
    //? Constructors

    fn new_let(var: Sid, expr: Option<ExprUsage>) -> Self {
        Self::Let(var, expr)
    }

    fn new_assign(var: Sid, left: ExprUsage, right: ExprUsage) -> Self {
        // When generating the left part, the identifier is considered used once because it is read in the expression
        // We should therefore revert a single use for the identifier

        //? Debug
        // eprintln!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        // eprintln!("Assignation of var: {:?}", var);
        // eprintln!("Before process:");
        // eprintln!("Left part: {:?}", left);
        // eprintln!("Right part: {:?}", right);

        let new_left_detailed = match left.detailed {
            DetailedExprUsage::Generic(mut g_use) => {
                g_use.remove_one_var_use_and_delete(&var); // May be changed back to the non removing version -> keep var used but with 0 uses
                DetailedExprUsage::new_generic(g_use)
            }
            DetailedExprUsage::BinOp(left_ident, _right_expr) => {
                let new_left_ident = match *left_ident {
                    DetailedExprUsage::Generic(mut g_use) => {
                        g_use.remove_one_var_use_and_delete(&var); // May be changed back to the non removing version -> keep var used but with 0 uses
                        Box::new(DetailedExprUsage::new_generic(g_use))
                    }
                    _ => unreachable!("Left part of an assignation should be a generic expression or a get array"),
                };

                DetailedExprUsage::BinOp(new_left_ident, _right_expr)
            }
            _ => unreachable!("Left part of an assignation should be a generic expression or a get array"),
        };

        // Generate the new Left part
        let new_left = ExprUsage::new(new_left_detailed);

        //? Debug
        // eprintln!("After process:");
        // eprintln!("Left part: {:?}", new_left);
        // eprintln!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

        Self::Assign(var, new_left, right)
    }

    fn new_expr(expr: ExprUsage) -> Self {
        Self::Expr(expr)
    }

    fn new_fn(func: Sid) -> Self {
        Self::Fn(func)
    }

    fn new_while(cond: ExprUsage, block: BlockUsage) -> Self {
        Self::While(cond, block)
    }

    //? Simplification

    fn as_simple_usage(&self) -> SimpleStmtUsage {
        match self {
            Self::Let(var, expr) => {
                let mut g_use = SimpleStmtUsage::new();
                g_use.defined_var = Some(var.clone());
                if let Some(expr) = expr {
                    g_use.concatenate(expr.clone().into());
                }
                g_use
            }
            Self::Assign(var, left, right) => {
                // We have two cases: a simple var assignation, or an array assignation
                // But, in both cases, we only have to get the uses of both, and concatenate them.
                // The use of the assigned variable will have already been removed on creation.
                // First case:
                // a = b * 3 + c; --> This will consider a, b and c used once. We remove a from the uses.
                // Second case:
                // a[b + c] = b * 3 + c; --> This will consider a used once, while b and c are used twice. We remove a from the uses.
                let mut g_use = SimpleStmtUsage::new();

                g_use.concatenate(left.clone().into());
                g_use.concatenate(right.clone().into());

                g_use.add_assigned_var(var);
                g_use
            }
            Self::Expr(expr) => expr.clone().into(),
            Self::Fn(func) => {
                let mut g_use = SimpleStmtUsage::new();
                g_use.defined_func = Some(func.clone());
                g_use
            },
            Self::While(cond, block) => {
                let mut g_use = SimpleStmtUsage::from(cond.clone());
                g_use.concatenate(SimpleStmtUsage::from(block.as_simple_usage()));
                g_use
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct StmtUsage {
    summary: SimpleStmtUsage,
    detailed: DetailedStmtUsage,
}

impl StmtUsage {
    //? Constructors

    fn new(detailed: DetailedStmtUsage) -> Self {
        let summary = detailed.as_simple_usage();
        Self { summary, detailed }
    }

    fn new_empty() -> Self {
        let summary = SimpleStmtUsage::new();
        let detailed = DetailedStmtUsage::new_expr(ExprUsage::new_empty());
        Self { summary, detailed }
    }

    fn new_let(var: Sid, expr: Option<ExprUsage>) -> Self {
        let detailed = DetailedStmtUsage::new_let(var, expr);
        StmtUsage::new(detailed)
    }

    fn new_assign(var: Sid, left: ExprUsage, right: ExprUsage) -> Self {
        let detailed = DetailedStmtUsage::new_assign(var, left, right);
        StmtUsage::new(detailed)
    }

    fn new_expr(expr: ExprUsage) -> Self {
        let detailed = DetailedStmtUsage::new_expr(expr);
        StmtUsage::new(detailed)
    }

    fn new_fn(func: Sid) -> Self {
        let detailed = DetailedStmtUsage::new_fn(func);
        StmtUsage::new(detailed)
    }

    fn new_while(cond: ExprUsage, block: BlockUsage) -> Self {
        let detailed = DetailedStmtUsage::new_while(cond, block);
        StmtUsage::new(detailed)
    }

    //? Getters

    fn get_used_vars(&self) -> HashMap<Sid, Uses> {
        self.summary.vars.clone()
    }

    fn get_used_funcs(&self) -> HashMap<Sid, Uses> {
        self.summary.funcs.clone()
    }

    fn get_assigned_vars(&self) -> Vec<Sid> {
        self.summary.assigned_vars.clone()
    }

    fn get_modify_current_scope(&self) -> bool {
        self.summary.modify_current_scope
    }

    fn get_modify_outer_scope(&self) -> bool {
        self.summary.modify_outer_scope
    }

    fn get_defined_var(&self) -> Option<Sid> {
        self.summary.defined_var.clone()
    }

    fn get_defined_func(&self) -> Option<Sid> {
        self.summary.defined_func.clone()
    }

    //? Helper method

    fn get_subtracted_stmt_usage(&self, other: &StmtUsage) -> StmtUsage {
        let summary = self.summary.get_subtracted_simple_stmt_usage(&other.summary);
        // We don't care about a complex usage for the subtraction
        let detailed = DetailedStmtUsage::new_expr(ExprUsage::new_empty());
        StmtUsage { summary, detailed } // We should not use new() since it uses the detailed usage to generate the summary
    }
}

impl From<ExprUsage> for StmtUsage {
    fn from(expr_usage: ExprUsage) -> Self {
        StmtUsage::new_expr(expr_usage)
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

    fn new_empty() -> Self {
        Self { stmt: Statement::get_empty_statement(), usage: StmtUsage::new_empty() }
    }

    fn get_stmt(&self) -> Statement {
        self.stmt.clone()
    }

    fn get_usage(&self) -> StmtUsage {
        self.usage.clone()
    }

    fn as_tuple(&self) -> (Statement, StmtUsage) {
        (self.stmt.clone(), self.usage.clone())
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Optimizer                                           #
//?#                                                                                               #
//?#################################################################################################

impl Optimize<Expr> for Expr {
    fn optimize(&self) -> Result<Expr, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_expr(self.clone())?;

        Ok(result)
    }
}

impl Optimize<Block> for Block {
    fn optimize(&self) -> Result<Block, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_block(self.clone())?;

        Ok(result)
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

    fn generate_sid(&self, name: &str) -> Sid {
        Sid::new(name.to_string(), self.scopes.len() - 1)
    }

    fn define_var(&mut self, var: String) -> Sid {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define_var(var.clone());
            // Then returns the generated Sid
            self.generate_sid(&var)
        } else {
            unreachable!("No scope to define variable {}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn remove_var(&mut self, var: &Sid) {
        let (var_name, scope_id) = var.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();
        scope.remove_var(var_name);
    }

    fn get_var_sid(&self, var_name: &str) -> Option<Sid> {
        // It returns the Sid of the closest defined variable with the same name (from the most outer scope)
        let nb_scopes = self.scopes.len();
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(_) = scope.vars.get(var_name) {
                return Some(Sid::new(var_name.to_string(), i));
            }
        }

        None // Not defined
    }

    fn get_var_uses(&self, var: &Sid) -> Option<Uses> {
        let (var_name, scope_id) = var.as_tuple();
        let scope = self.scopes.get(scope_id)?;
        if let Some(_) = scope.vars.get(&var_name) {
            Some(scope.get_var_uses(&var_name))
        } else {
            None // Not defined
        }
    }

    fn use_var(&mut self, var: &Sid) {
        let (var_name, scope_id) = var.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.vars.contains_key(&var_name) {
            scope.use_var(var_name);
        } else {
            unreachable!("Trying to use an undefined variable: {:?}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn use_var_multiple(&mut self, var: &Sid, uses: Uses) {
        let (var_name, scope_id) = var.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.vars.contains_key(&var_name) {
            scope.use_var_multiple(var_name, uses);
        } else {
            unreachable!("Trying to use an undefined variable: {:?}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn revert_var_uses(&mut self, var: &Sid, uses: Uses) {
        let (var_name, scope_id) = var.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.vars.contains_key(&var_name) {
            scope.revert_var_uses(var_name, uses);
        } else {
            unreachable!("Trying to revert uses of an undefined variable: {:?}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn define_func(&mut self, fn_decl: FnDeclaration) -> Sid {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define_func(fn_decl.clone());
            // Then returns the generated Sid
            self.generate_sid(&fn_decl.id)
        } else {
            unreachable!("No scope to define function {}", fn_decl.id); // Unreachable since the type checker will have already checked it.
        }
    }

    fn remove_func(&mut self, func: &Sid) {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();
        scope.remove_func(fn_name);
    }

    fn get_func_sid(&self, fn_name: &str) -> Option<Sid> {
        // It returns the Sid of the closest defined function with the same name (from the most outer scope)
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(_) = scope.functions.get(fn_name) {
                return Some(Sid::new(fn_name.to_string(), i));
            }
        }

        None // Not defined
    }

    fn get_func_uses(&self, func: &Sid) -> Option<Uses> {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get(scope_id)?;

        if let Some(_) = scope.functions.get(&fn_name) {
            Some(scope.get_func_uses(&fn_name))
        } else {
            None // Not defined
        }
    }

    fn get_func_decl(&self, func: &Sid) -> Option<&FnDeclaration> {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get(scope_id)?;

        if let Some(scope_fn) = scope.functions.get(&fn_name) {
            Some(scope_fn.get_fn_decl())
        } else {
            None // Not defined
        }
    }

    fn use_func(&mut self, func: &Sid) {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.functions.contains_key(&fn_name) {
            scope.use_func(fn_name);
        } else {
            unreachable!("Trying to use an undefined function: {:?}", func); // Unreachable since the type checker will have already checked it.
        }
    }

    fn use_func_multiple(&mut self, func: &Sid, uses: Uses) {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.functions.contains_key(&fn_name) {
            scope.use_func_multiple(fn_name, uses);
        } else {
            unreachable!("Trying to use an undefined function: {:?}", func); // Unreachable since the type checker will have already checked it.
        }
    }

    fn revert_func_uses(&mut self, func: &Sid, uses: Uses) {
        let (fn_name, scope_id) = func.as_tuple();
        let scope = self.scopes.get_mut(scope_id).unwrap();

        if scope.functions.contains_key(&fn_name) {
            scope.revert_func_uses(fn_name, uses);
        } else {
            unreachable!("Trying to revert uses of an undefined function: {:?}", func); // Unreachable since the type checker will have already checked it.
        }
    }

    fn apply_stmt_usage(&mut self, stmt_usage: &StmtUsage) {

        let used_vars = stmt_usage.get_used_vars();
        let used_funcs = stmt_usage.get_used_funcs();

        for (var, uses) in used_vars.iter() {
            self.use_var_multiple(var, *uses);
        }

        for (func, uses) in used_funcs.iter() {
            self.use_func_multiple(func, *uses);
        }
    }

    fn revert_stmt_usage(&mut self, stmt_usage: &StmtUsage) {
        let used_vars = stmt_usage.get_used_vars();
        let used_funcs = stmt_usage.get_used_funcs();

        for (var, uses) in used_vars.iter() {
            self.revert_var_uses(var, *uses);
        }

        for (func, uses) in used_funcs.iter() {
            self.revert_func_uses(func, *uses);
        }
    }

    fn get_current_scope_useless_vars(&self) -> Vec<Sid> {
        let current_scope_id = self.scopes.len() - 1;
        let current_scope = self.scopes.get(current_scope_id).unwrap();
        
        let useless_vars = current_scope.get_unused_vars();

        let mut useless_vars_sid = vec![];
        for var in useless_vars {
            useless_vars_sid.push(Sid::new(var, current_scope_id));
        }

        useless_vars_sid
    }

    fn get_current_scope_useless_funcs(&self) -> Vec<Sid> {
        let current_scope_id = self.scopes.len() - 1;
        let current_scope = self.scopes.get(current_scope_id).unwrap();
        
        let useless_funcs = current_scope.get_unused_funcs();

        let mut useless_funcs_sid = vec![];
        for func in useless_funcs {
            useless_funcs_sid.push(Sid::new(func, current_scope_id));
        }

        useless_funcs_sid
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

        let (operand_opt, operand_use) = self._optimize_expr(operand)?.as_tuple();

        // Does not change anything but just in case it does in the future
        let complete_use = ExprUsage::new_unop(operand_use.clone());

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
                        Ok(OptExpr::new(lit_expr, ExprUsage::new_empty()))
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => {
                let expr = Expr::UnOp(unop, Box::new(operand_opt));
                Ok(OptExpr::new(expr, complete_use))
            },
        }
    }

    fn optimize_binop(&mut self, binop: BinOp, left: Expr, right: Expr) -> Result<OptExpr, Error> {
        // For now, the binop optimization only optimizes the two operands, and returns the same binop if they are not both literals.
        // Arrays get operations can be optimized if the index is a literal
        // Boolean operations may be short-circuited

        let (left_opt, left_use) = self._optimize_expr(left)?.as_tuple();

        // Boolean short-circuiting -> allows to not even read the right operand
        if binop == BinOp::And {
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                let expr = Expr::Lit(Literal::Bool(false));
                return Ok(OptExpr::new(expr, left_use)); // We don't use the right operand
            }
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                let (right_opt, right_use) = self._optimize_expr(right)?.as_tuple(); // Need to read the right part here
                return Ok(OptExpr::new(right_opt, right_use)); // We don't use the left operand
            }
        } else if binop == BinOp::Or {
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                let expr = Expr::Lit(Literal::Bool(true));
                return Ok(OptExpr::new(expr, left_use)); // We don't use the right operand
            }
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                let (right_opt, right_use) = self._optimize_expr(right)?.as_tuple(); // Need to read the right part here
                return Ok(OptExpr::new(right_opt, right_use)); // We don't use the left operand
            }
        } else if binop == BinOp::Mul {
            if let Expr::Lit(Literal::Int(0)) = left_opt {
                let expr = Expr::Lit(Literal::Int(0));
                return Ok(OptExpr::new(expr, left_use)); // We don't use the right operand
            }
        }

        let (right_opt, right_use) = self._optimize_expr(right)?.as_tuple();

        if binop == BinOp::Mul {
            // also check for the right operand
            if let Expr::Lit(Literal::Int(0)) = right_opt {
                let expr = Expr::Lit(Literal::Int(0));
                return Ok(OptExpr::new(expr, right_use)); // We don't use the left operand
            }
        }

        let complete_use = ExprUsage::new_binop(left_use.clone(), right_use.clone());

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

        let opt_inner = self._optimize_expr(inner)?;
        let expr_usage = ExprUsage::new_par(opt_inner.get_usage());
        let inner_expr = opt_inner.get_expr();

        let opt_expr = OptExpr::new(inner_expr, expr_usage);

        Ok(opt_expr)

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

    fn get_call_use(&mut self, func: &Sid) -> ExprUsage {
        // Get the use of a function call
        // TODO: Support recursion, and mutual recursions
        // TODO: Once it is correctly computed once, store it somewhere to avoid recomputing it
        // TODO: Check if the function called has mutable references in its arguments. (need to implement references then)
        // -> function may modify its outer scope and therefore be required to be kept (it would be an assignment)
        
        let fn_decl = self.get_func_decl(func).unwrap();
        let (_, block_use) = self._optimize_block(fn_decl.body.clone()).unwrap().as_tuple();

        ExprUsage::from(block_use)
    }

    fn optimize_call(&mut self, ident: String, args: Arguments) -> Result<OptExpr, Error> {
        // To optimize a function call, we optimize each argument and keep the same function call.
        // Then, we use the function call usage to get the final usage of this expression.

        let func_sid = self.get_func_sid(&ident).unwrap();

        let mut args_opt = vec![];
        let mut args_use = vec![];
        for arg in args.0.iter() {
            let (arg_opt, arg_use) = self._optimize_expr(arg.clone())?.as_tuple();
            args_opt.push(arg_opt);
            args_use.push(arg_use);
        }

        let final_expr = Expr::Call(ident.clone(), Arguments(args_opt));

        // Adding the function call usage
        let call_use = ExprUsage::new_empty(); //self.get_call_use(&func_sid);
        let final_use = ExprUsage::new_call(func_sid, args_use, call_use);

        Ok(OptExpr::new(final_expr, final_use))
    }

    fn optimize_if_then_else(&mut self, cond: Expr, then_block: Block, else_block: Option<Block>) -> Result<OptExpr, Error> {
        // To optimize an if then else, we optimize the condition. If we can get a literal boolean, we can replace the if then else by the corresponding block.
        // We optimize the remaining block(s) as well.

        let (cond_opt, cond_use) = self._optimize_expr(cond)?.as_tuple();

        // If the condition is a literal, we can evaluate it and return the corresponding block.
        if let Expr::Lit(Literal::Bool(b)) = cond_opt {
            if b {
                let expr_to_opti = Expr::Block(then_block);
                let (block_opt, block_use) = self._optimize_expr(expr_to_opti)?.as_tuple();
                let final_use = block_use; // This is a simple block
                return Ok(OptExpr::new(block_opt, final_use));
            } else {
                if let Some(else_block) = else_block {
                    let expr_to_opti = Expr::Block(else_block);
                    let (block_opt, block_use) = self._optimize_expr(expr_to_opti)?.as_tuple();
                    let final_use = block_use; // This is a simple block
                    return Ok(OptExpr::new(block_opt, final_use));
                } else {
                    return Ok(OptExpr::new(Expr::get_empty_expr(), cond_use));
                }
            }
        }
        
        // Else, we optimize both blocks and return the if then else with the optimized condition.
        let (then_block_opt, then_block_use) = self._optimize_block(then_block)?.as_tuple();
        let (else_block_opt, else_block_use) = match else_block {
            Some(block) => {
                let (block_opt, block_use) = self._optimize_block(block)?.as_tuple();
                (Some(block_opt), Some(block_use))
            },
            None => (None, None),
        };

        let expr = Expr::IfThenElse(Box::new(cond_opt), then_block_opt, else_block_opt);
        let final_use = ExprUsage::new_if_then_else(cond_use, then_block_use, else_block_use);

        Ok(OptExpr::new(expr, final_use))
    }

    fn post_block_opt_process(&self, opt_block: OptBlock) -> OptExpr {
        // If the block can be removed and replaced by a simple expression, it will be a block with a single statement without a semi-colon.
        // If the block is useless, it will be empty. Therefore, we can remove it.

        let (block, block_usage) = opt_block.as_tuple();

        if block.statements.is_empty() {
            let expr = Expr::get_empty_expr(); // We return an empty expression.
            OptExpr::new(expr, ExprUsage::new_empty())
        } else if block.statements.len() == 1 && !block.semi {
            match block.statements[0].clone() {
                Statement::Expr(expr) => {
                    let expr_usage = ExprUsage::new_simple(block_usage.as_simple_usage());
                    OptExpr::new(expr, expr_usage)
                },
                Statement::While(_, _) => OptExpr::from(opt_block), // We keep the while loop
                _ => {
                    let expr = Expr::get_empty_expr(); // We return an empty expression.
                    OptExpr::new(expr, ExprUsage::new_empty())
                },
            }
        } else {
            // Else, the block can not be optimized out, so we keep it.
            OptExpr::from(opt_block)
        }
    }

    fn _optimize_expr(&mut self, expr: Expr) -> Result<OptExpr, Error> {

        match expr {
            Expr::Ident(ident) => {
                let var_sid = match self.get_var_sid(&ident){
                    Some(sid) => sid,
                    None => return Err(format!("Variable {} not defined", ident)),
                };

                let mut simple_usage = SimpleExprUsage::new();
                simple_usage.add_var_use(&var_sid);
                let expr_usage = ExprUsage::new_simple(simple_usage);
                Ok(OptExpr::new(Expr::Ident(ident), expr_usage))
            },
            Expr::Lit(lit) => Ok(OptExpr::new(Expr::Lit(lit), ExprUsage::new_empty())),
            Expr::BinOp(binop, left, right) => self.optimize_binop(binop, *left, *right),
            Expr::UnOp(unop, operand) => self.optimize_unop(unop, *operand),
            Expr::Par(inner) => self.optimize_par(*inner),
            Expr::Call(ident, args) => self.optimize_call(ident, args),
            Expr::IfThenElse(cond, then_block, else_block) => {
                self.optimize_if_then_else(*cond, then_block, else_block)
            }
            Expr::Block(block) => {
                let block_opt = self._optimize_block(block)?;

                Ok(self.post_block_opt_process(block_opt))
            }
        }
    }

    // Public function to get the optimized version of an expression
    pub fn optimize_expr(&mut self, expr: Expr) -> Result<Expr, Error> {
        let opt_expr = self._optimize_expr(expr);
        match opt_expr {
            Ok(opt_expr) => Ok(opt_expr.expr),
            Err(e) => Err(e),
        }
    }

    //?###################################################################
    //?#                              Statements                         #
    //?###################################################################

    fn update_simple_stmt_usage_booleans(&self, simple_stmt_usage: &mut SimpleStmtUsage) {
        // This modifies the given simple_stmt_usage to update its modification booleans

        let current_scope_id = self.scopes.len() - 1;

        // We shall determine whether the statement modifies an outer scope
        for assigned_var in simple_stmt_usage.assigned_vars.iter() {
            let scope_id = assigned_var.get_scope_id();

            if scope_id < current_scope_id {
                simple_stmt_usage.modify_outer_scope = true;
            } else if scope_id == current_scope_id {
                simple_stmt_usage.modify_current_scope = true;
            } else {
                unreachable!("ExprUsage with an assigned variable from a non-existing scope: {:?}", assigned_var);
            }

            if simple_stmt_usage.modify_current_scope && simple_stmt_usage.modify_outer_scope {
                break;
            }
        }
    }

    fn update_stmt_usage_booleans(&self, stmt_usage: &mut StmtUsage) {
        // This modifies the given stmt_usage to update its modification booleans
        self.update_simple_stmt_usage_booleans(&mut stmt_usage.summary);
    }

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
        let (block_opt, block_usage) = optimizer._optimize_block_with_param(fn_decl.body, is_main)?.as_tuple();
        // TODO: Do something special in case of an empty block for a function?
        // -> In this case, if we remove it, we need to identify every statement where it was used to be able
        //  to correctly update it.

        // TODO: Do something in case of a useless parameter

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
                let (expr_opt, expr_use) = match expr {
                    Some(e) => {
                        let (expr_opt, expr_use) = self._optimize_expr(e)?.as_tuple();
                        (Some(expr_opt), Some(expr_use))
                    },
                    None => (None, None),
                };

                // The variable is not defined here, but in the scope optimization code

                //? Debug
                // eprintln!("#########################");
                // eprintln!("Let statement for var: {}", ident);
                // eprintln!("Expression is: {}", expr_opt.clone().or(Some(Expr::Lit(Literal::Unit))).unwrap());
                // eprintln!("Expression summary usage: {:?}", expr_use.clone().or(Some(ExprUsage::new_empty())).unwrap().summary);

                let var_sid = self.generate_sid(&ident);
                let opt_stmt = Statement::Let(m, ident.clone(), ty, expr_opt);
                let mut stmt_use = StmtUsage::new_let(var_sid, expr_use);

                self.update_stmt_usage_booleans(&mut stmt_use);

                //? Debug
                // eprintln!("Statement summary usage: {:?}", stmt_use.summary);
                // eprintln!("#########################");

                Ok(OptStmt::new(opt_stmt, stmt_use))
            }
            Statement::Assign(ident_expr, expr) => {
                // If there is an expr, we optimize it
                let (expr_opt, expr_use) = self._optimize_expr(expr)?.as_tuple();

                // We shall as well optimize the left part in case of an array access,
                // so that the index gets optimized if it can be, and so that we can collect
                // the index computation usage.
                let (ident_expr_opt, ident_expr_use) = self._optimize_expr(ident_expr)?.as_tuple();

                let var_ident = match ident_expr_opt.clone() {
                    Expr::Ident(ident) => ident,
                    Expr::BinOp(BinOp::Get, left, _) => { // Array case
                        if let Expr::Ident(ident) = *left {
                            ident
                        } else {
                            unreachable!("Trying to assign to a non-ident in array get: {}", ident_expr_opt)
                        }
                    },
                    _ => unreachable!("Trying to assign to a non-ident: {}", ident_expr_opt),
                };

                let stmt = Statement::Assign(ident_expr_opt, expr_opt);

                let var_sid = match self.get_var_sid(&var_ident) {
                    Some(sid) => sid,
                    None => return Err(format!("Variable {} not defined", var_ident)),
                };
                let mut stmt_use = StmtUsage::new_assign(var_sid, ident_expr_use, expr_use);
                self.update_stmt_usage_booleans(&mut stmt_use);

                Ok(OptStmt::new(stmt, stmt_use))
            }
            Statement::Expr(expr) => {
                // - Expressions that are operations
                // - Expressions that are function calls which do not affect the program (no mutable references)

                let (expr_opt, expr_use) = self._optimize_expr(expr)?.as_tuple();
                let mut stmt_use = StmtUsage::new_expr(expr_use);
                self.update_stmt_usage_booleans(&mut stmt_use);

                if last_statement {
                    let stmt = Statement::Expr(expr_opt);
                    Ok(OptStmt::new(stmt, stmt_use))
                } else {
                    // If an expression modifies the current scope or the outer scope, it must be kept.
                    // Else, it can be removed

                    if stmt_use.get_modify_current_scope() || stmt_use.get_modify_outer_scope() {
                        let stmt = Statement::Expr(expr_opt); // The expression has already been optimized
                        Ok(OptStmt::new(stmt, stmt_use))
                    } else {
                        Ok(OptStmt::new(Statement::get_empty_statement(), StmtUsage::new_empty()))
                    }

                    //TODO: Remove if unnecessary
                    // match expr_opt.clone() {
                    //     Expr::Call(_, _) => {
                    //         let may_affect_prog: bool = false;

                    //         if may_affect_prog {
                    //             Ok(Statement::Expr(expr_opt))
                    //         } else {
                    //             Ok(Statement::get_empty_statement()) // We return an empty statement.
                    //         }
                    //     }
                    //     Expr::IfThenElse(_, _, _) => Ok(Statement::Expr(expr_opt)),
                    //     Expr::Block(block) => {
                    //         // If we have a block, it will have been optimized.
                    //         // If the block can be removed and replaced by a simple expression, it will be a block with a single statement without a semi-colon.
                    //         // If the block is useless, it will be empty. Therefore, we can remove it.

                    //         if block.statements.is_empty() {
                    //             Ok(Statement::get_empty_statement()) // We return an empty statement.
                    //         } else if block.statements.len() == 1 && !block.semi {
                    //             match block.statements[0].clone() {
                    //                 Statement::Expr(expr) => Ok(Statement::Expr(expr)),
                    //                 _ => Ok(Statement::get_empty_statement()), // We return an empty statement.
                    //             }
                    //         } else {
                    //             // Else, the block can not be optimized out, so we keep it.
                    //             Ok(Statement::Expr(Expr::Block(block)))
                    //         }
                    //     }
                    //     _ => Ok(Statement::get_empty_statement()), // We return an empty statement.
                    // }
                }
            }
            Statement::Fn(fn_decl) => {
                let opt_fn_decl = self.optimize_fn_declaration(fn_decl.clone())?;

                let func = self.generate_sid(&fn_decl.id.as_str());
                let mut stmt_use = StmtUsage::new_fn(func);
                self.update_stmt_usage_booleans(&mut stmt_use);

                Ok(OptStmt::new(Statement::Fn(opt_fn_decl), stmt_use))
            },
            Statement::While(cond, block) => {
                // We optimize the block away if the condition can be evaluated to false.
                let (cond_opt, cond_use) = self._optimize_expr(cond)?.as_tuple();

                if let Expr::Lit(Literal::Bool(false)) = cond_opt {
                    let stmt = Statement::get_empty_statement();
                    Ok(OptStmt::new(stmt, StmtUsage::new_empty()))
                } else {
                    // Else, we optimize the block
                    let (block_opt, block_use) = self._optimize_block(block)?.as_tuple();
                    let stmt = Statement::While(cond_opt, block_opt);

                    let mut stmt_use = StmtUsage::new_while(cond_use, block_use);
                    self.update_stmt_usage_booleans(&mut stmt_use);
                    
                    Ok(OptStmt::new(stmt, stmt_use))
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

    fn remove_assign_from_block(&self, opt_block: &OptBlock, var_to_remove: &Sid) -> OptBlock {
        // This takes an already optimized block and removes the assignments of the given variable from it
        // For instance, if we want to remove 'a' assignments:
        // {
        //    a = a + 1; // Should be removed
        //    b = b - 1;
        // }
        // Becomes
        // {
        //    b = b - 1;
        // }
        // We also update the usage accordingly, but we do not revert it yet

        // We do NOT have to think about possible assign in the block itself as these assignments will always be
        // considering outer scope variables
        // We simply have to go through all the statements and get the new ones without these assignments

        let (block, used) = opt_block.as_tuple();
        let block_scope_id = used.scope_id;

        let stmts = block.statements.clone();
        let stmts_usage = used.stmts_usage;

        let mut opt_stmts = vec![];
        for (stmt, usage) in stmts.into_iter().zip(stmts_usage.into_iter()) {
            opt_stmts.push(OptStmt::new(stmt, usage));
        }

        let mut new_opt_stmts = vec![];
        for opt_stmt in opt_stmts {
            let new_opt_stmt = self.remove_assign_from_stmt(&opt_stmt, var_to_remove);

            // Do not add empty statements
            if new_opt_stmt.get_stmt().is_empty_statement() {
                continue;
            }

            new_opt_stmts.push(new_opt_stmt);
        }

        let (new_stmts, new_stmts_usage): (Vec<Statement>, Vec<StmtUsage>) = new_opt_stmts.into_iter().map(|opt_stmt| opt_stmt.as_tuple()).unzip();

        let final_block = Block {
            statements: new_stmts,
            semi: block.semi,
        };

        //? Debug
        // eprintln!("________________________________________________________");
        // eprintln!("Block after removing assignments:");
        // eprintln!("{}", final_block);
        // eprintln!("New statements usage are:");
        // for usage in new_stmts_usage.iter() {
        //     eprintln!(" - {:?}", usage);
        // }

        let final_block_usage = self.build_block_return_usage(new_stmts_usage, block_scope_id);

        //? Debug
        // eprintln!("\nBuilt final block usage:\n{:?}", final_block_usage);
        // eprintln!("________________________________________________________");

        OptBlock::new(final_block, final_block_usage)
    }

    fn remove_assign_from_expr(&self, opt_expr: &OptExpr, var_to_remove: &Sid) -> OptExpr {
        // This takes an already optimized expression and removes the assignments of the given variable from it
        // For instance, if we want to remove 'a' assignments:
        // f(if b { a = 1; 3 } else { a = 2; 4 });
        // Becomes
        // f(if b { 3 } else { 4 });
        // We also update the usage accordingly, but we do not revert it yet

        let (expr, used) = opt_expr.as_tuple();

        if !used.get_assigned_vars().contains(var_to_remove) {
            return opt_expr.clone();
        } else {
            match expr {
                Expr::Ident(_) => {
                    // There cannot be an assignment here so we should not be able to reach this point
                    unreachable!("Trying to remove an assignment from an identifier: '{}'", expr);
                }
                Expr::Lit(_) => {
                    // There cannot be an assignment here so we should not be able to reach this point
                    unreachable!("Trying to remove an assignment from a literal: '{}'", expr);
                }
                Expr::BinOp(op, left, right) => {
                    // A BinOp can contain blocks, therefore it can contain undesired assignments
                    match used.detailed {
                        DetailedExprUsage::BinOp(left_usage, right_usage) => {
                            let left_usage = ExprUsage::new(*left_usage);
                            let right_usage = ExprUsage::new(*right_usage);

                            let left_opt_expr = OptExpr::new(*left, left_usage);
                            let right_opt_expr = OptExpr::new(*right, right_usage);

                            let left_opt_expr = self.remove_assign_from_expr(&left_opt_expr, var_to_remove);
                            let right_opt_expr = self.remove_assign_from_expr(&right_opt_expr, var_to_remove);

                            let expr_usage = ExprUsage::new_binop(left_opt_expr.get_usage(), right_opt_expr.get_usage());
                            OptExpr::new(Expr::BinOp(op, Box::new(left_opt_expr.get_expr()), Box::new(right_opt_expr.get_expr())), expr_usage)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (BinOp)"),
                    }
                }
                Expr::UnOp(op, operand) => {
                    // A UnOp can contain a block, therefore it can contain undesired assignments
                    match used.detailed {
                        DetailedExprUsage::UnOp(operand_usage) => {
                            let operand_usage = ExprUsage::new(*operand_usage);

                            let operand_opt_expr = OptExpr::new(*operand, operand_usage);

                            let operand_opt_expr = self.remove_assign_from_expr(&operand_opt_expr, var_to_remove);

                            let expr_usage = ExprUsage::new_unop(operand_opt_expr.get_usage());
                            OptExpr::new(Expr::UnOp(op, Box::new(operand_opt_expr.get_expr())), expr_usage)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (UnOp)"),
                    }
                }
                Expr::Par(inner) => {
                    match used.detailed {
                        DetailedExprUsage::Par(inner_usage) => {
                            let inner_usage = ExprUsage::new(*inner_usage);

                            let inner_opt_expr = OptExpr::new(*inner, inner_usage);

                            let inner_opt_expr = self.remove_assign_from_expr(&inner_opt_expr, var_to_remove);

                            let expr_usage = ExprUsage::new_par(inner_opt_expr.get_usage());
                            OptExpr::new(Expr::Par(Box::new(inner_opt_expr.get_expr())), expr_usage)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (Par)"),
                    }
                }
                Expr::Call(ident, args) => {
                    // Each argument from the call can contain blocks

                    match used.detailed {
                        DetailedExprUsage::Call(func, args_usage, call_usage) => {
                            let mut new_args = vec![];
                            let mut new_args_usage = vec![];
                            for (arg, arg_usage) in args.0.iter().zip(args_usage.iter()) {
                                let arg_opt_expr = OptExpr::new(arg.clone(), ExprUsage::new(arg_usage.clone()));
                                let arg_opt_expr = self.remove_assign_from_expr(&arg_opt_expr, var_to_remove);
                                new_args.push(arg_opt_expr.get_expr());
                                new_args_usage.push(arg_opt_expr.get_usage());
                            }

                            // We should also consider the function call usage
                            let func_decl = self.get_func_decl(&func).unwrap();
                            let call_opt_expr = OptExpr::new(Expr::Block(func_decl.body.clone()), ExprUsage::new(*call_usage));
                            let new_call_opt_expr = self.remove_assign_from_expr(&call_opt_expr, var_to_remove);

                            // TODO: Do something with the updated block
                            // TODO: It should be updated for every future function call

                            let expr_usage = ExprUsage::new_call(func, new_args_usage, new_call_opt_expr.get_usage());
                            OptExpr::new(Expr::Call(ident.clone(), Arguments(new_args)), expr_usage)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (Call)"),
                    }
                }
                Expr::IfThenElse(cond, then_block, else_block) => {
                    // The condition can contain blocks
                    // The blocks can contain undesired assignments

                    match used.detailed {
                        DetailedExprUsage::IfThenElse(cond_usage, then_usage, else_usage) => {
                            let cond_opt_expr = OptExpr::new(*cond, ExprUsage::new(*cond_usage));
                            let cond_opt_expr = self.remove_assign_from_expr(&cond_opt_expr, var_to_remove);

                            let then_opt_block = OptBlock::new(then_block.clone(), then_usage);
                            let then_opt_block = self.remove_assign_from_block(&then_opt_block, var_to_remove);

                            let else_opt_block = match else_block {
                                Some(block) => {
                                    let else_usage = match else_usage {
                                        Some(eu) => eu,
                                        None => unreachable!("Non coherent if-then-else usage: Else block is None while it is Some in the expression")
                                    };
                                    let opt_block = OptBlock::new(block.clone(), else_usage);
                                    let opt_block = self.remove_assign_from_block(&opt_block, var_to_remove);
                                    Some(opt_block)
                                },
                                None => None,
                            };
                            let (else_opt_block, else_usage) = else_opt_block.map_or(
                                (None, None), 
                                |b| (Some(b.get_block()), Some(b.get_usage()))
                            );

                            let final_expr = Expr::IfThenElse(Box::new(cond_opt_expr.get_expr()), then_opt_block.get_block(), else_opt_block);
                            let final_usage = ExprUsage::new_if_then_else(cond_opt_expr.get_usage(), then_opt_block.get_usage(), else_usage);
                            OptExpr::new(final_expr, final_usage)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (IfThenElse)"),
                    }
                }
                Expr::Block(block) => {
                    match used.detailed {
                        DetailedExprUsage::Block(block_usage) => {
                            let opt_block = OptBlock::new(block.clone(), block_usage);
                            let opt_block = self.remove_assign_from_block(&opt_block, var_to_remove);

                            // Applying the post process on blocks
                            self.post_block_opt_process(opt_block)
                        },
                        _ => unreachable!("ExprUsage does not match the expression (Block)"),
                    }
                }
            }
        }
    }

    fn remove_assign_from_stmt(&self, opt_stmt: &OptStmt, var_to_remove: &Sid) -> OptStmt {
        // This takes an already optimized statement and removes the assignments of the given variable from it
        // For instance, if we want to remove 'a' assignments:
        // a = 1; ----> empty statement
        // Or even
        // while b {
        //    a = a + 1; // Should be removed
        //    b = b - 1;
        // }
        // Becomes
        // while b {
        //    b = b - 1;
        // }
        // We also update the usage accordingly, but we do not revert it yet

        let (stmt, used) = opt_stmt.as_tuple();

        //? Debug
        // eprintln!("Removing unnecessary var {:?} from stmt:\n{}", var_to_remove, opt_stmt.get_stmt());
        // eprintln!("It has the following usage: {:?}", opt_stmt.get_usage());

        if !used.get_assigned_vars().contains(var_to_remove) {
            return opt_stmt.clone();
        } else {
            match stmt {
                Statement::Let(m, var_name, ty, expr) => {
                    let let_sid: Sid;
                    let opt_expr = match expr {
                        Some(e) => {
                            let opt_expr = match used.detailed {
                                DetailedStmtUsage::Let(sid, expr_usage) => {
                                    let_sid = sid; // We collect the correct Sid
                                    let e_usage = match expr_usage {
                                        Some(eu) => eu,
                                        None => unreachable!("Non coherent let usage: Expression usage is None while it is Some in the statement")
                                    };

                                    OptExpr::new(e.clone(), e_usage)
                                },
                                _ => unreachable!("StmtUsage does not match the statement (Let)"),
                            };

                            //? Debug
                            // eprintln!(" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ");
                            // eprintln!("opt_expr before is:\n{:?}", opt_expr);

                            let opt_expr = self.remove_assign_from_expr(&opt_expr, var_to_remove);

                            //? Debug
                            // eprintln!(" ------------> Expression in Let is:\n{}", opt_expr.get_expr());
                            // eprintln!(" ------------> Expression Usage in Let is:\n{:?}", opt_expr.get_usage().summary);
                            // eprintln!(" ------------> Detailed Expression Usage in Let is:\n{:?}", opt_expr.get_usage().detailed);

                            Some(opt_expr)
                        }
                        None => {
                            match used.detailed {
                                DetailedStmtUsage::Let(sid, _) => {
                                    let_sid = sid; // We collect the correct Sid
                                },
                                _ => unreachable!("StmtUsage does not match the statement (Let)"),
                            };
                            None
                        },
                    };

                    let (expr, expr_usage) = opt_expr.as_ref().map_or(
                        (None, None),
                        |opt_expr| (Some(opt_expr.get_expr()), Some(opt_expr.get_usage()))
                    );
                    let stmt_usage = StmtUsage::new_let(let_sid, expr_usage);
                    let stmt = Statement::Let(m, var_name.clone(), ty, expr);

                    OptStmt::new(stmt, stmt_usage)
                }
                Statement::Assign(left_expr, right_expr) => {
                    // For an assignment, we have two possibilities
                    // Either the statement is an immediate assign, or it is an array assignment.
                    // In both cases, we can treat the two sides the same way to try to remove any statement
                    // that would do the undesired assignment

                    // For instance, we can think at some think like this:
                    // a[{b = 1; 2}] = {b = 3; 4};   (where we would want to remove b)

                    // But it could also be a very simple case where:
                    // b = 2;

                    match used.detailed {
                        DetailedStmtUsage::Assign(sid, left_usage, right_usage) => {
                            if sid == var_to_remove.clone() {
                                // We shall remove this assignment
                                let stmt_usage = StmtUsage::new_empty();
                                let stmt = Statement::get_empty_statement();

                                return OptStmt::new(stmt, stmt_usage);
                            } else {
                                // The unwanted variable is hidden in expressions
                                let left_opt_expr = OptExpr::new(left_expr.clone(), left_usage);
                                let right_opt_expr = OptExpr::new(right_expr.clone(), right_usage);
    
                                let left_opt_expr = self.remove_assign_from_expr(&left_opt_expr, var_to_remove);
                                let right_opt_expr = self.remove_assign_from_expr(&right_opt_expr, var_to_remove);
    
                                let stmt_usage = StmtUsage::new_assign(sid, left_opt_expr.get_usage(), right_opt_expr.get_usage());
                                let stmt = Statement::Assign(left_opt_expr.get_expr(), right_opt_expr.get_expr());
    
                                OptStmt::new(stmt, stmt_usage)
                            }
                        },
                        _ => unreachable!("StmtUsage does not match the statement (Assign)"),
                    }
                }
                Statement::While(cond, block) => {
                    let (opt_cond, opt_block) = match used.detailed {
                        DetailedStmtUsage::While(cond_usage, block_usage) => {
                            let opt_cond = OptExpr::new(cond.clone(), cond_usage);
                            let opt_block = OptBlock::new(block.clone(), block_usage);
                            (opt_cond, opt_block)
                        },
                        _ => unreachable!("StmtUsage does not match the statement (While)"),
                    };
                    let opt_cond = self.remove_assign_from_expr(&opt_cond, var_to_remove);
                    let opt_block = self.remove_assign_from_block(&opt_block, var_to_remove);

                    let stmt_usage = StmtUsage::new_while(opt_cond.get_usage(), opt_block.get_usage());
                    let stmt = Statement::While(opt_cond.get_expr(), opt_block.get_block());

                    OptStmt::new(stmt, stmt_usage)
                }
                Statement::Expr(expr) => {
                    let opt_expr = match used.detailed {
                        DetailedStmtUsage::Expr(expr_usage) => OptExpr::new(expr, expr_usage),
                        _ => unreachable!("StmtUsage does not match the statement (Expr)"),
                    };
                    let opt_expr = self.remove_assign_from_expr(&opt_expr, var_to_remove);
                    
                    let stmt_usage = StmtUsage::new_expr(opt_expr.get_usage());
                    let stmt = Statement::Expr(opt_expr.get_expr());

                    OptStmt::new(stmt, stmt_usage)
                }
                Statement::Fn(_) => {
                    // There no modification to do here
                    opt_stmt.clone()
                }
            }
        }
    }

    // This recursive function removes useless redundant let statements and their assignments
    // For instance:
    // let var = {
    //     let c = 2; // Not detected as useless (Needs 3 iterations)
    //     let a = 1 - c; // Not detected as useless (Needs 2 iterations)
    //     let b = a + c; // Detected as useless (From the 1st iteration)
    //     let a = 3;
    //     let b: i32; // Detected as useless (From the 1st iteration)
    //     b = 3; // Detected as useless (From the 1st iteration)
    //     let b = a + 4;
    //     b
    // };
    fn optimize_block_redundant_vars(&mut self, init_statements: Vec<OptStmt>) -> Vec<OptStmt> {

        // Keep a copy of the scopes to go back to it in the recursive call
        let init_scopes = self.scopes.clone();

        //? Debug
        // eprintln!("Optimizing block redundant vars");
        // eprintln!("Initial scopes:\n{}", self.pretty_string_scopes());

        // We proceed a first redundant removal
        let mut opt_statements: Vec<OptStmt> = vec![];
        let nb_stmts = init_statements.len();

        for i in 0..nb_stmts {
            let opt_stmt = init_statements[i].clone();
            let (stmt, used) = opt_stmt.as_tuple();

            //? Debug
            // eprintln!("\n~~~\nApplying stmt uses of:\n{}\n{:?}", opt_stmt.get_stmt(), opt_stmt.get_usage());

            // Update the used variables and functions. (No need to compute it again)
            self.apply_stmt_usage(&used);

            // If the statement is a let statement, we shall define the variable
            // However, if it already exists in this same scope, we shall first verify whether it is useful or not.
            // If it is not used, it can be removed
            if let Statement::Let(_, ident, _, _) = stmt.clone() {
                let current_scope = self.scopes.last().unwrap();

                //? Debug
                // eprintln!("--> Let statement is: {}", opt_stmt.get_stmt());
                // eprintln!("--> Current scope is:\n{}", current_scope);

                if let Some(uses) = current_scope.vars.get(&ident) {
                    if *uses == 0 {
                        // The previously defined variable is not of use.
                        // We shall find and remove its definition and assignments from the already optimized statements
                        // We should start by the end of the list to remove the correct one

                        let sid = self.generate_sid(&ident);
                        let nb_opt_stmts = opt_statements.len();

                        //? Debug
                        // eprintln!("Removing unnecessary '{}' var in already pushed statements...", ident);

                        for (i, ostmt) in opt_statements.clone().iter().rev().enumerate() {
                            let (stmt, used) = ostmt.as_tuple();

                            if used.get_assigned_vars().contains(&sid) {
                                // We have found a statement where this variable is assigned a value
                                // We should therefore get the updated statement without this assignment
                                // Be aware that it is not necessarily an assign statement, it can also be a
                                // while statement, or a block expr or if-then-else expr

                                //? Debug
                                // eprintln!("Found statement containing the redundant var: {}", stmt);
                                // eprintln!("Usage is:\n{:?}", used);

                                let new_ostmt = self.remove_assign_from_stmt(&ostmt, &sid);
                                let (new_stmt, new_used) = new_ostmt.as_tuple();

                                //? Debug
                                // eprintln!("New statement is now: {}", new_stmt);
                                // eprintln!("And its new usage is:\n{:?}", new_used);

                                if new_stmt.is_empty_statement() {
                                    // The statement should be removed
                                    opt_statements.remove(nb_opt_stmts - 1 - i);
                                    // We shall revert the effect of this statement
                                    self.revert_stmt_usage(&used);
                                } else {
                                    // The statement should be replaced
                                    opt_statements[nb_opt_stmts - 1 - i] = new_ostmt;
                                    let reverting_usage = used.get_subtracted_stmt_usage(&new_used);
                                    self.revert_stmt_usage(&reverting_usage);
                                }
                            } else if used.get_defined_var() == Some(sid.clone()) {
                                // We have found the definition statement, it is the end
                                opt_statements.remove(nb_opt_stmts - 1 - i);
                                // We shall revert the effect of this statement
                                self.revert_stmt_usage(&used);
                                break;
                            }
                        }
                    }
                }

                self.define_var(ident.clone());
            }

            opt_statements.push(opt_stmt);
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

    fn build_block_return_usage(&self, stmt_usages: Vec<StmtUsage>, block_scope_id: usize) -> BlockUsage {
        // The ExprUsage of a returning block is extracted from the concatenation of every StmtUsage
        let mut block_stmt_usage = SimpleStmtUsage::new();
        let stmt_simple_usages = stmt_usages.iter().map(|su| su.summary.clone()).collect();
        block_stmt_usage.concatenate_vec(stmt_simple_usages);

        let mut simple_expr_result = SimpleExprUsage::new();

        // We should extract every variable and functions uses except for the ones that are defined in the block (scope id == last scope id)

        for (var, uses) in block_stmt_usage.vars.iter() {
            let scope_id = var.get_scope_id();
            if scope_id < block_scope_id {
                simple_expr_result.add_var_uses(var, *uses);
            }
        }

        for (func, uses) in block_stmt_usage.funcs.iter() {
            let scope_id = func.get_scope_id();
            if scope_id < block_scope_id {
                simple_expr_result.add_func_uses(func, *uses);
            }
        }

        // Finally, we should add the assigned variables which are from the outer scope as well

        for assigned_var in block_stmt_usage.assigned_vars.iter() {
            let scope_id = assigned_var.get_scope_id();
            if scope_id < block_scope_id {
                simple_expr_result.add_assigned_var(assigned_var);
            }
        }

        BlockUsage::new(simple_expr_result, block_scope_id, stmt_usages)
    }

    fn _optimize_block(&mut self, block: Block) -> Result<OptBlock, Error> {
        self._optimize_block_with_param(block, false)
    }

    fn _optimize_block_with_param(&mut self, block: Block, is_main: bool) -> Result<OptBlock, Error> {
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

        //? Debug
        // eprintln!("Block initial scopes:\n{}", self.pretty_string_scopes());
        // eprintln!("Optimizer initial scopes:\n{}", optim_copy.pretty_string_scopes());

        let mut opt_statements: Vec<OptStmt> = vec![];
        let mut block_affects_outer = false;

        let block_scope_id = self.scopes.len() - 1;

        // First optimization of each statement
        let nb_stmts = block.statements.len();
        for i in 0..nb_stmts {
            let statement = block.statements[i].clone();
            let last_stmt = (i == nb_stmts - 1) && !block.semi; // To avoid optimizing out the return value of a block

            let statement_opt = self.optimize_statement(statement.clone(), last_stmt)?;
            let (stmt, usage) = statement_opt.as_tuple();

            //? Debug
            // eprintln!(" - - - - - - - - - - - - - - - - - - ");
            // eprintln!("Optimized statement is: {}", stmt);
            // eprintln!("Statement summary usage: {:?}", usage.summary);

            // Skip the statement if it is useless
            if stmt.is_empty_statement() {
                continue;
            }

            // Updates the uses of variables and functions
            block_affects_outer = block_affects_outer || usage.get_modify_outer_scope();
            self.apply_stmt_usage(&usage);

            // If the statement is a let statement, we shall define the variable
            if let Statement::Let(_, ident, _, _) = stmt {
                self.define_var(ident.clone());
            }

            opt_statements.push(statement_opt);
        }

        // Check if the block is useless -> if it does not return any value + does not affect the outer scope
        // The main function can not be optimized out this way though
        if !is_main && block.semi && !block_affects_outer {
            // We remove the block scope
            self.remove_scope();

            // We return an empty block
            let b = Block::get_empty_block();
            let b_usage = BlockUsage::new_empty(block_scope_id);
            return Ok(OptBlock::new(b, b_usage));
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

        let mut useless_vars = self.get_current_scope_useless_vars();

        //? Debug
        // eprintln!("############################");
        // eprintln!("Block Optimization has reached final optimization step (variables removal)");
        // eprintln!("Useless vars: {:?}", useless_vars);
        // let _current_block = Block {
        //     statements: opt_statements.iter().map(|opt_stmt| opt_stmt.get_stmt().clone()).collect(),
        //     semi: block.semi,
        // };
        // eprintln!("Current block:\n{}", _current_block);
        // eprintln!("Here are the summary of each remaining stmt usage from the block:");
        // for opt_stmt in opt_statements.iter() {
        //     let (_, used) = opt_stmt.as_tuple();
        //     eprintln!("{:?}", used.summary);
        // }
        // eprintln!("############################");
        
        let mut _temp_i = 0;
        while !useless_vars.is_empty() {
            let mut new_opt_statements: Vec<OptStmt> = vec![];

            // We shall go through the statements in a reverse order to avoid removing the wrong definition of a var
            // For example:
            // let a = 1; <--- a is useful
            // let b = f(a);
            // let a = 0; <--- This one is not useful and this one should be removed
        
            for opt_stmt in opt_statements.iter().rev() {
                let (stmt, used) = opt_stmt.as_tuple();

                //? Debug
                // eprintln!("         ----------          ");
                // eprintln!("Processing opt statement:\n{}", stmt);
                // eprintln!("Statement summary usage:\n{:?}", used.summary);
                // eprintln!("Statement detailed usage:\n{:?}", used.detailed);

                let mut replacing_opt_stmt = opt_stmt.clone();
                let (_, mut replacing_used) = replacing_opt_stmt.as_tuple();

                // Progressively remove every useless variable assignment from this statement (in case of blocks)
                for sid in used.get_assigned_vars() {
                    if useless_vars.contains(&sid) {
                        // Assigning a value to a useless var
                        // We should get the updated statement without the useless assignment(s)
                        // Be aware that it is not necessarily an assign statement, it can also be a
                        // while statement, or a block expr or if-then-else expr

                        let new_opt_stmt = self.remove_assign_from_stmt(&replacing_opt_stmt, &sid);
                        let (new_stmt, new_used) = new_opt_stmt.as_tuple();

                        if new_stmt.is_empty_statement() {
                            // The statement should not be pushed
                            // We can break the loop here as there cannot be any remaining assignments in an empty statement
                            // We shall also revert the effect of this statement
                            self.revert_stmt_usage(&replacing_used);
                            replacing_used = StmtUsage::new_empty();
                            replacing_opt_stmt = OptStmt::new_empty();
                            break;
                        } else {
                            // The original statement should be replaced by the new one
                            // We should therefore update the uses accordingly
                            let reverting_usage = replacing_used.get_subtracted_stmt_usage(&new_used);
                            self.revert_stmt_usage(&reverting_usage);

                            replacing_used = new_used;
                            replacing_opt_stmt = new_opt_stmt;
                        }
                    }
                }
                
                // If the statement defines a useless variable, we shall remove it
                if let Some(var_id) = &replacing_used.get_defined_var() {
                    if useless_vars.contains(&var_id) {
                        // We revert the effect of this statement
                        self.revert_stmt_usage(&replacing_used);
                        // We remove this variable from the current scope
                        self.remove_var(var_id);

                        // We remove this variable from the useless variables
                        let useless_index = useless_vars.iter().position(|x| x == var_id).unwrap();
                        useless_vars.remove(useless_index);

                        // We replace the statement by an empty one
                        let replacing_stmt = Statement::get_empty_statement();
                        replacing_used = StmtUsage::new_empty();
                        replacing_opt_stmt = OptStmt::new_empty();
                    }
                }
                
                // Check if the statement is not empty
                if !replacing_opt_stmt.get_stmt().is_empty_statement() {
                    
                    //? Debug
                    // eprintln!("         ########            ");
                    // eprintln!("Initial opt statement was the following:\n{}", stmt);
                    // eprintln!("Initial statement summary usage:\n{:?}", used.summary);

                    // eprintln!("New opt statement is the following:\n{}", replacing_opt_stmt.get_stmt());
                    // eprintln!("New statement summary usage:\n{:?}", replacing_used.summary);

                    new_opt_statements.push(replacing_opt_stmt);
                }
            }
        
            // We shall reverse the order of statements to get back to the correct block order
            opt_statements = new_opt_statements;
            opt_statements.reverse();
            useless_vars = self.get_current_scope_useless_vars();
            
            _temp_i += 1;
            if _temp_i > 100 {
                panic!("Infinite loop in variables optimization: _temp_i = {}", _temp_i);
            }
        }

        //? --- Function optimizations

        let mut useless_funcs = self.get_current_scope_useless_funcs();

        //? Debug
        // eprintln!("############################");
        // eprintln!("Block optimization before functions removal");
        // let _current_block = Block {
        //     statements: opt_statements.iter().map(|opt_stmt| opt_stmt.get_stmt().clone()).collect(),
        //     semi: block.semi,
        // };
        // eprintln!("Current block:\n{}", _current_block);
        // eprintln!("Useless funcs: {:?}", useless_funcs);
        // eprintln!("Current scopes:\n{}", self.pretty_string_scopes());
        // eprintln!("Here are the summary of each remaining stmt usage from the block:");
        // for opt_stmt in opt_statements.iter() {
        //     let (_, used) = opt_stmt.as_tuple();
        //     eprintln!("{:?}", used.summary);
        // }
        // eprintln!("############################");

        let mut _temp_i = 0;
        while !useless_funcs.is_empty() {

            // Here we don't have to bother about the order since a function with a given name can only be defined once in some scope

            let mut new_opt_statements: Vec<OptStmt> = vec![];
        
            for opt_stmt in opt_statements.iter() {
                let (stmt, used) = opt_stmt.as_tuple();

                if let Some(fn_id) = &used.get_defined_func() {
                    if useless_funcs.contains(fn_id) {
                        // We do not add the statement to the final statements
                        // We revert the effect of this statement
                        self.revert_stmt_usage(&used);
                        // We remove this function from the current scope
                        self.remove_func(fn_id);
                    } else {
                        new_opt_statements.push(opt_stmt.clone());
                    }
                } else {
                    new_opt_statements.push(opt_stmt.clone());
                }
            }
        
            opt_statements = new_opt_statements;
            useless_funcs = self.get_current_scope_useless_funcs();
            
            _temp_i += 1;
            if _temp_i > 100 {
                panic!("Infinite loop in function optimization: _temp_i = {}", _temp_i);
            }
        }

        // Building the final block

        let (final_statements, stmt_usages) = opt_statements.iter().map(|opt_stmt| opt_stmt.as_tuple()).unzip();

        let opt_block = Block {
            statements: final_statements,
            semi: block.semi,
        };

        // Building the final expr usage
        let final_usage = self.build_block_return_usage(stmt_usages, block_scope_id);

        //? Debug
        // eprintln!("############################");
        // eprintln!("Block optimization finished!");
        // eprintln!("---> Final block:\n{}", opt_block);
        // eprintln!("---> Final scopes:\n{}", self.pretty_string_scopes());
        // eprintln!("---> Final return usage:\n{:?}", final_usage.return_usage);
        // eprintln!("---> Final detailed usage:\n{:?}", final_usage.stmts_usage);
        // eprintln!("############################");

        // Remove the block scope
        self.remove_scope();

        Ok(OptBlock::new(opt_block, final_usage))
    }

    // Public function to get the optimized version of a block
    pub fn optimize_block(&mut self, block: Block) -> Result<Block, Error> {
        let opt_block = self._optimize_block(block);
        match opt_block {
            Ok(opt_block) => Ok(opt_block.block),
            Err(e) => Err(e),
        }
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
                let main_sid = self.get_func_sid(&fn_decl.id).unwrap();
                self.use_func(&main_sid);
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

            let fn_sid = self.get_func_sid(&fn_decl.id).unwrap();
            if self.get_func_uses(&fn_sid).unwrap() > 0 {
                final_opt_fn_decls.push(fn_decl.clone());
            }
        }

        Ok(Prog(opt_fn_decls))
    }
}
