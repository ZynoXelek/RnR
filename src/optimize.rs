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
pub struct Scope {
    functions: HashMap<String, Uses>, // Tracks the defined functions and their number of uses
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

    //* Functions

    pub fn define_func(&mut self, fn_name: String) {
        if let Some(uses) = self.functions.get_mut(&fn_name) {
            unreachable!("Function {} is already defined", fn_name); // Unreachable since the type checker will have already checked it.
        } else {
            self.functions.insert(fn_name, 0);
        }
    }

    pub fn use_func(&mut self, fn_name: String) {
        if let Some(uses) = self.functions.get_mut(&fn_name) {
            *uses += 1;
        } else {
            unreachable!("Function {} is not defined", fn_name); // Unreachable since the type checker will have already checked it.
        }
    }

    pub fn get_func_uses(&self, fn_name: &str) -> Uses {
        if let Some(uses) = self.functions.get(fn_name) {
            *uses
        } else {
            unreachable!("Function {} is not defined", fn_name); // Unreachable since the type checker will have already checked it.
        }
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
        for (fn_name, uses) in self.functions.iter() {
            s.push_str(&format!("|\t{}: {}\n", fn_name, uses));
        }

        write!(f, "{}", s)
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
        let result = optimizer.optimize_expr(self.clone());
        // let result = optimizer.get_result().get_expr();

        Ok(result?)
    }
}

impl Optimize<Block> for Block {
    fn optimize(&self) -> Result<Block, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_block(self.clone());
        // let result = optimizer.get_result().get_block();

        Ok(result?)
    }
}

impl Optimize<Prog> for Prog {
    fn optimize(&self) -> Result<Prog, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_prog(self.clone());
        // let result = optimizer.get_result().get_prog();

        Ok(result?)
    }
}

pub struct Optimizer {
    // The optimizer will generate an optimized version of the AST by erasing useless parts of code,
    // and simplifying expressions

    // The optimizer keeps tracks of the scopes to determine whether a function/var is used at some point.
    scopes: Vec<Scope>,
}


// Define a private type for the return value of the update_used_vars_and_funcs function
struct UsedVarsFuncs {
    vars: Vec<String>, // The list of the names of each variable used in the statement (used means read, not written)
    funcs: Vec<String>, // The list of the names of each function called in the statement
    modify_outer_scope: bool, // Whether the statement modifies a variable from an outer scope
}

impl UsedVarsFuncs {
    fn new() -> Self {
        Self {
            vars: vec![],
            funcs: vec![],
            modify_outer_scope: false,
        }
    }

    fn concatenates(&mut self, other: UsedVarsFuncs) {
        self.vars.extend(other.vars);
        self.funcs.extend(other.funcs);
        self.modify_outer_scope = self.modify_outer_scope || other.modify_outer_scope;
    }
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
        //TODO: Add check to remove any useless variables/functions
        self.scopes.pop();
    }

    fn define_var(&mut self, var: String) {
        //TODO: Add check to remove a same var that would not be used

        if let Some(scope) = self.scopes.last_mut() {
            scope.define_var(var);
        } else {
            unreachable!("No scope to define variable {}", var); // Unreachable since the type checker will have already checked it.
        }
    }

    fn get_var_uses(&self, var: &str) -> Option<Uses> {
        for scope in self.scopes.iter().rev() {
            if let Some(uses) = scope.vars.get(var) {
                return Some(*uses);
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

    fn define_func(&mut self, fn_name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define_func(fn_name);
        } else {
            unreachable!("No scope to define function {}", fn_name); // Unreachable since the type checker will have already checked it.
        }
    }

    fn get_func_uses(&self, fn_name: &str) -> Option<Uses> {
        for scope in self.scopes.iter().rev() {
            if let Some(uses) = scope.functions.get(fn_name) {
                return Some(*uses);
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

    fn optimize_unop(&mut self, unop: UnOp, operand: Expr) -> Result<Expr, Error> {
        // For now, the unop optimization only optimizes the operand, and returns the same unop if it is not a literal.

        let operand_opt = self.optimize_expr(operand)?;

        // If the operand is the same unop for negations, we can remove them.
        if unop == UnOp::Neg {
            if let Expr::UnOp(UnOp::Neg, inner_inner) = operand_opt {
                return Ok(*inner_inner);
            }
        } else if unop == UnOp::Bang {
            if let Expr::UnOp(UnOp::Bang, inner_inner) = operand_opt {
                return Ok(*inner_inner);
            }
        }

        // If the operand is a literal, we can evaluate the operation, else we just return the unop with the optimized operand.
        match operand_opt {
            Expr::Lit(lit) => {
                // Use the vm to evaluate it
                let val = lit.into();
                let result = unop.eval(val);

                // Now we need to convert it as an Expr
                match result {
                    Ok(val) => {
                        let lit = Literal::from(val);
                        Ok(Expr::Lit(lit))
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => Ok(Expr::UnOp(unop, Box::new(operand_opt))),
        }
    }

    fn optimize_binop(&mut self, binop: BinOp, left: Expr, right: Expr) -> Result<Expr, Error> {
        // For now, the binop optimization only optimizes the two operands, and returns the same binop if they are not both literals.
        // Arrays get operations can be optimized if the index is a literal
        // Boolean operations may be short-circuited

        let left_opt = self.optimize_expr(left)?;
        let right_opt = self.optimize_expr(right)?;

        // Boolean short-circuiting
        if binop == BinOp::And {
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                return Ok(Expr::Lit(Literal::Bool(false)));
            }
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                return Ok(right_opt);
            }
        } else if binop == BinOp::Or {
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                return Ok(Expr::Lit(Literal::Bool(true)));
            }
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                return Ok(right_opt);
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
                        Ok(Expr::Lit(lit))
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => Ok(Expr::BinOp(binop, Box::new(left_opt), Box::new(right_opt))),
        }
    }

    fn optimize_par(&mut self, inner: Expr) -> Result<Expr, Error> {
        // For now, the Par optimization only optimizes the inner expression, and returns it without the parenthesis.

        let inner_opt = self.optimize_expr(inner)?;

        Ok(inner_opt)

        //? We may want to keep the outer parenthesis for the inner expression if it is an operation. In this case, uncomment the following lines:
        // // If the inner part is not an operation, we can remove the parenthesis.
        // // TODO: Check for priority of operations to remove useless parenthesis
        // match inner_opt {
        //     Expr::BinOp(_, _, _) | Expr::UnOp(_, _) => Ok(Expr::Par(Box::new(inner_opt))), // We keep the parenthesis.
        //     _ => Ok(inner_opt), // We can remove the parenthesis.
        // }
    }

    fn optimize_call(&mut self, ident: String, args: Arguments) -> Result<Expr, Error> {
        // To optimize a function call, we optimize each argument and keep the same function call.

        let mut args_opt = vec![];
        for arg in args.0.iter() {
            let arg_opt = self.optimize_expr(arg.clone())?;
            args_opt.push(arg_opt);
        }

        Ok(Expr::Call(ident, Arguments(args_opt)))
    }

    fn optimize_ifthenelse(&mut self, cond: Expr, then_block: Block, else_block: Option<Block>) -> Result<Expr, Error> {
        // To optimize an if then else, we optimize the condition. If we can get a literal boolean, we can replace the if then else by the corresponding block.
        // We optimize the remaining block(s) as well.

        let cond_opt = self.optimize_expr(cond)?;

        // If the condition is a literal, we can evaluate it and return the corresponding block.
        if let Expr::Lit(Literal::Bool(b)) = cond_opt {
            if b {
                let then_block_opt = self.optimize_block(then_block)?;
                return Ok(Expr::Block(then_block_opt));
            } else {
                if let Some(else_block) = else_block {
                    let else_block_opt = self.optimize_block(else_block)?;
                    return Ok(Expr::Block(else_block_opt));
                } else {
                    return Ok(Expr::Block(Block {
                        statements: vec![], // Generate an empty block.
                        semi: false,
                    }));
                }
            }
        }
        
        // Else, we optimize both blocks and return the if then else with the optimized condition.
        let then_block_opt = self.optimize_block(then_block)?;
        let else_block_opt = match else_block {
            Some(block) => Some(self.optimize_block(block)?),
            None => None,
        };

        Ok(Expr::IfThenElse(Box::new(cond_opt), then_block_opt, else_block_opt))
    }

    pub fn optimize_expr(&mut self, expr: Expr) -> Result<Expr, Error> {

        match expr {
            Expr::Ident(ident) => Ok(Expr::Ident(ident)), //TODO: Implement identifier optimizations?
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::BinOp(binop, left, right) => self.optimize_binop(binop, *left, *right),
            Expr::UnOp(unop, operand) => self.optimize_unop(unop, *operand),
            Expr::Par(inner) => self.optimize_par(*inner),
            Expr::Call(ident, args) => self.optimize_call(ident, args),
            Expr::IfThenElse(cond, then_block, else_block) => {
                self.optimize_ifthenelse(*cond, then_block, else_block)
            }
            Expr::Block(block) => {
                let opt_block = self.optimize_block(block);
                match opt_block {
                    Ok(b) => Ok(Expr::Block(b)),
                    Err(e) => Err(e),
                }
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

        // Then we can optimize its block
        let block_opt = optimizer.optimize_block(fn_decl.body)?;

        Ok(FnDeclaration {
            id: fn_decl.id,
            parameters: fn_decl.parameters,
            ty: fn_decl.ty,
            body: block_opt,
        })
    }

    fn optimize_statement(&mut self, statement: Statement, last_statement: bool) -> Result<Statement, Error> {
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
        let mut new_funcs: Vec<FnDeclaration> = Vec::new();

        for stmt in block.statements.iter() {
            match stmt {
                Statement::Fn(decl) => {
                    self.define_func(decl.id.clone());
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
                let mut used_vars_funcs = UsedVarsFuncs::new();
                used_vars_funcs.vars.push(ident.clone());
                
                used_vars_funcs
            }
            Expr::Lit(_) => {
                UsedVarsFuncs::new()
            }
            Expr::BinOp(_, left, right) => {
                let mut left_used = self.get_used_in_expr(left);
                let right_used = self.get_used_in_expr(right);

                left_used.concatenates(right_used);
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
                used_vars_funcs.funcs.push(ident.clone());

                for arg in args.0.iter() {
                    let arg_uses = self.get_used_in_expr(arg);
                    used_vars_funcs.concatenates(arg_uses);
                }

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

                // Then, we get the used variables and functions in the expression
                let mut used_vars_funcs = self.get_used_in_expr(expr);

                used_vars_funcs.modify_outer_scope = used_vars_funcs.modify_outer_scope || modify_outer_scope;

                used_vars_funcs
            }
            Statement::Expr(expr) => {
                self.get_used_in_expr(expr)
            }
            Statement::Fn(decl) => {
                // A function declaration does not use anything
                UsedVarsFuncs::new()
            }
            Statement::Let(_, _, _, expr) => {
                match expr {
                    Some(e) => {
                        self.get_used_in_expr(e)
                    }
                    None => UsedVarsFuncs::new()
                }
            }
            Statement::While(cond, _) => {
                // We don't need to bother about the block since it has already been processed the same way
                self.get_used_in_expr(cond)
            }
        }
    }

    pub fn optimize_block(&mut self, block: Block) -> Result<Block, Error> {
        // Optimizing blocks should remove any useless statements, and simplify useful ones.
        // If a block does not affect the program, it should be removed.
        // If it returns a simple literal or variable or operation, and the previous statements do not affect the program or anything, it should be replaced by the return value.
        // In addition, if it respects the previous description, and does not return any value, it should simply be removed.

        // What are useless statements, block relative?
        // - Let statements that are not used.
        // - Function declarations that are not used.

        let mut opt_statements: Vec<Statement> = vec![];

        // First define the block scope
        self.add_scope();

        // Then scan the block for function definitions
        self.define_block_functions(&block);

        //? Debug
        eprintln!("Optimizing block:\n{}", block);
        eprintln!("Scopes at the beginning of the block:");
        self.pretty_print_scopes();

        let mut block_affects_outer = false;

        let nb_stmts = block.statements.len();
        for i in 0..nb_stmts {
            let statement = block.statements[i].clone();
            let last_stmt = i == nb_stmts - 1;

            let statement_opt = self.optimize_statement(statement.clone(), last_stmt)?;

            // Skip the statement if it is useless
            if statement_opt == Statement::get_empty_statement() {
                continue;
            }

            // If the statement has not been optimized out yet, we can consider the variables/functions used.
            let modifications = self.get_used_vars_and_funcs(&statement_opt);

            // Updates the uses of variables and functions
            for var in modifications.vars.iter() {
                self.use_var(var.clone());
            }
            for func in modifications.funcs.iter() {
                self.use_func(func.clone());
            }
            block_affects_outer = block_affects_outer || modifications.modify_outer_scope;

            // If the statement is a let statement, we shall define the variable
            if let Statement::Let(_, ident, _, _) = statement_opt.clone() {
                self.define_var(ident.clone());
            }

            opt_statements.push(statement_opt);
        }

        let opt_block: Block;

        // More treatments
        // TODO: If there is an assignment on an unused variable, it should be removed

        // TODO: check if the block is useless or not. If it is, we remove it by returning an empty block
        // If it can be optimized to a single statement which is returned, we replace it with a block with this single statement.

        opt_block = Block {
            statements: opt_statements,
            semi: block.semi,
        };
        
        //? Debug
        eprintln!("Finished optimization of block:\n{}", block);
        eprintln!("Scopes at the end of the block:");
        self.pretty_print_scopes();

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
            self.define_func(fn_decl.id.clone());

            if fn_decl.id == "main" {
                // The main function automatically has 1 use
                self.use_func("main".to_string());
            }
        }

        // Then optimize each of them
        for fn_decl in prog.0.iter() {
            let fn_decl_opt = self.optimize_fn_declaration(fn_decl.clone())?;
            opt_fn_decls.push(fn_decl_opt);
        }

        // TODO: Remove unused functions
        // Be careful, since the main function does not have a 'real' impact, it should not be optimized by removing unused variables or anything.

        Ok(Prog(opt_fn_decls))
    }
}
