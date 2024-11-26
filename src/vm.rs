use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Parameters, Prog, Statement, UnOp, Type,
};
use crate::common::Eval;
use crate::error::*;
use crate::intrinsics::*;

//?#################################################################################################
//?#                                                                                               #
//?#                                            Val Type                                           #
//?#                                                                                               #
//?#################################################################################################

//? Definition

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Lit(Literal),
    UnInit,
    Mut(Box<Val>),
}

//? Conversions

impl From<Literal> for Val {
    fn from(lit: Literal) -> Self {
        Val::Lit(lit)
    }
}

impl From<Val> for Literal {
    fn from(val: Val) -> Self {
        match val {
            Val::Lit(lit) => lit,
            Val::Mut(val) => val.get_literal().unwrap(),
            _ => panic!("cannot convert to Literal from {:?}", val),
        }
    }
}

impl From<i32> for Val {
    fn from(val: i32) -> Self {
        Val::Lit(val.into())
    }
}

impl From<bool> for Val {
    fn from(val: bool) -> Self {
        Val::Lit(val.into())
    }
}

impl From<()> for Val {
    fn from(val: ()) -> Self {
        Val::Lit(val.into())
    }
}

impl From<String> for Val {
    fn from(val: String) -> Self {
        Val::Lit(val.into())
    }
}

impl From<Vec<Literal>> for Val {
    fn from(val: Vec<Literal>) -> Self {
        Val::Lit(val.into())
    }
}

impl From<Val> for i32 {
    fn from(val: Val) -> Self {
        match val {
            Val::Lit(Literal::Int(i)) => i,
            Val::Mut(val) => val.get_int().unwrap(),
            _ => panic!("cannot get int from {:?}", val),
        }
    }
}

impl From<Val> for bool {
    fn from(val: Val) -> Self {
        match val {
            Val::Lit(Literal::Bool(b)) => b,
            Val::Mut(val) => val.get_bool().unwrap(),
            _ => panic!("cannot get bool from {:?}", val),
        }
    }
}

impl From<Val> for String {
    fn from(val: Val) -> Self {
        match val {
            Val::Lit(Literal::String(s)) => s,
            Val::Mut(val) => val.get_string().unwrap(),
            _ => panic!("cannot get string from {:?}", val),
        }
    }
}

impl From<Val> for Vec<Literal> {
    fn from(val: Val) -> Self {
        match val {
            Val::Lit(Literal::Array(arr, size)) => arr,
            Val::Mut(val) => val.get_literal().unwrap().into(),
            _ => panic!("cannot get array from {:?}", val),
        }
    }
}

//? Helpers

// Helpers for Val
// Alternatively implement the TryFrom trait
impl Val {
    pub fn mutable_val(val: Val) -> Val {
        Val::Mut(Box::new(val))
    }

    pub fn get_literal(&self) -> Result<Literal, EvalError> {
        match self {
            Val::Lit(lit) => Ok(lit.clone()),
            Val::Mut(val) => val.get_literal(),
            _ => Err(EvalError::uninit()),
        }
    }

    pub fn get_bool(&self) -> Result<bool, EvalError> {
        match self {
            Val::Lit(Literal::Bool(b)) => Ok(*b),
            Val::Mut(val) => val.get_bool(),
            _ => Err(EvalError::invalid_extraction(Type::Bool, Type::from(self.clone()))),
        }
    }

    pub fn get_int(&self) -> Result<i32, EvalError> {
        match self {
            Val::Lit(Literal::Int(i)) => Ok(*i),
            Val::Mut(val) => val.get_int(),
            _ => Err(EvalError::invalid_extraction(Type::I32, Type::from(self.clone()))),
        }
    }

    pub fn get_string(&self) -> Result<String, EvalError> {
        match self {
            Val::Lit(Literal::String(s)) => Ok(s.clone()),
            Val::Mut(val) => val.get_string(),
            _ => Err(EvalError::invalid_extraction(Type::String, Type::from(self.clone()))),
        }
    }

    pub fn get_array(&self) -> Result<Vec<Literal>, EvalError> {
        match self {
            Val::Lit(Literal::Array(arr, size)) => Ok(arr.clone()),
            Val::Mut(val) => val.get_array(),
            _ => Err(EvalError::invalid_extraction(Type::Array(Box::new(Type::Blank), 0), Type::from(self.clone()))), // Not the best printing...
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Lit(lit) => write!(f, "{}", lit),
            Val::UnInit => write!(f, "UninitializedValue"),
            Val::Mut(val) => write!(f, "Mut({})", val),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Evaluation                                          #
//?#                                                                                               #
//?#################################################################################################

//? ------------- BinOp -------------

// Helper for BinOp
impl BinOp {
    // Evaluate Binary Operation to Value
    pub fn eval(&self, left: Val, right: Val) -> Result<Val, EvalError> {
        let left_clone = left.clone();
        let right_clone = right.clone();

        let left_lit = left.get_literal()?;
        let right_lit = right.get_literal()?;

        //TODO: Replace panic with custom Errors?
        match self {
            // Integer operations
            BinOp::Add => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l + r)),
                (Literal::String(l), Literal::String(r)) => Ok(Val::from(format!("{}{}", l, r))),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Sub => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l - r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Mul => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l * r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Div => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => match r {
                    0 => Err(EvalError::division_by_zero()),
                    _ => Ok(Val::from(l / r)),
                },
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            // Boolean operations
            BinOp::And => match (left_lit, right_lit) {
                (Literal::Bool(l), Literal::Bool(r)) => Ok(Val::from(l && r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Or => match (left_lit, right_lit) {
                (Literal::Bool(l), Literal::Bool(r)) => Ok(Val::from(l || r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            // Comparison operations
            BinOp::Eq => Ok(Val::from(left_lit == right_lit)),
            BinOp::Ne => Ok(Val::from(left_lit != right_lit)),
            BinOp::Gt => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l > r)),
                (Literal::String(l), Literal::String(r)) => Ok(Val::from(l > r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Lt => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l < r)),
                (Literal::String(l), Literal::String(r)) => Ok(Val::from(l < r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Ge => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l >= r)),
                (Literal::String(l), Literal::String(r)) => Ok(Val::from(l >= r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            BinOp::Le => match (left_lit, right_lit) {
                (Literal::Int(l), Literal::Int(r)) => Ok(Val::from(l <= r)),
                (Literal::String(l), Literal::String(r)) => Ok(Val::from(l <= r)),
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            // Array operations
            BinOp::Get => match (left_lit, right_lit) {
                (Literal::Array(arr, size), Literal::Int(i)) => {
                    if i < 0 || i as usize >= size {
                        return Err(EvalError::index_out_of_bounds(i as usize, size))
                    }
                    Ok(Val::from(arr[i as usize].clone()))
                }
                _ => Err(EvalError::binary_operation_error(*self, left, right))
            },
            _ => unimplemented!("BinOp::eval for {:?}", self),
        }
    }
}

//? ------------- UnOp -------------

// Helper for UnOp
impl UnOp {
    // Evaluate Unary Operation to Value
    pub fn eval(&self, operand: Val) -> Result<Val, EvalError> {
        let operand_clone = operand.clone();
        let operand_lit = operand.get_literal()?;

        //TODO: Replace panic with custom Errors?
        match self {
            UnOp::Neg => match operand_lit {
                Literal::Int(i) => Ok(Val::from(-i)),
                _ => Err(EvalError::unary_operation_error(*self, operand_clone)),
            },
            UnOp::Bang => match operand_lit {
                Literal::Bool(b) => Ok(Val::from(!b)),
                _ => Err(EvalError::unary_operation_error(*self, operand_clone)),
            },
            _ => unimplemented!("UnOp::eval for {:?}", self),
        }
    }
}

//? ------------- Expr -------------

impl Eval<Val> for Expr {
    fn eval(&self) -> Result<Val, EvalError> {
        let mut vm = VM::new();
        vm.eval_expr(self)
    }
}

//? ------------- Block -------------

impl Eval<Val> for Block {
    fn eval(&self) -> Result<Val, EvalError> {
        let mut vm = VM::new();
        vm.eval_block(self)
    }
}

//? ------------- Prog -------------

impl Eval<Val> for Prog {
    fn eval(&self) -> Result<Val, EvalError> {
        let mut vm = VM::new();

        // Set functions declarations in the VM
        for decl in self.0.iter() {
            vm.define_func(&decl)?;
        }

        // Look for a 'main' function
        for decl in self.0.iter() {
            // Does not support command line arguments for now
            if decl.id == "main" {
                return vm.eval_expr(&Expr::Call("main".to_string(), Arguments::new(vec![])));
            }
        }
        Err(EvalError::main_not_found())
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                        Virtual Machine                                        #
//?#                                                                                               #
//?#################################################################################################

// here you implement your VM
pub struct VM {
    // State of the VM -> Variables Environment
    var_env: Vec<HashMap<String, Val>>,
    // Functions must be accessible in the correct scope only.
    // On call, they use another VM to evaluate their block, initialized with the given arguments.
    // However, they should still have access to any function from the previous scopes.
    func_env: Vec<HashMap<String, FnDeclaration>>,

    // This stores the intrinsic methods of our VM
    intrinsics: HashMap<String, Intrinsic>,
}

impl VM {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut vm = VM {
            var_env: vec![],
            func_env: vec![],
            intrinsics: HashMap::new(),
        };

        // Define the program whole scope
        vm.add_new_scope();

        // Add the intrinsic functions to the VM
        vm.add_intrinsics();

        vm
    }

    // Creates a new vm from the current one, ready for a function call
    // It will have the same functions defined, but variables env reset and ready for the function call
    fn from_function_call(&mut self, params: &Parameters, args: &Arguments) -> VM {
        let mut vm = VM::new();
        let env = self.env_from_function_call(params, args);
        vm.var_env.push(env);
        vm.func_env = self.func_env.clone();
        vm
    }

    // Creates an environment from a function call
    // Type checker will have already verified the types and the number of arguments
    fn env_from_function_call(
        &mut self,
        params: &Parameters,
        args: &Arguments,
    ) -> HashMap<String, Val> {
        let mut env: HashMap<String, Val> = HashMap::new();
        for (param, arg) in params.0.iter().zip(args.0.iter()) {
            env.insert(param.id.clone(), self.eval_expr(arg).unwrap());
        }
        env
    }

    fn add_new_scope(&mut self) {
        self.var_env.push(HashMap::new());
        self.func_env.push(HashMap::new());
    }

    fn remove_scope(&mut self) {
        self.var_env.pop();
        self.func_env.pop();
    }

    // Define a new variable in the current scope
    fn define_var(&mut self, name: &str, val: Val) -> Result<(), EvalError> {
        if let Some(env) = self.var_env.last_mut() {
            // Shadowing is allowed so no need for verification
            env.insert(name.to_string(), val);
            Ok(())
        } else {
            Err(EvalError::scope_error(format!("no scope to define variable '{}'", name)))
        }
    }

    // Try to fetch a variable, starting from the top of the stack
    fn get_var(&self, name: &str) -> Result<Val, EvalError> {
        for env in self.var_env.iter().rev() {
            if let Some(val) = env.get(name) {
                return Ok(val.clone());
            }
        }
        Err(EvalError::variable_not_found(name.to_string()))
    }

    // Try to modify a variable, starting from the top of the stack
    fn set_var(&mut self, name: &str, val: Val) -> Result<Val, EvalError> {
        for env in self.var_env.iter_mut().rev() {
            if let Some(v) = env.get_mut(name) {
                *v = val;
                // Returns the assigned value
                return Ok(v.clone());
            }
        }
        Err(EvalError::variable_not_found(name.to_string()))
    }

    // Modify an array element
    fn modify_array_value(&mut self, name: &str, index: usize, val: Val) -> Result<(), EvalError> {
        for env in self.var_env.iter_mut().rev() {
            if let Some(v) = env.get_mut(name) {
                let mut array = v.get_array().unwrap();
                array[index] = val.get_literal().unwrap();
                self.set_var(name, Val::from(array))?;
                
                
                // Returns unit value
                return Ok(())
            }
        }
        Err(EvalError::variable_not_found(name.to_string()))
    }

    fn define_func(&mut self, func: &FnDeclaration) -> Result<(), EvalError> {
        let name = func.id.clone();
        if let Some(env) = self.func_env.last_mut() {
            // Can't define two functions with the same name in the same scope
            if env.contains_key(&name) {
                return Err(EvalError::function_already_defined(name));
            }
            env.insert(name.to_string(), func.clone());
            Ok(())
        } else {
            Err(EvalError::scope_error(format!("no scope to define function '{}'", name)))
        }
    }

    fn get_func(&self, name: &str) -> Result<FnDeclaration, EvalError> {
        for env in self.func_env.iter().rev() {
            if let Some(func) = env.get(name) {
                return Ok(func.clone());
            }
        }
        Err(EvalError::function_not_found(name.to_string()))
    }

    fn add_intrinsics(&mut self) {
        let intrinsic_functions = vec![
            vm_println(),
        ];

        for (func, intrinsic) in intrinsic_functions {
            self.define_func(&func).unwrap();
            self.intrinsics.insert(func.id.clone(), intrinsic);
        }
    }

    fn pretty_string_func_env(&self) -> String {
        let mut s = String::new();
        s.push_str("{");
        for (i, env) in self.func_env.iter().enumerate() {
            s.push_str("[");
            for (name, func) in env.iter() {
                s.push_str(&format!("{}, ", name));
            }
            s.push_str("], ");
        }
        s.push_str("}");
        s
    }

    fn pretty_print_state(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("Variables: {:?}\n", self.var_env));
        s.push_str(&format!("Functions: {}\n", self.pretty_string_func_env()));
        s
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Val, EvalError> {
        // DEBUG
        //eprintln!("Evaluating expression:\n{}", expr);
        //eprintln!("Current state:\n{}", self.pretty_print_state());

        match expr {
            Expr::Ident(ident) => self.get_var(ident),
            Expr::Lit(lit) => Ok(Val::from(lit.clone())),
            Expr::BinOp(op, left, right) => {
                let left_val = self.eval_expr(left)?;

                // Add boolean short-circuiting
                match op {
                    BinOp::And => {
                        if !left_val.get_bool().unwrap() { // Will not crash since Type will have been checked
                            return Ok(Val::from(false));
                        }
                    }
                    BinOp::Or => {
                        if left_val.get_bool().unwrap() { // Will not crash since Type will have been checked
                            return Ok(Val::from(true));
                        }
                    }
                    _ => (),
                }

                let right_val = self.eval_expr(right)?;
                op.eval(left_val, right_val)
            }
            Expr::UnOp(op, operand) => {
                let val = self.eval_expr(operand)?;
                op.eval(val)
            }
            Expr::Par(expr) => self.eval_expr(expr),
            Expr::Call(name, args) => {
                let func = self.get_func(name)?;

                // First check if it is an intrinsic function
                if let Some(intrinsic) = self.intrinsics.get(name).cloned() {
                    let mut lit_vec: Vec<Literal> = vec![];
                    for arg in args.0.iter() {
                        // Evaluate the arguments
                        let arg_lit = self.eval_expr(arg)?;
                        lit_vec.push(arg_lit.get_literal().unwrap());
                    }
                    return Ok(Val::from(intrinsic(lit_vec)));
                }

                // It is not an intrinsic function, so we evaluate it

                let mut vm = self.from_function_call(&func.parameters, &args.clone());

                // DEBUG
                //eprintln!("Calling func: {}", func.id);
                //eprintln!("Created VM with initial state:\n{}", vm.pretty_print_state());

                vm.eval_block(&func.body)
            }
            Expr::IfThenElse(cond, then_block, else_block) => {
                // Following unwrap should not be issues since the type checker will have already verified types
                if self.eval_expr(cond).unwrap().get_bool().unwrap() {
                    self.eval_block(then_block)
                } else {
                    match else_block {
                        Some(block) => self.eval_block(block),
                        None => Ok(Literal::Unit.into()),
                    }
                }
            }
            Expr::Block(block) => self.eval_block(block),
            _ => unimplemented!("VM eval not implemented for expression {:?}", expr),
        }
    }

    pub fn eval_stmt(&mut self, stmt: &Statement) -> Result<Val, EvalError> {
        // DEBUG
        //eprintln!("Evaluating statement:\n{}", stmt);
        //eprintln!("Current state:\n{}", self.pretty_print_state());

        match stmt {
            Statement::Let(mutable, name, _, option) => {
                let mut val = Val::UnInit;
                if let Some(expr) = option {
                    val = self.eval_expr(expr)?;
                }

                if mutable.0 {
                    val = Val::mutable_val(val);
                }

                Ok(Val::from(self.define_var(name, val)?)) // Returns a unit value
            }
            Statement::Assign(lexpr, rexpr) => {
                // Look at the left expression and assign the value of the right expression to it
                // Is only possible if the left expression is an identifier, or if it is an array access
                match lexpr {
                    Expr::Ident(ident) => {
                        let val = self.eval_expr(rexpr)?;

                        // Type checker will be in charge of verifying the types and the mutability of the variable
                        Ok(Val::from(self.set_var(ident, val)?)) // Returns the assigned value (for blocks)
                    }
                    
                    Expr::BinOp(BinOp::Get, left, right) => {
                        let left_expr = *(left.clone());
                        // Left val should be an array identifier
                        match left_expr {
                            Expr::Ident(ident) => {
                                let index = self.eval_expr(right)?.get_int()? as usize;

                                let val = self.eval_expr(rexpr)?;

                                // Type checker will be in charge of verifying the types and the mutability of the variable
                                // Returns a unit value (an array assignment do not return the assigned value in rust)
                                Ok(Val::from(self.modify_array_value(&ident, index, val)?))
                            },
                            _ => Err(EvalError::expected_identifier(lexpr.clone())),
                        }
                    }
                    _ => Err(EvalError::assignment_error(lexpr.clone())),
                }
            }
            Statement::While(cond, block) => {
                while self.eval_expr(cond).unwrap().get_bool().unwrap() {
                    self.eval_block(block)?;
                }
                Ok(Literal::Unit.into()) // Returns a unit value
            }
            Statement::Expr(expr) => self.eval_expr(expr), // Only statement that can return a value
            Statement::Fn(fn_decl) => {
                self.define_func(&fn_decl)?;
                Ok(Literal::Unit.into()) // Returns a unit value
            }
            _ => unimplemented!("VM eval not implemented for statement {:?}", stmt),
        }
    }

    pub fn eval_block(&mut self, block: &Block) -> Result<Val, EvalError> {
        // Evaluation of a block involves adding a new scope to the stack
        self.add_new_scope();

        // Scan the block for function definitions and define them in the VM
        self.define_block_functions(block)?;

        // DEBUG
        //eprintln!("Evaluating block...");
        //eprintln!("Block is:\n{}", block);
        //eprintln!("Actual state:\n{}", self.pretty_print_state());

        // Preparing to return the result of the block
        // The only possibility for a return value is the last statement of the block, not followed by a semicolon, and being an expression (can be a function call)
        let mut result: Val = Val::from(Literal::Unit);
        let size = block.statements.len();

        for index in 0..size {
            let stmt = &block.statements[index];
            match stmt {
                Statement::Fn(_) => continue, // Function definitions are already defined in the VM
                _ => {
                    if index == size - 1 && !block.semi {
                        // last statement
                        result = self.eval_stmt(stmt)?; // Should always be an expression or assignment evaluation
                                                        // (Will be checked by the type checker if I am correct)
                    } else {
                        self.eval_stmt(stmt)?;
                    }
                }
            }

            // DEBUG
            //eprintln!("Current block state:\n{}", self.pretty_print_state());
        }

        // DEBUG
        //eprintln!("Finished evaluating block...");
        //eprintln!("Block was:\n{}", block);
        //eprintln!("Actual state:\n{}", self.pretty_print_state());
        //eprintln!("Block result value is {:?} (semi = {})", result, block.semi);

        // Removing the scope from the stack
        self.remove_scope();

        // Returning the result of the block
        Ok(result)
    }

    fn define_block_functions(&mut self, block: &Block) -> Result<(), EvalError> {
        // This scans the whole block for function definitions in order to define them in the VM
        // and be able to call them before their definition
        for stmt in block.statements.iter() {
            match stmt {
                Statement::Fn(decl) => self.define_func(&decl)?,
                _ => (),
            }
        }
        Ok(())
    }
}
