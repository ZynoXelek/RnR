use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Mutable, Parameter, Parameters, Prog,
    Statement, Type, UnOp,
};
use crate::common::{Eval, EvalType};
use crate::error::{EvalError, TypeError};
use crate::intrinsics::*;

//?#################################################################################################
//?#                                                                                               #
//?#                                         TypeVal Type                                          #
//?#                                                                                               #
//?#################################################################################################

//? Definition

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVal {
    Type(Type),
    Uninit(Option<Type>), // Will allow to check for uninitialized variables
    Mut(Box<TypeVal>),    // Will allow to check for assignments to mutable variables
}

//? Conversions

impl From<Type> for TypeVal {
    fn from(t: Type) -> Self {
        TypeVal::Type(t)
    }
}

impl From<TypeVal> for Type {
    fn from(t: TypeVal) -> Self {
        match t {
            TypeVal::Type(t) => t,
            _ => panic!("TypeVal::Type expected"),
        }
    }
}

impl From<Literal> for TypeVal {
    fn from(lit: Literal) -> Self {
        let t: Type = lit.into();
        TypeVal::Type(t)
    }
}

impl From<Parameter> for TypeVal {
    fn from(param: Parameter) -> Self {
        let ty = TypeVal::Type(param.ty);
        if param.mutable.0 {
            TypeVal::mutable_type(ty)
        } else {
            ty
        }
    }
}

//? Helpers

impl TypeVal {
    pub fn uninitialized() -> TypeVal {
        TypeVal::Uninit(None)
    }

    pub fn mutable_type(t: TypeVal) -> TypeVal {
        match t {
            TypeVal::Mut(_) => t, // Useless to make a mutable type mutable
            _ => TypeVal::Mut(Box::new(t)),
        }
    }

    pub fn uninitialized_type(t: TypeVal) -> TypeVal {
        match t {
            TypeVal::Type(t) => TypeVal::Uninit(Some(t)),
            TypeVal::Mut(t) => TypeVal::mutable_type(TypeVal::uninitialized_type(*t)),
            TypeVal::Uninit(_) => t, // Already uninitialized
        }
    }

    pub fn initialized_type(t: TypeVal) -> TypeVal {
        match t {
            TypeVal::Uninit(t) => match t {
                Some(t) => match t {
                    Type::Any => panic!("Can't initialize an uninitialized and undefined type"),
                    _ => TypeVal::Type(t),
                },
                None => panic!("Can't initialize an uninitialized and undefined type"),
            },
            TypeVal::Mut(t) => TypeVal::mutable_type(TypeVal::initialized_type(*t)),
            TypeVal::Type(t) => match t {
                Type::Any => panic!("Can't initialize an uninitialized and undefined type"),
                _ => TypeVal::Type(t),
            },
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            TypeVal::Mut(_) => true,
            _ => false,
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        self._is_uninitialized_with_param(false)
    }

    pub fn _is_uninitialized_with_param(&self, look_into_arrays: bool) -> bool {
        match self {
            TypeVal::Type(t) => match t {
                Type::Any => true,
                Type::Array(ty, _) => {
                    if look_into_arrays {
                        let inner_type_val = TypeVal::Type(*ty.clone());
                        inner_type_val._is_uninitialized_with_param(true)
                    } else {
                        false
                    }
                }
                _ => false,
            },
            TypeVal::Uninit(_) => true,
            TypeVal::Mut(t) => t.is_uninitialized(),
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            TypeVal::Type(t) => Some(t.clone()),
            TypeVal::Uninit(t) => t.clone(),
            TypeVal::Mut(t) => t.get_type(),
        }
    }

    // When you don't want to get an uninitialized type
    // Will panic if the type is uninitialized (useful for tests)
    pub fn get_initialized_type(&self) -> Type {
        match self {
            TypeVal::Type(t) => match t {
                Type::Any => panic!("Can't get an initialized type from an undefined type"),
                _ => t.clone(),
            },
            TypeVal::Uninit(_) => panic!("Uninitialized type"),
            TypeVal::Mut(t) => t.get_initialized_type(),
        }
    }
}

impl fmt::Display for TypeVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeVal::Type(t) => write!(f, "{}", t),
            TypeVal::Uninit(t) => {
                if let Some(t) = t {
                    write!(f, "Uninit({})", t)
                } else {
                    write!(f, "Uninit(_)")
                }
            }
            TypeVal::Mut(t) => write!(f, "Mut({})", t),
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Evaluation                                          #
//?#                                                                                               #
//?#################################################################################################

//? ------------- BinOp -------------

impl BinOp {
    pub fn eval_type(&self, left: TypeVal, right: TypeVal) -> Result<TypeVal, TypeError> {
        // Here, we don't worry about uninitialized variables, as the type checker will have already checked that before calling this
        let left_type = left.get_type().unwrap();
        let right_type = right.get_type().unwrap();

        match self {
            // Integer only operations
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if left_type == Type::I32 && right_type == Type::I32 {
                    Ok(TypeVal::Type(Type::I32))
                } else {
                    Err(TypeError::binop_type_mismatch(
                        self.clone(),
                        left_type.clone(),
                        right_type.clone(),
                    ))
                }
            }
            // Boolean only operations
            BinOp::And | BinOp::Or => {
                if left_type == Type::Bool && right_type == Type::Bool {
                    Ok(TypeVal::Type(Type::Bool))
                } else {
                    Err(TypeError::binop_type_mismatch(
                        self.clone(),
                        left_type.clone(),
                        right_type.clone(),
                    ))
                }
            }
            // Comparison operations
            BinOp::Eq | BinOp::Ne => {
                if left_type == right_type {
                    Ok(TypeVal::Type(Type::Bool))
                } else {
                    Err(TypeError::binop_type_mismatch(
                        self.clone(),
                        left_type.clone(),
                        right_type.clone(),
                    ))
                }
            }
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if (left_type == Type::I32 || left_type == Type::String) && left_type == right_type
                {
                    Ok(TypeVal::Type(Type::Bool))
                } else {
                    Err(TypeError::binop_type_mismatch(
                        self.clone(),
                        left_type.clone(),
                        right_type.clone(),
                    ))
                }
            }
            // Array operations
            BinOp::Get => match (left_type.clone(), right_type.clone()) {
                (Type::Array(ty, _), Type::I32) => Ok(TypeVal::Type(*ty)),
                _ => Err(TypeError::binop_type_mismatch(
                    self.clone(),
                    left_type.clone(),
                    right_type.clone(),
                )),
            },
            // _ => unimplemented!("type eval not implemented for binary operation {:?}", self),
        }
    }
}

//? ------------- UnOp -------------

impl UnOp {
    fn eval_type(&self, operand: TypeVal) -> Result<TypeVal, TypeError> {
        // Here, we don't worry about uninitialized variables, as the type checker will have already checked that before calling this
        let operand_type = operand.get_type().unwrap();

        match self {
            UnOp::Neg => {
                if operand_type == Type::I32 {
                    Ok(TypeVal::Type(Type::I32))
                } else {
                    Err(TypeError::unop_type_mismatch(
                        self.clone(),
                        operand_type.clone(),
                    ))
                }
            }
            UnOp::Bang => {
                if operand_type == Type::Bool {
                    Ok(TypeVal::Type(Type::Bool))
                } else {
                    Err(TypeError::unop_type_mismatch(
                        self.clone(),
                        operand_type.clone(),
                    ))
                }
            } // _ => unimplemented!("type eval not implemented for unary operation {:?}", self),
        }
    }
}

//? ------------- Expr -------------

impl EvalType<TypeVal> for Expr {
    fn eval_type(&self) -> Result<TypeVal, TypeError> {
        let mut tvm = TVM::new();
        tvm.eval_type_expr(self)
    }
}

//? ------------- Block -------------

impl EvalType<TypeVal> for Block {
    fn eval_type(&self) -> Result<TypeVal, TypeError> {
        let mut tvm = TVM::new();
        tvm.eval_type_block(self)
    }
}

//? ------------ Prog -------------

impl EvalType<TypeVal> for Prog {
    fn eval_type(&self) -> Result<TypeVal, TypeError> {
        let mut tvm = TVM::new();

        // Set functions declarations in the TVM
        for decl in self.0.iter() {
            tvm.define_func(&decl)?;
        }

        // Check their correctness
        for decl in self.0.iter() {
            tvm.check_func_correctness(&decl)?;
        }

        // Look for a 'main' function
        for decl in self.0.iter() {
            // Does not support command line arguments for now
            if decl.id == "main" {
                // Make sure it has no parameters
                if !decl.parameters.0.is_empty() {
                    return Err(TypeError::main_with_parameters());
                }

                // Check that the return type is correct -> Should be Unit
                //? Theoretically, can also be Result<(), E>, where E implements the std::error::Error trait
                let return_type = &decl.ty;

                if let Some(ty) = return_type {
                    if *ty != Type::Unit {
                        return Err(TypeError::main_with_invalid_type(ty.clone()));
                    }
                }

                // Everything is fine
                let prog_type = tvm.eval_type_block(&decl.body).unwrap(); // It should therefore be of Unit Type
                let prog_type = prog_type.get_type();
                match prog_type {
                    Some(t) => {
                        if t == Type::Unit {
                            return Ok(TypeVal::Type(Type::Unit));
                        } else {
                            return Err(TypeError::main_with_invalid_type(t));
                        }
                    }
                    None => unreachable!("Main function returns an uninitialized type"),
                }
            }
        }
        Err(TypeError::main_not_found())
    }
}

//? ------------- General -------------

impl<T> Eval<Type> for T
where
    T: EvalType<TypeVal>,
{
    fn eval(&self) -> Result<Type, EvalError> {
        let type_val_res = self.eval_type();
        match type_val_res {
            Ok(type_val) => Ok(type_val.get_type().unwrap()),
            Err(e) => {
                return Err(EvalError::type_error(e));
            }
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                     Type Virtual Machine                                      #
//?#                                                                                               #
//?#################################################################################################

pub struct TVM {
    // State of the TVM -> Variables Environment
    var_env: Vec<HashMap<String, TypeVal>>,
    // Functions must be accessible in the correct scope only.
    // On definition, they will use another TVM to evaluate their block and check its correctness.
    // So we will check that types, number of arguments, and used variables are correct.
    // However, they should still have access to any function from the previous scopes.
    func_env: Vec<HashMap<String, FnDeclaration>>,

    // This stores the intrinsic methods of our TVM
    intrinsics: HashMap<String, Intrinsic>,
}

impl TVM {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut tvm = TVM {
            var_env: vec![],
            func_env: vec![],
            intrinsics: HashMap::new(),
        };

        // Define the program whole scope
        tvm.add_new_scope();

        // Add the intrinsic functions to the TVM
        tvm.add_intrinsics();

        tvm
    }

    // Creates a new tvm from the current one, ready for a function call
    // It will have the same functions defined, but variables env reset and ready for checking the function
    fn from_function_call(&mut self, params: &Parameters) -> TVM {
        let mut tvm = TVM::new();
        let env = self.env_from_function_call(params);
        tvm.var_env.push(env);
        tvm.func_env = self.func_env.clone();
        tvm
    }

    // Creates an environment for testing functions
    fn env_from_function_call(&mut self, params: &Parameters) -> HashMap<String, TypeVal> {
        let mut env: HashMap<String, TypeVal> = HashMap::new();
        for param in params.0.iter() {
            env.insert(param.id.clone(), TypeVal::from(param.clone()));
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

    // This function can compare an initial state, with the states extracted from the two branches of an if statement.
    // If a uninitialized variable from the initial state is initialized in both branches, it is considered initialized.
    // It checks that it is initialized to the same types, and initializes it in the initial state.
    // Then, it returns the initial state with the updated variables.
    fn check_if_branches_init(
        mut init_state: Vec<HashMap<String, TypeVal>>,
        then_state: Vec<HashMap<String, TypeVal>>,
        else_state: Vec<HashMap<String, TypeVal>>,
    ) -> Result<Vec<HashMap<String, TypeVal>>, TypeError> {
        // First, we extract the names and scope_ids of each uninitialized variable in the initial state
        let mut uninitialized_vars: HashMap<String, usize> = HashMap::new();
        for (scope_id, scope) in init_state.iter().enumerate() {
            for (name, val) in scope.iter() {
                if val.is_uninitialized() {
                    uninitialized_vars.insert(name.clone(), scope_id);
                }
            }
        }

        // Then, we check if the uninitialized variables are initialized in both branches
        for (name, scope_id) in uninitialized_vars.iter() {
            let then_val = then_state[*scope_id].get(name).unwrap(); // It shouldn't be possible that it is not present
            let else_val = else_state[*scope_id].get(name).unwrap(); // It shouldn't be possible that it is not present

            if then_val != else_val {
                return Err(TypeError::if_blocks_init_type_mismatch(
                    name.clone(),
                    then_val.clone(),
                    else_val.clone(),
                ));
            }

            // If everything is fine, we can initialize the variables in the initial state
            init_state[*scope_id].insert(name.clone(), then_val.clone());
        }

        Ok(init_state)
    }

    // Define a new variable in the current scope
    fn define_var(&mut self, name: &str, val: TypeVal) -> Result<(), TypeError> {
        if let Some(env) = self.var_env.last_mut() {
            // Shadowing is allowed so no need for verification
            env.insert(name.to_string(), val);
            Ok(())
        } else {
            Err(TypeError::scope_error(format!(
                "no scope to define variable '{}'",
                name
            )))
        }
    }

    // Try to fetch a variable, starting from the top of the stack, and return it with the corresponding scope where we have found it
    fn get_var_and_scope(&self, name: &str) -> Result<(TypeVal, usize), TypeError> {
        let nb_scopes = self.var_env.len();
        for i in 0..nb_scopes {
            // Reverse order to start from the top of the stack
            if let Some(val) = self.var_env[nb_scopes - 1 - i].get(name) {
                return Ok((val.clone(), nb_scopes - 1 - i));
            }
        }
        Err(TypeError::variable_not_found(name.to_string()))
    }

    // Try to fetch a variable, starting from the top of the stack
    fn get_var(&self, name: &str) -> Result<TypeVal, TypeError> {
        for env in self.var_env.iter().rev() {
            if let Some(val) = env.get(name) {
                return Ok(val.clone());
            }
        }
        Err(TypeError::variable_not_found(name.to_string()))
    }

    fn init_var(&mut self, name: &str, ty: TypeVal) -> Result<TypeVal, TypeError> {
        // It replaces the already defined variable with the initialized one

        let (_, scope) = self.get_var_and_scope(name)?; // Check that the variable exists

        // Now we can redefine the corresponding var
        self.var_env[scope].insert(name.to_string(), ty.clone());

        Ok(ty)
    }

    fn define_func(&mut self, func: &FnDeclaration) -> Result<(), TypeError> {
        // This should define a function in the current scope
        // At this step, we don't check their validity yet, so that they are available for calls in other functions

        let name = func.id.clone();
        if let Some(env) = self.func_env.last_mut() {
            // Can't define two functions with the same name in the same scope
            if env.contains_key(&name) {
                return Err(TypeError::function_already_defined(name));
            }
            env.insert(name.to_string(), func.clone());
            Ok(())
        } else {
            Err(TypeError::scope_error(format!(
                "no scope to define function '{}'",
                name
            )))
        }
    }

    fn check_func_correctness(&mut self, func: &FnDeclaration) -> Result<(), TypeError> {
        // This should create a new TVM, with the correct initial variable defined, and the correct accessible functions
        // Then, it can check the body of the function, and its return type

        let mut tvm = self.from_function_call(&func.parameters);
        let return_type = tvm.eval_type_block(&func.body)?;

        // Verify that it matches the declared return type
        let func_type = if let Some(ty) = &func.ty {
            ty.clone()
        } else {
            Type::Unit.into()
        };

        if return_type.get_type().unwrap() != func_type {
            return Err(TypeError::invalid_function_return_type(
                func.clone(),
                func_type,
                return_type.get_type().unwrap(),
            ));
        }

        Ok(())
    }

    fn get_func(&self, name: &str) -> Result<FnDeclaration, TypeError> {
        for env in self.func_env.iter().rev() {
            if let Some(func) = env.get(name) {
                return Ok(func.clone());
            }
        }
        Err(TypeError::function_not_found(name.to_string()))
    }

    fn add_intrinsics(&mut self) {
        let intrinsic_functions = vec![vm_println()];

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

    fn define_block_functions(&mut self, block: &Block) -> Result<(), TypeError> {
        // This scans the whole block for function definitions in order to define them in the TVM
        // and be able to call them before their definition
        // each function is then checked for correctness
        let mut new_funcs: Vec<FnDeclaration> = Vec::new();

        for stmt in block.statements.iter() {
            match stmt {
                Statement::Fn(decl) => {
                    self.define_func(&decl)?;
                    new_funcs.push(decl.clone());
                }
                _ => (),
            }
        }

        // Now that functions are defined, we can check their correctness
        for func in new_funcs.iter() {
            self.check_func_correctness(func)?;
        }

        Ok(())
    }

    fn check_intrinsic_call(&mut self, name: &str, args: &Arguments) -> Result<TypeVal, TypeError> {
        // Very specific but do not really know how to improve.
        // Could at least move this code to the intrinsic definition so it is centralized
        if let Some(intrinsic) = self.intrinsics.get(name) {
            match name {
                "println!" => self.check_println_call(args),
                _ => unimplemented!("intrinsic function '{}' not implemented", name),
            }
        } else {
            Err(TypeError::function_not_found(name.to_string()))
        }
    }

    fn check_println_call(&mut self, args: &Arguments) -> Result<TypeVal, TypeError> {
        // We need to verify that:
        // - There is at least one argument, of type String
        // - There are as much additional arguments as there are '{}' or '{:?}' in the string, whatever the types (at least for now)
        // - Every additional argument is initialized

        let fn_decl = self.get_func("println!")?;

        // Verifying the first argument
        if args.0.is_empty() {
            return Err(TypeError::invalid_number_of_arguments(
                fn_decl.clone(),
                1,
                0,
            ));
        }

        let first_arg = &args.0[0];
        let first_arg_type = self.eval_type_expr(first_arg)?;

        if first_arg_type.get_type().unwrap() != Type::String {
            return Err(TypeError::invalid_argument(
                fn_decl.clone(),
                Parameter::new(Mutable::new(false), "s".to_string(), Type::String),
                Type::String,
                first_arg_type.get_type().unwrap(),
            ));
        } else if first_arg_type.is_uninitialized() {
            return Err(TypeError::uninitialized_variable((*first_arg).clone()));
        }

        // Counting the number of '{}' and '{:?}' in the string
        let s = match first_arg {
            Expr::Lit(Literal::String(s)) => s,
            _ => unreachable!(),
        };

        // Matches on '{}' and '{:?}', but should not match on '{{}}' or '{{:?}}'
        let re = regex::Regex::new(r"(\{\})|(\{\:\?\})").unwrap();
        let nb_args = re.find_iter(s).count();

        // Verifying the number of arguments
        if args.0.len() - 1 != nb_args {
            return Err(TypeError::invalid_number_of_arguments(
                fn_decl.clone(),
                nb_args + 1,
                args.0.len(),
            ));
        }

        // Verifying the types of the additional arguments (any, but initialized)
        for (i, arg) in args.0.iter().skip(1).enumerate() {
            let arg_type = self.eval_type_expr(arg)?;
            if arg_type.is_uninitialized() {
                return Err(TypeError::uninitialized_variable((*arg).clone()));
            }
        }

        Ok(TypeVal::Type(Type::Unit)) // println! returns unit
    }

    pub fn eval_type_expr(&mut self, expr: &Expr) -> Result<TypeVal, TypeError> {
        match expr {
            Expr::Ident(name) => self.get_var(name),
            Expr::Lit(lit) => {
                // We need a special case for arrays, as we must check that it is valid in size and inner types

                match lit {
                    Literal::Array(array) => {
                        let (expressions, size) = array.as_tuple();

                        if size != expressions.len() {
                            // This would be an internal coding error, not an error from the user
                            unreachable!("Array size and expressions size mismatch");
                        }

                        if size == 0 {
                            // Empty array, cannot infer the type from it, it could be anything
                            return Ok(TypeVal::from(Type::Array(Box::new(Type::Any), 0)));
                        }

                        let first_type = {
                            let expr = expressions.first().unwrap();
                            let expr_type = self.eval_type_expr(expr)?;
                            if expr_type.is_uninitialized() {
                                return Err(TypeError::uninitialized_variable((*expr).clone()));
                            }
                            expr_type.get_type().unwrap()
                        };

                        let mut types: Vec<Type> = Vec::new();
                        types.push(first_type.clone());

                        let mut are_all_same: bool = true;

                        for i in 1..expressions.len() {
                            let expr = &expressions[i];

                            let expr_type = self.eval_type_expr(expr)?;
                            if expr_type.is_uninitialized() {
                                return Err(TypeError::uninitialized_variable((*expr).clone()));
                            }

                            let expr_type = expr_type.get_type().unwrap();
                            if first_type != expr_type {
                                are_all_same = false;
                            }

                            types.push(expr_type);
                        }

                        if !are_all_same {
                            return Err(TypeError::array_inconsistent_types(expr.clone(), types));
                        }

                        Ok(TypeVal::from(Type::Array(Box::new(first_type), size)))
                    }
                    _ => Ok(TypeVal::from(lit.clone())),
                }
            }
            Expr::BinOp(op, left, right) => {
                let left_type = self.eval_type_expr(left)?;
                // If it is uninitialized, we have an issue and won't be able to apply the binary operation
                if left_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((**left).clone()));
                }

                let right_type = self.eval_type_expr(right)?;
                // If it is uninitialized, we have an issue and won't be able to apply the binary operation
                if right_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((**right).clone()));
                }

                let binop_res = op.eval_type(left_type.clone(), right_type);
                match binop_res {
                    Ok(t) => Ok(t),
                    Err(e) => {
                        // Special case for arrays to have a better error message
                        if op.clone() == BinOp::Get {
                            return Err(TypeError::array_invalid_index(
                                (**left).clone(),
                                left_type.get_type().unwrap(),
                            )); // Can't panic since is_initialized
                        }
                        Err(e)
                    }
                }
            }
            Expr::UnOp(op, operand) => {
                let operand_type = self.eval_type_expr(operand)?;
                // If it is uninitialized, we have an issue and won't be able to apply the unary operation
                if operand_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((**operand).clone()));
                }

                op.eval_type(operand_type)
            }
            Expr::Par(expr) => self.eval_type_expr(expr),
            Expr::Call(name, args) => {
                // We need to check:
                // - The number of arguments
                // - The type of each one
                // - each argument is initialized
                // And then we can return the function type
                // The function itself is verified at its declaration, so there is no need to check the block inside the function here
                let func = self.get_func(name)?;
                let params = &func.parameters;

                if let Some(intrinsic) = self.intrinsics.get(name) {
                    return self.check_intrinsic_call(name, args);
                }

                let nb_args = args.0.len();
                let nb_params = params.0.len();

                if nb_args != nb_params {
                    return Err(TypeError::invalid_number_of_arguments(
                        func.clone(),
                        nb_params,
                        nb_args,
                    ));
                }

                for (param, arg) in params.0.iter().zip(args.0.iter()) {
                    let arg_type = self.eval_type_expr(arg)?;
                    if arg_type.get_type().unwrap() != param.ty {
                        return Err(TypeError::invalid_argument(
                            func.clone(),
                            param.clone(),
                            param.ty.clone(),
                            arg_type.get_type().unwrap(),
                        ));
                    } else if arg_type.is_uninitialized() {
                        return Err(TypeError::uninitialized_variable((*arg).clone()));
                    }
                }

                // The function is correctly called, returning its type
                Ok(TypeVal::from(func.get_return_type()))
            }
            Expr::IfThenElse(cond, then_block, else_block) => {
                // Here, we have to verify that:
                // - condition is a boolean
                // Both blocks are correct (which will be done by the eval_type_block function) and that both return the same type
                // It checks that any uninitialized variable before the if statement is either initialized in both branches
                // or remains uninitialized in both branches

                // Store the initial variable state
                let initial_state = self.var_env.clone();

                let cond_type = self.eval_type_expr(cond)?;

                // It should be an initialized boolean
                if cond_type.get_type().unwrap() != Type::Bool {
                    return Err(TypeError::invalid_if_condition(
                        (**cond).clone(),
                        cond_type.get_type().unwrap(),
                    ));
                } else if cond_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((**cond).clone()));
                }

                let then_type = self.eval_type_block(then_block)?;

                // If there is a else block, we have to check that both blocks return the same type
                if let Some(else_block) = else_block {
                    // Stores the state after the then block
                    let then_state = self.var_env.clone();
                    // Restores the initial state
                    self.var_env = initial_state.clone();

                    let else_type = self.eval_type_block(else_block)?;

                    // Stores the state after the else block
                    let else_state = self.var_env.clone();

                    // Checks for initialized variables
                    self.var_env =
                        TVM::check_if_branches_init(initial_state, then_state, else_state)?;

                    if then_type.get_type() != else_type.get_type() {
                        return Err(TypeError::if_blocks_type_mismatch(
                            then_type.get_type().unwrap(),
                            else_type.get_type().unwrap(),
                        ));
                    }
                }

                Ok(then_type)
            }
            Expr::Block(block) => self.eval_type_block(block),
            // _ => unimplemented!("type eval not implemented for expression {:?}", expr),
        }
    }

    pub fn eval_type_stmt(&mut self, stmt: &Statement) -> Result<TypeVal, TypeError> {
        match stmt {
            Statement::Let(mutable, name, ty, expr) => {
                // We have to check that the type is correct.
                // We can infer the type based on the expression if it is not specified
                // If there are both a type and an expression, we have to check that the expression type is correct
                // If there is none, it is an error

                // First, we have to check if a variable with the same name is already defined in the same scope
                // and if it is never initialized.
                let scope_vars = self.var_env.last().unwrap();
                if let Some(val) = scope_vars.get(name) {
                    if val._is_uninitialized_with_param(true) {
                        return Err(TypeError::never_initialized_variable(name.clone()));
                    }
                }

                let mut val: TypeVal = TypeVal::uninitialized();
                let mut expr_type: Option<Type> = None;

                if let Some(e) = expr {
                    let expr_val_type = self.eval_type_expr(e)?;
                    expr_type = Some(expr_val_type.get_type().unwrap());
                }

                if ty.is_some() && !ty.clone().unwrap().contains_any() {
                    let ty = ty.clone().unwrap();
                    if expr_type.is_some() {
                        let expr_type = expr_type.clone().unwrap();

                        if !ty.equals(&expr_type) {
                            // This support arrays and empty arrays
                            // There is both an expression and an explicit type, but they don't match
                            return Err(TypeError::let_type_mismatch(
                                name.clone(),
                                expr.clone().unwrap(),
                                ty,
                                expr_type,
                            ));
                        }
                    }
                    val = ty.into();
                } else if expr_type.is_some() {
                    val = expr_type.unwrap().into();
                }

                // Types are correct
                // Then, we can define the variable in the current scope

                if let None = expr {
                    // The variable is uninitialized
                    val = TypeVal::uninitialized_type(val);
                }

                if mutable.0 {
                    // The variable is mutable
                    val = TypeVal::mutable_type(val);
                }

                self.define_var(name, val.clone())?;

                Ok(TypeVal::Type(Type::Unit)) // Let statement returns unit
            }
            Statement::Assign(left, right) => {
                // We have to check that the left expression is a variable and that the right expression is of the same type
                // The left expression can either be a mutable, or an uninitialized variable
                // If it is none, it means that it is either not a variable, or that it is not mutable.

                let var_ident = match left.extract_var_identifier() {
                    Some(ident) => ident,
                    None => return Err(TypeError::assignment_invalid_left_expr((*left).clone())),
                };

                let left_type = self.eval_type_expr(left)?; // This will also verify that the array get is correct (uses integer indexes)

                let var_type = self.get_var(&var_ident)?;
                if !var_type.is_mutable() && !var_type.is_uninitialized() {
                    return Err(TypeError::assignment_invalid_left_expr((*left).clone()));
                }

                let right_type = self.eval_type_expr(right)?;

                // It must be of correct initialized type
                if left_type.get_type().is_some() {
                    let right_type = right_type.get_type().unwrap();
                    let left_type = left_type.get_type().unwrap();

                    if !left_type.equals(&right_type) {
                        return Err(TypeError::assignment_type_mismatch(
                            (*right).clone(),
                            left_type,
                            right_type,
                        ));
                    }
                } else if right_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((*right).clone()));
                }

                // Everything is correct, we can assign the value
                // If left expr was uninitialized, it is now initialized
                let r_ty = right_type.get_type().unwrap();

                if left_type.is_uninitialized() {
                    if !r_ty.contains_any() {
                        self.init_var(&var_ident, right_type)?;
                    } else if let TypeVal::Uninit(ty) = left_type.clone() {
                        match ty {
                            Some(t) => {
                                if !t.contains_any() {
                                    self.init_var(&var_ident, TypeVal::initialized_type(left_type))?;
                                }
                            }
                            None => unreachable!("Can't assign a value that is Any to an uninitialized variable with no type given"),
                        }
                    } else {
                        // The only possible case here would be
                        // let a: [_; 0];
                        // a = [];
                        // But this is also an issue for the original rust language. It needs type annotations
                        unreachable!("Can't assign a value that is Any to an uninitialized variable with no type given");
                    }
                }

                Ok(TypeVal::Type(Type::Unit)) // Assign statement returns unit
            }
            Statement::While(cond, block) => {
                // We should verify that the condition is an initialized boolean
                // And that the block returns unit
                let cond_type = self.eval_type_expr(cond)?;

                // It should be an initialized boolean
                if cond_type.is_uninitialized() {
                    return Err(TypeError::uninitialized_variable((*cond).clone()));
                } else if cond_type.get_type().unwrap() != Type::Bool {
                    return Err(TypeError::invalid_while_condition(
                        (*cond).clone(),
                        cond_type.get_type().unwrap(),
                    ));
                }

                let block_type = self.eval_type_block(block)?;

                if block_type.get_type().unwrap() != Type::Unit {
                    return Err(TypeError::while_block_type_mismatch(
                        block_type.get_type().unwrap(),
                    ));
                }

                Ok(TypeVal::Type(Type::Unit)) // While statement returns unit
            }
            Statement::Expr(expr) => self.eval_type_expr(expr),
            Statement::Fn(fn_decl) => {
                self.define_func(&fn_decl)?;
                Ok(Literal::Unit.into()) // Returns a unit value
            } // _ => unimplemented!("type eval not implemented for statement {:?}", stmt),
        }
    }

    pub fn eval_type_block(&mut self, block: &Block) -> Result<TypeVal, TypeError> {
        // We get in a new scope
        self.add_new_scope();

        // Scan the block for function definitions and define them in the TVM
        self.define_block_functions(block)?;

        // DEBUG
        //eprintln!("Evaluating block...");
        //eprintln!("Block is:\n{}", block);
        //eprintln!("Actual state:\n{}", self.pretty_print_state());

        // Preparing to return the result of the block
        // The only possibility for a return value is the last statement of the block, not followed by a semicolon, and being an expression (can be a function call)
        let mut result_type: TypeVal = TypeVal::Type(Type::Unit);
        let size = block.statements.len();

        for index in 0..size {
            let stmt = &block.statements[index];
            match stmt {
                Statement::Fn(_) => continue, // Function are already defined in the TVM
                _ => {
                    if index == size - 1 && !block.semi {
                        // last statement
                        result_type = self.eval_type_stmt(stmt)?;
                    } else {
                        self.eval_type_stmt(stmt)?;
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

        // Before removing scope, we need to verify that each variable has been initialized
        // An uninitialized variable should be an error
        let vars = self.var_env.last().unwrap();
        for (name, val) in vars.iter() {
            if val._is_uninitialized_with_param(true) {
                return Err(TypeError::never_initialized_variable(name.clone()));
            }
        }

        // Removing the scope from the stack
        self.remove_scope();

        // Returning the result of the block
        Ok(result_type)
    }
}
