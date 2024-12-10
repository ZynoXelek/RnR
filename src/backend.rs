use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Mutable, Parameter, Parameters, Prog,
    Statement, Type, UnOp,
};

use crate::common::*;
use crate::error::Error;
use mips::{
    asm::*,
    instr::Instr,
    instrs::Instrs,
    rf::{Reg, Reg::*},
    vm::Mips,
};

//?#################################################################################################
//?#                                                                                               #
//?#                                     Backend Scope Type                                        #
//?#                                                                                               #
//?#################################################################################################

//* stack frame layout:
// ... things before ...
// 16[fp]    arg 1
// 12[fp]    arg 2
//  8[fp]    arg 3
//  4[fp]    ra
//  0[fp]    old_fp
// -4[fp]    local 1
// -8[fp]    local 2, etc.

pub struct Scope {
    size: u32,                  // Tracks the size of the scope on fp values
    nb_vars: u32,               // Tracks the number of variables in the scope
    vars: HashMap<String, u32>, // Tracks the position of the variables on the stack (relative to fp): name -> position (-4, -8, ... but without the sign)
}

//? Helpers

impl Scope {
    pub fn new() -> Self {
        Self {
            size: 20, // Default size of the scope due to the frame layout
            nb_vars: 0,
            vars: HashMap::new(),
        }
    }

    pub fn add_var(&mut self, name: String) {
        self.nb_vars += 1;
        self.vars.insert(name, self.nb_vars * 4); // Each variable is 4 bytes
        self.size += 4; // Each variable takes 4 bytes
    }

    pub fn get_var(&self, name: &String) -> Option<&u32> {
        self.vars.get(name)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Scope: size={}, nb_vars={}, vars:\n",
            self.size, self.nb_vars
        )?;
        for (name, offset) in self.vars.iter() {
            write!(f, "\t{}: {}\n", name, offset)?;
        }
        write!(f, "")
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                     Processing programs                                       #
//?#                                                                                               #
//?#################################################################################################

impl GetMips for Expr {
    fn get_mips(&self) -> Result<Mips, Error> {
        let mut bvm = BVM::new();
        bvm.process_expr(self.clone());

        //? Debug, but may be commented out
        bvm.pretty_print_instructions();

        Ok(bvm.get_mips())
    }
}

impl GetMips for Block {
    fn get_mips(&self) -> Result<Mips, Error> {
        let mut bvm = BVM::new();
        bvm.process_block(self.clone());

        //? Debug, but may be commented out
        bvm.pretty_print_instructions();

        Ok(bvm.get_mips())
    }
}

impl GetMips for Prog {
    fn get_mips(&self) -> Result<Mips, Error> {
        todo!() //TODO: Implement mips for programs
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                   Backend Virtual Machine                                     #
//?#                                                                                               #
//?#################################################################################################

pub struct BVM {
    var_env: Vec<Scope>, // Tracks the variables in the current scope

    // Stores the current instructions
    instructions: Instrs,
}

impl BVM {
    pub fn new() -> Self {
        let mut bvm = Self {
            var_env: vec![],
            instructions: Instrs::new(),
        };

        bvm.add_scope(); // Add the global scope
        bvm
    }

    pub fn get_mips(&self) -> Mips {
        Mips::new(self.instructions.clone())
    }

    //? Helper method for instructions

    fn add_instr(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }

    pub fn pretty_print_instructions(&self) {
        println!("BVM Instructions:");
        for instr in self.instructions.iter() {
            println!("{}", instr);
        }
    }

    //? Helper methods to manage scopes

    //* stack frame layout:
    // ... things before ...
    // 16[fp]    arg 1
    // 12[fp]    arg 2
    //  8[fp]    arg 3
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.

    fn add_scope(&mut self) {
        let mut new_scope = Scope::new();
        self.add_instr(
            addiu(sp, sp, -(new_scope.size as i16))
                .comment("Allocate default space for the new scope"),
        );
        self.var_env.push(new_scope);

        // Then, push the current frame pointer, and set the new frame pointer to the current stack pointer
        self.add_instr(sw(fp, 0, sp).comment("Save the old frame pointer"));
        self.add_instr(
            addi(fp, sp, 0).comment("Set the new frame pointer to the current stack pointer"),
        );
    }

    fn remove_scope(&mut self) {
        let scope = self.var_env.pop().unwrap(); // Get last scope

        // Restore the old frame pointer
        self.add_instr(lw(fp, 0, fp).comment("Restore the old frame pointer"));

        self.add_instr(
            addiu(sp, sp, scope.size as i16).comment("Deallocate space used by the scope"),
        );
    }

    fn define_var(&mut self, name: &String) {
        self.var_env.last_mut().unwrap().add_var(name.clone());
    }

    //TODO: Remove if not necessary
    // // Computes the signed offset of a variable from the given scope in the stack relative to the current fp value (innermost scope)
    // fn get_var_offset_in_scope(&mut self, name: &String, scope_id: usize) -> Option<i32> {
    //     let mut scope_offset: i32 = 0;
    //     let nb_scopes = self.var_env.len();
    //     let final_scope_id = nb_scopes - scope_id - 1;

    //     if final_scope_id < 0 {
    //         return None; // The scope does not exist
    //     }

    //     for i in 0..final_scope_id {
    //         let scope = self.var_env.get(nb_scopes - i - 1).unwrap();
    //         // If we are not in the first scope, we should add the size of the scope itself to be able to go back to the local fp[0] position
    //         if i != 0 {
    //             scope_offset += scope.nb_vars as i32 * 4; // Each variable is 4 bytes
    //         }

    //         if i == final_scope_id {
    //             if let Some(offset) = scope.get_var(name) {
    //                 return Some(scope_offset - (*offset as i32));
    //             } else {
    //                 return None;
    //             }
    //         } else {
    //             // We add the base offset due to the frame layout
    //             scope_offset += 20;
    //         }
    //     }
    //     None
    // }

    //* stack frame layout:
    // ... things before ...
    // 16[fp]    arg 1
    // 12[fp]    arg 2
    //  8[fp]    arg 3
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.
    // Computes the signed offset of a variable in the stack relative to the current fp value (innermost scope)
    fn get_var_offset(&mut self, name: &String) -> Option<i32> {
        // The final offset depends on the scope it was defined in.
        let mut scope_offset: i32 = 0;
        for scope in self.var_env.iter().rev() {
            // If we are not in the first scope, we should add the size of the scope itself to be able to go back to the local fp[0] position
            if scope_offset != 0 {
                scope_offset += scope.nb_vars as i32 * 4; // Each variable is 4 bytes
            }

            if let Some(offset) = scope.get_var(name) {
                return Some(scope_offset - (*offset as i32));
            }
            // It was not defined in this scope, so we add the +20 bytes of the scope which are located above fp[0]
            scope_offset += 20;
        }
        None
    }

    fn pretty_print_scopes(&self) {
        println!("Current scopes:");
        for scope in self.var_env.iter() {
            println!("{}\n", scope);
        }
    }

    //? Helper methods to push and pop registers

    fn push(&mut self, reg: Reg) {
        self.add_instr(addiu(sp, sp, -4).comment("Allocate space on the stack"));
        self.add_instr(
            sw(reg, 0, sp).comment(format!("Store register {:?} on the stack", reg).as_str()),
        );
    }

    fn pop(&mut self, reg: Reg) {
        self.add_instr(
            lw(reg, 0, sp).comment(format!("Load register {:?} from the stack", reg).as_str()),
        );
        self.add_instr(addiu(sp, sp, 4).comment("Deallocate space on the stack"));
    }

    //? Helper methods to generate code for unary operations

    fn process_unop(&mut self, op: UnOp, expr: Expr) {
        self.process_expr(expr);
        self.pop(t0); // Pop the operand

        match op {
            UnOp::Bang => {
                self.add_instr(xori(t0, t0, 1).comment("Proceeds UnOp::Bang")); // Logical negation
            }
            UnOp::Neg => {
                self.add_instr(subu(t0, zero, t0).comment("Proceeds UnOp::Neg"));
                // Numeric negation
            }
        }

        // At the end, we push the result back on the stack
        self.push(t0);
    }

    //? Helper methods to generate code for binary operations

    fn process_binop(&mut self, op: BinOp, left: Expr, right: Expr) {
        self.process_expr(left); // left is the farthest in the stack
        self.process_expr(right); // right is the one at the top of the stack
        self.pop(t1); // Pop the right operand
        self.pop(t0); // Pop the left operand

        match op {
            // Integer operations
            BinOp::Add => {
                self.add_instr(add(t0, t0, t1).comment("Proceeds BinOp::Add"));
            }
            BinOp::Sub => {
                self.add_instr(sub(t0, t0, t1).comment("Proceeds BinOp::Sub"));
            }
            BinOp::Mul | BinOp::Div => todo!(), //TODO: Add support for multiplication and division
            // Boolean only operations
            BinOp::And => {
                self.add_instr(and(t0, t0, t1).comment("Proceeds BinOp::And"));
            }
            BinOp::Or => {
                self.add_instr(or(t0, t0, t1).comment("Proceeds BinOp::Or"));
            }
            // Comparison operations
            BinOp::Eq => {
                self.add_instr(beq(t0, t1, 2).comment("Begins BinOp::Eq")); // If equal, jump two instructions
                self.add_instr(addi(t0, zero, 0)); // Here, the result if false
                self.add_instr(b(1)); // Then we jump the next instruction
                self.add_instr(addi(t0, zero, 1).comment("Finishes BinOp::Eq"));
                // Here, the result if true
            }
            BinOp::Ne => {
                self.add_instr(bne(t0, t1, 2).comment("Begins BinOp::Ne")); // If equal, jump two instructions
                self.add_instr(addi(t0, zero, 0)); // Here, the result if false
                self.add_instr(b(1)); // Then we jump the next instruction
                self.add_instr(addi(t0, zero, 1).comment("Finishes BinOp::Ne"));
                // Here, the result if true
            }
            BinOp::Lt => {
                self.add_instr(slt(t0, t0, t1).comment("Proceeds BinOp::Lt")); // Less than (strictly)
            }
            BinOp::Le => {
                self.add_instr(beq(t0, t1, 2).comment("Begins BinOp::Le")); // If equal, jump two instructions
                self.add_instr(slt(t0, t0, t1));
                self.add_instr(b(1)); // Then we jump the next instruction
                self.add_instr(addi(t0, zero, 1).comment("Finishes BinOp::Le"));
                // Here, the result if true
            }
            BinOp::Gt => {
                // Greater than (strictly) is Not(less than or equal)
                // ---> This is lower or equal
                self.add_instr(beq(t0, t1, 2).comment("Begins BinOp::Gt")); // If equal, jump two instructions
                self.add_instr(slt(t0, t0, t1));
                self.add_instr(b(1)); // Then we jump the next instruction
                self.add_instr(addi(t0, zero, 1)); // Here, the result if true
                                                   // ---> This is the opposite
                self.add_instr(xori(t0, t0, 1).comment("Finishes BinOp::Gt")); // Do it in place
            }
            BinOp::Ge => {
                // Greater or equal than is Not(strictly less)
                // ---> This is strictly lower than
                self.add_instr(slt(t0, t0, t1).comment("Begins BinOp::Ge")); // Less than (strictly)
                                                                             // ---> This is the opposite
                self.add_instr(xori(t0, t0, 1).comment("Finishes BinOp::Ge")); // Do it in place
            }
            BinOp::Get => todo!(), //TODO: Add support for array operations
        }

        // At the end, we push the result back on the stack
        self.push(t0);
    }

    //? Helper methods to generate code for expressions

    fn process_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Lit(lit) => {
                match lit {
                    Literal::Int(i) => {
                        self.add_instr(
                            addi(t0, zero, i as i16)
                                .comment(format!("Load integer {} in t0", i).as_str()),
                        );
                        self.push(t0);
                    }
                    Literal::Bool(b) => {
                        self.add_instr(
                            addi(t0, zero, b as i16)
                                .comment(format!("Load boolean {} in t0", b).as_str()),
                        );
                        self.push(t0);
                    }
                    _ => todo!(), //TODO: Add support for other literals
                }
            }
            Expr::Ident(ident) => {
                // Try to find the variable in a scope
                let fp_offset = self.get_var_offset(&ident);
                match fp_offset {
                    Some(offset) => {
                        let offset = offset as i16;
                        self.add_instr(
                            lw(t0, offset, fp).comment( // Store variable value in register t0
                                format!(
                                    "Load the value of {} from the stack at relative position {}",
                                    ident, offset
                                )
                                .as_str(),
                            ),
                        );
                        self.push(t0); // Then push the value to the stack
                    }
                    None => panic!("Variable {} not defined", ident), //TODO: Change this to a custom error
                }
            }
            Expr::BinOp(op, left, right) => self.process_binop(op, *left, *right),
            Expr::UnOp(op, expr) => self.process_unop(op, *expr),
            Expr::Par(expr) => self.process_expr(*expr),
            Expr::Call(name, args) => todo!(), //TODO: Add support for function calls
            Expr::IfThenElse(cond, then_block, else_block) => todo!(), //TODO: Add support for if-then-else
            Expr::Block(block) => self.process_block(block),
        }
    }

    //? Helper methods to generate code for statements

    fn process_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Let(_, name, _, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.process_expr(expr); // The value is now located on top of the stack

                    // We just have to define it in our variable environment now
                    self.define_var(&name);
                }
            }
            Statement::Assign(left, right) => todo!(), //TODO: Add support for assign statements
            Statement::While(cond, block) => todo!(),  //TODO: Add support for while loops
            Statement::Expr(expr) => self.process_expr(expr),
            Statement::Fn(fn_decl) => todo!(), //TODO: Add support for function declarations
        }
    }

    //? Helper methods to generate code for blocks

    fn process_block(&mut self, block: Block) {
        // A block starts by adding a new scope
        self.add_scope();

        // Then, it processes all the statements one by one
        // No need to check anything here, as it will have been done by the type checker
        for stmt in block.statements {
            self.process_statement(stmt);
        }

        // If the block ended up without a semi-colon, it returns the last value to the scope before
        if !block.semi {
            self.pop(t0); // Pop the last value
        }

        // Finally, it removes the scope
        self.remove_scope();

        // If the block ended up without a semi-colon, it returns the last value to the scope before
        if !block.semi {
            self.push(t0); // Push the last value to the stack
        }
    }
}
