use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Mutable, Parameter, Parameters, Prog,
    Statement, Type, UnOp,
};

use crate::common::*;
use crate::error::Error;
use mips::instr;
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
        let expr_instrs = bvm.process_expr(self.clone());
        bvm.add_instrs(expr_instrs);

        //? Debug, but may be commented out
        // bvm.pretty_print_instructions();

        Ok(bvm.get_mips())
    }
}

impl GetMips for Block {
    fn get_mips(&self) -> Result<Mips, Error> {
        let mut bvm = BVM::new();
        let block_instrs = bvm.process_block(self.clone());
        bvm.add_instrs(block_instrs);

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

        let init_instr = bvm.add_scope(); // Add the global scope
        bvm.add_instrs(init_instr);

        bvm
    }

    pub fn get_mips(&self) -> Mips {
        Mips::new(self.instructions.clone())
    }

    //? Helper method for instructions

    fn add_instr(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }

    fn add_instrs(&mut self, instrs: Instrs) {
        for instr in instrs.iter() {
            self.add_instr(instr.clone());
        }
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

    fn add_scope(&mut self) -> Instrs {
        let mut new_scope = Scope::new();

        let mut instrs = Instrs::new();

        instrs.push(
            addiu(sp, sp, -(new_scope.size as i16)).comment(
                format!(
                    "Allocate default space for the new scope (id {})",
                    self.var_env.len()
                )
                .as_str(),
            ),
        );
        self.var_env.push(new_scope);

        // Then, push the current frame pointer, and set the new frame pointer to the current stack pointer
        instrs.push(sw(fp, 0, sp).comment("Save the old frame pointer"));
        instrs.push(
            addi(fp, sp, 0).comment("Set the new frame pointer to the current stack pointer"),
        );

        instrs
    }

    fn remove_scope(&mut self) -> Instrs {
        let scope = self.var_env.pop().unwrap(); // Get last scope

        let mut instrs = Instrs::new();

        // Restore the old frame pointer
        instrs.push(lw(fp, 0, fp).comment("Restore the old frame pointer"));

        instrs.push(
            addiu(sp, sp, scope.size as i16).comment(
                format!(
                    "Deallocate space used by the scope (id {})",
                    self.var_env.len()
                )
                .as_str(),
            ),
        );

        instrs
    }

    fn define_var(&mut self, name: &String) {
        self.var_env.last_mut().unwrap().add_var(name.clone());
    }

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

    fn push(&mut self, reg: Reg) -> Instrs {
        let mut push_instrs = Instrs::new();

        push_instrs.push(addiu(sp, sp, -4).comment("Allocate space on the stack"));
        push_instrs.push(
            sw(reg, 0, sp).comment(format!("Store register {:?} on the stack", reg).as_str()),
        );

        push_instrs
    }

    fn pop(&mut self, reg: Reg) -> Instrs {
        let mut pop_instrs = Instrs::new();

        pop_instrs.push(
            lw(reg, 0, sp).comment(format!("Load register {:?} from the stack", reg).as_str()),
        );
        pop_instrs.push(addiu(sp, sp, 4).comment("Deallocate space on the stack"));

        pop_instrs
    }

    //? Helper methods to generate code for unary operations

    fn process_unop(&mut self, op: UnOp, expr: Expr) -> Instrs {
        let mut unop_instrs = Instrs::new();

        unop_instrs.append(&mut self.process_expr(expr));
        unop_instrs.append(&mut self.pop(t0)); // Pop the operand

        match op {
            UnOp::Bang => {
                unop_instrs.push(xori(t0, t0, 1).comment("Proceed UnOp::Bang"));
                // Logical negation
            }
            UnOp::Neg => {
                unop_instrs.push(subu(t0, zero, t0).comment("Proceed UnOp::Neg"));
                // Numeric negation
            }
        }

        // At the end, we push the result back on the stack
        unop_instrs.append(&mut self.push(t0));

        unop_instrs
    }

    //? Helper methods to generate code for binary operations

    fn process_binop(&mut self, op: BinOp, left: Expr, right: Expr) -> Instrs {
        let mut binop_instrs = Instrs::new();

        binop_instrs.append(&mut self.process_expr(left)); // left is the farthest in the stack
        binop_instrs.append(&mut self.process_expr(right)); // right is the one at the top of the stack
        binop_instrs.append(&mut self.pop(t1)); // Pop the right operand
        binop_instrs.append(&mut self.pop(t0)); // Pop the left operand

        match op {
            // Integer operations
            BinOp::Add => {
                binop_instrs.push(addu(t0, t0, t1).comment("Proceed BinOp::Add"));
            }
            BinOp::Sub => {
                // Handles the subtraction overflow when going to negative values by using subu
                binop_instrs.push(subu(t0, t0, t1).comment("Proceed BinOp::Sub"));
            }
            BinOp::Mul => {
                // For a multiplication, we need to initialize a counter to do the correct number of multiplications
                // For now, not optimized, the counter is always the right operand
                // For an optimized version, we should take the lowest value (in absolute value) as the counter

                // Let's compute the result in t0. Let's store the left operand in t2.
                // Let's use t3 to store the (negative) counter, and t4 to store the boolean to show whether it was positive or not.
                binop_instrs.push(addu(t2, zero, t0).comment("Store the addition value (Begin multiplication)"));
                binop_instrs.push(addu(t0, zero, zero).comment("Initialize the result to 0"));
                binop_instrs.push(addi(t4, zero, 1).comment("Initialize the negative counter boolean to True"));
                
                // If the right operand is negative, we need to negate it
                binop_instrs.push(addu(t3, zero, t1).comment("Initialize the counter to the right operand"));
                binop_instrs.push(blez(t3, 2).comment("Check if the counter is negative. If so, skip the negation"));
                binop_instrs.push(subu(t3, zero, t3).comment("Negate the counter"));
                binop_instrs.push(addi(t4, zero, 0).comment("Set the negative counter boolean to False"));

                // Actual loop for the multiplication
                binop_instrs.push(beq(t3, zero, 3).comment("Multiplication loop: check if the counter is 0"));
                binop_instrs.push(addu(t0, t0, t2).comment("Proceed a single addition for the multiplication"));
                binop_instrs.push(addiu(t3, t3, 1).comment("Update the counter"));
                binop_instrs.push(b(-4).comment("Branch back to multiplication loop"));

                // At the end, if the counter was negative, we need to negate the result
                binop_instrs.push(beq(t4, zero, 1).comment("Skip next instruction if the counter was positive"));
                binop_instrs.push(subu(t0, zero, t0).comment("Negate the result (the counter was negative) (End multiplication)"));
            },
            BinOp::Div => {
                // For a division, we need to count the number of times we can subtract the right operand from the left operand

                // Let's compute the result in t0.
                // Let's store the positive left operand in t2 (and the remainder).
                // Let's store the positive right operand in t1.
                // Let's store the "is left operand negative?" in t4.
                // Let's store the "is right operand negative?" in t3.
                // Let's store the negated left operand in t6. (For boolean checks, since we only have branch on <= 0)
                // Let's store the negated right operand in t5. (For boolean checks, since we only have branch on <= 0)
                // Let's store the condition to check in t7.

                //* Initialization
                binop_instrs.push(addu(t2, zero, t0).comment("Store the left operand (Begin division)"));
                binop_instrs.push(subu(t6, zero, t2).comment("Store the negated left operand"));
                binop_instrs.push(subu(t5, zero, t1).comment("Store the negated right operand"));
                binop_instrs.push(addi(t4, zero, 0).comment("Initialize the 'is left operand negative?' boolean to False"));
                binop_instrs.push(addi(t3, zero, 0).comment("Initialize the 'is right operand negative?' boolean to False"));

                binop_instrs.push(blez(t6, 2).comment("Check if the left operand is positive. If so, skip the negation"));
                binop_instrs.push(subu(t2, zero, t2).comment("Negate the left operand to make it positive"));
                binop_instrs.push(addi(t4, zero, 1).comment("Set the 'is left operand negative?' boolean to True"));

                binop_instrs.push(blez(t5, 2).comment("Check if the right operand is positive. If so, skip the negation"));
                binop_instrs.push(subu(t1, zero, t1).comment("Negate the right operand to make it positive"));
                binop_instrs.push(addi(t3, zero, 1).comment("Set the 'is right operand negative?' boolean to True"));
                
                binop_instrs.push(addu(t0, zero, zero).comment("Initialize the result to 0"));

                //* Actual division loop
                binop_instrs.push(subu(t2, t2, t1).comment("Compute new remainder (Begin division loop)"));
                binop_instrs.push(beq(t2, zero, 1).comment("Division loop: check if the remainder is null (division must continue)"));
                binop_instrs.push(blez(t2, 2).comment("Division loop: check if the remainder is strictly negative (Division finished)"));
                binop_instrs.push(addi(t0, t0, 1).comment("Update the result"));
                binop_instrs.push(b(-5).comment("Branch back to division loop (End division loop)"));

                //* At the end, we need to check if the result should be negated
                binop_instrs.push(beq(t4, zero, 1).comment("Skip next result negation if the left operand was positive"));
                binop_instrs.push(subu(t0, zero, t0).comment("Negate the result (the left operand was negative)"));
                binop_instrs.push(beq(t3, zero, 1).comment("Skip next result negation if the right operand was positive"));
                binop_instrs.push(subu(t0, zero, t0).comment("Negate the result (the right operand was negative) (End division)"));
            },
            // Boolean only operations
            BinOp::And => {
                binop_instrs.push(and(t0, t0, t1).comment("Proceed BinOp::And"));
            }
            BinOp::Or => {
                binop_instrs.push(or(t0, t0, t1).comment("Proceed BinOp::Or"));
            }
            // Comparison operations
            BinOp::Eq => {
                binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Eq")); // If equal, jump two instructions
                binop_instrs.push(addi(t0, zero, 0)); // Here, the result if false
                binop_instrs.push(b(1)); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Eq"));
                // Here, the result if true
            }
            BinOp::Ne => {
                binop_instrs.push(bne(t0, t1, 2).comment("Begin BinOp::Ne")); // If equal, jump two instructions
                binop_instrs.push(addi(t0, zero, 0)); // Here, the result if false
                binop_instrs.push(b(1)); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Ne"));
                // Here, the result if true
            }
            BinOp::Lt => {
                binop_instrs.push(slt(t0, t0, t1).comment("Proceed BinOp::Lt"));
                // Less than (strictly)
            }
            BinOp::Le => {
                binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Le")); // If equal, jump two instructions
                binop_instrs.push(slt(t0, t0, t1));
                binop_instrs.push(b(1)); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Le"));
                // Here, the result if true
            }
            BinOp::Gt => {
                // Greater than (strictly) is Not(less than or equal)
                // ---> This is lower or equal
                binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Gt")); // If equal, jump two instructions
                binop_instrs.push(slt(t0, t0, t1));
                binop_instrs.push(b(1)); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1)); // Here, the result if true
                                                      // ---> This is the opposite
                binop_instrs.push(xori(t0, t0, 1).comment("Finish BinOp::Gt"));
                // Do it in place
            }
            BinOp::Ge => {
                // Greater or equal than is Not(strictly less)
                // ---> This is strictly lower than
                binop_instrs.push(slt(t0, t0, t1).comment("Begin BinOp::Ge")); // Less than (strictly)
                                                                                // ---> This is the opposite
                binop_instrs.push(xori(t0, t0, 1).comment("Finish BinOp::Ge"));
                // Do it in place
            }
            BinOp::Get => todo!(), //TODO: Add support for array operations
        }

        // At the end, we push the result back on the stack
        binop_instrs.append(&mut self.push(t0));

        binop_instrs
    }

    //? Helper methods to generate code for expressions

    fn process_expr(&mut self, expr: Expr) -> Instrs {
        let mut expr_instrs = Instrs::new();

        match expr {
            Expr::Lit(lit) => {
                match lit {
                    Literal::Int(i) => {
                        expr_instrs.push(
                            addi(t0, zero, i as i16)
                                .comment(format!("Load integer {} in t0", i).as_str()),
                        );
                        expr_instrs.append(&mut self.push(t0));
                    }
                    Literal::Bool(b) => {
                        expr_instrs.push(
                            addi(t0, zero, b as i16)
                                .comment(format!("Load boolean {} in t0", b).as_str()),
                        );
                        expr_instrs.append(&mut self.push(t0));
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
                        expr_instrs.push(
                            lw(t0, offset, fp).comment(
                                // Store variable value in register t0
                                format!(
                                    "Load the value of {} from the stack at relative position {}",
                                    ident, offset
                                )
                                .as_str(),
                            ),
                        );
                        expr_instrs.append(&mut self.push(t0)); // Then push the value to the stack
                    }
                    // This is unreachable because the type checker should have checked this before
                    None => unreachable!("Variable '{}' not defined", ident), //TODO: Change this to a custom error
                }
            }
            Expr::BinOp(op, left, right) => {
                expr_instrs.append(&mut self.process_binop(op, *left, *right))
            }
            Expr::UnOp(op, expr) => expr_instrs.append(&mut self.process_unop(op, *expr)),
            Expr::Par(expr) => expr_instrs.append(&mut self.process_expr(*expr)),
            Expr::Call(name, args) => {
                todo!() //TODO: Add support for function calls
            }
            Expr::IfThenElse(cond, then_block, else_block) => {
                // Process the condition
                expr_instrs.append(&mut self.process_expr(*cond)); // It is located in t0, AND at the top of the stack
                expr_instrs.append(&mut self.pop(t0)); // Pop the condition to t0 (seems useless, but if forgotten, the frame layout will be wrong)

                // Get the block instructions ready
                let mut then_instrs = self.process_block(then_block);
                let mut nb_then_instrs = then_instrs.len() as i16;

                // If the condition is false, we jump and avoid the then block
                // Jumping 0 is going to the next instruction, so we should jump the number of instructions in the then block
                // If there is an else block, we must add 1 because of the jump after the then block
                if else_block.is_some() {
                    nb_then_instrs += 1;
                }
                expr_instrs.push(
                    beq(t0, zero, nb_then_instrs).comment("IfThenElse jump on false condition"),
                );
                // Now we can put the then block
                expr_instrs.append(&mut then_instrs);

                if let Some(else_block) = else_block {
                    // Get the block instructions ready
                    let mut else_instrs = self.process_block(else_block);
                    let nb_else_instrs = else_instrs.len() as i16;

                    // If there is an else block, we should jump after the then block
                    expr_instrs.push(b(nb_else_instrs).comment("IfThenElse jump after then block"));
                    // Now we can put the else block
                    expr_instrs.append(&mut else_instrs);
                }
            }
            Expr::Block(block) => expr_instrs.append(&mut self.process_block(block)),
        }

        expr_instrs
    }

    //? Helper methods to generate code for statements

    fn process_statement(&mut self, stmt: Statement) -> Instrs {
        let mut stmt_instrs = Instrs::new();

        match stmt {
            Statement::Let(_, name, _, expr_opt) => {
                if let Some(expr) = expr_opt {
                    stmt_instrs.append(&mut self.process_expr(expr)); // The value is now located on top of the stack
                } else {
                    // If there is no expression, we just define the variable. It will be initialized to 0
                    stmt_instrs.push(addi(t0, zero, 0).comment("Initialize variable to 0"));
                    stmt_instrs.append(&mut self.push(t0)); // Push the value to the stack
                }

                // Now we define the variable in our environment
                self.define_var(&name);
            }
            Statement::Assign(left, right) => {
                // We first process the expression to get the new value
                stmt_instrs.append(&mut self.process_expr(right)); // The value is now located on top of the stack
                stmt_instrs.append(&mut self.pop(t0)); // Pop the value to t0

                // Debug
                self.pretty_print_scopes();

                match left {
                    Expr::Ident(name) => {
                        // To assign a value to a variable, we need to find it in the stack
                        let fp_offset = self.get_var_offset(&name);

                        println!("Var offset: {:?}", fp_offset);

                        // Then, we store the value in the variable
                        match fp_offset {
                            Some(offset) => {
                                let offset = offset as i16;
                                stmt_instrs.push(
                                    sw(t0, offset, fp).comment(
                                        // Store variable value in register t0
                                        format!(
                                            "Set the value of var '{}' in the stack at relative position {} to value of t0",
                                            name, offset
                                        )
                                        .as_str(),
                                    ),
                                );
                            }
                            // This is unreachable because the type checker should have checked this before
                            None => unreachable!("Variable '{}' not defined", name),
                        }
                    }
                    _ => todo!(), //TODO: Add support for other assignments such as arrays
                }
            }
            Statement::While(cond, block) => {
                // While loops are an if statement, where the last instruction jumps back to the condition
                let mut while_instrs = Instrs::new();

                // First look at the condition
                while_instrs.append(&mut self.process_expr(cond)); // It is located in t0, but also at the top of the stack
                while_instrs.append(&mut self.pop(t0)); // Pop the condition to t0 (seems useless, but if forgotten, the frame layout will be wrong)

                // Get the block instructions ready
                let mut block_instrs = self.process_block(block);
                let nb_block_instrs = block_instrs.len() as i16 + 1; // Needs +1 because will have a final jump back to the condition instruction

                // If the condition is false, we jump and avoid the block, and the jump back to the condition
                while_instrs.push(
                    beq(t0, zero, nb_block_instrs).comment("While jump on false condition"),
                );

                // Then we can add the block instructions
                while_instrs.append(&mut block_instrs);

                // And finally, we jump back to the condition
                let jump_back_nb = -(while_instrs.len() as i16) - 1; // Needs -1 because the jump is relative to the next instruction
                while_instrs.push(b(jump_back_nb).comment("While jump back to condition"));

                stmt_instrs.append(&mut while_instrs);
            },
            Statement::Expr(expr) => stmt_instrs.append(&mut self.process_expr(expr)),
            Statement::Fn(fn_decl) => todo!(), //TODO: Add support for function declarations
        }

        stmt_instrs
    }

    //? Helper methods to generate code for blocks

    fn process_block(&mut self, block: Block) -> Instrs {
        let mut block_instrs = Instrs::new();

        // A block starts by adding a new scope
        let mut scope_instr = self.add_scope();
        block_instrs.append(&mut scope_instr);

        // Then, it processes all the statements one by one
        // No need to check anything here, as it will have been done by the type checker
        for stmt in block.statements {
            block_instrs.append(&mut self.process_statement(stmt));
        }

        // If the block ended up without a semi-colon, it returns the last value to the scope before
        if !block.semi {
            block_instrs.append(&mut self.pop(t0)); // Pop the last value
        }

        // Finally, it removes the scope
        let mut scope_instr = self.remove_scope();
        block_instrs.append(&mut scope_instr);

        // If the block ended up without a semi-colon, it returns the last value to the scope before
        if !block.semi {
            block_instrs.append(&mut self.push(t0)); // Push the last value to the stack
        }

        block_instrs
    }
}
