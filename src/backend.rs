use core::fmt;
use std::cmp::min;
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

const DEFAULT_SCOPE_SIZE: u32 = 20;
const DEBUG_PRINTS: bool = false;

#[derive(Clone, Debug)]
pub struct Scope {
    size: u32,                       // Tracks the size of the scope on fp values (It only tracks the negative part of the stack)
    vars: HashMap<String, i32>,      // Tracks the position of the variables on the stack (relative to fp): name -> position (-4, -8, ... or even 16, 12, ...)
    functions: HashMap<String, u32>, // Tracks the position of the functions in the instructions memory
}

//? Helpers

impl Scope {
    // Vars will be stored on the stack.
    // Functions will be stocked in the instructions memory.
    pub fn new() -> Self {
        Self {
            size: 0, // Default size of the scope due to the frame layout
            vars: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    // Used to define variables, not functions arguments
    pub fn add_var(&mut self, name: String) {
        self.vars.insert(name, -(self.size as i32));
    }

    pub fn get_var(&self, name: &String) -> Option<&i32> {
        self.vars.get(name)
    }

    pub fn update_on_push(&mut self) {
        self.size += 4;
    }

    pub fn update_on_pop(&mut self) {
        self.size -= 4;
    }

    pub fn add_function(&mut self, name: String, offset: u32) {
        self.functions.insert(name, offset);
    }

    pub fn get_function(&self, name: &String) -> Option<&u32> {
        self.functions.get(name)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Scope: size={}\n| Vars:\n",
            self.size
        )?;
        for (name, offset) in self.vars.iter() {
            write!(f, "|\t- {}: {}\n", name, offset)?;
        }
        write!(f, "| Functions:\n")?;
        for (name, offset) in self.functions.iter() {
            write!(f, "|\t- {}: {}\n", name, offset)?;
        }
        write!(f, "")
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                     Processing programs                                       #
//?#                                                                                               #
//?#################################################################################################

impl GetInstructions for Expr {
    fn get_instructions(&self) -> Result<Instrs, Error> {
        let mut bvm = BVM::new();

        let expected_size = bvm.get_expr_len(self.clone());
        let expr_instrs = bvm.process_expr(self.clone());

        if DEBUG_PRINTS {
            println!(
                "Size verification (Expr): (does not include the default scope which is 4 instr long)\nExpected size: {}\nReal size: {}",
                expected_size,
                expr_instrs.len()
            );

            bvm.add_instrs(expr_instrs.clone());

            bvm.pretty_print_instructions();
        }
        
        Ok(expr_instrs)
    }
}

impl GetInstructions for Block {
    fn get_instructions(&self) -> Result<Instrs, Error> {
        let mut bvm = BVM::new();

        let expected_size = bvm.get_block_len(self.clone());
        let block_instrs = bvm.process_block(self.clone());

        if DEBUG_PRINTS {
            println!(
                "Size verification (Block): (does not include the default scope which is 4 instr long)\nExpected size: {}\nReal size: {}",
                expected_size,
                block_instrs.len()
            );

            bvm.add_instrs(block_instrs.clone());

            bvm.pretty_print_instructions();
        }

        Ok(block_instrs)
    }
}

impl GetInstructions for Prog {
    fn get_instructions(&self) -> Result<Instrs, Error> {
        let mut bvm = BVM::new();

        let expected_size = bvm.get_prog_len(self.clone());
        let prog_intrs = bvm.process_prog(self.clone());

        if DEBUG_PRINTS {
            println!(
                "Size verification (Prog): (does not include the default scope which is 4 instr long)\nExpected size: {}\nReal size: {}",
                expected_size,
                prog_intrs.len()
            );

            bvm.add_instrs(prog_intrs.clone());

            bvm.pretty_print_instructions();
        }

        Ok(prog_intrs)
    }
}

impl<T: GetInstructions> GetMips for T {
    fn get_mips(&self) -> Result<Mips, Error> {
        let instrs = self.get_instructions()?;
        let mips = Mips::new(instrs);
        Ok(mips)
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                     Format instructions                                       #
//?#                                                                                               #
//?#################################################################################################

pub fn get_formatted_instrs(instrs: Instrs) -> String {
    let mut formatted_instrs = String::new();
    let mut i = 0;
    for instr in instrs.iter() {
        formatted_instrs.push_str(&format!("{}\t{}\n", i, instr));
        i += 1;
    }
    formatted_instrs
}

//?#################################################################################################
//?#                                                                                               #
//?#                                   Backend Virtual Machine                                     #
//?#                                                                                               #
//?#################################################################################################

pub struct BVM {
    var_env: Vec<Scope>, // Tracks the variables in the current scope

    // Manage instructions and be able to get jump offsets
    pc: i32, // Program counter (used to keep track of the local instructions even when they are not added yet)
    instructions: Instrs, // Stores the current instructions
}

impl BVM {
    pub fn new() -> Self {
        let mut bvm = Self {
            var_env: vec![],
            pc: -1, // Start at -1 to be 0 on first instruction
            instructions: Instrs::new(),
        };

        let init_instr = bvm.add_scope(); // Add the global scope
        bvm.add_instrs(init_instr);

        bvm
    }

    pub fn get_instructions(&self) -> Instrs {
        self.instructions.clone()
    }

    pub fn get_mips(&self) -> Mips {
        Mips::new(self.instructions.clone())
    }

    //? Helper method for instructions

    fn add_instr(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }

    fn add_instrs(&mut self, instrs: Instrs) {
        // Update the program counter
        for instr in instrs.iter() {
            self.add_instr(instr.clone());
        }
    }

    pub fn pretty_print_instructions(&self) {
        println!(
            "BVM Instructions state:\npc (nb_instrs-1 -> from 0 to pc): {}\nInstructions:",
            self.pc
        );
        let mut i = 0;
        for instr in self.instructions.iter() {
            println!("{}\t{}", i, instr);
            i += 1;
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

    fn get_add_scope_len(&self) -> u32 {
        4 // 4 instructions
    }

    fn add_scope(&mut self) -> Instrs {
        let new_scope = Scope::new();
        let scope_id = self.var_env.len();
        self.var_env.push(new_scope);

        let mut instrs = Instrs::new();

        instrs.push(
            addiu(sp, sp, -(DEFAULT_SCOPE_SIZE as i16)).comment(
                format!(
                    "ADD_SCOPE {}: Allocate default space for the new scope",
                    scope_id
                )
                .as_str(),
            ),
        ); // This corresponds to 5 pushes

        // Then, push the current frame pointer, and set the new frame pointer to the current stack pointer
        instrs.push(
            sw(ra, 4, sp)
                .comment(format!("ADD_SCOPE {}: Save the return address", scope_id).as_str()),
        );
        instrs.push(
            sw(fp, 0, sp)
                .comment(format!("ADD_SCOPE {}: Save the old frame pointer", scope_id).as_str()),
        );
        instrs.push(
            addi(fp, sp, 0).comment(
                format!(
                    "ADD_SCOPE {}: Set the new frame pointer to the current stack pointer",
                    scope_id
                )
                .as_str(),
            ),
        );

        self.pc += 4;

        instrs
    }

    fn get_remove_scope_len(&self) -> u32 {
        3 // 3 instructions
    }

    fn remove_scope(&mut self) -> Instrs {
        let scope = self.var_env.pop().unwrap(); // Get last scope
        let scope_id = self.var_env.len();

        let mut instrs = Instrs::new();

        // Restore the old frame pointer and the return address
        instrs
            .push(lw(ra, 4, fp).comment(
                format!("REMOVE_SCOPE {}: Restore the return address", scope_id).as_str(),
            ));
        instrs.push(
            lw(fp, 0, fp).comment(
                format!("REMOVE_SCOPE {}: Restore the old frame pointer", scope_id).as_str(),
            ),
        );

        let total_size = scope.size + DEFAULT_SCOPE_SIZE;

        instrs.push(
            addiu(sp, sp, total_size as i16).comment(
                format!(
                    "REMOVE_SCOPE {}: Deallocate space used by the scope",
                    scope_id
                )
                .as_str(),
            ),
        ); // This pops everything that was not popped yet

        self.pc += 3;

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
                scope_offset += scope.size as i32; // Each variable is 4 bytes
            }

            if let Some(offset) = scope.get_var(name) {
                return Some(scope_offset + *offset);
            }
            // It was not defined in this scope, so we add the +20 bytes of the scope which are located above fp[0]
            scope_offset += DEFAULT_SCOPE_SIZE as i32;
        }
        None
    }

    fn define_function(&mut self, name: &String, offset: u32) {
        self.var_env
            .last_mut()
            .unwrap()
            .add_function(name.clone(), offset);
    }

    fn define_functions_vec(&mut self, functions: &Vec<FnDeclaration>) {
        // this method aims to define every function from the list without processing their bodies
        // so that they can call each other with the correct offsets and everything.

        let mut local_pc = self.pc as u32;

        for fd in functions.iter() {
            // The function will start at the next instruction
            self.define_function(&fd.id, local_pc + 1);
            // The function takes x instructions, where x is:
            let func_len = self.get_func_def_len(fd.clone());
            local_pc += func_len;
        }
    }

    fn get_function_offset(&mut self, name: &String) -> Option<u32> {
        for scope in self.var_env.iter().rev() {
            if let Some(offset) = scope.get_function(name) {
                return Some(*offset);
            }
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

    fn get_push_len(&self) -> u32 {
        2 // 2 instructions
    }

    fn push(&mut self, reg: Reg) -> Instrs {
        let mut push_instrs = Instrs::new();

        push_instrs.push(
            addiu(sp, sp, -4)
                .comment(format!("PUSH {:?}: Allocate space on the stack", reg).as_str()),
        );
        push_instrs
            .push(sw(reg, 0, sp).comment(
                format!("PUSH {:?}: Store register {:?} on the stack", reg, reg).as_str(),
            ));

        self.pc += 2;
        self.var_env.last_mut().unwrap().update_on_push(); // Update the size of the scope

        push_instrs
    }

    fn get_pop_len(&self) -> u32 {
        2 // 2 instructions
    }

    fn pop(&mut self, reg: Reg) -> Instrs {
        let mut pop_instrs = Instrs::new();

        pop_instrs.push(
            lw(reg, 0, sp).comment(
                format!("POP TO {:?}: Load register {:?} from the stack", reg, reg).as_str(),
            ),
        );
        pop_instrs.push(
            addiu(sp, sp, 4)
                .comment(format!("POP TO {:?}: Deallocate space on the stack", reg).as_str()),
        );

        self.pc += 2;
        self.var_env.last_mut().unwrap().update_on_pop(); // Update the size of the scope

        pop_instrs
    }

    //? Helper methods to generate code for unary operations

    fn get_unop_len(&self, op: UnOp, expr: Expr) -> u32 {
        let mut unop_len = self.get_expr_len(expr);
        unop_len += self.get_pop_len();
        unop_len += 1; // The operation itself
        unop_len += self.get_push_len();

        unop_len
    }

    fn process_unop(&mut self, op: UnOp, expr: Expr) -> Instrs {
        let mut unop_instrs = Instrs::new();

        unop_instrs.append(&mut self.process_expr(expr));
        unop_instrs.append(&mut self.pop(t0)); // Pop the operand

        match op {
            UnOp::Bang => {
                unop_instrs.push(xori(t0, t0, 1).comment("Proceed UnOp::Bang"));
                self.pc += 1;
                // Logical negation
            }
            UnOp::Neg => {
                unop_instrs.push(subu(t0, zero, t0).comment("Proceed UnOp::Neg"));
                self.pc += 1;
                // Numeric negation
            }
        }

        // At the end, we push the result back on the stack
        unop_instrs.append(&mut self.push(t0));

        unop_instrs
    }

    //? Helper methods to generate code for binary operations

    fn get_binop_len(&self, op: BinOp, left: Expr, right: Expr) -> u32 {
        let mut binop_len = self.get_expr_len(left);
        binop_len += self.get_expr_len(right);
        binop_len += self.get_pop_len() * 2;

        match op {
            BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Lt => {
                binop_len += 1; // The operation itself
            }
            BinOp::Mul => {
                binop_len += 13; // The operation itself
            }
            BinOp::Div => {
                binop_len += 21; // The operation itself
            }
            BinOp::Eq | BinOp::Ne | BinOp::Le => {
                binop_len += 4; // The operation itself
            }
            BinOp::Gt => {
                binop_len += 5; // The operation itself
            }
            BinOp::Ge => {
                binop_len += 2; // The operation itself
            }
            BinOp::Get => todo!(), //TODO: Add support for array operations
        }

        binop_len += self.get_push_len();

        binop_len
    }

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
                self.pc += 1;
            }
            BinOp::Sub => {
                // Handles the subtraction overflow when going to negative values by using subu
                binop_instrs.push(subu(t0, t0, t1).comment("Proceed BinOp::Sub"));
                self.pc += 1;
            }
            BinOp::Mul => {
                // For a multiplication, we need to initialize a counter to do the correct number of multiplications
                // For now, not optimized, the counter is always the right operand
                // For an optimized version, we should take the lowest value (in absolute value) as the counter

                // Let's compute the result in t0. Let's store the left operand in t2.
                // Let's use t3 to store the (negative) counter, and t4 to store the boolean to show whether it was positive or not.
                binop_instrs.push(
                    addu(t2, zero, t0).comment("Store the addition value (Begin multiplication)"),
                );
                binop_instrs.push(addu(t0, zero, zero).comment("Initialize the result to 0"));
                binop_instrs.push(
                    addi(t4, zero, 1).comment("Initialize the negative counter boolean to True"),
                );

                // If the right operand is negative, we need to negate it
                binop_instrs.push(
                    addu(t3, zero, t1).comment("Initialize the counter to the right operand"),
                );
                binop_instrs.push(
                    blez(t3, 2)
                        .comment("Check if the counter is negative. If so, skip the negation"),
                );
                binop_instrs.push(subu(t3, zero, t3).comment("Negate the counter"));
                binop_instrs
                    .push(addi(t4, zero, 0).comment("Set the negative counter boolean to False"));

                // Actual loop for the multiplication
                binop_instrs.push(
                    beq(t3, zero, 3).comment("Multiplication loop: check if the counter is 0"),
                );
                binop_instrs.push(
                    addu(t0, t0, t2).comment("Proceed a single addition for the multiplication"),
                );
                binop_instrs.push(addiu(t3, t3, 1).comment("Update the counter"));
                binop_instrs.push(b(-4).comment("Branch back to multiplication loop"));

                // At the end, if the counter was negative, we need to negate the result
                binop_instrs.push(
                    beq(t4, zero, 1).comment("Skip next instruction if the counter was positive"),
                );
                binop_instrs.push(
                    subu(t0, zero, t0).comment(
                        "Negate the result (the counter was negative) (End multiplication)",
                    ),
                );
                self.pc += 13;
            }
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
                binop_instrs
                    .push(addu(t2, zero, t0).comment("Store the left operand (Begin division)"));
                binop_instrs.push(subu(t6, zero, t2).comment("Store the negated left operand"));
                binop_instrs.push(subu(t5, zero, t1).comment("Store the negated right operand"));
                binop_instrs.push(
                    addi(t4, zero, 0)
                        .comment("Initialize the 'is left operand negative?' boolean to False"),
                );
                binop_instrs.push(
                    addi(t3, zero, 0)
                        .comment("Initialize the 'is right operand negative?' boolean to False"),
                );

                binop_instrs
                    .push(blez(t6, 2).comment(
                        "Check if the left operand is positive. If so, skip the negation",
                    ));
                binop_instrs.push(
                    subu(t2, zero, t2).comment("Negate the left operand to make it positive"),
                );
                binop_instrs.push(
                    addi(t4, zero, 1)
                        .comment("Set the 'is left operand negative?' boolean to True"),
                );

                binop_instrs.push(
                    blez(t5, 2).comment(
                        "Check if the right operand is positive. If so, skip the negation",
                    ),
                );
                binop_instrs.push(
                    subu(t1, zero, t1).comment("Negate the right operand to make it positive"),
                );
                binop_instrs.push(
                    addi(t3, zero, 1)
                        .comment("Set the 'is right operand negative?' boolean to True"),
                );

                binop_instrs.push(addu(t0, zero, zero).comment("Initialize the result to 0"));

                //* Actual division loop
                binop_instrs
                    .push(subu(t2, t2, t1).comment("Compute new remainder (Begin division loop)"));
                binop_instrs.push(beq(t2, zero, 1).comment(
                    "Division loop: check if the remainder is null (division must continue)",
                ));
                binop_instrs.push(blez(t2, 2).comment("Division loop: check if the remainder is strictly negative (Division finished)"));
                binop_instrs.push(addi(t0, t0, 1).comment("Update the result"));
                binop_instrs
                    .push(b(-5).comment("Branch back to division loop (End division loop)"));

                //* At the end, we need to check if the result should be negated
                binop_instrs.push(
                    beq(t4, zero, 1)
                        .comment("Skip next result negation if the left operand was positive"),
                );
                binop_instrs.push(
                    subu(t0, zero, t0).comment("Negate the result (the left operand was negative)"),
                );
                binop_instrs.push(
                    beq(t3, zero, 1)
                        .comment("Skip next result negation if the right operand was positive"),
                );
                binop_instrs.push(
                    subu(t0, zero, t0).comment(
                        "Negate the result (the right operand was negative) (End division)",
                    ),
                );

                self.pc += 21;
            }
            // Boolean only operations
            BinOp::And => {
                binop_instrs.push(and(t0, t0, t1).comment("Proceed BinOp::And"));
                self.pc += 1;
            }
            BinOp::Or => {
                binop_instrs.push(or(t0, t0, t1).comment("Proceed BinOp::Or"));
                self.pc += 1;
            }
            // Comparison operations
            BinOp::Eq => {
                binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Eq")); // If equal, jump two instructions
                binop_instrs.push(addi(t0, zero, 0).comment("BinOp::Eq is false")); // Here, the result if false
                binop_instrs.push(b(1).comment("BinOp::Eq jump next instruction since false")); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Eq"));
                // Here, the result if true
                self.pc += 4;
            }
            BinOp::Ne => {
                binop_instrs.push(bne(t0, t1, 2).comment("Begin BinOp::Ne")); // If equal, jump two instructions
                binop_instrs.push(addi(t0, zero, 0).comment("BinOp::Ne is false")); // Here, the result if false
                binop_instrs.push(b(1).comment("BinOp::Ne jump next instruction since false")); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Ne"));
                // Here, the result if true
                self.pc += 4;
            }
            BinOp::Lt => {
                binop_instrs.push(slt(t0, t0, t1).comment("Proceed BinOp::Lt"));
                // Less than (strictly)
                self.pc += 1;
            }
            BinOp::Le => {
                binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Le")); // If equal, jump two instructions
                binop_instrs.push(slt(t0, t0, t1));
                binop_instrs.push(b(1)); // Then we jump the next instruction
                binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Le"));
                // Here, the result if true
                self.pc += 4;
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
                self.pc += 5;
            }
            BinOp::Ge => {
                // Greater or equal than is Not(strictly less)
                // ---> This is strictly lower than
                binop_instrs.push(slt(t0, t0, t1).comment("Begin BinOp::Ge")); // Less than (strictly)
                                                                               // ---> This is the opposite
                binop_instrs.push(xori(t0, t0, 1).comment("Finish BinOp::Ge"));
                // Do it in place
                self.pc += 2;
            }
            BinOp::Get => todo!(), //TODO: Add support for array operations
        }

        // At the end, we push the result back on the stack
        binop_instrs.append(&mut self.push(t0));

        binop_instrs
    }

    //? Helper method to generate code for functions
    // Functions are generated in the following way:
    // - We use an intermediate scope to define the arguments of the function
    // - Then, we process the body of the function
    // - Finally, we return the value of the function if there is one using the t1 register to store the boolean

    //* stack frame layout on call:
    //
    // 16[fp]    arg 1
    // 12[fp]    arg 2
    //  8[fp]    arg 3
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.
    fn get_call_len(&self, name: String, args: Arguments) -> u32 {
        let mut call_len = self.get_add_scope_len(); // Add the new scope
        for arg in args.0.iter() {
            call_len += self.get_expr_len(arg.clone());
            call_len += self.get_pop_len();
            call_len += 1;
        }
        call_len += 1; // bal
        call_len += self.get_remove_scope_len(); // Remove the new scope
        call_len += 1; // beq
        call_len += self.get_push_len(); // Push the return value

        call_len
    }

    fn process_func_call(&mut self, name: String, args: Arguments) -> Instrs {
        // A function call.
        // It shall initialize the new scope and put the correct values in the stack.
        // Then, it shall jump to the function definition.
        // Finally, it shall return the value of the function and remove the scope.

        let mut func_instrs = Instrs::new();

        let nb_args = args.0.len();
        if nb_args > 3 {
            // We only support 3 arguments for now
            panic!("Only 3 arguments are supported for now");
        }

        // prelude
        func_instrs.append(&mut self.add_scope()); // Add the new scope to define the function arguments
        for i in 0..nb_args {
            // Generate code for each argument
            let arg_offset = 16 - i as i32 * 4;
            let arg_expr = args.0.get(i).unwrap().clone();
            func_instrs.append(&mut self.process_expr(arg_expr)); // The value is now located on top of the stack
            func_instrs.append(&mut self.pop(t0)); // Pop the value to t0
            self.pc += 1;
            func_instrs.push(
                sw(t0, arg_offset as i16, fp).comment(
                    // Store argument value from register t0 to the correct argument position in the stack
                    format!(
                        "FUNC_CALL '{}': Set the value of argument {} in the stack at relative position {} to value of t0",
                        name, i, arg_offset
                    )
                    .as_str(),
                ),
            );
        }

        // Function call
        let func_offset = self.get_function_offset(&name).unwrap();

        self.pc += 1; // Setting the correct pc before the jump
        let func_offset = func_offset as i32 - (self.pc as i32 + 1); // +1 because we "jump from the next instruction"

        func_instrs.push(
            bal(func_offset as i16).comment(
                format!(
                    "FUNC_CALL '{}': Branch and link to the function definition",
                    name
                )
                .as_str(),
            ),
        );

        // postlude
        // We can now remove the scope of the function
        func_instrs.append(&mut self.remove_scope());

        // If the function returns a value, we need to return it
        // To do so, we need to check if the function returns a value or not (it is stored in t1: 1 if there is a return value, 0 if there is none)
        self.pc += 1;
        func_instrs.push(
            beq(t1, zero, 2).comment(
                // A push takes two instructions
                format!(
                    "FUNC_CALL '{}': If function does not return a value, skip the return value",
                    name
                )
                .as_str(),
            ),
        );

        func_instrs.append(&mut self.push(t0)); // Push the return value to the stack

        func_instrs
    }

    fn generate_scopes_for_function_call(&self, fd: FnDeclaration) -> Vec<Scope> {
        // Generates a scope for the function call.
        // It only has the arguments of the function in it.
        // It knows every function from the previous scopes.

        let mut function_scopes: Vec<Scope> = self.var_env.clone();

        // Then, we remove every known variable from it because we won't need them (we are not even allowed to use them)
        for scope in function_scopes.iter_mut() {
            scope.vars.clear();
            // Keep its size just in case we would go back to previous scopes but should not happen
        }

        // Finally, we generate the correct variables for the function call
        let last_scope = function_scopes.last_mut().unwrap();

        let nb_params = min(fd.parameters.0.len(), 3); // We only support 3 arguments for now
        for i in 0..nb_params {
            let param_id = fd.parameters.0.get(i).unwrap().id.clone();
            last_scope.vars.insert(param_id, 16 - i as i32 * 4); // Each parameter is 4 bytes and starts at +16 and goes to +8
        }

        function_scopes
    }

    fn get_func_def_len(&self, fd: FnDeclaration) -> u32 {
        let mut func_def_len = self.get_block_len(fd.body.clone());

        if fd.ty.is_some() {
            func_def_len += self.get_pop_len();
            func_def_len += 1; // addi
        } else {
            func_def_len += 1; // addi
        }

        func_def_len += 1; // jr

        func_def_len
    }

    fn process_func_def(&mut self, fd: FnDeclaration) -> Instrs {
        // A function shall take the values of its arguments from the frame layout at offsets 16, 12, and 8.
        // This manages the function body.
        let mut func_instrs = Instrs::new();
        let func_name = fd.id.clone();

        // We need to store the position of the function in the instructions memory
        self.define_function(&func_name, (self.pc + 1) as u32); // The function starts on the next instruction

        // We will use a temporary scope to generate the correct function code
        let backup_var_env = self.var_env.clone(); // Save the initial scope
        self.var_env = self.generate_scopes_for_function_call(fd.clone()); // Generate the correct scope for the function call

        // Then, we need to process the block of the function
        func_instrs.append(&mut self.process_block(fd.body));

        // At the end, we need to return the value of the last instruction if the function returns a value
        // At this point, since the block has been processed, the last value is at the top of the stack
        // We need to pop it to t0
        // To show the caller that there is a return value, we will store in t1 whether the function returns a value or not
        // It will be 1 if there is one, and 0 if there is none (boolean "does the function return a value?")
        if fd.ty.is_some() {
            func_instrs.append(&mut self.pop(t0));
            self.pc += 1;
            func_instrs.push(
                addi(t1, zero, 1).comment(
                    format!("FUNC_DEF '{}': Function returns a value", func_name).as_str(),
                ),
            );
        } else {
            self.pc += 1;
            func_instrs.push(
                addi(t1, zero, 0).comment(
                    format!("FUNC_DEF '{}': Function returns no value", func_name).as_str(),
                ),
            );
        }

        // We can jump back to the caller, which will handle the return value
        self.pc += 1;
        func_instrs.push(
            jr(ra).comment(format!("FUNC_DEF '{}': Return to the caller", func_name).as_str()),
        );

        // At the end, we need to restore the initial scope for further instructions
        self.var_env = backup_var_env;

        func_instrs
    }

    //? Helper methods to generate code for expressions

    fn get_expr_len(&self, expr: Expr) -> u32 {
        let mut expr_len = 0;

        match expr {
            Expr::Lit(lit) => {
                match lit {
                    Literal::Int(_) | Literal::Bool(_) => {
                        expr_len += 1; // Just an addi instruction
                        expr_len += self.get_push_len();
                    }
                    _ => todo!(), //TODO: Add support for other literals
                }
            }
            Expr::Ident(_) => {
                expr_len += 1; // Just an lw instruction
                expr_len += self.get_push_len();
            }
            Expr::BinOp(op, left, right) => {
                expr_len += self.get_binop_len(op, *left, *right);
            }
            Expr::UnOp(op, expr) => {
                expr_len += self.get_unop_len(op, *expr);
            }
            Expr::Par(expr) => {
                expr_len += self.get_expr_len(*expr);
            }
            Expr::Call(name, args) => {
                expr_len += self.get_call_len(name, args);
            }
            Expr::IfThenElse(cond, then_block, else_block) => {
                expr_len += self.get_expr_len(*cond);
                expr_len += self.get_pop_len();

                expr_len += self.get_block_len(then_block) + 1; // +1 for the beq

                if let Some(else_block) = else_block {
                    expr_len += self.get_block_len(else_block) + 1; // +1 for the b
                }
            }
            Expr::Block(block) => {
                expr_len += self.get_block_len(block);
            }
        }

        expr_len
    }

    fn process_expr(&mut self, expr: Expr) -> Instrs {
        let mut expr_instrs = Instrs::new();

        match expr.clone() {
            Expr::Lit(lit) => {
                match lit {
                    Literal::Int(i) => {
                        self.pc += 1;
                        expr_instrs.push(
                            addi(t0, zero, i as i16)
                                .comment(format!("Expr::Lit: Load integer {} in t0", i).as_str()),
                        );
                        expr_instrs.append(&mut self.push(t0));
                    }
                    Literal::Bool(b) => {
                        self.pc += 1;
                        expr_instrs.push(
                            addi(t0, zero, b as i16)
                                .comment(format!("Expr::Lit: Load boolean {} in t0", b).as_str()),
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

                        self.pc += 1;
                        expr_instrs.push(
                            lw(t0, offset, fp).comment(
                                // Store variable value in register t0
                                format!(
                                    "Expr::Ident: Load the value of {} from the stack at relative position {} in t0",
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
            Expr::UnOp(op, expr) => {
                expr_instrs.append(&mut self.process_unop(op, *expr))
            },
            Expr::Par(expr) => {
                expr_instrs.append(&mut self.process_expr(*expr))
            },
            Expr::Call(name, args) => {
                expr_instrs.append(&mut self.process_func_call(name, args))
            },
            Expr::IfThenElse(cond, then_block, else_block) => {
                // Process the condition
                expr_instrs.append(&mut self.process_expr(*cond)); // It is located in t0, AND at the top of the stack
                expr_instrs.append(&mut self.pop(t0)); // Pop the condition to t0 (seems useless, but if forgotten, the frame layout will be wrong)

                let mut nb_then_instrs = self.get_block_len(then_block.clone()) as i16;
                // If the condition is false, we jump and avoid the then block
                // Jumping 0 is going to the next instruction, so we should jump the number of instructions in the then block
                // If there is an else block, we must add 1 because of the jump after the then block
                if else_block.is_some() {
                    nb_then_instrs += 1;
                }
                self.pc += 1;
                expr_instrs.push(
                    beq(t0, zero, nb_then_instrs).comment("IfThenElse jump on false condition"),
                );

                let scope_size_before_then = self.var_env.last().unwrap().size;

                // Now we can put the then block
                let mut then_instrs = self.process_block(then_block);
                expr_instrs.append(&mut then_instrs);

                if let Some(else_block) = else_block {
                    let nb_else_instrs = self.get_block_len(else_block.clone()) as i16;

                    // If there is an else block, we should jump after the then block
                    self.pc += 1;
                    expr_instrs.push(b(nb_else_instrs).comment("IfThenElse jump after then block"));
                    
                    // Now we can put the else block
                    // We should restore the scope size before the then block to process the else block (to avoid thinking there are two pushed values)
                    self.var_env.last_mut().unwrap().size = scope_size_before_then;
                    let mut else_instrs = self.process_block(else_block);
                    expr_instrs.append(&mut else_instrs);
                }
            }
            Expr::Block(block) => expr_instrs.append(&mut self.process_block(block)),
        }

        expr_instrs
    }

    //? Helper methods to generate code for statements

    fn get_statement_len(&self, stmt: Statement) -> u32 {
        let mut stmt_len = 0;

        match stmt {
            Statement::Let(_, _, _, expr_opt) => {
                if expr_opt.is_some() {
                    stmt_len += self.get_expr_len(expr_opt.unwrap());
                } else {
                    stmt_len += self.get_push_len();
                }
            }
            Statement::Assign(left, right) => {
                stmt_len += self.get_expr_len(right);
                stmt_len += self.get_pop_len();

                match left {
                    Expr::Ident(name) => {
                        stmt_len += 1; // Just an sw instruction
                    }
                    _ => todo!(), //TODO: Add support for other assignments such as arrays
                }
            }
            Statement::While(cond, block) => {
                stmt_len += self.get_expr_len(cond);
                stmt_len += self.get_pop_len();

                stmt_len += self.get_block_len(block) + 2; // +2 for the beq and the b instructions
            }
            Statement::Expr(expr) => {
                stmt_len += self.get_expr_len(expr); // Result is at the top of the stack
                stmt_len += self.get_pop_len(); // Pop the result
            }
            Statement::Fn(fn_decl) => (), // Functions definitions are processed when entering a block
        }

        stmt_len
    }

    fn process_statement(&mut self, stmt: Statement) -> Instrs {
        let mut stmt_instrs = Instrs::new();

        match stmt {
            Statement::Let(_, name, _, expr_opt) => {
                if let Some(expr) = expr_opt {
                    stmt_instrs.append(&mut self.process_expr(expr)); // The value is now located on top of the stack
                } else {
                    // If there is no expression, we just define the variable. It will be initialized to 0
                    self.pc += 1;
                    stmt_instrs.append(&mut self.push(zero)); // Push the 0 to the stack
                }

                // Now we define the variable in our environment
                self.define_var(&name);
            }
            Statement::Assign(left, right) => {
                // We first process the expression to get the new value
                stmt_instrs.append(&mut self.process_expr(right)); // The value is now located on top of the stack
                stmt_instrs.append(&mut self.pop(t0)); // Pop the value to t0

                match left {
                    Expr::Ident(name) => {
                        // To assign a value to a variable, we need to find it in the stack
                        let fp_offset = self.get_var_offset(&name);

                        // Then, we store the value in the variable
                        match fp_offset {
                            Some(offset) => {
                                let offset = offset as i16;
                                self.pc += 1;
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

                // Needs +1 because will have a final jump back to the condition instruction
                let nb_block_instrs = self.get_block_len(block.clone()) as i16 + 1;

                // If the condition is false, we jump and avoid the block, and then jump back to the condition
                self.pc += 1;
                while_instrs
                    .push(beq(t0, zero, nb_block_instrs).comment("While jump on false condition"));

                // Then we can add the block instructions
                let mut block_instrs = self.process_block(block);
                while_instrs.append(&mut block_instrs);

                // And finally, we jump back to the condition
                let jump_back_nb = -(while_instrs.len() as i16) - 1; // Needs -1 because the jump is relative to the next instruction
                self.pc += 1;
                while_instrs.push(b(jump_back_nb).comment("While jump back to condition"));

                stmt_instrs.append(&mut while_instrs);
            }
            Statement::Expr(expr) => {
                // An expression should not remain on the stack
                stmt_instrs.append(&mut self.process_expr(expr)); // Result is at the top of the stack
                stmt_instrs.append(&mut self.pop(t0)); // Pop the result
            },
            Statement::Fn(fn_decl) => {} // Functions definitions are processed when entering a block
        }

        stmt_instrs
    }

    //? Helper method to generate code for blocks

    fn get_define_block_functions_len(&self, block: Block) -> u32 {
        let mut def_len = 1; // Jump over the block of functions

        let mut func_vec: Vec<FnDeclaration> = Vec::new();

        for stmt in block.statements {
            match stmt {
                Statement::Fn(fd) => {
                    func_vec.push(fd);
                }
                _ => (),
            }
        }

        for fd in func_vec.iter() {
            def_len += self.get_func_def_len(fd.clone());
        }

        def_len
    }

    fn define_block_functions(&mut self, block: Block) -> Instrs {
        let mut func_instrs = Instrs::new();

        // We will add the beginning jump instruction at the end of the block, but we update pc here for the offsets
        self.pc += 1;

        let mut func_vec: Vec<FnDeclaration> = Vec::new();

        for stmt in block.statements {
            match stmt {
                Statement::Fn(fn_decl) => {
                    func_vec.push(fn_decl);
                }
                _ => (),
            }
        }

        self.define_functions_vec(&func_vec); // Define the functions without processing their bodies to allow them to call each other

        for fd in func_vec.iter() {
            func_instrs.append(&mut self.process_func_def(fd.clone())); // Get the bodies of the functions
        }

        // Now, we have to add a jump instruction before the definition of the block of functions to avoid calling them
        let jump_offset = func_instrs.len() as i16;

        let mut final_instrs = Instrs::new();
        final_instrs.push(b(jump_offset).comment("Jump over the block of functions"));
        final_instrs.append(&mut func_instrs);

        final_instrs
    }

    fn get_block_len(&self, block: Block) -> u32 {
        let mut block_len = self.get_add_scope_len(); // Add scope
        block_len += self.get_define_block_functions_len(block.clone()); // Define functions

        for stmt in block.statements {
            block_len += self.get_statement_len(stmt); // Process statements
        }

        block_len += self.get_remove_scope_len(); // Remove scope

        if !block.semi {
            block_len += self.get_push_len(); // Push the last value back to the stack
        }

        block_len
    }

    fn process_block(&mut self, block: Block) -> Instrs {
        let mut block_instrs = Instrs::new();

        // A block starts by adding a new scope
        let mut scope_instr = self.add_scope();
        block_instrs.append(&mut scope_instr);

        // Then we scan the block to find the functions
        block_instrs.append(&mut self.define_block_functions(block.clone()));

        // Then, it processes all the statements one by one
        // No need to check anything here, as it will have been done by the type checker
        for stmt in block.statements {
            block_instrs.append(&mut self.process_statement(stmt));
        }

        // Finally, it removes the scope
        let mut scope_instr = self.remove_scope();
        block_instrs.append(&mut scope_instr);

        // If the block ended up without a semi-colon, it returns the last value to the scope before
        if !block.semi {
            block_instrs.append(&mut self.push(t0)); // Push the last value back to the stack
        }

        block_instrs
    }

    //? Helper method to generate code for programs

    fn get_prog_len(&self, prog: Prog) -> u32 {
        let mut prog_len = 0;

        for fd in prog.0.iter() { // The order does not matter here
            prog_len += self.get_func_def_len(fd.clone());
        }

        prog_len += 1; // Jump over the block of functions

        prog_len += self.get_call_len("main".to_string(), Arguments::new(vec![]));

        prog_len
    }

    fn process_prog(&mut self, prog: Prog) -> Instrs {
        // To process a program, we need to process the main block
        // We shall define the block with every function, and then jump to the main function
        let mut prog_instrs = Instrs::new();

        let mut func_vec_new_order = Vec::new();

        // Let's first define the main function
        for fd in prog.0.iter() {
            if fd.id == "main" {
                func_vec_new_order.push(fd.clone());
                break;
            }
        }

        // Then, we process the remaining functions
        for fd in prog.0.iter() {
            if fd.id != "main" {
                func_vec_new_order.push(fd.clone());
            }
        }

        self.define_functions_vec(&func_vec_new_order); // Define the functions without processing their bodies to allow them to call each other

        for fd in func_vec_new_order.iter() {
            prog_instrs.append(&mut self.process_func_def(fd.clone())); // Get the bodies of the functions
        }

        // Now, we have to add a jump instruction before the definition of the block of functions to avoid calling them
        let jump_offset = prog_instrs.len() as i16;
        let mut final_instrs = Instrs::new();
        final_instrs.push(b(jump_offset).comment("Jump over the block of functions"));
        final_instrs.append(&mut prog_instrs);

        // Then, add the call to the main function
        let mut call_instrs = self.process_func_call("main".to_string(), Arguments::new(vec![]));
        final_instrs.append(&mut call_instrs);

        final_instrs
    }
}
