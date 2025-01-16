use core::fmt;
use std::collections::{HashMap, VecDeque};

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Mutable, Parameter, Parameters, Prog,
    Statement, Type, UnOp,
};

use crate::common::*;
use crate::error::Error; //TODO: Define proper custom errors
use mips::{
    asm::*,
    instr::Instr,
    instrs::Instrs,
    rf::{Reg, Reg::*},
    vm::Mips,
};

//?#################################################################################################
//?#                                                                                               #
//?#                                Backend Var and Scope Types                                    #
//?#                                                                                               #
//?#################################################################################################

//? OLD STACK FRAME - NOT UP TO DATE
//* stack frame layout:
// ... things before ...
// 16[fp]    arg 1
// 12[fp]    arg 2
//  8[fp]    arg 3
//  4[fp]    ra
//  0[fp]    old_fp
// -4[fp]    local 1
// -8[fp]    local 2, etc.

//? NEW STACK FRAME
//* stack frame layout:
// ... things before ...
//  4[fp]    ra
//  0[fp]    old_fp
// -4[fp]    local 1
// -8[fp]    local 2, etc.

// This modification is made so that I can use any number of arguments in functions
// Moreover, it reduces the size usage of the stack
// This is possible because when I call a function, I always use an intermediate scope to store the arguments:

// Example function call:
// {
//     let a = 3;
//     let b = false;

//     fn dummy(i: i32, b: bool) -> i32 {
//         if b {
//             return i;
//         } else {
//             return 0;
//         }
//     }

//     dummy(a, b)
// }

// This will lead to the following scopes:
// Scope 0: 'a', 'b' and 'dummy' are defined
// Scope 1: local 'i' and 'b' are defined
// Scope 3: function 'dummy' body is called and executed

// -----------------------------------------------------------------------------------------------

const DEFAULT_SCOPE_SIZE: u32 = 8; // Old had: 20;
const DEBUG_PRINTS: bool = false;

//? Helpers

#[derive(Clone, Debug, PartialEq)]
pub enum VarSize {
    Single {
        // Single value
        bytes_size: usize,
    },
    Array {
        // Array of size nb_elements of objects of size VarSize
        nb_elem: usize,
        elem_size: Box<VarSize>,
    },
}

impl VarSize {
    pub fn empty() -> Self {
        VarSize::Single { bytes_size: 0 }
    }

    pub fn default() -> Self {
        VarSize::Single { bytes_size: 4 }
    }

    // Size should be given in bytes (it should be a multiple of 4)
    pub fn new(bytes_size: usize) -> Self {
        VarSize::Single { bytes_size }
    }

    // Size should be given in number of elements ([1, 2] -> size = 2, not 8)
    pub fn new_array(nb_elem: usize, elem_size: VarSize) -> Self {
        VarSize::Array {
            nb_elem,
            elem_size: Box::new(elem_size),
        }
    }

    pub fn get_bytes_size(&self) -> usize {
        match self {
            VarSize::Single { bytes_size } => *bytes_size,
            VarSize::Array { nb_elem, elem_size } => nb_elem * elem_size.get_bytes_size(),
        }
    }

    pub fn get_inner_size(&self) -> Option<&VarSize> {
        match self {
            VarSize::Single { .. } => None,
            VarSize::Array { elem_size, .. } => Some(elem_size),
        }
    }

    pub fn get_inner_size_seq(&self) -> Vec<VarSize> {
        self.build_inner_size_seq(vec![])
    }

    fn build_inner_size_seq(&self, current_seq: Vec<VarSize>) -> Vec<VarSize> {
        match self {
            VarSize::Single { .. } => current_seq.clone(),
            VarSize::Array { elem_size, .. } => {
                let mut new_seq = current_seq.clone();
                new_seq.push(*elem_size.clone());
                elem_size.build_inner_size_seq(new_seq)
            }
        }
    }
}

impl From<Type> for VarSize {
    fn from(t: Type) -> Self {
        match t {
            Type::Any => panic!("Can't get a VarSize from an 'Any' type"),
            Type::I32 => VarSize::default(),
            Type::Bool => VarSize::default(),
            Type::String => unimplemented!("String type not yet supported in backend"),
            Type::Array(inner, size) => VarSize::new_array(size, VarSize::from(*inner)),
            Type::Unit => VarSize::empty(),
        }
    }
}

impl From<Option<Type>> for VarSize {
    fn from(t: Option<Type>) -> Self {
        match t {
            Some(t) => VarSize::from(t),
            None => VarSize::empty(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Var {
    offset: i32,   // Offset relative to the frame pointer (-4, -8, ... or even 16, 12, ...)
    size: VarSize, // Size of the variable
}

impl Var {
    pub fn new(offset: i32, size: VarSize) -> Self {
        Self { offset, size }
    }

    pub fn get_offset(&self) -> i32 {
        self.offset
    }

    pub fn get_var_size(&self) -> &VarSize {
        &self.size
    }

    pub fn as_tuple(&self) -> (i32, &VarSize) {
        (self.offset, &self.size)
    }

    pub fn get_bytes_size(&self) -> usize {
        self.size.get_bytes_size()
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "offset = {}, size = {}",
            self.offset,
            self.size.get_bytes_size()
        )
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    offset: u32,         // Tracks the position of the functions in the instructions memory
    decl: FnDeclaration, // The function declaration, to be accessible for return types and arguments
}

impl Func {
    pub fn new(offset: u32, decl: FnDeclaration) -> Self {
        Self { offset, decl }
    }

    pub fn get_offset(&self) -> u32 {
        self.offset
    }

    pub fn get_declaration(&self) -> &FnDeclaration {
        &self.decl
    }

    pub fn as_tuple(&self) -> (u32, FnDeclaration) {
        (self.offset, self.decl.clone())
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "offset = {}, signature: {}",
            self.offset,
            self.decl.get_signature_repr(0)
        )
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    functions: HashMap<String, Func>, // Tracks the position of the functions in the instructions memory

    size: u32, // Tracks the size of the scope on fp values (It only tracks the negative part of the stack)
    vars: HashMap<String, Var>, // Tracks the position of the variables on the stack (relative to fp): name -> var struct
    temps: VecDeque<Var>,       // Tracks the temporary values on the stack
}

impl Scope {
    // Vars will be stored on the stack.
    // Functions will be stocked in the instructions memory.
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),

            size: 0, // Default size of the scope due to the frame layout
            vars: HashMap::new(),
            temps: VecDeque::new(),
        }
    }

    // Used to define variables, not function arguments
    pub fn define_var(&mut self, name: String) {
        // When we define a variable, it means it is already on the stack, as a temporary for now
        // The offset of the variable is given by the size of the scope, and the size of the var itself

        let new_var = match self.temps.pop_front() {
            Some(v) => v,
            None => {
                unreachable!("Error: Trying to define a variable in a scope where there are no temporary variables to define as a var");
            }
        };

        self.vars.insert(name, new_var);
    }

    pub fn create_array(&mut self, size: usize) {
        // This takes the x last elements of the temporary values, and combines them in a single temporary value, which is an array.
        // It does not affect its size, as it combines without adding new values

        if size == 0 {
            // This is a unit type and it should be ignored
            return;
        }

        if size > self.temps.len() {
            unreachable!(
                "Error: Trying to create an array of size {} with only {} temporary values",
                size,
                self.temps.len()
            );
        }

        let last_temp = self.temps.pop_back().unwrap();
        let inner_size = last_temp.size.clone();
        let mut arr_offset = last_temp.get_offset();

        for _ in 1..size {
            let next_temp = self.temps.pop_back().unwrap();
            if next_temp.size != inner_size {
                unreachable!(
                    "Error: Trying to create an array with different types of temporary values"
                );
            }
            arr_offset = next_temp.get_offset();
        }

        let arr = Var::new(arr_offset, VarSize::new_array(size, inner_size));
        self.temps.push_back(arr);
    }

    pub fn create_array_matching_varsize(&mut self, var_size: &VarSize) {
        // This takes the last elements of the temporary values, and combines them to form an array matching the given var_size
        // It panics if it encounters an invalid value

        //? Debug
        // eprintln!("Trying to build an array matching VarSize: {:?} from the current scope:\n{}", var_size, self);

        let mut set_of_sizes = vec![var_size.clone()]; // The final var_size should be part of it
        set_of_sizes.append(&mut var_size.get_inner_size_seq());
        if set_of_sizes.len() == 0 {
            unreachable!(
                "Error: Trying to create an array with a non-array size ({:?})",
                var_size
            );
        }

        // Pop the last element as it is the final size (it may not even be an array)
        let expected_smaller_sizes = set_of_sizes.pop().unwrap().clone();

        // We don't need the size of the last element as we will check it when popping the temporary values
        let mut inner_element_sizes: Vec<usize> = set_of_sizes
            .iter()
            .map(|v| match v {
                VarSize::Single { .. } => {
                    unreachable!("Can't find a non Array VarSize in get_inner_size_seq()")
                }
                VarSize::Array { nb_elem, .. } => *nb_elem,
            })
            .collect();
        inner_element_sizes.reverse(); // reversing the order to be from the deepest to the most outer elements

        let total_size = var_size.get_bytes_size();
        let mut current_size = 0;
        let mut popped_temps = VecDeque::new(); // To avoid reverse operations

        //? Debug
        // eprintln!("Found set of sizes:\n{:?}", set_of_sizes);
        // eprintln!("Found inner element sizes:\n{:?}", inner_element_sizes);
        // eprintln!("Expected smaller sizes:\n{:?}", expected_smaller_sizes);

        while current_size < total_size {
            let last_temp = self.temps.pop_back().unwrap();
            let temp_var_size = last_temp.get_var_size().clone();

            if temp_var_size != expected_smaller_sizes {
                unreachable!(
                    "Error: Trying to create an array with invalid temporary value types -> found {:?}, expected {:?}",
                    temp_var_size,
                    expected_smaller_sizes
                );
            }

            let last_temp_size = last_temp.get_bytes_size();
            current_size += last_temp_size;
            popped_temps.push_front(last_temp);
        }

        if current_size != total_size {
            unreachable!(
                "Cannot get an incoherent final size if all temporary VarSizes were correct"
            );
        }

        // Should not have a real impact since all of them should have the same size, but allow to get back to the correct order (offsets)
        let mut new_temps: VecDeque<Var> = VecDeque::new();

        //? Debug
        // eprintln!("After first extraction, found temporary values:\n{:?}", popped_temps);
        // eprintln!("Scope is now: {}", self);

        for (i, inner_size) in inner_element_sizes.iter().enumerate() {
            let expected_inner_var_size = set_of_sizes.pop().unwrap(); // get the deepest element size

            while popped_temps.len() >= *inner_size {
                let mut inner_temps: VecDeque<Var> = VecDeque::new();
                for _ in 0..*inner_size {
                    let temp = popped_temps.pop_front().unwrap();
                    inner_temps.push_back(temp);
                }
                let offset = inner_temps[0].get_offset();
                let inner_var_size =
                    VarSize::new_array(*inner_size, inner_temps[0].get_var_size().clone());

                //? Debug
                // eprintln!("Building inner array n°{}/{} with temporary values:\n{:?}", i+1, inner_element_sizes.len(), inner_temps);
                // eprintln!("Extracted offset: {}, inner_var_size: {:?}", offset, inner_var_size);

                if inner_var_size != expected_inner_var_size {
                    unreachable!(
                        "Error: Trying to create an array with invalid inner temporary value types -> found {:?}, expected {:?}",
                        inner_var_size,
                        expected_inner_var_size
                    );
                }

                let inner_var = Var::new(offset, inner_var_size);
                new_temps.push_back(inner_var);
            }

            if popped_temps.len() != 0 {
                unreachable!(
                    "Error: Trying to create an array with an invalid number of temporary values left -> {}",
                    popped_temps.len()
                );
            }

            //? Debug
            // eprintln!("After additional extraction n°{}/{}, found temporary values:\n{:?}", i+1, inner_element_sizes.len(), new_temps);

            popped_temps = new_temps.clone();
            new_temps = VecDeque::new();
        }

        // At this point, our new_temps should contain only one element, which is the final array with the correct size
        if popped_temps.len() != 1 {
            unreachable!("There should only be one remaining value, which is the final array");
        }

        let final_array = popped_temps.pop_front().unwrap();
        let final_size = final_array.get_var_size().clone();
        if final_size != *var_size {
            unreachable!(
                "Error: Trying to create an array with invalid final temporary value VarSizes -> found {:?}, expected {:?}",
                final_size,
                var_size
            );
        }

        //? Debug
        // eprintln!("In the end, built final_array: {:?}", final_array);

        // Else, it is valid and we can push it back to the temporary values
        self.temps.push_back(final_array);

        //? Debug
        // eprintln!("Final scope is: {}", self);
    }

    pub fn update_last_temp_on_array_get(&mut self, result_size: &VarSize) {
        // This shall update the last temporary value (supposing it is an array)
        // so that it is split in two parts: the first element which is the return value
        // of a get operation, and the rest of the array afterwards.

        // The size of the scope is not modified in the end

        let result_total_size = result_size.get_bytes_size();

        if result_total_size == 0 {
            // This is a unit type and it should be ignored
            return;
        }

        let last_temp = match self.temps.pop_back() {
            Some(v) => v,
            None => {
                unreachable!("Error: Trying to update the last temporary value in a scope where there are no temporary values");
            }
        };

        let (last_temp_offset, last_var_size) = last_temp.as_tuple();
        let last_temp_size = last_temp.get_bytes_size();

        if last_temp_size < result_total_size {
            unreachable!(
                "Error: Trying to update the last temporary value with a result size bigger than the temporary value"
            );
        }

        let new_arr_size = match last_var_size {
            VarSize::Array { nb_elem, elem_size } => {
                let elem_size = *elem_size.clone();
                if elem_size != result_size.clone() {
                    unreachable!(
                        "Error: Get operation should have the same size as the inner elements of the array"
                    );
                }

                let new_nb_elem = nb_elem - 1;
                VarSize::new_array(new_nb_elem, elem_size)
            }
            _ => unreachable!("Error: Last temporary value is not an array"),
        };

        let new_temp = Var::new(last_temp_offset, result_size.clone());
        self.temps.push_back(new_temp);

        // Building the rest of the array
        let rest_temp = Var::new(last_temp_offset - result_total_size as i32, new_arr_size);
        self.temps.push_back(rest_temp);
    }

    pub fn get_var(&self, name: &String) -> Option<&Var> {
        self.vars.get(name)
    }

    pub fn update_on_push(&mut self, var_size: VarSize) {
        // Scope has not already been updated in size, so self.size begins at 0
        let vsize = var_size.get_bytes_size();

        if vsize == 0 {
            // This is a unit type and it should be ignored
            return;
        }

        let offset = -4 - (self.size as i32 - (vsize - 4) as i32);
        let var = Var::new(offset, var_size);
        self.size += var.get_bytes_size() as u32;
        self.temps.push_back(var);
    }

    pub fn update_on_pop(&mut self) {
        let var_size = match self.temps.pop_back() {
            Some(v) => v.get_bytes_size() as u32,
            None => {
                unreachable!("Error: Trying to pop a variable from a stack where there are no temporary values");
            }
        };
        self.size -= var_size;
    }

    pub fn update_on_move(&mut self, mut var: Var) {
        // Update current scope after a returned value has been pushed on its stack
        let var_offset = -(self.size as i32 + 4);
        let var_size = var.get_bytes_size();

        if var_size == 0 {
            // We ignore this variable, it is unit type
            return;
        }

        self.size += var_size as u32;

        // We should update the variable offset to the correct value based on the scope size
        var.offset = var_offset;
        self.temps.push_back(var);
    }

    pub fn define_function(&mut self, func: Func) {
        let name = func.decl.id.clone();
        self.functions.insert(name, func);
    }

    pub fn get_function(&self, name: &String) -> Option<&Func> {
        self.functions.get(name)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope: size={}\n| Vars:\n", self.size)?;
        for (name, var) in self.vars.iter() {
            write!(f, "|\t- {}: {}\n", name, var)?;
        }
        write!(f, "| Temps:\n")?;
        let mut i = 0;
        for var in self.temps.iter() {
            write!(f, "|\t- {}: {}\n", i, var)?;
            i += 1;
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

        let default_size = bvm.get_add_scope_len() + 1; // due to the initial scope and the finishing Halt
        let expected_size = bvm.get_expr_len(self) + default_size;

        bvm.full_process_expr(self.clone());
        let expr_instrs = bvm.get_instructions();

        if DEBUG_PRINTS {
            println!(
                "Size verification (Expr):\nExpected size: {}\nReal size: {}",
                expected_size,
                expr_instrs.len()
            );

            bvm.pretty_print_instructions();
        }

        Ok(expr_instrs)
    }
}

impl GetInstructions for Block {
    fn get_instructions(&self) -> Result<Instrs, Error> {
        let mut bvm = BVM::new();

        let default_size = bvm.get_add_scope_len() + 1; // due to the initial scope and the finishing Halt
        let expected_size = bvm.get_block_len(self) + default_size;

        bvm.full_process_block(self.clone());
        let block_instrs = bvm.get_instructions();

        if DEBUG_PRINTS {
            println!(
                "Size verification (Block):\nExpected size: {}\nReal size: {}",
                expected_size,
                block_instrs.len()
            );

            bvm.pretty_print_instructions();
        }

        Ok(block_instrs)
    }
}

impl GetInstructions for Prog {
    fn get_instructions(&self) -> Result<Instrs, Error> {
        let mut bvm = BVM::new();

        let default_size = bvm.get_add_scope_len(); // due to the initial scope
        let expected_size = bvm.get_prog_len(self) + default_size; // Halt included

        bvm.full_process_prog(self.clone());
        let prog_intrs = bvm.get_instructions();

        if DEBUG_PRINTS {
            println!(
                "Size verification (Prog):\nExpected size: {}\nReal size: {}",
                expected_size,
                prog_intrs.len()
            );

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

#[derive(Clone)]
pub struct BVM {
    scopes: Vec<Scope>, // Tracks the variables in the current scope

    // Manage instructions and be able to get jump offsets
    pc: i32, // Program counter (used to keep track of the local instructions even when they are not added yet)
    instructions: Instrs, // Stores the current instructions
}

impl BVM {
    pub fn new() -> Self {
        let mut bvm = Self {
            scopes: vec![],
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
        for instr in instrs.iter() {
            self.add_instr(instr.clone());
        }
    }

    fn add_halt(&mut self, comment: &str) {
        self.pc += 1;
        self.add_instr(halt().comment(comment));
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
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.

    fn get_add_scope_len(&self) -> u32 {
        4 // 4 instructions
    }

    fn add_scope(&mut self) -> Instrs {
        let new_scope = Scope::new();
        let scope_id = self.scopes.len();
        self.scopes.push(new_scope);

        let mut instrs = Instrs::new();

        instrs.push(
            addiu(sp, sp, -(DEFAULT_SCOPE_SIZE as i16)).comment(
                format!(
                    "ADD_SCOPE {}: Allocate default space for the new scope",
                    scope_id
                )
                .as_str(),
            ),
        ); // This corresponds to 2 pushes

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

    fn get_move_return_value_len(&self, temp_size: &VarSize) -> u32 {
        let temp_size = temp_size.get_bytes_size() as u32;

        if temp_size % 4 != 0 {
            unreachable!("WARNING: non-multiple of 4 size for temporary value in return value movement! ({})", temp_size);
        }

        let nb_moves = temp_size / 4;
        2 * nb_moves as u32 // 2 instructions per move
    }

    fn move_return_value(&mut self, temp_var: &Var, moving_offset: i32) -> Instrs {
        let mut instrs = Instrs::new();

        if moving_offset < 0 {
            unreachable!(
                "WARNING: negative moving offset in return value movement! ({})",
                moving_offset
            );
        }

        let (temp_offset, temp_size) = temp_var.as_tuple();
        let temp_size = temp_size.get_bytes_size();

        if temp_size % 4 != 0 {
            unreachable!("WARNING: non-multiple of 4 size for temporary value in return value movement! ({})", temp_size);
        }

        let nb_moves = temp_size / 4;
        for i in 0..nb_moves {
            let lw_offset = (temp_offset - 4 * i as i32) as i16;
            instrs.push(
                lw(t0, lw_offset, fp).comment(
                    format!(
                        "MOVE_RETURN_VALUE {}/{}: Load temporary value n°{} from the stack",
                        i + 1,
                        nb_moves,
                        i
                    )
                    .as_str(),
                ),
            );

            let sw_offset = (temp_offset + moving_offset - 4 * i as i32) as i16;
            instrs.push(
                sw(t0, sw_offset, fp).comment(
                    format!(
                        "MOVE_RETURN_VALUE {}/{}: Store temporary value n°{} to the stack",
                        i + 1,
                        nb_moves,
                        i
                    )
                    .as_str(),
                ),
            );

            self.pc += 2;
        }

        instrs
    }

    fn update_last_temp_on_array_get(&mut self, result_size: &VarSize) {
        let scope = self.scopes.last_mut().unwrap(); // Get last scope
        scope.update_last_temp_on_array_get(result_size);
    }

    fn remove_last_temp(&mut self) -> Instrs {
        // this removes the last temporary value from the stack, whatever its size
        // It does not pop it to any register, it just removes it from the stack

        let mut instrs = Instrs::new();

        let scope = self.scopes.last_mut().unwrap(); // Get last scope
        let last_temp = match scope.temps.pop_back() {
            Some(val) => val,
            None => return instrs, // There is no temporary value to remove
        };

        let (temp_offset, temp_size) = last_temp.as_tuple();
        let temp_size = temp_size.get_bytes_size();

        if temp_size % 4 != 0 {
            unreachable!("WARNING: non-multiple of 4 size for temporary value in return value movement! ({})", temp_size);
        }

        instrs.push(
            addiu(sp, sp, temp_size as i16).comment(
                format!(
                    "REMOVE_LAST_TEMP: Deallocate last temporary value of size {}",
                    temp_size
                )
                .as_str(),
            ),
        );
        self.pc += 1;

        scope.size -= temp_size as u32;

        instrs
    }

    //* stack frame layout of two following scopes:
    // Scope n°i
    //  4[fpi]    ra
    //  0[fpi]    old_fp
    // -4[fpi]    local 1
    // -8[fpi]    local 2, etc.
    // Scope n°i+1
    //  4[fpi+1]    ra
    //  0[fpi+1]    old_fp
    // -4[fpi+1]    local 1
    // -8[fpi+1]    local 2, etc.

    fn get_remove_scope_len(&self, return_value: Option<VarSize>) -> u32 {
        let mut nb_instrs = 1; // 1 instruction for the return address

        if let Some(size) = return_value {
            if size.get_bytes_size() > 0 {
                nb_instrs += 1; // Saving the frame pointer before moving
                nb_instrs += self.get_move_return_value_len(&size);
            }
        }

        nb_instrs += 2; // 2 instructions for the frame pointer and the space deallocation

        nb_instrs
    }

    fn remove_scope(&mut self) -> Instrs {
        let scope = self.scopes.pop().unwrap(); // Get last scope
        let scope_id = self.scopes.len();

        //? Debug
        // eprintln!("Removing scope n°{}:\n{}", scope_id, scope);

        let mut instrs = Instrs::new();

        // Restore the old frame pointer and the return address
        instrs
            .push(lw(ra, 4, fp).comment(
                format!("REMOVE_SCOPE {}: Restore the return address", scope_id).as_str(),
            ));
        self.pc += 1;

        // On scope removal, there are only two possibilities.
        // The scope either has 1 or 0 temporary values remaining.
        // If it has one, it means it is the returned value of the scope.
        // If it has zero, the scope is unit type.
        let nb_temps = scope.temps.len();
        if nb_temps >= 1 {
            // The scope has a return value. We shall move it to the stack of the previous scope,
            // to update both the backend state and the MIPS state.

            if nb_temps > 1 {
                eprintln!(
                    "/!\\ WARNING: Removing scope with two or more temporary values ({}) /!\\",
                    nb_temps
                );
            }

            // We should first save the fp value in a temporary register to be able to access it after moving the temporary value
            instrs.push(
                lw(t1, 0, fp).comment(
                    format!("REMOVE_SCOPE {}: Save the old frame pointer in t1 before moving return value", scope_id).as_str(),
                ),
            );

            self.pc += 1;

            let temp_val = scope.temps.front().unwrap().clone();
            let (temp_offset, temp_size) = temp_val.as_tuple();
            let temp_size = temp_size.get_bytes_size();

            // Look at the above description of Scope i and Scope i+1 and imagine moving local 2 to previous scope local 3
            let moving_offset = DEFAULT_SCOPE_SIZE as i32 - (temp_offset as i32 + 4);

            instrs.append(&mut self.move_return_value(&temp_val, moving_offset)); // only uses t0

            // We shall then update the previous scope
            let prev_scope = self.scopes.last_mut().unwrap();
            prev_scope.update_on_move(temp_val);

            // Finally, we get back the old frame pointer
            instrs.push(
                addiu(fp, t1, 0).comment(
                    format!(
                        "REMOVE_SCOPE {}: Restore the old frame pointer from t1",
                        scope_id
                    )
                    .as_str(),
                ),
            );

            // And we can deallocate the remaining of the current scope.
            // We should not forget that its size has been reduced by the size of the moved temporary value.
            let total_size = scope.size - temp_size as u32 + DEFAULT_SCOPE_SIZE;

            instrs.push(
                addiu(sp, sp, total_size as i16).comment(
                    format!(
                        "REMOVE_SCOPE {}: Deallocate space used by the scope",
                        scope_id
                    )
                    .as_str(),
                ),
            ); // This removes everything that was not removed yet

            self.pc += 2;
        } else {
            // The scope has no return value. Therefore, it is unit type.

            instrs.push(lw(fp, 0, fp).comment(
                format!("REMOVE_SCOPE {}: Restore the old frame pointer", scope_id).as_str(),
            ));

            let total_size = scope.size + DEFAULT_SCOPE_SIZE;

            instrs.push(
                addiu(sp, sp, total_size as i16).comment(
                    format!(
                        "REMOVE_SCOPE {}: Deallocate space used by the scope",
                        scope_id
                    )
                    .as_str(),
                ),
            ); // This removes everything that was not removed yet

            self.pc += 2;
        }

        instrs
    }

    fn define_var(&mut self, name: &String) {
        self.scopes.last_mut().unwrap().define_var(name.clone());
    }

    //* stack frame layout:
    // ... things before ...
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.
    // Computes the signed offset of a variable in the stack relative to the current fp value (innermost scope)
    fn get_var_offset(&self, name: &String) -> Option<(i32, &Var)> {
        // The final offset depends on the scope it was defined in.
        let mut scope_offset: i32 = 0;
        for scope in self.scopes.iter().rev() {
            // If we are not in the first scope, we should add the size of the scope itself to be able to go back to the local fp[0] position
            if scope_offset != 0 {
                scope_offset += scope.size as i32; // Each variable is 4 bytes
            }

            if let Some(var) = scope.get_var(name) {
                return Some((scope_offset + var.get_offset(), var));
            }
            // It was not defined in this scope, so we add the +20 bytes of the scope which are located above fp[0]
            scope_offset += DEFAULT_SCOPE_SIZE as i32;
        }
        None
    }

    fn define_function(&mut self, func: Func) {
        self.scopes.last_mut().unwrap().define_function(func);
    }

    fn define_functions_vec(&mut self, functions: &Vec<FnDeclaration>) {
        // this method aims to define every function from the list without processing their bodies
        // so that they can call each other with the correct offsets and everything.

        let mut local_pc = self.pc as u32;

        // We first need to define each function at least once, even with the wrong offset value,
        // so that they can call each other.
        // Then, we will update the pc values of each of them
        for fd in functions.iter() {
            // We dont care about the offset here
            let func = Func::new(0, fd.clone());
            self.define_function(func);
        }

        // Updating each of them
        for fd in functions.iter() {
            // The function will start at the next instruction
            let func = Func::new(local_pc + 1, fd.clone());
            self.define_function(func);
            // The function takes x instructions, where x is:
            let func_len = self.get_func_def_len(fd.clone());
            local_pc += func_len;
        }
    }

    fn get_function(&self, name: &String) -> Option<&Func> {
        for scope in self.scopes.iter().rev() {
            if let Some(func) = scope.get_function(name) {
                return Some(func);
            }
        }
        None
    }

    fn pretty_print_scopes(&self) {
        println!("Current scopes:");
        for (i, scope) in self.scopes.iter().enumerate() {
            println!("{}. {}", i, scope);
        }
    }

    //? Helper methods to push and pop registers

    // // fn get_push_len(&self) -> u32 {
    // //     2 // 2 instructions
    // // }

    // Push a register on the stack -> This is therefore always a 4 bytes value
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

        let var_size = VarSize::default();
        self.scopes.last_mut().unwrap().update_on_push(var_size); // Update the size of the scope

        push_instrs
    }

    // // fn get_pop_len(&self) -> u32 {
    // //     2 // 2 instructions
    // // }

    fn create_array(&mut self, size: usize) {
        self.scopes.last_mut().unwrap().create_array(size);
    }

    fn create_array_matching_varsize(&mut self, var_size: &VarSize) {
        self.scopes
            .last_mut()
            .unwrap()
            .create_array_matching_varsize(var_size);
    }

    // Pop the top of the stack into a register
    fn pop(&mut self, reg: Reg) -> Instrs {
        let mut pop_instrs = Instrs::new();

        // Checking if the last temp is a single element, else there is an issue
        let scope = self.scopes.last_mut().unwrap();
        let last_temp = scope.temps.back().unwrap();
        let temp_size = last_temp.get_bytes_size();
        if temp_size != 4 {
            unreachable!("Error: Trying to pop a variable from a stack where the last temporary value is not a single element");
        }

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

        scope.update_on_pop(); // Update the size of the scope

        pop_instrs
    }

    // Move the value
    fn move_address(
        &mut self,
        intermediate_reg_val: Reg,
        source_address: Reg,
        destination_address: Reg,
    ) -> Instrs {
        // This moves a value on the stack, which is at the address given by the source_address register, to the address given by the destination_address register
        // These are absolute addresses, not relative ones
        // It uses the intermediate register to store the value before moving it

        // It does not modify the two offset registers, but it modifies the intermediate register

        let mut move_instrs = Instrs::new();

        // Loading the value to the intermediate register thanks to the current offset register
        move_instrs.push(
            lw(intermediate_reg_val, 0, source_address).comment(
                format!(
                    "MOVE_ADDRESS: Load the value to move from source_address in {:?}",
                    intermediate_reg_val
                )
                .as_str(),
            ),
        );

        // Storing the value from the intermediate register to the new location
        move_instrs.push(
            sw(intermediate_reg_val, 0, destination_address).comment(
                format!(
                    "MOVE_ADDRESS: Store the value to move at destination address from {:?}",
                    intermediate_reg_val
                )
                .as_str(),
            ),
        );

        self.pc += 2;
        move_instrs
    }

    // Helpers for multiplication and division of registers

    fn mul(
        &mut self,
        res_reg: Reg,
        left_reg: Reg,
        right_reg: Reg,
        counter_reg: Reg,
        boolean_reg: Reg,
    ) -> Instrs {
        // For a multiplication, we need to initialize a counter to do the correct number of multiplications
        // For now, not optimized, the counter is always the right operand
        // For an optimized version, we should take the lowest value (in absolute value) as the counter

        // It only needs the left and right operands to be initialized

        // Every other register (except left and right) will be modified!

        let mut mul_instrs = Instrs::new();

        mul_instrs.push(addu(res_reg, zero, zero).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Initialize the result to 0",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(addi(boolean_reg, zero, 1).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Initialize the negative counter boolean to True",
            res_reg, left_reg, right_reg
        )));

        // If the right operand is negative, we need to negate it
        mul_instrs.push(addu(counter_reg, zero, right_reg).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Initialize the counter to the right operand",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(blez(counter_reg, 2).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Check if the counter is negative. If so, skip the negation",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(subu(counter_reg, zero, counter_reg).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Negate the counter",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(addi(boolean_reg, zero, 0).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Set the negative counter boolean to False",
            res_reg, left_reg, right_reg
        )));

        // Actual loop for the multiplication
        mul_instrs.push(beq(counter_reg, zero, 3).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Multiplication loop - check if the counter is 0",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(addu(res_reg, res_reg, left_reg).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Proceed a single addition for the multiplication",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(addiu(counter_reg, counter_reg, 1).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Update the counter",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(b(-4).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Branch back to multiplication loop",
            res_reg, left_reg, right_reg
        )));

        // At the end, if the counter was negative, we need to negate the result
        mul_instrs.push(beq(boolean_reg, zero, 1).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Skip next instruction if the counter was positive",
            res_reg, left_reg, right_reg
        )));
        mul_instrs.push(subu(res_reg, zero, res_reg).comment(&format!(
            "Mul ({:?} = {:?} x {:?}): Negate the result (the counter was negative) (End multiplication)",
            res_reg, left_reg, right_reg
        )));
        self.pc += 12;

        mul_instrs
    }

    fn div(
        &mut self,
        res_reg: Reg,
        left_reg: Reg,
        right_reg: Reg,
        l_boolean_reg: Reg,
        r_boolean_reg: Reg,
        neg_left_reg: Reg,
        neg_right_reg: Reg,
        cond_reg: Reg,
    ) -> Instrs {
        // For a division, we need to count the number of times we can subtract the right operand from the left operand

        // It only needs the left and right operands to be initialized

        // TODO: Optimize this?
        // Let's compute the result in res_reg.
        // Let's store the positive left operand in left_reg (and the remainder).
        // Let's store the positive right operand in right_reg.
        // Let's store the "is left operand negative?" in l_boolean_reg.
        // Let's store the "is right operand negative?" in r_boolean_reg.
        // Let's store the negated left operand in neg_left_reg. (For boolean checks, since we only have branch on <= 0)
        // Let's store the negated right operand in neg_right_reg. (For boolean checks, since we only have branch on <= 0)
        // Let's store the condition to check in cond_reg.

        let mut div_instrs = Instrs::new();

        //* Initialization
        div_instrs.push(subu(neg_left_reg, zero, left_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Store the negated left operand",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(subu(neg_right_reg, zero, right_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Store the negated right operand",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(addi(l_boolean_reg, zero, 0).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Initialize the 'is left operand negative?' boolean to False",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(addi(r_boolean_reg, zero, 0).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Initialize the 'is right operand negative?' boolean to False",
            res_reg, left_reg, right_reg
        )));

        div_instrs.push(blez(neg_left_reg, 2).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Check if the left operand is positive. If so, skip the negation",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(subu(left_reg, zero, left_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Negate the left operand to make it positive",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(addi(l_boolean_reg, zero, 1).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Set the 'is left operand negative?' boolean to True",
            res_reg, left_reg, right_reg
        )));

        div_instrs.push(blez(neg_right_reg, 2).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Check if the right operand is positive. If so, skip the negation",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(subu(right_reg, zero, right_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Negate the right operand to make it positive",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(addi(r_boolean_reg, zero, 1).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Set the 'is right operand negative?' boolean to True",
            res_reg, left_reg, right_reg
        )));

        div_instrs.push(addu(res_reg, zero, zero).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Initialize the result to 0",
            res_reg, left_reg, right_reg
        )));

        //* Actual division loop
        div_instrs.push(subu(left_reg, left_reg, right_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Compute new remainder (Begin division loop)",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(
            beq(left_reg, zero, 1)
                .comment(&format!("Div ({:?} = {:?} / {:?}): Division loop: check if the remainder is null (division must continue)", res_reg, left_reg, right_reg))
        );
        div_instrs.push(blez(left_reg, 2).comment(
            &format!("Div ({:?} = {:?} / {:?}): Division loop: check if the remainder is strictly negative (Division finished)", res_reg, left_reg, right_reg))
        );
        div_instrs.push(addi(res_reg, res_reg, 1).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Update the result",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(b(-5).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Branch back to division loop (End division loop)",
            res_reg, left_reg, right_reg
        )));

        //* At the end, we need to check if the result should be negated
        div_instrs.push(beq(l_boolean_reg, zero, 1).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Skip next result negation if the left operand was positive",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(subu(res_reg, zero, res_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Negate the result (the left operand was negative)",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(beq(r_boolean_reg, zero, 1).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Skip next result negation if the right operand was positive",
            res_reg, left_reg, right_reg
        )));
        div_instrs.push(subu(res_reg, zero, res_reg).comment(&format!(
            "Div ({:?} = {:?} / {:?}): Negate the result (the right operand was negative) (End division)",
            res_reg, left_reg, right_reg
        )));

        self.pc += 20;

        div_instrs
    }

    //? Helper methods to generate code for unary operations

    // // fn get_unop_len(&self, op: UnOp, expr: Expr) -> u32 {
    // //     let mut unop_len = self.get_expr_len(&expr);
    // //     unop_len += self.get_pop_len();
    // //     unop_len += 1; // The operation itself
    // //     unop_len += self.get_push_len();

    // //     unop_len
    // // }

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

    fn process_array_get(&mut self) -> Instrs {
        // To get the value of an array at a given index, we must think about the stack layout.
        // If we try to do the following:
        // let a = [1, 2, 3];
        // a[1]
        //
        // Then, the stack frame will be as follows: (supposing the scope is new)
        // -4[fp]    1 // a[0]
        // -8[fp]    2 // a[1]
        // -12[fp]   3 // a[2]
        //
        // t1 -> index = 1
        //
        // The final result should look like this:
        // -4[fp]    2 // a[1]
        //
        //
        // Another example:
        // let b = [[true, false], [false, true]];
        // b[1]
        //
        // Then, the stack frame will be as follows: (supposing the scope is new)
        // -4[fp]    1 // b[0][0]
        // -8[fp]    0 // b[0][1]
        // -12[fp]   0 // b[1][0]
        // -16[fp]   1 // b[1][1]
        //
        // t1 -> index = 1
        //
        // The final result should look like this:
        // -4[fp]    0 // b[1][0]
        // -8[fp]    1 // b[1][1]

        //? Debug
        // eprintln!("Processing array get");
        // self.pretty_print_scopes();

        let mut get_instrs = Instrs::new();

        // Therefore, we must move the array values, and remove the useless ones
        let scope = self.scopes.last_mut().unwrap();

        let arr_var = scope.temps.back().unwrap().clone(); // The array itself
        let (arr_offset, arr_size) = arr_var.as_tuple();
        let inner_size = match arr_size.get_inner_size() {
            Some(size) => size,
            None => {
                unreachable!("ERROR: Left element in BinOp::Get has no array VarSize")
            }
        };

        let inner_bytes_size = inner_size.get_bytes_size();
        let nb_moves = inner_bytes_size / 4;

        // We will need to get the correct offset value for the beginning of the movement.
        // However, it depends on the index value, so we can not compute it here, it will be at runtime.
        // Since lw and sw takes an imm16 as offsets, we will need to compute the correct values in a specific
        // register.

        // TODO: index out of bonds check? Or make the code generation require running the vm
        // TODO: (-v mandatory for -c).
        // If the index is 0, we don't have to do anything, so we'll branch the whole thing at the end
        let mut move_instrs = Instrs::new();

        // We will add the beginning jump instruction at the end of the array get, but we update pc here for the offsets
        self.pc += 1;

        // Storing the inner size of the array
        move_instrs.push(
            addiu(t3, zero, inner_bytes_size as i16)
                .comment("BinOp::Get - movement: Store the inner size of the array"),
        );
        self.pc += 1;

        // Getting the offset produced by the index
        move_instrs.append(
            // The smallest value between the bytes size and the index is necessarily the index, it should be the 'right'
            // operand for an optimized mul instruction
            &mut self
                .mul(t2, t3, t1, t4, t5)
                .comment("BinOp::Get - movement: Compute the offset produced by the index"),
        );

        // Setting base address (this is the beginning of the array - the place we want to move the values to)
        move_instrs.push(
            addiu(t0, fp, arr_offset as i16).comment(
                format!(
                    "BinOp::Get - movement: Set the base address of the array at offset {} (base destination address)",
                    arr_offset
                )
                .as_str(),
            ),
        );

        // We don't need t1 anymore
        // Applying the index offset (this is the beginning of the array value - the place we want to move the values from)
        // Don't forget it is a subtraction here! t1 = t0 (base address) - t2 (final index)
        move_instrs.push(
            subu(t1, t0, t2)
                .comment("BinOp::Get - movement: Apply the index offset (base source address)"),
        );
        self.pc += 2;

        // We are now ready to move each value
        // We should move it by the negated index offset value
        for i in 0..nb_moves {
            // We will move each value one by one (we don't need t3 anymore, it was storing the inner size)
            move_instrs.append(&mut self.move_address(t3, t1, t0).comment(
                format!("BinOp::Get - movement: Move the value at offset {}", i).as_str(),
            ));

            if i < nb_moves - 1 {
                // We should move both pointers to the next value -> subtract 4
                move_instrs.push(addiu(t1, t1, -4).comment(
                    "BinOp::Get - movement: Move the departure pointer to the next value",
                ));
                // We should move both pointers to the next value -> subtract 4
                move_instrs.push(
                    addiu(t0, t0, -4).comment(
                        "BinOp::Get - movement: Move the arrival pointer to the next value",
                    ),
                );
                self.pc += 2;
            }
        }

        // Movement is finished!
        let nb_instrs = move_instrs.len();
        // Branching the move part if index (t1) is 0
        get_instrs.push(beq(t1, zero, nb_instrs as i16).comment(
            "Begin BinOp::Get: Branch if the index is 0 (no need to move the array values)",
        )); // its pc has already been set before

        get_instrs.append(&mut move_instrs);

        // Now, we shall erase the remaining parts of the array if there are any
        // To do so, we can simply update the last temp value of the scope, and then remove the last temp value
        self.update_last_temp_on_array_get(inner_size);

        //? Debug
        // eprintln!("After temps update");
        // self.pretty_print_scopes();

        get_instrs.append(
            &mut self
                .remove_last_temp()
                .comment("BinOp::Get: Remove the remaining parts of the array"),
        );

        //? Debug
        // eprintln!("After last temp removal");
        // self.pretty_print_scopes();

        // At this point, the stack should only contain the get value!
        get_instrs
    }

    // // fn get_binop_len(&self, op: BinOp, left: Expr, right: Expr) -> u32 {
    // //     let mut binop_len = self.get_expr_len(&left);
    // //     binop_len += self.get_expr_len(&right);
    // //     binop_len += self.get_pop_len() * 2;

    // //     match op {
    // //         BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Lt => {
    // //             binop_len += 1; // The operation itself
    // //         }
    // //         BinOp::Mul => {
    // //             binop_len += 13; // The operation itself
    // //         }
    // //         BinOp::Div => {
    // //             binop_len += 21; // The operation itself
    // //         }
    // //         BinOp::Eq | BinOp::Ne | BinOp::Le => {
    // //             binop_len += 4; // The operation itself
    // //         }
    // //         BinOp::Gt => {
    // //             binop_len += 5; // The operation itself
    // //         }
    // //         BinOp::Ge => {
    // //             binop_len += 2; // The operation itself
    // //         }
    // //         BinOp::Get => todo!(),
    // //     }

    // //     binop_len += self.get_push_len();

    // //     binop_len
    // // }

    fn process_binop(&mut self, op: BinOp, left: Expr, right: Expr) -> Instrs {
        let mut binop_instrs = Instrs::new();

        binop_instrs.append(&mut self.process_expr(left)); // left is the farthest in the stack
        binop_instrs.append(&mut self.process_expr(right)); // right is the one at the top of the stack
        binop_instrs.append(&mut self.pop(t1)); // Pop the right operand

        if op != BinOp::Get {
            // Can't pop the left operand if it is an array
            binop_instrs.append(&mut self.pop(t0)); // Pop the left operand
        }

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
                binop_instrs
                    .push(addu(t2, zero, t0).comment("BinOp::Mul begin: store the addition value"));
                self.pc += 1;

                binop_instrs.append(
                    &mut self.mul(t0, t2, t1, t3, t4), // Proceed the multiplication
                )
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

                binop_instrs
                    .push(addu(t2, zero, t0).comment("BinOp::Div begin: store the left operand"));
                self.pc += 1;

                binop_instrs.append(
                    &mut self.div(t0, t2, t1, t4, t3, t6, t5, t7), // Proceed the division
                )
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
                // Lower or equal than is looking at greater or equal but from the right perspective
                // ---> This is strictly lower than
                binop_instrs.push(slt(t0, t1, t0).comment("Begin BinOp::Le")); // Less than (strictly)
                                                                               // ---> This is the opposite
                binop_instrs.push(xori(t0, t0, 1).comment("Finish BinOp::Le"));
                // Do it in place
                self.pc += 2;

                // Not optimized (Thanks to Eliott I have seen this!)
                // // binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Le")); // If equal, jump two instructions
                // // binop_instrs.push(slt(t0, t0, t1));
                // // binop_instrs.push(b(1)); // Then we jump the next instruction
                // // binop_instrs.push(addi(t0, zero, 1).comment("Finish BinOp::Le"));
                // // // Here, the result if true
                // // self.pc += 4;
            }
            BinOp::Gt => {
                // This is looking Lt but from the right perspective
                binop_instrs.push(slt(t0, t1, t0).comment("Begin BinOp::Gt")); // Less than (strictly)
                self.pc += 1;

                // Not optimized (Thanks to Eliott I have seen this!)
                // // // Greater than (strictly) is Not(less than or equal)
                // // // ---> This is lower or equal
                // // binop_instrs.push(beq(t0, t1, 2).comment("Begin BinOp::Gt")); // If equal, jump two instructions
                // // binop_instrs.push(slt(t0, t0, t1));
                // // binop_instrs.push(b(1)); // Then we jump the next instruction
                // // binop_instrs.push(addi(t0, zero, 1)); // Here, the result if true
                // //                                       // ---> This is the opposite
                // // binop_instrs.push(xori(t0, t0, 1).comment("Finish BinOp::Gt"));
                // // // Do it in place
                // // self.pc += 5;
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
            BinOp::Get => {
                binop_instrs.append(&mut self.process_array_get());
            }
        }

        if op != BinOp::Get {
            // At the end, we push the result back on the stack if it was not an array get
            // If it was, it is already on stack
            binop_instrs.append(&mut self.push(t0));
        }

        binop_instrs
    }

    //? Helper method to generate code for functions
    // Functions are generated in the following way:
    // - We use an intermediate scope to define the arguments of the function
    // - Then, we process the body of the function
    // - Finally, we return the value of the function if there is one using the t1 register to store the boolean

    //* stack frame layout on call:
    //
    //  4[fp]    ra
    //  0[fp]    old_fp
    // -4[fp]    local 1
    // -8[fp]    local 2, etc.

    // // fn get_call_len(&self, name: String, args: Arguments) -> u32 {
    // //     // Ignore intrinsics such as println!
    // //     if name.ends_with(&"!") {
    // //         return 0;
    // //     }

    // //     let func = match self.get_function(&name) {
    // //         Some(func) => func.clone(),
    // //         None => {
    // //             unreachable!("ERROR: Function '{}' not found in scopes", name);
    // //         }
    // //     };
    // //     let func_decl = func.get_declaration();
    // //     let return_size = VarSize::from(func_decl.ty.clone());

    // //     let mut call_len = self.get_add_scope_len(); // Add the new scope
    // //     for arg in args.0.iter() {
    // //         call_len += self.get_expr_len(arg);
    // //     }
    // //     call_len += 1; // bal
    // //     call_len += self.get_remove_scope_len(Some(return_size)); // Remove the new scope

    // //     call_len
    // // }

    fn process_func_call(&mut self, name: String, args: Arguments) -> Instrs {
        // A function call.
        // It shall initialize the new scope and put the correct values in the stack.
        // Then, it shall jump to the function definition.
        // Finally, it shall return the value of the function and remove the scope.

        // Ignore intrinsics such as println!
        if name.ends_with(&"!") {
            return Instrs::new();
        }

        let func = match self.get_function(&name) {
            Some(func) => func.clone(),
            None => {
                unreachable!("ERROR: Function '{}' not found in the environment", name);
            }
        };
        let (func_offset, func_decl) = func.as_tuple();

        let mut func_instrs = Instrs::new();
        let nb_args = args.0.len();

        // prelude
        func_instrs.append(&mut self.add_scope()); // Add the new scope to define the function arguments
        for i in 0..nb_args {
            // Generate code for each argument

            let arg_expr = args.0.get(i).unwrap().clone();
            func_instrs.append(&mut self.process_expr(arg_expr)); // The value is now located on top of the stack (No need to be moved)

            // Defining the correct argument so that it is not considered a temp but a var
            let param_name = func_decl.parameters.0.get(i).unwrap().id.clone();
            self.define_var(&param_name);
        }

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

        // We must then simulate the returned value in the defined scope so that the scope removal is correctly perfomed
        let scope = self.scopes.last_mut().unwrap();
        let var_size = VarSize::from(func_decl.ty);
        let var = Var::new(-4, var_size);
        scope.update_on_move(var); // If the return type is unit, it will be ignore by the function itself

        // postlude
        // We can now remove the scope of the function: the potential returned value will be moved automatically
        func_instrs.append(&mut self.remove_scope());

        func_instrs
    }

    fn generate_scopes_for_function_call(&self, fd: FnDeclaration) -> Vec<Scope> {
        // Generates a scope for the function call.
        // It only has the arguments of the function in it.
        // It knows every function from the previous scopes.

        // Previously, it used the stack layout where we could only support 3 arguments.
        // Now, parameters are directly stored as local variables from the previous scope

        let mut function_scopes: Vec<Scope> = self.scopes.clone();

        // Then, we remove every known variable from it because we won't need them (we are not even allowed to use them)
        for scope in function_scopes.iter_mut() {
            scope.vars.clear();
            // Keep its size just in case we would go back to previous scopes but should not happen (at least without references)
        }

        // Finally, we generate the correct variables for the function call
        let last_scope = function_scopes.last_mut().unwrap();

        let nb_params = fd.parameters.0.len();
        let mut param_offset = -4;

        for i in 0..nb_params {
            let param = fd.parameters.0.get(i).unwrap();
            let param_id = param.id.clone();
            let param_type = param.ty.clone();
            let param_size = VarSize::from(param_type.clone());

            let param_total_size = param_size.get_bytes_size() as i32;

            last_scope.size += param_total_size as u32;
            let param_var = Var::new(param_offset, param_size);
            last_scope.vars.insert(param_id, param_var);

            param_offset -= param_total_size;
        }

        function_scopes
    }

    fn get_func_def_len(&self, fd: FnDeclaration) -> u32 {
        // Use a copy of the current virtual machine to process the function definition and look at the the resulting size
        let mut vm = self.clone();
        let func_def_len = vm.process_func_def(fd.clone()).len() as u32;
        func_def_len

        // // let mut func_def_len = self.get_block_len(&fd.body);

        // // func_def_len += 1; // jr

        // // func_def_len
    }

    fn process_func_def(&mut self, fd: FnDeclaration) -> Instrs {
        // A function shall take the values of its arguments as local variables from the previous scope    // BEFORE: it was from the frame layout at offsets 16, 12, and 8.
        // This manages the function body.

        let mut func_instrs = Instrs::new();
        let func_name = fd.id.clone();

        // We need to store the position of the function in the instructions memory
        let func = Func::new((self.pc + 1) as u32, fd.clone());
        self.define_function(func); // The function starts on the next instruction

        // We will use a temporary scope to generate the correct function code
        let backup_var_env = self.scopes.clone(); // Save the initial scope
        self.scopes = self.generate_scopes_for_function_call(fd.clone()); // Generate the correct scope for the function call

        // Then, we need to process the block of the function
        func_instrs.append(&mut self.process_block(fd.body));

        // On block return, the returned value will automatically be moved to the previous scope if there is one

        // A main function should not return anywhere, but should instead Halt at its end
        if func_name == "main" {
            self.pc += 1;
            func_instrs
                .push(halt().comment("FUNC_DEF 'main': Halt the program at main function end"));
        } else {
            // We can jump back to the caller, which will handle the return value
            self.pc += 1;
            func_instrs.push(
                jr(ra).comment(format!("FUNC_DEF '{}': Return to the caller", func_name).as_str()),
            );
        }

        // At the end, we need to restore the initial scope for further instructions
        self.scopes = backup_var_env;

        func_instrs
    }

    //? Helper methods to generate code for expressions

    fn get_expr_len(&self, expr: &Expr) -> u32 {
        // Use a copy of the current virtual machine to process the expression and look at the the resulting size
        let mut vm = self.clone();
        let expr_len = vm.process_expr(expr.clone()).len() as u32;

        expr_len

        // // let mut expr_len = 0;

        // // match expr {
        // //     Expr::Lit(lit) => {
        // //         match lit {
        // //             Literal::Unit => {
        // //                 // Do nothing, ignore the unit type
        // //             }
        // //             Literal::Int(_) | Literal::Bool(_) => {
        // //                 expr_len += 1; // Just an addi instruction
        // //                 expr_len += self.get_push_len();
        // //             }
        // //             Literal::Array(arr) => {
        // //                 // The number of expressions depends on the size of the array
        // //                 for e in arr.get_values() {
        // //                     expr_len += self.get_expr_len(e.clone());
        // //                 }
        // //             }
        // //             _ => unimplemented!("Literal {:?} is not supported in backend", lit)
        // //         }
        // //     }
        // //     Expr::Ident(_) => {
        // //         expr_len += 1; // Just an lw instruction
        // //         expr_len += self.get_push_len();
        // //     }
        // //     Expr::BinOp(op, left, right) => {
        // //         expr_len += self.get_binop_len(op, *left, *right);
        // //     }
        // //     Expr::UnOp(op, expr) => {
        // //         expr_len += self.get_unop_len(op, *expr);
        // //     }
        // //     Expr::Par(expr) => {
        // //         expr_len += self.get_expr_len(*expr);
        // //     }
        // //     Expr::Call(name, args) => {
        // //         expr_len += self.get_call_len(name, args);
        // //     }
        // //     Expr::IfThenElse(cond, then_block, else_block) => {
        // //         expr_len += self.get_expr_len(*cond);
        // //         expr_len += self.get_pop_len();

        // //         expr_len += self.get_block_len(then_block) + 1; // +1 for the beq

        // //         if let Some(else_block) = else_block {
        // //             expr_len += self.get_block_len(else_block) + 1; // +1 for the b
        // //         }
        // //     }
        // //     Expr::Block(block) => {
        // //         expr_len += self.get_block_len(block);
        // //     }
        // // }

        // // expr_len
    }

    fn process_expr(&mut self, expr: Expr) -> Instrs {
        let mut expr_instrs = Instrs::new();

        match expr.clone() {
            Expr::Lit(lit) => {
                match lit {
                    Literal::Unit => {
                        // Do nothing, ignore the unit type
                    }
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
                    Literal::Array(arr) => {
                        // We process each expression
                        // This will result in pushing to the stack every element, one after the other as temporary values
                        // Finally, we will gather them into a single temporary value

                        //? Debug
                        // eprintln!("Processing array");
                        // self.pretty_print_scopes();

                        for (i, e) in arr.get_values().iter().enumerate() {
                            expr_instrs.append(
                                &mut self.process_expr(e.clone()).comment(
                                    format!(
                                        "Expr::Lit: Load array element {} of {}",
                                        i + 1,
                                        arr.get_size()
                                    )
                                    .as_str(),
                                ),
                            );

                            //? Debug
                            // eprintln!("After expr {}/{} processing", i + 1, arr.get_size());
                            // self.pretty_print_scopes();
                        }

                        self.create_array(arr.get_size());

                        //? Debug
                        // eprintln!("After array creation");
                        // self.pretty_print_scopes();
                    }
                    _ => unimplemented!("Literal {:?} is not supported in backend", lit),
                }
            }
            Expr::Ident(ident) => {
                // Try to find the variable in a scope

                let fp_offset = self.get_var_offset(&ident);

                match fp_offset {
                    Some((offset, var)) => {
                        let offset = offset as i16;
                        let var = var.clone();

                        let var_size = var.get_var_size();
                        let nb_items = var_size.get_bytes_size() / 4;

                        for i in 0..nb_items {
                            self.pc += 1;
                            expr_instrs.push(
                                lw(t0, offset - 4 * i as i16, fp).comment(
                                    // Store variable value(s) in register t0 (progressively process every item)
                                    format!(
                                        "Expr::Ident: Load the value of {} from the stack at relative position {} in t0 ({} / {})",
                                        ident, offset - 4 * i as i16, i + 1, nb_items
                                    )
                                    .as_str(),
                                ),
                            );
                            expr_instrs.append(
                                &mut self.push(t0).comment(
                                    // Then push the value to the stack
                                    format!(
                                        "Expr::Ident: Push the value of {} from t0 back to the top of the stack ({} / {})",
                                        ident, i + 1, nb_items
                                    )
                                    .as_str(),
                                )
                            );
                        }

                        if nb_items > 1 {
                            // We should combine the temporary values into a single array temporary value
                            self.create_array_matching_varsize(var.get_var_size());
                        }
                    }
                    // This is unreachable because the type checker should have checked this before
                    None => unreachable!("Variable '{}' not defined", ident),
                }
            }
            Expr::BinOp(op, left, right) => {
                expr_instrs.append(&mut self.process_binop(op, *left, *right))
            }
            Expr::UnOp(op, expr) => expr_instrs.append(&mut self.process_unop(op, *expr)),
            Expr::Par(expr) => expr_instrs.append(&mut self.process_expr(*expr)),
            Expr::Call(name, args) => expr_instrs.append(&mut self.process_func_call(name, args)),
            Expr::IfThenElse(cond, then_block, else_block) => {
                // Process the condition
                expr_instrs.append(&mut self.process_expr(*cond)); // It is located in t0, AND at the top of the stack
                expr_instrs.append(&mut self.pop(t0)); // Pop the condition to t0 (seems useless, but if forgotten, the frame layout will be wrong)

                let mut nb_then_instrs = self.get_block_len(&then_block) as i16;
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

                let scope_size_before_then = self.scopes.last().unwrap().size;

                // We must save our current scopes in case there is an else block
                let backup_var_env = self.scopes.clone();

                // Now we can put the then block
                let mut then_instrs = self.process_block(then_block);
                expr_instrs.append(&mut then_instrs);

                if let Some(else_block) = else_block {
                    // We need to restore the scopes before the then block since the two of them
                    // should have the same effect (type checked) and they can never happen at the same time
                    self.scopes = backup_var_env;

                    let nb_else_instrs = self.get_block_len(&else_block) as i16;

                    // If there is an else block, we should jump after the then block
                    self.pc += 1;
                    expr_instrs.push(b(nb_else_instrs).comment("IfThenElse jump after then block"));

                    // Now we can put the else block
                    // We should restore the scope size before the then block to process the else block (to avoid thinking there are two pushed values)
                    self.scopes.last_mut().unwrap().size = scope_size_before_then;
                    let mut else_instrs = self.process_block(else_block);
                    expr_instrs.append(&mut else_instrs);
                }
            }
            Expr::Block(block) => expr_instrs.append(&mut self.process_block(block)),
        }

        expr_instrs
    }

    pub fn full_process_expr(&mut self, expr: Expr) {
        let instrs = self.process_expr(expr);
        self.add_instrs(instrs);
        self.add_halt("End of expression");
    }

    //? Helper methods to generate code for statements

    fn get_statement_len(&self, stmt: &Statement, unit_type: bool) -> u32 {
        // Use a copy of the current virtual machine to process the statement and look at the the resulting size
        let mut vm = self.clone();
        let stmt_len = vm.process_statement(stmt.clone(), unit_type).len() as u32;

        stmt_len

        // // match stmt {
        // //     Statement::Let(_, _, _, expr_opt) => {
        // //         if expr_opt.is_some() {
        // //             stmt_len += self.get_expr_len(expr_opt.unwrap());
        // //         } else {
        // //             stmt_len += self.get_push_len();
        // //         }
        // //     }
        // //     Statement::Assign(left, right) => {
        // //         stmt_len += self.get_expr_len(right);
        // //         stmt_len += self.get_pop_len();

        // //         match left {
        // //             Expr::Ident(name) => {
        // //                 stmt_len += 1; // Just an sw instruction
        // //             }
        // //             _ => todo!(),
        // //         }
        // //     }
        // //     Statement::While(cond, block) => {
        // //         stmt_len += self.get_expr_len(cond);
        // //         stmt_len += self.get_pop_len();

        // //         stmt_len += self.get_block_len(block) + 2; // +2 for the beq and the b instructions
        // //     }
        // //     Statement::Expr(expr) => {
        // //         //! Not valid anymore
        // //         stmt_len += self.get_expr_len(expr); // Result is at the top of the stack (if not unit type)

        // //         if unit_type {
        // //             stmt_len += self.get_pop_len(); // Pop the result
        // //         }

        // //         // let nb_temp_vars = self.scopes.last().unwrap().temps.len();
        // //         // stmt_instrs.append(&mut self.process_expr(expr)); // Result is at the top of the stack (if not unit type)

        // //         // if self.scopes.last().unwrap().temps.len() > nb_temp_vars && unit_type {
        // //         //     // The expression returned a value, but the statement should be unit type
        // //         //     // We need to remove it from the stack
        // //         //     stmt_instrs.append(&mut self.remove_last_temp());
        // //         // }
        // //     }
        // //     Statement::Fn(fn_decl) => (), // Functions definitions are processed when entering a block
        // // }
    }

    fn process_statement(&mut self, stmt: Statement, unit_type: bool) -> Instrs {
        let mut stmt_instrs = Instrs::new();

        match stmt {
            Statement::Let(_, name, ty, expr_opt) => {
                // The type of the variable will have been set by the type checker.
                // If it is not, we cannot get the size of the variable, so we panic.
                let size = match ty {
                    Some(ty) => VarSize::from(ty),
                    None => unreachable!("Variable '{}' has no type", name), // unreachable since type checker adds missing type annotations
                };

                if let Some(expr) = expr_opt {
                    stmt_instrs.append(&mut self.process_expr(expr)); // The value is now located on top of the stack
                } else {
                    // If there is no expression, we just define the variable. It will be initialized to 0, or an array of 0 if it is an array
                    let nb_zeros = size.get_bytes_size() as i32 / 4;
                    for _ in 0..nb_zeros {
                        stmt_instrs.append(&mut self.push(zero)); // Push 0 to the stack
                    }
                }

                // Now we define the variable in our environment
                self.define_var(&name);

                // This does not push anything in the end (unit type)
            }
            Statement::Assign(left, right) => {
                // We first process the expression to get the new value

                //? Debug
                // eprintln!("Last scope before processing assignment:\n{}", self.scopes.last().unwrap());

                stmt_instrs.append(&mut self.process_expr(right)); // The value is now located on top of the stack

                //? Debug
                // eprintln!("Last scope after processing assignment's right side:\n{}", self.scopes.last().unwrap());

                let var_ident = match left.extract_var_identifier() {
                    Some(ident) => ident,
                    None => unreachable!("Left side of assignment is not a variable"),
                };

                let var_res = self.get_var_offset(&var_ident);
                let (fp_offset, var_obj) = match var_res {
                    Some((offset, var)) => (offset, var.clone()),
                    None => unreachable!("Variable '{}' not defined", var_ident),
                };

                let temp_var = match self.scopes.last_mut().unwrap().temps.back() {
                    // Not popped yet
                    Some(var) => var.clone(),
                    None => unreachable!("No temporary value to assign variable '{}'", var_ident),
                };
                let temp_offset = temp_var.get_offset();

                let get_seq = left.extract_get_sequence();
                let nb_gets = get_seq.len();
                let var_size_seq = var_obj.get_var_size().get_inner_size_seq();

                // Assert that we are not trying to access too deep in the array
                if nb_gets > var_size_seq.len() {
                    unreachable!("Array '{}' is not deep enough to access element", var_ident);
                }

                // We will use the move instruction to move the value from the stack to the variable location
                // This is independant of whether it is an array or not
                // We will look at the first temporary value and its size to know how many values to move

                // registers t8 and t9 are the only ones not being used by processing an expression

                // Getting the initial address of the temp value (source address)
                stmt_instrs.push(
                    addi(t8, fp, temp_offset as i16).comment(
                        format!(
                            "ASSIGN '{}': Set the initial stack address (source address) of temp value in t8",
                            var_ident
                        )
                        .as_str(),
                    ),
                );
                self.pc += 1;

                // Getting the initial address of the variable (destination address)
                stmt_instrs.push(
                    addi(t9, fp, fp_offset as i16).comment(
                        format!(
                            "ASSIGN '{}': Set the initial stack address (destination address) of var '{}' in t9",
                            var_ident, var_ident
                        )
                        .as_str(),
                    ),
                );
                self.pc += 1;

                if nb_gets >= 1 {
                    // We are defining a part of an array variable.
                    // We should therefore update the initial address of the variable to the correct position

                    for (i, get_expr) in get_seq.iter().enumerate() {
                        // First process the index expression to collet it in t0
                        //? Debug
                        stmt_instrs.append(&mut self.process_expr(get_seq[0].clone()));
                        stmt_instrs.append(&mut self.pop(t0)); // Pop the index to t0

                        // Then, we need to multiply the index by the size of the inner array
                        let inner_size = var_size_seq[i].get_bytes_size() as i16;
                        stmt_instrs.push(
                            addi(t1, zero, inner_size).comment(
                                format!(
                                    "ASSIGN '{}': Set the size of the inner array {}/{} in t1",
                                    var_ident,
                                    i + 1,
                                    nb_gets
                                )
                                .as_str(),
                            ),
                        );
                        self.pc += 1;

                        stmt_instrs.append(
                            &mut self.mul(t2, t0, t1, t3, t4).comment(
                                format!(
                                    "ASSIGN '{}': Multiply the index by the size of the inner array",
                                    var_ident
                                )
                                .as_str(),
                            ),
                        );

                        // Then, we remove the result to the initial address of the variable (we increase the offset)
                        stmt_instrs.push(
                            sub(t9, t9, t2).comment(
                                format!(
                                    "ASSIGN '{}': Update the destination address with 'get' {}/{} in t9",
                                    var_ident, i + 1, nb_gets
                                )
                                .as_str(),
                            ),
                        );
                        self.pc += 1;
                    }
                }

                // Now that both initial addresses are set, we can start moving one value after the other

                let temp_size = temp_var.get_bytes_size();
                let nb_moves = temp_size / 4;

                for i in 0..nb_moves {
                    stmt_instrs.append(
                        &mut self.move_address(t0, t8, t9).comment(
                            format!(
                                "ASSIGN '{}': Move value {}/{} from temp to var",
                                var_ident,
                                i + 1,
                                nb_moves
                            )
                            .as_str(),
                        ),
                    );

                    // If not last move, need to update both addresses (subtract 4)
                    if i < nb_moves - 1 {
                        stmt_instrs.push(
                            addi(t8, t8, -4).comment(
                                format!(
                                    "ASSIGN '{}': Update source address after move {}/{}",
                                    var_ident,
                                    i + 1,
                                    nb_moves
                                )
                                .as_str(),
                            ),
                        );
                        stmt_instrs.push(
                            addi(t9, t9, -4).comment(
                                format!(
                                    "ASSIGN '{}': Update destination address after move {}/{}",
                                    var_ident,
                                    i + 1,
                                    nb_moves
                                )
                                .as_str(),
                            ),
                        );
                        self.pc += 2;
                    }
                }

                // Finally, we remove the temporary value from the stack
                stmt_instrs.append(&mut self.remove_last_temp());

                // The assignment is finished!
                // Everything is in place!

                // This does not push anything in the end (unit type)
            }
            Statement::While(cond, block) => {
                // While loops are an if statement, where the last instruction jumps back to the condition
                let mut while_instrs = Instrs::new();

                // First look at the condition
                while_instrs.append(&mut self.process_expr(cond)); // It is located in t0, but also at the top of the stack
                while_instrs.append(&mut self.pop(t0)); // Pop the condition to t0 (seems useless, but if forgotten, the frame layout will be wrong)

                // Needs +1 because will have a final jump back to the condition instruction
                let nb_block_instrs = self.get_block_len(&block) as i16 + 1;

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

                // This does not push anything in the end (unit type)
            }
            Statement::Expr(expr) => {
                // An expression should not remain on the stack if the satement is meant to be unit type
                //? Debug
                // eprintln!("Before expression process:");
                // self.pretty_print_scopes();

                let nb_temp_vars = self.scopes.last().unwrap().temps.len();
                stmt_instrs.append(&mut self.process_expr(expr)); // Result is at the top of the stack (if not unit type)

                let returned_value = self.scopes.last().unwrap().temps.len() > nb_temp_vars;
                //? Debug
                // eprintln!(
                //     "After expression process: (unit type: {}, returned_value: {})",
                //     unit_type, returned_value
                // );
                // self.pretty_print_scopes();

                if returned_value && unit_type {
                    // The expression returned a value, but the statement should be unit type
                    // We need to remove it from the stack
                    stmt_instrs.append(&mut self.remove_last_temp());
                }
            }
            Statement::Fn(fn_decl) => {} // Functions definitions are processed when entering a block
        }

        stmt_instrs
    }

    //? Helper method to generate code for blocks

    // // fn get_define_block_functions_len(&mut self, block: Block) -> u32 {
    // //     // Should be called on a copy of the initial BVM to avoid side effects

    // //     let mut def_len = 1; // Jump over the block of functions

    // //     let _ = self.add_scope();

    // //     let mut func_vec: Vec<FnDeclaration> = Vec::new();

    // //     for stmt in block.statements {
    // //         match stmt {
    // //             Statement::Fn(fn_decl) => {
    // //                 func_vec.push(fn_decl);
    // //             }
    // //             _ => (),
    // //         }
    // //     }

    // //     for fd in func_vec.iter() {
    // //         let func = Func::new(0, fd.clone());
    // //         self.define_function(func); // Defines the dummy function so that function calls work
    // //     }

    // //     let defined_func = self.scopes.last().unwrap().functions.clone();

    // //     for (_, func) in defined_func.iter() {
    // //         def_len += self.get_func_def_len(func.get_declaration().clone());
    // //     }

    // //     def_len
    // // }

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

    fn get_block_len(&self, block: &Block) -> u32 {
        // Use a copy of the current virtual machine to process the block and look at the the resulting size
        let mut vm = self.clone();
        let block_len = vm.process_block(block.clone()).len() as u32;

        block_len

        // // let mut block_len = self.get_add_scope_len(); // Add scope

        // // // Create a temporary BVM to define the functions in
        // // let temp_bvm = &mut self.clone();

        // // block_len += temp_bvm.get_define_block_functions_len(block.clone()); // Define functions in the temp BVM as well

        // // let nb_statements = block.statements.len();
        // // for (i, stmt) in block.statements.iter().enumerate() {
        // //     // The only statement that should not be unit type is the last one if there is no semi.
        // //     let should_be_unit = !(i == nb_statements - 1 && !block.semi);
        // //     block_len += temp_bvm.get_statement_len(stmt.clone(), should_be_unit); // Process statements
        // // }

        // // let return_size = match block.return_type.clone() {
        // //     Some(ty) => VarSize::from(ty),
        // //     None => unreachable!("Type checker should have set block types"),
        // // };

        // // block_len += temp_bvm.get_remove_scope_len(Some(return_size)); // Remove scope

        // // block_len
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
        let nb_statements = block.statements.len();
        for (i, stmt) in block.statements.iter().enumerate() {
            // The only statement that should not be unit type is the last one if there is no semi.
            let should_be_unit = !(i == nb_statements - 1 && !block.semi);
            block_instrs.append(&mut self.process_statement(stmt.clone(), should_be_unit));
        }

        // Finally, it removes the scope
        let mut scope_instr = self.remove_scope();
        block_instrs.append(&mut scope_instr);

        block_instrs
    }

    pub fn full_process_block(&mut self, block: Block) {
        let instrs = self.process_block(block);
        self.add_instrs(instrs);
        self.add_halt("End of block");
    }

    //? Helper method to generate code for programs

    fn get_prog_len(&self, prog: &Prog) -> u32 {
        // Use a copy of the current virtual machine to process the program and look at the the resulting size
        let mut vm = self.clone();
        let prog_len = vm.process_prog(prog.clone()).len() as u32;

        prog_len

        // // let mut prog_len = 0;

        // // // We will use a temporary BVM to define the functions in
        // // let temp_bvm = &mut self.clone();

        // // // We shall first define each function once, whatever the offset used
        // // for fd in prog.0.iter() {
        // //     let func = Func::new(0, fd.clone());
        // //     temp_bvm.define_function(func); // Defines the dummy function so that function calls work
        // // }

        // // for fd in prog.0.iter() {
        // //     // The order does not matter here
        // //     prog_len += temp_bvm.get_func_def_len(fd.clone());
        // // }

        // // prog_len += 1; // Jump over the block of functions

        // // prog_len += temp_bvm.get_call_len("main".to_string(), Arguments::new(vec![]));

        // // prog_len
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

        // We will add the jump instruction later, but to correctly set the pc, we add it here
        self.pc += 1;

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

        // No need for an Halt instruction, it is automatically added at the end of the main declaration

        final_instrs
    }

    pub fn full_process_prog(&mut self, prog: Prog) {
        let instrs = self.process_prog(prog);
        self.add_instrs(instrs);
        // Halt is already added at the end of the program
    }
}
