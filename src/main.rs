// This file implements the CLI for the program.
use clap::Parser;
use mips::{error::Error as MipsError, instrs::Instrs, vm::Mips};
use std::{fmt, fs, io, str::FromStr};

use rnr::{
    ast::Prog,
    backend::get_formatted_instrs,
    common::{parse, Eval, EvalType, GetInstructions},
    error::{EvalError, TypeError, Error},
    type_check::TypeVal,
    vm::Val,
};

//?#####################################################
//?#              CLI Structs and Enums                #
//?#####################################################

#[derive(Parser, Debug, Clone)]
struct FilePath {
    path: String,
}

impl FilePath {
    fn new(path: &str) -> Self {
        Self {
            path: path.to_string(),
        }
    }
}

impl FromStr for FilePath {
    type Err = String; // Won't be used

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FilePath::new(s))
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.path)
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short = 'i', long = "input")] // Default values but can be changed this way
    input: Option<FilePath>,

    #[arg(short = 'a', long = "ast")]
    ast: Option<FilePath>,

    #[arg(short = 't', long = "type_check")]
    type_check: bool,

    #[arg(short = 'v', long = "virtual_machine", alias = "vm")]
    virtual_machine: bool,

    #[arg(short = 'c', long = "code_gen")]
    code_gen: bool,

    #[arg(long = "asm")] // No short since it would be ambiguous with the `--ast` flag
    asm: Option<FilePath>,

    #[arg(short = 'r', long = "run")]
    run: bool,
}

impl Args {
    fn get_file_path(&self) -> &str {
        match &self.input {
            Some(path) => path.path.as_str(),
            None => "main.rs",
        }
    }
}

//?#####################################################
//?#                Helper Functions                   #
//?#####################################################

const DEFAULT_COLOR: &str = "\x1b[0m";
const RED_COLOR: &str = "\x1b[31m";
const GREEN_COLOR: &str = "\x1b[32m";

// Parse the content of the file

fn parse_prog(content: &str) -> Prog {
    // Parse the content of the file
    let prog: Prog = parse(content);
    prog
}

fn parse_prog_from_file(path: &str) -> Result<Prog, io::Error> {
    // Trying to read the file
    let file = fs::read_to_string(path);
    match file {
        Ok(content) => {
            // Try to parse the file
            let prog = parse_prog(&content);
            println!(" * File parsing: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(prog)
        }
        Err(e) => {
            eprintln!(" * File parsing: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//?#####################################################
//?#                   SUBCOMMANDS                     #
//?#####################################################

//* AST command: `--ast <path>` / `-a <path>`

fn ast_subcommand(prog: Prog, path: &str) -> io::Result<()> {
    // Dumps the parsed AST to a file
    let str_repr = format!("{}", prog);

    // Trying to write the file
    let result = fs::write(path, str_repr);
    match result {
        Ok(_) => {
            println!(" * AST dump: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(" * AST dump: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* Type check command: `--type_check` / `-t`

fn type_check_subcommand(prog: Prog) -> Result<(), TypeError> {
    // Type check the parsed AST

    let prog_type: Result<TypeVal, TypeError> = prog.eval_type();

    match prog_type {
        Ok(_) => {
            println!(" * Type check: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(" * Type check: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* VM command: `--virtual_machine` / `-vm`

fn vm_subcommand(prog: Prog) -> Result<(), EvalError> {
    // Run the virtual machine

    println!(" * Running the virtual machine...");

    let prog_type: Result<Val, EvalError> = prog.eval();

    match prog_type {
        Ok(_) => {
            println!(" * VM evaluation: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(" * VM evaluation: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* Code generation command: `--code_gen` / `-c`

fn code_gen_subcommand(prog: Prog) -> Result<Instrs, Error> { //TODO: Use custom error type
    // Generate code

    let instrs = prog.get_instructions();

    match instrs {
        Ok(instrs) => {
            println!(" * Code generation: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(instrs)
        },
        Err(e) => {
            eprintln!(" * Code generation: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* ASM command: `-asm <path>`

fn asm_subcommand(instrs: Instrs, path: &str) -> io::Result<()> {
    // Dumps the generated code to a file

    let str_repr = get_formatted_instrs(instrs);

    // Trying to write the file
    let result = fs::write(path, str_repr);
    match result {
        Ok(_) => {
            println!(" * ASM dump: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(" * ASM dump: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* Run command: `-r`

fn run_subcommand(mips: &mut Mips) -> Result<(), MipsError> {
    // Run the generated code using the Mips VM

    let result = mips.run();
    match result {
        Ok(_) => {
            println!(" * Running code: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(" * Running code: {}error {:?}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//TODO: More? Running code gen from an ASM file for instance? (Need parsing for ASM)

//?#####################################################
//?#                      MAIN                         #
//?#####################################################

fn main() {
    let args = Args::parse();
    println!("Parsed args: {:?}", args);

    let prog = parse_prog_from_file(args.get_file_path()).unwrap();
    println!("Parsed program:\n{}", prog);

    // Process subcommands in order:
    // 1. AST
    if let Some(ast_path) = &args.ast {
        println!("\n ----------------- ");
        ast_subcommand(prog.clone(), ast_path.path.as_str()).unwrap();
    }

    // 2. Type check
    if args.type_check {
        println!("\n ----------------- ");
        type_check_subcommand(prog.clone()).unwrap();
    }

    // 3. Virtual machine
    if args.virtual_machine {
        println!("\n ----------------- ");
        vm_subcommand(prog.clone()).unwrap();
    }

    // 4. Code generation
    if args.code_gen {
        println!("\n ----------------- ");
        let instrs = code_gen_subcommand(prog.clone()).unwrap();
        println!("Generated code:\n{}", get_formatted_instrs(instrs.clone()));

        // 5. ASM
        if let Some(asm_path) = &args.asm {
            println!("\n ----------------- ");
            asm_subcommand(instrs.clone(), asm_path.path.as_str()).unwrap();

            // 6. Run
            if args.run {
                println!("\n ----------------- ");
                let mut mips = Mips::new(instrs);
                run_subcommand(&mut mips).unwrap();
            }
        }
    }
}
