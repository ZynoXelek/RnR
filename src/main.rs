// This file implements the CLI for the program.
use clap::Parser;
use mips::{error::Error as MipsError, instrs::Instrs, vm::Mips};
use std::{fmt, fs, io, str::FromStr};

use rnr::{
    asm_parse::parse_instrs,
    ast::Prog,
    backend::get_formatted_instrs,
    common::{parse, CheckType, Eval, GetInstructions, Optimize},
    error::{Error, EvalError, TypeError},
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
    #[arg(
        short = 'i',
        long = "input",
        value_name = "PATH",
        help = "Path to the input file to read the program from"
    )]
    input: Option<FilePath>,

    #[arg(
        short = 't',
        long = "type_check",
        help = "Type check the input program"
    )]
    type_check: bool,

    #[arg(
        short = 'o',
        long = "optimize",
        help = "Optimize the input program to remove dead code (requires type checking)"
    )]
    optimize: bool,

    #[arg(
        short = 'a',
        long = "ast",
        value_name = "PATH",
        help = "Write the read/generated AST to the given output file (is optimized if --optimize is set)"
    )]
    ast: Option<FilePath>,

    #[arg(
        short = 'v',
        long = "virtual_machine",
        alias = "vm",
        help = "Run the program in the RnR virtual machine (requires code optimization)"
    )]
    virtual_machine: bool,

    #[arg(
        short = 'c',
        long = "code_gen",
        help = "Generate the backend ASM code (requires code optimization)"
    )]
    code_gen: bool,

    #[arg(
        long = "asm",
        value_name = "PATH",
        help = "Write the generated ASM to the given output file (requires code generation)"
    )] // No short since it would be ambiguous with the `--ast` flag
    asm: Option<FilePath>,

    #[arg(
        short = 'r',
        long = "run",
        help = "Run the generated ASM code using the Mips VM (requires code generation)"
    )]
    run: bool,

    #[arg(
        long = "asm_input",
        alias = "asmi",
        value_name = "PATH",
        help = "Read an ASM representation from the given input file. The '--run' option will run this one instead. (incompatible with other flags)"
    )]
    asm_input: Option<FilePath>,
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
const YELLOW_COLOR: &str = "\x1b[33m";

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

fn type_check_subcommand(prog: Prog) -> Result<Prog, TypeError> {
    // Type check the parsed AST

    let checked_prog: Result<Prog, TypeError> = prog.check_type();

    match checked_prog {
        Ok(prog) => {
            println!(" * Type check: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(prog)
        }
        Err(e) => {
            eprintln!(" * Type check: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
            Err(e)
        }
    }
}

//* Optimize command: `--optimize` / `-o`

fn optimize_subcommand(prog: Prog) -> Result<Prog, Error> {
    //TODO: Use custom error type
    // Optimize the parsed AST

    let opt_prog: Result<Prog, Error> = prog.optimize();

    match opt_prog {
        Ok(opt_prog) => {
            println!(" * Optimization: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(opt_prog)
        }
        Err(e) => {
            eprintln!(" * Optimization: {}error {}{}", RED_COLOR, e, DEFAULT_COLOR);
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
            eprintln!(
                " * VM evaluation: {}error {}{}",
                RED_COLOR, e, DEFAULT_COLOR
            );
            Err(e)
        }
    }
}

//* Code generation command: `--code_gen` / `-c`

fn code_gen_subcommand(prog: Prog) -> Result<Instrs, Error> {
    //TODO: Use custom error type
    // Generate code

    let instrs = prog.get_instructions();

    match instrs {
        Ok(instrs) => {
            println!(
                " * Code generation: {}success!{}",
                GREEN_COLOR, DEFAULT_COLOR
            );
            Ok(instrs)
        }
        Err(e) => {
            eprintln!(
                " * Code generation: {}error {}{}",
                RED_COLOR, e, DEFAULT_COLOR
            );
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
        Ok(_) | Err(MipsError::Halt) => {
            println!(" * Running code: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
            Ok(())
        }
        Err(e) => {
            eprintln!(
                " * Running code: {}error {:?}{}",
                RED_COLOR, e, DEFAULT_COLOR
            );
            Err(e)
        }
    }
}

//* Load ASM from file: '--asm_input <path>' or '--asmi <path>'

fn load_asm_from_file(path: &str) -> Result<Instrs, Error> {
    // Reading an ASM program from a file

    let file = fs::read_to_string(path);
    match file {
        Ok(content) => {
            let instrs = parse_instrs(&content);

            match instrs {
                Ok(instrs) => {
                    println!(" * ASM loading: {}success!{}", GREEN_COLOR, DEFAULT_COLOR);
                    Ok(instrs)
                }
                Err(e) => {
                    eprintln!(
                        " * ASM loading: {}parsing error {}{}",
                        RED_COLOR, e, DEFAULT_COLOR
                    );
                    Err(e)
                }
            }
        }
        Err(e) => {
            eprintln!(
                " * ASM loading: {}error reading file {}{}",
                RED_COLOR, e, DEFAULT_COLOR
            );
            Err(e.to_string())
        }
    }
}

//?#####################################################
//?#                      MAIN                         #
//?#####################################################

fn main() {
    let args = Args::parse();
    //? Debug
    // eprintln!("Parsed args: {:?}", args);

    //* If an asm path is provided, we do not need to check a program.
    //* --asm_input is only compatible with the --run flag, not with any other flag.

    if let Some(asm_input) = &args.asm_input {
        if args.input.is_some()
            || args.ast.is_some()
            || args.type_check
            || args.optimize
            || args.virtual_machine
            || args.code_gen
            || args.asm.is_some()
        {
            eprintln!(
                "{}Warning: ASM input provided (--asm_input) but other flags are set (the only one compatible is --run). Ignoring other flags.{}",
                YELLOW_COLOR, DEFAULT_COLOR
            );
        }

        let instrs = load_asm_from_file(asm_input.path.as_str()).unwrap();

        println!("Loaded ASM code:\n{}", get_formatted_instrs(instrs.clone()));

        if args.run {
            println!("\n ----------------- ");
            let mut mips = Mips::new(instrs);
            _ = run_subcommand(&mut mips);
        }
        return;
    } else {
        let mut prog = parse_prog_from_file(args.get_file_path()).unwrap();
        println!("Parsed program:\n{}", prog);

        // Process subcommands in order:
        // 1. AST
        if let Some(ast_path) = &args.ast {
            println!("\n ----------------- ");
            ast_subcommand(prog.clone(), ast_path.path.as_str()).unwrap();
        }

        //? Print a warning if an option that requires a missing previous step is used
        //? To be able to optimize the program, run the virtual machine, or the code generation, we must have type checked the program first
        if args.optimize && !args.type_check {
            eprintln!(
                "{}Warning: Optimization requested (-o) without type checking (-t). Ignoring optimization.{}",
                YELLOW_COLOR, DEFAULT_COLOR
            );
        }

        if args.virtual_machine && !args.type_check {
            eprintln!(
                "{}Warning: VM requested (-v) without type checking (-t). Ignoring VM.{}",
                YELLOW_COLOR, DEFAULT_COLOR
            );
        }

        if args.code_gen && !args.type_check {
            eprintln!(
                "{}Warning: Code generation requested (-c) without type checking (-t). Ignoring code generation.{}",
                YELLOW_COLOR, DEFAULT_COLOR
            );
        }

        // 2. Type check
        if args.type_check {
            println!("\n ----------------- ");
            prog = type_check_subcommand(prog.clone()).unwrap();

            if args.code_gen && !args.optimize {
                eprintln!(
                    "{}Warning: Code generation requested (-c) without code optimization (-o). Ignoring code generation.{}",
                    YELLOW_COLOR, DEFAULT_COLOR
                );
            }

            // 3. Optimization
            if args.optimize {
                println!("\n ----------------- ");
                let opt_prog = optimize_subcommand(prog.clone()).unwrap();
                println!("Optimized program:\n{}", opt_prog);
                prog = opt_prog;

                // 3.5 Optimized AST
                if let Some(ast_path) = &args.ast {
                    println!("\n ----------------- ");
                    ast_subcommand(prog.clone(), ast_path.path.as_str()).unwrap();
                }
            }

            // 4. Virtual machine
            if args.virtual_machine {
                println!("\n ----------------- ");
                vm_subcommand(prog.clone()).unwrap();
            }

            // 5. Code generation
            if args.optimize && args.code_gen {
                //? Print a warning if an option that requires a missing previous step is used
                if args.asm.is_some() && !args.code_gen {
                    eprintln!(
                        "{}Warning: ASM output requested (--asm) without code generation (-c). Ignoring ASM output.{}",
                        YELLOW_COLOR, DEFAULT_COLOR
                    );
                }

                if args.run && !args.code_gen {
                    eprintln!(
                        "{}Warning: Run requested (-r) without code generation (-c). Ignoring run.{}",
                        YELLOW_COLOR, DEFAULT_COLOR
                    );
                }

                println!("\n ----------------- ");
                let instrs = code_gen_subcommand(prog.clone());
                if instrs.is_err() {
                    eprintln!(
                        "{}Error: Code generation failed. Skipping ASM and run steps.{}",
                        RED_COLOR, DEFAULT_COLOR
                    );
                    return;
                }
                let instrs = instrs.unwrap();
                println!("Generated code:\n{}", get_formatted_instrs(instrs.clone()));

                // 6. ASM
                if let Some(asm_path) = &args.asm {
                    println!("\n ----------------- ");
                    _ = asm_subcommand(instrs.clone(), asm_path.path.as_str());
                }

                // 7. Run
                if args.run {
                    println!("\n ----------------- ");
                    let mut mips = Mips::new(instrs);
                    _ = run_subcommand(&mut mips);
                }
            }
        }
    }
}
