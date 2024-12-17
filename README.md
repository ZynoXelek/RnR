# RnR: Rust in Rust

This project aims to implement a compiler able to manage a subset of the Rust language.

The compiler is coded in Rust, and supports the basics of the Rust language.

## Types

The implemented types are the following:

- **unit**: the basic unit type
- **i32**: basic integers
- **bool**: booleans
- **String**: to be able to print things in the terminal
- **Arrays**: arrays can be defined using any of the existing types.
- **References** (WIP)

## Operations

The implemented operations are the following:
- Unary Operators
  - **!**: boolean negation
  - **-**: integer negation
  - **&**: reference (WIP)
  - **\***: pointer (WIP)
- Binary Operators
  - **+**, **-**, **\***, **/**: basic operations for integers only
  - **&&**, **||**: basic operations for booleans only
  - **<**, **<=**, **>=**, **>**: comparison operations for integers and Strings
  - **==**, **!=**: comparison operations for any type
  - **\[i\]**: get operation for arrays

## How to use it

The compiler can be used thanks to its **CLI** which uses the **clap** crate. It includes basic commands.

### Basic behavior

The default command is the following one:
```bash
rnr
```

This only parses the content of the `main.rs` file located in the current folder. It does not proceed any additional treatment.

### Additional subcommands

Here are the possible subcommands you can add to apply other treatments.

#### Get help on commands

You can have additional information using the following subcommand:
```bash
-h
```
Or
```bash
--help
```

#### Change the input file

You can change the input file from `main.rs` to any other file by adding this subcommand:
```bash
-i <relative_input_path>
```
Or
```bash
--input <relative_input_path>
```

#### Dump the generated AST

You can dump the generated Abstract Syntax Tree from the parsed program in a file of your choice by adding the following subcommand:
```bash
-a <relative_output_path>
```
Or
```bash
--ast <relative_output_path>
```

#### Process type checking

You can process type checking on the parsed result by adding the following subcommand:
```bash
-t
```
Or
```bash
--type_check
```

#### Evaluate the parsed program using the RnR VM

You can process the evaluation of the parsed program using the RnR Virtual Machine by adding the following subcommand:
```bash
-v
```
Or
```bash
--vm
```
Or
```bash
--virtual_machine
```

#### Generate the assembler code

You can generate the assembler code corresponding to the parsed program with the RnR backend by adding the following subcommand:
```bash
-c
```
Or
```bash
--code_gen
```

#### Dump the generated ASM

You can dump the generated ASM from the parsed program in a file of your choice by adding the following subcommand: (It requires that you have used the `-c` or `--code_gen` subcommand)
```bash
--asm <relative_output_path>
```

#### Run the generated ASM

You can run the generated ASM from the parsed program by adding the following subcommand: (It requires that you have used the `-c` or `--code_gen` subcommand)
```bash
-r
```
Or
```bash
--run
```

## Some additional remarks on the project

### Parser

The RnR parser uses the **syn** crate. It implements a subset of it thanks to the generated **TokenStream**.
