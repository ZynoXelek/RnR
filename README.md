# RnR: Rust in Rust

This project aims to implement a compiler able to manage a subset of the Rust language, as part of the LTU **Compiler construction and formal languages** course.

The compiler is implemented in Rust, and supports the basics of the Rust language.
It uses the Mips crate from the teacher for the backend code generation.

## Table of content

<!-- TOC -->
- [RnR: Rust in Rust](#rnr-rust-in-rust)
  - [Table of content](#table-of-content)
  - [Implemented Types](#implemented-types)
  - [Implemented Operations](#implemented-operations)
  - [Examples of valid code](#examples-of-valid-code)
  - [How to use it](#how-to-use-it)
    - [Information about the repository](#information-about-the-repository)
      - [Building the release binary](#building-the-release-binary)
      - [Not building the release binary](#not-building-the-release-binary)
    - [Basic behavior](#basic-behavior)
    - [Additional subcommands](#additional-subcommands)
      - [Get help on commands](#get-help-on-commands)
      - [Change the input file](#change-the-input-file)
      - [Dump the generated AST](#dump-the-generated-ast)
      - [Process type checking](#process-type-checking)
      - [Optimize the input program](#optimize-the-input-program)
      - [Evaluate the parsed program using the RnR VM](#evaluate-the-parsed-program-using-the-rnr-vm)
      - [Generate the assembler code](#generate-the-assembler-code)
      - [Dump the generated ASM](#dump-the-generated-asm)
      - [Run the generated ASM](#run-the-generated-asm)
    - [Recommended commands](#recommended-commands)
  - [Some additional remarks on the project](#some-additional-remarks-on-the-project)
    - [Parser](#parser)
    - [CLI](#cli)
    - [Backend](#backend)
  - [Future implementations](#future-implementations)

## Implemented Types

The implemented types are the following:

- **unit**: the basic unit type -> `()`
- **i32**: basic integers -> `1, 2, -3`
- **bool**: booleans -> true, false
- **String**: to be able to print things in the terminal -> `"Hello World!"`
- **Arrays**: arrays can be defined using any of the existing types -> `[1, -2, 3 * 4]`
<!-- - **References** (WIP) -->

## Implemented Operations

The implemented operations are the following:

- Unary Operators
  - **!**: boolean negation -> `!true`
  - **-**: integer negation -> `-5`
  <!-- - **&**: reference (WIP)
  - **\***: pointer (WIP) -->
- Binary Operators
  - **+**, **-**, **\***, **/**: basic operations for integers only -> `3 * 4`
  - **&&**, **||**: basic operations for booleans only -> `true || false`
  - **<**, **<=**, **>=**, **>**: comparison operations for integers and Strings -> `"abc" < "bac"`
  - **==**, **!=**: comparison operations for any type -> `3 * 4 == 13 - 1`
  - **\[i\]**: get operation for arrays -> `[1, 2, 3][1]`

## Examples of valid code

Here is an example of a small program that RnR can compile. You can find other in `./examples`.

```rust
// An example program introducing a simple recursive factorial function

fn fact(n: i32) -> i32 {
    if n == 0 {
        1
    } else {
        n * fact(n - 1)
    }
}

fn main() {
    let n = 5;
    let a = fact(n);
    println!("{}! = {}", n, a);
}
```

## How to use it

### Information about the repository

The compiler can be used thanks to its **CLI** which uses the **clap** crate. It includes basic commands.

To be able to use them, you have two options once you have cloned this repository.

#### Building the release binary

You can build the binary from the cloned repository by calling the following command:

```bash
cargo build --release
```

Once it is done, the `rnr` executable will be located at `./target/release/rnr`. You can then use it with the commands described below. For instance:

```bash
rnr -i <path_to_my_file> --type_check -ovcr
```

#### Not building the release binary

You can also directly use cargo to use RnR. To do so, you will have to call `cargo run`. However, to be able to add options as described below, you will have to use `--` to tell the `cargo run` command that the following strings are not options, but should be passed to `main.rs`. If you don't, it will try to identify any option (`-t` for instance) as an option of the `cargo run` command. Here is an example:

```bash
cargo run -- -i <path_to_my_file> --type_check -ovcr
```

The first time you call it, it will build the whole project (not release version) and it will be quite long, but for future uses, it will be much quicker as it won't have to do it again.

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
--help
```

#### Change the input file

You can change the input file from `main.rs` to any other file by adding this subcommand:

```bash
-i <relative_input_path>
--input <relative_input_path>
```

#### Dump the generated AST

You can dump the generated Abstract Syntax Tree from the parsed program in a file of your choice by adding the following subcommand:

```bash
-a <relative_output_path>
--ast <relative_output_path>
```

> **Note:** If you use the optimizer option (`-o`/`--optimize`), the dumped AST will be the optimized AST extracted from your input program.

#### Process type checking

You can process type checking on the parsed result by adding the following subcommand:

```bash
-t
--type_check
```

This process will verify that your input program is formally correct.

#### Optimize the input program

You can process a program optimization on the parsed result by adding the following subcommand:

```bash
-o
--optimize
```

This process will try to eliminate any dead code from the input program.
It requires that the type-check option is used.

#### Evaluate the parsed program using the RnR VM

You can process the evaluation of the parsed program using the RnR Virtual Machine by adding the following subcommand:

```bash
-v
--vm
--virtual_machine
```

This process requires that the type-check option is used.
It also reveals array access errors by checking wether an index is within bonds or not. This is why this option is mandatory to generate the backend code, which does not implement these checks.

#### Generate the assembler code

You can generate the assembler code corresponding to the parsed program with the RnR backend by adding the following subcommand:

```bash
-c
--code_gen
```

This process requires that the evaluating option (`-v`) is used.

#### Dump the generated ASM

You can dump the generated ASM from the parsed program in a file of your choice by adding the following subcommand: (It requires that you have used the `-c` or `--code_gen` subcommand)

```bash
--asm <relative_output_path>
```

This process requires that the code generation option is used.

#### Run the generated ASM

You can run the generated ASM from the parsed program by adding the following subcommand: (It requires that you have used the `-c` or `--code_gen` subcommand)

```bash
-r
--run
```

This process requires that the code generation option is used.

### Recommended commands

The most useful commands are therefore the two following ones:

- Reading a custom file, type checking it, optimizing it, and running it through the virtual machine:

```bash
rnr -i <path_to_my_file> -tov
```

- Reading a custom file, type checking it, optimizing it, generating the ASM code and running it:

```bash
rnr -i <path_to_my_file> -tovcr
```

## Some additional remarks on the project

### Parser

The RnR parser uses the **syn** crate. It implements a subset of it thanks to the generated **TokenStream**.

### CLI

The Cli uses the **clap** crate to parse the input commands.

### Backend

The backend is emulated in a virtual machine which copies a subset of the behavior of the MIPS 3k processor. This crate can be found [here](https://vesuvio-git.neteq.ltu.se/pln/mips).

> Note: The backend code does not support terminal printing. Therefore, `println!` calls can only be verified by evaluating the parsed AST in the RnR virtual machine (`-v`).
> However it can use any of the other defined types and operations.

## Future implementations

RnR should still evolve in the future. There are several features from the original Rust language I would like to implement, such as:

- References
- New keywords (`return`, `break`, `continue`)
- Custom structures
- For loops and iterators
