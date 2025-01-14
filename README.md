# RnR: Rust in Rust

This project aims to implement a compiler able to manage a subset of the Rust language, as part of the LTU **Compiler construction and formal languages** course.

The compiler is implemented in Rust, and supports the basics of the Rust language.
It uses the Mips crate from the teacher for the backend code generation.

## Implemented Types

The implemented types are the following:

- **unit**: the basic unit type
- **i32**: basic integers
- **bool**: booleans
- **String**: to be able to print things in the terminal
- **Arrays**: arrays can be defined using any of the existing types.
<!-- - **References** (WIP) -->

## Implemented Operations

The implemented operations are the following:

- Unary Operators
  - **!**: boolean negation
  - **-**: integer negation
  <!-- - **&**: reference (WIP)
  - **\***: pointer (WIP) -->
- Binary Operators
  - **+**, **-**, **\***, **/**: basic operations for integers only
  - **&&**, **||**: basic operations for booleans only
  - **<**, **<=**, **>=**, **>**: comparison operations for integers and Strings
  - **==**, **!=**: comparison operations for any type
  - **\[i\]**: get operation for arrays

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

> **Note:** If you use the optimizer option (`-o`/`--optimize`), the dumped AST will be the optimized AST extracted from your input program.

#### Process type checking

You can process type checking on the parsed result by adding the following subcommand:

```bash
-t
```

Or

```bash
--type_check
```

This process will verify that your input program is formally correct.

#### Optimize the input program

You can process a program optimization on the parsed result by adding the following subcommand:

```bash
-o
```

Or

```bash
--optimize
```

This process will try to eliminate any dead code from the input program.
It requires that the type-check option is used.

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

This process requires that the type-check option is used.

#### Generate the assembler code

You can generate the assembler code corresponding to the parsed program with the RnR backend by adding the following subcommand:

```bash
-c
```

Or

```bash
--code_gen
```

This process requires that the optimization option is used.

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
```

Or

```bash
--run
```

This process requires that the code generation option is used.

### Recommended commands

The most useful commands are therefore the two following ones:

- Reading a custom file, type checking it, optimizing it, and running it through the virtual machine: `rnr -i <path_to_my_file> -tov`
- Reading a custom file, type checking it, optimizing it, generating the ASM code and running it: `rnr -i <path_to_my_file> -tovcr`

## Some additional remarks on the project

### Parser

The RnR parser uses the **syn** crate. It implements a subset of it thanks to the generated **TokenStream**.

### CLI

The Cli uses the **clap** crate to parse the input commands.

### Backend

The backend is emulated in a virtual machine which copies a subset of the behavior of the MIPS 3k processor. This crate can be found [here](https://vesuvio-git.neteq.ltu.se/pln/mips).

> Note: The backend code does not support terminal printing. Therefore, `println!` calls can only be verified by evaluating the parsed AST in the RnR virtual machine (`-v`).
