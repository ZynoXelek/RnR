# Changelog for RNR

## 2024-11-19

- Complete restructure to reduce re-implementation work. See README.md for up to date information.
- Add and adapt initial work from lab2 to get the basics of the AST and the Parser

## 2024-11-20

- Implemented base display methods for each AST type. No clean indentation yet.
- Finished implementing the parsing methods for every base AST type. (No arrays supported, no references either)
- Finished implementing the virtual machine and related methods for the base AST type. (However intrinsics not supported yet)

## 2024-11-22

- Made functions callable before definition
- Improved pretty printing to support correct indentation

## 2024-11-23

- Add short-circuiting for boolean operations
- Add intrinsics support. (For now, only println! To add more, need to add them explicitly in add_intrinsics() function)

## 2024-11-24

- WIP on arrays. Can now be declared (including using syntax `[len; init]`), modified and used. However, length is not used nor verified yet

## 2024-11-25

- Add custom errors for handling parser issues
- Add custom errors for handling evaluation issues
  
## 2024-11-26

- Add size data to arrays. For now, it is still using `Vec<Literal>` for convenience, but may be changed later?
- Add arrays to EBNF (May need to improve it later)

## 2024-11-27

- Fix array special definition, which was in the wrong order. Correct order is `[init; len]`. Need to add support for expr later on.

## 2024-11-29

- Implement Type VM for most cases, from basic `Expr` to entire `Prog`. It has array support in a similar way as the eval VM. Still need to add more tests and support nicely intrinsics.

## 2024-12-02

- Add support for `println!()` type checking. Counts the number of required arguments based on the number of `{}` or `{:?}` found in the string.

## 2024-12-06

- Fixed block parsing after I realized they were not correctly parsed as expressions when they did not finish by a semi-colon.
- Made it so that we can declare an uninitialized variable, without even explicitly declaring its type.
- Fixed if statements so that if a variable is initialized in a peculiar branch, it should be in the other one as well, with the same type.

## 2024-12-09

- Begin work on backend part. Now literals, binop, unop and variables definition are implemented. WIP.
- There is an issue with the way we define variables thanks to a block. Need to look into it.
- Fixed the issue.

## 2024-12-11

- Continue work on backend with assignment, if and while loops. Still WIP as there is an issue with inner loops and var modifications for if and while loops. Need to look into it.

## 2024-12-13

- Implement multiplication and division BinOp.
- Implement definition of uninitialized variables.
- Fix if and while issues.

## 2024-12-15

- Finished implementing functions and programs. Recursive functions can also be used.

## 2024-12-16

- Add first version of a CLI. It supports every required commands (`-h`, `-i`, `-a`, `-t`, `--vm`, `-c`, `--asm`, `-r`). However, it looks like the backend run does not work properly while the `test_backend.rs` succeeds on the same example.
- Lacks better help method (and help options for each option?)

## 2024-12-17

- Write `README.md` for the **RnR** project.
- Write a first iteration of the `type_rules.md`.
- Fix backend run issue by adding an `Halt` instruction at the end of the main call of a program. Issue was that the VM tried to read another instruction while there were no instructions left.

## 2025-01-06

- Implement ASM parsing and the possibility to load an ASM program and run it through `--asm_input <path>`.

## 2025-01-06 -> 2025-01-12

- Implement full AST optimization to remove useless parts of a given code. This first optimization stage does not replace identifiers with their values, but it removes any useless variable or function definition, and all their assignments. It also gets rid of useless operations on literals, useless blocks. It applies short-circuiting in some operations, and reduces it when it can.
- Modify AST display to look nicer in case of inner blocks, and add support to display if-then-else_if.

## 2025-01-13

- Redesign arrays in the whole RnR so that it accepts expressions in initialization, and any depth in array.

## 2025-01-14

- Redesign backend function calls so that it can accept any number of arguments.

## 2025-01-XX

- Redesign backend variables so that it takes variable size into account, and it reduces the amount of pushes and pops, especially in function returns and expressions evaluation.
- Implement arrays in the backend.

## TODO

**Mandatory:**

- CLI improve help and add one for each command
- Improve custom errors
- Improve arrays in EBNF
- Write SOS file
- Write type rules
- Write REFLECTION.md (including some additional feedback on how it went, when it was hard and everything... + What we have learnt)
- UPDATE README

**Optional:**

Primary:

- AST optimizations to have a simpler backend
- Backend optimizations
- Backend support for functions with any number of arguments (refactor the frame stack to remove arg1, arg2, arg3 and use the intermediate scopes as I already do)
- Implement arrays in backend
- Rework Arrays and especially array assignation because it only supports a single assignation for now -> Do not support `a[1][2] = 3;` for instance.
- Implement references everywhere
- Implement ownership?
- Implement custom struct?
- Add support for CONSTANT expression in array size, and any non unit type expression in the init value during array definition. It should support any valid type expression.

Secondary:

- May want to add usize type for indexing (would be useful for ranges as well). May need to add "as" keyword then to convert i32 to usize.
- Add return statement to be able to return from a while.
- Similarly, add break statement.
- Add for loops. (Need to add ranges then, which could benefit from the usize type)
- Optimize some evaluations?
