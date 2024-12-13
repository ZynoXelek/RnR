# Changelog for RNR

YOUR CHANGES/ADDED FEATURES HERE

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

## TODO

- Implement functions for the backend
- Implement references
- Write type rules
- Add support for CONSTANT expression in array size, and any non unit type expression in the init value during array definition.
- May want to add usize type for indexing (would be useful for ranges as well). May need to add "as" keyword then to convert i32 to usize.
- Add return statement to be able to return from a while.
- Similarly, add break statement.
- Add for loops. (Need to add ranges then, which could benefit from the usize type)
- Improve custom errors?
- Implement custom struct?
- Optimize some evaluations?
- Improve arrays in EBNF?
- Write SOS file
