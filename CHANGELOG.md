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

## 2024-11-25

- Add custom errors for handling parser issues
- Add custom errors for handling evaluation issues
  
## 2024-11-26

- Add size data to arrays. For now, it is still using `Vec<Literal>` for convenience, but may be changed later?
- Add arrays to EBNF (May need to improve it later)

## 2024-11-27

- Fix array special definition, which was in the wrong order. Correct order is `[init; len]`. Need to add support for expr later on.

## 2024-11-29

- Implement Type VM for most cases, from basic `Expr` to entire `Prog`. It has array support in a similar way as the eval VM. Still need to add more tests and support nicely intrinsics.

## 2024-12-02

- Add support for `println!()` type checking. Counts the number of required arguments based on the number of `{}` or `{:?}` found in the string.

## TODO

- Implement references
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
