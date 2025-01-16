# Learning outcomes of the course

Here are some thoughts on what I have learned during the course, and how it went.

## Lab 1 - Introduction

Lab 1 was really useful to set up our Rust installation and get our hands on the language. I have been able to begin understanding the ownership system and do some tests on my own. I have also learned the principle of macros and how they work, but also the use of traits and implementations.

This lab was really important for the rest of the project. Obviously, I did not become an expert in the Rust language with this lab only (and I am still not one) but it introduced everything so that I could get used to them progressively when developing my RnR version.

## Lab 2 - AST

Lab 2 explained the basics of how one can process an input program and generate an Abstract Syntax Tree to describe it. This was an introduction to parsing simple expressions, but also to the principle of precedence and how to correctly generate the AST in cases of binary operations with different priorities.

At the end of this lab, I had learned how to process string representation of expressions as a `TokenStream` by using the `proc_macro` crate. It was also the opportunity to test regex expressions in Rust and recall the specific concepts of regular expressions. It was also the first introduction to the `EBNF` language and the first iteration of it, for simple expressions only.

## Lab 3 - Parsing a full program, and evaluating in a VM

In this lab, I have written the parser for RnR so that a full program can be described with a complex and detailed AST.

Moreover, I have implemented a virtual machine able to evaluate a given expression, block, or program. This was the opportunity to recall how scopes work, and how to correctly define variables and functions in the VM so that a program is correctly evaluated (as it is in the original Rust language). This way, I have been able to develop the first iteration of the State representation of my program.

This was also the last time I began from scratch for the RnR project. After this lab, every next addition was built on top of the previous version.

## Lab 4 - Type checking an AST

Lab 4 has introduced the semantic analysis of an input program, and how one can verify it is fully correct.

I have then implemented a full type checker, able to type check any expression, block, or input program with respect to the original Rust language (restricted to our small subset of it). It also checks for mutability when a variable is assigned a new value, and it checks for never initialized variables in the code.

I have started implementing arrays at this point, but they were not correctly implemented for the next steps of the RnR project and I re-implemented them at the end.

## Lab 5 - Code generation

This was the lab where I had the most struggle, but it was really interesting. I learned how to convert an AST to processor instructions (ASM code), how one can define scopes using the frame layout, define functions, and process any of the basic expressions and statements. It uses the `Mips` crate.

Later, I have modified the model of frame pointer I am using, from the original one inspired by the Mips 3k architecture with 3 dedicated slots on each scope for function call, to a frame layout without these dedicated slots and where function arguments are directly pushed as locals on an intermediate stack. This way, I can pass any number of arguments to a function.

Initial frame layout:

```rust
// ... Previous scopes ...
// 16[fp]    arg 1
// 12[fp]    arg 2
//  8[fp]    arg 3
//  4[fp]    ra
//  0[fp]    old_fp
// -4[fp]    local 1
// -8[fp]    local 2, etc.
```

New frame layout:

```rust
// ... Previous scopes ...
//  4[fp]    ra
//  0[fp]    old_fp
// -4[fp]    local 1 (may be a function argument)
// -8[fp]    local 2 (may be a function argument), etc.
```

I decided to not use any labels, but rather compute the offset myself. It may have been one of the reasons I had so much struggle with this lab, but it was the opportunity to fully understand everything in my implementation.

From the start, recursive functions and mutually recursive functions were correctly handled as far as I have tested it. I eventually implemented array processing in the backend code generation, after a lot of modifications in it so that it keeps track of every temporary value currently on the stack, and their sizes.

## Lab 6 - Command line interface

This last lab was about putting all of it together and building a command-line interface to be able to use the RnR compiler to its full extent.

The CLI I have developed is quite simple and straightforward. It has a set of options, which for most of them requires the previous option to be selected as well to be processed. For instance, I cannot evaluate the program if I did not select the type checking option. It uses the `clap` crate.

Additionally, I have implemented the possibility to parse and read an already written ASM file. Then, we can run it through the Mips virtual machine. However, there are no checks done on the validity of the read ASM program.

## Additional work

When I was working on implementing arrays in the backend code generation, at some point I thought that implementing some code optimization would solve my problems when I did not know how to process arrays (since they can be of any size but we only have a limited number of registers to store data out of the stack - this was before I realized I could just let the arrays' values on the stack and move them one after the other). This was wrong, but it is why I decided to start working on an AST optimizer while I had not planned it.

I had some struggle with it as I wanted it to be clean and remove any dead code from the input program as well. However, it does not remove used variables to replace them with their values when it can be evaluated, but I may eventually add some additional optimization protocols which process more aggressive optimizations. I would then add a new CLI option to choose the degree of optimization we would like to apply to our input program.

It did not solve my arrays issue (stack movement did!), but I'm glad I worked on this additional step as it was really interesting. It allowed me to think of several edge cases and try to develop an optimizer which would always work properly.

## Conclusion

As a conclusion, I think I have indeed gained a lot of knowledge on various computer science-related subjects. It has been the opportunity for me to learn some Rust code, which I really enjoyed using, but also to practice test-oriented programming and try to make my code as clean and explained as I could (there may be some additional work to do at some places though, but I think I may keep on working on it to progressively make it better and better). I have also learned a lot of things about compilers because I only knew the basics, and I had never taken the time to try it out myself, except for parsers.

I really enjoyed this course, and I'm really glad I could take it because I had not the opportunity to take this kind of course back in my home university.
