# d7050e_lab3

In this lab you will implement the parser for "Rust in Rust" (RNR), a minimal subset of the Rust language.

You will also implement a Virtual Machine (VM) able to interpret RNR programs.

## Learning outcomes

- Abstract Syntax Tree (AST) to represent RNR.
- Parsing from Rust to AST.
- Variable environment representing state.
- Natural interpretation.

---

## Workflow

Start by back-porting your code from previous labs. It is encouraged to follow the AST given, and parse according to Rust syntax (then the given tests and examples will guide you towards a working implementation). If stepping away from Rust syntax, you need to clearly motivate your choices and provide tests accordingly.

Keep your `EBNF.md` , `sos.md`, and `CHANGELOG.md` up to date with the status of your development.

---

## Crate structure

The files are structures as follows:

Data structures:

- `ast.rs`, the internal representation of the parse tree, also used for semantic analysis and natural interpretation. (Notice, a realistic compiler typically use a large number of internal representations, `AST` -> `HIR` -> `MIR` -> ..., but we keep it simple here.)

- `ast_traits.rs`, functionality to display the AST in readable form.

- `parse.rs`, the parser.

API:s:

- `error.rs`, the definition of the error type.

- `common.rs`, common API for processing the AST.

Later we will add:

- `env.rs`, a generic stacked environment for interpretation and semantic analysis.

Interpretation:

- `vm.rs`, an AST level interpreter for the _natural_ semantics.

Documentation:

- `README.md`, this file.

- `ebnf.md`, the EBNF grammar for RNR.

- `sos.md`, formalization of semantics for RNR.

- `CHANGELOG.md`, tracking of project status.

---

## Workflow

- Start by implementing the `Display` trait for the AST data structures. Make it simple to start out with, later you can return to this and improve indentation to match the Rust syntax. To that end, you can look into the [pretty](https://crates.io/crates/pretty) crate for inspiration on indenting.

- Extend parsing and VM in tandem to step by step cover the RnR subset of Rust. Eventually your parser and VM should cover the complete AST (up to the level of programs). In this way you will see the capabilities of your compiler to

- At this point you may optionally add:

  - Support for arrays (including declarations, indexing and printing).
  - Syntactic sugar for chained if then else.
  - Shortcut evaluation.
  - Structural Operational Semantics (formalization of RnR), it is pretty cool and will make you look important. More on that later.

- You will in Lab4 introduce a type-checker based on the Lab3 code base (so no re-implementation/backporting should be required).

- In Lab5 you'll implement code generation for either the RISC-V or MIPS, both are similar RISC based 32-bit architectures with simple instruct sets.

---

## Some remarks

When you complete the mandatory parts (with corresponding tests passed and documentation updated) you will also pass the course.

For higher grades, document the set of added features in the `CHANGELOG.md`.

## License

Let knowledge be free! Free to use for any purpose.

## Reflection

- Reflect in your own words how you used the lab to obtain knowledge regarding above stated learning outcomes.
- [Your reflections here]

- Reflect in your own words how you used the lab to gain further knowledge in topics of your own interest.

[Your reflections here]
