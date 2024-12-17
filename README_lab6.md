# d7020_lab6

In this lab you will put all pieces together and implement a simple command line interface (cli) for your RnR compiler.

## Expected learning outcomes

- Rudimentary shell/terminal interaction

- Command line parsing (yes, yet another parser - compiler technology is everywhere)

- Optional, for higher grades

  - Improved error messages from your type checker, and/or other functions that may give rise to errors. You may look into [thiserror](https://crates.io/crates/thiserror), and [anyhow](https://crates.io/crates/anyhow), the former provides convenient ways to define error types, the latter streamlines handling of errors (context, conversion, chaining etc.)

  - Distinguish between warnings and hard errors.

  - Error recovery and the ability to produce multiple warnings/errors.

  - Use of Rust logging framework, for configurable tracing.

  - Colorized output for errors, results, etc. You can use e.g., [colored](https://crates.io/crates/colored) to spice up your compiler.

  - Detailed control over various features of your compiler, exposed through the `cli`.

  - Reading `asm` files, and running them using the `mips` crate `vm`.

## Command line parsing

The usability of tools (UX) largely depends on the interaction and feedback given, including `--help` displaying various arguments and their use.

In addition, sensible error messages hinting the user towards potential solutions, greatly improves the UX.

The Rust compiler is by itself designed ground up to provide best possible UX and can be seen as a blueprint for well designed user interfaces. To your help there are many crates helping you design your user interface. The most popular is [clap](https://crates.io/crates/clap), so I recommend you have a look at that.

Our RnR language does not come with any module system, thus we are concerned with compiling just a single file.

As a bare minimum your compiler should provide basic functionality to compile and run programs, e.g. as below:

```shell
rnr -h/--help # will show available arguments and their use
rnr # will parse `main.rs` in current folder.
rnr -i/--input <path> # will parse the file at `<path>` relative to the the current folder.
```

Each larger feature should be individually selectable:

- `-a/--ast <path>`, for dumping the parsed AST to a file at `<path>` relative to current folder.
- `-t/--type_check`, for running the type checker.
- `-vm/virtual_machine`, for running the `vm`.
- `-c/--code_gen`, for running the code generation.
- `-asm <path>`, for dumping the generated code to a file at `<path>` relative to current folder.
- `-r`, for running generated code using the `mips` crate `vm`.

You can think of various extensions, e.g., allowing the compiler to read (and run) an `asm` file (as suggested above). This will involve writing a simple parser for the textual assembly language. (To this end a cleverly designed `regexp` should likely work out, as the `asm` language is very simple). You are now a seasoned programmer, able to fearlessly approach such challenges :)

## Submission

For the `lab6` submission make sure your crate has an updated `README.md` (yes, you can now re-write the boring lab3 readme that you started out with, by a README describing the whole project, including the `cli`).

Update your `CHANGELOG.md`, in case you have collaborated with other students, make sure the `CHANGELOG.md` also contains author information for individual contributions. This is especially important if you aim for higher grades. (Collaboration is encouraged, however, be fair and give credit where credit is due.)

Also make sure your `ebnf.md` is up to date with the syntax of your RnR (you may assume that we already have tokens, thus no rules are needed for forming literals, strings, etc., keep it simple).

An extra plus for `sos.md` and `type_rules.md`, and besides making your repo look important they give a formalization for reasoning on the RnR language and the correctness of your compiler.

Finally add a file `REFLECTION.md`. Look through the learning objectives for each lab and make short comment how you used the lab to obtain knowledge regarding each learning outcome. (You may cut n' paste earlier reflections, add or remove to your liking.)

You may also include personal highs and lows, those aha moments, as well as o-shit moments you had to plow through to get to your destination.

Finally ask yourself, did you learn anything new in this course?

Formulate in your own words, gained knowledge (might not be strictly compiler tech related, but knowledge that you gained through the work.)

Thanks all for all your hard work, and I hope to see you around for the higher grade presentation in January (date to be determined).
