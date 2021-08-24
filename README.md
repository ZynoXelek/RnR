# d7050e_lab4

In `d7050e_lab3` we extended the language with expression blocks allowing sequences of block expressions (or commands). In this lab we will a simple type checker.

## Learning outcomes

The basics of type environments and type checking.

- `examples/ex_1_backport.rs`, back port your parser, such that you can parse while loops. If you have not yet done the optional assignments (nested if:s and unary expressions, you may do them now, or return to them later).
  
- `examples/ex_2_type_check.rs`, implement the type checker. There is a skeleton in `src/type_check.rs`. Make sure all relevant tests passes (not only the one in the example.)

- `examples/ex_3_type_rules.md`, here you formalize the type checker in terms of type rules.

- `examples/ex_4_type_inference.rs`, you can optionally extend the syntax and type checker allowing type inference of variables.

- `examples/ex_5_mut.rs`, you can optionally extend the syntax and type checker with mutability check.

- `examples/ex_6_ebnf.md`, put the EBNF for your current syntax here. (Even if you have not extended syntax, you will get another review of your EBNF.)

---

A well typed program in our Rust in Rust language. (Notice, we don't care about mutability yet.)

```rust
let c: i32 = 4 - 5;
let b: bool = false || true; 
b = b || true; 
c + c;
```

An illegally typed program:

```rust
let c: i32 = 4 - 5;
let b: bool = false || true; 
b = b + 1; 
c + false;
```

## Type checking

A type environment $`E: Id \rightarrow Type`$ holds bindings from identifiers to types. For the above example, we will start with an empty environment $`E`$. Type checking of an expression block amounts to iterating over the statements (starting from the first), and for each new let binding add it's type to the environment, and for assignments we check that the left and right hand expressions have the same type.

For the first statement `let c: i32 = 4 - 5;` we derive the type of the right hand expression (this is done by recursively traversing the expression).  So for this case we will check and evaluate the type of $`4 - 5`$. The operator $`-`$ expects left and right hand expressions of type $`i32`$ and evaluates to the type $`i32`$. In our simple langue we find integer, Boolean (and unit literals) at the leafs.

We then check that the right hand side is of the expected type ($`i32`$) add we add the binding $`c \rightarrow i32`$ to $`E`$. (Similarly the $`||`$ operator expects left and right hand expressions of type $`Bool`$ and evaluates to the type $`Bool`$, etc.).

The second statement `let b: bool = false || true;` is checked as above and we add $`b \rightarrow Bool`$ to $`E`$ (which now holds two bindings).

In the example first, we continue with the `b = b || 1;` and `c + c` block expressions and find them to be well typed. The expression block is not terminated by `;` and we can conclude the type of the block to be $`i32`$ (the type of the last block expression).

In the second example, we will encounter type errors, as the $`+`$ operator is not defined for Boolean types.

## Implementation

The `src/type_check.rs` contains a skeleton for your type check implementation. The function `unify` is used to check compatibility. You will run into the problem of overloading. For now you can leave `Eq` to work only on `I32` but later you may want to allow `Eq` also on `Bool`. (Overloading will affect both `unify` and `op_type`.)

The type checking functions are using the `Result<Type, TypeErr>`, allowing errors to be propagated using the `?` operator. The `TypeEnv` is implemented as an alias of `HashMap<String, Type>`, and holds the mapping from identifiers to types.

There are explicit attributes to suppress warnings, once you implement the corresponding code the attributes should be removed.
