# d7050e_lab4

In `d7050e_lab3` we extended the language with expression blocks allowing sequences of block expressions (commands) together with a VM (providing a natural interpretation). 

In this lab you will continue working on your `d7050e_lab3` implement and extend it with a simple type checker.

## Learning outcomes

- The role of semantic analysis in a compiler.

- The basics of type environments and type checking.

- Type inference and simple polymorphism (overloading).

- Mutability check.
  
- Optional for higher grades, includes but are not limited to:

  - Arrays.

  - User defined data structures (structs/enums).

  - Formalization of the type system in terms of `inference rules`.
  
---

## Basics of type checking.

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

Rust is a *statically* typed language where type checking is performed at compile time after initial parsing of the input program.

As you will see, type checking resembles the interpreter you implemented in Lab3, where evaluation resolves into types (instead of values).

### Statements

A type environment $`E: Id \rightarrow Type`$ holds bindings from identifiers to types. For the above example, we will start with an empty environment $`E`$. Type checking of an expression block amounts to iterating over the statements (starting from the first), and for each new let binding add it's type to the environment, and for assignments we check that the left and right hand expressions have the same type.

For the first statement `let c: i32 = 4 - 5;` we derive the type of the right hand expression (this is done by recursively traversing the expression).  So for this case we will check and evaluate the type of $`4 - 5`$. The operator $`-`$ expects left and right hand expressions of type $`i32`$ and evaluates to the type $`i32`$. In our simple language we find integer, Boolean (and unit literals) at the leafs.

We then check that the right hand side is of the expected type ($`i32`$) add we add the binding $`c \rightarrow i32`$ to $`E`$. (Similarly the $`||`$ operator expects left and right hand expressions of type $`Bool`$ and evaluates to the type $`Bool`$, etc.).

The second statement `let b: bool = false || true;` is checked as above and we add $`b \rightarrow Bool`$ to $`E`$ (which now holds two bindings).

In the example first, we continue with the `b = b || 1;` and `c + c` block expressions and find them to be well typed. The expression block is not terminated by `;` and we can conclude the type of the block to be $`i32`$ (the type of the last block expression).

In the second example, we will encounter type errors, as the $`+`$ operator is not defined for Boolean types.

### Similarities to interpretation

Similarly to the VM, the type environment can be treated as a stack of scopes, where new entries are added to the top of stack, and lookup is performed starting from the top.

### Differences to interpretation

Type checking starts from the program definition, type checking each top level function. Type checking a function amounts to checking each inner function declaration and the body once (while the interpreter would follow a trace of execution). 

You may chose to either implement a separate environment for functions, or extend the environment to hold $`E: Id \rightarrow FunDecl`$. Functions may not be shadowed in the same scope. You should type check each function when its introduced (added to the environment). The body expression block should be type checked as described above, and its return type checked against the function declaration.

When type checking a function call expression, you need to match each given argument given (expression), against the declared parameter type. The type of function call expression is given by the function declaration.

---

## Implementation and workflow

You can choose either to continue working in your Lab3 repository or create a new upstream "d7050e_lab4" (up to you).

Create a new module `type_check.rs` and add that to your library.

Take inspiration from your VM, and start by implementing type checking of literal expressions. 

Hint, you may introduce a function `unify`:

```rust
fn unify(got: Type, expected: Type) -> Result<Type, Errol> {
    match got == expected {
        true => Ok(expected),
        false => Err(format!("expected type {:?}, got type {:?}", expected, got)),
    }
}
```

Create you own test, (as you will support the same language as the interpreter, you may re-use the VM tests by changing them into type checking tests). Add additional fail tests to cover errors of interests.

Build from there, to support more complex expressions and blocks, until you cover your RnR language.

Use the real Rust language as a reference. Look at the error(s) (rust-analyzer) would produce and try to follow the Rust language for the supported subset. For your type checker it is sufficient to report one error at the time.

All in all, you will learn type checking by experience. You will run into problems and address them along the way. Eventually you might find that a rewrite is favorable, writing complex code cannot be expected to be perfect the first time around. 

### Additional hints:

As RnR does not implement traits, you can mimic this by *overloading* behavior for operators (e.g., comparisons) where applicable.

For type checking the `println!` intrinsic you can see it as a function with `String` and `i32` parameters and `()` return type.

For the type checking tests, you can add these helpers to your `test_util`.

```rust
/// Assert that type checking fails.
pub fn assert_type_fail<T>(p: &T)
where T: Eval<Type>
{
    let typ = p.eval();
    assert!(typ.is_err());
}

/// Alias for `assert_type_fail` to avoid specifying the generic type parameter.
pub fn assert_block_type_fail(p: &Block) {
    assert_type_fail(p);
}

/// Assert that type checking results in the expected type.
/// This implicitly means that type checking succeeds.
pub fn assert_type<T>(p: &T, expected: Type)
where T: Eval<Type>
{
    let typ = p.eval();
    println!("type checking result: {:?}", typ);
    assert!(typ.is_ok());
    let typ = typ.unwrap();
    assert_eq!(typ, expected);
}
```
