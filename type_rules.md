# Type rules for your Rust in Rust language

Optional, assignment, see slides Lecture 6 for further details.

Hint: Use inline latex math for GitLab.

## Types

Here are the basic typing rules for expressions

<!-- TODO: Add references -->

<!-- setting the language to ebnf make some strange String recognition -->
```text
(* Defined Types *)

Integer;
Bool;
String;
Unit;
Array(Type);

(* Unary Operators *)

!Bool = Bool;
-Integer = Integer;

(* Binary Operators *)

Integer, ("+" | "-" | "*" | "/"), Integer = Integer;
Bool, ("&&" | "||"), Bool = Bool;
Type, ("!=" | ==), Type = Bool;
Integer, ("<=" | "<" | ">=" | ">"), Integer = Bool;
String, ("<=" | "<" | ">=" | ">"), String = Bool;
Array(Type), "[", Integer, "]" = Type;

(* Infer the type of an expression *)
Integer = Digit, (Digit);
Bool = "true" | "false";
String = "'", {character}, "'";
Array(Type) = ("[", {Type, ","}, "]") | ("[" Type, ";", Integer, "]");
```

<!-- TODO: Complete -->
