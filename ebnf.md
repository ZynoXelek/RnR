# RnR EBNF Grammar

Here is the EBNF grammar of the whole RnR language.

It does not take into account the **semantical validity** of a program=, since it does not describe how types are verified. However, it explains how to write a **syntactically valid** program.

## Legend

I have used the following syntax (from the [wikipedia page](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form#Table_of_symbols)):

| Usage   | Notation    |
| ---    | ---   |
| definition | = |
| concatenation | , |
| termination | ; |
| alternation | \| |
| optional (none or one) | [...] |
| repetition (none or more) | {...} |

## EBNF

```EBNF
Lpar = "(";
Rpar = ")";

(* --- Integers --- *)

Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
PositiveInt = Digit, {Digit};

UnIntOp = "-";
FinalInt = [UnIntOp], PositiveInt;

BinIntOp = "+" | "-" | "*" | "/";

(* Uses recursion, inspired by Adrian's work when I reviewed it *)
IntExpr = FinalInt
        | [UnIntOp], Lpar, IntExpr, Rpar
        | IntExpr, BinIntOp, IntExpr;

(* --- Strings --- *)

letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" ;
symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
       | "'" | '"' | "=" | "|" | "." | "," | ";" | "-" 
       | "+" | "*" | "?" | "\n" | "\t" | "\r" | "\f" | "\b" ;
character = letter | digit | symbol | "_" | " " ;

String = "'", {character}, "'";

BinStringOp = "+";

StringExpr = String | StringExpr, BinStringOp, StringExpr;

(* --- Booleans --- *)

Bool = "true" | "false";

UnBoolOp = "!";
FinalBool = [UnBoolOp], Bool;

GenericCompOp = "==" | "!=";
OrderCompOp = ">" | "<" | ">=" | "=>";
BinBoolOp = "&&" | "||";

GenericOperand = FinalInt | String | FinalBool | array;

(* Usual boolean expression with '&&' and '||' operators *)
BoolExpr = FinalBool
         | [UnBoolOp], Lpar, BoolExpr, Rpar
         | BoolExpr, BinBoolOp, BoolExpr;

(* Make sure comparisons are not following without parenthesis *)
BoolCompExpr = GenericOperand, GenericCompOp, GenericOperand
             | IntExpr, OrderCompOp, IntExpr
             | StringExpr, OrderCompOp, StringExpr
             | Lpar, BoolCompExpr, Rpar, GenericCompOp, BoolExpr
             | BoolExpr, GenericCompOp, Lpar, BoolCompExpr, Rpar
             | Lpar, BoolCompExpr, Rpar, BoolCompExpr, Lpar, BoolCompExpr, Rpar;

(* Finally combine everything *)
BoolFinalExpr = BoolExpr
              | BoolCompExpr;
              | [UnBoolOp], Lpar, BoolFinalExpr, Rpar
              | BoolFinalExpr, BinBoolOp, BoolFinalExpr;

(* --- Arrays --- *)

BasicType = "i32" | "bool" | "String";
Type = BasicType | "[", Type, ";", PositiveInt "]";

content = BoolFinalExpr, {",", BoolFinalExpr}, [","]
        | String, {",", String}, [","]
        | IntExpr, {",", IntExpr}, [","]
        | array, {",", array}, [","];  (* Seems quite difficult to express the fact that inner arrays should be of the exact same type *)

array = "[", [content], "]";

(* How to properly express the fact that we can get an int/bool/String/array expression from accessing the array of the correct type, but also at a correct index? *)
GenericOperand = array, "[", FinalInt, "]";



(* --- Expressions --- *)

ident_begin = letter | "_";
ident_char = ident_begin | digit;
Identifier = ident_begin, {ident_char};

Block = "{", [{Statement, ";"}, Statement, [;]] "}";

IfThenElse = "if ", BoolFinalExpr, Block, [{"else if", BoolFinalExpr, Block}, ["else", Block]];

(* How to properly express the fact that we can get an int/bool/String/array expression from a function call, with correct parameters, but also with the correct return type? *)
Call = Identifier, "(", [{content, ","}, content, [","]], ")";

Expr = GenericOperand
     | Identifier
     | Call
     | IfThenElse
     | Block
     | "(", Expr, ")";



(* --- Statements --- *)

Let = "let ", ["mut "], identifier, [":", Type], ["=", Expr];

assign_left = Ident | assign_left, "[", FinalInt, "]";
Assign = assign_left, "=", Expr;

While = "while ", BoolFinalExpr, Block;

parameter = identifier, ":", Type;
parameters = [{parameter, ","}, parameter, [","]];
Fn = "fn ", identifier, "(", parameters, ")", [" -> ", Type];

Statement = Expr
          | Let
          | Assign
          | While
          | Fn;



(* --- Program --- *)

Main = "fn main() ", ["-> ()"], Block;
Program = {Fn}, Main;

```
