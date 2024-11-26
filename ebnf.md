# EBNF Grammar

Your EBNF grammar goes here.

```EBNF
Lpar = "(";
Rpar = ")";

(* --- Integers --- *)

Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
Int = Digit, {Digit};

UnIntOp = "-";
FinalInt = [UnIntOp], Int;

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

GenericOperand = FinalInt | String | FinalBool

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

content = BoolFinalExpr, {"," BoolFinalExpr}, [","]
        | String, {"," String}, [","]
        | IntExpr, {"," IntExpr}, [","]
        | array, {"," array}, [","];  (* Seems quite difficult to express the fact that inner arrays should be of the exact same type *)

array = "[", [content], "]";

(* How to express the fact that we can get an int/bool/String expression from accessing the array of the correct type, but also at a correct index? *)

```

While using the following syntax (from the [wikipedia page](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form#Table_of_symbols)):
| Usage   | Notation    |
| ---    | ---   |
| definition | = |
| concatenation | , |
| termination | ; |
| alternation | \| |
| optional (none or one) | [...] |
| repetition (none or more) | {...} |
