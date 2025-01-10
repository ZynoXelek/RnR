use crate::{
    ast::{
        Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Mutable, Parameter, Parameters,
        Prog, Statement, Type, UnOp,
    },
    error::*,
};

use syn::{
    parse::{Parse, ParseStream},
    Error as SynError, Result, Token,
};

use proc_macro2::TokenStream;

/// A small helper function for parsing source strings.
pub fn parse<T: Parse>(src: &str) -> T {
    try_parse::<T>(src).unwrap()
}

/// A small helper function for parsing source strings.
pub fn try_parse<T: Parse>(src: &str) -> Result<T> {
    let ts: proc_macro2::TokenStream = src.parse()?;
    syn::parse2::<T>(ts)
}

/// Might be useful if you are struggling with parsing something and you want to see what the
/// tokens are that syn produces.
pub fn try_parse_debug<T: Parse + std::fmt::Debug>(src: &str) -> Result<T> {
    println!("parsing source string:\n{}", src);
    let ts: proc_macro2::TokenStream = src.parse()?;
    println!("tokens:\n{}", ts);
    let result = syn::parse2::<T>(ts);
    println!("parsed AST:\n{:?}", result);
    result
}

// Back-port your parser
// You may want to put the tests in a module.
// See e.g., the vm.rs

//?#################################################################################################
//?#                                                                                               #
//?#                                         Literal Type                                          #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self> {
        // First check if the literal is of unit type '()'.
        // The TokenStream may either be empty in this case, or empty parentheses.
        let input_str = input.to_string();

        if input.is_empty() {
            return Ok(Literal::Unit);
        } else if input.peek(syn::token::Paren) {
            let content;
            let _ = syn::parenthesized!(content in input);

            if content.is_empty() {
                return Ok(Literal::Unit);
            }
        }

        // Support array literals, such as [1, 2, 3] or [1, 2, 3,] but also [3; 1] -> array of 3 elements filled with 1
        if input.peek(syn::token::Bracket) {
            let content;
            let _ = syn::bracketed!(content in input);

            // Now we have the content inside the brackets. ex: 1, 2, 3 or 1, 2, 3,
            // We can parse this content as a list of literals separated by commas
            let mut literals: Vec<Literal> = Vec::new();
            let mut size: usize = 0;

            // Check for the special syntax
            let str_repr = content.to_string();
            let r = regex::Regex::new(r"^.+;.+$").unwrap();

            if let Some(caps) = r.captures(&str_repr) {
                // This is the special syntax [init; size]
                
                // Parse the init value
                // TODO: allow for expressions in init value
                let init: Literal = content.parse()?;

                // Consume the semicolon
                let _: Token![;] = content.parse()?;

                // Parse the size
                // TODO: allow for CONSTANT expressions in size
                let size_lit: Literal = content.parse()?;
                let size: usize = size_lit.get_int() as usize;

                return Ok(Literal::Array(vec![init; size], size))
            } else {
                //TODO: Support expressions in array literals
                while !content.is_empty() {
                    let lit: Literal = content.parse()?;
                    literals.push(lit);
                    size += 1;

                    // If there is a comma, we consume it
                    if content.peek(Token![,]) {
                        let _: Token![,] = content.parse()?;
                    } else if !content.is_empty() {
                        // Wrong parsing, the array have several elements not separated by commas
                        let next_token = content.parse::<TokenStream>().unwrap().to_string();
                        return Err(ParsingError::expected_token(
                            ",".to_string(),
                            next_token,
                            input_str,
                            ParsingContext::Literal,
                        )
                        .into());
                    }
                }
            }

            return Ok(Literal::Array(literals, size));
        }

        // Use the "built in" syn parser for classic literals
        let lit: syn::Lit = input.parse()?;

        Ok(lit.into())
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          BinOp Type                                           #
//?#                                                                                               #
//?#################################################################################################

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        let input_str = input.to_string();

        // Integer operations
        if input.peek(Token![+]) {
            // consume the token
            let _: Token![+] = input.parse()?;
            Ok(BinOp::Add)
        } else if input.peek(Token![-]) {
            let _: Token![-] = input.parse()?;
            Ok(BinOp::Sub)
        } else if input.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            Ok(BinOp::Mul)
        } else if input.peek(Token![/]) {
            let _: Token![/] = input.parse()?;
            Ok(BinOp::Div)
        }
        // Boolean operations
        else if input.peek(Token![&&]) {
            let _: Token![&&] = input.parse()?;
            Ok(BinOp::And)
        } else if input.peek(Token![||]) {
            let _: Token![||] = input.parse()?;
            Ok(BinOp::Or)
        }
        // Comparison operations
        else if input.peek(Token![==]) {
            let _: Token![==] = input.parse()?;
            Ok(BinOp::Eq)
        } else if input.peek(Token![!=]) {
            let _: Token![!=] = input.parse()?;
            Ok(BinOp::Ne)
        } else if input.peek(Token![<=]) {
            // Must be put before < to avoid conflict
            let _: Token![<=] = input.parse()?;
            Ok(BinOp::Le)
        } else if input.peek(Token![>=]) {
            // Must be put before > to avoid conflict
            let _: Token![>=] = input.parse()?;
            Ok(BinOp::Ge)
        } else if input.peek(Token![<]) {
            let _: Token![<] = input.parse()?;
            Ok(BinOp::Lt)
        } else if input.peek(Token![>]) {
            let _: Token![>] = input.parse()?;
            Ok(BinOp::Gt)
        }
        // Array operations
        else if input.peek(syn::token::Bracket) {
            // This is an array access -> a[i]
            // Do not consume the token. Since it is a peculiar case, it will be handled by the binary expr parser
            Ok(BinOp::Get)
        } else if input.cursor().eof() {
            // No more tokens to parse
            Err(ParsingError::unexpected_eof(input_str, ParsingContext::BinOp).into())
        } else {
            // input.step(|cursor| Err(cursor.error("expected binary operator")))
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::invalid_token(next_token, input_str, ParsingContext::BinOp).into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           UnOp Type                                           #
//?#                                                                                               #
//?#################################################################################################

impl Parse for UnOp {
    fn parse(input: ParseStream) -> Result<Self> {
        let input_str = input.to_string();

        if input.peek(Token![!]) {
            let _: Token![!] = input.parse()?;
            Ok(UnOp::Bang)
        } else if input.peek(Token![-]) {
            let _: Token![-] = input.parse()?;
            Ok(UnOp::Neg)
        } else if input.cursor().eof() {
            // No more tokens to parse
            Err(ParsingError::unexpected_eof(input_str, ParsingContext::BinOp).into())
        } else {
            // input.step(|cursor| Err(cursor.error("expected binary operator")))
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::invalid_token(next_token, input_str, ParsingContext::BinOp).into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Expr Type                                           #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        // Expressions can be literals, identifiers, unary operations, binary operations, function calls, blocks, or if-then-else expressions.

        // Parse the first part of the expression.
        let left = parse_operand(input)?; //? Can parse literals, identifiers, parentheses, unary ops, function calls,
                                          //? blocks and if-then-else expressions
                                          // Now check if the rest is a BinOp Expr...
        if peek_binop(input) {
            // In that case, we have to parse the rest of the expression.
            parse_binary_op_expr(input, left, 0) //? Can parse binary ops
        } else {
            // Otherwise, the first part was the whole expression.
            Ok(left)
        }
    }
}

//? -------------------------------- Binary and Unary cases -------------------------------------------

// NOTE: About `peek_binop` and `peek_prio`:
// We need to be able to look ahead at the next operator, but without parsing an
// Op and thereby consuming its tokens. I have not found a good way to either do
// something like `input.peek(Op)` or `input.unread(op)` when using syn.
// So forking and parsing is the most concise solution I could find. /chrfin

/// Check if the next token is some (binary) operator.
fn peek_binop(input: ParseStream) -> bool {
    input.fork().parse::<BinOp>().is_ok()
}

/// Get the priority of the operator ahead. Assumes there is one!
fn peek_prio(input: ParseStream) -> u8 {
    input.fork().parse::<BinOp>().unwrap().priority()
}

/// Check if the next token is some (unary) operator.
fn peek_unop(input: ParseStream) -> bool {
    input.fork().parse::<UnOp>().is_ok()
}

fn parse_array_access(input: ParseStream) -> Result<(BinOp, Expr)> {
    // Should parse array access such as `a[i]`
    let input_str = input.to_string();

    // Check if the next token is a bracket
    if input.peek(syn::token::Bracket) {
        let content;
        let _ = syn::bracketed!(content in input);

        // Parse the index expression
        let index: Expr = content.parse()?;
        Ok((BinOp::Get, index))
    } else {
        let next_token = input.parse::<TokenStream>().unwrap().to_string();
        Err(ParsingError::expected_token(
            "[".to_string(),
            next_token,
            input_str,
            ParsingContext::Expr,
        )
        .into())
    }
}

/// Check if we've reached the end of a binary operator expression.
/// Depending on how expressions are parsed, an expression is usually terminated by the
/// input running out. But we may also run into some token that means the expression is done.
fn end_of_expr(input: ParseStream) -> bool {
    if input.is_empty() {
        true
    } else {
        // These may not be needed in practice (if we use something like
        // `syn::parenthesized!(...)` or `parse_terminated` for example).
        // But, in principle, we could for example reach the end of an array element or function
        // argument, etc. So let's be general.
        input.peek(Token![,])
            || input.peek(Token![;])
            || input.peek(syn::token::Brace)
            // || input.peek(syn::token::Bracket) // Because of array access, should not be considered as end of expr
            || input.peek(syn::token::Paren)
            || (input.peek(Token![=]) && !input.peek(Token![==])) // For assignments but not == comparisons
    }
}

/// Parse what could be an operand, i.e. the first part of a binary expression.
/// This could be a literal, an identifier, a unary op, an expression in parentheses, a block, or an if-statement.
/// For example: `3 + ...`, `x + ...`, `!true && ...`, `(1+2) + ...`, `[1,2,3][0] + ...`, '{...} + ...', or 'if ... {...} else {...} + ...'
fn parse_operand(input: ParseStream) -> Result<Expr> {
    let input_str = input.to_string();

    if peek_unop(input) {
        // This should parse unary operators such as `!true`, `-3`, etc.
        // However, array access reverses the order: ![true, false][0] -> !true
        let op: UnOp = input.parse()?;
        let operand = parse_operand(input)?;

        // To support binary operations with higher priority (For instance, array access)
        let final_operand = parse_binary_op_expr(input, operand, op.priority()).unwrap();

        // Some hotfix on unary operators to make it so that '-4' is parsed as
        // 'Expr::Lit(Literal::Int(-4))' instead of 'Expr::UnOp(UnOp::Neg, Expr::Lit(Literal::Int(4)))'

        if let UnOp::Neg = op {
            if let Expr::Lit(Literal::Int(i)) = final_operand {
                return Ok(Expr::Lit(Literal::Int(-i)));
            }
        }
        
        Ok(Expr::un_op(op, final_operand))
    } else if input.peek(syn::token::Paren) {
        // This should parse expressions in parentheses, such as `(1 + 2)`
        // Be aware that it will also match unit type `()`
        let content;
        let _ = syn::parenthesized!(content in input);
        if content.is_empty() {
            Ok(Expr::Lit(Literal::Unit))
        } else {
            let e: Expr = content.parse()?;
            Ok(Expr::Par(Box::new(e)))
        }
    } else if input.peek(Token![if]) {
        // This should parse if-then-else expressions such as `if true { 1 } else { 2 }`
        let if_then_else: IfThenOptElse = input.parse()?;
        Ok(Expr::from(if_then_else))
    } else if input.peek(syn::token::Brace) {
        // This should parse blocks such as `{ let a = 1; let b = 2; a + b }`
        let block: Block = input.parse()?;
        Ok(Expr::Block(block))
    } else if input.peek(syn::Ident) {
        // This should parse identifiers such as 'a', 'my_var', 'my_func(1, 2, 3)'
        parse_ident_or_call(input)
    } else if input.peek(syn::Lit) || input.peek(syn::token::Bracket) {
        // This should parse literals such as '1', 'true', '"hello"', '[1, 2,]', or even '()'
        let operand: Literal = input.parse()?;
        Ok(Expr::Lit(operand))
    } else if input.cursor().eof() {
        // No more tokens to parse
        Err(ParsingError::unexpected_eof(input_str, ParsingContext::Expr).into())
    } else {
        // input.step(|cursor| Err(cursor.error("expected operand")))
        let next_token = input.parse::<TokenStream>().unwrap().to_string();
        Err(ParsingError::invalid_token(next_token, input_str, ParsingContext::Operand).into())
    }
}

/// Parse an expression consisting of binary operators, such as `1 + 2`, `1 + 2 + 3`,
/// `1 + 2 * 3 + 4`, or `(1 + 2) * (2 + 3)`.
/// To be more specific: given some beginning part of an expression (in `left`), parse the
/// remainder of the expression, starting with the next operator. For example, `left` might be
/// `1` or `1 + 2`, and the input might be `+ 2` or `+ 2 + 3`, etc.
/// The priority (or precedence) of operators is taken into account during parsing
/// so we get the correct AST.
fn parse_binary_op_expr(input: ParseStream, left: Expr, min_prio: u8) -> Result<Expr> {
    let input_str = input.to_string();

    // Making sure there is anything to parse.
    if end_of_expr(input) {
        return Ok(left);
    }

    // Checks if there is indeed an operator to parse.
    if !peek_binop(input) {
        return Ok(left);
    }

    // Check the priority of the upcoming operator.
    // If it is lower than the minimum priority, we should return the current expression (left).
    let prio = peek_prio(input);
    if prio <= min_prio {
        return Ok(left);
    }

    // Parse the operator and the next right operand of the expression.
    let binop = input.parse::<BinOp>()?;
    let mut right: Expr;

    // Support for array access (a[i])
    if binop == BinOp::Get {
        // In this case, the parse for Get did not consume the bracket token.
        // We need to parse the index expression here.
        let (op, index) = parse_array_access(input)?;
        right = index;
    } else {
        right = parse_operand(input)?;
    }

    //TODO: Move this to type checker later (with custom error)
    // Checking that no unit type is used in binary operations
    if left.is_unit() || right.is_unit() {
        return Err(input.error("Unit type cannot be used in binary operations"));
    }

    // Checking comparisons to panic if they are directly following each other without parenthesis or anything.
    if binop.is_comparison() {
        // We only have to check the left side since the right side will be recursively checked.
        if left.is_comparison() {
            return Err(ParsingError::chained_comparisons(
                input_str,
                ParsingContext::BinaryOperation,
            )
            .into());
        }
    }

    // Looks for the next operator to know what to do afterwards.
    if end_of_expr(input) {
        // If we have reached the end of the expression, we are done parsing.
        return Ok(Expr::bin_op(binop, left, right));
    } else if peek_binop(input) {
        // The expression is not finished yet. ex: "+ 2 * 3 + ..."

        // Get next priority
        let next_prio = peek_prio(input);

        if next_prio > prio {
            // If the next operator has higher priority, we need to parse the right side first.
            // ex "+ 2 * 3 + 4" -> parse "2 * 3" first

            // keep track of the calling prio so that we get the right order if we have several different priority levels
            right = parse_binary_op_expr(input, right, prio)?;
        }

        // We can then continue with the parsing at the current min_prio
        parse_binary_op_expr(input, Expr::bin_op(binop, left, right), min_prio)
    } else {
        // There is still something to parse, but it is not an operator... What to do?
        // Doing some debug prints to help understand what is going on.

        let next_token = input.parse::<TokenStream>().unwrap().to_string();

        // DEBUG
        eprintln!("Unexpected case input not empty but not an operator");
        eprintln!("Current state:");
        eprintln!("Input: {:?}", input);
        eprintln!("Next token: {:?}", next_token);
        eprintln!("Left: {:?}", left);
        eprintln!("Right: {:?}", right);
        eprintln!("Priority: {:?} and Min Priority: {:?}", prio, min_prio);
        Err(
            ParsingError::invalid_token(next_token, input_str, ParsingContext::BinaryOperation)
                .into(),
        )
    }
}

//? -------------------------------- Identifier case ------------------------------------------------

fn parse_ident_or_call(input: ParseStream) -> Result<Expr> {
    // This should parse identifiers such as 'a', 'my_var'
    // It should also parse function calls such as 'my_func(1, 2, 3)'

    let input_str = input.to_string();

    // Check if the next token is an identifier
    if input.peek(syn::Ident) {
        let id: syn::Ident = input.parse()?;
        let mut id = id.to_string();

        // Check if we find parentheses after the identifier (or a ! for macros)
        if input.peek(syn::token::Paren)
            || (input.peek(Token![!]) && input.peek2(syn::token::Paren))
        {
            // This is a function call

            // Simulate macro-rules by checking if the next token is a '!'
            if input.peek(Token![!]) {
                let _: Token![!] = input.parse()?;
                id.push('!');
            }

            // Parse the arguments
            let args: Arguments = input.parse()?;

            Ok(Expr::Call(id, args))
        } else {
            // This is an identifier
            Ok(Expr::Ident(id))
        }
    } else {
        let next_token = input.parse::<TokenStream>().unwrap().to_string();
        Err(ParsingError::invalid_token(next_token, input_str, ParsingContext::Identifier).into())
    }
}

//? -------------------------------- If Then Else case ----------------------------------------------

//
// We want to parse strings like
// `if expr { then block }`
// and
// `if expr { then block } else { else block }
//
// The else arm is optional
struct IfThenOptElse(Expr, Block, Option<Block>);

impl From<IfThenOptElse> for Expr {
    fn from(ite: IfThenOptElse) -> Self {
        Expr::IfThenElse(Box::new(ite.0), ite.1, ite.2)
    }
}

impl Parse for IfThenOptElse {
    fn parse(input: ParseStream) -> Result<IfThenOptElse> {
        // If statements look like `if expr { then block }` or `if expr { then block } else { else block }`
        // But they can also use the notation `if expr { then block } else if expr { then block } ... else { else block }`
        // with multiple 'else if' which should be parsed as else { if expr { then block } else { ... } }

        let input_str = input.to_string();

        // Should begin by the identifier 'if'
        if input.peek(Token![if]) {
            // Consume the 'if' keyword
            let _ = input.parse::<Token![if]>()?;

            // Get condition expression
            let cond: Expr = input.parse()?;

            // Get the block for the 'then' arm
            let then_block: Block = input.parse()?;

            // Check if there is an 'else' arm
            let mut else_block: Option<Block> = None;
            if input.peek(Token![else]) {
                // Consume the 'else' keyword
                let _ = input.parse::<Token![else]>()?;

                // Check if it is an 'else if' or an 'else' block
                if input.peek(Token![if]) {
                    // Then, we can recursively parse the 'else if' arm into the else block
                    let else_if = IfThenOptElse::parse(input)?;
                    else_block = Some(convert_if_then_else_to_block(else_if));
                } else {
                    // Get the block for the 'else' arm, it is the end of any nested if-then-else
                    else_block = Some(input.parse()?);
                }
            }

            Ok(IfThenOptElse(cond, then_block, else_block))
        } else {
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "if".to_string(),
                next_token,
                input_str,
                ParsingContext::IfThenElse,
            )
            .into())
        }
    }
}

fn convert_if_then_else_to_block(if_then_else: IfThenOptElse) -> Block {
    // Useful to treat else if blocks as a single else block containing another if-then-else

    let mut statements: Vec<Statement> = Vec::new();

    // Do not need to end the block with a semicolon. Putting it to true would lead to if cond { then block } (else { else block });
    let semi = false;

    // Convert IfThenOptElse to expr
    let expr = Expr::from(if_then_else);
    statements.push(Statement::Expr(expr));

    Block::new(statements, semi)
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         'Type' Type                                           #
//?#                                                                                               #
//?#################################################################################################

use quote::quote;

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Type> {
        // The syn::Type is very complex and overkill
        // Types in Rust involve generics, paths
        // etc., etc., etc. ...
        //
        // To make things simple, we just turn the syn::Type
        // to a token stream (`quote`) and turn that into a String
        // and turn that into an &str (`as_str`)

        // Types are either specific identifiers or a '()' unit type
        // Now it can also be an array: Array<i32>

        let input_str = input.to_string();

        let mut typename: String = String::new();
        if input.peek(syn::Ident) {
            let id: syn::Ident = input.parse()?;
            typename = id.to_string();
        } else if input.peek(syn::token::Bracket) {
            // Support for array types
            let content;
            let _ = syn::bracketed!(content in input);

            typename.push('[');
            typename.push_str(&content.to_string());
            typename.push(']');

            // consume the content
            while !content.is_empty() {
                let _ = content.parse::<TokenStream>().unwrap().to_string();
            }
        } else if input.peek(syn::token::Paren) {
            let content;
            let _ = syn::parenthesized!(content in input);

            if content.is_empty() {
                typename = "()".to_string();
            }
        }

        if Type::is_valid_typename(typename.as_str()) {
            Ok(Type::new(typename.as_str()))
        } else {
            Err(ParsingError::invalid_typename(input_str, ParsingContext::Type).into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Arguments Type                                        #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Arguments {
    fn parse(input: ParseStream) -> Result<Arguments> {
        // arguments are enclosed in parentheses (can be empty)
        // they are separated by commas, with the last one being optional
        // ex: (1, 2, 3) or (1, 2, 3,)

        let input_str = input.to_string();

        if input.peek(syn::token::Paren) {
            let content;
            let _ = syn::parenthesized!(content in input);

            // Now we have the content inside the parentheses. ex: 1, 2, 3 or 1, 2, 3,
            // We can parse this content as a list of expressions separated by commas
            let mut args: Vec<Expr> = Vec::new();
            while !content.is_empty() {
                let expr: Expr = content.parse()?;
                args.push(expr);

                // If there is a comma, we consume it
                if content.peek(Token![,]) {
                    let _: Token![,] = content.parse()?;
                }
            }

            Ok(Arguments::new(args))
        } else {
            // This is not a valid argument list
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "(".to_string(),
                next_token,
                input_str,
                ParsingContext::Arguments,
            )
            .into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Parameter Type                                        #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Parameter {
    fn parse(input: ParseStream) -> Result<Parameter> {
        // A single parameter is an identifier followed by a colon and a type
        // It can be mutable or not
        // ex: a: i32 or mut a: i32

        let input_str = input.to_string();

        // Check for mutability
        let mut mutable = Mutable(false);
        if input.peek(Token![mut]) {
            let _: Token![mut] = input.parse()?;
            mutable = Mutable(true);
        }

        // Verify that the next token is an identifier
        if input.peek(syn::Ident) {
            let id: syn::Ident = input.parse()?;
            let id = id.to_string();

            // Verify that the next token is a colon
            if input.peek(Token![:]) {
                let _: Token![:] = input.parse()?;

                // Parse the type
                let ty: Type = input.parse()?;

                Ok(Parameter::new(mutable, id, ty))
            } else {
                let next_token = input.parse::<TokenStream>().unwrap().to_string();
                Err(ParsingError::expected_token(
                    ":".to_string(),
                    next_token,
                    input_str,
                    ParsingContext::Parameter,
                )
                .into())
            }
        } else {
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "identifier".to_string(),
                next_token,
                input_str,
                ParsingContext::Parameter,
            )
            .into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                        Parameters Type                                        #
//?#                                                                                               #
//?#################################################################################################

// Here we take advantage of the parser function `parse_terminated`
impl Parse for Parameters {
    fn parse(input: ParseStream) -> Result<Parameters> {
        // A list of parameters is enclosed in parentheses
        // ex: (a: i32, mut b: i32, c: bool)

        let input_str = input.to_string();

        // Check if the next token is a parenthesis
        if input.peek(syn::token::Paren) {
            let content;
            let _ = syn::parenthesized!(content in input);

            // Now we have the content inside the parentheses. ex: a: i32, mut b: i32, c: bool
            // We can parse this content as a list of parameters separated by commas
            let mut params: Vec<Parameter> = Vec::new();
            while !content.is_empty() {
                let param: Parameter = content.parse()?;
                params.push(param);

                // If there is a comma, we consume it
                if content.peek(Token![,]) {
                    let _: Token![,] = content.parse()?;
                }
            }

            Ok(Parameters::new(params))
        } else {
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "(".to_string(),
                next_token,
                input_str,
                ParsingContext::Parameters,
            )
            .into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                      FnDeclaration Type                                       #
//?#                                                                                               #
//?#################################################################################################

impl Parse for FnDeclaration {
    fn parse(input: ParseStream) -> Result<FnDeclaration> {
        // A function declaration is the keyword 'fn' followed by an identifier, a list of parameters,
        // an optional return type and a block

        let input_str = input.to_string();

        // Check for the 'fn' keyword
        if input.peek(Token![fn]) {
            // Consume the token
            let _: Token![fn] = input.parse()?;

            // Parse the identifier
            let id: syn::Ident = input.parse()?;
            let mut id = id.to_string();

            // Simulate macro-rules by accepting optional '!' in the identifier of the function
            if input.peek(Token![!]) {
                let _: Token![!] = input.parse()?;
                id.push('!');
            }

            // Parse the parameters
            let params: Parameters = input.parse()?;

            // Parse the optional return type
            let ty: Option<Type> = if input.peek(Token![->]) {
                let _: Token![->] = input.parse()?;
                Some(input.parse()?)
            } else {
                None
            };

            // Parse the function body
            let body: Block = input.parse()?;

            Ok(FnDeclaration::new(id, params, ty, body))
        } else {
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "fn".to_string(),
                next_token,
                input_str,
                ParsingContext::FnDeclaration,
            )
            .into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Statement Type                                        #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Statement {
    fn parse(input: ParseStream) -> Result<Statement> {
        // Statements can be let bindings, assignments, while loops, expressions or function declarations

        let input_str = input.to_string();

        if input.peek(Token![let]) {
            parse_let_binding(input)
        } else if input.peek(Token![while]) {
            parse_while_loops(input)
        } else if input.peek(Token![fn]) {
            // Parse a function declaration
            let f: FnDeclaration = input.parse()?;
            Ok(Statement::Fn(f))
        } else {
            // Parse an expression, or an assignment
            let expr: Expr = input.parse()?;

            // If blocks and generic blocks are expression statements that do not necessarily need a semicolon
            // They can not be followed by an assignment
            let block_type_expr = match expr {
                Expr::Block(_) | Expr::IfThenElse(_, _, _) => true,
                _ => false,
            };

            if end_of_statement(input) || block_type_expr {
                // This is an expression
                Ok(Statement::Expr(expr))
            } else if input.peek(Token![=]) {
                // This is an assignment
                let _: Token![=] = input.parse()?;
                let rhs: Expr = input.parse()?;
                Ok(Statement::Assign(expr, rhs))
            } else {
                // There is still something to parse, but it is not an assignment... What to do?
                // Doing some debug prints to help understand what is going on.

                let next_token = input.parse::<TokenStream>().unwrap().to_string();

                // DEBUG
                eprintln!("Unexpected case input not empty but not an assignment statement");
                eprintln!("Current state:");
                eprintln!("Input: {:?}", input);
                eprintln!("Next token: {:?}", next_token);
                eprintln!("Expr: {:?}", expr);
                Err(
                    ParsingError::invalid_token(next_token, input_str, ParsingContext::Statement)
                        .into(),
                )
            }
        }
    }
}

fn end_of_statement(input: ParseStream) -> bool {
    if input.is_empty() {
        true
    } else {
        // A statement is only terminated by a semicolon
        input.peek(Token![;])
    }
}

fn parse_let_binding(input: ParseStream) -> Result<Statement> {
    // a binding is a let keyword followed by an optional mutable, an identifier, an optional type and an optional expression

    let input_str = input.to_string();

    if input.peek(Token![let]) {
        // Consume the 'let' keyword
        let _: Token![let] = input.parse()?;

        let mut mutable = Mutable(false);
        // Parse the optional mutable
        if input.peek(Token![mut]) {
            let _: Token![mut] = input.parse()?;
            mutable = Mutable(true);
        }

        // Parse the identifier
        let id: syn::Ident = input.parse()?;
        let id = id.to_string();

        // Parse the optional type
        let ty: Option<Type> = if input.peek(Token![:]) {
            let _: Token![:] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };

        // Parse the optional expression
        let expr: Option<Expr> = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Statement::Let(mutable, id, ty, expr))
    } else {
        let next_token = input.parse::<TokenStream>().unwrap().to_string();
        Err(ParsingError::expected_token(
            "let".to_string(),
            next_token,
            input_str,
            ParsingContext::Statement,
        )
        .into())
    }
}

fn parse_while_loops(input: ParseStream) -> Result<Statement> {
    // a while loop is a while keyword followed by an expression and a block

    let input_str = input.to_string();

    if input.peek(Token![while]) {
        // Consume the 'while' keyword
        let _: Token![while] = input.parse()?;

        // Parse the condition expression
        let cond: Expr = input.parse()?;

        // Parse the block
        let block: Block = input.parse()?;

        Ok(Statement::While(cond, block))
    } else {
        let next_token = input.parse::<TokenStream>().unwrap().to_string();
        Err(ParsingError::expected_token(
            "while".to_string(),
            next_token,
            input_str,
            ParsingContext::Statement,
        )
        .into())
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Block Type                                          #
//?#                                                                                               #
//?#################################################################################################

use syn::punctuated::Punctuated;

// Here we take advantage of the parser function `parse_terminated`
impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Block> {
        // blocks are enclosed in braces
        // they contain a list of statements separated by semicolons
        // the last statement can be without a semicolon
        // ex: { let a = 1; let b = 2; } or { let a = 1; let b = 2 }

        let input_str = input.to_string();

        if input.peek(syn::token::Brace) {
            let content;
            let _ = syn::braced!(content in input);

            // Now we have the content inside the braces. ex: let a = 1; let b = 2; or let a = 1; let b = 2
            // We can parse this content as a list of statements separated by semi-colons
            let mut statements: Vec<Statement> = Vec::new();
            let mut semi = false;
            let mut valid_termination = true;
            while !content.is_empty() {
                let s: Statement = content.parse()?;
                let semi_required = s.requires_semi_colon();
                valid_termination = s.can_terminate_block();
                statements.push(s);

                // If there is a semi-colon, we consume it
                if content.peek(Token![;]) {
                    semi = true;
                    valid_termination = true; // Any statement ending with a semi-colon can terminate the block
                    let _: Token![;] = content.parse()?;
                } else {
                    semi = false;
                }

                // If there is no semi-colon, but the content is not empty, then there may be an error depending on
                // the type of statement we have just parsed
                if !content.is_empty() && !semi && semi_required {
                    break; // We break early to return an error
                }
            }

            if content.is_empty() {
                if valid_termination {
                    Ok(Block::new(statements, semi))
                } else {
                    // Invalid termination of block
                    Err(
                        ParsingError::unexpected_end_of_block(input_str, ParsingContext::Block)
                            .into(),
                    )
                }
            } else {
                // Expected semi-colon but found something else
                let next_token = content.parse::<TokenStream>().unwrap().to_string();
                Err(
                    ParsingError::invalid_token(next_token, input_str, ParsingContext::Block)
                        .into(),
                )
            }
        } else {
            // Expected braces but found something else
            let next_token = input.parse::<TokenStream>().unwrap().to_string();
            Err(ParsingError::expected_token(
                "{".to_string(),
                next_token,
                input_str,
                ParsingContext::Block,
            )
            .into())
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Prog Type                                           #
//?#                                                                                               #
//?#################################################################################################

impl Parse for Prog {
    fn parse(input: ParseStream) -> Result<Prog> {
        // A program is a list of function declarations
        // They are often separated by an empty line, but it is not present in the TokenStream whatsoever

        //? Debug
        // eprintln!("Parsing program");

        let mut fns: Vec<FnDeclaration> = Vec::new();
        while !input.is_empty() {
            //? Debug
            // eprintln!("Trying to parse a function from input:\n{}", input);
            let f: FnDeclaration = input.parse()?;
            //? Debug
            // eprintln!("Parsed function:\n{}", f);
            fns.push(f);
        }

        //? Debug
        // eprintln!("Parsed functions: {:?}", fns);

        Ok(Prog::new(fns))
    }
}
