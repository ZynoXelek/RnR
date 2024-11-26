use core::fmt;
use syn::Error as SynError;
use crate::ast::{Type, Expr, BinOp, UnOp};
use crate::vm::Val;

/// By default, Error is just a string.
/// If you want to implement some nicer and more elaborate error handling, you
/// can map `Error` to some other type.
pub type Error = String;

//? ---------------- Parsing Context ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum ParsingContext {
    // Basic types
    Type,
    Literal,
    Expr,
    BinOp,
    UnOp,
    Mutable,
    Statement,
    Block,
    Parameter,
    Parameters,
    Arguments,
    FnDeclaration,
    Prog,
    // Advanced Contexts
    Operand,
    BinaryOperation,
    UnaryOperation,
    Identifier,
    IfThenElse,
    WhileLoop,
}

impl fmt::Display for ParsingContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Basic types
            ParsingContext::Type => {
                write!(f, "type")
            }
            ParsingContext::Literal => {
                write!(f, "literal")
            }
            ParsingContext::Expr => {
                write!(f, "expression")
            }
            ParsingContext::BinOp => {
                write!(f, "binary operator")
            }
            ParsingContext::UnOp => {
                write!(f, "unary operator")
            }
            ParsingContext::Mutable => {
                write!(f, "mutable")
            }
            ParsingContext::Statement => {
                write!(f, "statement")
            }
            ParsingContext::Block => {
                write!(f, "block")
            }
            ParsingContext::Parameter => {
                write!(f, "parameter")
            }
            ParsingContext::Parameters => {
                write!(f, "parameters")
            }
            ParsingContext::Arguments => {
                write!(f, "arguments")
            }
            ParsingContext::FnDeclaration => {
                write!(f, "function declaration")
            }
            ParsingContext::Prog => {
                write!(f, "program")
            }
            // Advanced Contexts
            ParsingContext::Operand => {
                write!(f, "operand")
            }
            ParsingContext::BinaryOperation => {
                write!(f, "binary operation")
            }
            ParsingContext::UnaryOperation => {
                write!(f, "unary operation")
            }
            ParsingContext::Identifier => {
                write!(f, "identifier")
            }
            ParsingContext::IfThenElse => {
                write!(f, "if-then-else")
            }
            ParsingContext::WhileLoop => {
                write!(f, "while loop")
            }
        }
    }
}

//? ---------------- Parsing Error ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum ParsingError {
    InvalidToken {
        token: String,
        input_context: String,
        parsing_context: ParsingContext,
    },
    ExpectedToken {
        expected: String,
        found: String,
        input_context: String,
        parsing_context: ParsingContext,
    },
    UnexpectedEOF {
        input_context: String,
        parsing_context: ParsingContext,
    },
    ChainedComparisons {
        input_context: String,
        parsing_context: ParsingContext,
    },
    InvalidTypename {
        input_context: String,
        parsing_context: ParsingContext,
    },
    UnexpectedEndOfBlock {
        input_context: String,
        parsing_context: ParsingContext,
    }
}

impl ParsingError {
    pub fn invalid_token(token: String, input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::InvalidToken {
            token,
            input_context,
            parsing_context,
        }
    }

    pub fn expected_token(expected: String, found: String, input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::ExpectedToken {
            expected,
            found,
            input_context,
            parsing_context,
        }
    }

    pub fn unexpected_eof(input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::UnexpectedEOF {
            input_context,
            parsing_context,
        }
    }

    pub fn chained_comparisons(input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::ChainedComparisons {
            input_context,
            parsing_context,
        }
    }

    pub fn invalid_typename(input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::InvalidTypename {
            input_context,
            parsing_context,
        }
    }

    pub fn unexpected_end_of_block(input_context: String, parsing_context: ParsingContext) -> Self {
        ParsingError::UnexpectedEndOfBlock {
            input_context,
            parsing_context,
        }
    }
}

impl From<ParsingError> for SynError {
    fn from(err: ParsingError) -> Self {
        let message = &err.to_string();
        match err {
            //TODO: Add location information, or better use of tokens span
            ParsingError::InvalidToken { token, input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            },
            ParsingError::ExpectedToken { expected, found, input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            },
            ParsingError::UnexpectedEOF { input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            },
            ParsingError::ChainedComparisons { input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            },
            ParsingError::InvalidTypename { input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            },
            ParsingError::UnexpectedEndOfBlock { input_context, parsing_context } => {
                SynError::new_spanned(input_context, message)
            }
        }
    }
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parsing error: ")?;
        match self {
            ParsingError::InvalidToken { token, input_context, parsing_context } => {
                write!(f, "Invalid token '{}' in '{}' during {} parsing", token, input_context, parsing_context)
            },
            ParsingError::ExpectedToken { expected, found, input_context, parsing_context } => {
                write!(f, "Expected token '{}' in '{}' during {} parsing, found '{}'", expected, input_context, parsing_context, found)
            },
            ParsingError::UnexpectedEOF { input_context, parsing_context } => {
                write!(f, "Unexpected EOF in '{}' during {} parsing", input_context, parsing_context)
            },
            ParsingError::ChainedComparisons { input_context, parsing_context } => {
                write!(f, "Chained comparisons in '{}' during {} parsing", input_context, parsing_context)
            },
            ParsingError::InvalidTypename { input_context, parsing_context } => {
                write!(f, "Invalid typename in '{}' during {} parsing", input_context, parsing_context)
            },
            ParsingError::UnexpectedEndOfBlock { input_context, parsing_context } => {
                write!(f, "Unexpected end of block in '{}' during {} parsing", input_context, parsing_context)
            }
        }
    }
}

//? ---------------- Evaluation Error ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    InvalidExtraction {
        expected_type: Type,
        found_type: Type,
    },
    Uninit,
    MainNotFound,
    ScopeError {
        message: String,
    },
    VariableNotFound {
        name: String,
    },
    FunctionNotFound {
        name: String,
    },
    FunctionAlreadyDefined {
        name: String,
    },
    AssignmentError {
        expr: Expr,
    },
    ExpectedIdentifier {
        found: Expr,
    },
    BinaryOperationError {
        op: BinOp,
        left: Val,
        right: Val,
    },
    UnaryOperationError {
        op: UnOp,
        operand: Val,
    },
    DivisionByZero,
    IndexOutOfBounds {
        index: usize,
        size: usize,
    },
}

impl EvalError {
    pub fn invalid_extraction(expected_type: Type, found_type: Type) -> Self {
        EvalError::InvalidExtraction {
            expected_type,
            found_type,
        }
    }
    pub fn uninit() -> Self {
        EvalError::Uninit
    }
    pub fn main_not_found() -> Self {
        EvalError::MainNotFound
    }
    pub fn scope_error(message: String) -> Self {
        EvalError::ScopeError {
            message,
        }
    }
    pub fn variable_not_found(name: String) -> Self {
        EvalError::VariableNotFound {
            name,
        }
    }
    pub fn function_not_found(name: String) -> Self {
        EvalError::FunctionNotFound {
            name,
        }
    }
    pub fn function_already_defined(name: String) -> Self {
        EvalError::FunctionAlreadyDefined {
            name,
        }
    }
    pub fn assignment_error(expr: Expr) -> Self {
        EvalError::AssignmentError {
            expr,
        }
    }
    pub fn expected_identifier(found: Expr) -> Self {
        EvalError::ExpectedIdentifier {
            found,
        }
    }
    pub fn binary_operation_error(op: BinOp, left: Val, right: Val) -> Self {
        EvalError::BinaryOperationError {
            op,
            left,
            right,
        }
    }
    pub fn unary_operation_error(op: UnOp, operand: Val) -> Self {
        EvalError::UnaryOperationError {
            op,
            operand,
        }
    }
    pub fn division_by_zero() -> Self {
        EvalError::DivisionByZero
    }
    pub fn index_out_of_bounds(index: usize, size: usize) -> Self {
        EvalError::IndexOutOfBounds {
            index,
            size,
        }
    }
}

impl From<EvalError> for SynError {
    fn from(err: EvalError) -> Self {
        let message = err.to_string();
        match err {
            //TODO: Add location information, or better use of tokens span
            EvalError::InvalidExtraction { expected_type, found_type } => {
                SynError::new_spanned("", message)
            }
            EvalError::Uninit => {
                SynError::new_spanned("", message)
            }
            EvalError::MainNotFound => {
                SynError::new_spanned("", message)
            }
            EvalError::ScopeError { message } => {
                SynError::new_spanned("", message)
            }
            EvalError::VariableNotFound { name } => {
                SynError::new_spanned("", message)
            }
            EvalError::FunctionNotFound { name } => {
                SynError::new_spanned("", message)
            }
            EvalError::FunctionAlreadyDefined { name } => {
                SynError::new_spanned("", message)
            }
            EvalError::AssignmentError { expr } => {
                SynError::new_spanned("", message)
            }
            EvalError::ExpectedIdentifier { found } => {
                SynError::new_spanned("", message)
            }
            EvalError::BinaryOperationError { op, left, right } => {
                SynError::new_spanned("", message)
            }
            EvalError::UnaryOperationError { op, operand } => {
                SynError::new_spanned("", message)
            }
            EvalError::DivisionByZero => {
                SynError::new_spanned("", message)
            }
            EvalError::IndexOutOfBounds { index, size } => {
                SynError::new_spanned("", message)
            }
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Evaluation error: ")?;
        match self {
            EvalError::InvalidExtraction { expected_type, found_type } => {
                write!(f, "Invalid extraction: expected type '{}', found type '{}'", expected_type, found_type)
            }
            EvalError::Uninit => {
                write!(f, "Uninitialized value error")
            }
            EvalError::MainNotFound => {
                write!(f, "Main function not found in program")
            }
            EvalError::ScopeError { message } => {
                write!(f, "Scope error: {}", message)
            }
            EvalError::VariableNotFound { name } => {
                write!(f, "Variable '{}' not found", name)
            }
            EvalError::FunctionNotFound { name } => {
                write!(f, "Function '{}' not found", name)
            }
            EvalError::FunctionAlreadyDefined { name } => {
                write!(f, "Function '{}' already defined", name)
            }
            EvalError::AssignmentError { expr } => {
                write!(f, "Assignment error: invalid left expression '{}'", expr)
            }
            EvalError::ExpectedIdentifier { found } => {
                write!(f, "Expected identifier, found '{}'", found)
            }
            EvalError::BinaryOperationError { op, left, right } => {
                write!(f, "Binary operation error: invalid operation '{}' between '{}' and '{}'", op, left, right)
            }
            EvalError::UnaryOperationError { op, operand } => {
                write!(f, "Unary operation error: invalid operation '{}' on '{}'", op, operand)
            }
            EvalError::DivisionByZero => {
                write!(f, "Division by zero error")
            }
            EvalError::IndexOutOfBounds { index, size } => {
                write!(f, "Index out of bounds: index '{}' out of size '{}'", index, size)
            }
        }
    }
}
