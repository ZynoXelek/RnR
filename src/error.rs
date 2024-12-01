use crate::ast::{BinOp, Expr, FnDeclaration, Parameter, Type, UnOp};
use crate::vm::Val;
use core::fmt;
use syn::Error as SynError;

/// By default, Error is just a string.
/// If you want to implement some nicer and more elaborate error handling, you
/// can map `Error` to some other type.
pub type Error = String;

//?#################################################################################################
//?#                                                                                               #
//?#                                        Parsing Errors                                         #
//?#                                                                                               #
//?#################################################################################################

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
    },
}

impl ParsingError {
    pub fn invalid_token(
        token: String,
        input_context: String,
        parsing_context: ParsingContext,
    ) -> Self {
        ParsingError::InvalidToken {
            token,
            input_context,
            parsing_context,
        }
    }

    pub fn expected_token(
        expected: String,
        found: String,
        input_context: String,
        parsing_context: ParsingContext,
    ) -> Self {
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

// Needed to convert ParsingError to SynError in parse.rs
impl From<ParsingError> for SynError {
    fn from(err: ParsingError) -> Self {
        let message = &err.to_string();
        match err {
            //TODO: Add location information, or better use of tokens span
            ParsingError::InvalidToken {
                token,
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
            ParsingError::ExpectedToken {
                expected,
                found,
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
            ParsingError::UnexpectedEOF {
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
            ParsingError::ChainedComparisons {
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
            ParsingError::InvalidTypename {
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
            ParsingError::UnexpectedEndOfBlock {
                input_context,
                parsing_context,
            } => SynError::new_spanned(input_context, message),
        }
    }
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parsing error: ")?;
        match self {
            ParsingError::InvalidToken {
                token,
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Invalid token '{}' in '{}' during {} parsing",
                    token, input_context, parsing_context
                )
            }
            ParsingError::ExpectedToken {
                expected,
                found,
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Expected token '{}' in '{}' during {} parsing, found '{}'",
                    expected, input_context, parsing_context, found
                )
            }
            ParsingError::UnexpectedEOF {
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Unexpected EOF in '{}' during {} parsing",
                    input_context, parsing_context
                )
            }
            ParsingError::ChainedComparisons {
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Chained comparisons in '{}' during {} parsing",
                    input_context, parsing_context
                )
            }
            ParsingError::InvalidTypename {
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Invalid typename in '{}' during {} parsing",
                    input_context, parsing_context
                )
            }
            ParsingError::UnexpectedEndOfBlock {
                input_context,
                parsing_context,
            } => {
                write!(
                    f,
                    "Unexpected end of block in '{}' during {} parsing",
                    input_context, parsing_context
                )
            }
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Type Errors                                           #
//?#                                                                                               #
//?#################################################################################################

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    InvalidType {
        expected: Type,
        found: Type,
    },
    UninitializedVariable {
        expr: Expr, // Should be an identifier (nothing else can produce this error)
    },
    MainNotFound,
    MainWithParameters,
    MainWithInvalidType { // In Rust, a main function should return either () or Result<(), E>, where E implements the std::error::Error trait.
        found: Type,
    },
    BinOpTypeMismatch {
        op: BinOp,
        left: Type,
        right: Type,
    },
    UnOpTypeMismatch {
        op: UnOp,
        operand: Type,
    },
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
    InvalidNumberOfArguments {
        func: FnDeclaration,
        expected: usize,
        found: usize,
    },
    InvalidArgument {
        func: FnDeclaration,
        parameter: Parameter,
        expected: Type,
        found: Type,
    },
    InvalidFunctionReturnType {
        func: FnDeclaration,
        expected: Type,
        found: Type,
    },
    InvalidIfCondition {
        expr: Expr,
        found: Type,
    },
    IfBlocksTypeMismatch {
        then_type: Type,
        else_type: Type,
    },
    MissingVariableType {
        name: String,
    },
    LetTypeMismatch {
        name: String,
        expr: Expr,
        expected: Type,
        found: Type,
    },
    AssignmentInvalidLeftExpr {
        // When the left expression is not a mutable or uninitialized variable
        expr: Expr,
    },
    AssignmentTypeMismatch {
        // When the right expression is of the wrong type
        expr: Expr,
        expected: Type,
        found: Type,
    },
    ArrayInvalidIndex {
        // When the index is not an integer
        index: Expr,
        found: Type,
    },
    InvalidWhileCondition {
        expr: Expr,
        found: Type,
    },
    WhileBlockTypeMismatch {
        block_type: Type,
    },
}

impl TypeError {
    pub fn invalid_type(expected: Type, found: Type) -> Self {
        TypeError::InvalidType { expected, found }
    }

    pub fn uninitialized_variable(expr: Expr) -> Self {
        TypeError::UninitializedVariable { expr }
    }

    pub fn main_not_found() -> Self {
        TypeError::MainNotFound
    }

    pub fn main_with_parameters() -> Self {
        TypeError::MainWithParameters
    }

    pub fn main_with_invalid_type(found: Type) -> Self {
        TypeError::MainWithInvalidType { found }
    }

    pub fn binop_type_mismatch(op: BinOp, left: Type, right: Type) -> Self {
        TypeError::BinOpTypeMismatch { op, left, right }
    }

    pub fn unop_type_mismatch(op: UnOp, operand: Type) -> Self {
        TypeError::UnOpTypeMismatch { op, operand }
    }

    pub fn scope_error(message: String) -> Self {
        TypeError::ScopeError { message }
    }

    pub fn variable_not_found(name: String) -> Self {
        TypeError::VariableNotFound { name }
    }

    pub fn function_not_found(name: String) -> Self {
        TypeError::FunctionNotFound { name }
    }

    pub fn function_already_defined(name: String) -> Self {
        TypeError::FunctionAlreadyDefined { name }
    }

    pub fn invalid_number_of_arguments(func: FnDeclaration, expected: usize, found: usize) -> Self {
        TypeError::InvalidNumberOfArguments {
            func,
            expected,
            found,
        }
    }

    pub fn invalid_argument(
        func: FnDeclaration,
        parameter: Parameter,
        expected: Type,
        found: Type,
    ) -> Self {
        TypeError::InvalidArgument {
            func,
            parameter,
            expected,
            found,
        }
    }

    pub fn invalid_function_return_type(func: FnDeclaration, expected: Type, found: Type) -> Self {
        TypeError::InvalidFunctionReturnType {
            func,
            expected,
            found,
        }
    }

    pub fn invalid_if_condition(expr: Expr, found: Type) -> Self {
        TypeError::InvalidIfCondition { expr, found }
    }

    pub fn if_blocks_type_mismatch(then_type: Type, else_type: Type) -> Self {
        TypeError::IfBlocksTypeMismatch {
            then_type,
            else_type,
        }
    }

    pub fn missing_variable_type(name: String) -> Self {
        TypeError::MissingVariableType { name }
    }

    pub fn let_type_mismatch(name: String, expr: Expr, expected: Type, found: Type) -> Self {
        TypeError::LetTypeMismatch {
            name,
            expr,
            expected,
            found,
        }
    }

    pub fn assignment_invalid_left_expr(expr: Expr) -> Self {
        TypeError::AssignmentInvalidLeftExpr { expr }
    }

    pub fn assignment_type_mismatch(expr: Expr, expected: Type, found: Type) -> Self {
        TypeError::AssignmentTypeMismatch {
            expr,
            expected,
            found,
        }
    }

    pub fn array_invalid_index(index: Expr, found: Type) -> Self {
        TypeError::ArrayInvalidIndex { index, found }
    }

    pub fn invalid_while_condition(expr: Expr, found: Type) -> Self {
        TypeError::InvalidWhileCondition { expr, found }
    }

    pub fn while_block_type_mismatch(block_type: Type) -> Self {
        TypeError::WhileBlockTypeMismatch { block_type }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type error: ")?;
        match self {
            TypeError::InvalidType { expected, found } => {
                write!(
                    f,
                    "Invalid type: expected '{}', found '{}'",
                    expected, found
                )
            }
            TypeError::UninitializedVariable { expr } => {
                write!(f, "Uninitialized variable in expression '{}'", expr)
            }
            TypeError::MainNotFound => {
                write!(f, "Main function not found in program")
            }
            TypeError::MainWithParameters => {
                write!(f, "Main function should not have any parameters")
            }
            TypeError::MainWithInvalidType { found } => {
                write!(f, "Main function should return unit type, found '{}'", found)
            }
            TypeError::BinOpTypeMismatch { op, left, right } => {
                write!(
                    f,
                    "Binary operation type mismatch: invalid operation '{}' between '{}' and '{}'",
                    op, left, right
                )
            }
            TypeError::UnOpTypeMismatch { op, operand } => {
                write!(
                    f,
                    "Unary operation type mismatch: invalid operation '{}' on '{}'",
                    op, operand
                )
            }
            TypeError::ScopeError { message } => {
                write!(f, "Scope error: {}", message)
            }
            TypeError::VariableNotFound { name } => {
                write!(f, "Variable '{}' not found", name)
            }
            TypeError::FunctionNotFound { name } => {
                write!(f, "Function '{}' not found", name)
            }
            TypeError::FunctionAlreadyDefined { name } => {
                write!(f, "Function '{}' already defined", name)
            }
            TypeError::InvalidNumberOfArguments {
                func,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Invalid number of arguments for function '{}' expected '{}', found '{}'",
                    func.get_signature_repr(0),
                    expected,
                    found
                )
            }
            TypeError::InvalidArgument {
                func,
                parameter,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Invalid argument: expected type '{}' for parameter '{}' in function '{}', but found type '{}'",
                    expected, parameter.id, func.get_signature_repr(0), found
                )
            }
            TypeError::InvalidFunctionReturnType { func, expected, found } => {
                write!(
                    f,
                    "Invalid function return type: expected type '{}' for function '{}', but found type '{}'",
                    expected,
                    func.get_signature_repr(0),
                    found
                )
            }
            TypeError::InvalidIfCondition { expr, found } => {
                write!(
                    f,
                    "Invalid if condition: '{}' is of type '{}' while expected a boolean",
                    expr, found
                )
            }
            TypeError::IfBlocksTypeMismatch {
                then_type,
                else_type,
            } => {
                write!(
                    f,
                    "If blocks type mismatch: then block is of type '{}' while else block is of type '{}'",
                    then_type, else_type
                )
            }
            TypeError::MissingVariableType { name } => {
                write!(f, "Missing variable type for '{}'. You should either specify one, or initialize it with an expression.", name)
            }
            TypeError::LetTypeMismatch {
                name,
                expr,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Let type mismatch: expected type '{}' for variable '{}' but '{}' is of type '{}'",
                    expected, name, expr, found
                )
            }
            TypeError::AssignmentInvalidLeftExpr { expr } => {
                write!(f, "Assignment error: left expression '{}' is not a mutable or uninitialized variable", expr)
            }
            TypeError::AssignmentTypeMismatch {
                expr,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Assignment error: expected type '{}' for variable '{}' but '{}' is of type '{}'",
                    expected, expr, expr, found
                )
            }
            TypeError::ArrayInvalidIndex { index, found } => {
                write!(
                    f,
                    "Invalid array index: '{}' is of type '{}' while expected an integer",
                    index, found
                )
            }
            TypeError::InvalidWhileCondition { expr, found } => {
                write!(
                    f,
                    "Invalid while condition: '{}' is of type '{}' while expected a boolean",
                    expr, found
                )
            }
            TypeError::WhileBlockTypeMismatch { block_type } => {
                write!(
                    f,
                    "While block type mismatch: while block is of type '{}' while expected of unit type",
                    block_type
                )
            }
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                       Evaluation Errors                                       #
//?#                                                                                               #
//?#################################################################################################

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    InvalidExtraction {
        expected_type: Type,
        found_type: Type,
    },
    Uninit,
    MainNotFound,
    MainWithParameters,
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
    TypeError(TypeError),
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
    pub fn main_with_parameters() -> Self {
        EvalError::MainWithParameters
    }
    pub fn scope_error(message: String) -> Self {
        EvalError::ScopeError { message }
    }
    pub fn variable_not_found(name: String) -> Self {
        EvalError::VariableNotFound { name }
    }
    pub fn function_not_found(name: String) -> Self {
        EvalError::FunctionNotFound { name }
    }
    pub fn function_already_defined(name: String) -> Self {
        EvalError::FunctionAlreadyDefined { name }
    }
    pub fn assignment_error(expr: Expr) -> Self {
        EvalError::AssignmentError { expr }
    }
    pub fn expected_identifier(found: Expr) -> Self {
        EvalError::ExpectedIdentifier { found }
    }
    pub fn binary_operation_error(op: BinOp, left: Val, right: Val) -> Self {
        EvalError::BinaryOperationError { op, left, right }
    }
    pub fn unary_operation_error(op: UnOp, operand: Val) -> Self {
        EvalError::UnaryOperationError { op, operand }
    }
    pub fn division_by_zero() -> Self {
        EvalError::DivisionByZero
    }
    pub fn index_out_of_bounds(index: usize, size: usize) -> Self {
        EvalError::IndexOutOfBounds { index, size }
    }
    pub fn type_error(error: TypeError) -> Self {
        EvalError::TypeError(error)
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Evaluation error: ")?;
        match self {
            EvalError::InvalidExtraction {
                expected_type,
                found_type,
            } => {
                write!(
                    f,
                    "Invalid extraction: expected type '{}', found type '{}'",
                    expected_type, found_type
                )
            }
            EvalError::Uninit => {
                write!(f, "Uninitialized value error")
            }
            EvalError::MainNotFound => {
                write!(f, "Main function not found in program")
            }
            EvalError::MainWithParameters => {
                write!(f, "Main function should not have any parameters")
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
                write!(
                    f,
                    "Binary operation error: invalid operation '{}' between '{}' and '{}'",
                    op, left, right
                )
            }
            EvalError::UnaryOperationError { op, operand } => {
                write!(
                    f,
                    "Unary operation error: invalid operation '{}' on '{}'",
                    op, operand
                )
            }
            EvalError::DivisionByZero => {
                write!(f, "Division by zero error")
            }
            EvalError::IndexOutOfBounds { index, size } => {
                write!(
                    f,
                    "Index out of bounds: index '{}' out of size '{}'",
                    index, size
                )
            }
            EvalError::TypeError(error) => {
                write!(f, "{}", error)
            }
        }
    }
}
