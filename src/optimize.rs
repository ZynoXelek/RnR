use core::fmt;
// use std::collections::HashMap;

use crate::ast::{
    Arguments, BinOp, Block, Expr, FnDeclaration, Literal, Parameters, Prog, Statement, Type, UnOp,
};
use crate::vm::Val;
use crate::common::Optimize;
use crate::error::Error; //TODO: use custom error
use crate::intrinsics::*;

//?#################################################################################################
//?#                                                                                               #
//?#                                            AST Type                                           #
//?#                                                                                               #
//?#################################################################################################

// pub enum AST {
//     None,
//     Expr(Expr),
//     Block(Block),
//     Prog(Prog),
// }

// impl AST {
//     pub fn new() -> Self {
//         AST::None
//     }

//     pub fn get_expr(&self) -> Option<&Expr> {
//         match self {
//             AST::Expr(expr) => Some(expr),
//             _ => None,
//         }
//     }

//     pub fn get_block(&self) -> Option<&Block> {
//         match self {
//             AST::Block(block) => Some(block),
//             _ => None,
//         }
//     }

//     pub fn get_prog(&self) -> Option<&Prog> {
//         match self {
//             AST::Prog(prog) => Some(prog),
//             _ => None,
//         }
//     }
// }

// impl From<Expr> for AST {
//     fn from(expr: Expr) -> Self {
//         AST::Expr(expr)
//     }
// }

// impl From<Block> for AST {
//     fn from(block: Block) -> Self {
//         AST::Block(block)
//     }
// }

// impl From<Prog> for AST {
//     fn from(prog: Prog) -> Self {
//         AST::Prog(prog)
//     }
// }

//?#################################################################################################
//?#                                                                                               #
//?#                                           Optimizer                                           #
//?#                                                                                               #
//?#################################################################################################

impl Optimize<Expr> for Expr {
    fn optimize(&self) -> Result<Expr, Error> {
        let mut optimizer = Optimizer::new();
        let result = optimizer.optimize_expr(self.clone());
        // let result = optimizer.get_result().get_expr();

        Ok(result?)
    }
}

// here you implement your VM
pub struct Optimizer {
    // The optimizer will generate an optimized version of the AST by erasing useless parts of code,
    // and simplifying expressions
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            // result: AST::new()
            }
    }

    // pub fn get_result(&self) -> &AST {
    //     &self.result
    // }

    //? ----------------------- Optimizer functions -----------------------

    fn optimize_unop(&mut self, unop: UnOp, operand: Expr) -> Result<Expr, Error> {
        // For now, the unop optimization only optimizes the operand, and returns the same unop if it is not a literal.

        let operand_opt = self.optimize_expr(operand)?;

        // If the operand is the same unop for negations, we can remove them.
        if unop == UnOp::Neg {
            if let Expr::UnOp(UnOp::Neg, inner_inner) = operand_opt {
                return Ok(*inner_inner);
            }
        } else if unop == UnOp::Bang {
            if let Expr::UnOp(UnOp::Bang, inner_inner) = operand_opt {
                return Ok(*inner_inner);
            }
        }

        // If the operand is a literal, we can evaluate the operation, else we just return the unop with the optimized operand.
        match operand_opt {
            Expr::Lit(lit) => {
                // Use the vm to evaluate it
                let val = lit.into();
                let result = unop.eval(val);

                // Now we need to convert it as an Expr
                match result {
                    Ok(val) => {
                        let lit = Literal::from(val);
                        Ok(Expr::Lit(lit))
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => Ok(Expr::UnOp(unop, Box::new(operand_opt))),
        }
    }

    fn optimize_binop(&mut self, binop: BinOp, left: Expr, right: Expr) -> Result<Expr, Error> {
        // For now, the binop optimization only optimizes the two operands, and returns the same binop if they are not both literals.
        // Arrays get operations can be optimized if the index is a literal
        // Boolean operations may be short-circuited

        let left_opt = self.optimize_expr(left)?;
        let right_opt = self.optimize_expr(right)?;

        // Boolean short-circuiting
        if binop == BinOp::And {
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                return Ok(Expr::Lit(Literal::Bool(false)));
            }
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                return Ok(right_opt);
            }
        } else if binop == BinOp::Or {
            if let Expr::Lit(Literal::Bool(true)) = left_opt {
                return Ok(Expr::Lit(Literal::Bool(true)));
            }
            if let Expr::Lit(Literal::Bool(false)) = left_opt {
                return Ok(right_opt);
            }
        }

        //TODO: Improve optimization to support better optimizations
        // For instance, 3 + 4 + 5 + a => 12 + a
        // However, 3 + a + 4 + 5 => 12 + a as well (not supported yet)

        // If the operands are literals, we can evaluate the operation, else we just return the binop with the optimized operands.
        match (left_opt.clone(), right_opt.clone()) {
            (Expr::Lit(lit1), Expr::Lit(lit2)) => {
                // Use the vm to evaluate it
                let val1 = lit1.into();
                let val2 = lit2.into();
                let result = binop.eval(val1, val2);

                // Now we need to convert it as an Expr
                match result {
                    Ok(val) => {
                        let lit = Literal::from(val);
                        Ok(Expr::Lit(lit))
                    }
                    Err(e) => Err(e.to_string()), //TODO: Use custom error
                }
            }
            _ => Ok(Expr::BinOp(binop, Box::new(left_opt), Box::new(right_opt))),
        }
    }

    fn optimize_par(&mut self, inner: Expr) -> Result<Expr, Error> {
        // For now, the Par optimization only optimizes the inner expression, and returns it without the parenthesis.

        let inner_opt = self.optimize_expr(inner)?;

        Ok(inner_opt)

        //? We may want to keep the outer parenthesis for the inner expression if it is an operation.
        // // If the inner part is not an operation, we can remove the parenthesis.
        // match inner_opt {
        //     Expr::BinOp(_, _, _) | Expr::UnOp(_, _) => Ok(Expr::Par(Box::new(inner_opt))), // We keep the parenthesis.
        //     _ => Ok(inner_opt), // We can remove the parenthesis.
        // }
    }

    fn optimize_call(&mut self, ident: String, args: Arguments) -> Result<Expr, Error> {
        // To optimize a function call, we optimize each argument and keep the same function call.

        let mut args_opt = vec![];
        for arg in args.0.iter() {
            let arg_opt = self.optimize_expr(arg.clone())?;
            args_opt.push(arg_opt);
        }

        Ok(Expr::Call(ident, Arguments(args_opt)))
    }

    fn optimize_ifthenelse(&mut self, cond: Expr, then_block: Block, else_block: Option<Block>) -> Result<Expr, Error> {
        // To optimize an if then else, we optimize the condition. If we can get a literal boolean, we can replace the if then else by the corresponding block.
        // We optimize the remaining block(s) as well.

        let cond_opt = self.optimize_expr(cond)?;

        // If the condition is a literal, we can evaluate it and return the corresponding block.
        if let Expr::Lit(Literal::Bool(b)) = cond_opt {
            if b {
                let then_block_opt = self.optimize_block(then_block)?;
                return Ok(Expr::Block(then_block_opt));
            } else {
                if let Some(else_block) = else_block {
                    let else_block_opt = self.optimize_block(else_block)?;
                    return Ok(Expr::Block(else_block_opt));
                } else {
                    return Ok(Expr::Block(Block {
                        statements: vec![], // Generate an empty block
                        semi: false,
                    }));
                }
            }
        }
        
        // Else, we optimize both blocks and return the if then else with the optimized condition.
        let then_block_opt = self.optimize_block(then_block)?;
        let else_block_opt = match else_block {
            Some(block) => Some(self.optimize_block(block)?),
            None => None,
        };

        Ok(Expr::IfThenElse(Box::new(cond_opt), then_block_opt, else_block_opt))
    }

    pub fn optimize_expr(&mut self, expr: Expr) -> Result<Expr, Error> {

        match expr {
            Expr::Ident(ident) => Ok(Expr::Ident(ident)), //TODO: Implement identifier optimizations?
            Expr::Lit(lit) => Ok(Expr::Lit(lit)),
            Expr::BinOp(binop, left, right) => self.optimize_binop(binop, *left, *right),
            Expr::UnOp(unop, operand) => self.optimize_unop(unop, *operand),
            Expr::Par(inner) => self.optimize_par(*inner),
            Expr::Call(ident, args) => self.optimize_call(ident, args),
            Expr::IfThenElse(cond, then_block, else_block) => {
                self.optimize_ifthenelse(*cond, then_block, else_block)
            }
            Expr::Block(block) => {
                let opt_block = self.optimize_block(block);
                match opt_block {
                    Ok(b) => Ok(Expr::Block(b)),
                    Err(e) => Err(e),
                }
            }
        }
    }

    pub fn optimize_block(&mut self, block: Block) -> Result<Block, Error> {
        //TODO: Add block optimization, for now, it only returns the exact same block

        //TODO: Combine with block optim to replace simple blocks by their return value
        //TODO: Blocks that do not return a value, and do not do any peculiar things can be removed

        Ok(block)
    }
}
