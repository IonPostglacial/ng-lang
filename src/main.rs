mod lang;

use std::{error::Error, path::PathBuf};
use std::env;
use std::fmt::Write;
use lang::parsing::{Expression, TypeExpr, Param};

use crate::lang::{
    parsing::parse_string, 
    parsing::ExpressionKind,
    parsing::BinaryOperator,
    parsing::UnaryOperator,
    lexing::Code,
    lexing::CodeOrigin::Interactive, 
    lexing::CodeOrigin::File
};

fn write_type(res: &mut String, texpr: &TypeExpr) -> Result<(), std::fmt::Error> {
    let mut sep = "";
    write!(res, "{}", texpr.sym.name)?;
    if texpr.nullable {
        write!(res, "?")?;
    }
    if texpr.args.len() > 0 {
        write!(res, "[")?;
    }
    for arg in &texpr.args {
        write!(res, "{sep}")?;
        sep = ", ";
        write_type(res, arg)?;
    }
    if texpr.args.len() > 0 {
        write!(res, "]")?;
    }
    Ok(())
}

fn  write_params(res: &mut String, params: &Vec<Param>, indent: &str) -> Result<(), std::fmt::Error> {
    for param in params {
        write!(res, "{indent}    param {} : <", param.name.name)?;
        if let Some(type_expr) = param.type_id {
            write_type(res, &type_expr)?;
        }
        writeln!(res, ">")?;
    }
    Ok(())
}

fn write_expression_indent(res: &mut String, expr: &Expression, indent: &str) -> Result<(), Box<dyn Error>> {
    res.push_str(indent);
    match &expr.kind {
        ExpressionKind::LiteralStr(s) => {
            writeln!(res, "str: \"{s}\"")?;
        }
        ExpressionKind::LiteralNum(n) => {
            writeln!(res, "num: {n}")?;
        }
        ExpressionKind::LiteralArray(arr) => {
            writeln!(res, "array:")?;
            for expr in arr {
                write_expression_indent(res, expr, &format!("    {indent}"))?;
            }
        }
        ExpressionKind::UnaryOp(operation) => {
            match operation.op {
                UnaryOperator::Minus => write!(res, "<minus>")?,
                UnaryOperator::Plus => write!(res, "<plus>")?,
                UnaryOperator::Not => write!(res, "<not>")?,
            }
            write_expression_indent(res, &operation.operand, indent)?;
        }
        ExpressionKind::BinaryOp(operation) => {
            match operation.op {
                BinaryOperator::Dot => {
                    writeln!(res, "dot:")?;
                }
                BinaryOperator::Add => {
                    writeln!(res, "add:")?;
                }
                BinaryOperator::Sub => {
                    writeln!(res, "sub:")?;
                }
                BinaryOperator::Mul => {
                    writeln!(res, "mul:")?;
                }
                BinaryOperator::Div => {
                    writeln!(res, "div:")?;
                }
                BinaryOperator::Pow => {
                    writeln!(res, "pow:")?;
                }
                BinaryOperator::Mod => {
                    writeln!(res, "mod:")?;
                }
                BinaryOperator::Concat => {
                    writeln!(res, "concat:")?;
                }
                BinaryOperator::Eq => {
                    writeln!(res, "eq:")?;
                }
                BinaryOperator::Neq => {
                    writeln!(res, "neq:")?;
                }
                BinaryOperator::Lt => {
                    writeln!(res, "lt:")?;
                }
                BinaryOperator::Lte => {
                    writeln!(res, "lte:")?;
                }
                BinaryOperator::Gt => {
                    writeln!(res, "gt:")?;
                }
                BinaryOperator::Gte => {
                    writeln!(res, "gte:")?;
                }
                BinaryOperator::Or => {
                    writeln!(res, "or:")?;
                }
                BinaryOperator::And => {
                    writeln!(res, "and:")?;
                }
            }
            write_expression_indent(res, &operation.left, &format!("    {indent}"))?;
            write_expression_indent(res, &operation.right, &format!("    {indent}"))?;
        }
        ExpressionKind::Var(sym) => {
            writeln!(res, "var: '{}' from '{}'", sym.name, sym.path)?;
        }
        ExpressionKind::Def(def) => {
            write!(res, "def var '{}' : <", def.sym.name)?;
            if let Some(type_expr) = def.type_id {
                write_type(res, &type_expr)?;
            }
            writeln!(res, ">")?;
            if let Some(value) = &def.value {
                write_expression_indent(res, value, &format!("    {indent}"))?;
            }
        }
        ExpressionKind::Set(_) => todo!(),
        ExpressionKind::If(if_expr) => {
            writeln!(res, "if:")?;
            writeln!(res, "{indent}    cond:")?;
            write_expression_indent(res, &if_expr.cond, &format!("        {indent}"))?;
            writeln!(res, "{indent}    then:")?;
            write_expression_indent(res, &if_expr.then, &format!("        {indent}"))?;        
        }
        ExpressionKind::IfElse(if_else) => {
            writeln!(res, "if:")?;
            writeln!(res, "{indent}    cond:")?;
            write_expression_indent(res, &if_else.cond, &format!("        {indent}"))?;
            writeln!(res, "{indent}    then:")?;
            write_expression_indent(res, &if_else.then, &format!("        {indent}"))?;
            writeln!(res, "{indent}    else:")?;
            write_expression_indent(res, &if_else.other, &format!("        {indent}"))?;
        }
        ExpressionKind::For(for_in) => {
            writeln!(res, "for '{}' in:", for_in.sym.name)?;
            write_expression_indent(res, &for_in.collection, &format!("    {indent}"))?;
            writeln!(res, "{indent}    do:")?;
            write_expression_indent(res, &for_in.collection, &format!("        {indent}"))?;
        }
        ExpressionKind::Loop(loop_expr) => {
            writeln!(res, "loop:")?;
            write_expression_indent(res, &loop_expr.body, &format!("    {indent}"))?;
        }
        ExpressionKind::Break => {
            writeln!(res, "break")?;
        }
        ExpressionKind::Continue => {
            writeln!(res, "continue")?;
        }
        ExpressionKind::Fun(fun) => {
            writeln!(res, "function:")?;
            write_params(res, &fun.params, indent)?;
            write_expression_indent(res, &fun.body, &format!("    {indent}"))?;
        }
        ExpressionKind::Record(rec) => {
            writeln!(res, "def record: '{}'", rec.name.name)?;
            write_params(res, &rec.params, indent)?;
        }
        ExpressionKind::Call(call) => {
            writeln!(res, "calling:")?;
            writeln!(res, "{indent}    expression:")?;
            write_expression_indent(res, &call.callee, &format!("        {indent}"))?;
            writeln!(res, "{indent}    with arguments:")?;
            for arg in &call.args {
                write_expression_indent(res, &arg, &format!("        {indent}"))?;
            }
        }
        ExpressionKind::Index(index) => {
            writeln!(res, "indexing:")?;
            writeln!(res, "{indent}    expression:")?;
            write_expression_indent(res, &index.indexed, &format!("        {indent}"))?;
            writeln!(res, "{indent}    with indices:")?;
            for index in &index.indices {
                write_expression_indent(res, &index, &format!("        {indent}"))?;
            }
        }
        ExpressionKind::Seq(seq) => {
            writeln!(res, "seq:")?;
            for expr in seq {
                write_expression_indent(res, expr, &format!("    {indent}"))?;
            }
        }
    }
    Ok(())
}

fn expression_to_string(expr: &Expression) -> Result<String, Box<dyn Error>> {
    let mut res = String::new();
    write_expression_indent(&mut res, expr, "")?;
    Ok(res)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let help = "You need to pass a file path as argument";
    match args.as_slice() {
        [_, cmd, file_name] if cmd == "-f" => {
            let input = &*std::fs::read_to_string(file_name)?;
            println!("parsed:\n {}", expression_to_string(&parse_string(&Code { origin: File(PathBuf::from(file_name)), text: input })?)?);
        }
        [_, cmd, code] if cmd == "-i" => {
            println!("parsed:\n {}", expression_to_string(&parse_string(&Code { origin: Interactive, text: code })?)?);
        }
        _ => println!("{help}"),
    }
    Ok(())
}
