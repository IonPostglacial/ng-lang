use std::fmt::Debug;

use crate::lang::lexing::{CodeLocation, Code, CodeSpan, Keyword, Lexer, ParensKind, Token, TokenKind};
use internment::Intern;

use super::lexing::Punctuation;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Dot,
    Add, Sub, Mul, Div, Pow, Mod, Concat,
    Eq, Neq, Lt, Lte, Gt, Gte, Or, And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator { Minus, Plus, Not }

impl BinaryOperator {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOperator::Or => (1, 2),
            BinaryOperator::And => (3, 4),
            BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Lte | BinaryOperator::Gte |
            BinaryOperator::Eq | BinaryOperator::Neq => (5, 6),
            BinaryOperator::Concat | BinaryOperator::Add | BinaryOperator::Sub => (7, 8),
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => (9, 10),
            BinaryOperator::Pow => (11, 12),
            BinaryOperator::Dot => (13, 14),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation<'a> {
    pub op: UnaryOperator,
    pub operand: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation<'a> {
    pub op: BinaryOperator,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub path: String,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct CallExpr<'a> { pub callee: Expression<'a>, pub args: Vec<Expression<'a>> }

#[derive(Clone, Debug)]
pub struct IndexExpr<'a> { pub indexed: Expression<'a>, pub indices: Vec<Expression<'a>> }

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeExpr {
    pub sym: Intern<Symbol>,
    pub nullable: bool,
    pub args: Vec<Intern<TypeExpr>>,
}

#[derive(Clone, Debug)]
pub struct DefExpr<'a> {
    pub sym: Intern<Symbol>,
    pub type_id: Option<Intern<TypeExpr>>,
    pub value: Option<Expression<'a>>,
}

#[derive(Clone, Debug)]
pub struct SetExpr<'a> {
    pub sym: Intern<Symbol>,
    pub value: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct IfExpr<'a> {
    pub cond: Expression<'a>,
    pub then: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct IfElseExpr<'a> {
    pub cond: Expression<'a>,
    pub then: Expression<'a>,
    pub other: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct ForExpr<'a> {
    pub sym: Intern<Symbol>,
    pub collection: Expression<'a>,
    pub body: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct LoopExpr<'a> {
    pub body: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Intern<Symbol>,
    pub type_id: Option<Intern<TypeExpr>>,
}

#[derive(Clone, Debug)]
pub struct FunExpr<'a> {
    pub params: Vec<Param>,
    pub body: Expression<'a>,
}

#[derive(Clone, Debug)]
pub struct RecordExpr {
    pub name: Intern<Symbol>,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
    LiteralStr(&'a str),
    LiteralNum(i64),
    LiteralArray(Vec<Expression<'a>>),
    UnaryOp(Box<UnaryOperation<'a>>),
    BinaryOp(Box<BinaryOperation<'a>>),
    Var(Intern<Symbol>),
    Def(Box<DefExpr<'a>>),
    Set(Box<SetExpr<'a>>),
    If(Box<IfExpr<'a>>),
    IfElse(Box<IfElseExpr<'a>>),
    For(Box<ForExpr<'a>>),
    Loop(Box<LoopExpr<'a>>),
    Break,
    Continue,
    Fun(Box<FunExpr<'a>>),
    Record(Box<RecordExpr>),
    Call(Box<CallExpr<'a>>),
    Index(Box<IndexExpr<'a>>),
    Seq(Vec<Expression<'a>>),
}

#[derive(Debug, Clone)]
pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub span: CodeSpan,
}

#[derive(Debug, Clone)]
pub enum ParsingErrorKind {
    WrongToken(String),
    UnknownOperator(String),
    UnexpectedEnd,
}

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub span: CodeLocation,
    pub kind: ParsingErrorKind,
}

fn token_to_operator(op: &Token, code: &Code) -> Result<BinaryOperator, ParsingError> {
    match op.str(code.text) {
        "." => Ok(BinaryOperator::Dot),
        "+" => Ok(BinaryOperator::Add),
        "-" => Ok(BinaryOperator::Sub),
        "*" => Ok(BinaryOperator::Mul),
        "/" => Ok(BinaryOperator::Div),
        "^" => Ok(BinaryOperator::Pow),
        "%" => Ok(BinaryOperator::Mod),
        "=" => Ok(BinaryOperator::Eq),
        "!=" => Ok(BinaryOperator::Neq),
        "<" => Ok(BinaryOperator::Lt),
        "<=" => Ok(BinaryOperator::Lte),
        ">" => Ok(BinaryOperator::Gt),
        ">=" => Ok(BinaryOperator::Gte),
        "or" => Ok(BinaryOperator::Or),
        "and" => Ok(BinaryOperator::And),
        "&" => Ok(BinaryOperator::Concat),
        _ => return Err(ParsingError { 
            span: op.location(&code.origin), 
            kind: ParsingErrorKind::UnknownOperator(op.str(code.text).to_string()) 
        })
    }
}

fn parse_operation<'a>(lexer: &mut Lexer, left: Expression<'a>, op: Token, code: &'a Code, min_bp: u8) -> Result<Expression<'a>, ParsingError> {
    let mut expression = left;
    let mut op_tok = op;
    loop {
        let op = token_to_operator(&op_tok, code)?;
        let (l_bp, r_bp) = op.binding_power();
        if l_bp < min_bp {
            break;
        }
        lexer.next(code);
        let right = parse_tokens(lexer, code, r_bp)?;
        let span = CodeSpan { start: expression.span.start, end: right.span.end };
        let operation = BinaryOperation { op, left: expression, right };
        expression = Expression { kind: ExpressionKind::BinaryOp(Box::new(operation)), span };
        if let Some(tok) = lexer.peek(code) {            
            match tok.kind {
                TokenKind::Operator => {
                    op_tok = tok
                }
                _ => break,
            }
        } else {
            break;
        }
    }
    Ok(expression)
}

fn parse_symbol(src: &str) -> Intern<Symbol> {
    Intern::new(Symbol { path: String::new(), name: src.to_string() })
}

fn parse_word<'a>(lexer: &mut Lexer, code: &'a Code) ->  Result<Intern<Symbol>, ParsingError> {
    let start = lexer.current_position();
    let tok = expect_next_token(lexer, code, start)?;
    match tok.kind {
        TokenKind::Word => Ok(parse_symbol(tok.str(code.text))),
        kind => Err(ParsingError { 
            span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } }, 
            kind: ParsingErrorKind::WrongToken(format!("Expected word, got: {:?}", kind)) 
        })
    }
}

fn parse_sep_delimited_expressions<'a>(lexer: &mut Lexer, code: &'a Code, parens: ParensKind, sep: Punctuation, start: usize) ->  Result<Vec<Expression<'a>>, ParsingError> {
    let mut expressions: Vec<Expression> = Vec::new();
    while let Some(tok) = lexer.peek(code) {
        match tok.kind {
            TokenKind::Close(p) if p == parens => {
                lexer.next(code);
                break;
            }
            _ => {
                let inter = expect_peek_token(lexer, code, start)?;
                if inter.kind == TokenKind::Punctuation(sep) {
                    lexer.next(code);
                } else {
                    expressions.push(parse_tokens(lexer, code, 0)?)
                }
            }
        }
    }
    Ok(expressions)
}

fn parse_prefix_operator<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8, token: &Token) -> Result<UnaryOperation<'a>, ParsingError> {
    Ok(UnaryOperation { 
        op: match token.str(code.text) {
            "+" => UnaryOperator::Plus,
            "-" => UnaryOperator::Minus,
            "not" => UnaryOperator::Not,
            tok => return Err(ParsingError { 
                span: token.location(&code.origin), 
                kind: ParsingErrorKind::UnknownOperator(tok.to_string()) 
            }),
        },
        operand: parse_tokens(lexer, code, min_bp)?,
    })
}

fn parse_parens_expression<'a>(lexer: &mut Lexer, code: &'a Code, start: usize, parens: ParensKind) -> Result<Expression<'a>, ParsingError> {
    match parens {
        crate::lang::lexing::ParensKind::Regular => {
            parse_tokens(lexer, code, 0)
        }
        crate::lang::lexing::ParensKind::Squigly => {
            let expressions = parse_sep_delimited_expressions(lexer, code, parens, Punctuation::Semicolon, start)?;
            Ok(Expression { 
                kind: ExpressionKind::Seq(expressions), 
                span: CodeSpan { start, end: lexer.current_position() } 
            })
        }
        crate::lang::lexing::ParensKind::Square => {
            let expressions = parse_sep_delimited_expressions(lexer, code, parens, Punctuation::Comma, start)?;
            Ok(Expression { 
                kind: ExpressionKind::LiteralArray(expressions), 
                span: CodeSpan { start, end: lexer.current_position() } 
            })
        }
    }
}

fn parse_def_expression<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8, start: usize) -> Result<Expression<'a>, ParsingError> {
    let sym = parse_word(lexer, code)?;
    let type_id = try_parse_type(lexer, code, start)?;
    let eq_or_sep = expect_next_token(lexer, code, start)?;
    let value = match eq_or_sep.kind {
        TokenKind::Operator if token_to_operator(&eq_or_sep, code)? == BinaryOperator::Eq => {
            Some(parse_tokens(lexer, code, min_bp)?)
        }
        TokenKind::Punctuation(Punctuation::Semicolon) => None,
        kind => {
            Err(ParsingError { 
                span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } },
                kind: ParsingErrorKind::WrongToken(format!("Expected word, got: {:?}", kind))
            })?
        }
    };
    Ok(Expression { 
        kind: ExpressionKind::Def(Box::new(DefExpr { sym, type_id, value })), 
        span: CodeSpan { start, end: lexer.current_position() } 
    })
}

fn parse_set_expression<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8, start: usize) -> Result<Expression<'a>, ParsingError> {
    let sym = parse_word(lexer, code)?;
    let eq = expect_next_token(lexer, code, start)?;
    if eq.str(code.text) != "=" {
        return Err(ParsingError { 
            span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } },
            kind: ParsingErrorKind::WrongToken(format!("Expected '=', got: '{}'", eq.str(code.text)))
        })?
    }
    let value = parse_tokens(lexer, code, min_bp)?;
    Ok(Expression { 
        kind: ExpressionKind::Set(Box::new(SetExpr { sym, value })), 
        span: CodeSpan { start, end: lexer.current_position() } 
    })
}

fn parse_if<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8, start: usize) -> Result<Expression<'a>, ParsingError> {
    let cond = parse_tokens(lexer, code, min_bp)?;
    let then = parse_tokens(lexer, code, min_bp)?;
    match lexer.peek(code) {
        Some(tok) => {
            if tok.str(code.text) != "else" {
                Ok(Expression { 
                    kind: ExpressionKind::If(Box::new(IfExpr { cond, then })), 
                    span: CodeSpan { start, end: lexer.current_position() }
                })
            } else {
                lexer.next(code);
                let other = parse_tokens(lexer, code, min_bp)?;
                Ok(Expression { 
                    kind: ExpressionKind::IfElse(Box::new(IfElseExpr { cond, then, other })), 
                    span: CodeSpan { start, end: lexer.current_position() }
                })
            }
        }
        None => Ok(Expression { 
            kind: ExpressionKind::If(Box::new(IfExpr { cond, then })), 
            span: CodeSpan { start, end: lexer.current_position() }
        })
    }
}

fn parse_for<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8, start: usize) -> Result<Expression<'a>, ParsingError> {
    let sym = parse_word(lexer, code)?;
    let in_kw = expect_next_token(lexer, code, start)?;
    if in_kw.str(code.text) != "in" {
        return Err(ParsingError { 
            span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } },
            kind: ParsingErrorKind::WrongToken(format!("Expected 'in', got: '{}'", in_kw.str(code.text)))
        })?
    }
    let collection = parse_tokens(lexer, code, min_bp)?;
    let body = parse_tokens(lexer, code, min_bp)?;
    Ok(Expression { 
        kind: ExpressionKind::For(Box::new(ForExpr { sym, collection, body })), 
        span: CodeSpan { start, end: lexer.current_position() } 
    })
}

fn parse_type(lexer: &mut Lexer, code: &Code, start: usize) -> Result<Intern<TypeExpr>, ParsingError> {
    let sym = parse_word(lexer, code)?;
    let mut args: Vec<Intern<TypeExpr>> = Vec::new();
    if let Some(bracket) = lexer.peek(code) {
        if bracket.kind == TokenKind::Open(ParensKind::Square) {
            lexer.next(code);
            while let Some(tok) = lexer.peek(code) {
                match tok.kind {
                    TokenKind::Close(p) if p == ParensKind::Square => {
                        lexer.next(code);
                        break;
                    }
                    _ => {
                        let inter = expect_peek_token(lexer, code, start)?;
                        if inter.kind == TokenKind::Punctuation(Punctuation::Comma) {
                            lexer.next(code);
                        } else {
                            args.push(parse_type(lexer, code, start)?)
                        }
                    }
                }
            }
        }
    }
    Ok(Intern::new(TypeExpr { sym, nullable: false, args }))
}

fn try_parse_type(lexer: &mut Lexer, code: &Code, start: usize) -> Result<Option<Intern<TypeExpr>>, ParsingError> {
    let type_id = if let Some(maybe_colon) = lexer.peek(code) {
        if maybe_colon.kind == TokenKind::Punctuation(Punctuation::Colon) {
            lexer.next(code);
            Some(parse_type(lexer, code, start)?)
        } else {
            None
        }
    } else {
        None
    };
    Ok(type_id)
}

fn parse_param(lexer: &mut Lexer, code: &Code, start: usize) -> Result<Param, ParsingError> {
    let name = parse_word(lexer, code)?;
    let type_id = try_parse_type(lexer, code, start)?;
    Ok(Param { name, type_id })
}

fn parse_loop<'a>(lexer: &mut Lexer, code: &'a Code, start: usize) -> Result<Expression<'a>, ParsingError> {
    let body = parse_tokens(lexer, code, 0)?;
    Ok(Expression { 
        kind: ExpressionKind::Loop(Box::new(LoopExpr { body })), 
        span: CodeSpan { start, end: lexer.current_position() } 
    })
}

fn parse_break<'a>(lexer: &mut Lexer, start: usize) -> Expression<'a> {
    Expression { 
        kind: ExpressionKind::Break, 
        span: CodeSpan { start, end: lexer.current_position() } 
    }
}

fn parse_continue<'a>(lexer: &mut Lexer, start: usize) -> Expression<'a> {
    Expression { 
        kind: ExpressionKind::Continue, 
        span: CodeSpan { start, end: lexer.current_position() } 
    }
}

fn parse_params<'a>(lexer: &mut Lexer, code: &'a Code, start: usize) -> Result<Vec<Param>, ParsingError> {
    let parens = expect_next_token(lexer, code, start)?;
    if parens.str(code.text) != "(" {
        return Err(ParsingError { 
            span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } },
            kind: ParsingErrorKind::WrongToken(format!("Expected '(', got: '{}'", parens.str(code.text)))
        })?
    }
    let mut params: Vec<Param> = Vec::new();
    while let Some(tok) = lexer.peek(code) {
        match tok.kind {
            TokenKind::Close(p) if p == ParensKind::Regular => {
                lexer.next(code);
                break;
            }
            _ => {
                let inter = expect_peek_token(lexer, code, start)?;
                if inter.kind == TokenKind::Punctuation(Punctuation::Comma) {
                    lexer.next(code);
                } else {
                    params.push(parse_param(lexer, code, start)?)
                }
            }
        }
    }
    Ok(params)
}

fn parse_record<'a>(lexer: &mut Lexer, code: &'a Code, start: usize) -> Result<Expression<'a>, ParsingError> {
    let name = parse_word(lexer, code)?;
    let params = parse_params(lexer, code, start)?;
    Ok(Expression { 
        kind: ExpressionKind::Record(Box::new(RecordExpr { name, params })), 
        span: CodeSpan { start, end: lexer.current_position() } 
    })
}

fn expect_is_token(lexer: &mut Lexer, code: &Code, tok: Option<Token>, start: usize) -> Result<Token, ParsingError> {
    tok.ok_or_else(|| ParsingError { 
        span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } }, 
        kind: ParsingErrorKind::UnexpectedEnd
    })
}

fn expect_next_token(lexer: &mut Lexer, code: &Code, start: usize) -> Result<Token, ParsingError> {
    let tok = lexer.next(code);
    expect_is_token(lexer, code, tok, start)
}

fn expect_peek_token(lexer: &mut Lexer, code: &Code, start: usize) -> Result<Token, ParsingError> {
    let tok = lexer.peek(code);
    expect_is_token(lexer, code, tok, start)
}

fn parse_tokens<'a>(lexer: &mut Lexer, code: &'a Code, min_bp: u8) -> Result<Expression<'a>, ParsingError> {
    let start = lexer.current_position();
    let token = lexer.next(code).ok_or_else(|| ParsingError { 
        span: CodeLocation { origin: code.origin.clone(), 
            span: CodeSpan { 
                start: lexer.current_position(), 
                end: lexer.current_position() 
            }
        }, 
        kind: ParsingErrorKind::UnexpectedEnd
    })?;
    let mut current_expr = match token.kind {
        TokenKind::String => {
            Expression { kind: ExpressionKind::LiteralStr(token.str(code.text)), span: token.span() }
        }
        TokenKind::Number => {
            Expression { kind: ExpressionKind::LiteralNum(token.str(code.text).parse().expect("invalid number")), span: token.span() }
        }
        TokenKind::Word => {
            Expression { kind: ExpressionKind::Var(parse_symbol(token.str(code.text))), span: token.span() }
        }
        TokenKind::Operator => {
            Expression {
                kind: ExpressionKind::UnaryOp(Box::new(parse_prefix_operator(lexer, code, min_bp, &token)?)), 
                span: token.span()
            }
        }
        TokenKind::Open(parens) => {
            parse_parens_expression(lexer, code, start, parens)?
        }
        TokenKind::Close(_) => todo!(),
        TokenKind::Keyword(kw) => {
            match kw {
                Keyword::Def => parse_def_expression(lexer, code, min_bp, start)?,
                Keyword::Set => parse_set_expression(lexer, code, min_bp, start)?,
                Keyword::Fun => {
                    let params = parse_params(lexer, code, start)?;
                    let body = parse_tokens(lexer, code, min_bp)?;
                    Expression { 
                        kind: ExpressionKind::Fun(Box::new(FunExpr { params, body })), 
                        span: CodeSpan { start, end: lexer.current_position() } 
                    }
                }
                Keyword::If => parse_if(lexer, code, min_bp, start)?,
                Keyword::Else => todo!(),
                Keyword::For => parse_for(lexer, code, min_bp, start)?,
                Keyword::In => todo!(),
                Keyword::Loop => parse_loop(lexer, code, start)?,
                Keyword::Break => parse_break(lexer, start),
                Keyword::Continue => parse_continue(lexer, start),
                Keyword::Record => parse_record(lexer, code, start)?,
            }
        }
        TokenKind::Punctuation(_) => {
            return Err(ParsingError { 
                span: CodeLocation { origin: code.origin.clone(), span: CodeSpan { start, end: lexer.current_position() } }, 
                kind: ParsingErrorKind::WrongToken(format!("Unexpected token: {:?}", token.str(code.text))) 
            })
        }
    };
    while let Some(token) = lexer.peek(code) {
        match token.kind {
            TokenKind::Operator => {
                current_expr = parse_operation(lexer, current_expr, token, code, min_bp)?;
                break;
            }
            TokenKind::Open(parens) => {
                let start = lexer.current_position();
                let sep = match parens {
                    ParensKind::Regular | ParensKind::Square => Punctuation::Comma,
                    ParensKind::Squigly => return Ok(current_expr),
                };
                lexer.next(code);
                let expressions = parse_sep_delimited_expressions(lexer, code, parens, sep, start)?;
                let kind = match parens {
                    ParensKind::Regular => {
                        ExpressionKind::Call(Box::new(CallExpr { callee: current_expr, args: expressions }))
                    }
                    ParensKind::Square => {
                        ExpressionKind::Index(Box::new(IndexExpr { indexed: current_expr, indices: expressions }))
                    }
                    ParensKind::Squigly => break,
                };
                let span = CodeSpan { start, end: lexer.current_position() };
                current_expr = Expression { kind, span }
            }
            _ => {
                break;
            }
        }
    }
    Ok(current_expr)
}

pub fn parse_string<'a>(code: &'a Code) -> Result<Expression<'a>, ParsingError> {
    let mut lexer = Lexer::new();
    parse_tokens(&mut lexer, &code, 0)
}