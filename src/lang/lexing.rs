use std::{str::Chars, iter::Peekable, path::Path};


#[derive(Debug, Clone)]
pub enum CodeOrigin {
    Interactive,
    File(Box<Path>)
}

#[derive(Debug, Clone)]
pub struct Code<'a> {
    pub origin: CodeOrigin,
    pub text: &'a str,
}

#[derive(Debug, Clone)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct CodeLocation {
    pub origin: CodeOrigin,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParensKind {
    Regular, Squigly, Square
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Def, Set, If, Else, For, In, Loop, Break, Continue, Fun, Record,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    Semicolon, Comma, Colon,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    String, Number, Word, Operator,
    Punctuation(Punctuation),
    Keyword(Keyword),
    Open(ParensKind), Close(ParensKind),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: CodeSpan,
}

impl Token {
    pub fn str<'a>(&self, src: &'a str) -> &'a str {
        &src[self.span.start..self.span.end]
    }

    pub fn span(&self) -> CodeSpan {
        self.span.clone()
    }

    pub fn location(&self, origin: &CodeOrigin) -> CodeLocation {
        CodeLocation { origin: origin.clone(), span: self.span.clone() }
    }
}

fn is_space(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false
    }
}

fn is_parens(ch: char) -> bool {
    match ch {
        '{' | '}' | '(' | ')' | '[' | ']' => true,
        _ => false,
    }
}

fn is_operator(ch: char) -> bool {
    match ch {
        '+' | '-' | '*' | '/' | '%' | '&' | '<' | '>' | '=' | '.' => true,
        _ => false,
    }
}

fn is_punctuation(ch: char) -> bool {
    match ch {
        ',' | ';' | ':' => true,
        _ => false,
    }
}

fn is_separator(ch: char) -> bool {
    ch == '"' || ch == '\'' || is_space(ch) || is_parens(ch) || is_operator(ch) || is_punctuation(ch)
}

#[derive(Debug)]
pub struct Lexer {
    pos: usize,
    next_token: Option<Token>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer { pos: 0, next_token: None }
    }

    pub fn current_position(&self) -> usize {
        self.pos
    }

    fn consume_char(&mut self, text: &mut Peekable<Chars<'_>>) -> Option<char> {
        let ch = text.next();
        self.pos += 1;
        ch
    }

    fn consume_while(&mut self, text: &mut Peekable<Chars<'_>>, p: fn (char) -> bool) -> Option<usize> {
        loop {
            match text.peek() {
                Some(&ch) if p(ch) => {
                    self.consume_char(text);
                }
                None | Some(_) => { return Some(self.pos); }
            }
        }
    }

    fn consume_space(&mut self, text: &mut Peekable<Chars<'_>>) {
        self.consume_while(text, is_space);
    }

    fn consume_com(&mut self, text: &mut Peekable<Chars<'_>>) {
        self.consume_while(text, |ch| ch != '\n');
        
    }

    fn consume_str(&mut self, text: &mut Peekable<Chars<'_>>) -> Option<Token> {
        let start = self.pos + 1;
        let mut end;
        loop {
            self.consume_char(text);
            end = self.consume_while(text, |ch| ch != '"')?;
            self.consume_char(text);
            match text.peek() {
                Some('"') => continue,
                None | Some(_) => break,
            }
        }
        Some(Token { kind: TokenKind::String, span: CodeSpan { start, end } })
    }

    fn consume_num(&mut self, text: &mut Peekable<Chars<'_>>) -> Option<Token> {
        let start = self.pos;
        let end = self.consume_while(text, |ch| ch == '.' || !is_separator(ch))?;
        Some(Token { kind: TokenKind::Number, span: CodeSpan { start, end } })
    }

    fn consume_word(&mut self, src: &str, text: &mut Peekable<Chars<'_>>) -> Option<Token> {
        let start = self.pos;
        let end = self.consume_while(text, |ch| !is_separator(ch))?;
        Some(Token {
            kind: match &src[start..end] {
                "or" | "and" | "not" => TokenKind::Operator,
                "def" => TokenKind::Keyword(Keyword::Def),
                "set" => TokenKind::Keyword(Keyword::Set),
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "for" => TokenKind::Keyword(Keyword::For),
                "in" => TokenKind::Keyword(Keyword::In),
                "loop" => TokenKind::Keyword(Keyword::Loop),
                "break" => TokenKind::Keyword(Keyword::Break),
                "continue" => TokenKind::Keyword(Keyword::Continue),
                "fun" => TokenKind::Keyword(Keyword::Fun),
                "record" => TokenKind::Keyword(Keyword::Record),
                _ => TokenKind::Word,
            },
            span: CodeSpan { start, end }
        })
    }

    fn consume_operator(&mut self, text: &mut Peekable<Chars<'_>>) -> Option<Token> {
        let start = self.pos;
        if let Some(ch) = self.consume_char(text) {
            if ch == '<' || ch == '>' || ch == '=' {
                if let Some(next) = text.peek() {
                    match next {
                        '=' => {
                            self.consume_char(text);
                        }
                        _ => {}
                    }
                }
            }
            let end = self.pos;
            Some(Token { kind: TokenKind::Operator, span: CodeSpan { start, end } })
        } else {
            None
        }
    }

    fn consume_parens(&mut self, text: &mut Peekable<Chars<'_>>, ch: char) -> Option<Token> {
        let start = self.pos;
        self.consume_char(text);
        let end = self.pos;
        Some(Token { 
            span: CodeSpan { start, end },
            kind: match ch {
                '(' => TokenKind::Open(ParensKind::Regular),
                '{' => TokenKind::Open(ParensKind::Squigly),
                '[' => TokenKind::Open(ParensKind::Square),
                ')' => TokenKind::Close(ParensKind::Regular),
                '}' => TokenKind::Close(ParensKind::Squigly),
                ']' => TokenKind::Close(ParensKind::Square),
                _ => todo!(),
            } })
    }

    fn consume_punctuation(&mut self, text: &mut Peekable<Chars<'_>>, ch: char) -> Option<Token> {
        let start = self.pos;
        self.consume_char(text);
        let end = self.pos;
        Some(Token { span: CodeSpan { start, end },
            kind: match ch {
                ';' => TokenKind::Punctuation(Punctuation::Semicolon),
                ',' => TokenKind::Punctuation(Punctuation::Comma),
                ':' => TokenKind::Punctuation(Punctuation::Colon),
                _ => todo!(),
            } })
    }

    pub fn peek(&mut self, code: &Code) -> Option<Token> {
        let next = self.next(code);
        self.next_token = next.clone();
        next
    }

    pub fn next(&mut self, code: &Code) -> Option<Token> {
        if let Some(_) = &self.next_token {
            return self.next_token.take()
        }
        let mut text = code.text[self.pos..].chars().peekable();
        match text.peek() {
            Some('#') => {
                self.consume_com(&mut text);
                self.next(code)
            }
            Some('"') => self.consume_str(&mut text),
            Some('0'..='9') => self.consume_num(&mut text),
            Some(&ch) if is_space(ch) => {
                self.consume_space(&mut text);
                self.next(code)
            }
            Some(&ch) if is_punctuation(ch) => {
                self.consume_punctuation(&mut text, ch)
            }
            Some(&ch) if is_operator(ch) => {
                self.consume_operator(&mut text)
            }
            Some(&ch) if is_parens(ch) => {
                self.consume_parens(&mut text, ch)
            }
            Some(_) => self.consume_word(code.text, &mut text),
            None => None,
        }
    }
}