use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftBrace,
    RightBrace,
    StaticDeclaration,
    Declaration,
    Name(String),
    String(String),
    Int(i64),
    Op(ArithmeticOp),
}

pub fn parse_tokens(iter: &mut Peekable<Chars>) -> Vec<Token> {
    let mut tokens = Vec::new();

    while iter.peek().is_some() {
        if let Some(token) = parse_global(iter) {
            tokens.push(token);
        }
    }
    tokens
}

fn parse_global(iter: &mut Peekable<Chars>) -> Option<Token> {
    let ch = *iter.peek()?;
    match ch {
        '0'...'9' => parse_number(iter),
        '-' | '+' | '*' | '/' => parse_arithmetic_op(iter),
        'a'...'z'|'A'...'Z' => parse_name(iter),
        ':' => parse_declaration(iter),
        '(' => {iter.next(); Some(Token::LeftBrace)},
        ')' => {iter.next(); Some(Token::RightBrace)},
        '{' => {iter.next(); Some(Token::LeftCurlyBrace)},
        '}' => {iter.next(); Some(Token::RightCurlyBrace)},
        '"' => parse_string(iter),
        '\n'|' '|'\t' => {iter.next(); None},
        _ => panic!("Encountered invalid character in global scope '{}'", ch)
    }
}

fn parse_string(iter: &mut Peekable<Chars>) -> Option<Token> {
    let mut string = String::new();
    assert_eq!(iter.next()?, '"');
    while iter.peek()? != &'"' {
        string.push(iter.next()?);
    };
    assert_eq!(iter.next()?, '"');
    Some(Token::String(string))
}

fn parse_declaration(iter: &mut Peekable<Chars>) -> Option<Token> {
    assert_eq!(iter.next()?, ':');
    match iter.next()? {
        ':' => Some(Token::StaticDeclaration),
        '=' => Some(Token::Declaration),
        ch => panic!("Encountered invalid character when parsing declaration: {:?}", ch)
    }


}

fn parse_name(iter: &mut Peekable<Chars>) -> Option<Token> {
    let mut name = String::new();
    while let Some(&ch) = iter.peek() {
        match ch {
            'a'...'z'|'A'...'Z'|'0'...'9'|'_' => name.push(iter.next().unwrap()),
            _ => break
        };
    }

    Some(Token::Name(name))
}

fn parse_arithmetic_op(iter: &mut Peekable<Chars>) -> Option<Token> {
    let ch = iter.next()?;
    let op = match ch {
        '+' => ArithmeticOp::Add,
        '-' => ArithmeticOp::Sub,
        '*' => ArithmeticOp::Mul,
        '/' => ArithmeticOp::Div,
        _ => panic!("Encountered invalid character in number scope '{}'", ch)
    };
    Some(Token::Op(op))
}

fn parse_number(iter: &mut Peekable<Chars>) -> Option<Token> {
    let mut num = 0 as i64;
    while let Some(&ch) = iter.peek() {
        match ch {
            '0'...'9' => {num *= 10; num += iter.next().unwrap().to_digit(10)? as i64},
            _ => break
        };
    }

    Some(Token::Int(num))
}