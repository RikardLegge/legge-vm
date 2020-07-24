use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

#[derive(Debug)]
pub struct Token {
    start: usize,
    end: usize,
    pub tp: TokenType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Comment(String),
    LeftCurlyBrace,
    RightCurlyBrace,
    ListSeparator,
    EndStatement,
    LeftBrace,
    RightBrace,
    StaticDeclaration,
    Declaration,
    Assignment,
    ReturnTypes,
    Name(String),
    KeyName(String),
    String(String),
    Int(i64),
    Op(ArithmeticOp),
}

pub struct Tokenizer<'a> {
    iter: &'a mut Peekable<Enumerate<Chars<'a>>>,
    token_start_index: usize,
    current_index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn parse(iter: Chars<'a>) -> Vec<Token> {
        let mut iter = iter.enumerate().peekable();
        let mut parser = Tokenizer {
            iter: &mut iter,
            token_start_index: 0,
            current_index: 0,
        };
        let mut tokens = Vec::new();

        while parser.peek().is_some() {
            if let Some(token) = parser.parse_global() {
                tokens.push(token);
            }
        }
        tokens
    }

    fn peek(&mut self) -> Option<char> {
        match self.iter.peek() {
            Some((_, ch)) => Some(*ch),
            None => None,
        }
    }

    fn peek_ignore_whitespace(&mut self) -> Option<char> {
        loop {
            match self.peek()? {
                '\n' | ' ' | '\t' => self.next(),
                ch => return Some(ch),
            };
        }
    }

    fn next(&mut self) -> Option<char> {
        let (i, c) = self.iter.next()?;
        self.current_index = i;
        Some(c)
    }

    fn start_token(&mut self) {
        self.token_start_index = self.current_index
    }

    fn end_token(&mut self, tp: TokenType) -> Token {
        Token {
            tp,
            start: self.token_start_index,
            end: self.current_index,
        }
    }

    fn parse_global(&mut self) -> Option<Token> {
        let ch = self.peek_ignore_whitespace()?;
        self.start_token();
        let token_tp = match ch {
            '0'..='9' => self.parse_number()?,
            '+' | '*' => self.parse_arithmetic_op()?,
            '-' => self.parse_return_type_or_subtract()?,
            '/' => self.parse_comment_or_div()?,
            'a'..='z' | 'A'..='Z' => self.parse_name()?,
            ':' => self.parse_declaration()?,
            '=' => self.parse_assignment_or_eq()?,
            '(' => {
                self.next()?;
                TokenType::LeftBrace
            }
            ')' => {
                self.next()?;
                TokenType::RightBrace
            }
            '{' => {
                self.next()?;
                TokenType::LeftCurlyBrace
            }
            '}' => {
                self.next()?;
                TokenType::RightCurlyBrace
            }
            ',' => {
                self.next()?;
                TokenType::ListSeparator
            }
            ';' => {
                self.next()?;
                TokenType::EndStatement
            }
            '"' => self.parse_string()?,
            _ => panic!("Encountered invalid character in global scope '{}'", ch),
        };
        Some(self.end_token(token_tp))
    }

    fn parse_return_type_or_subtract(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, '-');
        let token = match self.peek()? {
            '>' => {
                self.next()?;
                TokenType::ReturnTypes
            }
            _ => TokenType::Op(ArithmeticOp::Sub),
        };
        Some(token)
    }

    fn parse_string(&mut self) -> Option<TokenType> {
        let mut string = String::new();
        assert_eq!(self.next()?, '"');
        while self.peek()? != '"' {
            string.push(self.next()?);
        }
        assert_eq!(self.next()?, '"');
        Some(TokenType::String(string))
    }

    fn parse_assignment_or_eq(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, '=');
        match self.peek()? {
            '=' => {
                self.next()?;
                Some(TokenType::Op(ArithmeticOp::Eq))
            }
            _ => Some(TokenType::Assignment),
        }
    }

    fn parse_declaration(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, ':');
        match self.next()? {
            ':' => Some(TokenType::StaticDeclaration),
            '=' => Some(TokenType::Declaration),
            ch => panic!(
                "Encountered invalid character when parsing declaration: {:?}",
                ch
            ),
        }
    }

    fn parse_name(&mut self) -> Option<TokenType> {
        let mut name = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => name.push(self.next().unwrap()),
                _ => break,
            };
        }

        match name.as_ref() {
            "fn" | "return" | "if" | "loop" | "break" | "continue" => {
                Some(TokenType::KeyName(name))
            }
            _ => Some(TokenType::Name(name)),
        }
    }

    fn parse_comment_or_div(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, '/');
        match self.next()? {
            '/' => {
                let mut comment = String::new();
                while self.peek()? != '\n' {
                    comment.push(self.next().unwrap())
                }
                Some(TokenType::Comment(comment))
            }
            _ => Some(TokenType::Op(ArithmeticOp::Div)),
        }
    }

    fn parse_arithmetic_op(&mut self) -> Option<TokenType> {
        let op = match self.next()? {
            '+' => ArithmeticOp::Add,
            '-' => ArithmeticOp::Sub,
            '*' => ArithmeticOp::Mul,
            '/' => ArithmeticOp::Div,
            ch => panic!("Encountered invalid character in number scope '{}'", ch),
        };
        Some(TokenType::Op(op))
    }

    fn parse_number(&mut self) -> Option<TokenType> {
        let mut num = 0 as i64;
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    num *= 10;
                    num += self.next().unwrap().to_digit(10)? as i64
                }
                _ => break,
            };
        }

        Some(TokenType::Int(num))
    }
}
