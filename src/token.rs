use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOP {
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
    Int(isize),
    Op(ArithmeticOP),
}

pub struct Tokenizer<'a> {
    iter: &'a mut Peekable<Enumerate<Chars<'a>>>,
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn parse(iter: Chars<'a>) -> Vec<Token> {
        let mut iter = iter.enumerate().peekable();
        let mut parser = Tokenizer {
            iter: &mut iter,
            index: 0,
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
        let (_, ch) = self.iter.peek()?;
        Some(*ch)
    }

    fn peek_ignore_whitespace(&mut self) -> Option<char> {
        loop {
            let (i, ch) = self.iter.peek()?;
            match ch {
                '\n' | ' ' | '\t' => self.next(),
                ch => {
                    self.index = *i;
                    break Some(*ch);
                }
            };
        }
    }

    fn next(&mut self) -> Option<char> {
        let (i, c) = self.iter.next()?;
        self.index = i;
        Some(c)
    }

    fn parse_global(&mut self) -> Option<Token> {
        let ch = self.peek_ignore_whitespace()?;
        let start = self.index;
        let tp = match ch {
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
        let end = self.index;
        Some(Token { tp, start, end })
    }

    fn parse_return_type_or_subtract(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, '-');
        let token = match self.peek()? {
            '>' => {
                self.next()?;
                TokenType::ReturnTypes
            }
            _ => TokenType::Op(ArithmeticOP::Sub),
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
                Some(TokenType::Op(ArithmeticOP::Eq))
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
            _ => Some(TokenType::Op(ArithmeticOP::Div)),
        }
    }

    fn parse_arithmetic_op(&mut self) -> Option<TokenType> {
        let op = match self.next()? {
            '+' => ArithmeticOP::Add,
            '-' => ArithmeticOP::Sub,
            '*' => ArithmeticOP::Mul,
            '/' => ArithmeticOP::Div,
            ch => panic!("Encountered invalid character in number scope '{}'", ch),
        };
        Some(TokenType::Op(op))
    }

    fn parse_number(&mut self) -> Option<TokenType> {
        let mut num = 0 as isize;
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    num *= 10;
                    num += self.next().unwrap().to_digit(10)? as isize
                }
                _ => break,
            };
        }

        Some(TokenType::Int(num))
    }
}
