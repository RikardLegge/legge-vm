use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Copy, Clone, PartialEq)]
pub enum ArithmeticOP {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

impl fmt::Debug for ArithmeticOP {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ArithmeticOP::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Eq => write!(f, "=="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenID(usize);
impl TokenID {
    fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub id: TokenID,
    pub line: usize,
    pub start: usize,
    pub end: usize,
    pub tp: TokenType,
}

#[derive(PartialEq, Clone)]
pub enum TokenType {
    Comment(String),
    LeftCurlyBrace,
    RightCurlyBrace,
    ListSeparator,
    EndStatement,
    LeftBrace,
    RightBrace,
    StaticDeclaration,
    VariableDeclaration,
    TypeDeclaration,
    Assignment,
    ReturnTypes,
    Name(String),
    KeyName(String),
    String(String),
    Int(isize),
    Op(ArithmeticOP),
}

impl fmt::Debug for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        match self {
            Comment(str) => write!(f, "// {}", str),
            LeftCurlyBrace => write!(f, "{{"),
            RightCurlyBrace => write!(f, "}}"),
            ListSeparator => write!(f, ","),
            EndStatement => write!(f, ";"),
            LeftBrace => write!(f, "("),
            RightBrace => write!(f, ")"),
            StaticDeclaration => write!(f, "::"),
            VariableDeclaration => write!(f, ":="),
            TypeDeclaration => write!(f, ":"),
            Assignment => write!(f, "="),
            ReturnTypes => write!(f, "->"),
            Name(ident) => write!(f, "{}", ident),
            KeyName(key) => write!(f, "{}", key),
            String(str) => write!(f, "{}", str),
            Int(num) => write!(f, "{}", num),
            Op(op) => write!(f, "{:?}", op),
        }
    }
}

pub struct Tokenizer<'a> {
    iter: &'a mut Peekable<Chars<'a>>,
    last_id: usize,
    index: usize,
    line_number: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn parse(iter: Chars<'a>) -> Vec<Token> {
        let mut iter = iter.peekable();
        let mut parser = Tokenizer {
            iter: &mut iter,
            line_number: 1,
            last_id: 0,
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
        let ch = self.iter.peek()?;
        Some(*ch)
    }

    fn peek_ignore_whitespace(&mut self) -> Option<char> {
        loop {
            let ch = self.iter.peek()?;
            match ch {
                '\n' => {
                    self.next();
                    self.line_number += 1;
                    self.index = 0;
                }
                ' ' | '\t' => {
                    self.next();
                }
                ch => {
                    break Some(*ch);
                }
            };
        }
    }

    fn next(&mut self) -> Option<char> {
        self.index += 1;
        Some(self.iter.next()?)
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
            ':' => self.parse_declaration_or_type()?,
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
        self.last_id += 1;
        let id = TokenID::new(self.last_id);
        Some(Token {
            id,
            line: self.line_number,
            start,
            end,
            tp,
        })
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

    fn parse_declaration_or_type(&mut self) -> Option<TokenType> {
        assert_eq!(self.next()?, ':');
        match self.peek()? {
            ':' => {
                self.next()?;
                Some(TokenType::StaticDeclaration)
            }
            '=' => {
                self.next()?;
                Some(TokenType::VariableDeclaration)
            }
            _ => Some(TokenType::TypeDeclaration),
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
            "fn" | "return" | "if" | "loop" | "break" | "continue" | "import" | "true"
            | "false" => Some(TokenType::KeyName(name)),
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
