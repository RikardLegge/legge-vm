use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::str::Chars;

pub fn from_chars(iter: Chars, size_prediction: Option<usize>) -> Vec<Token> {
    let mut iter = iter.peekable();
    let mut parser = Tokenizer::new(&mut iter);
    let mut tokens = match size_prediction {
        Some(size) => Vec::with_capacity(size / 2),
        None => Vec::new(),
    };
    while let Some(ch) = parser.peek_ignore_whitespace() {
        tokens.push(parser.parse_global(ch));
    }
    tokens
}

#[derive(Copy, Clone, PartialEq)]
pub enum ArithmeticOP {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    GEq,
    LEq,
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
            GEq => write!(f, ">="),
            LEq => write!(f, "<="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
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

#[derive(PartialEq, Copy, Clone)]
pub enum KeyName {
    Import,
    Type,
    If,
    True,
    False,
    Fn,
    Return,
    Loop,
    Break,
    Continue,
}

impl fmt::Debug for KeyName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use KeyName::*;
        match self {
            Import => write!(f, "import"),
            Type => write!(f, "type"),
            If => write!(f, "if"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Fn => write!(f, "fn"),
            Return => write!(f, "return"),
            Loop => write!(f, "loop"),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Comment(String),
    LeftCurlyBrace,
    RightCurlyBrace,
    ListSeparator,
    EndStatement,
    Dot,
    LeftBrace,
    RightBrace,
    ConstDeclaration,
    VariableDeclaration,
    TypeDeclaration,
    Assignment,
    ReturnTypes,
    Name(String),
    KeyName(KeyName),
    String(String),
    Int(isize, usize),
    Float(f64, usize, usize),
    Op(ArithmeticOP),
}
impl TokenType {
    pub fn is(&self, other: &Self) -> bool {
        use TokenType::*;
        match (self, other) {
            (Comment(..), Comment(..))
            | (Name(..), Name(..))
            | (KeyName(..), KeyName(..))
            | (String(..), String(..))
            | (Int(..), Int(..))
            | (Float(..), Float(..))
            | (Op(..), Op(..))
            | (LeftCurlyBrace, LeftCurlyBrace)
            | (ListSeparator, ListSeparator)
            | (EndStatement, EndStatement)
            | (Dot, Dot)
            | (LeftBrace, LeftBrace)
            | (RightBrace, RightBrace)
            | (ConstDeclaration, ConstDeclaration)
            | (VariableDeclaration, VariableDeclaration)
            | (TypeDeclaration, TypeDeclaration)
            | (Assignment, Assignment)
            | (ReturnTypes, ReturnTypes)
            | (RightCurlyBrace, RightCurlyBrace) => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenType {
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
            ConstDeclaration => write!(f, "::"),
            VariableDeclaration => write!(f, ":="),
            TypeDeclaration => write!(f, ":"),
            Assignment => write!(f, "="),
            ReturnTypes => write!(f, "->"),
            Dot => write!(f, "."),
            Name(ident) => write!(f, "{}", ident),
            KeyName(key) => write!(f, "{:?}", key),
            String(str) => write!(f, "\"{}\"", str),
            Op(op) => write!(f, "{:?}", op),

            // Enable nice formatting of numbers with leading zeros
            Int(num, significant) => format_number_leading_zeros(f, *num as f64, *significant, 0),
            Float(num, significant, decimal) => {
                format_number_leading_zeros(f, *num, *significant, *decimal)
            }
        }
    }
}

fn format_number_leading_zeros(
    f: &mut Formatter<'_>,
    num: f64,
    significant: usize,
    decimal: usize,
) -> fmt::Result {
    let width = (1.0 + num.log10().floor()) as usize;
    let left_pad = significant - width;
    let right_pad = num;
    write!(f, "{0:0<1$}{2:.3$}", "", left_pad, right_pad, decimal)
}

pub struct Tokenizer<'a> {
    iter: &'a mut Peekable<Chars<'a>>,
    last_id: usize,
    index: usize,
    line_number: usize,
}

impl<'a> Tokenizer<'a> {
    fn new<'b>(iter: &'b mut Peekable<Chars<'b>>) -> Tokenizer<'b> {
        Tokenizer {
            iter,
            line_number: 1,
            last_id: 0,
            index: 0,
        }
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

    fn parse_global(&mut self, ch: char) -> Token {
        let start = self.index;
        let tp = match ch {
            '0'..='9' => self.parse_number(),
            '+' | '*' => self.parse_arithmetic_op(),
            '-' => self.parse_return_type_or_subtract(),
            '/' => self.parse_comment_or_div(),
            'a'..='z' | 'A'..='Z' => self.parse_name(),
            ':' => self.parse_declaration_or_type(),
            '=' => self.parse_assignment_or_eq(),
            '>' => self.parse_greater(),
            '<' => self.parse_lesser(),
            '(' => {
                self.next().unwrap();
                TokenType::LeftBrace
            }
            ')' => {
                self.next().unwrap();
                TokenType::RightBrace
            }
            '{' => {
                self.next().unwrap();
                TokenType::LeftCurlyBrace
            }
            '}' => {
                self.next().unwrap();
                TokenType::RightCurlyBrace
            }
            ',' => {
                self.next().unwrap();
                TokenType::ListSeparator
            }
            ';' => {
                self.next().unwrap();
                TokenType::EndStatement
            }
            '.' => {
                self.next().unwrap();
                TokenType::Dot
            }
            '"' => self.parse_string(),
            _ => panic!("Encountered invalid character in global scope '{}'", ch),
        };
        let end = self.index;
        let line = self.line_number;
        self.last_id += 1;
        let id = TokenID::new(self.last_id);
        Token {
            id,
            line,
            start,
            end,
            tp,
        }
    }

    fn parse_return_type_or_subtract(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '-');
        match self.peek() {
            Some('>') => {
                self.next().unwrap();
                TokenType::ReturnTypes
            }
            _ => TokenType::Op(ArithmeticOP::Sub),
        }
    }

    fn parse_string(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '"');
        let mut string = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(_) => match self.next().unwrap() {
                    '"' => break,
                    char => string.push(char),
                },
            }
        }
        TokenType::String(string)
    }

    fn parse_assignment_or_eq(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '=');
        match self.peek() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::Eq)
            }
            _ => TokenType::Assignment,
        }
    }

    fn parse_greater(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '>');
        match self.peek() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::GEq)
            }
            _ => unimplemented!(),
        }
    }

    fn parse_lesser(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '<');
        match self.peek() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::LEq)
            }
            _ => unimplemented!(),
        }
    }

    fn parse_declaration_or_type(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), ':');
        match self.peek() {
            Some(':') => {
                self.next().unwrap();
                TokenType::ConstDeclaration
            }
            Some('=') => {
                self.next().unwrap();
                TokenType::VariableDeclaration
            }
            _ => TokenType::TypeDeclaration,
        }
    }

    fn parse_name(&mut self) -> TokenType {
        let mut name = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => name.push(self.next().unwrap()),
                _ => break,
            };
        }

        match name.as_ref() {
            "fn" => TokenType::KeyName(KeyName::Fn),
            "return" => TokenType::KeyName(KeyName::Return),
            "if" => TokenType::KeyName(KeyName::If),
            "loop" => TokenType::KeyName(KeyName::Loop),
            "break" => TokenType::KeyName(KeyName::Break),
            "continue" => TokenType::KeyName(KeyName::Continue),
            "import" => TokenType::KeyName(KeyName::Import),
            "true" => TokenType::KeyName(KeyName::True),
            "type" => TokenType::KeyName(KeyName::Type),
            "false" => TokenType::KeyName(KeyName::False),
            _ => TokenType::Name(name),
        }
    }

    fn parse_comment_or_div(&mut self) -> TokenType {
        assert_eq!(self.next().unwrap(), '/');
        match self.peek() {
            Some('/') => {
                self.next().unwrap();
                let mut comment = String::new();
                while let Some(char) = self.peek() {
                    if char == '\n' {
                        break;
                    }
                    comment.push(self.next().unwrap())
                }
                TokenType::Comment(comment)
            }
            _ => TokenType::Op(ArithmeticOP::Div),
        }
    }

    fn parse_arithmetic_op(&mut self) -> TokenType {
        let op = match self.next().unwrap() {
            '+' => ArithmeticOP::Add,
            '-' => ArithmeticOP::Sub,
            '*' => ArithmeticOP::Mul,
            '/' => ArithmeticOP::Div,
            ch => panic!("Encountered invalid character in number scope '{}'", ch),
        };
        TokenType::Op(op)
    }

    fn parse_number(&mut self) -> TokenType {
        let mut int = 0 as isize;
        let mut count = 0;
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    int *= 10;
                    let digit = self.next().unwrap().to_digit(10).unwrap() as isize;
                    int += digit;
                }
                _ => break,
            };
            count += 1;
        }
        match self.peek() {
            Some('.') => {
                self.next().unwrap();
                match self.peek() {
                    Some('0'..='9') => match self.parse_number() {
                        TokenType::Int(decimal, decimal_count) => {
                            let decimal = decimal as f64;
                            let pow = (decimal + 1.0).log10().ceil() as u32;
                            let divisor = (10 as usize).pow(pow);
                            let val = int as f64 + decimal / divisor as f64;
                            TokenType::Float(val, count, decimal_count)
                        }
                        _ => unimplemented!(),
                    },
                    _ => TokenType::Float(int as f64, count, 0),
                }
            }
            _ => TokenType::Int(int, count),
        }
    }
}
