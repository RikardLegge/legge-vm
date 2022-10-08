use std::fmt;

macro_rules! collect_while {
    ($self:ident, $val:pat) => {{
        let start = $self.index;
        while let Some(ch) = $self.peek() {
            match ch {
                $val => $self.next(),
                _ => break,
            };
        }
        $self.get_str(start)
    }};
}

macro_rules! collect_until {
    ($self:ident, $val:pat) => {{
        let start = $self.index;
        while let Some(ch) = $self.peek() {
            match ch {
                $val => break,
                _ => $self.next(),
            };
        }
        $self.get_str(start)
    }};
}

pub fn from_chars(iter: &str) -> Vec<Token> {
    let mut parser = Tokenizer::new(iter);

    while let Some(ch) = parser.peek_ignore_whitespace() {
        parser.start_token();
        let tp = parser.parse_global(ch);
        parser.end_token(tp);
    }
    parser.tokens
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Hash, Eq)]
pub struct TokenID(usize);
impl TokenID {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct TokenSourceInfo {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub id: TokenID,
    pub source: TokenSourceInfo,
    pub tp: TokenType<'a>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum KeyName {
    Import,
    Type,
    This,
    If,
    Else,
    True,
    False,
    Fn,
    Return,
    Loop,
    Break,
    Continue,
}

impl fmt::Display for KeyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use KeyName::*;
        match self {
            Import => write!(f, "import"),
            Type => write!(f, "type"),
            This => write!(f, "this"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOP {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    GEq,
    LEq,
}

impl TryFrom<char> for ArithmeticOP {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let op = match value {
            '+' => ArithmeticOP::Add,
            '-' => ArithmeticOP::Sub,
            '*' => ArithmeticOP::Mul,
            '/' => ArithmeticOP::Div,
            _ => Err(())?,
        };
        Ok(op)
    }
}

impl TryFrom<&str> for ArithmeticOP {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let op = match value {
            "+" => ArithmeticOP::Add,
            "-" => ArithmeticOP::Sub,
            "*" => ArithmeticOP::Mul,
            "/" => ArithmeticOP::Div,
            "==" => ArithmeticOP::Eq,
            ">=" => ArithmeticOP::GEq,
            "<=" => ArithmeticOP::LEq,
            _ => Err(())?,
        };
        Ok(op)
    }
}

impl fmt::Display for ArithmeticOP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType<'a> {
    Comment(&'a str),
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
    Name(&'a str),
    KeyName(KeyName),
    String(&'a str),
    Int(isize, &'a str),
    Float(f64, &'a str),
    Op(ArithmeticOP),
}

impl TryFrom<&str> for KeyName {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let keyname = match value {
            "fn" => KeyName::Fn,
            "return" => KeyName::Return,
            "if" => KeyName::If,
            "else" => KeyName::Else,
            "loop" => KeyName::Loop,
            "self" => KeyName::This,
            "break" => KeyName::Break,
            "continue" => KeyName::Continue,
            "import" => KeyName::Import,
            "true" => KeyName::True,
            "type" => KeyName::Type,
            "false" => KeyName::False,
            _ => Err(())?,
        };
        Ok(keyname)
    }
}

impl<'a> fmt::Display for TokenType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        match self {
            Comment(comment) => write!(f, "// {comment}"),
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
            Name(ident) => write!(f, "{ident}"),
            KeyName(key) => write!(f, "{key}"),
            String(str) => write!(f, "\"{str}\""),
            Op(op) => write!(f, "{op}"),
            Int(_, str_rep) => write!(f, "{str_rep}"),
            Float(_, str_rep) => write!(f, "{str_rep}"),
        }
    }
}

pub struct Tokenizer<'a> {
    iter: &'a str,
    last_id: usize,
    tokens: Vec<Token<'a>>,
    index: usize,
    line_index: usize,
    start: usize,
    line_number: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(iter: &str) -> Tokenizer {
        Tokenizer {
            iter,
            tokens: vec![],
            start: 0,
            line_number: 1,
            last_id: 0,
            line_index: 0,
            index: 0,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.iter
            .as_bytes()
            .get(self.index)
            .map(|byte| char::from_u32(*byte as u32).unwrap())
    }

    fn get_str(&self, start: usize) -> &'a str {
        let end = self.index - 1;
        self.iter.get(start..=end).unwrap()
    }

    fn peek_ignore_whitespace(&mut self) -> Option<char> {
        loop {
            let ch = self.peek()?;
            match ch {
                '\n' => {
                    self.next();
                    self.line_number += 1;
                    self.line_index = 0;
                }
                ' ' | '\t' => {
                    self.next();
                }
                ch => {
                    break Some(ch);
                }
            };
        }
    }

    fn next(&mut self) -> Option<char> {
        match self.peek() {
            None => None,
            Some(c) => {
                self.line_index += 1;
                self.index += 1;
                Some(c)
            }
        }
    }

    fn start_token(&mut self) {
        self.start = self.line_index;
    }
    fn end_token(&mut self, tp: TokenType<'a>) {
        let start = self.start;
        let end = self.line_index;
        let line = self.line_number;
        let id = TokenID::new(self.last_id);
        self.last_id += 1;
        let token = Token {
            id,
            source: TokenSourceInfo { line, start, end },
            tp,
        };
        self.tokens.push(token);
    }

    fn undo_token(&mut self) -> TokenType<'a> {
        let token = self.tokens.pop().unwrap();

        self.start = token.source.start;
        self.last_id -= 1;

        token.tp
    }

    fn parse_global<'b>(&'b mut self, ch: char) -> TokenType<'a> {
        match ch {
            '0'..='9' => self.parse_number(),
            '+' | '*' => self.parse_arithmetic_op(),
            '-' => self.parse_return_type_or_subtract(),
            '/' => self.parse_comment_or_div(),
            'a'..='z' | 'A'..='Z' | '_' => self.parse_name(),
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
        }
    }

    fn parse_return_type_or_subtract(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '-');
        match self.peek() {
            Some('>') => {
                self.next().unwrap();
                TokenType::ReturnTypes
            }
            _ => TokenType::Op(ArithmeticOP::Sub),
        }
    }

    fn parse_string(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '"');
        let string = collect_until!(self, '"');
        assert_eq!(self.next().unwrap(), '"');
        TokenType::String(string)
    }

    fn parse_assignment_or_eq(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '=');
        match self.peek() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::Eq)
            }
            _ => TokenType::Assignment,
        }
    }

    fn parse_greater(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '>');
        match self.peek() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::GEq)
            }
            _ => unimplemented!(),
        }
    }

    fn parse_lesser(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '<');
        match self.peek_ignore_whitespace() {
            Some('=') => {
                self.next().unwrap();
                TokenType::Op(ArithmeticOP::LEq)
            }
            _ => unimplemented!(),
        }
    }

    fn parse_declaration_or_type(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), ':');
        match self.peek_ignore_whitespace() {
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

    fn parse_import(&mut self) -> TokenType<'a> {
        self.end_token(TokenType::KeyName(KeyName::Import));
        let mut path = Vec::new();
        // If the first token is a dot, it is a local import
        let mut is_local = None;
        let tp = loop {
            if let Some(ch) = self.peek_ignore_whitespace() {
                self.start_token();
                let tp = self.parse_global(ch);
                match &tp {
                    TokenType::Name(part) => {
                        path.push(part.to_string());
                        is_local.get_or_insert(false);
                    }
                    TokenType::Dot => {
                        is_local.get_or_insert(true);
                    }
                    _ => break tp,
                }
                self.end_token(tp);
            } else {
                break self.undo_token();
            }
        };
        tp
    }

    fn parse_name(&mut self) -> TokenType<'a> {
        let name = collect_while!(self, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_');
        match KeyName::try_from(name.as_ref()) {
            Ok(KeyName::Import) => self.parse_import(),
            Ok(keyname) => TokenType::KeyName(keyname),
            Err(()) => TokenType::Name(name),
        }
    }

    fn parse_comment_or_div(&mut self) -> TokenType<'a> {
        assert_eq!(self.next().unwrap(), '/');
        match self.peek() {
            Some('/') => {
                self.next().unwrap();
                let comment = collect_until!(self, '\n');
                TokenType::Comment(comment)
            }
            _ => TokenType::Op(ArithmeticOP::Div),
        }
    }

    fn parse_arithmetic_op(&mut self) -> TokenType<'a> {
        let op = self.next().unwrap();
        match ArithmeticOP::try_from(op) {
            Ok(op) => TokenType::Op(op),
            Err(()) => panic!("Encountered invalid character in number scope '{}'", op),
        }
    }

    fn parse_number(&mut self) -> TokenType<'a> {
        let start = self.index;
        let mut int = 0 as isize;
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    int *= 10;
                    let digit = self.next().unwrap().to_digit(10).unwrap() as isize;
                    int += digit;
                }
                _ => break,
            };
        }
        match self.peek() {
            Some('.') => {
                self.next().unwrap();
                match self.peek() {
                    Some('0'..='9') => match self.parse_number() {
                        TokenType::Int(decimal, _) => {
                            let decimal = decimal as f64;
                            let pow = (decimal + 1.0).log10().ceil() as u32;
                            let divisor = (10 as usize).pow(pow);
                            let val = int as f64 + decimal / divisor as f64;
                            TokenType::Float(val, self.get_str(start))
                        }
                        _ => unimplemented!(),
                    },
                    _ => TokenType::Float(int as f64, self.get_str(start)),
                }
            }
            _ => TokenType::Int(int, self.get_str(start)),
        }
    }
}
