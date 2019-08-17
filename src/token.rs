use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    Op(ArithmeticOp)
}

pub struct Tokenizer<'a> {
    iter: &'a mut Peekable<Chars<'a>>
}

impl<'a> Tokenizer<'a> {
    pub fn parse(iter: &'a mut Peekable<Chars<'a>>) -> Vec<Token> {
        let mut parser = Tokenizer {iter};
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
            Some(ch) => Some(*ch),
            None => None
        }
    }

    fn peek_ignore_whitespace(&mut self) -> Option<char> {
        loop {
            match self.peek()? {
                '\n'|' '|'\t' => self.next(),
                ch => {return Some(ch)}
            };
        }
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn parse_global(&mut self) -> Option<Token> {
        let ch = self.peek_ignore_whitespace()?;
        match ch {
            '0'...'9' => self.parse_number(),
            '+' | '*' => self.parse_arithmetic_op(),
            '-'  => self.parse_return_type(),
            '/' => self.parse_comment(),
            'a'...'z'|'A'...'Z' => self.parse_name(),
            ':' => self.parse_declaration(),
            '=' => self.parse_assignment(),
            '(' => {self.next(); Some(Token::LeftBrace)},
            ')' => {self.next(); Some(Token::RightBrace)},
            '{' => {self.next(); Some(Token::LeftCurlyBrace)},
            '}' => {self.next(); Some(Token::RightCurlyBrace)},
            ',' => {self.next(); Some(Token::ListSeparator)},
            ';' => {self.next(); Some(Token::EndStatement)},
            '"' => self.parse_string(),
            _ => panic!("Encountered invalid character in global scope '{}'", ch)
        }
    }

    fn parse_return_type(&mut self) -> Option<Token> {
        assert_eq!(self.next()?, '-');
        match self.peek()? {
            '>' => {
                self.next()?;
                Some(Token::ReturnTypes)
            },
            _ => self.parse_arithmetic_op(),
        }
    }

    fn parse_string(&mut self) -> Option<Token> {
        let mut string = String::new();
        assert_eq!(self.next()?, '"');
        while self.peek()? != '"' {
            string.push(self.next()?);
        };
        assert_eq!(self.next()?, '"');
        Some(Token::String(string))
    }

    fn parse_assignment(&mut self) -> Option<Token> {
        assert_eq!(self.next()?, '=');
        match self.peek()? {
            '=' => {
                self.next()?;
                Some(Token::Op(ArithmeticOp::Eq))
            },
            _ => Some(Token::Assignment)
        }
    }

    fn parse_declaration(&mut self) -> Option<Token> {
        assert_eq!(self.next()?, ':');
        match self.next()? {
            ':' => Some(Token::StaticDeclaration),
            '=' => Some(Token::Declaration),
            ch => panic!("Encountered invalid character when parsing declaration: {:?}", ch)
        }
    }

    fn parse_name(&mut self) -> Option<Token> {
        let mut name = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                'a'...'z'|'A'...'Z'|'0'...'9'|'_' => name.push(self.next().unwrap()),
                _ => break
            };
        }

        let token = match name.as_ref() {
            "fn" | "return" => Token::KeyName(name),
            _ => Token::Name(name)
        };
        Some(token)
    }

    fn parse_comment(&mut self) -> Option<Token> {
        assert_eq!(self.next()?, '/');
        match self.next()? {
            '/' => {
                while self.next()? != '\n' {}
                None
            },
            _ => Some(Token::Op(ArithmeticOp::Div))
        }
    }

    fn parse_arithmetic_op(&mut self) -> Option<Token> {
        let op = match self.next()? {
            '+' => ArithmeticOp::Add,
            '-' => ArithmeticOp::Sub,
            '*' => ArithmeticOp::Mul,
            '/' => ArithmeticOp::Div,
            ch => panic!("Encountered invalid character in number scope '{}'", ch)
        };
        Some(Token::Op(op))
    }

    fn parse_number(&mut self) -> Option<Token> {
        let mut num = 0 as i64;
        while let Some(ch) = self.peek() {
            match ch {
                '0'...'9' => {num *= 10; num += self.next().unwrap().to_digit(10)? as i64},
                _ => break
            };
        }

        Some(Token::Int(num))
    }
}





