use std::{
    env, fs,
    io::{self, Write},
    process,
};

struct Lox {
    has_error: bool,
}

struct Scanner {
    source: String,
    tokens: Vec<Token>,
    line: usize,
    start: usize,
    current: usize,
}
impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            line: 1,
            start: 0,
            current: 0,
        }
    }

    fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });

        self.tokens
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        if let Some(c) = self.advance() {
            match c.to_string().as_str() {
                "(" => self.add_token(TokenType::LeftParen),
                ")" => self.add_token(TokenType::RightParen),
                "{" => self.add_token(TokenType::LeftBrace),
                "}" => self.add_token(TokenType::RightBrace),
                _ => {}
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        if self.current < self.source.len() {
            let ch = self.source.chars().by_ref().nth(self.current);
            ch
        } else {
            None
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_internal(token_type, None)
    }

    fn add_token_internal(&mut self, token_type: TokenType, literal: Option<String>) {
        let text = self.source.chars().nth(self.current).unwrap();
        self.tokens
            .push(Token::new(token_type, text.to_string(), literal, self.line))
    }
}

impl Lox {
    fn new() -> Self {
        Lox { has_error: false }
    }

    pub fn run_file(&mut self, path: &str) {
        let content = fs::read_to_string(path).expect("Couldn't read file");
        self.run(&content)
    }

    pub fn run(&mut self, content: &String) {
        let scanner = Scanner::new(content.to_owned());

        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:#?}", token)
        }
    }

    pub fn error(&mut self, line: i32, message: &str) {
        self.report(line, "".to_string(), message);
    }

    pub fn report(&mut self, line: i32, r#where: String, message: &str) {
        eprintln!("[line {} Error {} : {}", line, r#where, message);
    }

    pub fn run_prompt(&mut self) {
        let mut input = String::new();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            io::stdin().read_line(&mut input).unwrap();

            if input.trim().is_empty() {
                break;
            };

            println!("{:?}", input.trim());
            self.run(&input);
            input.clear();
            self.has_error = false
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    match args.len() {
        1 => lox.run_prompt(),
        2 => lox.run_file(&args[1]),
        _ => {
            println!("Usage: jlox [script]");
            process::exit(64);
        }
    }
    // dbg!(args);
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
    line: usize,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, literal: Option<String>, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    fn to_string(&mut self) -> String {
        format!(
            "{:?} {} {:?}",
            self.token_type,
            self.lexeme,
            Some(self.literal.clone())
        )
    }
}

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Band,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}
