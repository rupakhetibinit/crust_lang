use std::{
    any::Any,
    env,
    fmt::{format, Error},
    fs,
    io::{self, Write},
    process,
};

struct Lox {
    hasError: bool,
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
            token_type: TokenType::EOF,
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
        let (_, c) = self.advance().unwrap();
        match c.to_string().as_str() {
            "(" => self.add_token(TokenType::LEFT_PAREN),
            ")" => self.add_token(TokenType::RIGHT_PAREN),
            _ => {}
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.current += 1;
        self.source.char_indices().nth(self.current)
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_internal(token_type, None)
    }

    fn add_token_internal(&mut self, token_type: TokenType, literal: Option<String>) {
        let (source, text) = self.source.split_at(self.current);
        println!("{} {}", source, text);
        self.tokens.push(Token::new(
            token_type,
            source.to_string(),
            literal,
            self.line,
        ))
    }
}

impl Lox {
    fn new() -> Self {
        Lox { hasError: false }
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
            self.hasError = false
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
    dbg!(args);
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
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}
