use logos::Logos;

#[derive(Logos, Debug, Clone)]
pub enum Token {
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[token(";")]
    SemiColon,

    #[token(".")]
    Dot,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("%")]
    Modulo,

    #[token("::")]
    DoubleColon,

    #[token(":")]
    Colon,

    #[token("!")]
    Not,

    #[token(">=")]
    GreaterEqual,

    #[token("<=")]
    LesserEqual,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    NotEqual,

    #[token(">")]
    RightAngleBracket,

    #[token("<")]
    LeftAngleBracket,

    #[token("=")]
    Equal,

    #[token("?")]
    QuestionMark,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("impl")]
    Impl,

    #[token("for")]
    For,

    #[token("++")]
    PlusPlus,

    #[token("--")]
    MinusMinus,

    #[token(",")]
    Comma,
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::lexer::Token;

    #[test]
    pub fn lex_example_programs() {
        let sources = [
            "
for(let i = 0; i <=5 ; i++) {
    let x = i * i;
}


for(let i = 0; i <=5 ; ++i) {
    let x  = i * i;
    let x = 2.0;
    let y : f64 = x * 0.5;
}
        ",
            "
fn main(x: i32, y: i32) -> i32 {
    return x * 2 * y;
}

// This is a comment
fn test(x : i32, y : f32, z : String) -> f32 {
    return x + y
}
        ",
            "
fn test() -> i64 {
    return 2;
}

/* This is a test for multi line comment with the 
virtual machine and the bytecode compiler */

fn main() -> void {
    let z = 2 * 22;
    print(z);
}
        ",
        ];
        for source in sources {
            let mut lexer = Token::lexer(source);

            for token in lexer.by_ref() {
                println!("Token : {:?}", token);
                assert!(token.is_ok());
            }
        }
    }
}
