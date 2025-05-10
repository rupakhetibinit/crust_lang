use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

use crate::token::TokenType;

pub static HAD_ERROR: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

pub static KEYWORDS: Lazy<HashMap<&str, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::<&str, TokenType>::new();
    m.insert("and", TokenType::And);
    m.insert("class", TokenType::Class);
    m.insert("else", TokenType::Else);
    m.insert("false", TokenType::False);
    m.insert("for", TokenType::For);
    m.insert("fun", TokenType::Fun);
    m.insert("if", TokenType::If);
    m.insert("nil", TokenType::Nil);
    m.insert("or", TokenType::Or);
    m.insert("print", TokenType::Print);
    m.insert("return", TokenType::Return);
    m.insert("super", TokenType::Super);
    m.insert("this", TokenType::This);
    m.insert("true", TokenType::True);
    m.insert("var", TokenType::Var);
    m.insert("while", TokenType::While);
    m
});
