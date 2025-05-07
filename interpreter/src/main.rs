use std::env;

use new::{
    ast::{AstNode, AstNodeId, Symbol},
    interpreter::Interpreter,
    lexer::Lexer,
    parser::Parser,
    token::Token,
};
use slotmap::SlotMap;
use string_interner::{StringInterner, backend::StringBackend};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut interner = StringInterner::<StringBackend<Symbol>>::new();
    let mut ast_nodes = SlotMap::<AstNodeId, AstNode>::with_key();
    let mut lexer = Lexer::new(
        "let x = 2; let y = (1 + 2) * 3; 
        let main_thing = \"something\"; 
        let z = 1 * 2 + 3; 
        let f = 1 * (2 + 3); 
        let xy = ((1 + 2) * 3 + (4 * 5)); 
        let xyz = 1324 {}
        print(1 + 2 * 3);
        print(let);
        2;
        print(2 + 2);
        print(2 + 2 / 3);
        print(x);
        print(main_thing);",
    );

    let mut tokens = vec![];
    loop {
        let tok = lexer.next_token();
        if tok == Token::EOF {
            break;
        }
        tokens.push(tok);
    }

    let parser = Parser::new(tokens, &mut interner, &mut ast_nodes);
    let mut int = Interpreter::new(parser);

    int.evaluate().unwrap();
}
