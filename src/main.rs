use std::fs::read_to_string;

use connor::types::{Ast, Env};

fn main() {
    let input = read_to_string("source.nor").unwrap();
    let (_, nodes) = Ast::parse(&input).unwrap();

    let mut env = Env::default();
    for node in &nodes {
        node.check_type(&mut env);
    }

    let mut env = Env::default();
    for node in &nodes {
        let asm = node.assemble(&mut env);
        println!("{asm}");
    }

    //println!("Result: {ast:#?}");
}
