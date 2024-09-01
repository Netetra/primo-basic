use std::{env, fs, process};

use evaluator::Evaluator;
use parser::primo_parser;
mod ast;
mod evaluator;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file-path>", args[0]);
        process::exit(1);
    }

    let file_path = &args[1];
    let input = match fs::read_to_string(file_path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            process::exit(1);
        }
    };

    let ast = primo_parser::program(&input).unwrap();
    println!("AST: {:?}", &ast);
    let evaluator = Evaluator::new(&ast).unwrap();
    let result = evaluator.eval("main");
    println!("result: {:?}", result);
}
