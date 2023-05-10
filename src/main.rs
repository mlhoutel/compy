use crate::parse::process::oneline;
use crate::parse::python::{parse, serialize};
use std::fs;

mod parse;

fn main() {
    inline();
}

fn inline() {
    let source =
        fs::read_to_string("./tests/gen.py").expect("Should have been able to read the file");

    let ast = parse(&source.clone());
    let output = serialize(oneline(ast));

    fs::write("./tests/gen_inlined.py", output).expect("Should have been able to read the file");
}
