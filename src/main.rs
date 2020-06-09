
mod parser;

pub fn main() {
    let file = std::env::args().skip(1).next().unwrap();
    let program = std::fs::read_to_string(file).unwrap();
    let ast = parser::grammar::ProgramParser::new()
        .parse(&program)
        .map_err(|e| panic!("{}", e));
    println!("parsed:\n{:#?}", ast)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
