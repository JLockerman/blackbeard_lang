use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(dead_code)] pub grammar, "/parser/grammar.rs");

pub mod ast;

#[cfg(test)]
mod test {
    use super::grammar::{DeclarationParser};
    use super::ast::*;

    #[test]
    fn declaration() {
        let declaration = r#"
        object Foo {
            named: Integer,
            ptr: {}-> integer,
            ptr_ptr: {}-> {}-> integer,
            slice: {}->[...Integer],
            slice: {}->[...{}->Integer],
            array: {}->[4 Boolean],
        }
        "#;
        let ast = DeclarationParser::new().parse(declaration).unwrap();
        panic!("{:?}", ast);
        // assert_eq!(ast, Declaration::Object{
        //     name: "Foo".into(),
        //     fields: vec![],
        // })
    }

}
