use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(dead_code)] pub grammar, "/parser/grammar.rs");

pub mod ast;
