
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream}
    },
};

use lalrpop_util::ParseError;

use parser::grammar::Token;

mod parser;

pub fn main() {
    let file = std::env::args().skip(1).next().unwrap();
    let program = std::fs::read_to_string(&*file).unwrap();

    let mut files = SimpleFiles::new();
    let file_id = files.add(file, &*program);

    let ast = parser::grammar::ProgramParser::new()
        .parse(&program)
        .map_err(|e| {
            print_error(&mut files, file_id, e);
            std::process::exit(1)
        });
    println!("parsed:\n{:#?}", ast)
}

pub fn print_error(
    files: &mut SimpleFiles<String, &str>,
    file: usize,
    error: ParseError<usize, Token, &str>
) {
    use ParseError::*;
    let diagnostic = match error {
        InvalidToken { location } => {
            Diagnostic::error().with_message("invalid token")
                .with_labels(vec![
                    Label::primary(file, location..location)
                        .with_message("this is not a valid token")
                ])
        }
        UnrecognizedEOF { location, expected } => {
            Diagnostic::error().with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(file, location..location)
                        .with_message("file ends here")
                ])
                .with_notes(vec![format!("expected one of: {}", expected.join(", "))])
        }
        UnrecognizedToken { token, expected } => {
            Diagnostic::error().with_message("unexpected token")
                .with_labels(vec![
                    Label::primary(file, (token.0)..(token.2))
                        .with_message(format!("unexpected token: {}", token.1))
                ])
                .with_notes(vec![format!("expected one of: {}", expected.join(", "))])
        }
        ExtraToken { token } => {
            Diagnostic::error().with_message("extra token")
                .with_labels(vec![
                    Label::primary(file, (token.0)..(token.2))
                        .with_message("this token is not needed")
                ])
        }
        User { error } => {
            Diagnostic::error()
                .with_message(format!("parsing error {}", error))
        }
    };

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &*files, &diagnostic).unwrap();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
