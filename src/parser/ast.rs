
#[derive(Debug)]
pub enum Statement {
    Assignment{var: Option<String>, expr: Expression},
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum Declaration {
    Function{
        name: String,
        args: Vec<(String, Type)>,
        returns: Option<Type>,
        body: Vec<Statement>
    },
    Object{name: String, fields: Vec<(String, Type)>},
}

#[derive(Debug)]
pub enum Type {
    Named(String),
    Pointer(Box<Type>),
    Slice(Box<Type>),
    UninitSlice(Box<Type>),
    Array(u64, Box<Type>),
}

#[derive(Debug)]
pub enum Expression {
    Call{function: String, args: Vec<Expression>},
    Variable(String),
}
