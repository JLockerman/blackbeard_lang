
#[derive(Debug)]
pub enum Statement {
    Assignment{var: Option<Expression>, expr: Expression},
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
    Literal(Literal),
    Variable(String),
    Deref(Box<Expression>),
    PointerTo(Box<Expression>),
    Project(Box<Expression>, String),
    DerefIndex(Box<Expression>, Box<Expression>),
    Call{function: String, args: Vec<Expression>},
    Prefix(PrefixOp, Box<Expression>),
    Binary{op: BinOp, left: Box<Expression>, right: Box<Expression>},
}

#[derive(Debug)]
pub enum BinOp {
    Plus, Minus, Mul, Div, And, Or, Eq, Neq,
}

#[derive(Debug)]
pub enum PrefixOp {
    Neg, Not,
}

#[derive(Debug)]
pub enum Literal {
    Integer(u128),
    Float(f64),
    // String(&'static str),
}
