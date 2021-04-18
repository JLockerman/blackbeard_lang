
#[derive(Debug, Clone)]
pub enum Statement {
    Assignment{var: Option<Expression>, expr: Expression},
    Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function{
        name: String,
        args: Vec<(String, Type)>,
        returns: Option<Type>,
        body: Vec<Statement>
    },
    Object{name: String, fields: Vec<(String, Type)>},
}

#[derive(Debug, Clone)]
pub enum Type {
    Named(String),
    Pointer(Box<Type>),
    Slice(Box<Type>),
    UninitSlice(Box<Type>),
    Array(u64, Box<Type>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Project(Box<Expression>, Vec<Projection>),
    ProjectAndRef(Box<Expression>, Vec<Projection>),
    Call{function: String, args: Vec<Expression>},
    Prefix(PrefixOp, Box<Expression>),
    Binary{op: BinOp, left: Box<Expression>, right: Box<Expression>},
}

#[derive(Debug, Clone)]
pub enum Projection {
    Deref,
    Field(String),
    DerefField(String),
    DerefIndex(Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus, Minus, Mul, Div, And, Or, Eq, Neq,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Neg, Not,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u128),
    Float(f64),
    // String(&'static str),
}
