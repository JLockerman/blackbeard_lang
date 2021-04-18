/// simple AST-walking interpreter
/// well, not quite, since we don't require forward declarations, this
/// actually happens in multiple passes, first we discover all of the
///functions and types, then we execute

use std::{collections::{HashMap, HashSet}};

use crate::parser::ast::{self, *, Statement::*, Declaration::*,};

pub type Program<'a> = &'a [Statement];

pub fn execute(program: Program) -> Result<i64, self::Error> {
    let mut type_info = TypeInfo::default();
    let top_level_declarations = Declarations::try_from(program, &mut type_info)?;
    let mut executor = Executor::from(top_level_declarations, type_info);
    executor.execute_function_body(program)
}

struct Executor<'p> {
    prev_stacks: Vec<Stack>,
    current_stack: Stack,
    declarations: Declarations<'p>,
    type_info: TypeInfo,
    heap: Heap,
    next_heap: Heap,
}

impl<'p> Executor<'p> {
    fn from(top_level_declarations: Declarations<'p>, type_info: TypeInfo) -> Self {
        Self {
            prev_stacks: Vec::default(),
            current_stack: Stack::default(),
            type_info,
            declarations: top_level_declarations,
            heap: Heap::default(),
            next_heap: Heap::default(),
        }
    }
}

#[repr(usize)]
enum BuiltinType {
    Boolean = 4,
    Integer,
    Float,
    PointerToInteger,
    PointerToFloat,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Type {
    Base(TypeId),
    Pointer(Box<Type>),
    Slice(Box<Type>),
    UninitSlice(Box<Type>),
    Array(u64, Box<Type>),
}

type Stack = HashMap<String, Pointer>;
type Heap = Vec<HeapValue>;
type TypeId = usize;

#[derive(Debug, Copy, Clone)]
pub struct Pointer {
    location: usize,
    typ: TypeId,
}

#[derive(Debug)]
pub enum Error {
    MissingDeclaration(String),
    DuplicateFunctionDeclaration(String),
    DuplicateObjectDeclaration(String),
    NoValueAssignment,
    InvalidAssignmentType,
    MissingValue(String),
    NonPointerDeref,
    InvalidProjection,
    CannotAssign,
    InvalidReference,
    InvalidType,
}

#[derive(Debug, Copy, Clone)]
pub enum HeapValue {
    Uninit,
    Boolean(bool),
    Integer(i128),
    Float(f64),
    Pointer(Pointer),
    Moved(Pointer),
}

#[derive(Debug, Clone)]
pub enum ExpressionValue {
    Unit,
    Boolean(bool),
    Integer(i128),
    Float(f64),
    Pointer(Pointer),
    Object(TypeId, Vec<ExpressionValue>),
}

impl<'p> Executor<'p> {
    fn execute_function_body(
        &mut self,
        program: Program<'p>,
    ) -> Result<i64, self::Error> {
        for statement in program {
            match statement {
                Statement::Declaration(_) => continue,
                Assignment { var, expr } => {
                    let result = self.execute_expression(expr)?;
                    let typ = self.get_type_id(&result)?;
                    match (var.as_ref(), result) {
                        (None, _) => continue,
                        (Some(path), value) => {
                            let place = self.execute_lval_expression(path, typ)?;
                            self.store(place, value)?;
                        },
                    }
                },
            }
        }
        todo!()
    }

    fn execute_lval_expression(
        &mut self,
        expression: &Expression,
        typ: TypeId,
    ) -> Result<Pointer, self::Error> {
        use Expression::*;
        match expression {
            Literal(..) | Binary{..} | Prefix(..) | Call{..} | ProjectAndRef{..} =>
                Err(Error::CannotAssign),
            Expression::Variable(name) => {
                if let Some(pointer) = self.current_stack.get(&*name) {
                    return Ok(*pointer)
                }
                let pointer = self.allocate(typ)?;
                Ok(pointer)
            },
            Expression::Project(sub_expression, projections) => {
                let pointer = self.project_and_ref(sub_expression, projections)?;
                Ok(pointer)
            },
        }
    }

    fn execute_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<ExpressionValue, self::Error> {
        use Expression::*;
        use self::Literal::*;
        use self::Error::*;
        match expression {
            Literal(Integer(val)) => Ok(ExpressionValue::Integer(*val as _)),
            Literal(Float(val)) => Ok(ExpressionValue::Float(*val)),

            Variable(name) => {
                let pointer = self.current_stack.get(name);
                let pointer = match pointer {
                    None => return Err(MissingValue(name.clone())),
                    Some(pointer) => *pointer,
                };
                Ok(self.load(pointer))
            }

            Project(sub_expression, projections) => {
                let value = self.execute_expression(sub_expression)?;
                self.project(value, &projections)
            },

            ProjectAndRef(sub_expression, projections) => {
                let pointer = self.project_and_ref(sub_expression, projections)?;
                Ok(ExpressionValue::Pointer(pointer))
            },

            Call { function, args } => {
                todo!()
            }

            Prefix(op, sub_expression) =>
                Ok(self.prefix_op(*op, sub_expression)?),

            Binary { op, left, right } =>
                Ok(self.binary_op(*op, left, right)?),
        }
    }

    fn store(&mut self, pointer: Pointer, value: ExpressionValue) -> Result<usize, self::Error> {
        use ExpressionValue::*;
        match value {
            Unit => return Err(Error::NoValueAssignment),
            Boolean(value) => {
                self.heap[pointer.location] = HeapValue::Boolean(value);
                Ok(1)
            },
            Integer(value) => {
                self.heap[pointer.location] = HeapValue::Integer(value);
                Ok(1)
            },
            Float(value) => {
                self.heap[pointer.location] = HeapValue::Float(value);
                Ok(1)
            },
            Pointer(value) => {
                self.heap[pointer.location] = HeapValue::Pointer(value);
                Ok(1)
            },
            Object(typ, values) => {
                if pointer.typ != typ && typ != 0 {
                    return Err(Error::InvalidAssignmentType)
                }

                let mut offset = 0;
                for value in values {
                    let pointer = self::Pointer{
                        location: pointer.location + offset,
                        typ: 0,
                    };
                    offset += self.store(pointer, value)?;
                }
                Ok(offset)
            },
        }
    }

    fn load(&mut self, place: Pointer) -> ExpressionValue {
        todo!()
    }

    fn allocate(&mut self, typ: TypeId) -> Result<Pointer, Error> {
        let size = self.type_info.get_size(typ)?;
        //TODO checked arithmetic
        if self.heap.capacity() < self.heap.len() + size {
            self.gc();
            if self.heap.capacity() < self.heap.len() + size {
                self.heap.reserve(size)
            }
        }
        let location = self.heap.len();
        self.heap.extend((0..size).map(|_| HeapValue::Uninit));
        Ok(Pointer {location, typ})
    }

    fn gc(&mut self) {
        todo!()
    }

    fn project(&mut self, mut value: ExpressionValue, projections: &[Projection])
    -> Result<ExpressionValue, Error> {
        for projection in projections {
            use Projection::*;
            match (projection, &value) {
                (Deref, ExpressionValue::Pointer(pointer)) =>
                    value = self.load(*pointer),

                (Field(name), ExpressionValue::Object(..)) => {
                    let (typ, mut fields) = match value {
                        ExpressionValue::Object(typ, fields) => (typ, fields),
                        _ => unreachable!(),
                    };
                    let offset = self.type_info.get_offset(typ, &name)?;
                    value = fields.swap_remove(offset)
                },

                (DerefField(name), ExpressionValue::Pointer(pointer)) => {
                    value = self.load(*pointer);
                    let (typ, mut fields) = match value {
                        ExpressionValue::Object(typ, fields) =>
                            (typ, fields),
                        _ => return Err(Error::InvalidAssignmentType),
                    };
                    let offset = self.type_info.get_offset(typ, &name)?;
                    value = fields.swap_remove(offset)
                },

                (DerefIndex(index_expression), ExpressionValue::Pointer(pointer)) => {
                    let index = self.execute_expression(index_expression)?;
                    let index = match index {
                        ExpressionValue::Integer(index) => index,
                        _ => return Err(Error::InvalidAssignmentType),
                    };
                    todo!("bounds checking")
                },
                (_,_) => return Err(Error::InvalidAssignmentType),
            }
        }
        Ok(value)
    }

    fn project_and_ref(&mut self, mut value: &Expression, mut projections: &[Projection])
    -> Result<Pointer, Error> {
        use Projection::*;
        use ExpressionValue::{Pointer as PointerVal, Object as ObjectVal};

        let value = self.execute_expression(value)?;
        enum Value {
            Expr(ExpressionValue),
            Heap(Pointer),
        }
        use Value::*;
        let mut value = Value::Expr(value);

        for projection in projections {
            match projection {
                Deref => match value {
                    Expr(PointerVal(pointer)) => value = Heap(pointer),
                    Heap(pp) => {
                        if !self.type_info.is_pointer(pp.typ) {
                            return Err(Error::InvalidProjection)
                        }
                        match self.heap[pp.location] {
                            HeapValue::Pointer(p) => value = Heap(p),
                            _ => return Err(Error::InvalidProjection),
                        }
                    },
                    _ => return Err(Error::InvalidProjection),
                },
                Field(name) => match value {
                    Expr(ObjectVal(typ, mut values)) => {
                        let offset = self.type_info.get_offset(typ, &name)?;
                        let val = values.swap_remove(offset);
                        value = Expr(val)
                    },
                    Heap(mut pointer) => {
                        let (offset, typ) = self.type_info.get_field_info(pointer.typ, &name)?;
                        pointer.location += offset;
                        pointer.typ = typ;
                        value = Heap(pointer)
                    },
                    _ => return Err(Error::InvalidProjection),
                },
                DerefField(name) => match value {
                    Expr(PointerVal(mut pointer)) => {
                        let (offset, typ) = self.type_info.get_field_info(pointer.typ, &name)?;
                        pointer.location += offset;
                        pointer.typ = typ;
                        value = Heap(pointer)
                    },
                    Heap(pp) => {
                        let pointee = self.type_info.get_pointee(pp.typ)?;
                        let (offset, typ) = self.type_info.get_field_info(pointee, &name)?;
                        match self.heap[pp.location] {
                            HeapValue::Pointer(mut p) => {
                                p.location += offset;
                                p.typ = typ;
                                value = Heap(p)
                            },
                            _ => return Err(Error::InvalidProjection),
                        }
                    },
                    _ => return Err(Error::InvalidProjection),
                },
                DerefIndex(_) => {todo!()},
            }
        }
        todo!()
    }

    fn prefix_op(&mut self, op: PrefixOp, expression: &Expression)
    -> Result<ExpressionValue, Error> {
        use PrefixOp::*;
        use ExpressionValue::*;

        let value = self.execute_expression(expression)?;
        let value = match (op, value) {
            (Neg, Integer(value)) => Integer(-value),
            (Neg, Float(value)) => Float(-value),
            (Not, Boolean(value)) => Boolean(!value),
            _ => return Err(Error::InvalidAssignmentType),
        };
        Ok(value)
    }

    fn binary_op(&mut self, op: BinOp, left: &Expression, right: &Expression)
    -> Result<ExpressionValue, Error> {
        use BinOp::*;
        use ExpressionValue::*;
        match op {
            And => {
                let left = self.execute_expression(left)?;
                match left {
                    Boolean(false) => return Ok(Boolean(false)),
                    Boolean(true) => {
                        if let Boolean(b) = self.execute_expression(right)? {
                            return Ok(Boolean(b))
                        }
                    }
                    _ => {},
                }
                return Err(Error::InvalidAssignmentType)
            },
            Or => {
                let left = self.execute_expression(left)?;
                match left {
                    Boolean(true) => return Ok(Boolean(true)),
                    Boolean(false) => {
                        if let Boolean(b) = self.execute_expression(right)? {
                            return Ok(Boolean(b))
                        }
                    }
                    _ => {},
                }
                return Err(Error::InvalidAssignmentType)
            },
            _ => {},
        }

        let left = self.execute_expression(left)?;
        let right = self.execute_expression(right)?;
        let result = match (op, left, right) {
            (Plus,  Integer(left), Integer(right)) => Integer(left + right),
            (Minus, Integer(left), Integer(right)) => Integer(left - right),
            (Mul,   Integer(left), Integer(right)) => Integer(left * right),
            (Div,   Integer(left), Integer(right)) => Integer(left / right),
            (Eq,    Integer(left), Integer(right)) => Boolean(left == right),
            (Neq,   Integer(left), Integer(right)) => Boolean(left != right),

            (Plus,  Float(left), Float(right)) => Float(left + right),
            (Minus, Float(left), Float(right)) => Float(left - right),
            (Mul,   Float(left), Float(right)) => Float(left * right),
            (Div,   Float(left), Float(right)) => Float(left / right),
            (Eq,    Float(left), Float(right)) => Boolean(left == right),
            (Neq,   Float(left), Float(right)) => Boolean(left != right),

            _ => return Err(Error::InvalidAssignmentType),
        };
        Ok(result)
    }

    fn get_type_id(&self, value: &ExpressionValue) -> Result<TypeId, Error> {
        use ExpressionValue::*;
        match value {
            Unit => Err(Error::NoValueAssignment),
            Boolean(_) => Ok(BuiltinType::Boolean as _),
            Integer(_) => Ok(BuiltinType::Integer as _),
            Float(_) => Ok(BuiltinType::Float as _),
            //TODO
            Pointer(pointer) => Ok(self.type_info.pointer_to[&pointer.typ]),
            Object(typ, _) => Ok(*typ),
        }
    }
}

impl Default for TypeInfo {
    fn default() -> Self {
        let sizes: HashMap<TypeId, usize> = [
            (BuiltinType::Boolean as usize, 1),
            (BuiltinType::Integer as usize, 1),
            (BuiltinType::Float as usize, 1),
            (BuiltinType::PointerToInteger as usize, 1),
            (BuiltinType::PointerToFloat as usize, 1),
        ].iter().cloned().collect();
        Self {
            next_type_id: sizes.len(),
            sizes,
            field_info: HashMap::default(),
            points_to: HashMap::default(),
            pointer_to: HashMap::default(),
            slices: HashSet::default(),
            slices_of: HashMap::default(),
        }
    }
}

#[derive(Debug)]
struct TypeInfo {
    sizes: HashMap<TypeId, usize>,
    field_info: HashMap<TypeId, HashMap<String, (usize, TypeId)>>,
    points_to: HashMap<TypeId, TypeId>,
    pointer_to: HashMap<TypeId, TypeId>,
    slices: HashSet<TypeId>,
    slices_of: HashMap<TypeId, TypeId>,
    next_type_id: usize,
}

impl TypeInfo {
    fn new_id(&mut self) -> TypeId {
        let id = self.next_type_id;
        self.next_type_id += 1;
        id
    }

    fn get_field_info(&self, typ: TypeId, field: &str) -> Result<(usize, TypeId), Error> {
        self.field_info.get(&typ)
            .and_then(|fields| fields.get(field))
            .cloned()
            .ok_or(Error::InvalidProjection)
    }

    fn is_pointer(&self, typ: TypeId) -> bool {
        self.points_to.get(&typ).is_some()
    }

    fn get_pointee(&self, typ: TypeId) -> Result<TypeId, Error> {
        self.pointer_to.get(&typ).cloned().ok_or(Error::InvalidProjection)
    }

    fn get_size(&self, typ: TypeId) -> Result<usize, Error> {
        self.sizes.get(&typ).cloned().ok_or(Error::InvalidType)
    }

    fn get_offset(&self, typ: TypeId, field: &str) -> Result<usize, Error> {
        self.get_field_info(typ, field).map(|(offset, _)| offset)
    }
}

#[derive(Debug, Default)]
pub struct Declarations<'p> {
    functions: HashMap<String, FunctionDeclaration<'p>>,
    objects: HashMap<String, ObjectDeclaration>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'p> {
    args: Vec<(String, ast::Type)>,
    returns: Option<ast::Type>,
    declarations: Option<Declarations<'p>>,
    body: Program<'p>,
}

#[derive(Debug)]
pub struct ObjectDeclaration {
    fields: Vec<(String, ast::Type)>,
    id: TypeId,
}

impl<'p>  Declarations<'p> {
    fn try_from(program: Program<'p>, type_info:  &mut TypeInfo)
    -> Result<Self, self::Error> {
        use std::collections::hash_map::Entry::*;
        use self::Error::{DuplicateFunctionDeclaration, DuplicateObjectDeclaration};
        let mut declarations = Declarations::default();

        for statement in program {
            match statement {
                Assignment{..} => continue,
                Declaration(Function{name, args, returns, body}) => {
                    match declarations.functions.entry(name.clone()) {
                        Occupied(_) =>
                            return Err(DuplicateFunctionDeclaration(name.clone())),
                        Vacant(e) => {
                            e.insert(FunctionDeclaration {
                                args: Vec::clone(args),
                                returns: returns.clone(),
                                body: &*body,
                                declarations: None,
                            });
                        },
                    }
                },

                Declaration(Object{name, fields}) => {
                    match declarations.objects.entry(name.clone()) {
                        Occupied(_) =>
                            return Err(DuplicateObjectDeclaration(name.clone())),
                        Vacant(e) => {
                            e.insert(ObjectDeclaration{
                                fields: Vec::clone(fields),
                                id: type_info.new_id(),
                            });
                        }
                    }
                },
            };
        }

        //FIXME populate fields
        Ok(declarations)
    }
}
