use std::{cell::RefCell, fmt::Display, rc::Rc};

use rustc_hash::FxHashMap;

use crate::ast::{Expression, FunctionDecl};

use super::{
    scope::{Declarable, Scope},
    Class, ExecutionErrorKind, Instance, Interpreter, Value,
};

enum EvaluatedArgs {
    Zero,
    One(Value),
    Two(Value, Value),
    Three(Value, Value, Value),
    Many(Vec<Value>),
}

impl EvaluatedArgs {
    fn from_expressions(
        interpreter: &mut Interpreter,
        args: &[Expression],
    ) -> Result<Self, ExecutionErrorKind> {
        match args.len() {
            0 => Ok(Self::Zero),
            1 => Ok(Self::One(interpreter.evaluate(&args[0])?)),
            2 => Ok(Self::Two(
                interpreter.evaluate(&args[0])?,
                interpreter.evaluate(&args[1])?,
            )),
            3 => Ok(Self::Three(
                interpreter.evaluate(&args[0])?,
                interpreter.evaluate(&args[1])?,
                interpreter.evaluate(&args[2])?,
            )),
            _ => Ok(Self::Many(
                args.iter()
                    .map(|arg| interpreter.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        }
    }

    fn bind_values(
        &self,
        scope: &Rc<RefCell<Scope>>,
        decl: &FunctionDecl,
    ) -> Result<(), ExecutionErrorKind> {
        match self {
            Self::Zero => Ok(()),
            Self::One(value) => {
                scope
                    .borrow_mut()
                    .declare(decl.args[0].clone(), Declarable::Variable(value.clone()))?;
                Ok(())
            }
            Self::Two(value1, value2) => {
                scope
                    .borrow_mut()
                    .declare(decl.args[0].clone(), Declarable::Variable(value1.clone()))?;
                scope
                    .borrow_mut()
                    .declare(decl.args[1].clone(), Declarable::Variable(value2.clone()))?;
                Ok(())
            }
            Self::Three(value1, value2, value3) => {
                scope
                    .borrow_mut()
                    .declare(decl.args[0].clone(), Declarable::Variable(value1.clone()))?;
                scope
                    .borrow_mut()
                    .declare(decl.args[1].clone(), Declarable::Variable(value2.clone()))?;
                scope
                    .borrow_mut()
                    .declare(decl.args[2].clone(), Declarable::Variable(value3.clone()))?;
                Ok(())
            }
            Self::Many(values) => {
                for (arg_name, value) in decl.args.iter().zip(values.iter()) {
                    scope
                        .borrow_mut()
                        .declare(arg_name.clone(), Declarable::Variable(value.clone()))?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone)]
pub struct CallableFunction {
    pub scope: Rc<RefCell<Scope>>,
    pub decl: FunctionDecl,
    pub is_initializer: bool,
}

impl std::fmt::Debug for CallableFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallableFunction")
            .field("scope", &self.scope.as_ptr())
            .field("decl", &self.decl)
            .field("is_initializer", &self.is_initializer)
            .finish()
    }
}

impl CallableFunction {
    pub fn bind(&self, instance: &Rc<RefCell<Instance>>) -> Result<Self, ExecutionErrorKind> {
        let scope = Scope::boxed(Some(self.scope.clone()), false);
        scope.borrow_mut().declare(
            "this".to_string(),
            Declarable::Variable(Value::Instance(instance.clone())),
        )?;
        Ok(Self {
            scope,
            decl: self.decl.clone(),
            is_initializer: self.is_initializer,
        })
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[Expression],
    ) -> Result<Value, ExecutionErrorKind> {
        let evaluated_args = EvaluatedArgs::from_expressions(interpreter, args)?;

        let res = interpreter.execute_in_scope(
            Scope::boxed(Some(self.scope.clone()), true),
            |interpreter| {
                evaluated_args.bind_values(&interpreter.scope, &self.decl)?;

                let res = interpreter.execute(&self.decl.body)?.unwrap_or(Value::Nil);
                if self.is_initializer {
                    match Scope::get_at(interpreter.scope.clone(), 1, "this") {
                        Some(Declarable::Variable(Value::Instance(instance))) => {
                            Ok(Value::Instance(instance.clone()))
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Ok(res)
                }
            },
        )?;

        Ok(res)
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Function(CallableFunction),
    Builtin(fn(&[Value]) -> Result<Value, ExecutionErrorKind>, usize),
    ClassConstructor(Rc<Class>),
}

impl Callable {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[Expression],
    ) -> Result<Value, ExecutionErrorKind> {
        match self {
            Callable::Function(callable_function) => callable_function.call(interpreter, args),
            Callable::Builtin(f, _) => f(&args
                .iter()
                .map(|arg| interpreter.evaluate(arg))
                .collect::<Result<Vec<_>, _>>()?),
            Callable::ClassConstructor(class) => {
                let instance = Rc::new(RefCell::new(Instance {
                    class: class.clone(),
                    fields: FxHashMap::default(),
                }));
                if let Some(init) = class.methods.get("init") {
                    init.bind(&instance)?.call(interpreter, args)?;
                }
                Ok(Value::Instance(instance))
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Callable::Function(callable_function) => callable_function.decl.args.len(),
            Callable::Builtin(_, arity) => *arity,
            Callable::ClassConstructor(class) => {
                if let Some(init) = class.methods.get("init") {
                    init.decl.args.len()
                } else {
                    0
                }
            }
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Function(callable_function) => {
                write!(f, "<function {}>", callable_function.decl.args.len())
            }
            Callable::Builtin(_, arity) => write!(f, "<builtin function {}>", arity),
            Callable::ClassConstructor(class) => write!(f, "<class {}>", class.name),
        }
    }
}
