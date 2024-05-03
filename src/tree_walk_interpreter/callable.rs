use std::{cell::RefCell, fmt::Display, rc::Rc};

use rustc_hash::FxHashMap;

use crate::ast::{Expression, FunctionDecl};

use super::{
    scope::{Declarable, Scope},
    Class, ExecutionErrorKind, Instance, Interpreter, Value,
};

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
        let evaluated_args = args
            .iter()
            .map(|arg| interpreter.evaluate(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let res = interpreter.execute_in_scope(
            Scope::boxed(Some(self.scope.clone()), true),
            |interpreter| {
                for (arg_name, evaluated_arg) in
                    self.decl.args.iter().zip(evaluated_args.into_iter())
                {
                    interpreter
                        .scope
                        .borrow_mut()
                        .declare(arg_name.clone(), Declarable::Variable(evaluated_arg))?;
                }
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
