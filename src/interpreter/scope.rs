use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    rc::Rc,
};

use super::{
    callable::{Callable, CallableFunction},
    Class, ExecutionErrorKind, Value,
};

#[derive(Debug, Clone)]
pub enum Declarable {
    Variable(Value),
    Function(Callable),
    Class(Rc<Class>),
}

#[derive(Clone)]
pub struct Scope {
    declarables: HashMap<String, Declarable>,
    parent: Option<Rc<RefCell<Scope>>>,
    is_function: bool,
}

impl Scope {
    pub fn boxed(parent: Option<Rc<RefCell<Scope>>>, is_function: bool) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::new(parent, is_function)))
    }

    pub fn new(parent: Option<Rc<RefCell<Scope>>>, is_function: bool) -> Self {
        Self {
            declarables: HashMap::new(),
            parent,
            is_function,
        }
    }

    fn climb_scopes(mut scope: Rc<RefCell<Scope>>, distance: usize) -> Rc<RefCell<Scope>> {
        for _ in 0..distance {
            let parent = scope.borrow().parent().cloned();
            scope = if let Some(p) = parent {
                p.clone()
            } else {
                return scope;
            };
        }
        scope
    }

    pub fn get_at(scope: Rc<RefCell<Scope>>, depth: usize, name: &str) -> Option<Declarable> {
        let scope = Self::climb_scopes(scope, depth);
        let scope = scope.borrow();
        scope.get(name)
    }

    pub fn assign_at(
        scope: Rc<RefCell<Scope>>,
        depth: usize,
        name: &str,
        value: &Value,
    ) -> Option<Value> {
        let scope = Self::climb_scopes(scope, depth);
        let mut scope = scope.borrow_mut();
        scope.assign_variable(name, value)
    }

    pub fn get(&self, name: &str) -> Option<Declarable> {
        if let Some(declarable) = self.declarables.get(name) {
            return Some(declarable.clone());
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name).clone()
        } else {
            None
        }
    }

    pub fn declare(
        &mut self,
        name: String,
        declarable: Declarable,
    ) -> Result<(), ExecutionErrorKind> {
        match self.declarables.entry(name) {
            Entry::Occupied(mut o) => {
                if matches!(o.get(), Declarable::Function(_)) {
                    Err(ExecutionErrorKind::FunctionRedeclaration(o.key().clone()))
                } else {
                    o.insert(declarable);
                    Ok(())
                }
            }
            Entry::Vacant(v) => {
                v.insert(declarable);
                Ok(())
            }
        }
    }

    pub fn assign_variable<'a>(&'a mut self, name: &str, value: &Value) -> Option<Value> {
        if let Some(declarable) = self.declarables.get_mut(name) {
            if let Declarable::Variable(v) = declarable {
                *v = value.clone();
                return Some(v.clone());
            }
        } else if let Some(parent) = &self.parent {
            return parent.borrow_mut().assign_variable(name, value);
        }
        None
    }

    pub fn is_in_function(&self) -> bool {
        self.is_function
            || self
                .parent
                .as_ref()
                .map_or(false, |p| p.borrow().is_in_function())
    }

    pub fn parent(&self) -> Option<&Rc<RefCell<Scope>>> {
        self.parent.as_ref()
    }
}

impl Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(format!("Scope<{:?}>", std::ptr::from_ref(self)).as_str())
            .field(
                "declarables",
                &self
                    .declarables
                    .iter()
                    .map(|(name, declarable)| {
                        (
                            name.clone(),
                            match declarable {
                                Declarable::Variable(v) => v.clone().to_string(),
                                Declarable::Function(f) => match f {
                                    Callable::Function(CallableFunction {
                                        scope,
                                        args,
                                        body: statement,
                                    }) => {
                                        format!(
                                            "Function<{:?}>({:?}){}",
                                            scope.as_ptr(),
                                            args,
                                            statement
                                        )
                                    }
                                    Callable::Builtin(closure, arity) => {
                                        format!("Builtin<{:?}>({})", closure, arity)
                                    }
                                    Callable::ClassConstructor(class) => {
                                        format!("ClassConstructor<{:?}>", class)
                                    }
                                },
                                Declarable::Class(class) => {
                                    format!("Class({})", class.name)
                                }
                            },
                        )
                    })
                    .collect::<Vec<_>>(),
            )
            // .field("parent", &self.parent.as_ref().map(|p| p.as_ptr()))
            .field("is_function", &self.is_function)
            .finish()
    }
}
