use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    rc::Rc,
};

use super::{Callable, ExecutionErrorKind, Value};

#[derive(Debug, Clone)]
pub enum Declarable {
    Variable(Value),
    Function(Callable),
}

#[derive(Clone)]
pub struct Scope {
    declarables: HashMap<String, Declarable>,
    parent: Option<Rc<RefCell<Scope>>>,
    is_function: bool,
}

impl Scope {
    pub fn new(parent: Option<Rc<RefCell<Scope>>>, is_function: bool) -> Self {
        Self {
            declarables: HashMap::new(),
            parent,
            is_function,
        }
    }

    fn get(&self, name: &str) -> Option<Declarable> {
        if let Some(declarable) = self.declarables.get(name) {
            return Some(declarable.clone());
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name).clone()
        } else {
            None
        }
    }

    fn declare(&mut self, name: String, declarable: Declarable) -> Result<(), ExecutionErrorKind> {
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

    fn assign_variable<'a>(&'a mut self, name: &str, value: &Value) -> Option<Value> {
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

    fn is_in_function(&self) -> bool {
        self.is_function
            || self
                .parent
                .as_ref()
                .map_or(false, |p| p.borrow().is_in_function())
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
                                    Callable::Function(scope, args, statement) => {
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
                                },
                            },
                        )
                    })
                    .collect::<Vec<_>>(),
            )
            .field("parent", &self.parent.as_ref().map(|p| p.as_ptr()))
            .field("is_function", &self.is_function)
            .finish()
    }
}

#[derive(Clone)]
pub struct Environment {
    stack: Vec<Rc<RefCell<Scope>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            stack: vec![Rc::new(RefCell::new(Scope::new(None, false)))],
        }
    }

    pub fn get_at(&self, depth: usize, name: &str) -> Option<Declarable> {
        let scope = self.climb(depth);
        let scope = scope.borrow();
        scope.get(name)
    }

    pub fn current(&self) -> &Rc<RefCell<Scope>> {
        &self.stack.last().unwrap()
    }

    pub fn push(&mut self, scope: Rc<RefCell<Scope>>) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        } else {
            panic!("Cannot pop the global scope!");
        }
    }

    pub fn declare(
        &mut self,
        name: String,
        declarable: Declarable,
    ) -> Result<(), ExecutionErrorKind> {
        self.current().borrow_mut().declare(name, declarable)
    }

    pub fn assign_at(&mut self, depth: usize, name: &str, value: &Value) -> Option<Value> {
        let scope = self.climb(depth);
        let mut scope = scope.borrow_mut();
        scope.assign_variable(name, value)
    }

    fn climb(&self, depth: usize) -> Rc<RefCell<Scope>> {
        let mut scope = self.stack.last().unwrap().clone();
        for _ in 0..depth {
            let parent = scope.borrow().parent.clone();
            scope = if let Some(p) = parent {
                p
            } else {
                return scope;
            };
        }
        scope
    }

    pub fn is_in_function(&self) -> bool {
        self.current().borrow().is_in_function()
    }
}

impl Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.stack.iter().map(|s| (s.as_ptr(), s.borrow())))
            .finish()
    }
}
