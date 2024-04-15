use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::{Callable, ExecutionErrorKind, Value};

#[derive(Debug, Clone)]
pub enum Declarable {
    Variable(Value),
    Function(Callable),
}

#[derive(Debug, Clone)]
struct Scope {
    declarables: HashMap<String, Declarable>,
    parent: Option<Rc<RefCell<Scope>>>,
    is_function: bool,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>, is_function: bool) -> Self {
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

#[derive(Debug, Clone)]
pub struct Environment {
    current: Rc<RefCell<Scope>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            current: Rc::new(RefCell::new(Scope::new(None, false))),
        }
    }

    pub fn push(&mut self, is_function: bool) {
        let new_scope = Scope::new(Some(self.current.clone()), is_function);
        self.current = Rc::new(RefCell::new(new_scope));
    }

    pub fn pop(&mut self) {
        let parent = self.current.borrow().parent.clone();
        if let Some(p) = parent {
            self.current = p;
        } else {
            panic!("Cannot pop the global scope!");
        }
    }

    pub fn get(&self, name: &str) -> Option<Declarable> {
        self.current.borrow().get(name)
    }

    pub fn declare(
        &mut self,
        name: String,
        declarable: Declarable,
    ) -> Result<(), ExecutionErrorKind> {
        self.current.borrow_mut().declare(name, declarable)
    }

    pub fn assign_variable(&mut self, name: &str, value: &Value) -> Option<Value> {
        self.current.borrow_mut().assign_variable(name, value)
    }

    pub fn is_in_function(&self) -> bool {
        self.current.borrow().is_in_function()
    }
}
