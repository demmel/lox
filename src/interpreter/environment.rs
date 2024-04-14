use std::collections::{hash_map::Entry, HashMap};

use super::{Callable, ExecutionErrorKind, Value};

#[derive(Debug, Clone)]
pub enum Declarable {
    Variable(Value),
    Function(Callable),
}

#[derive(Debug, Clone)]
struct Scope {
    declarables: HashMap<String, Declarable>,
    is_function: bool,
}

impl Scope {
    fn new(is_function: bool) -> Self {
        Self {
            declarables: HashMap::new(),
            is_function,
        }
    }

    fn get(&self, name: &str) -> Option<&Declarable> {
        self.declarables.get(name)
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

    fn assign_variable<'a>(&'a mut self, name: &str, value: &Value) -> Option<&'a Value> {
        if let Some(Declarable::Variable(v)) = self.declarables.get_mut(name) {
            *v = value.clone();
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    stack: Vec<Scope>,
    is_returning: bool,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            stack: vec![Scope::new(false)],
            is_returning: false,
        }
    }

    pub fn push(&mut self, is_function: bool) {
        self.stack.push(Scope::new(is_function));
    }

    pub fn pop(&mut self) {
        self.stack.pop();
        if self.stack.is_empty() {
            panic!("Popped the last scope");
        }
    }

    pub fn get(&self, name: &str) -> Option<&Declarable> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn declare(
        &mut self,
        name: String,
        declarable: Declarable,
    ) -> Result<(), ExecutionErrorKind> {
        self.stack.last_mut().unwrap().declare(name, declarable)
    }

    pub fn assign_variable(&mut self, name: &str, value: &Value) -> Option<&Value> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(v) = scope.assign_variable(name, value) {
                return Some(v);
            }
        }
        None
    }

    pub fn is_in_function(&self) -> bool {
        self.stack.iter().any(|scope| scope.is_function)
    }

    pub fn is_returning(&self) -> bool {
        self.is_returning
    }

    pub fn set_returning(&mut self, is_returning: bool) {
        self.is_returning = is_returning;
    }
}
