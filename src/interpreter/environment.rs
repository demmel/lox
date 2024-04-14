use std::collections::HashMap;

use super::{Callable, ExecutionError, Value};

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Callable>,
    is_function: bool,
}

impl Scope {
    fn new(is_function: bool) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            is_function,
        }
    }

    fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn get_function(&self, name: &str) -> Option<&Callable> {
        self.functions.get(name)
    }

    fn declare_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn assign_variable<'a>(&'a mut self, name: &str, value: &Value) -> Option<&'a Value> {
        if let Some(v) = self.variables.get_mut(name) {
            *v = value.clone();
            Some(v)
        } else {
            None
        }
    }

    fn declare_function(&mut self, name: &str, callable: Callable) -> Result<(), ExecutionError> {
        self.functions.insert(name.to_string(), callable);
        Ok(())
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

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get_variable(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_callable(&self, name: &str) -> Option<&Callable> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get_function(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn declare_variable(&mut self, name: String, value: Value) {
        self.stack.last_mut().unwrap().declare_variable(name, value);
    }

    pub fn assign_variable(&mut self, name: &str, value: &Value) -> Option<&Value> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(v) = scope.assign_variable(name, value) {
                return Some(v);
            }
        }
        None
    }

    pub fn declare_function(
        &mut self,
        name: &str,
        callable: Callable,
    ) -> Result<(), ExecutionError> {
        self.stack
            .last_mut()
            .unwrap()
            .declare_function(name, callable)
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
