use std::collections::HashMap;

use crate::ast::{Expression, Function, Program, Statement};

#[derive(Debug, Clone, Copy)]
enum BindingState {
    Declared,
    Defined,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, BindingState>>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn resolve(&mut self, program: &mut Program) {
        for statement in &mut program.0 {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Block(statements) => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_statement(statement);
                }
                self.end_scope();
            }
            Statement::Expression(expression) => self.resolve_expression(expression),
            Statement::VarDeclaration(name, expression) => {
                self.declare(name.clone());
                self.resolve_expression(expression);
                self.define(name.clone());
            }
            Statement::FunctionDeclaration(Function {
                name,
                args: parameters,
                body,
            }) => {
                self.declare(name.clone());
                self.define(name.clone());
                self.resolve_function(parameters, body.as_mut());
            }
            Statement::Print(expression) => self.resolve_expression(expression),
            Statement::If(condition, then_branch, else_branch) => {
                self.resolve_expression(condition);
                self.resolve_statement(then_branch.as_mut());
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch.as_mut());
                }
            }
            Statement::While(condition, body) => {
                self.resolve_expression(condition);
                self.resolve_statement(body.as_mut());
            }
            Statement::Return(expression) => {
                if let Some(expression) = expression {
                    self.resolve_expression(expression);
                }
            }
            Statement::ClassDeclaration(name, methods) => {
                self.declare(name.clone());
                self.define(name.clone());
            }
        }
    }

    fn resolve_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Identifier { name, scope_depth } => {
                if let Some(scope) = self.scopes.last() {
                    match scope.get(name) {
                        Some(BindingState::Declared) => {
                            eprintln!("Error: Cannot read local variable in its own initializer.");
                        }
                        _ => {}
                    }
                }
                self.resolve_local(name, scope_depth);
            }
            Expression::Assign {
                name,
                expr,
                scope_depth,
            } => {
                self.resolve_expression(expr);
                self.resolve_local(name, scope_depth);
            }
            Expression::Literal(_) => {}
            Expression::Grouping(expression) => self.resolve_expression(expression),
            Expression::Binary(left, _, right) => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            Expression::Unary(_, right) => self.resolve_expression(right),
            Expression::Call(callee, arguments) => {
                self.resolve_expression(callee);
                for argument in arguments {
                    self.resolve_expression(argument);
                }
            }
        }
    }

    fn resolve_local(&mut self, name: &str, scope_depth: &mut usize) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                *scope_depth = i;
                break;
            }
        }
    }

    fn resolve_function(&mut self, parameters: &[String], body: &mut Statement) {
        self.begin_scope();
        for parameter in parameters {
            self.declare(parameter.clone());
            self.define(parameter.clone());
        }
        self.resolve_statement(body);
        self.end_scope();
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, BindingState::Declared);
        }
    }

    fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, BindingState::Defined);
        }
    }
}
