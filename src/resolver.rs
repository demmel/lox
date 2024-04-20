use std::collections::HashMap;

use crate::ast::{Expression, Function, Program, Statement};

#[derive(Debug, Clone, Copy)]
enum BindingState {
    Declared,
    Defined,
}

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy)]
enum ClassType {
    None,
    Class,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, BindingState>>,
    function_type: FunctionType,
    class_type: ClassType,
}

#[derive(Debug, thiserror::Error)]
pub enum ResolverError {
    #[error("Cannot return from top-level code.")]
    ReturnFromTopLevel,
    #[error("Cannot use 'this' outside of a class.")]
    ThisOutsideClass,
    #[error("Cannot return a value from an initializer.")]
    ReturnFromInitializer,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scopes: vec![HashMap::new()],
            function_type: FunctionType::None,
            class_type: ClassType::None,
        }
    }

    pub fn resolve(&mut self, program: &mut Program) -> Result<(), ResolverError> {
        for statement in &mut program.0 {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: &mut Statement) -> Result<(), ResolverError> {
        match statement {
            Statement::Block(statements) => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_statement(statement)?;
                }
                self.end_scope();
            }
            Statement::Expression(expression) => self.resolve_expression(expression)?,
            Statement::VarDeclaration(name, expression) => {
                self.declare(name.clone());
                self.resolve_expression(expression)?;
                self.define(name.clone());
            }
            Statement::FunctionDeclaration(Function {
                name,
                args: parameters,
                body,
            }) => {
                self.declare(name.clone());
                self.define(name.clone());
                self.resolve_function(parameters, body.as_mut(), FunctionType::Function)?;
            }
            Statement::Print(expression) => self.resolve_expression(expression)?,
            Statement::If(condition, then_branch, else_branch) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch.as_mut())?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch.as_mut())?;
                }
            }
            Statement::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body.as_mut())?;
            }
            Statement::Return(expression) => {
                match self.function_type {
                    FunctionType::None => return Err(ResolverError::ReturnFromTopLevel),
                    FunctionType::Initializer => {
                        if expression.is_some() {
                            return Err(ResolverError::ReturnFromInitializer);
                        }
                    }
                    _ => {}
                }

                if let Some(expression) = expression {
                    self.resolve_expression(expression)?;
                }
            }
            Statement::ClassDeclaration(name, methods) => {
                self.declare(name.clone());
                self.define(name.clone());

                let enclosing_class = self.class_type;
                self.class_type = ClassType::Class;

                self.begin_scope();

                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert("this".to_string(), BindingState::Defined);

                for method in methods {
                    let Function {
                        args: parameters,
                        body,
                        ..
                    } = method;
                    self.resolve_function(
                        parameters,
                        body.as_mut(),
                        if method.name == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Method
                        },
                    )?;
                }

                self.end_scope();

                self.class_type = enclosing_class;
            }
        }

        Ok(())
    }

    fn resolve_expression(&mut self, expression: &mut Expression) -> Result<(), ResolverError> {
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
                self.resolve_expression(expr)?;
                self.resolve_local(name, scope_depth);
            }
            Expression::Literal(_) => {}
            Expression::Grouping(expression) => self.resolve_expression(expression)?,
            Expression::Binary(left, _, right) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            Expression::Unary(_, right) => self.resolve_expression(right)?,
            Expression::Call(callee, arguments) => {
                self.resolve_expression(callee)?;
                for argument in arguments {
                    self.resolve_expression(argument)?;
                }
            }
            Expression::Get(expr, _name) => {
                self.resolve_expression(expr)?;
            }
            Expression::Set(expr, _name, value) => {
                self.resolve_expression(expr)?;
                self.resolve_expression(value)?;
            }
            Expression::This(scope_depth) => {
                if matches!(self.class_type, ClassType::None) {
                    return Err(ResolverError::ThisOutsideClass);
                }
                self.resolve_local("this", scope_depth);
            }
        };

        Ok(())
    }

    fn resolve_local(&mut self, name: &str, scope_depth: &mut usize) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                *scope_depth = i;
                break;
            }
        }
    }

    fn resolve_function(
        &mut self,
        parameters: &[String],
        body: &mut Statement,
        function_type: FunctionType,
    ) -> Result<(), ResolverError> {
        let enclosing_function = self.function_type;
        self.function_type = function_type;

        self.begin_scope();

        for parameter in parameters {
            self.declare(parameter.clone());
            self.define(parameter.clone());
        }
        self.resolve_statement(body)?;

        self.end_scope();

        self.function_type = enclosing_function;

        Ok(())
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
