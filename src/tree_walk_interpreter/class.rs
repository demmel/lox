use std::{collections::HashMap, rc::Rc};

use super::{callable::CallableFunction, Value};

pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub methods: HashMap<String, CallableFunction>,
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field(
                "superclass",
                &self.superclass.as_ref().map(|c| c.name.clone()),
            )
            .field("methods", &self.methods)
            .finish()
    }
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<&CallableFunction> {
        self.methods.get(name).or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.find_method(name))
        })
    }
}

pub struct Instance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}

impl std::fmt::Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Instance")
            .field("class", &self.class.name)
            .field("fields", &self.fields)
            .finish()
    }
}
