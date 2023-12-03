

pub trait AnyVar {
    fn name(&self) -> &str;
}

pub struct LocalVar {
    name: String
}

impl AnyVar for LocalVar {
    fn name(&self) -> &str {
        &self.name
    }
}

