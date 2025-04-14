#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    name: String,
}
impl LoxClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl std::fmt::Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
