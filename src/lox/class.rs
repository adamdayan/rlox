#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    name: String,
}
impl LoxClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
