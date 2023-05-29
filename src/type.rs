use crate::types::Type;

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Var => 0,
            Type::Void => 0,
            Type::U32 => 4,
            Type::Bool => 1,
        }
    }
}
