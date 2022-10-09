use crate::ast::NodeBody;
use crate::node::NodeID;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum State<Unlinked, Linked> {
    Unlinked(Unlinked),
    Linked(Linked),
}

impl<'a, Unlinked, T: NodeBody> TryFrom<&'a State<Unlinked, NodeID<T>>> for NodeID<T> {
    type Error = ();

    fn try_from(value: &'a State<Unlinked, NodeID<T>>) -> Result<Self, Self::Error> {
        match value {
            State::Unlinked(_) => Err(()),
            State::Linked(inner) => Ok(*inner),
        }
    }
}

impl<Unlinked, Linked> From<Unlinked> for State<Unlinked, Linked> {
    fn from(value: Unlinked) -> Self {
        State::Unlinked(value)
    }
}
