use crate::ast::AstContext;
use crate::node::{FunctionDeclaration, NodeIterator, NodeType, NodeUsage, TypeDeclaration};
use crate::{Ast, AstNode, Error, Expression, Result, State, Statement};
use crate::{Node, NodeID};

#[derive(Debug, Clone)]
pub struct Return {
    pub func: State<(), NodeID<FunctionDeclaration>>,
    pub value: Option<NodeID<Expression>>,
}

impl Node for Return {
    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        match self.value {
            Some(value) => NodeIterator::single(value),
            None => NodeIterator::empty(),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> Result<()> {
        let func = ast.walk_up(
            node_id,
            |node| match <&AstNode<FunctionDeclaration>>::try_from(node) {
                Ok(node) => Ok(Some(node.id)),
                Err(_) => Ok(None),
            },
        )?;
        if let Some(func) = func {
            ast.get_inner_mut(node_id).func = State::Linked(func);
            Ok(())
        } else {
            panic!();
        }
    }
}

#[derive(Debug, Clone)]
pub struct NodeState {
    pub state: State<String, NodeID<Variable>>,
}

impl Node for NodeState {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        let body = ast.get_body(node_id);
        match &body.state {
            State::Unlinked(_) => Err(Error::TypeNotInferred),
            State::Linked(linked_id) => match usage {
                NodeUsage::Type | NodeUsage::Value => {
                    ast.get_node_type(*linked_id, NodeUsage::Type)
                }
                NodeUsage::Call => Err(Error::TypeNotInferred),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub tp: Option<NodeType>,
}

impl AstNode<Variable> {
    pub fn type_declaration_id(
        node_id: NodeID<Variable>,
        ast: &Ast,
    ) -> Option<NodeID<TypeDeclaration>> {
        let node = ast.get(node_id);
        let parent = ast.get(node.parent_id?);
        let declaration: &AstNode<TypeDeclaration> = parent.try_into().ok()?;
        Some(declaration.id)
    }
}

impl Variable {
    pub fn new(name: String) -> Self {
        Variable { name, tp: None }
    }
}

impl Node for Variable {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        if let Some(tp) = &ast.get_body(node_id).tp {
            return Ok(tp.clone());
        }

        let node = ast.get(node_id);
        let parent_id = node.parent_id.ok_or_else(|| panic!())?;
        let parent = ast.get(parent_id);

        let statement: &Statement = parent.body.as_ref().unwrap().try_into()?;
        match node_usage {
            NodeUsage::Type => ast.get_node_type(parent_id, node_usage),
            NodeUsage::Call | NodeUsage::Value => {
                let value = statement.value().ok_or_else(|| panic!())?;
                ast.get_node_type(value, node_usage)
            }
        }
    }
}
