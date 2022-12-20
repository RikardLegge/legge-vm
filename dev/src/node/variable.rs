use crate::ast::AstContext;
use crate::node::{FunctionDeclaration, Loop, NodeIterator, NodeType, NodeUsage, TypeDeclaration};
use crate::{Ast, AstNode, Error, Expression, Result, State, Statement, VariableDeclaration};
use crate::{Node, NodeID};

#[derive(Debug, Clone)]
pub struct Break {
    pub r#loop: State<(), NodeID<Loop>>,
    pub value: Option<NodeID<Expression>>,
}

impl Node for Break {
    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        match self.value {
            Some(value) => NodeIterator::single(value),
            None => NodeIterator::empty(),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> Result<()> {
        let node = ast.get_body(node_id);
        let loop_id = match node.r#loop {
            State::Unlinked(_) => {
                let loop_id =
                    ast.walk_up(node_id, |node| match <&AstNode<Loop>>::try_from(node) {
                        Ok(node) => Ok(Some(node.id)),
                        Err(_) => Ok(None),
                    })?;
                if let Some(loop_id) = loop_id {
                    ast.get_body_mut(node_id).r#loop = State::Linked(loop_id);
                    loop_id
                } else {
                    unimplemented!();
                }
            }
            State::Linked(id) => id,
        };

        let loop_node = ast.get_body(loop_id);
        if loop_node.value.is_none() {
            let node = ast.get_body(node_id);
            let tp = if let Some(value) = node.value {
                ast.get_node_type(value, NodeUsage::Value)?
            } else {
                NodeType::Void
            };
            let loop_node = ast.get_body_mut(loop_id);
            loop_node.value = Some(tp);
        };

        Ok(())
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node = ast.get_body(node_id);
        let tp = if let Some(value) = node.value {
            ast.get_node_type(value, NodeUsage::Value)?
        } else {
            NodeType::Void
        };
        let loop_id: NodeID<Loop> = (&node.r#loop)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;
        let loop_tp = ast
            .get_body(loop_id)
            .value
            .as_ref()
            .ok_or_else(|| Error::TypeNotInferred(loop_id.into()))?;

        if &tp == loop_tp {
            Ok(())
        } else {
            Err(Error::TypeMissmatch(node_id.into(), loop_id.into()))
        }
    }
}

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
            ast.get_body_mut(node_id).func = State::Linked(func);
            Ok(())
        } else {
            panic!();
        }
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let body = ast.get_body(node_id);
        let func: NodeID<FunctionDeclaration> = (&body.func)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;

        let expected_return = &ast.get_body(func).returns;
        let got_return = body
            .value
            .map(|id| ast.get_node_type(id, NodeUsage::Value))
            .transpose()?
            .unwrap_or(NodeType::Void);

        if &got_return == expected_return {
            Ok(())
        } else {
            let return_stmt_id = body
                .value
                .map(|id| id.into())
                .unwrap_or_else(|| node_id.into());
            Err(Error::TypeMissmatch(func.into(), return_stmt_id))
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

    pub fn variable_declaration_id(
        node_id: NodeID<Variable>,
        ast: &Ast,
    ) -> Option<NodeID<VariableDeclaration>> {
        let node = ast.get(node_id);
        let parent = ast.get(node.parent_id?);
        let declaration: &AstNode<VariableDeclaration> = parent.try_into().ok()?;
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

        let statement = <&AstNode<Statement>>::try_from(parent)?.body();
        match node_usage {
            NodeUsage::Type => ast.get_node_type(parent_id, node_usage),
            NodeUsage::Call | NodeUsage::Value => {
                let value = statement.value().ok_or_else(|| panic!())?;
                ast.get_node_type(value, node_usage)
            }
        }
    }
}
