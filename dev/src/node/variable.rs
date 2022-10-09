// use crate::ast::{NodeBody, NodeID, NodeUsage};
// use crate::node::{
//     get_node_type, Ast, AstContext, AstNode, AstRootNode, NodeType, Statement, TypeDeclaration,
//     VariableDeclaration,
// };
// use crate::Result;
//
// #[derive(Debug, Clone)]
// pub struct Variable {
//     pub name: String,
//     pub tp: Option<NodeType>,
// }
//
// impl Variable {
//     pub fn new(name: String) -> Self {
//         Variable { name, tp: None }
//     }
// }
//
// impl NodeBody for Variable {
//     type Root = AstRootNode;
//     type NodeType = NodeType;
//     type AstContext = AstContext;
//     type Variable = Variable;
//
//     fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
//         if let Some(tp) = &ast.get_body(node_id).tp {
//             return Ok(tp.clone());
//         }
//
//         let node = ast.get(node_id);
//         let parent_id = node.parent_id.ok_or_else(|| panic!())?;
//         let parent = ast.get(parent_id);
//
//         let statement = <&AstNode<Statement>>::try_from(parent)?.body();
//         match node_usage {
//             NodeUsage::Type => get_node_type(ast, parent_id, node_usage),
//             NodeUsage::Call | NodeUsage::Value => {
//                 let value = statement.value().ok_or_else(|| panic!())?;
//                 get_node_type(ast, value, node_usage)
//             }
//         }
//     }
// }
//
// impl AstNode<Variable> {
//     pub fn type_declaration_id(
//         node_id: NodeID<Variable>,
//         ast: &Ast,
//     ) -> Option<NodeID<TypeDeclaration>> {
//         let node = ast.get(node_id);
//         let parent = ast.get(node.parent_id?);
//         let declaration: &AstNode<TypeDeclaration> = parent.try_into().ok()?;
//         Some(declaration.id)
//     }
//
//     pub fn variable_declaration_id(
//         node_id: NodeID<Variable>,
//         ast: &Ast,
//     ) -> Option<NodeID<VariableDeclaration>> {
//         let node = ast.get(node_id);
//         let parent = ast.get(node.parent_id?);
//         let declaration: &AstNode<VariableDeclaration> = parent.try_into().ok()?;
//         Some(declaration.id)
//     }
// }
