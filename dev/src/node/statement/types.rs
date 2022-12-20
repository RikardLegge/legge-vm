// use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
// use crate::node::{Ast, AstContext, AstRootNode, Expression, NodeType, Variable};
// use std::collections::HashMap;

// #[derive(Debug, Clone)]
// pub struct TypeDeclaration {
//     pub variable: NodeID<Variable>,
//     pub constructor: NodeID<Expression>,
//     pub fields: Vec<NodeID>,
//     pub associated_values: HashMap<String, NodeID<Variable>>,
// }
//
// impl TypeDeclaration {
//     pub fn new(variable: NodeID<Variable>, constructor: NodeID<Expression>) -> Self {
//         TypeDeclaration {
//             variable,
//             constructor,
//             fields: Vec::new(),
//             associated_values: HashMap::new(),
//         }
//     }
// }

// impl NodeBody for TypeDeclaration {
//     type Root = AstRootNode;
//     type NodeType = NodeType;
//     type AstContext = AstContext;
//     type Variable = Variable;

    // fn node_type(node_id: NodeID<Self>, _: &Ast, node_usage: NodeUsage) -> crate::Result<NodeType> {
    //     match node_usage {
    //         NodeUsage::Type => Ok(NodeType::Custom(node_id)),
    //         NodeUsage::Call | NodeUsage::Value => Ok(NodeType::Void),
    //     }
    // }

    // fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
    //     let props = NodeIterator::dual(self.variable, self.constructor);
    //     let fields = NodeIterator::slice(&self.fields);
    //     NodeIterator::chained(props, fields)
    // }

    // fn has_variable(&self, var: &str) -> crate::Result<Option<NodeID<Variable>>> {
    //     if let Some(variable_id) = self.associated_values.get(var) {
    //         Ok(Some(*variable_id))
    //     } else {
    //         Ok(None)
    //     }
    // }
}
