use crate::ast::{NodeBody, NodeID, NodeIDContext, NodeIterator, NodeIteratorBody, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstRootNode, Expression, NodeType, Variable, VariableValue,
};
use crate::Error;

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub variable: NodeID<VariableValue>,
    pub args: Vec<NodeID<Expression>>,
}

impl FunctionCall {
    pub fn new(variable: NodeID<VariableValue>, args: Vec<NodeID<Expression>>) -> Self {
        Self { variable, args }
    }
}

impl NodeBody for FunctionCall {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(
        node_id: NodeID<Self>,
        ast: &Ast,
        node_usage: NodeUsage,
    ) -> crate::Result<NodeType> {
        match node_usage {
            NodeUsage::Type => {
                let body = ast.get_body(node_id);
                get_node_type(ast, body.variable, node_usage)
            }
            NodeUsage::Call | NodeUsage::Value => {
                let body = ast.get_body(node_id);
                get_node_type(ast, body.variable, NodeUsage::Call)
            }
        }
    }

    fn children(&self, context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        let variable = NodeIterator::new(NodeIteratorBody::Single(NodeIDContext {
            node_id: self.variable.into(),
            context,
        }));
        let arguments = NodeIterator::slice(&self.args);
        NodeIterator::chained(variable, arguments)
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let call = ast.get_body(node_id);

        let var_id: NodeID<Variable> = (&ast.get_body(call.variable).variable)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(call.variable.into()))?;
        let var_tp = get_node_type(ast, var_id, NodeUsage::Value)?;

        let func_id = match var_tp {
            NodeType::Function(func_id) => func_id,
            _ => return Err(Error::TypeMissmatch(node_id.into(), var_id.into())),
        };
        let func = ast.get_body(func_id);

        if func.arguments.len() != call.args.len() {
            return Err(Error::TypeMissmatch(node_id.into(), func_id.into()));
        }

        for (func_arg_id, call_arg_id) in func
            .arguments
            .iter()
            .map(ToOwned::to_owned)
            .zip(call.args.iter().map(ToOwned::to_owned))
        {
            let func_arg_tp = get_node_type(ast, func_arg_id, NodeUsage::Type)?;
            let call_arg_tp = get_node_type(ast, call_arg_id, NodeUsage::Type)?;

            if func_arg_tp != call_arg_tp {
                return Err(Error::TypeMissmatch(func_arg_id.into(), call_arg_id.into()));
            }
        }

        Ok(())
    }
}
