#[macro_export]
macro_rules! impl_node_variant {
    ( impl $enum:ident for $variant:ident throws $error:ident :: $error_variant:ident ) => {
        impl<'a> TryFrom<&'a $enum> for &'a $variant {
            type Error = $error;

            fn try_from(node: &'a $enum) -> Result<Self, Self::Error> {
                match node {
                    $enum::$variant(ref value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $enum> for &'a mut $variant {
            type Error = $error;

            fn try_from(node: &'a mut $enum) -> Result<Self, Self::Error> {
                match node {
                    $enum::$variant(ref mut value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl TryFrom<$enum> for $variant {
            type Error = $error;

            fn try_from(node: $enum) -> Result<Self, Self::Error> {
                match node {
                    $enum::$variant(value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl From<$variant> for $enum {
            fn from(node: $variant) -> Self {
                $enum::$variant(node)
            }
        }
    };
}
#[macro_export]
macro_rules! impl_node_variant_id {
    ( impl $enum:ident for $variant:ident throws $error:ident :: $error_variant:ident) => {
        impl From<NodeID<$variant>> for NodeID<$enum> {
            fn from(node_id: NodeID<$variant>) -> Self {
                // Safety: NodeID does not change its representation with this cast.
                // Since a variant is a subset of an enum then this will always be ok.
                unsafe { std::mem::transmute(node_id) }
            }
        }

        impl $crate::node::AstNode<$variant> {
            pub fn body(&self) -> &$variant {
                self.body.as_ref().unwrap().try_into().unwrap()
            }

            pub fn body_mut(&mut self) -> &mut $variant {
                self.body.as_mut().unwrap().try_into().unwrap()
            }
        }
    };
}

#[macro_export]
macro_rules! impl_node_variant_root {
    ( impl $root:ident for $enum:ident :: $variant:ident throws $error:ident :: $error_variant:ident) => {
        impl<'a> TryFrom<&'a $root> for &'a $variant {
            type Error = $error;

            fn try_from(body: &'a $root) -> std::result::Result<Self, Self::Error> {
                let value = <&$enum>::try_from(body);
                match value {
                    Ok($enum::$variant(ref value)) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $root> for &'a mut $variant {
            type Error = $error;

            fn try_from(body: &'a mut $root) -> std::result::Result<Self, Self::Error> {
                let value = <&mut $enum>::try_from(body);
                match value {
                    Ok($enum::$variant(ref mut value)) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl TryFrom<$root> for $variant {
            type Error = $error;

            fn try_from(body: $root) -> std::result::Result<Self, Self::Error> {
                let value = <$enum>::try_from(body);
                match value {
                    Ok($enum::$variant(value)) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl From<$variant> for $root {
            fn from(variant: $variant) -> Self {
                $root::from($enum::$variant(variant))
            }
        }

        impl TryFrom<&$crate::node::AstNode> for &$crate::node::AstNode<$variant> {
            type Error = $error;

            fn try_from(node: &$crate::node::AstNode) -> std::result::Result<Self, Self::Error> {
                let body = node.body.as_ref().ok_or($error::$error_variant)?;
                match <&$variant>::try_from(body) {
                    Ok(_) => {
                        // Safety: The node must have a body of type $ty, since AstNode
                        // is repr(C), it must adhere to the C layout ABI and therefore
                        // the marker trait <T> will not change the binary representation.
                        let node: &$crate::node::AstNode<$variant> =
                            unsafe { std::mem::transmute(node) };
                        Ok(node)
                    }
                    _ => Err($error::$error_variant),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_node_trait {
    ( impl $enum:ident for { $($variant:ident),* $(,)* } ) => {

      impl Node for $enum {
            fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: $crate::node::NodeUsage) -> Result<NodeType> {
                match ast.get_body(node_id) {
                    $(
                        $enum::$variant(_) => {
                            // Safety: NodeID does not change its representation with this cast.
                            // Since the data is $variant we can update the node_id representation
                            // to reflect this fact.
                            let node_id: NodeID<$variant> = unsafe {std::mem::transmute(node_id) };
                            $variant::node_type(node_id, ast, usage)
                        }
                    ),*
                }
            }

            fn children(&self, context: AstContext) -> NodeIterator<'_> {
                match &self {
                    $(
                        $enum::$variant(value) => value.children(context)
                    ),*
                }
            }

            fn link(node_id: NodeID<Self>, ast: &mut $crate::ast::Ast, context: $crate::ast::AstContext) -> Result<()> {
                match ast.get_body(node_id) {
                    $(
                        $enum::$variant(_) => {
                            // Safety: NodeID does not change its representation with this cast.
                            // Since the data is $variant we can update the node_id representation
                            // to reflect this fact.
                            let node_id: NodeID<$variant> = unsafe {std::mem::transmute(node_id) };
                            $variant::link(node_id, ast, context)
                        }
                    ),*
                }
            }

            fn check(node_id: NodeID<Self>, ast: &mut $crate::ast::Ast) -> Result<()> {
                match ast.get_body(node_id) {
                    $(
                        $enum::$variant(_) => {
                            // Safety: NodeID does not change its representation with this cast.
                            // Since the data is $variant we can update the node_id representation
                            // to reflect this fact.
                            let node_id: NodeID<$variant> = unsafe {std::mem::transmute(node_id) };
                            $variant::check(node_id, ast)
                        }
                    ),*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_node {
    ( pub enum $root:ident => $enum:ident { $($variant:ident),* $(,)* } ) => {

        #[derive(Debug, Clone)]
        pub enum $enum {
          $($variant($variant)),*
        }

        $crate::impl_node_trait!(impl $enum for { $($variant),* });
        $(
            $crate::impl_node_variant!(impl $enum for $variant throws Error::InternalError);
            $crate::impl_node_variant_id!(impl $enum for $variant throws Error::InternalError);
            $crate::impl_node_variant_root!(impl $root for $enum::$variant throws Error::InternalError);
        )*

    };
}

#[macro_export]
macro_rules! impl_root_node {
    ( pub struct $root:ident($body:ident) ) => {
        #[derive(Debug, Clone)]
        #[repr(transparent)]
        pub struct $root($body);

        impl Node for $root {
            fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
                // Safety: Root only has one child, so any $root must also be a $body
                let node_id: NodeID<$body> = unsafe { std::mem::transmute(node_id) };
                $body::node_type(node_id, ast, usage)
            }

            fn children(&self, context: AstContext) -> NodeIterator<'_> {
                self.0.children(context)
            }

            fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> Result<()> {
                // Safety: Root only has one child, so any $root must also be a $body
                let node_id: NodeID<$body> = unsafe { std::mem::transmute(node_id) };
                $body::link(node_id, ast, context)
            }

            fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
                // Safety: Root only has one child, so any $root must also be a $body
                let node_id: NodeID<$body> = unsafe { std::mem::transmute(node_id) };
                $body::check(node_id, ast)
            }

            fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
                self.0.has_variable(var)
            }
        }

        impl From<$root> for $body {
            fn from(root: $root) -> Self {
                root.0
            }
        }

        impl<'a> From<&'a $root> for &'a $body {
            fn from(root: &'a $root) -> Self {
                &root.0
            }
        }

        impl<'a> From<&'a mut $root> for &'a mut $body {
            fn from(root: &'a mut $root) -> Self {
                &mut root.0
            }
        }

        impl From<$body> for $root {
            fn from(body: $body) -> Self {
                $root(body)
            }
        }

        impl From<NodeID> for NodeID<$root> {
            fn from(node_id: NodeID) -> Self {
                // Safety: Root only has one child, so any $root must also be a $body
                unsafe { std::mem::transmute(node_id) }
            }
        }

        impl From<NodeID> for NodeID<$body> {
            fn from(node_id: NodeID) -> Self {
                // Safety: Root only has one child, so any $root must also be a $body
                unsafe { std::mem::transmute(node_id) }
            }
        }

        impl From<NodeID<$root>> for NodeID<$body> {
            fn from(node_id: NodeID<$root>) -> Self {
                // Safety: Root only has one child, so any $root must also be a $body
                unsafe { std::mem::transmute(node_id) }
            }
        }

        impl From<NodeID<$body>> for NodeID<$root> {
            fn from(node_id: NodeID<$body>) -> Self {
                // Safety: Root only has one child, so any $root must also be a $body
                unsafe { std::mem::transmute(node_id) }
            }
        }
    };
}
