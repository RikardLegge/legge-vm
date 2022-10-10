#[macro_export]
macro_rules! impl_node_conversions {
    (
        Storage = $storage:ident,
        $enum:ident for $parent:ident
    ) => {
        impl From<$crate::ast::NodeID<$enum>> for $crate::ast::NodeID<$parent> {
            fn from(id: $crate::ast::NodeID<$enum>) -> Self {
                $crate::ast::NodeID::new(id.inner())
            }
        }

        impl From<$crate::ast::AstNode<$enum>> for $crate::ast::AstNode<$parent> {
            fn from(node: $crate::ast::AstNode<$enum>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl<'a> From<&'a $crate::ast::AstNode<$enum>> for &'a $crate::ast::AstNode<$parent> {
            fn from(node: &'a $crate::ast::AstNode<$enum>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl<'a> From<&'a mut $crate::ast::AstNode<$enum>>
            for &'a mut $crate::ast::AstNode<$parent>
        {
            fn from(node: &'a mut $crate::ast::AstNode<$enum>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl TryFrom<$crate::ast::AstNode<$parent>> for $crate::ast::AstNode<$enum> {
            type Error = ();

            fn try_from(node: $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$enum(value) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        unsafe { std::mem::transmute(node) }
                    }
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a $crate::ast::AstNode<$parent>> for &'a $crate::ast::AstNode<$enum> {
            type Error = ();

            fn try_from(node: &'a $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$enum(value) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        unsafe { std::mem::transmute(node) }
                    }
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $crate::ast::AstNode<$parent>>
            for &'a mut $crate::ast::AstNode<$enum>
        {
            type Error = ();

            fn try_from(node: &'a mut $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$enum(value) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        unsafe { std::mem::transmute(node) }
                    }
                    _ => Err(()),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! build_ast_node_child {
    (
        Root = $root:ident,
        Storage = $storage:ident,
        Parents = [$($parent:ident),* $(,)?] + $item:ident,
        $enum:ident $($rest:tt)+
    ) => {
        $crate::build_ast_node_child!(
            Root = $root,
            Storage = $storage,
            Parents = [$item, $($parent),*],
            $enum $($rest)*
        );

        $($crate::impl_node_conversions!(
            Storage = $storage,
            $enum for $parent
        );)*
        $crate::impl_node_conversions!(
            Storage = $storage,
            $enum for $item
        );
    };

    (
        Root = $root:ident,
        Storage = $storage:ident,
        Parents = $parents:tt,
        $enum:ident
        [
            $($variant_name:ident $variant_body:tt),* $(,)?
        ]
    ) => {
        $(
            $crate::build_ast_node_child!(
                Root = $root,
                Storage = $storage,
                Parents = $parents + $enum,
                $variant_name $variant_body
            );
        )*

        #[derive(Debug)]
        pub struct $enum();

        impl $crate::ast::NodeBody for $enum {
            type Root = $root;
            type Data = $enum;
        }

        impl From<$enum> for $storage {
            fn from(_: $enum) -> Self {
                Storage::$enum(())
            }
        }

        impl From<&$enum> for $storage {
            fn from(_: &$enum) -> Self {
                Storage::$enum(())
            }
        }
    };

    (
        Root = $root:ident,
        Storage = $storage:ident,
        Parents = [$($parent:ident),* $(,)?],
        $leaf:ident($data:ident)
    ) => {

        #[derive(Debug)]
        pub struct $leaf();

        impl $crate::ast::NodeBody for $leaf {
            type Root = $root;
            type Data = $data;
        }

        // Safety: Unclear why this has to be unsafe.
        // Better (un)safe than sorry?
        unsafe impl $crate::ast::NodeData for $data {
            type Node = $leaf;
        }

        $crate::impl_storage_conversions!($leaf($data) for $storage);
    };
}

#[macro_export]
macro_rules! impl_storage_conversions {
    ($leaf:ident($data:ident) for $storage:ident) => {
        impl From<$data> for $storage {
            fn from(value: $data) -> Self {
                Storage::$leaf(value)
            }
        }

        impl TryFrom<$storage> for $data {
            type Error = ();

            fn try_from(storage: $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$leaf(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a $storage> for &'a $data {
            type Error = ();

            fn try_from(storage: &'a $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$leaf(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $storage> for &'a mut $data {
            type Error = ();

            fn try_from(storage: &'a mut $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$leaf(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! build_ast {
    (
        Storage = $storage:ident,
        $root:ident $($rest:tt)?
    ) => {

        $crate::build_ast_node_child!(
            Root = $root,
            Storage = $storage,
            Parents = [],
            $root $($rest)*
        );

        impl Default for $storage {
            fn default() -> Self {
                Self::$root(())
            }
        }

        impl $crate::ast::NodeDataStorage for $root {
            type Storage = $storage;
        }
    };
}
