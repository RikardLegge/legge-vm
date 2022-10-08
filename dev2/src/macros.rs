#[macro_export]
macro_rules! build_ast_node_child {
    (
        Root = $root:ident,
        Variants = $variants:ident,
        Storage = $storage:ident,
        Parents = [$($parent:ident),* $(,)?] + $item:ident,
        Empty = $empty:ident,
        $enum:ident $rest:tt
    ) => {
        $crate::build_ast_node_child!(
            Root = $root,
            Variants = $variants,
            Storage = $storage,
            Parents = [$item, $($parent),*],
            Empty = $empty,
            $enum $rest
        );

        $(
            impl From<$crate::ast::NodeID<$enum>> for $crate::ast::NodeID<$parent> {
                fn from(id: $crate::ast::NodeID<$enum>) -> Self {
                    unsafe {std::mem::transmute(id)}
                }
            }
        )*

        impl From<$crate::ast::NodeID<$enum>> for $crate::ast::NodeID<$item> {
            fn from(id: $crate::ast::NodeID<$enum>) -> Self {
                unsafe {std::mem::transmute(id)}
            }
        }
    };

    (
        Root = $root:ident,
        Variants = $variants:ident,
        Storage = $storage:ident,
        Parents = $parents:tt,
        Empty = $empty:ident,
        $enum:ident
        [
            $($variant_name:ident $variant_body:tt),* $(,)?
        ]
    ) => {
        $(
            $crate::build_ast_node_child!(
                Root = $root,
                Variants = $variants,
                Storage = $storage,
                Parents = $parents + $enum,
                Empty = $empty,
                $variant_name $variant_body
            );

        )*

        #[derive(Debug)]
        pub struct $enum();
        // enum $enum {
        //     $($variant_name( $variant_name )),*
        // }

        impl $crate::ast::NodeBody for $enum {
            type Root = $root;
            type Variants = $variants;
            type Data = $empty;

            unsafe fn variant() -> Self::Variants {
                Self::Variants::$enum($enum())
            }
        }
    };

    (
        Root = $root:ident,
        Variants = $variants:ident,
        Storage = $storage:ident,
        Parents = [$($parent:ident),* $(,)?],
        Empty = $empty:ident,
        $leaf:ident($data:ident)
    ) => {

        #[derive(Debug)]
        pub struct $leaf();

        impl $crate::ast::NodeBody for $leaf {
            type Root = $root;
            type Variants = $variants;
            type Data = $data;

            unsafe fn variant() -> Self::Variants {
                Self::Variants::$leaf($leaf())
            }
        }

        impl $crate::ast::NodeData for $data {
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
                Storage::$data(value)
            }
        }

        impl TryFrom<$storage> for $data {
            type Error = ();

            fn try_from(storage: $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$data(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a $storage> for &'a $data {
            type Error = ();

            fn try_from(storage: &'a $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$data(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $storage> for &'a mut $data {
            type Error = ();

            fn try_from(storage: &'a mut $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$data(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! get_ast_variants {
    (@root $rest:tt) => {
        test [ $crate::get_ast_variants!($rest) ]
    };
    ($rest:tt) => {
        $item,
    }
}

#[macro_export]
macro_rules! build_ast {
    (
        Storage = $storage:ident,
        Variants = $variants:ident,
        Empty = $empty:ident,
        $root:ident $rest:tt
    ) => {

        $crate::build_ast_node_child!(
            Root = $root,
            Variants = $variants,
            Storage = $storage,
            Parents = [],
            Empty = $empty,
            $root $rest
        );

        $crate::impl_storage_conversions!($empty($empty) for $storage);

        impl Default for $storage {
            fn default() -> Self {
                Self::$empty($empty())
            }
        }

        impl $crate::ast::NodeDataStorage for $root {
            type Storage = $storage;
        }
    };
}
