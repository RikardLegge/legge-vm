use std::fmt::Debug;

pub trait _IsInvalid {}
pub trait _IsValid {}
pub trait _IsEmpty {}
pub trait _IsLinked {}
pub trait _IsTypesInferred {}
pub trait _IsTypesChecked {}

pub trait IsEmpty = _IsValid + _IsEmpty + Debug + 'static;
pub trait IsInvalid = _IsInvalid + Debug + 'static;
pub trait IsValid = _IsValid + Debug + 'static;
pub trait IsLinked = _IsValid + _IsLinked + Debug + 'static;
pub trait IsTypesInferred = _IsValid + _IsLinked + _IsTypesInferred + Debug + 'static;
pub trait IsTypesChecked =
    _IsValid + _IsLinked + _IsTypesInferred + _IsTypesChecked + Debug + 'static;

#[derive(Debug, Copy, Clone)]
pub struct Invalid {}
impl _IsInvalid for Invalid {}

#[derive(Debug, Copy, Clone)]
pub struct Empty {}
impl _IsValid for Empty {}
impl _IsEmpty for Empty {}

#[derive(Debug, Copy, Clone)]
pub struct Valid {}
impl _IsValid for Valid {}

#[derive(Debug, Copy, Clone)]
pub struct Linked {}
impl _IsValid for Linked {}
impl _IsLinked for Linked {}

#[derive(Debug, Copy, Clone)]
pub struct TypesInferred {}
impl _IsValid for TypesInferred {}
impl _IsLinked for TypesInferred {}
impl _IsTypesInferred for TypesInferred {}

#[derive(Debug, Copy, Clone)]
pub struct TypesChecked {}
impl _IsValid for TypesChecked {}
impl _IsLinked for TypesChecked {}
impl _IsTypesInferred for TypesChecked {}
impl _IsTypesChecked for TypesChecked {}
