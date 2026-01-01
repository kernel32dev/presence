pub mod debug;
mod impls;
pub mod salvo_oapi;
pub mod serde;
pub mod sqlx;
mod tests;

pub mod prelude {
    pub use crate::Presence::{Absent, Present};
    pub use crate::{OptionPresenceEx, Presence, Undefined};
    pub use presence_macros::presence_aware;
}

/// a unit struct that represents absence
///
/// unlike a presence that is absent, this uses no bytes
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Undefined;

/// like option, except that instead of T possibly being null, it's now possibly undefined
///
/// this has the semantics of Absent being actually absent, so for example, a `Presence<Option<T>>` field on a deserializable struct allows you to determine if the input was defined, null or completely ommited
///
/// in order to have special interactions with [`Debug`], [`serde::Serialize`] and [`serde::Deserialize`], Presence has to not implement any of those traits, which means you can't derive them in a struct that contains a Presence
///
/// to fix this you can add the [`presence_aware`] macro which will override the expansion of those derives to allow structs with presence to correctly implement those traits
///
/// other than that semantic change, this is more or less equivalent to the option, except for some features like `Try` that need unstable to be implemented
#[derive(Copy, Eq, Hash)]
pub enum Presence<T> {
    Present(T),
    Absent,
}

pub use presence_macros::*;

/// adds the methods [`OptionPresenceEx::into_presence`], [`OptionPresenceEx::as_presence_ref`] and [`OptionPresenceEx::as_presence_mut`] to [`Option`]
pub trait OptionPresenceEx {
    type T;
    /// equivalent to
    /// ```ignore
    /// match self {
    ///     Some(x) => Present(x),
    ///     None => Absent,
    /// }
    /// ```
    fn into_presence(self) -> Presence<Self::T>;
    /// equivalent to
    /// ```ignore
    /// match *self {
    ///     Some(ref x) => Present(x),
    ///     None => Absent,
    /// }
    /// ```
    /// or in other words
    /// ```ignore
    /// self.as_ref().into_presence()
    /// ```
    fn as_presence_ref(&self) -> Presence<&Self::T>;
    /// equivalent to
    /// ```ignore
    /// match *self {
    ///     Some(ref mut x) => Present(x),
    ///     None => Absent,
    /// }
    /// ```
    /// or in other words
    /// ```ignore
    /// self.as_mut().into_presence()
    /// ```
    fn as_presence_mut(&mut self) -> Presence<&mut Self::T>;

    /// equivalent to
    /// ```ignore
    /// match *self {
    ///     Some(ref x) => Present(&**x),
    ///     None => Absent,
    /// }
    /// ```
    /// or in other words
    /// ```ignore
    /// self.as_deref().into_presence()
    /// ```
    fn as_presence_deref(
        &self,
    ) -> Presence<&<<Self as OptionPresenceEx>::T as std::ops::Deref>::Target>
    where
        Self::T: std::ops::Deref;
    /// equivalent to
    /// ```ignore
    /// match *self {
    ///     Some(ref mut x) => Present(&mut **x),
    ///     None => Absent,
    /// }
    /// ```
    /// or in other words
    /// ```ignore
    /// self.as_deref_mut().into_presence()
    /// ```
    fn as_presence_deref_mut(
        &mut self,
    ) -> Presence<&mut <<Self as OptionPresenceEx>::T as std::ops::Deref>::Target>
    where
        Self::T: std::ops::DerefMut;
}

impl<T> OptionPresenceEx for Option<T> {
    type T = T;
    #[inline]
    fn into_presence(self) -> Presence<Self::T> {
        match self {
            Some(x) => Presence::Present(x),
            None => Presence::Absent,
        }
    }
    #[inline]
    fn as_presence_ref(&self) -> Presence<&Self::T> {
        match self {
            Some(x) => Presence::Present(x),
            None => Presence::Absent,
        }
    }
    #[inline]
    fn as_presence_mut(&mut self) -> Presence<&mut Self::T> {
        match self {
            Some(x) => Presence::Present(x),
            None => Presence::Absent,
        }
    }
    #[inline]
    fn as_presence_deref(&self) -> Presence<&T::Target>
    where
        T: std::ops::Deref,
    {
        match self {
            Some(x) => Presence::Present(&**x),
            None => Presence::Absent,
        }
    }
    #[inline]
    fn as_presence_deref_mut(&mut self) -> Presence<&mut T::Target>
    where
        T: std::ops::DerefMut,
    {
        match self {
            Some(x) => Presence::Present(&mut **x),
            None => Presence::Absent,
        }
    }
}
