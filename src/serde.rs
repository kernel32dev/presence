use crate::{Presence, Undefined};

pub use serde::*;

#[repr(transparent)]
pub struct PresenceAwareSerialize<T: ?Sized>(T);

impl<T: ?Sized> PresenceAwareSerialize<T> {
    pub const fn new(value: &T) -> &Self {
        unsafe { &*(value as *const T as *const Self) }
    }
    pub fn skip_serializing_if(this: &&Self) -> bool
    where
        Self: PresenceAwareSerializeEx,
    {
        PresenceAwareSerializeEx::skip_serializing_if(this)
    }
}

pub trait PresenceAwareSerializeEx {
    fn skip_serializing_if(this: &&Self) -> bool;
}

impl<T: ?Sized + serde::Serialize> PresenceAwareSerializeEx for PresenceAwareSerialize<T> {
    fn skip_serializing_if(_: &&Self) -> bool {
        false
    }
}

impl<T: serde::Serialize> PresenceAwareSerializeEx for PresenceAwareSerialize<Presence<T>> {
    fn skip_serializing_if(this: &&Self) -> bool {
        this.0.is_absent()
    }
}

impl PresenceAwareSerializeEx for Undefined {
    fn skip_serializing_if(_: &&Self) -> bool {
        true
    }
}

impl<T: ?Sized + serde::Serialize> serde::Serialize for PresenceAwareSerialize<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<T: serde::Serialize> serde::Serialize for PresenceAwareSerialize<Presence<T>> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.0 {
            Presence::Present(value) => value.serialize(serializer),
            Presence::Absent => Err(serde::ser::Error::custom(
                "can't serialize Presence::Absent",
            )),
        }
    }
}

impl serde::Serialize for PresenceAwareSerialize<Undefined> {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(
            "can't serialize Undefined",
        ))
    }
}

#[repr(transparent)]
pub struct PresenceAwareDeserialize<T>(pub Presence<T>);

impl<T> Default for PresenceAwareDeserialize<T> {
    fn default() -> Self {
        Self(Presence::Absent)
    }
}

pub trait PresenceAwareDeserializeEx<'de>: Sized {
    /// returns None if the value is absent and invalid
    ///
    /// returns Some if the value is present or absent and still valid despite being absent
    fn resolve_presence(this: Presence<Self>) -> Option<Self>;
}

impl<'de, T: serde::Deserialize<'de>> PresenceAwareDeserializeEx<'de> for T {
    fn resolve_presence(this: Presence<Self>) -> Option<Self> {
        this.into_opt()
    }
}

impl<'de, T: serde::Deserialize<'de>> PresenceAwareDeserializeEx<'de> for Presence<T> {
    fn resolve_presence(this: Presence<Self>) -> Option<Self> {
        Some(this.flatten())
    }
}

impl<'de> PresenceAwareDeserializeEx<'de> for Undefined {
    fn resolve_presence(_: Presence<Self>) -> Option<Self> {
        Some(Undefined)
    }
}

impl<'de, T> serde::Deserialize<'de> for PresenceAwareDeserialize<T>
where
    T: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer).map(|value| PresenceAwareDeserialize(Presence::Present(value)))
    }
}

impl<'de, T: serde::Deserialize<'de>> serde::Deserialize<'de>
    for PresenceAwareDeserialize<Presence<T>>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer)
            .map(|value| PresenceAwareDeserialize(Presence::Present(Presence::Present(value))))
    }
}

impl<'de> serde::Deserialize<'de>
    for PresenceAwareDeserialize<Undefined>
{
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(PresenceAwareDeserialize(Presence::Present(Undefined)))
    }
}
