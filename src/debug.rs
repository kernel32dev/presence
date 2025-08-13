use std::fmt::{Debug, Display};

use crate::{Presence, Undefined};

pub trait PresenceAwareDebug {
    fn as_presence_aware_debug(this: &Self) -> Option<&dyn Debug>;
}

impl<T: Debug> PresenceAwareDebug for T {
    fn as_presence_aware_debug(this: &Self) -> Option<&dyn Debug> {
        Some(this)
    }
}

impl<T: PresenceAwareDebug> PresenceAwareDebug for Presence<T> {
    fn as_presence_aware_debug(this: &Self) -> Option<&dyn Debug> {
        match this {
            Self::Present(value) => T::as_presence_aware_debug(value),
            Self::Absent => None,
        }
    }
}

impl PresenceAwareDebug for Undefined {
    fn as_presence_aware_debug(_: &Self) -> Option<&dyn Debug> {
        None
    }
}

#[derive(Clone, Copy)]
pub struct DebugPresence<'a, T>(Presence<&'a T>);

impl<'a, T: Debug> Debug for DebugPresence<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Presence::Present(value) => f.debug_tuple("Present").field(value).finish(),
            Presence::Absent => f.write_str("Absent"),
        }
    }
}

impl<T> Presence<T> {
    /// returns a reference that implements `Debug`
    pub fn debug(&self) -> DebugPresence<'_, T> {
        DebugPresence(self.as_ref())
    }
}

#[derive(Clone, Copy)]
pub struct DisplayPresence<'a, T>(Presence<&'a T>);

impl<'a, T: Display> Display for DisplayPresence<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Presence::Present(value) => std::fmt::Display::fmt(value, f),
            Presence::Absent => Ok(()),
        }
    }
}

impl<T> Presence<T> {
    /// returns a reference that implements `Display`
    ///
    /// when `Present`, simply displays the content
    ///
    /// when `Absent` displays nothing
    pub fn display(&self) -> DisplayPresence<'_, T> {
        DisplayPresence(self.as_ref())
    }
}
