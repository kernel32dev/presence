use crate::Presence::{self, Absent, Present};

use std::iter::FusedIterator;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::{cmp, hint, mem};

// methods and impls not inherited from Option
impl<T> Presence<T> {
    pub fn into_opt(self) -> Option<T> {
        match self {
            Present(x) => Some(x),
            Absent => None,
        }
    }
    pub fn as_opt_ref(&self) -> Option<&T> {
        match self {
            Present(x) => Some(x),
            Absent => None,
        }
    }
    pub fn as_opt_mut(&mut self) -> Option<&mut T> {
        match self {
            Present(x) => Some(x),
            Absent => None,
        }
    }
    pub fn as_opt_deref(&self) -> Option<&T::Target>
    where
        T: Deref,
    {
        match self {
            Present(x) => Some(&**x),
            Absent => None,
        }
    }
    pub fn as_opt_deref_mut(&mut self) -> Option<&mut T::Target>
    where
        T: DerefMut,
    {
        match self {
            Present(x) => Some(&mut **x),
            Absent => None,
        }
    }
}
impl<T> From<Option<T>> for Presence<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => Present(x),
            None => Absent,
        }
    }
}

impl<T> From<Presence<T>> for Option<T> {
    fn from(value: Presence<T>) -> Self {
        match value {
            Present(x) => Some(x),
            Absent => None,
        }
    }
}
// methods and impls inherited from Option
impl<T> Default for Presence<T> {
    fn default() -> Self {
        Self::Absent
    }
}

impl<T> Presence<T> {
    #[must_use = "if you intended to assert that this has a value, consider `.unwrap()` instead"]
    #[inline]
    pub const fn is_present(&self) -> bool {
        matches!(*self, Present(_))
    }
    #[must_use]
    #[inline]
    pub fn is_present_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Absent => false,
            Present(x) => f(x),
        }
    }
    #[must_use = "if you intended to assert that this doesn't have a value, consider \
                  wrapping this in an `assert!()` instead"]
    #[inline]
    pub const fn is_absent(&self) -> bool {
        !self.is_present()
    }
    #[must_use]
    #[inline]
    pub fn is_absent_or(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Absent => true,
            Present(x) => f(x),
        }
    }
    #[inline]
    pub const fn as_ref(&self) -> Presence<&T> {
        match *self {
            Present(ref x) => Present(x),
            Absent => Absent,
        }
    }
    #[inline]
    pub const fn as_mut(&mut self) -> Presence<&mut T> {
        match *self {
            Present(ref mut x) => Present(x),
            Absent => Absent,
        }
    }
    #[inline]
    #[must_use]
    pub const fn as_pin_ref(self: Pin<&Self>) -> Presence<Pin<&T>> {
        match Pin::get_ref(self).as_ref() {
            Present(x) => unsafe { Present(Pin::new_unchecked(x)) },
            Absent => Absent,
        }
    }
    #[inline]
    #[must_use]
    pub const fn as_pin_mut(self: Pin<&mut Self>) -> Presence<Pin<&mut T>> {
        unsafe {
            match Pin::get_unchecked_mut(self).as_mut() {
                Present(x) => Present(Pin::new_unchecked(x)),
                Absent => Absent,
            }
        }
    }
    #[inline]
    #[must_use]
    pub const fn as_slice(&self) -> &[T] {
        match self {
            Present(x) => std::slice::from_ref(x),
            Absent => &[],
        }
    }
    #[inline]
    #[must_use]
    pub const fn as_mut_slice(&mut self) -> &mut [T] {
        match self {
            Present(x) => std::slice::from_mut(x),
            Absent => &mut [],
        }
    }
    #[inline]
    #[track_caller]
    pub fn expect(self, msg: &str) -> T {
        match self {
            Present(val) => val,
            Absent => expect_failed(msg),
        }
    }
    #[inline(always)]
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            Present(val) => val,
            Absent => unwrap_failed(),
        }
    }
    #[inline]
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Present(x) => x,
            Absent => default,
        }
    }
    #[inline]
    #[track_caller]
    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            Present(x) => x,
            Absent => f(),
        }
    }
    #[inline]
    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Present(x) => x,
            Absent => T::default(),
        }
    }
    #[inline]
    #[track_caller]
    pub unsafe fn unwrap_unchecked(self) -> T {
        match self {
            Present(val) => val,
            Absent => unsafe { hint::unreachable_unchecked() },
        }
    }
    #[inline]
    pub fn map<U, F>(self, f: F) -> Presence<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Present(x) => Present(f(x)),
            Absent => Absent,
        }
    }
    #[inline]
    pub fn inspect<F: FnOnce(&T)>(self, f: F) -> Self {
        if let Present(ref x) = self {
            f(x);
        }
        self
    }
    #[inline]
    #[must_use = "if you don't need the returned value, use `if let` instead"]
    pub fn map_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Present(t) => f(t),
            Absent => default,
        }
    }
    #[inline]
    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(T) -> U,
    {
        match self {
            Present(t) => f(t),
            Absent => default(),
        }
    }
    #[inline]
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Present(v) => Ok(v),
            Absent => Err(err),
        }
    }
    #[inline]
    pub fn ok_or_else<E, F>(self, err: F) -> Result<T, E>
    where
        F: FnOnce() -> E,
    {
        match self {
            Present(v) => Ok(v),
            Absent => Err(err()),
        }
    }
    #[inline]
    pub fn as_deref(&self) -> Presence<&T::Target>
    where
        T: Deref,
    {
        self.as_ref().map(|t| t.deref())
    }
    #[inline]
    pub fn as_deref_mut(&mut self) -> Presence<&mut T::Target>
    where
        T: DerefMut,
    {
        self.as_mut().map(|t| t.deref_mut())
    }
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: Item {
                opt: self.as_opt_ref(),
            },
        }
    }
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            inner: Item {
                opt: self.as_opt_mut(),
            },
        }
    }
    #[inline]
    pub fn and<U>(self, optb: Presence<U>) -> Presence<U> {
        match self {
            Present(_) => optb,
            Absent => Absent,
        }
    }
    #[doc(alias = "flatmap")]
    #[inline]
    pub fn and_then<U, F>(self, f: F) -> Presence<U>
    where
        F: FnOnce(T) -> Presence<U>,
    {
        match self {
            Present(x) => f(x),
            Absent => Absent,
        }
    }
    #[inline]
    pub fn filter<P>(self, predicate: P) -> Self
    where
        P: FnOnce(&T) -> bool,
    {
        if let Present(x) = self {
            if predicate(&x) {
                return Present(x);
            }
        }
        Absent
    }
    #[inline]
    pub fn or(self, optb: Presence<T>) -> Presence<T> {
        match self {
            x @ Present(_) => x,
            Absent => optb,
        }
    }
    #[inline]
    pub fn or_else<F>(self, f: F) -> Presence<T>
    where
        F: FnOnce() -> Presence<T>,
    {
        match self {
            x @ Present(_) => x,
            Absent => f(),
        }
    }
    #[inline]
    pub fn xor(self, optb: Presence<T>) -> Presence<T> {
        match (self, optb) {
            (a @ Present(_), Absent) => a,
            (Absent, b @ Present(_)) => b,
            _ => Absent,
        }
    }
    #[must_use = "if you intended to set a value, consider assignment instead"]
    #[inline]
    pub fn insert(&mut self, value: T) -> &mut T {
        *self = Present(value);
        unsafe { self.as_mut().unwrap_unchecked() }
    }
    #[inline]
    pub fn get_or_insert(&mut self, value: T) -> &mut T {
        self.get_or_insert_with(|| value)
    }
    #[inline]
    pub fn get_or_insert_default(&mut self) -> &mut T
    where
        T: Default,
    {
        self.get_or_insert_with(T::default)
    }
    #[inline]
    pub fn get_or_insert_with<F>(&mut self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        if let Absent = self {
            *self = Present(f());
        }
        unsafe { self.as_mut().unwrap_unchecked() }
    }
    #[inline]
    pub const fn take(&mut self) -> Presence<T> {
        mem::replace(self, Absent)
    }
    #[inline]
    pub fn take_if<P>(&mut self, predicate: P) -> Presence<T>
    where
        P: FnOnce(&mut T) -> bool,
    {
        if self.as_mut().map_or(false, predicate) {
            self.take()
        } else {
            Absent
        }
    }
    #[inline]
    pub const fn replace(&mut self, value: T) -> Presence<T> {
        mem::replace(self, Present(value))
    }
    pub fn zip<U>(self, other: Presence<U>) -> Presence<(T, U)> {
        match (self, other) {
            (Present(a), Present(b)) => Present((a, b)),
            _ => Absent,
        }
    }
}
impl<T, U> Presence<(T, U)> {
    #[inline]
    pub fn unzip(self) -> (Presence<T>, Presence<U>) {
        match self {
            Present((a, b)) => (Present(a), Present(b)),
            Absent => (Absent, Absent),
        }
    }
}
impl<T> Presence<&T> {
    #[must_use = "`self` will be dropped if the result is not used"]
    pub const fn copied(self) -> Presence<T>
    where
        T: Copy,
    {
        match self {
            Present(&v) => Present(v),
            Absent => Absent,
        }
    }
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn cloned(self) -> Presence<T>
    where
        T: Clone,
    {
        match self {
            Present(t) => Present(t.clone()),
            Absent => Absent,
        }
    }
}
impl<T> Presence<&mut T> {
    #[must_use = "`self` will be dropped if the result is not used"]
    pub const fn copied(self) -> Presence<T>
    where
        T: Copy,
    {
        match self {
            Present(&mut t) => Present(t),
            Absent => Absent,
        }
    }
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn cloned(self) -> Presence<T>
    where
        T: Clone,
    {
        match self {
            Present(t) => Present(t.clone()),
            Absent => Absent,
        }
    }
}
impl<T, E> Presence<Result<T, E>> {
    #[inline]
    pub fn transpose(self) -> Result<Presence<T>, E> {
        match self {
            Present(Ok(x)) => Ok(Present(x)),
            Present(Err(e)) => Err(e),
            Absent => Ok(Absent),
        }
    }
}
#[inline]
#[cold]
#[track_caller]
const fn unwrap_failed() -> ! {
    None.unwrap()
}
#[inline]
#[cold]
#[track_caller]
const fn expect_failed(msg: &str) -> ! {
    None.expect(msg)
}
impl<T> Clone for Presence<T>
where
    T: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        match self {
            Present(x) => Present(x.clone()),
            Absent => Absent,
        }
    }
    #[inline]
    fn clone_from(&mut self, source: &Self) {
        match (self, source) {
            (Present(to), Present(from)) => to.clone_from(from),
            (to, from) => *to = from.clone(),
        }
    }
}
impl<T> IntoIterator for Presence<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    #[inline]
    fn into_iter(self) -> IntoIter<T> {
        IntoIter {
            inner: Item {
                opt: self.into_opt(),
            },
        }
    }
}
impl<'a, T> IntoIterator for &'a Presence<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}
impl<'a, T> IntoIterator for &'a mut Presence<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;
    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}
impl<T> From<T> for Presence<T> {
    fn from(val: T) -> Presence<T> {
        Present(val)
    }
}
impl<'a, T> From<&'a Presence<T>> for Presence<&'a T> {
    fn from(o: &'a Presence<T>) -> Presence<&'a T> {
        o.as_ref()
    }
}
impl<'a, T> From<&'a mut Presence<T>> for Presence<&'a mut T> {
    fn from(o: &'a mut Presence<T>) -> Presence<&'a mut T> {
        o.as_mut()
    }
}
impl<T: PartialEq> PartialEq for Presence<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Present(l), Present(r)) => *l == *r,
            (Present(_), Absent) => false,
            (Absent, Present(_)) => false,
            (Absent, Absent) => true,
        }
    }
}
impl<T: PartialOrd> PartialOrd for Presence<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Present(l), Present(r)) => l.partial_cmp(r),
            (Present(_), Absent) => Some(cmp::Ordering::Greater),
            (Absent, Present(_)) => Some(cmp::Ordering::Less),
            (Absent, Absent) => Some(cmp::Ordering::Equal),
        }
    }
}
impl<T: Ord> Ord for Presence<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Present(l), Present(r)) => l.cmp(r),
            (Present(_), Absent) => cmp::Ordering::Greater,
            (Absent, Present(_)) => cmp::Ordering::Less,
            (Absent, Absent) => cmp::Ordering::Equal,
        }
    }
}
#[derive(Clone, Debug)]
struct Item<A> {
    opt: Option<A>,
}
impl<A> Iterator for Item<A> {
    type Item = A;
    #[inline]
    fn next(&mut self) -> Option<A> {
        self.opt.take()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}
impl<A> DoubleEndedIterator for Item<A> {
    #[inline]
    fn next_back(&mut self) -> Option<A> {
        self.opt.take()
    }
}
impl<A> ExactSizeIterator for Item<A> {
    #[inline]
    fn len(&self) -> usize {
        match self.opt {
            Some(_) => 1,
            None => 0,
        }
    }
}
impl<A> FusedIterator for Item<A> {}
#[derive(Debug)]
pub struct Iter<'a, A: 'a> {
    inner: Item<&'a A>,
}
impl<'a, A> Iterator for Iter<'a, A> {
    type Item = &'a A;
    #[inline]
    fn next(&mut self) -> Option<&'a A> {
        self.inner.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}
impl<'a, A> DoubleEndedIterator for Iter<'a, A> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a A> {
        self.inner.next_back()
    }
}
impl<A> ExactSizeIterator for Iter<'_, A> {}
impl<A> FusedIterator for Iter<'_, A> {}
impl<A> Clone for Iter<'_, A> {
    #[inline]
    fn clone(&self) -> Self {
        Iter {
            inner: self.inner.clone(),
        }
    }
}
#[derive(Debug)]
pub struct IterMut<'a, A: 'a> {
    inner: Item<&'a mut A>,
}
impl<'a, A> Iterator for IterMut<'a, A> {
    type Item = &'a mut A;
    #[inline]
    fn next(&mut self) -> Option<&'a mut A> {
        self.inner.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}
impl<'a, A> DoubleEndedIterator for IterMut<'a, A> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut A> {
        self.inner.next_back()
    }
}
impl<A> ExactSizeIterator for IterMut<'_, A> {}
impl<A> FusedIterator for IterMut<'_, A> {}
#[derive(Clone, Debug)]
pub struct IntoIter<A> {
    inner: Item<A>,
}
impl<A> Iterator for IntoIter<A> {
    type Item = A;
    #[inline]
    fn next(&mut self) -> Option<A> {
        self.inner.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}
impl<A> DoubleEndedIterator for IntoIter<A> {
    #[inline]
    fn next_back(&mut self) -> Option<A> {
        self.inner.next_back()
    }
}
impl<A> ExactSizeIterator for IntoIter<A> {}
impl<A> FusedIterator for IntoIter<A> {}
impl<T> Presence<Presence<T>> {
    #[inline]
    pub fn flatten(self) -> Presence<T> {
        match self {
            Present(inner) => inner,
            Absent => Absent,
        }
    }
}
