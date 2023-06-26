use std::{ops::{Deref, DerefMut}, fmt::Debug};

pub trait Strip {
    fn lstrip(&self) -> Self;
    fn rstrip(&self) -> Self;
    fn stripc(&self, c: char) -> Self;
}

impl Strip for String {
    fn lstrip(&self) -> Self {
        self.trim_start_matches(char::is_whitespace).to_string()
    }

    fn rstrip(&self) -> Self {
        self.trim_end_matches(char::is_whitespace).to_string()
    }

    fn stripc(&self, c: char) -> Self {
        self.trim_matches(c).to_string()
    }
}

pub trait Reversible {
    fn reverse_in_place(&mut self);

    fn reverse(&self) -> Self;
}

impl Reversible for String {
    fn reverse_in_place(&mut self) {
        *self = self.chars().rev().collect::<String>();
    }

    fn reverse(&self) -> Self {
        self.chars().rev().collect::<String>()
    }
}

pub struct SharedPointer<T> {
    inner_pointer: *mut T,
}

impl<T> SharedPointer<T> {
    pub fn new(value: &T) -> SharedPointer<T> {
        Self {
            inner_pointer: value as *const T as *mut T,
        }
    }

    pub fn assign(&self, value: T) {
        *unsafe { self.inner_pointer.as_mut().unwrap() } = value;
    }

    pub fn as_ref(&self) -> &T {
        unsafe { self.inner_pointer.as_ref().unwrap() }
    }

    pub fn try_ref(&self) -> Option<&T> {
        unsafe { self.inner_pointer.as_ref() }
    }

    pub fn as_mut(&mut self) -> &mut T {
        unsafe { self.inner_pointer.as_mut().unwrap() }
    }

    pub fn try_mut(&mut self) -> Option<&mut T> {
        unsafe { self.inner_pointer.as_mut() }
    }
}

impl<T> Deref for SharedPointer<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.inner_pointer.as_ref().unwrap() }
    }
}

impl<T> DerefMut for SharedPointer<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.inner_pointer.as_mut().unwrap() }
    }
}

impl<T: Debug> Debug for SharedPointer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SharedPointer({:?})", self.as_ref())
    }
}