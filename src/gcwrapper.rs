use std::cell::RefCell;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use fxhash::FxHashMap;

pub struct GCWrapper<T: Clone> {
    pub hash: FxHashMap<String, T>,
    status: Rc<RefCell<FxHashMap<String, Lifetime>>>,
}

impl<T: Clone> Deref for GCWrapper<T> {
    type Target = FxHashMap<String, T>;

    fn deref(&self) -> &Self::Target {
        &self.hash
    }
}

impl<T: Clone> DerefMut for GCWrapper<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.hash
    }
}

#[derive(Clone, Debug)]
pub enum Lifetime {
    Scopes(isize),
    Static,
    Dynamic(String),
}

impl<T: Clone> GCWrapper<T> {
    pub fn new() -> Self {
        Self {
            hash: FxHashMap::default(),
            status: Rc::new(RefCell::new(FxHashMap::default())),
        }
    }

    pub fn insert_with_status(&mut self, name: String, val: T) {
        self.hash.insert(name.clone(), val);
        self.status.borrow_mut().insert(name, Lifetime::Scopes(1));
    }

    pub fn insert_static(&mut self, name: String, val: T) {
        self.hash.insert(name.clone(), val);
        self.status.borrow_mut().insert(name, Lifetime::Static);
    }

    pub fn insert_dynamic(&mut self, name: String, val: T, tied_to: String) -> Result<(), String> {
        self.hash.insert(name.clone(), val);
        if let Some(_) = self.status.borrow().get(&tied_to) {
            self.status
                .borrow_mut()
                .insert(name, Lifetime::Dynamic(tied_to));
            Ok(())
        } else {
            Err(format!(
                "Cannot tie lifetime of {} to {} because {} is not defined",
                name, tied_to, tied_to
            ))
        }
    }

    pub fn remove_with_status(&mut self, name: &String) -> Option<T> {
        if !self.hash.contains_key(name) {
            return None;
        }
        self.status.borrow_mut().remove(name);
        self.hash.remove(name)
    }

    pub fn map(&mut self) {
        let keys: Vec<_> = self.hash.keys().cloned().collect();
        for key in keys {
            if let Some(status) = self.status.borrow_mut().get_mut(&key) {
                match status {
                    Lifetime::Scopes(s) => {
                        *s += 1;
                    }
                    _ => continue,
                }
            }
        }
    }

    pub fn take(&mut self) {
        let keys: Vec<_> = self.status.borrow_mut().keys().cloned().collect();
        for key in keys {
            if let Some(status) = self.status.borrow_mut().get_mut(&key) {
                match status {
                    Lifetime::Scopes(s) => {
                        if *s - 1 < 1 {
                            self.hash.remove(&key);
                            while let Err(e) = self.status.try_borrow_mut() {
                                eprintln!("FATAL ERROR: INTERNAL ERROR: Could not borrow the garbage collector mutably:\n\t{}", e);
                                continue;
                            }
                            let mut borrow = self.status.borrow_mut();
                            borrow.remove(&key);
                        } else {
                            *s -= 1;
                        }
                    }
                    Lifetime::Dynamic(name) => {
                        if let Some(other_lifetime) = self.status.borrow().get(name) {
                            *status = other_lifetime.clone();
                        } else {
                            self.hash.remove(&key);
                            self.status.borrow_mut().remove(&key);
                        }
                    }
                    _ => continue,
                }
            }
        }
    }

    pub fn forget(&mut self, name: String) {
        self.status.borrow_mut().remove(&name);
    }

    pub fn find_lifetime_holder(&self, lifetime: &Lifetime) -> Result<String, String> {
        let mut seen = HashSet::new();
        let mut current_lifetime = lifetime;
        let binding = self.status.borrow();

        loop {
            match current_lifetime {
                Lifetime::Dynamic(name) => {
                    if seen.contains(name) {
                        return Err(format!(
                            "Cyclic reference detected involving lifetime '{}'",
                            name
                        ));
                    }
                    seen.insert(name.clone());
                    if let Some(holder_lifetime) = binding.get(name) {
                        current_lifetime = holder_lifetime;
                    } else {
                        return Ok(name.clone());
                    }
                }
                _ => {
                    return Err(format!(
                        "Lifetime holder not found for lifetime '{:?}'",
                        lifetime
                    ));
                }
            }
        }
    }
}