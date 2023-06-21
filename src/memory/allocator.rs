use crate::vm::Value as VMValue; // Importing crate::vm::Value as VMValue
use fxhash::FxHashMap;
use std::collections::HashSet;
use std::ptr::NonNull;

#[derive(Clone, Copy, PartialEq)]
enum Color {
    White,
    Gray,
    Black,
}

pub struct GarbageCollector {
    blocks: Vec<GcBlock>,
    marked: HashSet<NonNull<VMValue>>, // Using VMValue instead of crate::vm::Value
    scopes: Vec<HashSet<NonNull<VMValue>>>, // Using VMValue instead of crate::vm::Value
    roots: HashSet<NonNull<VMValue>>,
}

impl GarbageCollector {
    pub fn new() -> GarbageCollector {
        GarbageCollector {
            blocks: Vec::new(),
            marked: HashSet::new(),
            scopes: Vec::new(),
            roots: HashSet::new(),
        }
    }

    pub fn allocate(&mut self, value: VMValue) -> NonNull<VMValue> {
        // Using VMValue instead of crate::vm::Value
        let object = Box::new(value);
        let object_ptr = Box::into_raw(object) as *mut VMValue; // Using VMValue instead of crate::vm::Value
        let object_nonnull = NonNull::new(object_ptr).unwrap();

        // Find or create an available block
        let block = self.find_or_create_block();
        unsafe { &mut *block }.objects.push(object_nonnull);

        object_nonnull
    }

    pub fn allocate_in_scope(&mut self, value: VMValue) -> NonNull<VMValue> {
        // Using VMValue instead of crate::vm::Value
        let object_ptr = self.allocate(value);
        if self.scopes.len() == 1 {
            self.roots.insert(object_ptr);
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(object_ptr);
        }
        object_ptr
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    pub fn leave_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            for &object_ptr in &scope {
                self.marked.insert(object_ptr);
            }
        }
    }

    pub fn collect(&mut self, variables: &mut FxHashMap<String, *const VMValue>) {
        let mut colors = FxHashMap::default();
        self.mark(&mut colors);

        let mut to_remove = Vec::new();

        for (var_name, var_ptr) in variables.iter_mut() {
            let var_ptr_nonnull = NonNull::new(*var_ptr as *mut VMValue);
            if let Some(ptr) = var_ptr_nonnull {
                if !self.marked.contains(&ptr) {
                    // Object is not marked, remove it from the map
                    *var_ptr = std::ptr::null();
                    to_remove.push(var_name.clone());
                }
            }
        }

        for name in to_remove {
            variables.remove(&name);
        }

        self.sweep(&mut colors);
    }

    fn mark(&mut self, colors: &mut FxHashMap<NonNull<VMValue>, Color>) {
        let mut stack = Vec::new();

        for block in &self.blocks {
            for &object_ptr in &block.objects {
                colors.insert(object_ptr, Color::White);
            }
        }

        for &object_ptr in &self.roots {
            colors.insert(object_ptr, Color::Gray);
            stack.push(object_ptr);
        }

        while let Some(object_ptr) = stack.pop() {
            self.mark_object(object_ptr, &mut stack, colors);
            colors.insert(object_ptr, Color::Black);
        }
    }

    fn mark_object(
        &mut self,
        object_ptr: NonNull<VMValue>,
        stack: &mut Vec<NonNull<VMValue>>,
        colors: &mut FxHashMap<NonNull<VMValue>, Color>,
    ) {
        let object = unsafe { &mut *object_ptr.as_ptr() };

        match object {
            VMValue::PtrWrapper(reference) => {
                if let Some(reference_ptr) = unsafe {
                    reference
                        .as_ref()
                        .map(|r| r as *const VMValue as *mut VMValue)
                } {
                    let reference_ptr = unsafe { NonNull::new_unchecked(reference_ptr) };
                    if let Some(color) = colors.get_mut(&reference_ptr) {
                        if *color == Color::White {
                            *color = Color::Gray;
                            stack.push(reference_ptr);
                        }
                    }
                }
            }
            VMValue::List(ref mut list) => {
                for element_ptr in list.iter_mut() {
                    if let Some(element_ptr) = NonNull::new(&mut *element_ptr as *mut VMValue) {
                        if let Some(color) = colors.get_mut(&element_ptr) {
                            if *color == Color::White {
                                *color = Color::Gray;
                                stack.push(element_ptr);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    #[allow(unused)]
    fn is_root(&self, object_ptr: NonNull<VMValue>) -> bool {
        self.roots.contains(&object_ptr)
    }

    fn sweep(&mut self, colors: &FxHashMap<NonNull<VMValue>, Color>) {
        self.blocks.retain(|block| {
            let alive_objects: Vec<_> = block
                .objects
                .iter()
                .filter(|&&object_ptr| colors.get(&object_ptr) == Some(&Color::Black))
                .cloned()
                .collect();
            !alive_objects.is_empty()
        });
    }

    fn find_or_create_block(&mut self) -> *mut GcBlock {
        if let Some(block) = self.find_not_full_block() {
            block
        } else {
            let block = GcBlock::new();
            self.create_block(block)
        }
    }

    #[allow(unused)]
    fn get_block(&mut self) -> &mut GcBlock {
        unsafe { &mut *self.find_or_create_block() }
    }

    fn create_block(&mut self, block: GcBlock) -> &mut GcBlock {
        self.push_last_mut(block)
    }

    fn find_not_full_block(&mut self) -> Option<&mut GcBlock> {
        self.blocks.iter_mut().find(|block| !block.is_full())
    }

    fn push_last_mut(&mut self, block: GcBlock) -> &mut GcBlock {
        self.blocks.push(block);
        self.blocks.last_mut().unwrap()
    }

    fn dealloc(&mut self) {
        for block in &mut self.blocks {
            for &object_ptr in &block.objects {
                unsafe {
                    let _ = Box::from_raw(object_ptr.as_ptr());
                }
            }
        }
    }

    pub fn absorb(&mut self, other: &mut GarbageCollector) {
        // Move the blocks from the other GarbageCollector
        self.blocks.append(&mut other.blocks);

        // Move the marked objects from the other GarbageCollector
        self.marked.extend(other.marked.drain());

        // Move the scopes from the other GarbageCollector
        self.scopes.append(&mut other.scopes);

        // Move the roots from the other GarbageCollector
        self.roots.extend(other.roots.drain());
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        self.dealloc();
    }
}

pub struct GcBlock {
    objects: Vec<NonNull<VMValue>>, // Using VMValue instead of crate::vm::Value
}

impl GcBlock {
    fn new() -> GcBlock {
        GcBlock {
            objects: Vec::new(),
        }
    }

    fn is_full(&self) -> bool {
        self.objects.len() >= 256
    }
}
