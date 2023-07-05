use std::alloc::{alloc, dealloc, Layout, GlobalAlloc};
use std::collections::HashSet;
use std::cell::RefCell;
use lazy_static::lazy_static;

pub struct MemoryManager {
    freed_objects: RefCell<HashSet<*mut u8>>,
}

unsafe impl Send for MemoryManager {}
unsafe impl Sync for MemoryManager {}

impl MemoryManager {
    pub fn new() -> MemoryManager {
        MemoryManager {
            freed_objects: RefCell::new(HashSet::new()),
        }
    }

    pub fn allocate(&self, layout: Layout) -> *mut u8 {
        eprintln!("Allocating with layout: {:?}", layout);
        unsafe { alloc(layout) }
    }

    pub fn deallocate(&self, ptr: *mut u8, layout: Layout) {
        let freed_objects = self.freed_objects.borrow();
        eprintln!("Deallocating!");
        if freed_objects.contains(&ptr) {
            return; // Ignore deallocation of already freed objects
        }

        unsafe {
            dealloc(ptr, layout);
        }
        self.freed_objects.borrow_mut().insert(ptr);
    }
}

lazy_static! {
    static ref GLOBAL_ALLOCATOR: MemoryManager = MemoryManager::new();
}

unsafe impl GlobalAlloc for GLOBAL_ALLOCATOR {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.allocate(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.deallocate(ptr, layout)
    }
}

struct GlobalAllocatorRef(pub &'static GLOBAL_ALLOCATOR);

unsafe impl GlobalAlloc for GlobalAllocatorRef {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.0.alloc(layout)
    }       

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if ptr.is_null() {
            return
        } else {
            self.0.dealloc(ptr, layout)
        }
    }
}

#[global_allocator]
static GLOBAL_MEMORY_MANAGER: GlobalAllocatorRef = GlobalAllocatorRef(&GLOBAL_ALLOCATOR);
