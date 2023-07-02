#[derive(PartialEq, Eq, Debug)]
struct Node<'a, T> {
    value: T,
    next_node: *mut Node<'a, T>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct LinkList<'a, T> {
    items: Vec<*mut Node<'a, T>>,
    back: Option<*mut Node<'a, T>>,
    front: Option<*mut Node<'a, T>>
}

impl<'a, T> LinkList<'a, T> {
    pub fn new() -> LinkList<'a, T> {
        Self {
            items: vec![],
            back: None,
            front: None,
        }
    }

    pub fn push_back(&mut self, value: T) {
        if let Some(back) = self.back {
            let node = Box::into_raw(Box::new(Node {
                value,
                next_node: back,
            }));
            self.items.push(node);
        }
    }

    pub fn push_front(&mut self, value: T) {
        let node = Box::into_raw(Box::new(Node {
            value,
            next_node: std::ptr::null_mut(),
        }));
        self.items.push(node);
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if let Some(front) = self.front {
            let copied = front.clone();
            self.front = None;
            Some(unsafe { std::ptr::read(copied as *const Node<'a, T>).value })
        } else {
            None
        }
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if let Some(back) = self.back {
            let copied = back.clone();
            self.back = None;
            Some(unsafe { std::ptr::read(copied as *const Node<'a, T>).value })
        } else {
            None
        }
    }
}

impl<'a, T> Drop for LinkList<'a, T> {
    fn drop(&mut self) {
        unsafe {
            for item in self.items.iter_mut() {
                let _ = Box::from_raw(item);
            }
        }
    }
}