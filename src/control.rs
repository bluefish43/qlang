use std::io::{Write, self};

pub struct IOControllerW<'a, W: Write> {
    buffer: Vec<u8>,
    is_locked: bool,
    writer: &'a mut W,
}

impl<'a, W: Write> IOControllerW<'a, W> {
    pub fn new(writer: &'a mut W) -> IOControllerW<'a, W> {
        Self {
            buffer: Vec::new(),
            is_locked: false,
            writer,
        }
    }

    pub fn lock(&mut self) {
        self.is_locked = true;
    }

    pub fn write_pending(&mut self) -> io::Result<()> {
        self.writer.write_all(&self.buffer)
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    pub fn unlock(&mut self) {
        self.is_locked = false;
    }
}

impl<'a, W: std::io::Write> Write for IOControllerW<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_pending()?;
        if !self.is_locked {
            self.writer.write(buf)
        } else {
            self.buffer.append(&mut buf.to_vec());
            Ok(0)
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

pub struct IOControllerI {
    buffer: Vec<u8>,
}

impl IOControllerI {
    pub fn new() -> IOControllerI {
        IOControllerI {
            buffer: Vec::new(),
        }
    }

    pub fn get_buffer(&self) -> &[u8] {
        &self.buffer
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }
}