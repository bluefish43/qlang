use std::ffi::CString;
use std::sync::Arc;

#[cfg(unix)]
use libloading::os::unix::*;
#[cfg(windows)]
use libloading::os::windows::*;

use crate::Value;