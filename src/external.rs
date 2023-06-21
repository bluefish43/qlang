use std::ffi::CString;
use std::sync::Arc;

#[cfg(unix)]
use libloading::os::unix::*;
#[cfg(windows)]
use libloading::os::windows::*;

use crate::vm::RawValue;
use crate::Value;

pub fn get_functions(
    lib_path: &str,
    fn_names: &[&str],
) -> Result<
    Vec<(String, Arc<unsafe extern "C" fn(*const RawValue) -> Value>)>,
    Box<dyn std::error::Error>,
> {
    let lib: Library = unsafe { Library::new(lib_path)? };
    let mut fns = Vec::new();
    for name in fn_names {
        let c_name = CString::new(*name)?;
        unsafe {
            let func: Symbol<unsafe extern "C" fn(*const RawValue) -> Value> =
                lib.get(c_name.as_bytes_with_nul())?;
            fns.push((name.to_string(), Arc::new(*func)));
        }
    }
    Ok(fns)
}
