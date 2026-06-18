use std::cell::RefCell;
use std::ffi::{c_char, c_float, c_int, c_uint, CString};
use std::panic::{catch_unwind, AssertUnwindSafe};

use df::tract::{DfParams, DfTract, RuntimeParams};
use ndarray::Array2;

#[repr(C)]
pub struct dfn_handle {
    model: DfTract,
}

thread_local! {
    static LAST_ERROR: RefCell<CString> = RefCell::new(CString::new("").unwrap());
}

fn set_last_error(message: impl Into<String>) {
    let sanitized = message.into().replace('\0', " ");
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = CString::new(sanitized)
            .unwrap_or_else(|_| CString::new("DeepFilterNet error").unwrap());
    });
}

fn clear_last_error() {
    set_last_error("");
}

fn ffi_result<F>(f: F) -> c_int
where
    F: FnOnce() -> Result<(), String>,
{
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(Ok(())) => {
            clear_last_error();
            0
        }
        Ok(Err(err)) => {
            set_last_error(err);
            -1
        }
        Err(_) => {
            set_last_error("DeepFilterNet panicked");
            -1
        }
    }
}

#[no_mangle]
pub extern "C" fn dfn_create_default(
    channels: c_uint,
    attenuation_limit_db: c_float,
) -> *mut dfn_handle {
    match catch_unwind(AssertUnwindSafe(|| {
        let channels = channels.max(1) as usize;
        let df_params = DfParams::default();
        let runtime_params =
            RuntimeParams::default_with_ch(channels).with_atten_lim(attenuation_limit_db);
        DfTract::new(df_params, &runtime_params)
    })) {
        Ok(Ok(model)) => {
            clear_last_error();
            Box::into_raw(Box::new(dfn_handle { model }))
        }
        Ok(Err(err)) => {
            set_last_error(format!("{err:?}"));
            std::ptr::null_mut()
        }
        Err(_) => {
            set_last_error("DeepFilterNet panicked during initialization");
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn dfn_destroy(handle: *mut dfn_handle) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

#[no_mangle]
pub unsafe extern "C" fn dfn_sample_rate(handle: *const dfn_handle) -> c_uint {
    handle
        .as_ref()
        .map(|handle| handle.model.sr as c_uint)
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn dfn_frame_length(handle: *const dfn_handle) -> usize {
    handle
        .as_ref()
        .map(|handle| handle.model.hop_size)
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn dfn_delay_samples(handle: *const dfn_handle) -> usize {
    handle
        .as_ref()
        .map(|handle| {
            handle.model.fft_size - handle.model.hop_size
                + handle.model.lookahead * handle.model.hop_size
        })
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn dfn_set_attenuation_limit(
    handle: *mut dfn_handle,
    attenuation_limit_db: c_float,
) -> c_int {
    ffi_result(|| {
        let handle = handle
            .as_mut()
            .ok_or_else(|| "Invalid DeepFilterNet handle".to_string())?;
        handle.model.set_atten_lim(attenuation_limit_db);
        Ok(())
    })
}

#[no_mangle]
pub unsafe extern "C" fn dfn_set_post_filter_beta(handle: *mut dfn_handle, beta: c_float) -> c_int {
    ffi_result(|| {
        let handle = handle
            .as_mut()
            .ok_or_else(|| "Invalid DeepFilterNet handle".to_string())?;
        handle.model.set_pf_beta(beta);
        Ok(())
    })
}

#[no_mangle]
pub unsafe extern "C" fn dfn_reset(handle: *mut dfn_handle) -> c_int {
    ffi_result(|| {
        let handle = handle
            .as_mut()
            .ok_or_else(|| "Invalid DeepFilterNet handle".to_string())?;
        handle.model.init().map_err(|err| format!("{err:?}"))
    })
}

#[no_mangle]
pub unsafe extern "C" fn dfn_process_frame(
    handle: *mut dfn_handle,
    input: *const c_float,
    output: *mut c_float,
) -> c_int {
    ffi_result(|| {
        let handle = handle
            .as_mut()
            .ok_or_else(|| "Invalid DeepFilterNet handle".to_string())?;
        if input.is_null() || output.is_null() {
            return Err("Invalid DeepFilterNet audio buffer".to_string());
        }

        let channels = handle.model.ch;
        let hop_size = handle.model.hop_size;
        let input = Array2::from_shape_vec(
            (channels, hop_size),
            std::slice::from_raw_parts(input, channels * hop_size).to_vec(),
        )
        .map_err(|err| err.to_string())?;
        let mut enhanced = Array2::<f32>::zeros((channels, hop_size));

        handle
            .model
            .process(input.view(), enhanced.view_mut())
            .map_err(|err| format!("{err:?}"))?;

        std::ptr::copy_nonoverlapping(enhanced.as_ptr(), output, channels * hop_size);
        Ok(())
    })
}

#[no_mangle]
pub extern "C" fn dfn_last_error() -> *const c_char {
    LAST_ERROR.with(|cell| cell.borrow().as_ptr())
}
