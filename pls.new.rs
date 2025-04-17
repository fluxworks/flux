//! Cicada is a bash-like Unix shell written in Rust.
#![feature
(
    tool_lints,
)]

#![allow
(
    dead_code,
    unknown_lints,
    unreachable_patterns,
    unused_variables,
)]
/*
#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
*/
extern crate libc;
extern crate time as timed;
extern crate regex as re;
/*
extern crate clap;
extern crate getrandom;
extern crate libc;
extern crate nix;

extern crate unicode_normalization;
extern crate unicode_width;
*/
/// Macros
#[macro_use] pub mod macros
{

}
/// Memory allocation APIs.
pub mod alloc
{
    pub use std::alloc::{ * };
}
/// Utilities for dynamic typing or type reflection.
pub mod any
{
    pub use std::any::{ * };
}
/// SIMD and vendor intrinsics module.
pub mod arch
{
    pub use std::arch::{ * };
}
/// Utilities for the array primitive type.
pub mod array
{
    pub use std::array::{ * };
}
/// Operations on ASCII strings and characters.
pub mod ascii
{
    pub use std::ascii::{ * };
}
/// Support for capturing a stack backtrace of an OS thread
pub mod backtrace
{
    pub use std::backtrace::{ * };
}
/// A module for working with borrowed data.
pub mod borrow
{
    pub use std::borrow::{ * };
}
/// The Box<T> type for heap allocation.
pub mod boxed
{
    pub use std::boxed::{ * };
}
/// Shareable mutable containers.
pub mod cell
{
    pub use std::cell::{ * };
}
/// Utilities for the char primitive type.
pub mod char
{
    pub use std::char::{ * };
}
/// The Clone trait for types that cannot be ‘implicitly copied’.
pub mod clone
{
    pub use std::clone::{ * };
}
/// Utilities for comparing and ordering values.
pub mod cmp
{
    pub use std::cmp::{ * };
}
/// Collection types.
pub mod collections
{
    pub use std::collections::{ * };
}
/// Traits for conversions between types.
pub mod convert
{
    pub use std::convert::{ * };
}
/// The Default trait for types with a default value.
pub mod default
{
    pub use std::default::{ * };
}
/// Emission & Printing Utilities
pub mod emit
{

}
/// Inspection and manipulation of the process’s environment.
pub mod env
{
    pub use std::env::{ * };

    use ::
    {
        path::{ Path },
    };
    // pub fn init_path_env( ... )
    pub fn initialize_pathing()
    {
        let mut paths: Vec<String> = vec![];
        for x in
        [
            "/usr/local/sbin",
            "/usr/local/bin",
            "/usr/sbin",
            "/usr/bin",
            "/sbin",
            "/bin",
        ]
        {
            if Path::new(x).exists() { paths.push(x.to_string()); }
        }

        if let Ok(env_path) = var("PATH")
        {
            for x in env_path.split(":")
            {
                if !paths.contains(&x.to_string()) { paths.push(x.to_string()); }
            }
        }

        let paths = paths.join(":");
        set_var("PATH", paths);
    }

}
/// Expansions
pub mod expand
{

}
/// Interfaces for working with Errors.
pub mod error
{
    pub use std::error::{ * };

    use ::
    {
        fmt, io
    };
    /// Cross-platform interface to the `errno` variable.
    pub mod imply
    {
        pub mod unix
        {
            //! Implementation of `errno` functionality for Unix systems.
            use ::
            {
                error::{ Errno },
                libc::{self, c_int, size_t, strerror_r, strlen},                
                result::{ Result },
                *,
            };

            pub const STRERROR_NAME: &str = "strerror_r";

            extern "C"
            {
                #[cfg_attr(
                    any(
                        target_os = "macos",
                        target_os = "ios",
                        target_os = "tvos",
                        target_os = "watchos",
                        target_os = "visionos",
                        target_os = "freebsd"
                    ),
                    link_name = "__error"
                )]
                #[cfg_attr(
                    any(
                        target_os = "openbsd",
                        target_os = "netbsd",
                        target_os = "android",
                        target_os = "espidf",
                        target_os = "vxworks",
                        target_os = "cygwin",
                        target_env = "newlib"
                    ),
                    link_name = "__errno"
                )]
                #[cfg_attr(
                    any(target_os = "solaris", target_os = "illumos"),
                    link_name = "___errno"
                )]
                #[cfg_attr(target_os = "haiku", link_name = "_errnop")]
                #[cfg_attr(
                    any(
                        target_os = "linux",
                        target_os = "hurd",
                        target_os = "redox",
                        target_os = "dragonfly",
                        target_os = "emscripten",
                    ),
                    link_name = "__errno_location"
                )]
                #[cfg_attr(target_os = "aix", link_name = "_Errno")]
                #[cfg_attr(target_os = "nto", link_name = "__get_errno_ptr")]
                fn errno_location() -> *mut c_int;
            }

            fn from_utf8_lossy(input: &[u8]) -> &str
            {
                match str::from_utf8(input) {
                    Ok(valid) => valid,
                    Err(error) => unsafe { str::from_utf8_unchecked(&input[..error.valid_up_to()]) },
                }
            }

            pub fn with_description<F, T>(err: Errno, callback: F) -> T where
            F: FnOnce(Result<&str, Errno>) -> T,
            {
                let mut buf = [0u8; 1024];
                let c_str = unsafe {
                    let rc = strerror_r(err.0, buf.as_mut_ptr() as *mut _, buf.len() as size_t);
                    if rc != 0
                    {
                        let fm_err = match rc < 0 {
                            true => errno(),
                            false => Errno(rc),
                        };
                        if fm_err != Errno(libc::ERANGE) {
                            return callback(Err(fm_err));
                        }
                    }
                    let c_str_len = strlen(buf.as_ptr() as *const _);
                    &buf[..c_str_len]
                };
                callback(Ok(from_utf8_lossy(c_str)))
            }
            
            pub fn errno() -> Errno { unsafe { Errno(*errno_location()) } }

            pub fn set_errno(Errno(errno): Errno) { unsafe { *errno_location() = errno; } }
        }

        pub mod windows
        {
            //! Implementation of `errno` functionality for Windows.
            use ::
            {
                char::{ self, REPLACEMENT_CHARACTER },
                ptr
            };

            use ::error::Errno;

            pub const STRERROR_NAME: &str = "FormatMessageW";
            pub const FORMAT_MESSAGE_FROM_SYSTEM: FORMAT_MESSAGE_OPTIONS = FORMAT_MESSAGE_OPTIONS(4096u32);
            pub const FORMAT_MESSAGE_IGNORE_INSERTS: FORMAT_MESSAGE_OPTIONS = FORMAT_MESSAGE_OPTIONS(512u32);
            
            pub type WIN32_ERROR = u32;
            
            pub type PWSTR = *mut u16;

            #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, PartialEq )]
            pub struct FORMAT_MESSAGE_OPTIONS(pub u32);
            
            #[inline] pub unsafe fn GetLastError() -> WIN32_ERROR { 0 }
            #[inline] pub unsafe fn SetLastError( dwerrcode:WIN32_ERROR ) { }
            #[inline] pub unsafe fn FormatMessageW
            (
                dwflags: FORMAT_MESSAGE_OPTIONS, 
                lpsource: Option<*const ::ffi::c_void>, 
                dwmessageid: u32, 
                dwlanguageid: u32, 
                lpbuffer:PWSTR, 
                nsize: u32, 
                arguments: Option<*const *const i8>
            ) -> u32
            {
                0
            }

            fn from_utf16_lossy<'a>(input: &[u16], output: &'a mut [u8]) -> &'a str
            {
                let mut output_len = 0;
                for c in char::decode_utf16(input.iter().copied().take_while(|&x| x != 0))
                    .map(|x| x.unwrap_or(REPLACEMENT_CHARACTER))
                {
                    let c_len = c.len_utf8();
                    if c_len > output.len() - output_len {
                        break;
                    }
                    c.encode_utf8(&mut output[output_len..]);
                    output_len += c_len;
                }
                unsafe { str::from_utf8_unchecked(&output[..output_len]) }
            }

            pub fn with_description<F, T>(err: Errno, callback: F) -> T where
            F: FnOnce(Result<&str, Errno>) -> T
            {
                unsafe
                {
                    let lang_id = 0x0800_u32;
                    let mut buf = [0u16; 2048];
                    
                    let res = FormatMessageW
                    (
                        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                        ptr::null_mut(),
                        err.0 as u32,
                        lang_id,
                        buf.as_mut_ptr(),
                        buf.len() as u32,
                        ptr::null_mut(),
                    );
                    
                    if res == 0
                    {
                        let fm_err = errno();
                        return callback(Err(fm_err));
                    }

                    let mut msg = [0u8; 2048];
                    let msg = from_utf16_lossy(&buf[..res as usize], &mut msg[..]);
                    callback(Ok(msg.trim_end()))
                }
            }
            
            pub fn errno() -> Errno { unsafe { Errno(GetLastError() as i32) } }

            pub fn set_errno(Errno(errno): Errno) { unsafe { SetLastError(errno as WIN32_ERROR) } }
        }
    }

    #[cfg(unix)] pub use self::imply::unix as sys;
    #[cfg(windows)] pub use self::imply::windows as sys;
    /// Wraps a platform-specific error code.
    #[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
    pub struct Errno(pub i32);

    impl fmt::Debug for Errno
    {
        fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
        {
            sys::with_description(*self, |desc|
            {
                fmt.debug_struct("Errno")
                .field("code", &self.0)
                .field("description", &desc.ok())
                .finish()
            })
        }
    }

    impl fmt::Display for Errno
    {
        fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
        {
            sys::with_description(*self, |desc| match desc
            {
                Ok(desc) => fmt.write_str(desc),
                Err(fm_err) => write!(
                    fmt,
                    "OS error {} ({} returned error {})",
                    self.0,
                    sys::STRERROR_NAME,
                    fm_err.0
                ),
            })
        }
    }

    impl From<Errno> for i32
    {
        fn from(e: Errno) -> Self { e.0 }
    }
    
    impl Error for Errno
    {
        #[allow(deprecated)] fn description(&self) -> &str { "system error" }
    }
    
    impl From<Errno> for io::Error
    {
        fn from(errno: Errno) -> Self { io::Error::from_raw_os_error(errno.0) }
    }
    /// Returns the platform-specific value of `errno`.
    pub fn errno() -> Errno { sys::errno() }
    /// Sets the platform-specific value of `errno`.
    pub fn set_errno(err: Errno) { sys::set_errno(err) }
}
/// Constants for the f32 single-precision floating point type.
pub mod f32
{
    pub use std::f32::{ * };
}
/// Constants for the f64 double-precision floating point type.
pub mod f64
{
    pub use std::f64::{ * };
}
/// Utilities related to Foreign Function Interface bindings.
pub mod ffi
{
    pub use std::ffi::{ * };
}
/// Utilities for formatting and printing Strings.
pub mod fmt
{
    pub use std::fmt::{ * };
}
/// Filesystem manipulation operations.
pub mod fs
{
    pub use std::fs::{ * };
}
/// Asynchronous basic functionality.
pub mod future
{
    pub use std::future::{ * };
}
/// State Reading
pub mod get
{

}
/// Generic hashing support.
pub mod hash
{
    pub use std::hash::{ * };
}
/// Hints to compiler that affects how code should be emitted or optimized.
pub mod hint
{
    pub use std::hint::{ * };
}
/// State Verification
pub mod is
{
    use ::
    {
        regex::{ re_contains },
        *,
    };
    
    pub fn is_shell_altering_command(line: &str) -> bool
    {
        let line = line.trim();
        
        if re_contains(line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$") { return true; }
        
        line.starts_with("alias ")
        || line.starts_with("export ")
        || line.starts_with("unalias ")
        || line.starts_with("unset ")
        || line.starts_with("source ")
    }
}
/// Traits, helpers, and type definitions for core I/O functionality.
pub mod io
{
    pub use std::io::{ * };
}
/// Composable external iteration.
pub mod iter
{
    pub use std::iter::{ * };
}
/// Primitive traits and types representing basic properties of types.
pub mod marker
{
    pub use std::marker::{ * };
}
/// Basic functions for dealing with memory.
pub mod mem
{
    pub use std::mem::{ * };
}
/// Networking primitives for TCP/UDP communication.
pub mod net
{
    pub use std::net::{ * };
}
/// Additional functionality for numerics.
pub mod num
{
    pub use std::num::{ * };
}
/// Overloadable operators.
pub mod ops
{
    pub use std::ops::{ * };
}
/// Optional values.
pub mod option
{
    pub use std::option::{ * };
}
/// OS-specific functionality.
pub mod os
{
    pub use std::option::{ * };
}
/// Panic support in the standard library.
pub mod panic
{
    pub use std::panic::{ * };
}
/// Cross-platform path manipulation.
pub mod path
{
    pub use std::path::{ * };
}
/// Types that pin data to a location in memory.
pub mod pin
{
    pub use std::pin::{ * };
}
/// Types that pin data to a location in memory.
pub mod prelude
{
    pub use std::prelude::v1::{ * };
    
    pub use std::
    {
      assert, assert_eq, assert_ne, cfg, column, compile_error, concat, dbg, debug_assert, debug_assert_eq, 
      debug_assert_ne, env, eprint, eprintln, file, format, format_args, include, include_bytes, include_str, 
      is_x86_feature_detected, line, matches, module_path, option_env, panic, print, println, stringify, thread_local,
      todo, try, unimplemented, unreachable, vec, write, writeln
    };

}
pub use self::prelude::{ * };
/// Reexports the primitive types to allow usage that is not possibly shadowed by other declared types.
pub mod primitive
{
    pub use std::primitive::{ * };
}
/// A module for working with processes.
pub mod process
{
    pub use std::process::{ * };
}
/// A module for working with processes.
pub mod ptr
{
    pub use std::ptr::{ * };
}
/// Single-threaded reference-counting pointers.
pub mod rc
{
    pub use std::rc::{ * };
}
/// Regular expressions
pub mod regex
{
    pub use re::{ * };

    pub fn find_first_group(ptn: &str, text: &str) -> Option<String>
    {
        let re = match regex::Regex::new(ptn)
        {
            Ok(x) => x,
            Err(_) => return None,
        };

        match re.captures(text)
        {
            Some(caps) =>
            {
                if let Some(x) = caps.get(1) { return Some(x.as_str().to_owned()); }
            }

            None => { return None; }
        }

        None
    }

    pub fn re_contains(text: &str, ptn: &str) -> bool
    {
        let re = match regex::Regex::new(ptn)
        {
            Ok(x) => x,
            Err(e) =>
            {
                println!("Regex new error: {:?}", e);
                return false;
            }
        };

        re.is_match(text)
    }

    pub fn replace_all(text: &str, ptn: &str, ptn_to: &str) -> String
    {
        let re = regex::Regex::new(ptn).unwrap();
        let result = re.replace_all(text, ptn_to);
        result.to_string()
    }
}
/// Error handling with the Result type.
pub mod result
{
    pub use std::result::{ * };
}
/// Utilities for the slice primitive type.
pub mod slice
{
    pub use std::slice::{ * };
}
/// Utilities for the str primitive type.
pub mod str
{
    pub use std::str::{ * };
}
/// A UTF-8–encoded, growable string.
pub mod string
{
    pub use std::string::{ * };
}
/// Useful synchronization primitives.
pub mod sync
{
    pub use std::sync::{ * };
}
/// Shell
pub mod shell
{
    use ::error::errno;
    use std::collections::{HashMap, HashSet};
    use std::env;
    use std::io::Write;
    use std::mem;

    use regex::Regex;
    use uuid::Uuid;

    use crate::core;
    use crate::libs;
    use crate::parsers;
    use crate::tools;
    use crate::types::{self, CommandLine};

    #[derive(Debug, Clone)]
    pub struct Shell {
        pub jobs: HashMap<i32, types::Job>,
        pub aliases: HashMap<String, String>,
        pub envs: HashMap<String, String>,
        pub funcs: HashMap<String, String>,
        pub cmd: String,
        pub current_dir: String,
        pub previous_dir: String,
        pub previous_cmd: String,
        pub previous_status: i32,
        pub is_login: bool,
        pub exit_on_error: bool,
        pub has_terminal: bool,
        pub session_id: String,
    }

    impl Shell {
        pub fn new() -> Shell {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = tools::get_current_dir();
            // TODO: the shell proc may have terminal later
            // e.g. $ cicada foo.sh &
            // then with a $ fg
            let has_terminal = proc_has_terminal();
            let (session_id, _) = uuid.split_at(13);
            Shell {
                jobs: HashMap::new(),
                aliases: HashMap::new(),
                envs: HashMap::new(),
                funcs: HashMap::new(),
                cmd: String::new(),
                current_dir: current_dir.clone(),
                previous_dir: String::new(),
                previous_cmd: String::new(),
                previous_status: 0,
                is_login: false,
                exit_on_error: false,
                has_terminal,
                session_id: session_id.to_string(),
            }
        }

        pub fn insert_job(&mut self, gid: i32, pid: i32, cmd: &str, status: &str, bg: bool) {
            let mut i = 1;
            loop {
                let mut indexed_job_missing = false;
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        x.pids.push(pid);
                        x.cmd = format!("{} | {}", x.cmd, cmd);
                        return;
                    }
                } else {
                    indexed_job_missing = true;
                }

                if indexed_job_missing {
                    self.jobs.insert(
                        i,
                        types::Job {
                            cmd: cmd.to_string(),
                            id: i,
                            gid,
                            pids: vec![pid],
                            pids_stopped: HashSet::new(),
                            status: status.to_string(),
                            is_bg: bg,
                        },
                    );
                    return;
                }
                i += 1;
            }
        }

        pub fn get_job_by_id(&self, job_id: i32) -> Option<&types::Job> {
            self.jobs.get(&job_id)
        }

        pub fn mark_job_member_continued(&mut self, pid: i32,
                                        gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.pids_stopped.remove(&pid);
                        idx_found = i;
                        break;
                    }
                }


                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get(&idx_found)
        }

        pub fn mark_job_member_stopped(&mut self, pid: i32,
                                    gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.pids_stopped.insert(pid);
                        idx_found = i;
                        break;
                    }
                }


                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get(&idx_found)
        }

        pub fn get_job_by_gid(&self, gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }

            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get(&i) {
                    if x.gid == gid {
                        return Some(x);
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
            None
        }

        pub fn mark_job_as_running(&mut self, gid: i32, bg: bool) {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.status = "Running".to_string();
                        job.pids_stopped.clear();
                        job.is_bg = bg;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
        }

        pub fn mark_job_as_stopped(&mut self, gid: i32) {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        x.status = "Stopped".to_string();
                        x.is_bg = true;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
        }

        pub fn remove_pid_from_job(&mut self, gid: i32, pid: i32) -> Option<types::Job> {
            if self.jobs.is_empty() {
                return None;
            }

            let mut empty_pids = false;
            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        if let Ok(i_pid) = x.pids.binary_search(&pid) {
                            x.pids.remove(i_pid);
                        }
                        empty_pids = x.pids.is_empty();
                        break;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            if empty_pids {
                return self.jobs.remove(&i);
            }
            None
        }

        /// Update existing *ENV Variable* if such name exists in ENVs,
        /// otherwise, we define a local *Shell Variable*, which would not
        /// be exported into child processes.
        pub fn set_env(&mut self, name: &str, value: &str) {
            if env::var(name).is_ok() {
                env::set_var(name, value);
            } else {
                self.envs.insert(name.to_string(), value.to_string());
            }
        }

        /// get *Shell Variable*, or *ENV Variable*.
        pub fn get_env(&self, name: &str) -> Option<String> {
            match self.envs.get(name) {
                Some(x) => Some(x.to_string()),
                None => {
                    match env::var(name) {
                        Ok(x) => Some(x),
                        Err(_) => None,
                    }
                }
            }
        }

        /// Remove environment variable, function from the environment of
        /// the currently running process
        pub fn remove_env(&mut self, name: &str) -> bool {
            // function names can contain the `-` char.
            let ptn_env = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*$").unwrap();
            if !ptn_env.is_match(name) {
                return false;
            }

            env::remove_var(name);
            self.envs.remove(name);
            self.remove_func(name);
            true
        }

        pub fn remove_path(&mut self, path: &str) {
            if let Ok(paths) = env::var("PATH") {
                let mut paths_new: Vec<&str> = paths.split(":").collect();
                paths_new.retain(|&x| x != path);
                env::set_var("PATH", paths_new.join(":").as_str());
            }
        }

        fn remove_func(&mut self, name: &str) {
            self.funcs.remove(name);
        }

        pub fn set_func(&mut self, name: &str, value: &str) {
            self.funcs.insert(name.to_string(), value.to_string());
        }

        pub fn get_func(&self, name: &str) -> Option<String> {
            self.funcs.get(name).map(|x| x.to_string())
        }

        pub fn get_alias_list(&self) -> Vec<(String, String)> {
            let mut result = Vec::new();
            for (name, value) in &self.aliases {
                result.push((name.clone(), value.clone()));
            }
            result
        }

        pub fn add_alias(&mut self, name: &str, value: &str) {
            self.aliases.insert(name.to_string(), value.to_string());
        }

        pub fn is_alias(&self, name: &str) -> bool {
            self.aliases.contains_key(name)
        }

        pub fn remove_alias(&mut self, name: &str) -> bool {
            let opt = self.aliases.remove(name);
            opt.is_some()
        }

        pub fn get_alias_content(&self, name: &str) -> Option<String> {
            let result = match self.aliases.get(name) {
                Some(x) => x.to_string(),
                None => String::new(),
            };
            if result.is_empty() {
                None
            } else {
                Some(result)
            }
        }
    }

    pub unsafe fn give_terminal_to(gid: i32) -> bool {
        let mut mask: libc::sigset_t = mem::zeroed();
        let mut old_mask: libc::sigset_t = mem::zeroed();

        libc::sigemptyset(&mut mask);
        libc::sigaddset(&mut mask, libc::SIGTSTP);
        libc::sigaddset(&mut mask, libc::SIGTTIN);
        libc::sigaddset(&mut mask, libc::SIGTTOU);
        libc::sigaddset(&mut mask, libc::SIGCHLD);

        let rcode = libc::pthread_sigmask(libc::SIG_BLOCK, &mask, &mut old_mask);
        if rcode != 0 {
            log!("failed to call pthread_sigmask");
        }
        let rcode = libc::tcsetpgrp(1, gid);
        let given;
        if rcode == -1 {
            given = false;
            let e = errno();
            let code = e.0;
            log!("error in give_terminal_to() {}: {}", code, e);
        } else {
            given = true;
        }
        let rcode = libc::pthread_sigmask(libc::SIG_SETMASK, &old_mask, &mut mask);
        if rcode != 0 {
            log!("failed to call pthread_sigmask");
        }
        given
    }

    fn needs_globbing(line: &str) -> bool {
        let re = Regex::new(r"\*+").expect("Invalid regex ptn");
        re.is_match(line)
    }

    pub fn expand_glob(tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !needs_globbing(text) {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let item = text.as_str();

            if !item.contains('*') || item.trim().starts_with('\'') || item.trim().starts_with('"') {
                result.push(item.to_string());
            } else {
                let _basename = libs::path::basename(item);
                let show_hidden = _basename.starts_with(".*");

                match glob::glob(item) {
                    Ok(paths) => {
                        let mut is_empty = true;
                        for entry in paths {
                            match entry {
                                Ok(path) => {
                                    let file_path = path.to_string_lossy();
                                    let _basename = libs::path::basename(&file_path);
                                    if _basename == ".." || _basename == "." {
                                        continue;
                                    }
                                    if _basename.starts_with('.') && !show_hidden {
                                        // skip hidden files, you may need to
                                        // type `ls .*rc` instead of `ls *rc`
                                        continue;
                                    }
                                    result.push(file_path.to_string());
                                    is_empty = false;
                                }
                                Err(e) => {
                                    log!("glob error: {:?}", e);
                                }
                            }
                        }
                        if is_empty {
                            result.push(item.to_string());
                        }
                    }
                    Err(e) => {
                        println!("glob error: {:?}", e);
                        result.push(item.to_string());
                        return;
                    }
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, result) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in result.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_one_env(sh: &Shell, token: &str) -> String {
        // do not combine these two into one: `\{?..\}?`,
        // otherwize `}` in `{print $NF}` would gone.
        let re1 = Regex::new(r"^(.*?)\$([A-Za-z0-9_]+|\$|\?)(.*)$").unwrap();
        let re2 = Regex::new(r"(.*?)\$\{([A-Za-z0-9_]+|\$|\?)\}(.*)$").unwrap();
        if !re1.is_match(token) && !re2.is_match(token) {
            return token.to_string();
        }

        let mut result = String::new();
        let match_re1 = re1.is_match(token);
        let match_re2 = re2.is_match(token);
        if !match_re1 && !match_re2 {
            return token.to_string();
        }

        let cap_results = if match_re1 {
            re1.captures_iter(token)
        } else {
            re2.captures_iter(token)
        };

        for cap in cap_results {
            let head = cap[1].to_string();
            let tail = cap[3].to_string();
            let key = cap[2].to_string();
            if key == "?" {
                result.push_str(format!("{}{}", head, sh.previous_status).as_str());
            } else if key == "$" {
                unsafe {
                    let val = libc::getpid();
                    result.push_str(format!("{}{}", head, val).as_str());
                }
            } else if let Ok(val) = env::var(&key) {
                result.push_str(format!("{}{}", head, val).as_str());
            } else if let Some(val) = sh.get_env(&key) {
                result.push_str(format!("{}{}", head, val).as_str());
            } else {
                result.push_str(&head);
            }
            result.push_str(&tail);
        }

        result
    }

    fn need_expand_brace(line: &str) -> bool {
        libs::re::re_contains(line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"#)
    }

    fn brace_getitem(s: &str, depth: i32) -> (Vec<String>, String) {
        let mut out: Vec<String> = vec![String::new()];
        let mut ss = s.to_string();
        let mut tmp;
        while !ss.is_empty() {
            let c = match ss.chars().next() {
                Some(x) => x,
                None => {
                    return (out, ss);
                }
            };
            if depth > 0 && (c == ',' || c == '}') {
                return (out, ss);
            }
            if c == '{' {
                let mut sss = ss.clone();
                sss.remove(0);
                let result_groups = brace_getgroup(&sss, depth + 1);
                if let Some((out_group, s_group)) = result_groups {
                    let mut tmp_out = Vec::new();
                    for x in out.iter() {
                        for y in out_group.iter() {
                            let item = format!("{}{}", x, y);
                            tmp_out.push(item);
                        }
                    }
                    out = tmp_out;
                    ss = s_group.clone();
                    continue;
                }
            }
            // FIXME: here we mean more than one char.
            if c == '\\' && ss.len() > 1 {
                ss.remove(0);
                let c;
                match ss.chars().next() {
                    Some(x) => c = x,
                    None => {
                        return (out, ss)
                    }
                }

                tmp = format!("\\{}", c);
            } else {
                tmp = c.to_string();
            }
            let mut result = Vec::new();
            for x in out.iter() {
                let item = format!("{}{}", x, tmp);
                result.push(item);
            }
            out = result;
            ss.remove(0);
        }
        (out, ss)
    }

    fn brace_getgroup(s: &str, depth: i32) -> Option<(Vec<String>, String)> {
        let mut out: Vec<String> = Vec::new();
        let mut comma = false;
        let mut ss = s.to_string();
        while !ss.is_empty() {
            let (g, sss) = brace_getitem(ss.as_str(), depth);
            ss = sss.clone();
            if ss.is_empty() {
                break;
            }
            for x in g.iter() {
                out.push(x.clone());
            }

            let c = match ss.chars().next() {
                Some(x) => x,
                None => {
                    break;
                }
            };
            if c == '}' {
                let mut sss = ss.clone();
                sss.remove(0);
                if comma {
                    return Some((out, sss));
                }
                let mut result = Vec::new();
                for x in out.iter() {
                    let item = format!("{{{}}}", x);
                    result.push(item);
                }
                return Some((result, ss));
            }
            if c == ',' {
                comma = true;
                ss.remove(0);
            }
        }

        None
    }

    fn expand_brace(tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, token) in tokens.iter() {
            if !sep.is_empty() || !need_expand_brace(token) {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let items = brace_getitem(token, 0);
            for x in items.0 {
                result.push(x.clone());
            }
            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_brace_range(tokens: &mut types::Tokens) {
        let re;
        if let Ok(x) = Regex::new(r#"\{(-?[0-9]+)\.\.(-?[0-9]+)(\.\.)?([0-9]+)?\}"#) {
            re = x;
        } else {
            println_stderr!("cicada: re new error");
            return;
        }

        let mut idx: usize = 0;
        let mut buff: Vec<(usize, Vec<String>)> = Vec::new();
        for (sep, token) in tokens.iter() {
            if !sep.is_empty() || !re.is_match(token) {
                idx += 1;
                continue;
            }

            // safe to unwrap here, since the `is_match` above already validated
            let caps = re.captures(token).unwrap();

            let start = match caps[1].to_string().parse::<i32>() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };

            // incr is always positive
            let mut incr = if caps.get(4).is_none() {
                1
            } else {
                match caps[4].to_string().parse::<i32>() {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        return;
                    }
                }
            };
            if incr <= 1 {
                incr = 1;
            }

            let mut result: Vec<String> = Vec::new();
            let mut n = start;
            if start > end {
                while n >= end {
                    result.push(format!("{}", n));
                    n -= incr;
                }
            } else {
                while n <= end {
                    result.push(format!("{}", n));
                    n += incr;
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_alias(sh: &Shell, tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        let mut is_head = true;
        for (sep, text) in tokens.iter() {
            if sep.is_empty() && text == "|" {
                is_head = true;
                idx += 1;
                continue;
            }
            if is_head && text == "xargs" {
                idx += 1;
                continue;
            }

            if !is_head || !sh.is_alias(text) {
                idx += 1;
                is_head = false;
                continue;
            }

            if let Some(value) = sh.get_alias_content(text) {
                buff.push((idx, value.clone()));
            }

            idx += 1;
            is_head = false;
        }

        for (i, text) in buff.iter().rev() {
            let linfo = parsers::parser_line::parse_line(text);
            let tokens_ = linfo.tokens;
            tokens.remove(*i);
            for item in tokens_.iter().rev() {
                tokens.insert(*i, item.clone());
            }
        }
    }

    fn expand_home(tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !text.starts_with("~") {
                idx += 1;
                continue;
            }

            let mut s: String = text.clone();
            let ptn = r"^~(?P<tail>.*)";
            let re = Regex::new(ptn).expect("invalid re ptn");
            let home = tools::get_user_home();
            let ss = s.clone();
            let to = format!("{}$tail", home);
            let result = re.replace_all(ss.as_str(), to.as_str());
            s = result.to_string();

            buff.push((idx, s.clone()));
            idx += 1;
        }

        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }

    fn env_in_token(token: &str) -> bool {
        if libs::re::re_contains(token, r"\$\{?[\$\?]\}?") {
            return true;
        }

        let ptn_env_name = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let ptn_env = format!(r"\$\{{?{}\}}?", ptn_env_name);
        if !libs::re::re_contains(token, &ptn_env) {
            return false;
        }

        // do not expand env in a command substitution, e.g.:
        // - echo $(echo '$HOME')
        // - VERSION=$(foobar -h | grep 'version: v' | awk '{print $NF}')
        let ptn_cmd_sub1 = format!(r"^{}=`.*`$", ptn_env_name);
        let ptn_cmd_sub2 = format!(r"^{}=\$\(.*\)$", ptn_env_name);
        if libs::re::re_contains(token, &ptn_cmd_sub1)
            || libs::re::re_contains(token, &ptn_cmd_sub2)
            || libs::re::re_contains(token, r"^\$\(.+\)$")
        {
            return false;
        }

        // for cmd-line like `alias foo='echo $PWD'`
        let ptn_env = format!(r"='.*\$\{{?{}\}}?.*'$", ptn_env_name);
        !libs::re::re_contains(token, &ptn_env)
    }

    pub fn expand_env(sh: &Shell, tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, token) in tokens.iter() {
            if sep == "`" || sep == "'" {
                idx += 1;
                continue;
            }

            if !env_in_token(token) {
                idx += 1;
                continue;
            }

            let mut _token = token.clone();
            while env_in_token(&_token) {
                _token = expand_one_env(sh, &_token);
            }
            buff.push((idx, _token));
            idx += 1;
        }

        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }

    fn should_do_dollar_command_extension(line: &str) -> bool {
        libs::re::re_contains(line, r"\$\([^\)]+\)") &&
        !libs::re::re_contains(line, r"='.*\$\([^\)]+\).*'$")
    }

    fn do_command_substitution_for_dollar(sh: &mut Shell, tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();

        for (sep, token) in tokens.iter() {
            if sep == "'" || sep == "\\" || !should_do_dollar_command_extension(token) {
                idx += 1;
                continue;
            }

            let mut line = token.to_string();
            loop {
                if !should_do_dollar_command_extension(&line) {
                    break;
                }

                let ptn_cmd = r"\$\((.+)\)";
                let cmd = match libs::re::find_first_group(ptn_cmd, &line) {
                    Some(x) => x,
                    None => {
                        println_stderr!("cicada: calculator: no first group");
                        return;
                    }
                };

                let cmd_result = match CommandLine::from_line(&cmd, sh) {
                    Ok(c) => {
                        log!("run subcmd dollar: {:?}", &cmd);
                        let (term_given, cr) = core::run_pipeline(sh, &c, true, true, false);
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid(0);
                                give_terminal_to(gid);
                            }
                        }

                        cr
                    }
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        continue;
                    }
                };

                let output_txt = cmd_result.stdout.trim();

                let ptn = r"(?P<head>[^\$]*)\$\(.+\)(?P<tail>.*)";
                let re;
                if let Ok(x) = Regex::new(ptn) {
                    re = x;
                } else {
                    return;
                }

                let to = format!("${{head}}{}${{tail}}", output_txt);
                let line_ = line.clone();
                let result = re.replace(&line_, to.as_str());
                line = result.to_string();
            }

            buff.insert(idx, line.clone());
            idx += 1;
        }

        for (i, text) in buff.iter() {
            tokens[*i].1 = text.to_string();
        }
    }

    fn do_command_substitution_for_dot(sh: &mut Shell, tokens: &mut types::Tokens) {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();
        for (sep, token) in tokens.iter() {
            let new_token: String;
            if sep == "`" {
                log!("run subcmd dot1: {:?}", token);
                let cr = match CommandLine::from_line(token, sh) {
                    Ok(c) => {
                        let (term_given, _cr) = core::run_pipeline(sh, &c, true, true, false);
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid(0);
                                give_terminal_to(gid);
                            }
                        }

                        _cr
                    }
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        continue;
                    }
                };

                new_token = cr.stdout.trim().to_string();
            } else if sep == "\"" || sep.is_empty() {
                let re;
                if let Ok(x) = Regex::new(r"^([^`]*)`([^`]+)`(.*)$") {
                    re = x;
                } else {
                    println_stderr!("cicada: re new error");
                    return;
                }
                if !re.is_match(token) {
                    idx += 1;
                    continue;
                }
                let mut _token = token.clone();
                let mut _item = String::new();
                let mut _head = String::new();
                let mut _output = String::new();
                let mut _tail = String::new();
                loop {
                    if !re.is_match(&_token) {
                        if !_token.is_empty() {
                            _item = format!("{}{}", _item, _token);
                        }
                        break;
                    }
                    for cap in re.captures_iter(&_token) {
                        _head = cap[1].to_string();
                        _tail = cap[3].to_string();
                        log!("run subcmd dot2: {:?}", &cap[2]);

                        let cr = match CommandLine::from_line(&cap[2], sh) {
                            Ok(c) => {
                                let (term_given, _cr) = core::run_pipeline(sh, &c, true, true, false);
                                if term_given {
                                    unsafe {
                                        let gid = libc::getpgid(0);
                                        give_terminal_to(gid);
                                    }
                                }

                                _cr
                            }
                            Err(e) => {
                                println_stderr!("cicada: {}", e);
                                continue;
                            }
                        };

                        _output = cr.stdout.trim().to_string();
                    }
                    _item = format!("{}{}{}", _item, _head, _output);
                    if _tail.is_empty() {
                        break;
                    }
                    _token = _tail.clone();
                }
                new_token = _item;
            } else {
                idx += 1;
                continue;
            }

            buff.insert(idx, new_token.clone());
            idx += 1;
        }

        for (i, text) in buff.iter() {
            tokens[*i].1 = text.to_string();
        }
    }

    fn do_command_substitution(sh: &mut Shell, tokens: &mut types::Tokens) {
        do_command_substitution_for_dot(sh, tokens);
        do_command_substitution_for_dollar(sh, tokens);
    }

    pub fn do_expansion(sh: &mut Shell, tokens: &mut types::Tokens) {
        let line = parsers::parser_line::tokens_to_line(tokens);
        if tools::is_arithmetic(&line) {
            return;
        }

        if tokens.len() >= 2 && tokens[0].1 == "export" && tokens[1].1.starts_with("PROMPT=") {
            return;
        }

        expand_alias(sh, tokens);
        expand_home(tokens);
        expand_env(sh, tokens);
        expand_brace(tokens);
        expand_glob(tokens);
        do_command_substitution(sh, tokens);
        expand_brace_range(tokens);
    }

    pub fn trim_multiline_prompts(line: &str) -> String {
        // remove sub-prompts from multiple line mode
        // 1. assuming '\n' char cannot be typed manually?
        // 2. `>>` is defined as `src/prompt/multilines.rs`
        let line_new = libs::re::replace_all(line, r"\\\n>> ", "");
        let line_new = libs::re::replace_all(&line_new, r"\| *\n>> ", "| ");
        libs::re::replace_all(&line_new, r"(?P<NEWLINE>\n)>> ", "$NEWLINE")
    }

    fn proc_has_terminal() -> bool {
        unsafe {
            let tgid = libc::tcgetpgrp(0);
            let pgid = libc::getpgid(0);
            tgid == pgid
        }
    }
}
/// Message Passing
pub mod signals
{

}
/// Types and Traits for working with asynchronous tasks.
pub mod task
{
    pub use std::task::{ * };
}
/// Native threads.
pub mod thread
{
    pub use std::thread::{ * };
}
/// Temporal quantification.
pub mod time
{
    pub use std::time::{ * };
    pub use timed::{ * };
}
/// A contiguous growable array type with heap-allocated contents, written Vec<T>.
pub mod vec
{
    pub use std::vec::{ * };
}

pub unsafe fn domain()
{
    env::initialize_pathing();

    let mut sh = shell::Shell::new();
    let args: Vec<String> = env::args().collect();

    if libs::progopts::is_login(&args) {
        rcfile::load_rc_files(&mut sh);
        sh.is_login = true;
    }

    // Initialize command cache for highlighting
    highlight::init_command_cache();
    highlight::update_aliases(&sh);

    if libs::progopts::is_script(&args) {
        log!("run script: {:?} ", &args);
        let status = scripting::run_script(&mut sh, &args);
        std::process::exit(status);
    }

    if libs::progopts::is_command_string(&args) {
        // handles `cicada -c 'echo hi && echo yoo'`,
        // e.g. it could be triggered from Vim (`:!ls` etc).
        let line = tools::env_args_to_command_line();
        log!("run with -c args: {}", &line);
        execute::run_command_line(&mut sh, &line, false, false);
        std::process::exit(sh.previous_status);
    }

    if libs::progopts::is_non_tty() {
        // cases like open a new MacVim window,
        // (i.e. CMD+N) on an existing one
        execute::run_procs_for_non_tty(&mut sh);
        return;
    }

    let mut rl;
    match Interface::new("cicada") {
        Ok(x) => rl = x,
        Err(e) => {
            // non-tty will raise errors here
            println!("cicada: lineread error: {}", e);
            return;
        }
    }

    rl.define_function("enter-function", Arc::new(prompt::EnterFunction));
    rl.bind_sequence("\r", Command::from_str("enter-function"));

    let highlighter = highlight::create_highlighter();
    rl.set_highlighter(highlighter);

    history::init(&mut rl);
    rl.set_completer(Arc::new(completers::CicadaCompleter {
        sh: Arc::new(sh.clone()),
    }));

    let sig_handler_enabled = tools::is_signal_handler_enabled();
    if sig_handler_enabled {
        signals::setup_sigchld_handler();
        // block the signals at most of time, since Rust is not "async-signal-safe"
        // yet. see https://github.com/rust-lang/rfcs/issues/1368
        // we'll unblock them when necessary only.
        signals::block_signals();
    }

    loop {
        let prompt = prompt::get_prompt(&sh);
        match rl.set_prompt(&prompt) {
            Ok(_) => {}
            Err(e) => {
                println_stderr!("cicada: prompt error: {}", e);
            }
        }

        if sig_handler_enabled {
            // FIXME: in `rl.read_line()` below, there is lots of Rust code,
            // which may not be async-signal-safe. see follow links for details:
            // - https://ldpreload.com/blog/signalfd-is-useless
            // - https://man7.org/linux/man-pages/man7/signal-safety.7.html
            signals::unblock_signals();
        }
        match rl.read_line() {
            Ok(ReadResult::Input(line)) => {
                if sig_handler_enabled {
                    signals::block_signals();
                }

                let line = shell::trim_multiline_prompts(&line);
                if line.trim() == "" {
                    jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                    continue;
                }
                sh.cmd = line.clone();

                let tsb = ctime::DateTime::now().unix_timestamp();
                let mut line = line.clone();

                // since `!!` expansion is only meaningful in an interactive
                // shell we extend it here, instead of in `run_command_line()`.
                tools::extend_bangbang(&sh, &mut line);

                let mut status = 0;
                let cr_list = execute::run_command_line(&mut sh, &line, true, false);
                if let Some(last) = cr_list.last() {
                    status = last.status;
                }
                let tse = ctime::DateTime::now().unix_timestamp();

                if !sh.cmd.starts_with(' ') && line != sh.previous_cmd {
                    history::add(&sh, &mut rl, &line, status, tsb, tse);
                    sh.previous_cmd = line.clone();
                }

                if tools::is_shell_altering_command(&line) {
                    // since our shell object need to be passed into
                    // `lineread::Completer` with an Arc.
                    // I currently do not know how to share the same sh
                    // instance at hand with it.

                    // update the Arc clone when alias/function/env changes
                    rl.set_completer(Arc::new(completers::CicadaCompleter {
                        sh: Arc::new(sh.clone()),
                    }));

                    // Update aliases in the highlighter when they might have changed
                    highlight::update_aliases(&sh);
                }

                jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                continue;
            }
            Ok(ReadResult::Eof) => {
                if let Ok(x) = env::var("NO_EXIT_ON_CTRL_D") {
                    if x == "1" {
                        println!();
                    }
                } else {
                    println!("exit");
                    break;
                }
            }
            Ok(ReadResult::Signal(s)) => {
                println_stderr!("readline signal: {:?}", s);
            }
            Err(e) => {
                println_stderr!("readline error: {}", e);
                // There maybe other reason of this Err, but possibly it occurs
                // in cases we give term to a child, and it stops, and we
                // didn't have term back to shell in waitpid places. Here
                // it's a last resort.
                // FIXME: we only need this trick when job-control has issues
                unsafe {
                    let gid = libc::getpgid(0);
                    shell::give_terminal_to(gid);
                }
            }
        }
        if sig_handler_enabled {
            signals::block_signals();
        }
    }
}

fn main()
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);
        domain();
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1881
