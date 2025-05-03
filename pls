/*!
    pls is a bash-like Unix shell written in Rust.

    If you would like to use pls as a regular shell,
    please see details in [its repository]( https://github.com/mitnk/pls )

    Here is how to use pls as a library:

    **Add pls into Cargo.toml**

    ```ignore
    [dependencies]
    pls = "1.0"
    ```

    **Use pls functions**

    ```no_run
    extern crate pls;

    fn main()
    {
        let info = pls::parse_line( "echo 'hi yoo' | `which wc`" );
        assert!( info.is_complete );

        let tokens = info.tokens;
        assert_eq!( tokens.len(), 4 );

        assert_eq!( tokens[0].0, "" );
        assert_eq!( tokens[0].1, "echo" );

        assert_eq!( tokens[1].0, "'" );
        assert_eq!( tokens[1].1, "hi yoo" );

        assert_eq!( tokens[2].0, "" );
        assert_eq!( tokens[2].1, "|" );

        assert_eq!( tokens[3].0, "`" );
        assert_eq!( tokens[3].1, "which wc" );

        let out1 = pls::run( "ls Cargo.toml foo" );
        assert_eq!( out1.status, 1 );
        assert_eq!( out1.stdout, "Cargo.toml\n" );
        assert_eq!( out1.stderr, "ls:foo:No such file or directory\n" );

        let out2 = pls::run( "ls | wc" );
        assert_eq!( out2.status, 0 );
        assert_eq!( out2.stdout, "       4       4      33\n" );
    }
    ```
*/
/*
Features */
#![feature
( 

 )]
/*
Allowances */
#![allow
( 
    dead_code,
    non_camel_case_types,
    unknown_lints,
    unused_attributes,
    unused_imports,
    unused_unsafe,
    unused_variables,
    unreachable_code,    
 )]
/*
External Macros
#[macro_use] extern crate bitflags;
 */
 #[macro_use] extern crate lazy_static;
/*
External Crates */
extern crate libc;
extern crate rand;
extern crate regex as re;
extern crate nix;
extern crate time as timed;
/*
    extern crate fnv;
    extern crate libc;
    extern crate memchr;
    extern crate smallvec;
    extern crate time as timing;
    extern crate unicode_normalization;
    extern crate unicode_width; 
*/
#[macro_use] pub mod macros
{
    #[macro_export] macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            use ::io::Write;
            match writeln!( &mut ::io::stderr(), $fmt )
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            use ::io::Write;
            match writeln!( &mut ::io::stderr(), $fmt, $($arg)* )
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

    #[macro_export] macro_rules! log
    {
        ($fmt:expr) => (
            let log_file = if let Ok(x) = ::env::var("CICADA_LOG_FILE") {
                x.clone()
            } else {
                String::new()
            };

            if !log_file.is_empty() {
                use ::io::Write as _;

                let msg = $fmt;
                match ::fs::OpenOptions::new().append(true).create(true).open(&log_file) {
                    Ok(mut cfile) => {
                        let pid = ::process::getpid();
                        let now = ::time::c::DateTime::now();
                        let msg = format!("[{}][{}] {}", now, pid, msg);
                        let msg = if msg.ends_with('\n') { msg } else { format!("{}\n", msg) };
                        match cfile.write_all(msg.as_bytes()) {
                            Ok(_) => {}
                            Err(_) => println!("tlog: write_all error")
                        }
                    }
                    Err(_) => println!("tlog: open file error"),
                }

            }
        );

        ($fmt:expr, $($arg:tt)*) => (
            let msg = format!($fmt, $($arg)*);
            log!(&msg);
        );
    }

}

pub mod alloc
{
    pub use std::alloc::{ * };
}

pub mod array
{
    pub use std::array::{ * };
}

pub mod borrow
{
    //! A module for working with borrowed data.
    pub use std::borrow::{ * };
}

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod bytes
{
    use ::
    {
        *,
    };
}

pub mod cell
{
    pub use std::cell::{ * };
}

pub mod cmp
{
    pub use std::cmp::{ * };
}

pub mod collections
{
    //! Collection types.
    pub use std::collections::{ * };
}

pub mod commands
{
    //! Available Commands
    use ::
    {
        collections::HashSet,
        path::Path,
        sync::Mutex,
        *,
    };
    /*
        use std::ops::Range;
        use std::sync::Arc;
            use std::os::unix::fs::PermissionsExt;

        use lineread::highlighting::{Highlighter, Style};

        use crate::tools;
        use crate::shell;
        use crate::parsers::parser_line;
    */
    lazy_static!
    {
        static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
    }
    /// Initialize the available commands cache by scanning PATH directories
    pub fn initialize_cache()
    {
        let commands = scan_available_commands();
        
        if let Ok(mut cache) = AVAILABLE_COMMANDS.lock()
        {
            *cache = commands;
        }
    }

    pub fn scan_available_commands() -> HashSet<String>
    {
        let mut commands = HashSet::new();

        if let Ok(path_var) = env::var("PATH")
        {
            for path in path_var.split(':')
            {
                if path.is_empty() { continue; }

                let dir_path = Path::new(path);

                if !dir_path.is_dir() { continue; }

                if let Ok(entries) = fs::read_dir(dir_path)
                {
                    for entry in entries.filter_map(Result::ok)
                    {
                        if let Ok(file_type) = entry.file_type()
                        {
                            if file_type.is_file() || file_type.is_symlink()
                            {
                                if let Ok(metadata) = entry.metadata()
                                {
                                    /*
                                    // Check if file is executable
                                    if metadata.permissions().mode() & 0o111 != 0
                                    {
                                        if let Some(name) = entry.file_name().to_str() {
                                            commands.insert(name.to_string());
                                        }
                                    }
                                    */
                                    if let Some(name) = entry.file_name().to_str()
                                    { commands.insert(name.to_string()); }
                                }
                            }
                        }
                    }
                }
            }
        }

        commands
    }

    pub fn update_aliases(sh: &shell::Shell)
    {
        if let Ok(mut aliases) = ALIASES.lock()
        {
            aliases.clear();
            for alias_name in sh.aliases.keys()
            {
                aliases.insert(alias_name.clone());
            }
        }
    }
}

pub mod convert
{
    //! Traits for conversions between types.
    pub use std::convert::{ * };
}

pub mod env
{
    //! Inspection and manipulation of the process's environment.
    pub use std::env::{ * };
    use ::
    {
        fs::{ read_dir },
        path::{ Path },
        *,
    };

    pub fn initialize_paths()
    {
        let mut directories: Vec<String> = vec![];
        for directory in
        [
            "/usr/local/sbin",
            "/usr/local/bin",
            "/usr/sbin",
            "/usr/bin",
            "/sbin",
            "/bin",
        ]
        { 
            if Path::new( directory ).exists() { directories.push( directory.to_string() ); }
        }

        if let Ok( environment ) = var("PATH")
        {
            for directory in environment.split(":") 
            {
                println!( r#"{}"#, directory );

                if !directories.contains( &directory.to_string() ) 
                {
                    directories.push( directory.to_string() );
                }
            }
        }

        let directories = directories.join(":");
        set_var( "PATH", directories );
    }

    pub fn args_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = args();

        if env_args.len() <= 1 { return result; }

        for (i, arg) in env_args.enumerate()
        {
            if i == 0 || arg == "-c" { continue; }

            result.push_str(arg.as_str());
        }

        result
    }

    pub fn find_file_in_path(filename: &str, exec: bool) -> String
    {
        /*
        let env_path = match var("PATH") {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: error with env PATH: {:?}", e);
                return String::new();
            }
        };
        let vec_path: Vec<&str> = env_path.split(':').collect();
        for p in &vec_path {
            match read_dir(p) {
                Ok(list) => {
                    for entry in list.flatten() {
                        if let Ok(name) = entry.file_name().into_string() {
                            if name != filename {
                                continue;
                            }

                            if exec
                            {
                                let _mode = match entry.metadata() {
                                    Ok(x) => x,
                                    Err(e) => {
                                        println_stderr!("cicada: metadata error: {:?}", e);
                                        continue;
                                    }
                                };
                                let mode = _mode.permissions().mode();
                                if mode & 0o111 == 0 {  continue; }
                            }

                            return entry.path().to_string_lossy().to_string();
                        }
                    }
                }

                Err(e) =>
                {
                    if e.kind() == ErrorKind::NotFound { continue; }
                    log!("cicada: fs read_dir error: {}: {}", p, e);
                }
            }
        }*/

        String::new()
    }
}

pub mod error
{
    //! Error Handling With Results.
    pub use std::error::{ * };
    /*
    uuid::error.rs */
    pub mod uuid
    {
        use ::
        {
            str::{ from_utf8, from_utf8_unchecked, from_utf8_mut_unchecked },
            *,
        };
        /// A general error that can occur when working with UUIDs.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct Error(pub ErrorKind);

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum ErrorKind
        {
            /// Invalid character in the [`Uuid`] string.
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            Char { character: char, index: usize },
            /// A simple [`Uuid`] didn't contain 32 characters.
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            SimpleLength { len: usize },
            /// A byte array didn't contain 16 bytes
            ByteLength { len: usize },
            /// A hyphenated [`Uuid`] didn't contain 5 groups
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            GroupCount { count: usize },
            /// A hyphenated [`Uuid`] had a group that wasn't the right length
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            GroupLength {
                group: usize,
                len: usize,
                index: usize,
            },
            /// The input was not a valid UTF8 string
            InvalidUTF8,
            /// The UUID is nil.
            Nil,
            /// Some other error occurred.
            Other,
        }
        /// A string that is guaranteed to fail to parse to a [`Uuid`].
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct InvalidUuid<'a>(pub &'a [u8]);

        impl<'a> InvalidUuid<'a>
        {
            /// Converts the lightweight error type into detailed diagnostics.
            pub fn into_err(self) -> Error
            {
                let input_str = match from_utf8(self.0)
                {
                    Ok(s) => s,
                    Err(_) => return Error(ErrorKind::InvalidUTF8),
                };

                let (uuid_str, offset, simple) = match input_str.as_bytes()
                {
                    [b'{', s @ .., b'}'] => (s, 1, false),
                    [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..] => { (s, "urn:uuid:".len(), false) }
                    s => (s, 0, true),
                };

                let mut hyphen_count = 0;
                let mut group_bounds = [0; 4];
                let uuid_str = unsafe { from_utf8_unchecked(uuid_str) };

                for (index, character) in uuid_str.char_indices()
                {
                    let byte = character as u8;
                    if character as u32 - byte as u32 > 0
                    {
                        return Error(ErrorKind::Char
                        {
                            character,
                            index: index + offset + 1,
                        });
                    }
                    else if byte == b'-'
                    {
                        if hyphen_count < 4 { group_bounds[hyphen_count] = index; }
                        hyphen_count += 1;
                    }
                    else if !byte.is_ascii_hexdigit()
                    {
                        return Error(ErrorKind::Char
                        {
                            character: byte as char,
                            index: index + offset + 1,
                        });
                    }
                }

                if hyphen_count == 0 && simple
                {
                    Error(ErrorKind::SimpleLength
                    {
                        len: input_str.len(),
                    })
                }
                else if hyphen_count != 4
                {
                    Error(ErrorKind::GroupCount
                    {
                        count: hyphen_count + 1,
                    })
                }
                else
                {
                    const BLOCK_STARTS: [usize; 5] = [0, 9, 14, 19, 24];
                    for i in 0..4
                    {
                        if group_bounds[i] != BLOCK_STARTS[i + 1] - 1
                        {
                            return Error(ErrorKind::GroupLength
                            {
                                group: i,
                                len: group_bounds[i] - BLOCK_STARTS[i],
                                index: offset + BLOCK_STARTS[i] + 1,
                            });
                        }
                    }
                    
                    Error(ErrorKind::GroupLength
                    {
                        group: 4,
                        len: input_str.len() - BLOCK_STARTS[4],
                        index: offset + BLOCK_STARTS[4] + 1,
                    })
                }
            }
        }
        
        impl fmt::Display for Error
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                match self.0
                {
                    ErrorKind::Char
                    {
                        character, index, ..
                    } =>
                    {
                        write!(f, "invalid character: expected an optional prefix of `urn:uuid:` followed by [0-9a-fA-F-], found `{}` at {}", character, index)
                    }
                    
                    ErrorKind::SimpleLength { len } =>
                    {
                        write!
                        (
                            f,
                            "invalid length: expected length 32 for simple format, found {}",
                            len
                        )
                    }

                    ErrorKind::ByteLength { len } => { write!(f, "invalid length: expected 16 bytes, found {}", len) }
                    ErrorKind::GroupCount { count } => { write!(f, "invalid group count: expected 5, found {}", count) }
                    ErrorKind::GroupLength { group, len, .. } =>
                    {
                        let expected = [8, 4, 4, 4, 12][group];
                        write!(
                            f,
                            "invalid group length in group {}: expected {}, found {}",
                            group, expected, len
                        )
                    }
                    ErrorKind::InvalidUTF8 => write!(f, "non-UTF8 input"),
                    ErrorKind::Nil => write!(f, "the UUID is nil"),
                    ErrorKind::Other => write!(f, "failed to parse a UUID"),
                }
            }
        }
        
        impl ::error::Error for self::Error {}
    }
    /*
    errno 0.2.4*/
    pub mod no
    {        
        pub mod uefi
        {
            use ::
            {
                *,
            };
        }

        pub mod unix
        {
            use ::
            {
                error::{ Errno },
                libc::{self, c_int, size_t, strerror_r, strlen},
                *,
            };

            fn from_utf8_lossy(input: &[u8]) -> &str
            {
                match str::from_utf8(input)
                {
                    Ok(valid) => valid,
                    Err(error) => unsafe { str::from_utf8_unchecked(&input[..error.valid_up_to()]) },
                }
            }

            pub fn with_description<F, T>(err: Errno, callback: F) -> T where
            F: FnOnce(Result<&str, Errno>) -> T
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

            pub const STRERROR_NAME: &str = "strerror_r";

            pub fn errno() -> Errno {
                unsafe { Errno(*errno_location()) }
            }

            pub fn set_errno(Errno(errno): Errno) {
                unsafe {
                    *errno_location() = errno;
                }
            }
            
            fn errno_location() -> *mut c_int
            {
                0 as *mut c_int
            }
        }
        
        pub mod windows
        {
            use ::
            {
                char::{self, REPLACEMENT_CHARACTER},
                error::{ Errno },
                *,
            };
            /*
            use core::char::{self, REPLACEMENT_CHARACTER};
            use core::ptr;
            use core::str;
            use windows_sys::Win32::Foundation::{GetLastError, SetLastError, WIN32_ERROR};
            use windows_sys::Win32::System::Diagnostics::Debug::{
                FormatMessageW, FORMAT_MESSAGE_FROM_SYSTEM, FORMAT_MESSAGE_IGNORE_INSERTS,
            };
            */
            use crate::Errno;

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
                // This value is calculated from the macro
                // MAKELANGID(LANG_SYSTEM_DEFAULT, SUBLANG_SYS_DEFAULT)
                let lang_id = 0x0800_u32;

                let mut buf = [0u16; 2048];

                unsafe {
                    let res = FormatMessageW(
                        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                        ptr::null_mut(),
                        err.0 as u32,
                        lang_id,
                        buf.as_mut_ptr(),
                        buf.len() as u32,
                        ptr::null_mut(),
                    );
                    if res == 0 {
                        // Sometimes FormatMessageW can fail e.g. system doesn't like lang_id
                        let fm_err = errno();
                        return callback(Err(fm_err));
                    }

                    let mut msg = [0u8; 2048];
                    let msg = from_utf16_lossy(&buf[..res as usize], &mut msg[..]);
                    // Trim trailing CRLF inserted by FormatMessageW
                    callback(Ok(msg.trim_end()))
                }
            }

            pub const STRERROR_NAME: &str = "FormatMessageW";

            pub fn errno() -> Errno
            {
                unsafe { Errno(GetLastError() as i32) }
            }

            pub fn set_errno(Errno(errno): Errno)
            {
                unsafe { SetLastError(errno as WIN32_ERROR) }
            }
        }

        #[cfg(unix)] pub use self::unix as sys;
        #[cfg(unix)] pub use self::windows as sys;

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
                    Err(fm_err) => write
                    (
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
            fn description(&self) -> &str { "system error" }
        }
        
        impl From<Errno> for io::Error
        {
            fn from(errno: Errno) -> Self { io::Error::from_raw_os_error(errno.0) }
        }
        /// Returns the platform-specific value of `errno`.
        pub fn errno() -> Errno {
            sys::errno()
        }

        /// Sets the platform-specific value of `errno`.
        pub fn set_errno(err: Errno) {
            sys::set_errno(err)
        }
    }
}

pub mod ffi
{
    pub use std::ffi::{ * };
}

pub mod fmt
{
    //! Utilities for formatting and printing strings.
    //! Adapters for alternative string formats for UUID
    pub use std::fmt::{ * };
    use ::
    {
        borrow::{ Borrow },
        primitive::{ Braced, Hyphenated, Simple, Urn, Uuid, Variant },
        str::{ FromStr },
        string::{ String, ToString },
        *,
    };
    // rust-uuid v1.4::fmt.rs

    impl std::fmt::Debug for Uuid
    {
        #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
    }

    impl fmt::Display for Uuid
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
    }
    
    impl From<Uuid> for String
    {
        fn from(uuid: Uuid) -> Self { uuid.to_string() }
    }

    impl fmt::Display for Variant
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            match *self
            {
                Variant::NCS => write!(f, "NCS"),
                Variant::RFC4122 => write!(f, "RFC4122"),
                Variant::Microsoft => write!(f, "Microsoft"),
                Variant::Future => write!(f, "Future"),
            }
        }
    }

    impl fmt::LowerHex for Uuid
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            fmt::LowerHex::fmt(self.as_hyphenated(), f)
        }
    }

    impl fmt::UpperHex for Uuid
    {
        #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            fmt::UpperHex::fmt(self.as_hyphenated(), f)
        }
    }
}

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod get
{
    //! State Retrieval
    use ::
    {
        env::{ current_dir },
        path::{ PathBuf },
        *,
    };
    /*
    pub fn get_current_dir(...) -> String  */
    pub fn current_directory() -> String 
    {
        let mut current = PathBuf::new();
        
        match current_dir() 
        {
            Ok( directory ) => current = directory,
            Err( code ) => { println_stderr!("env current_dir() failed: {}", code ); }
        }

        let mut directory = "";
        
        match current.to_str()
        {
            Some( current ) => directory = current,
            None => { println_stderr!("current_dir to str failed."); }
        }

        directory.to_string()
    }
}

pub mod hash
{
    //! Generic hashing support.
    pub use std::hash::{ * };
}

pub mod hint
{
    pub use std::hint::{ * };
}

pub mod io
{
    //! Traits, helpers, and type definitions for core I/O functionality.
    pub use std::io::{ * };
    /// A borrowed byte buffer which is incrementally filled and initialized.
    pub struct BorrowedBuf<'data>
    {
        /// The buffer's underlying data.
        buf: &'data mut [::mem::MaybeUninit<u8>],
        /// The length of `self.buf` which is known to be filled.
        filled: usize,
        /// The length of `self.buf` which is known to be initialized.
        init: usize,
    }

    impl ::fmt::Debug for BorrowedBuf<'_>
    {
        fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result
        {
            f.debug_struct("BorrowedBuf")
            .field("init", &self.init)
            .field("filled", &self.filled)
            .field("capacity", &self.capacity())
            .finish()
        }
    }
    /// Creates a new `BorrowedBuf` from a fully initialized slice.
    impl<'data> From<&'data mut [u8]> for BorrowedBuf<'data>
    {
        #[inline] fn from(slice: &'data mut [u8]) -> BorrowedBuf<'data>
        {
            unsafe
            {
                let pointer:*mut [u8] = slice as *mut [u8];
                BorrowedBuf
                {
                    buf: ::mem::transmute( pointer ),//: unsafe { (slice as *mut [u8]).as_uninit_slice_mut().unwrap() },
                    filled: 0,
                    init: slice.len(),
                }
            }
        }
    }
    /// Creates a new `BorrowedBuf` from an uninitialized buffer.
    impl<'data> From<&'data mut [::mem::MaybeUninit<u8>]> for BorrowedBuf<'data>
    {
        #[inline] fn from(buf: &'data mut [::mem::MaybeUninit<u8>]) -> BorrowedBuf<'data>
        {
            BorrowedBuf { buf, filled: 0, init: 0 }
        }
    }

    impl<'data> BorrowedBuf<'data>
    {
        /// Returns the total capacity of the buffer.
        #[inline] pub fn capacity(&self) -> usize { self.buf.len() }
        /// Returns the length of the filled part of the buffer.
        #[inline] pub fn len(&self) -> usize { self.filled }
        /// Returns the length of the initialized part of the buffer.
        #[inline] pub fn init_len(&self) -> usize { self.init }
        /// Returns a shared reference to the filled portion of the buffer.
        #[inline] pub fn filled(&self) -> &[u8] 
        {
            unsafe
            {
                ::mem::transmute( self.buf.get_unchecked(..self.filled) )
            }
        }
        /// Returns a mutable reference to the filled portion of the buffer.
        #[inline] pub fn filled_mut(&mut self) -> &mut [u8]
        {
            unsafe
            {
                ::mem::transmute( self.buf.get_unchecked_mut(..self.filled) )
            }
        }
        /// Returns a shared reference to the filled portion of the buffer with its original lifetime.
        #[inline] pub fn into_filled(self) -> &'data [u8]
        {
            unsafe
            {
                ::mem::transmute( self.buf.get_unchecked(..self.filled) )
            }
        }
        /// Returns a mutable reference to the filled portion of the buffer with its original lifetime.
        #[inline] pub fn into_filled_mut(self) -> &'data mut [u8]
        {
            unsafe
            {
                ::mem::transmute( self.buf.get_unchecked_mut(..self.filled) )
            }
        }
        /// Returns a cursor over the unfilled part of the buffer.
        #[inline] pub fn unfilled<'this>(&'this mut self) -> BorrowedCursor<'this>
        {
            unsafe
            {
                BorrowedCursor
                {
                    start: self.filled,
                    buf: ::mem::transmute::<&'this mut BorrowedBuf<'data>, &'this mut BorrowedBuf<'this>>(self),
                }
            }
        }
        /// Clears the buffer, resetting the filled region to empty.
        #[inline] pub fn clear(&mut self) -> &mut Self
        {
            self.filled = 0;
            self
        }
        /// Asserts that the first `n` bytes of the buffer are initialized.
        #[inline] pub unsafe fn set_init(&mut self, n: usize) -> &mut Self
        {
            self.init = ::cmp::max(self.init, n);
            self
        }
    }
    /// A writeable view of the unfilled portion of a [`BorrowedBuf`].
    use mem::MaybeUninit;


    #[derive(Debug)]
    pub struct BorrowedCursor<'a>
    {
        /// The underlying buffer.
        buf: &'a mut BorrowedBuf<'a>,
        /// The length of the filled portion of the underlying buffer at the time of the cursor's creation.
        start: usize,
    }

    impl<'a> BorrowedCursor<'a>
    {
        /// Reborrows this cursor by cloning it with a smaller lifetime.
        #[inline] pub fn reborrow<'this>(&'this mut self) -> BorrowedCursor<'this>
        {
            unsafe
            {
                BorrowedCursor
                {
                    buf: ::mem::transmute::<&'this mut BorrowedBuf<'a>, &'this mut BorrowedBuf<'this>>( self.buf ),
                    start: self.start,
                }
            }
        }
        /// Returns the available space in the cursor.
        #[inline] pub fn capacity(&self) -> usize { self.buf.capacity() - self.buf.filled }
        /// Returns the number of bytes written to this cursor since it was created from a `BorrowedBuf`.
        #[inline] pub fn written(&self) -> usize { self.buf.filled - self.start }
        /// Returns a shared reference to the initialized portion of the cursor.
        #[inline] pub fn init_ref(&self) -> &[u8]
        {
            unsafe
            {
                unsafe
                {
                    ::mem::transmute( self.buf.buf.get_unchecked(self.buf.filled..self.buf.init) )
                }
            }
        }
        /// Returns a mutable reference to the initialized portion of the cursor.
        #[inline] pub fn init_mut(&mut self) -> &mut [u8]
        {
            unsafe
            {
                ::mem::transmute( self.buf.buf.get_unchecked_mut(self.buf.filled..self.buf.init) )
            }
        }
        /// Returns a mutable reference to the uninitialized part of the cursor.
        #[inline] pub fn uninit_mut(&mut self) -> &mut [MaybeUninit<u8>]
        {
            unsafe { self.buf.buf.get_unchecked_mut(self.buf.init..) }
        }
        /// Returns a mutable reference to the whole cursor.
        #[inline] pub unsafe fn as_mut(&mut self) -> &mut [MaybeUninit<u8>]
        {
            unsafe { self.buf.buf.get_unchecked_mut(self.buf.filled..) }
        }
        /// Advances the cursor by asserting that `n` bytes have been filled.
        #[inline] pub fn advance(&mut self, n: usize) -> &mut Self
        {
            /*
            let filled = self.buf.filled.strict_add(n);
            assert!(filled <= self.buf.init);
            self.buf.filled = filled;
            */
            self
        }
        /// Advances the cursor by asserting that `n` bytes have been filled.
        #[inline] pub unsafe fn advance_unchecked(&mut self, n: usize) -> &mut Self
        {
            self.buf.filled += n;
            self.buf.init = ::cmp::max(self.buf.init, self.buf.filled);
            self
        }
        /// Initializes all bytes in the cursor.
        #[inline] pub fn ensure_init(&mut self) -> &mut Self
        {
            unsafe
            {
                let uninit = self.uninit_mut();
                ::ptr::write_bytes(uninit.as_mut_ptr(), 0, uninit.len());
                self.buf.init = self.buf.capacity();
                self
            }
        }
        /// Asserts that the first `n` unfilled bytes of the cursor are initialized.
        #[inline] pub unsafe fn set_init(&mut self, n: usize) -> &mut Self
        {
            self.buf.init = ::cmp::max(self.buf.init, self.buf.filled + n);
            self
        }
        /// Appends data to the cursor, advancing position within its buffer.
        #[inline] pub fn append(&mut self, buf: &[u8])
        {
            unsafe
            {
                /*
                assert!(self.capacity() >= buf.len());
                self.as_mut()[..buf.len()].write_copy_of_slice(buf);
                self.set_init(buf.len());
                self.buf.filled += buf.len();
                */
            }
        }
    }
}

pub mod is
{
    //! State Testing
    use ::
    {
        *,
    };
    /*
    pub fn is_builtin(...) -> bool */
    pub fn builtin( s:&str ) -> bool
    {
        let builtins = 
        [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
            "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
            "minfd", "set", "unset", "unpath",
        ];
        builtins.contains(&s)
    }
    /*
    pub fn proc_has_terminal(...) -> bool */
    pub fn process_terminal() -> bool
    {
        unsafe
        {
            let tgid = libc::tcgetpgrp(0);
            let pgid = libc::getpgid(0);
            tgid == pgid
        }
    }
    /*
    pub fn is_login(...) -> bool */
    pub fn login(args: &[String]) -> bool
    {
        if !args.is_empty() && args[0].starts_with("-") { return true; }

        if args.len() > 1 && (args[1] == "--login" || args[1] == "-l") { return true; }

        if let Ok(term_program) = ::env::var("TERM_PROGRAM")
        {
            if term_program == "vscode" { return true; }
        }

        false
    }
    /*
    pub fn is_script(...) -> bool */
    pub fn script(args: &[String]) -> bool { args.len() > 1 && !args[1].starts_with("-") }
    /*
    pub fn is_command_string(...) -> bool */
    pub fn command_string(args: &[String]) -> bool { args.len() > 1 && args[1] == "-c" }
    /*
    pub fn is_non_tty(...) -> bool */
    pub fn non_tty() -> bool { unsafe { libc::isatty(0) == 0 } }
    /*
    pub fn is_symbol_like(...) -> bool */
    pub fn symbol_like(s: &str) -> bool
    {
        s.chars().all(|c|
        {
            ascii_alphanumeric(c) || ascii_punctuation(c)
        })
    }
    /*
    pub fn is_ascii_alphanumeric(...) -> bool */
    pub fn ascii_alphanumeric(c: char) -> bool
    {
        match c {
            '\u{0041}'..='\u{005A}' | '\u{0061}'..='\u{007A}' | '\u{0030}'..='\u{0039}' => true,
            _ => false,
        }
    }
    /*
    pub fn is_ascii_punctuation(...) -> bool */
    pub fn ascii_punctuation(c: char) -> bool
    {
        match c {
            '\u{0021}'..='\u{002F}'
            | '\u{003A}'..='\u{0040}'
            | '\u{005B}'..='\u{0060}'
            | '\u{007B}'..='\u{007E}' => true,
            _ => false,
        }
    }
}

pub mod lines
{
    /*
    linefeed v0.6.0*/
    use ::
    {
        primitive::{ CommandResult },
        *,
    };
    /*
    pub fn run_lines(sh: &mut shell::Shell, lines: &str, args: &Vec<String>, capture: bool) -> Vec<CommandResult> */
    pub fn run(sh: &mut shell::Shell, lines: &str, args: &Vec<String>, capture: bool) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        /*
        match parsers::locust::parse_lines(lines)
        {
            Ok(pairs_exp) => 
            {
                for pair in pairs_exp
                {
                    let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, false, capture);
                    cr_list.append(&mut _cr_list);
                }
            }
            
            Err(e) => 
            {
                println_stderr!("syntax error: {:?}", e);
                return cr_list;
            }
        } */

        cr_list
    }
}

pub mod marker
{
    pub use std::marker::{ * };
}

pub mod mem
{
    //! Basic functions for dealing with memory.
    pub use std::mem::{ * };
}

pub mod now
{
    use ::
    {
        collections::HashMap,
        io::{ Read as _ },
        primitive::{ CommandLine, CommandResult },
        shell::Shell,
        *,
    };
    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    /// example 1: `ls`
    /// example 2: `ls | wc`
    pub fn run_procedure(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> CommandResult
    {
        let log_cmd = !sh.cmd.starts_with(' ');
        match CommandLine::from_line(line, sh)
        {
            Ok(cl) =>
            {
                if cl.is_empty()
                {
                    if !cl.envs.is_empty() { set_shell_vars(sh, &cl.envs); }
                    return CommandResult::new();
                }

                let (term_given, cr) = process::run_pipeline(sh, &cl, tty, capture, log_cmd);
                if term_given
                {
                    unsafe
                    {
                        let gid = libc::getpgid(0);
                        terminal::give_to(gid);
                    }
                }

                cr
            }
            Err(e) => {
                println_stderr!("cicada: {}", e);
                CommandResult::from_status(0, 1)
            }
        }
    }

    pub fn run_command_line(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();

        for token in parsers::line::line_to_cmds(line)
        {
            if token == ";" || token == "&&" || token == "||"
            {
                sep = token.clone();
                continue;
            }

            if sep == "&&" && status != 0 { break; }

            if sep == "||" && status == 0 { break; }

            let cmd = token.clone();
            let cr = run_procedure(sh, &cmd, tty, capture);
            status = cr.status;
            sh.previous_status = status;
            cr_list.push(cr);
        }

        cr_list
    }
    
    /// Entry point for non-ttys (e.g. Cmd-N on MacVim)
    pub fn run_procs_for_non_tty(sh: &mut Shell)
    {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        match handle.read_to_string(&mut buffer) {
            Ok(_) => {
                log!("run non tty command: {}", &buffer);
                run_command_line(sh, &buffer, false, false);
            }
            Err(e) => {
                println!("cicada: stdin.read_to_string() failed: {:?}", e);
            }
        }
    }

    fn set_shell_vars(sh: &mut Shell, envs: &HashMap<String, String>)
    {
        for (name, value) in envs.iter()
        {
            sh.set_env(name, value);
        }
    }
}

pub mod num
{
    pub use std::num::{ * };
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod panic
{
    pub use std::panic::{ * };
}

pub mod parsers
{
    use ::
    {
        *
    };

    pub mod line
    {
        use ::
        {
            *
        };
        /// Parse command line for multiple commands.
        pub fn line_to_cmds(line: &str) -> Vec<String>
        {
            let mut result = Vec::new();
            /*
            let mut sep = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let len = line.chars().count();
            for (i, c) in line.chars().enumerate() {
                if has_backslash {
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if c == '\\' && sep != "'" {
                    has_backslash = true;
                    continue;
                }

                if c == '#' {
                    if sep.is_empty() {
                        break;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == '\'' || c == '"' || c == '`' {
                    if sep.is_empty() {
                        sep.push(c);
                        token.push(c);
                        continue;
                    } else if sep == c.to_string() {
                        token.push(c);
                        sep = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == '&' || c == '|' {
                    // needs watch ahead here
                    if sep.is_empty() {
                        if i + 1 == len {
                            // for bg commands, e.g. `ls &`
                            token.push(c);
                            continue;
                        } else {
                            let c_next = match line.chars().nth(i + 1) {
                                Some(x) => x,
                                None => {
                                    println!("chars nth error - should never happen");
                                    continue;
                                }
                            };

                            if c_next != c {
                                token.push(c);
                                continue;
                            }
                        }
                    }

                    if sep.is_empty() {
                        sep.push(c);
                        continue;
                    } else if c.to_string() == sep {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push(_token);
                        }
                        token = String::new();
                        result.push(format!("{}{}", sep, sep));
                        sep = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == ';' {
                    if sep.is_empty() {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push(_token);
                        }
                        result.push(String::from(";"));
                        token = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                token.push(c);
            }
            if !token.is_empty() {
                result.push(token.trim().to_string());
            }
            */
            result
        }
    }

    pub mod uuid
    {
        //! [`Uuid`] parsing constructs and utilities.
        use ::
        {
            convert::{ TryFrom },
            error::uuid::{ * },
            primitive::{ Uuid },
            string::{ String },
            *,
        };

        pub const HEX_TABLE: &[u8; 256] =
        &{
            let mut buf = [0; 256];
            let mut i: u8 = 0;

            loop {
                buf[i as usize] = match i {
                    b'0'..=b'9' => i - b'0',
                    b'a'..=b'f' => i - b'a' + 10,
                    b'A'..=b'F' => i - b'A' + 10,
                    _ => 0xff,
                };

                if i == 255 {
                    break buf;
                }

                i += 1
            }
        };

        pub const SHL4_TABLE: &[u8; 256] = 
        &{
            let mut buf = [0; 256];
            let mut i: u8 = 0;

            loop 
            {
                buf[i as usize] = i.wrapping_shl(4);

                if i == 255 {
                    break buf;
                }

                i += 1;
            }
        };

        impl str::FromStr for Uuid
        {
            type Err = Error;
            fn from_str(uuid_str: &str) -> Result<Self, Self::Err> { Uuid::parse_str(uuid_str) }
        }

        impl TryFrom<&'_ str> for Uuid
        {
            type Error = Error;
            fn try_from(uuid_str: &'_ str) -> Result<Self, Self::Error> { Uuid::parse_str(uuid_str) }
        }
        
        impl TryFrom<String> for Uuid 
        {
            type Error = Error;
            fn try_from(uuid_str: String) -> Result<Self, Self::Error> { Uuid::try_from(uuid_str.as_ref()) }
        }

        impl Uuid 
        {
            /// Parses a `Uuid` from a string of hexadecimal digits with optional hyphens.
            pub fn parse_str(input: &str) -> Result<Uuid, Error>
            {
                try_parse(input.as_bytes())
                .map(Uuid::from_bytes)
                .map_err(InvalidUuid::into_err)
            }
            /// Parses a `Uuid` from a string of hexadecimal digits with optional hyphens.
            pub const fn try_parse(input: &str) -> Result<Uuid, Error> {
                Self::try_parse_ascii(input.as_bytes())
            }
            /// Parses a `Uuid` from a string of hexadecimal digits with optional hyphens.
            pub const fn try_parse_ascii(input: &[u8]) -> Result<Uuid, Error>
            {
                match try_parse(input)
                {
                    Ok(bytes) => Ok(Uuid::from_bytes(bytes)),
                    Err(_) => Err(Error(ErrorKind::Other)),
                }
            }
        }

        pub const fn try_parse(input: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            match (input.len(), input)
            {
                (32, s) => simple(s),
                (36, s)
                | (38, [b'{', s @ .., b'}'])
                | (45, [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..]) => {
                    hyphenated(s)
                }
                _ => Err(InvalidUuid(input)),
            }
        }

        #[inline] pub const fn braced(input: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            if let (38, [b'{', s @ .., b'}']) = (input.len(), input) { hyphenated(s) }
            else { Err(InvalidUuid(input)) }
        }

        #[inline] pub const fn urn(input: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            if let (45, [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..]) = (input.len(), input)
            { hyphenated(s) }
            else { Err(InvalidUuid(input)) }
        }

        #[inline] pub const fn simple(s: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            if s.len() != 32 { return Err(InvalidUuid(s)); }

            let mut buf: [u8; 16] = [0; 16];
            let mut i = 0;

            while i < 16
            {
                let h1 = HEX_TABLE[s[i * 2] as usize];
                let h2 = HEX_TABLE[s[i * 2 + 1] as usize];
                
                if h1 | h2 == 0xff { return Err(InvalidUuid(s)); }
                
                buf[i] = SHL4_TABLE[h1 as usize] | h2;
                i += 1;
            }

            Ok(buf)
        }

        #[inline] pub const fn hyphenated(s: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            if s.len() != 36 { return Err(InvalidUuid(s)); }
            
            match [s[8], s[13], s[18], s[23]]
            {
                [b'-', b'-', b'-', b'-'] => {}
                _ => return Err(InvalidUuid(s)),
            }

            let positions: [u8; 8] = [0, 4, 9, 14, 19, 24, 28, 32];
            let mut buf: [u8; 16] = [0; 16];
            let mut j = 0;

            while j < 8 
            {
                let i = positions[j];
                let h1 = HEX_TABLE[s[i as usize] as usize];
                let h2 = HEX_TABLE[s[(i + 1) as usize] as usize];
                let h3 = HEX_TABLE[s[(i + 2) as usize] as usize];
                let h4 = HEX_TABLE[s[(i + 3) as usize] as usize];

                if h1 | h2 | h3 | h4 == 0xff { return Err(InvalidUuid(s)); }

                buf[j * 2] = SHL4_TABLE[h1 as usize] | h2;
                buf[j * 2 + 1] = SHL4_TABLE[h3 as usize] | h4;
                j += 1;
            }

            Ok(buf)
        }
    }
}

pub mod path
{
    //! Cross-platform path manipulation.
    pub use std::path::{ * };
}

pub mod primitive
{
    pub use std::prelude::v1::{ * };
    pub use std::primitive::{ * };

    use ::
    {
        borrow::{ Borrow },
        collections::{HashMap, HashSet},
        convert::{ TryFrom, TryInto },
        error::uuid::{ Error as UuidError, ErrorKind as UuidKind },
        fmt::{ LowerHex },
        hash::{ Hasher },
        ptr::{ self },
        regex::{ contains, Regex },
        str::{ FromStr, from_utf8_mut_unchecked },
        time::{ stamp as timestamp },
    };
    /*
    use crate::parsers;
    use crate::parsers::parser_line::tokens_to_redirections;
    use crate::shell;
    use crate::libs;
    use crate::tools;
    */
    pub type Token = (String, String);
    pub type Tokens = Vec<Token>;
    pub type Redirection = (String, String, String);

    macro_rules! impl_fmt_traits 
    {
        ($($T:ident<$($a:lifetime),*>),+) => {$(
            impl<$($a),*> ::fmt::Display for $T<$($a),*> {
                #[inline] fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result {
                    ::fmt::LowerHex::fmt(self, f)
                }
            }

            impl<$($a),*> ::fmt::LowerHex for $T<$($a),*> {
                fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result {
                    f.write_str(self.encode_lower(&mut [0; Self::LENGTH]))
                }
            }

            impl<$($a),*> ::fmt::UpperHex for $T<$($a),*> {
                fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result {
                    f.write_str(self.encode_upper(&mut [0; Self::LENGTH]))
                }
            }

            impl_fmt_from!($T<$($a),*>);
        )+}
    }

    macro_rules! impl_fmt_from 
    {
        ($T:ident<>) => {
            impl From<Uuid> for $T {
                #[inline] fn from(f: Uuid) -> Self {
                    $T(f)
                }
            }

            impl From<$T> for Uuid {
                #[inline] fn from(f: $T) -> Self {
                    f.into_uuid()
                }
            }

            impl AsRef<Uuid> for $T {
                #[inline] fn as_ref(&self) -> &Uuid {
                    &self.0
                }
            }

            impl Borrow<Uuid> for $T {
                #[inline] fn borrow(&self) -> &Uuid {
                    &self.0
                }
            }
        };
        ($T:ident<$a:lifetime>) => {
            impl<$a> From<&$a Uuid> for $T<$a> {
                #[inline] fn from(f: &$a Uuid) -> Self {
                    $T::from_uuid_ref(f)
                }
            }

            impl<$a> From<$T<$a>> for &$a Uuid {
                #[inline] fn from(f: $T<$a>) -> &$a Uuid {
                    f.0
                }
            }

            impl<$a> AsRef<Uuid> for $T<$a> {
                #[inline] fn as_ref(&self) -> &Uuid {
                    self.0
                }
            }

            impl<$a> Borrow<Uuid> for $T<$a> {
                #[inline] fn borrow(&self) -> &Uuid {
                    self.0
                }
            }
        };
    }

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub struct WaitStatus(i32, i32, i32);

    impl WaitStatus 
    {
        pub fn from_exited(pid: i32, status: i32) -> Self { WaitStatus(pid, 0, status) }
        pub fn from_signaled(pid: i32, sig: i32) -> Self { WaitStatus(pid, 1, sig) }
        pub fn from_stopped(pid: i32, sig: i32) -> Self { WaitStatus(pid, 2, sig) }
        pub fn from_continuted(pid: i32) -> Self { WaitStatus(pid, 3, 0) }
        pub fn from_others() -> Self { WaitStatus(0, 9, 9) }
        pub fn from_error(errno: i32) -> Self { WaitStatus(0, 255, errno) }
        pub fn empty() -> Self { WaitStatus(0, 0, 0) }
        pub fn is_error(&self) -> bool { self.1 == 255 }
        pub fn is_others(&self) -> bool { self.1 == 9 }
        pub fn is_signaled(&self) -> bool { self.1 == 1 }
        pub fn get_errno(&self) -> nix::Error { nix::Error::from_raw(self.2) }
        pub fn is_exited(&self) -> bool { self.0 != 0 && self.1 == 0 }
        pub fn is_stopped(&self) -> bool { self.1 == 2 }
        pub fn is_continued(&self) -> bool { self.1 == 3 }
        pub fn get_pid(&self) -> i32 { self.0 }
        fn _get_signaled_status(&self) -> i32 { self.2 + 128 }
        pub fn get_signal(&self) -> i32 { self.2 }
        pub fn get_name(&self) -> String
        {
            match true
            {
                true if self.is_exited()    => { return "Exited".to_string(); }
                true if self.is_stopped()   => { return "Stopped".to_string(); }
                true if self.is_continued() => { return "Continued".to_string(); }
                true if self.is_signaled()  => { return "Signaled".to_string(); }
                true if self.is_others()    => { return "Others".to_string(); }
                true if self.is_error()     => { return "Error".to_string(); }
                unknown                     => { return format!("unknown: {}", self.2); }
            }
        }

        pub fn get_status(&self) -> i32
        {
            if self.is_exited() { self.2 } 
            else { self._get_signaled_status() }
        }
    }

    impl ::fmt::Debug for WaitStatus
    {
        fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result
        {
            let mut formatter = f.debug_struct("WaitStatus");
            formatter.field("pid", &self.0);
            let name = self.get_name();
            formatter.field("name", &name);
            formatter.field("ext", &self.2);
            formatter.finish()
        }
    }
    
    #[derive(Debug)]
    pub struct LineInfo
    {
        pub tokens: Tokens,
        pub is_complete: bool,
    }

    impl LineInfo
    {
        pub fn new(tokens: Tokens) -> Self
        {
            LineInfo { tokens, is_complete: true }
        }
    }
    /**
    command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    ...
    Command 
    {
        tokens: [("", "ls"), ("", "-G"), ("\'", "foo bar")],
        redirects_to:
        [
            ("2", ">", "&1"),
            ("1", ">", "/dev/null"),
        ],
        redirect_from: Some(("<", "one-file")),
    }
    */
    #[derive(Debug)]
    pub struct Command
    {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }

    impl Command
    {
        /*
        pub fn from_tokens( tokens: Tokens ) -> Result<Command, String>
        {
            let mut tokens_new = tokens.clone();
            let mut redirects_from_type = String::new();
            let mut redirects_from_value = String::new();
            let mut has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");

            let mut len = tokens_new.len();
            while has_redirect_from
            {
                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<")
                {
                    redirects_from_type = "<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    
                    if len > idx
                    {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }

                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<<<")
                {
                    redirects_from_type = "<<<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    
                    if len > idx
                    {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }

                has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");
            }

            let tokens_final;
            let redirects_to;
            match tokens_to_redirections(&tokens_new)
            {
                Ok((_tokens, _redirects_to)) =>
                {
                    tokens_final = _tokens;
                    redirects_to = _redirects_to;
                }

                Err(e) => { return Err(e); }
            }

            let redirect_from = if redirects_from_type.is_empty() { None }

            else { Some((redirects_from_type, redirects_from_value)) };

            Ok(Command
            {
                tokens: tokens_final,
                redirects_to,
                redirect_from,
            })
        }
        */

        pub fn has_redirect_from(&self) -> bool
        {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<"
        }

        pub fn has_here_string(&self) -> bool
        {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<<<"
        }

        pub fn is_builtin(&self) -> bool { ::is::builtin(&self.tokens[0].1) }
    }

    #[derive(Debug)]
    pub struct CommandLine
    {
        pub line: String,
        pub commands: Vec<Command>,
        pub envs: HashMap<String, String>,
        pub background: bool,
    }

    impl CommandLine
    {
        /*
        pub fn from_line( line: &str, sh: &mut shell::Shell ) -> Result<CommandLine, String>
        {
            let linfo = parsers::line::parse( line );
            let mut tokens = linfo.tokens;
            shell::do_expansion(sh, &mut tokens);
            let envs = drain_env_tokens(&mut tokens);
            let mut background = false;
            let len = tokens.len();
            
            if len > 1 && tokens[len - 1].1 == "&"
            {
                background = true;
                tokens.pop();
            }

            let mut commands = Vec::new();
            for sub_tokens in split_tokens_by_pipes(&tokens)
            {
                match Command::from_tokens(sub_tokens)
                {
                    Ok(c) => { commands.push(c); }
                    Err(e) => { return Err(e); }
                }
            }

            Ok(CommandLine
            {
                line: line.to_string(),
                commands,
                envs,
                background,
            })
        }
        */
        pub fn from_line( line: &str, sh: &mut ::shell::Shell ) -> Result<CommandLine, String>
        {
            Ok( CommandLine
            {
                line: String::new(),
                commands: Vec::new(),
                envs: HashMap::new(),
                background:false,
            })
        }

        pub fn is_empty(&self) -> bool { self.commands.is_empty() }

        pub fn with_pipeline(&self) -> bool { self.commands.len() > 1 }

        pub fn is_single_and_builtin(&self) -> bool { self.commands.len() == 1 && self.commands[0].is_builtin() }
    }
    
    #[derive(Debug, Clone, Default)]
    pub struct Job
    {
        pub cmd: String,
        pub id: i32,
        pub gid: i32,
        pub pids: Vec<i32>,
        pub pids_stopped: HashSet<i32>,
        pub status: String,
        pub is_bg: bool,
    }

    impl Job
    {
        pub fn all_members_stopped(&self) -> bool
        {
            for pid in &self.pids
            {
                if !self.pids_stopped.contains(pid) { return false; }
            }
            
            true
        }

        pub fn all_members_running(&self) -> bool { self.pids_stopped.is_empty() }
    }

    #[derive(Clone, Debug, Default)]
    pub struct CommandResult
    {
        pub gid: i32,
        pub status: i32,
        pub stdout: String,
        pub stderr: String,
    }

    impl CommandResult
    {
        pub fn new() -> CommandResult
        {
            CommandResult
            {
                gid: 0,
                status: 0,
                stdout: String::new(),
                stderr: String::new(),
            }
        }

        pub fn from_status(gid: i32, status: i32) -> CommandResult
        {
            CommandResult
            {
                gid,
                status,
                stdout: String::new(),
                stderr: String::new(),
            }
        }

        pub fn error() -> CommandResult
        {
            CommandResult
            {
                gid: 0,
                status: 1,
                stdout: String::new(),
                stderr: String::new(),
            }
        }
    }
    
    #[derive( Clone, Debug, Default )]
    pub struct CommandOptions
    {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }

    pub fn split_tokens_by_pipes( tokens: &[Token] ) -> Vec<Tokens>
    {
        let mut cmd = Vec::new();
        let mut cmds = Vec::new();
        
        for token in tokens
        {
            let sep = &token.0;
            let value = &token.1;
            
            if sep.is_empty() && value == "|"
            {
                if cmd.is_empty() { return Vec::new(); }
                cmds.push( cmd.clone() );
                cmd = Vec::new();
            } else { cmd.push(token.clone()); }
        }
        
        if cmd.is_empty() { return Vec::new(); }
        
        cmds.push(cmd.clone());
        cmds
    }
    /*
    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String>
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") { break; }

            for cap in re.captures_iter(text)
            {
                let name = cap[1].to_string();
                let value = parsers::line::unquote(&cap[2]);
                envs.insert(name, value);
            }

            n += 1;
        }

        if n > 0 { tokens.drain(0..n); }
        
        envs
    }
    */
    /// A 128-bit (16 byte) buffer containing the UUID.
    pub type Bytes = [ u8; 16 ];
    /// The version of the UUID, denoting the generating algorithm.
    #[repr(u8)] #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Version
    {
        /// The "nil" (all zeros) UUID.
        Nil = 0u8,
        /// Version 1: Timestamp and node ID.
        Mac = 1,
        /// Version 2: DCE Security.
        Dce = 2,
        /// Version 3: MD5 hash.
        Md5 = 3,
        /// Version 4: Random.
        Random = 4,
        /// Version 5: SHA-1 hash.
        Sha1 = 5,
        /// Version 6: Sortable Timestamp and node ID.
        SortMac = 6,
        /// Version 7: Timestamp and random.
        SortRand = 7,
        /// Version 8: Custom.
        Custom = 8,
        /// The "max" (all ones) UUID.
        Max = 0xff,
    }
    /// The reserved variants of UUIDs.
    #[repr(u8)] #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Variant
    {
        /// Reserved by the NCS for backward compatibility.
        NCS = 0u8,
        /// As described in the RFC 9562 Specification (default).
        /// (for backward compatibility it is not yet renamed)
        RFC4122,
        /// Reserved by Microsoft for backward compatibility.
        Microsoft,
        /// Reserved for future expansion.
        Future,
    }
    /// A Universally Unique Identifier (UUID).
    #[repr( transparent )] #[derive( Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd )]
    pub struct Uuid( pub Bytes );

    impl Uuid
    {
        pub const UPPER: [u8; 16] = 
        [ b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F' ];
        pub const LOWER: [u8; 16] = 
        [ b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f' ];
        /// UUID namespace for Domain Name System (DNS).
        pub const NAMESPACE_DNS: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8, ]);
        /// UUID namespace for ISO Object Identifiers (OIDs).
        pub const NAMESPACE_OID: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        /// UUID namespace for Uniform Resource Locators (URLs).
        pub const NAMESPACE_URL: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        /// UUID namespace for X.500 Distinguished Names (DNs).
        pub const NAMESPACE_X500: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        
        /// The 'nil UUID' (all zeros).
        pub const fn nil() -> Self
        {
            Uuid::from_bytes([0; 16])
        }
        /// Returns the variant of the UUID structure.
        pub const fn get_variant(&self) -> Variant
        {
            match self.as_bytes()[8]
            {
                x if x & 0x80 == 0x00 => Variant::NCS,
                x if x & 0xc0 == 0x80 => Variant::RFC4122,
                x if x & 0xe0 == 0xc0 => Variant::Microsoft,
                x if x & 0xe0 == 0xe0 => Variant::Future,
                _ => Variant::Future,
            }
        }
        /// Returns the version number of the UUID.
        pub const fn get_version_num(&self) -> usize { (self.as_bytes()[6] >> 4) as usize }
        /// Returns the version of the UUID.
        pub const fn get_version(&self) -> Option<Version>
        {
            match self.get_version_num()
            {
                0 if self.is_nil() => Some(Version::Nil),
                1 => Some(Version::Mac),
                2 => Some(Version::Dce),
                3 => Some(Version::Md5),
                4 => Some(Version::Random),
                5 => Some(Version::Sha1),
                6 => Some(Version::SortMac),
                7 => Some(Version::SortRand),
                8 => Some(Version::Custom),
                0xf => Some(Version::Max),
                _ => None,
            }
        }
        /// Returns the four field values of the UUID.
        pub fn as_fields(&self) -> (u32, u16, u16, &[u8; 8])
        {
            let bytes = self.as_bytes();
            let d1 = (bytes[0] as u32) << 24
            | (bytes[1] as u32) << 16
            | (bytes[2] as u32) << 8
            | (bytes[3] as u32);

            let d2 = (bytes[4] as u16) << 8 | (bytes[5] as u16);
            let d3 = (bytes[6] as u16) << 8 | (bytes[7] as u16);
            let d4: &[u8; 8] = TryInto::try_into(&bytes[8..16]).unwrap();
            (d1, d2, d3, d4)
        }
        /// Returns the four field values of the UUID in little-endian order.
        pub fn to_fields_le(&self) -> (u32, u16, u16, &[u8; 8])
        {
            let d1 = (self.as_bytes()[0] as u32)
            | (self.as_bytes()[1] as u32) << 8
            | (self.as_bytes()[2] as u32) << 16
            | (self.as_bytes()[3] as u32) << 24;
            let d2 = (self.as_bytes()[4] as u16) | (self.as_bytes()[5] as u16) << 8;
            let d3 = (self.as_bytes()[6] as u16) | (self.as_bytes()[7] as u16) << 8;
            let d4: &[u8; 8] = TryInto::try_into(&self.as_bytes()[8..16]).unwrap();
            (d1, d2, d3, d4)
        }
        /// Returns a 128bit value containing the value.
        pub const fn as_u128(&self) -> u128 { u128::from_be_bytes(*self.as_bytes()) }
        /// Returns a 128bit little-endian value containing the value.
        pub const fn to_u128_le(&self) -> u128 { u128::from_le_bytes(*self.as_bytes()) }
        /// Returns two 64bit values containing the value.
        pub const fn as_u64_pair(&self) -> (u64, u64)
        {
            let value = self.as_u128();
            ((value >> 64) as u64, value as u64)
        }
        /// Returns a slice of 16 octets containing the value.
        #[inline] pub const fn as_bytes(&self) -> &Bytes { &self.0 }
        /// Consumes self and returns the underlying byte value of the UUID.
        #[inline] pub const fn into_bytes(self) -> Bytes { self.0 }
        /// Returns the bytes of the UUID in little-endian order.
        pub const fn to_bytes_le(&self) -> Bytes
        {
            [
                self.0[3], self.0[2], self.0[1], self.0[0], self.0[5], self.0[4], self.0[7], self.0[6],
                self.0[8], self.0[9], self.0[10], self.0[11], self.0[12], self.0[13], self.0[14], self.0[15],
            ]
        }
        /// Tests if the UUID is nil (all zeros).
        pub const fn is_nil(&self) -> bool { self.as_u128() == u128::MIN }
        /// Tests if the UUID is max (all ones).
        pub const fn is_max(&self) -> bool { self.as_u128() == u128::MAX }
        /// A buffer that can be used for `encode_...` calls, 
        /// that is guaranteed to be long enough for any of the format adapters.
        pub const fn encode_buffer() -> [u8; Urn::LENGTH] { [0; Urn::LENGTH] }
        /// Return the timestamp in a version-agnostic [`Timestamp`].
        pub const fn get_timestamp(&self) -> Option<timestamp::Timestamp>
        {
            match self.get_version()
            {
                Some(Version::Mac) =>
                {
                    let (ticks, counter) = timestamp::decode_gregorian_timestamp(self);
                    Some( timestamp::Timestamp::from_gregorian(ticks, counter))
                }
                Some(Version::SortMac) =>
                {
                    let (ticks, counter) = timestamp::decode_sorted_gregorian_timestamp(self);
                    Some( timestamp::Timestamp::from_gregorian(ticks, counter) )
                }
                Some(Version::SortRand) =>
                {
                    let millis = timestamp::decode_unix_timestamp_millis(self);
                    let seconds = millis / 1000;
                    let nanos = ((millis % 1000) * 1_000_000) as u32;
                    Some(timestamp::Timestamp::from_unix_time(seconds, nanos, 0, 0))
                }
                _ => None,
            }
        }
        /// If the UUID is the correct version (v1, or v6) this will return the node value as a 6-byte array.
        pub const fn get_node_id(&self) -> Option<[u8; 6]>
        {
            match self.get_version()
            {
                Some(Version::Mac) | Some(Version::SortMac) =>
                {
                    let mut node_id = [0; 6];
                    node_id[0] = self.0[10];
                    node_id[1] = self.0[11];
                    node_id[2] = self.0[12];
                    node_id[3] = self.0[13];
                    node_id[4] = self.0[14];
                    node_id[5] = self.0[15];
                    Some(node_id)
                }
                _ => None,
            }
        }        
        /// Get a [`Hyphenated`] formatter.
        #[inline] pub const fn hyphenated(self) -> Hyphenated { Hyphenated(self) }
        /// Get a borrowed [`Hyphenated`] formatter.
        #[inline] pub fn as_hyphenated(&self) -> &Hyphenated 
        { unsafe { &*(self as *const Uuid as *const Hyphenated) } }
        /// Get a [`Simple`] formatter.
        #[inline] pub const fn simple(self) -> Simple { Simple(self) }
        /// Get a borrowed [`Simple`] formatter.
        #[inline] pub fn as_simple(&self) -> &Simple {  unsafe { &*(self as *const Uuid as *const Simple) } }
        /// Get a [`Urn`] formatter.
        #[inline] pub const fn urn(self) -> Urn { Urn(self) }
        /// Get a borrowed [`Urn`] formatter.
        #[inline] pub fn as_urn(&self) -> &Urn { unsafe { &*(self as *const Uuid as *const Urn) } }
        /// Get a [`Braced`] formatter.
        #[inline] pub const fn braced(self) -> Braced { Braced(self) }
        /// Get a borrowed [`Braced`] formatter.
        #[inline] pub fn as_braced(&self) -> &Braced { unsafe { &*(self as *const Uuid as *const Braced) } }
        #[inline] const fn format_simple(src: &[u8; 16], upper: bool) -> [u8; 32]
        {
            let lut = if upper { &Self::UPPER } else { &Self::LOWER };
            let mut dst = [0; 32];
            let mut i = 0;

            while i < 16
            {
                let x = src[i];
                dst[i * 2] = lut[(x >> 4) as usize];
                dst[i * 2 + 1] = lut[(x & 0x0f) as usize];
                i += 1;
            }

            dst
        }

        #[inline] const fn format_hyphenated(src: &[u8; 16], upper: bool) -> [u8; 36]
        {
            let lut = if upper { &Self::UPPER } else { &Self::LOWER };
            let groups = [(0, 8), (9, 13), (14, 18), (19, 23), (24, 36)];
            let mut dst = [0; 36];
            let mut group_idx = 0;
            let mut i = 0;

            while group_idx < 5
            {
                let (start, end) = groups[group_idx];
                let mut j = start;
            
                while j < end
                {
                    let x = src[i];
                    i += 1;
                    dst[j] = lut[(x >> 4) as usize];
                    dst[j + 1] = lut[(x & 0x0f) as usize];
                    j += 2;
                }

                if group_idx < 4 { dst[end] = b'-'; }

                group_idx += 1;
            }

            dst
        }

        #[inline] fn encode_simple<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            let buf = &mut buffer[..Simple::LENGTH];
            let dst = buf.as_mut_ptr();
            unsafe
            {
                ptr::write(dst.cast(), format_simple(src, upper));
                from_utf8_mut_unchecked(buf)
            }
        }

        #[inline] fn encode_hyphenated<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            let buf = &mut buffer[..Hyphenated::LENGTH];
            let dst = buf.as_mut_ptr();
            unsafe
            {
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                from_utf8_mut_unchecked(buf)
            }
        }

        #[inline] fn encode_braced<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            let buf = &mut buffer[..Braced::LENGTH];
            buf[0] = b'{';
            buf[Braced::LENGTH - 1] = b'}';
            unsafe
            {
                let dst = buf.as_mut_ptr().add(1);

                ptr::write(dst.cast(), format_hyphenated(src, upper));
                from_utf8_mut_unchecked(buf)
            }
        }

        #[inline] fn encode_urn<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            let buf = &mut buffer[..Urn::LENGTH];
            buf[..9].copy_from_slice(b"urn:uuid:");
            unsafe
            {
                let dst = buf.as_mut_ptr().add(9);
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                from_utf8_mut_unchecked(buf)
            }
        }

        #[inline] pub const fn from_bytes(bytes: Bytes) -> Uuid { Uuid(bytes) }

        pub const fn from_fields(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Uuid
        {
            Uuid::from_bytes
            (
            [
                (d1 >> 24) as u8,
                (d1 >> 16) as u8,
                (d1 >> 8) as u8,
                d1 as u8,
                (d2 >> 8) as u8,
                d2 as u8,
                (d3 >> 8) as u8,
                d3 as u8,
                d4[0],
                d4[1],
                d4[2],
                d4[3],
                d4[4],
                d4[5],
                d4[6],
                d4[7],
            ])
        }
        pub const fn from_u128(v: u128) -> Self { Uuid::from_bytes(v.to_be_bytes()) }

        pub fn new_v4() -> Uuid 
        {
            Uuid::from_u128( ::random::random::<u128>() & 0xFFFFFFFFFFFF4FFFBFFFFFFFFFFFFFFF | 0x40008000000000000000 )
        }

        pub fn from_slice(b: &[u8]) -> Result<Uuid, UuidError>
        {
            if b.len() != 16 { return Err(UuidError(UuidKind::ByteLength { len: b.len() })); }
            let mut bytes: Bytes = [0; 16];
            bytes.copy_from_slice(b);
            Ok(Uuid::from_bytes(bytes))
        }
    }

    impl Default for Uuid
    {
        #[inline] fn default() -> Self { Uuid::nil() }
    }

    impl AsRef<Uuid> for Uuid
    {
        #[inline] fn as_ref(&self) -> &Uuid { self }
    }

    impl AsRef<[u8]> for Uuid
    {
        #[inline] fn as_ref(&self) -> &[u8] { &self.0 }
    }
    
    impl From<Uuid> for Vec<u8>
    {
        fn from(value: Uuid) -> Self { value.0.to_vec() }
    }
    
    impl TryFrom<Vec<u8>> for Uuid
    {
        type Error = UuidError;
        fn try_from( value:Vec<u8> ) -> Result<Self, Self::Error> { Uuid::from_slice(&value) }
    }
    
    #[inline] pub const fn format_simple(src: &[u8; 16], upper: bool) -> [u8; 32] 
    {
        let lut = if upper { &UPPER } else { &LOWER };
        let mut dst = [0; 32];
        let mut i = 0;

        while i < 16 
        {
            let x = src[i];
            dst[i * 2] = lut[(x >> 4) as usize];
            dst[i * 2 + 1] = lut[(x & 0x0f) as usize];
            i += 1;
        }

        dst
    }

    const UPPER: [u8; 16] = 
    [
        b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
    ];
    const LOWER: [u8; 16] = 
    [
        b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f',
    ];

    #[inline] pub const fn format_hyphenated(src: &[u8; 16], upper: bool) -> [u8; 36] 
    {
        let lut = if upper { &UPPER } else { &LOWER };
        let groups = [(0, 8), (9, 13), (14, 18), (19, 23), (24, 36)];
        let mut dst = [0; 36];
        let mut group_idx = 0;
        let mut i = 0;

        while group_idx < 5 
        {
            let (start, end) = groups[group_idx];
            let mut j = start;

            while j < end 
            {
                let x = src[i];
                i += 1;

                dst[j] = lut[(x >> 4) as usize];
                dst[j + 1] = lut[(x & 0x0f) as usize];
                j += 2;
            }

            if group_idx < 4 
            {
                dst[end] = b'-';
            }

            group_idx += 1;
        }

        dst
    }

    #[inline] pub fn encode_simple<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str 
    {
        let buf = &mut buffer[..Simple::LENGTH];
        let dst = buf.as_mut_ptr();
        unsafe 
        {
            ptr::write(dst.cast(), format_simple(src, upper));
            from_utf8_mut_unchecked(buf)
        }
    }

    #[inline] pub fn encode_hyphenated<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str 
    {
        let buf = &mut buffer[..Hyphenated::LENGTH];
        let dst = buf.as_mut_ptr();
        unsafe 
        {
            ptr::write(dst.cast(), format_hyphenated(src, upper));
            from_utf8_mut_unchecked(buf)
        }
    }

    #[inline] pub fn encode_braced<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str 
    {
        let buf = &mut buffer[..Braced::LENGTH];
        buf[0] = b'{';
        buf[Braced::LENGTH - 1] = b'}';        
        unsafe 
        {
            let dst = buf.as_mut_ptr().add(1);
            ptr::write(dst.cast(), format_hyphenated(src, upper));
            from_utf8_mut_unchecked(buf)
        }
    }

    #[inline] pub fn encode_urn<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
    {
        let buf = &mut buffer[..Urn::LENGTH];
        buf[..9].copy_from_slice(b"urn:uuid:");
        
        unsafe 
        {
            let dst = buf.as_mut_ptr().add(9);
            ptr::write(dst.cast(), format_hyphenated(src, upper));
            from_utf8_mut_unchecked(buf)
        }
    }
    
    #[repr(transparent)] #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub struct Hyphenated( pub Uuid );

    impl Hyphenated 
    {
        /// The length of a hyphenated [`Uuid`] string.
        pub const LENGTH: usize = 36;
        /// Creates a [`Hyphenated`] from a [`Uuid`].
        pub const fn from_uuid(uuid: Uuid) -> Self 
        {
            Hyphenated(uuid)
        }
        /// Writes the [`Uuid`] as a lower-case hyphenated string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_hyphenated(self.0.as_bytes(), buffer, false) }
        /// Writes the [`Uuid`] as an upper-case hyphenated string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str 
        { encode_hyphenated(self.0.as_bytes(), buffer, true) }
        /// Get a reference to the underlying [`Uuid`].
        pub const fn as_uuid(&self) -> &Uuid { &self.0 }
        /// Consumes the [`Hyphenated`], returning the underlying [`Uuid`].
        pub const fn into_uuid(self) -> Uuid { self.0 }
    }

    impl FromStr for Hyphenated
    {
        type Err = UuidError;

        fn from_str(s: &str) -> Result<Self, Self::Err>
        {
            crate::parsers::uuid::hyphenated(s.as_bytes())
            .map(|b| Hyphenated(Uuid(b)))
            .map_err(|invalid| invalid.into_err())
        }
    }
    /// Format a [`Uuid`] as a simple string, like `67e5504410b1426f9247bb680e5fe0c8`.
    #[repr(transparent)] #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]    
    pub struct Simple( pub Uuid );

    impl Simple
    {
        /// The length of a simple [`Uuid`] string.
        pub const LENGTH: usize = 32;
        /// Creates a [`Simple`] from a [`Uuid`].
        pub const fn from_uuid(uuid: Uuid) -> Self
        {
            Simple(uuid)
        }
        /// Writes the [`Uuid`] as a lower-case simple string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_simple(self.0.as_bytes(), buffer, false) }
        /// Writes the [`Uuid`] as an upper-case simple string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_simple(self.0.as_bytes(), buffer, true) }
        /// Get a reference to the underlying [`Uuid`].
        pub const fn as_uuid(&self) -> &Uuid { &self.0 }
        /// Consumes the [`Simple`], returning the underlying [`Uuid`].
        pub const fn into_uuid(self) -> Uuid { self.0 }
    }

    impl FromStr for Simple
    {
        type Err = UuidError;

        fn from_str(s: &str) -> Result<Self, Self::Err> 
        {
            crate::parsers::uuid::simple(s.as_bytes())
            .map(|b| Simple(Uuid(b)))
            .map_err(|invalid| invalid.into_err())
        }
    }
    /// Format a [`Uuid`] as a URN string, like `urn:uuid:67e55044-10b1-426f-9247-bb680e5fe0c8`.
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Urn( pub Uuid );

    impl Urn 
    {
        /// The length of a URN [`Uuid`] string.
        pub const LENGTH: usize = 45;
        /// Creates a [`Urn`] from a [`Uuid`].
        pub const fn from_uuid(uuid: Uuid) -> Self
        {
            Urn(uuid)
        }
        /// Writes the [`Uuid`] as a lower-case URN string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_urn(self.0.as_bytes(), buffer, false) }
        /// Writes the [`Uuid`] as an upper-case URN string to `buffer`, 
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_urn(self.0.as_bytes(), buffer, true) }
        /// Get a reference to the underlying [`Uuid`].
        pub const fn as_uuid(&self) -> &Uuid { &self.0 }
        /// Consumes the [`Urn`], returning the underlying [`Uuid`].
        pub const fn into_uuid(self) -> Uuid { self.0 }
    }

    impl FromStr for Urn 
    {
        type Err = UuidError;

        fn from_str(s: &str) -> Result<Self, Self::Err>
        {
            crate::parsers::uuid::urn(s.as_bytes())
            .map(|b| Urn(Uuid(b)))
            .map_err(|invalid| invalid.into_err())
        }
    }
    /// Format a [`Uuid`] as a braced hyphenated string, like `{67e55044-10b1-426f-9247-bb680e5fe0c8}`.
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Braced( pub Uuid );

    impl Braced 
    {
        /// The length of a braced [`Uuid`] string.
        pub const LENGTH: usize = 38;
        /// Creates a [`Braced`] from a [`Uuid`].
        pub const fn from_uuid(uuid: Uuid) -> Self
        {
            Braced(uuid)
        }
        /// Writes the [`Uuid`] as a lower-case hyphenated string surrounded by braces to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
        { encode_braced(self.0.as_bytes(), buffer, false) }
        /// Writes the [`Uuid`] as an upper-case hyphenated string surrounded by braces to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        #[inline] pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str 
        { encode_braced(self.0.as_bytes(), buffer, true) }
        /// Get a reference to the underlying [`Uuid`].
        pub const fn as_uuid(&self) -> &Uuid { &self.0 }
        /// Consumes the [`Braced`], returning the underlying [`Uuid`].
        pub const fn into_uuid(self) -> Uuid { self.0 }
    }

    impl FromStr for Braced
    {
        type Err = UuidError;

        fn from_str(s: &str) -> Result<Self, Self::Err>
        {
            crate::parsers::uuid::braced(s.as_bytes())
            .map(|b| Braced(Uuid(b)))
            .map_err(|invalid| invalid.into_err())
        }
    }

    impl_fmt_traits! 
    {
        Hyphenated<>,
        Simple<>,
        Urn<>,
        Braced<>
    }
}

pub mod process
{
    pub use std::process::{ * };

    use ::
    {
        primitive::{ CommandLine, CommandResult },
        *,
    };

    pub fn getpid() -> i32 { unsafe { libc::getpid() } }
    /// Run a pipeline (e.g. `echo hi | wc -l`)
    /// returns: (is-terminal-given, command-result)
    pub fn run_pipeline(
        sh: &mut ::shell::Shell,
        cl: &CommandLine,
        tty: bool,
        capture: bool,
        log_cmd: bool,
    ) -> (bool, CommandResult)
    {
        /*
        let mut term_given = false;
        if cl.background && capture {
            println_stderr!("cicada: cannot capture output of background cmd");
            return (term_given, CommandResult::error());
        }

        if let Some(cr) = try_run_calculator(&cl.line, capture) {
            return (term_given, cr);
        }

        // FIXME: move func-run into run single command
        if let Some(cr) = try_run_func(sh, cl, capture, log_cmd) {
            return (term_given, cr);
        }

        if log_cmd {
            log!("run: {}", cl.line);
        }

        let length = cl.commands.len();
        if length == 0 {
            println!("cicada: invalid command: cmds with empty length");
            return (false, CommandResult::error());
        }

        let mut pipes = Vec::new();
        let mut errored_pipes = false;
        for _ in 0..length - 1 {
            match pipe() {
                Ok(fds) => pipes.push(fds),
                Err(e) => {
                    errored_pipes = true;
                    println_stderr!("cicada: pipeline1: {}", e);
                    break;
                }
            }
        }

        if errored_pipes {
            // release fds that already created when errors occurred
            for fds in pipes {
                libs::close(fds.0);
                libs::close(fds.1);
            }
            return (false, CommandResult::error());
        }

        if pipes.len() + 1 != length {
            println!("cicada: invalid command: unmatched pipes count");
            return (false, CommandResult::error());
        }

        let mut pgid: i32 = 0;
        let mut fg_pids: Vec<i32> = Vec::new();

        let isatty = if tty {
            unsafe { libc::isatty(1) == 1 }
        } else {
            false
        };
        let options = CommandOptions {
            isatty,
            capture_output: capture,
            background: cl.background,
            envs: cl.envs.clone(),
        };

        let mut fds_capture_stdout = None;
        let mut fds_capture_stderr = None;
        if capture {
            match pipe() {
                Ok(fds) => fds_capture_stdout = Some(fds),
                Err(e) => {
                    println_stderr!("cicada: pipeline2: {}", e);
                    return (false, CommandResult::error());
                }
            }
            match pipe() {
                Ok(fds) => fds_capture_stderr = Some(fds),
                Err(e) => {
                    if let Some(fds) = fds_capture_stdout {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                    println_stderr!("cicada: pipeline3: {}", e);
                    return (false, CommandResult::error());
                }
            }
        }

        let mut cmd_result = CommandResult::new();
        for i in 0..length {
            let child_id: i32 = run_single_program(
                sh,
                cl,
                i,
                &options,
                &mut pgid,
                &mut term_given,
                &mut cmd_result,
                &pipes,
                &fds_capture_stdout,
                &fds_capture_stderr,
            );

            if child_id > 0 && !cl.background {
                fg_pids.push(child_id);
            }
        }

        if cl.is_single_and_builtin() {
            return (false, cmd_result);
        }

        if cl.background {
            if let Some(job) = sh.get_job_by_gid(pgid) {
                println_stderr!("[{}] {}", job.id, job.gid);
            }
        }

        if !fg_pids.is_empty() {
            let _cr = jobc::wait_fg_job(sh, pgid, &fg_pids);
            // for capture commands, e.g. `echo foo` in `echo "hello $(echo foo)"
            // the cmd_result is already built in loop calling run_single_program()
            // above.
            if !capture {
                cmd_result = _cr;
            }
        }

        (term_given, cmd_result)
        */
        ( false, CommandResult::new() )
    }

}

pub mod ptr
{
    pub use std::ptr::{ * };
}

pub mod random
{
    pub use ::rand::{ * };
    use ::
    {
        *,
    };
}

pub mod regex
{
    //! Regular Expression Implementation
    pub use ::re::{ * };

    pub fn find_first_group(ptn: &str, text: &str) -> Option<String>
    {
        let re = match Regex::new(ptn)
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

    pub fn contains(text: &str, ptn: &str) -> bool
    {
        let re = match Regex::new(ptn)
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
        let re = Regex::new(ptn).unwrap();
        let result = re.replace_all(text, ptn_to);
        result.to_string()
    }
}

pub mod result
{
    pub use std::result::{ * };
}

pub mod scripts
{
    
    use ::
    {
        fs::File,
        io::{ ErrorKind, Read as _ },
        path::{ Path },
        regex::{ Regex, RegexBuilder },
        *,
    };
    /*
    pub fn run_script(sh: &mut shell::Shell, args: &Vec<String>) -> i32 */
    pub fn run(sh: &mut shell::Shell, args: &Vec<String>) -> i32
    {
        let src_file = &args[1];
        let full_src_file: String;

        if src_file.contains('/') { full_src_file = src_file.clone(); }
        else
        {
            let full_path = env::find_file_in_path(src_file, false);
            if full_path.is_empty()
            {
                if !Path::new(src_file).exists()
                {
                    println_stderr!("cicada: {}: no such file", src_file);
                    return 1;
                }
                
                full_src_file = format!("./{}", src_file);
            }
            else { full_src_file = full_path.clone(); }
        }

        if !Path::new(&full_src_file).exists()
        {
            println_stderr!("cicada: {}: no such file", src_file);
            return 1;
        }

        if Path::new(&full_src_file).is_dir()
        {
            println_stderr!("cicada: {}: is a directory", src_file);
            return 1;
        }

        let mut file;
        
        match File::open(&full_src_file)
        {
            Ok(x) => file = x,
            Err(e) =>
            {
                println_stderr!("cicada: {}: failed to open file - {:?}", &full_src_file, e.kind());
                return 1;
            }
        }

        let mut text = String::new();
        
        match file.read_to_string(&mut text)
        {
            Ok(_) => {}
            Err(e) =>
            {
                match e.kind()
                {
                    ErrorKind::InvalidData =>
                    {
                        println_stderr!("cicada: {}: not a valid script file", &full_src_file);
                    }
                    
                    _ =>
                    {
                        println_stderr!("cicada: {}: error: {:?}", &full_src_file, e);
                    }
                }

                return 1;
            }
        }

        if text.contains("\\\n") {
            let re = RegexBuilder::new(r#"([ \t]*\\\n[ \t]+)|([ \t]+\\\n[ \t]*)"#)
                .multi_line(true).build().unwrap();
            text = re.replace_all(&text, " ").to_string();

            let re = RegexBuilder::new(r#"\\\n"#).multi_line(true).build().unwrap();
            text = re.replace_all(&text, "").to_string();
        }

        let re_func_head = Regex::new(r"^function ([a-zA-Z_-][a-zA-Z0-9_-]*) *(?:\(\))? *\{$").unwrap();
        let re_func_tail = Regex::new(r"^\}$").unwrap();
        let mut text_new = String::new();
        let mut enter_func = false;
        let mut func_name = String::new();
        let mut func_body = String::new();
        for line in text.clone().lines() {
            if re_func_head.is_match(line.trim()) {
                enter_func = true;
                let cap = re_func_head.captures(line.trim()).unwrap();
                func_name = cap[1].to_string();
                func_body = String::new();
                continue;
            }
            if re_func_tail.is_match(line.trim()) {
                sh.set_func(&func_name, &func_body);
                enter_func = false;
                continue;
            }
            if enter_func {
                func_body.push_str(line);
                func_body.push('\n');
            } else {
                text_new.push_str(line);
                text_new.push('\n');
            }
        }

        let mut status = 0;
        let cr_list = lines::run(sh, &text_new, args, false);
        if let Some(last) = cr_list.last() {
            status = last.status;
        }

        // FIXME: We probably need to fix the issue in the `set` builtin,
        // which currently set `exit_on_error` at the shell session level,
        // we should instead set in a script-level.
        // Here is a work-around ugly fix.
        sh.exit_on_error = false;

        status
    }
}

pub mod shell
{
    //! Simple Shell Interface
    use ::
    {
        collections::{ HashMap },
        primitive::{ Uuid },
        *,
    };

    #[derive( Clone, Debug )]
    pub struct Shell
    {
        pub jobs: HashMap<i32, primitive::Job>,
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
        pub seed:usize,
    }

    impl Shell
    {
        pub fn new() -> Shell
        {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = get::current_directory();
            let has_terminal = is::process_terminal();
            let (session_id, _) = uuid.split_at(13);
            Shell
            {
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
                seed:random::random::<usize>(),
            }
        }
        /*
        Update existing *ENV Variable* if such name exists in ENVs. */
        pub fn set_env(&mut self, name: &str, value: &str)
        {
            if env::var(name).is_ok() { env::set_var(name, value); }
            else { self.envs.insert(name.to_string(), value.to_string()); }
        }

        pub fn set_func(&mut self, name: &str, value: &str) { self.funcs.insert(name.to_string(), value.to_string());         }
    }
}

pub mod slice
{
    pub use std::slice::{ * };
}

pub mod str
{
    pub use std::str::{ * };
    /// Converts a slice of bytes to a string slice 
    /// without checking that the string contains valid UTF-8; mutable version.
    #[inline] #[must_use] pub const unsafe fn from_utf8_mut_unchecked(v: &mut [u8]) -> &mut str
    {
        unsafe { &mut *(v as *mut [u8] as *mut str) }
    }
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod sync
{
    pub mod atomic
    {
        pub use std::sync::atomic::{ * };
        
    }

    pub use std::sync::{ atomic as _, * };
}

pub mod system
{
    use ::
    {
        *,
    };

    pub mod common
    {
        //! Platform-independent platform abstraction
        use ::
        {
            *,
        };
        /*
        terminfo v0.9.0*/
    }
    // libstd::sys::pal::<target_os>
    pub mod os
    {
        use ::
        {
            *,
        };

        pub mod uefi
        {
            use ::
            {
                *,
            };
        }
        
        pub mod unix
        {
            use ::
            {
                *,
            };
        }

        pub mod unsupported
        {
            use ::
            {
                *,
            };
        }

        pub mod windows
        {
            use ::
            {
                *,
            };
        }
        
        #[cfg( target_os = "uefi" )] pub use self::uefi::{ * };
        #[cfg( unix )] pub use self::unix::{ * };
        #[cfg( windows )] pub use self::windows::{ * };

    }
    /*
    mortal v0.2.4*/
} pub use self::system::{ self as sys };

pub mod terminal
{
    use ::
    {
        nix::errno::errno,
        *,
    };
    /*
    pub unsafe fn give_terminal_to(...) -> bool */
    pub unsafe fn give_to(gid: i32) -> bool
    {
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
            //let code = e.0;
            log!("error in give_terminal_to() {}: {}", 0, e);
        } else {
            given = true;
        }
        let rcode = libc::pthread_sigmask(libc::SIG_SETMASK, &old_mask, &mut mask);
        if rcode != 0 {
            log!("failed to call pthread_sigmask");
        }
        given
    }
}

pub mod thread
{
    pub use std::thread::{ * };
}

pub mod time
{
    pub use std::time::{ * };
    pub mod c
    {
        pub use ::timed::{ * };
        
        #[derive(Debug, PartialEq, Eq)]
        pub struct DateTime
        {
            odt:OffsetDateTime,
        }

        impl DateTime
        {
            pub fn now() -> Self
            {
                let odt: OffsetDateTime = match OffsetDateTime::now_local()
                {
                    Ok(dt) => dt,
                    Err(_) => OffsetDateTime::now_utc(),
                };

                DateTime { odt }
            }

            pub fn from_timestamp(ts: f64) -> Self
            {
                let dummy_now = Self::now();
                let offset_seconds = dummy_now.odt.offset().whole_minutes() * 60;
                let ts_nano = (ts + offset_seconds as f64) * 1000000000.0;
                let odt: OffsetDateTime = match OffsetDateTime::from_unix_timestamp_nanos(ts_nano as i128)
                {
                    Ok(x) => x,
                    Err(_) => OffsetDateTime::now_utc(),
                };

                DateTime { odt }
            }

            pub fn unix_timestamp(&self) -> f64 { self.odt.unix_timestamp_nanos() as f64 / 1000000000.0 }
        }

        impl ::fmt::Display for DateTime
        {
            fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result
            {
                write!
                (
                    f,
                    "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:03}",
                    self.odt.year(),
                    self.odt.month() as u8,
                    self.odt.day(),
                    self.odt.hour(),
                    self.odt.minute(),
                    self.odt.second(),
                    self.odt.millisecond(),
                )
            }
        }
    }

    pub mod stamp
    {
        //! Generating UUIDs from timestamps.
        use ::
        {
            primitive::{ Uuid },
            *,
        };
        /// The number of 100 nanosecond ticks between the RFC 9562 epoch (`1582-10-15 00:00:00`) 
        /// and the Unix epoch (`1970-01-01 00:00:00`).
        pub const UUID_TICKS_BETWEEN_EPOCHS: u64 = 0x01B2_1DD2_1381_4000;
        /// A timestamp that can be encoded into a UUID.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct Timestamp
        {
            seconds: u64,
            subsec_nanos: u32,
            counter: u128,
            usable_counter_bits: u8,
        }

        impl Timestamp
        {
            /// Get a timestamp representing the current system time and up to a 128-bit counter.
            pub fn now(context: impl ClockSequence<Output = impl Into<u128>>) -> Self
            {
                let (seconds, subsec_nanos) = now();
                let (counter, seconds, subsec_nanos) = context.generate_timestamp_sequence(seconds, subsec_nanos);
                let counter = counter.into();
                let usable_counter_bits = context.usable_bits() as u8;

                Timestamp
                {
                    seconds,
                    subsec_nanos,
                    counter,
                    usable_counter_bits,
                }
            }
            /// Construct a `Timestamp` from the number of 100 nanosecond ticks since 00:00:00.00, 15 October 1582
            /// (the date of Gregorian reform to the Christian calendar) and a 14-bit counter, 
            /// as used in versions 1 and 6 UUIDs.
            pub const fn from_gregorian(ticks: u64, counter: u16) -> Self
            {
                let (seconds, subsec_nanos) = Self::gregorian_to_unix(ticks);

                Timestamp
                {
                    seconds,
                    subsec_nanos,
                    counter: counter as u128,
                    usable_counter_bits: 14,
                }
            }
            /// Construct a `Timestamp` from a Unix timestamp and up to a 128-bit counter, as used in version 7 UUIDs.
            pub const fn from_unix_time
            (
                seconds: u64,
                subsec_nanos: u32,
                counter: u128,
                usable_counter_bits: u8,
            ) -> Self
            {
                Timestamp
                {
                    seconds,
                    subsec_nanos,
                    counter,
                    usable_counter_bits,
                }
            }
            /// Construct a `Timestamp` from a Unix timestamp and up to a 128-bit counter, as used in version 7 UUIDs.
            pub fn from_unix
            (
                context: impl ClockSequence<Output = impl Into<u128>>,
                seconds: u64,
                subsec_nanos: u32,
            ) -> Self
            {
                let (counter, seconds, subsec_nanos) = context.generate_timestamp_sequence(seconds, subsec_nanos);
                let counter = counter.into();
                let usable_counter_bits = context.usable_bits() as u8;

                Timestamp
                {
                    seconds,
                    subsec_nanos,
                    counter,
                    usable_counter_bits,
                }
            }
            /// Get the value of the timestamp as the number of 100 nanosecond ticks since 00:00:00.00,
            /// 15 October 1582 and a 14-bit counter, as used in versions 1 and 6 UUIDs.
            pub const fn to_gregorian(&self) -> (u64, u16)
            {
                (
                    Self::unix_to_gregorian_ticks(self.seconds, self.subsec_nanos),
                    (self.counter as u16) & 0x3FFF,
                )
            }

            // NOTE: This method is not public; the usable counter bits are lost in a version 7 UUID
            // so can't be reliably recovered.
            #[cfg(feature = "v7")]
            pub const fn counter(&self) -> (u128, u8) {
                (self.counter, self.usable_counter_bits)
            }

            const fn unix_to_gregorian_ticks(seconds: u64, nanos: u32) -> u64
            {
                UUID_TICKS_BETWEEN_EPOCHS
                .wrapping_add(seconds.wrapping_mul(10_000_000))
                .wrapping_add(nanos as u64 / 100)
            }

            const fn gregorian_to_unix(ticks: u64) -> (u64, u32)
            {
                (
                    ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) / 10_000_000,
                    (ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) % 10_000_000) as u32 * 100,
                )
            }
        }
        
        impl Timestamp
        {
            pub const fn from_rfc4122(ticks: u64, counter: u16) -> Self { Timestamp::from_gregorian(ticks, counter) }
            
            pub const fn to_rfc4122(&self) -> (u64, u16) { self.to_gregorian() }
            
            pub const fn to_unix_nanos(&self) -> u32
            {
                panic!("`Timestamp::to_unix_nanos()` is deprecated and will be removed: use `Timestamp::to_unix()`")
            }
        }

        pub const fn encode_gregorian_timestamp
        (
            ticks: u64,
            counter: u16,
            node_id: &[u8; 6],
        ) -> Uuid
        {
            let time_low = (ticks & 0xFFFF_FFFF) as u32;
            let time_mid = ((ticks >> 32) & 0xFFFF) as u16;
            let time_high_and_version = (((ticks >> 48) & 0x0FFF) as u16) | (1 << 12);
            let mut d4 = [0; 8];
            d4[0] = (((counter & 0x3F00) >> 8) as u8) | 0x80;
            d4[1] = (counter & 0xFF) as u8;
            d4[2] = node_id[0];
            d4[3] = node_id[1];
            d4[4] = node_id[2];
            d4[5] = node_id[3];
            d4[6] = node_id[4];
            d4[7] = node_id[5];
            Uuid::from_fields(time_low, time_mid, time_high_and_version, &d4)
        }

        pub const fn decode_gregorian_timestamp(uuid: &Uuid) -> (u64, u16)
        {
            let bytes = uuid.as_bytes();
            let ticks: u64 = ((bytes[6] & 0x0F) as u64) << 56
            | (bytes[7] as u64) << 48
            | (bytes[4] as u64) << 40
            | (bytes[5] as u64) << 32
            | (bytes[0] as u64) << 24
            | (bytes[1] as u64) << 16
            | (bytes[2] as u64) << 8
            | (bytes[3] as u64);
            let counter: u16 = ((bytes[8] & 0x3F) as u16) << 8 | (bytes[9] as u16);
            (ticks, counter)
        }

        pub const fn encode_sorted_gregorian_timestamp
        (
            ticks: u64,
            counter: u16,
            node_id: &[u8; 6],
        ) -> Uuid
        {
            let time_high = ((ticks >> 28) & 0xFFFF_FFFF) as u32;
            let time_mid = ((ticks >> 12) & 0xFFFF) as u16;
            let time_low_and_version = ((ticks & 0x0FFF) as u16) | (0x6 << 12);
            let mut d4 = [0; 8];
            d4[0] = (((counter & 0x3F00) >> 8) as u8) | 0x80;
            d4[1] = (counter & 0xFF) as u8;
            d4[2] = node_id[0];
            d4[3] = node_id[1];
            d4[4] = node_id[2];
            d4[5] = node_id[3];
            d4[6] = node_id[4];
            d4[7] = node_id[5];
            Uuid::from_fields(time_high, time_mid, time_low_and_version, &d4)
        }

        pub const fn decode_sorted_gregorian_timestamp(uuid: &Uuid) -> (u64, u16)
        {
            let bytes = uuid.as_bytes();
            let ticks: u64 = ((bytes[0]) as u64) << 52
            | (bytes[1] as u64) << 44
            | (bytes[2] as u64) << 36
            | (bytes[3] as u64) << 28
            | (bytes[4] as u64) << 20
            | (bytes[5] as u64) << 12
            | ((bytes[6] & 0xF) as u64) << 8
            | (bytes[7] as u64);
            let counter: u16 = ((bytes[8] & 0x3F) as u16) << 8 | (bytes[9] as u16);
            (ticks, counter)
        }

        pub const fn encode_unix_timestamp_millis(
            millis: u64,
            counter_random_bytes: &[u8; 10],
        ) -> Uuid
        {
            let millis_high = ((millis >> 16) & 0xFFFF_FFFF) as u32;
            let millis_low = (millis & 0xFFFF) as u16;
            let counter_random_version = (counter_random_bytes[1] as u16
            | ((counter_random_bytes[0] as u16) << 8) & 0x0FFF)
            | (0x7 << 12);

            let mut d4 = [0; 8];
            d4[0] = (counter_random_bytes[2] & 0x3F) | 0x80;
            d4[1] = counter_random_bytes[3];
            d4[2] = counter_random_bytes[4];
            d4[3] = counter_random_bytes[5];
            d4[4] = counter_random_bytes[6];
            d4[5] = counter_random_bytes[7];
            d4[6] = counter_random_bytes[8];
            d4[7] = counter_random_bytes[9];
            Uuid::from_fields(millis_high, millis_low, counter_random_version, &d4)
        }

        pub const fn decode_unix_timestamp_millis(uuid: &Uuid) -> u64
        {
            let bytes = uuid.as_bytes();
            let millis: u64 = (bytes[0] as u64) << 40
            | (bytes[1] as u64) << 32
            | (bytes[2] as u64) << 24
            | (bytes[3] as u64) << 16
            | (bytes[4] as u64) << 8
            | (bytes[5] as u64);
            millis
        }
        
        fn now() -> (u64, u32)
        {
            let dur = ::time::SystemTime::UNIX_EPOCH.elapsed().expect
            (
                "Getting elapsed time since UNIX_EPOCH. If this fails, we've somehow violated causality",
            );
            (dur.as_secs(), dur.subsec_nanos())
        }
        /// A counter that can be used by versions 1 and 6 UUIDs to support the uniqueness of timestamps.
        pub trait ClockSequence
        {
            /// The type of sequence returned by this counter.
            type Output;
            /// Get the next value in the sequence to feed into a timestamp.
            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output;
            /// Get the next value in the sequence, potentially also adjusting the timestamp.
            fn generate_timestamp_sequence
            (
                &self,
                seconds: u64,
                subsec_nanos: u32,
            ) -> (Self::Output, u64, u32)
            {
                (
                    self.generate_sequence(seconds, subsec_nanos),
                    seconds,
                    subsec_nanos,
                )
            }

            /// The number of usable bits from the least significant bit in the result of [`ClockSequence::generate_sequence`]
            /// or [`ClockSequence::generate_timestamp_sequence`].
            fn usable_bits(&self) -> usize where
            Self::Output: Sized
            { cmp::min(128, core::mem::size_of::<Self::Output>()) }
        }

        impl<'a, T: ClockSequence + ?Sized> ClockSequence for &'a T
        {
            type Output = T::Output;

            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output
            { (**self).generate_sequence(seconds, subsec_nanos) }

            fn generate_timestamp_sequence
            (
                &self,
                seconds: u64,
                subsec_nanos: u32,
            ) -> (Self::Output, u64, u32)
            { (**self).generate_timestamp_sequence(seconds, subsec_nanos) }

            fn usable_bits(&self) -> usize where
            Self::Output: Sized
            { (**self).usable_bits() }
        }
    }
}

pub mod vec
{
    pub use std::vec::{ * };
}

pub unsafe fn domain()
{
    unsafe
    {   
        env::initialize_paths();
        let mut pls = shell::Shell::new();
        let arguments: Vec<String> = env::args().collect();
        commands::initialize_cache();
        commands::update_aliases( &pls );

        match true
        {
            true if is::script(&arguments) =>
            {
                log!("run script: {:?} ", &arguments);
                let status = scripts::run(&mut pls, &arguments);
                process::exit(status);
            }
            
            true if is::command_string(&arguments) =>
            {
                let line = env::args_to_command_line();
                log!("run with -c args: {}", &line);
                now::run_command_line(&mut pls, &line, false, false);
                process::exit( pls.previous_status);
            }
            
            true if is::non_tty() =>
            {
                now::run_procs_for_non_tty( &mut pls );
                return;
            }

            _ =>
            {
                println!( r#"( {} )::seed( {} )"#, pls.session_id, is::login( &arguments ) );
            }
        }
        /*
        loop
        {

        }*/
    }
}

fn main()
{
    unsafe
    {
        libc::signal( libc::SIGPIPE, libc::SIG_DFL );
        libc::signal( libc::SIGTSTP, libc::SIG_IGN );
        libc::signal( libc::SIGQUIT, libc::SIG_IGN );
        domain();
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 3836
