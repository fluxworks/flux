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
*/
#[macro_use] extern crate lazy_static;
/*
*/
extern crate libc;
extern crate nix;
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
    use ::
    {
        fs::{ OpenOptions },
        io::{ Write },
        *,
    };
    /*
        use std::collections::HashMap;
        use std::env;
        use std::fs::File;
        use std::fs::OpenOptions;
        use std::io::Write;
        use std::os::unix::io::IntoRawFd;
        use std::path::{Path, PathBuf};
    */

    #[macro_export] macro_rules! log
    {
        ($fmt:expr) =>
        (
            let log_file = if let Ok(x) = ::env::var("CICADA_LOG_FILE") { x.clone() } 
            else { String::new() };

            if !log_file.is_empty()
            {

                let msg = $fmt;
                match  OpenOptions::new().append(true).create(true).open(&log_file)
                {
                    Ok(mut cfile) =>
                    {
                        let pid = $crate::get::pid();
                        let now = $crate::time::c::DateTime::now();
                        let msg = format!("[{}][{}] {}", now, pid, msg);
                        let msg = if msg.ends_with('\n') { msg } else { format!("{}\n", msg) };

                        match cfile.write_all(msg.as_bytes())
                        {
                            Ok(_) => {}
                            Err(_) => println!("tlog: write_all error")
                        }
                    }
                    Err(_) => println!("tlog: open file error"),
                }
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            let msg = format!($fmt, $($arg)*);
            log!(&msg);
        );
    }

    #[macro_export] macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            match writeln!(&mut $crate::io::stderr(), $fmt)
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            match writeln!(&mut $crate::io::stderr(), $fmt, $($arg)*)
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

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
/// Custom Commands
pub mod command
{
    //! Defines the set of line editing commands
    use ::
    {
        borrow::Cow::{ self, Borrowed, Owned },
        char::{ escape_sequence },
        *,
    };

    macro_rules! define_commands
    {
        ( $( #[$meta:meta] $name:ident => $str:expr , )+ ) =>
        {
            /// Represents a command to modify `Reader` state
            #[derive(Clone, Debug, Eq, PartialEq)]
            pub enum Command
            {
                $( #[$meta] $name , )+
                /// Custom application-defined command
                Custom(Cow<'static, str>),
                /// Execute a given key sequence
                Macro(Cow<'static, str>),
            }

            /// List of all command names
            pub static COMMANDS: &[&str] = &[ $( $str ),+ ];

            impl fmt::Display for Command
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match *self
                    {
                        $( Command::$name => f.write_str($str) , )+
                        Command::Custom(ref s) => f.write_str(s),
                        Command::Macro(ref s) => write!(f, "\"{}\"",
                            escape_sequence(s))
                    }
                }
            }

            impl Command
            {
                /// Constructs a command from a `'static str` reference.
                pub fn from_str(name: &'static str) -> Command {
                    Command::opt_from_str(name)
                        .unwrap_or_else(|| Command::Custom(Borrowed(name)))
                }
                /// Constructs a command from a non-`'static` string-like type.
                pub fn from_string<T>(name: T) -> Command where
                T: AsRef<str> + Into<String>
                {
                    Command::opt_from_str(name.as_ref())
                        .unwrap_or_else(|| Command::Custom(Owned(name.into())))
                }

                fn opt_from_str(s: &str) -> Option<Command>
                {
                    match s
                    {
                        $( $str => Some(Command::$name), )+
                        _ => None
                    }
                }
            }
        }
    }

    define_commands!
    {
        /// Abort history search
        Abort => "abort",
        /// Accepts the current input line
        AcceptLine => "accept-line",
        /// Perform completion
        Complete => "complete",
        /// Insert all completions into the input buffer
        InsertCompletions => "insert-completions",
        /// Show possible completions
        PossibleCompletions => "possible-completions",
        /// Insert the next possible completion
        MenuComplete => "menu-complete",
        /// Insert the previous possible completion
        MenuCompleteBackward => "menu-complete-backward",
        /// Begin numeric argument input
        DigitArgument => "digit-argument",
        /// Insert character or sequence at the cursor
        SelfInsert => "self-insert",
        /// Inserts a tab character
        TabInsert => "tab-insert",
        /// Toggles insert/overwrite mode
        OverwriteMode => "overwrite-mode",
        /// Insert a comment and accept input
        InsertComment => "insert-comment",
        /// Move the cursor backward one character
        BackwardChar => "backward-char",
        /// Move the cursor forward one character
        ForwardChar => "forward-char",
        /// Search for a given character
        CharacterSearch => "character-search",
        /// Search backward for a given character
        CharacterSearchBackward => "character-search-backward",
        /// Move the cursor backward one word
        BackwardWord => "backward-word",
        /// Move the cursor forward one word
        ForwardWord => "forward-word",
        /// Kill all characters before the cursor
        BackwardKillLine => "backward-kill-line",
        /// Kill all characters after the cursor
        KillLine => "kill-line",
        /// Kill a word before the cursor
        BackwardKillWord => "backward-kill-word",
        /// Kill a word after the cursor
        KillWord => "kill-word",
        /// Kill a word before the cursor, delimited by whitespace
        UnixWordRubout => "unix-word-rubout",
        /// Clear the screen
        ClearScreen => "clear-screen",
        /// Move the cursor to the beginning of the line
        BeginningOfLine => "beginning-of-line",
        /// Move the cursor to the end of the line
        EndOfLine => "end-of-line",
        /// Delete one character before the cursor
        BackwardDeleteChar => "backward-delete-char",
        /// Delete one character after the cursor
        DeleteChar => "delete-char",
        /// Drag the character before the cursor forward
        TransposeChars => "transpose-chars",
        /// Drag the word before the cursor forward
        TransposeWords => "transpose-words",
        /// Move to the first line of history
        BeginningOfHistory => "beginning-of-history",
        /// Move to the last line of history
        EndOfHistory => "end-of-history",
        /// Select next line in history
        NextHistory => "next-history",
        /// Select previous line in history
        PreviousHistory => "previous-history",
        /// Incremental search in history
        ForwardSearchHistory => "forward-search-history",
        /// Incremental reverse search in history
        ReverseSearchHistory => "reverse-search-history",
        /// Non-incremental forward history search using input up to the cursor
        HistorySearchForward => "history-search-forward",
        /// Non-incremental backward history search using input up to the cursor
        HistorySearchBackward => "history-search-backward",
        /// Insert literal character
        QuotedInsert => "quoted-insert",
        /// Insert text into buffer from the kill ring
        Yank => "yank",
        /// Rotate the kill ring and yank the new top
        YankPop => "yank-pop",
    }
    /// Describes the category of a command
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum Category
    {
        /// Completion command
        Complete,
        /// Kill command
        Kill,
        /// Non-incremental search command
        Search,
        /// Incremental search command
        IncrementalSearch,
        /// Yank command
        Yank,
        /// Digit argument command
        Digit,
        /// Other command
        Other,
    }

    impl Command
    {
        /// Returns the category of the command
        pub fn category(&self) -> Category
        {
            use self::Command::*;

            match *self
            {
                DigitArgument => Category::Digit,
                Complete | InsertCompletions | PossibleCompletions |
                    MenuComplete | MenuCompleteBackward => Category::Complete,
                BackwardKillLine | KillLine | BackwardKillWord | KillWord |
                    UnixWordRubout => Category::Kill,
                ForwardSearchHistory | ReverseSearchHistory => Category::IncrementalSearch,
                HistorySearchForward | HistorySearchBackward => Category::Search,
                Yank | YankPop => Category::Yank,
                _ => Category::Other
            }
        }
    }

    impl Default for Command
    {
        fn default() -> Self { Command::Custom(Borrowed("")) }
    }

    /**/
    pub mod alias
    {

    }
    /**/
    pub mod bg
    {

    }
    /**/
    pub mod cd
    {

    }
    /**/
    pub mod cinfo
    {

    }
    /**/
    pub mod exec
    {

    }
    /**/
    pub mod exit
    {

    }
    /**/
    pub mod export
    {

    }
    /**/
    pub mod fg
    {

    }
    /**/
    pub mod history
    {

    }
    /**/
    pub mod jobs
    {

    }
    /**/
    pub mod read
    {

    }
    /**/
    pub mod source
    {

    }
    /**/
    pub mod unalias
    {

    }
    /**/
    pub mod vox
    {

    }
    /**/
    pub mod ulimit
    {

    }
    /**/
    pub mod minfd
    {

    }
    /**/
    pub mod set
    {

    }
    /**/
    pub mod unpath
    {

    }
    /**/
    pub mod unset
    {

    }
    /**/
    pub mod utils
    {

    }

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
    // pub fn env_args_to_command_line() -> String
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
    //! Provides the `Function` trait for implementing custom `Prompter` commands
    pub use std::ffi::{ * };
    use ::
    {
        command::{ Category },
        terminal::{ Terminal, Terminals, prompter::{ Prompter } },
        *,
    };    
    /*
    use crate::terminal::Terminals;    
    use std::io;
    use lineread::{ Terminal};
    */
    /// Implements custom functionality for a `Prompter` command
    pub trait Function<Term:Terminals>: Send + Sync
    {
        /// Executes the function.
        fn execute(&self, prompter: &mut Prompter<Term>, count: i32, ch: char) -> io::Result<()>;
        /// Returns the command category.
        fn category(&self) -> Category { Category::Other }
    }

    impl<F, Term: Terminal> Function<Term> for F where
    F: Send + Sync,
    F: Fn(&mut Prompter<Term>, i32, char) -> io::Result<()>
    {
        fn execute(&self, prompter: &mut Prompter<Term>, count: i32, ch: char) -> io::Result<()>
        {
            self(prompter, count, ch)
        }
    }

    pub struct EnterFunction;

    impl<T: Terminals> Function<T> for EnterFunction
    {
        fn execute(&self, prompter: &mut Prompter<T>, count: i32, _ch: char) -> io::Result<()>
        {
            let buf = prompter.buffer();
            let linfo = parsers::line::parse(buf);
            
            if linfo.is_complete { prompter.accept_input() }
            else if count > 0
            {
                match prompter.insert(count as usize, '\n')
                {
                    Ok(_) => {},
                    Err(e) => { println!("sub-prompt error: {}", e); }
                }

                prompter.insert_str(">> ")
            }
            else { Ok(()) }
        }
    }
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
    use ::
    {
        path::{ Path },
        *
    };

    pub fn get_rc_file() -> String
    {
        let dir_config = tools::get_config_dir();
        let rc_file = format!("{}/cicadarc", dir_config);
        
        if Path::new(&rc_file).exists() { return rc_file; }
        
        let home = tools::get_user_home();
        let rc_file_home = format!("{}/{}", home, ".cicadarc");
        
        if Path::new(&rc_file_home).exists() { return rc_file_home; }
        
        rc_file
    }

    pub fn load_rc_files(sh: &mut shell::Shell)
    {
        let rc_file = get_rc_file();

        if !Path::new(&rc_file).exists() { return; }

        let args = vec!["source".to_string(), rc_file];
        scripts::run(sh, &args);
    }
}
/// Asynchronous basic functionality.
pub mod future
{
    pub use std::future::{ * };
}
/// State Reading
pub mod get
{
    use ::
    {
        *,
    };
    /*
    pub fn getpid() -> i32 */
    pub fn pid() -> i32 { unsafe { libc::getpid() } }
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
        env::{ var },
        regex::{ re_contains },
        *,
    };
    /*
    is_command_string( ... ) -> bool */
    pub fn command_string( args:&[String] ) -> bool { args.len() > 1 && args[1] == "-c" }
    /*
    is_login( ... ) -> bool */
    pub fn login( args:&[String] ) -> bool
    {
        if !args.is_empty() && args[0].starts_with("-") { return true; }

        if args.len() > 1 && (args[1] == "--login" || args[1] == "-l") { return true; }

        if let Ok(term_program) = var("TERM_PROGRAM") { if term_program == "vscode" { return true; } }

        false
    }
    /*
    is_non_tty( ... ) -> bool */
    pub fn non_tty() -> bool { unsafe { libc::isatty(0) == 0 } }
    /*
    is_shell_altering_command( ... ) -> bool */
    pub fn shell_altering_command( line:&str ) -> bool
    {
        let line = line.trim();
        
        if re_contains(line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$") { return true; }
        
        line.starts_with("alias ")
        || line.starts_with("export ")
        || line.starts_with("unalias ")
        || line.starts_with("unset ")
        || line.starts_with("source ")
    }
    /*
    is_script( ... ) -> bool */
    pub fn script( args:&[String] ) -> bool { args.len() > 1 && !args[1].starts_with("-") }
}
/// Traits, helpers, and type definitions for core I/O functionality.
pub mod io
{
    pub use std::io::{ * };

    pub mod reader
    {

    }

    pub mod writer
    {
        
    }
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
/// Execution API
pub mod now
{
    use ::
    {
        io::{ Read, Write },
        nix::
        {
          unistd::{ execve, ForkResult },
        },
        os::
        {
            unix::io::FromRawFd,
            fd::{ RawFd }
        },
        regex::{ Regex },
        shell::{ self, Shell },
        *,
    };
    /*
        use std::collections::HashMap;
        use std::io::{self, Read, Write};
        
        use crate::types::{CommandLine, CommandResult, Tokens};
        
        use std::ffi::{CStr, CString};
        use std::fs::File;
        
        use libs::pipes::pipe;
        
        use crate::shell::{self, Shell};
        
        use crate::types::{CommandLine, CommandOptions, CommandResult};
    */
    pub fn run_command_line(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();

        for token in parsers::line::to_cmds(line)
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
    /// Run a single command.
    fn run_single_program
    (
        sh: &mut shell::Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        options: &CommandOptions,
        pgid: &mut i32,
        term_given: &mut bool,
        cmd_result: &mut CommandResult,
        pipes: &[(RawFd, RawFd)],
        fds_capture_stdout: &Option<(RawFd, RawFd)>,
        fds_capture_stderr: &Option<(RawFd, RawFd)>,
    ) -> i32
    {
        unsafe
        {
            let capture = options.capture_output;

            if cl.is_single_and_builtin()
            {
                if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture)
                {
                    *cmd_result = cr;
                    return unsafe { libc::getpid() };
                }

                println_stderr!("cicada: error when run singler builtin");
                log!("error when run singler builtin: {:?}", cl);
                return 1;
            }

            let pipes_count = pipes.len();
            let mut fds_stdin = None;
            let cmd = cl.commands.get(idx_cmd).unwrap();

            if cmd.has_here_string()
            {
                match process::pipe()
                {
                    Ok(fds) => fds_stdin = Some(fds),
                    Err(e) =>
                    {
                        println_stderr!("cicada: pipeline4: {}", e);
                        return 1;
                    }
                }
            }

            match process::fork()
            {
                Ok(ForkResult::Child) =>
                {
                    libc::signal(libc::SIGTSTP, libc::SIG_DFL);
                    libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                    
                    if idx_cmd > 0
                    {
                        for i in 0..idx_cmd - 1
                        {
                            let fds = pipes[i];
                            libs::close(fds.0);
                            libs::close(fds.1);
                        }
                    }
                    
                    for i in idx_cmd + 1..pipes_count
                    {
                        let fds = pipes[i];
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        if let Some(fds) = fds_capture_stdout
                        {
                            libs::close(fds.0);
                            libs::close(fds.1);
                        }

                        if let Some(fds) = fds_capture_stderr
                        {
                            libs::close(fds.0);
                            libs::close(fds.1);
                        }
                    }

                    if idx_cmd == 0
                    {
                        let pid = libc::getpid();
                        libc::setpgid(0, pid);
                    }
                    
                    else { libc::setpgid(0, *pgid); }
                    
                    if idx_cmd > 0
                    {
                        let fds_prev = pipes[idx_cmd - 1];
                        libs::dup2(fds_prev.0, 0);
                        libs::close(fds_prev.0);
                        libs::close(fds_prev.1);
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        libs::dup2(fds.1, 1);
                        libs::close(fds.1);
                        libs::close(fds.0);
                    }

                    if cmd.has_redirect_from()
                    {
                        if let Some(redirect_from) = &cmd.redirect_from
                        {
                            let fd = tools::get_fd_from_file(&redirect_from.clone().1);
                            if fd == -1 { process::exit(1); }

                            libs::dup2(fd, 0);
                            libs::close(fd);
                        }
                    }

                    if cmd.has_here_string()
                    {
                        if let Some(fds) = fds_stdin
                        {
                            libs::close(fds.1);
                            libs::dup2(fds.0, 0);
                            libs::close(fds.0);
                        }
                    }

                    let mut stdout_redirected = false;
                    let mut stderr_redirected = false;
                    
                    for item in &cmd.redirects_to
                    {
                        let from_ = &item.0;
                        let op_ = &item.1;
                        let to_ = &item.2;
                        
                        if to_ == "&1" && from_ == "2"
                        {
                            if idx_cmd < pipes_count
                            {
                                libs::dup2(1, 2);
                            }
                            
                            else if !options.capture_output
                            {
                                let fd = libs::dup(1);
                                if fd == -1
                                {
                                    println_stderr!("cicada: dup error");
                                    process::exit(1);
                                }

                                libs::dup2(fd, 2);
                            }
                            
                            else { }
                        }
                        
                        else if to_ == "&2" && from_ == "1"
                        {
                            if idx_cmd < pipes_count || !options.capture_output
                            {
                                let fd = libs::dup(2);
                                if fd == -1
                                {
                                    println_stderr!("cicada: dup error");
                                    process::exit(1);
                                }

                                libs::dup2(fd, 1);
                            }
                            
                            else { }
                        } 
                        
                        else
                        {
                            let append = op_ == ">>";
                            match tools::create_raw_fd_from_file(to_, append)
                            {
                                Ok(fd) =>
                                {
                                    if fd == -1
                                    {
                                        println_stderr!("cicada: fork: fd error");
                                        process::exit(1);
                                    }

                                    if from_ == "1"
                                    {
                                        libs::dup2(fd, 1);
                                        stdout_redirected = true;
                                    }
                                    
                                    else
                                    {
                                        libs::dup2(fd, 2);
                                        stderr_redirected = true;
                                    }
                                }

                                Err(e) =>
                                {
                                    println_stderr!("cicada: fork: {}", e);
                                    process::exit(1);
                                }
                            }
                        }
                    }
                    
                    if idx_cmd == pipes_count && options.capture_output
                    {
                        if !stdout_redirected
                        {
                            if let Some(fds) = fds_capture_stdout
                            {
                                libs::close(fds.0);
                                libs::dup2(fds.1, 1);
                                libs::close(fds.1);
                            }
                        }

                        if !stderr_redirected
                        {
                            if let Some(fds) = fds_capture_stderr
                            {
                                libs::close(fds.0);
                                libs::dup2(fds.1, 2);
                                libs::close(fds.1);
                            }
                        }
                    }

                    if cmd.is_builtin()
                    {
                        if let Some(status) = try_run_builtin_in_subprocess(sh, cl, idx_cmd, capture)
                        { process::exit(status); }
                    }
                    
                    let mut c_envs: Vec<_> = vars().map(|(k, v)|
                    {
                        CString::new(format!("{}={}", k, v).as_str()).expect("CString error")
                    }).collect();

                    for (key, value) in cl.envs.iter()
                    {
                        c_envs.push( CString::new(format!("{}={}", key, value).as_str()).expect("CString error"), );
                    }

                    let program = &cmd.tokens[0].1;
                    let path = if program.contains('/') {
                        program.clone()
                    }
                    
                    else {  libs::path::find_file_in_path(program, true) };

                    if path.is_empty()
                    {
                        println_stderr!("cicada: {}: command not found", program);
                        process::exit(127);
                    }

                    let c_program = CString::new(path.as_str()).expect("CString::new failed");
                    let c_args: Vec<_> = cmd
                    .tokens
                    .iter()
                    .map(|x| CString::new(x.1.as_str()).expect("CString error"))
                    .collect();

                    let c_args: Vec<&CStr> = c_args.iter().map(|x| x.as_c_str()).collect();
                    let c_envs: Vec<&CStr> = c_envs.iter().map(|x| x.as_c_str()).collect();
                    match execve(&c_program, &c_args, &c_envs)
                    {
                        Ok(_) => {}
                        Err(e) => match e
                        {
                            nix::Error::ENOEXEC => 
                            { println_stderr!("cicada: {}: exec format error (ENOEXEC)", program); }
                            
                            nix::Error::ENOENT => 
                            { println_stderr!("cicada: {}: file does not exist", program); }
                            
                            nix::Error::EACCES => 
                            { println_stderr!("cicada: {}: Permission denied", program); }

                            _ => { println_stderr!("cicada: {}: {:?}", program, e); }
                        },
                    }

                    process::exit(1);
                }

                Ok(ForkResult::Parent { child, .. }) =>
                {
                    let pid: i32 = child.into();
                    if idx_cmd == 0
                    {
                        *pgid = pid;
                        if sh.has_terminal && options.isatty && !cl.background 
                        { *term_given = shell::give_terminal_to(pid); }
                    }

                    if options.isatty && !options.capture_output
                    {
                        let _cmd = parsers::parser_line::tokens_to_line(&cmd.tokens);
                        sh.insert_job(*pgid, pid, &_cmd, "Running", cl.background);
                    }

                    if let Some(redirect_from) = &cmd.redirect_from
                    {
                        if redirect_from.0 == "<<<"
                        {
                            if let Some(fds) = fds_stdin
                            {
                                unsafe
                                {
                                    libs::close(fds.0);

                                    let mut f = File::from_raw_fd(fds.1);
                                    
                                    match f.write_all(redirect_from.1.clone().as_bytes())
                                    {
                                        Ok(_) => {}
                                        Err(e) => println_stderr!("cicada: write_all: {}", e),
                                    }
                                    
                                    match f.write_all(b"\n")
                                    {
                                        Ok(_) => {}
                                        Err(e) => println_stderr!("cicada: write_all: {}", e),
                                    }
                                }
                            }
                        }
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        libs::close(fds.1);
                    }
                    
                    if idx_cmd > 0
                    {
                        let fds = pipes[idx_cmd - 1];
                        libs::close(fds.0);
                    }

                    if idx_cmd == pipes_count && options.capture_output
                    {
                        let mut s_out = String::new();
                        let mut s_err = String::new();

                        if let Some(fds) = fds_capture_stdout
                        {
                            libs::close(fds.1);

                            let mut f = File::from_raw_fd(fds.0);
                            match f.read_to_string(&mut s_out)
                            {
                                Ok(_) => {}
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
                            }
                        }
                        
                        if let Some(fds) = fds_capture_stderr
                        {
                            libs::close(fds.1);
                            let mut f_err = File::from_raw_fd(fds.0);
                            match f_err.read_to_string(&mut s_err)
                            {
                                Ok(_) => {}
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
                            }
                        }

                        *cmd_result = CommandResult
                        {
                            gid: *pgid,
                            status: 0,
                            stdout: s_out.clone(),
                            stderr: s_err.clone(),
                        };
                    }

                    pid
                }

                Err(_) =>
                {
                    println_stderr!("Fork failed");
                    *cmd_result = CommandResult::error();
                    0
                }
            }
        }
    }
    /// Run a pipeline (e.g. `echo hi | wc -l`)
    /// returns: (is-terminal-given, command-result)
    pub fn run_pipeline( sh:&mut shell::Shell, cl:&CommandLine, tty:bool, capture:bool, log_cmd:bool ) -> 
    ( bool, CommandResult )
    {
        unsafe
        {
            let mut term_given = false;
        
            if cl.background && capture
            {
                println_stderr!("cicada: cannot capture output of background cmd");
                return (term_given, CommandResult::error());
            }

            if let Some(cr) = try_run_calculator(&cl.line, capture) { return (term_given, cr); }
            
            if let Some(cr) = try_run_func(sh, cl, capture, log_cmd) { return (term_given, cr); }

            if log_cmd { log!("run: {}", cl.line); }

            let length = cl.commands.len();
            
            if length == 0
            {
                println!("cicada: invalid command: cmds with empty length");
                return (false, CommandResult::error());
            }

            let mut pipes = Vec::new();
            let mut errored_pipes = false;
            
            for _ in 0..length - 1
            {
                match pipe()
                {
                    Ok(fds) => pipes.push(fds),
                    Err(e) =>
                    {
                        errored_pipes = true;
                        println_stderr!("cicada: pipeline1: {}", e);
                        break;
                    }
                }
            }

            if errored_pipes
            {
                for fds in pipes
                {
                    libs::close(fds.0);
                    libs::close(fds.1);
                }
                
                return (false, CommandResult::error());
            }

            if pipes.len() + 1 != length
            {
                println!("cicada: invalid command: unmatched pipes count");
                return (false, CommandResult::error());
            }

            let mut pgid: i32 = 0;
            let mut fg_pids: Vec<i32> = Vec::new();

            let isatty = if tty { libc::isatty(1) == 1 } 
            else { false };

            let options = CommandOptions
            {
                isatty,
                capture_output: capture,
                background: cl.background,
                envs: cl.envs.clone(),
            };

            let mut fds_capture_stdout = None;
            let mut fds_capture_stderr = None;
            
            if capture
            {
                match pipe()
                {
                    Ok(fds) => fds_capture_stdout = Some(fds),
                    Err(e) =>
                    {
                        println_stderr!("cicada: pipeline2: {}", e);
                        return (false, CommandResult::error());
                    }
                }

                match pipe()
                {
                    Ok(fds) => fds_capture_stderr = Some(fds),
                    Err(e) =>
                    {
                        if let Some(fds) = fds_capture_stdout
                        {
                            libs::close(fds.0);
                            libs::close(fds.1);
                        }

                        println_stderr!("cicada: pipeline3: {}", e);
                        return (false, CommandResult::error());
                    }
                }
            }

            let mut cmd_result = CommandResult::new();

            for i in 0..length
            {
                let child_id: i32 = run_single_program
                (
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

                if child_id > 0 && !cl.background { fg_pids.push(child_id); }
            }

            if cl.is_single_and_builtin() { return (false, cmd_result); }

            if cl.background
            {
                if let Some(job) = sh.get_job_by_gid(pgid) { println_stderr!("[{}] {}", job.id, job.gid); }
            }

            if !fg_pids.is_empty()
            {
                let _cr = jobc::wait_fg_job(sh, pgid, &fg_pids);
                
                if !capture { cmd_result = _cr; }
            }
            
            (term_given, cmd_result)
        }
    }
    /// Entry point for non-ttys (e.g. Cmd-N on MacVim)
    pub fn run_procedures_for_non_tty(sh: &mut Shell)
    {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        match handle.read_to_string(&mut buffer)
        {
            Ok(_) =>
            {
                log!("run non tty command: {}", &buffer);
                run_command_line(sh, &buffer, false, false);
            }
            
            Err(e) =>
            {
                println!("cicada: stdin.read_to_string() failed: {:?}", e);
            }
        }
    }
    //  fn run_proc(...) -> CommandResult
    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    fn run_procedure( sh:&mut Shell, line:&str, tty:bool, capture:bool ) -> CommandResult
    {
        unsafe
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

                    let (term_given, cr) = core::run_pipeline(sh, &cl, tty, capture, log_cmd);
                    if term_given
                    {
                        unsafe {
                            let gid = libc::getpgid(0);
                            shell::give_terminal_to(gid);
                        }
                    }

                    cr
                }
                
                Err(e) =>
                {
                    println_stderr!("cicada: {}", e);
                    CommandResult::from_status(0, 1)
                }
            }
        }
    }
    
    fn try_run_builtin( sh: &mut Shell, cl: &CommandLine, idx_cmd: usize, capture: bool ) -> Option<CommandResult>
    {
        let capture = capture && idx_cmd + 1 == cl.commands.len();

        if idx_cmd >= cl.commands.len()
        {
            println_stderr!("unexpected error in try_run_builtin");
            return None;
        }

        let cmd = &cl.commands[idx_cmd];
        let tokens = cmd.tokens.clone();
        let cname = tokens[0].1.clone();
        
        if cname == "alias" {
            let cr = ffi::alias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "bg" {
            let cr = ffi::bg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cd" {
            let cr = ffi::cd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cinfo" {
            let cr = ffi::cinfo::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exec" {
            let cr = ffi::exec::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exit" {
            let cr = ffi::exit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "export" {
            let cr = ffi::export::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "fg" {
            let cr = ffi::fg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "history" {
            let cr = ffi::history::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "jobs" {
            let cr = ffi::jobs::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "minfd" {
            let cr = ffi::minfd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "read" {
            let cr = ffi::read::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "set" {
            let cr = ffi::set::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "source" {
            let cr = ffi::source::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "ulimit" {
            let cr = ffi::ulimit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unalias" {
            let cr = ffi::unalias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unset" {
            let cr = ffi::unset::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unpath" {
            let cr = ffi::unpath::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "vox" {
            let cr = ffi::vox::run(sh, cl, cmd, capture);
            return Some(cr);
        }
        None
    }
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
    pub use std::os::{ * };
}
/// Panic support in the standard library.
pub mod panic
{
    pub use std::panic::{ * };
}
/**/
pub mod parsers
{
    pub mod line
    {
        use ::
        {
            *,
        };
        /*
            use regex::Regex;

            use crate::libs;
            use crate::tools;
            use crate::types::{LineInfo, Redirection, Tokens};
        */

        /// Parse command line to tokens
        /// pub fn parse_line(...) -> LineInfo
        pub fn parse( line:&str ) -> LineInfo
        {
            // FIXME: let rewrite this parse part and make it a separated lib
            let mut result = Vec::new();
            if tools::is_arithmetic(line) {
                for x in line.split(' ') {
                    result.push((String::from(""), x.to_string()));
                }
                return LineInfo::new(result);
            }

            let mut sep = String::new();
            // `sep_second` is for commands like this:
            //    export DIR=`brew --prefix openssl`/include
            // it only could have non-empty value when sep is empty.
            let mut sep_second = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let mut met_parenthesis = false;
            let mut new_round = true;
            let mut skip_next = false;
            let mut has_dollar = false;
            let mut parens_left_ignored = false;

            // for cmds like: `ll foo\>bar end` -> `ll 'foo>bar' end`
            let mut sep_made = String::new();

            // using semi_ok makes quite dirty here
            // it is mainly for path completion like:
            // $ ls "foo b<TAB>
            // # then got `"foo bar"/`, then hit tab again:
            // $ ls "foo bar"/<TAB>
            // # should got:
            // $ ls "foo bar/the-single-file.txt"
            // also using semi_ok makes the following command works as expected:
            // $ touch "foo"/bar.txt  # create bar.txt under ./foo directory
            let mut semi_ok = false;
            let count_chars = line.chars().count();
            for (i, c) in line.chars().enumerate() {
                if skip_next {
                    skip_next = false;
                    continue;
                }

                if has_backslash && sep.is_empty() && (c == '>' || c == '<') {
                    sep_made = String::from("'");
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if has_backslash && sep == "\"" && c != '\"' {
                    // constant with bash: "\"" --> "; "\a" --> \a
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if has_backslash {
                    if new_round && sep.is_empty() && (c == '|' || c == '$') && token.is_empty() {
                        sep = String::from("\\");
                        token = format!("{}", c);
                    } else {
                        token.push(c);
                    }
                    new_round = false;
                    has_backslash = false;
                    continue;
                }

                if c == '$' {
                    has_dollar = true;
                }

                // for cases like: echo $(foo bar)
                if c == '(' && sep.is_empty() {
                    if !has_dollar && token.is_empty() {
                        // temp solution for cmd like `(ls)`, `(ls -lh)`
                        parens_left_ignored = true;
                        continue;
                    }
                    met_parenthesis = true;
                }
                if c == ')' {
                    if parens_left_ignored && !has_dollar {
                        // temp solution for cmd like `(ls)`, `(ls -lh)`
                        if i == count_chars - 1 ||
                                (i + 1 < count_chars &&
                                line.chars().nth(i + 1).unwrap() == ' ') {
                            continue;
                        }
                    }
                    if sep.is_empty() {
                        met_parenthesis = false;
                    }
                }

                if c == '\\' {
                    if sep == "'" || !sep_second.is_empty() {
                        token.push(c)
                    } else {
                        has_backslash = true;
                    }
                    continue;
                }

                if new_round {
                    if c == ' ' {
                        continue;
                    } else if c == '"' || c == '\'' || c == '`' {
                        sep = c.to_string();
                        new_round = false;
                        continue;
                    }

                    sep = String::new();

                    if c == '#' {
                        // handle inline comments
                        break;
                    }

                    if c == '|' {
                        if i + 1 < count_chars && line.chars().nth(i + 1).unwrap() == '|' {
                            result.push((String::from(""), "||".to_string()));
                            skip_next = true;
                        } else {
                            result.push((String::from(""), "|".to_string()));
                        }
                        new_round = true;
                        continue;
                    }

                    token.push(c);
                    new_round = false;
                    continue;
                }

                if c == '|' && !has_backslash {
                    if semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        result.push((String::from(""), "|".to_string()));
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        continue;
                    } else if !met_parenthesis && sep_second.is_empty() && sep.is_empty() {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((String::from(""), token));
                        }
                        result.push((String::from(""), "|".to_string()));
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        continue;
                    }
                }

                if c == ' ' {
                    if semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        continue;
                    }

                    if has_backslash {
                        has_backslash = false;
                        token.push(c);
                        continue;
                    }

                    if met_parenthesis {
                        token.push(c);
                        continue;
                    }

                    if sep == "\\" {
                        result.push((String::from("\\"), token));
                        token = String::new();
                        new_round = true;
                        continue;
                    }

                    if sep.is_empty() {
                        if sep_second.is_empty() {
                            if sep.is_empty() && !sep_made.is_empty() {
                                result.push((sep_made.clone(), token));
                                sep_made = String::new();
                            } else {
                                result.push((String::from(""), token));
                            }
                            token = String::new();
                            new_round = true;
                            continue;
                        } else {
                            token.push(c);
                            continue;
                        }
                    } else {
                        token.push(c);
                        continue;
                    }
                }

                if c == '\'' || c == '"' || c == '`' {
                    if has_backslash {
                        has_backslash = false;
                        token.push(c);
                        continue;
                    }

                    if sep != c.to_string() && semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        // do not use continue here!
                    }

                    if sep != c.to_string() && met_parenthesis {
                        token.push(c);
                        continue;
                    }
                    if sep.is_empty() && !sep_second.is_empty() && sep_second != c.to_string() {
                        token.push(c);
                        continue;
                    }

                    if sep.is_empty() {
                        let is_an_env = libs::re::re_contains(&token, r"^[a-zA-Z0-9_]+=.*$");
                        if !is_an_env && (c == '\'' || c == '"') {
                            sep = c.to_string();
                            continue;
                        }

                        token.push(c);
                        if sep_second.is_empty() {
                            sep_second = c.to_string();
                        } else if sep_second == c.to_string() {
                            sep_second = String::new();
                        }
                        continue;
                    } else if sep == c.to_string() {
                        semi_ok = true;
                        continue;
                    } else {
                        token.push(c);
                    }
                } else {
                    if has_backslash {
                        has_backslash = false;
                        if sep == "\"" || sep == "'" {
                            token.push('\\');
                        }
                    }
                    token.push(c);
                }
            }
            if !token.is_empty() || semi_ok {
                if sep.is_empty() && !sep_made.is_empty() {
                    result.push((sep_made.clone(), token));
                } else {
                    result.push((sep.clone(), token));
                }
            }

            let mut is_line_complete = true;
            if !result.is_empty() {
                let token_last = result[result.len() - 1].clone();
                if token_last.0.is_empty() && token_last.1 == "|" {
                    is_line_complete = false;
                }
            }

            if !sep.is_empty() {
                is_line_complete = semi_ok;
            }
            if has_backslash {
                is_line_complete = false;
            }

            LineInfo { tokens: result, is_complete: is_line_complete }
        }
        //  pub fn line_to_cmds(...) -> Vec<String>
        /// Parse command line for multiple commands.
        pub fn to_cmds( line:&str ) -> Vec<String>
        {
            let mut result = Vec::new();
            let mut sep = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let len = line.chars().count();

            for (i, c) in line.chars().enumerate()
            {
                if has_backslash
                {
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if c == '\\' && sep != "'"
                {
                    has_backslash = true;
                    continue;
                }

                if c == '#'
                {
                    if sep.is_empty() {
                        break;
                    } else {
                        token.push(c);
                        continue;
                    }
                }

                if c == '\'' || c == '"' || c == '`'
                {
                    if sep.is_empty()
                    {
                        sep.push(c);
                        token.push(c);
                        continue;
                    }
                    
                    else if sep == c.to_string()
                    {
                        token.push(c);
                        sep = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push(c);
                        continue;
                    }
                }

                if c == '&' || c == '|'
                {
                    if sep.is_empty()
                    {
                        if i + 1 == len
                        {
                            token.push(c);
                            continue;
                        }
                        
                        else
                        {
                            let c_next = match line.chars().nth(i + 1)
                            {
                                Some(x) => x,
                                None =>
                                {
                                    println!("chars nth error - should never happen");
                                    continue;
                                }
                            };

                            if c_next != c
                            {
                                token.push(c);
                                continue;
                            }
                        }
                    }

                    if sep.is_empty()
                    {
                        sep.push(c);
                        continue;
                    }
                    
                    else if c.to_string() == sep
                    {
                        let _token = token.trim().to_string();
                        
                        if !_token.is_empty() { result.push(_token); }

                        token = String::new();
                        result.push(format!("{}{}", sep, sep));
                        sep = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push(c);
                        continue;
                    }
                }

                if c == ';'
                {
                    if sep.is_empty()
                    {
                        let _token = token.trim().to_string();
                        
                        if !_token.is_empty() { result.push(_token); }

                        result.push(String::from(";"));
                        token = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push(c);
                        continue;
                    }
                }

                token.push(c);
            }

            if !token.is_empty() { result.push(token.trim().to_string()); }

            result
        }
    }
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

    use ::
    {
        libc::{ c_int },
        nix::
        {
            unistd::{ fork as nix_fork, ForkResult },
            Error, Result,
        },
        os::fd::{ RawFd },
        *,
    };

    pub fn close(fd: i32) { unsafe { libc::close(fd); } }

    pub fn dup(fd: i32) -> i32 { unsafe { libc::dup(fd) } }

    pub fn dup2(src: i32, dst: i32) { unsafe { libc::dup2(src, dst); } }
    
    pub fn fork() -> Result<ForkResult> { unsafe{ nix_fork() } }    

    pub fn pipe() -> std::result::Result<(RawFd, RawFd), Error>
    {
        unsafe
        {
            let mut fds = mem::MaybeUninit::<[c_int; 2]>::uninit();
            let res = libc::pipe(fds.as_mut_ptr() as *mut c_int);
            Error::result( res )?;
            Ok((fds.assume_init()[0], fds.assume_init()[1]))
        }
    }
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
/**/
pub mod scripts
{
    use ::
    {
        *,
    };
    /*
    pub fn run_script(...) -> i32 */
    pub fn run(sh: &mut shell::Shell, args: &Vec<String>) -> i32
    {
        let src_file = &args[1];
        let full_src_file: String;
        if src_file.contains('/') {
            full_src_file = src_file.clone();
        } else {
            let full_path = libs::path::find_file_in_path(src_file, false);
            if full_path.is_empty() {
                // not in PATH and not in current work directory
                if !Path::new(src_file).exists() {
                    println_stderr!("cicada: {}: no such file", src_file);
                    return 1;
                }
                full_src_file = format!("./{}", src_file);
            } else {
                full_src_file = full_path.clone();
            }
        }

        if !Path::new(&full_src_file).exists() {
            println_stderr!("cicada: {}: no such file", src_file);
            return 1;
        }
        if Path::new(&full_src_file).is_dir() {
            println_stderr!("cicada: {}: is a directory", src_file);
            return 1;
        }

        let mut file;
        match File::open(&full_src_file) {
            Ok(x) => file = x,
            Err(e) => {
                println_stderr!("cicada: {}: failed to open file - {:?}", &full_src_file, e.kind());
                return 1;
            }
        }
        let mut text = String::new();
        match file.read_to_string(&mut text) {
            Ok(_) => {}
            Err(e) => {
                match e.kind() {
                    ErrorKind::InvalidData => {
                        println_stderr!("cicada: {}: not a valid script file", &full_src_file);
                    }
                    _ => {
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
        let cr_list = run_lines(sh, &text_new, args, false);
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
/// Shell
pub mod shell
{
    /*
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
    */
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
/// Types and Traits for working with asynchronous tasks.
pub mod task
{
    pub use std::task::{ * };
}
/**/
pub mod terminal
{
    use ::
    {
        io::{ self, BufRead, BufReader, BufWriter, Read as _, Seek, SeekFrom, Write as _, },
        sync::{ Arc, Mutex, MutexGuard },
        *,
    };
    /*
    use std::borrow::Cow;
    use std::fmt;
    use std::fs::{File, OpenOptions};
    use std::path::Path;
    use std::sync::{Arc, Mutex, MutexGuard};
    use std::time::Duration;

    use crate::command::Command;
    use crate::complete::Completer;
    use crate::function::Function;
    use crate::inputrc::Directive;
    use crate::reader::{Read, Reader, ReadLock, ReadResult};
    use crate::terminal::{DefaultTerminal, Signal, Terminal};
    use crate::variables::Variable;
    use crate::writer::{Write, Writer, WriteLock};
    */
    /// ANSI color codes wrapped with \x01 and \x02 for lineread
    pub const GREEN: &str = "\x01\x1b[0;32m\x02";

    pub mod highlighter
    {
        //! Syntax highlighting functionality for the terminal interface.
        use ::
        {
            collections::{ HashSet },
            parsers::{ line },
            path::{ Path },
            os::unix::fs::{ PermissionsExt },
            ops::{ Range },
            sync::{ Arc, Mutex },
            *,
        };
        
        lazy_static!
        {
            static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
            static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        }
        /// Represents a style to be applied to a text range.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Style
        {
            /// A style using raw ANSI color codes
            AnsiColor(String),
            /// The default terminal style
            Default,
        }
        /// A trait for providing style information for a line of text.
        pub trait Highlighter
        {
            /// Takes the current line buffer and returns a list of styled ranges.
            fn highlight(&self, line: &str) -> Vec<(Range<usize>, Style)>;
        }
        /// Initialize the available commands cache by scanning PATH directories
        pub fn init_command_cache()
        {
            let commands = scan_available_commands();
            if let Ok(mut cache) = AVAILABLE_COMMANDS.lock()
            {
                *cache = commands;
            }
        }
        /// Update aliases in the highlighter's cache
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

        fn scan_available_commands() -> HashSet<String>
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
                                        if metadata.permissions().mode() & 0o111 != 0
                                        {
                                            if let Some(name) = entry.file_name().to_str()
                                            { commands.insert(name.to_string()); }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            commands
        }
        
        #[derive(Clone)]
        pub struct CicadaHighlighter;
        
        impl Highlighter for CicadaHighlighter
        {
            fn highlight( &self, line:&str ) -> Vec<(Range<usize>, Style)>
            {
                let mut styles = Vec::new();

                if line.is_empty() { return styles; }

                let line_info = parsers::line::parse( line );
                
                if line_info.tokens.is_empty()
                {
                    styles.push((0..line.len(), Style::Default));
                    return styles;
                }

                let mut current_byte_idx = 0;
                let mut is_start_of_segment = true;

                for token in &line_info.tokens {
                    // Find the range in the original line for this token
                    match find_token_range_heuristic(line, current_byte_idx, token) {
                        Some(token_range) => {
                            // Style potential whitespace before the token
                            if token_range.start > current_byte_idx {
                                styles.push((current_byte_idx..token_range.start, Style::Default));
                            }

                            let (_sep, word) = token;
                            let mut current_token_style = Style::Default;

                            if is_start_of_segment && !word.is_empty() {
                                if is_command(word) {
                                    current_token_style = Style::AnsiColor(GREEN.to_string());
                                }
                                // Only the first non-empty token in a segment can be a command
                                is_start_of_segment = false;
                            }

                            styles.push((token_range.clone(), current_token_style));

                            // Check if this token marks the end of a command segment
                            if ["|", "&&", "||", ";"].contains(&word.as_str()) {
                                is_start_of_segment = true;
                            }

                            current_byte_idx = token_range.end;
                        }
                        None => {
                            // If we can't map a token, style the rest of the line as default and stop.
                            if current_byte_idx < line.len() {
                            styles.push((current_byte_idx..line.len(), Style::Default));
                            }
                            current_byte_idx = line.len(); // Mark as done
                            break; // Stop processing further tokens
                        }
                    }
                }

                // Style any remaining characters after the last processed token
                if current_byte_idx < line.len() {
                    styles.push((current_byte_idx..line.len(), Style::Default));
                }

                styles
            }
        }

        pub fn create_highlighter() -> Arc<CicadaHighlighter> { Arc::new( CicadaHighlighter ) }
    }
    pub use self::highlighter::{ Highlighter };
    
    pub mod main
    {

    }
    
    pub mod preset
    {
        
    }

    pub mod prompter
    {
        use ::
        {
            *,
        };
        /*
        use std::io;
        use std::mem::replace;
        use std::ops::Range;
        use std::sync::Arc;
        use std::time::Instant;

        use mortal::FindResult;

        use crate::chars::{is_ctrl, is_printable, DELETE, EOF};
        use crate::command::{Category, Command};
        use crate::complete::Completion;
        use crate::function::Function;
        use crate::reader::{BindingIter, InputState, ReadLock, ReadResult};
        use crate::table::{format_columns, Line, Table};
        use crate::terminal::{CursorMode, Signal, Size, Terminal};
        use crate::util::{
            get_open_paren, find_matching_paren, first_word,
            longest_common_prefix, repeat_char,
            back_n_words, forward_n_words,
            backward_char, forward_char, backward_word, forward_word,
            word_start, word_end, RangeArgument,
        };
        use crate::variables::VariableIter;
        use crate::writer::{
            BLINK_DURATION, display_str,
            Digit, Display, HistoryIter, PromptType, Writer, WriteLock,
        };
        */
        /// Provides access to the current state of input while a `read_line` call is in progress.
        pub struct Prompter<'a, 'b: 'a, Term: 'b + Terminals>
        {
            pub read: &'a mut ReadLock<'b, Term>,
            write: WriteLock<'b, Term>,
        }

        impl<'a, 'b: 'a, Term: 'b + Terminals> Prompter<'a, 'b, Term> 
        {
            pub fn new(read: &'a mut ReadLock<'b, Term>, write: WriteLock<'b, Term>) -> Prompter<'a, 'b, Term>
            {
                Prompter{read, write}
            }

            /// Returns a `Writer` instance using the currently held write lock.
            pub fn writer_append<'c>(&'c mut self) -> io::Result<Writer<'c, 'b, Term>> {
                Writer::with_ref(&mut self.write, false)
            }
            /// Returns a `Writer` instance using the currently held write lock.
            pub fn writer_erase<'c>(&'c mut self) -> io::Result<Writer<'c, 'b, Term>> {
                Writer::with_ref(&mut self.write, true)
            }
            /// Resets input state at the start of `read_line`
            fn reset_input(&mut self) {
                self.read.reset_data();
                self.write.reset_data();
            }

            pub fn start_read_line(&mut self) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.is_prompt_drawn = true;
                self.write.update_size()?;
                self.write.draw_prompt()
            }

            pub fn end_read_line(&mut self) -> io::Result<()> {
                self.write.expire_blink()?;

                if self.read.overwrite_mode {
                    self.write.set_cursor_mode(CursorMode::Normal)?;
                }
                if self.write.is_prompt_drawn {
                    self.write.move_to_end()?;
                    self.write.write_str("\n")?;
                    self.write.is_prompt_drawn = false;
                }

                self.reset_input();
                self.read.state = InputState::Inactive;

                Ok(())
            }

            pub fn handle_input(&mut self, ch: char) -> io::Result<Option<ReadResult>> {
                self.write.expire_blink()?;

                match self.read.state {
                    InputState::Inactive => panic!("input received in inactive state"),
                    InputState::NewSequence => {
                        if ch == EOF && self.write.buffer.is_empty() {
                            self.write.write_str("\n")?;
                            self.write.is_prompt_drawn = false;
                            return Ok(Some(ReadResult::Eof));
                        } else {
                            self.read.sequence.push(ch);
                            self.execute_sequence()?;

                            if self.read.input_accepted {
                                let s = replace(&mut self.write.buffer, String::new());
                                return Ok(Some(ReadResult::Input(s)));
                            }
                        }
                    }
                    InputState::ContinueSequence{expiry: _} => {
                        self.read.sequence.push(ch);

                        self.execute_sequence()?;

                        if self.read.input_accepted {
                            let s = replace(&mut self.write.buffer, String::new());
                            return Ok(Some(ReadResult::Input(s)));
                        }
                    }
                    InputState::Number => {
                        if let Some(digit) = ch.to_digit(10) {
                            self.write.input_arg.input(digit as i32);

                            if self.write.input_arg.is_out_of_bounds() {
                                self.read.state = InputState::NewSequence;
                                self.write.input_arg = Digit::None;
                                self.write.explicit_arg = false;
                                self.write.redraw_prompt(PromptType::Normal)?;
                            } else {
                                self.write.redraw_prompt(PromptType::Number)?;
                            }
                        } else {
                            self.read.state = InputState::NewSequence;
                            self.write.redraw_prompt(PromptType::Normal)?;
                            self.read.macro_buffer.insert(0, ch);
                        }
                    }
                    InputState::CharSearch{n, backward} => {
                        if n != 0 {
                            if backward {
                                self.write.backward_search_char(n, ch)?;
                            } else {
                                self.write.forward_search_char(n, ch)?;
                            }
                        }
                        self.read.state = InputState::NewSequence;
                    }
                    InputState::TextSearch => {
                        if ch == DELETE {
                            {
                                let write = &mut *self.write;
                                write.search_buffer.pop();
                                write.last_search.clone_from(&write.search_buffer);
                            }
                            self.write.search_history_update()?;
                        } else if self.is_abort(ch) {
                            self.abort_search_history()?;
                        } else if is_ctrl(ch) {
                            self.end_search_history()?;
                            self.read.macro_buffer.insert(0, ch);
                        } else {
                            {
                                let write = &mut *self.write;
                                write.search_buffer.push(ch);
                                write.last_search.clone_from(&write.search_buffer);
                            }
                            self.write.search_history_update()?;
                        }
                    }
                    InputState::CompleteIntro => {
                        match ch {
                            'y' | 'Y' | ' ' => {
                                self.write.write_str("\n")?;
                                self.show_completions_page(0)?;
                            }
                            '\r' | '\n' => {
                                self.write.write_str("\n")?;
                                self.show_completions_line(0)?;
                            }
                            'q' | 'Q' |
                            'n' | 'N' | DELETE => {
                                self.write.write_str("\n")?;
                                self.end_page_completions()?;
                            }
                            _ => ()
                        }
                    }
                    InputState::CompleteMore(offset) => {
                        match ch {
                            'y' | 'Y' | ' ' => {
                                self.write.clear_prompt()?;
                                self.show_completions_page(offset)?;
                            }
                            '\r' | '\n' => {
                                self.write.clear_prompt()?;
                                self.show_completions_line(offset)?;
                            }
                            'q' | 'Q' |
                            'n' | 'N' | DELETE => {
                                self.write.clear_prompt()?;
                                self.end_page_completions()?;
                            }
                            _ => ()
                        }
                    }
                    InputState::QuotedInsert(n) => {
                        if n != 0 {
                            self.insert(n, ch)?;
                        }
                        self.read.state = InputState::NewSequence;
                    }
                }

                Ok(None)
            }
            /// Returns the current buffer.
            pub fn buffer(&self) -> &str {
                &self.write.buffer
            }
            /// Returns the "backup" buffer.
            ///
            /// When the user is currently editing a history entry, the backup buffer
            /// contains the original user input.
            pub fn backup_buffer(&self) -> &str {
                &self.write.backup_buffer
            }
            /// Returns the command `Category` of the most recently executed command.
            ///
            /// Some commands may use this to influence behavior of repeated commands.
            pub fn last_command_category(&self) -> Category {
                self.read.last_cmd
            }
            /// Returns the set of characters that indicate a word break.
            pub fn word_break_chars(&self) -> &str {
                &self.read.word_break
            }
            /// Sets the buffer to the given value.
            ///
            /// The cursor is moved to the end of the buffer.
            pub fn set_buffer(&mut self, buf: &str) -> io::Result<()> {
                self.write.set_buffer(buf)
            }
            /// Returns the current position of the cursor.
            pub fn cursor(&self) -> usize {
                self.write.cursor
            }
            /// Sets the cursor to the given position within the buffer.
            pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
                self.write.set_cursor(pos)
            }
            /// Sets the prompt that will be displayed when `read_line` is called.
            pub fn set_prompt(&mut self, prompt: &str) -> io::Result<()> {
                self.write.set_prompt(prompt)
            }
            /// Returns the size of the terminal at the last draw operation.
            pub fn screen_size(&self) -> Size {
                self.write.screen_size
            }
            /// Returns whether a numerical argument was explicitly supplied by the user.
            pub fn explicit_arg(&self) -> bool {
                self.write.explicit_arg
            }
            /// Returns the current input sequence.
            pub fn sequence(&self) -> &str {
                &self.read.sequence
            }
            /// Returns an iterator over bound sequences
            pub fn bindings(&self) -> BindingIter {
                self.read.bindings()
            }
            /// Returns an iterator over variable values.
            pub fn variables(&self) -> VariableIter {
                self.read.variables()
            }
            /// Returns an iterator over history entries
            pub fn history(&self) -> HistoryIter {
                self.write.history()
            }
            /// Returns the index into history currently being edited.
            pub fn history_index(&self) -> Option<usize> {
                self.write.history_index
            }
            /// Returns the current number of history entries.
            pub fn history_len(&self) -> usize {
                self.write.history.len()
            }

            fn next_history(&mut self, n: usize) -> io::Result<()> {
                self.write.next_history(n)
            }

            fn prev_history(&mut self, n: usize) -> io::Result<()> {
                self.write.prev_history(n)
            }
            /// Selects the history entry currently being edited by the user.
            pub fn select_history_entry(&mut self, new: Option<usize>) -> io::Result<()> {
                self.write.select_history_entry(new)
            }
            /// Returns the current set of completions.
            pub fn completions(&self) -> Option<&[Completion]> {
                self.read.completions.as_ref().map(|v| &v[..])
            }
            /// Sets the current set of completions.
            pub fn set_completions(&mut self, completions: Option<Vec<Completion>>) {
                self.read.completions = completions;
            }
            /// Attempts to execute the current sequence.
            fn execute_sequence(&mut self) -> io::Result<()> {
                match self.find_binding(&self.read.sequence) {
                    FindResult::Found(cmd) => {
                        let ch = self.read.sequence.chars().last().unwrap();
                        let n = self.write.input_arg.to_i32();

                        self.read.state = InputState::NewSequence;
                        self.execute_command(cmd, n, ch)?;
                        self.read.sequence.clear();
                    }
                    FindResult::NotFound => {
                        self.read.state = InputState::NewSequence;
                        self.insert_first_char()?;
                    }
                    FindResult::Incomplete => {
                        let expiry = None;
                        self.read.state = InputState::ContinueSequence{expiry};
                    }
                    FindResult::Undecided(_) => {
                        let expiry = self.keyseq_expiry();
                        self.read.state = InputState::ContinueSequence{expiry};
                    }
                }

                Ok(())
            }

            fn force_execute_sequence(&mut self) -> io::Result<()> {
                self.read.state = InputState::NewSequence;

                match self.find_binding(&self.read.sequence) {
                    FindResult::Found(cmd) |
                    FindResult::Undecided(cmd) => {
                        let ch = self.read.sequence.chars().last().unwrap();
                        let n = self.write.input_arg.to_i32();

                        self.execute_command(cmd, n, ch)?;
                        self.read.sequence.clear();
                    }
                    FindResult::NotFound => {
                        self.insert_first_char()?;
                    }
                    FindResult::Incomplete => unreachable!(),
                }

                Ok(())
            }
            /// Execute the command `SelfInsert` on the first character in the input sequence, if it is printable.
            fn insert_first_char(&mut self) -> io::Result<()> {
                let (first, rest) = {
                    let mut chars = self.read.sequence.chars();

                    (chars.next().unwrap(), chars.as_str().to_owned())
                };

                self.read.sequence.clear();

                if is_printable(first) {
                    let n = self.write.input_arg.to_i32();
                    self.execute_command(Command::SelfInsert, n, first)?;
                }

                if !rest.is_empty() {
                    self.read.queue_input(&rest);
                }

                Ok(())
            }

            fn find_binding(&self, seq: &str) -> FindResult<Command> {
                self.read.bindings.find(seq).cloned()
            }

            fn get_function(&self, name: &str) -> Option<&Arc<dyn Function<Term>>> {
                self.read.functions.get(name)
            }

            fn is_abort(&self, ch: char) -> bool {
                let mut buf = [0; 4];
                let s = ch.encode_utf8(&mut buf);

                self.find_binding(&s) == FindResult::Found(Command::Abort)
            }

            fn execute_command(&mut self, cmd: Command, n: i32, ch: char) -> io::Result<()> {
                use crate::command::Command::*;

                let mut category = cmd.category();

                if self.read.overwrite_mode {
                    match cmd {
                        DigitArgument | SelfInsert => (),
                        BackwardDeleteChar if n >= 0 => (),
                        _ => self.read.overwritten_chars.clear()
                    }
                }

                match cmd {
                    Abort => (),
                    AcceptLine => {
                        self.accept_input()?;
                    }
                    Complete => {
                        if !self.read.disable_completion {
                            self.complete_word()?;
                        } else if is_printable(ch) {
                            self.execute_command(SelfInsert, n, ch)?;
                        }
                    }
                    InsertCompletions => {
                        if self.read.completions.is_none() {
                            self.build_completions();
                        }

                        if let Some(completions) = self.read.completions.take() {
                            self.insert_completions(&completions)?;
                            self.read.completions = Some(completions);
                        }
                    }
                    PossibleCompletions => {
                        if self.read.completions.is_none() {
                            self.build_completions();
                        }

                        if let Some(completions) = self.read.completions.take() {
                            self.show_completions(&completions)?;
                            self.read.completions = Some(completions);
                        }
                    }
                    MenuComplete => {
                        if self.read.completions.is_none() {
                            self.build_completions();
                        }

                        if n > 0 {
                            self.next_completion(n as usize)?;
                        } else {
                            self.prev_completion((-n) as usize)?;
                        }
                    }
                    MenuCompleteBackward => {
                        if self.read.completions.is_none() {
                            self.build_completions();
                        }

                        if n > 0 {
                            self.prev_completion(n as usize)?;
                        } else {
                            self.next_completion((-n) as usize)?;
                        }
                    }
                    DigitArgument => {
                        self.read.state = InputState::Number;
                        self.write.set_digit_from_char(ch);
                        self.write.redraw_prompt(PromptType::Number)?;
                    }
                    SelfInsert => {
                        if n > 0 {
                            let n = n as usize;

                            if self.read.overwrite_mode {
                                self.overwrite(n, ch)?;
                            } else {
                                self.insert(n, ch)?;
                            }

                            if self.read.blink_matching_paren {
                                if let Some(open) = get_open_paren(ch) {
                                    if let Some(pos) = find_matching_paren(
                                            &self.write.buffer[..self.write.cursor],
                                            &self.read.string_chars, open, ch) {
                                        self.blink(pos)?;
                                    }
                                }
                            }
                        }
                    }
                    TabInsert => {
                        if n > 0 {
                            self.insert(n as usize, '\t')?;
                        }
                    }
                    InsertComment => {
                        if self.explicit_arg() &&
                                self.write.buffer.starts_with(&self.read.comment_begin[..]) {
                            self.write.move_to(0)?;
                            let n = self.read.comment_begin.len();

                            self.delete_range(..n)?;
                            self.accept_input()?;
                        } else {
                            self.write.move_to(0)?;
                            let s = self.read.comment_begin.clone();
                            self.insert_str(&s)?;
                            self.accept_input()?;
                        }
                    }
                    BackwardChar => {
                        if n > 0 {
                            self.write.backward_char(n as usize)?;
                        } else if n < 0 {
                            self.write.forward_char((-n) as usize)?;
                        }
                    }
                    ForwardChar => {
                        if n > 0 {
                            self.write.forward_char(n as usize)?;
                        } else if n < 0 {
                            self.write.backward_char((-n) as usize)?;
                        }
                    }
                    CharacterSearch => {
                        if n >= 0 {
                            self.read.state = InputState::CharSearch{
                                n: n as usize,
                                backward: false,
                            }
                        } else {
                            self.read.state = InputState::CharSearch{
                                n: (-n) as usize,
                                backward: true,
                            };
                        }
                    }
                    CharacterSearchBackward => {
                        if n >= 0 {
                            self.read.state = InputState::CharSearch{
                                n: n as usize,
                                backward: true,
                            }
                        } else {
                            self.read.state = InputState::CharSearch{
                                n: (-n) as usize,
                                backward: false,
                            };
                        }
                    }
                    BackwardWord => {
                        if n > 0 {
                            self.backward_word(n as usize)?;
                        } else if n < 0 {
                            self.forward_word((-n) as usize)?;
                        }
                    }
                    ForwardWord => {
                        if n > 0 {
                            let pos = forward_word(n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            self.write.move_to(pos)?;
                        } else if n < 0 {
                            let pos = forward_word((-n) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            self.write.move_to(pos)?;
                        }
                    }
                    BackwardKillLine => {
                        let r = ..self.write.cursor;
                        self.kill_range(r)?;
                    }
                    KillLine => {
                        let r = self.write.cursor..;
                        self.kill_range(r)?;
                    }
                    BackwardKillWord => {
                        if n > 0 {
                            let pos = backward_word(n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            let r = pos..self.write.cursor;
                            self.kill_range(r)?;
                        } else if n < 0 {
                            let pos = forward_word((-n) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            let r = self.write.cursor..pos;
                            self.kill_range(r)?;
                        }
                    }
                    KillWord => {
                        if n > 0 {
                            let pos = forward_word(n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            let r = self.write.cursor..pos;
                            self.kill_range(r)?;
                        } else if n < 0 {
                            let pos = backward_word((-n) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break);
                            let r = pos..self.write.cursor;
                            self.kill_range(r)?;
                        }
                    }
                    UnixWordRubout => {
                        if n > 0 {
                            let pos = backward_word(n as usize,
                                &self.write.buffer, self.write.cursor, " \t\n");
                            let r = pos..self.write.cursor;
                            self.kill_range(r)?;
                        } else if n < 0 {
                            let pos = forward_word((-n) as usize,
                                &self.write.buffer, self.write.cursor, " \t\n");
                            let r = self.write.cursor..pos;
                            self.kill_range(r)?;
                        }
                    }
                    ClearScreen => {
                        self.write.clear_screen()?;
                    }
                    BeginningOfLine => self.write.move_to(0)?,
                    EndOfLine => self.write.move_to_end()?,
                    BackwardDeleteChar => {
                        if n > 0 {
                            if self.read.overwrite_mode {
                                self.overwrite_back(n as usize)?;
                            } else {
                                let pos = backward_char(n as usize,
                                    &self.write.buffer, self.write.cursor);
                                let r = pos..self.write.cursor;
                                self.delete_range(r)?;
                            }
                        } else if n < 0 {
                            let pos = forward_char((-n) as usize,
                                &self.write.buffer, self.write.cursor);
                            let r = self.write.cursor..pos;
                            self.delete_range(r)?;
                        }
                    }
                    DeleteChar => {
                        if n > 0 {
                            let pos = forward_char(n as usize,
                                &self.write.buffer, self.write.cursor);
                            let r = self.write.cursor..pos;
                            self.delete_range(r)?;
                        } else if n < 0 {
                            let pos = backward_char(n as usize,
                                &self.write.buffer, self.write.cursor);
                            let r = pos..self.write.cursor;
                            self.delete_range(r)?;
                        }
                    }
                    TransposeChars => {
                        if n != 0 && self.write.cursor != 0 {
                            let (src, dest);

                            if !self.explicit_arg() && self.write.cursor == self.write.buffer.len() {
                                let end = backward_char(1, &self.write.buffer, self.write.cursor);
                                let start = backward_char(1, &self.write.buffer, end);

                                src = start..end;
                                dest = end..self.write.cursor;
                            } else {
                                let start = backward_char(1, &self.write.buffer, self.write.cursor);
                                let end = self.write.cursor;

                                src = start..end;

                                dest = if n < 0 {
                                    let back = backward_char((-n) as usize, &self.write.buffer, start);
                                    back..start
                                } else {
                                    let fwd = forward_char(n as usize + 1, &self.write.buffer, start);
                                    end..fwd
                                };
                            }

                            self.transpose_range(src, dest)?;
                        }
                    }
                    TransposeWords => {
                        if n != 0 {
                            if let Some(first) = first_word(&self.write.buffer[..self.write.cursor], &self.read.word_break) {
                                let start = word_start(&self.write.buffer, self.write.cursor, &self.read.word_break);

                                if first != start {
                                    let (src, dest);

                                    if !self.explicit_arg() && start == self.write.buffer.len() {
                                        let dest_start = backward_word(1, &self.write.buffer, start, &self.read.word_break);
                                        let dest_end = word_end(&self.write.buffer, dest_start, &self.read.word_break);

                                        let src_start = backward_word(1, &self.write.buffer, dest_start, &self.read.word_break);
                                        let src_end = word_end(&self.write.buffer, src_start, &self.read.word_break);

                                        src = src_start..src_end;
                                        dest = dest_start..dest_end;
                                    } else {
                                        let src_start = backward_word(1, &self.write.buffer, start, &self.read.word_break);
                                        let src_end = word_end(&self.write.buffer, src_start, &self.read.word_break);

                                        src = src_start..src_end;

                                        dest = if n < 0 {
                                            back_n_words((-n) as usize, &self.write.buffer, src_start, &self.read.word_break)
                                        } else {
                                            forward_n_words(n as usize, &self.write.buffer, src_start, &self.read.word_break)
                                        };
                                    }

                                    self.transpose_range(src, dest)?;
                                }
                            }
                        }
                    }
                    BeginningOfHistory => {
                        self.select_history_entry(Some(0))?;
                    }
                    EndOfHistory => {
                        self.select_history_entry(None)?;
                    }
                    NextHistory => {
                        if n > 0 {
                            self.next_history(n as usize)?;
                        } else if n < 0 {
                            self.prev_history((-n) as usize)?;
                        }
                    }
                    PreviousHistory => {
                        if n > 0 {
                            self.prev_history(n as usize)?;
                        } else if n < 0 {
                            self.next_history((-n) as usize)?;
                        }
                    }
                    ForwardSearchHistory => {
                        self.read.state = InputState::TextSearch;
                        if self.read.last_cmd == Category::IncrementalSearch {
                            self.write.continue_search_history(false)?;
                        } else {
                            self.write.start_search_history(false)?;
                        }
                    }
                    ReverseSearchHistory => {
                        self.read.state = InputState::TextSearch;
                        if self.read.last_cmd == Category::IncrementalSearch {
                            self.write.continue_search_history(true)?;
                        } else {
                            self.write.start_search_history(true)?;
                        }
                    }
                    HistorySearchForward => {
                        if self.read.last_cmd == Category::Search {
                            self.write.continue_history_search(false)?;
                        } else {
                            self.write.start_history_search(false)?;
                        }
                    }
                    HistorySearchBackward => {
                        if self.read.last_cmd == Category::Search {
                            self.write.continue_history_search(true)?;
                        } else {
                            self.write.start_history_search(true)?;
                        }
                    }
                    QuotedInsert => {
                        self.read.state = InputState::QuotedInsert(
                            if n >= 0 { n as usize } else { 0 });
                    }
                    OverwriteMode => {
                        self.read.overwrite_mode = !self.read.overwrite_mode;

                        if !self.read.overwrite_mode {
                            self.read.overwritten_append = 0;
                            self.read.overwritten_chars.clear();
                        }

                        let mode = if self.read.overwrite_mode {
                            CursorMode::Overwrite
                        } else {
                            CursorMode::Normal
                        };

                        self.write.set_cursor_mode(mode)?;
                    }
                    Yank => {
                        self.yank()?;
                    }
                    YankPop => {
                        self.yank_pop()?;
                    }
                    Custom(ref name) => {
                        if let Some(fun) = self.get_function(name).cloned() {
                            fun.execute(self, n, ch)?;

                            category = fun.category();
                        }
                    }
                    Macro(ref seq) => {
                        self.read.queue_input(seq);
                    }
                }

                if category != Category::Digit {
                    self.write.input_arg = Digit::None;
                    self.write.explicit_arg = false;

                    self.read.last_cmd = category;

                    if category != Category::Complete {
                        self.read.completions = None;
                    }

                    if category != Category::Yank {
                        self.read.last_yank = None;
                    }
                }

                Ok(())
            }
            /// Accepts the current input buffer as user input.
            pub fn accept_input(&mut self) -> io::Result<()> 
            {
                self.write.move_to_end()?;
                self.write.write_str("\n")?;
                self.read.input_accepted = true;
                self.write.is_prompt_drawn = false;
                Ok(())
            }
            /// Moves the cursor to the given position, waits for 500 milliseconds (or until next user input), 
            /// then restores the original cursor position.
            pub fn blink(&mut self, pos: usize) -> io::Result<()> {
                self.write.blink(pos)?;

                self.read.max_wait_duration = Some(BLINK_DURATION);

                Ok(())
            }

            fn check_expire_blink(&mut self, now: Instant) -> io::Result<()> {
                if self.write.check_expire_blink(now)? {
                    self.read.max_wait_duration = None;
                }

                Ok(())
            }

            fn check_expire_sequence(&mut self, now: Instant) -> io::Result<()> {
                if let InputState::ContinueSequence{expiry: Some(expiry)} = self.read.state {
                    if now >= expiry {
                        self.read.max_wait_duration = None;
                        self.force_execute_sequence()?;
                    }
                }

                Ok(())
            }

            fn keyseq_expiry(&mut self) -> Option<Instant> {
                if let Some(t) = self.read.keyseq_timeout {
                    self.read.max_wait_duration = Some(t);
                    Some(Instant::now() + t)
                } else {
                    None
                }
            }

            pub fn check_expire_timeout(&mut self) -> io::Result<()> {
                let now = Instant::now();

                self.check_expire_blink(now)?;
                self.check_expire_sequence(now)
            }

            fn expire_blink(&mut self) -> io::Result<()> {
                self.read.max_wait_duration = None;
                self.write.expire_blink()
            }

            fn build_completions(&mut self) {
                let compl = self.read.completer.clone();
                let end = self.write.cursor;
                let start = compl.word_start(&self.write.buffer, end, self);

                if start > end {
                    panic!("Completer::word_start returned invalid index; \
                        start > end ({} > {})", start, end);
                }

                let unquoted = compl.unquote(&self.write.buffer[start..end]).into_owned();

                let completions = compl.complete(&unquoted, self, start, end);
                let n_completions = completions.as_ref().map_or(0, |c| c.len());

                self.read.completions = completions;
                self.read.completion_index = n_completions;
                self.read.completion_start = start;
                self.read.completion_prefix = end;
            }

            fn complete_word(&mut self) -> io::Result<()> {
                if let Some(completions) = self.read.completions.take() {
                    if completions.len() == 1 {
                        self.substitute_completion(&completions[0])?;
                    } else {
                        self.show_completions(&completions)?;
                        self.read.completions = Some(completions);
                    }
                } else {
                    self.build_completions();
                    let completions = self.read.completions.take().unwrap_or_default();

                    if completions.len() == 1 {
                        self.substitute_completion(&completions[0])?;
                    } else if !completions.is_empty() {
                        let start = self.read.completion_start;
                        let end = self.write.cursor;

                        {
                            let pfx = longest_common_prefix(completions.iter()
                                .map(|compl| &compl.completion[..]))
                                .unwrap_or_default();
                            self.replace_str_forward(start..end, &pfx)?;
                        }

                        self.read.completions = Some(completions);
                    }
                }

                Ok(())
            }

            fn substitute_completion(&mut self, compl: &Completion) -> io::Result<()> {
                let mut s = self.read.completer.quote(&compl.completion);

                if let Some(suffix) = compl.suffix.with_default(self.read.completion_append_character) {
                    s.to_mut().push(suffix);
                }

                let start = self.read.completion_start;
                let end = self.write.cursor;
                self.replace_str_forward(start..end, &s)
            }

            fn insert_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
                let mut words = String::new();

                for compl in completions {
                    words.push_str(&self.read.completer.unquote(&compl.completion));
                    words.push(' ');
                }

                let start = self.read.completion_start;
                let end = self.write.cursor;

                self.replace_str_forward(start..end, &words)
            }

            fn show_completions(&mut self, completions: &[Completion]) -> io::Result<()> {
                if completions.is_empty() {
                    return Ok(());
                }

                let eff_width = self.write.screen_size.columns
                    .min(self.read.completion_display_width);

                let completions = completions.iter()
                    .map(|compl| display_str(&compl.display(), Display::default()).into_owned())
                    .collect::<Vec<_>>();

                let cols = format_columns(&completions, eff_width,
                    self.read.print_completions_horizontally);
                let table = Table::new(&completions, cols.as_ref().map(|c| &c[..]),
                    self.read.print_completions_horizontally);

                self.write.write_str("\n")?;

                let n_completions = completions.len();

                if self.read.page_completions &&
                        n_completions >= self.read.completion_query_items {
                    self.start_page_completions(n_completions)
                } else {
                    self.show_list_completions(table)?;
                    self.write.draw_prompt()
                }
            }

            fn start_page_completions(&mut self, n_completions: usize) -> io::Result<()> {
                self.read.state = InputState::CompleteIntro;
                self.write.redraw_prompt(PromptType::CompleteIntro(n_completions))
            }

            fn end_page_completions(&mut self) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.prompt_type = PromptType::Normal;
                self.write.draw_prompt()
            }

            fn is_paging_completions(&self) -> bool {
                match self.read.state {
                    InputState::CompleteMore(_) => true,
                    _ => false
                }
            }

            fn show_completions_page(&mut self, offset: usize) -> io::Result<()> {
                if let Some(compl) = self.read.completions.take() {
                    let width = self.write.screen_size.columns
                        .min(self.read.completion_display_width);
                    let n_lines = self.write.screen_size.lines - 1;

                    let completions = compl.iter()
                        .map(|compl| display_str(&compl.display(), Display::default()).into_owned())
                        .collect::<Vec<_>>();

                    let cols = format_columns(&completions, width,
                        self.read.print_completions_horizontally);
                    let mut table = Table::new(&completions, cols.as_ref().map(|c| &c[..]),
                        self.read.print_completions_horizontally);

                    for row in table.by_ref().skip(offset).take(n_lines) {
                        self.show_completion_line(row)?;
                    }

                    if table.has_more() {
                        self.read.completions = Some(compl);
                        self.read.state = InputState::CompleteMore(offset + n_lines);
                        self.write.prompt_type = PromptType::CompleteMore;
                        self.write.draw_prompt()?;
                    } else {
                        self.end_page_completions()?;
                    }
                }

                Ok(())
            }

            fn show_completions_line(&mut self, offset: usize) -> io::Result<()> {
                if let Some(compl) = self.read.completions.take() {
                    let width = self.write.screen_size.columns
                        .min(self.read.completion_display_width);
                    let completions = compl.iter()
                        .map(|compl| display_str(&compl.display(), Display::default()).into_owned())
                        .collect::<Vec<_>>();

                    let cols = format_columns(&completions, width,
                        self.read.print_completions_horizontally);
                    let mut table = Table::new(&completions, cols.as_ref().map(|c| &c[..]),
                        self.read.print_completions_horizontally);

                    if let Some(row) = table.by_ref().skip(offset).next() {
                        self.show_completion_line(row)?;
                    }

                    if table.has_more() {
                        self.read.completions = Some(compl);
                        self.read.state = InputState::CompleteMore(offset + 1);
                        self.write.prompt_type = PromptType::CompleteMore;
                        self.write.draw_prompt()?;
                    } else {
                        self.end_page_completions()?;
                    }
                }

                Ok(())
            }

            fn show_completion_line<S: AsRef<str>>(&mut self, line: Line<S>) -> io::Result<()> {
                let mut space = 0;

                for (width, name) in line {
                    self.write.move_right(space)?;
                    self.write.write_str(name)?;
                    space = width - name.chars().count();
                }

                self.write.write_str("\n")
            }

            fn show_list_completions<S: AsRef<str>>(&mut self, table: Table<S>) -> io::Result<()> {
                for line in table {
                    let mut space = 0;

                    for (width, name) in line {
                        self.write.move_right(space)?;
                        self.write.write_str(name)?;
                        space = width - name.chars().count();
                    }
                    self.write.write_str("\n")?;
                }

                Ok(())
            }

            fn next_completion(&mut self, n: usize) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or(0, |c| c.len());
                let max = len + 1;

                let old = self.read.completion_index;
                let new = (old + n) % max;

                if old != new {
                    self.set_completion(new)?;
                }

                Ok(())
            }

            fn prev_completion(&mut self, n: usize) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or(0, |c| c.len());
                let max = len + 1;

                let old = self.read.completion_index;
                let new = if n <= old {
                    max - old - n
                } else {
                    old - n
                };

                self.set_completion(new)
            }

            fn set_completion(&mut self, new: usize) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or(0, |c| c.len());
                let old = self.read.completion_index;

                if old != new {
                    self.read.completion_index = new;

                    if new == len {
                        let start = self.read.completion_prefix;
                        let end = self.write.cursor;

                        self.delete_range(start..end)?;
                    } else {
                        let start = self.read.completion_start;
                        let end = self.write.cursor;
                        let s = self.read.completions.as_ref().unwrap()[new]
                            .completion(self.read.completion_append_character).into_owned();

                        self.replace_str_forward(start..end, &s)?;
                    }
                }

                Ok(())
            }

            fn abort_search_history(&mut self) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.read.last_cmd = Category::Other;
                self.write.abort_search_history()
            }

            fn end_search_history(&mut self) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.end_search_history()
            }

            pub fn handle_resize(&mut self, size: Size) -> io::Result<()> {
                self.expire_blink()?;

                if self.is_paging_completions() {
                    self.end_page_completions()?;
                }

                self.write.screen_size = size;

                let p = self.write.prompt_type;
                self.write.redraw_prompt(p)
            }

            pub fn handle_signal(&mut self, signal: Signal) -> io::Result<()> {
                self.expire_blink()?;

                match signal {
                    Signal::Continue => {
                        self.write.draw_prompt()?;
                    }
                    Signal::Interrupt => {
                        self.read.macro_buffer.clear();
                        self.write.move_to_end()?;

                        if self.read.echo_control_characters {
                            self.write.write_str("^C")?;
                        }

                        self.write.write_str("\n")?;
                        self.reset_input();
                        self.write.draw_prompt()?;
                    }
                    _ => ()
                }

                Ok(())
            }

            fn backward_word(&mut self, n: usize) -> io::Result<()> {
                let pos = backward_word(n,
                    &self.write.buffer, self.write.cursor, &self.read.word_break);
                self.write.move_to(pos)
            }

            fn forward_word(&mut self, n: usize) -> io::Result<()> {
                let pos = forward_word(n,
                    &self.write.buffer, self.write.cursor, &self.read.word_break);
                self.write.move_to(pos)
            }
            /// Deletes a range of text from the input buffer.
            pub fn delete_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
                self.write.delete_range(range)
            }
            /// Deletes a range from the buffer and adds the removed text to the kill ring.
            pub fn kill_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
                let start = range.start().cloned().unwrap_or(0);
                let end = range.end().cloned().unwrap_or_else(|| self.write.buffer.len());
                let len = end - start;

                if len != 0 {
                    let buf = self.write.buffer[start..end].to_owned();

                    if self.read.last_cmd != Category::Kill {
                        self.push_kill_ring(buf);
                    } else if end == self.write.cursor {
                        self.prepend_kill_ring(buf);
                    } else {
                        self.append_kill_ring(buf);
                    }

                    self.delete_range(start..end)?;
                }

                Ok(())
            }

            fn push_kill_ring(&mut self, s: String) {
                if self.read.kill_ring.len() == self.read.kill_ring.capacity() {
                    self.read.kill_ring.pop_back();
                }
                self.read.kill_ring.push_front(s);
            }

            fn rotate_kill_ring(&mut self) {
                if let Some(kill) = self.read.kill_ring.pop_front() {
                    self.read.kill_ring.push_back(kill);
                }
            }

            fn append_kill_ring(&mut self, s: String) {
                if let Some(kill) = self.read.kill_ring.front_mut() {
                    kill.push_str(&s);
                    return;
                }
                self.push_kill_ring(s);
            }

            fn prepend_kill_ring(&mut self, s: String) {
                if let Some(kill) = self.read.kill_ring.front_mut() {
                    kill.insert_str(0, &s);
                    return;
                }
                self.push_kill_ring(s);
            }
            /// Transposes two regions of the buffer, `src` and `dest`.
            pub fn transpose_range(&mut self, src: Range<usize>, dest: Range<usize>)
                    -> io::Result<()> {
                self.write.transpose_range(src, dest)
            }
            /// Insert text from the front of the kill ring at the current cursor position.
            pub fn yank(&mut self) -> io::Result<()> {
                if let Some(kill) = self.read.kill_ring.front().cloned() {
                    let start = self.write.cursor;
                    self.read.last_yank = Some((start, start + kill.len()));

                    self.insert_str(&kill)?;
                }

                Ok(())
            }
            /// Rotates the kill ring and replaces yanked text with the new front.
            pub fn yank_pop(&mut self) -> io::Result<()> {
                if let Some((start, end)) = self.read.last_yank {
                    self.rotate_kill_ring();

                    if let Some(kill) = self.read.kill_ring.front().cloned() {
                        self.read.last_yank = Some((start, start + kill.len()));

                        self.write.move_to(start)?;
                        self.replace_str_forward(start..end, &kill)?;
                    }
                }

                Ok(())
            }
            /// Overwrite `n` characters; assumes `n >= 1`
            fn overwrite(&mut self, n: usize, ch: char) -> io::Result<()> {
                let start = self.write.cursor;
                let end = forward_char(n, &self.write.buffer, start);

                {
                    let over = &self.write.buffer[start..end];
                    let n_chars = over.chars().count();

                    if n > n_chars {
                        self.read.overwritten_append += n - n_chars;
                    }

                    if !over.is_empty() {
                        self.read.overwritten_chars.push_str(&over);
                    }
                }

                let s = repeat_char(ch, n);
                self.replace_str_forward(start..end, &s)
            }

            fn overwrite_back(&mut self, mut n: usize) -> io::Result<()> {
                if self.read.overwritten_append != 0 {
                    let n_del = n.min(self.read.overwritten_append);

                    let pos = backward_char(n_del, &self.write.buffer, self.write.cursor);
                    let r = pos..self.write.cursor;
                    self.delete_range(r)?;

                    self.read.overwritten_append -= n_del;
                    n -= n_del;
                }

                if n != 0 && !self.read.overwritten_chars.is_empty() {
                    let n_repl = n.min(self.read.overwritten_chars.chars().count());

                    let pos = backward_char(n_repl, &self.write.buffer, self.write.cursor);

                    let over_pos = backward_char(n_repl,
                        &self.read.overwritten_chars, self.read.overwritten_chars.len());

                    let over = self.read.overwritten_chars.drain(over_pos..).collect::<String>();

                    let r = pos..self.write.cursor;
                    self.replace_str_backward(r, &over)?;

                    n -= n_repl;
                }

                if n != 0 {
                    self.write.backward_char(n)?;
                }

                Ok(())
            }
            /// Insert a given character at the current cursor position `n` times.
            pub fn insert(&mut self, n: usize, ch: char) -> io::Result<()> {
                if n != 0 {
                    let s = repeat_char(ch, n);
                    self.insert_str(&s)?;
                }

                Ok(())
            }
            /// Insert a string at the current cursor position.
            pub fn insert_str(&mut self, s: &str) -> io::Result<()> {
                self.write.insert_str(s)
            }
            /// Replaces a range in the buffer and redraws.
            pub fn replace_str_backward<R: RangeArgument<usize>>(&mut self,
                    range: R, s: &str) -> io::Result<()> {
                self.replace_str_impl(range, s)?;
                let len = self.write.buffer.len();
                self.write.move_from(len)
            }
            /// Replaces a range in the buffer and redraws.
            pub fn replace_str_forward<R: RangeArgument<usize>>(&mut self,
                    range: R, s: &str) -> io::Result<()> {
                self.replace_str_impl(range, s)?;
                self.write.cursor += s.len();
                let len = self.write.buffer.len();
                self.write.move_from(len)
            }
            /// Replaces a range in the buffer and redraws.
            fn replace_str_impl<R: RangeArgument<usize>>(&mut self,
                    range: R, s: &str) -> io::Result<()> {
                let start = range.start().cloned().unwrap_or(0);
                let end = range.end().cloned().unwrap_or_else(|| self.write.buffer.len());
                self.write.move_to(start)?;

                let _ = self.write.buffer.drain(start..end);
                let cursor = self.write.cursor;
                self.write.buffer.insert_str(cursor, s);

                self.write.draw_buffer(cursor)?;
                self.write.clear_to_screen_end()
            }
        }
    }
    /// The main interface to input reading and other terminal operations
    pub struct Interface<Term: Terminals>
    {
        term: Arc<Term>,
        read: Arc<Mutex<Read<Term>>>,
        write: Arc<Mutex<Write>>,
        highlighter: Option<Arc<dyn Highlighter + Send + Sync>>,
    }

    impl Interface<DefaultTerminal> 
    {
        /// Creates a new `Interface` with the given application name.
        pub fn new<T>(application: T) -> io::Result<Interface<DefaultTerminal>>
                where T: Into<Cow<'static, str>> {
            let term = DefaultTerminal::new()?;
            Interface::with_term(application, term)
        }
    }

    impl<Term: Terminals> Interface<Term>
    {
        /// Creates a new `Interface` instance with a particular terminal implementation.
        pub fn with_term<T>(application: T, term: Term) -> io::Result<Interface<Term>> where 
        T:Into<Cow<'static, str>>
        {
            let size = term.lock_write().size()?;
            let read = Read::new(&term, application.into());

            Ok(Interface
            {
                term: Arc::new(term),
                read: Arc::new(Mutex::new(read)),
                write: Arc::new(Mutex::new(Write::new(size))),
                highlighter: None,
            })
        }
        /// Acquires the read lock and returns a `Reader` instance.
        pub fn lock_reader(&self) -> Reader<Term> {
            Reader::new(self, self.lock_read())
        }
        /// Acquires the write lock and returns a `Writer` instance.
        pub fn lock_writer_append(&self) -> io::Result<Writer<Term>> {
            Writer::with_lock(self.lock_write()?, false)
        }
        /// Acquires the write lock and returns a `Writer` instance.
        pub fn lock_writer_erase(&self) -> io::Result<Writer<Term>> {
            Writer::with_lock(self.lock_write()?, true)
        }

        fn lock_read(&self) -> ReadLock<Term> {
            ReadLock::new(
                self.term.lock_read(),
                self.read.lock().expect("Interface::lock_read"))
        }

        pub fn lock_write(&self)
                -> io::Result<WriteLock<Term>> {
            let guard = self.write.lock().unwrap();
            let term_writer = self.term.lock_write();

            Ok(WriteLock::new(term_writer, guard, self.highlighter.clone()))
        }

        pub fn lock_write_data(&self) -> MutexGuard<Write> {
            self.write.lock().expect("Interface::lock_write_data")
        }
    }
    /// ## Locking
    /// The following methods internally acquire the read lock.
    impl<Term: Terminals> Interface<Term> 
    {
        /// Interactively reads a line from the terminal device.
        pub fn read_line(&self) -> io::Result<ReadResult> {
            self.lock_reader().read_line()
        }
        /// Performs one step of the interactive `read_line` loop.
        pub fn read_line_step(&self, timeout: Option<Duration>)
                -> io::Result<Option<ReadResult>> {
            self.lock_reader().read_line_step(timeout)
        }
        /// Cancels an in-progress `read_line` operation.
        pub fn cancel_read_line(&self) -> io::Result<()> {
            self.lock_reader().cancel_read_line()
        }
        /// Returns a clone of the current completer instance.
        pub fn completer(&self) -> Arc<dyn Completer<Term>> {
            self.lock_reader().completer().clone()
        }
        /// Replaces the current completer, returning the previous instance.
        pub fn set_completer(&self, completer: Arc<dyn Completer<Term>>)
                -> Arc<dyn Completer<Term>> {
            self.lock_reader().set_completer(completer)
        }
        /// Returns the value of the named variable or `None` if no such variable exists.
        pub fn get_variable(&self, name: &str) -> Option<Variable> {
            self.lock_reader().get_variable(name)
        }
        /// Sets the value of the named variable and returns the previous value.
        pub fn set_variable(&self, name: &str, value: &str) -> Option<Variable> {
            self.lock_reader().set_variable(name, value)
        }
        /// Returns whether the given `Signal` is ignored.
        pub fn ignore_signal(&self, signal: Signal) -> bool {
            self.lock_reader().ignore_signal(signal)
        }
        /// Sets whether the given `Signal` will be ignored.
        pub fn set_ignore_signal(&self, signal: Signal, set: bool) {
            self.lock_reader().set_ignore_signal(signal, set)
        }
        /// Returns whether the given `Signal` is reported.
        pub fn report_signal(&self, signal: Signal) -> bool {
            self.lock_reader().report_signal(signal)
        }
        /// Sets whether the given `Signal` is reported.
        pub fn set_report_signal(&self, signal: Signal, set: bool) {
            self.lock_reader().set_report_signal(signal, set)
        }
        /// Binds a sequence to a command.
        pub fn bind_sequence<T>(&self, seq: T, cmd: Command) -> Option<Command> where 
        T: Into<Cow<'static, str>> {
            self.lock_reader().bind_sequence(seq, cmd)
        }

        /// Binds a sequence to a command, if and only if the given sequence is not already bound to a command.
        pub fn bind_sequence_if_unbound<T>(&self, seq: T, cmd: Command) -> bool where
        T: Into<Cow<'static, str>> {
            self.lock_reader().bind_sequence_if_unbound(seq, cmd)
        }
        /// Removes a binding for the given sequence.
        pub fn unbind_sequence(&self, seq: &str) -> Option<Command> {
            self.lock_reader().unbind_sequence(seq)
        }
        /// Defines a named function to which sequences may be bound.
        pub fn define_function<T>(&self, name: T, cmd: Arc<dyn Function<Term>>)
                -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
            self.lock_reader().define_function(name, cmd)
        }
        /// Removes a function defined with the given name.
        pub fn remove_function(&self, name: &str) -> Option<Arc<dyn Function<Term>>> {
            self.lock_reader().remove_function(name)
        }
        /// Evaluates a series of configuration directives.
        pub fn evaluate_directives(&self, dirs: Vec<Directive>) {
            self.lock_reader().evaluate_directives(&self.term, dirs)
        }
        /// Evaluates a single configuration directive.
        pub fn evaluate_directive(&self, dir: Directive) {
            self.lock_reader().evaluate_directive(&self.term, dir)
        }
    }
    /// ## Locking
    /// The following methods internally acquire the write lock.
    impl<Term: Terminals> Interface<Term>
    {
        /// Returns the current input buffer.
        pub fn buffer(&self) -> String {
            self.lock_write().map(|lock| lock.buffer.to_owned()).unwrap_or_default()
        }
        /// Returns the current number of history entries.
        pub fn history_len(&self) -> usize {
            self.lock_write().map(|lock| lock.history_len()).unwrap_or(0)
        }
        /// Returns the maximum number of history entries.
        pub fn history_size(&self) -> usize {
            self.lock_write().map(|lock| lock.history_size()).unwrap_or(0)
        }
        /// Save history entries to the specified file.
        pub fn save_history<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
            let path = path.as_ref();
            let mut w = self.lock_write()?;

            if !path.exists() || w.history_size() == !0 {
                self.append_history(path, &w)?;
            } else {
                self.rewrite_history(path, &w)?;
            }

            w.reset_new_history();
            Ok(())
        }

        fn append_history<P: AsRef<Path>>(&self, path: P, w: &WriteLock<Term>)
                -> io::Result<()> {
            let file = OpenOptions::new()
                .append(true)
                .create(true)
                .open(path.as_ref())?;

            self.append_history_to(&file, w)
        }

        fn append_history_to(&self, file: &File, w: &WriteLock<Term>) -> io::Result<()> {
            let mut wtr = BufWriter::new(file);

            for entry in w.new_history() {
                wtr.write_all(entry.as_bytes())?;
                wtr.write_all(b"\n")?;
            }

            wtr.flush()
        }

        fn rewrite_history<P: AsRef<Path>>(&self, path: P, w: &WriteLock<Term>)
                -> io::Result<()> {
            fn nth_line(s: &str, n: usize) -> Option<usize> {
                let start = s.as_ptr() as usize;

                s.lines().nth(n)
                    .map(|s| s.as_ptr() as usize - start)
            }

            let mut file = OpenOptions::new()
                .create(true)
                .read(true)
                .write(true)
                .open(path.as_ref())?;

            let mut hist = String::new();

            file.read_to_string(&mut hist)?;

            let n_lines = hist.lines().count();
            let n = n_lines.saturating_sub(
                w.history_size() - w.new_history_entries());

            if n != 0 {
                if let Some(pos) = nth_line(&hist, n) {
                    file.seek(SeekFrom::Start(0))?;
                    file.write_all(hist[pos..].as_bytes())?;

                    let n = file.seek(SeekFrom::Current(0))?;
                    file.set_len(n)?;
                }
            }

            self.append_history_to(&file, w)
        }
        /// Load history entries from the specified file.
        pub fn load_history<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
            let mut writer = self.lock_write()?;

            let file = File::open(&path)?;
            let rdr = BufReader::new(file);

            for line in rdr.lines() {
                writer.add_history(line?);
            }

            writer.reset_new_history();

            Ok(())
        }
        /// Writes formatted text to the terminal display.
        pub fn write_fmt(&self, args: fmt::Arguments) -> io::Result<()> {
            let s = args.to_string();
            self.write_str(&s)
        }

        fn write_str(&self, line: &str) -> io::Result<()> {
            self.lock_writer_erase()?.write_str(line)
        }
    }
    /// ## Locking
    /// The following methods internally acquire both the read and write locks.
    impl<Term: Terminals> Interface<Term> 
    {
        /// Sets the prompt that will be displayed when `read_line` is called.
        pub fn set_prompt(&self, prompt: &str) -> io::Result<()> {
            self.lock_reader().set_prompt(prompt)
        }
        /// Sets the input buffer to the given string.
        pub fn set_buffer(&self, buf: &str) -> io::Result<()> {
            self.lock_reader().set_buffer(buf)
        }
        /// Sets the cursor position in the input buffer.
        pub fn set_cursor(&self, pos: usize) -> io::Result<()> {
            self.lock_reader().set_cursor(pos)
        }
        /// Adds a line to history.
        pub fn add_history(&self, line: String) {
            self.lock_reader().add_history(line);
        }
        /// Adds a line to history, unless it is identical to the most recent entry.
        pub fn add_history_unique(&self, line: String) {
            self.lock_reader().add_history_unique(line);
        }
        /// Removes all history entries.
        pub fn clear_history(&self) {
            self.lock_reader().clear_history();
        }
        /// Removes the history entry at the given index.
        pub fn remove_history(&self, idx: usize) {
            self.lock_reader().remove_history(idx);
        }
        /// Sets the maximum number of history entries.
        pub fn set_history_size(&self, n: usize) {
            self.lock_reader().set_history_size(n);
        }
        /// Truncates history to the only the most recent `n` entries.
        pub fn truncate_history(&self, n: usize) {
            self.lock_reader().truncate_history(n);
        }
        /// Sets the syntax highlighter for the input line.
        pub fn set_highlighter(&mut self, highlighter: Arc<dyn Highlighter + Send + Sync>) {
            self.highlighter = Some(highlighter);
        }
    }
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

    pub mod c
    {
        use ::
        {
            time::{ OffsetDateTime },
            *,
        };

        #[derive(Debug, PartialEq, Eq)]
        pub struct DateTime
        {
            odt: OffsetDateTime,
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

            pub fn unix_timestamp(&self) -> f64
            {
                self.odt.unix_timestamp_nanos() as f64 / 1000000000.0
            }
        }

        impl fmt::Display for DateTime
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Displayed
            {
                write!(f, "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:03}",
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
}
/// A contiguous growable array type with heap-allocated contents, written Vec<T>.
pub mod vec
{
    pub use std::vec::{ * };
}

pub unsafe fn domain()
{
    use ::
    {
        command::{ Command },
        sync::{ Arc },
        terminal::{ highlighter, Interface },
        *,
    };

    env::initialize_pathing();

    let mut sh = shell::Shell::new();
    let args: Vec<String> = env::args().collect();

    if is::login(&args)
    {
        fs::load_rc_files(&mut sh);
        sh.is_login = true;
    }
    
    highlighter::init_command_cache();
    highlighter::update_aliases(&sh);

    if is::script(&args)
    {
        log!("run script: {:?} ", &args);
        let status = scripts::run( &mut sh, &args );
        process::exit(status);
    }

    if is::command_string(&args)
    {
        let line = env::args_to_command_line();
        log!("run with -c args: {}", &line);
        now::run_command_line(&mut sh, &line, false, false);
        process::exit(sh.previous_status);
    }

    if is::non_tty()
    {
        now::run_procedures_for_non_tty( &mut sh );
        return;
    }

    let mut rl;
    match Interface::new("cicada")
    {
        Ok(x) => rl = x,
        Err(e) =>
        {
            println!("cicada: lineread error: {}", e);
            return;
        }
    }

    rl.define_function("enter-function", Arc::new( prompt::EnterFunction ));
    rl.bind_sequence("\r", Command::from_str("enter-function"));

    let highlighter = highlighter::create_highlighter();
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
// 5739
