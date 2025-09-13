/*!
Cicada is a simple Unix shell written in Rust. */

#![allow
(
    dead_code,
    missing_abi,
    missing_docs, 
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    nonstandard_style,
    redundant_semicolons,
    unsafe_op_in_unsafe_fn,
    unused_imports,
)]

#[macro_use]
pub mod macros
{
    use ::
    {
        *,
    };
    
    /// A macro for defining #[cfg] if-else statements.
    macro_rules! cfg_if 
    {
        ($
        (
            if #[cfg($($meta:meta),*)] { $($it:item)* }
        ) else * else
        {
            $($it2:item)*
        }) =>
        {
            __cfg_if_items!
            {
                () ;
                $( ( ($($meta),*) ($($it)*) ), )*
                ( () ($($it2)*) ),
            }
        }
    }

    macro_rules! __cfg_if_items
    {
        (($($not:meta,)*) ; ) => {};
        (($($not:meta,)*) ; ( ($($m:meta),*) ($($it:item)*) ), $($rest:tt)*) =>
        {
            __cfg_if_apply! { cfg(all($($m,)* not(any($($not),*)))), $($it)* }
            __cfg_if_items! { ($($not,)* $($m,)*) ; $($rest)* }
        }
    }

    macro_rules! __cfg_if_apply
    {
        ($m:meta, $($it:item)*) => { $(#[$m] $it)* }
    }

    macro_rules! s
    {
        ($(pub struct $i:ident { $($field:tt)* })*) => 
        ($(
            __item!
            {
                #[repr(C)]
                pub struct $i { $($field)* }
            }
            
            impl Copy for $i {}
            impl Clone for $i
            { 
                fn clone(&self) -> $i { *self }
            }
        )*)
    }

    macro_rules! f
    {
        ($(pub fn $i:ident($($arg:ident: $argty:ty),*) -> $ret:ty 
        {
            $($body:stmt);*
        })*) => 
        ($(
            #[inline] pub unsafe extern fn $i($($arg: $argty),*) -> $ret
            {
                $($body);*
            }
        )*)
    }

    macro_rules! __item
    {
        ($i:item) => ($i)
    }
    
    macro_rules! println_stderr
    {
        ($fmt:expr) => (
            match writeln!(&mut ::io::stderr(), $fmt) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
        ($fmt:expr, $($arg:tt)*) => (
            match writeln!(&mut ::io::stderr(), $fmt, $($arg)*) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

    macro_rules! compile_error
    {
        ($msg:expr $(,)?) => {{ /* compiler built-in */ }};
    }

    macro_rules! is_aarch64_feature_detected
    {
        ($t: tt) => {
            compile_error!(
                r#"
            is_aarch64_feature_detected can only be used on AArch64 targets.
            You can prevent it from being used in other architectures by
            guarding it behind a cfg(target_arch) as follows:

                #[cfg(target_arch = "aarch64")] {
                    if is_aarch64_feature_detected(...) { ... }
                }
            "#
            )
        };
    }

    macro_rules! log {
        ($($tt:tt)*) => {
            #[cfg(feature = "logging")]
            {
                $($tt)*
            }
        }
    }

    macro_rules! debug {
        ($($tt:tt)*) => { log!(log::debug!($($tt)*)) }
    }

    macro_rules! trace {
        ($($tt:tt)*) => { log!(log::trace!($($tt)*)) }
    }

}

pub mod arch
{
    pub use std::arch::{*};
}

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod cmp
{
    pub use std::cmp::{ * };
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod env
{
    pub use std::env::{ * };
}

pub mod ffi
{
    pub use std::ffi::{*};
}

pub mod fmt
{
    pub use std::fmt::{*};
}

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod error
{
    pub use std::error::{ * };
}

pub mod io
{
    pub use std::io::{ * };
}

pub mod iter
{
    pub use std::iter::{*};
}

pub mod libc
{
    #[macro_use] mod macros
    {
        use ::
        {
            *,
        };
    }
    // Use repr(u8) as LLVM expects `void*` to be the same as `i8*` to help enable
    // more optimization opportunities around it recognizing things like
    // malloc/free.
    #[repr(u8)]
    pub enum c_void {
        // Two dummy variants so the #[repr] attribute can be used.
        #[doc(hidden)]
        __variant1,
        #[doc(hidden)]
        __variant2,
    }

    pub type int8_t = i8;
    pub type int16_t = i16;
    pub type int32_t = i32;
    pub type int64_t = i64;
    pub type uint8_t = u8;
    pub type uint16_t = u16;
    pub type uint32_t = u32;
    pub type uint64_t = u64;

    pub type c_schar = i8;
    pub type c_uchar = u8;
    pub type c_short = i16;
    pub type c_ushort = u16;
    pub type c_int = i32;
    pub type c_uint = u32;
    pub type c_float = f32;
    pub type c_double = f64;
    pub type c_longlong = i64;
    pub type c_ulonglong = u64;
    pub type intmax_t = i64;
    pub type uintmax_t = u64;

    pub type size_t = usize;
    pub type ptrdiff_t = isize;
    pub type intptr_t = isize;
    pub type uintptr_t = usize;
    pub type ssize_t = isize;

    pub enum FILE {}
    pub enum fpos_t {} // TODO: fill this out with a struct

    extern 
    {
        pub fn isalnum(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isalpha(c: ::libc::c_int) -> ::libc::c_int;
        pub fn iscntrl(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isdigit(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isgraph(c: ::libc::c_int) -> ::libc::c_int;
        pub fn islower(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isprint(c: ::libc::c_int) -> ::libc::c_int;
        pub fn ispunct(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isspace(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isupper(c: ::libc::c_int) -> ::libc::c_int;
        pub fn isxdigit(c: ::libc::c_int) -> ::libc::c_int;
        pub fn tolower(c: ::libc::c_int) -> ::libc::c_int;
        pub fn toupper(c: ::libc::c_int) -> ::libc::c_int;

        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "fopen$UNIX2003")]
        pub fn fopen(filename: *const c_char,
                    mode: *const c_char) -> *mut FILE;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "freopen$UNIX2003")]
        pub fn freopen(filename: *const c_char, mode: *const c_char,
                    file: *mut FILE) -> *mut FILE;
        pub fn fflush(file: *mut FILE) -> ::libc::c_int;
        pub fn fclose(file: *mut FILE) -> ::libc::c_int;
        pub fn remove(filename: *const c_char) -> ::libc::c_int;
        pub fn rename(oldname: *const c_char, newname: *const c_char) -> ::libc::c_int;
        pub fn tmpfile() -> *mut FILE;
        pub fn setvbuf(stream: *mut FILE,
                    buffer: *mut c_char,
                    mode: ::libc::c_int,
                    size: ::libc::size_t) -> ::libc::c_int;
        pub fn setbuf(stream: *mut FILE, buf: *mut c_char);
        pub fn fgetc(stream: *mut FILE) -> ::libc::c_int;
        pub fn fgets(buf: *mut c_char, n: ::libc::c_int, stream: *mut FILE) -> *mut c_char;
        pub fn fputc(c: ::libc::c_int, stream: *mut FILE) -> ::libc::c_int;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "fputs$UNIX2003")]
        pub fn fputs(s: *const c_char, stream: *mut FILE)-> ::libc::c_int;
        pub fn puts(s: *const c_char) -> ::libc::c_int;
        pub fn ungetc(c: ::libc::c_int, stream: *mut FILE) -> ::libc::c_int;
        pub fn fread(ptr: *mut c_void,
                    size: ::libc::size_t,
                    nobj: ::libc::size_t,
                    stream: *mut FILE)
                    -> size_t;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "fwrite$UNIX2003")]
        pub fn fwrite(ptr: *const c_void,
                    size: ::libc::size_t,
                    nobj: ::libc::size_t,
                    stream: *mut FILE)
                    -> size_t;
        pub fn fseek(stream: *mut FILE, offset: ::libc::c_long, whence: ::libc::c_int) -> ::libc::c_int;
        pub fn ftell(stream: *mut FILE) -> ::libc::c_long;
        pub fn rewind(stream: *mut FILE);
        pub fn fgetpos(stream: *mut FILE, ptr: *mut fpos_t) -> ::libc::c_int;
        pub fn fsetpos(stream: *mut FILE, ptr: *const fpos_t) -> ::libc::c_int;
        pub fn feof(stream: *mut FILE) -> ::libc::c_int;
        pub fn ferror(stream: *mut FILE) -> ::libc::c_int;
        pub fn perror(s: *const c_char);
        pub fn atoi(s: *const c_char) -> ::libc::c_int;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "strtod$UNIX2003")]
        pub fn strtod(s: *const c_char, endp: *mut *mut c_char) -> ::libc::c_double;
        pub fn strtol(s: *const c_char,
                    endp: *mut *mut c_char, base: ::libc::c_int) -> ::libc::c_long;
        pub fn strtoul(s: *const c_char, endp: *mut *mut c_char,
                    base: ::libc::c_int) -> ::libc::c_ulong;
        pub fn calloc(nobj: ::libc::size_t, size: ::libc::size_t) -> *mut c_void;
        pub fn malloc(size: ::libc::size_t) -> *mut c_void;
        pub fn realloc(p: *mut c_void, size: ::libc::size_t) -> *mut c_void;
        pub fn free(p: *mut c_void);
        pub fn exit(status: ::libc::c_int) -> !;
        pub fn _exit(status: ::libc::c_int) -> !;
        pub fn atexit(cb: extern fn()) -> ::libc::c_int;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "system$UNIX2003")]
        pub fn system(s: *const c_char) -> ::libc::c_int;
        pub fn getenv(s: *const c_char) -> *mut c_char;

        pub fn strcpy(dst: *mut c_char, src: *const c_char) -> *mut c_char;
        pub fn strncpy(dst: *mut c_char, src: *const c_char, n: ::libc::size_t)
                    -> *mut c_char;
        pub fn strcat(s: *mut c_char, ct: *const c_char) -> *mut c_char;
        pub fn strncat(s: *mut c_char, ct: *const c_char, n: ::libc::size_t) -> *mut c_char;
        pub fn strcmp(cs: *const c_char, ct: *const c_char) -> ::libc::c_int;
        pub fn strncmp(cs: *const c_char, ct: *const c_char, n: ::libc::size_t) -> ::libc::c_int;
        pub fn strcoll(cs: *const c_char, ct: *const c_char) -> ::libc::c_int;
        pub fn strchr(cs: *const c_char, c: ::libc::c_int) -> *mut c_char;
        pub fn strrchr(cs: *const c_char, c: ::libc::c_int) -> *mut c_char;
        pub fn strspn(cs: *const c_char, ct: *const c_char) -> size_t;
        pub fn strcspn(cs: *const c_char, ct: *const c_char) -> size_t;
        pub fn strpbrk(cs: *const c_char, ct: *const c_char) -> *mut c_char;
        pub fn strstr(cs: *const c_char, ct: *const c_char) -> *mut c_char;
        pub fn strlen(cs: *const c_char) -> size_t;
        #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                link_name = "strerror$UNIX2003")]
        pub fn strerror(n: ::libc::c_int) -> *mut c_char;
        pub fn strtok(s: *mut c_char, t: *const c_char) -> *mut c_char;
        pub fn strxfrm(s: *mut c_char, ct: *const c_char, n: ::libc::size_t) -> size_t;
        pub fn wcslen(buf: *const wchar_t) -> size_t;

        pub fn memcmp(cx: *const c_void, ct: *const c_void, n: ::libc::size_t) -> ::libc::c_int;
        pub fn memchr(cx: *const c_void, c: ::libc::c_int, n: ::libc::size_t) -> *mut c_void;
    }

    // These are all inline functions on android, so they end up just being entirely
    // missing on that platform.
    #[cfg(not(target_os = "android"))]
    extern 
    {
        pub fn abs(i: ::libc::c_int) -> ::libc::c_int;
        pub fn atof(s: *const c_char) -> ::libc::c_double;
        pub fn labs(i: ::libc::c_long) -> ::libc::c_long;
        pub fn rand() -> ::libc::c_int;
        pub fn srand(seed: ::libc::c_uint);
    }

    cfg_if! 
    {
        if #[cfg(windows)] 
        {
            mod windows
            {
                //! Windows CRT definitions
                use ::
                {
                    *
                };
                                
                pub type c_char = i8;
                pub type c_long = i32;
                pub type c_ulong = u32;
                pub type wchar_t = u16;

                pub type clock_t = i32;

                cfg_if!
                {
                    if #[cfg(all(target_arch = "x86", target_env = "gnu"))] { pub type time_t = i32; }				
                    else { pub type time_t = i64; }
                }

                pub type off_t = i32;
                pub type dev_t = u32;
                pub type ino_t = u16;
                pub enum timezone {}
                pub type time64_t = i64;

                s!
                {
                    pub struct stat
                    {
                        pub st_dev: dev_t,
                        pub st_ino: ino_t,
                        pub st_mode: u16,
                        pub st_nlink: ::libc::c_short,
                        pub st_uid: ::libc::c_short,
                        pub st_gid: ::libc::c_short,
                        pub st_rdev: dev_t,
                        pub st_size: i64,
                        pub st_atime: time64_t,
                        pub st_mtime: time64_t,
                        pub st_ctime: time64_t,
                    }
                    
                    pub struct utimbuf
                    {
                        pub actime: time64_t,
                        pub modtime: time64_t,
                    }

                    pub struct timeval
                    {
                        pub tv_sec: ::libc::c_long,
                        pub tv_usec: ::libc::c_long,
                    }

                    pub struct timespec
                    {
                        pub tv_sec: time_t,
                        pub tv_nsec: ::libc::c_long,
                    }
                }

                pub const EXIT_FAILURE: ::libc::c_int = 1;
                pub const EXIT_SUCCESS: ::libc::c_int = 0;
                pub const RAND_MAX: ::libc::c_int = 32767;
                pub const EOF: ::libc::c_int = -1;
                pub const SEEK_SET: ::libc::c_int = 0;
                pub const SEEK_CUR: ::libc::c_int = 1;
                pub const SEEK_END: ::libc::c_int = 2;
                pub const _IOFBF: ::libc::c_int = 0;
                pub const _IONBF: ::libc::c_int = 4;
                pub const _IOLBF: ::libc::c_int = 64;
                pub const BUFSIZ: ::libc::c_uint = 512;
                pub const FOPEN_MAX: ::libc::c_uint = 20;
                pub const FILENAME_MAX: ::libc::c_uint = 260;

                cfg_if!
                {
                    if #[cfg(all(target_env = "gnu"))]
                    {
                        pub const L_tmpnam: ::libc::c_uint = 14;
                        pub const TMP_MAX: ::libc::c_uint = 0x7fff;
                    }
                    
                    else
                    {
                        pub const L_tmpnam: ::libc::c_uint = 260;
                        pub const TMP_MAX: ::libc::c_uint = 0x7fff_ffff;
                    }
                }

                pub const O_RDONLY: ::libc::c_int = 0;
                pub const O_WRONLY: ::libc::c_int = 1;
                pub const O_RDWR: ::libc::c_int = 2;
                pub const O_APPEND: ::libc::c_int = 8;
                pub const O_CREAT: ::libc::c_int = 256;
                pub const O_EXCL: ::libc::c_int = 1024;
                pub const O_TEXT: ::libc::c_int = 16384;
                pub const O_BINARY: ::libc::c_int = 32768;
                pub const O_NOINHERIT: ::libc::c_int = 128;
                pub const O_TRUNC: ::libc::c_int = 512;
                pub const S_IFCHR: ::libc::c_int = 8192;
                pub const S_IFDIR: ::libc::c_int = 16384;
                pub const S_IFREG: ::libc::c_int = 32768;
                pub const S_IFMT: ::libc::c_int = 61440;
                pub const S_IEXEC: ::libc::c_int = 64;
                pub const S_IWRITE: ::libc::c_int = 128;
                pub const S_IREAD: ::libc::c_int = 256;

                #[cfg(target_env = "msvc")]
                #[link(name = "msvcrt")]
                extern {}

                extern
                {
                    #[link_name = "_chmod"]
                    pub fn chmod(path: *const c_char, mode: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_wchmod"]
                    pub fn wchmod(path: *const wchar_t, mode: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_mkdir"]
                    pub fn mkdir(path: *const c_char) -> ::libc::c_int;
                    #[link_name = "_wrmdir"]
                    pub fn wrmdir(path: *const wchar_t) -> ::libc::c_int;
                    #[link_name = "_fstat64"]
                    pub fn fstat(fildes: ::libc::c_int, buf: *mut stat) -> ::libc::c_int;
                    #[link_name = "_stat64"]
                    pub fn stat(path: *const c_char, buf: *mut stat) -> ::libc::c_int;
                    #[link_name = "_wstat64"]
                    pub fn wstat(path: *const wchar_t, buf: *mut stat) -> ::libc::c_int;
                    #[link_name = "_wutime64"]
                    pub fn wutime(file: *const wchar_t, buf: *mut ::libc::utimbuf) -> ::libc::c_int;
                    #[link_name = "_popen"]
                    pub fn popen(command: *const c_char, mode: *const c_char) -> *mut ::libc::FILE;
                    #[link_name = "_pclose"]
                    pub fn pclose(stream: *mut ::libc::FILE) -> ::libc::c_int;
                    #[link_name = "_fdopen"]
                    pub fn fdopen(fd: ::libc::c_int, mode: *const c_char) -> *mut ::libc::FILE;
                    #[link_name = "_fileno"]
                    pub fn fileno(stream: *mut ::libc::FILE) -> ::libc::c_int;
                    #[link_name = "_open"]
                    pub fn open(path: *const c_char, oflag: ::libc::c_int, ...) -> ::libc::c_int;
                    #[link_name = "_wopen"]
                    pub fn wopen(path: *const wchar_t, oflag: ::libc::c_int, ...) -> ::libc::c_int;
                    #[link_name = "_creat"]
                    pub fn creat(path: *const c_char, mode: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_access"]
                    pub fn access(path: *const c_char, amode: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_chdir"]
                    pub fn chdir(dir: *const c_char) -> ::libc::c_int;
                    #[link_name = "_close"]
                    pub fn close(fd: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_dup"]
                    pub fn dup(fd: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_dup2"]
                    pub fn dup2(src: ::libc::c_int, dst: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_execv"]
                    pub fn execv(prog: *const c_char, argv: *const *const c_char) -> ::intptr_t;
                    #[link_name = "_execve"]
                    pub fn execve(prog: *const c_char, argv: *const *const c_char,
                                envp: *const *const c_char) -> ::libc::c_int;
                    #[link_name = "_execvp"]
                    pub fn execvp(c: *const c_char, argv: *const *const c_char) -> ::libc::c_int;
                    #[link_name = "_execvpe"]
                    pub fn execvpe(c: *const c_char, argv: *const *const c_char,
                                envp: *const *const c_char) -> ::libc::c_int;
                    #[link_name = "_getcwd"]
                    pub fn getcwd(buf: *mut c_char, size: ::libc::c_int) -> *mut c_char;
                    #[link_name = "_getpid"]
                    pub fn getpid() -> ::libc::c_int;
                    #[link_name = "_isatty"]
                    pub fn isatty(fd: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_lseek"]
                    pub fn lseek(fd: ::libc::c_int, offset: ::libc::c_long, origin: ::libc::c_int) -> ::libc::c_long;
                    #[link_name = "_pipe"]
                    pub fn pipe(fds: *mut ::libc::c_int, psize: ::libc::c_uint, textmode: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_read"]
                    pub fn read(fd: ::libc::c_int, buf: *mut ::libc::c_void, count: ::libc::c_uint) -> ::libc::c_int;
                    #[link_name = "_rmdir"]
                    pub fn rmdir(path: *const c_char) -> ::libc::c_int;
                    #[link_name = "_unlink"]
                    pub fn unlink(c: *const c_char) -> ::libc::c_int;
                    #[link_name = "_write"]
                    pub fn write(fd: ::libc::c_int, buf: *const ::libc::c_void, count: ::libc::c_uint) -> ::libc::c_int;
                    #[link_name = "_commit"]
                    pub fn commit(fd: ::libc::c_int) -> ::libc::c_int;
                    #[link_name = "_get_osfhandle"]
                    pub fn get_osfhandle(fd: ::libc::c_int) -> ::intptr_t;
                    #[link_name = "_open_osfhandle"]
                    pub fn open_osfhandle(osfhandle: ::libc::intptr_t, flags: ::libc::c_int) -> ::libc::c_int;
                }

            }
            
            pub use self::windows::*;
        }
        
        else
        {
            mod unix
            {
                //! Definitions found commonly among almost all Unix derivatives.
                use ::
                {
                    *
                };
                
                pub type pid_t = i32;
                pub type uid_t = u32;
                pub type gid_t = u32;
                pub type in_addr_t = u32;
                pub type in_port_t = u16;
                pub type sighandler_t = ::libc::size_t;

                pub enum DIR {}

                s!
                {
                    pub struct utimbuf {
                        pub actime: time_t,
                        pub modtime: time_t,
                    }

                    pub struct timeval {
                        pub tv_sec: time_t,
                        pub tv_usec: suseconds_t,
                    }

                    pub struct timespec {
                        pub tv_sec: time_t,
                        pub tv_nsec: ::libc::c_long,
                    }

                    pub struct rlimit {
                        pub rlim_cur: rlim_t,
                        pub rlim_max: rlim_t,
                    }

                    pub struct rusage {
                        pub ru_utime: timeval,
                        pub ru_stime: timeval,
                        pub ru_maxrss: ::libc::c_long,
                        pub ru_ixrss: ::libc::c_long,
                        pub ru_idrss: ::libc::c_long,
                        pub ru_isrss: ::libc::c_long,
                        pub ru_minflt: ::libc::c_long,
                        pub ru_majflt: ::libc::c_long,
                        pub ru_nswap: ::libc::c_long,
                        pub ru_inblock: ::libc::c_long,
                        pub ru_oublock: ::libc::c_long,
                        pub ru_msgsnd: ::libc::c_long,
                        pub ru_msgrcv: ::libc::c_long,
                        pub ru_nsignals: ::libc::c_long,
                        pub ru_nvcsw: ::libc::c_long,
                        pub ru_nivcsw: ::libc::c_long,

                        #[cfg(target_env = "musl")]
                        __reserved: [c_long; 16],
                    }

                    pub struct in_addr {
                        pub s_addr: in_addr_t,
                    }

                    pub struct in6_addr {
                        pub s6_addr: [u8; 16],
                        __align: [u32; 0],
                    }

                    pub struct ip_mreq {
                        pub imr_multiaddr: in_addr,
                        pub imr_interface: in_addr,
                    }

                    pub struct ipv6_mreq {
                        pub ipv6mr_multiaddr: in6_addr,
                        #[cfg(target_os = "android")]
                        pub ipv6mr_interface: ::libc::c_int,
                        #[cfg(not(target_os = "android"))]
                        pub ipv6mr_interface: ::libc::c_uint,
                    }

                    pub struct Dl_info {
                        pub dli_fname: *const ::libc::c_char,
                        pub dli_fbase: *mut ::libc::c_void,
                        pub dli_sname: *const ::libc::c_char,
                        pub dli_saddr: *mut ::libc::c_void,
                    }
                }

                pub const WNOHANG: ::libc::c_int = 1;
                pub const SIG_DFL: sighandler_t = 0 as sighandler_t;
                pub const SIG_IGN: sighandler_t = 1 as sighandler_t;
                pub const SIG_ERR: sighandler_t = !0 as sighandler_t;

                cfg_if!
                {
                    if #[cfg(feature = "default")] {
                        // cargo build, don't pull in anything extra as the libstd  dep
                        // already pulls in all libs.
                    } else if #[cfg(target_env = "musl")] {
                        #[link(name = "c", kind = "static")]
                        extern {}
                    } else {
                        #[link(name = "c")]
                        #[link(name = "m")]
                        extern {}
                    }
                }

                extern
                {
                    pub fn socket(domain: ::libc::c_int, ty: ::libc::c_int, protocol: ::libc::c_int) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "connect$UNIX2003")]
                    pub fn connect(socket: ::libc::c_int, address: *const sockaddr,
                                len: socklen_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "bind$UNIX2003")]
                    pub fn bind(socket: ::libc::c_int, address: *const sockaddr,
                                address_len: socklen_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "listen$UNIX2003")]
                    pub fn listen(socket: ::libc::c_int, backlog: ::libc::c_int) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "accept$UNIX2003")]
                    pub fn accept(socket: ::libc::c_int, address: *mut sockaddr,
                                address_len: *mut socklen_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "getpeername$UNIX2003")]
                    pub fn getpeername(socket: ::libc::c_int, address: *mut sockaddr,
                                    address_len: *mut socklen_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "getsockname$UNIX2003")]
                    pub fn getsockname(socket: ::libc::c_int, address: *mut sockaddr,
                                    address_len: *mut socklen_t) -> ::libc::c_int;
                    pub fn setsockopt(socket: ::libc::c_int, level: ::libc::c_int, name: ::libc::c_int,
                                    value: *const ::libc::c_void,
                                    option_len: socklen_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "sendto$UNIX2003")]
                    pub fn sendto(socket: ::libc::c_int, buf: *const ::libc::c_void, len: ::libc::size_t,
                                flags: ::libc::c_int, addr: *const sockaddr,
                                addrlen: socklen_t) -> ::libc::ssize_t;
                    pub fn shutdown(socket: ::libc::c_int, how: ::libc::c_int) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "chmod$UNIX2003")]
                    pub fn chmod(path: *const c_char, mode: mode_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "fchmod$UNIX2003")]
                    pub fn fchmod(fd: ::libc::c_int, mode: mode_t) -> ::libc::c_int;

                    #[cfg_attr(target_os = "macos", link_name = "fstat$INODE64")]
                    pub fn fstat(fildes: ::libc::c_int, buf: *mut stat) -> ::libc::c_int;

                    pub fn mkdir(path: *const c_char, mode: mode_t) -> ::libc::c_int;

                    #[cfg_attr(target_os = "macos", link_name = "stat$INODE64")]
                    pub fn stat(path: *const c_char, buf: *mut stat) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "popen$UNIX2003")]
                    pub fn popen(command: *const c_char,
                                mode: *const c_char) -> *mut ::libc::FILE;
                    pub fn pclose(stream: *mut ::libc::FILE) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "fdopen$UNIX2003")]
                    pub fn fdopen(fd: ::libc::c_int, mode: *const c_char) -> *mut ::libc::FILE;
                    pub fn fileno(stream: *mut ::libc::FILE) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "open$UNIX2003")]
                    pub fn open(path: *const c_char, oflag: ::libc::c_int, ...) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "creat$UNIX2003")]
                    pub fn creat(path: *const c_char, mode: mode_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "fcntl$UNIX2003")]
                    pub fn fcntl(fd: ::libc::c_int, cmd: ::libc::c_int, ...) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "opendir$INODE64")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "opendir$INODE64$UNIX2003")]
                    pub fn opendir(dirname: *const c_char) -> *mut ::libc::DIR;
                    #[cfg_attr(target_os = "macos", link_name = "readdir_r$INODE64")]
                    pub fn readdir_r(dirp: *mut ::libc::DIR, entry: *mut ::libc::dirent,
                                    result: *mut *mut ::libc::dirent) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "closedir$UNIX2003")]
                    pub fn closedir(dirp: *mut ::libc::DIR) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "rewinddir$INODE64")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "rewinddir$INODE64$UNIX2003")]
                    pub fn rewinddir(dirp: *mut ::libc::DIR);

                    pub fn access(path: *const c_char, amode: ::libc::c_int) -> ::libc::c_int;
                    pub fn alarm(seconds: ::libc::c_uint) -> ::libc::c_uint;
                    pub fn chdir(dir: *const c_char) -> ::libc::c_int;
                    pub fn chown(path: *const c_char, uid: uid_t,
                                gid: gid_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "close$UNIX2003")]
                    pub fn close(fd: ::libc::c_int) -> ::libc::c_int;
                    pub fn dup(fd: ::libc::c_int) -> ::libc::c_int;
                    pub fn dup2(src: ::libc::c_int, dst: ::libc::c_int) -> ::libc::c_int;
                    pub fn execv(prog: *const c_char,
                                argv: *const *const c_char) -> ::libc::c_int;
                    pub fn execve(prog: *const c_char, argv: *const *const c_char,
                                envp: *const *const c_char)
                                -> ::libc::c_int;
                    pub fn execvp(c: *const c_char,
                                argv: *const *const c_char) -> ::libc::c_int;
                    pub fn fork() -> pid_t;
                    pub fn fpathconf(filedes: ::libc::c_int, name: ::libc::c_int) -> ::libc::c_long;
                    pub fn getcwd(buf: *mut c_char, size: ::libc::size_t) -> *mut c_char;
                    pub fn getegid() -> gid_t;
                    pub fn geteuid() -> uid_t;
                    pub fn getgid() -> gid_t;
                    pub fn getgroups(ngroups_max: ::libc::c_int, groups: *mut gid_t)
                                    -> ::libc::c_int;
                    pub fn getlogin() -> *mut c_char;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "getopt$UNIX2003")]
                    pub fn getopt(argc: ::libc::c_int, argv: *const *mut c_char,
                                optstr: *const c_char) -> ::libc::c_int;
                    pub fn getpgrp() -> pid_t;
                    pub fn getpid() -> pid_t;
                    pub fn getppid() -> pid_t;
                    pub fn getuid() -> uid_t;
                    pub fn isatty(fd: ::libc::c_int) -> ::libc::c_int;
                    pub fn link(src: *const c_char, dst: *const c_char) -> ::libc::c_int;
                    pub fn lseek(fd: ::libc::c_int, offset: off_t, whence: ::libc::c_int) -> off_t;
                    pub fn pathconf(path: *const c_char, name: ::libc::c_int) -> ::libc::c_long;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pause$UNIX2003")]
                    pub fn pause() -> ::libc::c_int;
                    pub fn pipe(fds: *mut ::libc::c_int) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "read$UNIX2003")]
                    pub fn read(fd: ::libc::c_int, buf: *mut ::libc::c_void, count: ::libc::size_t)
                                -> ::libc::ssize_t;
                    pub fn rmdir(path: *const c_char) -> ::libc::c_int;
                    pub fn setgid(gid: gid_t) -> ::libc::c_int;
                    pub fn setpgid(pid: pid_t, pgid: pid_t) -> ::libc::c_int;
                    pub fn setsid() -> pid_t;
                    pub fn setuid(uid: uid_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "sleep$UNIX2003")]
                    pub fn sleep(secs: ::libc::c_uint) -> ::libc::c_uint;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "nanosleep$UNIX2003")]
                    pub fn nanosleep(rqtp: *const timespec,
                                    rmtp: *mut timespec) -> ::libc::c_int;
                    pub fn tcgetpgrp(fd: ::libc::c_int) -> pid_t;
                    pub fn ttyname(fd: ::libc::c_int) -> *mut c_char;
                    pub fn unlink(c: *const c_char) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "wait$UNIX2003")]
                    pub fn wait(status: *mut ::libc::c_int) -> pid_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "waitpid$UNIX2003")]
                    pub fn waitpid(pid: pid_t, status: *mut ::libc::c_int, options: ::libc::c_int)
                                -> pid_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "write$UNIX2003")]
                    pub fn write(fd: ::libc::c_int, buf: *const ::libc::c_void, count: ::libc::size_t)
                                -> ::libc::ssize_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pread$UNIX2003")]
                    pub fn pread(fd: ::libc::c_int, buf: *mut ::libc::c_void, count: ::libc::size_t,
                                offset: off_t) -> ::libc::ssize_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pwrite$UNIX2003")]
                    pub fn pwrite(fd: ::libc::c_int, buf: *const ::libc::c_void, count: ::libc::size_t,
                                offset: off_t) -> ::libc::ssize_t;
                    pub fn utime(file: *const c_char, buf: *const ::libc::utimbuf) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                                link_name = "kill$UNIX2003")]
                    pub fn kill(pid: pid_t, sig: ::libc::c_int) -> ::libc::c_int;

                    pub fn mlock(addr: *const ::libc::c_void, len: ::libc::size_t) -> ::libc::c_int;
                    pub fn munlock(addr: *const ::libc::c_void, len: ::libc::size_t) -> ::libc::c_int;
                    pub fn mlockall(flags: ::libc::c_int) -> ::libc::c_int;
                    pub fn munlockall() -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "mmap$UNIX2003")]
                    pub fn mmap(addr: *mut ::libc::c_void,
                                len: ::libc::size_t,
                                prot: ::libc::c_int,
                                flags: ::libc::c_int,
                                fd: ::libc::c_int,
                                offset: off_t)
                                -> *mut ::libc::c_void;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "munmap$UNIX2003")]
                    pub fn munmap(addr: *mut ::libc::c_void, len: ::libc::size_t) -> ::libc::c_int;

                    pub fn if_nametoindex(ifname: *const c_char) -> ::libc::c_uint;

                    #[cfg_attr(target_os = "macos", link_name = "lstat$INODE64")]
                    pub fn lstat(path: *const c_char, buf: *mut stat) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "fsync$UNIX2003")]
                    pub fn fsync(fd: ::libc::c_int) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "setenv$UNIX2003")]
                    pub fn setenv(name: *const c_char, val: *const c_char,
                                overwrite: ::libc::c_int) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "unsetenv$UNIX2003")]
                    pub fn unsetenv(name: *const c_char) -> ::libc::c_int;

                    pub fn symlink(path1: *const c_char,
                                path2: *const c_char) -> ::libc::c_int;

                    pub fn ftruncate(fd: ::libc::c_int, length: off_t) -> ::libc::c_int;

                    #[cfg_attr(target_os = "android", link_name = "bsd_signal")]
                    pub fn signal(signum: ::libc::c_int, handler: sighandler_t) -> sighandler_t;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "getrlimit$UNIX2003")]
                    pub fn getrlimit(resource: ::libc::c_int, rlim: *mut rlimit) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "setrlimit$UNIX2003")]
                    pub fn setrlimit(resource: ::libc::c_int, rlim: *const rlimit) -> ::libc::c_int;
                    pub fn getrusage(resource: ::libc::c_int, usage: *mut rusage) -> ::libc::c_int;

                    pub fn getdtablesize() -> ::libc::c_int;
                    #[cfg_attr(any(target_os = "macos", target_os = "ios"),
                            link_name = "realpath$DARWIN_EXTSN")]
                    pub fn realpath(pathname: *const ::libc::c_char, resolved: *mut ::libc::c_char)
                                    -> *mut ::libc::c_char;

                    pub fn flock(fd: ::libc::c_int, operation: ::libc::c_int) -> ::libc::c_int;

                    pub fn gettimeofday(tp: *mut ::libc::timeval,
                                        tz: *mut ::libc::c_void) -> ::libc::c_int;

                    pub fn pthread_self() -> ::libc::pthread_t;
                    pub fn pthread_create(native: *mut ::libc::pthread_t,
                                        attr: *const ::libc::pthread_attr_t,
                                        f: extern fn(*mut ::libc::c_void) -> *mut ::libc::c_void,
                                        value: *mut ::libc::c_void) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_join$UNIX2003")]
                    pub fn pthread_join(native: ::libc::pthread_t,
                                        value: *mut *mut ::libc::c_void) -> ::libc::c_int;
                    pub fn pthread_attr_init(attr: *mut ::libc::pthread_attr_t) -> ::libc::c_int;
                    pub fn pthread_attr_destroy(attr: *mut ::libc::pthread_attr_t) -> ::libc::c_int;
                    pub fn pthread_attr_setstacksize(attr: *mut ::libc::pthread_attr_t,
                                                    stack_size: ::libc::size_t) -> ::libc::c_int;
                    pub fn pthread_attr_setdetachstate(attr: *mut ::libc::pthread_attr_t,
                                                    state: ::libc::c_int) -> ::libc::c_int;
                    pub fn pthread_detach(thread: ::libc::pthread_t) -> ::libc::c_int;
                    pub fn sched_yield() -> ::libc::c_int;
                    pub fn pthread_key_create(key: *mut pthread_key_t,
                                            dtor: Option<unsafe extern fn(*mut ::libc::c_void)>)
                                            -> ::libc::c_int;
                    pub fn pthread_key_delete(key: pthread_key_t) -> ::libc::c_int;
                    pub fn pthread_getspecific(key: pthread_key_t) -> *mut ::libc::c_void;
                    pub fn pthread_setspecific(key: pthread_key_t, value: *const ::libc::c_void)
                                            -> ::libc::c_int;
                    pub fn pthread_mutex_init(lock: *mut pthread_mutex_t,
                                            attr: *const pthread_mutexattr_t) -> ::libc::c_int;
                    pub fn pthread_mutex_destroy(lock: *mut pthread_mutex_t) -> ::libc::c_int;
                    pub fn pthread_mutex_lock(lock: *mut pthread_mutex_t) -> ::libc::c_int;
                    pub fn pthread_mutex_trylock(lock: *mut pthread_mutex_t) -> ::libc::c_int;
                    pub fn pthread_mutex_unlock(lock: *mut pthread_mutex_t) -> ::libc::c_int;

                    pub fn pthread_mutexattr_init(attr: *mut pthread_mutexattr_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_mutexattr_destroy$UNIX2003")]
                    pub fn pthread_mutexattr_destroy(attr: *mut pthread_mutexattr_t) -> ::libc::c_int;
                    pub fn pthread_mutexattr_settype(attr: *mut pthread_mutexattr_t,
                                                    _type: ::libc::c_int) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_cond_wait$UNIX2003")]
                    pub fn pthread_cond_wait(cond: *mut pthread_cond_t,
                                            lock: *mut pthread_mutex_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_cond_timedwait$UNIX2003")]
                    pub fn pthread_cond_timedwait(cond: *mut pthread_cond_t,
                                            lock: *mut pthread_mutex_t,
                                            abstime: *const ::libc::timespec) -> ::libc::c_int;
                    pub fn pthread_cond_signal(cond: *mut pthread_cond_t) -> ::libc::c_int;
                    pub fn pthread_cond_broadcast(cond: *mut pthread_cond_t) -> ::libc::c_int;
                    pub fn pthread_cond_destroy(cond: *mut pthread_cond_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_destroy$UNIX2003")]
                    pub fn pthread_rwlock_destroy(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_rdlock$UNIX2003")]
                    pub fn pthread_rwlock_rdlock(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_tryrdlock$UNIX2003")]
                    pub fn pthread_rwlock_tryrdlock(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_wrlock$UNIX2003")]
                    pub fn pthread_rwlock_wrlock(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_trywrlock$UNIX2003")]
                    pub fn pthread_rwlock_trywrlock(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_rwlock_unlock$UNIX2003")]
                    pub fn pthread_rwlock_unlock(lock: *mut pthread_rwlock_t) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pthread_sigmask$UNIX2003")]
                    pub fn pthread_sigmask(how: ::libc::c_int, set: *const sigset_t,
                                        oldset: *mut sigset_t) -> ::libc::c_int;

                    // #[cfg_attr(target_os = "linux", link_name = "__xpg_strerror_r")]
                    pub fn strerror_r(errnum: ::libc::c_int, buf: *mut c_char,
                                    buflen: ::libc::size_t) -> ::libc::c_int;

                    pub fn getsockopt(sockfd: ::libc::c_int,
                                    level: ::libc::c_int,
                                    optname: ::libc::c_int,
                                    optval: *mut ::libc::c_void,
                                    optlen: *mut ::libc::socklen_t) -> ::libc::c_int;
                    pub fn raise(signum: ::libc::c_int) -> ::libc::c_int;
                    pub fn sigaction(signum: ::libc::c_int,
                                    act: *const sigaction,
                                    oldact: *mut sigaction) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "sigaltstack$UNIX2003")]
                    pub fn sigaltstack(ss: *const stack_t,
                                    oss: *mut stack_t) -> ::libc::c_int;

                    pub fn utimes(filename: *const ::libc::c_char,
                                times: *const ::libc::timeval) -> ::libc::c_int;
                    pub fn dlopen(filename: *const ::libc::c_char,
                                flag: ::libc::c_int) -> *mut ::libc::c_void;
                    pub fn dlerror() -> *mut ::libc::c_char;
                    pub fn dlsym(handle: *mut ::libc::c_void,
                                symbol: *const ::libc::c_char) -> *mut ::libc::c_void;
                    pub fn dlclose(handle: *mut ::libc::c_void) -> ::libc::c_int;
                    pub fn dladdr(addr: *const ::libc::c_void, info: *mut Dl_info) -> ::libc::c_int;

                    pub fn getaddrinfo(node: *const c_char,
                                    service: *const c_char,
                                    hints: *const addrinfo,
                                    res: *mut *mut addrinfo) -> ::libc::c_int;
                    pub fn freeaddrinfo(res: *mut addrinfo);
                    pub fn gai_strerror(errcode: ::libc::c_int) -> *const ::libc::c_char;
                }

                #[cfg(not(target_os = "android"))]
                extern
                {
                    pub fn getifaddrs(ifap: *mut *mut ifaddrs) -> ::libc::c_int;
                    pub fn freeifaddrs(ifa: *mut ifaddrs);
                    #[cfg_attr(target_os = "macos", link_name = "glob$INODE64")]
                    pub fn glob(pattern: *const c_char,
                                flags: ::libc::c_int,
                                errfunc: Option<extern "C" fn(epath: *const c_char,
                                                                    errno: ::libc::c_int) -> ::libc::c_int>,
                                pglob: *mut glob_t) -> ::libc::c_int;
                    pub fn globfree(pglob: *mut glob_t);

                    pub fn posix_madvise(addr: *mut ::libc::c_void, len: ::libc::size_t, advice: ::libc::c_int)
                                        -> ::libc::c_int;

                    pub fn shm_unlink(name: *const c_char) -> ::libc::c_int;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "seekdir$INODE64")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "seekdir$INODE64$UNIX2003")]
                    pub fn seekdir(dirp: *mut ::libc::DIR, loc: ::libc::c_long);

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "telldir$INODE64")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "telldir$INODE64$UNIX2003")]
                    pub fn telldir(dirp: *mut ::libc::DIR) -> ::libc::c_long;

                    pub fn getsid(pid: pid_t) -> pid_t;
                    pub fn madvise(addr: *mut ::libc::c_void, len: ::libc::size_t, advice: ::libc::c_int)
                                -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "putenv$UNIX2003")]
                    pub fn putenv(string: *mut c_char) -> ::libc::c_int;
                    pub fn readlink(path: *const c_char,
                                    buf: *mut c_char,
                                    bufsz: ::libc::size_t)
                                    -> ::libc::ssize_t;

                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "msync$UNIX2003")]
                    pub fn msync(addr: *mut ::libc::c_void, len: ::libc::size_t, flags: ::libc::c_int) -> ::libc::c_int;
                    pub fn sysconf(name: ::libc::c_int) -> ::libc::c_long;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "usleep$UNIX2003")]
                    pub fn usleep(secs: ::libc::c_uint) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "recvfrom$UNIX2003")]
                    pub fn recvfrom(socket: ::libc::c_int, buf: *mut ::libc::c_void, len: ::libc::size_t,
                                    flags: ::libc::c_int, addr: *mut sockaddr,
                                    addrlen: *mut socklen_t) -> ::libc::ssize_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "send$UNIX2003")]
                    pub fn send(socket: ::libc::c_int, buf: *const ::libc::c_void, len: ::libc::size_t,
                                flags: ::libc::c_int) -> ::libc::ssize_t;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "recv$UNIX2003")]
                    pub fn recv(socket: ::libc::c_int, buf: *mut ::libc::c_void, len: ::libc::size_t,
                                flags: ::libc::c_int) -> ::libc::ssize_t;
                    pub fn mkfifo(path: *const c_char, mode: mode_t) -> ::libc::c_int;

                    pub fn getpwuid_r(uid: ::libc::uid_t,
                                    pwd: *mut passwd,
                                    buf: *mut ::libc::c_char,
                                    buflen: ::libc::size_t,
                                    result: *mut *mut passwd) -> ::libc::c_int;
                    pub fn posix_memalign(memptr: *mut *mut ::libc::c_void,
                                        align: ::libc::size_t,
                                        size: ::libc::size_t) -> ::libc::c_int;
                    pub fn sigemptyset(set: *mut sigset_t) -> ::libc::c_int;
                    pub fn sigaddset(set: *mut sigset_t, signum: ::libc::c_int) -> ::libc::c_int;
                    pub fn sigfillset(set: *mut sigset_t) -> ::libc::c_int;
                    pub fn sigdelset(set: *mut sigset_t, signum: ::libc::c_int) -> ::libc::c_int;
                    pub fn sigismember(set: *const sigset_t, signum: ::libc::c_int) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "select$1050")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "select$UNIX2003")]
                    pub fn select(nfds: ::libc::c_int,
                                readfs: *mut fd_set,
                                writefds: *mut fd_set,
                                errorfds: *mut fd_set,
                                timeout: *mut timeval) -> ::libc::c_int;
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86_64"),
                            link_name = "pselect$1050")]
                    #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                            link_name = "pselect$UNIX2003")]
                    pub fn pselect(nfds: ::libc::c_int,
                                readfs: *mut fd_set,
                                writefds: *mut fd_set,
                                errorfds: *mut fd_set,
                                timeout: *const timespec,
                                sigmask: *const sigset_t) -> ::libc::c_int;
                    pub fn fseeko(stream: *mut ::libc::FILE,
                                offset: ::libc::off_t,
                                whence: ::libc::c_int) -> ::libc::c_int;
                    pub fn ftello(stream: *mut ::libc::FILE) -> ::libc::off_t;
                }

                cfg_if!
                {
                    if #[cfg(any(target_os = "linux", target_os = "android"))]
                    {
                        mod notbsd
                        {
                            use ::
                            {
                                *,
                            };
                            
                            pub type rlim_t = c_ulong;
                            pub type sa_family_t = u16;
                            pub type pthread_key_t = ::libc::c_uint;

                            pub enum timezone {}

                            s! {
                                pub struct sockaddr {
                                    pub sa_family: sa_family_t,
                                    pub sa_data: [::libc::c_char; 14],
                                }

                                pub struct sockaddr_in {
                                    pub sin_family: sa_family_t,
                                    pub sin_port: ::libc::in_port_t,
                                    pub sin_addr: ::libc::in_addr,
                                    pub sin_zero: [u8; 8],
                                }

                                pub struct sockaddr_in6 {
                                    pub sin6_family: sa_family_t,
                                    pub sin6_port: ::libc::in_port_t,
                                    pub sin6_flowinfo: u32,
                                    pub sin6_addr: ::libc::in6_addr,
                                    pub sin6_scope_id: u32,
                                }

                                pub struct sockaddr_un {
                                    pub sun_family: sa_family_t,
                                    pub sun_path: [::libc::c_char; 108]
                                }

                                pub struct sockaddr_storage {
                                    pub ss_family: sa_family_t,
                                    __ss_align: ::libc::size_t,
                                    #[cfg(target_pointer_width = "32")]
                                    __ss_pad2: [u8; 128 - 2 * 4],
                                    #[cfg(target_pointer_width = "64")]
                                    __ss_pad2: [u8; 128 - 2 * 8],
                                }

                                pub struct addrinfo {
                                    pub ai_flags: ::libc::c_int,
                                    pub ai_family: ::libc::c_int,
                                    pub ai_socktype: ::libc::c_int,
                                    pub ai_protocol: ::libc::c_int,
                                    pub ai_addrlen: socklen_t,

                                    #[cfg(target_os = "linux")]
                                    pub ai_addr: *mut ::libc::sockaddr,

                                    pub ai_canonname: *mut c_char,

                                    #[cfg(target_os = "android")]
                                    pub ai_addr: *mut ::libc::sockaddr,

                                    pub ai_next: *mut addrinfo,
                                }

                                pub struct sockaddr_ll {
                                    pub sll_family: ::libc::c_ushort,
                                    pub sll_protocol: ::libc::c_ushort,
                                    pub sll_ifindex: ::libc::c_int,
                                    pub sll_hatype: ::libc::c_ushort,
                                    pub sll_pkttype: ::libc::c_uchar,
                                    pub sll_halen: ::libc::c_uchar,
                                    pub sll_addr: [::libc::c_uchar; 8]
                                }

                                pub struct fd_set {
                                    fds_bits: [::libc::c_ulong; FD_SETSIZE / ULONG_SIZE],
                                }
                            }

                            // intentionally not public, only used for fd_set
                            #[cfg(target_pointer_width = "32")]
                            const ULONG_SIZE: usize = 32;
                            #[cfg(target_pointer_width = "64")]
                            const ULONG_SIZE: usize = 64;

                            pub const EXIT_FAILURE: ::libc::c_int = 1;
                            pub const EXIT_SUCCESS: ::libc::c_int = 0;
                            pub const RAND_MAX: ::libc::c_int = 2147483647;
                            pub const EOF: ::libc::c_int = -1;
                            pub const SEEK_SET: ::libc::c_int = 0;
                            pub const SEEK_CUR: ::libc::c_int = 1;
                            pub const SEEK_END: ::libc::c_int = 2;
                            pub const _IOFBF: ::libc::c_int = 0;
                            pub const _IONBF: ::libc::c_int = 2;
                            pub const _IOLBF: ::libc::c_int = 1;

                            pub const F_DUPFD: ::libc::c_int = 0;
                            pub const F_GETFD: ::libc::c_int = 1;
                            pub const F_SETFD: ::libc::c_int = 2;
                            pub const F_GETFL: ::libc::c_int = 3;
                            pub const F_SETFL: ::libc::c_int = 4;

                            pub const SIGTRAP: ::libc::c_int = 5;

                            pub const PTHREAD_CREATE_JOINABLE: ::libc::c_int = 0;
                            pub const PTHREAD_CREATE_DETACHED: ::libc::c_int = 1;

                            pub const CLOCK_REALTIME: ::libc::c_int = 0;
                            pub const CLOCK_MONOTONIC: ::libc::c_int = 1;

                            pub const RLIMIT_CPU: ::libc::c_int = 0;
                            pub const RLIMIT_FSIZE: ::libc::c_int = 1;
                            pub const RLIMIT_DATA: ::libc::c_int = 2;
                            pub const RLIMIT_STACK: ::libc::c_int = 3;
                            pub const RLIMIT_CORE: ::libc::c_int = 4;
                            pub const RLIMIT_LOCKS: ::libc::c_int = 10;
                            pub const RLIMIT_SIGPENDING: ::libc::c_int = 11;
                            pub const RLIMIT_MSGQUEUE: ::libc::c_int = 12;
                            pub const RLIMIT_NICE: ::libc::c_int = 13;
                            pub const RLIMIT_RTPRIO: ::libc::c_int = 14;

                            pub const RUSAGE_SELF: ::libc::c_int = 0;

                            pub const O_RDONLY: ::libc::c_int = 0;
                            pub const O_WRONLY: ::libc::c_int = 1;
                            pub const O_RDWR: ::libc::c_int = 2;
                            pub const O_TRUNC: ::libc::c_int = 512;
                            pub const O_CLOEXEC: ::libc::c_int = 0x80000;
                            pub const S_IFIFO: ::libc::mode_t = 4096;
                            pub const S_IFCHR: ::libc::mode_t = 8192;
                            pub const S_IFBLK: ::libc::mode_t = 24576;
                            pub const S_IFDIR: ::libc::mode_t = 16384;
                            pub const S_IFREG: ::libc::mode_t = 32768;
                            pub const S_IFLNK: ::libc::mode_t = 40960;
                            pub const S_IFSOCK: ::libc::mode_t = 49152;
                            pub const S_IFMT: ::libc::mode_t = 61440;
                            pub const S_IRWXU: ::libc::mode_t = 448;
                            pub const S_IXUSR: ::libc::mode_t = 64;
                            pub const S_IWUSR: ::libc::mode_t = 128;
                            pub const S_IRUSR: ::libc::mode_t = 256;
                            pub const S_IRWXG: ::libc::mode_t = 56;
                            pub const S_IXGRP: ::libc::mode_t = 8;
                            pub const S_IWGRP: ::libc::mode_t = 16;
                            pub const S_IRGRP: ::libc::mode_t = 32;
                            pub const S_IRWXO: ::libc::mode_t = 7;
                            pub const S_IXOTH: ::libc::mode_t = 1;
                            pub const S_IWOTH: ::libc::mode_t = 2;
                            pub const S_IROTH: ::libc::mode_t = 4;
                            pub const F_OK: ::libc::c_int = 0;
                            pub const R_OK: ::libc::c_int = 4;
                            pub const W_OK: ::libc::c_int = 2;
                            pub const X_OK: ::libc::c_int = 1;
                            pub const STDIN_FILENO: ::libc::c_int = 0;
                            pub const STDOUT_FILENO: ::libc::c_int = 1;
                            pub const STDERR_FILENO: ::libc::c_int = 2;
                            pub const SIGHUP: ::libc::c_int = 1;
                            pub const SIGINT: ::libc::c_int = 2;
                            pub const SIGQUIT: ::libc::c_int = 3;
                            pub const SIGILL: ::libc::c_int = 4;
                            pub const SIGABRT: ::libc::c_int = 6;
                            pub const SIGFPE: ::libc::c_int = 8;
                            pub const SIGKILL: ::libc::c_int = 9;
                            pub const SIGSEGV: ::libc::c_int = 11;
                            pub const SIGPIPE: ::libc::c_int = 13;
                            pub const SIGALRM: ::libc::c_int = 14;
                            pub const SIGTERM: ::libc::c_int = 15;

                            pub const PROT_NONE: ::libc::c_int = 0;
                            pub const PROT_READ: ::libc::c_int = 1;
                            pub const PROT_WRITE: ::libc::c_int = 2;
                            pub const PROT_EXEC: ::libc::c_int = 4;

                            pub const MAP_FILE: ::libc::c_int = 0x0000;
                            pub const MAP_SHARED: ::libc::c_int = 0x0001;
                            pub const MAP_PRIVATE: ::libc::c_int = 0x0002;
                            pub const MAP_FIXED: ::libc::c_int = 0x0010;

                            pub const MAP_FAILED: *mut ::libc::c_void = !0 as *mut ::libc::c_void;

                            pub const MCL_CURRENT: ::libc::c_int = 0x0001;
                            pub const MCL_FUTURE: ::libc::c_int = 0x0002;

                            pub const MS_ASYNC: ::libc::c_int = 0x0001;
                            pub const MS_INVALIDATE: ::libc::c_int = 0x0002;
                            pub const MS_SYNC: ::libc::c_int = 0x0004;

                            pub const EPERM: ::libc::c_int = 1;
                            pub const ENOENT: ::libc::c_int = 2;
                            pub const ESRCH: ::libc::c_int = 3;
                            pub const EINTR: ::libc::c_int = 4;
                            pub const EIO: ::libc::c_int = 5;
                            pub const ENXIO: ::libc::c_int = 6;
                            pub const E2BIG: ::libc::c_int = 7;
                            pub const ENOEXEC: ::libc::c_int = 8;
                            pub const EBADF: ::libc::c_int = 9;
                            pub const ECHILD: ::libc::c_int = 10;
                            pub const EAGAIN: ::libc::c_int = 11;
                            pub const ENOMEM: ::libc::c_int = 12;
                            pub const EACCES: ::libc::c_int = 13;
                            pub const EFAULT: ::libc::c_int = 14;
                            pub const ENOTBLK: ::libc::c_int = 15;
                            pub const EBUSY: ::libc::c_int = 16;
                            pub const EEXIST: ::libc::c_int = 17;
                            pub const EXDEV: ::libc::c_int = 18;
                            pub const ENODEV: ::libc::c_int = 19;
                            pub const ENOTDIR: ::libc::c_int = 20;
                            pub const EISDIR: ::libc::c_int = 21;
                            pub const EINVAL: ::libc::c_int = 22;
                            pub const ENFILE: ::libc::c_int = 23;
                            pub const EMFILE: ::libc::c_int = 24;
                            pub const ENOTTY: ::libc::c_int = 25;
                            pub const ETXTBSY: ::libc::c_int = 26;
                            pub const EFBIG: ::libc::c_int = 27;
                            pub const ENOSPC: ::libc::c_int = 28;
                            pub const ESPIPE: ::libc::c_int = 29;
                            pub const EROFS: ::libc::c_int = 30;
                            pub const EMLINK: ::libc::c_int = 31;
                            pub const EPIPE: ::libc::c_int = 32;
                            pub const EDOM: ::libc::c_int = 33;
                            pub const ERANGE: ::libc::c_int = 34;
                            pub const EWOULDBLOCK: ::libc::c_int = EAGAIN;

                            pub const EBFONT: ::libc::c_int = 59;
                            pub const ENOSTR: ::libc::c_int = 60;
                            pub const ENODATA: ::libc::c_int = 61;
                            pub const ETIME: ::libc::c_int = 62;
                            pub const ENOSR: ::libc::c_int = 63;
                            pub const ENONET: ::libc::c_int = 64;
                            pub const ENOPKG: ::libc::c_int = 65;
                            pub const EREMOTE: ::libc::c_int = 66;
                            pub const ENOLINK: ::libc::c_int = 67;
                            pub const EADV: ::libc::c_int = 68;
                            pub const ESRMNT: ::libc::c_int = 69;
                            pub const ECOMM: ::libc::c_int = 70;
                            pub const EPROTO: ::libc::c_int = 71;
                            pub const EDOTDOT: ::libc::c_int = 73;

                            pub const AF_PACKET: ::libc::c_int = 17;
                            pub const IPPROTO_RAW: ::libc::c_int = 255;

                            pub const PROT_GROWSDOWN: ::libc::c_int = 0x1000000;
                            pub const PROT_GROWSUP: ::libc::c_int = 0x2000000;

                            pub const MAP_TYPE: ::libc::c_int = 0x000f;

                            pub const MADV_NORMAL: ::libc::c_int = 0;
                            pub const MADV_RANDOM: ::libc::c_int = 1;
                            pub const MADV_SEQUENTIAL: ::libc::c_int = 2;
                            pub const MADV_WILLNEED: ::libc::c_int = 3;
                            pub const MADV_DONTNEED: ::libc::c_int = 4;
                            pub const MADV_REMOVE: ::libc::c_int = 9;
                            pub const MADV_DONTFORK: ::libc::c_int = 10;
                            pub const MADV_DOFORK: ::libc::c_int = 11;
                            pub const MADV_MERGEABLE: ::libc::c_int = 12;
                            pub const MADV_UNMERGEABLE: ::libc::c_int = 13;
                            pub const MADV_HWPOISON: ::libc::c_int = 100;

                            pub const IFF_LOOPBACK: ::libc::c_int = 0x8;

                            pub const AF_UNIX: ::libc::c_int = 1;
                            pub const AF_INET: ::libc::c_int = 2;
                            pub const AF_INET6: ::libc::c_int = 10;
                            pub const SOCK_RAW: ::libc::c_int = 3;
                            pub const IPPROTO_TCP: ::libc::c_int = 6;
                            pub const IPPROTO_IP: ::libc::c_int = 0;
                            pub const IPPROTO_IPV6: ::libc::c_int = 41;
                            pub const IP_MULTICAST_TTL: ::libc::c_int = 33;
                            pub const IP_MULTICAST_LOOP: ::libc::c_int = 34;
                            pub const IP_TTL: ::libc::c_int = 2;
                            pub const IP_HDRINCL: ::libc::c_int = 3;
                            pub const IP_ADD_MEMBERSHIP: ::libc::c_int = 35;
                            pub const IP_DROP_MEMBERSHIP: ::libc::c_int = 36;
                            pub const IPV6_ADD_MEMBERSHIP: ::libc::c_int = 20;
                            pub const IPV6_DROP_MEMBERSHIP: ::libc::c_int = 21;

                            pub const TCP_NODELAY: ::libc::c_int = 1;
                            pub const TCP_MAXSEG: ::libc::c_int = 2;
                            pub const TCP_CORK: ::libc::c_int = 3;
                            pub const TCP_KEEPIDLE: ::libc::c_int = 4;
                            pub const TCP_KEEPINTVL: ::libc::c_int = 5;
                            pub const TCP_KEEPCNT: ::libc::c_int = 6;
                            pub const TCP_SYNCNT: ::libc::c_int = 7;
                            pub const TCP_LINGER2: ::libc::c_int = 8;
                            pub const TCP_DEFER_ACCEPT: ::libc::c_int = 9;
                            pub const TCP_WINDOW_CLAMP: ::libc::c_int = 10;
                            pub const TCP_INFO: ::libc::c_int = 11;
                            pub const TCP_QUICKACK: ::libc::c_int = 12;
                            pub const TCP_CONGESTION: ::libc::c_int = 13;

                            pub const IPV6_MULTICAST_LOOP: ::libc::c_int = 19;
                            pub const IPV6_V6ONLY: ::libc::c_int = 26;

                            pub const SO_DEBUG: ::libc::c_int = 1;

                            pub const SHUT_RD: ::libc::c_int = 0;
                            pub const SHUT_WR: ::libc::c_int = 1;
                            pub const SHUT_RDWR: ::libc::c_int = 2;

                            pub const LOCK_SH: ::libc::c_int = 1;
                            pub const LOCK_EX: ::libc::c_int = 2;
                            pub const LOCK_NB: ::libc::c_int = 4;
                            pub const LOCK_UN: ::libc::c_int = 8;

                            pub const SIGSTKSZ: ::libc::size_t = 8192;

                            pub const SA_NODEFER: ::libc::c_int = 0x40000000;
                            pub const SA_RESETHAND: ::libc::c_int = 0x80000000u32 as i32;
                            pub const SA_RESTART: ::libc::c_int = 0x10000000;
                            pub const SA_NOCLDSTOP: ::libc::c_int = 0x00000001;

                            pub const FD_SETSIZE: usize = 1024;

                            f! {
                                pub fn FD_CLR(fd: ::libc::c_int, set: *mut fd_set) -> () {
                                    let fd = fd as usize;
                                    let size = mem::size_of_val(&(*set).fds_bits[0]);
                                    (*set).fds_bits[fd / size] &= !(1 << (fd % size));
                                    return
                                }

                                pub fn FD_ISSET(fd: ::libc::c_int, set: *mut fd_set) -> bool {
                                    let fd = fd as usize;
                                    let size = mem::size_of_val(&(*set).fds_bits[0]);
                                    return ((*set).fds_bits[fd / size] & (1 << (fd % size))) != 0
                                }

                                pub fn FD_SET(fd: ::libc::c_int, set: *mut fd_set) -> () {
                                    let fd = fd as usize;
                                    let size = mem::size_of_val(&(*set).fds_bits[0]);
                                    (*set).fds_bits[fd / size] |= 1 << (fd % size);
                                    return
                                }

                                pub fn FD_ZERO(set: *mut fd_set) -> () {
                                    for slot in (*set).fds_bits.iter_mut() {
                                        *slot = 0;
                                    }
                                }

                                pub fn WIFEXITED(status: ::libc::c_int) -> bool {
                                    (status & 0xff) == 0
                                }

                                pub fn WEXITSTATUS(status: ::libc::c_int) -> ::libc::c_int {
                                    (status >> 8) & 0xff
                                }

                                pub fn WTERMSIG(status: ::libc::c_int) -> ::libc::c_int {
                                    status & 0x7f
                                }
                            }

                            extern {
                                pub fn fdatasync(fd: ::libc::c_int) -> ::libc::c_int;
                                pub fn mincore(addr: *mut ::libc::c_void, len: ::libc::size_t,
                                            vec: *mut ::libc::c_uchar) -> ::libc::c_int;
                                pub fn clock_gettime(clk_id: ::libc::c_int, tp: *mut ::libc::timespec) -> ::libc::c_int;
                                pub fn prctl(option: ::libc::c_int, ...) -> ::libc::c_int;
                                pub fn pthread_getattr_np(native: ::libc::pthread_t,
                                                        attr: *mut ::libc::pthread_attr_t) -> ::libc::c_int;
                                pub fn pthread_attr_getguardsize(attr: *const ::libc::pthread_attr_t,
                                                                guardsize: *mut ::libc::size_t) -> ::libc::c_int;
                                pub fn pthread_attr_getstack(attr: *const ::libc::pthread_attr_t,
                                                            stackaddr: *mut *mut ::libc::c_void,
                                                            stacksize: *mut ::libc::size_t) -> ::libc::c_int;
                                pub fn memalign(align: ::libc::size_t, size: ::libc::size_t) -> *mut ::libc::c_void;
                                pub fn setgroups(ngroups: ::libc::size_t,
                                                ptr: *const ::libc::gid_t) -> ::libc::c_int;
                            }

                            cfg_if!
                            {
                                if #[cfg(target_os = "linux")]
                                {
                                    mod linux
                                    {
                                        //! Linux-specific definitions for linux-like values
                                        use ::
                                        {
                                            *
                                        };
                                        
                                        pub type useconds_t = u32;
                                        pub type dev_t = u64;
                                        pub type socklen_t = u32;
                                        pub type pthread_t = c_ulong;
                                        pub type mode_t = u32;
                                        pub type ino64_t = u64;
                                        pub type off64_t = i64;
                                        pub type blkcnt64_t = i64;
                                        pub type rlim64_t = u64;

                                        pub enum fpos64_t {}

                                        s!
                                        {
                                            pub struct dirent {
                                                pub d_ino: ::libc::ino_t,
                                                pub d_off: ::libc::off_t,
                                                pub d_reclen: ::libc::c_ushort,
                                                pub d_type: ::libc::c_uchar,
                                                pub d_name: [::libc::c_char; 256],
                                            }

                                            pub struct dirent64 {
                                                pub d_ino: ::libc::ino64_t,
                                                pub d_off: ::libc::off64_t,
                                                pub d_reclen: ::libc::c_ushort,
                                                pub d_type: ::libc::c_uchar,
                                                pub d_name: [::libc::c_char; 256],
                                            }

                                            pub struct rlimit64 {
                                                pub rlim_cur: rlim64_t,
                                                pub rlim_max: rlim64_t,
                                            }

                                            pub struct glob_t {
                                                pub gl_pathc: ::libc::size_t,
                                                pub gl_pathv: *mut *mut c_char,
                                                pub gl_offs: ::libc::size_t,
                                                pub gl_flags: ::libc::c_int,

                                                __unused1: *mut ::libc::c_void,
                                                __unused2: *mut ::libc::c_void,
                                                __unused3: *mut ::libc::c_void,
                                                __unused4: *mut ::libc::c_void,
                                                __unused5: *mut ::libc::c_void,
                                            }

                                            pub struct ifaddrs {
                                                pub ifa_next: *mut ifaddrs,
                                                pub ifa_name: *mut c_char,
                                                pub ifa_flags: ::libc::c_uint,
                                                pub ifa_addr: *mut ::libc::sockaddr,
                                                pub ifa_netmask: *mut ::libc::sockaddr,
                                                pub ifa_ifu: *mut ::libc::sockaddr, // FIXME This should be a union
                                                pub ifa_data: *mut ::libc::c_void
                                            }

                                            pub struct pthread_mutex_t {
                                                #[cfg(any(target_arch = "mips", target_arch = "mipsel",
                                                        target_arch = "arm"))]
                                                __align: [::libc::c_long; 0],
                                                #[cfg(not(any(target_arch = "mips", target_arch = "mipsel",
                                                            target_arch = "arm")))]
                                                __align: [::libc::c_longlong; 0],
                                                size: [u8; __SIZEOF_PTHREAD_MUTEX_T],
                                            }

                                            pub struct pthread_rwlock_t {
                                                #[cfg(any(target_arch = "mips", target_arch = "mipsel",
                                                        target_arch = "arm"))]
                                                __align: [::libc::c_long; 0],
                                                #[cfg(not(any(target_arch = "mips", target_arch = "mipsel",
                                                            target_arch = "arm")))]
                                                __align: [::libc::c_longlong; 0],
                                                size: [u8; __SIZEOF_PTHREAD_RWLOCK_T],
                                            }

                                            pub struct pthread_mutexattr_t {
                                                #[cfg(target_arch = "x86_64")]
                                                __align: [::libc::c_int; 0],
                                                #[cfg(not(target_arch = "x86_64"))]
                                                __align: [::libc::c_long; 0],
                                                size: [u8; __SIZEOF_PTHREAD_MUTEXATTR_T],
                                            }

                                            pub struct pthread_cond_t {
                                                __align: [::libc::c_longlong; 0],
                                                size: [u8; __SIZEOF_PTHREAD_COND_T],
                                            }

                                            pub struct passwd {
                                                pub pw_name: *mut ::libc::c_char,
                                                pub pw_passwd: *mut ::libc::c_char,
                                                pub pw_uid: ::libc::uid_t,
                                                pub pw_gid: ::libc::gid_t,
                                                pub pw_gecos: *mut ::libc::c_char,
                                                pub pw_dir: *mut ::libc::c_char,
                                                pub pw_shell: *mut ::libc::c_char,
                                            }
                                        }

                                        pub const FILENAME_MAX: ::libc::c_uint = 4096;
                                        pub const L_tmpnam: ::libc::c_uint = 20;
                                        pub const _PC_NAME_MAX: ::libc::c_int = 3;

                                        pub const _SC_ARG_MAX: ::libc::c_int = 0;
                                        pub const _SC_CHILD_MAX: ::libc::c_int = 1;
                                        pub const _SC_CLK_TCK: ::libc::c_int = 2;
                                        pub const _SC_NGROUPS_MAX: ::libc::c_int = 3;
                                        pub const _SC_OPEN_MAX: ::libc::c_int = 4;
                                        pub const _SC_STREAM_MAX: ::libc::c_int = 5;
                                        pub const _SC_TZNAME_MAX: ::libc::c_int = 6;
                                        pub const _SC_JOB_CONTROL: ::libc::c_int = 7;
                                        pub const _SC_SAVED_IDS: ::libc::c_int = 8;
                                        pub const _SC_REALTIME_SIGNALS: ::libc::c_int = 9;
                                        pub const _SC_PRIORITY_SCHEDULING: ::libc::c_int = 10;
                                        pub const _SC_TIMERS: ::libc::c_int = 11;
                                        pub const _SC_ASYNCHRONOUS_IO: ::libc::c_int = 12;
                                        pub const _SC_PRIORITIZED_IO: ::libc::c_int = 13;
                                        pub const _SC_SYNCHRONIZED_IO: ::libc::c_int = 14;
                                        pub const _SC_FSYNC: ::libc::c_int = 15;
                                        pub const _SC_MAPPED_FILES: ::libc::c_int = 16;
                                        pub const _SC_MEMLOCK: ::libc::c_int = 17;
                                        pub const _SC_MEMLOCK_RANGE: ::libc::c_int = 18;
                                        pub const _SC_MEMORY_PROTECTION: ::libc::c_int = 19;
                                        pub const _SC_MESSAGE_PASSING: ::libc::c_int = 20;
                                        pub const _SC_SEMAPHORES: ::libc::c_int = 21;
                                        pub const _SC_SHARED_MEMORY_OBJECTS: ::libc::c_int = 22;
                                        pub const _SC_AIO_LISTIO_MAX: ::libc::c_int = 23;
                                        pub const _SC_AIO_MAX: ::libc::c_int = 24;
                                        pub const _SC_AIO_PRIO_DELTA_MAX: ::libc::c_int = 25;
                                        pub const _SC_DELAYTIMER_MAX: ::libc::c_int = 26;
                                        pub const _SC_MQ_OPEN_MAX: ::libc::c_int = 27;
                                        pub const _SC_MQ_PRIO_MAX: ::libc::c_int = 28;
                                        pub const _SC_VERSION: ::libc::c_int = 29;
                                        pub const _SC_PAGESIZE: ::libc::c_int = 30;
                                        pub const _SC_RTSIG_MAX: ::libc::c_int = 31;
                                        pub const _SC_SEM_NSEMS_MAX: ::libc::c_int = 32;
                                        pub const _SC_SEM_VALUE_MAX: ::libc::c_int = 33;
                                        pub const _SC_SIGQUEUE_MAX: ::libc::c_int = 34;
                                        pub const _SC_TIMER_MAX: ::libc::c_int = 35;
                                        pub const _SC_BC_BASE_MAX: ::libc::c_int = 36;
                                        pub const _SC_BC_DIM_MAX: ::libc::c_int = 37;
                                        pub const _SC_BC_SCALE_MAX: ::libc::c_int = 38;
                                        pub const _SC_BC_STRING_MAX: ::libc::c_int = 39;
                                        pub const _SC_COLL_WEIGHTS_MAX: ::libc::c_int = 40;
                                        pub const _SC_EXPR_NEST_MAX: ::libc::c_int = 42;
                                        pub const _SC_LINE_MAX: ::libc::c_int = 43;
                                        pub const _SC_RE_DUP_MAX: ::libc::c_int = 44;
                                        pub const _SC_2_VERSION: ::libc::c_int = 46;
                                        pub const _SC_2_C_BIND: ::libc::c_int = 47;
                                        pub const _SC_2_C_DEV: ::libc::c_int = 48;
                                        pub const _SC_2_FORT_DEV: ::libc::c_int = 49;
                                        pub const _SC_2_FORT_RUN: ::libc::c_int = 50;
                                        pub const _SC_2_SW_DEV: ::libc::c_int = 51;
                                        pub const _SC_2_LOCALEDEF: ::libc::c_int = 52;
                                        pub const _SC_IOV_MAX: ::libc::c_int = 60;
                                        pub const _SC_THREADS: ::libc::c_int = 67;
                                        pub const _SC_THREAD_SAFE_FUNCTIONS: ::libc::c_int = 68;
                                        pub const _SC_GETGR_R_SIZE_MAX: ::libc::c_int = 69;
                                        pub const _SC_GETPW_R_SIZE_MAX: ::libc::c_int = 70;
                                        pub const _SC_LOGIN_NAME_MAX: ::libc::c_int = 71;
                                        pub const _SC_TTY_NAME_MAX: ::libc::c_int = 72;
                                        pub const _SC_THREAD_DESTRUCTOR_ITERATIONS: ::libc::c_int = 73;
                                        pub const _SC_THREAD_KEYS_MAX: ::libc::c_int = 74;
                                        pub const _SC_THREAD_STACK_MIN: ::libc::c_int = 75;
                                        pub const _SC_THREAD_THREADS_MAX: ::libc::c_int = 76;
                                        pub const _SC_THREAD_ATTR_STACKADDR: ::libc::c_int = 77;
                                        pub const _SC_THREAD_ATTR_STACKSIZE: ::libc::c_int = 78;
                                        pub const _SC_THREAD_PRIORITY_SCHEDULING: ::libc::c_int = 79;
                                        pub const _SC_THREAD_PRIO_INHERIT: ::libc::c_int = 80;
                                        pub const _SC_THREAD_PRIO_PROTECT: ::libc::c_int = 81;
                                        pub const _SC_NPROCESSORS_ONLN: ::libc::c_int = 84;
                                        pub const _SC_ATEXIT_MAX: ::libc::c_int = 87;
                                        pub const _SC_XOPEN_VERSION: ::libc::c_int = 89;
                                        pub const _SC_XOPEN_XCU_VERSION: ::libc::c_int = 90;
                                        pub const _SC_XOPEN_UNIX: ::libc::c_int = 91;
                                        pub const _SC_XOPEN_CRYPT: ::libc::c_int = 92;
                                        pub const _SC_XOPEN_ENH_I18N: ::libc::c_int = 93;
                                        pub const _SC_XOPEN_SHM: ::libc::c_int = 94;
                                        pub const _SC_2_CHAR_TERM: ::libc::c_int = 95;
                                        pub const _SC_2_UPE: ::libc::c_int = 97;
                                        pub const _SC_XBS5_ILP32_OFF32: ::libc::c_int = 125;
                                        pub const _SC_XBS5_ILP32_OFFBIG: ::libc::c_int = 126;
                                        pub const _SC_XBS5_LPBIG_OFFBIG: ::libc::c_int = 128;
                                        pub const _SC_XOPEN_LEGACY: ::libc::c_int = 129;
                                        pub const _SC_XOPEN_REALTIME: ::libc::c_int = 130;
                                        pub const _SC_XOPEN_REALTIME_THREADS: ::libc::c_int = 131;

                                        pub const RLIM_SAVED_MAX: ::libc::rlim_t = RLIM_INFINITY;
                                        pub const RLIM_SAVED_CUR: ::libc::rlim_t = RLIM_INFINITY;

                                        pub const GLOB_ERR: ::libc::c_int = 1 << 0;
                                        pub const GLOB_MARK: ::libc::c_int = 1 << 1;
                                        pub const GLOB_NOSORT: ::libc::c_int = 1 << 2;
                                        pub const GLOB_DOOFFS: ::libc::c_int = 1 << 3;
                                        pub const GLOB_NOCHECK: ::libc::c_int = 1 << 4;
                                        pub const GLOB_APPEND: ::libc::c_int = 1 << 5;
                                        pub const GLOB_NOESCAPE: ::libc::c_int = 1 << 6;

                                        pub const GLOB_NOSPACE: ::libc::c_int = 1;
                                        pub const GLOB_ABORTED: ::libc::c_int = 2;
                                        pub const GLOB_NOMATCH: ::libc::c_int = 3;

                                        pub const POSIX_MADV_NORMAL: ::libc::c_int = 0;
                                        pub const POSIX_MADV_RANDOM: ::libc::c_int = 1;
                                        pub const POSIX_MADV_SEQUENTIAL: ::libc::c_int = 2;
                                        pub const POSIX_MADV_WILLNEED: ::libc::c_int = 3;

                                        pub const S_IEXEC: mode_t = 64;
                                        pub const S_IWRITE: mode_t = 128;
                                        pub const S_IREAD: mode_t = 256;

                                        pub const F_LOCK: ::libc::c_int = 1;
                                        pub const F_TEST: ::libc::c_int = 3;
                                        pub const F_TLOCK: ::libc::c_int = 2;
                                        pub const F_ULOCK: ::libc::c_int = 0;

                                        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
                                        pub const MAP_32BIT: ::libc::c_int = 0x0040;

                                        pub const TCP_MD5SIG: ::libc::c_int = 14;

                                        pub const F_DUPFD_CLOEXEC: ::libc::c_int = 1030;

                                        pub const PTHREAD_MUTEX_INITIALIZER: pthread_mutex_t = pthread_mutex_t {
                                            __align: [],
                                            size: [0; __SIZEOF_PTHREAD_MUTEX_T],
                                        };
                                        pub const PTHREAD_COND_INITIALIZER: pthread_cond_t = pthread_cond_t {
                                            __align: [],
                                            size: [0; __SIZEOF_PTHREAD_COND_T],
                                        };
                                        pub const PTHREAD_RWLOCK_INITIALIZER: pthread_rwlock_t = pthread_rwlock_t {
                                            __align: [],
                                            size: [0; __SIZEOF_PTHREAD_RWLOCK_T],
                                        };
                                        pub const PTHREAD_MUTEX_RECURSIVE: ::libc::c_int = 1;
                                        pub const __SIZEOF_PTHREAD_COND_T: usize = 48;

                                        extern
                                        {
                                            pub fn shm_open(name: *const c_char, oflag: ::libc::c_int,
                                                            mode: mode_t) -> ::libc::c_int;
                                            pub fn mprotect(addr: *mut ::libc::c_void, len: ::libc::size_t, prot: ::libc::c_int)
                                                            -> ::libc::c_int;
                                            pub fn __errno_location() -> *mut ::libc::c_int;

                                            pub fn fopen64(filename: *const c_char,
                                                        mode: *const c_char) -> *mut ::libc::FILE;
                                            pub fn freopen64(filename: *const c_char, mode: *const c_char,
                                                            file: *mut ::libc::FILE) -> *mut ::libc::FILE;
                                            pub fn tmpfile64() -> *mut ::libc::FILE;
                                            pub fn fgetpos64(stream: *mut ::libc::FILE, ptr: *mut fpos64_t) -> ::libc::c_int;
                                            pub fn fsetpos64(stream: *mut ::libc::FILE, ptr: *const fpos64_t) -> ::libc::c_int;
                                            pub fn fstat64(fildes: ::libc::c_int, buf: *mut stat64) -> ::libc::c_int;
                                            pub fn stat64(path: *const c_char, buf: *mut stat64) -> ::libc::c_int;
                                            pub fn open64(path: *const c_char, oflag: ::libc::c_int, ...) -> ::libc::c_int;
                                            pub fn creat64(path: *const c_char, mode: mode_t) -> ::libc::c_int;
                                            pub fn lseek64(fd: ::libc::c_int, offset: off64_t, whence: ::libc::c_int) -> off64_t;
                                            pub fn pread64(fd: ::libc::c_int, buf: *mut ::libc::c_void, count: ::libc::size_t,
                                                        offset: off64_t) -> ::libc::ssize_t;
                                            pub fn pwrite64(fd: ::libc::c_int, buf: *const ::libc::c_void, count: ::libc::size_t,
                                                            offset: off64_t) -> ::libc::ssize_t;
                                            pub fn mmap64(addr: *mut ::libc::c_void,
                                                        len: ::libc::size_t,
                                                        prot: ::libc::c_int,
                                                        flags: ::libc::c_int,
                                                        fd: ::libc::c_int,
                                                        offset: off64_t)
                                                        -> *mut ::libc::c_void;
                                            pub fn lstat64(path: *const c_char, buf: *mut stat64) -> ::libc::c_int;
                                            pub fn ftruncate64(fd: ::libc::c_int, length: off64_t) -> ::libc::c_int;
                                            pub fn readdir64_r(dirp: *mut ::libc::DIR, entry: *mut ::libc::dirent64,
                                                            result: *mut *mut ::libc::dirent64) -> ::libc::c_int;

                                            pub fn getrlimit64(resource: ::libc::c_int, rlim: *mut rlimit64) -> ::libc::c_int;
                                            pub fn setrlimit64(resource: ::libc::c_int, rlim: *const rlimit64) -> ::libc::c_int;
                                            pub fn fseeko64(stream: *mut ::libc::FILE,
                                                            offset: ::libc::off64_t,
                                                            whence: ::libc::c_int) -> ::libc::c_int;
                                            pub fn ftello64(stream: *mut ::libc::FILE) -> ::libc::off64_t;
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(any(target_env = "musl"))] {
                                                pub const PTHREAD_STACK_MIN: ::libc::size_t = 2048;
                                            } else if #[cfg(any(target_arch = "arm", target_arch = "x86",
                                                                target_arch = "x86_64"))] {
                                                pub const PTHREAD_STACK_MIN: ::libc::size_t = 16384;
                                            } else {
                                                pub const PTHREAD_STACK_MIN: ::libc::size_t = 131072;
                                            }
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(target_env = "musl")]
                                            {
                                                mod musl
                                                {
                                                    use ::
                                                    { 
                                                        *,
                                                    };
                                                    
                                                    pub const BUFSIZ: ::libc::c_uint = 1024;
                                                    pub const TMP_MAX: ::libc::c_uint = 10000;
                                                    pub const FOPEN_MAX: ::libc::c_uint = 1000;
                                                    pub const POSIX_MADV_DONTNEED: ::libc::c_int = 0;
                                                    pub const O_ACCMODE: ::libc::c_int = 0o10000003;
                                                    pub const RUSAGE_CHILDREN: ::libc::c_int = 1;

                                                    extern {
                                                        pub fn ioctl(fd: ::libc::c_int, request: ::libc::c_int, ...) -> ::libc::c_int;
                                                    }
                                                }
                                                
                                                pub use self::musl::*;
                                            }
                                            
                                            else
                                            {
                                                mod notmusl
                                                {
                                                    use ::
                                                    { 
                                                        *,
                                                    };
                                                    
                                                    s!
                                                    {
                                                        pub struct glob64_t {
                                                            pub gl_pathc: ::libc::size_t,
                                                            pub gl_pathv: *mut *mut ::libc::c_char,
                                                            pub gl_offs: ::libc::size_t,
                                                            pub gl_flags: ::libc::c_int,

                                                            __unused1: *mut ::libc::c_void,
                                                            __unused2: *mut ::libc::c_void,
                                                            __unused3: *mut ::libc::c_void,
                                                            __unused4: *mut ::libc::c_void,
                                                            __unused5: *mut ::libc::c_void,
                                                        }
                                                    }

                                                    pub const BUFSIZ: ::libc::c_uint = 8192;
                                                    pub const TMP_MAX: ::libc::c_uint = 238328;
                                                    pub const FOPEN_MAX: ::libc::c_uint = 16;
                                                    pub const POSIX_MADV_DONTNEED: ::libc::c_int = 4;
                                                    pub const _SC_2_C_VERSION: ::libc::c_int = 96;
                                                    pub const RUSAGE_THREAD: ::libc::c_int = 1;
                                                    pub const O_ACCMODE: ::libc::c_int = 3;
                                                    pub const RUSAGE_CHILDREN: ::libc::c_int = -1;

                                                    extern
                                                    {
                                                        pub fn sysctl(name: *mut ::libc::c_int,
                                                                    namelen: ::libc::c_int,
                                                                    oldp: *mut ::libc::c_void,
                                                                    oldlenp: *mut ::libc::size_t,
                                                                    newp: *mut ::libc::c_void,
                                                                    newlen: ::libc::size_t)
                                                                    -> ::libc::c_int;
                                                        pub fn ioctl(fd: ::libc::c_int, request: ::libc::c_ulong, ...) -> ::libc::c_int;
                                                        pub fn backtrace(buf: *mut *mut ::libc::c_void,
                                                                        sz: ::libc::c_int) -> ::libc::c_int;
                                                        pub fn glob64(pattern: *const ::libc::c_char,
                                                                    flags: ::libc::c_int,
                                                                    errfunc: Option<extern "C" fn(epath: *const ::libc::c_char,
                                                                                                        errno: ::libc::c_int) -> ::libc::c_int>,
                                                                    pglob: *mut glob64_t) -> ::libc::c_int;
                                                        pub fn globfree64(pglob: *mut glob64_t);
                                                    }

                                                }
                                                
                                                pub use self::notmusl::*;
                                            }
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(any(target_arch = "mips", target_arch = "mipsel"))]
                                            {
                                                mod mips
                                                {
                                                    use ::
                                                    {
                                                        *
                                                    };
                                                    pub type c_char = i8;
                                                    pub type c_long = i32;
                                                    pub type c_ulong = u32;
                                                    pub type clock_t = i32;
                                                    pub type time_t = i32;
                                                    pub type suseconds_t = i32;
                                                    pub type wchar_t = i32;
                                                    pub type off_t = i32;
                                                    pub type ino_t = u32;
                                                    pub type blkcnt_t = i32;
                                                    pub type blksize_t = i32;
                                                    pub type nlink_t = u32;

                                                    s!
                                                    {
                                                        pub struct stat {
                                                            pub st_dev: ::libc::c_ulong,
                                                            st_pad1: [::libc::c_long; 3],
                                                            pub st_ino: ::libc::ino_t,
                                                            pub st_mode: ::libc::mode_t,
                                                            pub st_nlink: ::nlink_t,
                                                            pub st_uid: ::libc::uid_t,
                                                            pub st_gid: ::libc::gid_t,
                                                            pub st_rdev: ::libc::c_ulong,
                                                            pub st_pad2: [::libc::c_long; 2],
                                                            pub st_size: ::libc::off_t,
                                                            st_pad3: ::libc::c_long,
                                                            pub st_atime: ::libc::time_t,
                                                            pub st_atime_nsec: ::libc::c_long,
                                                            pub st_mtime: ::libc::time_t,
                                                            pub st_mtime_nsec: ::libc::c_long,
                                                            pub st_ctime: ::libc::time_t,
                                                            pub st_ctime_nsec: ::libc::c_long,
                                                            pub st_blksize: ::blksize_t,
                                                            pub st_blocks: ::blkcnt_t,
                                                            st_pad5: [::libc::c_long; 14],
                                                        }

                                                        pub struct stat64 {
                                                            pub st_dev: ::libc::c_ulong,
                                                            st_pad1: [::libc::c_long; 3],
                                                            pub st_ino: ::libc::ino64_t,
                                                            pub st_mode: ::libc::mode_t,
                                                            pub st_nlink: ::nlink_t,
                                                            pub st_uid: ::libc::uid_t,
                                                            pub st_gid: ::libc::gid_t,
                                                            pub st_rdev: ::libc::c_ulong,
                                                            st_pad2: [::libc::c_long; 2],
                                                            pub st_size: ::libc::off64_t,
                                                            pub st_atime: ::libc::time_t,
                                                            pub st_atime_nsec: ::libc::c_long,
                                                            pub st_mtime: ::libc::time_t,
                                                            pub st_mtime_nsec: ::libc::c_long,
                                                            pub st_ctime: ::libc::time_t,
                                                            pub st_ctime_nsec: ::libc::c_long,
                                                            pub st_blksize: ::blksize_t,
                                                            st_pad3: ::libc::c_long,
                                                            pub st_blocks: ::blkcnt64_t,
                                                            st_pad5: [::libc::c_long; 14],
                                                        }

                                                        pub struct pthread_attr_t {
                                                            __size: [u32; 9]
                                                        }

                                                        pub struct sigaction {
                                                            pub sa_flags: ::libc::c_uint,
                                                            pub sa_sigaction: ::libc::sighandler_t,
                                                            pub sa_mask: sigset_t,
                                                            _restorer: *mut ::libc::c_void,
                                                            _resv: [::libc::c_int; 1],
                                                        }

                                                        pub struct stack_t {
                                                            pub ss_sp: *mut ::libc::c_void,
                                                            pub ss_size: ::libc::size_t,
                                                            pub ss_flags: ::libc::c_int,
                                                        }

                                                        pub struct sigset_t {
                                                            __val: [::libc::c_ulong; 32],
                                                        }

                                                        pub struct siginfo_t {
                                                            pub si_signo: ::libc::c_int,
                                                            pub si_code: ::libc::c_int,
                                                            pub si_errno: ::libc::c_int,
                                                            pub _pad: [::libc::c_int; 29],
                                                        }
                                                    }

                                                    pub const RLIMIT_NOFILE: ::libc::c_int = 5;
                                                    pub const RLIMIT_AS: ::libc::c_int = 6;
                                                    pub const RLIMIT_RSS: ::libc::c_int = 7;
                                                    pub const RLIMIT_NPROC: ::libc::c_int = 8;
                                                    pub const RLIMIT_MEMLOCK: ::libc::c_int = 9;
                                                    pub const RLIMIT_NLIMITS: ::libc::c_int = 15;
                                                    pub const RLIM_INFINITY: ::libc::rlim_t = 0x7fffffff;

                                                    pub const O_APPEND: ::libc::c_int = 8;
                                                    pub const O_CREAT: ::libc::c_int = 256;
                                                    pub const O_EXCL: ::libc::c_int = 1024;
                                                    pub const O_NOCTTY: ::libc::c_int = 2048;
                                                    pub const O_NONBLOCK: ::libc::c_int = 128;
                                                    pub const O_SYNC: ::libc::c_int = 0x10;
                                                    pub const O_RSYNC: ::libc::c_int = 0x10;
                                                    pub const O_DSYNC: ::libc::c_int = 0x10;

                                                    pub const EDEADLK: ::libc::c_int = 45;
                                                    pub const ENAMETOOLONG: ::libc::c_int = 78;
                                                    pub const ENOLCK: ::libc::c_int = 46;
                                                    pub const ENOSYS: ::libc::c_int = 89;
                                                    pub const ENOTEMPTY: ::libc::c_int = 93;
                                                    pub const ELOOP: ::libc::c_int = 90;
                                                    pub const ENOMSG: ::libc::c_int = 35;
                                                    pub const EIDRM: ::libc::c_int = 36;
                                                    pub const ECHRNG: ::libc::c_int = 37;
                                                    pub const EL2NSYNC: ::libc::c_int = 38;
                                                    pub const EL3HLT: ::libc::c_int = 39;
                                                    pub const EL3RST: ::libc::c_int = 40;
                                                    pub const ELNRNG: ::libc::c_int = 41;
                                                    pub const EUNATCH: ::libc::c_int = 42;
                                                    pub const ENOCSI: ::libc::c_int = 43;
                                                    pub const EL2HLT: ::libc::c_int = 44;
                                                    pub const EBADE: ::libc::c_int = 50;
                                                    pub const EBADR: ::libc::c_int = 51;
                                                    pub const EXFULL: ::libc::c_int = 52;
                                                    pub const ENOANO: ::libc::c_int = 53;
                                                    pub const EBADRQC: ::libc::c_int = 54;
                                                    pub const EBADSLT: ::libc::c_int = 55;
                                                    pub const EDEADLOCK: ::libc::c_int = 56;
                                                    pub const EMULTIHOP: ::libc::c_int = 74;
                                                    pub const EOVERFLOW: ::libc::c_int = 79;
                                                    pub const ENOTUNIQ: ::libc::c_int = 80;
                                                    pub const EBADFD: ::libc::c_int = 81;
                                                    pub const EBADMSG: ::libc::c_int = 77;
                                                    pub const EREMCHG: ::libc::c_int = 82;
                                                    pub const ELIBACC: ::libc::c_int = 83;
                                                    pub const ELIBBAD: ::libc::c_int = 84;
                                                    pub const ELIBSCN: ::libc::c_int = 85;
                                                    pub const ELIBMAX: ::libc::c_int = 86;
                                                    pub const ELIBEXEC: ::libc::c_int = 87;
                                                    pub const EILSEQ: ::libc::c_int = 88;
                                                    pub const ERESTART: ::libc::c_int = 91;
                                                    pub const ESTRPIPE: ::libc::c_int = 92;
                                                    pub const EUSERS: ::libc::c_int = 94;
                                                    pub const ENOTSOCK: ::libc::c_int = 95;
                                                    pub const EDESTADDRREQ: ::libc::c_int = 96;
                                                    pub const EMSGSIZE: ::libc::c_int = 97;
                                                    pub const EPROTOTYPE: ::libc::c_int = 98;
                                                    pub const ENOPROTOOPT: ::libc::c_int = 99;
                                                    pub const EPROTONOSUPPORT: ::libc::c_int = 120;
                                                    pub const ESOCKTNOSUPPORT: ::libc::c_int = 121;
                                                    pub const EOPNOTSUPP: ::libc::c_int = 122;
                                                    pub const EPFNOSUPPORT: ::libc::c_int = 123;
                                                    pub const EAFNOSUPPORT: ::libc::c_int = 124;
                                                    pub const EADDRINUSE: ::libc::c_int = 125;
                                                    pub const EADDRNOTAVAIL: ::libc::c_int = 126;
                                                    pub const ENETDOWN: ::libc::c_int = 127;
                                                    pub const ENETUNREACH: ::libc::c_int = 128;
                                                    pub const ENETRESET: ::libc::c_int = 129;
                                                    pub const ECONNABORTED: ::libc::c_int = 130;
                                                    pub const ECONNRESET: ::libc::c_int = 131;
                                                    pub const ENOBUFS: ::libc::c_int = 132;
                                                    pub const EISCONN: ::libc::c_int = 133;
                                                    pub const ENOTCONN: ::libc::c_int = 134;
                                                    pub const ESHUTDOWN: ::libc::c_int = 143;
                                                    pub const ETOOMANYREFS: ::libc::c_int = 144;
                                                    pub const ETIMEDOUT: ::libc::c_int = 145;
                                                    pub const ECONNREFUSED: ::libc::c_int = 146;
                                                    pub const EHOSTDOWN: ::libc::c_int = 147;
                                                    pub const EHOSTUNREACH: ::libc::c_int = 148;
                                                    pub const EALREADY: ::libc::c_int = 149;
                                                    pub const EINPROGRESS: ::libc::c_int = 150;
                                                    pub const ESTALE: ::libc::c_int = 151;
                                                    pub const EUCLEAN: ::libc::c_int = 135;
                                                    pub const ENOTNAM: ::libc::c_int = 137;
                                                    pub const ENAVAIL: ::libc::c_int = 138;
                                                    pub const EISNAM: ::libc::c_int = 139;
                                                    pub const EREMOTEIO: ::libc::c_int = 140;
                                                    pub const EDQUOT: ::libc::c_int = 1133;
                                                    pub const ENOMEDIUM: ::libc::c_int = 159;
                                                    pub const EMEDIUMTYPE: ::libc::c_int = 160;
                                                    pub const ECANCELED: ::libc::c_int = 158;
                                                    pub const ENOKEY: ::libc::c_int = 161;
                                                    pub const EKEYEXPIRED: ::libc::c_int = 162;
                                                    pub const EKEYREVOKED: ::libc::c_int = 163;
                                                    pub const EKEYREJECTED: ::libc::c_int = 164;
                                                    pub const EOWNERDEAD: ::libc::c_int = 165;
                                                    pub const ENOTRECOVERABLE: ::libc::c_int = 166;
                                                    pub const ERFKILL: ::libc::c_int = 167;

                                                    pub const MAP_NORESERVE: ::libc::c_int = 0x400;
                                                    pub const MAP_ANON: ::libc::c_int = 0x800;
                                                    pub const MAP_ANONYMOUS: ::libc::c_int = 0x800;
                                                    pub const MAP_GROWSDOWN: ::libc::c_int = 0x1000;
                                                    pub const MAP_DENYWRITE: ::libc::c_int = 0x2000;
                                                    pub const MAP_EXECUTABLE: ::libc::c_int = 0x4000;
                                                    pub const MAP_LOCKED: ::libc::c_int = 0x8000;
                                                    pub const MAP_POPULATE: ::libc::c_int = 0x10000;
                                                    pub const MAP_NONBLOCK: ::libc::c_int = 0x20000;

                                                    pub const SOCK_STREAM: ::libc::c_int = 2;
                                                    pub const SOCK_DGRAM: ::libc::c_int = 1;

                                                    pub const SOL_SOCKET: ::libc::c_int = 0xffff;

                                                    pub const SO_REUSEADDR: ::libc::c_int = 4;
                                                    pub const SO_TYPE: ::libc::c_int = 4104;
                                                    pub const SO_ERROR: ::libc::c_int = 4103;
                                                    pub const SO_DONTROUTE: ::libc::c_int = 16;
                                                    pub const SO_BROADCAST: ::libc::c_int = 32;
                                                    pub const SO_SNDBUF: ::libc::c_int = 4097;
                                                    pub const SO_RCVBUF: ::libc::c_int = 4098;
                                                    pub const SO_KEEPALIVE: ::libc::c_int = 8;
                                                    pub const SO_OOBINLINE: ::libc::c_int = 256;
                                                    pub const SO_LINGER: ::libc::c_int = 128;
                                                    pub const SO_RCVLOWAT: ::libc::c_int = 4100;
                                                    pub const SO_SNDLOWAT: ::libc::c_int = 4099;
                                                    pub const SO_RCVTIMEO: ::libc::c_int = 4102;
                                                    pub const SO_SNDTIMEO: ::libc::c_int = 4101;
                                                    pub const SO_ACCEPTCONN: ::libc::c_int = 4105;

                                                    pub const __SIZEOF_PTHREAD_MUTEX_T: usize = 24;
                                                    pub const __SIZEOF_PTHREAD_RWLOCK_T: usize = 32;
                                                    pub const __SIZEOF_PTHREAD_MUTEXATTR_T: usize = 4;

                                                    pub const FIOCLEX: ::libc::c_ulong = 0x6601;
                                                    pub const FIONBIO: ::libc::c_ulong = 0x667e;

                                                    pub const SA_ONSTACK: ::libc::c_uint = 0x08000000;
                                                    pub const SA_SIGINFO: ::libc::c_uint = 0x00000008;
                                                    pub const SA_NOCLDWAIT: ::libc::c_uint = 0x00010000;

                                                    pub const SIGCHLD: ::libc::c_int = 18;
                                                    pub const SIGBUS: ::libc::c_int = 10;

                                                    pub const SIG_SETMASK: ::libc::c_int = 3;

                                                    extern
                                                    {
                                                        pub fn getnameinfo(sa: *const ::libc::sockaddr,
                                                                        salen: ::libc::socklen_t,
                                                                        host: *mut ::libc::c_char,
                                                                        hostlen: ::libc::socklen_t,
                                                                        serv: *mut ::libc::c_char,
                                                                        sevlen: ::libc::socklen_t,
                                                                        flags: ::libc::c_uint) -> ::libc::c_int;
                                                    }

                                                }
                                                pub use self::mips::*;
                                            }
                                            
                                            else
                                            {
                                                mod notmips
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    s!
                                                    {
                                                        pub struct sigaction {
                                                            pub sa_sigaction: ::libc::sighandler_t,
                                                            pub sa_mask: ::libc::sigset_t,
                                                            pub sa_flags: ::libc::c_int,
                                                            _restorer: *mut ::libc::c_void,
                                                        }

                                                        pub struct stack_t {
                                                            pub ss_sp: *mut ::libc::c_void,
                                                            pub ss_flags: ::libc::c_int,
                                                            pub ss_size: ::libc::size_t
                                                        }

                                                        pub struct siginfo_t {
                                                            pub si_signo: ::libc::c_int,
                                                            pub si_errno: ::libc::c_int,
                                                            pub si_code: ::libc::c_int,
                                                            pub _pad: [::libc::c_int; 29],
                                                            _align: [usize; 0],
                                                        }
                                                    }

                                                    pub const RLIMIT_RSS: ::libc::c_int = 5;
                                                    pub const RLIMIT_NOFILE: ::libc::c_int = 7;
                                                    pub const RLIMIT_AS: ::libc::c_int = 9;
                                                    pub const RLIMIT_NPROC: ::libc::c_int = 6;
                                                    pub const RLIMIT_MEMLOCK: ::libc::c_int = 8;
                                                    pub const RLIM_INFINITY: ::libc::rlim_t = !0;
                                                    pub const RLIMIT_RTTIME: ::libc::c_int = 15;
                                                    pub const RLIMIT_NLIMITS: ::libc::c_int = 16;

                                                    pub const O_APPEND: ::libc::c_int = 1024;
                                                    pub const O_CREAT: ::libc::c_int = 64;
                                                    pub const O_EXCL: ::libc::c_int = 128;
                                                    pub const O_NOCTTY: ::libc::c_int = 256;
                                                    pub const O_NONBLOCK: ::libc::c_int = 2048;
                                                    pub const O_SYNC: ::libc::c_int = 1052672;
                                                    pub const O_RSYNC: ::libc::c_int = 1052672;
                                                    pub const O_DSYNC: ::libc::c_int = 4096;

                                                    pub const MAP_ANON: ::libc::c_int = 0x0020;
                                                    pub const MAP_ANONYMOUS: ::libc::c_int = 0x0020;
                                                    pub const MAP_GROWSDOWN: ::libc::c_int = 0x0100;
                                                    pub const MAP_DENYWRITE: ::libc::c_int = 0x0800;
                                                    pub const MAP_EXECUTABLE: ::libc::c_int = 0x01000;
                                                    pub const MAP_LOCKED: ::libc::c_int = 0x02000;
                                                    pub const MAP_NORESERVE: ::libc::c_int = 0x04000;
                                                    pub const MAP_POPULATE: ::libc::c_int = 0x08000;
                                                    pub const MAP_NONBLOCK: ::libc::c_int = 0x010000;
                                                    pub const MAP_STACK: ::libc::c_int = 0x020000;

                                                    pub const EDEADLK: ::libc::c_int = 35;
                                                    pub const ENAMETOOLONG: ::libc::c_int = 36;
                                                    pub const ENOLCK: ::libc::c_int = 37;
                                                    pub const ENOSYS: ::libc::c_int = 38;
                                                    pub const ENOTEMPTY: ::libc::c_int = 39;
                                                    pub const ELOOP: ::libc::c_int = 40;
                                                    pub const ENOMSG: ::libc::c_int = 42;
                                                    pub const EIDRM: ::libc::c_int = 43;
                                                    pub const ECHRNG: ::libc::c_int = 44;
                                                    pub const EL2NSYNC: ::libc::c_int = 45;
                                                    pub const EL3HLT: ::libc::c_int = 46;
                                                    pub const EL3RST: ::libc::c_int = 47;
                                                    pub const ELNRNG: ::libc::c_int = 48;
                                                    pub const EUNATCH: ::libc::c_int = 49;
                                                    pub const ENOCSI: ::libc::c_int = 50;
                                                    pub const EL2HLT: ::libc::c_int = 51;
                                                    pub const EBADE: ::libc::c_int = 52;
                                                    pub const EBADR: ::libc::c_int = 53;
                                                    pub const EXFULL: ::libc::c_int = 54;
                                                    pub const ENOANO: ::libc::c_int = 55;
                                                    pub const EBADRQC: ::libc::c_int = 56;
                                                    pub const EBADSLT: ::libc::c_int = 57;
                                                    pub const EDEADLOCK: ::libc::c_int = EDEADLK;
                                                    pub const EMULTIHOP: ::libc::c_int = 72;
                                                    pub const EOVERFLOW: ::libc::c_int = 75;
                                                    pub const ENOTUNIQ: ::libc::c_int = 76;
                                                    pub const EBADFD: ::libc::c_int = 77;
                                                    pub const EBADMSG: ::libc::c_int = 74;
                                                    pub const EREMCHG: ::libc::c_int = 78;
                                                    pub const ELIBACC: ::libc::c_int = 79;
                                                    pub const ELIBBAD: ::libc::c_int = 80;
                                                    pub const ELIBSCN: ::libc::c_int = 81;
                                                    pub const ELIBMAX: ::libc::c_int = 82;
                                                    pub const ELIBEXEC: ::libc::c_int = 83;
                                                    pub const EILSEQ: ::libc::c_int = 84;
                                                    pub const ERESTART: ::libc::c_int = 85;
                                                    pub const ESTRPIPE: ::libc::c_int = 86;
                                                    pub const EUSERS: ::libc::c_int = 87;
                                                    pub const ENOTSOCK: ::libc::c_int = 88;
                                                    pub const EDESTADDRREQ: ::libc::c_int = 89;
                                                    pub const EMSGSIZE: ::libc::c_int = 90;
                                                    pub const EPROTOTYPE: ::libc::c_int = 91;
                                                    pub const ENOPROTOOPT: ::libc::c_int = 92;
                                                    pub const EPROTONOSUPPORT: ::libc::c_int = 93;
                                                    pub const ESOCKTNOSUPPORT: ::libc::c_int = 94;
                                                    pub const EOPNOTSUPP: ::libc::c_int = 95;
                                                    pub const EPFNOSUPPORT: ::libc::c_int = 96;
                                                    pub const EAFNOSUPPORT: ::libc::c_int = 97;
                                                    pub const EADDRINUSE: ::libc::c_int = 98;
                                                    pub const EADDRNOTAVAIL: ::libc::c_int = 99;
                                                    pub const ENETDOWN: ::libc::c_int = 100;
                                                    pub const ENETUNREACH: ::libc::c_int = 101;
                                                    pub const ENETRESET: ::libc::c_int = 102;
                                                    pub const ECONNABORTED: ::libc::c_int = 103;
                                                    pub const ECONNRESET: ::libc::c_int = 104;
                                                    pub const ENOBUFS: ::libc::c_int = 105;
                                                    pub const EISCONN: ::libc::c_int = 106;
                                                    pub const ENOTCONN: ::libc::c_int = 107;
                                                    pub const ESHUTDOWN: ::libc::c_int = 108;
                                                    pub const ETOOMANYREFS: ::libc::c_int = 109;
                                                    pub const ETIMEDOUT: ::libc::c_int = 110;
                                                    pub const ECONNREFUSED: ::libc::c_int = 111;
                                                    pub const EHOSTDOWN: ::libc::c_int = 112;
                                                    pub const EHOSTUNREACH: ::libc::c_int = 113;
                                                    pub const EALREADY: ::libc::c_int = 114;
                                                    pub const EINPROGRESS: ::libc::c_int = 115;
                                                    pub const ESTALE: ::libc::c_int = 116;
                                                    pub const EUCLEAN: ::libc::c_int = 117;
                                                    pub const ENOTNAM: ::libc::c_int = 118;
                                                    pub const ENAVAIL: ::libc::c_int = 119;
                                                    pub const EISNAM: ::libc::c_int = 120;
                                                    pub const EREMOTEIO: ::libc::c_int = 121;
                                                    pub const EDQUOT: ::libc::c_int = 122;
                                                    pub const ENOMEDIUM: ::libc::c_int = 123;
                                                    pub const EMEDIUMTYPE: ::libc::c_int = 124;
                                                    pub const ECANCELED: ::libc::c_int = 125;
                                                    pub const ENOKEY: ::libc::c_int = 126;
                                                    pub const EKEYEXPIRED: ::libc::c_int = 127;
                                                    pub const EKEYREVOKED: ::libc::c_int = 128;
                                                    pub const EKEYREJECTED: ::libc::c_int = 129;
                                                    pub const EOWNERDEAD: ::libc::c_int = 130;
                                                    pub const ENOTRECOVERABLE: ::libc::c_int = 131;
                                                    pub const EHWPOISON: ::libc::c_int = 133;
                                                    pub const ERFKILL: ::libc::c_int = 132;

                                                    pub const SOCK_STREAM: ::libc::c_int = 1;
                                                    pub const SOCK_DGRAM: ::libc::c_int = 2;

                                                    pub const SOL_SOCKET: ::libc::c_int = 1;

                                                    pub const SO_REUSEADDR: ::libc::c_int = 2;
                                                    pub const SO_TYPE: ::libc::c_int = 3;
                                                    pub const SO_ERROR: ::libc::c_int = 4;
                                                    pub const SO_DONTROUTE: ::libc::c_int = 5;
                                                    pub const SO_BROADCAST: ::libc::c_int = 6;
                                                    pub const SO_SNDBUF: ::libc::c_int = 7;
                                                    pub const SO_RCVBUF: ::libc::c_int = 8;
                                                    pub const SO_KEEPALIVE: ::libc::c_int = 9;
                                                    pub const SO_OOBINLINE: ::libc::c_int = 10;
                                                    pub const SO_LINGER: ::libc::c_int = 13;
                                                    pub const SO_REUSEPORT: ::libc::c_int = 15;
                                                    pub const SO_RCVLOWAT: ::libc::c_int = 18;
                                                    pub const SO_SNDLOWAT: ::libc::c_int = 19;
                                                    pub const SO_RCVTIMEO: ::libc::c_int = 20;
                                                    pub const SO_SNDTIMEO: ::libc::c_int = 21;
                                                    pub const SO_ACCEPTCONN: ::libc::c_int = 30;

                                                    pub const TCP_COOKIE_TRANSACTIONS: ::libc::c_int = 15;
                                                    pub const TCP_THIN_LINEAR_TIMEOUTS: ::libc::c_int = 16;
                                                    pub const TCP_THIN_DUPACK: ::libc::c_int = 17;
                                                    pub const TCP_USER_TIMEOUT: ::libc::c_int = 18;
                                                    pub const TCP_REPAIR: ::libc::c_int = 19;
                                                    pub const TCP_REPAIR_QUEUE: ::libc::c_int = 20;
                                                    pub const TCP_QUEUE_SEQ: ::libc::c_int = 21;
                                                    pub const TCP_REPAIR_OPTIONS: ::libc::c_int = 22;
                                                    pub const TCP_FASTOPEN: ::libc::c_int = 23;
                                                    pub const TCP_TIMESTAMP: ::libc::c_int = 24;

                                                    pub const FIOCLEX: ::libc::c_ulong = 0x5451;
                                                    pub const FIONBIO: ::libc::c_ulong = 0x5421;

                                                    pub const SA_ONSTACK: ::libc::c_int = 0x08000000;
                                                    pub const SA_SIGINFO: ::libc::c_int = 0x00000004;
                                                    pub const SA_NOCLDWAIT: ::libc::c_int = 0x00000002;

                                                    pub const SIGCHLD: ::libc::c_int = 17;
                                                    pub const SIGBUS: ::libc::c_int = 7;
                                                    pub const SIG_SETMASK: ::libc::c_int = 2;

                                                    extern
                                                    {
                                                        pub fn getnameinfo(sa: *const ::libc::sockaddr,
                                                                        salen: ::libc::socklen_t,
                                                                        host: *mut ::libc::c_char,
                                                                        hostlen: ::libc::socklen_t,
                                                                        serv: *mut ::libc::c_char,
                                                                        sevlen: ::libc::socklen_t,
                                                                        flags: ::libc::c_int) -> ::libc::c_int;
                                                    }

                                                    cfg_if! 
                                                    {
                                                        if #[cfg(any(target_arch = "x86", target_arch = "arm"))]
                                                        {
                                                            mod b32
                                                            {
                                                                //! 32-bit specific definitions for linux-like values
                                                                use ::
                                                                {
                                                                    *,
                                                                };
                                                                
                                                                pub type c_long = i32;
                                                                pub type c_ulong = u32;
                                                                pub type clock_t = i32;
                                                                pub type time_t = i32;
                                                                pub type suseconds_t = i32;
                                                                pub type ino_t = u32;
                                                                pub type off_t = i32;
                                                                pub type blkcnt_t = i32;

                                                                pub type blksize_t = i32;
                                                                pub type nlink_t = u32;

                                                                pub const __SIZEOF_PTHREAD_MUTEX_T: usize = 24;
                                                                pub const __SIZEOF_PTHREAD_RWLOCK_T: usize = 32;
                                                                pub const __SIZEOF_PTHREAD_MUTEXATTR_T: usize = 4;

                                                                s!
                                                                {
                                                                    pub struct stat
                                                                    {
                                                                        pub st_dev: ::libc::dev_t,
                                                                        __pad1: ::libc::c_short,
                                                                        pub st_ino: ::libc::ino_t,
                                                                        pub st_mode: ::libc::mode_t,
                                                                        pub st_nlink: ::libc::nlink_t,
                                                                        pub st_uid: ::libc::uid_t,
                                                                        pub st_gid: ::libc::gid_t,
                                                                        pub st_rdev: ::libc::dev_t,
                                                                        __pad2: ::libc::c_short,
                                                                        pub st_size: ::libc::off_t,
                                                                        pub st_blksize: ::libc::blksize_t,
                                                                        pub st_blocks: ::libc::blkcnt_t,
                                                                        pub st_atime: ::libc::time_t,
                                                                        pub st_atime_nsec: ::libc::c_long,
                                                                        pub st_mtime: ::libc::time_t,
                                                                        pub st_mtime_nsec: ::libc::c_long,
                                                                        pub st_ctime: ::libc::time_t,
                                                                        pub st_ctime_nsec: ::libc::c_long,
                                                                        __unused4: ::libc::c_long,
                                                                        __unused5: ::libc::c_long,
                                                                    }

                                                                    pub struct stat64 {
                                                                        pub st_dev: ::libc::dev_t,
                                                                        __pad1: ::libc::c_uint,
                                                                        __st_ino: ::libc::ino_t,
                                                                        pub st_mode: ::libc::mode_t,
                                                                        pub st_nlink: ::libc::nlink_t,
                                                                        pub st_uid: ::libc::uid_t,
                                                                        pub st_gid: ::libc::gid_t,
                                                                        pub st_rdev: ::libc::dev_t,
                                                                        __pad2: ::libc::c_uint,
                                                                        pub st_size: ::libc::off64_t,
                                                                        pub st_blksize: ::libc::blksize_t,
                                                                        pub st_blocks: ::libc::blkcnt64_t,
                                                                        pub st_atime: ::libc::time_t,
                                                                        pub st_atime_nsec: ::libc::c_long,
                                                                        pub st_mtime: ::libc::time_t,
                                                                        pub st_mtime_nsec: ::libc::c_long,
                                                                        pub st_ctime: ::libc::time_t,
                                                                        pub st_ctime_nsec: ::libc::c_long,
                                                                        pub st_ino: ::libc::ino64_t,
                                                                    }

                                                                    pub struct pthread_attr_t {
                                                                        __size: [u32; 9]
                                                                    }

                                                                    pub struct sigset_t {
                                                                        __val: [::libc::c_ulong; 32],
                                                                    }
                                                                }

                                                                cfg_if!
                                                                {
                                                                    if #[cfg(target_arch = "x86")]
                                                                    {
                                                                        mod x86
                                                                        {
                                                                            use ::
                                                                            {
                                                                                *,
                                                                            };
                                                                            pub type c_char = i8;
                                                                            pub type wchar_t = i32;
                                                                        }
                                                                        
                                                                        pub use self::x86::*;
                                                                    }
                                                                    
                                                                    else if #[cfg(target_arch = "arm")]
                                                                    {
                                                                        mod arm
                                                                        {
                                                                            use ::
                                                                            {
                                                                                *,
                                                                            };
                                                                            
                                                                            pub type c_char = u8;
                                                                            pub type wchar_t = u32;

                                                                        }
                                                                        pub use self::arm::*;
                                                                    }
                                                                    
                                                                    else { }
                                                                }

                                                            }
                                                            
                                                            pub use self::b32::*;
                                                        }
                                                        
                                                        else if #[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))] 
                                                        {
                                                            mod b64
                                                            {
                                                                //! 64-bit specific definitions for linux-like values
                                                                use ::
                                                                {
                                                                    *,
                                                                };
                                                                
                                                                pub type c_long = i64;
                                                                pub type c_ulong = u64;
                                                                pub type clock_t = i64;
                                                                pub type time_t = i64;
                                                                pub type suseconds_t = i64;
                                                                pub type ino_t = u64;
                                                                pub type off_t = i64;
                                                                pub type blkcnt_t = i64;

                                                                s! {
                                                                    pub struct sigset_t {
                                                                        __val: [::libc::c_ulong; 16],
                                                                    }
                                                                }

                                                                pub const __SIZEOF_PTHREAD_RWLOCK_T: usize = 56;

                                                                cfg_if!
                                                                {
                                                                    if #[cfg(target_arch = "aarch64")]
                                                                    {
                                                                        mod aarch64
                                                                        {
                                                                            //! AArch64-specific definitions for 64-bit linux-like values
                                                                            use ::
                                                                            {
                                                                                *,
                                                                            };
                                                                            
                                                                            pub type c_char = u8;
                                                                            pub type wchar_t = u32;
                                                                            pub type nlink_t = u32;
                                                                            pub type blksize_t = i32;

                                                                            pub const __SIZEOF_PTHREAD_MUTEX_T: usize = 48;
                                                                            pub const __SIZEOF_PTHREAD_MUTEXATTR_T: usize = 8;

                                                                            s!
                                                                            {
                                                                                pub struct stat {
                                                                                    pub st_dev: ::libc::dev_t,
                                                                                    pub st_ino: ::libc::ino_t,
                                                                                    pub st_mode: ::libc::mode_t,
                                                                                    pub st_nlink: ::libc::nlink_t,
                                                                                    pub st_uid: ::libc::uid_t,
                                                                                    pub st_gid: ::libc::gid_t,
                                                                                    pub st_rdev: ::libc::dev_t,
                                                                                    __pad1: ::libc::dev_t,
                                                                                    pub st_size: ::libc::off_t,
                                                                                    pub st_blksize: ::libc::blksize_t,
                                                                                    __pad2: ::libc::c_int,
                                                                                    pub st_blocks: ::libc::blkcnt_t,
                                                                                    pub st_atime: ::libc::time_t,
                                                                                    pub st_atime_nsec: ::libc::c_long,
                                                                                    pub st_mtime: ::libc::time_t,
                                                                                    pub st_mtime_nsec: ::libc::c_long,
                                                                                    pub st_ctime: ::libc::time_t,
                                                                                    pub st_ctime_nsec: ::libc::c_long,
                                                                                    __unused: [::libc::c_int; 2],
                                                                                }

                                                                                pub struct stat64 {
                                                                                    pub st_dev: ::libc::dev_t,
                                                                                    pub st_ino: ::libc::ino_t,
                                                                                    pub st_mode: ::libc::mode_t,
                                                                                    pub st_nlink: ::libc::nlink_t,
                                                                                    pub st_uid: ::libc::uid_t,
                                                                                    pub st_gid: ::libc::gid_t,
                                                                                    pub st_rdev: ::libc::dev_t,
                                                                                    __pad1: ::libc::dev_t,
                                                                                    pub st_size: ::libc::off64_t,
                                                                                    pub st_blksize: ::libc::blksize_t,
                                                                                    __pad2: ::libc::c_int,
                                                                                    pub st_blocks: ::libc::blkcnt64_t,
                                                                                    pub st_atime: ::libc::time_t,
                                                                                    pub st_atime_nsec: ::libc::c_long,
                                                                                    pub st_mtime: ::libc::time_t,
                                                                                    pub st_mtime_nsec: ::libc::c_long,
                                                                                    pub st_ctime: ::libc::time_t,
                                                                                    pub st_ctime_nsec: ::libc::c_long,
                                                                                    __unused: [::libc::c_int; 2],
                                                                                }

                                                                                pub struct pthread_attr_t {
                                                                                    __size: [u64; 8]
                                                                                }
                                                                            }

                                                                        }
                                                                        
                                                                        pub use self::aarch64::*;
                                                                    }
                                                                    
                                                                    else
                                                                    {
                                                                        mod x86_64
                                                                        {
                                                                            //! x86_64-specific definitions for 64-bit linux-like values
                                                                            use ::
                                                                            {
                                                                                *,
                                                                            };
                                                                            
                                                                            pub type c_char = i8;
                                                                            pub type wchar_t = i32;
                                                                            pub type nlink_t = u64;
                                                                            pub type blksize_t = i64;

                                                                            pub const __SIZEOF_PTHREAD_MUTEX_T: usize = 40;
                                                                            pub const __SIZEOF_PTHREAD_MUTEXATTR_T: usize = 4;

                                                                            s! {
                                                                                pub struct stat {
                                                                                    pub st_dev: ::libc::dev_t,
                                                                                    pub st_ino: ::libc::ino_t,
                                                                                    pub st_nlink: ::libc::nlink_t,
                                                                                    pub st_mode: ::libc::mode_t,
                                                                                    pub st_uid: ::libc::uid_t,
                                                                                    pub st_gid: ::libc::gid_t,
                                                                                    __pad0: ::libc::c_int,
                                                                                    pub st_rdev: ::libc::dev_t,
                                                                                    pub st_size: ::libc::off_t,
                                                                                    pub st_blksize: ::libc::blksize_t,
                                                                                    pub st_blocks: ::libc::blkcnt_t,
                                                                                    pub st_atime: ::libc::time_t,
                                                                                    pub st_atime_nsec: ::libc::c_long,
                                                                                    pub st_mtime: ::libc::time_t,
                                                                                    pub st_mtime_nsec: ::libc::c_long,
                                                                                    pub st_ctime: ::libc::time_t,
                                                                                    pub st_ctime_nsec: ::libc::c_long,
                                                                                    __unused: [::libc::c_long; 3],
                                                                                }

                                                                                pub struct stat64 {
                                                                                    pub st_dev: ::libc::dev_t,
                                                                                    pub st_ino: ::libc::ino64_t,
                                                                                    pub st_nlink: ::libc::nlink_t,
                                                                                    pub st_mode: ::libc::mode_t,
                                                                                    pub st_uid: ::libc::uid_t,
                                                                                    pub st_gid: ::libc::gid_t,
                                                                                    __pad0: ::libc::c_int,
                                                                                    pub st_rdev: ::libc::dev_t,
                                                                                    pub st_size: ::libc::off_t,
                                                                                    pub st_blksize: ::libc::blksize_t,
                                                                                    pub st_blocks: ::libc::blkcnt64_t,
                                                                                    pub st_atime: ::libc::time_t,
                                                                                    pub st_atime_nsec: ::libc::c_long,
                                                                                    pub st_mtime: ::libc::time_t,
                                                                                    pub st_mtime_nsec: ::libc::c_long,
                                                                                    pub st_ctime: ::libc::time_t,
                                                                                    pub st_ctime_nsec: ::libc::c_long,
                                                                                    __reserved: [::libc::c_long; 3],
                                                                                }

                                                                                pub struct pthread_attr_t {
                                                                                    __size: [u64; 7]
                                                                                }
                                                                            }

                                                                        }
                                                                        
                                                                        pub use self::x86_64::*;
                                                                    }
                                                                }
                                                            }
                                                            pub use self::b64::*;
                                                        }
                                                        
                                                        else { }
                                                    }

                                                }
                                                
                                                pub use self::notmips::*;
                                            }
                                        }

                                    }
                                    
                                    pub use self::linux::*;
                                }
                                
                                else if #[cfg(target_os = "android")]
                                {
                                    mod android
                                    {
                                        //! Android-specific definitions for linux-like values
                                        use ::
                                        {
                                            *
                                        };
                                        
                                        pub type c_char = u8;
                                        pub type c_long = i32;
                                        pub type c_ulong = u32;
                                        pub type clock_t = i32;
                                        pub type time_t = i32;
                                        pub type suseconds_t = i32;
                                        pub type wchar_t = u32;
                                        pub type off_t = i32;
                                        pub type ino_t = u32;
                                        pub type blkcnt_t = u32;
                                        pub type blksize_t = u32;
                                        pub type dev_t = u32;
                                        pub type mode_t = u16;
                                        pub type nlink_t = u16;
                                        pub type useconds_t = i32;
                                        pub type socklen_t = i32;
                                        pub type pthread_t = c_long;
                                        pub type pthread_mutexattr_t = ::libc::c_long;
                                        pub type sigset_t = c_ulong;

                                        s! {
                                            pub struct stat {
                                                pub st_dev: ::libc::c_ulonglong,
                                                __pad0: [::libc::c_uchar; 4],
                                                __st_ino: ::libc::ino_t,
                                                pub st_mode: ::libc::c_uint,
                                                pub st_nlink: ::libc::c_uint,
                                                pub st_uid: ::libc::c_ulong,
                                                pub st_gid: ::libc::c_ulong,
                                                pub st_rdev: ::libc::c_ulonglong,
                                                __pad3: [::libc::c_uchar; 4],
                                                pub st_size: ::libc::c_longlong,
                                                pub st_blksize: blksize_t,
                                                pub st_blocks: ::libc::c_ulonglong,
                                                pub st_atime: ::libc::c_ulong,
                                                pub st_atime_nsec: ::libc::c_ulong,
                                                pub st_mtime: ::libc::c_ulong,
                                                pub st_mtime_nsec: ::libc::c_ulong,
                                                pub st_ctime: ::libc::c_ulong,
                                                pub st_ctime_nsec: ::libc::c_ulong,
                                                pub st_ino: ::libc::c_ulonglong,
                                            }

                                            pub struct dirent {
                                                pub d_ino: u64,
                                                pub d_off: i64,
                                                pub d_reclen: ::libc::c_ushort,
                                                pub d_type: ::libc::c_uchar,
                                                pub d_name: [::libc::c_char; 256],
                                            }

                                            pub struct pthread_attr_t {
                                                pub flags: ::libc::uint32_t,
                                                pub stack_base: *mut ::libc::c_void,
                                                pub stack_size: ::libc::size_t,
                                                pub guard_size: ::libc::size_t,
                                                pub sched_policy: ::libc::int32_t,
                                                pub sched_priority: ::libc::int32_t,
                                            }

                                            pub struct pthread_mutex_t { value: ::libc::c_int }

                                            pub struct pthread_cond_t { value: ::libc::c_int }

                                            pub struct pthread_rwlock_t {
                                                lock: pthread_mutex_t,
                                                cond: pthread_cond_t,
                                                numLocks: ::libc::c_int,
                                                writerThreadId: ::libc::c_int,
                                                pendingReaders: ::libc::c_int,
                                                pendingWriters: ::libc::c_int,
                                                reserved: [*mut ::libc::c_void; 4],
                                            }

                                            pub struct passwd {
                                                pub pw_name: *mut ::libc::c_char,
                                                pub pw_passwd: *mut ::libc::c_char,
                                                pub pw_uid: ::libc::uid_t,
                                                pub pw_gid: ::libc::gid_t,
                                                pub pw_dir: *mut ::libc::c_char,
                                                pub pw_shell: *mut ::libc::c_char,
                                            }

                                            pub struct stack_t {
                                                pub ss_sp: *mut ::libc::c_void,
                                                pub ss_flags: ::libc::c_int,
                                                pub ss_size: ::libc::size_t
                                            }

                                            pub struct siginfo_t {
                                                pub si_signo: ::libc::c_int,
                                                pub si_errno: ::libc::c_int,
                                                pub si_code: ::libc::c_int,
                                                pub _pad: [::libc::c_int; 29],
                                            }
                                        }

                                        pub const BUFSIZ: ::libc::c_uint = 1024;
                                        pub const FILENAME_MAX: ::libc::c_uint = 1024;
                                        pub const FOPEN_MAX: ::libc::c_uint = 20;
                                        pub const L_tmpnam: ::libc::c_uint = 1024;
                                        pub const TMP_MAX: ::libc::c_uint = 308915776;
                                        pub const _PC_NAME_MAX: ::libc::c_int = 4;

                                        pub const FIONBIO: ::libc::c_int = 0x5421;

                                        pub const _SC_ARG_MAX: ::libc::c_int = 0;
                                        pub const _SC_BC_BASE_MAX: ::libc::c_int = 1;
                                        pub const _SC_BC_DIM_MAX: ::libc::c_int = 2;
                                        pub const _SC_BC_SCALE_MAX: ::libc::c_int = 3;
                                        pub const _SC_BC_STRING_MAX: ::libc::c_int = 4;
                                        pub const _SC_CHILD_MAX: ::libc::c_int = 5;
                                        pub const _SC_CLK_TCK: ::libc::c_int = 6;
                                        pub const _SC_COLL_WEIGHTS_MAX: ::libc::c_int = 7;
                                        pub const _SC_EXPR_NEST_MAX: ::libc::c_int = 8;
                                        pub const _SC_LINE_MAX: ::libc::c_int = 9;
                                        pub const _SC_NGROUPS_MAX: ::libc::c_int = 10;
                                        pub const _SC_OPEN_MAX: ::libc::c_int = 11;
                                        pub const _SC_2_C_BIND: ::libc::c_int = 13;
                                        pub const _SC_2_C_DEV: ::libc::c_int = 14;
                                        pub const _SC_2_C_VERSION: ::libc::c_int = 15;
                                        pub const _SC_2_CHAR_TERM: ::libc::c_int = 16;
                                        pub const _SC_2_FORT_DEV: ::libc::c_int = 17;
                                        pub const _SC_2_FORT_RUN: ::libc::c_int = 18;
                                        pub const _SC_2_LOCALEDEF: ::libc::c_int = 19;
                                        pub const _SC_2_SW_DEV: ::libc::c_int = 20;
                                        pub const _SC_2_UPE: ::libc::c_int = 21;
                                        pub const _SC_2_VERSION: ::libc::c_int = 22;
                                        pub const _SC_JOB_CONTROL: ::libc::c_int = 23;
                                        pub const _SC_SAVED_IDS: ::libc::c_int = 24;
                                        pub const _SC_VERSION: ::libc::c_int = 25;
                                        pub const _SC_RE_DUP_MAX: ::libc::c_int = 26;
                                        pub const _SC_STREAM_MAX: ::libc::c_int = 27;
                                        pub const _SC_TZNAME_MAX: ::libc::c_int = 28;
                                        pub const _SC_XOPEN_CRYPT: ::libc::c_int = 29;
                                        pub const _SC_XOPEN_ENH_I18N: ::libc::c_int = 30;
                                        pub const _SC_XOPEN_SHM: ::libc::c_int = 31;
                                        pub const _SC_XOPEN_VERSION: ::libc::c_int = 32;
                                        pub const _SC_XOPEN_XCU_VERSION: ::libc::c_int = 33;
                                        pub const _SC_XOPEN_REALTIME: ::libc::c_int = 34;
                                        pub const _SC_XOPEN_REALTIME_THREADS: ::libc::c_int = 35;
                                        pub const _SC_XOPEN_LEGACY: ::libc::c_int = 36;
                                        pub const _SC_ATEXIT_MAX: ::libc::c_int = 37;
                                        pub const _SC_IOV_MAX: ::libc::c_int = 38;
                                        pub const _SC_PAGESIZE: ::libc::c_int = 39;
                                        pub const _SC_XOPEN_UNIX: ::libc::c_int = 41;
                                        pub const _SC_MQ_PRIO_MAX: ::libc::c_int = 51;
                                        pub const _SC_GETGR_R_SIZE_MAX: ::libc::c_int = 71;
                                        pub const _SC_GETPW_R_SIZE_MAX: ::libc::c_int = 72;
                                        pub const _SC_LOGIN_NAME_MAX: ::libc::c_int = 73;
                                        pub const _SC_THREAD_DESTRUCTOR_ITERATIONS: ::libc::c_int = 74;
                                        pub const _SC_THREAD_KEYS_MAX: ::libc::c_int = 75;
                                        pub const _SC_THREAD_STACK_MIN: ::libc::c_int = 76;
                                        pub const _SC_THREAD_THREADS_MAX: ::libc::c_int = 77;
                                        pub const _SC_TTY_NAME_MAX: ::libc::c_int = 78;
                                        pub const _SC_THREADS: ::libc::c_int = 79;
                                        pub const _SC_THREAD_ATTR_STACKADDR: ::libc::c_int = 80;
                                        pub const _SC_THREAD_ATTR_STACKSIZE: ::libc::c_int = 81;
                                        pub const _SC_THREAD_PRIORITY_SCHEDULING: ::libc::c_int = 82;
                                        pub const _SC_THREAD_PRIO_INHERIT: ::libc::c_int = 83;
                                        pub const _SC_THREAD_PRIO_PROTECT: ::libc::c_int = 84;
                                        pub const _SC_THREAD_SAFE_FUNCTIONS: ::libc::c_int = 85;

                                        pub const PTHREAD_STACK_MIN: ::libc::size_t = 8192;
                                        pub const PTHREAD_MUTEX_INITIALIZER: pthread_mutex_t = pthread_mutex_t {
                                            value: 0,
                                        };
                                        pub const PTHREAD_COND_INITIALIZER: pthread_cond_t = pthread_cond_t {
                                            value: 0,
                                        };
                                        pub const PTHREAD_RWLOCK_INITIALIZER: pthread_rwlock_t = pthread_rwlock_t {
                                            lock: PTHREAD_MUTEX_INITIALIZER,
                                            cond: PTHREAD_COND_INITIALIZER,
                                            numLocks: 0,
                                            writerThreadId: 0,
                                            pendingReaders: 0,
                                            pendingWriters: 0,
                                            reserved: [0 as *mut _; 4],
                                        };
                                        pub const PTHREAD_MUTEX_RECURSIVE: ::libc::c_int = 1;

                                        pub const FIOCLEX: ::libc::c_ulong = 0x5451;

                                        pub const SA_ONSTACK: ::libc::c_ulong = 0x08000000;
                                        pub const SA_SIGINFO: ::libc::c_ulong = 0x00000004;
                                        pub const SA_NOCLDWAIT: ::libc::c_ulong = 0x00000002;

                                        pub const SIGCHLD: ::libc::c_int = 17;
                                        pub const SIGBUS: ::libc::c_int = 7;
                                        pub const SIG_SETMASK: ::libc::c_int = 2;

                                        pub const O_ACCMODE: ::libc::c_int = 3;
                                        pub const O_SYNC: ::libc::c_int = 0x1000;

                                        pub const RUSAGE_CHILDREN: ::libc::c_int = -1;

                                        extern {
                                            pub fn madvise(addr: *const ::libc::c_void, len: ::libc::size_t, advice: ::libc::c_int)
                                                        -> ::libc::c_int;
                                            pub fn ioctl(fd: ::libc::c_int, request: ::libc::c_int, ...) -> ::libc::c_int;
                                            pub fn putenv(string: *const ::libc::c_char) -> ::libc::c_int;
                                            pub fn readlink(path: *const ::libc::c_char,
                                                            buf: *mut ::libc::c_char,
                                                            bufsz: ::libc::size_t)
                                                            -> ::libc::c_int;
                                            pub fn msync(addr: *const ::libc::c_void, len: ::libc::size_t,
                                                        flags: ::libc::c_int) -> ::libc::c_int;
                                            pub fn mprotect(addr: *const ::libc::c_void, len: ::libc::size_t, prot: ::libc::c_int)
                                                            -> ::libc::c_int;
                                            pub fn sysconf(name: ::libc::c_int) -> ::libc::c_long;
                                            pub fn usleep(secs: ::libc::c_ulong) -> ::libc::c_int;
                                            pub fn recvfrom(socket: ::libc::c_int, buf: *mut ::libc::c_void, len: ::libc::size_t,
                                                            flags: ::libc::c_uint, addr: *const ::sockaddr,
                                                            addrlen: *mut ::libc::socklen_t) -> ::libc::ssize_t;
                                            pub fn send(socket: ::libc::c_int, buf: *const ::libc::c_void, len: ::libc::size_t,
                                                        flags: ::libc::c_uint) -> ::libc::ssize_t;
                                            pub fn recv(socket: ::libc::c_int, buf: *mut ::libc::c_void, len: ::libc::size_t,
                                                        flags: ::libc::c_uint) -> ::libc::ssize_t;
                                            pub fn getnameinfo(sa: *const ::sockaddr,
                                                            salen: ::libc::socklen_t,
                                                            host: *mut ::libc::c_char,
                                                            hostlen: ::libc::size_t,
                                                            serv: *mut ::libc::c_char,
                                                            sevlen: ::libc::size_t,
                                                            flags: ::libc::c_int) -> ::libc::c_int;
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(target_pointer_width = "32")]
                                            {
                                                mod b32
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    
                                                    s!
                                                    {
                                                        pub struct sigaction {
                                                            pub sa_sigaction: ::libc::sighandler_t,
                                                            pub sa_mask: ::libc::sigset_t,
                                                            pub sa_flags: ::libc::c_ulong,
                                                            pub sa_restorer: Option<extern fn()>,
                                                        }
                                                    }

                                                }
                                                
                                                pub use self::b32::*;
                                            }
                                            
                                            else if #[cfg(target_pointer_width = "64")]
                                            {
                                                mod b64
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    
                                                    s! {
                                                        pub struct sigaction {
                                                            pub sa_flags: ::libc::c_uint,
                                                            pub sa_sigaction: sighandler_t,
                                                            pub sa_mask: sigset_t,
                                                            _restorer: *mut ::libc::c_void,
                                                        }
                                                    }

                                                    

                                                }
                                                
                                                pub use self::b64::*;
                                            }
                                            
                                            else { }
                                        }



                                    }
                                    
                                    pub use self::android::*;
                                }
                                
                                else { }
                            }
                            /*
                            //! x86_64-specific definitions for 64-bit linux-like values

                            use ::prelude::*;
                            use ::{off64_t, off_t};*/

                            pub type wchar_t = i32;
                            pub type nlink_t = u64;
                            pub type blksize_t = i64;
                            pub type greg_t = i64;
                            pub type suseconds_t = i64;
                            pub type __u64 = ::libc::c_ulonglong;
                            pub type __s64 = ::libc::c_longlong;

                            s!
                            {
                                pub struct sigaction {
                                    pub sa_sigaction: ::libc::sighandler_t,
                                    pub sa_mask: ::libc::sigset_t,
                                    #[cfg(target_arch = "sparc64")]
                                    __reserved0: ::libc::c_int,
                                    pub sa_flags: ::libc::c_int,
                                    pub sa_restorer: Option<extern "C" fn()>,
                                } 

                                pub struct flock {
                                    pub l_type: ::libc::c_short,
                                    pub l_whence: ::libc::c_short,
                                    pub l_start: off_t,
                                    pub l_len: off_t,
                                    pub l_pid: ::libc::pid_t,
                                }

                                pub struct flock64 {
                                    pub l_type: ::libc::c_short,
                                    pub l_whence: ::libc::c_short,
                                    pub l_start: off64_t,
                                    pub l_len: off64_t,
                                    pub l_pid: ::libc::pid_t,
                                }

                                pub struct siginfo_t {
                                    pub si_signo: ::libc::c_int,
                                    pub si_errno: ::libc::c_int,
                                    pub si_code: ::libc::c_int,
                                    #[doc(hidden)]
                                    #[deprecated(
                                        since = "0.2.54",
                                        note = "Please leave a comment on \
                                            https://github.com/rust-lang/libc/pull/1316 if you're using \
                                            this field"
                                    )]
                                    pub _pad: [::libc::c_int; 29],
                                    _align: [u64; 0],
                                }

                                pub struct stack_t {
                                    pub ss_sp: *mut ::libc::c_void,
                                    pub ss_flags: ::libc::c_int,
                                    pub ss_size: ::libc::size_t,
                                }

                                pub struct stat {
                                    pub st_dev: ::libc::dev_t,
                                    pub st_ino: ::libc::ino_t,
                                    pub st_nlink: ::libc::nlink_t,
                                    pub st_mode: ::libc::mode_t,
                                    pub st_uid: ::libc::uid_t,
                                    pub st_gid: ::libc::gid_t,
                                    __pad0: ::libc::c_int,
                                    pub st_rdev: ::libc::dev_t,
                                    pub st_size: off_t,
                                    pub st_blksize: ::libc::blksize_t,
                                    pub st_blocks: ::libc::blkcnt_t,
                                    pub st_atime: ::libc::time_t,
                                    pub st_atime_nsec: i64,
                                    pub st_mtime: ::libc::time_t,
                                    pub st_mtime_nsec: i64,
                                    pub st_ctime: ::libc::time_t,
                                    pub st_ctime_nsec: i64,
                                    __unused: [i64; 3],
                                }

                                pub struct stat64 {
                                    pub st_dev: ::libc::dev_t,
                                    pub st_ino: ::libc::ino64_t,
                                    pub st_nlink: ::libc::nlink_t,
                                    pub st_mode: ::libc::mode_t,
                                    pub st_uid: ::libc::uid_t,
                                    pub st_gid: ::libc::gid_t,
                                    __pad0: ::libc::c_int,
                                    pub st_rdev: ::libc::dev_t,
                                    pub st_size: off_t,
                                    pub st_blksize: ::libc::blksize_t,
                                    pub st_blocks: ::libc::blkcnt64_t,
                                    pub st_atime: ::libc::time_t,
                                    pub st_atime_nsec: i64,
                                    pub st_mtime: ::libc::time_t,
                                    pub st_mtime_nsec: i64,
                                    pub st_ctime: ::libc::time_t,
                                    pub st_ctime_nsec: i64,
                                    __reserved: [i64; 3],
                                } 

                                pub struct statvfs64 {
                                    pub f_bsize: ::libc::c_ulong,
                                    pub f_frsize: ::libc::c_ulong,
                                    pub f_blocks: u64,
                                    pub f_bfree: u64,
                                    pub f_bavail: u64,
                                    pub f_files: u64,
                                    pub f_ffree: u64,
                                    pub f_favail: u64,
                                    pub f_fsid: ::libc::c_ulong,
                                    pub f_flag: ::libc::c_ulong,
                                    pub f_namemax: ::libc::c_ulong,
                                    __f_spare: [::libc::c_int; 6],
                                }

                                pub struct pthread_attr_t {
                                    #[cfg(target_pointer_width = "32")]
                                    __size: [u32; 8],
                                    #[cfg(target_pointer_width = "64")]
                                    __size: [u64; 7],
                                }

                                pub struct _libc_fpxreg {
                                    pub significand: [u16; 4],
                                    pub exponent: u16,
                                    __private: [u16; 3],
                                }

                                pub struct _libc_xmmreg {
                                    pub element: [u32; 4],
                                }

                                pub struct _libc_fpstate {
                                    pub cwd: u16,
                                    pub swd: u16,
                                    pub ftw: u16,
                                    pub fop: u16,
                                    pub rip: u64,
                                    pub rdp: u64,
                                    pub mxcsr: u32,
                                    pub mxcr_mask: u32,
                                    pub _st: [_libc_fpxreg; 8],
                                    pub _xmm: [_libc_xmmreg; 16],
                                    __private: [u64; 12],
                                }

                                pub struct user_regs_struct {
                                    pub r15: ::libc::c_ulonglong,
                                    pub r14: ::libc::c_ulonglong,
                                    pub r13: ::libc::c_ulonglong,
                                    pub r12: ::libc::c_ulonglong,
                                    pub rbp: ::libc::c_ulonglong,
                                    pub rbx: ::libc::c_ulonglong,
                                    pub r11: ::libc::c_ulonglong,
                                    pub r10: ::libc::c_ulonglong,
                                    pub r9: ::libc::c_ulonglong,
                                    pub r8: ::libc::c_ulonglong,
                                    pub rax: ::libc::c_ulonglong,
                                    pub rcx: ::libc::c_ulonglong,
                                    pub rdx: ::libc::c_ulonglong,
                                    pub rsi: ::libc::c_ulonglong,
                                    pub rdi: ::libc::c_ulonglong,
                                    pub orig_rax: ::libc::c_ulonglong,
                                    pub rip: ::libc::c_ulonglong,
                                    pub cs: ::libc::c_ulonglong,
                                    pub eflags: ::libc::c_ulonglong,
                                    pub rsp: ::libc::c_ulonglong,
                                    pub ss: ::libc::c_ulonglong,
                                    pub fs_base: ::libc::c_ulonglong,
                                    pub gs_base: ::libc::c_ulonglong,
                                    pub ds: ::libc::c_ulonglong,
                                    pub es: ::libc::c_ulonglong,
                                    pub fs: ::libc::c_ulonglong,
                                    pub gs: ::libc::c_ulonglong,
                                }
                            } 
                            

                                #[repr(align(8))]
                                pub struct clone_args
                                {
                                    pub flags: ::libc::c_ulonglong,
                                    pub pidfd: ::libc::c_ulonglong,
                                    pub child_tid: ::libc::c_ulonglong,
                                    pub parent_tid: ::libc::c_ulonglong,
                                    pub exit_signal: ::libc::c_ulonglong,
                                    pub stack: ::libc::c_ulonglong,
                                    pub stack_size: ::libc::c_ulonglong,
                                    pub tls: ::libc::c_ulonglong,
                                    pub set_tid: ::libc::c_ulonglong,
                                    pub set_tid_size: ::libc::c_ulonglong,
                                    pub cgroup: ::libc::c_ulonglong,
                                }

                            pub const POSIX_FADV_DONTNEED: ::libc::c_int = 4;
                            pub const POSIX_FADV_NOREUSE: ::libc::c_int = 5;

                            pub const VEOF: usize = 4;
                            pub const RTLD_DEEPBIND: ::libc::c_int = 0x8;
                            pub const RTLD_GLOBAL: ::libc::c_int = 0x100;
                            pub const RTLD_NOLOAD: ::libc::c_int = 0x4;

                            pub const O_APPEND: ::libc::c_int = 1024;
                            pub const O_CREAT: ::libc::c_int = 64;
                            pub const O_EXCL: ::libc::c_int = 128;
                            pub const O_NOCTTY: ::libc::c_int = 256;
                            pub const O_NONBLOCK: ::libc::c_int = 2048;
                            pub const O_SYNC: ::libc::c_int = 1052672;
                            pub const O_RSYNC: ::libc::c_int = 1052672;
                            pub const O_DSYNC: ::libc::c_int = 4096;
                            pub const O_FSYNC: ::libc::c_int = 0x101000;
                            pub const O_NOATIME: ::libc::c_int = 0o1000000;
                            pub const O_PATH: ::libc::c_int = 0o10000000;
                            pub const O_TMPFILE: ::libc::c_int = 0o20000000 | O_DIRECTORY;

                            pub const MADV_SOFT_OFFLINE: ::libc::c_int = 101;
                            pub const MAP_GROWSDOWN: ::libc::c_int = 0x0100;

                            pub const EDEADLK: ::libc::c_int = 35;
                            pub const ENAMETOOLONG: ::libc::c_int = 36;
                            pub const ENOLCK: ::libc::c_int = 37;
                            pub const ENOSYS: ::libc::c_int = 38;
                            pub const ENOTEMPTY: ::libc::c_int = 39;
                            pub const ELOOP: ::libc::c_int = 40;
                            pub const ENOMSG: ::libc::c_int = 42;
                            pub const EIDRM: ::libc::c_int = 43;
                            pub const ECHRNG: ::libc::c_int = 44;
                            pub const EL2NSYNC: ::libc::c_int = 45;
                            pub const EL3HLT: ::libc::c_int = 46;
                            pub const EL3RST: ::libc::c_int = 47;
                            pub const ELNRNG: ::libc::c_int = 48;
                            pub const EUNATCH: ::libc::c_int = 49;
                            pub const ENOCSI: ::libc::c_int = 50;
                            pub const EL2HLT: ::libc::c_int = 51;
                            pub const EBADE: ::libc::c_int = 52;
                            pub const EBADR: ::libc::c_int = 53;
                            pub const EXFULL: ::libc::c_int = 54;
                            pub const ENOANO: ::libc::c_int = 55;
                            pub const EBADRQC: ::libc::c_int = 56;
                            pub const EBADSLT: ::libc::c_int = 57;
                            pub const EMULTIHOP: ::libc::c_int = 72;
                            pub const EOVERFLOW: ::libc::c_int = 75;
                            pub const ENOTUNIQ: ::libc::c_int = 76;
                            pub const EBADFD: ::libc::c_int = 77;
                            pub const EBADMSG: ::libc::c_int = 74;
                            pub const EREMCHG: ::libc::c_int = 78;
                            pub const ELIBACC: ::libc::c_int = 79;
                            pub const ELIBBAD: ::libc::c_int = 80;
                            pub const ELIBSCN: ::libc::c_int = 81;
                            pub const ELIBMAX: ::libc::c_int = 82;
                            pub const ELIBEXEC: ::libc::c_int = 83;
                            pub const EILSEQ: ::libc::c_int = 84;
                            pub const ERESTART: ::libc::c_int = 85;
                            pub const ESTRPIPE: ::libc::c_int = 86;
                            pub const EUSERS: ::libc::c_int = 87;
                            pub const ENOTSOCK: ::libc::c_int = 88;
                            pub const EDESTADDRREQ: ::libc::c_int = 89;
                            pub const EMSGSIZE: ::libc::c_int = 90;
                            pub const EPROTOTYPE: ::libc::c_int = 91;
                            pub const ENOPROTOOPT: ::libc::c_int = 92;
                            pub const EPROTONOSUPPORT: ::libc::c_int = 93;
                            pub const ESOCKTNOSUPPORT: ::libc::c_int = 94;
                            pub const EOPNOTSUPP: ::libc::c_int = 95;
                            pub const EPFNOSUPPORT: ::libc::c_int = 96;
                            pub const EAFNOSUPPORT: ::libc::c_int = 97;
                            pub const EADDRINUSE: ::libc::c_int = 98;
                            pub const EADDRNOTAVAIL: ::libc::c_int = 99;
                            pub const ENETDOWN: ::libc::c_int = 100;
                            pub const ENETUNREACH: ::libc::c_int = 101;
                            pub const ENETRESET: ::libc::c_int = 102;
                            pub const ECONNABORTED: ::libc::c_int = 103;
                            pub const ECONNRESET: ::libc::c_int = 104;
                            pub const ENOBUFS: ::libc::c_int = 105;
                            pub const EISCONN: ::libc::c_int = 106;
                            pub const ENOTCONN: ::libc::c_int = 107;
                            pub const ESHUTDOWN: ::libc::c_int = 108;
                            pub const ETOOMANYREFS: ::libc::c_int = 109;
                            pub const ETIMEDOUT: ::libc::c_int = 110;
                            pub const ECONNREFUSED: ::libc::c_int = 111;
                            pub const EHOSTDOWN: ::libc::c_int = 112;
                            pub const EHOSTUNREACH: ::libc::c_int = 113;
                            pub const EALREADY: ::libc::c_int = 114;
                            pub const EINPROGRESS: ::libc::c_int = 115;
                            pub const ESTALE: ::libc::c_int = 116;
                            pub const EDQUOT: ::libc::c_int = 122;
                            pub const ENOMEDIUM: ::libc::c_int = 123;
                            pub const EMEDIUMTYPE: ::libc::c_int = 124;
                            pub const ECANCELED: ::libc::c_int = 125;
                            pub const ENOKEY: ::libc::c_int = 126;
                            pub const EKEYEXPIRED: ::libc::c_int = 127;
                            pub const EKEYREVOKED: ::libc::c_int = 128;
                            pub const EKEYREJECTED: ::libc::c_int = 129;
                            pub const EOWNERDEAD: ::libc::c_int = 130;
                            pub const ENOTRECOVERABLE: ::libc::c_int = 131;
                            pub const EHWPOISON: ::libc::c_int = 133;
                            pub const ERFKILL: ::libc::c_int = 132;

                            pub const SOCK_STREAM: ::libc::c_int = 1;
                            pub const SOCK_DGRAM: ::libc::c_int = 2;

                            pub const SA_ONSTACK: ::libc::c_int = 0x08000000;
                            pub const SA_SIGINFO: ::libc::c_int = 0x00000004;
                            pub const SA_NOCLDWAIT: ::libc::c_int = 0x00000002;

                            pub const SIGTTIN: ::libc::c_int = 21;
                            pub const SIGTTOU: ::libc::c_int = 22;
                            pub const SIGXCPU: ::libc::c_int = 24;
                            pub const SIGXFSZ: ::libc::c_int = 25;
                            pub const SIGVTALRM: ::libc::c_int = 26;
                            pub const SIGPROF: ::libc::c_int = 27;
                            pub const SIGWINCH: ::libc::c_int = 28;
                            pub const SIGCHLD: ::libc::c_int = 17;
                            pub const SIGBUS: ::libc::c_int = 7;
                            pub const SIGUSR1: ::libc::c_int = 10;
                            pub const SIGUSR2: ::libc::c_int = 12;
                            pub const SIGCONT: ::libc::c_int = 18;
                            pub const SIGSTOP: ::libc::c_int = 19;
                            pub const SIGTSTP: ::libc::c_int = 20;
                            pub const SIGURG: ::libc::c_int = 23;
                            pub const SIGIO: ::libc::c_int = 29;
                            pub const SIGSYS: ::libc::c_int = 31;
                            pub const SIGSTKFLT: ::libc::c_int = 16;
                            #[deprecated(since = "0.2.55", note = "Use SIGSYS instead")]
                            pub const SIGUNUSED: ::libc::c_int = 31;
                            pub const SIGPOLL: ::libc::c_int = 29;
                            pub const SIGPWR: ::libc::c_int = 30;
                            pub const SIG_SETMASK: ::libc::c_int = 2;
                            pub const SIG_BLOCK: ::libc::c_int = 0x000000;
                            pub const SIG_UNBLOCK: ::libc::c_int = 0x01;

                            pub const POLLWRNORM: ::libc::c_short = 0x100;
                            pub const POLLWRBAND: ::libc::c_short = 0x200;

                            pub const O_ASYNC: ::libc::c_int = 0x2000;
                            pub const O_NDELAY: ::libc::c_int = 0x800;

                            pub const PTRACE_DETACH: ::libc::c_uint = 17;
                            pub const PTRACE_GET_RSEQ_CONFIGURATION: ::libc::c_uint = 0x420f;

                            pub const EFD_NONBLOCK: ::libc::c_int = 0x800;

                            pub const F_GETLK: ::libc::c_int = 5;
                            pub const F_GETOWN: ::libc::c_int = 9;
                            pub const F_SETOWN: ::libc::c_int = 8;
                            pub const F_SETLK: ::libc::c_int = 6;
                            pub const F_SETLKW: ::libc::c_int = 7;
                            pub const F_OFD_GETLK: ::libc::c_int = 36;
                            pub const F_OFD_SETLK: ::libc::c_int = 37;
                            pub const F_OFD_SETLKW: ::libc::c_int = 38;

                            pub const F_RDLCK: ::libc::c_int = 0;
                            pub const F_WRLCK: ::libc::c_int = 1;
                            pub const F_UNLCK: ::libc::c_int = 2;

                            pub const SFD_NONBLOCK: ::libc::c_int = 0x0800;

                            pub const TCSANOW: ::libc::c_int = 0;
                            pub const TCSADRAIN: ::libc::c_int = 1;
                            pub const TCSAFLUSH: ::libc::c_int = 2;

                            pub const SFD_CLOEXEC: ::libc::c_int = 0x080000;

                            pub const NCCS: usize = 32;
                            /*
                            pub const O_TRUNC: ::libc::c_int = 512;

                            pub const O_CLOEXEC: ::libc::c_int = 0x80000;

                            pub const EBFONT: ::libc::c_int = 59;
                            pub const ENOSTR: ::libc::c_int = 60;
                            pub const ENODATA: ::libc::c_int = 61;
                            pub const ETIME: ::libc::c_int = 62;
                            pub const ENOSR: ::libc::c_int = 63;
                            pub const ENONET: ::libc::c_int = 64;
                            pub const ENOPKG: ::libc::c_int = 65;
                            pub const EREMOTE: ::libc::c_int = 66;
                            pub const ENOLINK: ::libc::c_int = 67;
                            pub const EADV: ::libc::c_int = 68;
                            pub const ESRMNT: ::libc::c_int = 69;
                            pub const ECOMM: ::libc::c_int = 70;
                            pub const EPROTO: ::libc::c_int = 71;
                            pub const EDOTDOT: ::libc::c_int = 73;

                            pub const SA_NODEFER: ::libc::c_int = 0x40000000;
                            pub const SA_RESETHAND: ::libc::c_int = 0x80000000;
                            pub const SA_RESTART: ::libc::c_int = 0x10000000;
                            pub const SA_NOCLDSTOP: ::libc::c_int = 0x00000001; */

                            pub const EPOLL_CLOEXEC: ::libc::c_int = 0x80000;

                            pub const EFD_CLOEXEC: ::libc::c_int = 0x80000;

                            pub const __SIZEOF_PTHREAD_CONDATTR_T: usize = 4;
                            pub const __SIZEOF_PTHREAD_MUTEXATTR_T: usize = 4;
                            pub const __SIZEOF_PTHREAD_BARRIERATTR_T: usize = 4;

                            pub const O_DIRECT: ::libc::c_int = 0x4000;
                            pub const O_DIRECTORY: ::libc::c_int = 0x10000;
                            pub const O_NOFOLLOW: ::libc::c_int = 0x20000;

                            pub const MAP_HUGETLB: ::libc::c_int = 0x040000;
                            pub const MAP_LOCKED: ::libc::c_int = 0x02000;
                            pub const MAP_NORESERVE: ::libc::c_int = 0x04000;
                            pub const MAP_32BIT: ::libc::c_int = 0x0040;
                            pub const MAP_ANON: ::libc::c_int = 0x0020;
                            pub const MAP_ANONYMOUS: ::libc::c_int = 0x0020;
                            pub const MAP_DENYWRITE: ::libc::c_int = 0x0800;
                            pub const MAP_EXECUTABLE: ::libc::c_int = 0x01000;
                            pub const MAP_POPULATE: ::libc::c_int = 0x08000;
                            pub const MAP_NONBLOCK: ::libc::c_int = 0x010000;
                            pub const MAP_STACK: ::libc::c_int = 0x020000;
                            pub const MAP_SYNC: ::libc::c_int = 0x080000;

                            pub const EDEADLOCK: ::libc::c_int = 35;
                            pub const EUCLEAN: ::libc::c_int = 117;
                            pub const ENOTNAM: ::libc::c_int = 118;
                            pub const ENAVAIL: ::libc::c_int = 119;
                            pub const EISNAM: ::libc::c_int = 120;
                            pub const EREMOTEIO: ::libc::c_int = 121;

                            pub const PTRACE_GETFPREGS: ::libc::c_uint = 14;
                            pub const PTRACE_SETFPREGS: ::libc::c_uint = 15;
                            pub const PTRACE_GETFPXREGS: ::libc::c_uint = 18;
                            pub const PTRACE_SETFPXREGS: ::libc::c_uint = 19;
                            pub const PTRACE_GETREGS: ::libc::c_uint = 12;
                            pub const PTRACE_SETREGS: ::libc::c_uint = 13;
                            pub const PTRACE_PEEKSIGINFO_SHARED: ::libc::c_uint = 1;
                            pub const PTRACE_SYSEMU: ::libc::c_uint = 31;
                            pub const PTRACE_SYSEMU_SINGLESTEP: ::libc::c_uint = 32;

                            pub const PR_GET_SPECULATION_CTRL: ::libc::c_int = 52;
                            pub const PR_SET_SPECULATION_CTRL: ::libc::c_int = 53;
                            pub const PR_SPEC_NOT_AFFECTED: ::libc::c_uint = 0;
                            pub const PR_SPEC_PRCTL: ::libc::c_uint = 1 << 0;
                            pub const PR_SPEC_ENABLE: ::libc::c_uint = 1 << 1;
                            pub const PR_SPEC_DISABLE: ::libc::c_uint = 1 << 2;
                            pub const PR_SPEC_FORCE_DISABLE: ::libc::c_uint = 1 << 3;
                            pub const PR_SPEC_DISABLE_NOEXEC: ::libc::c_uint = 1 << 4;
                            pub const PR_SPEC_STORE_BYPASS: ::libc::c_int = 0;
                            pub const PR_SPEC_INDIRECT_BRANCH: ::libc::c_int = 1;
                            // FIXME(linux): perharps for later
                            //pub const PR_SPEC_L1D_FLUSH: ::libc::c_int = 2;

                            //pub const MCL_CURRENT: ::libc::c_int = 0x0001;
                            //pub const MCL_FUTURE: ::libc::c_int = 0x0002;
                            pub const MCL_ONFAULT: ::libc::c_int = 0x0004;

                            //pub const SIGSTKSZ: ::libc::size_t = 8192;
                            pub const MINSIGSTKSZ: ::libc::size_t = 2048;
                            pub const VWERASE: usize = 14;
                            pub const VREPRINT: usize = 12;
                            pub const VSUSP: usize = 10;
                            pub const VSTART: usize = 8;
                            pub const VSTOP: usize = 9;
                            pub const VDISCARD: usize = 13;
                            pub const VTIME: usize = 5;

                            pub const VEOL: usize = 11;
                            pub const VEOL2: usize = 16;
                            pub const VMIN: usize = 6; 

                            // offsets in user_regs_structs, from sys/reg.h
                            pub const R15: ::libc::c_int = 0;
                            pub const R14: ::libc::c_int = 1;
                            pub const R13: ::libc::c_int = 2;
                            pub const R12: ::libc::c_int = 3;
                            pub const RBP: ::libc::c_int = 4;
                            pub const RBX: ::libc::c_int = 5;
                            pub const R11: ::libc::c_int = 6;
                            pub const R10: ::libc::c_int = 7;
                            pub const R9: ::libc::c_int = 8;
                            pub const R8: ::libc::c_int = 9;
                            pub const RAX: ::libc::c_int = 10;
                            pub const RCX: ::libc::c_int = 11;
                            pub const RDX: ::libc::c_int = 12;
                            pub const RSI: ::libc::c_int = 13;
                            pub const RDI: ::libc::c_int = 14;
                            pub const ORIG_RAX: ::libc::c_int = 15;
                            pub const RIP: ::libc::c_int = 16;
                            pub const CS: ::libc::c_int = 17;
                            pub const EFLAGS: ::libc::c_int = 18;
                            pub const RSP: ::libc::c_int = 19;
                            pub const SS: ::libc::c_int = 20;
                            pub const FS_BASE: ::libc::c_int = 21;
                            pub const GS_BASE: ::libc::c_int = 22;
                            pub const DS: ::libc::c_int = 23;
                            pub const ES: ::libc::c_int = 24;
                            pub const FS: ::libc::c_int = 25;
                            pub const GS: ::libc::c_int = 26;

                            // offsets in mcontext_t.gregs from sys/ucontext.h
                            pub const REG_R8: ::libc::c_int = 0;
                            pub const REG_R9: ::libc::c_int = 1;
                            pub const REG_R10: ::libc::c_int = 2;
                            pub const REG_R11: ::libc::c_int = 3;
                            pub const REG_R12: ::libc::c_int = 4;
                            pub const REG_R13: ::libc::c_int = 5;
                            pub const REG_R14: ::libc::c_int = 6;
                            pub const REG_R15: ::libc::c_int = 7;
                            pub const REG_RDI: ::libc::c_int = 8;
                            pub const REG_RSI: ::libc::c_int = 9;
                            pub const REG_RBP: ::libc::c_int = 10;
                            pub const REG_RBX: ::libc::c_int = 11;
                            pub const REG_RDX: ::libc::c_int = 12;
                            pub const REG_RAX: ::libc::c_int = 13;
                            pub const REG_RCX: ::libc::c_int = 14;
                            pub const REG_RSP: ::libc::c_int = 15;
                            pub const REG_RIP: ::libc::c_int = 16;
                            pub const REG_EFL: ::libc::c_int = 17;
                            pub const REG_CSGSFS: ::libc::c_int = 18;
                            pub const REG_ERR: ::libc::c_int = 19;
                            pub const REG_TRAPNO: ::libc::c_int = 20;
                            pub const REG_OLDMASK: ::libc::c_int = 21;
                            pub const REG_CR2: ::libc::c_int = 22; 
                        }
                        pub use self::notbsd::*;
                    }
                    
                    else if #[cfg(any(target_os = "macos",
                    target_os = "ios",
                    target_os = "freebsd",
                    target_os = "dragonfly",
                    target_os = "openbsd",
                    target_os = "netbsd",
                    target_os = "bitrig"))]
                    {
                        mod bsd
                        {
                            use ::
                            {
                                *,
                            };
                            
                            pub type c_char = i8;
                            pub type wchar_t = i32;
                            pub type off_t = i64;
                            pub type useconds_t = u32;
                            pub type blkcnt_t = i64;
                            pub type socklen_t = u32;
                            pub type sa_family_t = u8;
                            pub type pthread_t = ::uintptr_t;

                            s! 
                            {
                                pub struct sockaddr {
                                    pub sa_len: u8,
                                    pub sa_family: sa_family_t,
                                    pub sa_data: [::libc::c_char; 14],
                                }

                                pub struct sockaddr_in {
                                    pub sin_len: u8,
                                    pub sin_family: sa_family_t,
                                    pub sin_port: ::libc::in_port_t,
                                    pub sin_addr: ::libc::in_addr,
                                    pub sin_zero: [::libc::c_char; 8],
                                }

                                pub struct sockaddr_in6 {
                                    pub sin6_len: u8,
                                    pub sin6_family: sa_family_t,
                                    pub sin6_port: ::libc::in_port_t,
                                    pub sin6_flowinfo: u32,
                                    pub sin6_addr: ::libc::in6_addr,
                                    pub sin6_scope_id: u32,
                                }

                                pub struct sockaddr_un {
                                    pub sun_len: u8,
                                    pub sun_family: sa_family_t,
                                    pub sun_path: [c_char; 104]
                                }

                                pub struct passwd {
                                    pub pw_name: *mut ::libc::c_char,
                                    pub pw_passwd: *mut ::libc::c_char,
                                    pub pw_uid: ::libc::uid_t,
                                    pub pw_gid: ::libc::gid_t,
                                    pub pw_change: ::libc::time_t,
                                    pub pw_class: *mut ::libc::c_char,
                                    pub pw_gecos: *mut ::libc::c_char,
                                    pub pw_dir: *mut ::libc::c_char,
                                    pub pw_shell: *mut ::libc::c_char,
                                    pub pw_expire: ::libc::time_t,

                                    #[cfg(not(any(target_os = "macos", target_os = "ios")))]
                                    pub pw_fields: ::libc::c_int,
                                }

                                pub struct ifaddrs {
                                    pub ifa_next: *mut ifaddrs,
                                    pub ifa_name: *mut ::libc::c_char,
                                    pub ifa_flags: ::libc::c_uint,
                                    pub ifa_addr: *mut ::libc::sockaddr,
                                    pub ifa_netmask: *mut ::libc::sockaddr,
                                    pub ifa_dstaddr: *mut ::libc::sockaddr,
                                    pub ifa_data: *mut ::libc::c_void
                                }

                                pub struct fd_set {
                                    fds_bits: [i32; FD_SETSIZE / 32],
                                }
                            }

                            pub const FIOCLEX: ::libc::c_ulong = 0x20006601;
                            pub const FIONBIO: ::libc::c_ulong = 0x8004667e;

                            pub const SA_ONSTACK: ::libc::c_int = 0x0001;
                            pub const SA_SIGINFO: ::libc::c_int = 0x0040;
                            pub const SA_RESTART: ::libc::c_int = 0x0002;
                            pub const SA_RESETHAND: ::libc::c_int = 0x0004;
                            pub const SA_NOCLDSTOP: ::libc::c_int = 0x0008;
                            pub const SA_NODEFER: ::libc::c_int = 0x0010;
                            pub const SA_NOCLDWAIT: ::libc::c_int = 0x0020;

                            pub const SIGCHLD: ::libc::c_int = 20;
                            pub const SIGBUS: ::libc::c_int = 10;
                            pub const SIG_SETMASK: ::libc::c_int = 3;

                            pub const IPV6_MULTICAST_LOOP: ::libc::c_int = 11;
                            pub const IPV6_V6ONLY: ::libc::c_int = 27;

                            pub const FD_SETSIZE: usize = 1024;

                            f!
                            {
                                pub fn FD_CLR(fd: ::libc::c_int, set: *mut fd_set) -> () {
                                    let fd = fd as usize;
                                    (*set).fds_bits[fd / 32] &= !(1 << (fd % 32));
                                    return
                                }

                                pub fn FD_ISSET(fd: ::libc::c_int, set: *mut fd_set) -> bool {
                                    let fd = fd as usize;
                                    return ((*set).fds_bits[fd / 32] & (1 << (fd % 32))) != 0
                                }

                                pub fn FD_SET(fd: ::libc::c_int, set: *mut fd_set) -> () {
                                    let fd = fd as usize;
                                    (*set).fds_bits[fd / 32] |= 1 << (fd % 32);
                                    return
                                }

                                pub fn FD_ZERO(set: *mut fd_set) -> () {
                                    for slot in (*set).fds_bits.iter_mut() {
                                        *slot = 0;
                                    }
                                }

                                pub fn WIFEXITED(status: ::libc::c_int) -> bool {
                                    (status & 0x7f) == 0
                                }

                                pub fn WEXITSTATUS(status: ::libc::c_int) -> ::libc::c_int {
                                    status >> 8
                                }

                                pub fn WTERMSIG(status: ::libc::c_int) -> ::libc::c_int {
                                    status & 0o177
                                }
                            }

                            extern 
                            {
                                pub fn mincore(addr: *const ::libc::c_void, len: ::libc::size_t,
                                            vec: *mut c_char) -> ::libc::c_int;
                                pub fn sysctlnametomib(name: *const c_char,
                                                    mibp: *mut ::libc::c_int,
                                                    sizep: *mut ::libc::size_t)
                                                    -> ::libc::c_int;
                                pub fn setgroups(ngroups: ::libc::c_int,
                                                ptr: *const ::libc::gid_t) -> ::libc::c_int;
                                pub fn ioctl(fd: ::libc::c_int, request: ::libc::c_ulong, ...) -> ::libc::c_int;
                                pub fn getnameinfo(sa: *const ::sockaddr,
                                                salen: ::libc::socklen_t,
                                                host: *mut ::libc::c_char,
                                                hostlen: ::libc::socklen_t,
                                                serv: *mut ::libc::c_char,
                                                sevlen: ::libc::socklen_t,
                                                flags: ::libc::c_int) -> ::libc::c_int;
                            }

                            cfg_if!
                            {
                                if #[cfg(any(target_os = "macos", target_os = "ios"))]
                                {
                                    mod apple
                                    {
                                        use ::
                                        {
                                            *,
                                        };
                                        //! Apple (ios/darwin)-specific definitions
                                        pub type clock_t = c_ulong;
                                        pub type time_t = c_long;
                                        pub type suseconds_t = i32;
                                        pub type dev_t = i32;
                                        pub type ino_t = u64;
                                        pub type mode_t = u16;
                                        pub type nlink_t = u16;
                                        pub type blksize_t = i32;
                                        pub type rlim_t = u64;
                                        pub type mach_timebase_info_data_t = mach_timebase_info;
                                        pub type pthread_key_t = c_ulong;
                                        pub type sigset_t = u32;

                                        pub enum timezone {}

                                        s! 
                                        {
                                            pub struct glob_t {
                                                pub gl_pathc:  ::size_t,
                                                __unused1: ::libc::c_int,
                                                pub gl_offs:   ::size_t,
                                                __unused2: ::libc::c_int,
                                                pub gl_pathv:  *mut *mut ::libc::c_char,

                                                __unused3: *mut ::libc::c_void,

                                                __unused4: *mut ::libc::c_void,
                                                __unused5: *mut ::libc::c_void,
                                                __unused6: *mut ::libc::c_void,
                                                __unused7: *mut ::libc::c_void,
                                                __unused8: *mut ::libc::c_void,
                                            }

                                            pub struct sockaddr_storage {
                                                pub ss_len: u8,
                                                pub ss_family: ::libc::sa_family_t,
                                                __ss_pad1: [u8; 6],
                                                __ss_align: i64,
                                                __ss_pad2: [u8; 112],
                                            }

                                            pub struct addrinfo {
                                                pub ai_flags: ::libc::c_int,
                                                pub ai_family: ::libc::c_int,
                                                pub ai_socktype: ::libc::c_int,
                                                pub ai_protocol: ::libc::c_int,
                                                pub ai_addrlen: ::libc::socklen_t,
                                                pub ai_canonname: *mut ::libc::c_char,
                                                pub ai_addr: *mut ::libc::sockaddr,
                                                pub ai_next: *mut addrinfo,
                                            }

                                            pub struct mach_timebase_info {
                                                pub numer: u32,
                                                pub denom: u32,
                                            }

                                            pub struct stat {
                                                pub st_dev: dev_t,
                                                pub st_mode: mode_t,
                                                pub st_nlink: nlink_t,
                                                pub st_ino: ino_t,
                                                pub st_uid: ::libc::uid_t,
                                                pub st_gid: ::libc::gid_t,
                                                pub st_rdev: dev_t,
                                                pub st_atime: time_t,
                                                pub st_atime_nsec: ::libc::c_long,
                                                pub st_mtime: time_t,
                                                pub st_mtime_nsec: ::libc::c_long,
                                                pub st_ctime: time_t,
                                                pub st_ctime_nsec: ::libc::c_long,
                                                pub st_birthtime: time_t,
                                                pub st_birthtime_nsec: ::libc::c_long,
                                                pub st_size: ::libc::off_t,
                                                pub st_blocks: ::blkcnt_t,
                                                pub st_blksize: blksize_t,
                                                pub st_flags: ::libc::uint32_t,
                                                pub st_gen: ::libc::uint32_t,
                                                pub st_lspare: ::libc::int32_t,
                                                pub st_qspare: [::libc::int64_t; 2],
                                            }

                                            pub struct dirent {
                                                pub d_ino: u64,
                                                pub d_seekoff: u64,
                                                pub d_reclen: u16,
                                                pub d_namlen: u16,
                                                pub d_type: u8,
                                                pub d_name: [::libc::c_char; 1024],
                                            }

                                            pub struct pthread_mutex_t {
                                                __sig: ::libc::c_long,
                                                __opaque: [u8; __PTHREAD_MUTEX_SIZE__],
                                            }

                                            pub struct pthread_mutexattr_t {
                                                __sig: ::libc::c_long,
                                                __opaque: [u8; 8],
                                            }

                                            pub struct pthread_cond_t {
                                                __sig: ::libc::c_long,
                                                __opaque: [u8; __PTHREAD_COND_SIZE__],
                                            }

                                            pub struct pthread_rwlock_t {
                                                __sig: ::libc::c_long,
                                                __opaque: [u8; __PTHREAD_RWLOCK_SIZE__],
                                            }

                                            pub struct siginfo_t {
                                                pub si_signo: ::libc::c_int,
                                                pub si_errno: ::libc::c_int,
                                                pub si_code: ::libc::c_int,
                                                pub si_pid: ::libc::pid_t,
                                                pub si_uid: ::libc::uid_t,
                                                pub si_status: ::libc::c_int,
                                                pub si_addr: *mut ::libc::c_void,
                                                _pad: [usize; 9],
                                            }

                                            pub struct sigaction {
                                                pub sa_sigaction: ::libc::sighandler_t,
                                                pub sa_mask: sigset_t,
                                                pub sa_flags: ::libc::c_int,
                                            }

                                            pub struct stack_t {
                                                pub ss_sp: *mut ::libc::c_void,
                                                pub ss_size: ::libc::size_t,
                                                pub ss_flags: ::libc::c_int,
                                            }
                                        }

                                        pub const EXIT_FAILURE: ::libc::c_int = 1;
                                        pub const EXIT_SUCCESS: ::libc::c_int = 0;
                                        pub const RAND_MAX: ::libc::c_int = 2147483647;
                                        pub const EOF: ::libc::c_int = -1;
                                        pub const SEEK_SET: ::libc::c_int = 0;
                                        pub const SEEK_CUR: ::libc::c_int = 1;
                                        pub const SEEK_END: ::libc::c_int = 2;
                                        pub const _IOFBF: ::libc::c_int = 0;
                                        pub const _IONBF: ::libc::c_int = 2;
                                        pub const _IOLBF: ::libc::c_int = 1;
                                        pub const BUFSIZ: ::libc::c_uint = 1024;
                                        pub const FOPEN_MAX: ::libc::c_uint = 20;
                                        pub const FILENAME_MAX: ::libc::c_uint = 1024;
                                        pub const L_tmpnam: ::libc::c_uint = 1024;
                                        pub const TMP_MAX: ::libc::c_uint = 308915776;
                                        pub const _PC_NAME_MAX: ::libc::c_int = 4;

                                        pub const O_RDONLY: ::libc::c_int = 0;
                                        pub const O_WRONLY: ::libc::c_int = 1;
                                        pub const O_RDWR: ::libc::c_int = 2;
                                        pub const O_APPEND: ::libc::c_int = 8;
                                        pub const O_CREAT: ::libc::c_int = 512;
                                        pub const O_EXCL: ::libc::c_int = 2048;
                                        pub const O_NOCTTY: ::libc::c_int = 131072;
                                        pub const O_TRUNC: ::libc::c_int = 1024;
                                        pub const O_CLOEXEC: ::libc::c_int = 0x1000000;
                                        pub const S_IFIFO: mode_t = 4096;
                                        pub const S_IFCHR: mode_t = 8192;
                                        pub const S_IFBLK: mode_t = 24576;
                                        pub const S_IFDIR: mode_t = 16384;
                                        pub const S_IFREG: mode_t = 32768;
                                        pub const S_IFLNK: mode_t = 40960;
                                        pub const S_IFSOCK: mode_t = 49152;
                                        pub const S_IFMT: mode_t = 61440;
                                        pub const S_IEXEC: mode_t = 64;
                                        pub const S_IWRITE: mode_t = 128;
                                        pub const S_IREAD: mode_t = 256;
                                        pub const S_IRWXU: mode_t = 448;
                                        pub const S_IXUSR: mode_t = 64;
                                        pub const S_IWUSR: mode_t = 128;
                                        pub const S_IRUSR: mode_t = 256;
                                        pub const S_IRWXG: mode_t = 56;
                                        pub const S_IXGRP: mode_t = 8;
                                        pub const S_IWGRP: mode_t = 16;
                                        pub const S_IRGRP: mode_t = 32;
                                        pub const S_IRWXO: mode_t = 7;
                                        pub const S_IXOTH: mode_t = 1;
                                        pub const S_IWOTH: mode_t = 2;
                                        pub const S_IROTH: mode_t = 4;
                                        pub const F_OK: ::libc::c_int = 0;
                                        pub const R_OK: ::libc::c_int = 4;
                                        pub const W_OK: ::libc::c_int = 2;
                                        pub const X_OK: ::libc::c_int = 1;
                                        pub const STDIN_FILENO: ::libc::c_int = 0;
                                        pub const STDOUT_FILENO: ::libc::c_int = 1;
                                        pub const STDERR_FILENO: ::libc::c_int = 2;
                                        pub const F_LOCK: ::libc::c_int = 1;
                                        pub const F_TEST: ::libc::c_int = 3;
                                        pub const F_TLOCK: ::libc::c_int = 2;
                                        pub const F_ULOCK: ::libc::c_int = 0;
                                        pub const F_DUPFD_CLOEXEC: ::libc::c_int = 67;
                                        pub const SIGHUP: ::libc::c_int = 1;
                                        pub const SIGINT: ::libc::c_int = 2;
                                        pub const SIGQUIT: ::libc::c_int = 3;
                                        pub const SIGILL: ::libc::c_int = 4;
                                        pub const SIGABRT: ::libc::c_int = 6;
                                        pub const SIGFPE: ::libc::c_int = 8;
                                        pub const SIGKILL: ::libc::c_int = 9;
                                        pub const SIGSEGV: ::libc::c_int = 11;
                                        pub const SIGPIPE: ::libc::c_int = 13;
                                        pub const SIGALRM: ::libc::c_int = 14;
                                        pub const SIGTERM: ::libc::c_int = 15;

                                        pub const PROT_NONE: ::libc::c_int = 0;
                                        pub const PROT_READ: ::libc::c_int = 1;
                                        pub const PROT_WRITE: ::libc::c_int = 2;
                                        pub const PROT_EXEC: ::libc::c_int = 4;

                                        pub const MAP_FILE: ::libc::c_int = 0x0000;
                                        pub const MAP_SHARED: ::libc::c_int = 0x0001;
                                        pub const MAP_PRIVATE: ::libc::c_int = 0x0002;
                                        pub const MAP_FIXED: ::libc::c_int = 0x0010;
                                        pub const MAP_ANON: ::libc::c_int = 0x1000;

                                        pub const MAP_FAILED: *mut ::libc::c_void = !0 as *mut ::libc::c_void;

                                        pub const MCL_CURRENT: ::libc::c_int = 0x0001;
                                        pub const MCL_FUTURE: ::libc::c_int = 0x0002;

                                        pub const MS_ASYNC: ::libc::c_int = 0x0001;
                                        pub const MS_INVALIDATE: ::libc::c_int = 0x0002;
                                        pub const MS_SYNC: ::libc::c_int = 0x0010;

                                        pub const MS_KILLPAGES: ::libc::c_int = 0x0004;
                                        pub const MS_DEACTIVATE: ::libc::c_int = 0x0008;

                                        pub const EPERM: ::libc::c_int = 1;
                                        pub const ENOENT: ::libc::c_int = 2;
                                        pub const ESRCH: ::libc::c_int = 3;
                                        pub const EINTR: ::libc::c_int = 4;
                                        pub const EIO: ::libc::c_int = 5;
                                        pub const ENXIO: ::libc::c_int = 6;
                                        pub const E2BIG: ::libc::c_int = 7;
                                        pub const ENOEXEC: ::libc::c_int = 8;
                                        pub const EBADF: ::libc::c_int = 9;
                                        pub const ECHILD: ::libc::c_int = 10;
                                        pub const EDEADLK: ::libc::c_int = 11;
                                        pub const ENOMEM: ::libc::c_int = 12;
                                        pub const EACCES: ::libc::c_int = 13;
                                        pub const EFAULT: ::libc::c_int = 14;
                                        pub const ENOTBLK: ::libc::c_int = 15;
                                        pub const EBUSY: ::libc::c_int = 16;
                                        pub const EEXIST: ::libc::c_int = 17;
                                        pub const EXDEV: ::libc::c_int = 18;
                                        pub const ENODEV: ::libc::c_int = 19;
                                        pub const ENOTDIR: ::libc::c_int = 20;
                                        pub const EISDIR: ::libc::c_int = 21;
                                        pub const EINVAL: ::libc::c_int = 22;
                                        pub const ENFILE: ::libc::c_int = 23;
                                        pub const EMFILE: ::libc::c_int = 24;
                                        pub const ENOTTY: ::libc::c_int = 25;
                                        pub const ETXTBSY: ::libc::c_int = 26;
                                        pub const EFBIG: ::libc::c_int = 27;
                                        pub const ENOSPC: ::libc::c_int = 28;
                                        pub const ESPIPE: ::libc::c_int = 29;
                                        pub const EROFS: ::libc::c_int = 30;
                                        pub const EMLINK: ::libc::c_int = 31;
                                        pub const EPIPE: ::libc::c_int = 32;
                                        pub const EDOM: ::libc::c_int = 33;
                                        pub const ERANGE: ::libc::c_int = 34;
                                        pub const EAGAIN: ::libc::c_int = 35;
                                        pub const EWOULDBLOCK: ::libc::c_int = EAGAIN;
                                        pub const EINPROGRESS: ::libc::c_int = 36;
                                        pub const EALREADY: ::libc::c_int = 37;
                                        pub const ENOTSOCK: ::libc::c_int = 38;
                                        pub const EDESTADDRREQ: ::libc::c_int = 39;
                                        pub const EMSGSIZE: ::libc::c_int = 40;
                                        pub const EPROTOTYPE: ::libc::c_int = 41;
                                        pub const ENOPROTOOPT: ::libc::c_int = 42;
                                        pub const EPROTONOSUPPORT: ::libc::c_int = 43;
                                        pub const ESOCKTNOSUPPORT: ::libc::c_int = 44;
                                        pub const ENOTSUP: ::libc::c_int = 45;
                                        pub const EPFNOSUPPORT: ::libc::c_int = 46;
                                        pub const EAFNOSUPPORT: ::libc::c_int = 47;
                                        pub const EADDRINUSE: ::libc::c_int = 48;
                                        pub const EADDRNOTAVAIL: ::libc::c_int = 49;
                                        pub const ENETDOWN: ::libc::c_int = 50;
                                        pub const ENETUNREACH: ::libc::c_int = 51;
                                        pub const ENETRESET: ::libc::c_int = 52;
                                        pub const ECONNABORTED: ::libc::c_int = 53;
                                        pub const ECONNRESET: ::libc::c_int = 54;
                                        pub const ENOBUFS: ::libc::c_int = 55;
                                        pub const EISCONN: ::libc::c_int = 56;
                                        pub const ENOTCONN: ::libc::c_int = 57;
                                        pub const ESHUTDOWN: ::libc::c_int = 58;
                                        pub const ETOOMANYREFS: ::libc::c_int = 59;
                                        pub const ETIMEDOUT: ::libc::c_int = 60;
                                        pub const ECONNREFUSED: ::libc::c_int = 61;
                                        pub const ELOOP: ::libc::c_int = 62;
                                        pub const ENAMETOOLONG: ::libc::c_int = 63;
                                        pub const EHOSTDOWN: ::libc::c_int = 64;
                                        pub const EHOSTUNREACH: ::libc::c_int = 65;
                                        pub const ENOTEMPTY: ::libc::c_int = 66;
                                        pub const EPROCLIM: ::libc::c_int = 67;
                                        pub const EUSERS: ::libc::c_int = 68;
                                        pub const EDQUOT: ::libc::c_int = 69;
                                        pub const ESTALE: ::libc::c_int = 70;
                                        pub const EREMOTE: ::libc::c_int = 71;
                                        pub const EBADRPC: ::libc::c_int = 72;
                                        pub const ERPCMISMATCH: ::libc::c_int = 73;
                                        pub const EPROGUNAVAIL: ::libc::c_int = 74;
                                        pub const EPROGMISMATCH: ::libc::c_int = 75;
                                        pub const EPROCUNAVAIL: ::libc::c_int = 76;
                                        pub const ENOLCK: ::libc::c_int = 77;
                                        pub const ENOSYS: ::libc::c_int = 78;
                                        pub const EFTYPE: ::libc::c_int = 79;
                                        pub const EAUTH: ::libc::c_int = 80;
                                        pub const ENEEDAUTH: ::libc::c_int = 81;
                                        pub const EPWROFF: ::libc::c_int = 82;
                                        pub const EDEVERR: ::libc::c_int = 83;
                                        pub const EOVERFLOW: ::libc::c_int = 84;
                                        pub const EBADEXEC: ::libc::c_int = 85;
                                        pub const EBADARCH: ::libc::c_int = 86;
                                        pub const ESHLIBVERS: ::libc::c_int = 87;
                                        pub const EBADMACHO: ::libc::c_int = 88;
                                        pub const ECANCELED: ::libc::c_int = 89;
                                        pub const EIDRM: ::libc::c_int = 90;
                                        pub const ENOMSG: ::libc::c_int = 91;
                                        pub const EILSEQ: ::libc::c_int = 92;
                                        pub const ENOATTR: ::libc::c_int = 93;
                                        pub const EBADMSG: ::libc::c_int = 94;
                                        pub const EMULTIHOP: ::libc::c_int = 95;
                                        pub const ENODATA: ::libc::c_int = 96;
                                        pub const ENOLINK: ::libc::c_int = 97;
                                        pub const ENOSR: ::libc::c_int = 98;
                                        pub const ENOSTR: ::libc::c_int = 99;
                                        pub const EPROTO: ::libc::c_int = 100;
                                        pub const ETIME: ::libc::c_int = 101;
                                        pub const EOPNOTSUPP: ::libc::c_int = 102;
                                        pub const ENOPOLICY: ::libc::c_int = 103;
                                        pub const ENOTRECOVERABLE: ::libc::c_int = 104;
                                        pub const EOWNERDEAD: ::libc::c_int = 105;
                                        pub const EQFULL: ::libc::c_int = 106;
                                        pub const ELAST: ::libc::c_int = 106;

                                        pub const F_DUPFD: ::libc::c_int = 0;
                                        pub const F_GETFD: ::libc::c_int = 1;
                                        pub const F_SETFD: ::libc::c_int = 2;
                                        pub const F_GETFL: ::libc::c_int = 3;
                                        pub const F_SETFL: ::libc::c_int = 4;

                                        pub const O_ACCMODE: ::libc::c_int = 3;

                                        pub const SIGTRAP: ::libc::c_int = 5;

                                        pub const GLOB_APPEND  : ::libc::c_int = 0x0001;
                                        pub const GLOB_DOOFFS  : ::libc::c_int = 0x0002;
                                        pub const GLOB_ERR     : ::libc::c_int = 0x0004;
                                        pub const GLOB_MARK    : ::libc::c_int = 0x0008;
                                        pub const GLOB_NOCHECK : ::libc::c_int = 0x0010;
                                        pub const GLOB_NOSORT  : ::libc::c_int = 0x0020;
                                        pub const GLOB_NOESCAPE: ::libc::c_int = 0x2000;

                                        pub const GLOB_NOSPACE : ::libc::c_int = -1;
                                        pub const GLOB_ABORTED : ::libc::c_int = -2;
                                        pub const GLOB_NOMATCH : ::libc::c_int = -3;

                                        pub const POSIX_MADV_NORMAL: ::libc::c_int = 0;
                                        pub const POSIX_MADV_RANDOM: ::libc::c_int = 1;
                                        pub const POSIX_MADV_SEQUENTIAL: ::libc::c_int = 2;
                                        pub const POSIX_MADV_WILLNEED: ::libc::c_int = 3;
                                        pub const POSIX_MADV_DONTNEED: ::libc::c_int = 4;

                                        pub const _SC_IOV_MAX: ::libc::c_int = 56;
                                        pub const _SC_GETGR_R_SIZE_MAX: ::libc::c_int = 70;
                                        pub const _SC_GETPW_R_SIZE_MAX: ::libc::c_int = 71;
                                        pub const _SC_LOGIN_NAME_MAX: ::libc::c_int = 73;
                                        pub const _SC_MQ_PRIO_MAX: ::libc::c_int = 75;
                                        pub const _SC_THREAD_ATTR_STACKADDR: ::libc::c_int = 82;
                                        pub const _SC_THREAD_ATTR_STACKSIZE: ::libc::c_int = 83;
                                        pub const _SC_THREAD_DESTRUCTOR_ITERATIONS: ::libc::c_int = 85;
                                        pub const _SC_THREAD_KEYS_MAX: ::libc::c_int = 86;
                                        pub const _SC_THREAD_PRIO_INHERIT: ::libc::c_int = 87;
                                        pub const _SC_THREAD_PRIO_PROTECT: ::libc::c_int = 88;
                                        pub const _SC_THREAD_PRIORITY_SCHEDULING: ::libc::c_int = 89;
                                        pub const _SC_THREAD_PROCESS_SHARED: ::libc::c_int = 90;
                                        pub const _SC_THREAD_SAFE_FUNCTIONS: ::libc::c_int = 91;
                                        pub const _SC_THREAD_STACK_MIN: ::libc::c_int = 93;
                                        pub const _SC_THREAD_THREADS_MAX: ::libc::c_int = 94;
                                        pub const _SC_THREADS: ::libc::c_int = 96;
                                        pub const _SC_TTY_NAME_MAX: ::libc::c_int = 101;
                                        pub const _SC_ATEXIT_MAX: ::libc::c_int = 107;
                                        pub const _SC_XOPEN_CRYPT: ::libc::c_int = 108;
                                        pub const _SC_XOPEN_ENH_I18N: ::libc::c_int = 109;
                                        pub const _SC_XOPEN_LEGACY: ::libc::c_int = 110;
                                        pub const _SC_XOPEN_REALTIME: ::libc::c_int = 111;
                                        pub const _SC_XOPEN_REALTIME_THREADS: ::libc::c_int = 112;
                                        pub const _SC_XOPEN_SHM: ::libc::c_int = 113;
                                        pub const _SC_XOPEN_UNIX: ::libc::c_int = 115;
                                        pub const _SC_XOPEN_VERSION: ::libc::c_int = 116;
                                        pub const _SC_XOPEN_XCU_VERSION: ::libc::c_int = 121;

                                        pub const PTHREAD_CREATE_JOINABLE: ::libc::c_int = 1;
                                        pub const PTHREAD_CREATE_DETACHED: ::libc::c_int = 2;
                                        pub const PTHREAD_STACK_MIN: ::libc::size_t = 8192;

                                        pub const RLIMIT_CPU: ::libc::c_int = 0;
                                        pub const RLIMIT_FSIZE: ::libc::c_int = 1;
                                        pub const RLIMIT_DATA: ::libc::c_int = 2;
                                        pub const RLIMIT_STACK: ::libc::c_int = 3;
                                        pub const RLIMIT_CORE: ::libc::c_int = 4;
                                        pub const RLIMIT_AS: ::libc::c_int = 5;
                                        pub const RLIMIT_MEMLOCK: ::libc::c_int = 6;
                                        pub const RLIMIT_NPROC: ::libc::c_int = 7;
                                        pub const RLIMIT_NOFILE: ::libc::c_int = 8;
                                        pub const RLIM_NLIMITS: ::libc::c_int = 9;
                                        pub const _RLIMIT_POSIX_FLAG: ::libc::c_int = 0x1000;

                                        pub const RLIM_INFINITY: rlim_t = 0x7fff_ffff_ffff_ffff;

                                        pub const RUSAGE_SELF: ::libc::c_int = 0;
                                        pub const RUSAGE_CHILDREN: ::libc::c_int = -1;

                                        pub const MADV_NORMAL: ::libc::c_int = 0;
                                        pub const MADV_RANDOM: ::libc::c_int = 1;
                                        pub const MADV_SEQUENTIAL: ::libc::c_int = 2;
                                        pub const MADV_WILLNEED: ::libc::c_int = 3;
                                        pub const MADV_DONTNEED: ::libc::c_int = 4;
                                        pub const MADV_FREE: ::libc::c_int = 5;
                                        pub const MADV_ZERO_WIRED_PAGES: ::libc::c_int = 6;
                                        pub const MADV_FREE_REUSABLE: ::libc::c_int = 7;
                                        pub const MADV_FREE_REUSE: ::libc::c_int = 8;
                                        pub const MADV_CAN_REUSE: ::libc::c_int = 9;

                                        pub const MINCORE_INCORE: ::libc::c_int =  0x1;
                                        pub const MINCORE_REFERENCED: ::libc::c_int = 0x2;
                                        pub const MINCORE_MODIFIED: ::libc::c_int = 0x4;
                                        pub const MINCORE_REFERENCED_OTHER: ::libc::c_int = 0x8;
                                        pub const MINCORE_MODIFIED_OTHER: ::libc::c_int = 0x10;

                                        pub const AF_UNIX: ::libc::c_int = 1;
                                        pub const AF_INET: ::libc::c_int = 2;
                                        pub const AF_INET6: ::libc::c_int = 30;
                                        pub const SOCK_STREAM: ::libc::c_int = 1;
                                        pub const SOCK_DGRAM: ::libc::c_int = 2;
                                        pub const SOCK_RAW: ::libc::c_int = 3;
                                        pub const IPPROTO_TCP: ::libc::c_int = 6;
                                        pub const IPPROTO_IP: ::libc::c_int = 0;
                                        pub const IPPROTO_IPV6: ::libc::c_int = 41;
                                        pub const IP_MULTICAST_TTL: ::libc::c_int = 10;
                                        pub const IP_MULTICAST_LOOP: ::libc::c_int = 11;
                                        pub const IP_TTL: ::libc::c_int = 4;
                                        pub const IP_HDRINCL: ::libc::c_int = 2;
                                        pub const IP_ADD_MEMBERSHIP: ::libc::c_int = 12;
                                        pub const IP_DROP_MEMBERSHIP: ::libc::c_int = 13;
                                        pub const IPV6_JOIN_GROUP: ::libc::c_int = 12;
                                        pub const IPV6_LEAVE_GROUP: ::libc::c_int = 13;

                                        pub const TCP_NODELAY: ::libc::c_int = 0x01;
                                        pub const TCP_KEEPALIVE: ::libc::c_int = 0x10;
                                        pub const SOL_SOCKET: ::libc::c_int = 0xffff;

                                        pub const SO_DEBUG: ::libc::c_int = 0x01;
                                        pub const SO_ACCEPTCONN: ::libc::c_int = 0x0002;
                                        pub const SO_REUSEADDR: ::libc::c_int = 0x0004;
                                        pub const SO_KEEPALIVE: ::libc::c_int = 0x0008;
                                        pub const SO_DONTROUTE: ::libc::c_int = 0x0010;
                                        pub const SO_BROADCAST: ::libc::c_int = 0x0020;
                                        pub const SO_USELOOPBACK: ::libc::c_int = 0x0040;
                                        pub const SO_LINGER: ::libc::c_int = 0x0080;
                                        pub const SO_OOBINLINE: ::libc::c_int = 0x0100;
                                        pub const SO_REUSEPORT: ::libc::c_int = 0x0200;
                                        pub const SO_SNDBUF: ::libc::c_int = 0x1001;
                                        pub const SO_RCVBUF: ::libc::c_int = 0x1002;
                                        pub const SO_SNDLOWAT: ::libc::c_int = 0x1003;
                                        pub const SO_RCVLOWAT: ::libc::c_int = 0x1004;
                                        pub const SO_SNDTIMEO: ::libc::c_int = 0x1005;
                                        pub const SO_RCVTIMEO: ::libc::c_int = 0x1006;
                                        pub const SO_ERROR: ::libc::c_int = 0x1007;
                                        pub const SO_TYPE: ::libc::c_int = 0x1008;

                                        pub const IFF_LOOPBACK: ::libc::c_int = 0x8;

                                        pub const SHUT_RD: ::libc::c_int = 0;
                                        pub const SHUT_WR: ::libc::c_int = 1;
                                        pub const SHUT_RDWR: ::libc::c_int = 2;

                                        pub const LOCK_SH: ::libc::c_int = 1;
                                        pub const LOCK_EX: ::libc::c_int = 2;
                                        pub const LOCK_NB: ::libc::c_int = 4;
                                        pub const LOCK_UN: ::libc::c_int = 8;

                                        pub const O_DSYNC: ::libc::c_int = 4194304;
                                        pub const O_SYNC: ::libc::c_int = 128;
                                        pub const O_NONBLOCK: ::libc::c_int = 4;
                                        pub const F_GETPATH: ::libc::c_int = 50;
                                        pub const F_FULLFSYNC: ::libc::c_int = 51;

                                        pub const MAP_COPY: ::libc::c_int = 0x0002;
                                        pub const MAP_RENAME: ::libc::c_int = 0x0020;
                                        pub const MAP_NORESERVE: ::libc::c_int = 0x0040;
                                        pub const MAP_NOEXTEND: ::libc::c_int = 0x0100;
                                        pub const MAP_HASSEMAPHORE: ::libc::c_int = 0x0200;
                                        pub const MAP_NOCACHE: ::libc::c_int = 0x0400;
                                        pub const MAP_JIT: ::libc::c_int = 0x0800;

                                        pub const IPPROTO_RAW: ::libc::c_int = 255;

                                        pub const SO_NREAD: ::libc::c_int = 0x1020;
                                        pub const SO_NKE: ::libc::c_int = 0x1021;
                                        pub const SO_NOSIGPIPE: ::libc::c_int = 0x1022;
                                        pub const SO_NOADDRERR: ::libc::c_int = 0x1023;
                                        pub const SO_NWRITE: ::libc::c_int = 0x1024;
                                        pub const SO_DONTTRUNC: ::libc::c_int = 0x2000;
                                        pub const SO_WANTMORE: ::libc::c_int = 0x4000;
                                        pub const SO_WANTOOBFLAG: ::libc::c_int = 0x8000;

                                        pub const PATH_MAX: ::libc::c_int = 1024;

                                        pub const _SC_ARG_MAX: ::libc::c_int = 1;
                                        pub const _SC_CHILD_MAX: ::libc::c_int = 2;
                                        pub const _SC_CLK_TCK: ::libc::c_int = 3;
                                        pub const _SC_NGROUPS_MAX: ::libc::c_int = 4;
                                        pub const _SC_OPEN_MAX: ::libc::c_int = 5;
                                        pub const _SC_JOB_CONTROL: ::libc::c_int = 6;
                                        pub const _SC_SAVED_IDS: ::libc::c_int = 7;
                                        pub const _SC_VERSION: ::libc::c_int = 8;
                                        pub const _SC_BC_BASE_MAX: ::libc::c_int = 9;
                                        pub const _SC_BC_DIM_MAX: ::libc::c_int = 10;
                                        pub const _SC_BC_SCALE_MAX: ::libc::c_int = 11;
                                        pub const _SC_BC_STRING_MAX: ::libc::c_int = 12;
                                        pub const _SC_COLL_WEIGHTS_MAX: ::libc::c_int = 13;
                                        pub const _SC_EXPR_NEST_MAX: ::libc::c_int = 14;
                                        pub const _SC_LINE_MAX: ::libc::c_int = 15;
                                        pub const _SC_RE_DUP_MAX: ::libc::c_int = 16;
                                        pub const _SC_2_VERSION: ::libc::c_int = 17;
                                        pub const _SC_2_C_BIND: ::libc::c_int = 18;
                                        pub const _SC_2_C_DEV: ::libc::c_int = 19;
                                        pub const _SC_2_CHAR_TERM: ::libc::c_int = 20;
                                        pub const _SC_2_FORT_DEV: ::libc::c_int = 21;
                                        pub const _SC_2_FORT_RUN: ::libc::c_int = 22;
                                        pub const _SC_2_LOCALEDEF: ::libc::c_int = 23;
                                        pub const _SC_2_SW_DEV: ::libc::c_int = 24;
                                        pub const _SC_2_UPE: ::libc::c_int = 25;
                                        pub const _SC_STREAM_MAX: ::libc::c_int = 26;
                                        pub const _SC_TZNAME_MAX: ::libc::c_int = 27;
                                        pub const _SC_ASYNCHRONOUS_IO: ::libc::c_int = 28;
                                        pub const _SC_PAGESIZE: ::libc::c_int = 29;
                                        pub const _SC_MEMLOCK: ::libc::c_int = 30;
                                        pub const _SC_MEMLOCK_RANGE: ::libc::c_int = 31;
                                        pub const _SC_MEMORY_PROTECTION: ::libc::c_int = 32;
                                        pub const _SC_MESSAGE_PASSING: ::libc::c_int = 33;
                                        pub const _SC_PRIORITIZED_IO: ::libc::c_int = 34;
                                        pub const _SC_PRIORITY_SCHEDULING: ::libc::c_int = 35;
                                        pub const _SC_REALTIME_SIGNALS: ::libc::c_int = 36;
                                        pub const _SC_SEMAPHORES: ::libc::c_int = 37;
                                        pub const _SC_FSYNC: ::libc::c_int = 38;
                                        pub const _SC_SHARED_MEMORY_OBJECTS: ::libc::c_int = 39;
                                        pub const _SC_SYNCHRONIZED_IO: ::libc::c_int = 40;
                                        pub const _SC_TIMERS: ::libc::c_int = 41;
                                        pub const _SC_AIO_LISTIO_MAX: ::libc::c_int = 42;
                                        pub const _SC_AIO_MAX: ::libc::c_int = 43;
                                        pub const _SC_AIO_PRIO_DELTA_MAX: ::libc::c_int = 44;
                                        pub const _SC_DELAYTIMER_MAX: ::libc::c_int = 45;
                                        pub const _SC_MQ_OPEN_MAX: ::libc::c_int = 46;
                                        pub const _SC_MAPPED_FILES: ::libc::c_int = 47;
                                        pub const _SC_RTSIG_MAX: ::libc::c_int = 48;
                                        pub const _SC_SEM_NSEMS_MAX: ::libc::c_int = 49;
                                        pub const _SC_SEM_VALUE_MAX: ::libc::c_int = 50;
                                        pub const _SC_SIGQUEUE_MAX: ::libc::c_int = 51;
                                        pub const _SC_TIMER_MAX: ::libc::c_int = 52;
                                        pub const _SC_NPROCESSORS_CONF: ::libc::c_int = 57;
                                        pub const _SC_NPROCESSORS_ONLN: ::libc::c_int = 58;
                                        pub const _SC_2_PBS: ::libc::c_int = 59;
                                        pub const _SC_2_PBS_ACCOUNTING: ::libc::c_int = 60;
                                        pub const _SC_2_PBS_CHECKPOINT: ::libc::c_int = 61;
                                        pub const _SC_2_PBS_LOCATE: ::libc::c_int = 62;
                                        pub const _SC_2_PBS_MESSAGE: ::libc::c_int = 63;
                                        pub const _SC_2_PBS_TRACK: ::libc::c_int = 64;
                                        pub const _SC_ADVISORY_INFO: ::libc::c_int = 65;
                                        pub const _SC_BARRIERS: ::libc::c_int = 66;
                                        pub const _SC_CLOCK_SELECTION: ::libc::c_int = 67;
                                        pub const _SC_CPUTIME: ::libc::c_int = 68;
                                        pub const _SC_FILE_LOCKING: ::libc::c_int = 69;
                                        pub const _SC_HOST_NAME_MAX: ::libc::c_int = 72;
                                        pub const _SC_MONOTONIC_CLOCK: ::libc::c_int = 74;
                                        pub const _SC_READER_WRITER_LOCKS: ::libc::c_int = 76;
                                        pub const _SC_REGEXP: ::libc::c_int = 77;
                                        pub const _SC_SHELL: ::libc::c_int = 78;
                                        pub const _SC_SPAWN: ::libc::c_int = 79;
                                        pub const _SC_SPIN_LOCKS: ::libc::c_int = 80;
                                        pub const _SC_SPORADIC_SERVER: ::libc::c_int = 81;
                                        pub const _SC_THREAD_CPUTIME: ::libc::c_int = 84;
                                        pub const _SC_THREAD_SPORADIC_SERVER: ::libc::c_int = 92;
                                        pub const _SC_TIMEOUTS: ::libc::c_int = 95;
                                        pub const _SC_TRACE: ::libc::c_int = 97;
                                        pub const _SC_TRACE_EVENT_FILTER: ::libc::c_int = 98;
                                        pub const _SC_TRACE_INHERIT: ::libc::c_int = 99;
                                        pub const _SC_TRACE_LOG: ::libc::c_int = 100;
                                        pub const _SC_TYPED_MEMORY_OBJECTS: ::libc::c_int = 102;
                                        pub const _SC_V6_ILP32_OFF32: ::libc::c_int = 103;
                                        pub const _SC_V6_ILP32_OFFBIG: ::libc::c_int = 104;
                                        pub const _SC_V6_LP64_OFF64: ::libc::c_int = 105;
                                        pub const _SC_V6_LPBIG_OFFBIG: ::libc::c_int = 106;
                                        pub const _SC_IPV6: ::libc::c_int = 118;
                                        pub const _SC_RAW_SOCKETS: ::libc::c_int = 119;
                                        pub const _SC_SYMLOOP_MAX: ::libc::c_int = 120;
                                        pub const _SC_PAGE_SIZE: ::libc::c_int = _SC_PAGESIZE;
                                        pub const _SC_XOPEN_STREAMS: ::libc::c_int = 114;
                                        pub const _SC_XBS5_ILP32_OFF32: ::libc::c_int = 122;
                                        pub const _SC_XBS5_ILP32_OFFBIG: ::libc::c_int = 123;
                                        pub const _SC_XBS5_LP64_OFF64: ::libc::c_int = 124;
                                        pub const _SC_XBS5_LPBIG_OFFBIG: ::libc::c_int = 125;
                                        pub const _SC_SS_REPL_MAX: ::libc::c_int = 126;
                                        pub const _SC_TRACE_EVENT_NAME_MAX: ::libc::c_int = 127;
                                        pub const _SC_TRACE_NAME_MAX: ::libc::c_int = 128;
                                        pub const _SC_TRACE_SYS_MAX: ::libc::c_int = 129;
                                        pub const _SC_TRACE_USER_EVENT_MAX: ::libc::c_int = 130;
                                        pub const _SC_PASS_MAX: ::libc::c_int = 131;

                                        pub const PTHREAD_MUTEX_RECURSIVE: ::libc::c_int = 2;
                                        pub const _PTHREAD_MUTEX_SIG_init: ::libc::c_long = 0x32AAABA7;
                                        pub const _PTHREAD_COND_SIG_init: ::libc::c_long = 0x3CB0B1BB;
                                        pub const _PTHREAD_RWLOCK_SIG_init: ::libc::c_long = 0x2DA8B3B4;
                                        pub const PTHREAD_MUTEX_INITIALIZER: pthread_mutex_t = pthread_mutex_t {
                                            __sig: _PTHREAD_MUTEX_SIG_init,
                                            __opaque: [0; __PTHREAD_MUTEX_SIZE__],
                                        };
                                        pub const PTHREAD_COND_INITIALIZER: pthread_cond_t = pthread_cond_t {
                                            __sig: _PTHREAD_COND_SIG_init,
                                            __opaque: [0; __PTHREAD_COND_SIZE__],
                                        };
                                        pub const PTHREAD_RWLOCK_INITIALIZER: pthread_rwlock_t = pthread_rwlock_t {
                                            __sig: _PTHREAD_RWLOCK_SIG_init,
                                            __opaque: [0; __PTHREAD_RWLOCK_SIZE__],
                                        };

                                        pub const SIGSTKSZ: ::libc::size_t = 131072;

                                        extern
                                        {
                                            #[cfg_attr(all(target_os = "macos", target_arch = "x86"),
                                                    link_name = "mprotect$UNIX2003")]
                                            pub fn mprotect(addr: *mut ::libc::c_void, len: ::libc::size_t, prot: ::libc::c_int)
                                                            -> ::libc::c_int;
                                            pub fn shm_open(name: *const ::libc::c_char, oflag: ::libc::c_int, ...) -> ::libc::c_int;
                                            pub fn sysctl(name: *mut ::libc::c_int,
                                                        namelen: ::libc::c_uint,
                                                        oldp: *mut ::libc::c_void,
                                                        oldlenp: *mut ::libc::size_t,
                                                        newp: *mut ::libc::c_void,
                                                        newlen: ::libc::size_t)
                                                        -> ::libc::c_int;
                                            pub fn sysctlbyname(name: *const ::libc::c_char,
                                                                oldp: *mut ::libc::c_void,
                                                                oldlenp: *mut ::libc::size_t,
                                                                newp: *mut ::libc::c_void,
                                                                newlen: ::libc::size_t)
                                                                -> ::libc::c_int;
                                            pub fn mach_absolute_time() -> u64;
                                            pub fn mach_timebase_info(info: *mut ::mach_timebase_info) -> ::libc::c_int;
                                            pub fn pthread_setname_np(name: *const ::libc::c_char) -> ::libc::c_int;
                                            pub fn pthread_get_stackaddr_np(thread: ::libc::pthread_t) -> *mut ::libc::c_void;
                                            pub fn pthread_get_stacksize_np(thread: ::libc::pthread_t) -> ::size_t;
                                            pub fn __error() -> *mut ::libc::c_int;
                                            pub fn backtrace(buf: *mut *mut ::libc::c_void,
                                                            sz: ::libc::c_int) -> ::libc::c_int;
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(any(target_arch = "arm", target_arch = "x86"))]
                                            {
                                                mod b32
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    //! 32-bit specific Apple (ios/darwin) definitions
                                                    pub type c_long = i32;
                                                    pub type c_ulong = u32;

                                                    pub const __PTHREAD_MUTEX_SIZE__: usize = 40;
                                                    pub const __PTHREAD_COND_SIZE__: usize = 24;
                                                    pub const __PTHREAD_RWLOCK_SIZE__: usize = 124;

                                                    s!
                                                    {
                                                        pub struct pthread_attr_t {
                                                            __sig: ::libc::c_long,
                                                            __opaque: [::libc::c_char; 36]
                                                        }
                                                    }
                                                }
                                                
                                                pub use self::b32::*;
                                            }
                                            
                                            else if #[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
                                            {
                                                mod b64
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    //! 64-bit specific Apple (ios/darwin) definitions
                                                    pub type c_long = i64;
                                                    pub type c_ulong = u64;

                                                    pub const __PTHREAD_MUTEX_SIZE__: usize = 56;
                                                    pub const __PTHREAD_COND_SIZE__: usize = 40;
                                                    pub const __PTHREAD_RWLOCK_SIZE__: usize = 192;

                                                    s! {
                                                        pub struct pthread_attr_t {
                                                            __sig: ::libc::c_long,
                                                            __opaque: [::libc::c_char; 56]
                                                        }
                                                    }
                                                }
                                                
                                                pub use self::b64::*;
                                            }
                                            
                                            else { }
                                        }
                                    }
                                    
                                    pub use self::apple::*;
                                }
                                
                                else if #[cfg(any(target_os = "openbsd", target_os = "netbsd", target_os = "dragonfly"))]
                                {
                                    mod openbsdlike
                                    {
                                        use ::
                                        {
                                            *,
                                        };
                                        
                                        pub type c_long = i64;
                                        pub type c_ulong = u64;
                                        pub type clock_t = i64;
                                        pub type time_t = i64;
                                        pub type suseconds_t = i64;
                                        pub type dev_t = i32;
                                        pub type mode_t = u32;
                                        pub type nlink_t = uint32_t;
                                        pub type blksize_t = uint32_t;
                                        pub type ino_t = uint64_t;
                                        pub type fflags_t = u32;
                                        pub type pthread_attr_t = *mut c_void;
                                        pub type sigset_t = ::libc::c_uint;
                                        pub type pthread_key_t = ::libc::c_int;
                                        pub type pthread_mutex_t = *mut ::libc::c_void;
                                        pub type pthread_mutexattr_t = *mut ::libc::c_void;
                                        pub type pthread_cond_t = *mut ::libc::c_void;
                                        pub type pthread_rwlock_t = *mut ::libc::c_void;
                                        pub type rlim_t = u64;


                                        pub enum timezone {}

                                        s! {
                                            pub struct dirent {
                                                pub d_fileno: ::libc::ino_t,
                                                pub d_off: ::libc::off_t,
                                                pub d_reclen: u16,
                                                pub d_type: u8,
                                                pub d_namelen: u8,
                                                __d_padding: [u8; 4],
                                                pub d_name: [::libc::c_char; 256],
                                            }

                                            pub struct siginfo_t {
                                                pub si_signo: ::libc::c_int,
                                                pub si_code: ::libc::c_int,
                                                pub si_errno: ::libc::c_int,
                                                pub si_addr: *mut ::libc::c_void
                                            }

                                            pub struct sigaction {
                                                pub sa_sigaction: sighandler_t,
                                                pub sa_mask: sigset_t,
                                                pub sa_flags: ::libc::c_int,
                                            }

                                            pub struct stack_t {
                                                pub ss_sp: *mut ::libc::c_void,
                                                pub ss_size: ::libc::size_t,
                                                pub ss_flags: ::libc::c_int,
                                            }

                                            pub struct sockaddr_storage {
                                                pub ss_len: u8,
                                                pub ss_family: sa_family_t,
                                                __ss_pad1: [u8; 6],
                                                __ss_pad2: i64,
                                                __ss_pad3: [u8; 240],
                                            }

                                            pub struct addrinfo {
                                                pub ai_flags: ::libc::c_int,
                                                pub ai_family: ::libc::c_int,
                                                pub ai_socktype: ::libc::c_int,
                                                pub ai_protocol: ::libc::c_int,
                                                pub ai_addrlen: socklen_t,
                                                pub ai_addr: *mut sockaddr,
                                                pub ai_canonname: *mut c_char,
                                                pub ai_next: *mut addrinfo,
                                            }

                                            pub struct stat {
                                                pub st_mode: mode_t,
                                                pub st_dev: dev_t,
                                                pub st_ino: ino_t,
                                                pub st_nlink: nlink_t,
                                                pub st_uid: uid_t,
                                                pub st_gid: gid_t,
                                                pub st_rdev: dev_t,
                                                pub st_atime: time_t,
                                                pub st_atime_nsec: ::libc::c_long,
                                                pub st_mtime: time_t,
                                                pub st_mtime_nsec: ::libc::c_long,
                                                pub st_ctime: time_t,
                                                pub st_ctime_nsec: ::libc::c_long,
                                                pub st_size: off_t,
                                                pub st_blocks: blkcnt_t,
                                                pub st_blksize: blksize_t,
                                                pub st_flags: fflags_t,
                                                pub st_gen: uint32_t,
                                                pub st_birthtime: time_t,
                                                pub st_birthtime_nsec: ::libc::c_long,
                                            }

                                            pub struct stack_t {
                                                pub ss_sp: *mut ::libc::c_void,
                                                pub ss_size: ::libc::size_t,
                                                pub ss_flags: ::libc::c_int,
                                            }
                                        }

                                        pub const EXIT_FAILURE : ::libc::c_int = 1;
                                        pub const EXIT_SUCCESS : ::libc::c_int = 0;
                                        pub const RAND_MAX : ::libc::c_int = 2147483647;
                                        pub const EOF : ::libc::c_int = -1;
                                        pub const SEEK_SET : ::libc::c_int = 0;
                                        pub const SEEK_CUR : ::libc::c_int = 1;
                                        pub const SEEK_END : ::libc::c_int = 2;
                                        pub const _IOFBF : ::libc::c_int = 0;
                                        pub const _IONBF : ::libc::c_int = 2;
                                        pub const _IOLBF : ::libc::c_int = 1;
                                        pub const BUFSIZ : ::libc::c_uint = 1024;
                                        pub const FOPEN_MAX : ::libc::c_uint = 20;
                                        pub const FILENAME_MAX : ::libc::c_uint = 1024;
                                        pub const L_tmpnam : ::libc::c_uint = 1024;
                                        pub const TMP_MAX : ::libc::c_uint = 308915776;
                                        pub const O_RDONLY : ::libc::c_int = 0;
                                        pub const O_WRONLY : ::libc::c_int = 1;
                                        pub const O_RDWR : ::libc::c_int = 2;
                                        pub const O_APPEND : ::libc::c_int = 8;
                                        pub const O_CREAT : ::libc::c_int = 512;
                                        pub const O_EXCL : ::libc::c_int = 2048;
                                        pub const O_NOCTTY : ::libc::c_int = 32768;
                                        pub const O_TRUNC : ::libc::c_int = 1024;
                                        pub const O_CLOEXEC: ::libc::c_int = 0x10000;
                                        pub const S_IFIFO : mode_t = 4096;
                                        pub const S_IFCHR : mode_t = 8192;
                                        pub const S_IFBLK : mode_t = 24576;
                                        pub const S_IFDIR : mode_t = 16384;
                                        pub const S_IFREG : mode_t = 32768;
                                        pub const S_IFLNK : mode_t = 40960;
                                        pub const S_IFSOCK : mode_t = 49152;
                                        pub const S_IFMT : mode_t = 61440;
                                        pub const S_IEXEC : mode_t = 64;
                                        pub const S_IWRITE : mode_t = 128;
                                        pub const S_IREAD : mode_t = 256;
                                        pub const S_IRWXU : mode_t = 448;
                                        pub const S_IXUSR : mode_t = 64;
                                        pub const S_IWUSR : mode_t = 128;
                                        pub const S_IRUSR : mode_t = 256;
                                        pub const S_IRWXG : mode_t = 56;
                                        pub const S_IXGRP : mode_t = 8;
                                        pub const S_IWGRP : mode_t = 16;
                                        pub const S_IRGRP : mode_t = 32;
                                        pub const S_IRWXO : mode_t = 7;
                                        pub const S_IXOTH : mode_t = 1;
                                        pub const S_IWOTH : mode_t = 2;
                                        pub const S_IROTH : mode_t = 4;
                                        pub const F_OK : ::libc::c_int = 0;
                                        pub const R_OK : ::libc::c_int = 4;
                                        pub const W_OK : ::libc::c_int = 2;
                                        pub const X_OK : ::libc::c_int = 1;
                                        pub const STDIN_FILENO : ::libc::c_int = 0;
                                        pub const STDOUT_FILENO : ::libc::c_int = 1;
                                        pub const STDERR_FILENO : ::libc::c_int = 2;
                                        pub const F_LOCK : ::libc::c_int = 1;
                                        pub const F_TEST : ::libc::c_int = 3;
                                        pub const F_TLOCK : ::libc::c_int = 2;
                                        pub const F_ULOCK : ::libc::c_int = 0;
                                        pub const SIGHUP : ::libc::c_int = 1;
                                        pub const SIGINT : ::libc::c_int = 2;
                                        pub const SIGQUIT : ::libc::c_int = 3;
                                        pub const SIGILL : ::libc::c_int = 4;
                                        pub const SIGABRT : ::libc::c_int = 6;
                                        pub const SIGFPE : ::libc::c_int = 8;
                                        pub const SIGKILL : ::libc::c_int = 9;
                                        pub const SIGSEGV : ::libc::c_int = 11;
                                        pub const SIGPIPE : ::libc::c_int = 13;
                                        pub const SIGALRM : ::libc::c_int = 14;
                                        pub const SIGTERM : ::libc::c_int = 15;

                                        pub const PROT_NONE : ::libc::c_int = 0;
                                        pub const PROT_READ : ::libc::c_int = 1;
                                        pub const PROT_WRITE : ::libc::c_int = 2;
                                        pub const PROT_EXEC : ::libc::c_int = 4;

                                        pub const MAP_FILE : ::libc::c_int = 0x0000;
                                        pub const MAP_SHARED : ::libc::c_int = 0x0001;
                                        pub const MAP_PRIVATE : ::libc::c_int = 0x0002;
                                        pub const MAP_FIXED : ::libc::c_int = 0x0010;
                                        pub const MAP_ANON : ::libc::c_int = 0x1000;

                                        pub const MAP_FAILED : *mut c_void = !0 as *mut c_void;

                                        pub const MCL_CURRENT : ::libc::c_int = 0x0001;
                                        pub const MCL_FUTURE : ::libc::c_int = 0x0002;

                                        pub const MS_ASYNC : ::libc::c_int = 0x0001;
                                        pub const MS_SYNC : ::libc::c_int = 0x0002;
                                        pub const MS_INVALIDATE : ::libc::c_int = 0x0004;

                                        pub const EPERM : ::libc::c_int = 1;
                                        pub const ENOENT : ::libc::c_int = 2;
                                        pub const ESRCH : ::libc::c_int = 3;
                                        pub const EINTR : ::libc::c_int = 4;
                                        pub const EIO : ::libc::c_int = 5;
                                        pub const ENXIO : ::libc::c_int = 6;
                                        pub const E2BIG : ::libc::c_int = 7;
                                        pub const ENOEXEC : ::libc::c_int = 8;
                                        pub const EBADF : ::libc::c_int = 9;
                                        pub const ECHILD : ::libc::c_int = 10;
                                        pub const EDEADLK : ::libc::c_int = 11;
                                        pub const ENOMEM : ::libc::c_int = 12;
                                        pub const EACCES : ::libc::c_int = 13;
                                        pub const EFAULT : ::libc::c_int = 14;
                                        pub const ENOTBLK : ::libc::c_int = 15;
                                        pub const EBUSY : ::libc::c_int = 16;
                                        pub const EEXIST : ::libc::c_int = 17;
                                        pub const EXDEV : ::libc::c_int = 18;
                                        pub const ENODEV : ::libc::c_int = 19;
                                        pub const ENOTDIR : ::libc::c_int = 20;
                                        pub const EISDIR : ::libc::c_int = 21;
                                        pub const EINVAL : ::libc::c_int = 22;
                                        pub const ENFILE : ::libc::c_int = 23;
                                        pub const EMFILE : ::libc::c_int = 24;
                                        pub const ENOTTY : ::libc::c_int = 25;
                                        pub const ETXTBSY : ::libc::c_int = 26;
                                        pub const EFBIG : ::libc::c_int = 27;
                                        pub const ENOSPC : ::libc::c_int = 28;
                                        pub const ESPIPE : ::libc::c_int = 29;
                                        pub const EROFS : ::libc::c_int = 30;
                                        pub const EMLINK : ::libc::c_int = 31;
                                        pub const EPIPE : ::libc::c_int = 32;
                                        pub const EDOM : ::libc::c_int = 33;
                                        pub const ERANGE : ::libc::c_int = 34;
                                        pub const EAGAIN : ::libc::c_int = 35;
                                        pub const EWOULDBLOCK : ::libc::c_int = 35;
                                        pub const EINPROGRESS : ::libc::c_int = 36;
                                        pub const EALREADY : ::libc::c_int = 37;
                                        pub const ENOTSOCK : ::libc::c_int = 38;
                                        pub const EDESTADDRREQ : ::libc::c_int = 39;
                                        pub const EMSGSIZE : ::libc::c_int = 40;
                                        pub const EPROTOTYPE : ::libc::c_int = 41;
                                        pub const ENOPROTOOPT : ::libc::c_int = 42;
                                        pub const EPROTONOSUPPORT : ::libc::c_int = 43;
                                        pub const ESOCKTNOSUPPORT : ::libc::c_int = 44;
                                        pub const EOPNOTSUPP : ::libc::c_int = 45;
                                        pub const EPFNOSUPPORT : ::libc::c_int = 46;
                                        pub const EAFNOSUPPORT : ::libc::c_int = 47;
                                        pub const EADDRINUSE : ::libc::c_int = 48;
                                        pub const EADDRNOTAVAIL : ::libc::c_int = 49;
                                        pub const ENETDOWN : ::libc::c_int = 50;
                                        pub const ENETUNREACH : ::libc::c_int = 51;
                                        pub const ENETRESET : ::libc::c_int = 52;
                                        pub const ECONNABORTED : ::libc::c_int = 53;
                                        pub const ECONNRESET : ::libc::c_int = 54;
                                        pub const ENOBUFS : ::libc::c_int = 55;
                                        pub const EISCONN : ::libc::c_int = 56;
                                        pub const ENOTCONN : ::libc::c_int = 57;
                                        pub const ESHUTDOWN : ::libc::c_int = 58;
                                        pub const ETOOMANYREFS : ::libc::c_int = 59;
                                        pub const ETIMEDOUT : ::libc::c_int = 60;
                                        pub const ECONNREFUSED : ::libc::c_int = 61;
                                        pub const ELOOP : ::libc::c_int = 62;
                                        pub const ENAMETOOLONG : ::libc::c_int = 63;
                                        pub const EHOSTDOWN : ::libc::c_int = 64;
                                        pub const EHOSTUNREACH : ::libc::c_int = 65;
                                        pub const ENOTEMPTY : ::libc::c_int = 66;
                                        pub const EPROCLIM : ::libc::c_int = 67;
                                        pub const EUSERS : ::libc::c_int = 68;
                                        pub const EDQUOT : ::libc::c_int = 69;
                                        pub const ESTALE : ::libc::c_int = 70;
                                        pub const EREMOTE : ::libc::c_int = 71;
                                        pub const EBADRPC : ::libc::c_int = 72;
                                        pub const ERPCMISMATCH : ::libc::c_int = 73;
                                        pub const EPROGUNAVAIL : ::libc::c_int = 74;
                                        pub const EPROGMISMATCH : ::libc::c_int = 75;
                                        pub const EPROCUNAVAIL : ::libc::c_int = 76;
                                        pub const ENOLCK : ::libc::c_int = 77;
                                        pub const ENOSYS : ::libc::c_int = 78;
                                        pub const EFTYPE : ::libc::c_int = 79;
                                        pub const EAUTH : ::libc::c_int = 80;
                                        pub const ENEEDAUTH : ::libc::c_int = 81;
                                        pub const EIPSEC : ::libc::c_int = 82;
                                        pub const ENOATTR : ::libc::c_int = 83;
                                        pub const EILSEQ : ::libc::c_int = 84;
                                        pub const ENOMEDIUM : ::libc::c_int = 85;
                                        pub const EMEDIUMTYPE : ::libc::c_int = 86;
                                        pub const EOVERFLOW : ::libc::c_int = 87;
                                        pub const ECANCELED : ::libc::c_int = 88;
                                        pub const EIDRM : ::libc::c_int = 89;
                                        pub const ENOMSG : ::libc::c_int = 90;
                                        pub const ENOTSUP : ::libc::c_int = 91;
                                        pub const ELAST : ::libc::c_int = 91; // must be equal to largest errno

                                        pub const F_DUPFD : ::libc::c_int = 0;
                                        pub const F_GETFD : ::libc::c_int = 1;
                                        pub const F_SETFD : ::libc::c_int = 2;
                                        pub const F_GETFL : ::libc::c_int = 3;
                                        pub const F_SETFL : ::libc::c_int = 4;
                                        pub const F_GETOWN : ::libc::c_int = 5;
                                        pub const F_SETOWN : ::libc::c_int = 6;
                                        pub const F_GETLK : ::libc::c_int = 7;
                                        pub const F_SETLK : ::libc::c_int = 8;
                                        pub const F_SETLKW : ::libc::c_int = 9;
                                        pub const F_DUPFD_CLOEXEC : ::libc::c_int = 10;

                                        pub const SIGTRAP : ::libc::c_int = 5;

                                        pub const GLOB_APPEND   : ::libc::c_int = 0x0001;
                                        pub const GLOB_DOOFFS   : ::libc::c_int = 0x0002;
                                        pub const GLOB_ERR      : ::libc::c_int = 0x0004;
                                        pub const GLOB_MARK     : ::libc::c_int = 0x0008;
                                        pub const GLOB_NOCHECK  : ::libc::c_int = 0x0010;
                                        pub const GLOB_NOSORT   : ::libc::c_int = 0x0020;
                                        pub const GLOB_NOESCAPE : ::libc::c_int = 0x1000;

                                        pub const GLOB_NOSPACE  : ::libc::c_int = -1;
                                        pub const GLOB_ABORTED  : ::libc::c_int = -2;
                                        pub const GLOB_NOMATCH  : ::libc::c_int = -3;
                                        pub const GLOB_NOSYS : ::libc::c_int = -4;

                                        pub const POSIX_MADV_NORMAL : ::libc::c_int = 0;
                                        pub const POSIX_MADV_RANDOM : ::libc::c_int = 1;
                                        pub const POSIX_MADV_SEQUENTIAL : ::libc::c_int = 2;
                                        pub const POSIX_MADV_WILLNEED : ::libc::c_int = 3;
                                        pub const POSIX_MADV_DONTNEED : ::libc::c_int = 4;

                                        pub const _SC_IOV_MAX : ::libc::c_int = 51;
                                        pub const _SC_GETGR_R_SIZE_MAX : ::libc::c_int = 100;
                                        pub const _SC_GETPW_R_SIZE_MAX : ::libc::c_int = 101;
                                        pub const _SC_LOGIN_NAME_MAX : ::libc::c_int = 102;
                                        pub const _SC_MQ_PRIO_MAX : ::libc::c_int = 59;
                                        pub const _SC_THREAD_ATTR_STACKADDR : ::libc::c_int = 77;
                                        pub const _SC_THREAD_ATTR_STACKSIZE : ::libc::c_int = 78;
                                        pub const _SC_THREAD_DESTRUCTOR_ITERATIONS : ::libc::c_int = 80;
                                        pub const _SC_THREAD_KEYS_MAX : ::libc::c_int = 81;
                                        pub const _SC_THREAD_PRIO_INHERIT : ::libc::c_int = 82;
                                        pub const _SC_THREAD_PRIO_PROTECT : ::libc::c_int = 83;
                                        pub const _SC_THREAD_PRIORITY_SCHEDULING : ::libc::c_int = 84;
                                        pub const _SC_THREAD_PROCESS_SHARED : ::libc::c_int = 85;
                                        pub const _SC_THREAD_SAFE_FUNCTIONS : ::libc::c_int = 103;
                                        pub const _SC_THREAD_STACK_MIN : ::libc::c_int = 89;
                                        pub const _SC_THREAD_THREADS_MAX : ::libc::c_int = 90;
                                        pub const _SC_THREADS : ::libc::c_int = 91;
                                        pub const _SC_TTY_NAME_MAX : ::libc::c_int = 107;
                                        pub const _SC_ATEXIT_MAX : ::libc::c_int = 46;
                                        pub const _SC_XOPEN_CRYPT : ::libc::c_int = 117;
                                        pub const _SC_XOPEN_ENH_I18N : ::libc::c_int = 118;
                                        pub const _SC_XOPEN_LEGACY : ::libc::c_int = 119;
                                        pub const _SC_XOPEN_REALTIME : ::libc::c_int = 120;
                                        pub const _SC_XOPEN_REALTIME_THREADS : ::libc::c_int = 121;
                                        pub const _SC_XOPEN_SHM : ::libc::c_int = 30;
                                        pub const _SC_XOPEN_UNIX : ::libc::c_int = 123;
                                        pub const _SC_XOPEN_VERSION : ::libc::c_int = 125;

                                        pub const PTHREAD_CREATE_JOINABLE : ::libc::c_int = 0;
                                        pub const PTHREAD_CREATE_DETACHED : ::libc::c_int = 1;
                                        pub const PTHREAD_STACK_MIN : ::libc::size_t = 2048;

                                        pub const CLOCK_REALTIME : ::libc::c_int = 0;
                                        pub const CLOCK_MONOTONIC : ::libc::c_int = 3;

                                        pub const RLIMIT_CPU: ::libc::c_int = 0;
                                        pub const RLIMIT_FSIZE: ::libc::c_int = 1;
                                        pub const RLIMIT_DATA: ::libc::c_int = 2;
                                        pub const RLIMIT_STACK: ::libc::c_int = 3;
                                        pub const RLIMIT_CORE: ::libc::c_int = 4;
                                        pub const RLIMIT_RSS: ::libc::c_int = 5;
                                        pub const RLIMIT_MEMLOCK: ::libc::c_int = 6;
                                        pub const RLIMIT_NPROC: ::libc::c_int = 7;
                                        pub const RLIMIT_NOFILE: ::libc::c_int = 8;
                                        pub const RLIM_NLIMITS: ::libc::c_int = 9;

                                        pub const RLIM_INFINITY: rlim_t = 0x7fff_ffff_ffff_ffff;
                                        pub const RLIM_SAVED_MAX: rlim_t = RLIM_INFINITY;
                                        pub const RLIM_SAVED_CUR: rlim_t = RLIM_INFINITY;

                                        pub const RUSAGE_SELF: ::libc::c_int = 0;
                                        pub const RUSAGE_CHILDREN: ::libc::c_int = -1;
                                        pub const RUSAGE_THREAD: ::libc::c_int = 1;

                                        pub const MADV_NORMAL : ::libc::c_int = 0;
                                        pub const MADV_RANDOM : ::libc::c_int = 1;
                                        pub const MADV_SEQUENTIAL : ::libc::c_int = 2;
                                        pub const MADV_WILLNEED : ::libc::c_int = 3;
                                        pub const MADV_DONTNEED : ::libc::c_int = 4;
                                        pub const MADV_FREE : ::libc::c_int = 6;

                                        pub const AF_UNIX: ::libc::c_int = 1;
                                        pub const AF_INET: ::libc::c_int = 2;
                                        pub const AF_INET6: ::libc::c_int = 24;
                                        pub const SOCK_STREAM: ::libc::c_int = 1;
                                        pub const SOCK_DGRAM: ::libc::c_int = 2;
                                        pub const SOCK_RAW: ::libc::c_int = 3;
                                        pub const IPPROTO_TCP: ::libc::c_int = 6;
                                        pub const IPPROTO_IP: ::libc::c_int = 0;
                                        pub const IPPROTO_IPV6: ::libc::c_int = 41;
                                        pub const IP_MULTICAST_TTL: ::libc::c_int = 10;
                                        pub const IP_MULTICAST_LOOP: ::libc::c_int = 11;
                                        pub const IP_TTL: ::libc::c_int = 4;
                                        pub const IP_HDRINCL: ::libc::c_int = 2;
                                        pub const IP_ADD_MEMBERSHIP: ::libc::c_int = 12;
                                        pub const IP_DROP_MEMBERSHIP: ::libc::c_int = 13;
                                        pub const IPV6_ADD_MEMBERSHIP: ::libc::c_int = 12; // don't exist
                                        pub const IPV6_DROP_MEMBERSHIP: ::libc::c_int = 13; // don't exist

                                        pub const TCP_NODELAY: ::libc::c_int = 0x01;
                                        pub const SOL_SOCKET: ::libc::c_int = 0xffff;
                                        pub const SO_DEBUG: ::libc::c_int = 0x01;
                                        pub const SO_ACCEPTCONN: ::libc::c_int = 0x0002;
                                        pub const SO_REUSEADDR: ::libc::c_int = 0x0004;
                                        pub const SO_KEEPALIVE: ::libc::c_int = 0x0008;
                                        pub const SO_DONTROUTE: ::libc::c_int = 0x0010;
                                        pub const SO_BROADCAST: ::libc::c_int = 0x0020;
                                        pub const SO_USELOOPBACK: ::libc::c_int = 0x0040;
                                        pub const SO_LINGER: ::libc::c_int = 0x0080;
                                        pub const SO_OOBINLINE: ::libc::c_int = 0x0100;
                                        pub const SO_REUSEPORT: ::libc::c_int = 0x0200;
                                        pub const SO_SNDBUF: ::libc::c_int = 0x1001;
                                        pub const SO_RCVBUF: ::libc::c_int = 0x1002;
                                        pub const SO_SNDLOWAT: ::libc::c_int = 0x1003;
                                        pub const SO_RCVLOWAT: ::libc::c_int = 0x1004;
                                        pub const SO_SNDTIMEO: ::libc::c_int = 0x1005;
                                        pub const SO_RCVTIMEO: ::libc::c_int = 0x1006;
                                        pub const SO_ERROR: ::libc::c_int = 0x1007;
                                        pub const SO_TYPE: ::libc::c_int = 0x1008;

                                        pub const IFF_LOOPBACK: ::libc::c_int = 0x8;

                                        pub const SHUT_RD: ::libc::c_int = 0;
                                        pub const SHUT_WR: ::libc::c_int = 1;
                                        pub const SHUT_RDWR: ::libc::c_int = 2;

                                        pub const LOCK_SH: ::libc::c_int = 1;
                                        pub const LOCK_EX: ::libc::c_int = 2;
                                        pub const LOCK_NB: ::libc::c_int = 4;
                                        pub const LOCK_UN: ::libc::c_int = 8;

                                        pub const O_DSYNC : ::libc::c_int = 128; // same as SYNC
                                        pub const O_SYNC : ::libc::c_int = 128;
                                        pub const O_NONBLOCK : ::libc::c_int = 4;
                                        pub const CTL_KERN : ::libc::c_int = 1;
                                        pub const KERN_PROC : ::libc::c_int = 66;

                                        pub const MAP_COPY : ::libc::c_int = 0x0002;
                                        pub const MAP_RENAME : ::libc::c_int = 0x0000;
                                        pub const MAP_NORESERVE : ::libc::c_int = 0x0000;
                                        pub const MAP_NOEXTEND : ::libc::c_int = 0x0000;
                                        pub const MAP_HASSEMAPHORE : ::libc::c_int = 0x0000;

                                        pub const IPPROTO_RAW : ::libc::c_int = 255;

                                        pub const PATH_MAX: ::libc::c_int = 1024;

                                        pub const _SC_ARG_MAX : ::libc::c_int = 1;
                                        pub const _SC_CHILD_MAX : ::libc::c_int = 2;
                                        pub const _SC_CLK_TCK : ::libc::c_int = 3;
                                        pub const _SC_NGROUPS_MAX : ::libc::c_int = 4;
                                        pub const _SC_OPEN_MAX : ::libc::c_int = 5;
                                        pub const _SC_JOB_CONTROL : ::libc::c_int = 6;
                                        pub const _SC_SAVED_IDS : ::libc::c_int = 7;
                                        pub const _SC_VERSION : ::libc::c_int = 8;
                                        pub const _SC_BC_BASE_MAX : ::libc::c_int = 9;
                                        pub const _SC_BC_DIM_MAX : ::libc::c_int = 10;
                                        pub const _SC_BC_SCALE_MAX : ::libc::c_int = 11;
                                        pub const _SC_BC_STRING_MAX : ::libc::c_int = 12;
                                        pub const _SC_COLL_WEIGHTS_MAX : ::libc::c_int = 13;
                                        pub const _SC_EXPR_NEST_MAX : ::libc::c_int = 14;
                                        pub const _SC_LINE_MAX : ::libc::c_int = 15;
                                        pub const _SC_RE_DUP_MAX : ::libc::c_int = 16;
                                        pub const _SC_2_VERSION : ::libc::c_int = 17;
                                        pub const _SC_2_C_BIND : ::libc::c_int = 18;
                                        pub const _SC_2_C_DEV : ::libc::c_int = 19;
                                        pub const _SC_2_CHAR_TERM : ::libc::c_int = 20;
                                        pub const _SC_2_FORT_DEV : ::libc::c_int = 21;
                                        pub const _SC_2_FORT_RUN : ::libc::c_int = 22;
                                        pub const _SC_2_LOCALEDEF : ::libc::c_int = 23;
                                        pub const _SC_2_SW_DEV : ::libc::c_int = 24;
                                        pub const _SC_2_UPE : ::libc::c_int = 25;
                                        pub const _SC_STREAM_MAX : ::libc::c_int = 26;
                                        pub const _SC_TZNAME_MAX : ::libc::c_int = 27;
                                        pub const _SC_PAGESIZE : ::libc::c_int = 28;
                                        pub const _SC_FSYNC : ::libc::c_int = 29;
                                        pub const _SC_SEM_NSEMS_MAX : ::libc::c_int = 31;
                                        pub const _SC_SEM_VALUE_MAX : ::libc::c_int = 32;
                                        pub const _SC_AIO_LISTIO_MAX : ::libc::c_int = 42;
                                        pub const _SC_AIO_MAX : ::libc::c_int = 43;
                                        pub const _SC_AIO_PRIO_DELTA_MAX : ::libc::c_int = 44;
                                        pub const _SC_ASYNCHRONOUS_IO : ::libc::c_int = 45;
                                        pub const _SC_DELAYTIMER_MAX : ::libc::c_int = 50;
                                        pub const _SC_MAPPED_FILES : ::libc::c_int = 53;
                                        pub const _SC_MEMLOCK : ::libc::c_int = 54;
                                        pub const _SC_MEMLOCK_RANGE : ::libc::c_int = 55;
                                        pub const _SC_MEMORY_PROTECTION : ::libc::c_int = 56;
                                        pub const _SC_MESSAGE_PASSING : ::libc::c_int = 57;
                                        pub const _SC_MQ_OPEN_MAX : ::libc::c_int = 58;
                                        pub const _SC_PRIORITIZED_IO : ::libc::c_int = 60;
                                        pub const _SC_PRIORITY_SCHEDULING : ::libc::c_int = 61;
                                        pub const _SC_REALTIME_SIGNALS : ::libc::c_int = 64;
                                        pub const _SC_RTSIG_MAX : ::libc::c_int = 66;
                                        pub const _SC_SEMAPHORES : ::libc::c_int = 67;
                                        pub const _SC_SHARED_MEMORY_OBJECTS : ::libc::c_int = 68;
                                        pub const _SC_SIGQUEUE_MAX : ::libc::c_int = 70;
                                        pub const _SC_SYNCHRONIZED_IO : ::libc::c_int = 75;
                                        pub const _SC_TIMER_MAX : ::libc::c_int = 93;
                                        pub const _SC_TIMERS : ::libc::c_int = 94;

                                        pub const PTHREAD_MUTEX_INITIALIZER: pthread_mutex_t = 0 as *mut _;
                                        pub const PTHREAD_COND_INITIALIZER: pthread_cond_t = 0 as *mut _;
                                        pub const PTHREAD_RWLOCK_INITIALIZER: pthread_rwlock_t = 0 as *mut _;
                                        pub const PTHREAD_MUTEX_RECURSIVE: ::libc::c_int = 2;
                                        pub const SIGSTKSZ: ::libc::size_t = 131072;

                                        extern {
                                            pub fn mprotect(addr: *const ::libc::c_void, len: ::libc::size_t, prot: ::libc::c_int)
                                                            -> ::libc::c_int;
                                            pub fn shm_open(name: *const ::libc::c_char, oflag: ::libc::c_int, mode: ::libc::mode_t)
                                                            -> ::libc::c_int;
                                            pub fn sysctl(name: *mut c_int,
                                                        namelen: ::libc::c_uint,
                                                        oldp: *mut ::libc::c_void,
                                                        oldlenp: *mut size_t,
                                                        newp: *mut ::libc::c_void,
                                                        newlen: ::libc::size_t)
                                                        -> ::libc::c_int;
                                            pub fn sysctlbyname(name: *const c_char,
                                                                oldp: *mut ::libc::c_void,
                                                                oldlenp: *mut size_t,
                                                                newp: *mut ::libc::c_void,
                                                                newlen: ::libc::size_t)
                                                                -> ::libc::c_int;
                                            pub fn clock_gettime(clk_id: ::libc::c_int, tp: *mut ::libc::timespec) -> ::libc::c_int;
                                            pub fn pthread_set_name_np(tid: ::libc::pthread_t, name: *const ::libc::c_char);
                                            pub fn pthread_main_np() -> ::libc::c_uint;
                                            pub fn pthread_stackseg_np(thread: pthread_t,
                                                                    sinfo: *mut stack_t) -> ::libc::c_uint;
                                            pub fn __errno() -> *const ::libc::c_int;
                                            pub fn backtrace(buf: *mut *mut ::libc::c_void,
                                                            sz: ::libc::size_t) -> ::size_t;
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(target_os = "bitrig")]
                                            {
                                                mod bitrig
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    
                                                    s!
                                                    {
                                                        pub struct glob_t {
                                                            pub gl_pathc:  c_int,
                                                            pub gl_matchc: ::libc::c_int,
                                                            pub gl_offs:   c_int,
                                                            pub gl_flags:  c_int,
                                                            pub gl_pathv:  *mut *mut c_char,
                                                            __unused1: *mut c_void,
                                                            __unused2: *mut c_void,
                                                            __unused3: *mut c_void,
                                                            __unused4: *mut c_void,
                                                            __unused5: *mut c_void,
                                                            __unused6: *mut c_void,
                                                            __unused7: *mut c_void,
                                                        }
                                                    }
                                                }
                                                
                                                use self::bitrig::*;
                                            }
                                            
                                            else
                                            {
                                                mod openbsd
                                                {
                                                    use ::
                                                    {
                                                        *,
                                                    };
                                                    
                                                    s!
                                                    {
                                                        pub struct glob_t {
                                                            pub gl_pathc:  c_int,
                                                            __unused1: ::libc::c_int,
                                                            pub gl_offs:   c_int,
                                                            __unused2: ::libc::c_int,
                                                            pub gl_pathv:  *mut *mut c_char,

                                                            __unused3: *mut c_void,

                                                            __unused4: *mut c_void,
                                                            __unused5: *mut c_void,
                                                            __unused6: *mut c_void,
                                                            __unused7: *mut c_void,
                                                            __unused8: *mut c_void,
                                                            __unused9: *mut c_void,
                                                        }
                                                    }
                                                }
                                                
                                                use self::openbsd::*;
                                            }
                                        }

                                    }
                                    
                                    pub use self::openbsdlike::*;
                                }
                                
                                else if #[cfg(any(target_os = "freebsd", target_os = "dragonfly"))]
                                {
                                    mod freebsdlike
                                    {
                                        use ::
                                        {
                                            *,
                                        };
                                        
                                        pub type clock_t = i32;
                                        pub type dev_t = u32;
                                        pub type ino_t = u32;
                                        pub type mode_t = u16;
                                        pub type nlink_t = u16;
                                        pub type blksize_t = u32;
                                        pub type fflags_t = u32;
                                        pub type pthread_attr_t = *mut ::libc::c_void;
                                        pub type rlim_t = i64;
                                        pub type pthread_mutex_t = *mut ::libc::c_void;
                                        pub type pthread_mutexattr_t = *mut ::libc::c_void;
                                        pub type pthread_cond_t = *mut ::libc::c_void;
                                        pub type pthread_rwlock_t = *mut ::libc::c_void;
                                        pub type pthread_key_t = ::libc::c_int;

                                        pub enum timezone {}

                                        s! {
                                            pub struct dirent {
                                                pub d_fileno: u32,
                                                pub d_reclen: u16,
                                                pub d_type: u8,
                                                pub d_namelen: u8,
                                                pub d_name: [::libc::c_char; 256],
                                            }

                                            pub struct glob_t {
                                                pub gl_pathc: ::libc::size_t,
                                                __unused1: ::libc::size_t,
                                                pub gl_offs: ::libc::size_t,
                                                __unused2: ::libc::c_int,
                                                pub gl_pathv:  *mut *mut ::libc::c_char,

                                                __unused3: *mut ::libc::c_void,

                                                __unused4: *mut ::libc::c_void,
                                                __unused5: *mut ::libc::c_void,
                                                __unused6: *mut ::libc::c_void,
                                                __unused7: *mut ::libc::c_void,
                                                __unused8: *mut ::libc::c_void,
                                            }

                                            pub struct sockaddr_storage {
                                                pub ss_len: u8,
                                                pub ss_family: ::libc::sa_family_t,
                                                __ss_pad1: [u8; 6],
                                                __ss_align: i64,
                                                __ss_pad2: [u8; 112],
                                            }

                                            pub struct addrinfo {
                                                pub ai_flags: ::libc::c_int,
                                                pub ai_family: ::libc::c_int,
                                                pub ai_socktype: ::libc::c_int,
                                                pub ai_protocol: ::libc::c_int,
                                                pub ai_addrlen: ::libc::socklen_t,
                                                pub ai_canonname: *mut ::libc::c_char,
                                                pub ai_addr: *mut ::libc::sockaddr,
                                                pub ai_next: *mut addrinfo,
                                            }

                                            pub struct sigset_t {
                                                bits: [u32; 4],
                                            }

                                            pub struct siginfo_t {
                                                pub si_signo: ::libc::c_int,
                                                pub si_errno: ::libc::c_int,
                                                pub si_code: ::libc::c_int,
                                                pub si_pid: ::libc::pid_t,
                                                pub si_uid: ::libc::uid_t,
                                                pub si_status: ::libc::c_int,
                                                pub si_addr: *mut ::libc::c_void,
                                                _pad: [::libc::c_int; 12],
                                            }

                                            pub struct sigaction {
                                                pub sa_sigaction: ::libc::sighandler_t,
                                                pub sa_flags: ::libc::c_int,
                                                pub sa_mask: sigset_t,
                                            }

                                            pub struct stack_t {
                                                pub ss_sp: *mut ::libc::c_char,
                                                pub ss_size: ::libc::size_t,
                                                pub ss_flags: ::libc::c_int,
                                            }
                                        }

                                        pub const EXIT_FAILURE: ::libc::c_int = 1;
                                        pub const EXIT_SUCCESS: ::libc::c_int = 0;
                                        pub const RAND_MAX: ::libc::c_int = 0x7fff_fffd;
                                        pub const EOF: ::libc::c_int = -1;
                                        pub const SEEK_SET: ::libc::c_int = 0;
                                        pub const SEEK_CUR: ::libc::c_int = 1;
                                        pub const SEEK_END: ::libc::c_int = 2;
                                        pub const _IOFBF: ::libc::c_int = 0;
                                        pub const _IONBF: ::libc::c_int = 2;
                                        pub const _IOLBF: ::libc::c_int = 1;
                                        pub const BUFSIZ: ::libc::c_uint = 1024;
                                        pub const FOPEN_MAX: ::libc::c_uint = 20;
                                        pub const FILENAME_MAX: ::libc::c_uint = 1024;
                                        pub const L_tmpnam: ::libc::c_uint = 1024;
                                        pub const TMP_MAX: ::libc::c_uint = 308915776;

                                        pub const O_RDONLY: ::libc::c_int = 0;
                                        pub const O_WRONLY: ::libc::c_int = 1;
                                        pub const O_RDWR: ::libc::c_int = 2;
                                        pub const O_APPEND: ::libc::c_int = 8;
                                        pub const O_CREAT: ::libc::c_int = 512;
                                        pub const O_EXCL: ::libc::c_int = 2048;
                                        pub const O_NOCTTY: ::libc::c_int = 32768;
                                        pub const O_TRUNC: ::libc::c_int = 1024;
                                        pub const O_CLOEXEC: ::libc::c_int = 0x00100000;
                                        pub const S_IFIFO: mode_t = 4096;
                                        pub const S_IFCHR: mode_t = 8192;
                                        pub const S_IFBLK: mode_t = 24576;
                                        pub const S_IFDIR: mode_t = 16384;
                                        pub const S_IFREG: mode_t = 32768;
                                        pub const S_IFLNK: mode_t = 40960;
                                        pub const S_IFSOCK: mode_t = 49152;
                                        pub const S_IFMT: mode_t = 61440;
                                        pub const S_IEXEC: mode_t = 64;
                                        pub const S_IWRITE: mode_t = 128;
                                        pub const S_IREAD: mode_t = 256;
                                        pub const S_IRWXU: mode_t = 448;
                                        pub const S_IXUSR: mode_t = 64;
                                        pub const S_IWUSR: mode_t = 128;
                                        pub const S_IRUSR: mode_t = 256;
                                        pub const S_IRWXG: mode_t = 56;
                                        pub const S_IXGRP: mode_t = 8;
                                        pub const S_IWGRP: mode_t = 16;
                                        pub const S_IRGRP: mode_t = 32;
                                        pub const S_IRWXO: mode_t = 7;
                                        pub const S_IXOTH: mode_t = 1;
                                        pub const S_IWOTH: mode_t = 2;
                                        pub const S_IROTH: mode_t = 4;
                                        pub const F_OK: ::libc::c_int = 0;
                                        pub const R_OK: ::libc::c_int = 4;
                                        pub const W_OK: ::libc::c_int = 2;
                                        pub const X_OK: ::libc::c_int = 1;
                                        pub const STDIN_FILENO: ::libc::c_int = 0;
                                        pub const STDOUT_FILENO: ::libc::c_int = 1;
                                        pub const STDERR_FILENO: ::libc::c_int = 2;
                                        pub const F_LOCK: ::libc::c_int = 1;
                                        pub const F_TEST: ::libc::c_int = 3;
                                        pub const F_TLOCK: ::libc::c_int = 2;
                                        pub const F_ULOCK: ::libc::c_int = 0;
                                        pub const F_DUPFD_CLOEXEC: ::libc::c_int = 17;
                                        pub const SIGHUP: ::libc::c_int = 1;
                                        pub const SIGINT: ::libc::c_int = 2;
                                        pub const SIGQUIT: ::libc::c_int = 3;
                                        pub const SIGILL: ::libc::c_int = 4;
                                        pub const SIGABRT: ::libc::c_int = 6;
                                        pub const SIGFPE: ::libc::c_int = 8;
                                        pub const SIGKILL: ::libc::c_int = 9;
                                        pub const SIGSEGV: ::libc::c_int = 11;
                                        pub const SIGPIPE: ::libc::c_int = 13;
                                        pub const SIGALRM: ::libc::c_int = 14;
                                        pub const SIGTERM: ::libc::c_int = 15;

                                        pub const PROT_NONE: ::libc::c_int = 0;
                                        pub const PROT_READ: ::libc::c_int = 1;
                                        pub const PROT_WRITE: ::libc::c_int = 2;
                                        pub const PROT_EXEC: ::libc::c_int = 4;

                                        pub const MAP_FILE: ::libc::c_int = 0x0000;
                                        pub const MAP_SHARED: ::libc::c_int = 0x0001;
                                        pub const MAP_PRIVATE: ::libc::c_int = 0x0002;
                                        pub const MAP_FIXED: ::libc::c_int = 0x0010;
                                        pub const MAP_ANON: ::libc::c_int = 0x1000;

                                        pub const MAP_FAILED: *mut ::libc::c_void = !0 as *mut ::libc::c_void;

                                        pub const MCL_CURRENT: ::libc::c_int = 0x0001;
                                        pub const MCL_FUTURE: ::libc::c_int = 0x0002;

                                        pub const MS_SYNC: ::libc::c_int = 0x0000;
                                        pub const MS_ASYNC: ::libc::c_int = 0x0001;
                                        pub const MS_INVALIDATE: ::libc::c_int = 0x0002;

                                        pub const EPERM: ::libc::c_int = 1;
                                        pub const ENOENT: ::libc::c_int = 2;
                                        pub const ESRCH: ::libc::c_int = 3;
                                        pub const EINTR: ::libc::c_int = 4;
                                        pub const EIO: ::libc::c_int = 5;
                                        pub const ENXIO: ::libc::c_int = 6;
                                        pub const E2BIG: ::libc::c_int = 7;
                                        pub const ENOEXEC: ::libc::c_int = 8;
                                        pub const EBADF: ::libc::c_int = 9;
                                        pub const ECHILD: ::libc::c_int = 10;
                                        pub const EDEADLK: ::libc::c_int = 11;
                                        pub const ENOMEM: ::libc::c_int = 12;
                                        pub const EACCES: ::libc::c_int = 13;
                                        pub const EFAULT: ::libc::c_int = 14;
                                        pub const ENOTBLK: ::libc::c_int = 15;
                                        pub const EBUSY: ::libc::c_int = 16;
                                        pub const EEXIST: ::libc::c_int = 17;
                                        pub const EXDEV: ::libc::c_int = 18;
                                        pub const ENODEV: ::libc::c_int = 19;
                                        pub const ENOTDIR: ::libc::c_int = 20;
                                        pub const EISDIR: ::libc::c_int = 21;
                                        pub const EINVAL: ::libc::c_int = 22;
                                        pub const ENFILE: ::libc::c_int = 23;
                                        pub const EMFILE: ::libc::c_int = 24;
                                        pub const ENOTTY: ::libc::c_int = 25;
                                        pub const ETXTBSY: ::libc::c_int = 26;
                                        pub const EFBIG: ::libc::c_int = 27;
                                        pub const ENOSPC: ::libc::c_int = 28;
                                        pub const ESPIPE: ::libc::c_int = 29;
                                        pub const EROFS: ::libc::c_int = 30;
                                        pub const EMLINK: ::libc::c_int = 31;
                                        pub const EPIPE: ::libc::c_int = 32;
                                        pub const EDOM: ::libc::c_int = 33;
                                        pub const ERANGE: ::libc::c_int = 34;
                                        pub const EAGAIN: ::libc::c_int = 35;
                                        pub const EWOULDBLOCK: ::libc::c_int = 35;
                                        pub const EINPROGRESS: ::libc::c_int = 36;
                                        pub const EALREADY: ::libc::c_int = 37;
                                        pub const ENOTSOCK: ::libc::c_int = 38;
                                        pub const EDESTADDRREQ: ::libc::c_int = 39;
                                        pub const EMSGSIZE: ::libc::c_int = 40;
                                        pub const EPROTOTYPE: ::libc::c_int = 41;
                                        pub const ENOPROTOOPT: ::libc::c_int = 42;
                                        pub const EPROTONOSUPPORT: ::libc::c_int = 43;
                                        pub const ESOCKTNOSUPPORT: ::libc::c_int = 44;
                                        pub const EOPNOTSUPP: ::libc::c_int = 45;
                                        pub const EPFNOSUPPORT: ::libc::c_int = 46;
                                        pub const EAFNOSUPPORT: ::libc::c_int = 47;
                                        pub const EADDRINUSE: ::libc::c_int = 48;
                                        pub const EADDRNOTAVAIL: ::libc::c_int = 49;
                                        pub const ENETDOWN: ::libc::c_int = 50;
                                        pub const ENETUNREACH: ::libc::c_int = 51;
                                        pub const ENETRESET: ::libc::c_int = 52;
                                        pub const ECONNABORTED: ::libc::c_int = 53;
                                        pub const ECONNRESET: ::libc::c_int = 54;
                                        pub const ENOBUFS: ::libc::c_int = 55;
                                        pub const EISCONN: ::libc::c_int = 56;
                                        pub const ENOTCONN: ::libc::c_int = 57;
                                        pub const ESHUTDOWN: ::libc::c_int = 58;
                                        pub const ETOOMANYREFS: ::libc::c_int = 59;
                                        pub const ETIMEDOUT: ::libc::c_int = 60;
                                        pub const ECONNREFUSED: ::libc::c_int = 61;
                                        pub const ELOOP: ::libc::c_int = 62;
                                        pub const ENAMETOOLONG: ::libc::c_int = 63;
                                        pub const EHOSTDOWN: ::libc::c_int = 64;
                                        pub const EHOSTUNREACH: ::libc::c_int = 65;
                                        pub const ENOTEMPTY: ::libc::c_int = 66;
                                        pub const EPROCLIM: ::libc::c_int = 67;
                                        pub const EUSERS: ::libc::c_int = 68;
                                        pub const EDQUOT: ::libc::c_int = 69;
                                        pub const ESTALE: ::libc::c_int = 70;
                                        pub const EREMOTE: ::libc::c_int = 71;
                                        pub const EBADRPC: ::libc::c_int = 72;
                                        pub const ERPCMISMATCH: ::libc::c_int = 73;
                                        pub const EPROGUNAVAIL: ::libc::c_int = 74;
                                        pub const EPROGMISMATCH: ::libc::c_int = 75;
                                        pub const EPROCUNAVAIL: ::libc::c_int = 76;
                                        pub const ENOLCK: ::libc::c_int = 77;
                                        pub const ENOSYS: ::libc::c_int = 78;
                                        pub const EFTYPE: ::libc::c_int = 79;
                                        pub const EAUTH: ::libc::c_int = 80;
                                        pub const ENEEDAUTH: ::libc::c_int = 81;
                                        pub const EIDRM: ::libc::c_int = 82;
                                        pub const ENOMSG: ::libc::c_int = 83;
                                        pub const EOVERFLOW: ::libc::c_int = 84;
                                        pub const ECANCELED: ::libc::c_int = 85;
                                        pub const EILSEQ: ::libc::c_int = 86;
                                        pub const ENOATTR: ::libc::c_int = 87;
                                        pub const EDOOFUS: ::libc::c_int = 88;
                                        pub const EBADMSG: ::libc::c_int = 89;
                                        pub const EMULTIHOP: ::libc::c_int = 90;
                                        pub const ENOLINK: ::libc::c_int = 91;
                                        pub const EPROTO: ::libc::c_int = 92;
                                        pub const ELAST: ::libc::c_int = 96;

                                        pub const F_DUPFD: ::libc::c_int = 0;
                                        pub const F_GETFD: ::libc::c_int = 1;
                                        pub const F_SETFD: ::libc::c_int = 2;
                                        pub const F_GETFL: ::libc::c_int = 3;
                                        pub const F_SETFL: ::libc::c_int = 4;

                                        pub const SIGTRAP: ::libc::c_int = 5;

                                        pub const GLOB_APPEND  : ::libc::c_int = 0x0001;
                                        pub const GLOB_DOOFFS  : ::libc::c_int = 0x0002;
                                        pub const GLOB_ERR     : ::libc::c_int = 0x0004;
                                        pub const GLOB_MARK    : ::libc::c_int = 0x0008;
                                        pub const GLOB_NOCHECK : ::libc::c_int = 0x0010;
                                        pub const GLOB_NOSORT  : ::libc::c_int = 0x0020;
                                        pub const GLOB_NOESCAPE: ::libc::c_int = 0x2000;

                                        pub const GLOB_NOSPACE : ::libc::c_int = -1;
                                        pub const GLOB_ABORTED : ::libc::c_int = -2;
                                        pub const GLOB_NOMATCH : ::libc::c_int = -3;

                                        pub const POSIX_MADV_NORMAL: ::libc::c_int = 0;
                                        pub const POSIX_MADV_RANDOM: ::libc::c_int = 1;
                                        pub const POSIX_MADV_SEQUENTIAL: ::libc::c_int = 2;
                                        pub const POSIX_MADV_WILLNEED: ::libc::c_int = 3;
                                        pub const POSIX_MADV_DONTNEED: ::libc::c_int = 4;

                                        pub const _SC_IOV_MAX: ::libc::c_int = 56;
                                        pub const _SC_GETGR_R_SIZE_MAX: ::libc::c_int = 70;
                                        pub const _SC_GETPW_R_SIZE_MAX: ::libc::c_int = 71;
                                        pub const _SC_LOGIN_NAME_MAX: ::libc::c_int = 73;
                                        pub const _SC_MQ_PRIO_MAX: ::libc::c_int = 75;
                                        pub const _SC_THREAD_ATTR_STACKADDR: ::libc::c_int = 82;
                                        pub const _SC_THREAD_ATTR_STACKSIZE: ::libc::c_int = 83;
                                        pub const _SC_THREAD_DESTRUCTOR_ITERATIONS: ::libc::c_int = 85;
                                        pub const _SC_THREAD_KEYS_MAX: ::libc::c_int = 86;
                                        pub const _SC_THREAD_PRIO_INHERIT: ::libc::c_int = 87;
                                        pub const _SC_THREAD_PRIO_PROTECT: ::libc::c_int = 88;
                                        pub const _SC_THREAD_PRIORITY_SCHEDULING: ::libc::c_int = 89;
                                        pub const _SC_THREAD_PROCESS_SHARED: ::libc::c_int = 90;
                                        pub const _SC_THREAD_SAFE_FUNCTIONS: ::libc::c_int = 91;
                                        pub const _SC_THREAD_STACK_MIN: ::libc::c_int = 93;
                                        pub const _SC_THREAD_THREADS_MAX: ::libc::c_int = 94;
                                        pub const _SC_THREADS: ::libc::c_int = 96;
                                        pub const _SC_TTY_NAME_MAX: ::libc::c_int = 101;
                                        pub const _SC_ATEXIT_MAX: ::libc::c_int = 107;
                                        pub const _SC_XOPEN_CRYPT: ::libc::c_int = 108;
                                        pub const _SC_XOPEN_ENH_I18N: ::libc::c_int = 109;
                                        pub const _SC_XOPEN_LEGACY: ::libc::c_int = 110;
                                        pub const _SC_XOPEN_REALTIME: ::libc::c_int = 111;
                                        pub const _SC_XOPEN_REALTIME_THREADS: ::libc::c_int = 112;
                                        pub const _SC_XOPEN_SHM: ::libc::c_int = 113;
                                        pub const _SC_XOPEN_UNIX: ::libc::c_int = 115;
                                        pub const _SC_XOPEN_VERSION: ::libc::c_int = 116;
                                        pub const _SC_XOPEN_XCU_VERSION: ::libc::c_int = 117;

                                        pub const PTHREAD_CREATE_JOINABLE: ::libc::c_int = 0;
                                        pub const PTHREAD_CREATE_DETACHED: ::libc::c_int = 1;

                                        pub const CLOCK_REALTIME: ::libc::c_int = 0;
                                        pub const CLOCK_MONOTONIC: ::libc::c_int = 4;

                                        pub const RLIMIT_CPU: ::libc::c_int = 0;
                                        pub const RLIMIT_FSIZE: ::libc::c_int = 1;
                                        pub const RLIMIT_DATA: ::libc::c_int = 2;
                                        pub const RLIMIT_STACK: ::libc::c_int = 3;
                                        pub const RLIMIT_CORE: ::libc::c_int = 4;
                                        pub const RLIMIT_RSS: ::libc::c_int = 5;
                                        pub const RLIMIT_MEMLOCK: ::libc::c_int = 6;
                                        pub const RLIMIT_NPROC: ::libc::c_int = 7;
                                        pub const RLIMIT_NOFILE: ::libc::c_int = 8;
                                        pub const RLIMIT_SBSIZE: ::libc::c_int = 9;
                                        pub const RLIMIT_VMEM: ::libc::c_int = 10;
                                        pub const RLIMIT_AS: ::libc::c_int = RLIMIT_VMEM;
                                        pub const RLIMIT_NPTS: ::libc::c_int = 11;
                                        pub const RLIMIT_SWAP: ::libc::c_int = 12;

                                        pub const RLIM_NLIMITS: rlim_t = 13;
                                        pub const RLIM_INFINITY: rlim_t = 0x7fff_ffff_ffff_ffff;

                                        pub const RUSAGE_SELF: ::libc::c_int = 0;
                                        pub const RUSAGE_CHILDREN: ::libc::c_int = -1;
                                        pub const RUSAGE_THREAD: ::libc::c_int = 1;

                                        pub const MADV_NORMAL: ::libc::c_int = 0;
                                        pub const MADV_RANDOM: ::libc::c_int = 1;
                                        pub const MADV_SEQUENTIAL: ::libc::c_int = 2;
                                        pub const MADV_WILLNEED: ::libc::c_int = 3;
                                        pub const MADV_DONTNEED: ::libc::c_int = 4;
                                        pub const MADV_FREE: ::libc::c_int = 5;
                                        pub const MADV_NOSYNC: ::libc::c_int = 6;
                                        pub const MADV_AUTOSYNC: ::libc::c_int = 7;
                                        pub const MADV_NOCORE: ::libc::c_int = 8;
                                        pub const MADV_CORE: ::libc::c_int = 9;
                                        pub const MADV_PROTECT: ::libc::c_int = 10;

                                        pub const MINCORE_INCORE: ::libc::c_int =  0x1;
                                        pub const MINCORE_REFERENCED: ::libc::c_int = 0x2;
                                        pub const MINCORE_MODIFIED: ::libc::c_int = 0x4;
                                        pub const MINCORE_REFERENCED_OTHER: ::libc::c_int = 0x8;
                                        pub const MINCORE_MODIFIED_OTHER: ::libc::c_int = 0x10;
                                        pub const MINCORE_SUPER: ::libc::c_int = 0x20;

                                        pub const AF_INET: ::libc::c_int = 2;
                                        pub const AF_INET6: ::libc::c_int = 28;
                                        pub const AF_UNIX: ::libc::c_int = 1;
                                        pub const SOCK_STREAM: ::libc::c_int = 1;
                                        pub const SOCK_DGRAM: ::libc::c_int = 2;
                                        pub const SOCK_RAW: ::libc::c_int = 3;
                                        pub const IPPROTO_TCP: ::libc::c_int = 6;
                                        pub const IPPROTO_IP: ::libc::c_int = 0;
                                        pub const IPPROTO_IPV6: ::libc::c_int = 41;
                                        pub const IP_MULTICAST_TTL: ::libc::c_int = 10;
                                        pub const IP_MULTICAST_LOOP: ::libc::c_int = 11;
                                        pub const IP_TTL: ::libc::c_int = 4;
                                        pub const IP_HDRINCL: ::libc::c_int = 2;
                                        pub const IP_ADD_MEMBERSHIP: ::libc::c_int = 12;
                                        pub const IP_DROP_MEMBERSHIP: ::libc::c_int = 13;
                                        pub const IPV6_JOIN_GROUP: ::libc::c_int = 12;
                                        pub const IPV6_LEAVE_GROUP: ::libc::c_int = 13;

                                        pub const TCP_NODELAY: ::libc::c_int = 1;
                                        pub const TCP_KEEPIDLE: ::libc::c_int = 256;
                                        pub const SOL_SOCKET: ::libc::c_int = 0xffff;
                                        pub const SO_DEBUG: ::libc::c_int = 0x01;
                                        pub const SO_ACCEPTCONN: ::libc::c_int = 0x0002;
                                        pub const SO_REUSEADDR: ::libc::c_int = 0x0004;
                                        pub const SO_KEEPALIVE: ::libc::c_int = 0x0008;
                                        pub const SO_DONTROUTE: ::libc::c_int = 0x0010;
                                        pub const SO_BROADCAST: ::libc::c_int = 0x0020;
                                        pub const SO_USELOOPBACK: ::libc::c_int = 0x0040;
                                        pub const SO_LINGER: ::libc::c_int = 0x0080;
                                        pub const SO_OOBINLINE: ::libc::c_int = 0x0100;
                                        pub const SO_REUSEPORT: ::libc::c_int = 0x0200;
                                        pub const SO_SNDBUF: ::libc::c_int = 0x1001;
                                        pub const SO_RCVBUF: ::libc::c_int = 0x1002;
                                        pub const SO_SNDLOWAT: ::libc::c_int = 0x1003;
                                        pub const SO_RCVLOWAT: ::libc::c_int = 0x1004;
                                        pub const SO_SNDTIMEO: ::libc::c_int = 0x1005;
                                        pub const SO_RCVTIMEO: ::libc::c_int = 0x1006;
                                        pub const SO_ERROR: ::libc::c_int = 0x1007;
                                        pub const SO_TYPE: ::libc::c_int = 0x1008;

                                        pub const IFF_LOOPBACK: ::libc::c_int = 0x8;

                                        pub const SHUT_RD: ::libc::c_int = 0;
                                        pub const SHUT_WR: ::libc::c_int = 1;
                                        pub const SHUT_RDWR: ::libc::c_int = 2;

                                        pub const LOCK_SH: ::libc::c_int = 1;
                                        pub const LOCK_EX: ::libc::c_int = 2;
                                        pub const LOCK_NB: ::libc::c_int = 4;
                                        pub const LOCK_UN: ::libc::c_int = 8;

                                        pub const O_SYNC: ::libc::c_int = 128;
                                        pub const O_NONBLOCK: ::libc::c_int = 4;
                                        pub const CTL_KERN: ::libc::c_int = 1;
                                        pub const KERN_PROC: ::libc::c_int = 14;

                                        pub const MAP_COPY: ::libc::c_int = 0x0002;
                                        pub const MAP_RENAME: ::libc::c_int = 0x0020;
                                        pub const MAP_NORESERVE: ::libc::c_int = 0x0040;
                                        pub const MAP_HASSEMAPHORE: ::libc::c_int = 0x0200;
                                        pub const MAP_STACK: ::libc::c_int = 0x0400;
                                        pub const MAP_NOSYNC: ::libc::c_int = 0x0800;
                                        pub const MAP_NOCORE: ::libc::c_int = 0x020000;

                                        pub const IPPROTO_RAW: ::libc::c_int = 255;

                                        pub const _SC_ARG_MAX: ::libc::c_int = 1;
                                        pub const _SC_CHILD_MAX: ::libc::c_int = 2;
                                        pub const _SC_CLK_TCK: ::libc::c_int = 3;
                                        pub const _SC_NGROUPS_MAX: ::libc::c_int = 4;
                                        pub const _SC_OPEN_MAX: ::libc::c_int = 5;
                                        pub const _SC_JOB_CONTROL: ::libc::c_int = 6;
                                        pub const _SC_SAVED_IDS: ::libc::c_int = 7;
                                        pub const _SC_VERSION: ::libc::c_int = 8;
                                        pub const _SC_BC_BASE_MAX: ::libc::c_int = 9;
                                        pub const _SC_BC_DIM_MAX: ::libc::c_int = 10;
                                        pub const _SC_BC_SCALE_MAX: ::libc::c_int = 11;
                                        pub const _SC_BC_STRING_MAX: ::libc::c_int = 12;
                                        pub const _SC_COLL_WEIGHTS_MAX: ::libc::c_int = 13;
                                        pub const _SC_EXPR_NEST_MAX: ::libc::c_int = 14;
                                        pub const _SC_LINE_MAX: ::libc::c_int = 15;
                                        pub const _SC_RE_DUP_MAX: ::libc::c_int = 16;
                                        pub const _SC_2_VERSION: ::libc::c_int = 17;
                                        pub const _SC_2_C_BIND: ::libc::c_int = 18;
                                        pub const _SC_2_C_DEV: ::libc::c_int = 19;
                                        pub const _SC_2_CHAR_TERM: ::libc::c_int = 20;
                                        pub const _SC_2_FORT_DEV: ::libc::c_int = 21;
                                        pub const _SC_2_FORT_RUN: ::libc::c_int = 22;
                                        pub const _SC_2_LOCALEDEF: ::libc::c_int = 23;
                                        pub const _SC_2_SW_DEV: ::libc::c_int = 24;
                                        pub const _SC_2_UPE: ::libc::c_int = 25;
                                        pub const _SC_STREAM_MAX: ::libc::c_int = 26;
                                        pub const _SC_TZNAME_MAX: ::libc::c_int = 27;
                                        pub const _SC_ASYNCHRONOUS_IO: ::libc::c_int = 28;
                                        pub const _SC_MAPPED_FILES: ::libc::c_int = 29;
                                        pub const _SC_MEMLOCK: ::libc::c_int = 30;
                                        pub const _SC_MEMLOCK_RANGE: ::libc::c_int = 31;
                                        pub const _SC_MEMORY_PROTECTION: ::libc::c_int = 32;
                                        pub const _SC_MESSAGE_PASSING: ::libc::c_int = 33;
                                        pub const _SC_PRIORITIZED_IO: ::libc::c_int = 34;
                                        pub const _SC_PRIORITY_SCHEDULING: ::libc::c_int = 35;
                                        pub const _SC_REALTIME_SIGNALS: ::libc::c_int = 36;
                                        pub const _SC_SEMAPHORES: ::libc::c_int = 37;
                                        pub const _SC_FSYNC: ::libc::c_int = 38;
                                        pub const _SC_SHARED_MEMORY_OBJECTS: ::libc::c_int = 39;
                                        pub const _SC_SYNCHRONIZED_IO: ::libc::c_int = 40;
                                        pub const _SC_TIMERS: ::libc::c_int = 41;
                                        pub const _SC_AIO_LISTIO_MAX: ::libc::c_int = 42;
                                        pub const _SC_AIO_MAX: ::libc::c_int = 43;
                                        pub const _SC_AIO_PRIO_DELTA_MAX: ::libc::c_int = 44;
                                        pub const _SC_DELAYTIMER_MAX: ::libc::c_int = 45;
                                        pub const _SC_MQ_OPEN_MAX: ::libc::c_int = 46;
                                        pub const _SC_PAGESIZE: ::libc::c_int = 47;
                                        pub const _SC_RTSIG_MAX: ::libc::c_int = 48;
                                        pub const _SC_SEM_NSEMS_MAX: ::libc::c_int = 49;
                                        pub const _SC_SEM_VALUE_MAX: ::libc::c_int = 50;
                                        pub const _SC_SIGQUEUE_MAX: ::libc::c_int = 51;
                                        pub const _SC_TIMER_MAX: ::libc::c_int = 52;

                                        pub const PTHREAD_MUTEX_INITIALIZER: pthread_mutex_t = 0 as *mut _;
                                        pub const PTHREAD_COND_INITIALIZER: pthread_cond_t = 0 as *mut _;
                                        pub const PTHREAD_RWLOCK_INITIALIZER: pthread_rwlock_t = 0 as *mut _;
                                        pub const PTHREAD_MUTEX_RECURSIVE: ::libc::c_int = 2;

                                        extern
                                        {
                                            pub fn mprotect(addr: *const ::libc::c_void, len: ::libc::size_t, prot: ::libc::c_int)
                                                            -> ::libc::c_int;
                                            pub fn shm_open(name: *const ::libc::c_char, oflag: ::libc::c_int, mode: ::libc::mode_t)
                                                            -> ::libc::c_int;
                                            pub fn sysctl(name: *const ::libc::c_int,
                                                        namelen: ::libc::c_uint,
                                                        oldp: *mut ::libc::c_void,
                                                        oldlenp: *mut size_t,
                                                        newp: *const ::libc::c_void,
                                                        newlen: ::libc::size_t)
                                                        -> ::libc::c_int;
                                            pub fn sysctlbyname(name: *const ::libc::c_char,
                                                                oldp: *mut ::libc::c_void,
                                                                oldlenp: *mut size_t,
                                                                newp: *const ::libc::c_void,
                                                                newlen: ::libc::size_t)
                                                                -> ::libc::c_int;
                                            pub fn clock_gettime(clk_id: ::libc::c_int, tp: *mut ::libc::timespec) -> ::libc::c_int;
                                            pub fn pthread_set_name_np(tid: ::libc::pthread_t, name: *const ::libc::c_char);
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(target_arch = "x86")]
                                            {
                                                mod x86
                                                {
                                                    use ::
                                                    {
                                                        *
                                                    };
                                                    
                                                    pub type c_long = i32;
                                                    pub type c_ulong = u32;
                                                    pub type time_t = i32;
                                                    pub type suseconds_t = i32;

                                                    s! {
                                                        pub struct stat {
                                                            pub st_dev: ::libc::dev_t,
                                                            pub st_ino: ::libc::ino_t,
                                                            pub st_mode: ::libc::mode_t,
                                                            pub st_nlink: ::nlink_t,
                                                            pub st_uid: ::libc::uid_t,
                                                            pub st_gid: ::libc::gid_t,
                                                            pub st_rdev: ::libc::dev_t,
                                                            pub st_atime: ::libc::time_t,
                                                            pub st_atime_nsec: ::libc::c_long,
                                                            pub st_mtime: ::libc::time_t,
                                                            pub st_mtime_nsec: ::libc::c_long,
                                                            pub st_ctime: ::libc::time_t,
                                                            pub st_ctime_nsec: ::libc::c_long,
                                                            pub st_size: ::libc::off_t,
                                                            pub st_blocks: ::blkcnt_t,
                                                            pub st_blksize: ::blksize_t,
                                                            pub st_flags: ::fflags_t,
                                                            pub st_gen: ::libc::uint32_t,
                                                            pub st_lspare: ::libc::int32_t,
                                                            pub st_birthtime: ::libc::time_t,
                                                            pub st_birthtime_nsec: ::libc::c_long,
                                                            __unused: [u8; 8],
                                                        }
                                                    }

                                                }
                                                
                                                pub use self::x86::*;
                                            }
                                            
                                            else if #[cfg(target_arch = "x86_64")]
                                            {
                                                mod x86_64
                                                {
                                                    use ::
                                                    {
                                                        *
                                                    };
                                                    
                                                    pub type c_long = i64;
                                                    pub type c_ulong = u64;
                                                    pub type time_t = i64;
                                                    pub type suseconds_t = i64;

                                                    s!
                                                    {
                                                        pub struct stat
                                                        {
                                                            pub st_dev: ::libc::dev_t,
                                                            pub st_ino: ::libc::ino_t,
                                                            pub st_mode: ::libc::mode_t,
                                                            pub st_nlink: ::nlink_t,
                                                            pub st_uid: ::libc::uid_t,
                                                            pub st_gid: ::libc::gid_t,
                                                            pub st_rdev: ::libc::dev_t,
                                                            pub st_atime: ::libc::time_t,
                                                            pub st_atime_nsec: ::libc::c_long,
                                                            pub st_mtime: ::libc::time_t,
                                                            pub st_mtime_nsec: ::libc::c_long,
                                                            pub st_ctime: ::libc::time_t,
                                                            pub st_ctime_nsec: ::libc::c_long,
                                                            pub st_size: ::libc::off_t,
                                                            pub st_blocks: ::blkcnt_t,
                                                            pub st_blksize: ::blksize_t,
                                                            pub st_flags: ::fflags_t,
                                                            pub st_gen: ::libc::uint32_t,
                                                            pub st_lspare: ::libc::int32_t,
                                                            pub st_birthtime: ::libc::time_t,
                                                            pub st_birthtime_nsec: ::libc::c_long,
                                                        }
                                                    }

                                                }
                                                
                                                pub use self::x86_64::*;
                                            }
                                            
                                            else {}
                                        }

                                        cfg_if!
                                        {
                                            if #[cfg(target_os = "freebsd")]
                                            {
                                                mod freebsd
                                                {
                                                    use ::
                                                    {
                                                        *
                                                    };
                                                    
                                                    pub const PTHREAD_STACK_MIN: ::libc::size_t = 2048;
                                                    pub const KERN_PROC_PATHNAME: ::libc::c_int = 12;

                                                    extern
                                                    {
                                                        pub fn __error() -> *mut ::libc::c_int;
                                                    }
                                                }
                                                
                                                pub use self::freebsd::*;
                                            }
                                            
                                            else if #[cfg(target_os = "dragonfly")]
                                            {
                                                mod dragonfly
                                                {
                                                    use ::
                                                    {
                                                        *
                                                    };
                                                    
                                                    pub const PTHREAD_STACK_MIN: ::libc::size_t = 1024;
                                                    pub const KERN_PROC_PATHNAME: ::libc::c_int = 9;

                                                    extern {
                                                        pub fn __dfly_error() -> *const ::libc::c_int;
                                                    }
                                                }
                                                
                                                pub use self::dragonfly::*;
                                            }
                                            
                                            else {}
                                        }

                                    }
                                    
                                    pub use self::freebsdlike::*;
                                }
                                
                                else { }
                            }

                        }
                        
                        pub use self::bsd::*;
                    }
                    
                    else { }
                }
            }
            
            pub use self::unix::*;
        }
    }
}

pub mod marker
{
    pub use std::marker::{ * };
}

pub mod mem
{
    pub use std::mem::{ * };

    pub mod chr
    {
        /*!
        This library provides heavily optimized routines for string search primitives.*/
        use ::
        {
            *
        };
        /*
        #![deny(missing_docs)]
        #![no_std]
        // It's just not worth trying to squash all dead code warnings. Pretty
        // unfortunate IMO. Not really sure how to fix this other than to either
        // live with it or sprinkle a whole mess of `cfg` annotations everywhere.
        #![cfg_attr(
            not(any(
                all(target_arch = "x86_64", target_feature = "sse2"),
                target_arch = "wasm32",
                target_arch = "aarch64",
            )),
            allow(dead_code)
        )]
        // Same deal for miri.
        #![cfg_attr(miri, allow(dead_code, unused_macros))]

        // Supporting 8-bit (or others) would be fine. If you need it, please submit a
        // bug report at https://github.com/BurntSushi/memchr
        #[cfg(not(any(
            target_pointer_width = "16",
            target_pointer_width = "32",
            target_pointer_width = "64"
        )))]
        compile_error!("memchr currently not supported on non-{16,32,64}");

        #[cfg(any(test, feature = "std"))]
        extern crate std;

        #[cfg(any(test, feature = "alloc"))]
        extern crate alloc;

        pub use crate::memchr::{
            memchr, memchr2, memchr2_iter, memchr3, memchr3_iter, memchr_iter,
            memrchr, memrchr2, memrchr2_iter, memrchr3, memrchr3_iter, memrchr_iter,
            Memchr, Memchr2, Memchr3,
        };
        */
        pub mod arch
        {
            /*!
            A module with low-level architecture dependent routines. */
            use ::
            {
                *,
            };
            
            pub mod all
            {
                /*!
                Contains architecture independent routines. */
                use ::
                {
                    *,
                };
                /*
                */

                pub mod memchr
                {
                    /*!
                    Provides architecture independent implementations of `memchr` and friends. */
                    use ::
                    {
                        *,
                    };
                    /*
                    use crate::{arch::generic::memchr as generic, ext::Pointer};
                    */
                    /// The number of bytes in a single `usize` value.
                    const USIZE_BYTES: usize = (usize::BITS / 8) as usize;
                    /// The bits that must be zero for a `*const usize` to be properly aligned.
                    const USIZE_ALIGN: usize = USIZE_BYTES - 1;
                    /// Finds all occurrences of a single byte in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct One
                    {
                        s1: u8,
                        v1: usize,
                    }

                    impl One
                    {
                        /// The number of bytes we examine per each iteration of our search loop.
                        const LOOP_BYTES: usize = 2 * USIZE_BYTES;
                        /// Create a new searcher that finds occurrences of the byte given.
                        #[inline] pub fn new(needle: u8) -> One { One { s1: needle, v1: splat(needle) } }
                        /// Return the first occurrence of the needle in the given haystack.
                        #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize> {
                            unsafe {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.find_raw(s, e)
                                })
                            }
                        }
                        /// Return the last occurrence of the needle in the given haystack.
                        #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize> {
                            unsafe {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.rfind_raw(s, e)
                                })
                            }
                        }
                        /// Counts all occurrences of this byte in the given haystack.
                        #[inline] pub fn count(&self, haystack: &[u8]) -> usize 
                        {
                            unsafe {
                                let start = haystack.as_ptr();
                                let end = start.add(haystack.len());
                                self.count_raw(start, end)
                            }
                        }
                        /// Like `find`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let chunk = start.cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let mut cur =
                                start.add(USIZE_BYTES - (start.as_usize() & USIZE_ALIGN));
                            debug_assert!(cur > start);
                            if len <= One::LOOP_BYTES {
                                return generic::fwd_byte_by_byte(cur, end, confirm);
                            }
                            debug_assert!(end.sub(One::LOOP_BYTES) >= start);
                            while cur <= end.sub(One::LOOP_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let a = cur.cast::<usize>().read();
                                let b = cur.add(USIZE_BYTES).cast::<usize>().read();
                                if self.has_needle(a) || self.has_needle(b) {
                                    break;
                                }
                                cur = cur.add(One::LOOP_BYTES);
                            }
                            generic::fwd_byte_by_byte(cur, end, confirm)
                        }
                        /// Like `rfind`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn rfind_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8> 
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let chunk = end.sub(USIZE_BYTES).cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let mut cur = end.sub(end.as_usize() & USIZE_ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            if len <= One::LOOP_BYTES {
                                return generic::rev_byte_by_byte(start, cur, confirm);
                            }
                            while cur >= start.add(One::LOOP_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let a = cur.sub(2 * USIZE_BYTES).cast::<usize>().read();
                                let b = cur.sub(1 * USIZE_BYTES).cast::<usize>().read();
                                if self.has_needle(a) || self.has_needle(b) {
                                    break;
                                }
                                cur = cur.sub(One::LOOP_BYTES);
                            }
                            generic::rev_byte_by_byte(start, cur, confirm)
                        }
                        /// Counts all occurrences of this byte in the given haystack represented by raw pointers.
                        #[inline] pub unsafe fn count_raw(&self, start: *const u8, end: *const u8) -> usize 
                        {
                            if start >= end {
                                return 0;
                            }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::count_byte_by_byte(start, end, confirm);
                            }
                            
                            let mut cur =
                                start.add(USIZE_BYTES - (start.as_usize() & USIZE_ALIGN));
                            debug_assert!(cur > start);
                            
                            let mut count = generic::count_byte_by_byte(start, cur, confirm);
                            if len <= One::LOOP_BYTES {
                                return count + generic::count_byte_by_byte(cur, end, confirm);
                            }
                            debug_assert!(end.sub(One::LOOP_BYTES) >= start);
                            while cur <= end.sub(One::LOOP_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let a = cur.cast::<usize>().read();
                                let b = cur.add(USIZE_BYTES).cast::<usize>().read();
                                count += self.count_bytes(a);
                                count += self.count_bytes(b);
                                cur = cur.add(One::LOOP_BYTES);
                            }
                            count += generic::count_byte_by_byte(cur, end, confirm);
                            count
                        }
                        /// Returns an iterator over all occurrences of the needle byte in the given haystack.
                        pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> OneIter<'a, 'h> { OneIter { searcher: self, it: generic::Iter::new(haystack) } }

                        #[inline( always )] fn has_needle(&self, chunk: usize) -> bool { has_zero_byte(self.v1 ^ chunk) }

                        #[inline( always )] fn count_bytes(&self, chunk: usize) -> usize { count_bytes(self.v1 ^ chunk) }

                        #[inline( always )] fn confirm(&self, haystack_byte: u8) -> bool { self.s1 == haystack_byte }
                    }
                    /// An iterator over all occurrences of a single byte in a haystack.
                    #[derive(Clone, Debug)]
                    pub struct OneIter<'a, 'h>
                    {
                        /// The underlying memchr searcher.
                        searcher: &'a One,
                        /// Generic iterator implementation.
                        it: generic::Iter<'h>,
                    }

                    impl<'a, 'h> Iterator for OneIter<'a, 'h>
                    {
                        type Item = usize;

                        #[inline] fn next(&mut self) -> Option<usize> 
                        {
                            unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                        }

                        #[inline] fn count(self) -> usize
                        {
                            self.it.count(|s, e| 
                            {
                                unsafe { self.searcher.count_raw(s, e) }
                            })
                        }

                        #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                    }

                    impl<'a, 'h> DoubleEndedIterator for OneIter<'a, 'h> 
                    {
                        #[inline] fn next_back(&mut self) -> Option<usize> 
                        {
                            unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                        }
                    }
                    /// Finds all occurrences of two bytes in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Two 
                    {
                        s1: u8,
                        s2: u8,
                        v1: usize,
                        v2: usize,
                    }

                    impl Two 
                    {
                        /// Create a new searcher that finds occurrences of the two needle bytes given.
                        #[inline] pub fn new(needle1: u8, needle2: u8) -> Two 
                        {
                            Two 
                            {
                                s1: needle1,
                                s2: needle2,
                                v1: splat(needle1),
                                v2: splat(needle2),
                            }
                        }
                        /// Return the first occurrence of one of the needle bytes in the given haystack.
                        #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize> 
                        {
                            unsafe
                            {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.find_raw(s, e)
                                })
                            }
                        }
                        /// Return the last occurrence of one of the needle bytes in the given haystack.
                        #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize> 
                        {
                            unsafe {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.rfind_raw(s, e)
                                })
                            }
                        }
                        /// Like `find`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let chunk = start.cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let mut cur =
                                start.add(USIZE_BYTES - (start.as_usize() & USIZE_ALIGN));
                            debug_assert!(cur > start);
                            debug_assert!(end.sub(USIZE_BYTES) >= start);
                            while cur <= end.sub(USIZE_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let chunk = cur.cast::<usize>().read();
                                if self.has_needle(chunk) {
                                    break;
                                }
                                cur = cur.add(USIZE_BYTES);
                            }
                            generic::fwd_byte_by_byte(cur, end, confirm)
                        }
                        /// Like `rfind`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn rfind_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8> 
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let chunk = end.sub(USIZE_BYTES).cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let mut cur = end.sub(end.as_usize() & USIZE_ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            while cur >= start.add(USIZE_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let chunk = cur.sub(USIZE_BYTES).cast::<usize>().read();
                                if self.has_needle(chunk) {
                                    break;
                                }
                                cur = cur.sub(USIZE_BYTES);
                            }
                            generic::rev_byte_by_byte(start, cur, confirm)
                        }
                        /// Returns an iterator over all occurrences of one of the needle bytes in the given haystack.
                        pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> TwoIter<'a, 'h> { TwoIter { searcher: self, it: generic::Iter::new(haystack) } }

                        #[inline( always )] fn has_needle(&self, chunk: usize) -> bool { has_zero_byte(self.v1 ^ chunk) || has_zero_byte(self.v2 ^ chunk) }

                        #[inline( always )] fn confirm(&self, haystack_byte: u8) -> bool { self.s1 == haystack_byte || self.s2 == haystack_byte }
                    }
                    /// An iterator over all occurrences of two possible bytes in a haystack.
                    #[derive(Clone, Debug)]
                    pub struct TwoIter<'a, 'h>
                    {
                        /// The underlying memchr searcher.
                        searcher: &'a Two,
                        /// Generic iterator implementation.
                        it: generic::Iter<'h>,
                    }

                    impl<'a, 'h> Iterator for TwoIter<'a, 'h>
                    {
                        type Item = usize;
                        #[inline] fn next(&mut self) -> Option<usize>
                        {
                            unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                        }

                        #[inline] fn size_hint(&self) -> (usize, Option<usize>) {
                            self.it.size_hint()
                        }
                    }

                    impl<'a, 'h> DoubleEndedIterator for TwoIter<'a, 'h>
                    {
                        #[inline] fn next_back(&mut self) -> Option<usize> 
                        {
                            unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                        }
                    }
                    /// Finds all occurrences of three bytes in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Three 
                    {
                        s1: u8,
                        s2: u8,
                        s3: u8,
                        v1: usize,
                        v2: usize,
                        v3: usize,
                    }

                    impl Three
                    {
                        /// Create a new searcher that finds occurrences of the three needle bytes given.
                        #[inline] pub fn new(needle1: u8, needle2: u8, needle3: u8) -> Three
                        {
                            Three
                            {
                                s1: needle1,
                                s2: needle2,
                                s3: needle3,
                                v1: splat(needle1),
                                v2: splat(needle2),
                                v3: splat(needle3),
                            }
                        }
                        /// Return the first occurrence of one of the needle bytes in the given haystack.
                        #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize> 
                        {
                            unsafe
                            {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.find_raw(s, e)
                                })
                            }
                        }
                        /// Return the last occurrence of one of the needle bytes in the given haystack.
                        #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize> 
                        {
                            unsafe 
                            {
                                generic::search_slice_with_raw(haystack, |s, e| {
                                    self.rfind_raw(s, e)
                                })
                            }
                        }
                        /// Like `find`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let chunk = start.cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::fwd_byte_by_byte(start, end, confirm);
                            }
                            
                            let mut cur =
                                start.add(USIZE_BYTES - (start.as_usize() & USIZE_ALIGN));
                            debug_assert!(cur > start);
                            debug_assert!(end.sub(USIZE_BYTES) >= start);
                            while cur <= end.sub(USIZE_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let chunk = cur.cast::<usize>().read();
                                if self.has_needle(chunk) {
                                    break;
                                }
                                cur = cur.add(USIZE_BYTES);
                            }
                            generic::fwd_byte_by_byte(cur, end, confirm)
                        }
                        /// Like `rfind`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn rfind_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            if start >= end { return None; }
                            let confirm = |b| self.confirm(b);
                            let len = end.distance(start);
                            if len < USIZE_BYTES {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let chunk = end.sub(USIZE_BYTES).cast::<usize>().read_unaligned();
                            if self.has_needle(chunk) {
                                return generic::rev_byte_by_byte(start, end, confirm);
                            }

                            let mut cur = end.sub(end.as_usize() & USIZE_ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            while cur >= start.add(USIZE_BYTES) {
                                debug_assert_eq!(0, cur.as_usize() % USIZE_BYTES);

                                let chunk = cur.sub(USIZE_BYTES).cast::<usize>().read();
                                if self.has_needle(chunk) {
                                    break;
                                }
                                cur = cur.sub(USIZE_BYTES);
                            }
                            generic::rev_byte_by_byte(start, cur, confirm)
                        }
                        /// Returns an iterator over all occurrences of one of the needle bytes in the given haystack.
                        pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> ThreeIter<'a, 'h>
                        {
                            ThreeIter { searcher: self, it: generic::Iter::new(haystack) }
                        }

                        #[inline( always )] fn has_needle(&self, chunk: usize) -> bool
                        {
                            has_zero_byte(self.v1 ^ chunk)
                                || has_zero_byte(self.v2 ^ chunk)
                                || has_zero_byte(self.v3 ^ chunk)
                        }

                        #[inline( always )] fn confirm(&self, haystack_byte: u8) -> bool
                        {
                            self.s1 == haystack_byte
                                || self.s2 == haystack_byte
                                || self.s3 == haystack_byte
                        }
                    }
                    /// An iterator over all occurrences of three possible bytes in a haystack.
                    #[derive(Clone, Debug)]
                    pub struct ThreeIter<'a, 'h>
                    {
                        /// The underlying memchr searcher.
                        searcher: &'a Three,
                        /// Generic iterator implementation.
                        it: generic::Iter<'h>,
                    }

                    impl<'a, 'h> Iterator for ThreeIter<'a, 'h>
                    {
                        type Item = usize;
                        #[inline] fn next(&mut self) -> Option<usize>
                        {
                            unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                        }

                        #[inline] fn size_hint(&self) -> (usize, Option<usize>) {
                            self.it.size_hint()
                        }
                    }

                    impl<'a, 'h> DoubleEndedIterator for ThreeIter<'a, 'h>
                    {
                        #[inline] fn next_back(&mut self) -> Option<usize> 
                        {
                            unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                        }
                    }
                    /// Return `true` if `x` contains any zero byte.
                    #[inline( always )] fn has_zero_byte(x: usize) -> bool 
                    {
                        const LO: usize = splat(0x01);
                        const HI: usize = splat(0x80);

                        (x.wrapping_sub(LO) & !x & HI) != 0
                    }

                    #[inline( always )] fn count_bytes(chunk: usize) -> usize
                    {
                        const LO: usize = splat(0x01);
                        const HI: usize = splat(0x80);

                        (chunk.wrapping_sub(LO) & !chunk & HI).count_ones() as usize
                    }
                    /// Repeat the given byte into a word size number.
                    #[inline(always)] const fn splat(b: u8) -> usize { (b as usize) * (usize::MAX / 255) }
                }

                pub mod packedpair
                {
                    /*!
                    Provides an architecture independent implementation of the "packed pair" algorithm. */
                    use ::
                    {
                        *,
                    };
                    /*
                    */
                    use ::mem::chr::memchr;

                    pub mod default_rank
                    {
                        use ::
                        {
                            *,
                        };
                        /*
                        */
                        pub const RANK: [u8; 256] = 
                        [
                            55,  // '\x00'
                            52,  // '\x01'
                            51,  // '\x02'
                            50,  // '\x03'
                            49,  // '\x04'
                            48,  // '\x05'
                            47,  // '\x06'
                            46,  // '\x07'
                            45,  // '\x08'
                            103, // '\t'
                            242, // '\n'
                            66,  // '\x0b'
                            67,  // '\x0c'
                            229, // '\r'
                            44,  // '\x0e'
                            43,  // '\x0f'
                            42,  // '\x10'
                            41,  // '\x11'
                            40,  // '\x12'
                            39,  // '\x13'
                            38,  // '\x14'
                            37,  // '\x15'
                            36,  // '\x16'
                            35,  // '\x17'
                            34,  // '\x18'
                            33,  // '\x19'
                            56,  // '\x1a'
                            32,  // '\x1b'
                            31,  // '\x1c'
                            30,  // '\x1d'
                            29,  // '\x1e'
                            28,  // '\x1f'
                            255, // ' '
                            148, // '!'
                            164, // '"'
                            149, // '#'
                            136, // '$'
                            160, // '%'
                            155, // '&'
                            173, // "'"
                            221, // '('
                            222, // ')'
                            134, // '*'
                            122, // '+'
                            232, // ','
                            202, // '-'
                            215, // '.'
                            224, // '/'
                            208, // '0'
                            220, // '1'
                            204, // '2'
                            187, // '3'
                            183, // '4'
                            179, // '5'
                            177, // '6'
                            168, // '7'
                            178, // '8'
                            200, // '9'
                            226, // ':'
                            195, // ';'
                            154, // '<'
                            184, // '='
                            174, // '>'
                            126, // '?'
                            120, // '@'
                            191, // 'A'
                            157, // 'B'
                            194, // 'C'
                            170, // 'D'
                            189, // 'E'
                            162, // 'F'
                            161, // 'G'
                            150, // 'H'
                            193, // 'I'
                            142, // 'J'
                            137, // 'K'
                            171, // 'L'
                            176, // 'M'
                            185, // 'N'
                            167, // 'O'
                            186, // 'P'
                            112, // 'Q'
                            175, // 'R'
                            192, // 'S'
                            188, // 'T'
                            156, // 'U'
                            140, // 'V'
                            143, // 'W'
                            123, // 'X'
                            133, // 'Y'
                            128, // 'Z'
                            147, // '['
                            138, // '\\'
                            146, // ']'
                            114, // '^'
                            223, // '_'
                            151, // '`'
                            249, // 'a'
                            216, // 'b'
                            238, // 'c'
                            236, // 'd'
                            253, // 'e'
                            227, // 'f'
                            218, // 'g'
                            230, // 'h'
                            247, // 'i'
                            135, // 'j'
                            180, // 'k'
                            241, // 'l'
                            233, // 'm'
                            246, // 'n'
                            244, // 'o'
                            231, // 'p'
                            139, // 'q'
                            245, // 'r'
                            243, // 's'
                            251, // 't'
                            235, // 'u'
                            201, // 'v'
                            196, // 'w'
                            240, // 'x'
                            214, // 'y'
                            152, // 'z'
                            182, // '{'
                            205, // '|'
                            181, // '}'
                            127, // '~'
                            27,  // '\x7f'
                            212, // '\x80'
                            211, // '\x81'
                            210, // '\x82'
                            213, // '\x83'
                            228, // '\x84'
                            197, // '\x85'
                            169, // '\x86'
                            159, // '\x87'
                            131, // '\x88'
                            172, // '\x89'
                            105, // '\x8a'
                            80,  // '\x8b'
                            98,  // '\x8c'
                            96,  // '\x8d'
                            97,  // '\x8e'
                            81,  // '\x8f'
                            207, // '\x90'
                            145, // '\x91'
                            116, // '\x92'
                            115, // '\x93'
                            144, // '\x94'
                            130, // '\x95'
                            153, // '\x96'
                            121, // '\x97'
                            107, // '\x98'
                            132, // '\x99'
                            109, // '\x9a'
                            110, // '\x9b'
                            124, // '\x9c'
                            111, // '\x9d'
                            82,  // '\x9e'
                            108, // '\x9f'
                            118, // '\xa0'
                            141, // '¡'
                            113, // '¢'
                            129, // '£'
                            119, // '¤'
                            125, // '¥'
                            165, // '¦'
                            117, // '§'
                            92,  // '¨'
                            106, // '©'
                            83,  // 'ª'
                            72,  // '«'
                            99,  // '¬'
                            93,  // '\xad'
                            65,  // '®'
                            79,  // '¯'
                            166, // '°'
                            237, // '±'
                            163, // '²'
                            199, // '³'
                            190, // '´'
                            225, // 'µ'
                            209, // '¶'
                            203, // '·'
                            198, // '¸'
                            217, // '¹'
                            219, // 'º'
                            206, // '»'
                            234, // '¼'
                            248, // '½'
                            158, // '¾'
                            239, // '¿'
                            255, // 'À'
                            255, // 'Á'
                            255, // 'Â'
                            255, // 'Ã'
                            255, // 'Ä'
                            255, // 'Å'
                            255, // 'Æ'
                            255, // 'Ç'
                            255, // 'È'
                            255, // 'É'
                            255, // 'Ê'
                            255, // 'Ë'
                            255, // 'Ì'
                            255, // 'Í'
                            255, // 'Î'
                            255, // 'Ï'
                            255, // 'Ð'
                            255, // 'Ñ'
                            255, // 'Ò'
                            255, // 'Ó'
                            255, // 'Ô'
                            255, // 'Õ'
                            255, // 'Ö'
                            255, // '×'
                            255, // 'Ø'
                            255, // 'Ù'
                            255, // 'Ú'
                            255, // 'Û'
                            255, // 'Ü'
                            255, // 'Ý'
                            255, // 'Þ'
                            255, // 'ß'
                            255, // 'à'
                            255, // 'á'
                            255, // 'â'
                            255, // 'ã'
                            255, // 'ä'
                            255, // 'å'
                            255, // 'æ'
                            255, // 'ç'
                            255, // 'è'
                            255, // 'é'
                            255, // 'ê'
                            255, // 'ë'
                            255, // 'ì'
                            255, // 'í'
                            255, // 'î'
                            255, // 'ï'
                            255, // 'ð'
                            255, // 'ñ'
                            255, // 'ò'
                            255, // 'ó'
                            255, // 'ô'
                            255, // 'õ'
                            255, // 'ö'
                            255, // '÷'
                            255, // 'ø'
                            255, // 'ù'
                            255, // 'ú'
                            255, // 'û'
                            255, // 'ü'
                            255, // 'ý'
                            255, // 'þ'
                            255, // 'ÿ'
                        ];

                    }
                    /// An architecture independent "packed pair" finder.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Finder
                    {
                        pair: Pair,
                        byte1: u8,
                        byte2: u8,
                    }

                    impl Finder
                    {
                        /// Create a new prefilter that reports possible locations where the given needle matches.
                        #[inline] pub fn new(needle: &[u8]) -> Option<Finder> 
                        { Finder::with_pair(needle, Pair::new(needle)?) }
                        /// Create a new prefilter using the pair given.
                        #[inline] pub fn with_pair(needle: &[u8], pair: Pair) -> Option<Finder>
                        {
                            let byte1 = needle[usize::from(pair.index1())];
                            let byte2 = needle[usize::from(pair.index2())];
                            
                            Some(Finder { pair, byte1, byte2 })
                        }
                        /// Run this finder on the given haystack as a prefilter.
                        #[inline] pub fn find_prefilter(&self, haystack: &[u8]) -> Option<usize>
                        {
                            let mut i = 0;
                            let index1 = usize::from(self.pair.index1());
                            let index2 = usize::from(self.pair.index2());
                            loop
                            {
                                i += memchr(self.byte1, &haystack[i..])?;
                                let found = i;
                                i += 1;
                                
                                let aligned1 = match found.checked_sub(index1)
                                {
                                    None => continue,
                                    Some(aligned1) => aligned1,
                                };
                                
                                let aligned2 = match aligned1.checked_add(index2)
                                {
                                    None => continue,
                                    Some(aligned_index2) => aligned_index2,
                                };
                                
                                if haystack.get(aligned2).map_or(true, |&b| b != self.byte2) { continue; }
                                
                                return Some(aligned1);
                            }
                        }
                        /// Returns the pair of offsets (into the needle) used to check as a predicate 
                        /// before confirming whether a needle exists at a particular position.
                        #[inline] pub fn pair(&self) -> &Pair { &self.pair }
                    }
                    /// A pair of byte offsets into a needle to use as a predicate.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Pair
                    {
                        index1: u8,
                        index2: u8,
                    }

                    impl Pair
                    {
                        /// Create a new pair of offsets from the given needle.
                        #[inline] pub fn new(needle: &[u8]) -> Option<Pair>
                        { Pair::with_ranker(needle, DefaultFrequencyRank) }
                        /// Create a new pair of offsets from the given needle and ranker.
                        #[inline] pub fn with_ranker<R: HeuristicFrequencyRank>
                        (
                            needle: &[u8],
                            ranker: R,
                        ) -> Option<Pair> 
                        {
                            if needle.len() <= 1 { return None; }
                            
                            let (mut rare1, mut index1) = (needle[0], 0);
                            let (mut rare2, mut index2) = (needle[1], 1);
                            
                            if ranker.rank(rare2) < ranker.rank(rare1)
                            {
                                ::mem::swap(&mut rare1, &mut rare2);
                                ::mem::swap(&mut index1, &mut index2);
                            }
                            
                            let max = usize::from(::u8::MAX);
                            
                            for (i, &b) in needle.iter().enumerate().take(max).skip(2)
                            {
                                if ranker.rank(b) < ranker.rank(rare1)
                                {
                                    rare2 = rare1;
                                    index2 = index1;
                                    rare1 = b;
                                    index1 = u8::try_from(i).unwrap();
                                }
                                else if b != rare1 && ranker.rank(b) < ranker.rank(rare2)
                                {
                                    rare2 = b;
                                    index2 = u8::try_from(i).unwrap();
                                }
                            }
                            
                            assert_ne!(index1, index2);
                            Some(Pair { index1, index2 })
                        }
                        /// Create a new pair using the offsets given for the needle given.
                        #[inline] pub fn with_indices
                        (
                            needle: &[u8],
                            index1: u8,
                            index2: u8,
                        ) -> Option<Pair>
                        {
                            if index1 == index2 { return None; }
                            
                            if usize::from(index1) >= needle.len() { return None; }
                            
                            if usize::from(index2) >= needle.len() { return None; }
                            
                            Some(Pair { index1, index2 })
                        }
                        /// Returns the first offset of the pair.
                        #[inline] pub fn index1(&self) -> u8 { self.index1 }
                        /// Returns the second offset of the pair.
                        #[inline] pub fn index2(&self) -> u8 { self.index2 }
                    }
                    /// This trait allows the user to customize the heuristic used to determine the relative frequency 
                    /// of a given byte in the dataset being searched.
                    pub trait HeuristicFrequencyRank
                    {
                        /// Return the heuristic frequency rank of the given byte.
                        fn rank(&self, byte: u8) -> u8;
                    }
                    /// The default byte frequency heuristic that is good for most haystacks.
                    pub struct DefaultFrequencyRank;

                    impl HeuristicFrequencyRank for DefaultFrequencyRank
                    {
                        fn rank(&self, byte: u8) -> u8
                        {
                            self::default_rank::RANK[usize::from(byte)]
                        }
                    }
                    /// Permits passing any implementation of `HeuristicFrequencyRank` as a borrowed version of itself.
                    impl<'a, R> HeuristicFrequencyRank for &'a R where
                    R: HeuristicFrequencyRank
                    {
                        fn rank(&self, byte: u8) -> u8 { (**self).rank(byte) }
                    }
                }

                pub mod rabinkarp
                {
                    /*!
                    An implementation of the [Rabin-Karp substring search algorithm][rabinkarp]. */
                    use ::
                    {
                        *,
                    };
                    /*
                    */
                    use ::mem::chr::ext::Pointer;
                    /// A forward substring searcher using the Rabin-Karp algorithm.
                    #[derive(Clone, Debug)]
                    pub struct Finder
                    {
                        /// The actual hash.
                        hash: Hash,
                        /// The factor needed to multiply a byte by in order to subtract it from the hash.
                        hash_2pow: u32,
                    }

                    impl Finder
                    {
                        /// Create a new Rabin-Karp forward searcher for the given `needle`.
                        #[inline] pub fn new(needle: &[u8]) -> Finder
                        {
                            let mut s = Finder { hash: Hash::new(), hash_2pow: 1 };
                            let first_byte = match needle.get(0) {
                                None => return s,
                                Some(&first_byte) => first_byte,
                            };
                            s.hash.add(first_byte);
                            for b in needle.iter().copied().skip(1) {
                                s.hash.add(b);
                                s.hash_2pow = s.hash_2pow.wrapping_shl(1);
                            }
                            s
                        }
                        /// Return the first occurrence of the `needle` in the `haystack` given.
                        #[inline] pub fn find(&self, haystack: &[u8], needle: &[u8]) -> Option<usize>
                        {
                            unsafe
                            {
                                let hstart = haystack.as_ptr();
                                let hend = hstart.add(haystack.len());
                                let nstart = needle.as_ptr();
                                let nend = nstart.add(needle.len());
                                let found = self.find_raw(hstart, hend, nstart, nend)?;
                                Some(found.distance(hstart))
                            }
                        }
                        /// Like `find`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn find_raw
                        (
                            &self,
                            hstart: *const u8,
                            hend: *const u8,
                            nstart: *const u8,
                            nend: *const u8,
                        ) -> Option<*const u8> 
                        {
                            let hlen = hend.distance(hstart);
                            let nlen = nend.distance(nstart);
                            if nlen > hlen { return None; }
                            
                            let mut cur = hstart;
                            let end = hend.sub(nlen);
                            let mut hash = Hash::forward(cur, cur.add(nlen));
                            loop
                            {
                                if self.hash == hash && is_equal_raw(cur, nstart, nlen) { return Some(cur); }
                                
                                if cur >= end { return None; }
                                
                                hash.roll(self, cur.read(), cur.add(nlen).read());
                                cur = cur.add(1);
                            }
                        }
                    }
                    /// A reverse substring searcher using the Rabin-Karp algorithm.
                    #[derive(Clone, Debug)]
                    pub struct FinderRev(Finder);

                    impl FinderRev
                    {
                        /// Create a new Rabin-Karp reverse searcher for the given `needle`.
                        #[inline] pub fn new(needle: &[u8]) -> FinderRev
                        {
                            let mut s = FinderRev(Finder { hash: Hash::new(), hash_2pow: 1 });
                            let last_byte = match needle.last() {
                                None => return s,
                                Some(&last_byte) => last_byte,
                            };
                            s.0.hash.add(last_byte);
                            for b in needle.iter().rev().copied().skip(1) {
                                s.0.hash.add(b);
                                s.0.hash_2pow = s.0.hash_2pow.wrapping_shl(1);
                            }
                            s
                        }
                        /// Return the last occurrence of the `needle` in the `haystack` given.
                        #[inline] pub fn rfind(&self, haystack: &[u8], needle: &[u8]) -> Option<usize>
                        {
                            unsafe
                            {
                                let hstart = haystack.as_ptr();
                                let hend = hstart.add(haystack.len());
                                let nstart = needle.as_ptr();
                                let nend = nstart.add(needle.len());
                                let found = self.rfind_raw(hstart, hend, nstart, nend)?;
                                Some(found.distance(hstart))
                            }
                        }
                        /// Like `rfind`, but accepts and returns raw pointers.
                        #[inline] pub unsafe fn rfind_raw
                        (
                            &self,
                            hstart: *const u8,
                            hend: *const u8,
                            nstart: *const u8,
                            nend: *const u8,
                        ) -> Option<*const u8>
                        {
                            let hlen = hend.distance(hstart);
                            let nlen = nend.distance(nstart);
                            if nlen > hlen { return None; }
                            let mut cur = hend.sub(nlen);
                            let start = hstart;
                            let mut hash = Hash::reverse(cur, cur.add(nlen));
                            loop
                            {
                                if self.0.hash == hash && is_equal_raw(cur, nstart, nlen) { return Some(cur); }
                                
                                if cur <= start { return None; }
                                
                                cur = cur.sub(1);
                                hash.roll(&self.0, cur.add(nlen).read(), cur.read());
                            }
                        }
                    }
                    /// Whether RK is believed to be very fast for the given needle/haystack.
                    #[inline] pub fn is_fast(haystack: &[u8], _needle: &[u8]) -> bool { haystack.len() < 16 }
                    /// A Rabin-Karp hash.
                    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
                    struct Hash(u32);

                    impl Hash
                    {
                        /// Create a new hash that represents the empty string.
                        #[inline( always )] fn new() -> Hash { Hash(0) }
                        /// Create a new hash from the bytes given for use in forward searches.
                        #[inline( always )] unsafe fn forward(mut start: *const u8, end: *const u8) -> Hash
                        {
                            let mut hash = Hash::new();
                            while start < end
                            {
                                hash.add(start.read());
                                start = start.add(1);
                            }
                            
                            hash
                        }
                        /// Create a new hash from the bytes given for use in reverse searches.
                        #[inline( always )] unsafe fn reverse(start: *const u8, mut end: *const u8) -> Hash
                        {
                            let mut hash = Hash::new();
                            
                            while start < end
                            {
                                end = end.sub(1);
                                hash.add(end.read());
                            }
                            
                            hash
                        }
                        /// Add 'new' and remove 'old' from this hash.
                        #[inline( always )] fn roll(&mut self, finder: &Finder, old: u8, new: u8)
                        {
                            self.del(finder, old);
                            self.add(new);
                        }
                        /// Add a byte to this hash.
                        #[inline( always )] fn add(&mut self, byte: u8)
                        { self.0 = self.0.wrapping_shl(1).wrapping_add(u32::from(byte)); }
                        /// Remove a byte from this hash.
                        #[inline( always )] fn del(&mut self, finder: &Finder, byte: u8)
                        {
                            let factor = finder.hash_2pow;
                            self.0 = self.0.wrapping_sub(u32::from(byte).wrapping_mul(factor));
                        }
                    }
                    /// Returns true when `x[i] == y[i]` for all `0 <= i < n`.
                    #[cold] #[inline( never )] unsafe fn is_equal_raw(x: *const u8, y: *const u8, n: usize) -> bool
                    { ::mem::chr::arch::all::is_equal_raw(x, y, n) }
                }

                pub mod shiftor
                {
                    /*!
                    An implementation of the [Shift-Or substring search algorithm][shiftor]. */
                    use ::
                    {
                        boxed::{ Box },
                        *,
                    };
                    /*
                    */
                    /// The type of our mask.
                    type Mask = u16;
                    /// A forward substring searcher using the Shift-Or algorithm.
                    #[derive(Debug)]
                    pub struct Finder
                    {
                        masks: Box<[Mask; 256]>,
                        needle_len: usize,
                    }

                    impl Finder
                    {
                        const MAX_NEEDLE_LEN: usize = (Mask::BITS - 1) as usize;
                        /// Create a new Shift-Or forward searcher for the given `needle`.
                        #[inline] pub fn new(needle: &[u8]) -> Option<Finder>
                        {
                            let needle_len = needle.len();
                            
                            if needle_len > Finder::MAX_NEEDLE_LEN { return None; }
                            
                            let mut searcher = Finder { masks: Box::from([!0; 256]), needle_len };
                            
                            for (i, &byte) in needle.iter().enumerate()
                            {
                                searcher.masks[usize::from(byte)] &= !(1 << i);
                            }
                            
                            Some(searcher)
                        }
                        /// Return the first occurrence of the needle given to `Finder::new` in the `haystack` given.
                        #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                        {
                            if self.needle_len == 0 { return Some(0); }
                            
                            let mut result = !1;
                            
                            for (i, &byte) in haystack.iter().enumerate()
                            {
                                result |= self.masks[usize::from(byte)];
                                result <<= 1;
                                if result & (1 << self.needle_len) == 0 {
                                    return Some(i + 1 - self.needle_len);
                                }
                            }
                            
                            None
                        }
                    }

                }

                pub mod twoway
                {
                    use ::
                    {
                        mem::chr::
                        {
                            arch::all::{ is_prefix, is_suffix },
                            memmem::Pre,
                        },
                        *,
                    };
                    /*
                    */
                    /// A forward substring searcher that uses the Two-Way algorithm.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Finder(TwoWay);
                    /// A reverse substring searcher that uses the Two-Way algorithm.
                    #[derive(Clone, Copy, Debug)]
                    pub struct FinderRev(TwoWay);
                    /// An implementation of the TwoWay substring search algorithm.
                    #[derive(Clone, Copy, Debug)]
                    struct TwoWay
                    {
                        /// A small bitset used as a quick prefilter.
                        byteset: ApproximateByteSet,
                        /// A critical position in needle.
                        critical_pos: usize,
                        /// The amount we shift by in the Two-Way search algorithm.
                        shift: Shift,
                    }

                    impl Finder
                    {
                        /// Create a searcher that finds occurrences of the given `needle`.
                        #[inline] pub fn new(needle: &[u8]) -> Finder
                        {
                            let byteset = ApproximateByteSet::new(needle);
                            let min_suffix = Suffix::forward(needle, SuffixKind::Minimal);
                            let max_suffix = Suffix::forward(needle, SuffixKind::Maximal);
                            
                            let (period_lower_bound, critical_pos) =
                            if min_suffix.pos > max_suffix.pos { (min_suffix.period, min_suffix.pos) }
                            else { (max_suffix.period, max_suffix.pos) };
                            
                            let shift = Shift::forward(needle, period_lower_bound, critical_pos);
                            Finder(TwoWay { byteset, critical_pos, shift })
                        }
                        /// Returns the first occurrence of `needle` in the given `haystack`,
                        /// or `None` if no such occurrence could be found.
                        #[inline] pub fn find(&self, haystack: &[u8], needle: &[u8]) -> Option<usize>
                        { self.find_with_prefilter(None, haystack, needle) }
                        /// This is like [`Finder::find`], but it accepts a prefilter for accelerating searches.
                        #[inline( always )] pub fn find_with_prefilter
                        (
                            &self,
                            pre: Option<Pre<'_>>,
                            haystack: &[u8],
                            needle: &[u8],
                        ) -> Option<usize>
                        {
                            match self.0.shift
                            {
                                Shift::Small { period } => { self.find_small_imp(pre, haystack, needle, period) }			
                                Shift::Large { shift } => { self.find_large_imp(pre, haystack, needle, shift) }
                            }
                        }
                        
                        #[inline( always) ] fn find_small_imp
                        (
                            &self,
                            mut pre: Option<Pre<'_>>,
                            haystack: &[u8],
                            needle: &[u8],
                            period: usize,
                        ) -> Option<usize>
                        {
                            let mut pos = 0;
                            let mut shift = 0;
                            let last_byte_pos = match needle.len().checked_sub(1)
                            {
                                None => return Some(pos),
                                Some(last_byte) => last_byte,
                            };
                            
                            while pos + needle.len() <= haystack.len()
                            {
                                let mut i = cmp::max(self.0.critical_pos, shift);
                                
                                if let Some(pre) = pre.as_mut()
                                {
                                    if pre.is_effective()
                                    {
                                        pos += pre.find(&haystack[pos..])?;
                                        shift = 0;
                                        i = self.0.critical_pos;
                                        
                                        if pos + needle.len() > haystack.len() { return None; }
                                    }
                                }
                                
                                if !self.0.byteset.contains(haystack[pos + last_byte_pos])
                                {
                                    pos += needle.len();
                                    shift = 0;
                                    continue;
                                }
                                
                                while i < needle.len() && needle[i] == haystack[pos + i]
                                {
                                    i += 1;
                                }
                                
                                if i < needle.len()
                                {
                                    pos += i - self.0.critical_pos + 1;
                                    shift = 0;
                                }
                                
                                else
                                {
                                    let mut j = self.0.critical_pos;
                                    
                                    while j > shift && needle[j] == haystack[pos + j]
                                    {
                                        j -= 1;
                                    }
                                    
                                    if j <= shift && needle[shift] == haystack[pos + shift] { return Some(pos); }
                                    
                                    pos += period;
                                    shift = needle.len() - period;
                                }
                            }
                            
                            None
                        }

                        #[inline( always) ] fn find_large_imp
                        (
                            &self,
                            mut pre: Option<Pre<'_>>,
                            haystack: &[u8],
                            needle: &[u8],
                            shift: usize,
                        ) -> Option<usize>
                        {
                            let mut pos = 0;
                            
                            let last_byte_pos = match needle.len().checked_sub(1)
                            {
                                None => return Some(pos),
                                Some(last_byte) => last_byte,
                            };
                            
                            'outer: while pos + needle.len() <= haystack.len()
                            {
                                if let Some(pre) = pre.as_mut()
                                {
                                    if pre.is_effective()
                                    {
                                        pos += pre.find(&haystack[pos..])?;
                                        
                                        if pos + needle.len() > haystack.len() { return None; }
                                    }
                                }

                                if !self.0.byteset.contains(haystack[pos + last_byte_pos])
                                {
                                    pos += needle.len();
                                    continue;
                                }
                                
                                let mut i = self.0.critical_pos;
                                
                                while i < needle.len() && needle[i] == haystack[pos + i]
                                {
                                    i += 1;
                                }
                                
                                if i < needle.len() { pos += i - self.0.critical_pos + 1; }
                                else
                                {
                                    for j in (0..self.0.critical_pos).rev()
                                    {
                                        if needle[j] != haystack[pos + j]
                                        {
                                            pos += shift;
                                            continue 'outer;
                                        }
                                    }
                                    
                                    return Some(pos);
                                }
                            }
                            
                            None
                        }
                    }

                    impl FinderRev
                    {
                        /// Create a searcher that finds occurrences of the given `needle`.
                        #[inline] pub fn new(needle: &[u8]) -> FinderRev
                        {
                            let byteset = ApproximateByteSet::new(needle);
                            let min_suffix = Suffix::reverse(needle, SuffixKind::Minimal);
                            let max_suffix = Suffix::reverse(needle, SuffixKind::Maximal);

                            let (period_lower_bound, critical_pos) =
                            if min_suffix.pos < max_suffix.pos { (min_suffix.period, min_suffix.pos) }
                            else { (max_suffix.period, max_suffix.pos) };
                                
                            let shift = Shift::reverse(needle, period_lower_bound, critical_pos);
                            FinderRev(TwoWay { byteset, critical_pos, shift })
                        }
                        /// Returns the last occurrence of `needle` in the given `haystack`, 
                        /// or `None` if no such occurrence could be found.
                        #[inline] pub fn rfind(&self, haystack: &[u8], needle: &[u8]) -> Option<usize>
                        {
                            match self.0.shift
                            {
                                Shift::Small { period } => { self.rfind_small_imp(haystack, needle, period) }
                                Shift::Large { shift } => { self.rfind_large_imp(haystack, needle, shift) }
                            }
                        }

                        #[inline( always) ] fn rfind_small_imp
                        (
                            &self,
                            haystack: &[u8],
                            needle: &[u8],
                            period: usize,
                        ) -> Option<usize>
                        {
                            let nlen = needle.len();
                            let mut pos = haystack.len();
                            let mut shift = nlen;
                            let first_byte = match needle.get(0)
                            {
                                None => return Some(pos),
                                Some(&first_byte) => first_byte,
                            };
                            
                            while pos >= nlen
                            {
                                if !self.0.byteset.contains(haystack[pos - nlen])
                                {
                                    pos -= nlen;
                                    shift = nlen;
                                    continue;
                                }
                                
                                let mut i = cmp::min(self.0.critical_pos, shift);
                                
                                while i > 0 && needle[i - 1] == haystack[pos - nlen + i - 1]
                                {
                                    i -= 1;
                                }
                                
                                if i > 0 || first_byte != haystack[pos - nlen]
                                {
                                    pos -= self.0.critical_pos - i + 1;
                                    shift = nlen;
                                }
                                
                                else
                                {
                                    let mut j = self.0.critical_pos;
                                    
                                    while j < shift && needle[j] == haystack[pos - nlen + j]
                                    {
                                        j += 1;
                                    }
                                    
                                    if j >= shift { return Some(pos - nlen); }
                                    
                                    pos -= period;
                                    shift = period;
                                }
                            }
                            
                            None
                        }

                        #[inline( always) ] fn rfind_large_imp
                        (
                            &self,
                            haystack: &[u8],
                            needle: &[u8],
                            shift: usize,
                        ) -> Option<usize>
                        {
                            let nlen = needle.len();
                            let mut pos = haystack.len();
                            let first_byte = match needle.get(0)
                            {
                                None => return Some(pos),
                                Some(&first_byte) => first_byte,
                            };
                            
                            while pos >= nlen
                            {
                                if !self.0.byteset.contains(haystack[pos - nlen])
                                {
                                    pos -= nlen;
                                    continue;
                                }
                                
                                let mut i = self.0.critical_pos;
                                
                                while i > 0 && needle[i - 1] == haystack[pos - nlen + i - 1]
                                {
                                    i -= 1;
                                }
                                
                                if i > 0 || first_byte != haystack[pos - nlen] { pos -= self.0.critical_pos - i + 1; }
                                
                                else
                                {
                                    let mut j = self.0.critical_pos;
                                    
                                    while j < nlen && needle[j] == haystack[pos - nlen + j] { j += 1; }
                                    
                                    if j == nlen { return Some(pos - nlen); }
                                    
                                    pos -= shift;
                                }
                            }
                            
                            None
                        }
                    }
                    /// A representation of the amount we're allowed to shift by during Two-Way search.
                    #[derive(Clone, Copy, Debug)]
                    enum Shift
                    {
                        Small { period: usize },
                        Large { shift: usize },
                    }

                    impl Shift
                    {
                        /// Compute the shift for a given needle in the forward direction.
                        fn forward
                        (
                            needle: &[u8],
                            period_lower_bound: usize,
                            critical_pos: usize,
                        ) -> Shift
                        {
                            let large = cmp::max(critical_pos, needle.len() - critical_pos);
                            
                            if critical_pos * 2 >= needle.len() { return Shift::Large { shift: large }; }

                            let (u, v) = needle.split_at(critical_pos);
                            
                            if !is_suffix(&v[..period_lower_bound], u) { return Shift::Large { shift: large }; }
                            
                            Shift::Small { period: period_lower_bound }
                        }
                        /// Compute the shift for a given needle in the reverse direction.
                        fn reverse
                        (
                            needle: &[u8],
                            period_lower_bound: usize,
                            critical_pos: usize,
                        ) -> Shift
                        {
                            let large = cmp::max(critical_pos, needle.len() - critical_pos);
                            
                            if (needle.len() - critical_pos) * 2 >= needle.len() { return Shift::Large { shift: large }; }

                            let (v, u) = needle.split_at(critical_pos);
                            
                            if !is_prefix(&v[v.len() - period_lower_bound..], u) { return Shift::Large { shift: large }; }
                            
                            Shift::Small { period: period_lower_bound }
                        }
                    }
                    /// A suffix extracted from a needle along with its period.
                    #[derive(Debug)]
                    struct Suffix
                    {
                        /// The starting position of this suffix.
                        pos: usize,
                        /// The period of this suffix.
                        period: usize,
                    }

                    impl Suffix
                    {
                        fn forward(needle: &[u8], kind: SuffixKind) -> Suffix
                        {
                            let mut suffix = Suffix { pos: 0, period: 1 };
                            let mut candidate_start = 1;
                            let mut offset = 0;

                            while candidate_start + offset < needle.len()
                            {
                                let current = needle[suffix.pos + offset];
                                let candidate = needle[candidate_start + offset];
                                match kind.cmp(current, candidate)
                                {
                                    SuffixOrdering::Accept =>
                                    {
                                        suffix = Suffix { pos: candidate_start, period: 1 };
                                        candidate_start += 1;
                                        offset = 0;
                                    }
                                    
                                    SuffixOrdering::Skip =>
                                    {
                                        candidate_start += offset + 1;
                                        offset = 0;
                                        suffix.period = candidate_start - suffix.pos;
                                    }
                                    
                                    SuffixOrdering::Push =>
                                    {
                                        if offset + 1 == suffix.period
                                        {
                                            candidate_start += suffix.period;
                                            offset = 0;
                                        }
                                        else { offset += 1; }
                                    }
                                }
                            }
                            
                            suffix
                        }

                        fn reverse(needle: &[u8], kind: SuffixKind) -> Suffix
                        {
                            let mut suffix = Suffix { pos: needle.len(), period: 1 };
                            
                            if needle.len() == 1 { return suffix; }
                            
                            let mut candidate_start = match needle.len().checked_sub(1)
                            {
                                None => return suffix,
                                Some(candidate_start) => candidate_start,
                            };
                            
                            let mut offset = 0;

                            while offset < candidate_start
                            {
                                let current = needle[suffix.pos - offset - 1];
                                let candidate = needle[candidate_start - offset - 1];
                                
                                match kind.cmp(current, candidate) 
                                {
                                    SuffixOrdering::Accept => 
                                    {
                                        suffix = Suffix { pos: candidate_start, period: 1 };
                                        candidate_start -= 1;
                                        offset = 0;
                                    }
                                    
                                    SuffixOrdering::Skip => 
                                    {
                                        candidate_start -= offset + 1;
                                        offset = 0;
                                        suffix.period = suffix.pos - candidate_start;
                                    }
                                    
                                    SuffixOrdering::Push => 
                                    {
                                        
                                        if offset + 1 == suffix.period 
                                        {
                                            candidate_start -= suffix.period;
                                            offset = 0;
                                        }
                                        
                                        else { offset += 1; }
                                    }
                                }
                            }
                            
                            suffix
                        }
                    }
                    /// The kind of suffix to extract.
                    #[derive(Clone, Copy, Debug)]
                    enum SuffixKind
                    {
                        /// Extract the smallest lexicographic suffix from a string.
                        Minimal,
                        /// Extract the largest lexicographic suffix from a string.
                        Maximal,
                    }
                    /// The result of comparing corresponding bytes between two suffixes.
                    #[derive(Clone, Copy, Debug)]
                    enum SuffixOrdering
                    {
                        /// This occurs when the given candidate byte indicates that the candidate suffix is better 
                        ///than the current maximal (or minimal) suffix.
                        Accept,
                        /// This occurs when the given candidate byte excludes the candidate suffix from being better 
                        ///than the current maximal (or minimal) suffix.
                        Skip,
                        /// This occurs when no decision to accept or skip the candidate suffix can be made.
                        Push,
                    }

                    impl SuffixKind
                    {
                        /// Returns true if and only if the given candidate byte indicates that it should replace 
                        /// the current suffix as the maximal (or minimal) suffix.
                        fn cmp(self, current: u8, candidate: u8) -> SuffixOrdering
                        {
                            use self::SuffixOrdering::*;

                            match self
                            {
                                SuffixKind::Minimal if candidate < current => Accept,
                                SuffixKind::Minimal if candidate > current => Skip,
                                SuffixKind::Minimal => Push,
                                SuffixKind::Maximal if candidate > current => Accept,
                                SuffixKind::Maximal if candidate < current => Skip,
                                SuffixKind::Maximal => Push,
                            }
                        }
                    }
                    /// A bitset used to track whether a particular byte exists in a needle or not.
                    #[derive(Clone, Copy, Debug)]
                    struct ApproximateByteSet(u64);

                    impl ApproximateByteSet
                    {
                        /// Create a new set from the given needle.
                        fn new(needle: &[u8]) -> ApproximateByteSet
                        {
                            let mut bits = 0;
                            for &b in needle {
                                bits |= 1 << (b % 64);
                            }
                            ApproximateByteSet(bits)
                        }
                        /// Return true if and only if the given byte might be in this set.
                        #[inline( always) ] fn contains(&self, byte: u8) -> bool { self.0 & (1 << (byte % 64)) != 0 }
                    }
                    /// Returns true if and only if `needle` is a prefix of `haystack`.
                    #[inline( always )] pub fn is_prefix(haystack: &[u8], needle: &[u8]) -> bool 
                    { needle.len() <= haystack.len() && is_equal(&haystack[..needle.len()], needle) }
                    /// Returns true if and only if `needle` is a suffix of `haystack`.
                    #[inline( always )] pub fn is_suffix(haystack: &[u8], needle: &[u8]) -> bool 
                    {
                        needle.len() <= haystack.len() && is_equal(&haystack[haystack.len() - needle.len()..], needle)
                    }
                    /// Compare corresponding bytes in `x` and `y` for equality.
                    #[inline( always )] pub fn is_equal(x: &[u8], y: &[u8]) -> bool 
                    {
                        if x.len() != y.len() { return false; }
                        
                        unsafe { is_equal_raw(x.as_ptr(), y.as_ptr(), x.len()) }
                    }
                    /// Compare `n` bytes at the given pointers for equality.
                    #[inline(always)] pub unsafe fn is_equal_raw
                    (
                        mut x: *const u8,
                        mut y: *const u8,
                        n: usize,
                    ) -> bool 
                    {
                        if n < 4 {
                            return match n {
                                0 => true,
                                1 => x.read() == y.read(),
                                2 => {
                                    x.cast::<u16>().read_unaligned()
                                        == y.cast::<u16>().read_unaligned()
                                }
                                
                                3 => x.cast::<[u8; 3]>().read() == y.cast::<[u8; 3]>().read(),
                                _ => unreachable!(),
                            };
                        }
                        
                        let xend = x.add(n.wrapping_sub(4));
                        let yend = y.add(n.wrapping_sub(4));
                        while x < xend 
                        {
                            let vx = x.cast::<u32>().read_unaligned();
                            let vy = y.cast::<u32>().read_unaligned();
                            if vx != vy {
                                return false;
                            }
                            x = x.add(4);
                            y = y.add(4);
                        }
                        let vx = xend.cast::<u32>().read_unaligned();
                        let vy = yend.cast::<u32>().read_unaligned();
                        vx == vy
                    }
                }
            }

            pub mod generic
            {
                /*!
                This module defines "generic" routines that can be specialized to specific architectures.*/
                use ::
                {
                    *,
                };
                /*
                */
                pub mod memchr
                {
                    /*!
                    Generic crate-internal routines for the `memchr` family of functions. */
                    use ::
                    {
                        mem::chr::
                        {
                            ext::Pointer,
                            vector::{MoveMask, Vector},
                        },
                        *,
                    };
                    /*
                    */
                    /// Finds all occurrences of a single byte in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct One<V>
                    {
                        s1: u8,
                        v1: V,
                    }

                    impl<V: Vector> One<V>
                    {
                        /// The number of bytes we examine per each iteration of our search loop.
                        const LOOP_SIZE: usize = 4 * V::BYTES;
                        /// Create a new searcher that finds occurrences of the byte given.
                        #[inline( always )] pub unsafe fn new(needle: u8) -> One<V>
                        { One { s1: needle, v1: V::splat(needle) } }
                        /// Returns the needle given to `One::new`.
                        #[inline( always )] pub fn needle1(&self) -> u8 { self.s1 }
                        /// Return a pointer to the first occurrence of the needle in the given haystack.
                        #[inline( always )] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::first_offset;
                            let len = end.distance(start);
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );
                            
                            if let Some(cur) = self.search_chunk(start, topos) { return Some(cur); }
                            
                            let mut cur = start.add(V::BYTES - (start.as_usize() & V::ALIGN));
                            debug_assert!(cur > start && end.sub(V::BYTES) >= start);
                            if len >= Self::LOOP_SIZE
                            {
                                while cur <= end.sub(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(1 * V::BYTES));
                                    let c = V::load_aligned(cur.add(2 * V::BYTES));
                                    let d = V::load_aligned(cur.add(3 * V::BYTES));
                                    let eqa = self.v1.cmpeq(a);
                                    let eqb = self.v1.cmpeq(b);
                                    let eqc = self.v1.cmpeq(c);
                                    let eqd = self.v1.cmpeq(d);
                                    let or1 = eqa.or(eqb);
                                    let or2 = eqc.or(eqd);
                                    let or3 = or1.or(or2);
                                    
                                    if or3.movemask_will_have_non_zero() 
                                    {
                                        let mask = eqa.movemask();
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(topos(mask)));
                                        }

                                        let mask = eqb.movemask();
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(1 * V::BYTES).add(topos(mask)));
                                        }

                                        let mask = eqc.movemask();
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(2 * V::BYTES).add(topos(mask)));
                                        }

                                        let mask = eqd.movemask();
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(3 * V::BYTES).add(topos(mask)));
                                    }
                                    
                                    cur = cur.add(Self::LOOP_SIZE);
                                }
                            }
                            
                            while cur <= end.sub(V::BYTES)
                            {
                                debug_assert!(end.distance(cur) >= V::BYTES);
                                if let Some(cur) = self.search_chunk(cur, topos) { return Some(cur); }
                                
                                cur = cur.add(V::BYTES);
                            }
                            
                            if cur < end
                            {
                                debug_assert!(end.distance(cur) < V::BYTES);
                                cur = cur.sub(V::BYTES - end.distance(cur));
                                debug_assert_eq!(end.distance(cur), V::BYTES);
                                return self.search_chunk(cur, topos);
                            }
                            
                            None
                        }
                        /// Return a pointer to the last occurrence of the needle in the given haystack.
                        #[inline( always )] pub unsafe fn rfind_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::last_offset;
                            let len = end.distance(start);
                            
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );

                            if let Some(cur) = self.search_chunk(end.sub(V::BYTES), topos) { return Some(cur); }
                            
                            let mut cur = end.sub(end.as_usize() & V::ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            
                            if len >= Self::LOOP_SIZE
                            {
                                while cur >= start.add(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    cur = cur.sub(Self::LOOP_SIZE);
                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(1 * V::BYTES));
                                    let c = V::load_aligned(cur.add(2 * V::BYTES));
                                    let d = V::load_aligned(cur.add(3 * V::BYTES));
                                    let eqa = self.v1.cmpeq(a);
                                    let eqb = self.v1.cmpeq(b);
                                    let eqc = self.v1.cmpeq(c);
                                    let eqd = self.v1.cmpeq(d);
                                    let or1 = eqa.or(eqb);
                                    let or2 = eqc.or(eqd);
                                    let or3 = or1.or(or2);
                                    
                                    if or3.movemask_will_have_non_zero()
                                    {
                                        let mask = eqd.movemask();
                                        
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(3 * V::BYTES).add(topos(mask)));
                                        }

                                        let mask = eqc.movemask();
                                        
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(2 * V::BYTES).add(topos(mask)));
                                        }

                                        let mask = eqb.movemask();
                                        
                                        if mask.has_non_zero() 
                                        {
                                            return Some(cur.add(1 * V::BYTES).add(topos(mask)));
                                        }

                                        let mask = eqa.movemask();
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(topos(mask)));
                                    }
                                }
                            }
                            
                            while cur >= start.add(V::BYTES)
                            {
                                debug_assert!(cur.distance(start) >= V::BYTES);
                                cur = cur.sub(V::BYTES);
                                
                                if let Some(cur) = self.search_chunk(cur, topos) { return Some(cur); }
                            }
                            
                            if cur > start
                            {
                                debug_assert!(cur.distance(start) < V::BYTES);
                                return self.search_chunk(start, topos);
                            }
                            
                            None
                        }
                        /// Return a count of all matching bytes in the given haystack.
                        #[inline( always )] pub unsafe fn count_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> usize
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let confirm = |b| b == self.needle1();
                            let len = end.distance(start);
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );
                            
                            let mut cur = start.add(V::BYTES - (start.as_usize() & V::ALIGN));
                            
                            let mut count = count_byte_by_byte(start, cur, confirm);
                            debug_assert!(cur > start && end.sub(V::BYTES) >= start);
                            if len >= Self::LOOP_SIZE
                            {
                                while cur <= end.sub(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(1 * V::BYTES));
                                    let c = V::load_aligned(cur.add(2 * V::BYTES));
                                    let d = V::load_aligned(cur.add(3 * V::BYTES));
                                    let eqa = self.v1.cmpeq(a);
                                    let eqb = self.v1.cmpeq(b);
                                    let eqc = self.v1.cmpeq(c);
                                    let eqd = self.v1.cmpeq(d);
                                    count += eqa.movemask().count_ones();
                                    count += eqb.movemask().count_ones();
                                    count += eqc.movemask().count_ones();
                                    count += eqd.movemask().count_ones();
                                    cur = cur.add(Self::LOOP_SIZE);
                                }
                            }
                            
                            while cur <= end.sub(V::BYTES)
                            {
                                debug_assert!(end.distance(cur) >= V::BYTES);
                                let chunk = V::load_unaligned(cur);
                                count += self.v1.cmpeq(chunk).movemask().count_ones();
                                cur = cur.add(V::BYTES);
                            }
                            
                            count += count_byte_by_byte(cur, end, confirm);
                            count
                        }
                        /// Search `V::BYTES` starting at `cur` via an unaligned load.
                        #[inline( always )] unsafe fn search_chunk
                        (
                            &self,
                            cur: *const u8,
                            mask_to_offset: impl Fn(V::Mask) -> usize,
                        ) -> Option<*const u8>
                        {
                            let chunk = V::load_unaligned(cur);
                            let mask = self.v1.cmpeq(chunk).movemask();
                            
                            if mask.has_non_zero() { Some(cur.add(mask_to_offset(mask))) }
                            else { None }
                        }
                    }
                    /// Finds all occurrences of two bytes in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Two<V>
                    {
                        s1: u8,
                        s2: u8,
                        v1: V,
                        v2: V,
                    }

                    impl<V: Vector> Two<V>
                    {
                        /// The number of bytes we examine per each iteration of our search loop.
                        const LOOP_SIZE: usize = 2 * V::BYTES;
                        /// Create a new searcher that finds occurrences of the byte given.
                        #[inline( always )] pub unsafe fn new(needle1: u8, needle2: u8) -> Two<V>
                        {
                            Two
                            {
                                s1: needle1,
                                s2: needle2,
                                v1: V::splat(needle1),
                                v2: V::splat(needle2),
                            }
                        }
                        /// Returns the first needle given to `Two::new`.
                        #[inline( always )] pub fn needle1(&self) -> u8 { self.s1 }
                        /// Returns the second needle given to `Two::new`.
                        #[inline( always )] pub fn needle2(&self) -> u8 { self.s2 }
                        /// Return a pointer to the first occurrence of one of the needles in the given haystack.
                        #[inline( always )] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::first_offset;
                            let len = end.distance(start);
                            debug_assert!(
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );
                            
                            if let Some(cur) = self.search_chunk(start, topos) { return Some(cur); }
                            
                            let mut cur = start.add(V::BYTES - (start.as_usize() & V::ALIGN));
                            debug_assert!(cur > start && end.sub(V::BYTES) >= start);
                            
                            if len >= Self::LOOP_SIZE
                            {
                                while cur <= end.sub(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(V::BYTES));
                                    let eqa1 = self.v1.cmpeq(a);
                                    let eqb1 = self.v1.cmpeq(b);
                                    let eqa2 = self.v2.cmpeq(a);
                                    let eqb2 = self.v2.cmpeq(b);
                                    let or1 = eqa1.or(eqb1);
                                    let or2 = eqa2.or(eqb2);
                                    let or3 = or1.or(or2);
                                    
                                    if or3.movemask_will_have_non_zero()
                                    {
                                        let mask = eqa1.movemask().or(eqa2.movemask());
                                        
                                        if mask.has_non_zero() { return Some(cur.add(topos(mask))); }

                                        let mask = eqb1.movemask().or(eqb2.movemask());
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(V::BYTES).add(topos(mask)));
                                    }
                                    
                                    cur = cur.add(Self::LOOP_SIZE);
                                }
                            }
                            
                            while cur <= end.sub(V::BYTES)
                            {
                                debug_assert!(end.distance(cur) >= V::BYTES);
                                if let Some(cur) = self.search_chunk(cur, topos) { return Some(cur); }
                                
                                cur = cur.add(V::BYTES);
                            }
                            
                            if cur < end
                            {
                                debug_assert!(end.distance(cur) < V::BYTES);
                                cur = cur.sub(V::BYTES - end.distance(cur));
                                debug_assert_eq!(end.distance(cur), V::BYTES);
                                return self.search_chunk(cur, topos);
                            }
                            
                            None
                        }
                        /// Return a pointer to the last occurrence of the needle in the given haystack.
                        #[inline( always )] pub unsafe fn rfind_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::last_offset;
                            let len = end.distance(start);
                            
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );

                            if let Some(cur) = self.search_chunk(end.sub(V::BYTES), topos) { return Some(cur); }
                            
                            let mut cur = end.sub(end.as_usize() & V::ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            if len >= Self::LOOP_SIZE
                            {
                                while cur >= start.add(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    cur = cur.sub(Self::LOOP_SIZE);
                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(V::BYTES));
                                    let eqa1 = self.v1.cmpeq(a);
                                    let eqb1 = self.v1.cmpeq(b);
                                    let eqa2 = self.v2.cmpeq(a);
                                    let eqb2 = self.v2.cmpeq(b);
                                    let or1 = eqa1.or(eqb1);
                                    let or2 = eqa2.or(eqb2);
                                    let or3 = or1.or(or2);
                                    
                                    if or3.movemask_will_have_non_zero()
                                    {
                                        let mask = eqb1.movemask().or(eqb2.movemask());
                                        
                                        if mask.has_non_zero() { return Some(cur.add(V::BYTES).add(topos(mask))); }

                                        let mask = eqa1.movemask().or(eqa2.movemask());
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(topos(mask)));
                                    }
                                }
                            }
                            
                            while cur >= start.add(V::BYTES)
                            {
                                debug_assert!(cur.distance(start) >= V::BYTES);
                                cur = cur.sub(V::BYTES);
                                
                                if let Some(cur) = self.search_chunk(cur, topos) { return Some(cur); }
                            }
                            
                            if cur > start
                            {
                                debug_assert!(cur.distance(start) < V::BYTES);
                                return self.search_chunk(start, topos);
                            }
                            
                            None
                        }
                        /// Search `V::BYTES` starting at `cur` via an unaligned load.
                        #[inline( always )] unsafe fn search_chunk
                        (
                            &self,
                            cur: *const u8,
                            mask_to_offset: impl Fn(V::Mask) -> usize,
                        ) -> Option<*const u8>
                        {
                            let chunk = V::load_unaligned(cur);
                            let eq1 = self.v1.cmpeq(chunk);
                            let eq2 = self.v2.cmpeq(chunk);
                            let mask = eq1.or(eq2).movemask();
                            
                            if mask.has_non_zero()
                            {
                                let mask1 = eq1.movemask();
                                let mask2 = eq2.movemask();
                                Some(cur.add(mask_to_offset(mask1.or(mask2))))
                            }
                            
                            else { None }
                        }
                    }
                    /// Finds all occurrences of two bytes in a haystack.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Three<V>
                    {
                        s1: u8,
                        s2: u8,
                        s3: u8,
                        v1: V,
                        v2: V,
                        v3: V,
                    }

                    impl<V: Vector> Three<V>
                    {
                        /// The number of bytes we examine per each iteration of our search loop.
                        const LOOP_SIZE: usize = 2 * V::BYTES;
                        /// Create a new searcher that finds occurrences of the byte given.
                        #[inline( always )] pub unsafe fn new
                        (
                            needle1: u8,
                            needle2: u8,
                            needle3: u8,
                        ) -> Three<V>
                        {
                            Three
                            {
                                s1: needle1,
                                s2: needle2,
                                s3: needle3,
                                v1: V::splat(needle1),
                                v2: V::splat(needle2),
                                v3: V::splat(needle3),
                            }
                        }
                        /// Returns the first needle given to `Three::new`.
                        #[inline( always )] pub fn needle1(&self) -> u8 { self.s1 }
                        /// Returns the second needle given to `Three::new`.
                        #[inline( always )] pub fn needle2(&self) -> u8 { self.s2 }
                        /// Returns the third needle given to `Three::new`.
                        #[inline( always )] pub fn needle3(&self) -> u8 { self.s3 }
                        /// Return a pointer to the first occurrence of one of the needles in the given haystack.
                        #[inline( always )] pub unsafe fn find_raw
                        (
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8> 
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::first_offset;
                            let len = end.distance(start);
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );
                            
                            if let Some(cur) = self.search_chunk(start, topos) { return Some(cur); }
                            
                            let mut cur = start.add(V::BYTES - (start.as_usize() & V::ALIGN));
                            debug_assert!(cur > start && end.sub(V::BYTES) >= start);
                            
                            if len >= Self::LOOP_SIZE
                            {
                                while cur <= end.sub(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(V::BYTES));
                                    let eqa1 = self.v1.cmpeq(a);
                                    let eqb1 = self.v1.cmpeq(b);
                                    let eqa2 = self.v2.cmpeq(a);
                                    let eqb2 = self.v2.cmpeq(b);
                                    let eqa3 = self.v3.cmpeq(a);
                                    let eqb3 = self.v3.cmpeq(b);
                                    let or1 = eqa1.or(eqb1);
                                    let or2 = eqa2.or(eqb2);
                                    let or3 = eqa3.or(eqb3);
                                    let or4 = or1.or(or2);
                                    let or5 = or3.or(or4);
                                    
                                    if or5.movemask_will_have_non_zero()
                                    {
                                        let mask = eqa1
                                        .movemask()
                                        .or(eqa2.movemask())
                                        .or(eqa3.movemask());
                                        
                                        if mask.has_non_zero() { return Some(cur.add(topos(mask))); }

                                        let mask = eqb1
                                        .movemask()
                                        .or(eqb2.movemask())
                                        .or(eqb3.movemask());
                                        
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(V::BYTES).add(topos(mask)));
                                    }
                                    
                                    cur = cur.add(Self::LOOP_SIZE);
                                }
                            }
                            
                            while cur <= end.sub(V::BYTES)
                            {
                                debug_assert!(end.distance(cur) >= V::BYTES);
                                
                                if let Some(cur) = self.search_chunk(cur, topos) { return Some(cur); }
                                
                                cur = cur.add(V::BYTES);
                            }
                            
                            if cur < end 
                            {
                                debug_assert!(end.distance(cur) < V::BYTES);
                                cur = cur.sub(V::BYTES - end.distance(cur));
                                debug_assert_eq!(end.distance(cur), V::BYTES);
                                return self.search_chunk(cur, topos);
                            }
                            
                            None
                        }
                        /// Return a pointer to the last occurrence of the needle in the given haystack.
                        #[inline( always )] pub unsafe fn rfind_raw(
                            &self,
                            start: *const u8,
                            end: *const u8,
                        ) -> Option<*const u8>
                        {
                            debug_assert!(V::BYTES <= 32, "vector cannot be bigger than 32 bytes");

                            let topos = V::Mask::last_offset;
                            let len = end.distance(start);
                            debug_assert!
                            (
                                len >= V::BYTES,
                                "haystack has length {}, but must be at least {}",
                                len,
                                V::BYTES
                            );

                            if let Some(cur) = self.search_chunk(end.sub(V::BYTES), topos) { return Some(cur); }
                            
                            let mut cur = end.sub(end.as_usize() & V::ALIGN);
                            debug_assert!(start <= cur && cur <= end);
                            
                            if len >= Self::LOOP_SIZE
                            {
                                while cur >= start.add(Self::LOOP_SIZE)
                                {
                                    debug_assert_eq!(0, cur.as_usize() % V::BYTES);

                                    cur = cur.sub(Self::LOOP_SIZE);
                                    let a = V::load_aligned(cur);
                                    let b = V::load_aligned(cur.add(V::BYTES));
                                    let eqa1 = self.v1.cmpeq(a);
                                    let eqb1 = self.v1.cmpeq(b);
                                    let eqa2 = self.v2.cmpeq(a);
                                    let eqb2 = self.v2.cmpeq(b);
                                    let eqa3 = self.v3.cmpeq(a);
                                    let eqb3 = self.v3.cmpeq(b);
                                    let or1 = eqa1.or(eqb1);
                                    let or2 = eqa2.or(eqb2);
                                    let or3 = eqa3.or(eqb3);
                                    let or4 = or1.or(or2);
                                    let or5 = or3.or(or4);
                                    
                                    if or5.movemask_will_have_non_zero()
                                    {
                                        let mask = eqb1
                                        .movemask()
                                        .or(eqb2.movemask())
                                        .or(eqb3.movemask());
                                        
                                        if mask.has_non_zero() { return Some(cur.add(V::BYTES).add(topos(mask))); }

                                        let mask = eqa1
                                        .movemask()
                                        .or(eqa2.movemask())
                                        .or(eqa3.movemask());
                                        
                                        debug_assert!(mask.has_non_zero());
                                        return Some(cur.add(topos(mask)));
                                    }
                                }
                            }
                            
                            while cur >= start.add(V::BYTES)
                            {
                                debug_assert!(cur.distance(start) >= V::BYTES);
                                cur = cur.sub(V::BYTES);
                                if let Some(cur) = self.search_chunk(cur, topos) {
                                    return Some(cur);
                                }
                            }
                            
                            if cur > start
                            {
                                debug_assert!(cur.distance(start) < V::BYTES);
                                return self.search_chunk(start, topos);
                            }
                            
                            None
                        }
                        /// Search `V::BYTES` starting at `cur` via an unaligned load.
                        #[inline( always )] unsafe fn search_chunk
                        (
                            &self,
                            cur: *const u8,
                            mask_to_offset: impl Fn(V::Mask) -> usize,
                        ) -> Option<*const u8>
                        {
                            let chunk = V::load_unaligned(cur);
                            let eq1 = self.v1.cmpeq(chunk);
                            let eq2 = self.v2.cmpeq(chunk);
                            let eq3 = self.v3.cmpeq(chunk);
                            let mask = eq1.or(eq2).or(eq3).movemask();
                            
                            if mask.has_non_zero()
                            {
                                let mask1 = eq1.movemask();
                                let mask2 = eq2.movemask();
                                let mask3 = eq3.movemask();
                                Some(cur.add(mask_to_offset(mask1.or(mask2).or(mask3))))
                            }
                            
                            else { None }
                        }
                    }
                    /// An iterator over all occurrences of a set of bytes in a haystack.
                    #[derive(Clone, Debug)]
                    pub struct Iter<'h>
                    {
                        /// The original starting point into the haystack.
                        original_start: *const u8,
                        /// The current starting point into the haystack.
                        start: *const u8,
                        /// The current ending point into the haystack.reverse search will begin.
                        end: *const u8,
                        /// A marker for tracking the lifetime of the start/cur_start/cur_end pointers above, 
                        ///which all point into the haystack.
                        haystack: ::marker::PhantomData<&'h [u8]>,
                    }

                    impl<'h> Iter<'h>
                    {
                        /// Create a new generic memchr iterator.
                        #[inline( always )] pub fn new(haystack: &'h [u8]) -> Iter<'h>
                        {
                            Iter
                            {
                                original_start: haystack.as_ptr(),
                                start: haystack.as_ptr(),
                                end: haystack.as_ptr().wrapping_add(haystack.len()),
                                haystack: ::marker::PhantomData,
                            }
                        }
                        /// Returns the next occurrence in the forward direction.
                        #[inline( always )] pub unsafe fn next
                        (
                            &mut self,
                            mut find_raw: impl FnMut(*const u8, *const u8) -> Option<*const u8>,
                        ) -> Option<usize>
                        {
                            let found = find_raw(self.start, self.end)?;
                            let result = found.distance(self.original_start);
                            self.start = found.add(1);
                            Some(result)
                        }
                        /// Returns the number of remaining elements in this iterator.
                        #[inline( always )] pub fn count
                        (
                            self,
                            mut count_raw: impl FnMut(*const u8, *const u8) -> usize,
                        ) -> usize
                        { count_raw(self.start, self.end) }
                        /// Returns the next occurrence in reverse.
                        #[inline( always )] pub unsafe fn next_back
                        (
                            &mut self,
                            mut rfind_raw: impl FnMut(*const u8, *const u8) -> Option<*const u8>,
                        ) -> Option<usize>
                        {
                            let found = rfind_raw(self.start, self.end)?;
                            let result = found.distance(self.original_start);
                            self.end = found;
                            Some(result)
                        }
                        /// Provides an implementation of `Iterator::size_hint`.
                        #[inline( always )] pub fn size_hint(&self) -> (usize, Option<usize>)
                        { (0, Some(self.end.as_usize().saturating_sub(self.start.as_usize()))) }
                    }
                    /// Search a slice using a function that operates on raw pointers.
                    #[inline( always )] pub unsafe fn search_slice_with_raw
                    (
                        haystack: &[u8],
                        mut find_raw: impl FnMut(*const u8, *const u8) -> Option<*const u8>,
                    ) -> Option<usize>
                    {
                        let start = haystack.as_ptr();
                        let end = start.add(haystack.len());
                        let found = find_raw(start, end)?;
                        Some(found.distance(start))
                    }
                    /// Performs a forward byte-at-a-time loop until either `ptr >= end_ptr` or until `confirm(*ptr)` returns `true`.
                    #[inline( always )] pub unsafe fn fwd_byte_by_byte<F: Fn(u8) -> bool>
                    (
                        start: *const u8,
                        end: *const u8,
                        confirm: F,
                    ) -> Option<*const u8>
                    {
                        debug_assert!(start <= end);
                        let mut ptr = start;
                        
                        while ptr < end
                        {
                            if confirm(*ptr) { return Some(ptr); }
                            
                            ptr = ptr.offset(1);
                        }
                        
                        None
                    }
                    /// Performs a reverse byte-at-a-time loop until either `ptr < start_ptr`
                    /// or until `confirm(*ptr)` returns `true`.
                    #[inline( always )] pub unsafe fn rev_byte_by_byte<F: Fn(u8) -> bool>
                    (
                        start: *const u8,
                        end: *const u8,
                        confirm: F,
                    ) -> Option<*const u8>
                    {
                        debug_assert!(start <= end);

                        let mut ptr = end;
                        
                        while ptr > start
                        {
                            ptr = ptr.offset(-1);
                            
                            if confirm(*ptr) { return Some(ptr); }
                        }
                        
                        None
                    }
                    /// Performs a forward byte-at-a-time loop until `ptr >= end_ptr`
                    /// and returns the number of times `confirm(*ptr)` returns `true`.
                    #[inline( always )] pub unsafe fn count_byte_by_byte<F: Fn(u8) -> bool>
                    (
                        start: *const u8,
                        end: *const u8,
                        confirm: F,
                    ) -> usize
                    {
                        debug_assert!(start <= end);
                        let mut ptr = start;
                        let mut count = 0;
                        
                        while ptr < end
                        {
                            if confirm(*ptr) { count += 1; }
                            
                            ptr = ptr.offset(1);
                        }
                        
                        count
                    }

                }

                pub mod packedpair
                {
                    use ::
                    {
                        mem::chr::
                        {
                            arch::all::{is_equal_raw, packedpair::Pair},
                            ext::Pointer,
                            vector::{MoveMask, Vector},
                        },
                        *,
                    };
                    /*
                    */
                    /// A generic architecture dependent "packed pair" finder.
                    #[derive(Clone, Copy, Debug)]
                    pub struct Finder<V>
                    {
                        pair: Pair,
                        v1: V,
                        v2: V,
                        min_haystack_len: usize,
                    }

                    impl<V: Vector> Finder<V> 
                    {
                        /// Create a new pair searcher.
                        #[inline( always )] pub unsafe fn new(needle: &[u8], pair: Pair) -> Finder<V>
                        {
                            let max_index = pair.index1().max(pair.index2());
                            let min_haystack_len = ::cmp::max(needle.len(), usize::from(max_index) + V::BYTES);
                            let v1 = V::splat(needle[usize::from(pair.index1())]);
                            let v2 = V::splat(needle[usize::from(pair.index2())]);
                            Finder { pair, v1, v2, min_haystack_len }
                        }
                        /// Searches the given haystack for the given needle.
                        #[inline( always )] pub unsafe fn find
                        (
                            &self,
                            haystack: &[u8],
                            needle: &[u8],
                        ) -> Option<usize>
                        {
                            assert!
                            (
                                haystack.len() >= self.min_haystack_len,
                                "haystack too small, should be at least {} but got {}",
                                self.min_haystack_len,
                                haystack.len(),
                            );

                            let all = V::Mask::all_zeros_except_least_significant(0);
                            let start = haystack.as_ptr();
                            let end = start.add(haystack.len());
                            let max = end.sub(self.min_haystack_len);
                            let mut cur = start;

                            while cur <= max 
                            {
                                if let Some(chunki) = self.find_in_chunk(needle, cur, end, all)
                                { return Some(matched(start, cur, chunki)); }

                                cur = cur.add(V::BYTES);
                            }
                            
                            if cur < end
                            {
                                let remaining = end.distance(cur);
                                
                                debug_assert!
                                (
                                    remaining < self.min_haystack_len,
                                    "remaining bytes should be smaller than the minimum haystack \
                                    length of {}, but there are {} bytes remaining",
                                    self.min_haystack_len,
                                    remaining,
                                );
                                
                                if remaining < needle.len() { return None; }
                                
                                debug_assert!
                                (
                                    max < cur,
                                    "after main loop, cur should have exceeded max",
                                );

                                let overlap = cur.distance(max);
                                
                                debug_assert!
                                (
                                    overlap > 0,
                                    "overlap ({}) must always be non-zero",
                                    overlap,
                                );
                                
                                debug_assert!
                                (
                                    overlap < V::BYTES,
                                    "overlap ({}) cannot possibly be >= than a vector ({})",
                                    overlap,
                                    V::BYTES,
                                );
                                
                                let mask = V::Mask::all_zeros_except_least_significant(overlap);
                                cur = max;
                                let m = self.find_in_chunk(needle, cur, end, mask);

                                if let Some(chunki) = m { return Some(matched(start, cur, chunki)); }
                            }
                            
                            None
                        }
                        /// Searches the given haystack for offsets that represent candidate matches of the `needle`
                        /// given to this finder's constructor.
                        #[inline( always )] pub unsafe fn find_prefilter
                        (
                            &self,
                            haystack: &[u8],
                        ) -> Option<usize>
                        {
                            assert!
                            (
                                haystack.len() >= self.min_haystack_len,
                                "haystack too small, should be at least {} but got {}",
                                self.min_haystack_len,
                                haystack.len(),
                            );

                            let start = haystack.as_ptr();
                            let end = start.add(haystack.len());
                            let max = end.sub(self.min_haystack_len);
                            let mut cur = start;
                            
                            while cur <= max
                            {
                                if let Some(chunki) = self.find_prefilter_in_chunk(cur)
                                { return Some(matched(start, cur, chunki)); }

                                cur = cur.add(V::BYTES);
                            }

                            if cur < end
                            {
                                cur = max;

                                if let Some(chunki) = self.find_prefilter_in_chunk(cur)
                                { return Some(matched(start, cur, chunki)); }
                            }
                            
                            None
                        }
                        /// Search for an occurrence of our byte pair from the needle in the chunk
                        /// pointed to by cur, with the end of the haystack pointed to by end.
                        #[inline( always )] unsafe fn find_in_chunk
                        (
                            &self,
                            needle: &[u8],
                            cur: *const u8,
                            end: *const u8,
                            mask: V::Mask,
                        ) -> Option<usize>
                        {
                            let index1 = usize::from(self.pair.index1());
                            let index2 = usize::from(self.pair.index2());
                            let chunk1 = V::load_unaligned(cur.add(index1));
                            let chunk2 = V::load_unaligned(cur.add(index2));
                            let eq1 = chunk1.cmpeq(self.v1);
                            let eq2 = chunk2.cmpeq(self.v2);

                            let mut offsets = eq1.and(eq2).movemask().and(mask);
                            
                            while offsets.has_non_zero()
                            {
                                let offset = offsets.first_offset();
                                let cur = cur.add(offset);
                                
                                if end.sub(needle.len()) < cur { return None; }

                                if is_equal_raw(needle.as_ptr(), cur, needle.len()) { return Some(offset); }

                                offsets = offsets.clear_least_significant_bit();
                            }
                            
                            None
                        }
                        /// Search for an occurrence of our byte pair from the needle in the chunk pointed to by cur,
                        /// with the end of the haystack pointed to by end.
                        #[inline( always )] unsafe fn find_prefilter_in_chunk(&self, cur: *const u8) -> Option<usize>
                        {
                            let index1 = usize::from(self.pair.index1());
                            let index2 = usize::from(self.pair.index2());
                            let chunk1 = V::load_unaligned(cur.add(index1));
                            let chunk2 = V::load_unaligned(cur.add(index2));
                            let eq1 = chunk1.cmpeq(self.v1);
                            let eq2 = chunk2.cmpeq(self.v2);

                            let offsets = eq1.and(eq2).movemask();
                            
                            if !offsets.has_non_zero() { return None; }

                            Some(offsets.first_offset())
                        }
                        /// Returns the pair of offsets (into the needle) used to check as a predicate before 
                        /// confirming whether a needle exists at a particular position.
                        #[inline] pub fn pair(&self) -> &Pair { &self.pair }
                        /// Returns the minimum haystack length that this `Finder` can search.
                        #[inline( always )] pub fn min_haystack_len(&self) -> usize { self.min_haystack_len }
                    }
                    /// Accepts a chunk-relative offset and returns a haystack relative offset.
                    #[inline( always )] unsafe fn matched(start: *const u8, cur: *const u8, chunki: usize) -> usize
                    { cur.distance(start) + chunki }
                }
            }

            pub mod x86_64
            {
                /*!
                Vector algorithms for the `x86_64` target. */
                use ::
                {
                    *,
                };
                /*
                */
                pub mod avx2
                {
                    /*!
                    Algorithms for the `x86_64` target using 256-bit vectors via AVX2. */
                    use ::
                    {
                        *,
                    };
                    /*
                    */
                    pub mod memchr
                    {
                        /*!
                        This module defines 256-bit vector implementations of `memchr` and friends. */
                        use ::
                        {
                            arch::x86_64::{ __m128i, __m256i },
                            mem::chr::
                            {
                                arch::generic::memchr as generic, 
                                ext::Pointer, 
                                vector::Vector,
                            },
                            *,
                        };
                        /*
                        */
                        /// Finds all occurrences of a single byte in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct One
                        {
                            /// Used for haystacks less than 32 bytes.
                            sse2: generic::One<__m128i>,
                            /// Used for haystacks bigger than 32 bytes.
                            avx2: generic::One<__m256i>,
                        }

                        impl One
                        {
                            /// Create a new searcher that finds occurrences of the needle byte given.
                            ///
                            /// This particular searcher is specialized to use AVX2 vector instructions
                            /// that typically make it quite fast. (SSE2 is used for haystacks that
                            /// are too short to accommodate an AVX2 vector.)
                            ///
                            /// If either SSE2 or AVX2 is unavailable in the current environment, then
                            /// `None` is returned.
                            #[inline] pub fn new(needle: u8) -> Option<One>
                            {
                                if One::is_available() {
                                    unsafe { Some(One::new_unchecked(needle)) }
                                } else { None }
                            } 
                            /// Create a new finder specific to AVX2 vectors and routines without
                            /// checking that either SSE2 or AVX2 is available.
                            #[target_feature(enable = "sse2", enable = "avx2")]
                            #[inline] pub unsafe fn new_unchecked(needle: u8) -> One
                            {
                                One
                                {
                                    sse2: generic::One::new(needle),
                                    avx2: generic::One::new(needle),
                                }
                            }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(not(target_feature = "sse2"))]
                                {
                                    false
                                }
                                #[cfg(target_feature = "sse2")]
                                {
                                    #[cfg(target_feature = "avx2")]
                                    {
                                        true
                                    }
                                    #[cfg(not(target_feature = "avx2"))]
                                    {
                                        #[cfg(feature = "std")]
                                        {
                                            std::is_x86_feature_detected!("avx2")
                                        }
                                        #[cfg(not(feature = "std"))]
                                        {
                                            false
                                        }
                                    }
                                }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| {
                                        self.rfind_raw(s, e)
                                    })
                                }
                            }
                            /// Counts all occurrences of this byte in the given haystack.
                            #[inline] pub fn count(&self, haystack: &[u8]) -> usize
                            {
                                unsafe
                                {
                                    let start = haystack.as_ptr();
                                    let end = start.add(haystack.len());
                                    self.count_raw(start, end)
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                let len = end.distance(start);

                                if len < __m256i::BYTES
                                {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::fwd_byte_by_byte(start, end, |b| { b == self.sse2.needle1() })
                                    }
                                    else
                                    {
                                        self.find_raw_sse2(start, end)
                                    };
                                }
                                
                                self.find_raw_avx2(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn rfind_raw
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }
                                let len = end.distance(start);
                                if len < __m256i::BYTES
                                {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::rev_byte_by_byte(start, end, |b| { b == self.sse2.needle1() })
                                    }
                                    
                                    else { self.rfind_raw_sse2(start, end) };
                                }
                                
                                self.rfind_raw_avx2(start, end)
                            }
                            /// Counts all occurrences of this byte in the given haystack represented by raw pointers.
                            #[inline] pub unsafe fn count_raw(&self, start: *const u8, end: *const u8) -> usize
                            {
                                if start >= end { return 0; }

                                let len = end.distance(start);
                                
                                if len < __m256i::BYTES
                                {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::count_byte_by_byte(start, end, |b| { b == self.sse2.needle1() })
                                    }
                                    else { self.count_raw_sse2(start, end) };
                                }
                                
                                self.count_raw_avx2(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_sse2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.sse2.find_raw(start, end) }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_sse2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.sse2.rfind_raw(start, end) }
                            /// Execute a count using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn count_raw_sse2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> usize
                            { self.sse2.count_raw(start, end) }
                            /// Execute a search using AVX2 vectors and routines.
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn find_raw_avx2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.avx2.find_raw(start, end) }
                            /// Execute a search using AVX2 vectors and routines.
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn rfind_raw_avx2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.avx2.rfind_raw(start, end) }
                            /// Execute a count using AVX2 vectors and routines.
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn count_raw_avx2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> usize { self.avx2.count_raw(start, end) }
                            /// Returns an iterator over all occurrences of the needle byte in the given haystack.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> OneIter<'a, 'h>
                            {
                                OneIter { searcher: self, it: generic::Iter::new(haystack) }
                            }
                        }
                        /// An iterator over all occurrences of a single byte in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct OneIter<'a, 'h>
                        {
                            searcher: &'a One,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for OneIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize>
                            {
                                unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                            }

                            #[inline] fn count(self) -> usize
                            {
                                self.it.count(|s, e|
                                {
                                    unsafe { self.searcher.count_raw(s, e) }
                                })
                            }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                        }

                        impl<'a, 'h> DoubleEndedIterator for OneIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            {
                                unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                            }
                        }

                        impl<'a, 'h> ::iter::FusedIterator for OneIter<'a, 'h> {}
                        /// Finds all occurrences of two bytes in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct Two
                        {
                            /// Used for haystacks less than 32 bytes.
                            sse2: generic::Two<__m128i>,
                            /// Used for haystacks bigger than 32 bytes.
                            avx2: generic::Two<__m256i>,
                        }

                        impl Two
                        {
                            /// Create a new searcher that finds occurrences of the needle bytes given.
                            #[inline] pub fn new(needle1: u8, needle2: u8) -> Option<Two>
                            {
                                if Two::is_available() { unsafe { Some(Two::new_unchecked(needle1, needle2)) } } 
                                else { None }
                            }
                            /// Create a new finder specific to AVX2 vectors and routines without checking 
                            /// that either SSE2 or AVX2 is available.
                            #[target_feature(enable = "sse2", enable = "avx2")] 
                            #[inline] pub unsafe fn new_unchecked(needle1: u8, needle2: u8) -> Two
                            {
                                Two
                                {
                                    sse2: generic::Two::new(needle1, needle2),
                                    avx2: generic::Two::new(needle1, needle2),
                                }
                            }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(not(target_feature = "sse2"))]
                                {
                                    false
                                }

                                #[cfg(target_feature = "sse2")]
                                {
                                    #[cfg(target_feature = "avx2")]
                                    {
                                        true
                                    }

                                    #[cfg(not(target_feature = "avx2"))]
                                    {
                                        #[cfg(feature = "std")]
                                        {
                                            std::is_x86_feature_detected!("avx2")
                                        }

                                        #[cfg(not(feature = "std"))]
                                        {
                                            false
                                        }
                                    }
                                }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.rfind_raw(s, e) })
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                let len = end.distance(start);
                                
                                if len < __m256i::BYTES
                                {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::fwd_byte_by_byte(start, end, |b| { b == self.sse2.needle1() || b == self.sse2.needle2() })
                                    }

                                    else { self.find_raw_sse2(start, end) };
                                }

                                self.find_raw_avx2(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.
                            ///
                            /// When a match is found, the pointer returned is guaranteed to be
                            /// `>= start` and `< end`.
                            ///
                            /// This routine is useful if you're already using raw pointers and would
                            /// like to avoid converting back to a slice before executing a search.
                            ///
                            /// # Safety
                            ///
                            /// * Both `start` and `end` must be valid for reads.
                            /// * Both `start` and `end` must point to an initialized value.
                            /// * Both `start` and `end` must point to the same allocated object and
                            /// must either be in bounds or at most one byte past the end of the
                            /// allocated object.
                            /// * Both `start` and `end` must be _derived from_ a pointer to the same
                            /// object.
                            /// * The distance between `start` and `end` must not overflow `isize`.
                            /// * The distance being in bounds must not rely on "wrapping around" the
                            /// address space.
                            ///
                            /// Note that callers may pass a pair of pointers such that `start >= end`.
                            /// In that case, `None` will always be returned.
                            #[inline] pub unsafe fn rfind_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> {
                                if start >= end { return None; }
                                let len = end.distance(start);
                                if len < __m256i::BYTES {
                                    return if len < __m128i::BYTES {
                                        // SAFETY: We require the caller to pass valid start/end
                                        // pointers.
                                        generic::rev_byte_by_byte(start, end, |b| {
                                            b == self.sse2.needle1() || b == self.sse2.needle2()
                                        })
                                    } else {
                                        // SAFETY: We require the caller to pass valid start/end
                                        // pointers.
                                        self.rfind_raw_sse2(start, end)
                                    };
                                }
                                // SAFETY: Building a `Two` means it's safe to call both 'sse2' and
                                // 'avx2' routines. Also, we've checked that our haystack is big
                                // enough to run on the vector routine. Pointer validity is caller's
                                // responsibility.
                                //
                                // See note in forward routine above for why we don't just call
                                // `self.avx2.rfind_raw` directly here.
                                self.rfind_raw_avx2(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            ///
                            /// # Safety
                            ///
                            /// Same as [`Two::find_raw`], except the distance between `start` and
                            /// `end` must be at least the size of an SSE2 vector (in bytes).
                            ///
                            /// (The target feature safety obligation is automatically fulfilled by
                            /// virtue of being a method on `Two`, which can only be constructed
                            /// when it is safe to call `sse2`/`avx2` routines.)
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_sse2
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> {
                                self.sse2.find_raw(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            ///
                            /// # Safety
                            ///
                            /// Same as [`Two::rfind_raw`], except the distance between `start` and
                            /// `end` must be at least the size of an SSE2 vector (in bytes).
                            ///
                            /// (The target feature safety obligation is automatically fulfilled by
                            /// virtue of being a method on `Two`, which can only be constructed
                            /// when it is safe to call `sse2`/`avx2` routines.)
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_sse2
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> {
                                self.sse2.rfind_raw(start, end)
                            }
                            /// Execute a search using AVX2 vectors and routines.
                            ///
                            /// # Safety
                            ///
                            /// Same as [`Two::find_raw`], except the distance between `start` and
                            /// `end` must be at least the size of an AVX2 vector (in bytes).
                            ///
                            /// (The target feature safety obligation is automatically fulfilled by
                            /// virtue of being a method on `Two`, which can only be constructed
                            /// when it is safe to call `sse2`/`avx2` routines.)
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn find_raw_avx2
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> {
                                self.avx2.find_raw(start, end)
                            }
                            /// Execute a search using AVX2 vectors and routines.
                            ///
                            /// # Safety
                            ///
                            /// Same as [`Two::rfind_raw`], except the distance between `start` and
                            /// `end` must be at least the size of an AVX2 vector (in bytes).
                            ///
                            /// (The target feature safety obligation is automatically fulfilled by
                            /// virtue of being a method on `Two`, which can only be constructed
                            /// when it is safe to call `sse2`/`avx2` routines.)
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn rfind_raw_avx2
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> {
                                self.avx2.rfind_raw(start, end)
                            }
                            /// Returns an iterator over all occurrences of the needle bytes in the
                            /// given haystack.
                            ///
                            /// The iterator returned implements `DoubleEndedIterator`. This means it
                            /// can also be used to find occurrences in reverse order.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> TwoIter<'a, 'h> {
                                TwoIter { searcher: self, it: generic::Iter::new(haystack) }
                            }
                        }
                        /// An iterator over all occurrences of two possible bytes in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct TwoIter<'a, 'h>
                        {
                            searcher: &'a Two,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for TwoIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize>
                            { unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) } }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                        }

                        impl<'a, 'h> DoubleEndedIterator for TwoIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            {
                                unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                            }
                        }

                        impl<'a, 'h> ::iter::FusedIterator for TwoIter<'a, 'h> {}
                        /// Finds all occurrences of three bytes in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct Three
                        {
                            /// Used for haystacks less than 32 bytes.
                            sse2: generic::Three<__m128i>,
                            /// Used for haystacks bigger than 32 bytes.
                            avx2: generic::Three<__m256i>,
                        }

                        impl Three
                        {
                            /// Create a new searcher that finds occurrences of the needle bytes given.
                            #[inline] pub fn new(needle1: u8, needle2: u8, needle3: u8) -> Option<Three>
                            {
                                if Three::is_available()
                                {
                                    unsafe { Some(Three::new_unchecked(needle1, needle2, needle3)) }
                                } else { None }
                            }
                            /// Create a new finder specific to AVX2 vectors and routines without
                            /// checking that either SSE2 or AVX2 is available.
                            #[target_feature(enable = "sse2", enable = "avx2")]
                            #[inline] pub unsafe fn new_unchecked
                            (
                                needle1: u8,
                                needle2: u8,
                                needle3: u8,
                            ) -> Three
                            {
                                Three
                                {
                                    sse2: generic::Three::new(needle1, needle2, needle3),
                                    avx2: generic::Three::new(needle1, needle2, needle3),
                                }
                            }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(not(target_feature = "sse2"))]
                                {
                                    false
                                }
                                #[cfg(target_feature = "sse2")]
                                {
                                    #[cfg(target_feature = "avx2")]
                                    {
                                        true
                                    }
                                    #[cfg(not(target_feature = "avx2"))]
                                    {
                                        #[cfg(feature = "std")]
                                        {
                                            std::is_x86_feature_detected!("avx2")
                                        }
                                        #[cfg(not(feature = "std"))]
                                        {
                                            false
                                        }
                                    }
                                }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e|
                                    {
                                        self.rfind_raw(s, e)
                                    })
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }
                                let len = end.distance(start);
                                if len < __m256i::BYTES
                                {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::fwd_byte_by_byte(start, end, |b|
                                        {
                                            b == self.sse2.needle1()
                                                || b == self.sse2.needle2()
                                                || b == self.sse2.needle3()
                                        })
                                    } else { self.find_raw_sse2(start, end) };
                                }
                                
                                self.find_raw_avx2(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn rfind_raw
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8> 
                            {
                                if start >= end { return None; }
                                
                                let len = end.distance(start);

                                if len < __m256i::BYTES {
                                    return if len < __m128i::BYTES
                                    {
                                        generic::rev_byte_by_byte(start, end, |b|
                                        {
                                            b == self.sse2.needle1()
                                                || b == self.sse2.needle2()
                                                || b == self.sse2.needle3()
                                        })
                                    }
                                    else { self.rfind_raw_sse2(start, end) };
                                }
                                
                                self.rfind_raw_avx2(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_sse2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.sse2.find_raw(start, end) }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_sse2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.sse2.rfind_raw(start, end) }
                            /// Execute a search using AVX2 vectors and routines.
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn find_raw_avx2
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.avx2.find_raw(start, end) }
                            /// Execute a search using AVX2 vectors and routines.
                            #[target_feature(enable = "avx2")]
                            #[inline] unsafe fn rfind_raw_avx
                            
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.avx2.rfind_raw(start, end) }
                            /// Returns an iterator over all occurrences of the needle bytes in the given haystack.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> ThreeIter<'a, 'h>
                            { ThreeIter { searcher: self, it: generic::Iter::new(haystack) } }
                        }
                        /// An iterator over all occurrences of three possible bytes in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct ThreeIter<'a, 'h>
                        {
                            searcher: &'a Three,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for ThreeIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize> { unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) } }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                        }

                        impl<'a, 'h> DoubleEndedIterator for ThreeIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            { unsafe { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) } }
                        }

                        impl<'a, 'h> ::iter::FusedIterator for ThreeIter<'a, 'h> {}
                    }

                    pub mod packedpair
                    {
                        /*!
                        A 256-bit vector implementation of the "packed pair" SIMD algorithm.*/
                        use ::
                        {
                            arch::x86_64::{__m128i, __m256i},
                            mem::chr::
                            {
                                arch::
                                {
                                    all::packedpair::Pair,
                                    generic::packedpair,
                                }
                            },
                            *,
                        };
                        /*
                        */
                        /// A "packed pair" finder that uses 256-bit vector operations.
                        #[derive(Clone, Copy, Debug)]
                        pub struct Finder 
                        {
                            sse2: packedpair::Finder<__m128i>,
                            avx2: packedpair::Finder<__m256i>,
                        }

                        impl Finder
                        {
                            /// Create a new pair searcher. The searcher returned can either report
                            /// exact matches of `needle` or act as a prefilter and report candidate
                            /// positions of `needle`.
                            #[inline] pub fn new(needle: &[u8]) -> Option<Finder>
                            { Finder::with_pair(needle, Pair::new(needle)?) }
                            /// Create a new "packed pair" finder using the pair of bytes given.
                            #[inline] pub fn with_pair(needle: &[u8], pair: Pair) -> Option<Finder>
                            {
                                if Finder::is_available() { unsafe { Some(Finder::with_pair_impl(needle, pair)) } }
                                else { None }
                            }
                            /// Create a new `Finder` specific to SSE2 vectors and routines.
                            #[target_feature(enable = "sse2", enable = "avx2")]
                            #[inline] unsafe fn with_pair_impl(needle: &[u8], pair: Pair) -> Finder
                            {
                                let sse2 = packedpair::Finder::<__m128i>::new(needle, pair);
                                let avx2 = packedpair::Finder::<__m256i>::new(needle, pair);
                                Finder { sse2, avx2 }
                            }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(not(target_feature = "sse2"))]
                                {
                                    false
                                }
                                #[cfg(target_feature = "sse2")]
                                {
                                    #[cfg(target_feature = "avx2")]
                                    {
                                        true
                                    }
                                    #[cfg(not(target_feature = "avx2"))]
                                    {
                                        #[cfg(feature = "std")]
                                        {
                                            std::is_x86_feature_detected!("avx2")
                                        }
                                        #[cfg(not(feature = "std"))]
                                        {
                                            false
                                        }
                                    }
                                }
                            }
                            /// Execute a search using AVX2 vectors and routines.
                            #[inline] pub fn find(&self, haystack: &[u8], needle: &[u8]) -> Option<usize>
                            { unsafe { self.find_impl(haystack, needle) } }
                            /// Run this finder on the given haystack as a prefilter.
                            #[inline] pub fn find_prefilter(&self, haystack: &[u8]) -> Option<usize>
                            { unsafe { self.find_prefilter_impl(haystack) } }
                            /// Execute a search using AVX2 vectors and routines.
                            #[target_feature(enable = "sse2", enable = "avx2")]
                            #[inline] unsafe fn find_impl
                            
                            (
                                &self,
                                haystack: &[u8],
                                needle: &[u8],
                            ) -> Option<usize>
                            {
                                if haystack.len() < self.avx2.min_haystack_len() { self.sse2.find(haystack, needle) }
                                else { self.avx2.find(haystack, needle) }
                            }
                            /// Execute a prefilter search using AVX2 vectors and routines.
                            #[target_feature(enable = "sse2", enable = "avx2")]
                            #[inline] unsafe fn find_prefilter_impl(&self, haystack: &[u8]) -> Option<usize>
                            {
                                if haystack.len() < self.avx2.min_haystack_len() { self.sse2.find_prefilter(haystack) }

                                else { self.avx2.find_prefilter(haystack) }
                            }
                            /// Returns the pair of offsets (into the needle) used to check as a predicate before 
                            /// confirming whether a needle exists at a particular position.
                            #[inline] pub fn pair(&self) -> &Pair { self.avx2.pair() }
                            /// Returns the minimum haystack length that this `Finder` can search.
                            #[inline] pub fn min_haystack_len(&self) -> usize { self.sse2.min_haystack_len() }
                        }

                    }
                }

                pub mod sse2
                {
                    /*!
                    Algorithms for the `x86_64` target using 128-bit vectors via SSE2. */
                    use ::
                    {
                        *,
                    };
                    /*
                    */
                    pub mod memchr
                    {
                        /*!
                        This module defines 128-bit vector implementations of `memchr` and friends. */
                        use ::
                        {
                            arch::x86_64::__m128i,
                            mem::chr::
                            {
                                arch::generic::memchr as generic, ext::Pointer, vector::Vector,
                            },
                            *,
                        };
                        /*
                        */
                        /// Finds all occurrences of a single byte in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct One(generic::One<__m128i>);

                        impl One
                        {
                            /// Create a new searcher that finds occurrences of the needle byte given.
                            #[inline] pub fn new(needle: u8) -> Option<One>
                            {
                                if One::is_available() { unsafe { Some(One::new_unchecked(needle)) } }
                                else { None }
                            }
                            /// Create a new finder specific to SSE2 vectors and routines without
                            /// checking that SSE2 is available.
                            #[target_feature(enable = "sse2")]
                            #[inline] pub unsafe fn new_unchecked(needle: u8) -> One
                            { One(generic::One::new(needle)) }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(target_feature = "sse2")] { true }
                                #[cfg(not(target_feature = "sse2"))] { false }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.rfind_raw(s, e) })
                                }
                            }
                            /// Counts all occurrences of this byte in the given haystack.
                            #[inline] pub fn count(&self, haystack: &[u8]) -> usize
                            {
                                unsafe
                                {
                                    let start = haystack.as_ptr();
                                    let end = start.add(haystack.len());
                                    self.count_raw(start, end)
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::fwd_byte_by_byte(start, end, |b| { b == self.0.needle1() });
                                }

                                self.find_raw_impl(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.                            
                            #[inline] pub unsafe fn rfind_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::rev_byte_by_byte(start, end, |b| { b == self.0.needle1() });
                                }
                                
                                self.rfind_raw_impl(start, end)
                            }
                            /// Counts all occurrences of this byte in the given haystack represented by raw pointers.
                            #[inline] pub unsafe fn count_raw(&self, start: *const u8, end: *const u8) -> usize
                            {
                                if start >= end { return 0; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::count_byte_by_byte(start, end, |b| { b == self.0.needle1() });
                                }

                                self.count_raw_impl(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.find_raw(start, end) }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.rfind_raw(start, end) }
                            /// Execute a count using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn count_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> usize {
                                self.0.count_raw(start, end)
                            }
                            /// Returns an iterator over all occurrences of the needle byte in the given haystack.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> OneIter<'a, 'h>
                            {
                                OneIter { searcher: self, it: generic::Iter::new(haystack) }
                            }
                        }
                        /// An iterator over all occurrences of a single byte in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct OneIter<'a, 'h>
                        {
                            searcher: &'a One,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for OneIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize>
                            {
                                unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                            }

                            #[inline] fn count(self) -> usize 
                            { self.it.count(|s, e| { unsafe { self.searcher.count_raw(s, e) } }) }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                        }

                        impl<'a, 'h> DoubleEndedIterator for OneIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            {
                                unsafe
                                { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                            }
                        }

                        impl<'a, 'h> core::iter::FusedIterator for OneIter<'a, 'h> {}
                        /// Finds all occurrences of two bytes in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct Two(generic::Two<__m128i>);

                        impl Two
                        {
                            /// Create a new searcher that finds occurrences of the needle bytes given.
                            #[inline] pub fn new(needle1: u8, needle2: u8) -> Option<Two>
                            {
                                if Two::is_available() { unsafe { Some(Two::new_unchecked(needle1, needle2)) } }
                                else { None }
                            }
                            /// Create a new finder specific to SSE2 vectors and routines without
                            /// checking that SSE2 is available.
                            #[target_feature(enable = "sse2")]
                            #[inline] pub unsafe fn new_unchecked(needle1: u8, needle2: u8) -> Two
                            { Two(generic::Two::new(needle1, needle2)) }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool
                            {
                                #[cfg(target_feature = "sse2")] { true }
                                #[cfg(not(target_feature = "sse2"))] { false }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given
                            /// haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.rfind_raw(s, e) })
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }
                                
                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::fwd_byte_by_byte
                                    (
                                        start, 
                                        end, 
                                        | b | { b == self.0.needle1() || b == self.0.needle2() }
                                    );
                                }
                                
                                self.find_raw_impl(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn rfind_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::rev_byte_by_byte
                                    (
                                        start, 
                                        end,
                                        |b| { b == self.0.needle1() || b == self.0.needle2() }
                                    );
                                }
                                
                                self.rfind_raw_impl(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.find_raw(start, end) }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.rfind_raw(start, end) }
                            /// Returns an iterator over all occurrences of the needle bytes in the given haystack.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> TwoIter<'a, 'h>
                            { TwoIter { searcher: self, it: generic::Iter::new(haystack) } }
                        }
                        /// An iterator over all occurrences of two possible bytes in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct TwoIter<'a, 'h>
                        {
                            searcher: &'a Two,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for TwoIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize>
                            {
                                unsafe
                                { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                            }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) {
                                self.it.size_hint()
                            }
                        }

                        impl<'a, 'h> DoubleEndedIterator for TwoIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            {
                                unsafe
                                { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                            }
                        }

                        impl<'a, 'h> core::iter::FusedIterator for TwoIter<'a, 'h> {}
                        /// Finds all occurrences of three bytes in a haystack.
                        #[derive(Clone, Copy, Debug)]
                        pub struct Three(generic::Three<__m128i>);

                        impl Three 
                        {
                            /// Create a new searcher that finds occurrences of the needle bytes given.
                            #[inline] pub fn new(needle1: u8, needle2: u8, needle3: u8) -> Option<Three> 
                            {
                                if Three::is_available() 
                                { unsafe { Some(Three::new_unchecked(needle1, needle2, needle3)) } }
                                
                                else { None }
                            }
                            /// Create a new finder specific to SSE2 vectors and routines without
                            /// checking that SSE2 is available.
                            #[target_feature(enable = "sse2")]
                            #[inline] pub unsafe fn new_unchecked
                            (
                                needle1: u8,
                                needle2: u8,
                                needle3: u8,
                            ) -> Three
                            { Three(generic::Three::new(needle1, needle2, needle3)) }
                            /// Returns true when this implementation is available in the current environment.
                            #[inline] pub fn is_available() -> bool 
                            {
                                #[cfg(target_feature = "sse2")]
                                {
                                    true
                                }
                                #[cfg(not(target_feature = "sse2"))]
                                {
                                    false
                                }
                            }
                            /// Return the first occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.find_raw(s, e) })
                                }
                            }
                            /// Return the last occurrence of one of the needle bytes in the given haystack.
                            #[inline] pub fn rfind(&self, haystack: &[u8]) -> Option<usize>
                            {
                                unsafe
                                {
                                    generic::search_slice_with_raw(haystack, |s, e| { self.rfind_raw(s, e) })
                                }
                            }
                            /// Like `find`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn find_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::fwd_byte_by_byte
                                    (
                                        start, 
                                        end, 
                                        |b|
                                        {
                                            b == self.0.needle1()
                                            || b == self.0.needle2()
                                            || b == self.0.needle3()
                                        });
                                }
                                
                                self.find_raw_impl(start, end)
                            }
                            /// Like `rfind`, but accepts and returns raw pointers.
                            #[inline] pub unsafe fn rfind_raw
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            {
                                if start >= end { return None; }

                                if end.distance(start) < __m128i::BYTES
                                {
                                    return generic::rev_byte_by_byte
                                    (
                                        start, 
                                        end, 
                                        |b|
                                        {
                                            b == self.0.needle1()
                                            || b == self.0.needle2()
                                            || b == self.0.needle3()
                                        }
                                    );
                                }
                                
                                self.rfind_raw_impl(start, end)
                            }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn find_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.find_raw(start, end) }
                            /// Execute a search using SSE2 vectors and routines.
                            #[target_feature(enable = "sse2")]
                            #[inline] unsafe fn rfind_raw_impl
                            (
                                &self,
                                start: *const u8,
                                end: *const u8,
                            ) -> Option<*const u8>
                            { self.0.rfind_raw(start, end) }
                            /// Returns an iterator over all occurrences of the needle byte in the given haystack.
                            #[inline] pub fn iter<'a, 'h>(&'a self, haystack: &'h [u8]) -> ThreeIter<'a, 'h>
                            {
                                ThreeIter { searcher: self, it: generic::Iter::new(haystack) }
                            }
                        }
                        /// An iterator over all occurrences of three possible bytes in a haystack.
                        #[derive(Clone, Debug)]
                        pub struct ThreeIter<'a, 'h>
                        {
                            searcher: &'a Three,
                            it: generic::Iter<'h>,
                        }

                        impl<'a, 'h> Iterator for ThreeIter<'a, 'h>
                        {
                            type Item = usize;

                            #[inline] fn next(&mut self) -> Option<usize>
                            {
                                unsafe { self.it.next(|s, e| self.searcher.find_raw(s, e)) }
                            }

                            #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
                        }

                        impl<'a, 'h> DoubleEndedIterator for ThreeIter<'a, 'h>
                        {
                            #[inline] fn next_back(&mut self) -> Option<usize>
                            {
                                unsafe
                                { self.it.next_back(|s, e| self.searcher.rfind_raw(s, e)) }
                            }
                        }

                        impl<'a, 'h> core::iter::FusedIterator for ThreeIter<'a, 'h> {}
                    }

                    pub mod packedpair
                    {
                        use ::
                        {
                            *,
                        };
                        /*
                        */

                    }


                }

                pub mod memchr
                {
                    use ::
                    {
                        *,
                    };
                    /*
                    */
                }
            }
        }

        pub mod cow
        {
            use ::
            {
                *,
            };
            /// A specialized copy-on-write byte string.
            #[derive(Clone, Debug)]
            pub struct CowBytes<'a>(Imp<'a>);

            #[derive(Clone, Debug)]
            enum Imp<'a> 
            {
                Borrowed(&'a [u8]),
                Owned( ::boxed::Box<[u8]>),
            }
            
            impl<'a> ops::Deref for CowBytes<'a> 
            {
                type Target = [u8];
                #[inline(always)] fn deref(&self) -> &[u8] { self.as_slice() }
            }

            impl<'a> CowBytes<'a>
            {
                /// Create a new borrowed CowBytes.
                #[inline( always )] pub fn new<B: ?Sized + AsRef<[u8]>>(bytes: &'a B) -> CowBytes<'a> { CowBytes(Imp::new(bytes.as_ref())) }
                /// Create a new owned CowBytes.
                #[inline( always )] fn new_owned(bytes: ::boxed::Box<[u8]>) -> CowBytes<'static> { CowBytes(Imp::Owned(bytes)) }
                /// Return a borrowed byte string, regardless of whether this is an owned or borrowed byte string internally.
                #[inline( always )] pub fn as_slice(&self) -> &[u8] { self.0.as_slice() }
                /// Return an owned version of this copy-on-write byte string.
                #[inline( always )] pub fn into_owned(self) -> CowBytes<'static>
                {
                    match self.0
                    {
                        Imp::Borrowed(b) => { CowBytes::new_owned( ::boxed::Box::from(b)) }
                        Imp::Owned(b) => CowBytes::new_owned(b),
                    }
                }
            }

            impl<'a> Imp<'a>
            {
                #[inline( always )] pub fn new(bytes: &'a [u8]) -> Imp<'a> { Imp::Borrowed(bytes) }
                
                #[inline( always )] pub fn as_slice(&self) -> &[u8]
                {
                    match self 
                    {
                        Imp::Owned(ref x) => x,
                        Imp::Borrowed(x) => x,
                    }
                }
            }
        }

        pub mod ext
        {
            use ::
            {
                *,
            };
            /*
            */
            /// A trait for adding some helper routines to pointers.
            pub trait Pointer
            {
                /// Returns the distance, in units of `T`, between `self` and `origin`.
                unsafe fn distance(self, origin: Self) -> usize;

                /// Casts this pointer to `usize`.
                fn as_usize(self) -> usize;
            }

            impl<T> Pointer for *const T
            {
                unsafe fn distance(self, origin: *const T) -> usize { usize::try_from(self.offset_from(origin)).unwrap_unchecked() }

                fn as_usize(self) -> usize { self as usize }
            }

            impl<T> Pointer for *mut T 
            {
                unsafe fn distance(self, origin: *mut T) -> usize { (self as *const T).distance(origin as *const T) }

                fn as_usize(self) -> usize { (self as *const T).as_usize() }
            }
            /// A trait for adding some helper routines to raw bytes.
            pub trait Byte
            {
                /// Converts this byte to a `char` if it's ASCII. Otherwise panics.
                fn to_char(self) -> char;
            }

            impl Byte for u8
            {
                fn to_char(self) -> char
                {
                    assert!(self.is_ascii());
                    char::from(self)
                }
            }

        }

        pub mod memchr
        {
            use ::
            {
                ::iter::Rev,
                *,
            };
            /*
            */
            use super::arch::generic::memchr as generic;
            /// Search for the first occurrence of a byte in a slice.
            #[inline] pub fn memchr(needle: u8, haystack: &[u8]) -> Option<usize>
            {
                unsafe 
                {
                    generic::search_slice_with_raw(haystack, |start, end| {
                        memchr_raw(needle, start, end)
                    })
                }
            }
            /// Search for the last occurrence of a byte in a slice.
            #[inline] pub fn memrchr(needle: u8, haystack: &[u8]) -> Option<usize>
            {
                unsafe
                {
                    generic::search_slice_with_raw(haystack, |start, end| {
                        memrchr_raw(needle, start, end)
                    })
                }
            }
            /// Search for the first occurrence of two possible bytes in a haystack.
            #[inline] pub fn memchr2(needle1: u8, needle2: u8, haystack: &[u8]) -> Option<usize>
            {
                unsafe {
                    generic::search_slice_with_raw(haystack, |start, end| {
                        memchr2_raw(needle1, needle2, start, end)
                    })
                }
            }
            /// Search for the last occurrence of two possible bytes in a haystack.
            #[inline] pub fn memrchr2(needle1: u8, needle2: u8, haystack: &[u8]) -> Option<usize>
            {
                unsafe
                {
                    generic::search_slice_with_raw(haystack, |start, end| {
                        memrchr2_raw(needle1, needle2, start, end)
                    })
                }
            }
            /// Search for the first occurrence of three possible bytes in a haystack.
            #[inline] pub fn memchr3
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                haystack: &[u8],
            ) -> Option<usize>
            {
                unsafe 
                {
                    generic::search_slice_with_raw(haystack, |start, end|
                    {
                        memchr3_raw(needle1, needle2, needle3, start, end)
                    })
                }
            }
            /// Search for the last occurrence of three possible bytes in a haystack.
            #[inline] pub fn memrchr3
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                haystack: &[u8],
            ) -> Option<usize>
            {
                unsafe 
                {
                    generic::search_slice_with_raw(haystack, |start, end|
                    {
                        memrchr3_raw(needle1, needle2, needle3, start, end)
                    })
                }
            }
            /// Returns an iterator over all occurrences of the needle in a haystack.
            #[inline] pub fn memchr_iter<'h>(needle: u8, haystack: &'h [u8]) -> Memchr<'h> { Memchr::new(needle, haystack) }
            /// Returns an iterator over all occurrences of the needle in a haystack, in reverse.
            #[inline] pub fn memrchr_iter(needle: u8, haystack: &[u8]) -> Rev<Memchr<'_>>
            { Memchr::new(needle, haystack).rev() }
            /// Returns an iterator over all occurrences of the needles in a haystack.
            #[inline] pub fn memchr2_iter<'h>
            (
                needle1: u8,
                needle2: u8,
                haystack: &'h [u8],
            ) -> Memchr2<'h> 
            { Memchr2::new(needle1, needle2, haystack) }
            /// Returns an iterator over all occurrences of the needles in a haystack, in reverse.
            #[inline] pub fn memrchr2_iter
            (
                needle1: u8,
                needle2: u8,
                haystack: &[u8],
            ) -> Rev<Memchr2<'_>>
            { Memchr2::new(needle1, needle2, haystack).rev() }
            /// Returns an iterator over all occurrences of the needles in a haystack.
            #[inline] pub fn memchr3_iter<'h>
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                haystack: &'h [u8],
            ) -> Memchr3<'h>
            { Memchr3::new(needle1, needle2, needle3, haystack) }
            /// Returns an iterator over all occurrences of the needles in a haystack, in reverse.
            #[inline] pub fn memrchr3_iter
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                haystack: &[u8],
            ) -> Rev<Memchr3<'_>>
            {
                Memchr3::new(needle1, needle2, needle3, haystack).rev()
            }
            /// An iterator over all occurrences of a single byte in a haystack.
            #[derive(Clone, Debug)]
            pub struct Memchr<'h>
            {
                needle1: u8,
                it: super::arch::generic::memchr::Iter<'h>,
            }

            impl<'h> Memchr<'h>
            {
                /// Returns an iterator over all occurrences of the needle byte in the given haystack.
                #[inline] pub fn new(needle1: u8, haystack: &'h [u8]) -> Memchr<'h>
                {
                    Memchr
                    {
                        needle1,
                        it: super::arch::generic::memchr::Iter::new(haystack),
                    }
                }
            }

            impl<'h> Iterator for Memchr<'h>
            {
                type Item = usize;
                #[inline] fn next(&mut self) -> Option<usize> 
                {
                    unsafe 
                    {
                        self.it.next(|s, e| memchr_raw(self.needle1, s, e))
                    }
                }

                #[inline] fn count(self) -> usize 
                {
                    self.it.count(|s, e| 
                    {
                        unsafe { count_raw(self.needle1, s, e) }
                    })
                }

                #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
            }

            impl<'h> DoubleEndedIterator for Memchr<'h>
            {
                #[inline] fn next_back(&mut self) -> Option<usize> 
                {
                    unsafe { self.it.next_back(|s, e| memrchr_raw(self.needle1, s, e)) }
                }
            }

            impl<'h> ::iter::FusedIterator for Memchr<'h> {}

            /// An iterator over all occurrences of two possible bytes in a haystack.
            #[derive(Clone, Debug)]
            pub struct Memchr2<'h> 
            {
                needle1: u8,
                needle2: u8,
                it: super::arch::generic::memchr::Iter<'h>,
            }

            impl<'h> Memchr2<'h>
            {
                /// Returns an iterator over all occurrences of the needle bytes in the given haystack.
                #[inline] pub fn new(needle1: u8, needle2: u8, haystack: &'h [u8]) -> Memchr2<'h>
                {
                    Memchr2
                    {
                        needle1,
                        needle2,
                        it: super::arch::generic::memchr::Iter::new(haystack),
                    }
                }
            }

            impl<'h> Iterator for Memchr2<'h>
            {
                type Item = usize;
                #[inline] fn next(&mut self) -> Option<usize>
                {
                    unsafe 
                    {
                        self.it.next(|s, e| memchr2_raw(self.needle1, self.needle2, s, e))
                    }
                }

                #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.it.size_hint() }
            }

            impl<'h> DoubleEndedIterator for Memchr2<'h> 
            {
                #[inline] fn next_back(&mut self) -> Option<usize> 
                {
                    unsafe {
                        self.it.next_back(|s, e| {
                            memrchr2_raw(self.needle1, self.needle2, s, e)
                        })
                    }
                }
            }

            impl<'h> ::iter::FusedIterator for Memchr2<'h> {}

            /// An iterator over all occurrences of three possible bytes in a haystack.
            #[derive(Clone, Debug)]
            pub struct Memchr3<'h>
            {
                needle1: u8,
                needle2: u8,
                needle3: u8,
                it: super::arch::generic::memchr::Iter<'h>,
            }

            impl<'h> Memchr3<'h>
            {
                /// Returns an iterator over all occurrences of the needle bytes in the
                /// given haystack.
                #[inline] pub fn new
                (
                    needle1: u8,
                    needle2: u8,
                    needle3: u8,
                    haystack: &'h [u8],
                ) -> Memchr3<'h> {
                    Memchr3 {
                        needle1,
                        needle2,
                        needle3,
                        it: super::arch::generic::memchr::Iter::new(haystack),
                    }
                }
            }

            impl<'h> Iterator for Memchr3<'h>
            {
                type Item = usize;
                #[inline] fn next(&mut self) -> Option<usize>
                {
                    unsafe
                    {
                        self.it.next(|s, e| {
                            memchr3_raw(self.needle1, self.needle2, self.needle3, s, e)
                        })
                    }
                }

                #[inline] fn size_hint(&self) -> (usize, Option<usize>) 
                {
                    self.it.size_hint()
                }
            }

            impl<'h> DoubleEndedIterator for Memchr3<'h>
            {
                #[inline] fn next_back(&mut self) -> Option<usize>
                {
                    unsafe 
                    {
                        self.it.next_back(|s, e| {
                            memrchr3_raw(self.needle1, self.needle2, self.needle3, s, e)
                        })
                    }
                }
            }

            impl<'h> ::iter::FusedIterator for Memchr3<'h> {}

            /// memchr, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memchr_raw
            (
                needle: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8>
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memchr_raw(needle, start, end)
                }
                #[cfg(target_arch = "wasm32")]
                {
                    super::arch::wasm32::memchr::memchr_raw(needle, start, end)
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memchr_raw(needle, start, end)
                }
                #[cfg(not(any(
                    target_arch = "x86_64",
                    target_arch = "wasm32",
                    target_arch = "aarch64"
                )))]
                {
                    super::arch::all::memchr::One::new(needle).find_raw(start, end)
                }
            }
            /// memrchr, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memrchr_raw
            (
                needle: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8>
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memrchr_raw(needle, start, end)
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memrchr_raw(needle, start, end)
                }
            }
            /// memchr2, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memchr2_raw
            (
                needle1: u8,
                needle2: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8>
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memchr2_raw(needle1, needle2, start, end)
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memchr2_raw(needle1, needle2, start, end)
                }
            }
            /// memrchr2, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memrchr2_raw
            (
                needle1: u8,
                needle2: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8>
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memrchr2_raw(needle1, needle2, start, end)
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memrchr2_raw(
                        needle1, needle2, start, end,
                    )
                }
            }
            /// memchr3, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memchr3_raw
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8>
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memchr3_raw(
                        needle1, needle2, needle3, start, end,
                    )
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memchr3_raw(
                        needle1, needle2, needle3, start, end,
                    )
                }
            }
            /// memrchr3, but using raw pointers to represent the haystack.
            #[inline] unsafe fn memrchr3_raw
            (
                needle1: u8,
                needle2: u8,
                needle3: u8,
                start: *const u8,
                end: *const u8,
            ) -> Option<*const u8> 
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::memrchr3_raw(
                        needle1, needle2, needle3, start, end,
                    )
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::memrchr3_raw(
                        needle1, needle2, needle3, start, end,
                    )
                }
            }
            /// Count all matching bytes, but using raw pointers to represent the haystack.
            #[inline] unsafe fn count_raw(needle: u8, start: *const u8, end: *const u8) -> usize
            {
                #[cfg(target_arch = "x86_64")]
                {
                    super::arch::x86_64::memchr::count_raw(needle, start, end)
                }
                #[cfg(target_arch = "aarch64")]
                {
                    super::arch::aarch64::memchr::count_raw(needle, start, end)
                }
            }
        }

        pub mod memmem
        {
            /*!
            This module provides forward and reverse substring search routines. */
            use ::
            {
                *,
            };
            /*
            // This is exported here for use in the super::arch::all::twoway
            // implementation. This is essentially an abstraction breaker. Namely, the
            // public API of twoway doesn't support providing a prefilter, but its crate
            // internal API does. The main reason for this is that I didn't want to do the
            // API design required to support it without a concrete use case.
            pub use crate::memmem::searcher::Pre;

            use crate::{
                arch::all::{
                    packedpair::{DefaultFrequencyRank, HeuristicFrequencyRank},
                    rabinkarp,
                },
                cow::CowBytes,
                memmem::searcher::{PrefilterState, Searcher, SearcherRev},
            };
            */
            pub mod searcher
            {
                use ::
                {
                    *,
                };
                /*
                use super::arch::all::{
                    packedpair::{HeuristicFrequencyRank, Pair},
                    rabinkarp, twoway,
                };

                #[cfg(target_arch = "aarch64")]
                use super::arch::aarch64::neon::packedpair as neon;
                #[cfg(target_arch = "wasm32")]
                use super::arch::wasm32::simd128::packedpair as simd128;
                #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                use super::arch::x86_64::{
                    avx2::packedpair as avx2, sse2::packedpair as sse2,
                };
                */
                /// A "meta" substring searcher.
                ///
                /// To a first approximation, this chooses what it believes to be the "best"
                /// substring search implemnetation based on the needle at construction time.
                /// Then, every call to `find` will execute that particular implementation. To
                /// a second approximation, multiple substring search algorithms may be used,
                /// depending on the haystack. For example, for supremely short haystacks,
                /// Rabin-Karp is typically used.
                ///
                /// See the documentation on `Prefilter` for an explanation of the dispatching
                /// mechanism. The quick summary is that an enum has too much overhead and
                /// we can't use dynamic dispatch via traits because we need to work in a
                /// core-only environment. (Dynamic dispatch works in core-only, but you
                /// need `&dyn Trait` and we really need a `Box<dyn Trait>` here. The latter
                /// requires `alloc`.) So instead, we use a union and an appropriately paired
                /// free function to read from the correct field on the union and execute the
                /// chosen substring search implementation.
                #[derive(Clone)]
                pub struct Searcher 
                {
                    call: SearcherKindFn,
                    kind: SearcherKind,
                    rabinkarp: rabinkarp::Finder,
                }

                impl Searcher 
                {
                    /// Creates a new "meta" substring searcher that attempts to choose the
                    /// best algorithm based on the needle, heuristics and what the current
                    /// target supports.
                    #[inline] pub fn new<R: HeuristicFrequencyRank>
                    (
                        prefilter: PrefilterConfig,
                        ranker: R,
                        needle: &[u8],
                    ) -> Searcher 
                    {
                        let rabinkarp = rabinkarp::Finder::new(needle);
                        if needle.len() <= 1 {
                            return if needle.is_empty() {
                                trace!("building empty substring searcher");
                                Searcher {
                                    call: searcher_kind_empty,
                                    kind: SearcherKind { empty: () },
                                    rabinkarp,
                                }
                            } else {
                                trace!("building one-byte substring searcher");
                                debug_assert_eq!(1, needle.len());
                                Searcher {
                                    call: searcher_kind_one_byte,
                                    kind: SearcherKind { one_byte: needle[0] },
                                    rabinkarp,
                                }
                            };
                        }
                        let pair = match Pair::with_ranker(needle, &ranker) {
                            Some(pair) => pair,
                            None => return Searcher::twoway(needle, rabinkarp, None),
                        };
                        debug_assert_ne!(
                            pair.index1(),
                            pair.index2(),
                            "pair offsets should not be equivalent"
                        );
                        #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                        {
                            if let Some(pp) = avx2::Finder::with_pair(needle, pair) {
                                if do_packed_search(needle) {
                                    trace!("building x86_64 AVX2 substring searcher");
                                    let kind = SearcherKind { avx2: pp };
                                    Searcher { call: searcher_kind_avx2, kind, rabinkarp }
                                } else if prefilter.is_none() {
                                    Searcher::twoway(needle, rabinkarp, None)
                                } else {
                                    let prestrat = Prefilter::avx2(pp, needle);
                                    Searcher::twoway(needle, rabinkarp, Some(prestrat))
                                }
                            } else if let Some(pp) = sse2::Finder::with_pair(needle, pair) {
                                if do_packed_search(needle) {
                                    trace!("building x86_64 SSE2 substring searcher");
                                    let kind = SearcherKind { sse2: pp };
                                    Searcher { call: searcher_kind_sse2, kind, rabinkarp }
                                } else if prefilter.is_none() {
                                    Searcher::twoway(needle, rabinkarp, None)
                                } else {
                                    let prestrat = Prefilter::sse2(pp, needle);
                                    Searcher::twoway(needle, rabinkarp, Some(prestrat))
                                }
                            } else if prefilter.is_none() {
                                Searcher::twoway(needle, rabinkarp, None)
                            } else {
                                // We're pretty unlikely to get to this point, but it is
                                // possible to be running on x86_64 without SSE2. Namely, it's
                                // really up to the OS whether it wants to support vector
                                // registers or not.
                                let prestrat = Prefilter::fallback(ranker, pair, needle);
                                Searcher::twoway(needle, rabinkarp, prestrat)
                            }
                        }
                        #[cfg(target_arch = "aarch64")]
                        {
                            if let Some(pp) = neon::Finder::with_pair(needle, pair) {
                                if do_packed_search(needle) {
                                    trace!("building aarch64 neon substring searcher");
                                    let kind = SearcherKind { neon: pp };
                                    Searcher { call: searcher_kind_neon, kind, rabinkarp }
                                } else if prefilter.is_none() {
                                    Searcher::twoway(needle, rabinkarp, None)
                                } else {
                                    let prestrat = Prefilter::neon(pp, needle);
                                    Searcher::twoway(needle, rabinkarp, Some(prestrat))
                                }
                            } else if prefilter.is_none() {
                                Searcher::twoway(needle, rabinkarp, None)
                            } else {
                                let prestrat = Prefilter::fallback(ranker, pair, needle);
                                Searcher::twoway(needle, rabinkarp, prestrat)
                            }
                        }
                    }
                    /// Creates a new searcher that always uses the Two-Way algorithm. This is
                    /// typically used when vector algorithms are unavailable or inappropriate.
                    /// (For example, when the needle is "too long.")
                    ///
                    /// If a prefilter is given, then the searcher returned will be accelerated
                    /// by the prefilter.
                    #[inline] fn twoway
                    (
                        needle: &[u8],
                        rabinkarp: rabinkarp::Finder,
                        prestrat: Option<Prefilter>,
                    ) -> Searcher 
                    {
                        let finder = twoway::Finder::new(needle);
                        match prestrat {
                            None => {
                                trace!("building scalar two-way substring searcher");
                                let kind = SearcherKind { two_way: finder };
                                Searcher { call: searcher_kind_two_way, kind, rabinkarp }
                            }
                            Some(prestrat) => {
                                trace!(
                                    "building scalar two-way \
                                    substring searcher with a prefilter"
                                );
                                let two_way_with_prefilter =
                                    TwoWayWithPrefilter { finder, prestrat };
                                let kind = SearcherKind { two_way_with_prefilter };
                                Searcher {
                                    call: searcher_kind_two_way_with_prefilter,
                                    kind,
                                    rabinkarp,
                                }
                            }
                        }
                    }
                    /// Searches the given haystack for the given needle.
                    #[inline( always )] pub fn find
                    (
                        &self,
                        prestate: &mut PrefilterState,
                        haystack: &[u8],
                        needle: &[u8],
                    ) -> Option<usize>
                    {
                        if haystack.len() < needle.len() {
                            None
                        } else 
                        {
                            unsafe { (self.call)(self, prestate, haystack, needle) }
                        }
                    }
                }

                impl ::fmt::Debug for Searcher 
                {
                    fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result 
                    {
                        f.debug_struct("Searcher")
                        .field("call", &"<searcher function>")
                        .field("kind", &"<searcher kind union>")
                        .field("rabinkarp", &self.rabinkarp)
                        .finish()
                    }
                }
                /// A union indicating one of several possible substring search implementations
                /// that are in active use.
                #[derive(Clone, Copy)]
                union SearcherKind
                {
                    empty: (),
                    one_byte: u8,
                    two_way: twoway::Finder,
                    two_way_with_prefilter: TwoWayWithPrefilter,
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    sse2: ::arch::x86_64::sse2::packedpair::Finder,
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    avx2: ::arch::x86_64::avx2::packedpair::Finder,
                    #[cfg(target_arch = "wasm32")]
                    simd128: ::arch::wasm32::simd128::packedpair::Finder,
                    #[cfg(target_arch = "aarch64")]
                    neon: ::arch::aarch64::neon::packedpair::Finder,
                }
                /// A two-way substring searcher with a prefilter.
                #[derive(Copy, Clone, Debug)]
                struct TwoWayWithPrefilter
                {
                    finder: twoway::Finder,
                    prestrat: Prefilter,
                }
                /// The type of a substring search function.
                type SearcherKindFn = unsafe fn
                (
                    searcher: &Searcher,
                    prestate: &mut PrefilterState,
                    haystack: &[u8],
                    needle: &[u8],
                ) -> Option<usize>;
                /// Reads from the `empty` field of `SearcherKind` to handle the case of
                /// searching for the empty needle. Works on all platforms.
                unsafe fn searcher_kind_empty
                (
                    _searcher: &Searcher,
                    _prestate: &mut PrefilterState,
                    _haystack: &[u8],
                    _needle: &[u8],
                ) -> Option<usize> { Some(0) }
                /// Reads from the `one_byte` field of `SearcherKind` to handle the case of searching for a single byte needle.
                unsafe fn searcher_kind_one_byte
                (
                    searcher: &Searcher,
                    _prestate: &mut PrefilterState,
                    haystack: &[u8],
                    _needle: &[u8],
                ) -> Option<usize> 
                {
                    let needle = searcher.kind.one_byte;
                    ::mem::chr(needle, haystack)
                }
                /// Reads from the `two_way` field of `SearcherKind` to handle the case of 
                /// searching for an arbitrary needle without prefilter acceleration.
                unsafe fn searcher_kind_two_way
                (
                    searcher: &Searcher,
                    _prestate: &mut PrefilterState,
                    haystack: &[u8],
                    needle: &[u8],
                ) -> Option<usize> 
                {
                    if rabinkarp::is_fast(haystack, needle) { searcher.rabinkarp.find(haystack, needle) }                    
                    else { searcher.kind.two_way.find(haystack, needle) }
                }
                /// Reads from the `two_way_with_prefilter` field of `SearcherKind` to handle
                /// the case of searching for an arbitrary needle with prefilter acceleration.
                unsafe fn searcher_kind_two_way_with_prefilter
                (
                    searcher: &Searcher,
                    prestate: &mut PrefilterState,
                    haystack: &[u8],
                    needle: &[u8],
                ) -> Option<usize> 
                {
                    if rabinkarp::is_fast(haystack, needle)
                    {
                        searcher.rabinkarp.find(haystack, needle)
                    }
                    else
                    {
                        let TwoWayWithPrefilter { ref finder, ref prestrat } =
                            searcher.kind.two_way_with_prefilter;
                        let pre = Pre { prestate, prestrat };
                        finder.find_with_prefilter(Some(pre), haystack, needle)
                    }
                }
                /// Reads from the `sse2` field of `SearcherKind` to execute the x86_64 SSE2
                /// vectorized substring search implementation.
                #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                unsafe fn searcher_kind_sse2
                (
                    searcher: &Searcher,
                    _prestate: &mut PrefilterState,
                    haystack: &[u8],
                    needle: &[u8],
                ) -> Option<usize>
                {
                    let finder = &searcher.kind.sse2;

                    if haystack.len() < finder.min_haystack_len() { searcher.rabinkarp.find(haystack, needle) }
                    else { finder.find(haystack, needle) }
                }
                /// Reads from the `avx2` field of `SearcherKind` to execute the x86_64 AVX2
                /// vectorized substring search implementation.
                #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                unsafe fn searcher_kind_avx2
                (
                    searcher: &Searcher,
                    _prestate: &mut PrefilterState,
                    haystack: &[u8],
                    needle: &[u8],
                ) -> Option<usize>
                {
                    let finder = &searcher.kind.avx2;

                    if haystack.len() < finder.min_haystack_len() { searcher.rabinkarp.find(haystack, needle) }
                    else { finder.find(haystack, needle) }
                }
                /// A reverse substring searcher.
                #[derive(Clone, Debug)]
                pub struct SearcherRev 
                {
                    kind: SearcherRevKind,
                    rabinkarp: rabinkarp::FinderRev,
                }
                /// The kind of the reverse searcher.
                #[derive(Clone, Debug)]
                enum SearcherRevKind 
                {
                    Empty,
                    OneByte { needle: u8 },
                    TwoWay { finder: twoway::FinderRev },
                }

                impl SearcherRev 
                {
                    /// Creates a new searcher for finding occurrences of the given needle in reverse.
                    #[inline] pub fn new(needle: &[u8]) -> SearcherRev
                    {
                        let kind = if needle.len() <= 1 
                        {
                            if needle.is_empty() {
                                trace!("building empty reverse substring searcher");
                                SearcherRevKind::Empty
                            } else {
                                trace!("building one-byte reverse substring searcher");
                                debug_assert_eq!(1, needle.len());
                                SearcherRevKind::OneByte { needle: needle[0] }
                            }
                        }
                        else 
                        {
                            trace!("building scalar two-way reverse substring searcher");
                            let finder = twoway::FinderRev::new(needle);
                            SearcherRevKind::TwoWay { finder }
                        };

                        let rabinkarp = rabinkarp::FinderRev::new(needle);
                        SearcherRev { kind, rabinkarp }
                    }
                    /// Searches the given haystack for the last occurrence of the given needle.
                    #[inline] pub fn rfind
                    (
                        &self,
                        haystack: &[u8],
                        needle: &[u8],
                    ) -> Option<usize>
                    {
                        if haystack.len() < needle.len() { return None; }

                        match self.kind
                        {
                            SearcherRevKind::Empty => Some(haystack.len()),
                            SearcherRevKind::OneByte { needle } => { ::mem::chr(needle, haystack) }
                            SearcherRevKind::TwoWay { ref finder } => 
                            {
                                if rabinkarp::is_fast(haystack, needle) {
                                    self.rabinkarp.rfind(haystack, needle)
                                } else {
                                    finder.rfind(haystack, needle)
                                }
                            }
                        }
                    }
                }
                /// Prefilter controls whether heuristics are used to accelerate searching.
                #[non_exhaustive] #[derive(Clone, Copy, Debug)]                
                pub enum PrefilterConfig 
                {
                    /// Never used a prefilter in substring search.
                    None,
                    /// Automatically detect whether a heuristic prefilter should be used.
                    Auto,
                }

                impl Default for PrefilterConfig 
                {
                    fn default() -> PrefilterConfig { PrefilterConfig::Auto }
                }

                impl PrefilterConfig 
                {
                    /// Returns true when this prefilter is set to the `None` variant.
                    fn is_none(&self) -> bool { matches!(*self, PrefilterConfig::None) }
                }
                /// The implementation of a prefilter.
                #[derive(Clone, Copy)]
                struct Prefilter
                {
                    call: PrefilterKindFn,
                    kind: PrefilterKind,
                    rarest_byte: u8,
                    rarest_offset: u8,
                }

                impl Prefilter
                {
                    /// Return a "fallback" prefilter, but only if it is believed to be
                    /// effective.
                    #[inline] fn fallback<R: HeuristicFrequencyRank>(
                        ranker: R,
                        pair: Pair,
                        needle: &[u8],
                    ) -> Option<Prefilter> {
                        /// The maximum frequency rank permitted for the fallback prefilter.
                        /// If the rarest byte in the needle has a frequency rank above this
                        /// value, then no prefilter is used if the fallback prefilter would
                        /// otherwise be selected.
                        const MAX_FALLBACK_RANK: u8 = 250;

                        trace!("building fallback prefilter");
                        let rarest_offset = pair.index1();
                        let rarest_byte = needle[usize::from(rarest_offset)];
                        let rarest_rank = ranker.rank(rarest_byte);
                        if rarest_rank > MAX_FALLBACK_RANK {
                            None
                        } else {
                            let finder = super::arch::all::packedpair::Finder::with_pair(
                                needle,
                                pair.clone(),
                            )?;
                            let call = prefilter_kind_fallback;
                            let kind = PrefilterKind { fallback: finder };
                            Some(Prefilter { call, kind, rarest_byte, rarest_offset })
                        }
                    }
                    /// Return a prefilter using a x86_64 SSE2 vector algorithm.
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    #[inline] fn sse2(finder: sse2::Finder, needle: &[u8]) -> Prefilter {
                        trace!("building x86_64 SSE2 prefilter");
                        let rarest_offset = finder.pair().index1();
                        let rarest_byte = needle[usize::from(rarest_offset)];
                        Prefilter {
                            call: prefilter_kind_sse2,
                            kind: PrefilterKind { sse2: finder },
                            rarest_byte,
                            rarest_offset,
                        }
                    }
                    /// Return a prefilter using a x86_64 AVX2 vector algorithm.
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    #[inline] fn avx2(finder: avx2::Finder, needle: &[u8]) -> Prefilter {
                        trace!("building x86_64 AVX2 prefilter");
                        let rarest_offset = finder.pair().index1();
                        let rarest_byte = needle[usize::from(rarest_offset)];
                        Prefilter {
                            call: prefilter_kind_avx2,
                            kind: PrefilterKind { avx2: finder },
                            rarest_byte,
                            rarest_offset,
                        }
                    }
                    /// Return a prefilter using a wasm32 simd128 vector algorithm.
                    #[cfg(target_arch = "wasm32")]
                    #[inline] fn simd128(finder: simd128::Finder, needle: &[u8]) -> Prefilter {
                        trace!("building wasm32 simd128 prefilter");
                        let rarest_offset = finder.pair().index1();
                        let rarest_byte = needle[usize::from(rarest_offset)];
                        Prefilter {
                            call: prefilter_kind_simd128,
                            kind: PrefilterKind { simd128: finder },
                            rarest_byte,
                            rarest_offset,
                        }
                    }
                    /// Return a prefilter using a aarch64 neon vector algorithm.
                    #[cfg(target_arch = "aarch64")]
                    #[inline] fn neon(finder: neon::Finder, needle: &[u8]) -> Prefilter {
                        trace!("building aarch64 neon prefilter");
                        let rarest_offset = finder.pair().index1();
                        let rarest_byte = needle[usize::from(rarest_offset)];
                        Prefilter {
                            call: prefilter_kind_neon,
                            kind: PrefilterKind { neon: finder },
                            rarest_byte,
                            rarest_offset,
                        }
                    }
                    /// Return a *candidate* position for a match.
                    ///
                    /// When this returns an offset, it implies that a match could begin at
                    /// that offset, but it may not. That is, it is possible for a false
                    /// positive to be returned.
                    ///
                    /// When `None` is returned, then it is guaranteed that there are no
                    /// matches for the needle in the given haystack. That is, it is impossible
                    /// for a false negative to be returned.
                    ///
                    /// The purpose of this routine is to look for candidate matching positions
                    /// as quickly as possible before running a (likely) slower confirmation
                    /// step.
                    #[inline] fn find(&self, haystack: &[u8]) -> Option<usize> {
                        // SAFETY: By construction, we've ensured that the function in
                        // `self.call` is properly paired with the union used in `self.kind`.
                        unsafe { (self.call)(self, haystack) }
                    }
                    /// A "simple" prefilter that just looks for the occurrence of the rarest
                    /// byte from the needle. This is generally only used for very small
                    /// haystacks.
                    #[inline] fn find_simple(&self, haystack: &[u8]) -> Option<usize> {
                        // We don't use crate::memchr here because the haystack should be small
                        // enough that memchr won't be able to use vector routines anyway. So
                        // we just skip straight to the fallback implementation which is likely
                        // faster. (A byte-at-a-time loop is only used when the haystack is
                        // smaller than `size_of::<usize>()`.)
                        super::arch::all::memchr::One::new(self.rarest_byte)
                            .find(haystack)
                            .map(|i| i.saturating_sub(usize::from(self.rarest_offset)))
                    }
                }

                impl ::fmt::Debug for Prefilter 
                {
                    fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result {
                        f.debug_struct("Prefilter")
                            .field("call", &"<prefilter function>")
                            .field("kind", &"<prefilter kind union>")
                            .field("rarest_byte", &self.rarest_byte)
                            .field("rarest_offset", &self.rarest_offset)
                            .finish()
                    }
                }
                /// A union indicating one of several possible prefilters that are in active use.
                #[derive(Clone, Copy)]
                union PrefilterKind
                {
                    fallback: ::arch::all::packedpair::Finder,
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    sse2: ::arch::x86_64::sse2::packedpair::Finder,
                    #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                    avx2: ::arch::x86_64::avx2::packedpair::Finder,
                }
                /// The type of a prefilter function.
                type PrefilterKindFn = unsafe fn(strat: &Prefilter, haystack: &[u8]) -> Option<usize>;
                /// Reads from the `fallback` field of `PrefilterKind` to execute the fallback prefilter.
                unsafe fn prefilter_kind_fallback
                (
                    strat: &Prefilter,
                    haystack: &[u8],
                ) -> Option<usize>
                { strat.kind.fallback.find_prefilter(haystack) }
                /// Reads from the `sse2` field of `PrefilterKind` to execute the x86_64 SSE2 prefilter.
                #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                unsafe fn prefilter_kind_sse2
                (
                    strat: &Prefilter,
                    haystack: &[u8],
                ) -> Option<usize>
                {
                    let finder = &strat.kind.sse2;
                    if haystack.len() < finder.min_haystack_len() { strat.find_simple(haystack) }
                    else { finder.find_prefilter(haystack) }
                }
                /// Reads from the `avx2` field of `PrefilterKind` to execute the x86_64 AVX2 prefilter.
                #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
                unsafe fn prefilter_kind_avx2
                (
                    strat: &Prefilter,
                    haystack: &[u8],
                ) -> Option<usize>
                {
                    let finder = &strat.kind.avx2;
                    if haystack.len() < finder.min_haystack_len()
                    {
                        strat.find_simple(haystack)
                    } 
                    else
                    {
                        finder.find_prefilter(haystack)
                    }
                }
                /// PrefilterState tracks state associated with the effectiveness of a prefilter.
                #[derive(Clone, Copy, Debug)]
                pub struct PrefilterState
                {
                    /// The number of skips that has been executed.
                    skips: u32,
                    /// The total number of bytes that have been skipped.
                    skipped: u32,
                }

                impl PrefilterState
                {
                    /// The minimum number of skip attempts to try before considering whether a prefilter is effective or not.
                    const MIN_SKIPS: u32 = 50;
                    /// The minimum amount of bytes that skipping must average.
                    const MIN_SKIP_BYTES: u32 = 8;
                    /// Create a fresh prefilter state.
                    #[inline] pub fn new() -> PrefilterState { PrefilterState { skips: 1, skipped: 0 } }
                    /// Update this state with the number of bytes skipped on the last invocation of the prefilter.
                    #[inline] fn update(&mut self, skipped: usize)
                    {
                        self.skips = self.skips.saturating_add(1);
                        self.skipped = match u32::try_from(skipped)
                        {
                            Err(_) => ::u32::MAX,
                            Ok(skipped) => self.skipped.saturating_add(skipped),
                        };
                    }
                    /// Return true if and only if this state indicates that a prefilter is still effective.
                    #[inline] fn is_effective(&mut self) -> bool
                    {
                        if self.is_inert() { return false; }

                        if self.skips() < PrefilterState::MIN_SKIPS { return true; }

                        if self.skipped >= PrefilterState::MIN_SKIP_BYTES * self.skips() { return true; }
                        
                        self.skips = 0;
                        false
                    }
                    /// Returns true if the prefilter this state represents should no longer be used.
                    #[inline] fn is_inert(&self) -> bool { self.skips == 0 }
                    /// Returns the total number of times the prefilter has been used.
                    #[inline] fn skips(&self) -> u32 { self.skips.saturating_sub(1) }
                }
                /// A combination of prefilter effectiveness state and the prefilter itself.
                #[derive(Debug)]
                pub struct Pre<'a>
                {
                    /// State that tracks the effectiveness of a prefilter.
                    prestate: &'a mut PrefilterState,
                    /// The actual prefilter.
                    prestrat: &'a Prefilter,
                }

                impl<'a> Pre<'a>
                {
                    /// Call this prefilter on the given haystack with the given needle.
                    #[inline] pub fn find(&mut self, haystack: &[u8]) -> Option<usize>
                    {
                        let result = self.prestrat.find(haystack);
                        self.prestate.update(result.unwrap_or(haystack.len()));
                        result
                    }
                    /// Return true if and only if this prefilter should be used.
                    #[inline] pub fn is_effective(&mut self) -> bool { self.prestate.is_effective() }
                }
                /// Returns true if the needle has the right characteristics for a vector algorithm to handle the entirety of substring search.
                #[inline] fn do_packed_search(needle: &[u8]) -> bool
                {
                    /// The minimum length of a needle required for this algorithm.
                    const MIN_LEN: usize = 2;
                    /// The maximum length of a needle required for this algorithm.
                    const MAX_LEN: usize = 32;
                    MIN_LEN <= needle.len() && needle.len() <= MAX_LEN
                }

            }
            /// Returns an iterator over all non-overlapping occurrences of a substring in a haystack.
            #[inline] pub fn find_iter<'h, 'n, N: 'n + ?Sized + AsRef<[u8]>>( haystack: &'h [u8], needle: &'n N ) -> FindIter<'h, 'n>
            { FindIter::new(haystack, Finder::new(needle)) }
            /// Returns a reverse iterator over all non-overlapping occurrences of a substring in a haystack.
            #[inline] pub fn rfind_iter<'h, 'n, N: 'n + ?Sized + AsRef<[u8]>>( haystack: &'h [u8], needle: &'n N ) -> FindRevIter<'h, 'n>
            { FindRevIter::new(haystack, FinderRev::new(needle)) }
            /// Returns the index of the first occurrence of the given needle.
            #[inline] pub fn find(haystack: &[u8], needle: &[u8]) -> Option<usize>
            {
                if haystack.len() < 64 { rabinkarp::Finder::new(needle).find(haystack, needle) }
                else { Finder::new(needle).find(haystack) }
            }
            /// Returns the index of the last occurrence of the given needle.
            #[inline] pub fn rfind(haystack: &[u8], needle: &[u8]) -> Option<usize>
            {
                if haystack.len() < 64 { rabinkarp::FinderRev::new(needle).rfind(haystack, needle) }
                else { FinderRev::new(needle).rfind(haystack) }
            }
            /// An iterator over non-overlapping substring matches.
            #[derive(Debug)]
            pub struct FindIter<'h, 'n>
            {
                haystack: &'h [u8],
                prestate: PrefilterState,
                finder: Finder<'n>,
                pos: usize,
            }

            impl<'h, 'n> FindIter<'h, 'n>
            {
                #[inline( always )] pub fn new
                (
                    haystack: &'h [u8],
                    finder: Finder<'n>,
                ) -> FindIter<'h, 'n>
                {
                    let prestate = PrefilterState::new();
                    FindIter { haystack, prestate, finder, pos: 0 }
                }
                /// Convert this iterator into its owned variant, such that it no longer borrows the finder and needle.
                #[inline] pub fn into_owned(self) -> FindIter<'h, 'static>
                {
                    FindIter
                    {
                        haystack: self.haystack,
                        prestate: self.prestate,
                        finder: self.finder.into_owned(),
                        pos: self.pos,
                    }
                }
            }

            impl<'h, 'n> Iterator for FindIter<'h, 'n>
            {
                type Item = usize;
                fn next(&mut self) -> Option<usize>
                {
                    let needle = self.finder.needle();
                    let haystack = self.haystack.get(self.pos..)?;
                    let idx = self.finder.searcher.find(&mut self.prestate, haystack, needle)?;
                    let pos = self.pos + idx;
                    self.pos = pos + needle.len().max(1);
                    Some(pos)
                }

                fn size_hint(&self) -> (usize, Option<usize>)
                {
                    match self.haystack.len().checked_sub(self.pos)
                    {
                        None => (0, Some(0)),
                        Some(haystack_len) => match self.finder.needle().len()
                        {
                            0 =>
                            (
                                haystack_len.saturating_add(1),
                                haystack_len.checked_add(1),
                            ),
                            needle_len => (0, Some(haystack_len / needle_len)),
                        },
                    }
                }
            }
            /// An iterator over non-overlapping substring matches in reverse.
            #[derive(Debug)]
            pub struct FindRevIter<'h, 'n>
            {
                haystack: &'h [u8],
                finder: FinderRev<'n>,
                /// When searching with an empty needle, this gets set to `None` after
                /// we've yielded the last element at `0`.
                pos: Option<usize>,
            }

            impl<'h, 'n> FindRevIter<'h, 'n>
            {
                #[inline( always )] pub fn new
                (
                    haystack: &'h [u8],
                    finder: FinderRev<'n>,
                ) -> FindRevIter<'h, 'n>
                {
                    let pos = Some(haystack.len());
                    FindRevIter { haystack, finder, pos }
                }
                /// Convert this iterator into its owned variant, such that it no longer borrows the finder and needle.
                #[inline] pub fn into_owned(self) -> FindRevIter<'h, 'static>
                {
                    FindRevIter {
                        haystack: self.haystack,
                        finder: self.finder.into_owned(),
                        pos: self.pos,
                    }
                }
            }

            impl<'h, 'n> Iterator for FindRevIter<'h, 'n>
            {
                type Item = usize;
                fn next(&mut self) -> Option<usize>
                {
                    let pos = match self.pos
                    {
                        None => return None,
                        Some(pos) => pos,
                    };

                    let result = self.finder.rfind(&self.haystack[..pos]);
                    match result
                    {
                        None => None,
                        Some(i) =>
                        {
                            if pos == i { self.pos = pos.checked_sub(1); }
                            else { self.pos = Some(i); }

                            Some(i)
                        }
                    }
                }
            }
            /// A single substring searcher fixed to a particular needle.
            #[derive(Clone, Debug)]
            pub struct Finder<'n>
             {
                needle: CowBytes<'n>,
                searcher: Searcher,
            }

            impl<'n> Finder<'n>
            {
                /// Create a new finder for the given needle.
                #[inline] pub fn new<B: ?Sized + AsRef<[u8]>>(needle: &'n B) -> Finder<'n> { FinderBuilder::new().build_forward(needle) }
                /// Returns the index of the first occurrence of this needle in the given haystack.
                #[inline] pub fn find(&self, haystack: &[u8]) -> Option<usize>
                {
                    let mut prestate = PrefilterState::new();
                    let needle = self.needle.as_slice();
                    self.searcher.find(&mut prestate, haystack, needle)
                }
                /// Returns an iterator over all occurrences of a substring in a haystack.
                #[inline] pub fn find_iter<'a, 'h>
                (
                    &'a self,
                    haystack: &'h [u8],
                ) -> FindIter<'h, 'a>
                {
                    FindIter::new(haystack, self.as_ref())
                }
                /// Convert this finder into its owned variant, such that it no longer borrows the needle.
                #[inline] pub fn into_owned(self) -> Finder<'static>
                {
                    Finder
                    {
                        needle: self.needle.into_owned(),
                        searcher: self.searcher.clone(),
                    }
                }
                /// Convert this finder into its borrowed variant.
                #[inline] pub fn as_ref(&self) -> Finder<'_>
                {
                    Finder
                    {
                        needle: CowBytes::new(self.needle()),
                        searcher: self.searcher.clone(),
                    }
                }
                /// Returns the needle that this finder searches for.
                #[inline] pub fn needle(&self) -> &[u8]
                {
                    self.needle.as_slice()
                }
            }
            /// A single substring reverse searcher fixed to a particular needle.
            #[derive(Clone, Debug)]
            pub struct FinderRev<'n>
            {
                needle: CowBytes<'n>,
                searcher: SearcherRev,
            }

            impl<'n> FinderRev<'n> 
            {
                /// Create a new reverse finder for the given needle.
                #[inline] pub fn new<B: ?Sized + AsRef<[u8]>>(needle: &'n B) -> FinderRev<'n>
                {
                    FinderBuilder::new().build_reverse(needle)
                }
                /// Returns the index of the last occurrence of this needle in the given haystack.
                pub fn rfind<B: AsRef<[u8]>>(&self, haystack: B) -> Option<usize>
                {
                    self.searcher.rfind(haystack.as_ref(), self.needle.as_slice())
                }
                /// Returns a reverse iterator over all occurrences of a substring in a haystack.
                #[inline] pub fn rfind_iter<'a, 'h>
                (
                    &'a self,
                    haystack: &'h [u8],
                ) -> FindRevIter<'h, 'a>
                {
                    FindRevIter::new(haystack, self.as_ref())
                }
                /// Convert this finder into its owned variant, such that it no longer borrows the needle.
                #[inline] pub fn into_owned(self) -> FinderRev<'static>
                {
                    FinderRev
                    {
                        needle: self.needle.into_owned(),
                        searcher: self.searcher.clone(),
                    }
                }
                /// Convert this finder into its borrowed variant.
                #[inline] pub fn as_ref(&self) -> FinderRev<'_>
                {
                    FinderRev
                    {
                        needle: CowBytes::new(self.needle()),
                        searcher: self.searcher.clone(),
                    }
                }
                /// Returns the needle that this finder searches for.
                #[inline] pub fn needle(&self) -> &[u8] { self.needle.as_slice() }
            }
            /// A builder for constructing non-default forward or reverse memmem finders.
            #[derive(Clone, Debug, Default)]
            pub struct FinderBuilder
            {
                prefilter: Prefilter,
            }

            impl FinderBuilder
            {
                /// Create a new finder builder with default settings.
                pub fn new() -> FinderBuilder { FinderBuilder::default() }
                /// Build a forward finder using the given needle from the current settings.
                pub fn build_forward<'n, B: ?Sized + AsRef<[u8]>>
                (
                    &self,
                    needle: &'n B,
                ) -> Finder<'n>
                {
                    self.build_forward_with_ranker(DefaultFrequencyRank, needle)
                }
                /// Build a forward finder using the given needle and a 
                /// custom heuristic for determining the frequency of a given byte in the dataset.
                pub fn build_forward_with_ranker
                <
                    'n,
                    R: HeuristicFrequencyRank,
                    B: ?Sized + AsRef<[u8]>,
                >
                (
                    &self,
                    ranker: R,
                    needle: &'n B,
                ) -> Finder<'n>
                {
                    let needle = needle.as_ref();
                    Finder
                    {
                        needle: CowBytes::new(needle),
                        searcher: Searcher::new(self.prefilter, ranker, needle),
                    }
                }
                /// Build a reverse finder using the given needle from the current settings.
                pub fn build_reverse<'n, B: ?Sized + AsRef<[u8]>>
                (
                    &self,
                    needle: &'n B,
                ) -> FinderRev<'n>
                {
                    let needle = needle.as_ref();
                    FinderRev
                    {
                        needle: CowBytes::new(needle),
                        searcher: SearcherRev::new(needle),
                    }
                }
                /// Configure the prefilter setting for the finder.
                pub fn prefilter(&mut self, prefilter: Prefilter) -> &mut FinderBuilder
                {
                    self.prefilter = prefilter;
                    self
                }
            }
        }

        pub mod vector
        {
            /// Traits for describing vector operations used by vectorized searchers.
            use ::
            {
                *,
            };
            /*
            */
            /// A trait for describing vector operations used by vectorized searchers.
            pub trait Vector: Copy + ::fmt::Debug
            {
                /// The number of bits in the vector.
                const BITS: usize;
                /// The number of bytes in the vector.
                const BYTES: usize;
                /// The bits that must be zero in order for a `*const u8` pointer to be correctly aligned to read vector values.
                const ALIGN: usize;
                /// The type of the value returned by `Vector::movemask`.
                type Mask: MoveMask;
                /// Create a vector with 8-bit lanes with the given byte repeated into each lane.
                unsafe fn splat(byte: u8) -> Self;
                /// Read a vector-size number of bytes from the given pointer.
                unsafe fn load_aligned(data: *const u8) -> Self;
                /// Read a vector-size number of bytes from the given pointer.
                unsafe fn load_unaligned(data: *const u8) -> Self;
                /// _mm_movemask_epi8 or _mm256_movemask_epi8
                unsafe fn movemask(self) -> Self::Mask;
                /// _mm_cmpeq_epi8 or _mm256_cmpeq_epi8
                unsafe fn cmpeq(self, vector2: Self) -> Self;
                /// _mm_and_si128 or _mm256_and_si256
                unsafe fn and(self, vector2: Self) -> Self;
                /// _mm_or or _mm256_or_si256
                unsafe fn or(self, vector2: Self) -> Self;
                /// Returns true if and only if `Self::movemask` would return a mask that contains at least one non-zero bit.
                unsafe fn movemask_will_have_non_zero(self) -> bool { self.movemask().has_non_zero() }
            }
            /// A trait that abstracts over a vector-to-scalar operation called "move mask."
            pub trait MoveMask: Copy + ::fmt::Debug 
            {
                /// Return a mask that is all zeros except for the least significant `n` lanes in a corresponding vector.
                fn all_zeros_except_least_significant(n: usize) -> Self;
                /// Returns true if and only if this mask has a a non-zero bit anywhere.
                fn has_non_zero(self) -> bool;
                /// Returns the number of bits set to 1 in this mask.
                fn count_ones(self) -> usize;
                /// Does a bitwise `and` operation between `self` and `other`.
                fn and(self, other: Self) -> Self;
                /// Does a bitwise `or` operation between `self` and `other`.
                fn or(self, other: Self) -> Self;
                /// Returns a mask that is equivalent to `self` but with the least significant 1-bit set to 0.
                fn clear_least_significant_bit(self) -> Self;
                /// Returns the offset of the first non-zero lane this mask represents.
                fn first_offset(self) -> usize;
                /// Returns the offset of the last non-zero lane this mask represents.
                fn last_offset(self) -> usize;
            }
            /// This is a "sensible" movemask implementation where each bit represents whether the most significant bit is set 
            /// in each corresponding lane of a vector.
            #[derive(Clone, Copy, Debug)]
            pub struct SensibleMoveMask(u32);

            impl SensibleMoveMask
            {
                /// Get the mask in a form suitable for computing offsets.
                #[inline(always)] fn get_for_offset(self) -> u32
                {
                    #[cfg(target_endian = "big")]
                    {
                        self.0.swap_bytes()
                    }
                    #[cfg(target_endian = "little")]
                    {
                        self.0
                    }
                }
            }

            impl MoveMask for SensibleMoveMask 
            {
                #[inline( always )] fn all_zeros_except_least_significant(n: usize) -> SensibleMoveMask
                {
                    debug_assert!(n < 32); 
                    SensibleMoveMask(!((1 << n) - 1))
                }
                #[inline( always )] fn has_non_zero(self) -> bool { self.0 != 0 }
                #[inline( always )] fn count_ones(self) -> usize { self.0.count_ones() as usize }
                #[inline( always )] fn and(self, other: SensibleMoveMask) -> SensibleMoveMask { SensibleMoveMask(self.0 & other.0) }
                #[inline( always )] fn or(self, other: SensibleMoveMask) -> SensibleMoveMask { SensibleMoveMask(self.0 | other.0) }
                #[inline( always )] fn clear_least_significant_bit(self) -> SensibleMoveMask { SensibleMoveMask(self.0 & (self.0 - 1)) }
                #[inline( always )] fn first_offset(self) -> usize { self.get_for_offset().trailing_zeros() as usize }
                #[inline( always )] fn last_offset(self) -> usize { 32 - self.get_for_offset().leading_zeros() as usize - 1 }
            }

            #[cfg(target_arch = "x86_64")]
            mod x86sse2
            {
                use ::arch::x86_64::*;

                use super::{SensibleMoveMask, Vector};

                impl Vector for __m128i
                {
                    const BITS: usize = 128;
                    const BYTES: usize = 16;
                    const ALIGN: usize = Self::BYTES - 1;

                    type Mask = SensibleMoveMask;

                    #[inline( always )] unsafe fn splat(byte: u8) -> __m128i {
                        _mm_set1_epi8(byte as i8)
                    }

                    #[inline( always )] unsafe fn load_aligned(data: *const u8) -> __m128i {
                        _mm_load_si128(data as *const __m128i)
                    }

                    #[inline( always )] unsafe fn load_unaligned(data: *const u8) -> __m128i {
                        _mm_loadu_si128(data as *const __m128i)
                    }

                    #[inline( always )] unsafe fn movemask(self) -> SensibleMoveMask {
                        SensibleMoveMask(_mm_movemask_epi8(self) as u32)
                    }

                    #[inline( always )] unsafe fn cmpeq(self, vector2: Self) -> __m128i {
                        _mm_cmpeq_epi8(self, vector2)
                    }

                    #[inline( always )] unsafe fn and(self, vector2: Self) -> __m128i {
                        _mm_and_si128(self, vector2)
                    }

                    #[inline( always )] unsafe fn or(self, vector2: Self) -> __m128i {
                        _mm_or_si128(self, vector2)
                    }
                }
            }
            #[cfg(target_arch = "x86_64")]
            mod x86avx2
            {
                use ::arch::x86_64::*;

                use super::{SensibleMoveMask, Vector};

                impl Vector for __m256i
                {
                    const BITS: usize = 256;
                    const BYTES: usize = 32;
                    const ALIGN: usize = Self::BYTES - 1;

                    type Mask = SensibleMoveMask;

                    #[inline( always )] unsafe fn splat(byte: u8) -> __m256i {
                        _mm256_set1_epi8(byte as i8)
                    }

                    #[inline( always )] unsafe fn load_aligned(data: *const u8) -> __m256i {
                        _mm256_load_si256(data as *const __m256i)
                    }

                    #[inline( always )] unsafe fn load_unaligned(data: *const u8) -> __m256i {
                        _mm256_loadu_si256(data as *const __m256i)
                    }

                    #[inline( always )] unsafe fn movemask(self) -> SensibleMoveMask {
                        SensibleMoveMask(_mm256_movemask_epi8(self) as u32)
                    }

                    #[inline( always )] unsafe fn cmpeq(self, vector2: Self) -> __m256i {
                        _mm256_cmpeq_epi8(self, vector2)
                    }

                    #[inline( always )] unsafe fn and(self, vector2: Self) -> __m256i {
                        _mm256_and_si256(self, vector2)
                    }

                    #[inline( always )] unsafe fn or(self, vector2: Self) -> __m256i {
                        _mm256_or_si256(self, vector2)
                    }
                }
            }
        }
    }
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod os
{
    pub mod unix
    {
        pub use std::os::unix::{ * };
    }

    pub use std::os::{ * };
}

pub mod path
{
    pub use std::path::{ * };
}

pub mod process
{
    pub use std::process::{ * };
}

pub mod ptr
{
    pub use std::ptr::{ * };
}

pub mod regex
{
    /*!
    This crate provides routines for searching strings for matches of a [regular expression] (aka "regex"). */
    use ::
    {
        *,
    };
    /*
    #![no_std]
    #![deny(missing_docs)]
    #![cfg_attr(feature = "pattern", feature(pattern))]
    #![warn(missing_debug_implementations)]

    #[cfg(doctest)]
    doc_comment::doctest!("../README.md");

    extern crate alloc;
    #[cfg(any(test, feature = "std"))]
    extern crate std;
    pub use crate::error::Error;

    pub use crate::{builders::string::*, regex::string::*, regexset::string::*};
    */

    pub mod automata
    {
        use ::
        {
            *,
        };
        /*
        */
    }

    pub mod builders
    {
        /*
        This module defines an internal builder that encapsulates all interaction with meta::Regex construction, 
        and then 4 public API builders that wrap around it. */
        use ::
        {
            regex::
            {
                automata::
                {
                    meta::{ self },
                    nfa::thompson::{ WhichCaptures },
                    util::{ syntax }, MatchKind,
                },
                error::{ Error },
            },
            string::{ String, ToString },
            sync::{ Arc },
            vec::{ self, Vec },
            *,
        };
        /*
        */
        /// A builder for constructing a `Regex`, `bytes::Regex`, `RegexSet` or a `bytes::RegexSet`.
        #[derive(Clone, Debug)]
        struct Builder
        {
            pats: Vec<String>,
            metac: meta::Config,
            syntaxc: syntax::Config,
        }

        impl Default for Builder 
        {
            fn default() -> Builder {
                let metac = meta::Config::new()
                    .nfa_size_limit(Some(10 * (1 << 20)))
                    .hybrid_cache_capacity(2 * (1 << 20));
                Builder { pats: vec![], metac, syntaxc: syntax::Config::default() }
            }
        }

        impl Builder 
        {
            fn new<I, S>(patterns: I) -> Builder
            where
                S: AsRef<str>,
                I: IntoIterator<Item = S>,
            {
                let mut b = Builder::default();
                b.pats.extend(patterns.into_iter().map(|p| p.as_ref().to_string()));
                b
            }

            fn build_one_string(&self) -> Result<crate::Regex, Error> {
                assert_eq!(1, self.pats.len());
                let metac = self
                    .metac
                    .clone()
                    .match_kind(MatchKind::LeftmostFirst)
                    .utf8_empty(true);
                let syntaxc = self.syntaxc.clone().utf8(true);
                let pattern = Arc::from(self.pats[0].as_str());
                meta::Builder::new()
                    .configure(metac)
                    .syntax(syntaxc)
                    .build(&pattern)
                    .map(|meta| crate::Regex { meta, pattern })
                    .map_err(Error::from_meta_build_error)
            }

            fn build_one_bytes(&self) -> Result<crate::bytes::Regex, Error> {
                assert_eq!(1, self.pats.len());
                let metac = self
                    .metac
                    .clone()
                    .match_kind(MatchKind::LeftmostFirst)
                    .utf8_empty(false);
                let syntaxc = self.syntaxc.clone().utf8(false);
                let pattern = Arc::from(self.pats[0].as_str());
                meta::Builder::new()
                    .configure(metac)
                    .syntax(syntaxc)
                    .build(&pattern)
                    .map(|meta| crate::bytes::Regex { meta, pattern })
                    .map_err(Error::from_meta_build_error)
            }

            fn build_many_string(&self) -> Result<crate::RegexSet, Error> {
                let metac = self
                    .metac
                    .clone()
                    .match_kind(MatchKind::All)
                    .utf8_empty(true)
                    .which_captures(WhichCaptures::None);
                let syntaxc = self.syntaxc.clone().utf8(true);
                let patterns = Arc::from(self.pats.as_slice());
                meta::Builder::new()
                    .configure(metac)
                    .syntax(syntaxc)
                    .build_many(&patterns)
                    .map(|meta| crate::RegexSet { meta, patterns })
                    .map_err(Error::from_meta_build_error)
            }

            fn build_many_bytes(&self) -> Result<crate::bytes::RegexSet, Error> {
                let metac = self
                    .metac
                    .clone()
                    .match_kind(MatchKind::All)
                    .utf8_empty(false)
                    .which_captures(WhichCaptures::None);
                let syntaxc = self.syntaxc.clone().utf8(false);
                let patterns = Arc::from(self.pats.as_slice());
                meta::Builder::new()
                    .configure(metac)
                    .syntax(syntaxc)
                    .build_many(&patterns)
                    .map(|meta| crate::bytes::RegexSet { meta, patterns })
                    .map_err(Error::from_meta_build_error)
            }

            fn case_insensitive(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.case_insensitive(yes);
                self
            }

            fn multi_line(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.multi_line(yes);
                self
            }

            fn dot_matches_new_line(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.dot_matches_new_line(yes);
                self
            }

            fn crlf(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.crlf(yes);
                self
            }

            fn line_terminator(&mut self, byte: u8) -> &mut Builder {
                self.metac = self.metac.clone().line_terminator(byte);
                self.syntaxc = self.syntaxc.line_terminator(byte);
                self
            }

            fn swap_greed(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.swap_greed(yes);
                self
            }

            fn ignore_whitespace(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.ignore_whitespace(yes);
                self
            }

            fn unicode(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.unicode(yes);
                self
            }

            fn octal(&mut self, yes: bool) -> &mut Builder {
                self.syntaxc = self.syntaxc.octal(yes);
                self
            }

            fn size_limit(&mut self, limit: usize) -> &mut Builder {
                self.metac = self.metac.clone().nfa_size_limit(Some(limit));
                self
            }

            fn dfa_size_limit(&mut self, limit: usize) -> &mut Builder {
                self.metac = self.metac.clone().hybrid_cache_capacity(limit);
                self
            }

            fn nest_limit(&mut self, limit: u32) -> &mut Builder {
                self.syntaxc = self.syntaxc.nest_limit(limit);
                self
            }
        }

        pub mod string
        {
            use ::regex::{error::Error, Regex, RegexSet};

            use super::Builder;

            /// A configurable builder for a [`Regex`].
            #[derive(Clone, Debug)]
            pub struct RegexBuilder
            {
                builder: Builder,
            }

            impl RegexBuilder
            {
                /// Create a new builder with a default configuration for the given
                /// pattern.
                pub fn new(pattern: &str) -> RegexBuilder {
                    RegexBuilder { builder: Builder::new([pattern]) }
                }
                /// Compiles the pattern given to `RegexBuilder::new` with the
                /// configuration set on this builder.
                pub fn build(&self) -> Result<Regex, Error> {
                    self.builder.build_one_string()
                }
                /// This configures Unicode mode for the entire pattern.
                pub fn unicode(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.unicode(yes);
                    self
                }
                /// This configures whether to enable case insensitive matching for the entire pattern.
                pub fn case_insensitive(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.case_insensitive(yes);
                    self
                }
                /// This configures multi-line mode for the entire pattern.
                pub fn multi_line(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.multi_line(yes);
                    self
                }
                /// This configures dot-matches-new-line mode for the entire pattern.
                pub fn dot_matches_new_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexBuilder
                {
                    self.builder.dot_matches_new_line(yes);
                    self
                }
                /// This configures CRLF mode for the entire pattern.
                pub fn crlf(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.crlf(yes);
                    self
                }
                /// Configures the line terminator to be used by the regex.
                pub fn line_terminator(&mut self, byte: u8) -> &mut RegexBuilder
                {
                    self.builder.line_terminator(byte);
                    self
                }
                /// This configures swap-greed mode for the entire pattern.
                pub fn swap_greed(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.swap_greed(yes);
                    self
                }
                /// This configures verbose mode for the entire pattern.
                pub fn ignore_whitespace(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.ignore_whitespace(yes);
                    self
                }
                /// This configures octal mode for the entire pattern.
                pub fn octal(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.octal(yes);
                    self
                }
                /// Sets the approximate size limit, in bytes, of the compiled regex.
                pub fn size_limit(&mut self, bytes: usize) -> &mut RegexBuilder
                {
                    self.builder.size_limit(bytes);
                    self
                }
                /// Set the approximate capacity, in bytes, of the cache of transitions used by the lazy DFA.
                pub fn dfa_size_limit(&mut self, bytes: usize) -> &mut RegexBuilder
                {
                    self.builder.dfa_size_limit(bytes);
                    self
                }
                /// Set the nesting limit for this parser.
                pub fn nest_limit(&mut self, limit: u32) -> &mut RegexBuilder
                {
                    self.builder.nest_limit(limit);
                    self
                }
            }
            /// A configurable builder for a [`RegexSet`].
            #[derive(Clone, Debug)]
            pub struct RegexSetBuilder
            {
                builder: Builder,
            }

            impl RegexSetBuilder
            {
                /// Create a new builder with a default configuration for the given
                /// patterns.
                pub fn new<I, S>(patterns: I) -> RegexSetBuilder
                where
                    I: IntoIterator<Item = S>,
                    S: AsRef<str>,
                {
                    RegexSetBuilder { builder: Builder::new(patterns) }
                }
                /// Compiles the patterns given to `RegexSetBuilder::new` with the
                /// configuration set on this builder.
                pub fn build(&self) -> Result<RegexSet, Error> {
                    self.builder.build_many_string()
                }
                /// This configures Unicode mode for the all of the patterns.
                pub fn unicode(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.unicode(yes);
                    self
                }
                /// This configures whether to enable case insensitive matching for all
                /// of the patterns.
                pub fn case_insensitive(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.case_insensitive(yes);
                    self
                }
                /// This configures multi-line mode for all of the patterns.
                pub fn multi_line(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.multi_line(yes);
                    self
                }
                /// This configures dot-matches-new-line mode for the entire pattern.
                pub fn dot_matches_new_line
                (
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.dot_matches_new_line(yes);
                    self
                }
                /// This configures CRLF mode for all of the patterns.
                pub fn crlf(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.crlf(yes);
                    self
                }
                /// Configures the line terminator to be used by the regex.
                pub fn line_terminator(&mut self, byte: u8) -> &mut RegexSetBuilder
                {
                    self.builder.line_terminator(byte);
                    self
                }
                /// This configures swap-greed mode for all of the patterns.
                pub fn swap_greed(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.swap_greed(yes);
                    self
                }
                /// This configures verbose mode for all of the patterns.
                pub fn ignore_whitespace
                (
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.ignore_whitespace(yes);
                    self
                }
                /// This configures octal mode for all of the patterns.
                pub fn octal(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.octal(yes);
                    self
                }
                /// Sets the approximate size limit, in bytes, of the compiled regex.
                pub fn size_limit(&mut self, bytes: usize) -> &mut RegexSetBuilder
                {
                    self.builder.size_limit(bytes);
                    self
                }
                /// Set the approximate capacity, in bytes, of the cache of transitions
                /// used by the lazy DFA.
                pub fn dfa_size_limit
                (
                    &mut self,
                    bytes: usize,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.dfa_size_limit(bytes);
                    self
                }
                /// Set the nesting limit for this parser.
                pub fn nest_limit(&mut self, limit: u32) -> &mut RegexSetBuilder
                {
                    self.builder.nest_limit(limit);
                    self
                }
            }
        }

        pub mod bytes 
        {
            use ::regex::
            {
                bytes::{Regex, RegexSet},
                error::Error,
            };

            use super::Builder;

            /// A configurable builder for a [`Regex`].
            ///
            /// This builder can be used to programmatically set flags such as `i`
            /// (case insensitive) and `x` (for verbose mode). This builder can also be
            /// used to configure things like the line terminator and a size limit on
            /// the compiled regular expression.
            #[derive(Clone, Debug)]
            pub struct RegexBuilder {
                builder: Builder,
            }

            impl RegexBuilder {
                /// Create a new builder with a default configuration for the given
                /// pattern.
                ///
                /// If the pattern is invalid or exceeds the configured size limits,
                /// then an error will be returned when [`RegexBuilder::build`] is
                /// called.
                pub fn new(pattern: &str) -> RegexBuilder {
                    RegexBuilder { builder: Builder::new([pattern]) }
                }
                /// Compiles the pattern given to `RegexBuilder::new` with the
                /// configuration set on this builder.
                ///
                /// If the pattern isn't a valid regex or if a configured size limit
                /// was exceeded, then an error is returned.
                pub fn build(&self) -> Result<Regex, Error> {
                    self.builder.build_one_bytes()
                }
                /// This configures Unicode mode for the entire pattern.
                ///
                /// Enabling Unicode mode does a number of things:
                ///
                /// * Most fundamentally, it causes the fundamental atom of matching
                /// to be a single codepoint. When Unicode mode is disabled, it's a
                /// single byte. For example, when Unicode mode is enabled, `.` will
                /// match `💩` once, where as it will match 4 times when Unicode mode
                /// is disabled. (Since the UTF-8 encoding of `💩` is 4 bytes long.)
                /// * Case insensitive matching uses Unicode simple case folding rules.
                /// * Unicode character classes like `\p{Letter}` and `\p{Greek}` are
                /// available.
                /// * Perl character classes are Unicode aware. That is, `\w`, `\s` and
                /// `\d`.
                /// * The word boundary assertions, `\b` and `\B`, use the Unicode
                /// definition of a word character.
                ///
                /// Note that unlike the top-level `Regex` for searching `&str`, it
                /// is permitted to disable Unicode mode even if the resulting pattern
                /// could match invalid UTF-8. For example, `(?-u:.)` is not a valid
                /// pattern for a top-level `Regex`, but is valid for a `bytes::Regex`.
                ///
                /// For more details on the Unicode support in this crate, see the
                /// [Unicode section](crate#unicode) in this crate's top-level
                /// documentation.
                ///
                /// The default for this is `true`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"\w")
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally greek letters would be included in \w, but since
                /// // Unicode mode is disabled, it only matches ASCII letters.
                /// assert!(!re.is_match("δ".as_bytes()));
                ///
                /// let re = RegexBuilder::new(r"s")
                ///     .case_insensitive(true)
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally 'ſ' is included when searching for 's' case
                /// // insensitively due to Unicode's simple case folding rules. But
                /// // when Unicode mode is disabled, only ASCII case insensitive rules
                /// // are used.
                /// assert!(!re.is_match("ſ".as_bytes()));
                /// ```
                ///
                /// Since this builder is for constructing a [`bytes::Regex`](Regex),
                /// one can disable Unicode mode even if it would match invalid UTF-8:
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r".")
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally greek letters would be included in \w, but since
                /// // Unicode mode is disabled, it only matches ASCII letters.
                /// assert!(re.is_match(b"\xFF"));
                /// ```
                pub fn unicode(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.unicode(yes);
                    self
                }
                /// This configures whether to enable case insensitive matching for the
                /// entire pattern.
                ///
                /// This setting can also be configured using the inline flag `i`
                /// in the pattern. For example, `(?i:foo)` matches `foo` case
                /// insensitively while `(?-i:foo)` matches `foo` case sensitively.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"foo(?-i:bar)quux")
                ///     .case_insensitive(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"FoObarQuUx"));
                /// // Even though case insensitive matching is enabled in the builder,
                /// // it can be locally disabled within the pattern. In this case,
                /// // `bar` is matched case sensitively.
                /// assert!(!re.is_match(b"fooBARquux"));
                /// ```
                pub fn case_insensitive(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.case_insensitive(yes);
                    self
                }
                /// This configures multi-line mode for the entire pattern.
                ///
                /// Enabling multi-line mode changes the behavior of the `^` and `$`
                /// anchor assertions. Instead of only matching at the beginning and
                /// end of a haystack, respectively, multi-line mode causes them to
                /// match at the beginning and end of a line *in addition* to the
                /// beginning and end of a haystack. More precisely, `^` will match at
                /// the position immediately following a `\n` and `$` will match at the
                /// position immediately preceding a `\n`.
                ///
                /// The behavior of this option can be impacted by other settings too:
                ///
                /// * The [`RegexBuilder::line_terminator`] option changes `\n` above
                /// to any ASCII byte.
                /// * The [`RegexBuilder::crlf`] option changes the line terminator to
                /// be either `\r` or `\n`, but never at the position between a `\r`
                /// and `\n`.
                ///
                /// This setting can also be configured using the inline flag `m` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"^foo$")
                ///     .multi_line(true)
                ///     .build()
                ///     .unwrap();
                /// assert_eq!(Some(1..4), re.find(b"\nfoo\n").map(|m| m.range()));
                /// ```
                pub fn multi_line(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.multi_line(yes);
                    self
                }
                /// This configures dot-matches-new-line mode for the entire pattern.
                ///
                /// Perhaps surprisingly, the default behavior for `.` is not to match
                /// any character, but rather, to match any character except for the
                /// line terminator (which is `\n` by default). When this mode is
                /// enabled, the behavior changes such that `.` truly matches any
                /// character.
                ///
                /// This setting can also be configured using the inline flag `s` in
                /// the pattern. For example, `(?s:.)` and `\p{any}` are equivalent
                /// regexes.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"foo.bar")
                ///     .dot_matches_new_line(true)
                ///     .build()
                ///     .unwrap();
                /// let hay = b"foo\nbar";
                /// assert_eq!(Some(&b"foo\nbar"[..]), re.find(hay).map(|m| m.as_bytes()));
                /// ```
                pub fn dot_matches_new_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexBuilder
                {
                    self.builder.dot_matches_new_line(yes);
                    self
                }
                /// This configures CRLF mode for the entire pattern.
                ///
                /// When CRLF mode is enabled, both `\r` ("carriage return" or CR for
                /// short) and `\n` ("line feed" or LF for short) are treated as line
                /// terminators. This results in the following:
                ///
                /// * Unless dot-matches-new-line mode is enabled, `.` will now match
                /// any character except for `\n` and `\r`.
                /// * When multi-line mode is enabled, `^` will match immediately
                /// following a `\n` or a `\r`. Similarly, `$` will match immediately
                /// preceding a `\n` or a `\r`. Neither `^` nor `$` will ever match
                /// between `\r` and `\n`.
                ///
                /// This setting can also be configured using the inline flag `R` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"^foo$")
                ///     .multi_line(true)
                ///     .crlf(true)
                ///     .build()
                ///     .unwrap();
                /// let hay = b"\r\nfoo\r\n";
                /// // If CRLF mode weren't enabled here, then '$' wouldn't match
                /// // immediately after 'foo', and thus no match would be found.
                /// assert_eq!(Some(&b"foo"[..]), re.find(hay).map(|m| m.as_bytes()));
                /// ```
                ///
                /// This example demonstrates that `^` will never match at a position
                /// between `\r` and `\n`. (`$` will similarly not match between a `\r`
                /// and a `\n`.)
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"^")
                ///     .multi_line(true)
                ///     .crlf(true)
                ///     .build()
                ///     .unwrap();
                /// let hay = b"\r\n\r\n";
                /// let ranges: Vec<_> = re.find_iter(hay).map(|m| m.range()).collect();
                /// assert_eq!(ranges, vec![0..0, 2..2, 4..4]);
                /// ```
                pub fn crlf(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.crlf(yes);
                    self
                }
                /// Configures the line terminator to be used by the regex.
                ///
                /// The line terminator is relevant in two ways for a particular regex:
                ///
                /// * When dot-matches-new-line mode is *not* enabled (the default),
                /// then `.` will match any character except for the configured line
                /// terminator.
                /// * When multi-line mode is enabled (not the default), then `^` and
                /// `$` will match immediately after and before, respectively, a line
                /// terminator.
                ///
                /// In both cases, if CRLF mode is enabled in a particular context,
                /// then it takes precedence over any configured line terminator.
                ///
                /// This option cannot be configured from within the pattern.
                ///
                /// The default line terminator is `\n`.
                ///
                /// # Example
                ///
                /// This shows how to treat the NUL byte as a line terminator. This can
                /// be a useful heuristic when searching binary data.
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"^foo$")
                ///     .multi_line(true)
                ///     .line_terminator(b'\x00')
                ///     .build()
                ///     .unwrap();
                /// let hay = b"\x00foo\x00";
                /// assert_eq!(Some(1..4), re.find(hay).map(|m| m.range()));
                /// ```
                ///
                /// This example shows that the behavior of `.` is impacted by this
                /// setting as well:
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r".")
                ///     .line_terminator(b'\x00')
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"\n"));
                /// assert!(!re.is_match(b"\x00"));
                /// ```
                ///
                /// This shows that building a regex will work even when the byte
                /// given is not ASCII. This is unlike the top-level `Regex` API where
                /// matching invalid UTF-8 is not allowed.
                ///
                /// Note though that you must disable Unicode mode. This is required
                /// because Unicode mode requires matching one codepoint at a time,
                /// and there is no way to match a non-ASCII byte as if it were a
                /// codepoint.
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// assert!(
                ///     RegexBuilder::new(r".")
                ///         .unicode(false)
                ///         .line_terminator(0x80)
                ///         .build()
                ///         .is_ok(),
                /// );
                /// ```
                pub fn line_terminator(&mut self, byte: u8) -> &mut RegexBuilder
                {
                    self.builder.line_terminator(byte);
                    self
                }
                /// This configures swap-greed mode for the entire pattern.
                ///
                /// When swap-greed mode is enabled, patterns like `a+` will become
                /// non-greedy and patterns like `a+?` will become greedy. In other
                /// words, the meanings of `a+` and `a+?` are switched.
                ///
                /// This setting can also be configured using the inline flag `U` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let re = RegexBuilder::new(r"a+")
                ///     .swap_greed(true)
                ///     .build()
                ///     .unwrap();
                /// assert_eq!(Some(&b"a"[..]), re.find(b"aaa").map(|m| m.as_bytes()));
                /// ```
                pub fn swap_greed(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.swap_greed(yes);
                    self
                }
                /// This configures verbose mode for the entire pattern.
                ///
                /// When enabled, whitespace will treated as insignificant in the
                /// pattern and `#` can be used to start a comment until the next new
                /// line.
                ///
                /// Normally, in most places in a pattern, whitespace is treated
                /// literally. For example ` +` will match one or more ASCII whitespace
                /// characters.
                ///
                /// When verbose mode is enabled, `\#` can be used to match a literal
                /// `#` and `\ ` can be used to match a literal ASCII whitespace
                /// character.
                ///
                /// Verbose mode is useful for permitting regexes to be formatted and
                /// broken up more nicely. This may make them more easily readable.
                ///
                /// This setting can also be configured using the inline flag `x` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// let pat = r"
                ///     \b
                ///     (?<first>\p{Uppercase}\w*)  # always start with uppercase letter
                ///     [\s--\n]+                   # whitespace should separate names
                ///     (?: # middle name can be an initial!
                ///         (?:(?<initial>\p{Uppercase})\.|(?<middle>\p{Uppercase}\w*))
                ///         [\s--\n]+
                ///     )?
                ///     (?<last>\p{Uppercase}\w*)
                ///     \b
                /// ";
                /// let re = RegexBuilder::new(pat)
                ///     .ignore_whitespace(true)
                ///     .build()
                ///     .unwrap();
                ///
                /// let caps = re.captures(b"Harry Potter").unwrap();
                /// assert_eq!(&b"Harry"[..], &caps["first"]);
                /// assert_eq!(&b"Potter"[..], &caps["last"]);
                ///
                /// let caps = re.captures(b"Harry J. Potter").unwrap();
                /// assert_eq!(&b"Harry"[..], &caps["first"]);
                /// // Since a middle name/initial isn't required for an overall match,
                /// // we can't assume that 'initial' or 'middle' will be populated!
                /// assert_eq!(
                ///     Some(&b"J"[..]),
                ///     caps.name("initial").map(|m| m.as_bytes()),
                /// );
                /// assert_eq!(None, caps.name("middle").map(|m| m.as_bytes()));
                /// assert_eq!(&b"Potter"[..], &caps["last"]);
                ///
                /// let caps = re.captures(b"Harry James Potter").unwrap();
                /// assert_eq!(&b"Harry"[..], &caps["first"]);
                /// // Since a middle name/initial isn't required for an overall match,
                /// // we can't assume that 'initial' or 'middle' will be populated!
                /// assert_eq!(None, caps.name("initial").map(|m| m.as_bytes()));
                /// assert_eq!(
                ///     Some(&b"James"[..]),
                ///     caps.name("middle").map(|m| m.as_bytes()),
                /// );
                /// assert_eq!(&b"Potter"[..], &caps["last"]);
                /// ```
                pub fn ignore_whitespace(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.ignore_whitespace(yes);
                    self
                }
                /// This configures octal mode for the entire pattern.
                ///
                /// Octal syntax is a little-known way of uttering Unicode codepoints
                /// in a pattern. For example, `a`, `\x61`, `\u0061` and `\141` are all
                /// equivalent patterns, where the last example shows octal syntax.
                ///
                /// While supporting octal syntax isn't in and of itself a problem,
                /// it does make good error messages harder. That is, in PCRE based
                /// regex engines, syntax like `\1` invokes a backreference, which is
                /// explicitly unsupported this library. However, many users expect
                /// backreferences to be supported. Therefore, when octal support
                /// is disabled, the error message will explicitly mention that
                /// backreferences aren't supported.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// // Normally this pattern would not compile, with an error message
                /// // about backreferences not being supported. But with octal mode
                /// // enabled, octal escape sequences work.
                /// let re = RegexBuilder::new(r"\141")
                ///     .octal(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"a"));
                /// ```
                pub fn octal(&mut self, yes: bool) -> &mut RegexBuilder
                {
                    self.builder.octal(yes);
                    self
                }
                /// Sets the approximate size limit, in bytes, of the compiled regex.
                ///
                /// This roughly corresponds to the number of heap memory, in
                /// bytes, occupied by a single regex. If the regex would otherwise
                /// approximately exceed this limit, then compiling that regex will
                /// fail.
                ///
                /// The main utility of a method like this is to avoid compiling
                /// regexes that use an unexpected amount of resources, such as
                /// time and memory. Even if the memory usage of a large regex is
                /// acceptable, its search time may not be. Namely, worst case time
                /// complexity for search is `O(m * n)`, where `m ~ len(pattern)` and
                /// `n ~ len(haystack)`. That is, search time depends, in part, on the
                /// size of the compiled regex. This means that putting a limit on the
                /// size of the regex limits how much a regex can impact search time.
                ///
                /// For more information about regex size limits, see the section on
                /// [untrusted inputs](crate#untrusted-input) in the top-level crate
                /// documentation.
                ///
                /// The default for this is some reasonable number that permits most
                /// patterns to compile successfully.
                ///
                /// # Example
                ///
                /// ```
                /// # if !cfg!(target_pointer_width = "64") { return; } // see #1041
                /// use regex::bytes::RegexBuilder;
                ///
                /// // It may surprise you how big some seemingly small patterns can
                /// // be! Since \w is Unicode aware, this generates a regex that can
                /// // match approximately 140,000 distinct codepoints.
                /// assert!(RegexBuilder::new(r"\w").size_limit(45_000).build().is_err());
                /// ```
                pub fn size_limit(&mut self, bytes: usize) -> &mut RegexBuilder
                {
                    self.builder.size_limit(bytes);
                    self
                }
                /// Set the approximate capacity, in bytes, of the cache of transitions
                /// used by the lazy DFA.
                ///
                /// While the lazy DFA isn't always used, in tends to be the most
                /// commonly use regex engine in default configurations. It tends to
                /// adopt the performance profile of a fully build DFA, but without the
                /// downside of taking worst case exponential time to build.
                ///
                /// The downside is that it needs to keep a cache of transitions and
                /// states that are built while running a search, and this cache
                /// can fill up. When it fills up, the cache will reset itself. Any
                /// previously generated states and transitions will then need to be
                /// re-generated. If this happens too many times, then this library
                /// will bail out of using the lazy DFA and switch to a different regex
                /// engine.
                ///
                /// If your regex provokes this particular downside of the lazy DFA,
                /// then it may be beneficial to increase its cache capacity. This will
                /// potentially reduce the frequency of cache resetting (ideally to
                /// `0`). While it won't fix all potential performance problems with
                /// the lazy DFA, increasing the cache capacity does fix some.
                ///
                /// There is no easy way to determine, a priori, whether increasing
                /// this cache capacity will help. In general, the larger your regex,
                /// the more cache it's likely to use. But that isn't an ironclad rule.
                /// For example, a regex like `[01]*1[01]{N}` would normally produce a
                /// fully build DFA that is exponential in size with respect to `N`.
                /// The lazy DFA will prevent exponential space blow-up, but it cache
                /// is likely to fill up, even when it's large and even for smallish
                /// values of `N`.
                ///
                /// If you aren't sure whether this helps or not, it is sensible to
                /// set this to some arbitrarily large number in testing, such as
                /// `usize::MAX`. Namely, this represents the amount of capacity that
                /// *may* be used. It's probably not a good idea to use `usize::MAX` in
                /// production though, since it implies there are no controls on heap
                /// memory used by this library during a search. In effect, set it to
                /// whatever you're willing to allocate for a single regex search.
                pub fn dfa_size_limit(&mut self, bytes: usize) -> &mut RegexBuilder
                {
                    self.builder.dfa_size_limit(bytes);
                    self
                }
                /// Set the nesting limit for this parser.
                ///
                /// The nesting limit controls how deep the abstract syntax tree is
                /// allowed to be. If the AST exceeds the given limit (e.g., with too
                /// many nested groups), then an error is returned by the parser.
                ///
                /// The purpose of this limit is to act as a heuristic to prevent stack
                /// overflow for consumers that do structural induction on an AST using
                /// explicit recursion. While this crate never does this (instead using
                /// constant stack space and moving the call stack to the heap), other
                /// crates may.
                ///
                /// This limit is not checked until the entire AST is parsed.
                /// Therefore, if callers want to put a limit on the amount of heap
                /// space used, then they should impose a limit on the length, in
                /// bytes, of the concrete pattern string. In particular, this is
                /// viable since this parser implementation will limit itself to heap
                /// space proportional to the length of the pattern string. See also
                /// the [untrusted inputs](crate#untrusted-input) section in the
                /// top-level crate documentation for more information about this.
                ///
                /// Note that a nest limit of `0` will return a nest limit error for
                /// most patterns but not all. For example, a nest limit of `0` permits
                /// `a` but not `ab`, since `ab` requires an explicit concatenation,
                /// which results in a nest depth of `1`. In general, a nest limit is
                /// not something that manifests in an obvious way in the concrete
                /// syntax, therefore, it should not be used in a granular way.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexBuilder;
                ///
                /// assert!(RegexBuilder::new(r"a").nest_limit(0).build().is_ok());
                /// assert!(RegexBuilder::new(r"ab").nest_limit(0).build().is_err());
                /// ```
                pub fn nest_limit(&mut self, limit: u32) -> &mut RegexBuilder
                {
                    self.builder.nest_limit(limit);
                    self
                }
            }
            /// A configurable builder for a [`RegexSet`].
            ///
            /// This builder can be used to programmatically set flags such as `i`
            /// (case insensitive) and `x` (for verbose mode). This builder can also be
            /// used to configure things like the line terminator and a size limit on
            /// the compiled regular expression.
            #[derive(Clone, Debug)]
            pub struct RegexSetBuilder {
                builder: Builder,
            }

            impl RegexSetBuilder {
                /// Create a new builder with a default configuration for the given
                /// patterns.
                ///
                /// If the patterns are invalid or exceed the configured size limits,
                /// then an error will be returned when [`RegexSetBuilder::build`] is
                /// called.
                pub fn new<I, S>(patterns: I) -> RegexSetBuilder
                where
                    I: IntoIterator<Item = S>,
                    S: AsRef<str>,
                {
                    RegexSetBuilder { builder: Builder::new(patterns) }
                }
                /// Compiles the patterns given to `RegexSetBuilder::new` with the
                /// configuration set on this builder.
                ///
                /// If the patterns aren't valid regexes or if a configured size limit
                /// was exceeded, then an error is returned.
                pub fn build(&self) -> Result<RegexSet, Error> {
                    self.builder.build_many_bytes()
                }
                /// This configures Unicode mode for the all of the patterns.
                ///
                /// Enabling Unicode mode does a number of things:
                ///
                /// * Most fundamentally, it causes the fundamental atom of matching
                /// to be a single codepoint. When Unicode mode is disabled, it's a
                /// single byte. For example, when Unicode mode is enabled, `.` will
                /// match `💩` once, where as it will match 4 times when Unicode mode
                /// is disabled. (Since the UTF-8 encoding of `💩` is 4 bytes long.)
                /// * Case insensitive matching uses Unicode simple case folding rules.
                /// * Unicode character classes like `\p{Letter}` and `\p{Greek}` are
                /// available.
                /// * Perl character classes are Unicode aware. That is, `\w`, `\s` and
                /// `\d`.
                /// * The word boundary assertions, `\b` and `\B`, use the Unicode
                /// definition of a word character.
                ///
                /// Note that unlike the top-level `RegexSet` for searching `&str`,
                /// it is permitted to disable Unicode mode even if the resulting
                /// pattern could match invalid UTF-8. For example, `(?-u:.)` is not
                /// a valid pattern for a top-level `RegexSet`, but is valid for a
                /// `bytes::RegexSet`.
                ///
                /// For more details on the Unicode support in this crate, see the
                /// [Unicode section](crate#unicode) in this crate's top-level
                /// documentation.
                ///
                /// The default for this is `true`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"\w"])
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally greek letters would be included in \w, but since
                /// // Unicode mode is disabled, it only matches ASCII letters.
                /// assert!(!re.is_match("δ".as_bytes()));
                ///
                /// let re = RegexSetBuilder::new([r"s"])
                ///     .case_insensitive(true)
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally 'ſ' is included when searching for 's' case
                /// // insensitively due to Unicode's simple case folding rules. But
                /// // when Unicode mode is disabled, only ASCII case insensitive rules
                /// // are used.
                /// assert!(!re.is_match("ſ".as_bytes()));
                /// ```
                ///
                /// Since this builder is for constructing a
                /// [`bytes::RegexSet`](RegexSet), one can disable Unicode mode even if
                /// it would match invalid UTF-8:
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"."])
                ///     .unicode(false)
                ///     .build()
                ///     .unwrap();
                /// // Normally greek letters would be included in \w, but since
                /// // Unicode mode is disabled, it only matches ASCII letters.
                /// assert!(re.is_match(b"\xFF"));
                /// ```
                pub fn unicode(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.unicode(yes);
                    self
                }
                /// This configures whether to enable case insensitive matching for all
                /// of the patterns.
                ///
                /// This setting can also be configured using the inline flag `i`
                /// in the pattern. For example, `(?i:foo)` matches `foo` case
                /// insensitively while `(?-i:foo)` matches `foo` case sensitively.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"foo(?-i:bar)quux"])
                ///     .case_insensitive(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"FoObarQuUx"));
                /// // Even though case insensitive matching is enabled in the builder,
                /// // it can be locally disabled within the pattern. In this case,
                /// // `bar` is matched case sensitively.
                /// assert!(!re.is_match(b"fooBARquux"));
                /// ```
                pub fn case_insensitive(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.case_insensitive(yes);
                    self
                }
                /// This configures multi-line mode for all of the patterns.
                ///
                /// Enabling multi-line mode changes the behavior of the `^` and `$`
                /// anchor assertions. Instead of only matching at the beginning and
                /// end of a haystack, respectively, multi-line mode causes them to
                /// match at the beginning and end of a line *in addition* to the
                /// beginning and end of a haystack. More precisely, `^` will match at
                /// the position immediately following a `\n` and `$` will match at the
                /// position immediately preceding a `\n`.
                ///
                /// The behavior of this option can be impacted by other settings too:
                ///
                /// * The [`RegexSetBuilder::line_terminator`] option changes `\n`
                /// above to any ASCII byte.
                /// * The [`RegexSetBuilder::crlf`] option changes the line terminator
                /// to be either `\r` or `\n`, but never at the position between a `\r`
                /// and `\n`.
                ///
                /// This setting can also be configured using the inline flag `m` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"^foo$"])
                ///     .multi_line(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"\nfoo\n"));
                /// ```
                pub fn multi_line(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.multi_line(yes);
                    self
                }
                /// This configures dot-matches-new-line mode for the entire pattern.
                ///
                /// Perhaps surprisingly, the default behavior for `.` is not to match
                /// any character, but rather, to match any character except for the
                /// line terminator (which is `\n` by default). When this mode is
                /// enabled, the behavior changes such that `.` truly matches any
                /// character.
                ///
                /// This setting can also be configured using the inline flag `s` in
                /// the pattern. For example, `(?s:.)` and `\p{any}` are equivalent
                /// regexes.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"foo.bar"])
                ///     .dot_matches_new_line(true)
                ///     .build()
                ///     .unwrap();
                /// let hay = b"foo\nbar";
                /// assert!(re.is_match(hay));
                /// ```
                pub fn dot_matches_new_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.dot_matches_new_line(yes);
                    self
                }
                /// This configures CRLF mode for all of the patterns.
                ///
                /// When CRLF mode is enabled, both `\r` ("carriage return" or CR for
                /// short) and `\n` ("line feed" or LF for short) are treated as line
                /// terminators. This results in the following:
                ///
                /// * Unless dot-matches-new-line mode is enabled, `.` will now match
                /// any character except for `\n` and `\r`.
                /// * When multi-line mode is enabled, `^` will match immediately
                /// following a `\n` or a `\r`. Similarly, `$` will match immediately
                /// preceding a `\n` or a `\r`. Neither `^` nor `$` will ever match
                /// between `\r` and `\n`.
                ///
                /// This setting can also be configured using the inline flag `R` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"^foo$"])
                ///     .multi_line(true)
                ///     .crlf(true)
                ///     .build()
                ///     .unwrap();
                /// let hay = b"\r\nfoo\r\n";
                /// // If CRLF mode weren't enabled here, then '$' wouldn't match
                /// // immediately after 'foo', and thus no match would be found.
                /// assert!(re.is_match(hay));
                /// ```
                ///
                /// This example demonstrates that `^` will never match at a position
                /// between `\r` and `\n`. (`$` will similarly not match between a `\r`
                /// and a `\n`.)
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"^\n"])
                ///     .multi_line(true)
                ///     .crlf(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(!re.is_match(b"\r\n"));
                /// ```
                pub fn crlf(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.crlf(yes);
                    self
                }
                /// Configures the line terminator to be used by the regex.
                ///
                /// The line terminator is relevant in two ways for a particular regex:
                ///
                /// * When dot-matches-new-line mode is *not* enabled (the default),
                /// then `.` will match any character except for the configured line
                /// terminator.
                /// * When multi-line mode is enabled (not the default), then `^` and
                /// `$` will match immediately after and before, respectively, a line
                /// terminator.
                ///
                /// In both cases, if CRLF mode is enabled in a particular context,
                /// then it takes precedence over any configured line terminator.
                ///
                /// This option cannot be configured from within the pattern.
                ///
                /// The default line terminator is `\n`.
                ///
                /// # Example
                ///
                /// This shows how to treat the NUL byte as a line terminator. This can
                /// be a useful heuristic when searching binary data.
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"^foo$"])
                ///     .multi_line(true)
                ///     .line_terminator(b'\x00')
                ///     .build()
                ///     .unwrap();
                /// let hay = b"\x00foo\x00";
                /// assert!(re.is_match(hay));
                /// ```
                ///
                /// This example shows that the behavior of `.` is impacted by this
                /// setting as well:
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let re = RegexSetBuilder::new([r"."])
                ///     .line_terminator(b'\x00')
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"\n"));
                /// assert!(!re.is_match(b"\x00"));
                /// ```
                ///
                /// This shows that building a regex will work even when the byte given
                /// is not ASCII. This is unlike the top-level `RegexSet` API where
                /// matching invalid UTF-8 is not allowed.
                ///
                /// Note though that you must disable Unicode mode. This is required
                /// because Unicode mode requires matching one codepoint at a time,
                /// and there is no way to match a non-ASCII byte as if it were a
                /// codepoint.
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// assert!(
                ///     RegexSetBuilder::new([r"."])
                ///         .unicode(false)
                ///         .line_terminator(0x80)
                ///         .build()
                ///         .is_ok(),
                /// );
                /// ```
                pub fn line_terminator(&mut self, byte: u8) -> &mut RegexSetBuilder
                {
                    self.builder.line_terminator(byte);
                    self
                }
                /// This configures swap-greed mode for all of the patterns.
                ///
                /// When swap-greed mode is enabled, patterns like `a+` will become
                /// non-greedy and patterns like `a+?` will become greedy. In other
                /// words, the meanings of `a+` and `a+?` are switched.
                ///
                /// This setting can also be configured using the inline flag `U` in
                /// the pattern.
                ///
                /// Note that this is generally not useful for a `RegexSet` since a
                /// `RegexSet` can only report whether a pattern matches or not. Since
                /// greediness never impacts whether a match is found or not (only the
                /// offsets of the match), it follows that whether parts of a pattern
                /// are greedy or not doesn't matter for a `RegexSet`.
                ///
                /// The default for this is `false`.
                pub fn swap_greed(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.swap_greed(yes);
                    self
                }
                /// This configures verbose mode for all of the patterns.
                ///
                /// When enabled, whitespace will treated as insignificant in the
                /// pattern and `#` can be used to start a comment until the next new
                /// line.
                ///
                /// Normally, in most places in a pattern, whitespace is treated
                /// literally. For example ` +` will match one or more ASCII whitespace
                /// characters.
                ///
                /// When verbose mode is enabled, `\#` can be used to match a literal
                /// `#` and `\ ` can be used to match a literal ASCII whitespace
                /// character.
                ///
                /// Verbose mode is useful for permitting regexes to be formatted and
                /// broken up more nicely. This may make them more easily readable.
                ///
                /// This setting can also be configured using the inline flag `x` in
                /// the pattern.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// let pat = r"
                ///     \b
                ///     (?<first>\p{Uppercase}\w*)  # always start with uppercase letter
                ///     [\s--\n]+                   # whitespace should separate names
                ///     (?: # middle name can be an initial!
                ///         (?:(?<initial>\p{Uppercase})\.|(?<middle>\p{Uppercase}\w*))
                ///         [\s--\n]+
                ///     )?
                ///     (?<last>\p{Uppercase}\w*)
                ///     \b
                /// ";
                /// let re = RegexSetBuilder::new([pat])
                ///     .ignore_whitespace(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"Harry Potter"));
                /// assert!(re.is_match(b"Harry J. Potter"));
                /// assert!(re.is_match(b"Harry James Potter"));
                /// assert!(!re.is_match(b"harry J. Potter"));
                /// ```
                pub fn ignore_whitespace(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.ignore_whitespace(yes);
                    self
                }
                /// This configures octal mode for all of the patterns.
                ///
                /// Octal syntax is a little-known way of uttering Unicode codepoints
                /// in a pattern. For example, `a`, `\x61`, `\u0061` and `\141` are all
                /// equivalent patterns, where the last example shows octal syntax.
                ///
                /// While supporting octal syntax isn't in and of itself a problem,
                /// it does make good error messages harder. That is, in PCRE based
                /// regex engines, syntax like `\1` invokes a backreference, which is
                /// explicitly unsupported this library. However, many users expect
                /// backreferences to be supported. Therefore, when octal support
                /// is disabled, the error message will explicitly mention that
                /// backreferences aren't supported.
                ///
                /// The default for this is `false`.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// // Normally this pattern would not compile, with an error message
                /// // about backreferences not being supported. But with octal mode
                /// // enabled, octal escape sequences work.
                /// let re = RegexSetBuilder::new([r"\141"])
                ///     .octal(true)
                ///     .build()
                ///     .unwrap();
                /// assert!(re.is_match(b"a"));
                /// ```
                pub fn octal(&mut self, yes: bool) -> &mut RegexSetBuilder
                {
                    self.builder.octal(yes);
                    self
                }
                /// Sets the approximate size limit, in bytes, of the compiled regex.
                ///
                /// This roughly corresponds to the number of heap memory, in
                /// bytes, occupied by a single regex. If the regex would otherwise
                /// approximately exceed this limit, then compiling that regex will
                /// fail.
                ///
                /// The main utility of a method like this is to avoid compiling
                /// regexes that use an unexpected amount of resources, such as
                /// time and memory. Even if the memory usage of a large regex is
                /// acceptable, its search time may not be. Namely, worst case time
                /// complexity for search is `O(m * n)`, where `m ~ len(pattern)` and
                /// `n ~ len(haystack)`. That is, search time depends, in part, on the
                /// size of the compiled regex. This means that putting a limit on the
                /// size of the regex limits how much a regex can impact search time.
                ///
                /// For more information about regex size limits, see the section on
                /// [untrusted inputs](crate#untrusted-input) in the top-level crate
                /// documentation.
                ///
                /// The default for this is some reasonable number that permits most
                /// patterns to compile successfully.
                ///
                /// # Example
                ///
                /// ```
                /// # if !cfg!(target_pointer_width = "64") { return; } // see #1041
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// // It may surprise you how big some seemingly small patterns can
                /// // be! Since \w is Unicode aware, this generates a regex that can
                /// // match approximately 140,000 distinct codepoints.
                /// assert!(
                ///     RegexSetBuilder::new([r"\w"])
                ///         .size_limit(45_000)
                ///         .build()
                ///         .is_err()
                /// );
                /// ```
                pub fn size_limit(&mut self, bytes: usize) -> &mut RegexSetBuilder
                {
                    self.builder.size_limit(bytes);
                    self
                }
                /// Set the approximate capacity, in bytes, of the cache of transitions
                /// used by the lazy DFA.
                ///
                /// While the lazy DFA isn't always used, in tends to be the most
                /// commonly use regex engine in default configurations. It tends to
                /// adopt the performance profile of a fully build DFA, but without the
                /// downside of taking worst case exponential time to build.
                ///
                /// The downside is that it needs to keep a cache of transitions and
                /// states that are built while running a search, and this cache
                /// can fill up. When it fills up, the cache will reset itself. Any
                /// previously generated states and transitions will then need to be
                /// re-generated. If this happens too many times, then this library
                /// will bail out of using the lazy DFA and switch to a different regex
                /// engine.
                ///
                /// If your regex provokes this particular downside of the lazy DFA,
                /// then it may be beneficial to increase its cache capacity. This will
                /// potentially reduce the frequency of cache resetting (ideally to
                /// `0`). While it won't fix all potential performance problems with
                /// the lazy DFA, increasing the cache capacity does fix some.
                ///
                /// There is no easy way to determine, a priori, whether increasing
                /// this cache capacity will help. In general, the larger your regex,
                /// the more cache it's likely to use. But that isn't an ironclad rule.
                /// For example, a regex like `[01]*1[01]{N}` would normally produce a
                /// fully build DFA that is exponential in size with respect to `N`.
                /// The lazy DFA will prevent exponential space blow-up, but it cache
                /// is likely to fill up, even when it's large and even for smallish
                /// values of `N`.
                ///
                /// If you aren't sure whether this helps or not, it is sensible to
                /// set this to some arbitrarily large number in testing, such as
                /// `usize::MAX`. Namely, this represents the amount of capacity that
                /// *may* be used. It's probably not a good idea to use `usize::MAX` in
                /// production though, since it implies there are no controls on heap
                /// memory used by this library during a search. In effect, set it to
                /// whatever you're willing to allocate for a single regex search.
                pub fn dfa_size_limit(
                    &mut self,
                    bytes: usize,
                ) -> &mut RegexSetBuilder
                {
                    self.builder.dfa_size_limit(bytes);
                    self
                }
                /// Set the nesting limit for this parser.
                ///
                /// The nesting limit controls how deep the abstract syntax tree is
                /// allowed to be. If the AST exceeds the given limit (e.g., with too
                /// many nested groups), then an error is returned by the parser.
                ///
                /// The purpose of this limit is to act as a heuristic to prevent stack
                /// overflow for consumers that do structural induction on an AST using
                /// explicit recursion. While this crate never does this (instead using
                /// constant stack space and moving the call stack to the heap), other
                /// crates may.
                ///
                /// This limit is not checked until the entire AST is parsed.
                /// Therefore, if callers want to put a limit on the amount of heap
                /// space used, then they should impose a limit on the length, in
                /// bytes, of the concrete pattern string. In particular, this is
                /// viable since this parser implementation will limit itself to heap
                /// space proportional to the length of the pattern string. See also
                /// the [untrusted inputs](crate#untrusted-input) section in the
                /// top-level crate documentation for more information about this.
                ///
                /// Note that a nest limit of `0` will return a nest limit error for
                /// most patterns but not all. For example, a nest limit of `0` permits
                /// `a` but not `ab`, since `ab` requires an explicit concatenation,
                /// which results in a nest depth of `1`. In general, a nest limit is
                /// not something that manifests in an obvious way in the concrete
                /// syntax, therefore, it should not be used in a granular way.
                ///
                /// # Example
                ///
                /// ```
                /// use regex::bytes::RegexSetBuilder;
                ///
                /// assert!(RegexSetBuilder::new([r"a"]).nest_limit(0).build().is_ok());
                /// assert!(RegexSetBuilder::new([r"ab"]).nest_limit(0).build().is_err());
                /// ```
                pub fn nest_limit(&mut self, limit: u32) -> &mut RegexSetBuilder
                {
                    self.builder.nest_limit(limit);
                    self
                }
            }
        }
    }

    pub mod bytes
    {
        /*!
        Search for regex matches in `&[u8]` haystacks. */
        use ::
        {
            *,
        };
        /*
        pub use crate::{builders::bytes::*, regex::bytes::*, regexset::bytes::*};
        */
    }

    pub mod error
    {
        use ::
        {
            regex::automata::meta,
            string::{String, ToString},
            *,
        };
        /*
        */
        /// An error that occurred during parsing or compiling a regular expression.
        #[non_exhaustive]
        #[derive(Clone, PartialEq)]
        pub enum Error
        {
            /// A syntax error.
            Syntax(String),
            /// The compiled program exceeded the set size limit.
            CompiledTooBig(usize),
        }

        impl Error
        {
            pub fn from_meta_build_error(err: meta::BuildError) -> Error
            {
                if let Some(size_limit) = err.size_limit() {
                    Error::CompiledTooBig(size_limit)
                } else if let Some(ref err) = err.syntax_error() {
                    Error::Syntax(err.to_string())
                } else {
                    Error::Syntax(err.to_string())
                }
            }
        }
        
        impl ::error::Error for Error
        {
            #[allow(deprecated)]
            fn description(&self) -> &str {
                match *self {
                    Error::Syntax(ref err) => err,
                    Error::CompiledTooBig(_) => "compiled program too big",
                }
            }
        }

        impl ::fmt::Display for Error
        {
            fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result {
                match *self {
                    Error::Syntax(ref err) => err.fmt(f),
                    Error::CompiledTooBig(limit) => write!(
                        f,
                        "Compiled regex exceeds size limit of {limit} bytes."
                    ),
                }
            }
        }
        
        impl ::fmt::Debug for Error
        {
            fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result {
                match *self {
                    Error::Syntax(ref err) => {
                        let hr: String = ::iter::repeat('~').take(79).collect();
                        writeln!(f, "Syntax(")?;
                        writeln!(f, "{hr}")?;
                        writeln!(f, "{err}")?;
                        writeln!(f, "{hr}")?;
                        write!(f, ")")?;
                        Ok(())
                    }
                    Error::CompiledTooBig(limit) => {
                        f.debug_tuple("CompiledTooBig").field(&limit).finish()
                    }
                }
            }
        }
    }

    pub mod find_byte
    {
        use ::
        {
            *,
        };
        /*
        */
        /// Searches for the given needle in the given haystack.
        pub fn find_byte(needle: u8, haystack: &[u8]) -> Option<usize>
        {
            fn imp(needle: u8, haystack: &[u8]) -> Option<usize> 
            {
                ::mem::chr::memchr(needle, haystack)
            }

            imp(needle, haystack)
        }

    }

    pub mod pattern
    {
        use ::
        {
            *,
        };
        /*
        */
    }

    pub mod regex
    {
        use ::
        {
            *,
        };
        /*
        */
    }

    pub mod regexset
    {
        use ::
        {
            *,
        };
        /*
        */
    }

    pub mod syntax
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    /// Escapes all regular expression meta characters in `pattern`.
    ///
    /// The string returned may be safely used as a literal in a regular
    /// expression.
    pub fn escape(pattern: &str) -> ::string::String {
        ::regex::syntax::escape(pattern)
    }

}

pub mod result
{
    pub use std::result::{ * };
}

pub mod str
{
    pub use std::str::{ * };
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod sync
{
    pub use std::sync::{ * };
}

pub mod system
{
    pub mod common
    {
        use ::
        {
            *,
        };
    }
}

pub mod tools
{
    use ::
    { 
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

    use regex::Regex;

    use ::execute;
    use ::libs::re::re_contains;
    use ::parsers;
    use ::shell;
    */
    pub fn is_signal_handler_enabled() -> bool
    {
        env::var("CICADA_ENABLE_SIG_HANDLER").is_ok_and(|x| x == "1")
    }

    pub fn get_user_name() -> String
    {
        match env::var("USER") {
            Ok(x) => {
                return x;
            }
            Err(e) => {
                log!("cicada: env USER error: {}", e);
            }
        }
        let cmd_result = execute::run("whoami");
        cmd_result.stdout.trim().to_string()
    }

    pub fn get_user_home() -> String
    {
        match env::var("HOME") {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: env HOME error: {}", e);
                String::new()
            }
        }
    }

    pub fn get_config_dir() -> String
    {
        if let Ok(x) = env::var("XDG_CONFIG_HOME") {
            format!("{}/cicada", x)
        } else {
            let home = get_user_home();
            format!("{}/.config/cicada", home)
        }
    }

    pub fn get_user_completer_dir() -> String
    {
        let dir_config = get_config_dir();
        format!("{}/completers", dir_config)
    }

    pub fn unquote(s: &str) -> String
    {
        let args = parsers::parser_line::line_to_plain_tokens(s);
        if args.is_empty() {
            return String::new();
        }
        args[0].clone()
    }

    pub fn is_env(line: &str) -> bool
    {
        re_contains(line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$")
    }

    // #[allow(clippy::trivial_regex)]
    pub fn extend_bangbang(sh: &shell::Shell, line: &mut String)
    {
        if !re_contains(line, r"!!") {
            return;
        }
        if sh.previous_cmd.is_empty() {
            return;
        }

        let re = Regex::new(r"!!").unwrap();
        let mut replaced = false;
        let mut new_line = String::new();
        let linfo = parsers::parser_line::parse_line(line);
        for (sep, token) in linfo.tokens {
            if !sep.is_empty() {
                new_line.push_str(&sep);
            }

            if re_contains(&token, r"!!") && sep != "'" {
                let line2 = token.clone();
                let result = re.replace_all(&line2, sh.previous_cmd.as_str());
                new_line.push_str(&result);
                replaced = true;
            } else {
                new_line.push_str(&token);
            }

            if !sep.is_empty() {
                new_line.push_str(&sep);
            }
            new_line.push(' ');
        }

        *line = new_line.trim_end().to_string();
        // print full line after extending
        if replaced {
            println!("{}", line);
        }
    }

    pub fn wrap_sep_string(sep: &str, s: &str) -> String
    {
        let mut _token = String::new();
        let mut met_subsep = false;
        // let set previous_subsep to any char except '`' or '"'
        let mut previous_subsep = 'N';
        for c in s.chars() {
            // handle cmds like: export DIR=`brew --prefix openssl`/include
            // or like: export foo="hello world"
            if sep.is_empty() && (c == '`' || c == '"') {
                if !met_subsep {
                    met_subsep = true;
                    previous_subsep = c;
                } else if c == previous_subsep {
                    met_subsep = false;
                    previous_subsep = 'N';
                }
            }
            if c.to_string() == sep {
                _token.push('\\');
            }
            if c == ' ' && sep.is_empty() && !met_subsep {
                _token.push('\\');
            }
            _token.push(c);
        }
        format!("{}{}{}", sep, _token, sep)
    }

    pub fn env_args_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = env::args();
        if env_args.len() <= 1 {
            return result;
        }
        for (i, arg) in env_args.enumerate() {
            if i == 0 || arg == "-c" {
                continue;
            }
            result.push_str(arg.as_str());
        }
        result
    }

    extern "C"
    {
        fn gethostname(name: *mut libc::c_char, size: ::libc::size_t) -> libc::c_int;
    }

    /// via: https://gist.github.com/conradkleinespel/6c8174aee28fa22bfe26
    pub fn get_hostname() -> String
    {
        let len = 255;
        let mut buf = Vec::<u8>::with_capacity(len);

        let ptr = buf.as_mut_slice().as_mut_ptr();

        let err = unsafe { gethostname(ptr as *mut libc::c_char, len as libc::size_t) } as i32;

        match err {
            0 => {
                let real_len;
                let mut i = 0;
                loop {
                    let byte = unsafe { *(((ptr as u64) + (i as u64)) as *const u8) };
                    if byte == 0 {
                        real_len = i;
                        break;
                    }

                    i += 1;
                }
                unsafe { buf.set_len(real_len) }
                String::from_utf8_lossy(buf.as_slice()).into_owned()
            }
            _ => String::from("unknown"),
        }
    }

    pub fn is_arithmetic(line: &str) -> bool
    {
        if !re_contains(line, r"[0-9]+") {
            return false;
        }
        if !re_contains(line, r"\+|\-|\*|/|\^") {
            return false;
        }
        re_contains(line, r"^[ 0-9\.\(\)\+\-\*/\^]+[\.0-9 \)]$")
    }

    pub fn create_raw_fd_from_file(file_name: &str, append: bool) -> Result<i32, String>
    {
        let mut oos = OpenOptions::new();
        if append {
            oos.append(true);
        } else {
            oos.write(true);
            oos.truncate(true);
        }
        match oos.create(true).open(file_name) {
            Ok(x) => {
                let fd = x.into_raw_fd();
                Ok(fd)
            }
            Err(e) => Err(format!("{}", e)),
        }
    }

    pub fn get_fd_from_file(file_name: &str) -> i32
    {
        let path = Path::new(file_name);
        let display = path.display();
        let file = match File::open(path) {
            Err(why) => {
                println_stderr!("cicada: {}: {}", display, why);
                return -1;
            }
            Ok(file) => file,
        };
        file.into_raw_fd()
    }

    pub fn escape_path(path: &str) -> String
    {
        let re = Regex::new(r##"(?P<c>[!\(\)<>,\?\]\[\{\} \\'"`*\^#|$&;])"##).unwrap();
        re.replace_all(path, "\\$c").to_string()
    }

    pub fn get_current_dir() -> String
    {
        let mut current_dir = PathBuf::new();
        match env::current_dir() {
            Ok(x) => current_dir = x,
            Err(e) => {
                println_stderr!("env current_dir() failed: {}", e);
            }
        }
        let mut str_current_dir = "";
        match current_dir.to_str() {
            Some(x) => str_current_dir = x,
            None => {
                println_stderr!("current_dir to str failed.");
            }
        }
        str_current_dir.to_string()
    }

    pub fn split_into_fields
    (
        sh: &shell::Shell,
        line: &str,
        envs: &HashMap<String, String>,
    ) -> Vec<String>
    {
        let ifs_chars;
        if envs.contains_key("IFS") {
            ifs_chars = envs[&"IFS".to_string()].chars().collect();
        } else if let Some(x) = sh.get_env("IFS") {
            ifs_chars = x.chars().collect();
        } else if let Ok(x) = env::var("IFS") {
            ifs_chars = x.chars().collect();
        } else {
            ifs_chars = vec![];
        }

        if ifs_chars.is_empty() {
            line.split(&[' ', '\t', '\n'][..])
                .map(|x| x.to_string())
                .collect()
        } else {
            line.split(&ifs_chars[..]).map(|x| x.to_string()).collect()
        }
    }

    pub fn is_builtin(s: &str) -> bool
    {
        let builtins = [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg", "history", "jobs", "read",
            "source", "ulimit", "unalias", "vox", "minfd", "set", "unset", "unpath",
        ];
        builtins.contains(&s)
    }

    pub fn init_path_env()
    {
        // order matters. took from `runc spec`
        let mut paths: Vec<String> = vec![];
        for x in [
            "/usr/local/sbin",
            "/usr/local/bin",
            "/usr/sbin",
            "/usr/bin",
            "/sbin",
            "/bin",
        ] {
            if Path::new(x).exists() {
                paths.push(x.to_string());
            }
        }

        if let Ok(env_path) = env::var("PATH") {
            for x in env_path.split(":") {
                if !paths.contains(&x.to_string()) {
                    paths.push(x.to_string());
                }
            }
        }
        let paths = paths.join(":");
        env::set_var("PATH", paths);
    }

    pub fn is_shell_altering_command(line: &str) -> bool
    {
        let line = line.trim();
        if re_contains(line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$") {
            return true;
        }
        line.starts_with("alias ")
            || line.starts_with("export ")
            || line.starts_with("unalias ")
            || line.starts_with("unset ")
            || line.starts_with("source ")
    }
}

pub mod vec
{
    pub use std::vec::{ * };
}


pub fn domain() -> Result<(), Box<dyn std::error::Error>>
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);
        tools::init_path_env();
        /*
        Return.*/
        Ok(())
    }
}

pub fn main() -> Result<(), Box<dyn std::error::Error>>
{
    domain()
}
// 14065 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
