/*!
Cicada is a simple Unix shell written in Rust. */

#![allow
(
    dead_code,
    missing_abi,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    redundant_semicolons,
    unused_imports,
)]

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod error
{
    pub use std::error::{ * };
}

pub mod libc
{
    #[macro_use] mod macros
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

                            use crate::prelude::*;
                            use crate::{off64_t, off_t};*/

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

pub mod mem
{
    pub use std::mem::{ * };
}

pub mod result
{
    pub use std::result::{ * };
}


pub fn domain() -> Result<(), Box<dyn std::error::Error>>
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);
        /*
        Return.*/
        Ok(())
    }
}

pub fn main() -> Result<(), Box<dyn std::error::Error>>
{
    domain()
}
// 5651 ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
