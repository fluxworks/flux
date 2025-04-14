//! Cicada is a bash-like Unix shell written in Rust.
//!
//! If you would like to use cicada as a regular shell,
//! please see details in [its repository](https://github.com/mitnk/cicada)
//!
//! Here is how to use cicada as a library:
//!
//! **Add cicada into Cargo.toml**
//!
//! ```ignore
//! [dependencies]
//! cicada = "1.0"
//! ```
//!
//! **Use cicada functions**
//!
//! ```no_run
//! extern crate cicada;
//!
//! fn main() {
//!     let info = cicada::parse_line("echo 'hi yoo' | `which wc`");
//!     assert!(info.is_complete);
//!
//!     let tokens = info.tokens;
//!     assert_eq!(tokens.len(), 4);
//!
//!     assert_eq!(tokens[0].0, "");
//!     assert_eq!(tokens[0].1, "echo");
//!
//!     assert_eq!(tokens[1].0, "'");
//!     assert_eq!(tokens[1].1, "hi yoo");
//!
//!     assert_eq!(tokens[2].0, "");
//!     assert_eq!(tokens[2].1, "|");
//!
//!     assert_eq!(tokens[3].0, "`");
//!     assert_eq!(tokens[3].1, "which wc");
//!
//!     let out1 = cicada::run("ls Cargo.toml foo");
//!     assert_eq!(out1.status, 1);
//!     assert_eq!(out1.stdout, "Cargo.toml\n");
//!     assert_eq!(out1.stderr, "ls: foo: No such file or directory\n");
//!
//!     let out2 = cicada::run("ls | wc");
//!     assert_eq!(out2.status, 0);
//!     assert_eq!(out2.stdout, "       4       4      33\n");
//! }
//! ```
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
/**/
pub use std::{ * };

/**/
#[macro_use] extern crate lazy_static;
/**/
extern crate getrandom;
extern crate libc;
extern crate nix;
extern crate regex;
/*
#![allow(unknown_lints)]
// #![feature(tool_lints)]
extern crate errno;
extern crate exec;
extern crate glob;
extern crate libc;
extern crate lineread;
extern crate nix;
extern crate regex;
extern crate rusqlite;
extern crate yaml_rust;

extern crate clap;

#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;
use std::io::Write;
use std::sync::Arc;

use lineread::{Command, Interface, ReadResult};

    extern crate errno;
    extern crate exec;
    extern crate glob;
    extern crate lineread;
    extern crate pest;
    extern crate rusqlite;
    //
    #[macro_use] extern crate pest_derive;
*/
#[macro_use] mod tlog
{
    use ::
    {
        *,
    };

    pub fn getpid() -> i32 { unsafe { libc::getpid() } }

    #[macro_export]
    macro_rules! log
    {
        ($fmt:expr) => 
        (
            let log_file = if let Ok(x) = ::env::var("CICADA_LOG_FILE") { x.clone() } 
            else { String::new() };

            if !log_file.is_empty()
            {
                use ::io::Write as _;

                let msg = $fmt;
                match ::fs::OpenOptions::new().append(true).create(true).open(&log_file)
                {
                    Ok(mut cfile) =>
                    {
                        let pid = ::tlog::getpid();
                        let now = ::ctime::DateTime::now();
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
}

#[macro_use] mod tools
{
    use ::
    {
        collections::{ HashMap },
        fs::{ File },
        fs::{ OpenOptions },
        libs::re::{ re_contains },
        os::unix::io::{ IntoRawFd },
        path::{ Path, PathBuf },
        regex::{ Regex },        
        *,
    };
    /**/
    extern "C"
    {
        fn gethostname(name: *mut libc::c_char, size: libc::size_t) -> libc::c_int;
    }
    /*
    */
    macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            match writeln!(&mut stderr(), $fmt)
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            match writeln!(&mut stderr(), $fmt, $($arg)*)
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

    pub fn is_signal_handler_enabled() -> bool { env::var("CICADA_ENABLE_SIG_HANDLER").map_or(false, |x| x == "1") }

    pub fn get_user_name() -> String
    {
        match env::var("USER")
        {
            Ok(x) => { return x; }
            Err(e) => { log!("cicada: env USER error: {}", e); }
        }

        let cmd_result = execute::run("whoami");
        return cmd_result.stdout.trim().to_string();
    }

    pub fn get_user_home() -> String
    {
        match env::var("HOME")
        {
            Ok(x) => x,
            Err(e) =>
            {
                println_stderr!("cicada: env HOME error: {}", e);
                String::new()
            }
        }
    }

    pub fn get_config_dir() -> String
    {
        if let Ok(x) = env::var("XDG_CONFIG_HOME") { format!("{}/cicada", x) }
        else
        {
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
        if args.is_empty() { return String::new(); }
        args[0].clone()
    }

    pub fn is_env(line: &str) -> bool { re_contains(line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$") }
    
    pub fn extend_bangbang(sh: &shell::Shell, line: &mut String)
    {
        if !re_contains(line, r"!!") { return; }

        if sh.previous_cmd.is_empty() { return; }

        let re = Regex::new(r"!!").unwrap();
        let mut replaced = false;
        let mut new_line = String::new();
        let linfo = parsers::parser_line::parse_line(line);
        for (sep, token) in linfo.tokens
        {
            if !sep.is_empty() { new_line.push_str(&sep); }

            if re_contains(&token, r"!!") && sep != "'"
            {
                let line2 = token.clone();
                let result = re.replace_all(&line2, sh.previous_cmd.as_str());
                new_line.push_str(&result);
                replaced = true;
            }
            
            else { new_line.push_str(&token); }

            if !sep.is_empty() { new_line.push_str(&sep); }

            new_line.push(' ');
        }

        *line = new_line.trim_end().to_string();
        if replaced { println!("{}", line); }
    }

    pub fn wrap_sep_string(sep: &str, s: &str) -> String
    {
        let mut _token = String::new();
        let mut met_subsep = false;
        let mut previous_subsep = 'N';
        for c in s.chars()
        {
            if sep.is_empty() && (c == '`' || c == '"')
            {
                if !met_subsep
                {
                    met_subsep = true;
                    previous_subsep = c;
                }
                
                else if c == previous_subsep
                {
                    met_subsep = false;
                    previous_subsep = 'N';
                }
            }

            if c.to_string() == sep { _token.push('\\'); }

            if c == ' ' && sep.is_empty() && !met_subsep { _token.push('\\'); }

            _token.push(c);
        }

        format!("{}{}{}", sep, _token, sep)
    }

    pub fn env_args_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = env::args();
        
        if env_args.len() <= 1 { return result; }

        for (i, arg) in env_args.enumerate()
        {
            if i == 0 || arg == "-c" { continue; }

            result.push_str(arg.as_str());
        }

        result
    }
    
    pub fn get_hostname() -> String
    {
        unsafe
        {
            let len = 255;
            let mut buf = Vec::<u8>::with_capacity(len);
            let ptr = buf.as_mut_slice().as_mut_ptr();
            let err = gethostname(ptr as *mut libc::c_char, len as libc::size_t) as i32;

            match err
            {
                0 =>
                {
                    let real_len;
                    let mut i = 0;
                    
                    loop
                    {
                        let byte = *(((ptr as u64) + (i as u64)) as *const u8);
                        if byte == 0
                        {
                            real_len = i;
                            break;
                        }

                        i += 1;
                    }

                    buf.set_len(real_len);
                    String::from_utf8_lossy(buf.as_slice()).into_owned()
                }

                _ => String::from("unknown"),
            }
        }
    }

    pub fn is_arithmetic(line: &str) -> bool
    {
        if !re_contains(line, r"[0-9]+") { return false; }

        if !re_contains(line, r"\+|\-|\*|/|\^") { return false; }

        re_contains(line, r"^[ 0-9\.\(\)\+\-\*/\^]+[\.0-9 \)]$")
    }

    pub fn create_raw_fd_from_file(file_name: &str, append: bool) -> Result<i32, String>
    {
        let mut oos = OpenOptions::new();
        if append { oos.append(true); }
        
        else
        {
            oos.write(true);
            oos.truncate(true);
        }

        match oos.create(true).open(file_name)
        {
            Ok(x) =>
            {
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
        let file = match File::open(path)
        {
            Err(why) =>
            {
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
        return re.replace_all(path, "\\$c").to_string();
    }

    pub fn get_current_dir() -> String
    {
        let mut current_dir = PathBuf::new();
        match env::current_dir()
        {
            Ok(x) => current_dir = x,
            Err(e) => { println_stderr!("env current_dir() failed: {}", e); }
        }

        let mut str_current_dir = "";
        match current_dir.to_str()
        {
            Some(x) => str_current_dir = x,
            None => { println_stderr!("current_dir to str failed."); }
        }

        str_current_dir.to_string()
    }

    pub fn split_into_fields( sh: &shell::Shell, line: &str, envs: &HashMap<String, String> ) -> Vec<String>
    {
        let ifs_chars;

        if envs.contains_key("IFS"){ ifs_chars = envs[&"IFS".to_string()].chars().collect(); }
        
        else if let Some(x) = sh.get_env("IFS") { ifs_chars = x.chars().collect(); }
        
        else if let Ok(x) = env::var("IFS") { ifs_chars = x.chars().collect(); }
        
        else { ifs_chars = vec![]; }

        if ifs_chars.is_empty()
        {
            return line
            .split(&[' ', '\t', '\n'][..])
            .map(|x| x.to_string())
            .collect();
        }
        else { return line.split(&ifs_chars[..]).map(|x| x.to_string()).collect(); }
    }

    pub fn is_builtin(s: &str) -> bool 
    {
        let builtins = 
        [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
            "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
            "minfd", "set", "unset", "unpath",
        ];

        builtins.contains(&s)
    }

    pub fn init_path_env() 
    {
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
/**/
pub mod ctime
{

}
/**/
pub mod builtins
{

}
/**/
pub mod calculator
{

}
/**/
pub mod completers
{

}
/**/
pub mod execute
{

}
/**/
pub mod highlight
{

}
/**/
pub mod history
{

}
/**/
pub mod jobc
{

}
/**/
pub mod libs
{

}
/**/
pub mod parsers
{

}
/**/
pub mod prompt
{

}
/**/
pub mod rcfile
{

} 
/**/
pub mod types
{
    use ::
    {
        collections::{ HashMap, HashSet },
        parsers::
        {
            self,
            parser_line::tokens_to_redirections,
        },
        regex::{ Regex },
        *,
    };

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
                true if self.is_exited()    => { "Exited".to_string() }
                true if self.is_stopped()   => { "Stopped".to_string() }
                true if self.is_continued() => { "Continued".to_string() }
                true if self.is_signaled()  => { "Signaled".to_string() }
                true if self.is_others()    => { "Others".to_string() }
                true if self.is_error()     => { "Error".to_string() }
                true if self.is_exited()    => { "Exited".to_string() }
                true if self.is_exited()    => { "Exited".to_string() }
                unknown                     => { format!( "unknown: {}", self.2 ) }
            }
        }

        pub fn get_status(&self) -> i32 {
            if self.is_exited() {
                self.2
            } else {
                self._get_signaled_status()
            }
        }
    }

    impl fmt::Debug for WaitStatus
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            let mut formatter = f.debug_struct("WaitStatus");
            formatter.field("pid", &self.0);
            let name = self.get_name();
            formatter.field("name", &name);
            formatter.field("ext", &self.2);
            formatter.finish()
        }
    }

    pub type Token = (String, String);
    pub type Tokens = Vec<Token>;
    pub type Redirection = (String, String, String);

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
    /// command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    /// Command {
    ///     tokens: [("", "ls"), ("", "-G"), ("\'", "foo bar")],
    ///     redirects_to: [
    ///         ("2", ">", "&1"),
    ///         ("1", ">", "/dev/null"),
    ///     ],
    ///     redirect_from: Some(("<", "one-file")),
    /// }
    #[derive(Debug)]
    pub struct Command 
    {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }

    #[derive(Debug)]
    pub struct CommandLine
    {
        pub line: String,
        pub commands: Vec<Command>,
        pub envs: HashMap<String, String>,
        pub background: bool,
    }

    impl Command
    {
        pub fn from_tokens(tokens: Tokens) -> Result<Command, String>
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

            Ok(
                Command
                {
                    tokens: tokens_final,
                    redirects_to,
                    redirect_from,
                })
            }

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

        pub fn is_builtin(&self) -> bool { tools::is_builtin(&self.tokens[0].1) }
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

    #[allow(dead_code)]
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
        pub const fn new() -> CommandResult
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

    #[allow(dead_code)]
    #[derive(Clone, Debug, Default)]
    pub struct CommandOptions
    {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }

    fn split_tokens_by_pipes(tokens: &[Token]) -> Vec<Tokens>
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
                cmds.push(cmd.clone());
                cmd = Vec::new();
            } 
            else { cmd.push(token.clone()); }
        }

        if cmd.is_empty() { return Vec::new(); }

        cmds.push(cmd.clone());
        cmds
    }

    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String>
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !libs::re::re_contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") { break; }

            for cap in re.captures_iter(text)
            {
                let name = cap[1].to_string();
                let value = parsers::parser_line::unquote(&cap[2]);
                envs.insert(name, value);
            }

            n += 1;
        }

        if n > 0 { tokens.drain(0..n); }
        envs
    }

    impl CommandLine
    {
        pub fn from_line(line: &str, sh: &mut shell::Shell) -> Result<CommandLine, String>
        {
            let linfo = parsers::parser_line::parse_line(line);
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

            Ok
            (
                CommandLine
                {
                    line:line.to_string(),
                    commands,
                    envs,
                    background,
                }
            ) 
        }

        pub fn is_empty(&self) -> bool { self.commands.is_empty() }

        pub fn with_pipeline(&self) -> bool { self.commands.len() > 1 }

        pub fn is_single_and_builtin(&self) -> bool { self.commands.len() == 1 && self.commands[0].is_builtin() }
    }
}
/*
uuid v0.0.0 */
pub mod uuid
{
    //! Generate and parse universally unique identifiers (UUIDs).
    pub mod builder
    {
        //! A Builder type for [`Uuid`]s.
        use ::
        {
            uuid::
            {
                error::{ * },
                timestamp, Bytes, Uuid, Variant, Version,
            },
            *,
        };
        /// A builder for creating a UUID.
        #[derive(Debug)]
        pub struct Builder( Uuid );

        impl Uuid
        {
            /// The 'nil UUID' (all zeros).
            pub const fn nil() -> Self { Uuid::from_bytes([0; 16]) }
            /// The 'max UUID' (all ones).
            pub const fn max() -> Self { Uuid::from_bytes([0xFF; 16]) }
            /// Creates a UUID from four field values.
            pub const fn from_fields(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Uuid
            {
                Uuid::from_bytes
                ([
                    (d1 >> 24) as u8, (d1 >> 16) as u8, (d1 >> 8) as u8, d1 as u8,
                    (d2 >> 8) as u8, d2 as u8,
                    (d3 >> 8) as u8, d3 as u8,
                    d4[0], d4[1], d4[2], d4[3], d4[4], d4[5], d4[6], d4[7],
                ])
            }
            /// Creates a UUID from four field values in little-endian order.
            pub const fn from_fields_le(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Uuid
            {
                Uuid::from_bytes
                ([
                    d1 as u8, (d1 >> 8) as u8, (d1 >> 16) as u8, (d1 >> 24) as u8,
                    (d2) as u8, (d2 >> 8) as u8,
                    d3 as u8, (d3 >> 8) as u8,
                    d4[0], d4[1], d4[2], d4[3], d4[4], d4[5], d4[6], d4[7],
                ])
            }
            /// Creates a UUID from a 128bit value.
            pub const fn from_u128(v: u128) -> Self
            {
                Uuid::from_bytes
                ([
                    (v >> 120) as u8, (v >> 112) as u8, (v >> 104) as u8, (v >> 96) as u8, (v >> 88) as u8, 
                    (v >> 80) as u8, (v >> 72) as u8, (v >> 64) as u8, (v >> 56) as u8, (v >> 48) as u8, 
                    (v >> 40) as u8, (v >> 32) as u8, (v >> 24) as u8, (v >> 16) as u8, (v >> 8) as u8, v as u8,
                ])
            }
            /// Creates a UUID from a 128bit value in little-endian order.
            pub const fn from_u128_le(v: u128) -> Self
            {
                Uuid::from_bytes
                ([
                    v as u8, (v >> 8) as u8, (v >> 16) as u8, (v >> 24) as u8, (v >> 32) as u8, (v >> 40) as u8, 
                    (v >> 48) as u8, (v >> 56) as u8, (v >> 64) as u8, (v >> 72) as u8, (v >> 80) as u8, 
                    (v >> 88) as u8, (v >> 96) as u8, (v >> 104) as u8, (v >> 112) as u8, (v >> 120) as u8,
                ])
            }
            /// Creates a UUID from two 64bit values.
            pub const fn from_u64_pair(high_bits: u64, low_bits: u64) -> Self
            {
                Uuid::from_bytes
                ([
                    (high_bits >> 56) as u8, (high_bits >> 48) as u8, (high_bits >> 40) as u8,
                    (high_bits >> 32) as u8,  (high_bits >> 24) as u8, (high_bits >> 16) as u8,
                    (high_bits >> 8) as u8, high_bits as u8,

                    (low_bits >> 56) as u8, (low_bits >> 48) as u8, (low_bits >> 40) as u8, (low_bits >> 32) as u8,
                    (low_bits >> 24) as u8, (low_bits >> 16) as u8, (low_bits >> 8) as u8, low_bits as u8,
                ])
            }
            /// Creates a UUID using the supplied bytes.
            pub fn from_slice(b: &[u8]) -> Result<Uuid, Error>
            {
                if b.len() != 16 { return Err(Error(ErrorKind::ByteLength { len: b.len() })); }

                let mut bytes: Bytes = [0; 16];
                bytes.copy_from_slice(b);
                Ok(Uuid::from_bytes(bytes))
            }

            /// Creates a UUID using the supplied bytes in little endian order.
            pub fn from_slice_le(b: &[u8]) -> Result<Uuid, Error>
            {
                if b.len() != 16 { return Err(Error(ErrorKind::ByteLength { len: b.len() })); }

                let mut bytes: Bytes = [0; 16];
                bytes.copy_from_slice(b);
                Ok(Uuid::from_bytes_le(bytes))
            }
            /// Creates a UUID using the supplied bytes.
            pub const fn from_bytes(bytes: Bytes) -> Uuid { Uuid(bytes) }
            /// Creates a UUID using the supplied bytes in little endian order.
            pub const fn from_bytes_le(b: Bytes) -> Uuid
            {
                Uuid
                ([
                    b[3], b[2], b[1], b[0], b[5], b[4], b[7], b[6], b[8], 
                    b[9], b[10], b[11], b[12], b[13], b[14], b[15],
                ])
            }
            /// Creates a reference to a UUID from a reference to the supplied bytes.
            pub fn from_bytes_ref(bytes: &Bytes) -> &Uuid
            { unsafe { &*(bytes as *const Bytes as *const Uuid) } }
        }

        impl Builder
        {
            /// Creates a `Builder` using the supplied bytes.
            pub const fn from_bytes(b: Bytes) -> Self { Builder(Uuid::from_bytes(b)) }
            /// Creates a `Builder` using the supplied bytes in little endian order.
            pub const fn from_bytes_le(b: Bytes) -> Self { Builder(Uuid::from_bytes_le(b)) }
            /// Creates a `Builder` for a version 1 UUID using the supplied timestamp and node ID.
            pub const fn from_rfc4122_timestamp(ticks: u64, counter: u16, node_id: &[u8; 6]) -> Self 
            { Builder(timestamp::encode_rfc4122_timestamp(ticks, counter, node_id)) }
            /// Creates a `Builder` for a version 3 UUID using the supplied MD5 hashed bytes.
            pub const fn from_md5_bytes(md5_bytes: Bytes) -> Self
            {
                Builder(Uuid::from_bytes(md5_bytes))
                .with_variant(Variant::RFC4122)
                .with_version(Version::Md5)
            }
            /// Creates a `Builder` for a version 4 UUID using the supplied random bytes.
            pub const fn from_random_bytes(random_bytes: Bytes) -> Self
            {
                Builder(Uuid::from_bytes(random_bytes))
                .with_variant(Variant::RFC4122)
                .with_version(Version::Random)
            }
            /// Creates a `Builder` for a version 5 UUID using the supplied SHA-1 hashed bytes.
            pub const fn from_sha1_bytes(sha1_bytes: Bytes) -> Self
            {
                Builder(Uuid::from_bytes(sha1_bytes))
                .with_variant(Variant::RFC4122)
                .with_version(Version::Sha1)
            }
            /// Creates a `Builder` for a version 6 UUID using the supplied timestamp and node ID.
            pub const fn from_sorted_rfc4122_timestamp( ticks: u64, counter: u16, node_id: &[u8; 6] ) -> Self
            { Builder(timestamp::encode_sorted_rfc4122_timestamp( ticks, counter, node_id )) }
            /// Creates a `Builder` for a version 7 UUID using the supplied Unix timestamp and random bytes.
            pub const fn from_unix_timestamp_millis(millis: u64, random_bytes: &[u8; 10]) -> Self
            {
                Builder(timestamp::encode_unix_timestamp_millis
                (
                    millis,
                    random_bytes,
                ))
            }
            /// Creates a `Builder` for a version 8 UUID using the supplied user-defined bytes.
            pub const fn from_custom_bytes(custom_bytes: Bytes) -> Self
            {
                Builder::from_bytes(custom_bytes)
                .with_variant(Variant::RFC4122)
                .with_version(Version::Custom)
            }
            /// Creates a `Builder` using the supplied bytes.
            pub fn from_slice(b: &[u8]) -> Result<Self, Error> { Ok(Builder(Uuid::from_slice(b)?)) }
            /// Creates a `Builder` using the supplied bytes in little endian order.
            pub fn from_slice_le(b: &[u8]) -> Result<Self, Error> { Ok(Builder(Uuid::from_slice_le(b)?)) }
            /// Creates a `Builder` from four field values.
            pub const fn from_fields(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Self 
            { Builder(Uuid::from_fields(d1, d2, d3, d4)) }
            /// Creates a `Builder` from four field values.
            pub const fn from_fields_le(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Self 
            { Builder(Uuid::from_fields_le(d1, d2, d3, d4)) }
            /// Creates a `Builder` from a 128bit value.
            pub const fn from_u128(v: u128) -> Self { Builder(Uuid::from_u128(v)) }
            /// Creates a UUID from a 128bit value in little-endian order.
            pub const fn from_u128_le(v: u128) -> Self { Builder(Uuid::from_u128_le(v)) }
            /// Creates a `Builder` with an initial [`Uuid::nil`].
            pub const fn nil() -> Self { Builder(Uuid::nil()) }
            /// Specifies the variant of the UUID.
            pub fn set_variant(&mut self, v: Variant) -> &mut Self
            {
                *self = Builder(self.0).with_variant(v);
                self
            }
            /// Specifies the variant of the UUID.
            pub const fn with_variant(mut self, v: Variant) -> Self
            {
                let byte = (self.0).0[8];

                (self.0).0[8] = match v
                {
                    Variant::NCS => byte & 0x7f,
                    Variant::RFC4122 => (byte & 0x3f) | 0x80,
                    Variant::Microsoft => (byte & 0x1f) | 0xc0,
                    Variant::Future => byte | 0xe0,
                };

                self
            }
            /// Specifies the version number of the UUID.
            pub fn set_version(&mut self, v: Version) -> &mut Self
            {
                *self = Builder(self.0).with_version(v);
                self
            }
            /// Specifies the version number of the UUID.
            pub const fn with_version(mut self, v: Version) -> Self
            {
                (self.0).0[6] = ((self.0).0[6] & 0x0f) | ((v as u8) << 4);
                self
            }
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid { &self.0 }
            /// Convert the builder into a [`Uuid`].
            pub const fn into_uuid(self) -> Uuid { self.0 }
        }
    }
    pub mod error
    {
        use ::
        {
            *,
        };
        /// A general error that can occur when working with UUIDs.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct Error(pub(crate) ErrorKind);

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub(crate) enum ErrorKind
        {
            /// Invalid character in the [`Uuid`] string.
            Char { character: char, index: usize },
            /// A simple [`Uuid`] didn't contain 32 characters.
            SimpleLength { len: usize },
            /// A byte array didn't contain 16 bytes
            ByteLength { len: usize },
            /// A hyphenated [`Uuid`] didn't contain 5 groups
            GroupCount { count: usize },
            /// A hyphenated [`Uuid`] had a group that wasn't the right length
            GroupLength
            {
                group: usize,
                len: usize,
                index: usize,
            },
            /// The input was not a valid UTF8 string
            InvalidUTF8,
            /// Some other error occurred.
            Other,
        }
        /// A string that is guaranteed to fail to parse to a [`Uuid`].
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct InvalidUuid<'a>(pub(crate) &'a [u8]);

        impl<'a> InvalidUuid<'a>
        {
            /// Converts the lightweight error type into detailed diagnostics.
            pub fn into_err(self) -> Error
            {
                // Check whether or not the input was ever actually a valid UTF8 string
                let input_str = match std::str::from_utf8(self.0)
                {
                    Ok(s) => s,
                    Err(_) => return Error(ErrorKind::InvalidUTF8),
                };

                let (uuid_str, offset, simple) = match input_str.as_bytes()
                {
                    [b'{', s @ .., b'}'] => (s, 1, false),
                    [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..] =>
                    {
                        (s, "urn:uuid:".len(), false)
                    }
                    
                    s => (s, 0, true),
                };

                let mut hyphen_count = 0;
                let mut group_bounds = [0; 4];
                let uuid_str = unsafe { str::from_utf8_unchecked(uuid_str) };

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
                    
                    else if !matches!(byte, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
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
                        len: input_str.len()
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
                    ErrorKind::Char { character, index, .. } => 
                    { write!(f, "invalid character: expected an optional prefix of `urn:uuid:` followed by [0-9a-fA-F-], found `{}` at {}", character, index) }
                    ErrorKind::SimpleLength { len } =>
                    {
                        write ( f, "invalid length: expected length 32 for simple format, found {}", len )
                    }
                    ErrorKind::ByteLength { len } =>
                    {
                        write!(f, "invalid length: expected 16 bytes, found {}", len)
                    }
                    ErrorKind::GroupCount { count } =>
                    {
                        write!(f, "invalid group count: expected 5, found {}", count)
                    }
                    ErrorKind::GroupLength { group, len, .. } =>
                    {
                        let expected = [8, 4, 4, 4, 12][group];
                        write!
                        (
                            f,
                            "invalid group length in group {}: expected {}, found {}",
                            group, expected, len
                        )
                    }
                    ErrorKind::InvalidUTF8 => write!(f, "non-UTF8 input"),
                    ErrorKind::Other => write!(f, "failed to parse a UUID"),
                }
            }
        }
    }
    pub mod parser
    {
        //! [`Uuid`] parsing constructs and utilities.
        use ::
        {
            convert::{ TryFrom },
            uuid::
            {
                error::*,
                Uuid,
            },
            *,
        };

        const HEX_TABLE: &[u8; 256] = &
        {
            let mut buf = [0; 256];
            let mut i: u8 = 0;

            loop
            {
                buf[i as usize] = match i
                {
                    b'0'..=b'9' => i - b'0',
                    b'a'..=b'f' => i - b'a' + 10,
                    b'A'..=b'F' => i - b'A' + 10,
                    _ => 0xff,
                };

                if i == 255 { break buf; }

                i += 1
            }
        };

        const SHL4_TABLE: &[u8; 256] = &
        {
            let mut buf = [0; 256];
            let mut i: u8 = 0;

            loop
            {
                buf[i as usize] = i.wrapping_shl(4);

                if i == 255 { break buf; }

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
            pub const fn try_parse(input: &str) -> Result<Uuid, Error> { Self::try_parse_ascii(input.as_bytes()) }
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

        const fn try_parse(input: &[u8]) -> Result<[u8; 16], InvalidUuid>
        {
            let result = match (input.len(), input)
            {
                (32, s) => parse_simple(s),
                (36, s)
                | (38, [b'{', s @ .., b'}'])
                | (45, [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..]) => {
                    parse_hyphenated(s)
                }
                _ => Err(()),
            };

            match result
            {
                Ok(b) => Ok(b),
                Err(()) => Err(InvalidUuid(input)),
            }
        }

        #[inline] const fn parse_simple(s: &[u8]) -> Result<[u8; 16], ()>
        {
            if s.len() != 32 { return Err(()); }

            let mut buf: [u8; 16] = [0; 16];
            let mut i = 0;

            while i < 16
            {
                let h1 = HEX_TABLE[s[i * 2] as usize];
                let h2 = HEX_TABLE[s[i * 2 + 1] as usize];
                
                if h1 | h2 == 0xff { return Err(()); }

                buf[i] = SHL4_TABLE[h1 as usize] | h2;
                i += 1;
            }

            Ok(buf)
        }

        #[inline] const fn parse_hyphenated(s: &[u8]) -> Result<[u8; 16], ()>
        {            
            if s.len() != 36 { return Err(()); }
            
            match [s[8], s[13], s[18], s[23]]
            {
                [b'-', b'-', b'-', b'-'] => {}
                _ => return Err(()),
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

                if h1 | h2 | h3 | h4 == 0xff { return Err(()); }

                buf[j * 2] = SHL4_TABLE[h1 as usize] | h2;
                buf[j * 2 + 1] = SHL4_TABLE[h3 as usize] | h4;
                j += 1;
            }

            Ok(buf)
        }
    }
    pub mod fmt
    {
        //! Adapters for alternative string formats.
        use ::
        {
            borrow::{ Borrow },
            uuid::{ Uuid, Variant },
            *,
        };

        macro_rules! impl_fmt_traits
        {
            ($($T:ident<$($a:lifetime),*>),+) => 
            {$(
                impl<$($a),*> fmt::Display for $T<$($a),*>
                {
                    #[inline]
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
                }

                impl<$($a),*> fmt::LowerHex for $T<$($a),*> 
                {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    { f.write_str(self.encode_lower(&mut [0; Self::LENGTH])) }
                }

                impl<$($a),*> fmt::UpperHex for $T<$($a),*>
                {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    { f.write_str(self.encode_upper(&mut [0; Self::LENGTH])) }
                }

                impl_fmt_from!($T<$($a),*>);
            )+}
        }

        macro_rules! impl_fmt_from
        {
            ($T:ident<>) =>
            {
                impl From<Uuid> for $T {
                    #[inline]
                    fn from(f: Uuid) -> Self {
                        $T(f)
                    }
                }

                impl From<$T> for Uuid {
                    #[inline]
                    fn from(f: $T) -> Self {
                        f.into_uuid()
                    }
                }

                impl AsRef<Uuid> for $T {
                    #[inline]
                    fn as_ref(&self) -> &Uuid {
                        &self.0
                    }
                }

                impl Borrow<Uuid> for $T {
                    #[inline]
                    fn borrow(&self) -> &Uuid {
                        &self.0
                    }
                }
            };

            ($T:ident<$a:lifetime>) => 
            {
                impl<$a> From<&$a Uuid> for $T<$a> {
                    #[inline]
                    fn from(f: &$a Uuid) -> Self {
                        $T::from_uuid_ref(f)
                    }
                }

                impl<$a> From<$T<$a>> for &$a Uuid {
                    #[inline]
                    fn from(f: $T<$a>) -> &$a Uuid {
                        f.0
                    }
                }

                impl<$a> AsRef<Uuid> for $T<$a> {
                    #[inline]
                    fn as_ref(&self) -> &Uuid {
                        self.0
                    }
                }

                impl<$a> Borrow<Uuid> for $T<$a> {
                    #[inline]
                    fn borrow(&self) -> &Uuid {
                        self.0
                    }
                }
            };
        }

        impl fmt::Debug for Uuid
        {
            #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
        }

        impl fmt::Display for Uuid
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
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
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self.as_hyphenated(), f) }
        }

        impl fmt::UpperHex for Uuid
        {
            #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            { fmt::UpperHex::fmt(self.as_hyphenated(), f) }
        }
        /// Format a [`Uuid`] as a hyphenated string, like `67e55044-10b1-426f-9247-bb680e5fe0c8`.
        #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub struct Hyphenated( Uuid );
        /// Format a [`Uuid`] as a simple string, like `67e5504410b1426f9247bb680e5fe0c8`.
        #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub struct Simple( Uuid );
        /// Format a [`Uuid`] as a URN string, like `urn:uuid:67e55044-10b1-426f-9247-bb680e5fe0c8`.
        #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub struct Urn( Uuid );
        /// Format a [`Uuid`] as a braced hyphenated string, like `{67e55044-10b1-426f-9247-bb680e5fe0c8}`.
        #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub struct Braced( Uuid );

        impl Uuid
        {
            /// Get a [`Hyphenated`] formatter.
            #[inline] pub const fn hyphenated(self) -> Hyphenated { Hyphenated(self) }
            /// Get a borrowed [`Hyphenated`] formatter.
            #[inline] pub fn as_hyphenated(&self) -> &Hyphenated 
            { unsafe { &*(self as *const Uuid as *const Hyphenated) } }
            /// Get a [`Simple`] formatter.
            #[inline] pub const fn simple(self) -> Simple { Simple(self) }
            /// Get a borrowed [`Simple`] formatter.
            #[inline] pub fn as_simple(&self) -> &Simple { unsafe { &*(self as *const Uuid as *const Simple) } }
            /// Get a [`Urn`] formatter.
            #[inline] pub const fn urn(self) -> Urn { Urn(self) }
            /// Get a borrowed [`Urn`] formatter.
            #[inline] pub fn as_urn(&self) -> &Urn { unsafe { &*(self as *const Uuid as *const Urn) } }
            /// Get a [`Braced`] formatter.
            #[inline]  pub const fn braced(self) -> Braced { Braced(self) }
            /// Get a borrowed [`Braced`] formatter.
            #[inline]
            pub fn as_braced(&self) -> &Braced { unsafe { &*(self as *const Uuid as *const Braced) } }
        }

        const UPPER: [u8; 16] =
        [
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
        ];

        const LOWER: [u8; 16] =
        [
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f',
        ];

        #[inline] const fn format_simple(src: &[u8; 16], upper: bool) -> [u8; 32]
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

        #[inline] const fn format_hyphenated(src: &[u8; 16], upper: bool) -> [u8; 36]
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

                if group_idx < 4 { dst[end] = b'-'; }
                
                group_idx += 1;
            }

            dst
        }

        #[inline] fn encode_simple<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            unsafe
            {
                let buf = &mut buffer[..Simple::LENGTH];
                let dst = buf.as_mut_ptr();
                ptr::write(dst.cast(), format_simple(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }

        #[inline] fn encode_hyphenated<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            unsafe
            {
                let buf = &mut buffer[..Hyphenated::LENGTH];
                let dst = buf.as_mut_ptr();
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }

        #[inline] fn encode_braced<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            unsafe
            {
                let buf = &mut buffer[..Braced::LENGTH];
                buf[0] = b'{';
                buf[Braced::LENGTH - 1] = b'}';
                let dst = buf.as_mut_ptr().add(1);
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }

        #[inline] fn encode_urn<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str
        {
            unsafe
            {
                let buf = &mut buffer[..Urn::LENGTH];
                buf[..9].copy_from_slice(b"urn:uuid:");
                let dst = buf.as_mut_ptr().add(9);
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }

        impl Hyphenated
        {
            /// The length of a hyphenated [`Uuid`] string.
            pub const LENGTH: usize = 36;

            /// Creates a [`Hyphenated`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self { Hyphenated(uuid) }
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

        impl Braced
        {
            /// The length of a braced [`Uuid`] string.
            pub const LENGTH: usize = 38;
            /// Creates a [`Braced`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self { Braced(uuid) }
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

        impl Simple
        {
            /// The length of a simple [`Uuid`] string.
            pub const LENGTH: usize = 32;

            /// Creates a [`Simple`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self { Simple(uuid) }
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

        impl Urn
        {
            /// The length of a URN [`Uuid`] string.
            pub const LENGTH: usize = 45;
            /// Creates a [`Urn`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self { Urn(uuid) }
            /// Writes the [`Uuid`] as a lower-case URN string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline] pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
            { encode_urn(self.0.as_bytes(), buffer, false) }
            /// Writes the [`Uuid`] as an upper-case URN string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline] pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str
            { encode_urn(self.0.as_bytes(), buffer, true) }
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid { &self.0 }
            /// Consumes the [`Urn`], returning the underlying [`Uuid`].
            pub const fn into_uuid(self) -> Uuid { self.0 }
        }

        impl_fmt_traits!
        {
            Hyphenated<>,
            Simple<>,
            Urn<>,
            Braced<>
        }
    }
    pub mod timestamp
    {
        //! Generating UUIDs from timestamps.
        use ::
        {
            uuid::Uuid,
            *,
        };

        /// The number of 100 nanosecond ticks between the RFC4122 epoch (`1582-10-15 00:00:00`)
        /// and the Unix epoch (`1970-01-01 00:00:00`).
        pub const UUID_TICKS_BETWEEN_EPOCHS: u64 = 0x01B2_1DD2_1381_4000;
        /// A timestamp that can be encoded into a UUID.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct Timestamp 
        {
            pub(crate) seconds: u64,
            pub(crate) nanos: u32,
        }

        impl Timestamp
        {
            /// Get a timestamp representing the current system time.
            pub fn now(context: impl ClockSequence<Output = u16>) -> Self
            {
                let _ = context;
                let (seconds, nanos) = now();

                Timestamp
                {
                    seconds,
                    nanos,
                }
            }
            /// Construct a `Timestamp` from an RFC4122 timestamp and counter, as used in versions 1 and 6 UUIDs.
            pub const fn from_rfc4122(ticks: u64, counter: u16) -> Self
            {
                let _ = counter;
                let (seconds, nanos) = Self::rfc4122_to_unix(ticks);
                Timestamp
                {
                    seconds,
                    nanos,
                }
            }
            /// Construct a `Timestamp` from a Unix timestamp, as used in version 7 UUIDs.
            pub fn from_unix(context: impl ClockSequence<Output = u16>, seconds: u64, nanos: u32) -> Self
            {
                let _ = context;
                Timestamp { seconds, nanos }
            }
            /// Get the value of timestamp as an RFC4122 timestamp and counter, as used in versions 1 and 6 UUIDs.
            #[cfg(any(feature = "v1", feature = "v6"))]
            pub const fn to_rfc4122(&self) -> (u64, u16)
            {
                ( 0, 0 )
            }
            /// Get the value of the timestamp as a Unix timestamp, as used in version 7 UUIDs.
            pub const fn to_unix(&self) -> (u64, u32)
            {
                (self.seconds, self.nanos)
            }
            
            const fn unix_to_rfc4122_ticks(seconds: u64, nanos: u32) -> u64
            {
                0
            }

            const fn rfc4122_to_unix(ticks: u64) -> (u64, u32)
            {
                (
                    ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) / 10_000_000,
                    (ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) % 10_000_000) as u32 * 100,
                )
            }

            #[deprecated(note = "use `to_unix` instead; this method will be removed in a future release")]
            /// Get the number of fractional nanoseconds in the Unix timestamp.
            pub const fn to_unix_nanos(&self) -> u32
            {
                panic!("`Timestamp::to_unix_nanos` is deprecated and will be removed: use `Timestamp::to_unix` instead")
            }
        }

        pub const fn encode_rfc4122_timestamp(ticks: u64, counter: u16, node_id: &[u8; 6]) -> Uuid
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

        pub const fn decode_rfc4122_timestamp(uuid: &Uuid) -> (u64, u16)
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
        
        pub const fn encode_sorted_rfc4122_timestamp( ticks: u64, counter: u16, node_id: &[u8; 6] ) -> Uuid
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
        
        pub const fn decode_sorted_rfc4122_timestamp(uuid: &Uuid) -> (u64, u16)
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
        
        pub const fn encode_unix_timestamp_millis(millis: u64, random_bytes: &[u8; 10]) -> Uuid
        {
            let millis_high = ((millis >> 16) & 0xFFFF_FFFF) as u32;
            let millis_low = (millis & 0xFFFF) as u16;

            let random_and_version = 
            (random_bytes[1] as u16 | ((random_bytes[0] as u16) << 8) & 0x0FFF) | (0x7 << 12);

            let mut d4 = [0; 8];

            d4[0] = (random_bytes[2] & 0x3F) | 0x80;
            d4[1] = random_bytes[3];
            d4[2] = random_bytes[4];
            d4[3] = random_bytes[5];
            d4[4] = random_bytes[6];
            d4[5] = random_bytes[7];
            d4[6] = random_bytes[8];
            d4[7] = random_bytes[9];

            Uuid::from_fields(millis_high, millis_low, random_and_version, &d4)
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
            let dur = std::time::SystemTime::UNIX_EPOCH.elapsed().expect
            (
                "Getting elapsed time since UNIX_EPOCH. If this fails, we've somehow violated causality",
            );

            (dur.as_secs(), dur.subsec_nanos())
        }
        /// A counter that can be used by version 1 and version 6 UUIDs to support the uniqueness of timestamps.
        pub trait ClockSequence
        {
            /// The type of sequence returned by this counter.
            type Output;
            /// Get the next value in the sequence to feed into a timestamp.
            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output;
        }

        impl<'a, T: ClockSequence + ?Sized> ClockSequence for &'a T
        {
            type Output = T::Output;
            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output
            { (**self).generate_sequence(seconds, subsec_nanos) }
        }
        /// Default implementations for the [`ClockSequence`] trait.
        pub mod context
        {
            use super::ClockSequence;

            /// An empty counter that will always return the value `0`.
            #[derive(Debug, Clone, Copy, Default)]
            pub struct NoContext;

            impl ClockSequence for NoContext
            {
                type Output = u16;

                fn generate_sequence(&self, _seconds: u64, _nanos: u32) -> Self::Output { 0 }
            }
        }
    }
    pub use self::timestamp::{context::NoContext, ClockSequence, Timestamp};  
    
    pub mod v1
    {
        /* UNSUPPORTED */        
    }
    mod v3
    {
        /* UNSUPPORTED */        
    }
    mod v4
    {
        use super::Uuid;
        impl Uuid
        {
            /// Creates a random UUID.
            pub fn new_v4() -> Uuid
            {
                ::uuid::Builder::from_random_bytes( ::uuid::rng::bytes() ).into_uuid()
            }
        }
    }
    mod v5
    {
        /* UNSUPPORTED */        
    }
    mod v6
    {
        /* UNSUPPORTED */        
    }
    mod v7
    {
        /* UNSUPPORTED */        
    }
    mod v8
    {
        /* UNSUPPORTED */        
    }

    pub mod rng
    {
        pub fn bytes() -> [u8; 16]
        {
             let mut bytes = [0u8; 16];

            getrandom::getrandom(&mut bytes).unwrap_or_else(|err| 
            {
                panic!("could not retrieve random bytes for uuid: {}", err)
            });

            bytes
        }
    }

    #[macro_use] mod macros
    {
        macro_rules! define_uuid_macro
        {
            {$(#[$doc:meta])*} =>
            {

                $(#[$doc])* 
                #[macro_export] macro_rules! uuid 
                {
                    ($uuid:literal) => 
                    {{
                        const OUTPUT: $crate::uuid::Uuid = match $crate::uuid::Uuid::try_parse($uuid)
                        {
                            Ok(u) => u,
                            Err(_) =>
                            {
                                let _ = ["invalid uuid representation"][1];
                                loop {} // -> never type
                            }
                        };

                        OUTPUT
                    }};
                }
            }
        }

        define_uuid_macro!
        {
            /// Parse [`Uuid`][uuid::Uuid]s from string literals at compile time.
        }
    }
    /// A 128-bit (16 byte) buffer containing the UUID.
    pub type Bytes = [u8; 16];
    /// A 128-bit (16 byte) buffer containing the UUID.
    #[repr( u8 )] #[non_exhaustive] #[derive( Clone, Copy, Debug, PartialEq )]
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
    #[repr( u8 )] #[non_exhaustive] #[derive( Clone, Copy, Debug, PartialEq )]
    pub enum Variant
    {
        /// Reserved by the NCS for backward compatibility.
        NCS = 0u8,
        /// As described in the RFC4122 Specification (default).
        RFC4122,
        /// Reserved by Microsoft for backward compatibility.
        Microsoft,
        /// Reserved for future expansion.
        Future,
    }
    /// A Universally Unique Identifier (UUID).
    #[repr( transparent )] #[derive( Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd )]    
    pub struct Uuid( Bytes );

    impl Uuid
    {
        /// UUID namespace for Domain Name System (DNS).
        pub const NAMESPACE_DNS: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        /// UUID namespace for ISO Object Identifiers (OIDs).
        pub const NAMESPACE_OID: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        /// UUID namespace for Uniform Resource Locators (URLs).
        pub const NAMESPACE_URL: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
        /// UUID namespace for X.500 Distinguished Names (DNs).
        pub const NAMESPACE_X500: Self = Uuid
        ([ 0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 ]);
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

            let d4: &[u8; 8] = convert::TryInto::try_into(&bytes[8..16]).unwrap();

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
            let d4: &[u8; 8] = convert::TryInto::try_into(&self.as_bytes()[8..16]).unwrap();
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
        pub const fn as_bytes(&self) -> &Bytes { &self.0 }
        /// Consumes self and returns the underlying byte value of the UUID.
        pub const fn into_bytes(self) -> Bytes { self.0 }
        /// Returns the bytes of the UUID in little-endian order.
        pub const fn to_bytes_le(&self) -> Bytes
        {
            [
                self.0[3], self.0[2], self.0[1], self.0[0], self.0[5], self.0[4], self.0[7], self.0[6], self.0[8], 
                self.0[9], self.0[10], self.0[11], self.0[12], self.0[13], self.0[14], self.0[15],
            ]
        }
        /// Tests if the UUID is nil (all zeros).
        pub const fn is_nil(&self) -> bool { self.as_u128() == u128::MIN }
        /// Tests if the UUID is max (all ones).
        pub const fn is_max(&self) -> bool { self.as_u128() == u128::MAX }
        /// A buffer that can be used for `encode_...` calls, that is
        /// guaranteed to be long enough for any of the format adapters.
        pub const fn encode_buffer() -> [u8; fmt::Urn::LENGTH] { [0; fmt::Urn::LENGTH] }
        /// If the UUID is the correct version (v1, v6, or v7) this will return the timestamp,
        // and counter portion parsed from a V1 UUID.
        pub const fn get_timestamp(&self) -> Option<Timestamp>
        {
            match self.get_version()
            {
                Some(Version::Mac) =>
                {
                    let (ticks, counter) = timestamp::decode_rfc4122_timestamp(self);
                    Some(Timestamp::from_rfc4122(ticks, counter))
                }
                
                Some(Version::SortMac) =>
                {
                    let (ticks, counter) = timestamp::decode_sorted_rfc4122_timestamp(self);
                    Some(Timestamp::from_rfc4122(ticks, counter))
                }
                
                Some(Version::SortRand) =>
                {
                    let millis = timestamp::decode_unix_timestamp_millis(self);
                    let seconds = millis / 1000;
                    let nanos = ((millis % 1000) * 1_000_000) as u32;

                    Some(Timestamp
                    {
                        seconds,
                        nanos,
                        #[cfg(any(feature = "v1", feature = "v6"))]
                        counter: 0,
                    })
                }
                _ => None,
            }
        }
    }

    impl Default for Uuid
    {
        #[inline] fn default() -> Self { Uuid::nil() }
    }

    impl AsRef<[u8]> for Uuid
    {
        #[inline] fn as_ref(&self) -> &[u8] { &self.0 }
    }
    
    use ::
    {
        *,
    };

    pub use self::{ builder::Builder, error::Error };
}
/**/
pub mod scripting
{
    use ::
    {
        fs::{ File },
        io::{ Read, Write, ErrorKind },
        path::{ Path },
        regex::{ Regex, RegexBuilder },
        types::{ self, CommandResult },
        *,
    };
    /**/
    pub fn run_script(sh: &mut shell::Shell, args: &Vec<String>) -> i32
    {
        let src_file = &args[1];
        let full_src_file: String;

        if src_file.contains('/') { full_src_file = src_file.clone(); } 
        else
        {
            let full_path = libs::path::find_file_in_path(src_file, false);
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
                    { println_stderr!("cicada: {}: not a valid script file", &full_src_file); }
                    _ => { println_stderr!("cicada: {}: error: {:?}", &full_src_file, e); }
                }

                return 1;
            }
        }

        if text.contains("\\\n") 
        {
            let re = RegexBuilder::new(r#"([ \t]*\\\n[ \t]+)|([ \t]+\\\n[ \t]*)"#).multi_line(true).build().unwrap();
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
        
        for line in text.clone().lines()
        {
            if re_func_head.is_match(line.trim())
            {
                enter_func = true;
                let cap = re_func_head.captures(line.trim()).unwrap();
                func_name = cap[1].to_string();
                func_body = String::new();
                continue;
            }
            
            if re_func_tail.is_match(line.trim())
            {
                sh.set_func(&func_name, &func_body);
                enter_func = false;
                continue;
            }
            
            if enter_func
            {
                func_body.push_str(line);
                func_body.push('\n');
            }
            
            else
            {
                text_new.push_str(line);
                text_new.push('\n');
            }
        }

        let mut status = 0;
        let cr_list = run_lines(sh, &text_new, args, false);
        if let Some(last) = cr_list.last() { status = last.status; }
        
        sh.exit_on_error = false;
        status
    }

    pub fn run_lines(sh: &mut shell::Shell, lines: &str, args: &Vec<String>, capture: bool) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();

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
        }

        cr_list
    }

    fn expand_args(line: &str, args: &[String]) -> String
    {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        parsers::parser_line::tokens_to_line(&tokens)
    }

    fn expand_line_to_toknes(line: &str, args: &[String], sh: &mut shell::Shell) -> types::Tokens
    {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        shell::do_expansion(sh, &mut tokens);
        tokens
    }

    fn is_args_in_token(token: &str) -> bool { libs::re::re_contains(token, r"\$\{?[0-9@]+\}?") }

    fn expand_args_for_single_token(token: &str, args: &[String]) -> String
    {
        let re = Regex::new(r"^(.*?)\$\{?([0-9]+|@)\}?(.*)$").unwrap();
        
        if !re.is_match(token) { return token.to_string(); }

        let mut result = String::new();
        let mut _token = token.to_string();
        let mut _head = String::new();
        let mut _output = String::new();
        let mut _tail = String::new();
        loop
        {
            if !re.is_match(&_token) 
            {
                if !_token.is_empty() { result.push_str(&_token); }
                break;
            }

            for cap in re.captures_iter(&_token)
            {
                _head = cap[1].to_string();
                _tail = cap[3].to_string();
                let _key = cap[2].to_string();
                if _key == "@" { result.push_str(format!("{}{}", _head, args[1..].join(" ")).as_str()); } 
                else if let Ok(arg_idx) = _key.parse::<usize>()
                {
                    if arg_idx < args.len() { result.push_str(format!("{}{}", _head, args[arg_idx]).as_str()); }
                    else { result.push_str(&_head); }
                }
                else { result.push_str(&_head); }
            }

            if _tail.is_empty() { break; }

            _token = _tail.clone();
        }

        result
    }

    fn expand_args_in_tokens(tokens: &mut types::Tokens, args: &[String])
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, token) in tokens.iter()
        {
            if sep == "`" || sep == "'" || !is_args_in_token(token)
            {
                idx += 1;
                continue;
            }

            let _token = expand_args_for_single_token(token, args);
            buff.push((idx, _token));
            idx += 1;
        }

        for (i, text) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }

    fn run_exp_test_br
    (
        sh: &mut shell::Shell, 
        pair_br: Pair<parsers::locust::Rule>, 
        args: &Vec<String>, 
        in_loop: bool, 
        capture: bool
    ) -> (Vec<CommandResult>, bool, bool, bool)
    {
        let mut cr_list = Vec::new();
        let pairs = pair_br.into_inner();
        let mut test_pass = false;

        for pair in pairs
        {
            let rule = pair.as_rule();

            if rule == parsers::locust::Rule::IF_HEAD 
            || rule == parsers::locust::Rule::IF_ELSEIF_HEAD 
            || rule == parsers::locust::Rule::WHILE_HEAD
            {
                let pairs_test: Vec<Pair<parsers::locust::Rule>> = pair.into_inner().collect();
                let pair_test = &pairs_test[0];
                let line = pair_test.as_str().trim();
                let line_new = expand_args(line, &args[1..]);
                let mut _cr_list = execute::run_command_line(sh, &line_new, true, capture);
                
                if let Some(last) = _cr_list.last()
                {
                    if last.status == 0 { test_pass = true; }
                }
                
                continue;
            }

            if rule == parsers::locust::Rule::KW_ELSE
            {
                test_pass = true;
                continue;
            }

            if rule == parsers::locust::Rule::EXP_BODY
            {
                if !test_pass { return (cr_list, false, false, false); }

                let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);
                return (cr_list, true, _cont, _brk);
            }

            unreachable!();
        }

        (cr_list, test_pass, false, false)
    }

    fn run_exp_if
    (
        sh: &mut shell::Shell,
        pair_if: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        in_loop: bool,
        capture: bool
    ) -> ( Vec<CommandResult>, bool, bool )
    {
        let mut cr_list = Vec::new();
        let pairs = pair_if.into_inner();
        let mut met_continue = false;
        let mut met_break = false;
        
        for pair in pairs
        {
            let (mut _cr_list, passed, _cont, _brk) = run_exp_test_br(sh, pair, args, in_loop, capture);
            met_continue = _cont;
            met_break = _brk;
            cr_list.append(&mut _cr_list);

            if passed { break; }
        }

        (cr_list, met_continue, met_break)
    }

    fn get_for_result_from_init(sh: &mut shell::Shell, pair_init: Pair<parsers::locust::Rule>, args: &[String]) -> 
    Vec<String> 
    {
        let mut result: Vec<String> = Vec::new();
        let pairs = pair_init.into_inner();
        
        for pair in pairs
        {
            let rule = pair.as_rule();
            
            if rule == parsers::locust::Rule::TEST
            {
                let line = pair.as_str().trim();
                let tokens = expand_line_to_toknes(line, &args[1..], sh);
                
                for (sep, token) in tokens
                {
                    if sep.is_empty()
                    {
                        for x in token.split_whitespace()
                        {
                            result.push(x.to_string());
                        }
                    }
                    
                    else { result.push(token.clone()); }
                }
            }
        }

        result
    }

    fn get_for_result_list(sh: &mut shell::Shell, pair_head: Pair<parsers::locust::Rule>, args: &[String]) -> 
    Vec<String>
    {
        let pairs = pair_head.into_inner();
        
        for pair in pairs
        {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT { return get_for_result_from_init(sh, pair, args); }
        }

        Vec::new()
    }

    fn get_for_var_name(pair_head: Pair<parsers::locust::Rule>) -> String
    {
        let pairs = pair_head.into_inner();
        for pair in pairs
        {
            let rule = pair.as_rule();
            
            if rule == parsers::locust::Rule::FOR_INIT
            {
                let pairs_init = pair.into_inner();
                
                for pair_init in pairs_init
                {
                    let rule_init = pair_init.as_rule();
                    if rule_init == parsers::locust::Rule::FOR_VAR
                    {
                        let line = pair_init.as_str().trim();
                        return line.to_string();
                    }
                }
            }
        }
        
        String::new()
    }

    fn run_exp_for( sh:&mut shell::Shell, pair_for:Pair<parsers::locust::Rule>, args:&Vec<String>, capture:bool ) ->
    Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let pairs = pair_for.into_inner();
        let mut result_list: Vec<String> = Vec::new();
        let mut var_name: String = String::new();
        
        for pair in pairs
        {
            let rule = pair.as_rule();
            
            if rule == parsers::locust::Rule::FOR_HEAD
            {
                var_name = get_for_var_name(pair.clone());
                result_list = get_for_result_list(sh, pair.clone(), args);
                continue;
            }

            if rule == parsers::locust::Rule::EXP_BODY
            {
                for value in &result_list
                {
                    sh.set_env(&var_name, value);
                    let (mut _cr_list, _cont, _brk) = run_exp( sh, pair.clone(), args, true, capture );
                    cr_list.append(&mut _cr_list);

                    if _brk { break; }
                }
            }
        }

        cr_list
    }

    fn run_exp_while
    (
        sh: &mut shell::Shell,
        pair_while: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        capture: bool
    ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        loop
        {
            let (mut _cr_list, passed, _cont, _brk) = run_exp_test_br(sh, pair_while.clone(), args, true, capture);
            cr_list.append(&mut _cr_list);

            if !passed || _brk { break; }
        }

        cr_list
    }

    fn run_exp
    (
        sh: &mut shell::Shell,
        pair_in: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        in_loop: bool,
        capture: bool
    ) -> (Vec<CommandResult>, bool, bool)
    {
        let mut cr_list = Vec::new();
        let pairs = pair_in.into_inner();

        for pair in pairs
        {
            let line = pair.as_str().trim();
            if line.is_empty() { continue; }

            let rule = pair.as_rule();

            if rule == parsers::locust::Rule::CMD
            {
                if line == "continue"
                {
                    if in_loop { return (cr_list, true, false); }
                    
                    else
                    {
                        println_stderr!("cicada: continue: only meaningful in loops");
                        continue;
                    }
                }
                
                if line == "break" {
                    if in_loop {
                        return (cr_list, false, true);
                    } else {
                        println_stderr!("cicada: break: only meaningful in loops");
                        continue;
                    }
                }

                let line_new = expand_args(line, &args[1..]);
                let mut _cr_list = execute::run_command_line(sh, &line_new, true, capture);
                cr_list.append(&mut _cr_list);
                if let Some(last) = cr_list.last() {
                    let status = last.status;
                    if status != 0 && sh.exit_on_error {
                        return (cr_list, false, false);
                    }
                }
            }

            else if rule == parsers::locust::Rule::EXP_IF
            {
                let (mut _cr_list, _cont, _brk) = run_exp_if(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);

                if _cont { return (cr_list, true, false); }

                if _brk { return (cr_list, false, true); }
            }

            else if rule == parsers::locust::Rule::EXP_FOR
            {
                let mut _cr_list = run_exp_for(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            }
            
            else if rule == parsers::locust::Rule::EXP_WHILE
            {
                let mut _cr_list = run_exp_while(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            }
        }

        (cr_list, false, false)
    }
}
/**/
pub mod shell
{
    use ::
    {
        collections::{ HashMap, HashSet },
        regex::{ Regex },
        types::{ self, CommandLine },
        *,
    };
    /**/
    #[derive(Debug, Clone)]
    pub struct Shell
    {
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

    impl Shell
    {
        pub fn new() -> Shell
        {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = tools::get_current_dir();
            let has_terminal = proc_has_terminal();
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
            }
        }

        pub fn insert_job(&mut self, gid: i32, pid: i32, cmd: &str, status: &str, bg: bool)
        {
            let mut i = 1;
            loop
            {
                let mut indexed_job_missing = false;
                if let Some(x) = self.jobs.get_mut(&i)
                {
                    if x.gid == gid
                    {
                        x.pids.push(pid);
                        x.cmd = format!("{} | {}", x.cmd, cmd);
                        return;
                    }
                }
                else { indexed_job_missing = true; }

                if indexed_job_missing
                {
                    self.jobs.insert
                    (
                        i,
                        types::Job
                        {
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

        pub fn get_job_by_id(&self, job_id: i32) -> Option<&types::Job> { self.jobs.get(&job_id) }

        pub fn mark_job_member_continued(&mut self, pid: i32, gid: i32) -> Option<&types::Job>
        {
            if self.jobs.is_empty() { return None; }

            let mut i = 1;
            let mut idx_found = 0;
            loop
            {
                if let Some(job) = self.jobs.get_mut(&i)
                {
                    if job.gid == gid
                    {
                        job.pids_stopped.remove(&pid);
                        idx_found = i;
                        break;
                    }
                }

                i += 1;
                if i >= 65535 { break; }
            }

            self.jobs.get(&idx_found)
        }

        pub fn mark_job_member_stopped(&mut self, pid: i32, gid: i32) -> Option<&types::Job>
        {
            if self.jobs.is_empty() { return None; }

            let mut i = 1;
            let mut idx_found = 0;
            loop
            {
                if let Some(job) = self.jobs.get_mut(&i)
                {
                    if job.gid == gid
                    {
                        job.pids_stopped.insert(pid);
                        idx_found = i;
                        break;
                    }
                }

                i += 1;
                if i >= 65535 { break; }
            }

            self.jobs.get(&idx_found)
        }

        pub fn get_job_by_gid(&self, gid: i32) -> Option<&types::Job>
        {
            if self.jobs.is_empty() { return None; }

            let mut i = 1;
            loop
            {
                if let Some(x) = self.jobs.get(&i)
                {
                    if x.gid == gid { return Some(x); }
                }

                i += 1;
                if i >= 65535 { break; }
            }

            None
        }

        pub fn mark_job_as_running(&mut self, gid: i32, bg: bool)
        {
            if self.jobs.is_empty() { return; }

            let mut i = 1;
            loop
            {
                if let Some(job) = self.jobs.get_mut(&i)
                {
                    if job.gid == gid
                    {
                        job.status = "Running".to_string();
                        job.pids_stopped.clear();
                        job.is_bg = bg;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 { break; }
            }
        }

        pub fn mark_job_as_stopped(&mut self, gid: i32)
        {
            if self.jobs.is_empty() { return; }

            let mut i = 1;
            loop
            {
                if let Some(x) = self.jobs.get_mut(&i)
                {
                    if x.gid == gid
                    {
                        x.status = "Stopped".to_string();
                        x.is_bg = true;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 { break; }
            }
        }

        pub fn remove_pid_from_job(&mut self, gid: i32, pid: i32) -> Option<types::Job>
        {
            if self.jobs.is_empty() { return None; }

            let mut empty_pids = false;
            let mut i = 1;
            loop
            {
                if let Some(x) = self.jobs.get_mut(&i)
                {
                    if x.gid == gid
                    {
                        if let Ok(i_pid) = x.pids.binary_search(&pid) { x.pids.remove(i_pid); }
                        empty_pids = x.pids.is_empty();
                        break;
                    }
                }

                i += 1;
                if i >= 65535 { break; }
            }

            if empty_pids { return self.jobs.remove(&i); }

            None
        }
        /// Update existing <ENVVAR> if such name exists in ENVs, or, we define a local *Shell Variable*,
        /// which would not be exported into child processes.
        pub fn set_env(&mut self, name: &str, value: &str)
        {
            if env::var(name).is_ok() { env::set_var(name, value); }
            else { self.envs.insert(name.to_string(), value.to_string()); }
        }
        /// get *Shell Variable*, or *ENV Variable*.
        pub fn get_env(&self, name: &str) -> Option<String>
        {
            match self.envs.get(name)
            {
                Some(x) => Some(x.to_string()),
                None =>
                {
                    match env::var(name)
                    {
                        Ok(x) => Some(x),
                        Err(_) => None,
                    }
                }
            }
        }
        /// Remove environment variable, function from the environment of the currently running process
        pub fn remove_env(&mut self, name: &str) -> bool
        {
            let ptn_env = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*$").unwrap();
            if !ptn_env.is_match(name) { return false; }

            env::remove_var(name);
            self.envs.remove(name);
            self.remove_func(name);
            true
        }

        pub fn remove_path(&mut self, path: &str)
        {
            if let Ok(paths) = env::var("PATH")
            {
                let mut paths_new: Vec<&str> = paths.split(":").collect();
                paths_new.retain(|&x| x != path);
                env::set_var("PATH", paths_new.join(":").as_str());
            }
        }

        pub fn set_func( &mut self, name:&str, value:&str )
        { self.funcs.insert( name.to_string(), value.to_string() ); }

        pub fn get_func(&self, name: &str) -> Option<String> { self.funcs.get(name).map(|x| x.to_string()) }

        pub fn get_alias_list(&self) -> Vec<(String, String)>
        {
            let mut result = Vec::new();
            
            for (name, value) in &self.aliases
            {
                result.push((name.clone(), value.clone()));
            }

            result
        }

        pub fn add_alias(&mut self, name: &str, value: &str) { self.aliases.insert(name.to_string(), value.to_string()); }

        pub fn is_alias(&self, name: &str) -> bool { self.aliases.contains_key(name) }

        pub fn remove_alias(&mut self, name: &str) -> bool
        {
            let opt = self.aliases.remove(name);
            opt.is_some()
        }

        pub fn get_alias_content(&self, name: &str) -> Option<String>
        {
            let result = match self.aliases.get(name)
            {
                Some(x) => x.to_string(),
                None => String::new(),
            };

            if result.is_empty() { None }
            else { Some(result) }
        }

        fn remove_func(&mut self, name: &str) { self.funcs.remove(name); }
    }

    pub unsafe fn give_terminal_to(gid: i32) -> bool 
    {
        let mut mask: libc::sigset_t = mem::zeroed();
        let mut old_mask: libc::sigset_t = mem::zeroed();
        libc::sigemptyset(&mut mask);
        libc::sigaddset(&mut mask, libc::SIGTSTP);
        libc::sigaddset(&mut mask, libc::SIGTTIN);
        libc::sigaddset(&mut mask, libc::SIGTTOU);
        libc::sigaddset(&mut mask, libc::SIGCHLD);
        let rcode = libc::pthread_sigmask(libc::SIG_BLOCK, &mask, &mut old_mask);

        if rcode != 0 { log!("failed to call pthread_sigmask"); }

        let rcode = libc::tcsetpgrp(1, gid);
        let given;
        
        if rcode == -1
        {
            given = false;
            let e = errno();
            let code = e.0;
            log!("error in give_terminal_to() {}: {}", code, e);
        }
        else { given = true; }

        let rcode = libc::pthread_sigmask(libc::SIG_SETMASK, &old_mask, &mut mask);
        if rcode != 0 { log!("failed to call pthread_sigmask"); }

        given
    }

    fn needs_globbing(line: &str) -> bool
    {
        let re = Regex::new(r"\*+").expect("Invalid regex ptn");
        re.is_match(line)
    }

    pub fn expand_glob(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        
        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !needs_globbing(text)
            {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let item = text.as_str();

            if !item.contains('*') || item.trim().starts_with('\'') || item.trim().starts_with('"')
            { result.push(item.to_string()); }
            
            else
            {
                let _basename = libs::path::basename(item);
                let show_hidden = _basename.starts_with(".*");

                match glob::glob(item)
                {
                    Ok(paths) =>
                    {
                        let mut is_empty = true;

                        for entry in paths
                        {
                            match entry
                            {
                                Ok(path) =>
                                {
                                    let file_path = path.to_string_lossy();
                                    let _basename = libs::path::basename(&file_path);
                                    if _basename == ".." || _basename == "." { continue; }

                                    if _basename.starts_with('.') && !show_hidden { continue; }

                                    result.push(file_path.to_string());
                                    is_empty = false;
                                }

                                Err(e) => { log!("glob error: {:?}", e); }
                            }
                        }
                        if is_empty {
                            result.push(item.to_string());
                        }
                    }

                    Err(e) =>
                    {
                        println!("glob error: {:?}", e);
                        result.push(item.to_string());
                        return;
                    }
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, result) in buff.iter().rev()
        {
            tokens.remove(*i);
            
            for (j, token) in result.iter().enumerate()
            {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_one_env(sh: &Shell, token: &str) -> String
    {
        unsafe 
        {            
            let re1 = Regex::new(r"^(.*?)\$([A-Za-z0-9_]+|\$|\?)(.*)$").unwrap();
            let re2 = Regex::new(r"(.*?)\$\{([A-Za-z0-9_]+|\$|\?)\}(.*)$").unwrap();
            
            if !re1.is_match(token) && !re2.is_match(token) { return token.to_string(); }

            let mut result = String::new();
            let match_re1 = re1.is_match(token);
            let match_re2 = re2.is_match(token);
            
            if !match_re1 && !match_re2 { return token.to_string(); }

            let cap_results = if match_re1 { re1.captures_iter(token) } else { re2.captures_iter(token) };

            for cap in cap_results
            {
                let head = cap[1].to_string();
                let tail = cap[3].to_string();
                let key = cap[2].to_string();
                if key == "?" { result.push_str(format!("{}{}", head, sh.previous_status).as_str()); }
                else if key == "$"
                {
                    let val = libc::getpid();
                    result.push_str(format!("{}{}", head, val).as_str());
                }
                else if let Ok(val) = env::var(&key) { result.push_str(format!("{}{}", head, val).as_str()); }
                else if let Some(val) = sh.get_env(&key) { result.push_str(format!("{}{}", head, val).as_str()); }
                else { result.push_str(&head); }
                result.push_str(&tail);
            }

            result
        }
    }

    fn need_expand_brace(line: &str) -> bool { libs::re::re_contains(line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"#) }

    fn brace_getitem(s: &str, depth: i32) -> (Vec<String>, String)
    {
        let mut out: Vec<String> = vec![String::new()];
        let mut ss = s.to_string();
        let mut tmp;

        while !ss.is_empty()
        {
            let c = match ss.chars().next()
            {
                Some(x) => x,
                None => {
                    return (out, ss);
                }
            };

            if depth > 0 && (c == ',' || c == '}') { return (out, ss); }
            
            if c == '{' 
            {
                let mut sss = ss.clone();
                sss.remove(0);
                let result_groups = brace_getgroup(&sss, depth + 1);
                if let Some((out_group, s_group)) = result_groups
                {
                    let mut tmp_out = Vec::new();
                    for x in out.iter()
                    {
                        for y in out_group.iter()
                        {
                            let item = format!("{}{}", x, y);
                            tmp_out.push(item);
                        }
                    }

                    out = tmp_out;
                    ss = s_group.clone();
                    continue;
                }
            }
            
            if c == '\\' && ss.len() > 1
            {
                ss.remove(0);
                let c;
                match ss.chars().next()
                {
                    Some(x) => c = x,
                    None => { return (out, ss) }
                }

                tmp = format!("\\{}", c);
            }
            else { tmp = c.to_string(); }

            let mut result = Vec::new();
            
            for x in out.iter()
            {
                let item = format!("{}{}", x, tmp);
                result.push(item);
            }

            out = result;
            ss.remove(0);
        }

        (out, ss)
    }

    fn brace_getgroup(s: &str, depth: i32) -> Option<(Vec<String>, String)>
    {
        let mut out: Vec<String> = Vec::new();
        let mut comma = false;
        let mut ss = s.to_string();

        while !ss.is_empty()
        {
            let (g, sss) = brace_getitem(ss.as_str(), depth);
            ss = sss.clone();
            
            if ss.is_empty() { break; }

            for x in g.iter()
            {
                out.push(x.clone());
            }

            let c = match ss.chars().next()
            {
                Some(x) => x,
                None => { break; }
            };

            if c == '}'
            {
                let mut sss = ss.clone();
                sss.remove(0);

                if comma { return Some((out, sss)); }
                
                let mut result = Vec::new();
                
                for x in out.iter()
                {
                    let item = format!("{{{}}}", x);
                    result.push(item);
                }

                return Some((result, ss));
            }

            if c == ','
            {
                comma = true;
                ss.remove(0);
            }
        }

        None
    }

    fn expand_brace(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        
        for (sep, token) in tokens.iter()
        {
            if !sep.is_empty() || !need_expand_brace(token)
            {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let items = brace_getitem(token, 0);
            
            for x in items.0
            {
                result.push(x.clone());
            }
            
            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev()
        {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate()
            {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_brace_range(tokens: &mut types::Tokens)
    {
        let re;
        if let Ok(x) = Regex::new(r#"\{(-?[0-9]+)\.\.(-?[0-9]+)(\.\.)?([0-9]+)?\}"#) { re = x; }

        else
        {
            println_stderr!("cicada: re new error");
            return;
        }

        let mut idx: usize = 0;
        let mut buff: Vec<(usize, Vec<String>)> = Vec::new();
        for (sep, token) in tokens.iter()
        {
            if !sep.is_empty() || !re.is_match(token)
            {
                idx += 1;
                continue;
            }
            
            let caps = re.captures(token).unwrap();
            let start = match caps[1].to_string().parse::<i32>()
            {
                Ok(x) => x,
                Err(e) =>
                {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>()
            {
                Ok(x) => x,
                Err(e) =>
                {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };
            
            let mut incr = if caps.get(4).is_none() { 1 }
            else
            {
                match caps[4].to_string().parse::<i32>()
                {
                    Ok(x) => x,
                    Err(e) =>
                    {
                        println_stderr!("cicada: {}", e);
                        return;
                    }
                }
            };

            if incr <= 1 { incr = 1; }

            let mut result: Vec<String> = Vec::new();
            let mut n = start;
            if start > end 
            {
                while n >= end
                {
                    result.push(format!("{}", n));
                    n -= incr;
                }
            }
            
            else
            {
                while n <= end
                {
                    result.push(format!("{}", n));
                    n += incr;
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev()
        {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate()
            {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    fn expand_alias(sh: &Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        let mut is_head = true;
        for (sep, text) in tokens.iter()
        {
            if sep.is_empty() && text == "|"
            {
                is_head = true;
                idx += 1;
                continue;
            }
            
            if is_head && text == "xargs"
            {
                idx += 1;
                continue;
            }

            if !is_head || !sh.is_alias(text)
            {
                idx += 1;
                is_head = false;
                continue;
            }

            if let Some(value) = sh.get_alias_content(text) { buff.push((idx, value.clone())); }

            idx += 1;
            is_head = false;
        }

        for (i, text) in buff.iter().rev()
        {
            let linfo = parsers::parser_line::parse_line(text);
            let tokens_ = linfo.tokens;
            tokens.remove(*i);
            for item in tokens_.iter().rev()
            {
                tokens.insert(*i, item.clone());
            }
        }
    }

    fn expand_home(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !text.starts_with("~")
            {
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

        for (i, text) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }

    fn env_in_token(token: &str) -> bool
    {
        if libs::re::re_contains(token, r"\$\{?[\$\?]\}?") { return true; }

        let ptn_env_name = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let ptn_env = format!(r"\$\{{?{}\}}?", ptn_env_name);

        if !libs::re::re_contains(token, &ptn_env) { return false; }
        
        let ptn_cmd_sub1 = format!(r"^{}=`.*`$", ptn_env_name);
        let ptn_cmd_sub2 = format!(r"^{}=\$\(.*\)$", ptn_env_name);
        if libs::re::re_contains(token, &ptn_cmd_sub1)
        || libs::re::re_contains(token, &ptn_cmd_sub2)
        || libs::re::re_contains(token, r"^\$\(.+\)$")
        { return false; }
        
        let ptn_env = format!(r"='.*\$\{{?{}\}}?.*'$", ptn_env_name);
        !libs::re::re_contains(token, &ptn_env)
    }

    pub fn expand_env(sh: &Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, token) in tokens.iter()
        {
            if sep == "`" || sep == "'"
            {
                idx += 1;
                continue;
            }

            if !env_in_token(token)
            {
                idx += 1;
                continue;
            }

            let mut _token = token.clone();
            
            while env_in_token(&_token)
            {
                _token = expand_one_env(sh, &_token);
            }

            buff.push((idx, _token));
            idx += 1;
        }

        for (i, text) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }

    fn should_do_dollar_command_extension(line: &str) -> bool
    {
        libs::re::re_contains(line, r"\$\([^\)]+\)") &&
        !libs::re::re_contains(line, r"='.*\$\([^\)]+\).*'$")
    }

    fn do_command_substitution_for_dollar(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        unsafe
        {
            let mut idx: usize = 0;
            let mut buff: HashMap<usize, String> = HashMap::new();

            for (sep, token) in tokens.iter() 
            {
                if sep == "'" || sep == "\\" || !should_do_dollar_command_extension(token)
                {
                    idx += 1;
                    continue;
                }

                let mut line = token.to_string();
                loop
                {
                    if !should_do_dollar_command_extension(&line) { break; }

                    let ptn_cmd = r"\$\((.+)\)";
                    let cmd = match libs::re::find_first_group(ptn_cmd, &line)
                    {
                        Some(x) => x,
                        None => {
                            println_stderr!("cicada: calculator: no first group");
                            return;
                        }
                    };

                    let cmd_result = match CommandLine::from_line(&cmd, sh)
                    {
                        Ok(c) =>
                        {
                            log!("run subcmd dollar: {:?}", &cmd);
                            let (term_given, cr) = core::run_pipeline(sh, &c, true, true, false);
                            if term_given
                            {
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

            for (i, text) in buff.iter() 
            {
                tokens[*i].1 = text.to_string();
            }

        }
    }

    fn do_command_substitution_for_dot(sh: &mut Shell, tokens: &mut types::Tokens)
    {
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

    fn do_command_substitution(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        do_command_substitution_for_dot(sh, tokens);
        do_command_substitution_for_dollar(sh, tokens);
    }

    pub fn do_expansion(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        let line = parsers::parser_line::tokens_to_line(tokens);
        if tools::is_arithmetic(&line) { }

        if tokens.len() >= 2 && tokens[0].1 == "export" && tokens[1].1.starts_with("PROMPT=") { return; }

        expand_alias(sh, tokens);
        expand_home(tokens);
        expand_env(sh, tokens);
        expand_brace(tokens);
        expand_glob(tokens);
        do_command_substitution(sh, tokens);
        expand_brace_range(tokens);
    }

    pub fn trim_multiline_prompts(line: &str) -> String
    {
        let line_new = libs::re::replace_all(line, r"\\\n>> ", "");
        let line_new = libs::re::replace_all(&line_new, r"\| *\n>> ", "| ");
        libs::re::replace_all(&line_new, r"(?P<NEWLINE>\n)>> ", "$NEWLINE")
    }

    fn proc_has_terminal() -> bool
    {
        unsafe
        {
            let tgid = libc::tcgetpgrp(0);
            let pgid = libc::getpgid(0);
            tgid == pgid
        }
    }
}
/**/
pub mod signals
{
    /*
    use errno::{errno, set_errno};
    */
    use ::
    {
        collections::{ HashMap, HashSet },
        nix::
        {
            sys::
            {
                signal,
                wait::{ WaitPidFlag as WF, WaitStatus as WS, waitpid },
            },
            unistd::{ Pid },
        },
        sync::{ Mutex },
        *,
    };


    lazy_static! 
    {
        pub static ref REAP_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
        pub static ref STOP_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        pub static ref CONT_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        pub static ref KILL_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
    }

    pub fn killed_map_insert(pid: i32, sig: i32)
    {
        if let Ok(mut m) = KILL_MAP.try_lock() { m.insert(pid, sig); }
    }

    pub fn killed_map_pop(pid: i32) -> Option<i32>
    {
        if let Ok(mut m) = KILL_MAP.try_lock() { m.remove(&pid) }
        else { None }
    }

    pub fn insert_cont_map(pid: i32)
    {
        if let Ok(mut m) = CONT_MAP.try_lock() { m.insert(pid); }
    }

    pub fn pop_cont_map(pid: i32) -> bool
    {
        match CONT_MAP.try_lock()
        {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }

    pub fn insert_stopped_map(pid: i32)
    {
        if let Ok(mut m) = STOP_MAP.try_lock() { m.insert(pid); }
    }

    pub fn pop_stopped_map(pid: i32) -> bool
    {
        match STOP_MAP.try_lock()
        {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }

    pub fn insert_reap_map(pid: i32, status: i32)
    {
        if let Ok(mut m) = REAP_MAP.try_lock() { m.insert(pid, status); }
    }

    pub fn pop_reap_map(pid: i32) -> Option<i32>
    {
        match REAP_MAP.try_lock()
        {
            Ok(mut m) => m.remove(&pid),
            Err(_) => None,
        }
    }

    pub fn block_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_BLOCK, Some(&sigset), None)
        {
            Ok(_) => {},
            Err(e) => {
                log!("sigprocmask block error: {:?}", e);
            }
        }
    }

    pub fn unblock_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_UNBLOCK, Some(&sigset), None)
        {
            Ok(_) => {},
            Err(e) => { log!("sigprocmask unblock error: {:?}", e); }
        }
    }
    
    pub extern "C" fn handle_sigchld(_sig: i32)
    {
        let saved_errno = errno();
        let options = Some(WF::WUNTRACED | WF::WNOHANG | WF::WCONTINUED);
        loop
        {
            match waitpid(Pid::from_raw(-1), options)
            {
                Ok(WS::Exited(pid, status))              => { insert_reap_map(i32::from(pid), status); }
                Ok(WS::Stopped(pid, _sig))               => { insert_stopped_map(i32::from(pid)); }
                Ok(WS::Continued(pid))                   => { insert_cont_map(i32::from(pid)); }
                Ok(WS::Signaled(pid, sig, _core_dumped)) => { killed_map_insert(i32::from(pid), sig as i32); }
                Ok(WS::StillAlive)                       => { break; }
                Ok(_others)                              => { /* log!("sigchld others: {:?}", _others); */ }
                Err(e) =>
                {
                    if e == nix::Error::ECHILD { break; }
                    log!("chld waitpid error: {:?}", e);
                    break;
                }
            }
        }

        set_errno(saved_errno);
    }

    pub fn setup_sigchld_handler()
    {
        unsafe
        {
            let sigset = signal::SigSet::empty();
            let handler = signal::SigHandler::Handler(handle_sigchld);
            let flags = signal::SaFlags::SA_RESTART;
            let sa = signal::SigAction::new(handler, flags, sigset);        
            match signal::sigaction(signal::SIGCHLD, &sa)
            {
                Ok(_) => {},
                Err(e) => { log!("sigaction error: {:?}", e); }
            }
        }
    }
}
/**/
use ::
{
    io::{ stderr, Write },
    sync::{ Arc },
};
/// Represents an error calling `exec`.
pub use ::types::CommandResult;
pub use ::types::LineInfo;
/// Parse a command to tokens.
pub fn parse_line(cmd: &str) -> LineInfo { parsers::parser_line::parse_line( cmd ) }
/// Run a command or a pipeline.
pub fn run( line:&str ) -> CommandResult { execute::run( line ) }

pub unsafe fn domain()
{
    tools::init_path_env();
    let mut sh = shell::Shell::new();
    let args: Vec<String> = env::args().collect();

    if libs::progopts::is_login(&args)
    {
        rcfile::load_rc_files(&mut sh);
        sh.is_login = true;
    }

    // Initialize command cache for highlighting
    highlight::init_command_cache();
    highlight::update_aliases(&sh);

    if libs::progopts::is_script(&args)
    {
        log!("run script: {:?} ", &args);
        let status = scripting::run_script(&mut sh, &args);
        ::process::exit(status);
    }

    if libs::progopts::is_command_string(&args)
    {
        let line = tools::env_args_to_command_line();
        log!("run with -c args: {}", &line);
        execute::run_command_line(&mut sh, &line, false, false);
        ::process::exit(sh.previous_status);
    }

    if libs::progopts::is_non_tty()
    {
        execute::run_procs_for_non_tty(&mut sh);
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

    rl.define_function("enter-function", Arc::new(prompt::EnterFunction));
    rl.bind_sequence("\r", Command::from_str("enter-function"));

    let highlighter = highlight::create_highlighter();
    rl.set_highlighter(highlighter);

    history::init(&mut rl);
    rl.set_completer(Arc::new(completers::CicadaCompleter 
    {
        sh: Arc::new(sh.clone()),
    }));

    let sig_handler_enabled = tools::is_signal_handler_enabled();
    if sig_handler_enabled 
    {
        signals::setup_sigchld_handler();
        signals::block_signals();
    }
    /*
    REPL Loop */
    loop
    {
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
// 4360
