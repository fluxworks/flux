#![allow(unknown_lints)]
/*
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
extern crate pest_derive; */ 



pub mod borrow
{
    pub use std::borrow::{ * };
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod convert
{
    pub use std::convert::{ * };
}

pub mod env
{
    pub use std::env::{ * };
}

pub mod error
{
    pub use std::error::{ * };
}

pub mod ffi
{
    pub use std::ffi::{ * };
}

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod hash
{
    pub use std::hash::{ * };
}

pub mod io
{
    pub use std::io::{ * };
}

pub mod iter
{
    pub use std::iter::{ * };
}

pub mod marker
{
    pub use std::marker::{ * };
}

pub mod mem
{
    pub use std::mem::{ * };
}

pub mod num
{
    pub use std::num::{ * };
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod os
{
    pub mod unix
    {
        #[cfg(unix)] pub use std::os::unix::{ * };    
    }

    pub mod windows
    {
        #[cfg(windows)] pub use std::os::windows::{ * };    
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

pub mod time
{
    pub use std::time::{ * };
}

pub mod vec
{
    pub use std::vec::{ * };
    /* smallvec = "1.6.1" */
    pub mod small
    {
        use ::
        {
            *,
        };
    }
}

/*
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
*/
pub mod _
{
    use ::
    {
        *,
    };
}
/* bitflags = "2.0" */
#[macro_use] pub mod bitflags
{
    use ::
    {
        *,
    };
}

#[macro_use] pub mod lazy_static
{
    use ::
    {
        *,
    };
}

#[macro_use] pub mod pest_derive
{
    use ::
    {
        *,
    };
}

#[macro_use] pub mod tlog
{
    use ::
    {
        *,
    };
    pub fn getpid() -> i32 {
        unsafe { libc::getpid() }
    }
    
    #[macro_export]
    macro_rules! log {
        ($fmt:expr) => (
            let log_file = if let Ok(x) = std::env::var("CICADA_LOG_FILE") {
                x.clone()
            } else {
                String::new()
            };
    
            if !log_file.is_empty() {
                use std::io::Write as _;
    
                let msg = $fmt;
                match std::fs::OpenOptions::new().append(true).create(true).open(&log_file) {
                    Ok(mut cfile) => {
                        let pid = $crate::tlog::getpid();
                        let now = $crate::ctime::DateTime::now();
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

#[macro_use] pub mod tools
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
    
    use crate::execute;
    use crate::libs::re::re_contains;
    use crate::parsers;
    use crate::shell;
    */
    macro_rules! println_stderr 
    {
        ($fmt:expr) => (
            match writeln!(&mut ::std::io::stderr(), $fmt) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
        ($fmt:expr, $($arg:tt)*) => (
            match writeln!(&mut ::std::io::stderr(), $fmt, $($arg)*) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

    pub fn is_signal_handler_enabled() -> bool {
        env::var("CICADA_ENABLE_SIG_HANDLER").map_or(false, |x| x == "1")
    }

    pub fn get_user_name() -> String {
        match env::var("USER") {
            Ok(x) => {
                return x;
            }
            Err(e) => {
                log!("cicada: env USER error: {}", e);
            }
        }
        let cmd_result = execute::run("whoami");
        return cmd_result.stdout.trim().to_string();
    }

    pub fn get_user_home() -> String {
        match env::var("HOME") {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: env HOME error: {}", e);
                String::new()
            }
        }
    }

    pub fn get_config_dir() -> String {
        if let Ok(x) = env::var("XDG_CONFIG_HOME") {
            format!("{}/cicada", x)
        } else {
            let home = get_user_home();
            format!("{}/.config/cicada", home)
        }
    }

    pub fn get_user_completer_dir() -> String {
        let dir_config = get_config_dir();
        format!("{}/completers", dir_config)
    }

    pub fn unquote(s: &str) -> String {
        let args = parsers::parser_line::line_to_plain_tokens(s);
        if args.is_empty() {
            return String::new();
        }
        args[0].clone()
    }

    pub fn is_env(line: &str) -> bool {
        re_contains(line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$")
    }

    // #[allow(clippy::trivial_regex)]
    pub fn extend_bangbang(sh: &shell::Shell, line: &mut String) {
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

    pub fn wrap_sep_string(sep: &str, s: &str) -> String {
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

    pub fn env_args_to_command_line() -> String {
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

    extern "C" {
        fn gethostname(name: *mut libc::c_char, size: libc::size_t) -> libc::c_int;
    }

    /// via: https://gist.github.com/conradkleinespel/6c8174aee28fa22bfe26
    pub fn get_hostname() -> String {
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

    pub fn is_arithmetic(line: &str) -> bool {
        if !re_contains(line, r"[0-9]+") {
            return false;
        }
        if !re_contains(line, r"\+|\-|\*|/|\^") {
            return false;
        }
        re_contains(line, r"^[ 0-9\.\(\)\+\-\*/\^]+[\.0-9 \)]$")
    }

    pub fn create_raw_fd_from_file(file_name: &str, append: bool) -> Result<i32, String> {
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

    pub fn get_fd_from_file(file_name: &str) -> i32 {
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

    pub fn escape_path(path: &str) -> String {
        let re = Regex::new(r##"(?P<c>[!\(\)<>,\?\]\[\{\} \\'"`*\^#|$&;])"##).unwrap();
        return re.replace_all(path, "\\$c").to_string();
    }

    pub fn get_current_dir() -> String {
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

    pub fn split_into_fields(
        sh: &shell::Shell,
        line: &str,
        envs: &HashMap<String, String>,
    ) -> Vec<String> {
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
            return line
                .split(&[' ', '\t', '\n'][..])
                .map(|x| x.to_string())
                .collect();
        } else {
            return line.split(&ifs_chars[..]).map(|x| x.to_string()).collect();
        }
    }

    pub fn is_builtin(s: &str) -> bool {
        let builtins = [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
            "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
            "minfd", "set", "unset", "unpath",
        ];
        builtins.contains(&s)
    }

    pub fn init_path_env() {
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

    pub fn is_shell_altering_command(line: &str) -> bool {
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

#[macro_use] pub mod macros
{
    use ::
    {
        *,
    };
}

pub mod clap
{
    use ::
    {
        *,
    };
    // default-features = false
    pub mod v233
    {
        use ::
        {
            *,
        };
    }
    // default-features = ["std", "derive", "help"]
    pub mod v450
    {
        use ::
        {
            *,
        };
    }
}

pub mod errno
{
    use ::
    {
        *,
    };
}

pub mod exec
{
    use ::
    {
        *,
    };
}

pub mod fallible
{
    use ::
    {
        *,
    };
    /* fallible-iterator = "0.3" */
    pub mod iterator
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    /* fallible-streaming-iterator = "0.1" */
    pub mod streaming
    {
        use ::
        {
            *,
        };
        /*
        */
    }
}

pub mod glob
{
    use ::
    {
        *,
    };
}
/* hashlink = "0.9" */
pub mod hashlink
{
    use ::
    {
        *,
    };
    /*
    */
}

pub mod libc
{
    use ::
    {
        *,
    };
    
    // pub mod jobc
    pub mod job
    {
        use ::
        {
            *,
        };
        /*
        use std::io::Write;
        
        use nix::sys::signal::Signal;
        use nix::sys::wait::waitpid;
        use nix::sys::wait::WaitPidFlag as WF;
        use nix::sys::wait::WaitStatus as WS;
        use nix::unistd::Pid;
        
        use crate::shell;
        use crate::signals;
        use crate::types::{self, CommandResult};
        */
        pub fn get_job_line(job: &types::Job, trim: bool) -> String {
            let mut cmd = job.cmd.clone();
            if trim && cmd.len() > 50 {
                cmd.truncate(50);
                cmd.push_str(" ...");
            }
            let _cmd = if job.is_bg && job.status == "Running" {
                format!("{} &", cmd)
            } else {
                cmd
            };
            format!("[{}] {}  {}   {}", job.id, job.gid, job.status, _cmd)
        }
        
        pub fn print_job(job: &types::Job) {
            let line = get_job_line(job, true);
            println_stderr!("{}", line);
        }
        
        pub fn mark_job_as_done(sh: &mut shell::Shell, gid: i32, pid: i32, reason: &str) {
            if let Some(mut job) = sh.remove_pid_from_job(gid, pid) {
                job.status = reason.to_string();
                if job.is_bg {
                    println_stderr!("");
                    print_job(&job);
                }
            }
        }
        
        pub fn mark_job_as_stopped(sh: &mut shell::Shell, gid: i32, report: bool) {
            sh.mark_job_as_stopped(gid);
            if !report {
                return;
            }
        
            // add an extra line to separate output of fg commands if any.
            if let Some(job) = sh.get_job_by_gid(gid) {
                println_stderr!("");
                print_job(job);
            }
        }
        
        pub fn mark_job_member_stopped(sh: &mut shell::Shell, pid: i32, gid: i32, report: bool) {
            let _gid = if gid == 0 {
                unsafe { libc::getpgid(pid) }
            } else {
                gid
            };
        
            if let Some(job) = sh.mark_job_member_stopped(pid, gid) {
                if job.all_members_stopped() {
                    mark_job_as_stopped(sh, gid, report);
                }
            }
        }
        
        pub fn mark_job_member_continued(sh: &mut shell::Shell, pid: i32, gid: i32) {
            let _gid = if gid == 0 {
                unsafe { libc::getpgid(pid) }
            } else {
                gid
            };
        
            if let Some(job) = sh.mark_job_member_continued(pid, gid) {
                if job.all_members_running() {
                    mark_job_as_running(sh, gid, true);
                }
            }
        }
        
        pub fn mark_job_as_running(sh: &mut shell::Shell, gid: i32, bg: bool) {
            sh.mark_job_as_running(gid, bg);
        }
        
        #[allow(unreachable_patterns)]
        pub fn waitpidx(wpid: i32, block: bool) -> types::WaitStatus {
            let options = if block {
                Some(WF::WUNTRACED | WF::WCONTINUED)
            } else {
                Some(WF::WUNTRACED | WF::WCONTINUED | WF::WNOHANG)
            };
            match waitpid(Pid::from_raw(wpid), options) {
                Ok(WS::Exited(pid, status)) => {
                    let pid = i32::from(pid);
                    types::WaitStatus::from_exited(pid, status)
                }
                Ok(WS::Stopped(pid, sig)) => {
                    let pid = i32::from(pid);
                    types::WaitStatus::from_stopped(pid, sig as i32)
                }
                Ok(WS::Continued(pid)) => {
                    let pid = i32::from(pid);
                    types::WaitStatus::from_continuted(pid)
                }
                Ok(WS::Signaled(pid, sig, _core_dumped)) => {
                    let pid = i32::from(pid);
                    types::WaitStatus::from_signaled(pid, sig as i32)
                }
                Ok(WS::StillAlive) => {
                    types::WaitStatus::empty()
                }
                Ok(_others) => {
                    // this is for PtraceEvent and PtraceSyscall on Linux,
                    // unreachable on other platforms.
                    types::WaitStatus::from_others()
                }
                Err(e) => {
                    types::WaitStatus::from_error(e as i32)
                }
            }
        }
        
        pub fn wait_fg_job(sh: &mut shell::Shell, gid: i32, pids: &[i32]) -> CommandResult {
            let mut cmd_result = CommandResult::new();
            let mut count_waited = 0;
            let count_child = pids.len();
            if count_child == 0 {
                return cmd_result;
            }
            let pid_last = pids.last().unwrap();
        
            loop {
                let ws = waitpidx(-1, true);
                // here when we calling waitpidx(), all signals should have
                // been masked. There should no errors (ECHILD/EINTR etc) happen.
                if ws.is_error() {
                    let err = ws.get_errno();
                    if err == nix::Error::ECHILD {
                        break;
                    }
        
                    log!("jobc unexpected waitpid error: {}", err);
                    cmd_result = CommandResult::from_status(gid, err as i32);
                    break;
                }
        
                let pid = ws.get_pid();
                let is_a_fg_child = pids.contains(&pid);
                if is_a_fg_child && !ws.is_continued() {
                    count_waited += 1;
                }
        
                if ws.is_exited() {
                    if is_a_fg_child {
                        mark_job_as_done(sh, gid, pid, "Done");
                    } else {
                        let status = ws.get_status();
                        signals::insert_reap_map(pid, status);
                    }
                } else if ws.is_stopped() {
                    if is_a_fg_child {
                        // for stop signal of fg job (current job)
                        // i.e. Ctrl-Z is pressed on the fg job
                        mark_job_member_stopped(sh, pid, gid, true);
                    } else {
                        // for stop signal of bg jobs
                        signals::insert_stopped_map(pid);
                        mark_job_member_stopped(sh, pid, 0, false);
                    }
                } else if ws.is_continued() {
                    if !is_a_fg_child {
                        signals::insert_cont_map(pid);
                    }
                    continue;
                } else if ws.is_signaled() {
                    if is_a_fg_child {
                        mark_job_as_done(sh, gid, pid, "Killed");
                    } else {
                        signals::killed_map_insert(pid, ws.get_signal());
                    }
                }
        
                if is_a_fg_child && pid == *pid_last {
                    let status = ws.get_status();
                    cmd_result.status = status;
                }
        
                if count_waited >= count_child {
                    break;
                }
            }
            cmd_result
        }
        
        pub fn try_wait_bg_jobs(sh: &mut shell::Shell, report: bool, sig_handler_enabled: bool) {
            if sh.jobs.is_empty() {
                return;
            }
        
            if !sig_handler_enabled {
                // we need to wait pids in case CICADA_ENABLE_SIG_HANDLER=0
                signals::handle_sigchld(Signal::SIGCHLD as i32);
            }
        
            let jobs = sh.jobs.clone();
            for (_i, job) in jobs.iter() {
                for pid in job.pids.iter() {
                    if let Some(_status) = signals::pop_reap_map(*pid) {
                        mark_job_as_done(sh, job.gid, *pid, "Done");
                        continue;
                    }
        
                    if let Some(sig) = signals::killed_map_pop(*pid) {
                        let reason = if sig == Signal::SIGQUIT as i32 {
                            format!("Quit: {}", sig)
                        } else if sig == Signal::SIGINT as i32 {
                            format!("Interrupt: {}", sig)
                        } else if sig == Signal::SIGKILL as i32 {
                            format!("Killed: {}", sig)
                        } else if sig == Signal::SIGTERM as i32 {
                            format!("Terminated: {}", sig)
                        } else {
                            format!("Killed: {}", sig)
                        };
                        mark_job_as_done(sh, job.gid, *pid, &reason);
                        continue;
                    }
        
                    if signals::pop_stopped_map(*pid) {
                        mark_job_member_stopped(sh, *pid, job.gid, report);
                    } else if signals::pop_cont_map(*pid) {
                        mark_job_member_continued(sh, *pid, job.gid);
                    }
                }
            }
        }
    }
    // pub mod ctime
    pub mod time
    {
        use ::
        {
            *,
        };
        /*
        use std::fmt;
        use time::OffsetDateTime;
        */
        #[derive(Debug, PartialEq, Eq)]
        pub struct DateTime {
            odt: OffsetDateTime,
        }
        
        impl DateTime {
            pub fn now() -> Self {
                let odt: OffsetDateTime = match OffsetDateTime::now_local() {
                    Ok(dt) => dt,
                    Err(_) => OffsetDateTime::now_utc(),
                };
                DateTime { odt }
            }
        
            pub fn from_timestamp(ts: f64) -> Self {
                let dummy_now = Self::now();
                let offset_seconds = dummy_now.odt.offset().whole_minutes() * 60;
                let ts_nano = (ts + offset_seconds as f64) * 1000000000.0;
                let odt: OffsetDateTime = match OffsetDateTime::from_unix_timestamp_nanos(ts_nano as i128) {
                    Ok(x) => x,
                    Err(_) => OffsetDateTime::now_utc(),
                };
                DateTime { odt }
            }
        
            pub fn unix_timestamp(&self) -> f64 {
                self.odt.unix_timestamp_nanos() as f64 / 1000000000.0
            }
        }
        
        impl fmt::Display for DateTime {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

pub mod lineread
{
    use ::
    {
        *,
    };
} pub use lineread::{Command, Interface, ReadResult};

//linked-hash-map = ">=0.0.9, <0.6"
pub mod linked_hash_map
{
    //! A `HashMap` wrapper that holds key-value pairs in insertion order.
    use ::
    {
        *,
    };
    /*
    use std::borrow::Borrow;
    use std::cmp::Ordering;
    use std::collections::hash_map::{self, HashMap};
    use std::fmt;
    use std::hash::{BuildHasher, Hash, Hasher};
    use std::iter;
    use std::marker;
    use std::mem;
    use std::ops::{Index, IndexMut};
    use std::ptr::{self, addr_of_mut};
    */
    struct KeyRef<K> {
        k: *const K,
    }

    struct Node<K, V> {
        next: *mut Node<K, V>,
        prev: *mut Node<K, V>,
        key: K,
        value: V,
    }

    /// A linked hash map.
    pub struct LinkedHashMap<K, V, S = hash_map::RandomState> {
        map: HashMap<KeyRef<K>, *mut Node<K, V>, S>,
        head: *mut Node<K, V>,
        free: *mut Node<K, V>,
    }

    impl<K: Hash> Hash for KeyRef<K> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            unsafe { (*self.k).hash(state) }
        }
    }

    impl<K: PartialEq> PartialEq for KeyRef<K> {
        fn eq(&self, other: &Self) -> bool {
            unsafe { (*self.k).eq(&*other.k) }
        }
    }

    impl<K: Eq> Eq for KeyRef<K> {}
    
    #[derive(Hash, PartialEq, Eq)]
    #[repr(transparent)]
    struct Qey<Q: ?Sized>(Q);

    impl<Q: ?Sized> Qey<Q> {
        fn from_ref(q: &Q) -> &Self {
            unsafe { mem::transmute(q) }
        }
    }

    impl<K, Q: ?Sized> Borrow<Qey<Q>> for KeyRef<K>
    where
        K: Borrow<Q>,
    {
        fn borrow(&self) -> &Qey<Q> {
            Qey::from_ref(unsafe { (*self.k).borrow() })
        }
    }

    impl<K, V> Node<K, V> {
        fn new(k: K, v: V) -> Self {
            Node {
                key: k,
                value: v,
                next: ptr::null_mut(),
                prev: ptr::null_mut(),
            }
        }
    }

    // drop empty node without dropping its key and value
    unsafe fn drop_empty_node<K, V>(the_box: *mut Node<K, V>) {
        let layout = std::alloc::Layout::new::<Node<K, V>>();
        std::alloc::dealloc(the_box as *mut u8, layout);
    }

    impl<K: Hash + Eq, V> LinkedHashMap<K, V> {
        /// Creates a linked hash map.
        pub fn new() -> Self {
            Self::with_map(HashMap::new())
        }

        /// Creates an empty linked hash map with the given initial capacity.
        pub fn with_capacity(capacity: usize) -> Self {
            Self::with_map(HashMap::with_capacity(capacity))
        }
    }

    impl<K, V, S> LinkedHashMap<K, V, S> {
        #[inline]
        fn detach(&mut self, node: *mut Node<K, V>) {
            unsafe {
                (*(*node).prev).next = (*node).next;
                (*(*node).next).prev = (*node).prev;
            }
        }

        #[inline]
        fn attach(&mut self, node: *mut Node<K, V>) {
            unsafe {
                (*node).next = (*self.head).next;
                (*node).prev = self.head;
                (*self.head).next = node;
                (*(*node).next).prev = node;
            }
        }
        
        unsafe fn drop_entries(&mut self) {
            let mut cur = (*self.head).next;
            while cur != self.head {
                let next = (*cur).next;
                Box::from_raw(cur);
                cur = next;
            }
        }

        fn clear_free_list(&mut self) {
            unsafe {
                let mut free = self.free;
                while !free.is_null() {
                    let next_free = (*free).next;
                    drop_empty_node(free);
                    free = next_free;
                }
                self.free = ptr::null_mut();
            }
        }

        fn ensure_guard_node(&mut self) {
            if self.head.is_null() {
                // allocate the guard node if not present
                unsafe {
                    let node_layout = std::alloc::Layout::new::<Node<K, V>>();
                    self.head = std::alloc::alloc(node_layout) as *mut Node<K, V>;
                    (*self.head).next = self.head;
                    (*self.head).prev = self.head;
                }
            }
        }
    }

    impl<K: Hash + Eq, V, S: BuildHasher> LinkedHashMap<K, V, S> {
        fn with_map(map: HashMap<KeyRef<K>, *mut Node<K, V>, S>) -> Self {
            LinkedHashMap {
                map,
                head: ptr::null_mut(),
                free: ptr::null_mut(),
            }
        }

        /// Creates an empty linked hash map with the given initial hash builder.
        pub fn with_hasher(hash_builder: S) -> Self {
            Self::with_map(HashMap::with_hasher(hash_builder))
        }

        /// Creates an empty linked hash map with the given initial capacity and hash builder.
        pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Self {
            Self::with_map(HashMap::with_capacity_and_hasher(capacity, hash_builder))
        }

        /// Reserves capacity for at least `additional` more elements to be inserted into the map.
        pub fn reserve(&mut self, additional: usize) {
            self.map.reserve(additional);
        }

        /// Shrinks the capacity of the map as much as possible. 
        pub fn shrink_to_fit(&mut self) {
            self.map.shrink_to_fit();
            self.clear_free_list();
        }

        /// Gets the given key's corresponding entry in the map for in-place manipulation.
        pub fn entry(&mut self, k: K) -> Entry<K, V, S> {
            let self_ptr: *mut Self = self;

            if let Some(entry) = self.map.get_mut(&KeyRef { k: &k }) {
                return Entry::Occupied(OccupiedEntry {
                    entry: *entry,
                    map: self_ptr,
                    marker: marker::PhantomData,
                });
            }

            Entry::Vacant(VacantEntry { key: k, map: self })
        }

        /// Returns an iterator visiting all entries in insertion order.
        pub fn entries(&mut self) -> Entries<K, V, S> {
            let head = if !self.head.is_null() {
                unsafe { (*self.head).prev }
            } else {
                ptr::null_mut()
            };
            Entries {
                map: self,
                head,
                remaining: self.len(),
                marker: marker::PhantomData,
            }
        }

        /// Inserts a key-value pair into the map.
        pub fn insert(&mut self, k: K, v: V) -> Option<V> {
            self.ensure_guard_node();

            let (node, old_val) = match self.map.get(&KeyRef { k: &k }) {
                Some(node) => {
                    let old_val = unsafe { ptr::replace(&mut (**node).value, v) };
                    (*node, Some(old_val))
                }
                None => {
                    let node = if self.free.is_null() {
                        Box::into_raw(Box::new(Node::new(k, v)))
                    } else {
                        // use a recycled box
                        unsafe {
                            let free = self.free;
                            self.free = (*free).next;
                            ptr::write(free, Node::new(k, v));
                            free
                        }
                    };
                    (node, None)
                }
            };
            match old_val {
                Some(_) => {
                    self.detach(node);
                    self.attach(node);
                }
                None => {
                    let keyref = unsafe { &(*node).key };
                    self.map.insert(KeyRef { k: keyref }, node);
                    self.attach(node);
                }
            }
            old_val
        }

        /// Checks if the map contains the given key.
        pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
        {
            self.map.contains_key(Qey::from_ref(k))
        }

        /// Returns the value corresponding to the key in the map.
        pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
        {
            self.map
                .get(Qey::from_ref(k))
                .map(|e| unsafe { &(**e).value })
        }

        /// Returns the mutable reference corresponding to the key in the map.
        pub fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
        {
            self.map
                .get(Qey::from_ref(k))
                .map(|e| unsafe { &mut (**e).value })
        }

        /// Returns the value corresponding to the key in the map.
        pub fn get_refresh<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
        {
            let (value, node_ptr_opt) = match self.map.get(Qey::from_ref(k)) {
                None => (None, None),
                Some(node) => (Some(unsafe { &mut (**node).value }), Some(*node)),
            };
            if let Some(node_ptr) = node_ptr_opt {
                self.detach(node_ptr);
                self.attach(node_ptr);
            }
            value
        }

        /// Removes and returns the value corresponding to the key from the map.
        pub fn remove<Q: ?Sized>(&mut self, k: &Q) -> Option<V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
        {
            let removed = self.map.remove(Qey::from_ref(k));
            removed.map(|node| {
                self.detach(node);
                unsafe {
                    // add to free list
                    (*node).next = self.free;
                    self.free = node;
                    // drop the key and return the value
                    drop(ptr::read(&(*node).key));
                    ptr::read(&(*node).value)
                }
            })
        }

        /// Returns the maximum number of key-value pairs the map can hold without reallocating.
        pub fn capacity(&self) -> usize {
            self.map.capacity()
        }

        /// Removes the first entry.
        #[inline]
        pub fn pop_front(&mut self) -> Option<(K, V)> {
            if self.is_empty() {
                return None;
            }
            let lru = unsafe { (*self.head).prev };
            self.detach(lru);
            self.map
                .remove(&KeyRef {
                    k: unsafe { &(*lru).key },
                })
                .map(|e| {
                    let e = *unsafe { Box::from_raw(e) };
                    (e.key, e.value)
                })
        }

        /// Gets the first entry.
        #[inline]
        pub fn front(&self) -> Option<(&K, &V)> {
            if self.is_empty() {
                return None;
            }
            let lru = unsafe { (*self.head).prev };
            self.map
                .get(&KeyRef {
                    k: unsafe { &(*lru).key },
                })
                .map(|e| unsafe { (&(**e).key, &(**e).value) })
        }

        /// Removes the last entry.
        #[inline]
        pub fn pop_back(&mut self) -> Option<(K, V)> {
            if self.is_empty() {
                return None;
            }
            let mru = unsafe { (*self.head).next };
            self.detach(mru);
            self.map
                .remove(&KeyRef {
                    k: unsafe { &(*mru).key },
                })
                .map(|e| {
                    let e = *unsafe { Box::from_raw(e) };
                    (e.key, e.value)
                })
        }

        /// Gets the last entry.
        #[inline]
        pub fn back(&self) -> Option<(&K, &V)> {
            if self.is_empty() {
                return None;
            }
            let mru = unsafe { (*self.head).next };
            self.map
                .get(&KeyRef {
                    k: unsafe { &(*mru).key },
                })
                .map(|e| unsafe { (&(**e).key, &(**e).value) })
        }

        /// Returns the number of key-value pairs in the map.
        pub fn len(&self) -> usize {
            self.map.len()
        }

        /// Returns whether the map is currently empty.
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Returns a reference to the map's hasher.
        pub fn hasher(&self) -> &S {
            self.map.hasher()
        }

        /// Clears the map of all key-value pairs.
        pub fn clear(&mut self) {
            self.map.clear();
            // update the guard node if present
            if !self.head.is_null() {
                unsafe {
                    self.drop_entries();
                    (*self.head).prev = self.head;
                    (*self.head).next = self.head;
                }
            }
        }

        /// Returns a double-ended iterator visiting all key-value pairs in order of insertion.
        pub fn iter(&self) -> Iter<K, V> {
            let head = if self.head.is_null() {
                ptr::null_mut()
            } else {
                unsafe { (*self.head).prev }
            };
            Iter {
                head,
                tail: self.head,
                remaining: self.len(),
                marker: marker::PhantomData,
            }
        }

        /// Returns a double-ended iterator visiting all key-value pairs in order of insertion.
        pub fn iter_mut(&mut self) -> IterMut<K, V> {
            let head = if self.head.is_null() {
                ptr::null_mut()
            } else {
                unsafe { (*self.head).prev }
            };
            IterMut {
                head,
                tail: self.head,
                remaining: self.len(),
                marker: marker::PhantomData,
            }
        }

        /// Clears the map, returning all key-value pairs as an iterator.
        pub fn drain(&mut self) -> Drain<K, V> {
            let len = self.len();
            self.map.clear();
            let (head, tail) = if len != 0 {
                unsafe {
                    debug_assert!(!self.head.is_null());
                    debug_assert!(!(*self.head).prev.is_null());
                    debug_assert!((*self.head).prev != self.head);
                    let head = (*self.head).prev;
                    let tail = (*self.head).next;
                    (*self.head).prev = self.head;
                    (*self.head).next = self.head;
                    (*head).next = self.free;
                    (*tail).prev = ptr::null_mut();
                    self.free = tail;
                    (head, tail)
                }
            } else {
                (ptr::null_mut(), ptr::null_mut())
            };

            Drain {
                head,
                tail,
                remaining: len,
                marker: marker::PhantomData,
            }
        }

        /// Returns a double-ended iterator visiting all key in order of insertion.
        pub fn keys(&self) -> Keys<K, V> {
            Keys { inner: self.iter() }
        }

        /// Returns a double-ended iterator visiting all values in order of insertion.
        pub fn values(&self) -> Values<K, V> {
            Values { inner: self.iter() }
        }
    }

    impl<'a, K, V, S, Q: ?Sized> Index<&'a Q> for LinkedHashMap<K, V, S>
    where
        K: Hash + Eq + Borrow<Q>,
        S: BuildHasher,
        Q: Eq + Hash,
    {
        type Output = V;

        fn index(&self, index: &'a Q) -> &V {
            self.get(index).expect("no entry found for key")
        }
    }

    impl<'a, K, V, S, Q: ?Sized> IndexMut<&'a Q> for LinkedHashMap<K, V, S>
    where
        K: Hash + Eq + Borrow<Q>,
        S: BuildHasher,
        Q: Eq + Hash,
    {
        fn index_mut(&mut self, index: &'a Q) -> &mut V {
            self.get_mut(index).expect("no entry found for key")
        }
    }

    impl<K: Hash + Eq + Clone, V: Clone, S: BuildHasher + Clone> Clone for LinkedHashMap<K, V, S> {
        fn clone(&self) -> Self {
            let mut map = Self::with_hasher(self.map.hasher().clone());
            map.extend(self.iter().map(|(k, v)| (k.clone(), v.clone())));
            map
        }
    }

    impl<K: Hash + Eq, V, S: BuildHasher + Default> Default for LinkedHashMap<K, V, S> {
        fn default() -> Self {
            Self::with_hasher(S::default())
        }
    }

    impl<K: Hash + Eq, V, S: BuildHasher> Extend<(K, V)> for LinkedHashMap<K, V, S> {
        fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
            for (k, v) in iter {
                self.insert(k, v);
            }
        }
    }

    impl<'a, K, V, S> Extend<(&'a K, &'a V)> for LinkedHashMap<K, V, S>
    where
        K: 'a + Hash + Eq + Copy,
        V: 'a + Copy,
        S: BuildHasher,
    {
        fn extend<I: IntoIterator<Item = (&'a K, &'a V)>>(&mut self, iter: I) {
            for (&k, &v) in iter {
                self.insert(k, v);
            }
        }
    }

    impl<K: Hash + Eq, V, S: BuildHasher + Default> iter::FromIterator<(K, V)>
        for LinkedHashMap<K, V, S>
    {
        fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
            let iter = iter.into_iter();
            let mut map = Self::with_capacity_and_hasher(iter.size_hint().0, S::default());
            map.extend(iter);
            map
        }
    }

    impl<A: fmt::Debug + Hash + Eq, B: fmt::Debug, S: BuildHasher> fmt::Debug
        for LinkedHashMap<A, B, S>
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.debug_map().entries(self).finish()
        }
    }

    impl<K: Hash + Eq, V: PartialEq, S: BuildHasher> PartialEq for LinkedHashMap<K, V, S> {
        fn eq(&self, other: &Self) -> bool {
            self.len() == other.len() && self.iter().eq(other)
        }
    }

    impl<K: Hash + Eq, V: Eq, S: BuildHasher> Eq for LinkedHashMap<K, V, S> {}

    impl<K: Hash + Eq + PartialOrd, V: PartialOrd, S: BuildHasher> PartialOrd
        for LinkedHashMap<K, V, S>
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.iter().partial_cmp(other)
        }

        fn lt(&self, other: &Self) -> bool {
            self.iter().lt(other)
        }

        fn le(&self, other: &Self) -> bool {
            self.iter().le(other)
        }

        fn ge(&self, other: &Self) -> bool {
            self.iter().ge(other)
        }

        fn gt(&self, other: &Self) -> bool {
            self.iter().gt(other)
        }
    }

    impl<K: Hash + Eq + Ord, V: Ord, S: BuildHasher> Ord for LinkedHashMap<K, V, S> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.iter().cmp(other)
        }
    }

    impl<K: Hash + Eq, V: Hash, S: BuildHasher> Hash for LinkedHashMap<K, V, S> {
        fn hash<H: Hasher>(&self, h: &mut H) {
            for e in self.iter() {
                e.hash(h);
            }
        }
    }

    unsafe impl<K: Send, V: Send, S: Send> Send for LinkedHashMap<K, V, S> {}

    unsafe impl<K: Sync, V: Sync, S: Sync> Sync for LinkedHashMap<K, V, S> {}

    impl<K, V, S> Drop for LinkedHashMap<K, V, S> {
        fn drop(&mut self) {
            if !self.head.is_null() {
                unsafe {
                    self.drop_entries();
                    drop_empty_node(self.head);
                }
            }
            self.clear_free_list();
        }
    }

    /// An insertion-order iterator over a `LinkedHashMap`'s entries, with immutable references to the
    /// values.
    pub struct Iter<'a, K: 'a, V: 'a> {
        head: *const Node<K, V>,
        tail: *const Node<K, V>,
        remaining: usize,
        marker: marker::PhantomData<(&'a K, &'a V)>,
    }

    /// An insertion-order iterator over a `LinkedHashMap`'s entries, with mutable references to the
    /// values.
    pub struct IterMut<'a, K: 'a, V: 'a> {
        head: *mut Node<K, V>,
        tail: *mut Node<K, V>,
        remaining: usize,
        marker: marker::PhantomData<(&'a K, &'a mut V)>,
    }

    /// A consuming insertion-order iterator over a `LinkedHashMap`'s entries.
    pub struct IntoIter<K, V> {
        head: *mut Node<K, V>,
        tail: *mut Node<K, V>,
        remaining: usize,
        marker: marker::PhantomData<(K, V)>,
    }

    /// A draining insertion-order iterator over a `LinkedHashMap`'s entries.
    pub struct Drain<'a, K, V> {
        head: *mut Node<K, V>,
        tail: *mut Node<K, V>,
        remaining: usize,
        marker: marker::PhantomData<&'a mut (K, V)>,
    }

    /// An insertion-order iterator over a `LinkedHashMap`'s entries represented as
    /// an `OccupiedEntry`.
    pub struct Entries<'a, K: 'a, V: 'a, S: 'a = hash_map::RandomState> {
        map: *mut LinkedHashMap<K, V, S>,
        head: *mut Node<K, V>,
        remaining: usize,
        marker: marker::PhantomData<(&'a K, &'a mut V, &'a S)>,
    }

    unsafe impl<'a, K, V> Send for Iter<'a, K, V>
    where
        K: Send,
        V: Send,
    {
    }

    unsafe impl<'a, K, V> Send for IterMut<'a, K, V>
    where
        K: Send,
        V: Send,
    {
    }

    unsafe impl<'a, K, V> Send for Drain<'a, K, V>
    where
        K: Send,
        V: Send,
    {
    }

    unsafe impl<K, V> Send for IntoIter<K, V>
    where
        K: Send,
        V: Send,
    {
    }

    unsafe impl<'a, K, V, S> Send for Entries<'a, K, V, S>
    where
        K: Send,
        V: Send,
        S: Send,
    {
    }

    unsafe impl<'a, K, V> Sync for Iter<'a, K, V>
    where
        K: Sync,
        V: Sync,
    {
    }

    unsafe impl<'a, K, V> Sync for IterMut<'a, K, V>
    where
        K: Sync,
        V: Sync,
    {
    }

    unsafe impl<'a, K, V> Sync for Drain<'a, K, V>
    where
        K: Sync,
        V: Sync,
    {
    }
    unsafe impl<K, V> Sync for IntoIter<K, V>
    where
        K: Sync,
        V: Sync,
    {
    }

    unsafe impl<'a, K, V, S> Sync for Entries<'a, K, V, S>
    where
        K: Sync,
        V: Sync,
        S: Sync,
    {
    }

    impl<'a, K, V> Clone for Iter<'a, K, V> {
        fn clone(&self) -> Self {
            Iter { ..*self }
        }
    }

    impl<K, V> Clone for IntoIter<K, V>
    where
        K: Clone,
        V: Clone,
    {
        fn clone(&self) -> Self {
            if self.remaining == 0 {
                return IntoIter { ..*self };
            }

            fn clone_node<K, V>(e: *mut Node<K, V>) -> *mut Node<K, V>
            where
                K: Clone,
                V: Clone,
            {
                Box::into_raw(Box::new(Node::new(unsafe { (*e).key.clone() }, unsafe {
                    (*e).value.clone()
                })))
            }

            let mut cur = self.head;
            let head = clone_node(cur);
            let mut tail = head;
            for _ in 1..self.remaining {
                unsafe {
                    (*tail).prev = clone_node((*cur).prev);
                    (*(*tail).prev).next = tail;
                    tail = (*tail).prev;
                    cur = (*cur).prev;
                }
            }

            IntoIter {
                head,
                tail,
                remaining: self.remaining,
                marker: marker::PhantomData,
            }
        }
    }

    impl<'a, K, V> Iterator for Iter<'a, K, V> {
        type Item = (&'a K, &'a V);

        fn next(&mut self) -> Option<(&'a K, &'a V)> {
            if self.head == self.tail {
                None
            } else {
                self.remaining -= 1;
                unsafe {
                    let r = Some((&(*self.head).key, &(*self.head).value));
                    self.head = (*self.head).prev;
                    r
                }
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.remaining, Some(self.remaining))
        }
    }

    impl<'a, K, V> Iterator for IterMut<'a, K, V> {
        type Item = (&'a K, &'a mut V);

        fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
            if self.head == self.tail {
                None
            } else {
                self.remaining -= 1;
                unsafe {
                    let r = Some((&(*self.head).key, &mut (*self.head).value));
                    self.head = (*self.head).prev;
                    r
                }
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.remaining, Some(self.remaining))
        }
    }

    impl<K, V> Iterator for IntoIter<K, V> {
        type Item = (K, V);

        fn next(&mut self) -> Option<(K, V)> {
            if self.remaining == 0 {
                return None;
            }
            self.remaining -= 1;
            unsafe {
                let prev = (*self.head).prev;
                let e = *Box::from_raw(self.head);
                self.head = prev;
                Some((e.key, e.value))
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.remaining, Some(self.remaining))
        }
    }

    impl<'a, K, V> Iterator for Drain<'a, K, V> {
        type Item = (K, V);

        fn next(&mut self) -> Option<(K, V)> {
            if self.remaining == 0 {
                return None;
            }
            self.remaining -= 1;
            unsafe {
                let prev = (*self.head).prev;
                let k = addr_of_mut!((*self.head).key).read();
                let v = addr_of_mut!((*self.head).value).read();
                self.head = prev;
                Some((k, v))
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.remaining, Some(self.remaining))
        }
    }

    impl<'a, K, V> DoubleEndedIterator for Drain<'a, K, V> {
        fn next_back(&mut self) -> Option<(K, V)> {
            if self.remaining == 0 {
                return None;
            }
            self.remaining -= 1;
            unsafe {
                let next = (*self.tail).next;
                let k = addr_of_mut!((*self.tail).key).read();
                let v = addr_of_mut!((*self.tail).value).read();
                self.tail = next;
                Some((k, v))
            }
        }
    }

    impl<'a, K, V> ExactSizeIterator for Drain<'a, K, V> {
        fn len(&self) -> usize {
            self.remaining
        }
    }

    impl<'a, K, V, S: BuildHasher> Iterator for Entries<'a, K, V, S> {
        type Item = OccupiedEntry<'a, K, V, S>;

        fn next(&mut self) -> Option<OccupiedEntry<'a, K, V, S>> {
            if self.remaining == 0 {
                None
            } else {
                self.remaining -= 1;
                unsafe {
                    let r = Some(OccupiedEntry {
                        map: self.map,
                        entry: self.head,
                        marker: marker::PhantomData,
                    });

                    self.head = (*self.head).prev;
                    r
                }
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.remaining, Some(self.remaining))
        }
    }

    impl<'a, K, V> DoubleEndedIterator for Iter<'a, K, V> {
        fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
            if self.head == self.tail {
                None
            } else {
                self.remaining -= 1;
                unsafe {
                    self.tail = (*self.tail).next;
                    Some((&(*self.tail).key, &(*self.tail).value))
                }
            }
        }
    }

    impl<'a, K, V> DoubleEndedIterator for IterMut<'a, K, V> {
        fn next_back(&mut self) -> Option<(&'a K, &'a mut V)> {
            if self.head == self.tail {
                None
            } else {
                self.remaining -= 1;
                unsafe {
                    self.tail = (*self.tail).next;
                    Some((&(*self.tail).key, &mut (*self.tail).value))
                }
            }
        }
    }

    impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
        fn next_back(&mut self) -> Option<(K, V)> {
            if self.remaining == 0 {
                return None;
            }
            self.remaining -= 1;
            unsafe {
                let next = (*self.tail).next;
                let e = *Box::from_raw(self.tail);
                self.tail = next;
                Some((e.key, e.value))
            }
        }
    }

    impl<'a, K, V> ExactSizeIterator for Iter<'a, K, V> {
        fn len(&self) -> usize {
            self.remaining
        }
    }

    impl<'a, K, V> ExactSizeIterator for IterMut<'a, K, V> {
        fn len(&self) -> usize {
            self.remaining
        }
    }

    impl<K, V> ExactSizeIterator for IntoIter<K, V> {
        fn len(&self) -> usize {
            self.remaining
        }
    }

    impl<K, V> Drop for IntoIter<K, V> {
        fn drop(&mut self) {
            for _ in 0..self.remaining {
                unsafe {
                    let next = (*self.tail).next;
                    Box::from_raw(self.tail);
                    self.tail = next;
                }
            }
        }
    }

    impl<'a, K, V> Drop for Drain<'a, K, V> {
        fn drop(&mut self) {
            for _ in self {}
        }
    }

    /// An insertion-order iterator over a `LinkedHashMap`'s keys.
    pub struct Keys<'a, K: 'a, V: 'a> {
        inner: Iter<'a, K, V>,
    }

    impl<'a, K, V> Clone for Keys<'a, K, V> {
        fn clone(&self) -> Self {
            Keys {
                inner: self.inner.clone(),
            }
        }
    }

    impl<'a, K, V> Iterator for Keys<'a, K, V> {
        type Item = &'a K;

        #[inline]
        fn next(&mut self) -> Option<&'a K> {
            self.inner.next().map(|e| e.0)
        }
        #[inline]
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<'a, K, V> DoubleEndedIterator for Keys<'a, K, V> {
        #[inline]
        fn next_back(&mut self) -> Option<&'a K> {
            self.inner.next_back().map(|e| e.0)
        }
    }

    impl<'a, K, V> ExactSizeIterator for Keys<'a, K, V> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    /// An insertion-order iterator over a `LinkedHashMap`'s values.
    pub struct Values<'a, K: 'a, V: 'a> {
        inner: Iter<'a, K, V>,
    }

    impl<'a, K, V> Clone for Values<'a, K, V> {
        fn clone(&self) -> Self {
            Values {
                inner: self.inner.clone(),
            }
        }
    }

    impl<'a, K, V> Iterator for Values<'a, K, V> {
        type Item = &'a V;

        #[inline]
        fn next(&mut self) -> Option<&'a V> {
            self.inner.next().map(|e| e.1)
        }
        #[inline]
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<'a, K, V> DoubleEndedIterator for Values<'a, K, V> {
        #[inline]
        fn next_back(&mut self) -> Option<&'a V> {
            self.inner.next_back().map(|e| e.1)
        }
    }

    impl<'a, K, V> ExactSizeIterator for Values<'a, K, V> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    impl<'a, K: Hash + Eq, V, S: BuildHasher> IntoIterator for &'a LinkedHashMap<K, V, S> {
        type Item = (&'a K, &'a V);
        type IntoIter = Iter<'a, K, V>;
        fn into_iter(self) -> Iter<'a, K, V> {
            self.iter()
        }
    }

    impl<'a, K: Hash + Eq, V, S: BuildHasher> IntoIterator for &'a mut LinkedHashMap<K, V, S> {
        type Item = (&'a K, &'a mut V);
        type IntoIter = IterMut<'a, K, V>;
        fn into_iter(self) -> IterMut<'a, K, V> {
            self.iter_mut()
        }
    }

    impl<K: Hash + Eq, V, S: BuildHasher> IntoIterator for LinkedHashMap<K, V, S> {
        type Item = (K, V);
        type IntoIter = IntoIter<K, V>;
        fn into_iter(mut self) -> IntoIter<K, V> {
            let (head, tail) = if !self.head.is_null() {
                unsafe { ((*self.head).prev, (*self.head).next) }
            } else {
                (ptr::null_mut(), ptr::null_mut())
            };
            let len = self.len();

            if !self.head.is_null() {
                unsafe { drop_empty_node(self.head) }
            }
            self.clear_free_list();
            // drop the HashMap but not the LinkedHashMap
            unsafe {
                ptr::drop_in_place(&mut self.map);
            }
            mem::forget(self);

            IntoIter {
                head,
                tail,
                remaining: len,
                marker: marker::PhantomData,
            }
        }
    }

    /// A view into a single location in a map, which may be vacant or occupied.
    pub enum Entry<'a, K: 'a, V: 'a, S: 'a = hash_map::RandomState> {
        /// An occupied Entry.
        Occupied(OccupiedEntry<'a, K, V, S>),
        /// A vacant Entry.
        Vacant(VacantEntry<'a, K, V, S>),
    }

    /// A view into a single occupied location in a `LinkedHashMap`.
    pub struct OccupiedEntry<'a, K: 'a, V: 'a, S: 'a = hash_map::RandomState> {
        entry: *mut Node<K, V>,
        map: *mut LinkedHashMap<K, V, S>,
        marker: marker::PhantomData<&'a K>,
    }

    /// A view into a single empty location in a `LinkedHashMap`.
    pub struct VacantEntry<'a, K: 'a, V: 'a, S: 'a = hash_map::RandomState> {
        key: K,
        map: &'a mut LinkedHashMap<K, V, S>,
    }

    impl<'a, K: Hash + Eq, V, S: BuildHasher> Entry<'a, K, V, S> {
        /// Returns the entry key
        pub fn key(&self) -> &K {
            match *self {
                Entry::Occupied(ref e) => e.key(),
                Entry::Vacant(ref e) => e.key(),
            }
        }

        /// Ensures a value is in the entry by inserting the default if empty, and returns
        /// a mutable reference to the value in the entry.
        pub fn or_insert(self, default: V) -> &'a mut V {
            match self {
                Entry::Occupied(entry) => entry.into_mut(),
                Entry::Vacant(entry) => entry.insert(default),
            }
        }

        /// Ensures a value is in the entry by inserting the result of the default function if empty,
        /// and returns a mutable reference to the value in the entry.
        pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
            match self {
                Entry::Occupied(entry) => entry.into_mut(),
                Entry::Vacant(entry) => entry.insert(default()),
            }
        }

        /// Provides in-place mutable access to an occupied entry before any
        /// potential inserts into the map.
        pub fn and_modify<F>(self, f: F) -> Self
        where
            F: FnOnce(&mut V),
        {
            match self {
                Entry::Occupied(mut entry) => {
                    f(entry.get_mut());
                    Entry::Occupied(entry)
                }
                Entry::Vacant(entry) => Entry::Vacant(entry),
            }
        }

        /// Ensures a value is in the entry by inserting the default value if empty,
        /// and returns a mutable reference to the value in the entry.
        pub fn or_default(self) -> &'a mut V
        where
            V: Default,
        {
            match self {
                Entry::Occupied(entry) => entry.into_mut(),
                Entry::Vacant(entry) => entry.insert(V::default()),
            }
        }
    }

    impl<'a, K: Hash + Eq, V, S: BuildHasher> OccupiedEntry<'a, K, V, S> {
        /// Gets a reference to the entry key
        pub fn key(&self) -> &K {
            unsafe { &(*self.entry).key }
        }

        /// Gets a reference to the value in the entry.
        pub fn get(&self) -> &V {
            unsafe { &(*self.entry).value }
        }

        /// Gets a mutable reference to the value in the entry.
        pub fn get_mut(&mut self) -> &mut V {
            unsafe { &mut (*self.entry).value }
        }

        /// Converts the OccupiedEntry into a mutable reference to the value in the entry
        /// with a lifetime bound to the map itself
        pub fn into_mut(self) -> &'a mut V {
            unsafe { &mut (*self.entry).value }
        }

        /// Sets the value of the entry, and returns the entry's old value
        pub fn insert(&mut self, value: V) -> V {
            unsafe {
                (*self.map).ensure_guard_node();

                let old_val = mem::replace(&mut (*self.entry).value, value);
                let node_ptr: *mut Node<K, V> = self.entry;

                // Existing node, just update LRU position
                (*self.map).detach(node_ptr);
                (*self.map).attach(node_ptr);

                old_val
            }
        }

        /// Takes the value out of the entry, and returns it
        pub fn remove(self) -> V {
            unsafe { (*self.map).remove(&(*self.entry).key) }.unwrap()
        }
    }

    impl<'a, K: 'a + Hash + Eq, V: 'a, S: BuildHasher> VacantEntry<'a, K, V, S> {
        /// Gets a reference to the entry key
        pub fn key(&self) -> &K {
            &self.key
        }

        /// Sets the value of the entry with the VacantEntry's key,
        /// and returns a mutable reference to it
        pub fn insert(self, value: V) -> &'a mut V {
            self.map.ensure_guard_node();

            let node = if self.map.free.is_null() {
                Box::into_raw(Box::new(Node::new(self.key, value)))
            } else {
                // use a recycled box
                unsafe {
                    let free = self.map.free;
                    self.map.free = (*free).next;
                    ptr::write(free, Node::new(self.key, value));
                    free
                }
            };

            let keyref = unsafe { &(*node).key };

            self.map.attach(node);

            let ret = self.map.map.entry(KeyRef { k: keyref }).or_insert(node);
            unsafe { &mut (**ret).value }
        }
    }
}

pub mod nix
{
    use ::
    {
        *,
    };
}

pub mod pest
{
    use ::
    {
        *,
    };
}
// random v0.8
pub mod random
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
}

pub mod rusqlite
{
    //! Rusqlite is an ergonomic wrapper for using SQLite from Rust.
    use ::
    {
        *,
    };
    /*
    pub use libsqlite3_sys as ffi;
    
    use std::cell::RefCell;
    use std::default::Default;
    use std::ffi::{CStr, CString};
    use std::fmt;
    use std::os::raw::{c_char, c_int};
    
    use std::path::Path;
    use std::result;
    use std::str;
    use std::sync::{Arc, Mutex};
    
    use crate::cache::StatementCache;
    use crate::inner_connection::InnerConnection;
    use crate::raw_statement::RawStatement;
    use crate::types::ValueRef;
    
    pub use crate::cache::CachedStatement;
    #[cfg(feature = "column_decltype")]
    pub use crate::column::Column;
    pub use crate::error::{to_sqlite_error, Error};
    pub use crate::ffi::ErrorCode;
    #[cfg(feature = "load_extension")]
    pub use crate::load_extension_guard::LoadExtensionGuard;
    pub use crate::params::{params_from_iter, Params, ParamsFromIter};
    pub use crate::row::{AndThenRows, Map, MappedRows, Row, RowIndex, Rows};
    pub use crate::statement::{Statement, StatementStatus};
    #[cfg(feature = "modern_sqlite")]
    pub use crate::transaction::TransactionState;
    pub use crate::transaction::{DropBehavior, Savepoint, Transaction, TransactionBehavior};
    pub use crate::types::ToSql;
    pub use crate::version::*;
    #[cfg(feature = "rusqlite-macros")]
    #[doc(hidden)]
    pub use rusqlite_macros::__bind;
    */
    /*libsqlite3-sys 0.30.0*/
    pub mod system
    {
        use ::
        {
            *,
        };
        /*
        use std::mem;
        */
        mod error
        {
            use ::
            {
                *,
            };
            /*
            use std::error;
            use std::fmt;
            use std::os::raw::c_int;
            */
            /// Error Codes
            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            #[non_exhaustive]
            pub enum ErrorCode {
                /// Internal logic error in SQLite
                InternalMalfunction,
                /// Access permission denied
                PermissionDenied,
                /// Callback routine requested an abort
                OperationAborted,
                /// The database file is locked
                DatabaseBusy,
                /// A table in the database is locked
                DatabaseLocked,
                /// A malloc() failed
                OutOfMemory,
                /// Attempt to write a readonly database
                ReadOnly,
                /// Operation terminated by sqlite3_interrupt()
                OperationInterrupted,
                /// Some kind of disk I/O error occurred
                SystemIoFailure,
                /// The database disk image is malformed
                DatabaseCorrupt,
                /// Unknown opcode in sqlite3_file_control()
                NotFound,
                /// Insertion failed because database is full
                DiskFull,
                /// Unable to open the database file
                CannotOpen,
                /// Database lock protocol error
                FileLockingProtocolFailed,
                /// The database schema changed
                SchemaChanged,
                /// String or BLOB exceeds size limit
                TooBig,
                /// Abort due to constraint violation
                ConstraintViolation,
                /// Data type mismatch
                TypeMismatch,
                /// Library used incorrectly
                ApiMisuse,
                /// Uses OS features not supported on host
                NoLargeFileSupport,
                /// Authorization denied
                AuthorizationForStatementDenied,
                /// 2nd parameter to sqlite3_bind out of range
                ParameterOutOfRange,
                /// File opened that is not a database file
                NotADatabase,
                /// SQL error or missing database
                Unknown,
            }
            
            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            pub struct Error {
                pub code: ErrorCode,
                pub extended_code: c_int,
            }
            
            impl Error {
                #[must_use]
                pub fn new(result_code: c_int) -> Error {
                    let code = match result_code & 0xff {
                        super::SQLITE_INTERNAL => ErrorCode::InternalMalfunction,
                        super::SQLITE_PERM => ErrorCode::PermissionDenied,
                        super::SQLITE_ABORT => ErrorCode::OperationAborted,
                        super::SQLITE_BUSY => ErrorCode::DatabaseBusy,
                        super::SQLITE_LOCKED => ErrorCode::DatabaseLocked,
                        super::SQLITE_NOMEM => ErrorCode::OutOfMemory,
                        super::SQLITE_READONLY => ErrorCode::ReadOnly,
                        super::SQLITE_INTERRUPT => ErrorCode::OperationInterrupted,
                        super::SQLITE_IOERR => ErrorCode::SystemIoFailure,
                        super::SQLITE_CORRUPT => ErrorCode::DatabaseCorrupt,
                        super::SQLITE_NOTFOUND => ErrorCode::NotFound,
                        super::SQLITE_FULL => ErrorCode::DiskFull,
                        super::SQLITE_CANTOPEN => ErrorCode::CannotOpen,
                        super::SQLITE_PROTOCOL => ErrorCode::FileLockingProtocolFailed,
                        super::SQLITE_SCHEMA => ErrorCode::SchemaChanged,
                        super::SQLITE_TOOBIG => ErrorCode::TooBig,
                        super::SQLITE_CONSTRAINT => ErrorCode::ConstraintViolation,
                        super::SQLITE_MISMATCH => ErrorCode::TypeMismatch,
                        super::SQLITE_MISUSE => ErrorCode::ApiMisuse,
                        super::SQLITE_NOLFS => ErrorCode::NoLargeFileSupport,
                        super::SQLITE_AUTH => ErrorCode::AuthorizationForStatementDenied,
                        super::SQLITE_RANGE => ErrorCode::ParameterOutOfRange,
                        super::SQLITE_NOTADB => ErrorCode::NotADatabase,
                        _ => ErrorCode::Unknown,
                    };
            
                    Error {
                        code,
                        extended_code: result_code,
                    }
                }
            }
            
            impl fmt::Display for Error {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "Error code {}: {}",
                        self.extended_code,
                        code_to_str(self.extended_code)
                    )
                }
            }
            
            impl error::Error for Error {
                fn description(&self) -> &str {
                    code_to_str(self.extended_code)
                }
            }
            
            // Result codes.
            // Note: These are not public because our bindgen bindings export whichever
            // constants are present in the current version of SQLite. We repeat them here,
            // so we don't have to worry about which version of SQLite added which
            // constants, and we only use them to implement code_to_str below.
            
            // Extended result codes.
            
            const SQLITE_ERROR_MISSING_COLLSEQ: c_int = super::SQLITE_ERROR | (1 << 8);
            const SQLITE_ERROR_RETRY: c_int = super::SQLITE_ERROR | (2 << 8);
            const SQLITE_ERROR_SNAPSHOT: c_int = super::SQLITE_ERROR | (3 << 8);
            
            const SQLITE_IOERR_BEGIN_ATOMIC: c_int = super::SQLITE_IOERR | (29 << 8);
            const SQLITE_IOERR_COMMIT_ATOMIC: c_int = super::SQLITE_IOERR | (30 << 8);
            const SQLITE_IOERR_ROLLBACK_ATOMIC: c_int = super::SQLITE_IOERR | (31 << 8);
            const SQLITE_IOERR_DATA: c_int = super::SQLITE_IOERR | (32 << 8);
            const SQLITE_IOERR_CORRUPTFS: c_int = super::SQLITE_IOERR | (33 << 8);
            const SQLITE_IOERR_IN_PAGE: c_int = super::SQLITE_IOERR | (34 << 8);
            
            const SQLITE_LOCKED_VTAB: c_int = super::SQLITE_LOCKED | (2 << 8);
            
            const SQLITE_BUSY_TIMEOUT: c_int = super::SQLITE_BUSY | (3 << 8);
            
            const SQLITE_CANTOPEN_SYMLINK: c_int = super::SQLITE_CANTOPEN | (6 << 8);
            
            const SQLITE_CORRUPT_SEQUENCE: c_int = super::SQLITE_CORRUPT | (2 << 8);
            const SQLITE_CORRUPT_INDEX: c_int = super::SQLITE_CORRUPT | (3 << 8);
            
            const SQLITE_READONLY_CANTINIT: c_int = super::SQLITE_READONLY | (5 << 8);
            const SQLITE_READONLY_DIRECTORY: c_int = super::SQLITE_READONLY | (6 << 8);
            
            const SQLITE_CONSTRAINT_PINNED: c_int = super::SQLITE_CONSTRAINT | (11 << 8);
            const SQLITE_CONSTRAINT_DATATYPE: c_int = super::SQLITE_CONSTRAINT | (12 << 8);
            
            #[must_use]
            pub fn code_to_str(code: c_int) -> &'static str {
                match code {
                    super::SQLITE_OK        => "Successful result",
                    super::SQLITE_ERROR     => "SQL error or missing database",
                    super::SQLITE_INTERNAL  => "Internal logic error in SQLite",
                    super::SQLITE_PERM      => "Access permission denied",
                    super::SQLITE_ABORT     => "Callback routine requested an abort",
                    super::SQLITE_BUSY      => "The database file is locked",
                    super::SQLITE_LOCKED    => "A table in the database is locked",
                    super::SQLITE_NOMEM     => "A malloc() failed",
                    super::SQLITE_READONLY  => "Attempt to write a readonly database",
                    super::SQLITE_INTERRUPT => "Operation terminated by sqlite3_interrupt()",
                    super::SQLITE_IOERR     => "Some kind of disk I/O error occurred",
                    super::SQLITE_CORRUPT   => "The database disk image is malformed",
                    super::SQLITE_NOTFOUND  => "Unknown opcode in sqlite3_file_control()",
                    super::SQLITE_FULL      => "Insertion failed because database is full",
                    super::SQLITE_CANTOPEN  => "Unable to open the database file",
                    super::SQLITE_PROTOCOL  => "Database lock protocol error",
                    super::SQLITE_EMPTY     => "Database is empty",
                    super::SQLITE_SCHEMA    => "The database schema changed",
                    super::SQLITE_TOOBIG    => "String or BLOB exceeds size limit",
                    super::SQLITE_CONSTRAINT=> "Abort due to constraint violation",
                    super::SQLITE_MISMATCH  => "Data type mismatch",
                    super::SQLITE_MISUSE    => "Library used incorrectly",
                    super::SQLITE_NOLFS     => "Uses OS features not supported on host",
                    super::SQLITE_AUTH      => "Authorization denied",
                    super::SQLITE_FORMAT    => "Auxiliary database format error",
                    super::SQLITE_RANGE     => "2nd parameter to sqlite3_bind out of range",
                    super::SQLITE_NOTADB    => "File opened that is not a database file",
                    super::SQLITE_NOTICE    => "Notifications from sqlite3_log()",
                    super::SQLITE_WARNING   => "Warnings from sqlite3_log()",
                    super::SQLITE_ROW       => "sqlite3_step() has another row ready",
                    super::SQLITE_DONE      => "sqlite3_step() has finished executing",
            
                    SQLITE_ERROR_MISSING_COLLSEQ   => "SQLITE_ERROR_MISSING_COLLSEQ",
                    SQLITE_ERROR_RETRY   => "SQLITE_ERROR_RETRY",
                    SQLITE_ERROR_SNAPSHOT   => "SQLITE_ERROR_SNAPSHOT",
            
                    super::SQLITE_IOERR_READ              => "Error reading from disk",
                    super::SQLITE_IOERR_SHORT_READ        => "Unable to obtain number of requested bytes (file truncated?)",
                    super::SQLITE_IOERR_WRITE             => "Error writing to disk",
                    super::SQLITE_IOERR_FSYNC             => "Error flushing data to persistent storage (fsync)",
                    super::SQLITE_IOERR_DIR_FSYNC         => "Error calling fsync on a directory",
                    super::SQLITE_IOERR_TRUNCATE          => "Error attempting to truncate file",
                    super::SQLITE_IOERR_FSTAT             => "Error invoking fstat to get file metadata",
                    super::SQLITE_IOERR_UNLOCK            => "I/O error within xUnlock of a VFS object",
                    super::SQLITE_IOERR_RDLOCK            => "I/O error within xLock of a VFS object (trying to obtain a read lock)",
                    super::SQLITE_IOERR_DELETE            => "I/O error within xDelete of a VFS object",
                    super::SQLITE_IOERR_BLOCKED           => "SQLITE_IOERR_BLOCKED", // no longer used
                    super::SQLITE_IOERR_NOMEM             => "Out of memory in I/O layer",
                    super::SQLITE_IOERR_ACCESS            => "I/O error within xAccess of a VFS object",
                    super::SQLITE_IOERR_CHECKRESERVEDLOCK => "I/O error within then xCheckReservedLock method",
                    super::SQLITE_IOERR_LOCK              => "I/O error in the advisory file locking layer",
                    super::SQLITE_IOERR_CLOSE             => "I/O error within the xClose method",
                    super::SQLITE_IOERR_DIR_CLOSE         => "SQLITE_IOERR_DIR_CLOSE", // no longer used
                    super::SQLITE_IOERR_SHMOPEN           => "I/O error within the xShmMap method (trying to open a new shared-memory segment)",
                    super::SQLITE_IOERR_SHMSIZE           => "I/O error within the xShmMap method (trying to resize an existing shared-memory segment)",
                    super::SQLITE_IOERR_SHMLOCK           => "SQLITE_IOERR_SHMLOCK", // no longer used
                    super::SQLITE_IOERR_SHMMAP            => "I/O error within the xShmMap method (trying to map a shared-memory segment into process address space)",
                    super::SQLITE_IOERR_SEEK              => "I/O error within the xRead or xWrite (trying to seek within a file)",
                    super::SQLITE_IOERR_DELETE_NOENT      => "File being deleted does not exist",
                    super::SQLITE_IOERR_MMAP              => "I/O error while trying to map or unmap part of the database file into process address space",
                    super::SQLITE_IOERR_GETTEMPPATH       => "VFS is unable to determine a suitable directory for temporary files",
                    super::SQLITE_IOERR_CONVPATH          => "cygwin_conv_path() system call failed",
                    super::SQLITE_IOERR_VNODE             => "SQLITE_IOERR_VNODE", // not documented?
                    super::SQLITE_IOERR_AUTH              => "SQLITE_IOERR_AUTH",
                    SQLITE_IOERR_BEGIN_ATOMIC      => "SQLITE_IOERR_BEGIN_ATOMIC",
                    SQLITE_IOERR_COMMIT_ATOMIC     => "SQLITE_IOERR_COMMIT_ATOMIC",
                    SQLITE_IOERR_ROLLBACK_ATOMIC   => "SQLITE_IOERR_ROLLBACK_ATOMIC",
                    SQLITE_IOERR_DATA   => "SQLITE_IOERR_DATA",
                    SQLITE_IOERR_CORRUPTFS   => "SQLITE_IOERR_CORRUPTFS",
                    SQLITE_IOERR_IN_PAGE   => "SQLITE_IOERR_IN_PAGE",
            
                    super::SQLITE_LOCKED_SHAREDCACHE      => "Locking conflict due to another connection with a shared cache",
                    SQLITE_LOCKED_VTAB             => "SQLITE_LOCKED_VTAB",
            
                    super::SQLITE_BUSY_RECOVERY           => "Another process is recovering a WAL mode database file",
                    super::SQLITE_BUSY_SNAPSHOT           => "Cannot promote read transaction to write transaction because of writes by another connection",
                    SQLITE_BUSY_TIMEOUT           => "SQLITE_BUSY_TIMEOUT",
            
                    super::SQLITE_CANTOPEN_NOTEMPDIR      => "SQLITE_CANTOPEN_NOTEMPDIR", // no longer used
                    super::SQLITE_CANTOPEN_ISDIR          => "Attempted to open directory as file",
                    super::SQLITE_CANTOPEN_FULLPATH       => "Unable to convert filename into full pathname",
                    super::SQLITE_CANTOPEN_CONVPATH       => "cygwin_conv_path() system call failed",
                    SQLITE_CANTOPEN_SYMLINK       => "SQLITE_CANTOPEN_SYMLINK",
            
                    super::SQLITE_CORRUPT_VTAB            => "Content in the virtual table is corrupt",
                    SQLITE_CORRUPT_SEQUENCE        => "SQLITE_CORRUPT_SEQUENCE",
                    SQLITE_CORRUPT_INDEX        => "SQLITE_CORRUPT_INDEX",
            
                    super::SQLITE_READONLY_RECOVERY       => "WAL mode database file needs recovery (requires write access)",
                    super::SQLITE_READONLY_CANTLOCK       => "Shared-memory file associated with WAL mode database is read-only",
                    super::SQLITE_READONLY_ROLLBACK       => "Database has hot journal that must be rolled back (requires write access)",
                    super::SQLITE_READONLY_DBMOVED        => "Database cannot be modified because database file has moved",
                    SQLITE_READONLY_CANTINIT       => "SQLITE_READONLY_CANTINIT",
                    SQLITE_READONLY_DIRECTORY      => "SQLITE_READONLY_DIRECTORY",
            
                    super::SQLITE_ABORT_ROLLBACK          => "Transaction was rolled back",
            
                    super::SQLITE_CONSTRAINT_CHECK        => "A CHECK constraint failed",
                    super::SQLITE_CONSTRAINT_COMMITHOOK   => "Commit hook caused rollback",
                    super::SQLITE_CONSTRAINT_FOREIGNKEY   => "Foreign key constraint failed",
                    super::SQLITE_CONSTRAINT_FUNCTION     => "Error returned from extension function",
                    super::SQLITE_CONSTRAINT_NOTNULL      => "A NOT NULL constraint failed",
                    super::SQLITE_CONSTRAINT_PRIMARYKEY   => "A PRIMARY KEY constraint failed",
                    super::SQLITE_CONSTRAINT_TRIGGER      => "A RAISE function within a trigger fired",
                    super::SQLITE_CONSTRAINT_UNIQUE       => "A UNIQUE constraint failed",
                    super::SQLITE_CONSTRAINT_VTAB         => "An application-defined virtual table error occurred",
                    super::SQLITE_CONSTRAINT_ROWID        => "A non-unique rowid occurred",
                    SQLITE_CONSTRAINT_PINNED        => "SQLITE_CONSTRAINT_PINNED",
                    SQLITE_CONSTRAINT_DATATYPE        => "SQLITE_CONSTRAINT_DATATYPE",
            
                    super::SQLITE_NOTICE_RECOVER_WAL      => "A WAL mode database file was recovered",
                    super::SQLITE_NOTICE_RECOVER_ROLLBACK => "Hot journal was rolled back",
            
                    super::SQLITE_WARNING_AUTOINDEX       => "Automatic indexing used - database might benefit from additional indexes",
            
                    super::SQLITE_AUTH_USER               => "SQLITE_AUTH_USER", // not documented?
            
                    _ => "Unknown error code",
                }
            }
            
            /// Loadable extension initialization error
            #[cfg(feature = "loadable_extension")]
            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            #[non_exhaustive]
            pub enum InitError {
                /// Version mismatch between the extension and the SQLite3 library
                VersionMismatch { compile_time: i32, runtime: i32 },
                /// Invalid function pointer in one of sqlite3_api_routines fields
                NullFunctionPointer,
            }
            #[cfg(feature = "loadable_extension")]
            impl ::std::fmt::Display for InitError {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    match *self {
                        InitError::VersionMismatch {
                            compile_time,
                            runtime,
                        } => {
                            write!(f, "SQLite version mismatch: {runtime} < {compile_time}")
                        }
                        InitError::NullFunctionPointer => {
                            write!(f, "Some sqlite3_api_routines fields are null")
                        }
                    }
                }
            }
            #[cfg(feature = "loadable_extension")]
            impl error::Error for InitError {}
        } pub use self::error::*;
        
        extern "C"
        {
            pub fn sqlite3_auto_extension(
                xEntryPoint: ::std::option::Option<
                    unsafe extern "C" fn(
                        db: *mut sqlite3,
                        pzErrMsg: *mut *mut ::std::os::raw::c_char,
                        _: *const sqlite3_api_routines,
                    ) -> ::std::os::raw::c_int,
                >,
            ) -> ::std::os::raw::c_int;
        }
        
        extern "C"
        {
            pub fn sqlite3_cancel_auto_extension(
                xEntryPoint: ::std::option::Option<
                    unsafe extern "C" fn(
                        db: *mut sqlite3,
                        pzErrMsg: *mut *mut ::std::os::raw::c_char,
                        _: *const sqlite3_api_routines,
                    ) -> ::std::os::raw::c_int,
                >,
            ) -> ::std::os::raw::c_int;
        }
        
        pub const SQLITE_VERSION: &[u8; 7] = b"3.46.0\0";
        pub const SQLITE_VERSION_NUMBER: i32 = 3046000;
        pub const SQLITE_SOURCE_ID: &[u8; 85] =
            b"2024-05-23 13:25:27 96c92aba00c8375bc32fafcdf12429c58bd8aabfcadab6683e35bbb9cdebf19e\0";
        pub const SQLITE_OK: i32 = 0;
        pub const SQLITE_ERROR: i32 = 1;
        pub const SQLITE_INTERNAL: i32 = 2;
        pub const SQLITE_PERM: i32 = 3;
        pub const SQLITE_ABORT: i32 = 4;
        pub const SQLITE_BUSY: i32 = 5;
        pub const SQLITE_LOCKED: i32 = 6;
        pub const SQLITE_NOMEM: i32 = 7;
        pub const SQLITE_READONLY: i32 = 8;
        pub const SQLITE_INTERRUPT: i32 = 9;
        pub const SQLITE_IOERR: i32 = 10;
        pub const SQLITE_CORRUPT: i32 = 11;
        pub const SQLITE_NOTFOUND: i32 = 12;
        pub const SQLITE_FULL: i32 = 13;
        pub const SQLITE_CANTOPEN: i32 = 14;
        pub const SQLITE_PROTOCOL: i32 = 15;
        pub const SQLITE_EMPTY: i32 = 16;
        pub const SQLITE_SCHEMA: i32 = 17;
        pub const SQLITE_TOOBIG: i32 = 18;
        pub const SQLITE_CONSTRAINT: i32 = 19;
        pub const SQLITE_MISMATCH: i32 = 20;
        pub const SQLITE_MISUSE: i32 = 21;
        pub const SQLITE_NOLFS: i32 = 22;
        pub const SQLITE_AUTH: i32 = 23;
        pub const SQLITE_FORMAT: i32 = 24;
        pub const SQLITE_RANGE: i32 = 25;
        pub const SQLITE_NOTADB: i32 = 26;
        pub const SQLITE_NOTICE: i32 = 27;
        pub const SQLITE_WARNING: i32 = 28;
        pub const SQLITE_ROW: i32 = 100;
        pub const SQLITE_DONE: i32 = 101;
        pub const SQLITE_ERROR_MISSING_COLLSEQ: i32 = 257;
        pub const SQLITE_ERROR_RETRY: i32 = 513;
        pub const SQLITE_ERROR_SNAPSHOT: i32 = 769;
        pub const SQLITE_IOERR_READ: i32 = 266;
        pub const SQLITE_IOERR_SHORT_READ: i32 = 522;
        pub const SQLITE_IOERR_WRITE: i32 = 778;
        pub const SQLITE_IOERR_FSYNC: i32 = 1034;
        pub const SQLITE_IOERR_DIR_FSYNC: i32 = 1290;
        pub const SQLITE_IOERR_TRUNCATE: i32 = 1546;
        pub const SQLITE_IOERR_FSTAT: i32 = 1802;
        pub const SQLITE_IOERR_UNLOCK: i32 = 2058;
        pub const SQLITE_IOERR_RDLOCK: i32 = 2314;
        pub const SQLITE_IOERR_DELETE: i32 = 2570;
        pub const SQLITE_IOERR_BLOCKED: i32 = 2826;
        pub const SQLITE_IOERR_NOMEM: i32 = 3082;
        pub const SQLITE_IOERR_ACCESS: i32 = 3338;
        pub const SQLITE_IOERR_CHECKRESERVEDLOCK: i32 = 3594;
        pub const SQLITE_IOERR_LOCK: i32 = 3850;
        pub const SQLITE_IOERR_CLOSE: i32 = 4106;
        pub const SQLITE_IOERR_DIR_CLOSE: i32 = 4362;
        pub const SQLITE_IOERR_SHMOPEN: i32 = 4618;
        pub const SQLITE_IOERR_SHMSIZE: i32 = 4874;
        pub const SQLITE_IOERR_SHMLOCK: i32 = 5130;
        pub const SQLITE_IOERR_SHMMAP: i32 = 5386;
        pub const SQLITE_IOERR_SEEK: i32 = 5642;
        pub const SQLITE_IOERR_DELETE_NOENT: i32 = 5898;
        pub const SQLITE_IOERR_MMAP: i32 = 6154;
        pub const SQLITE_IOERR_GETTEMPPATH: i32 = 6410;
        pub const SQLITE_IOERR_CONVPATH: i32 = 6666;
        pub const SQLITE_IOERR_VNODE: i32 = 6922;
        pub const SQLITE_IOERR_AUTH: i32 = 7178;
        pub const SQLITE_IOERR_BEGIN_ATOMIC: i32 = 7434;
        pub const SQLITE_IOERR_COMMIT_ATOMIC: i32 = 7690;
        pub const SQLITE_IOERR_ROLLBACK_ATOMIC: i32 = 7946;
        pub const SQLITE_IOERR_DATA: i32 = 8202;
        pub const SQLITE_IOERR_CORRUPTFS: i32 = 8458;
        pub const SQLITE_IOERR_IN_PAGE: i32 = 8714;
        pub const SQLITE_LOCKED_SHAREDCACHE: i32 = 262;
        pub const SQLITE_LOCKED_VTAB: i32 = 518;
        pub const SQLITE_BUSY_RECOVERY: i32 = 261;
        pub const SQLITE_BUSY_SNAPSHOT: i32 = 517;
        pub const SQLITE_BUSY_TIMEOUT: i32 = 773;
        pub const SQLITE_CANTOPEN_NOTEMPDIR: i32 = 270;
        pub const SQLITE_CANTOPEN_ISDIR: i32 = 526;
        pub const SQLITE_CANTOPEN_FULLPATH: i32 = 782;
        pub const SQLITE_CANTOPEN_CONVPATH: i32 = 1038;
        pub const SQLITE_CANTOPEN_DIRTYWAL: i32 = 1294;
        pub const SQLITE_CANTOPEN_SYMLINK: i32 = 1550;
        pub const SQLITE_CORRUPT_VTAB: i32 = 267;
        pub const SQLITE_CORRUPT_SEQUENCE: i32 = 523;
        pub const SQLITE_CORRUPT_INDEX: i32 = 779;
        pub const SQLITE_READONLY_RECOVERY: i32 = 264;
        pub const SQLITE_READONLY_CANTLOCK: i32 = 520;
        pub const SQLITE_READONLY_ROLLBACK: i32 = 776;
        pub const SQLITE_READONLY_DBMOVED: i32 = 1032;
        pub const SQLITE_READONLY_CANTINIT: i32 = 1288;
        pub const SQLITE_READONLY_DIRECTORY: i32 = 1544;
        pub const SQLITE_ABORT_ROLLBACK: i32 = 516;
        pub const SQLITE_CONSTRAINT_CHECK: i32 = 275;
        pub const SQLITE_CONSTRAINT_COMMITHOOK: i32 = 531;
        pub const SQLITE_CONSTRAINT_FOREIGNKEY: i32 = 787;
        pub const SQLITE_CONSTRAINT_FUNCTION: i32 = 1043;
        pub const SQLITE_CONSTRAINT_NOTNULL: i32 = 1299;
        pub const SQLITE_CONSTRAINT_PRIMARYKEY: i32 = 1555;
        pub const SQLITE_CONSTRAINT_TRIGGER: i32 = 1811;
        pub const SQLITE_CONSTRAINT_UNIQUE: i32 = 2067;
        pub const SQLITE_CONSTRAINT_VTAB: i32 = 2323;
        pub const SQLITE_CONSTRAINT_ROWID: i32 = 2579;
        pub const SQLITE_CONSTRAINT_PINNED: i32 = 2835;
        pub const SQLITE_CONSTRAINT_DATATYPE: i32 = 3091;
        pub const SQLITE_NOTICE_RECOVER_WAL: i32 = 283;
        pub const SQLITE_NOTICE_RECOVER_ROLLBACK: i32 = 539;
        pub const SQLITE_NOTICE_RBU: i32 = 795;
        pub const SQLITE_WARNING_AUTOINDEX: i32 = 284;
        pub const SQLITE_AUTH_USER: i32 = 279;
        pub const SQLITE_OK_LOAD_PERMANENTLY: i32 = 256;
        pub const SQLITE_OK_SYMLINK: i32 = 512;
        pub const SQLITE_OPEN_READONLY: i32 = 1;
        pub const SQLITE_OPEN_READWRITE: i32 = 2;
        pub const SQLITE_OPEN_CREATE: i32 = 4;
        pub const SQLITE_OPEN_DELETEONCLOSE: i32 = 8;
        pub const SQLITE_OPEN_EXCLUSIVE: i32 = 16;
        pub const SQLITE_OPEN_AUTOPROXY: i32 = 32;
        pub const SQLITE_OPEN_URI: i32 = 64;
        pub const SQLITE_OPEN_MEMORY: i32 = 128;
        pub const SQLITE_OPEN_MAIN_DB: i32 = 256;
        pub const SQLITE_OPEN_TEMP_DB: i32 = 512;
        pub const SQLITE_OPEN_TRANSIENT_DB: i32 = 1024;
        pub const SQLITE_OPEN_MAIN_JOURNAL: i32 = 2048;
        pub const SQLITE_OPEN_TEMP_JOURNAL: i32 = 4096;
        pub const SQLITE_OPEN_SUBJOURNAL: i32 = 8192;
        pub const SQLITE_OPEN_SUPER_JOURNAL: i32 = 16384;
        pub const SQLITE_OPEN_NOMUTEX: i32 = 32768;
        pub const SQLITE_OPEN_FULLMUTEX: i32 = 65536;
        pub const SQLITE_OPEN_SHAREDCACHE: i32 = 131072;
        pub const SQLITE_OPEN_PRIVATECACHE: i32 = 262144;
        pub const SQLITE_OPEN_WAL: i32 = 524288;
        pub const SQLITE_OPEN_NOFOLLOW: i32 = 16777216;
        pub const SQLITE_OPEN_EXRESCODE: i32 = 33554432;
        pub const SQLITE_OPEN_MASTER_JOURNAL: i32 = 16384;
        pub const SQLITE_IOCAP_ATOMIC: i32 = 1;
        pub const SQLITE_IOCAP_ATOMIC512: i32 = 2;
        pub const SQLITE_IOCAP_ATOMIC1K: i32 = 4;
        pub const SQLITE_IOCAP_ATOMIC2K: i32 = 8;
        pub const SQLITE_IOCAP_ATOMIC4K: i32 = 16;
        pub const SQLITE_IOCAP_ATOMIC8K: i32 = 32;
        pub const SQLITE_IOCAP_ATOMIC16K: i32 = 64;
        pub const SQLITE_IOCAP_ATOMIC32K: i32 = 128;
        pub const SQLITE_IOCAP_ATOMIC64K: i32 = 256;
        pub const SQLITE_IOCAP_SAFE_APPEND: i32 = 512;
        pub const SQLITE_IOCAP_SEQUENTIAL: i32 = 1024;
        pub const SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN: i32 = 2048;
        pub const SQLITE_IOCAP_POWERSAFE_OVERWRITE: i32 = 4096;
        pub const SQLITE_IOCAP_IMMUTABLE: i32 = 8192;
        pub const SQLITE_IOCAP_BATCH_ATOMIC: i32 = 16384;
        pub const SQLITE_LOCK_NONE: i32 = 0;
        pub const SQLITE_LOCK_SHARED: i32 = 1;
        pub const SQLITE_LOCK_RESERVED: i32 = 2;
        pub const SQLITE_LOCK_PENDING: i32 = 3;
        pub const SQLITE_LOCK_EXCLUSIVE: i32 = 4;
        pub const SQLITE_SYNC_NORMAL: i32 = 2;
        pub const SQLITE_SYNC_FULL: i32 = 3;
        pub const SQLITE_SYNC_DATAONLY: i32 = 16;
        pub const SQLITE_FCNTL_LOCKSTATE: i32 = 1;
        pub const SQLITE_FCNTL_GET_LOCKPROXYFILE: i32 = 2;
        pub const SQLITE_FCNTL_SET_LOCKPROXYFILE: i32 = 3;
        pub const SQLITE_FCNTL_LAST_ERRNO: i32 = 4;
        pub const SQLITE_FCNTL_SIZE_HINT: i32 = 5;
        pub const SQLITE_FCNTL_CHUNK_SIZE: i32 = 6;
        pub const SQLITE_FCNTL_FILE_POINTER: i32 = 7;
        pub const SQLITE_FCNTL_SYNC_OMITTED: i32 = 8;
        pub const SQLITE_FCNTL_WIN32_AV_RETRY: i32 = 9;
        pub const SQLITE_FCNTL_PERSIST_WAL: i32 = 10;
        pub const SQLITE_FCNTL_OVERWRITE: i32 = 11;
        pub const SQLITE_FCNTL_VFSNAME: i32 = 12;
        pub const SQLITE_FCNTL_POWERSAFE_OVERWRITE: i32 = 13;
        pub const SQLITE_FCNTL_PRAGMA: i32 = 14;
        pub const SQLITE_FCNTL_BUSYHANDLER: i32 = 15;
        pub const SQLITE_FCNTL_TEMPFILENAME: i32 = 16;
        pub const SQLITE_FCNTL_MMAP_SIZE: i32 = 18;
        pub const SQLITE_FCNTL_TRACE: i32 = 19;
        pub const SQLITE_FCNTL_HAS_MOVED: i32 = 20;
        pub const SQLITE_FCNTL_SYNC: i32 = 21;
        pub const SQLITE_FCNTL_COMMIT_PHASETWO: i32 = 22;
        pub const SQLITE_FCNTL_WIN32_SET_HANDLE: i32 = 23;
        pub const SQLITE_FCNTL_WAL_BLOCK: i32 = 24;
        pub const SQLITE_FCNTL_ZIPVFS: i32 = 25;
        pub const SQLITE_FCNTL_RBU: i32 = 26;
        pub const SQLITE_FCNTL_VFS_POINTER: i32 = 27;
        pub const SQLITE_FCNTL_JOURNAL_POINTER: i32 = 28;
        pub const SQLITE_FCNTL_WIN32_GET_HANDLE: i32 = 29;
        pub const SQLITE_FCNTL_PDB: i32 = 30;
        pub const SQLITE_FCNTL_BEGIN_ATOMIC_WRITE: i32 = 31;
        pub const SQLITE_FCNTL_COMMIT_ATOMIC_WRITE: i32 = 32;
        pub const SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE: i32 = 33;
        pub const SQLITE_FCNTL_LOCK_TIMEOUT: i32 = 34;
        pub const SQLITE_FCNTL_DATA_VERSION: i32 = 35;
        pub const SQLITE_FCNTL_SIZE_LIMIT: i32 = 36;
        pub const SQLITE_FCNTL_CKPT_DONE: i32 = 37;
        pub const SQLITE_FCNTL_RESERVE_BYTES: i32 = 38;
        pub const SQLITE_FCNTL_CKPT_START: i32 = 39;
        pub const SQLITE_FCNTL_EXTERNAL_READER: i32 = 40;
        pub const SQLITE_FCNTL_CKSM_FILE: i32 = 41;
        pub const SQLITE_FCNTL_RESET_CACHE: i32 = 42;
        pub const SQLITE_GET_LOCKPROXYFILE: i32 = 2;
        pub const SQLITE_SET_LOCKPROXYFILE: i32 = 3;
        pub const SQLITE_LAST_ERRNO: i32 = 4;
        pub const SQLITE_ACCESS_EXISTS: i32 = 0;
        pub const SQLITE_ACCESS_READWRITE: i32 = 1;
        pub const SQLITE_ACCESS_READ: i32 = 2;
        pub const SQLITE_SHM_UNLOCK: i32 = 1;
        pub const SQLITE_SHM_LOCK: i32 = 2;
        pub const SQLITE_SHM_SHARED: i32 = 4;
        pub const SQLITE_SHM_EXCLUSIVE: i32 = 8;
        pub const SQLITE_SHM_NLOCK: i32 = 8;
        pub const SQLITE_CONFIG_SINGLETHREAD: i32 = 1;
        pub const SQLITE_CONFIG_MULTITHREAD: i32 = 2;
        pub const SQLITE_CONFIG_SERIALIZED: i32 = 3;
        pub const SQLITE_CONFIG_MALLOC: i32 = 4;
        pub const SQLITE_CONFIG_GETMALLOC: i32 = 5;
        pub const SQLITE_CONFIG_SCRATCH: i32 = 6;
        pub const SQLITE_CONFIG_PAGECACHE: i32 = 7;
        pub const SQLITE_CONFIG_HEAP: i32 = 8;
        pub const SQLITE_CONFIG_MEMSTATUS: i32 = 9;
        pub const SQLITE_CONFIG_MUTEX: i32 = 10;
        pub const SQLITE_CONFIG_GETMUTEX: i32 = 11;
        pub const SQLITE_CONFIG_LOOKASIDE: i32 = 13;
        pub const SQLITE_CONFIG_PCACHE: i32 = 14;
        pub const SQLITE_CONFIG_GETPCACHE: i32 = 15;
        pub const SQLITE_CONFIG_LOG: i32 = 16;
        pub const SQLITE_CONFIG_URI: i32 = 17;
        pub const SQLITE_CONFIG_PCACHE2: i32 = 18;
        pub const SQLITE_CONFIG_GETPCACHE2: i32 = 19;
        pub const SQLITE_CONFIG_COVERING_INDEX_SCAN: i32 = 20;
        pub const SQLITE_CONFIG_SQLLOG: i32 = 21;
        pub const SQLITE_CONFIG_MMAP_SIZE: i32 = 22;
        pub const SQLITE_CONFIG_WIN32_HEAPSIZE: i32 = 23;
        pub const SQLITE_CONFIG_PCACHE_HDRSZ: i32 = 24;
        pub const SQLITE_CONFIG_PMASZ: i32 = 25;
        pub const SQLITE_CONFIG_STMTJRNL_SPILL: i32 = 26;
        pub const SQLITE_CONFIG_SMALL_MALLOC: i32 = 27;
        pub const SQLITE_CONFIG_SORTERREF_SIZE: i32 = 28;
        pub const SQLITE_CONFIG_MEMDB_MAXSIZE: i32 = 29;
        pub const SQLITE_CONFIG_ROWID_IN_VIEW: i32 = 30;
        pub const SQLITE_DBCONFIG_MAINDBNAME: i32 = 1000;
        pub const SQLITE_DBCONFIG_LOOKASIDE: i32 = 1001;
        pub const SQLITE_DBCONFIG_ENABLE_FKEY: i32 = 1002;
        pub const SQLITE_DBCONFIG_ENABLE_TRIGGER: i32 = 1003;
        pub const SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER: i32 = 1004;
        pub const SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION: i32 = 1005;
        pub const SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE: i32 = 1006;
        pub const SQLITE_DBCONFIG_ENABLE_QPSG: i32 = 1007;
        pub const SQLITE_DBCONFIG_TRIGGER_EQP: i32 = 1008;
        pub const SQLITE_DBCONFIG_RESET_DATABASE: i32 = 1009;
        pub const SQLITE_DBCONFIG_DEFENSIVE: i32 = 1010;
        pub const SQLITE_DBCONFIG_WRITABLE_SCHEMA: i32 = 1011;
        pub const SQLITE_DBCONFIG_LEGACY_ALTER_TABLE: i32 = 1012;
        pub const SQLITE_DBCONFIG_DQS_DML: i32 = 1013;
        pub const SQLITE_DBCONFIG_DQS_DDL: i32 = 1014;
        pub const SQLITE_DBCONFIG_ENABLE_VIEW: i32 = 1015;
        pub const SQLITE_DBCONFIG_LEGACY_FILE_FORMAT: i32 = 1016;
        pub const SQLITE_DBCONFIG_TRUSTED_SCHEMA: i32 = 1017;
        pub const SQLITE_DBCONFIG_STMT_SCANSTATUS: i32 = 1018;
        pub const SQLITE_DBCONFIG_REVERSE_SCANORDER: i32 = 1019;
        pub const SQLITE_DBCONFIG_MAX: i32 = 1019;
        pub const SQLITE_DENY: i32 = 1;
        pub const SQLITE_IGNORE: i32 = 2;
        pub const SQLITE_CREATE_INDEX: i32 = 1;
        pub const SQLITE_CREATE_TABLE: i32 = 2;
        pub const SQLITE_CREATE_TEMP_INDEX: i32 = 3;
        pub const SQLITE_CREATE_TEMP_TABLE: i32 = 4;
        pub const SQLITE_CREATE_TEMP_TRIGGER: i32 = 5;
        pub const SQLITE_CREATE_TEMP_VIEW: i32 = 6;
        pub const SQLITE_CREATE_TRIGGER: i32 = 7;
        pub const SQLITE_CREATE_VIEW: i32 = 8;
        pub const SQLITE_DELETE: i32 = 9;
        pub const SQLITE_DROP_INDEX: i32 = 10;
        pub const SQLITE_DROP_TABLE: i32 = 11;
        pub const SQLITE_DROP_TEMP_INDEX: i32 = 12;
        pub const SQLITE_DROP_TEMP_TABLE: i32 = 13;
        pub const SQLITE_DROP_TEMP_TRIGGER: i32 = 14;
        pub const SQLITE_DROP_TEMP_VIEW: i32 = 15;
        pub const SQLITE_DROP_TRIGGER: i32 = 16;
        pub const SQLITE_DROP_VIEW: i32 = 17;
        pub const SQLITE_INSERT: i32 = 18;
        pub const SQLITE_PRAGMA: i32 = 19;
        pub const SQLITE_READ: i32 = 20;
        pub const SQLITE_SELECT: i32 = 21;
        pub const SQLITE_TRANSACTION: i32 = 22;
        pub const SQLITE_UPDATE: i32 = 23;
        pub const SQLITE_ATTACH: i32 = 24;
        pub const SQLITE_DETACH: i32 = 25;
        pub const SQLITE_ALTER_TABLE: i32 = 26;
        pub const SQLITE_REINDEX: i32 = 27;
        pub const SQLITE_ANALYZE: i32 = 28;
        pub const SQLITE_CREATE_VTABLE: i32 = 29;
        pub const SQLITE_DROP_VTABLE: i32 = 30;
        pub const SQLITE_FUNCTION: i32 = 31;
        pub const SQLITE_SAVEPOINT: i32 = 32;
        pub const SQLITE_COPY: i32 = 0;
        pub const SQLITE_RECURSIVE: i32 = 33;
        pub const SQLITE_TRACE_STMT: i32 = 1;
        pub const SQLITE_TRACE_PROFILE: i32 = 2;
        pub const SQLITE_TRACE_ROW: i32 = 4;
        pub const SQLITE_TRACE_CLOSE: i32 = 8;
        pub const SQLITE_LIMIT_LENGTH: i32 = 0;
        pub const SQLITE_LIMIT_SQL_LENGTH: i32 = 1;
        pub const SQLITE_LIMIT_COLUMN: i32 = 2;
        pub const SQLITE_LIMIT_EXPR_DEPTH: i32 = 3;
        pub const SQLITE_LIMIT_COMPOUND_SELECT: i32 = 4;
        pub const SQLITE_LIMIT_VDBE_OP: i32 = 5;
        pub const SQLITE_LIMIT_FUNCTION_ARG: i32 = 6;
        pub const SQLITE_LIMIT_ATTACHED: i32 = 7;
        pub const SQLITE_LIMIT_LIKE_PATTERN_LENGTH: i32 = 8;
        pub const SQLITE_LIMIT_VARIABLE_NUMBER: i32 = 9;
        pub const SQLITE_LIMIT_TRIGGER_DEPTH: i32 = 10;
        pub const SQLITE_LIMIT_WORKER_THREADS: i32 = 11;
        pub const SQLITE_PREPARE_PERSISTENT: ::std::os::raw::c_uint = 1;
        pub const SQLITE_PREPARE_NORMALIZE: ::std::os::raw::c_uint = 2;
        pub const SQLITE_PREPARE_NO_VTAB: ::std::os::raw::c_uint = 4;
        pub const SQLITE_INTEGER: i32 = 1;
        pub const SQLITE_FLOAT: i32 = 2;
        pub const SQLITE_BLOB: i32 = 4;
        pub const SQLITE_NULL: i32 = 5;
        pub const SQLITE_TEXT: i32 = 3;
        pub const SQLITE3_TEXT: i32 = 3;
        pub const SQLITE_UTF8: i32 = 1;
        pub const SQLITE_UTF16LE: i32 = 2;
        pub const SQLITE_UTF16BE: i32 = 3;
        pub const SQLITE_UTF16: i32 = 4;
        pub const SQLITE_ANY: i32 = 5;
        pub const SQLITE_UTF16_ALIGNED: i32 = 8;
        pub const SQLITE_DETERMINISTIC: i32 = 2048;
        pub const SQLITE_DIRECTONLY: i32 = 524288;
        pub const SQLITE_SUBTYPE: i32 = 1048576;
        pub const SQLITE_INNOCUOUS: i32 = 2097152;
        pub const SQLITE_RESULT_SUBTYPE: i32 = 16777216;
        pub const SQLITE_WIN32_DATA_DIRECTORY_TYPE: i32 = 1;
        pub const SQLITE_WIN32_TEMP_DIRECTORY_TYPE: i32 = 2;
        pub const SQLITE_TXN_NONE: i32 = 0;
        pub const SQLITE_TXN_READ: i32 = 1;
        pub const SQLITE_TXN_WRITE: i32 = 2;
        pub const SQLITE_INDEX_SCAN_UNIQUE: i32 = 1;
        pub const SQLITE_INDEX_CONSTRAINT_EQ: i32 = 2;
        pub const SQLITE_INDEX_CONSTRAINT_GT: i32 = 4;
        pub const SQLITE_INDEX_CONSTRAINT_LE: i32 = 8;
        pub const SQLITE_INDEX_CONSTRAINT_LT: i32 = 16;
        pub const SQLITE_INDEX_CONSTRAINT_GE: i32 = 32;
        pub const SQLITE_INDEX_CONSTRAINT_MATCH: i32 = 64;
        pub const SQLITE_INDEX_CONSTRAINT_LIKE: i32 = 65;
        pub const SQLITE_INDEX_CONSTRAINT_GLOB: i32 = 66;
        pub const SQLITE_INDEX_CONSTRAINT_REGEXP: i32 = 67;
        pub const SQLITE_INDEX_CONSTRAINT_NE: i32 = 68;
        pub const SQLITE_INDEX_CONSTRAINT_ISNOT: i32 = 69;
        pub const SQLITE_INDEX_CONSTRAINT_ISNOTNULL: i32 = 70;
        pub const SQLITE_INDEX_CONSTRAINT_ISNULL: i32 = 71;
        pub const SQLITE_INDEX_CONSTRAINT_IS: i32 = 72;
        pub const SQLITE_INDEX_CONSTRAINT_LIMIT: i32 = 73;
        pub const SQLITE_INDEX_CONSTRAINT_OFFSET: i32 = 74;
        pub const SQLITE_INDEX_CONSTRAINT_FUNCTION: i32 = 150;
        pub const SQLITE_MUTEX_FAST: i32 = 0;
        pub const SQLITE_MUTEX_RECURSIVE: i32 = 1;
        pub const SQLITE_MUTEX_STATIC_MAIN: i32 = 2;
        pub const SQLITE_MUTEX_STATIC_MEM: i32 = 3;
        pub const SQLITE_MUTEX_STATIC_MEM2: i32 = 4;
        pub const SQLITE_MUTEX_STATIC_OPEN: i32 = 4;
        pub const SQLITE_MUTEX_STATIC_PRNG: i32 = 5;
        pub const SQLITE_MUTEX_STATIC_LRU: i32 = 6;
        pub const SQLITE_MUTEX_STATIC_LRU2: i32 = 7;
        pub const SQLITE_MUTEX_STATIC_PMEM: i32 = 7;
        pub const SQLITE_MUTEX_STATIC_APP1: i32 = 8;
        pub const SQLITE_MUTEX_STATIC_APP2: i32 = 9;
        pub const SQLITE_MUTEX_STATIC_APP3: i32 = 10;
        pub const SQLITE_MUTEX_STATIC_VFS1: i32 = 11;
        pub const SQLITE_MUTEX_STATIC_VFS2: i32 = 12;
        pub const SQLITE_MUTEX_STATIC_VFS3: i32 = 13;
        pub const SQLITE_MUTEX_STATIC_MASTER: i32 = 2;
        pub const SQLITE_TESTCTRL_FIRST: i32 = 5;
        pub const SQLITE_TESTCTRL_PRNG_SAVE: i32 = 5;
        pub const SQLITE_TESTCTRL_PRNG_RESTORE: i32 = 6;
        pub const SQLITE_TESTCTRL_PRNG_RESET: i32 = 7;
        pub const SQLITE_TESTCTRL_FK_NO_ACTION: i32 = 7;
        pub const SQLITE_TESTCTRL_BITVEC_TEST: i32 = 8;
        pub const SQLITE_TESTCTRL_FAULT_INSTALL: i32 = 9;
        pub const SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS: i32 = 10;
        pub const SQLITE_TESTCTRL_PENDING_BYTE: i32 = 11;
        pub const SQLITE_TESTCTRL_ASSERT: i32 = 12;
        pub const SQLITE_TESTCTRL_ALWAYS: i32 = 13;
        pub const SQLITE_TESTCTRL_RESERVE: i32 = 14;
        pub const SQLITE_TESTCTRL_JSON_SELFCHECK: i32 = 14;
        pub const SQLITE_TESTCTRL_OPTIMIZATIONS: i32 = 15;
        pub const SQLITE_TESTCTRL_ISKEYWORD: i32 = 16;
        pub const SQLITE_TESTCTRL_SCRATCHMALLOC: i32 = 17;
        pub const SQLITE_TESTCTRL_INTERNAL_FUNCTIONS: i32 = 17;
        pub const SQLITE_TESTCTRL_LOCALTIME_FAULT: i32 = 18;
        pub const SQLITE_TESTCTRL_EXPLAIN_STMT: i32 = 19;
        pub const SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD: i32 = 19;
        pub const SQLITE_TESTCTRL_NEVER_CORRUPT: i32 = 20;
        pub const SQLITE_TESTCTRL_VDBE_COVERAGE: i32 = 21;
        pub const SQLITE_TESTCTRL_BYTEORDER: i32 = 22;
        pub const SQLITE_TESTCTRL_ISINIT: i32 = 23;
        pub const SQLITE_TESTCTRL_SORTER_MMAP: i32 = 24;
        pub const SQLITE_TESTCTRL_IMPOSTER: i32 = 25;
        pub const SQLITE_TESTCTRL_PARSER_COVERAGE: i32 = 26;
        pub const SQLITE_TESTCTRL_RESULT_INTREAL: i32 = 27;
        pub const SQLITE_TESTCTRL_PRNG_SEED: i32 = 28;
        pub const SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS: i32 = 29;
        pub const SQLITE_TESTCTRL_SEEK_COUNT: i32 = 30;
        pub const SQLITE_TESTCTRL_TRACEFLAGS: i32 = 31;
        pub const SQLITE_TESTCTRL_TUNE: i32 = 32;
        pub const SQLITE_TESTCTRL_LOGEST: i32 = 33;
        pub const SQLITE_TESTCTRL_USELONGDOUBLE: i32 = 34;
        pub const SQLITE_TESTCTRL_LAST: i32 = 34;
        pub const SQLITE_STATUS_MEMORY_USED: i32 = 0;
        pub const SQLITE_STATUS_PAGECACHE_USED: i32 = 1;
        pub const SQLITE_STATUS_PAGECACHE_OVERFLOW: i32 = 2;
        pub const SQLITE_STATUS_SCRATCH_USED: i32 = 3;
        pub const SQLITE_STATUS_SCRATCH_OVERFLOW: i32 = 4;
        pub const SQLITE_STATUS_MALLOC_SIZE: i32 = 5;
        pub const SQLITE_STATUS_PARSER_STACK: i32 = 6;
        pub const SQLITE_STATUS_PAGECACHE_SIZE: i32 = 7;
        pub const SQLITE_STATUS_SCRATCH_SIZE: i32 = 8;
        pub const SQLITE_STATUS_MALLOC_COUNT: i32 = 9;
        pub const SQLITE_DBSTATUS_LOOKASIDE_USED: i32 = 0;
        pub const SQLITE_DBSTATUS_CACHE_USED: i32 = 1;
        pub const SQLITE_DBSTATUS_SCHEMA_USED: i32 = 2;
        pub const SQLITE_DBSTATUS_STMT_USED: i32 = 3;
        pub const SQLITE_DBSTATUS_LOOKASIDE_HIT: i32 = 4;
        pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE: i32 = 5;
        pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL: i32 = 6;
        pub const SQLITE_DBSTATUS_CACHE_HIT: i32 = 7;
        pub const SQLITE_DBSTATUS_CACHE_MISS: i32 = 8;
        pub const SQLITE_DBSTATUS_CACHE_WRITE: i32 = 9;
        pub const SQLITE_DBSTATUS_DEFERRED_FKS: i32 = 10;
        pub const SQLITE_DBSTATUS_CACHE_USED_SHARED: i32 = 11;
        pub const SQLITE_DBSTATUS_CACHE_SPILL: i32 = 12;
        pub const SQLITE_DBSTATUS_MAX: i32 = 12;
        pub const SQLITE_STMTSTATUS_FULLSCAN_STEP: i32 = 1;
        pub const SQLITE_STMTSTATUS_SORT: i32 = 2;
        pub const SQLITE_STMTSTATUS_AUTOINDEX: i32 = 3;
        pub const SQLITE_STMTSTATUS_VM_STEP: i32 = 4;
        pub const SQLITE_STMTSTATUS_REPREPARE: i32 = 5;
        pub const SQLITE_STMTSTATUS_RUN: i32 = 6;
        pub const SQLITE_STMTSTATUS_FILTER_MISS: i32 = 7;
        pub const SQLITE_STMTSTATUS_FILTER_HIT: i32 = 8;
        pub const SQLITE_STMTSTATUS_MEMUSED: i32 = 99;
        pub const SQLITE_CHECKPOINT_PASSIVE: i32 = 0;
        pub const SQLITE_CHECKPOINT_FULL: i32 = 1;
        pub const SQLITE_CHECKPOINT_RESTART: i32 = 2;
        pub const SQLITE_CHECKPOINT_TRUNCATE: i32 = 3;
        pub const SQLITE_VTAB_CONSTRAINT_SUPPORT: i32 = 1;
        pub const SQLITE_VTAB_INNOCUOUS: i32 = 2;
        pub const SQLITE_VTAB_DIRECTONLY: i32 = 3;
        pub const SQLITE_VTAB_USES_ALL_SCHEMAS: i32 = 4;
        pub const SQLITE_ROLLBACK: i32 = 1;
        pub const SQLITE_FAIL: i32 = 3;
        pub const SQLITE_REPLACE: i32 = 5;
        pub const SQLITE_SCANSTAT_NLOOP: i32 = 0;
        pub const SQLITE_SCANSTAT_NVISIT: i32 = 1;
        pub const SQLITE_SCANSTAT_EST: i32 = 2;
        pub const SQLITE_SCANSTAT_NAME: i32 = 3;
        pub const SQLITE_SCANSTAT_EXPLAIN: i32 = 4;
        pub const SQLITE_SCANSTAT_SELECTID: i32 = 5;
        pub const SQLITE_SCANSTAT_PARENTID: i32 = 6;
        pub const SQLITE_SCANSTAT_NCYCLE: i32 = 7;
        pub const SQLITE_SCANSTAT_COMPLEX: i32 = 1;
        pub const SQLITE_SERIALIZE_NOCOPY: ::std::os::raw::c_uint = 1;
        pub const SQLITE_DESERIALIZE_FREEONCLOSE: ::std::os::raw::c_uint = 1;
        pub const SQLITE_DESERIALIZE_RESIZEABLE: ::std::os::raw::c_uint = 2;
        pub const SQLITE_DESERIALIZE_READONLY: ::std::os::raw::c_uint = 4;
        pub const NOT_WITHIN: i32 = 0;
        pub const PARTLY_WITHIN: i32 = 1;
        pub const FULLY_WITHIN: i32 = 2;
        pub const SQLITE_SESSION_OBJCONFIG_SIZE: i32 = 1;
        pub const SQLITE_SESSION_OBJCONFIG_ROWID: i32 = 2;
        pub const SQLITE_CHANGESETSTART_INVERT: i32 = 2;
        pub const SQLITE_CHANGESETAPPLY_NOSAVEPOINT: i32 = 1;
        pub const SQLITE_CHANGESETAPPLY_INVERT: i32 = 2;
        pub const SQLITE_CHANGESETAPPLY_IGNORENOOP: i32 = 4;
        pub const SQLITE_CHANGESETAPPLY_FKNOACTION: i32 = 8;
        pub const SQLITE_CHANGESET_DATA: i32 = 1;
        pub const SQLITE_CHANGESET_NOTFOUND: i32 = 2;
        pub const SQLITE_CHANGESET_CONFLICT: i32 = 3;
        pub const SQLITE_CHANGESET_CONSTRAINT: i32 = 4;
        pub const SQLITE_CHANGESET_FOREIGN_KEY: i32 = 5;
        pub const SQLITE_CHANGESET_OMIT: i32 = 0;
        pub const SQLITE_CHANGESET_REPLACE: i32 = 1;
        pub const SQLITE_CHANGESET_ABORT: i32 = 2;
        pub const SQLITE_SESSION_CONFIG_STRMSIZE: i32 = 1;
        pub const FTS5_TOKENIZE_QUERY: i32 = 1;
        pub const FTS5_TOKENIZE_PREFIX: i32 = 2;
        pub const FTS5_TOKENIZE_DOCUMENT: i32 = 4;
        pub const FTS5_TOKENIZE_AUX: i32 = 8;
        pub const FTS5_TOKEN_COLOCATED: i32 = 1;
        extern "C" {
            pub static sqlite3_version: [::std::os::raw::c_char; 0usize];
        }
        extern "C" {
            pub fn sqlite3_libversion() -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_sourceid() -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_libversion_number() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_compileoption_used(
                zOptName: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_compileoption_get(N: ::std::os::raw::c_int) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_threadsafe() -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3 {
            _unused: [u8; 0],
        }
        pub type sqlite_int64 = ::std::os::raw::c_longlong;
        pub type sqlite_uint64 = ::std::os::raw::c_ulonglong;
        pub type sqlite3_int64 = sqlite_int64;
        pub type sqlite3_uint64 = sqlite_uint64;
        extern "C" {
            pub fn sqlite3_close(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        pub type sqlite3_callback = ::std::option::Option<
            unsafe extern "C" fn(
                arg1: *mut ::std::os::raw::c_void,
                arg2: ::std::os::raw::c_int,
                arg3: *mut *mut ::std::os::raw::c_char,
                arg4: *mut *mut ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int,
        >;
        extern "C" {
            pub fn sqlite3_exec(
                arg1: *mut sqlite3,
                sql: *const ::std::os::raw::c_char,
                callback: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut *mut ::std::os::raw::c_char,
                        arg4: *mut *mut ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                arg2: *mut ::std::os::raw::c_void,
                errmsg: *mut *mut ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_file {
            pub pMethods: *const sqlite3_io_methods,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_io_methods {
            pub iVersion: ::std::os::raw::c_int,
            pub xClose: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::std::os::raw::c_int,
            >,
            pub xRead: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    arg2: *mut ::std::os::raw::c_void,
                    iAmt: ::std::os::raw::c_int,
                    iOfst: sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xWrite: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    arg2: *const ::std::os::raw::c_void,
                    iAmt: ::std::os::raw::c_int,
                    iOfst: sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xTruncate: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_file, size: sqlite3_int64) -> ::std::os::raw::c_int,
            >,
            pub xSync: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    flags: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xFileSize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    pSize: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xLock: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    arg2: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xUnlock: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    arg2: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xCheckReservedLock: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    pResOut: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xFileControl: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    op: ::std::os::raw::c_int,
                    pArg: *mut ::std::os::raw::c_void,
                ) -> ::std::os::raw::c_int,
            >,
            pub xSectorSize: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::std::os::raw::c_int,
            >,
            pub xDeviceCharacteristics: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::std::os::raw::c_int,
            >,
            pub xShmMap: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    iPg: ::std::os::raw::c_int,
                    pgsz: ::std::os::raw::c_int,
                    arg2: ::std::os::raw::c_int,
                    arg3: *mut *mut ::std::os::raw::c_void,
                ) -> ::std::os::raw::c_int,
            >,
            pub xShmLock: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    offset: ::std::os::raw::c_int,
                    n: ::std::os::raw::c_int,
                    flags: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xShmBarrier: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file)>,
            pub xShmUnmap: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    deleteFlag: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xFetch: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    iOfst: sqlite3_int64,
                    iAmt: ::std::os::raw::c_int,
                    pp: *mut *mut ::std::os::raw::c_void,
                ) -> ::std::os::raw::c_int,
            >,
            pub xUnfetch: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_file,
                    iOfst: sqlite3_int64,
                    p: *mut ::std::os::raw::c_void,
                ) -> ::std::os::raw::c_int,
            >,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_mutex {
            _unused: [u8; 0],
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_api_routines {
            _unused: [u8; 0],
        }
        pub type sqlite3_filename = *const ::std::os::raw::c_char;
        pub type sqlite3_syscall_ptr = ::std::option::Option<unsafe extern "C" fn()>;
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_vfs {
            pub iVersion: ::std::os::raw::c_int,
            pub szOsFile: ::std::os::raw::c_int,
            pub mxPathname: ::std::os::raw::c_int,
            pub pNext: *mut sqlite3_vfs,
            pub zName: *const ::std::os::raw::c_char,
            pub pAppData: *mut ::std::os::raw::c_void,
            pub xOpen: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: sqlite3_filename,
                    arg2: *mut sqlite3_file,
                    flags: ::std::os::raw::c_int,
                    pOutFlags: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xDelete: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                    syncDir: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xAccess: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                    flags: ::std::os::raw::c_int,
                    pResOut: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xFullPathname: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                    nOut: ::std::os::raw::c_int,
                    zOut: *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xDlOpen: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zFilename: *const ::std::os::raw::c_char,
                ) -> *mut ::std::os::raw::c_void,
            >,
            pub xDlError: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    nByte: ::std::os::raw::c_int,
                    zErrMsg: *mut ::std::os::raw::c_char,
                ),
            >,
            pub xDlSym: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    arg2: *mut ::std::os::raw::c_void,
                    zSymbol: *const ::std::os::raw::c_char,
                ) -> ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_vfs,
                        arg2: *mut ::std::os::raw::c_void,
                        zSymbol: *const ::std::os::raw::c_char,
                    ),
                >,
            >,
            pub xDlClose: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut ::std::os::raw::c_void),
            >,
            pub xRandomness: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    nByte: ::std::os::raw::c_int,
                    zOut: *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xSleep: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    microseconds: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xCurrentTime: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut f64) -> ::std::os::raw::c_int,
            >,
            pub xGetLastError: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    arg2: ::std::os::raw::c_int,
                    arg3: *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xCurrentTimeInt64: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    arg2: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xSetSystemCall: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                    arg2: sqlite3_syscall_ptr,
                ) -> ::std::os::raw::c_int,
            >,
            pub xGetSystemCall: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                ) -> sqlite3_syscall_ptr,
            >,
            pub xNextSystemCall: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vfs,
                    zName: *const ::std::os::raw::c_char,
                ) -> *const ::std::os::raw::c_char,
            >,
        }
        extern "C" {
            pub fn sqlite3_initialize() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_shutdown() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_os_init() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_os_end() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_config(arg1: ::std::os::raw::c_int, ...) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_db_config(
                arg1: *mut sqlite3,
                op: ::std::os::raw::c_int,
                ...
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_mem_methods {
            pub xMalloc: ::std::option::Option<
                unsafe extern "C" fn(arg1: ::std::os::raw::c_int) -> *mut ::std::os::raw::c_void,
            >,
            pub xFree: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            pub xRealloc: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut ::std::os::raw::c_void,
                    arg2: ::std::os::raw::c_int,
                ) -> *mut ::std::os::raw::c_void,
            >,
            pub xSize: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
            >,
            pub xRoundup: ::std::option::Option<
                unsafe extern "C" fn(arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int,
            >,
            pub xInit: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
            >,
            pub xShutdown: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            pub pAppData: *mut ::std::os::raw::c_void,
        }
        extern "C" {
            pub fn sqlite3_extended_result_codes(
                arg1: *mut sqlite3,
                onoff: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_last_insert_rowid(arg1: *mut sqlite3) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_set_last_insert_rowid(arg1: *mut sqlite3, arg2: sqlite3_int64);
        }
        extern "C" {
            pub fn sqlite3_changes(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_changes64(arg1: *mut sqlite3) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_total_changes(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_total_changes64(arg1: *mut sqlite3) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_interrupt(arg1: *mut sqlite3);
        }
        extern "C" {
            pub fn sqlite3_is_interrupted(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_complete(sql: *const ::std::os::raw::c_char) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_busy_handler(
                arg1: *mut sqlite3,
                arg2: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                arg3: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_busy_timeout(
                arg1: *mut sqlite3,
                ms: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_get_table(
                db: *mut sqlite3,
                zSql: *const ::std::os::raw::c_char,
                pazResult: *mut *mut *mut ::std::os::raw::c_char,
                pnRow: *mut ::std::os::raw::c_int,
                pnColumn: *mut ::std::os::raw::c_int,
                pzErrmsg: *mut *mut ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_free_table(result: *mut *mut ::std::os::raw::c_char);
        }
        extern "C" {
            pub fn sqlite3_mprintf(arg1: *const ::std::os::raw::c_char, ...)
                -> *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_snprintf(
                arg1: ::std::os::raw::c_int,
                arg2: *mut ::std::os::raw::c_char,
                arg3: *const ::std::os::raw::c_char,
                ...
            ) -> *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_malloc(arg1: ::std::os::raw::c_int) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_malloc64(arg1: sqlite3_uint64) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_realloc(
                arg1: *mut ::std::os::raw::c_void,
                arg2: ::std::os::raw::c_int,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_realloc64(
                arg1: *mut ::std::os::raw::c_void,
                arg2: sqlite3_uint64,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_free(arg1: *mut ::std::os::raw::c_void);
        }
        extern "C" {
            pub fn sqlite3_msize(arg1: *mut ::std::os::raw::c_void) -> sqlite3_uint64;
        }
        extern "C" {
            pub fn sqlite3_memory_used() -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_memory_highwater(resetFlag: ::std::os::raw::c_int) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_randomness(N: ::std::os::raw::c_int, P: *mut ::std::os::raw::c_void);
        }
        extern "C" {
            pub fn sqlite3_set_authorizer(
                arg1: *mut sqlite3,
                xAuth: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: ::std::os::raw::c_int,
                        arg3: *const ::std::os::raw::c_char,
                        arg4: *const ::std::os::raw::c_char,
                        arg5: *const ::std::os::raw::c_char,
                        arg6: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                pUserData: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_trace(
                arg1: *mut sqlite3,
                xTrace: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: *const ::std::os::raw::c_char,
                    ),
                >,
                arg2: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_profile(
                arg1: *mut sqlite3,
                xProfile: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: *const ::std::os::raw::c_char,
                        arg3: sqlite3_uint64,
                    ),
                >,
                arg2: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_trace_v2(
                arg1: *mut sqlite3,
                uMask: ::std::os::raw::c_uint,
                xCallback: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: ::std::os::raw::c_uint,
                        arg2: *mut ::std::os::raw::c_void,
                        arg3: *mut ::std::os::raw::c_void,
                        arg4: *mut ::std::os::raw::c_void,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_progress_handler(
                arg1: *mut sqlite3,
                arg2: ::std::os::raw::c_int,
                arg3: ::std::option::Option<
                    unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
                >,
                arg4: *mut ::std::os::raw::c_void,
            );
        }
        extern "C" {
            pub fn sqlite3_open(
                filename: *const ::std::os::raw::c_char,
                ppDb: *mut *mut sqlite3,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_open_v2(
                filename: *const ::std::os::raw::c_char,
                ppDb: *mut *mut sqlite3,
                flags: ::std::os::raw::c_int,
                zVfs: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_uri_parameter(
                z: sqlite3_filename,
                zParam: *const ::std::os::raw::c_char,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_uri_boolean(
                z: sqlite3_filename,
                zParam: *const ::std::os::raw::c_char,
                bDefault: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_uri_int64(
                arg1: sqlite3_filename,
                arg2: *const ::std::os::raw::c_char,
                arg3: sqlite3_int64,
            ) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_uri_key(
                z: sqlite3_filename,
                N: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_filename_database(arg1: sqlite3_filename) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_filename_journal(arg1: sqlite3_filename) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_filename_wal(arg1: sqlite3_filename) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_database_file_object(arg1: *const ::std::os::raw::c_char) -> *mut sqlite3_file;
        }
        extern "C" {
            pub fn sqlite3_create_filename(
                zDatabase: *const ::std::os::raw::c_char,
                zJournal: *const ::std::os::raw::c_char,
                zWal: *const ::std::os::raw::c_char,
                nParam: ::std::os::raw::c_int,
                azParam: *mut *const ::std::os::raw::c_char,
            ) -> sqlite3_filename;
        }
        extern "C" {
            pub fn sqlite3_free_filename(arg1: sqlite3_filename);
        }
        extern "C" {
            pub fn sqlite3_errcode(db: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_extended_errcode(db: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_errmsg(arg1: *mut sqlite3) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_errstr(arg1: ::std::os::raw::c_int) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_error_offset(db: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_stmt {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3_limit(
                arg1: *mut sqlite3,
                id: ::std::os::raw::c_int,
                newVal: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_prepare_v2(
                db: *mut sqlite3,
                zSql: *const ::std::os::raw::c_char,
                nByte: ::std::os::raw::c_int,
                ppStmt: *mut *mut sqlite3_stmt,
                pzTail: *mut *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_prepare_v3(
                db: *mut sqlite3,
                zSql: *const ::std::os::raw::c_char,
                nByte: ::std::os::raw::c_int,
                prepFlags: ::std::os::raw::c_uint,
                ppStmt: *mut *mut sqlite3_stmt,
                pzTail: *mut *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_sql(pStmt: *mut sqlite3_stmt) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_expanded_sql(pStmt: *mut sqlite3_stmt) -> *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_stmt_readonly(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_isexplain(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_explain(
                pStmt: *mut sqlite3_stmt,
                eMode: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_busy(arg1: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_value {
            _unused: [u8; 0],
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_context {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3_bind_blob(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *const ::std::os::raw::c_void,
                n: ::std::os::raw::c_int,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_blob64(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *const ::std::os::raw::c_void,
                arg4: sqlite3_uint64,
                arg5: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_double(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: f64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_int(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_int64(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: sqlite3_int64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_null(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_text(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *const ::std::os::raw::c_char,
                arg4: ::std::os::raw::c_int,
                arg5: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_text64(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *const ::std::os::raw::c_char,
                arg4: sqlite3_uint64,
                arg5: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                encoding: ::std::os::raw::c_uchar,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_value(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *const sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_pointer(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: *mut ::std::os::raw::c_void,
                arg4: *const ::std::os::raw::c_char,
                arg5: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_zeroblob(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                n: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_zeroblob64(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
                arg3: sqlite3_uint64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_parameter_count(arg1: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_bind_parameter_name(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_bind_parameter_index(
                arg1: *mut sqlite3_stmt,
                zName: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_clear_bindings(arg1: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_column_count(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_column_name(
                arg1: *mut sqlite3_stmt,
                N: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_column_database_name(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_column_table_name(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_column_origin_name(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_column_decltype(
                arg1: *mut sqlite3_stmt,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_step(arg1: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_data_count(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_column_blob(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_column_double(arg1: *mut sqlite3_stmt, iCol: ::std::os::raw::c_int) -> f64;
        }
        extern "C" {
            pub fn sqlite3_column_int(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_column_int64(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_column_text(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_uchar;
        }
        extern "C" {
            pub fn sqlite3_column_value(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> *mut sqlite3_value;
        }
        extern "C" {
            pub fn sqlite3_column_bytes(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_column_type(
                arg1: *mut sqlite3_stmt,
                iCol: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_finalize(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_reset(pStmt: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_create_function_v2(
                db: *mut sqlite3,
                zFunctionName: *const ::std::os::raw::c_char,
                nArg: ::std::os::raw::c_int,
                eTextRep: ::std::os::raw::c_int,
                pApp: *mut ::std::os::raw::c_void,
                xFunc: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_context,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut *mut sqlite3_value,
                    ),
                >,
                xStep: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_context,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut *mut sqlite3_value,
                    ),
                >,
                xFinal: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_context)>,
                xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_create_window_function(
                db: *mut sqlite3,
                zFunctionName: *const ::std::os::raw::c_char,
                nArg: ::std::os::raw::c_int,
                eTextRep: ::std::os::raw::c_int,
                pApp: *mut ::std::os::raw::c_void,
                xStep: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_context,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut *mut sqlite3_value,
                    ),
                >,
                xFinal: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_context)>,
                xValue: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_context)>,
                xInverse: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_context,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut *mut sqlite3_value,
                    ),
                >,
                xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_aggregate_count(arg1: *mut sqlite3_context) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_expired(arg1: *mut sqlite3_stmt) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_transfer_bindings(
                arg1: *mut sqlite3_stmt,
                arg2: *mut sqlite3_stmt,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_global_recover() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_thread_cleanup();
        }
        extern "C" {
            pub fn sqlite3_memory_alarm(
                arg1: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: sqlite3_int64,
                        arg3: ::std::os::raw::c_int,
                    ),
                >,
                arg2: *mut ::std::os::raw::c_void,
                arg3: sqlite3_int64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_blob(arg1: *mut sqlite3_value) -> *const ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_value_double(arg1: *mut sqlite3_value) -> f64;
        }
        extern "C" {
            pub fn sqlite3_value_int(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_int64(arg1: *mut sqlite3_value) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_value_pointer(
                arg1: *mut sqlite3_value,
                arg2: *const ::std::os::raw::c_char,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_value_text(arg1: *mut sqlite3_value) -> *const ::std::os::raw::c_uchar;
        }
        extern "C" {
            pub fn sqlite3_value_bytes(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_type(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_numeric_type(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_nochange(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_frombind(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_encoding(arg1: *mut sqlite3_value) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_value_subtype(arg1: *mut sqlite3_value) -> ::std::os::raw::c_uint;
        }
        extern "C" {
            pub fn sqlite3_value_dup(arg1: *const sqlite3_value) -> *mut sqlite3_value;
        }
        extern "C" {
            pub fn sqlite3_value_free(arg1: *mut sqlite3_value);
        }
        extern "C" {
            pub fn sqlite3_aggregate_context(
                arg1: *mut sqlite3_context,
                nBytes: ::std::os::raw::c_int,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_user_data(arg1: *mut sqlite3_context) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_context_db_handle(arg1: *mut sqlite3_context) -> *mut sqlite3;
        }
        extern "C" {
            pub fn sqlite3_get_auxdata(
                arg1: *mut sqlite3_context,
                N: ::std::os::raw::c_int,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_set_auxdata(
                arg1: *mut sqlite3_context,
                N: ::std::os::raw::c_int,
                arg2: *mut ::std::os::raw::c_void,
                arg3: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            );
        }
        extern "C" {
            pub fn sqlite3_get_clientdata(
                arg1: *mut sqlite3,
                arg2: *const ::std::os::raw::c_char,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_set_clientdata(
                arg1: *mut sqlite3,
                arg2: *const ::std::os::raw::c_char,
                arg3: *mut ::std::os::raw::c_void,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        pub type sqlite3_destructor_type =
            ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>;
        extern "C" {
            pub fn sqlite3_result_blob(
                arg1: *mut sqlite3_context,
                arg2: *const ::std::os::raw::c_void,
                arg3: ::std::os::raw::c_int,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            );
        }
        extern "C" {
            pub fn sqlite3_result_blob64(
                arg1: *mut sqlite3_context,
                arg2: *const ::std::os::raw::c_void,
                arg3: sqlite3_uint64,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            );
        }
        extern "C" {
            pub fn sqlite3_result_double(arg1: *mut sqlite3_context, arg2: f64);
        }
        extern "C" {
            pub fn sqlite3_result_error(
                arg1: *mut sqlite3_context,
                arg2: *const ::std::os::raw::c_char,
                arg3: ::std::os::raw::c_int,
            );
        }
        extern "C" {
            pub fn sqlite3_result_error_toobig(arg1: *mut sqlite3_context);
        }
        extern "C" {
            pub fn sqlite3_result_error_nomem(arg1: *mut sqlite3_context);
        }
        extern "C" {
            pub fn sqlite3_result_error_code(arg1: *mut sqlite3_context, arg2: ::std::os::raw::c_int);
        }
        extern "C" {
            pub fn sqlite3_result_int(arg1: *mut sqlite3_context, arg2: ::std::os::raw::c_int);
        }
        extern "C" {
            pub fn sqlite3_result_int64(arg1: *mut sqlite3_context, arg2: sqlite3_int64);
        }
        extern "C" {
            pub fn sqlite3_result_null(arg1: *mut sqlite3_context);
        }
        extern "C" {
            pub fn sqlite3_result_text(
                arg1: *mut sqlite3_context,
                arg2: *const ::std::os::raw::c_char,
                arg3: ::std::os::raw::c_int,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            );
        }
        extern "C" {
            pub fn sqlite3_result_text64(
                arg1: *mut sqlite3_context,
                arg2: *const ::std::os::raw::c_char,
                arg3: sqlite3_uint64,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                encoding: ::std::os::raw::c_uchar,
            );
        }
        extern "C" {
            pub fn sqlite3_result_value(arg1: *mut sqlite3_context, arg2: *mut sqlite3_value);
        }
        extern "C" {
            pub fn sqlite3_result_pointer(
                arg1: *mut sqlite3_context,
                arg2: *mut ::std::os::raw::c_void,
                arg3: *const ::std::os::raw::c_char,
                arg4: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            );
        }
        extern "C" {
            pub fn sqlite3_result_zeroblob(arg1: *mut sqlite3_context, n: ::std::os::raw::c_int);
        }
        extern "C" {
            pub fn sqlite3_result_zeroblob64(
                arg1: *mut sqlite3_context,
                n: sqlite3_uint64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_result_subtype(arg1: *mut sqlite3_context, arg2: ::std::os::raw::c_uint);
        }
        extern "C" {
            pub fn sqlite3_create_collation_v2(
                arg1: *mut sqlite3,
                zName: *const ::std::os::raw::c_char,
                eTextRep: ::std::os::raw::c_int,
                pArg: *mut ::std::os::raw::c_void,
                xCompare: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: ::std::os::raw::c_int,
                        arg3: *const ::std::os::raw::c_void,
                        arg4: ::std::os::raw::c_int,
                        arg5: *const ::std::os::raw::c_void,
                    ) -> ::std::os::raw::c_int,
                >,
                xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_collation_needed(
                arg1: *mut sqlite3,
                arg2: *mut ::std::os::raw::c_void,
                arg3: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: *mut sqlite3,
                        eTextRep: ::std::os::raw::c_int,
                        arg3: *const ::std::os::raw::c_char,
                    ),
                >,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_sleep(arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub static mut sqlite3_temp_directory: *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub static mut sqlite3_data_directory: *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_win32_set_directory(
                type_: ::std::os::raw::c_ulong,
                zValue: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_win32_set_directory8(
                type_: ::std::os::raw::c_ulong,
                zValue: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_get_autocommit(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_db_handle(arg1: *mut sqlite3_stmt) -> *mut sqlite3;
        }
        extern "C" {
            pub fn sqlite3_db_name(
                db: *mut sqlite3,
                N: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_db_filename(
                db: *mut sqlite3,
                zDbName: *const ::std::os::raw::c_char,
            ) -> sqlite3_filename;
        }
        extern "C" {
            pub fn sqlite3_db_readonly(
                db: *mut sqlite3,
                zDbName: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_txn_state(
                arg1: *mut sqlite3,
                zSchema: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_next_stmt(pDb: *mut sqlite3, pStmt: *mut sqlite3_stmt) -> *mut sqlite3_stmt;
        }
        extern "C" {
            pub fn sqlite3_commit_hook(
                arg1: *mut sqlite3,
                arg2: ::std::option::Option<
                    unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
                >,
                arg3: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_rollback_hook(
                arg1: *mut sqlite3,
                arg2: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                arg3: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_autovacuum_pages(
                db: *mut sqlite3,
                arg1: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: *const ::std::os::raw::c_char,
                        arg3: ::std::os::raw::c_uint,
                        arg4: ::std::os::raw::c_uint,
                        arg5: ::std::os::raw::c_uint,
                    ) -> ::std::os::raw::c_uint,
                >,
                arg2: *mut ::std::os::raw::c_void,
                arg3: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_update_hook(
                arg1: *mut sqlite3,
                arg2: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: ::std::os::raw::c_int,
                        arg3: *const ::std::os::raw::c_char,
                        arg4: *const ::std::os::raw::c_char,
                        arg5: sqlite3_int64,
                    ),
                >,
                arg3: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_enable_shared_cache(arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_release_memory(arg1: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_db_release_memory(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_soft_heap_limit64(N: sqlite3_int64) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_hard_heap_limit64(N: sqlite3_int64) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3_soft_heap_limit(N: ::std::os::raw::c_int);
        }
        extern "C" {
            pub fn sqlite3_table_column_metadata(
                db: *mut sqlite3,
                zDbName: *const ::std::os::raw::c_char,
                zTableName: *const ::std::os::raw::c_char,
                zColumnName: *const ::std::os::raw::c_char,
                pzDataType: *mut *const ::std::os::raw::c_char,
                pzCollSeq: *mut *const ::std::os::raw::c_char,
                pNotNull: *mut ::std::os::raw::c_int,
                pPrimaryKey: *mut ::std::os::raw::c_int,
                pAutoinc: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_load_extension(
                db: *mut sqlite3,
                zFile: *const ::std::os::raw::c_char,
                zProc: *const ::std::os::raw::c_char,
                pzErrMsg: *mut *mut ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_enable_load_extension(
                db: *mut sqlite3,
                onoff: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_reset_auto_extension();
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_module {
            pub iVersion: ::std::os::raw::c_int,
            pub xCreate: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3,
                    pAux: *mut ::std::os::raw::c_void,
                    argc: ::std::os::raw::c_int,
                    argv: *const *const ::std::os::raw::c_char,
                    ppVTab: *mut *mut sqlite3_vtab,
                    arg2: *mut *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xConnect: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3,
                    pAux: *mut ::std::os::raw::c_void,
                    argc: ::std::os::raw::c_int,
                    argv: *const *const ::std::os::raw::c_char,
                    ppVTab: *mut *mut sqlite3_vtab,
                    arg2: *mut *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xBestIndex: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    arg1: *mut sqlite3_index_info,
                ) -> ::std::os::raw::c_int,
            >,
            pub xDisconnect: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xDestroy: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xOpen: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    ppCursor: *mut *mut sqlite3_vtab_cursor,
                ) -> ::std::os::raw::c_int,
            >,
            pub xClose: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::std::os::raw::c_int,
            >,
            pub xFilter: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vtab_cursor,
                    idxNum: ::std::os::raw::c_int,
                    idxStr: *const ::std::os::raw::c_char,
                    argc: ::std::os::raw::c_int,
                    argv: *mut *mut sqlite3_value,
                ) -> ::std::os::raw::c_int,
            >,
            pub xNext: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::std::os::raw::c_int,
            >,
            pub xEof: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::std::os::raw::c_int,
            >,
            pub xColumn: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vtab_cursor,
                    arg2: *mut sqlite3_context,
                    arg3: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xRowid: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vtab_cursor,
                    pRowid: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xUpdate: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_vtab,
                    arg2: ::std::os::raw::c_int,
                    arg3: *mut *mut sqlite3_value,
                    arg4: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xBegin: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xSync: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xCommit: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xRollback: ::std::option::Option<
                unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::std::os::raw::c_int,
            >,
            pub xFindFunction: ::std::option::Option<
                unsafe extern "C" fn(
                    pVtab: *mut sqlite3_vtab,
                    nArg: ::std::os::raw::c_int,
                    zName: *const ::std::os::raw::c_char,
                    pxFunc: *mut ::std::option::Option<
                        unsafe extern "C" fn(
                            arg1: *mut sqlite3_context,
                            arg2: ::std::os::raw::c_int,
                            arg3: *mut *mut sqlite3_value,
                        ),
                    >,
                    ppArg: *mut *mut ::std::os::raw::c_void,
                ) -> ::std::os::raw::c_int,
            >,
            pub xRename: ::std::option::Option<
                unsafe extern "C" fn(
                    pVtab: *mut sqlite3_vtab,
                    zNew: *const ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
            pub xSavepoint: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    arg1: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xRelease: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    arg1: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xRollbackTo: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    arg1: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xShadowName: ::std::option::Option<
                unsafe extern "C" fn(arg1: *const ::std::os::raw::c_char) -> ::std::os::raw::c_int,
            >,
            pub xIntegrity: ::std::option::Option<
                unsafe extern "C" fn(
                    pVTab: *mut sqlite3_vtab,
                    zSchema: *const ::std::os::raw::c_char,
                    zTabName: *const ::std::os::raw::c_char,
                    mFlags: ::std::os::raw::c_int,
                    pzErr: *mut *mut ::std::os::raw::c_char,
                ) -> ::std::os::raw::c_int,
            >,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_index_info {
            pub nConstraint: ::std::os::raw::c_int,
            pub aConstraint: *mut sqlite3_index_constraint,
            pub nOrderBy: ::std::os::raw::c_int,
            pub aOrderBy: *mut sqlite3_index_orderby,
            pub aConstraintUsage: *mut sqlite3_index_constraint_usage,
            pub idxNum: ::std::os::raw::c_int,
            pub idxStr: *mut ::std::os::raw::c_char,
            pub needToFreeIdxStr: ::std::os::raw::c_int,
            pub orderByConsumed: ::std::os::raw::c_int,
            pub estimatedCost: f64,
            pub estimatedRows: sqlite3_int64,
            pub idxFlags: ::std::os::raw::c_int,
            pub colUsed: sqlite3_uint64,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_index_constraint {
            pub iColumn: ::std::os::raw::c_int,
            pub op: ::std::os::raw::c_uchar,
            pub usable: ::std::os::raw::c_uchar,
            pub iTermOffset: ::std::os::raw::c_int,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_index_orderby {
            pub iColumn: ::std::os::raw::c_int,
            pub desc: ::std::os::raw::c_uchar,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_index_constraint_usage {
            pub argvIndex: ::std::os::raw::c_int,
            pub omit: ::std::os::raw::c_uchar,
        }
        extern "C" {
            pub fn sqlite3_create_module_v2(
                db: *mut sqlite3,
                zName: *const ::std::os::raw::c_char,
                p: *const sqlite3_module,
                pClientData: *mut ::std::os::raw::c_void,
                xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_drop_modules(
                db: *mut sqlite3,
                azKeep: *mut *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_vtab {
            pub pModule: *const sqlite3_module,
            pub nRef: ::std::os::raw::c_int,
            pub zErrMsg: *mut ::std::os::raw::c_char,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_vtab_cursor {
            pub pVtab: *mut sqlite3_vtab,
        }
        extern "C" {
            pub fn sqlite3_declare_vtab(
                arg1: *mut sqlite3,
                zSQL: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_overload_function(
                arg1: *mut sqlite3,
                zFuncName: *const ::std::os::raw::c_char,
                nArg: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_blob {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3_blob_open(
                arg1: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
                zTable: *const ::std::os::raw::c_char,
                zColumn: *const ::std::os::raw::c_char,
                iRow: sqlite3_int64,
                flags: ::std::os::raw::c_int,
                ppBlob: *mut *mut sqlite3_blob,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_blob_reopen(
                arg1: *mut sqlite3_blob,
                arg2: sqlite3_int64,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_blob_close(arg1: *mut sqlite3_blob) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_blob_bytes(arg1: *mut sqlite3_blob) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_blob_read(
                arg1: *mut sqlite3_blob,
                Z: *mut ::std::os::raw::c_void,
                N: ::std::os::raw::c_int,
                iOffset: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_blob_write(
                arg1: *mut sqlite3_blob,
                z: *const ::std::os::raw::c_void,
                n: ::std::os::raw::c_int,
                iOffset: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vfs_find(zVfsName: *const ::std::os::raw::c_char) -> *mut sqlite3_vfs;
        }
        extern "C" {
            pub fn sqlite3_vfs_register(
                arg1: *mut sqlite3_vfs,
                makeDflt: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vfs_unregister(arg1: *mut sqlite3_vfs) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_mutex_alloc(arg1: ::std::os::raw::c_int) -> *mut sqlite3_mutex;
        }
        extern "C" {
            pub fn sqlite3_mutex_free(arg1: *mut sqlite3_mutex);
        }
        extern "C" {
            pub fn sqlite3_mutex_enter(arg1: *mut sqlite3_mutex);
        }
        extern "C" {
            pub fn sqlite3_mutex_try(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_mutex_leave(arg1: *mut sqlite3_mutex);
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_mutex_methods {
            pub xMutexInit: ::std::option::Option<unsafe extern "C" fn() -> ::std::os::raw::c_int>,
            pub xMutexEnd: ::std::option::Option<unsafe extern "C" fn() -> ::std::os::raw::c_int>,
            pub xMutexAlloc: ::std::option::Option<
                unsafe extern "C" fn(arg1: ::std::os::raw::c_int) -> *mut sqlite3_mutex,
            >,
            pub xMutexFree: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
            pub xMutexEnter: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
            pub xMutexTry: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int,
            >,
            pub xMutexLeave: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
            pub xMutexHeld: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int,
            >,
            pub xMutexNotheld: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int,
            >,
        }
        extern "C" {
            pub fn sqlite3_mutex_held(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_mutex_notheld(arg1: *mut sqlite3_mutex) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_db_mutex(arg1: *mut sqlite3) -> *mut sqlite3_mutex;
        }
        extern "C" {
            pub fn sqlite3_file_control(
                arg1: *mut sqlite3,
                zDbName: *const ::std::os::raw::c_char,
                op: ::std::os::raw::c_int,
                arg2: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_test_control(op: ::std::os::raw::c_int, ...) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_keyword_count() -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_keyword_name(
                arg1: ::std::os::raw::c_int,
                arg2: *mut *const ::std::os::raw::c_char,
                arg3: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_keyword_check(
                arg1: *const ::std::os::raw::c_char,
                arg2: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_str {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3_str_new(arg1: *mut sqlite3) -> *mut sqlite3_str;
        }
        extern "C" {
            pub fn sqlite3_str_finish(arg1: *mut sqlite3_str) -> *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_str_appendf(arg1: *mut sqlite3_str, zFormat: *const ::std::os::raw::c_char, ...);
        }
        extern "C" {
            pub fn sqlite3_str_append(
                arg1: *mut sqlite3_str,
                zIn: *const ::std::os::raw::c_char,
                N: ::std::os::raw::c_int,
            );
        }
        extern "C" {
            pub fn sqlite3_str_appendall(arg1: *mut sqlite3_str, zIn: *const ::std::os::raw::c_char);
        }
        extern "C" {
            pub fn sqlite3_str_appendchar(
                arg1: *mut sqlite3_str,
                N: ::std::os::raw::c_int,
                C: ::std::os::raw::c_char,
            );
        }
        extern "C" {
            pub fn sqlite3_str_reset(arg1: *mut sqlite3_str);
        }
        extern "C" {
            pub fn sqlite3_str_errcode(arg1: *mut sqlite3_str) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_str_length(arg1: *mut sqlite3_str) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_str_value(arg1: *mut sqlite3_str) -> *mut ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_status(
                op: ::std::os::raw::c_int,
                pCurrent: *mut ::std::os::raw::c_int,
                pHighwater: *mut ::std::os::raw::c_int,
                resetFlag: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_status64(
                op: ::std::os::raw::c_int,
                pCurrent: *mut sqlite3_int64,
                pHighwater: *mut sqlite3_int64,
                resetFlag: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_db_status(
                arg1: *mut sqlite3,
                op: ::std::os::raw::c_int,
                pCur: *mut ::std::os::raw::c_int,
                pHiwtr: *mut ::std::os::raw::c_int,
                resetFlg: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_status(
                arg1: *mut sqlite3_stmt,
                op: ::std::os::raw::c_int,
                resetFlg: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_pcache {
            _unused: [u8; 0],
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_pcache_page {
            pub pBuf: *mut ::std::os::raw::c_void,
            pub pExtra: *mut ::std::os::raw::c_void,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_pcache_methods2 {
            pub iVersion: ::std::os::raw::c_int,
            pub pArg: *mut ::std::os::raw::c_void,
            pub xInit: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
            >,
            pub xShutdown: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            pub xCreate: ::std::option::Option<
                unsafe extern "C" fn(
                    szPage: ::std::os::raw::c_int,
                    szExtra: ::std::os::raw::c_int,
                    bPurgeable: ::std::os::raw::c_int,
                ) -> *mut sqlite3_pcache,
            >,
            pub xCachesize: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::std::os::raw::c_int),
            >,
            pub xPagecount: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::std::os::raw::c_int,
            >,
            pub xFetch: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    key: ::std::os::raw::c_uint,
                    createFlag: ::std::os::raw::c_int,
                ) -> *mut sqlite3_pcache_page,
            >,
            pub xUnpin: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    arg2: *mut sqlite3_pcache_page,
                    discard: ::std::os::raw::c_int,
                ),
            >,
            pub xRekey: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    arg2: *mut sqlite3_pcache_page,
                    oldKey: ::std::os::raw::c_uint,
                    newKey: ::std::os::raw::c_uint,
                ),
            >,
            pub xTruncate: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::std::os::raw::c_uint),
            >,
            pub xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
            pub xShrink: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_pcache_methods {
            pub pArg: *mut ::std::os::raw::c_void,
            pub xInit: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void) -> ::std::os::raw::c_int,
            >,
            pub xShutdown: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            pub xCreate: ::std::option::Option<
                unsafe extern "C" fn(
                    szPage: ::std::os::raw::c_int,
                    bPurgeable: ::std::os::raw::c_int,
                ) -> *mut sqlite3_pcache,
            >,
            pub xCachesize: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::std::os::raw::c_int),
            >,
            pub xPagecount: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::std::os::raw::c_int,
            >,
            pub xFetch: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    key: ::std::os::raw::c_uint,
                    createFlag: ::std::os::raw::c_int,
                ) -> *mut ::std::os::raw::c_void,
            >,
            pub xUnpin: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    arg2: *mut ::std::os::raw::c_void,
                    discard: ::std::os::raw::c_int,
                ),
            >,
            pub xRekey: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut sqlite3_pcache,
                    arg2: *mut ::std::os::raw::c_void,
                    oldKey: ::std::os::raw::c_uint,
                    newKey: ::std::os::raw::c_uint,
                ),
            >,
            pub xTruncate: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::std::os::raw::c_uint),
            >,
            pub xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_backup {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3_backup_init(
                pDest: *mut sqlite3,
                zDestName: *const ::std::os::raw::c_char,
                pSource: *mut sqlite3,
                zSourceName: *const ::std::os::raw::c_char,
            ) -> *mut sqlite3_backup;
        }
        extern "C" {
            pub fn sqlite3_backup_step(
                p: *mut sqlite3_backup,
                nPage: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_backup_finish(p: *mut sqlite3_backup) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_backup_remaining(p: *mut sqlite3_backup) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_backup_pagecount(p: *mut sqlite3_backup) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_unlock_notify(
                pBlocked: *mut sqlite3,
                xNotify: ::std::option::Option<
                    unsafe extern "C" fn(
                        apArg: *mut *mut ::std::os::raw::c_void,
                        nArg: ::std::os::raw::c_int,
                    ),
                >,
                pNotifyArg: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stricmp(
                arg1: *const ::std::os::raw::c_char,
                arg2: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_strnicmp(
                arg1: *const ::std::os::raw::c_char,
                arg2: *const ::std::os::raw::c_char,
                arg3: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_strglob(
                zGlob: *const ::std::os::raw::c_char,
                zStr: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_strlike(
                zGlob: *const ::std::os::raw::c_char,
                zStr: *const ::std::os::raw::c_char,
                cEsc: ::std::os::raw::c_uint,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_log(
                iErrCode: ::std::os::raw::c_int,
                zFormat: *const ::std::os::raw::c_char,
                ...
            );
        }
        extern "C" {
            pub fn sqlite3_wal_hook(
                arg1: *mut sqlite3,
                arg2: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut ::std::os::raw::c_void,
                        arg2: *mut sqlite3,
                        arg3: *const ::std::os::raw::c_char,
                        arg4: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                arg3: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_wal_autocheckpoint(
                db: *mut sqlite3,
                N: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_wal_checkpoint(
                db: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_wal_checkpoint_v2(
                db: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
                eMode: ::std::os::raw::c_int,
                pnLog: *mut ::std::os::raw::c_int,
                pnCkpt: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_config(
                arg1: *mut sqlite3,
                op: ::std::os::raw::c_int,
                ...
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_on_conflict(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_nochange(arg1: *mut sqlite3_context) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_collation(
                arg1: *mut sqlite3_index_info,
                arg2: ::std::os::raw::c_int,
            ) -> *const ::std::os::raw::c_char;
        }
        extern "C" {
            pub fn sqlite3_vtab_distinct(arg1: *mut sqlite3_index_info) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_in(
                arg1: *mut sqlite3_index_info,
                iCons: ::std::os::raw::c_int,
                bHandle: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_in_first(
                pVal: *mut sqlite3_value,
                ppOut: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_in_next(
                pVal: *mut sqlite3_value,
                ppOut: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_vtab_rhs_value(
                arg1: *mut sqlite3_index_info,
                arg2: ::std::os::raw::c_int,
                ppVal: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_scanstatus(
                pStmt: *mut sqlite3_stmt,
                idx: ::std::os::raw::c_int,
                iScanStatusOp: ::std::os::raw::c_int,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_scanstatus_v2(
                pStmt: *mut sqlite3_stmt,
                idx: ::std::os::raw::c_int,
                iScanStatusOp: ::std::os::raw::c_int,
                flags: ::std::os::raw::c_int,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_stmt_scanstatus_reset(arg1: *mut sqlite3_stmt);
        }
        extern "C" {
            pub fn sqlite3_db_cacheflush(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_preupdate_hook(
                db: *mut sqlite3,
                xPreUpdate: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        db: *mut sqlite3,
                        op: ::std::os::raw::c_int,
                        zDb: *const ::std::os::raw::c_char,
                        zName: *const ::std::os::raw::c_char,
                        iKey1: sqlite3_int64,
                        iKey2: sqlite3_int64,
                    ),
                >,
                arg1: *mut ::std::os::raw::c_void,
            ) -> *mut ::std::os::raw::c_void;
        }
        extern "C" {
            pub fn sqlite3_preupdate_old(
                arg1: *mut sqlite3,
                arg2: ::std::os::raw::c_int,
                arg3: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_preupdate_count(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_preupdate_depth(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_preupdate_new(
                arg1: *mut sqlite3,
                arg2: ::std::os::raw::c_int,
                arg3: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_preupdate_blobwrite(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_system_errno(arg1: *mut sqlite3) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_snapshot {
            pub hidden: [::std::os::raw::c_uchar; 48usize],
        }
        extern "C" {
            pub fn sqlite3_snapshot_get(
                db: *mut sqlite3,
                zSchema: *const ::std::os::raw::c_char,
                ppSnapshot: *mut *mut sqlite3_snapshot,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_snapshot_open(
                db: *mut sqlite3,
                zSchema: *const ::std::os::raw::c_char,
                pSnapshot: *mut sqlite3_snapshot,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_snapshot_free(arg1: *mut sqlite3_snapshot);
        }
        extern "C" {
            pub fn sqlite3_snapshot_cmp(
                p1: *mut sqlite3_snapshot,
                p2: *mut sqlite3_snapshot,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_snapshot_recover(
                db: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3_serialize(
                db: *mut sqlite3,
                zSchema: *const ::std::os::raw::c_char,
                piSize: *mut sqlite3_int64,
                mFlags: ::std::os::raw::c_uint,
            ) -> *mut ::std::os::raw::c_uchar;
        }
        extern "C" {
            pub fn sqlite3_deserialize(
                db: *mut sqlite3,
                zSchema: *const ::std::os::raw::c_char,
                pData: *mut ::std::os::raw::c_uchar,
                szDb: sqlite3_int64,
                szBuf: sqlite3_int64,
                mFlags: ::std::os::raw::c_uint,
            ) -> ::std::os::raw::c_int;
        }
        pub type sqlite3_rtree_dbl = f64;
        extern "C" {
            pub fn sqlite3_rtree_geometry_callback(
                db: *mut sqlite3,
                zGeom: *const ::std::os::raw::c_char,
                xGeom: ::std::option::Option<
                    unsafe extern "C" fn(
                        arg1: *mut sqlite3_rtree_geometry,
                        arg2: ::std::os::raw::c_int,
                        arg3: *mut sqlite3_rtree_dbl,
                        arg4: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pContext: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_rtree_geometry {
            pub pContext: *mut ::std::os::raw::c_void,
            pub nParam: ::std::os::raw::c_int,
            pub aParam: *mut sqlite3_rtree_dbl,
            pub pUser: *mut ::std::os::raw::c_void,
            pub xDelUser: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
        }
        extern "C" {
            pub fn sqlite3_rtree_query_callback(
                db: *mut sqlite3,
                zQueryFunc: *const ::std::os::raw::c_char,
                xQueryFunc: ::std::option::Option<
                    unsafe extern "C" fn(arg1: *mut sqlite3_rtree_query_info) -> ::std::os::raw::c_int,
                >,
                pContext: *mut ::std::os::raw::c_void,
                xDestructor: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_rtree_query_info {
            pub pContext: *mut ::std::os::raw::c_void,
            pub nParam: ::std::os::raw::c_int,
            pub aParam: *mut sqlite3_rtree_dbl,
            pub pUser: *mut ::std::os::raw::c_void,
            pub xDelUser: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
            pub aCoord: *mut sqlite3_rtree_dbl,
            pub anQueue: *mut ::std::os::raw::c_uint,
            pub nCoord: ::std::os::raw::c_int,
            pub iLevel: ::std::os::raw::c_int,
            pub mxLevel: ::std::os::raw::c_int,
            pub iRowid: sqlite3_int64,
            pub rParentScore: sqlite3_rtree_dbl,
            pub eParentWithin: ::std::os::raw::c_int,
            pub eWithin: ::std::os::raw::c_int,
            pub rScore: sqlite3_rtree_dbl,
            pub apSqlParam: *mut *mut sqlite3_value,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_session {
            _unused: [u8; 0],
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_changeset_iter {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3session_create(
                db: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
                ppSession: *mut *mut sqlite3_session,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_delete(pSession: *mut sqlite3_session);
        }
        extern "C" {
            pub fn sqlite3session_object_config(
                arg1: *mut sqlite3_session,
                op: ::std::os::raw::c_int,
                pArg: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_enable(
                pSession: *mut sqlite3_session,
                bEnable: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_indirect(
                pSession: *mut sqlite3_session,
                bIndirect: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_attach(
                pSession: *mut sqlite3_session,
                zTab: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_table_filter(
                pSession: *mut sqlite3_session,
                xFilter: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        zTab: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
            );
        }
        extern "C" {
            pub fn sqlite3session_changeset(
                pSession: *mut sqlite3_session,
                pnChangeset: *mut ::std::os::raw::c_int,
                ppChangeset: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_changeset_size(pSession: *mut sqlite3_session) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3session_diff(
                pSession: *mut sqlite3_session,
                zFromDb: *const ::std::os::raw::c_char,
                zTbl: *const ::std::os::raw::c_char,
                pzErrMsg: *mut *mut ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_patchset(
                pSession: *mut sqlite3_session,
                pnPatchset: *mut ::std::os::raw::c_int,
                ppPatchset: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_isempty(pSession: *mut sqlite3_session) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_memory_used(pSession: *mut sqlite3_session) -> sqlite3_int64;
        }
        extern "C" {
            pub fn sqlite3changeset_start(
                pp: *mut *mut sqlite3_changeset_iter,
                nChangeset: ::std::os::raw::c_int,
                pChangeset: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_start_v2(
                pp: *mut *mut sqlite3_changeset_iter,
                nChangeset: ::std::os::raw::c_int,
                pChangeset: *mut ::std::os::raw::c_void,
                flags: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_next(pIter: *mut sqlite3_changeset_iter) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_op(
                pIter: *mut sqlite3_changeset_iter,
                pzTab: *mut *const ::std::os::raw::c_char,
                pnCol: *mut ::std::os::raw::c_int,
                pOp: *mut ::std::os::raw::c_int,
                pbIndirect: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_pk(
                pIter: *mut sqlite3_changeset_iter,
                pabPK: *mut *mut ::std::os::raw::c_uchar,
                pnCol: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_old(
                pIter: *mut sqlite3_changeset_iter,
                iVal: ::std::os::raw::c_int,
                ppValue: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_new(
                pIter: *mut sqlite3_changeset_iter,
                iVal: ::std::os::raw::c_int,
                ppValue: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_conflict(
                pIter: *mut sqlite3_changeset_iter,
                iVal: ::std::os::raw::c_int,
                ppValue: *mut *mut sqlite3_value,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_fk_conflicts(
                pIter: *mut sqlite3_changeset_iter,
                pnOut: *mut ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_finalize(pIter: *mut sqlite3_changeset_iter) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_invert(
                nIn: ::std::os::raw::c_int,
                pIn: *const ::std::os::raw::c_void,
                pnOut: *mut ::std::os::raw::c_int,
                ppOut: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_concat(
                nA: ::std::os::raw::c_int,
                pA: *mut ::std::os::raw::c_void,
                nB: ::std::os::raw::c_int,
                pB: *mut ::std::os::raw::c_void,
                pnOut: *mut ::std::os::raw::c_int,
                ppOut: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_upgrade(
                db: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
                nIn: ::std::os::raw::c_int,
                pIn: *const ::std::os::raw::c_void,
                pnOut: *mut ::std::os::raw::c_int,
                ppOut: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_changegroup {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3changegroup_new(pp: *mut *mut sqlite3_changegroup) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_schema(
                arg1: *mut sqlite3_changegroup,
                arg2: *mut sqlite3,
                zDb: *const ::std::os::raw::c_char,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_add(
                arg1: *mut sqlite3_changegroup,
                nData: ::std::os::raw::c_int,
                pData: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_add_change(
                arg1: *mut sqlite3_changegroup,
                arg2: *mut sqlite3_changeset_iter,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_output(
                arg1: *mut sqlite3_changegroup,
                pnData: *mut ::std::os::raw::c_int,
                ppData: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_delete(arg1: *mut sqlite3_changegroup);
        }
        extern "C" {
            pub fn sqlite3changeset_apply(
                db: *mut sqlite3,
                nChangeset: ::std::os::raw::c_int,
                pChangeset: *mut ::std::os::raw::c_void,
                xFilter: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        zTab: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                xConflict: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        eConflict: ::std::os::raw::c_int,
                        p: *mut sqlite3_changeset_iter,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_apply_v2(
                db: *mut sqlite3,
                nChangeset: ::std::os::raw::c_int,
                pChangeset: *mut ::std::os::raw::c_void,
                xFilter: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        zTab: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                xConflict: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        eConflict: ::std::os::raw::c_int,
                        p: *mut sqlite3_changeset_iter,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
                ppRebase: *mut *mut ::std::os::raw::c_void,
                pnRebase: *mut ::std::os::raw::c_int,
                flags: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct sqlite3_rebaser {
            _unused: [u8; 0],
        }
        extern "C" {
            pub fn sqlite3rebaser_create(ppNew: *mut *mut sqlite3_rebaser) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3rebaser_configure(
                arg1: *mut sqlite3_rebaser,
                nRebase: ::std::os::raw::c_int,
                pRebase: *const ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3rebaser_rebase(
                arg1: *mut sqlite3_rebaser,
                nIn: ::std::os::raw::c_int,
                pIn: *const ::std::os::raw::c_void,
                pnOut: *mut ::std::os::raw::c_int,
                ppOut: *mut *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3rebaser_delete(p: *mut sqlite3_rebaser);
        }
        extern "C" {
            pub fn sqlite3changeset_apply_strm(
                db: *mut sqlite3,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
                xFilter: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        zTab: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                xConflict: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        eConflict: ::std::os::raw::c_int,
                        p: *mut sqlite3_changeset_iter,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_apply_v2_strm(
                db: *mut sqlite3,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
                xFilter: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        zTab: *const ::std::os::raw::c_char,
                    ) -> ::std::os::raw::c_int,
                >,
                xConflict: ::std::option::Option<
                    unsafe extern "C" fn(
                        pCtx: *mut ::std::os::raw::c_void,
                        eConflict: ::std::os::raw::c_int,
                        p: *mut sqlite3_changeset_iter,
                    ) -> ::std::os::raw::c_int,
                >,
                pCtx: *mut ::std::os::raw::c_void,
                ppRebase: *mut *mut ::std::os::raw::c_void,
                pnRebase: *mut ::std::os::raw::c_int,
                flags: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_concat_strm(
                xInputA: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pInA: *mut ::std::os::raw::c_void,
                xInputB: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pInB: *mut ::std::os::raw::c_void,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_invert_strm(
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_start_strm(
                pp: *mut *mut sqlite3_changeset_iter,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changeset_start_v2_strm(
                pp: *mut *mut sqlite3_changeset_iter,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
                flags: ::std::os::raw::c_int,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_changeset_strm(
                pSession: *mut sqlite3_session,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_patchset_strm(
                pSession: *mut sqlite3_session,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_add_strm(
                arg1: *mut sqlite3_changegroup,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3changegroup_output_strm(
                arg1: *mut sqlite3_changegroup,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3rebaser_rebase_strm(
                pRebaser: *mut sqlite3_rebaser,
                xInput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pIn: *mut ::std::os::raw::c_void,
                        pData: *mut ::std::os::raw::c_void,
                        pnData: *mut ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pIn: *mut ::std::os::raw::c_void,
                xOutput: ::std::option::Option<
                    unsafe extern "C" fn(
                        pOut: *mut ::std::os::raw::c_void,
                        pData: *const ::std::os::raw::c_void,
                        nData: ::std::os::raw::c_int,
                    ) -> ::std::os::raw::c_int,
                >,
                pOut: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        extern "C" {
            pub fn sqlite3session_config(
                op: ::std::os::raw::c_int,
                pArg: *mut ::std::os::raw::c_void,
            ) -> ::std::os::raw::c_int;
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct Fts5Context {
            _unused: [u8; 0],
        }
        pub type fts5_extension_function = ::std::option::Option<
            unsafe extern "C" fn(
                pApi: *const Fts5ExtensionApi,
                pFts: *mut Fts5Context,
                pCtx: *mut sqlite3_context,
                nVal: ::std::os::raw::c_int,
                apVal: *mut *mut sqlite3_value,
            ),
        >;
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct Fts5PhraseIter {
            pub a: *const ::std::os::raw::c_uchar,
            pub b: *const ::std::os::raw::c_uchar,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct Fts5ExtensionApi {
            pub iVersion: ::std::os::raw::c_int,
            pub xUserData: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut Fts5Context) -> *mut ::std::os::raw::c_void,
            >,
            pub xColumnCount: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::std::os::raw::c_int,
            >,
            pub xRowCount: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    pnRow: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xColumnTotalSize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iCol: ::std::os::raw::c_int,
                    pnToken: *mut sqlite3_int64,
                ) -> ::std::os::raw::c_int,
            >,
            pub xTokenize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    pText: *const ::std::os::raw::c_char,
                    nText: ::std::os::raw::c_int,
                    pCtx: *mut ::std::os::raw::c_void,
                    xToken: ::std::option::Option<
                        unsafe extern "C" fn(
                            arg1: *mut ::std::os::raw::c_void,
                            arg2: ::std::os::raw::c_int,
                            arg3: *const ::std::os::raw::c_char,
                            arg4: ::std::os::raw::c_int,
                            arg5: ::std::os::raw::c_int,
                            arg6: ::std::os::raw::c_int,
                        ) -> ::std::os::raw::c_int,
                    >,
                ) -> ::std::os::raw::c_int,
            >,
            pub xPhraseCount: ::std::option::Option<
                unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::std::os::raw::c_int,
            >,
            pub xPhraseSize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iPhrase: ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xInstCount: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    pnInst: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xInst: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iIdx: ::std::os::raw::c_int,
                    piPhrase: *mut ::std::os::raw::c_int,
                    piCol: *mut ::std::os::raw::c_int,
                    piOff: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xRowid:
                ::std::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> sqlite3_int64>,
            pub xColumnText: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iCol: ::std::os::raw::c_int,
                    pz: *mut *const ::std::os::raw::c_char,
                    pn: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xColumnSize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iCol: ::std::os::raw::c_int,
                    pnToken: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xQueryPhrase: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iPhrase: ::std::os::raw::c_int,
                    pUserData: *mut ::std::os::raw::c_void,
                    arg2: ::std::option::Option<
                        unsafe extern "C" fn(
                            arg1: *const Fts5ExtensionApi,
                            arg2: *mut Fts5Context,
                            arg3: *mut ::std::os::raw::c_void,
                        ) -> ::std::os::raw::c_int,
                    >,
                ) -> ::std::os::raw::c_int,
            >,
            pub xSetAuxdata: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    pAux: *mut ::std::os::raw::c_void,
                    xDelete: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                ) -> ::std::os::raw::c_int,
            >,
            pub xGetAuxdata: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    bClear: ::std::os::raw::c_int,
                ) -> *mut ::std::os::raw::c_void,
            >,
            pub xPhraseFirst: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iPhrase: ::std::os::raw::c_int,
                    arg2: *mut Fts5PhraseIter,
                    arg3: *mut ::std::os::raw::c_int,
                    arg4: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xPhraseNext: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    arg2: *mut Fts5PhraseIter,
                    piCol: *mut ::std::os::raw::c_int,
                    piOff: *mut ::std::os::raw::c_int,
                ),
            >,
            pub xPhraseFirstColumn: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iPhrase: ::std::os::raw::c_int,
                    arg2: *mut Fts5PhraseIter,
                    arg3: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xPhraseNextColumn: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    arg2: *mut Fts5PhraseIter,
                    piCol: *mut ::std::os::raw::c_int,
                ),
            >,
            pub xQueryToken: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iPhrase: ::std::os::raw::c_int,
                    iToken: ::std::os::raw::c_int,
                    ppToken: *mut *const ::std::os::raw::c_char,
                    pnToken: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
            pub xInstToken: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Context,
                    iIdx: ::std::os::raw::c_int,
                    iToken: ::std::os::raw::c_int,
                    arg2: *mut *const ::std::os::raw::c_char,
                    arg3: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int,
            >,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct Fts5Tokenizer {
            _unused: [u8; 0],
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct fts5_tokenizer {
            pub xCreate: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut ::std::os::raw::c_void,
                    azArg: *mut *const ::std::os::raw::c_char,
                    nArg: ::std::os::raw::c_int,
                    ppOut: *mut *mut Fts5Tokenizer,
                ) -> ::std::os::raw::c_int,
            >,
            pub xDelete: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Tokenizer)>,
            pub xTokenize: ::std::option::Option<
                unsafe extern "C" fn(
                    arg1: *mut Fts5Tokenizer,
                    pCtx: *mut ::std::os::raw::c_void,
                    flags: ::std::os::raw::c_int,
                    pText: *const ::std::os::raw::c_char,
                    nText: ::std::os::raw::c_int,
                    xToken: ::std::option::Option<
                        unsafe extern "C" fn(
                            pCtx: *mut ::std::os::raw::c_void,
                            tflags: ::std::os::raw::c_int,
                            pToken: *const ::std::os::raw::c_char,
                            nToken: ::std::os::raw::c_int,
                            iStart: ::std::os::raw::c_int,
                            iEnd: ::std::os::raw::c_int,
                        ) -> ::std::os::raw::c_int,
                    >,
                ) -> ::std::os::raw::c_int,
            >,
        }
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct fts5_api {
            pub iVersion: ::std::os::raw::c_int,
            pub xCreateTokenizer: ::std::option::Option<
                unsafe extern "C" fn(
                    pApi: *mut fts5_api,
                    zName: *const ::std::os::raw::c_char,
                    pUserData: *mut ::std::os::raw::c_void,
                    pTokenizer: *mut fts5_tokenizer,
                    xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                ) -> ::std::os::raw::c_int,
            >,
            pub xFindTokenizer: ::std::option::Option<
                unsafe extern "C" fn(
                    pApi: *mut fts5_api,
                    zName: *const ::std::os::raw::c_char,
                    ppUserData: *mut *mut ::std::os::raw::c_void,
                    pTokenizer: *mut fts5_tokenizer,
                ) -> ::std::os::raw::c_int,
            >,
            pub xCreateFunction: ::std::option::Option<
                unsafe extern "C" fn(
                    pApi: *mut fts5_api,
                    zName: *const ::std::os::raw::c_char,
                    pUserData: *mut ::std::os::raw::c_void,
                    xFunction: fts5_extension_function,
                    xDestroy: ::std::option::Option<unsafe extern "C" fn(arg1: *mut ::std::os::raw::c_void)>,
                ) -> ::std::os::raw::c_int,
            >,
        }
        
        #[must_use]
        pub fn SQLITE_STATIC() -> sqlite3_destructor_type {
            None
        }
        
        #[must_use]
        pub fn SQLITE_TRANSIENT() -> sqlite3_destructor_type {
            Some(unsafe { mem::transmute::<isize, unsafe extern "C" fn(*mut std::ffi::c_void)>(-1_isize) })
        }
        
        #[allow(clippy::all)]
        mod bindings {
            include!(concat!(env!("OUT_DIR"), "/bindgen.rs"));
        }
        pub use bindings::*;
        
        impl Default for sqlite3_vtab {
            fn default() -> Self {
                unsafe { mem::zeroed() }
            }
        }
        
        impl Default for sqlite3_vtab_cursor {
            fn default() -> Self {
                unsafe { mem::zeroed() }
            }
        }

    }

    mod error
    {
        use ::
        {
            *,
        };
        /*
        use crate::types::FromSqlError;
        use crate::types::Type;
        use crate::{errmsg_to_string, ffi, Result};
        use std::error;
        use std::fmt;
        use std::os::raw::c_int;
        use std::path::PathBuf;
        use std::str;
        */
        /// Enum listing possible errors from rusqlite.
        #[derive(Debug)]
        #[allow(clippy::enum_variant_names)]
        #[non_exhaustive]
        pub enum Error {
            /// An error from an underlying SQLite call.
            SqliteFailure(ffi::Error, Option<String>),
        
            /// Error reported when attempting to open a connection when SQLite was
            /// configured to allow single-threaded use only.
            SqliteSingleThreadedMode,
        
            /// Error when the value of a particular column is requested, but it cannot
            /// be converted to the requested Rust type.
            FromSqlConversionFailure(usize, Type, Box<dyn error::Error + Send + Sync + 'static>),
        
            /// Error when SQLite gives us an integral value outside the range of the
            /// requested type (e.g., trying to get the value 1000 into a `u8`).
            /// The associated `usize` is the column index,
            /// and the associated `i64` is the value returned by SQLite.
            IntegralValueOutOfRange(usize, i64),
        
            /// Error converting a string to UTF-8.
            Utf8Error(str::Utf8Error),
        
            /// Error converting a string to a C-compatible string because it contained
            /// an embedded nul.
            NulError(std::ffi::NulError),
        
            /// Error when using SQL named parameters and passing a parameter name not
            /// present in the SQL.
            InvalidParameterName(String),
        
            /// Error converting a file path to a string.
            InvalidPath(PathBuf),
        
            /// Error returned when an [`execute`](crate::Connection::execute) call
            /// returns rows.
            ExecuteReturnedResults,
        
            /// Error when a query that was expected to return at least one row (e.g.,
            /// for [`query_row`](crate::Connection::query_row)) did not return any.
            QueryReturnedNoRows,
        
            /// Error when the value of a particular column is requested, but the index
            /// is out of range for the statement.
            InvalidColumnIndex(usize),
        
            /// Error when the value of a named column is requested, but no column
            /// matches the name for the statement.
            InvalidColumnName(String),
        
            /// Error when the value of a particular column is requested, but the type
            /// of the result in that column cannot be converted to the requested
            /// Rust type.
            InvalidColumnType(usize, String, Type),
        
            /// Error when a query that was expected to insert one row did not insert
            /// any or insert many.
            StatementChangedRows(usize),
        
            /// Error returned by
            /// [`functions::Context::get`](crate::functions::Context::get) when the
            /// function argument cannot be converted to the requested type.
            #[cfg(feature = "functions")]
            #[cfg_attr(docsrs, doc(cfg(feature = "functions")))]
            InvalidFunctionParameterType(usize, Type),
            /// Error returned by [`vtab::Values::get`](crate::vtab::Values::get) when
            /// the filter argument cannot be converted to the requested type.
            #[cfg(feature = "vtab")]
            #[cfg_attr(docsrs, doc(cfg(feature = "vtab")))]
            InvalidFilterParameterType(usize, Type),
        
            /// An error case available for implementors of custom user functions (e.g.,
            /// [`create_scalar_function`](crate::Connection::create_scalar_function)).
            #[cfg(feature = "functions")]
            #[cfg_attr(docsrs, doc(cfg(feature = "functions")))]
            #[allow(dead_code)]
            UserFunctionError(Box<dyn error::Error + Send + Sync + 'static>),
        
            /// Error available for the implementors of the
            /// [`ToSql`](crate::types::ToSql) trait.
            ToSqlConversionFailure(Box<dyn error::Error + Send + Sync + 'static>),
        
            /// Error when the SQL is not a `SELECT`, is not read-only.
            InvalidQuery,
        
            /// An error case available for implementors of custom modules (e.g.,
            /// [`create_module`](crate::Connection::create_module)).
            #[cfg(feature = "vtab")]
            #[cfg_attr(docsrs, doc(cfg(feature = "vtab")))]
            #[allow(dead_code)]
            ModuleError(String),
        
            /// An unwinding panic occurs in a UDF (user-defined function).
            UnwindingPanic,
        
            /// An error returned when
            /// [`Context::get_aux`](crate::functions::Context::get_aux) attempts to
            /// retrieve data of a different type than what had been stored using
            /// [`Context::set_aux`](crate::functions::Context::set_aux).
            #[cfg(feature = "functions")]
            #[cfg_attr(docsrs, doc(cfg(feature = "functions")))]
            GetAuxWrongType,
        
            /// Error when the SQL contains multiple statements.
            MultipleStatement,
            /// Error when the number of bound parameters does not match the number of
            /// parameters in the query. The first `usize` is how many parameters were
            /// given, the 2nd is how many were expected.
            InvalidParameterCount(usize, usize),
        
            /// Returned from various functions in the Blob IO positional API. For
            /// example,
            /// [`Blob::raw_read_at_exact`](crate::blob::Blob::raw_read_at_exact) will
            /// return it if the blob has insufficient data.
            #[cfg(feature = "blob")]
            #[cfg_attr(docsrs, doc(cfg(feature = "blob")))]
            BlobSizeError,
            /// Error referencing a specific token in the input SQL
            #[cfg(feature = "modern_sqlite")] // 3.38.0
            #[cfg_attr(docsrs, doc(cfg(feature = "modern_sqlite")))]
            SqlInputError {
                /// error code
                error: ffi::Error,
                /// error message
                msg: String,
                /// SQL input
                sql: String,
                /// byte offset of the start of invalid token
                offset: c_int,
            },
            /// Loadable extension initialization error
            #[cfg(feature = "loadable_extension")]
            #[cfg_attr(docsrs, doc(cfg(feature = "loadable_extension")))]
            InitError(ffi::InitError),
            /// Error when the schema of a particular database is requested, but the index
            /// is out of range.
            #[cfg(feature = "modern_sqlite")] // 3.39.0
            #[cfg_attr(docsrs, doc(cfg(feature = "modern_sqlite")))]
            InvalidDatabaseIndex(usize),
        }
        
        impl PartialEq for Error {
            fn eq(&self, other: &Error) -> bool {
                match (self, other) {
                    (Error::SqliteFailure(e1, s1), Error::SqliteFailure(e2, s2)) => e1 == e2 && s1 == s2,
                    (Error::SqliteSingleThreadedMode, Error::SqliteSingleThreadedMode) => true,
                    (Error::IntegralValueOutOfRange(i1, n1), Error::IntegralValueOutOfRange(i2, n2)) => {
                        i1 == i2 && n1 == n2
                    }
                    (Error::Utf8Error(e1), Error::Utf8Error(e2)) => e1 == e2,
                    (Error::NulError(e1), Error::NulError(e2)) => e1 == e2,
                    (Error::InvalidParameterName(n1), Error::InvalidParameterName(n2)) => n1 == n2,
                    (Error::InvalidPath(p1), Error::InvalidPath(p2)) => p1 == p2,
                    (Error::ExecuteReturnedResults, Error::ExecuteReturnedResults) => true,
                    (Error::QueryReturnedNoRows, Error::QueryReturnedNoRows) => true,
                    (Error::InvalidColumnIndex(i1), Error::InvalidColumnIndex(i2)) => i1 == i2,
                    (Error::InvalidColumnName(n1), Error::InvalidColumnName(n2)) => n1 == n2,
                    (Error::InvalidColumnType(i1, n1, t1), Error::InvalidColumnType(i2, n2, t2)) => {
                        i1 == i2 && t1 == t2 && n1 == n2
                    }
                    (Error::StatementChangedRows(n1), Error::StatementChangedRows(n2)) => n1 == n2,
                    #[cfg(feature = "functions")]
                    (
                        Error::InvalidFunctionParameterType(i1, t1),
                        Error::InvalidFunctionParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    #[cfg(feature = "vtab")]
                    (
                        Error::InvalidFilterParameterType(i1, t1),
                        Error::InvalidFilterParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    (Error::InvalidQuery, Error::InvalidQuery) => true,
                    #[cfg(feature = "vtab")]
                    (Error::ModuleError(s1), Error::ModuleError(s2)) => s1 == s2,
                    (Error::UnwindingPanic, Error::UnwindingPanic) => true,
                    #[cfg(feature = "functions")]
                    (Error::GetAuxWrongType, Error::GetAuxWrongType) => true,
                    (Error::InvalidParameterCount(i1, n1), Error::InvalidParameterCount(i2, n2)) => {
                        i1 == i2 && n1 == n2
                    }
                    #[cfg(feature = "blob")]
                    (Error::BlobSizeError, Error::BlobSizeError) => true,
                    #[cfg(feature = "modern_sqlite")]
                    (
                        Error::SqlInputError {
                            error: e1,
                            msg: m1,
                            sql: s1,
                            offset: o1,
                        },
                        Error::SqlInputError {
                            error: e2,
                            msg: m2,
                            sql: s2,
                            offset: o2,
                        },
                    ) => e1 == e2 && m1 == m2 && s1 == s2 && o1 == o2,
                    #[cfg(feature = "loadable_extension")]
                    (Error::InitError(e1), Error::InitError(e2)) => e1 == e2,
                    #[cfg(feature = "modern_sqlite")]
                    (Error::InvalidDatabaseIndex(i1), Error::InvalidDatabaseIndex(i2)) => i1 == i2,
                    (..) => false,
                }
            }
        }
        
        impl From<str::Utf8Error> for Error {
            #[cold]
            fn from(err: str::Utf8Error) -> Error {
                Error::Utf8Error(err)
            }
        }
        
        impl From<std::ffi::NulError> for Error {
            #[cold]
            fn from(err: std::ffi::NulError) -> Error {
                Error::NulError(err)
            }
        }
        
        const UNKNOWN_COLUMN: usize = usize::MAX;
        
        /// The conversion isn't precise, but it's convenient to have it
        /// to allow use of `get_raw(…).as_…()?` in callbacks that take `Error`.
        impl From<FromSqlError> for Error {
            #[cold]
            fn from(err: FromSqlError) -> Error {
                // The error type requires index and type fields, but they aren't known in this
                // context.
                match err {
                    FromSqlError::OutOfRange(val) => Error::IntegralValueOutOfRange(UNKNOWN_COLUMN, val),
                    FromSqlError::InvalidBlobSize { .. } => {
                        Error::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Blob, Box::new(err))
                    }
                    FromSqlError::Other(source) => {
                        Error::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, source)
                    }
                    _ => Error::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, Box::new(err)),
                }
            }
        }
        
        #[cfg(feature = "loadable_extension")]
        impl From<ffi::InitError> for Error {
            #[cold]
            fn from(err: ffi::InitError) -> Error {
                Error::InitError(err)
            }
        }
        
        impl fmt::Display for Error {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match *self {
                    Error::SqliteFailure(ref err, None) => err.fmt(f),
                    Error::SqliteFailure(_, Some(ref s)) => write!(f, "{s}"),
                    Error::SqliteSingleThreadedMode => write!(
                        f,
                        "SQLite was compiled or configured for single-threaded use only"
                    ),
                    Error::FromSqlConversionFailure(i, ref t, ref err) => {
                        if i != UNKNOWN_COLUMN {
                            write!(f, "Conversion error from type {t} at index: {i}, {err}")
                        } else {
                            err.fmt(f)
                        }
                    }
                    Error::IntegralValueOutOfRange(col, val) => {
                        if col != UNKNOWN_COLUMN {
                            write!(f, "Integer {val} out of range at index {col}")
                        } else {
                            write!(f, "Integer {val} out of range")
                        }
                    }
                    Error::Utf8Error(ref err) => err.fmt(f),
                    Error::NulError(ref err) => err.fmt(f),
                    Error::InvalidParameterName(ref name) => write!(f, "Invalid parameter name: {name}"),
                    Error::InvalidPath(ref p) => write!(f, "Invalid path: {}", p.to_string_lossy()),
                    Error::ExecuteReturnedResults => {
                        write!(f, "Execute returned results - did you mean to call query?")
                    }
                    Error::QueryReturnedNoRows => write!(f, "Query returned no rows"),
                    Error::InvalidColumnIndex(i) => write!(f, "Invalid column index: {i}"),
                    Error::InvalidColumnName(ref name) => write!(f, "Invalid column name: {name}"),
                    Error::InvalidColumnType(i, ref name, ref t) => {
                        write!(f, "Invalid column type {t} at index: {i}, name: {name}")
                    }
                    Error::InvalidParameterCount(i1, n1) => write!(
                        f,
                        "Wrong number of parameters passed to query. Got {i1}, needed {n1}"
                    ),
                    Error::StatementChangedRows(i) => write!(f, "Query changed {i} rows"),
        
                    #[cfg(feature = "functions")]
                    Error::InvalidFunctionParameterType(i, ref t) => {
                        write!(f, "Invalid function parameter type {t} at index {i}")
                    }
                    #[cfg(feature = "vtab")]
                    Error::InvalidFilterParameterType(i, ref t) => {
                        write!(f, "Invalid filter parameter type {t} at index {i}")
                    }
                    #[cfg(feature = "functions")]
                    Error::UserFunctionError(ref err) => err.fmt(f),
                    Error::ToSqlConversionFailure(ref err) => err.fmt(f),
                    Error::InvalidQuery => write!(f, "Query is not read-only"),
                    #[cfg(feature = "vtab")]
                    Error::ModuleError(ref desc) => write!(f, "{desc}"),
                    Error::UnwindingPanic => write!(f, "unwinding panic"),
                    #[cfg(feature = "functions")]
                    Error::GetAuxWrongType => write!(f, "get_aux called with wrong type"),
                    Error::MultipleStatement => write!(f, "Multiple statements provided"),
                    #[cfg(feature = "blob")]
                    Error::BlobSizeError => "Blob size is insufficient".fmt(f),
                    #[cfg(feature = "modern_sqlite")]
                    Error::SqlInputError {
                        ref msg,
                        offset,
                        ref sql,
                        ..
                    } => write!(f, "{msg} in {sql} at offset {offset}"),
                    #[cfg(feature = "loadable_extension")]
                    Error::InitError(ref err) => err.fmt(f),
                    #[cfg(feature = "modern_sqlite")]
                    Error::InvalidDatabaseIndex(i) => write!(f, "Invalid database index: {i}"),
                }
            }
        }
        
        impl error::Error for Error {
            fn source(&self) -> Option<&(dyn error::Error + 'static)> {
                match *self {
                    Error::SqliteFailure(ref err, _) => Some(err),
                    Error::Utf8Error(ref err) => Some(err),
                    Error::NulError(ref err) => Some(err),
        
                    Error::IntegralValueOutOfRange(..)
                    | Error::SqliteSingleThreadedMode
                    | Error::InvalidParameterName(_)
                    | Error::ExecuteReturnedResults
                    | Error::QueryReturnedNoRows
                    | Error::InvalidColumnIndex(_)
                    | Error::InvalidColumnName(_)
                    | Error::InvalidColumnType(..)
                    | Error::InvalidPath(_)
                    | Error::InvalidParameterCount(..)
                    | Error::StatementChangedRows(_)
                    | Error::InvalidQuery
                    | Error::MultipleStatement => None,
        
                    #[cfg(feature = "functions")]
                    Error::InvalidFunctionParameterType(..) => None,
                    #[cfg(feature = "vtab")]
                    Error::InvalidFilterParameterType(..) => None,
        
                    #[cfg(feature = "functions")]
                    Error::UserFunctionError(ref err) => Some(&**err),
        
                    Error::FromSqlConversionFailure(_, _, ref err)
                    | Error::ToSqlConversionFailure(ref err) => Some(&**err),
        
                    #[cfg(feature = "vtab")]
                    Error::ModuleError(_) => None,
        
                    Error::UnwindingPanic => None,
        
                    #[cfg(feature = "functions")]
                    Error::GetAuxWrongType => None,
        
                    #[cfg(feature = "blob")]
                    Error::BlobSizeError => None,
                    #[cfg(feature = "modern_sqlite")]
                    Error::SqlInputError { ref error, .. } => Some(error),
                    #[cfg(feature = "loadable_extension")]
                    Error::InitError(ref err) => Some(err),
                    #[cfg(feature = "modern_sqlite")]
                    Error::InvalidDatabaseIndex(_) => None,
                }
            }
        }
        
        impl Error {
            /// Returns the underlying SQLite error if this is [`Error::SqliteFailure`].
            #[inline]
            #[must_use]
            pub fn sqlite_error(&self) -> Option<&ffi::Error> {
                match self {
                    Self::SqliteFailure(error, _) => Some(error),
                    _ => None,
                }
            }
        
            /// Returns the underlying SQLite error code if this is
            /// [`Error::SqliteFailure`].
            #[inline]
            #[must_use]
            pub fn sqlite_error_code(&self) -> Option<ffi::ErrorCode> {
                self.sqlite_error().map(|error| error.code)
            }
        }
        
        // These are public but not re-exported by lib.rs, so only visible within crate.
        
        #[cold]
        pub fn error_from_sqlite_code(code: c_int, message: Option<String>) -> Error {
            Error::SqliteFailure(ffi::Error::new(code), message)
        }
        
        #[cold]
        pub unsafe fn error_from_handle(db: *mut ffi::sqlite3, code: c_int) -> Error {
            let message = if db.is_null() {
                None
            } else {
                Some(errmsg_to_string(ffi::sqlite3_errmsg(db)))
            };
            error_from_sqlite_code(code, message)
        }
        
        #[cold]
        #[cfg(not(feature = "modern_sqlite"))] // SQLite >= 3.38.0
        pub unsafe fn error_with_offset(db: *mut ffi::sqlite3, code: c_int, _sql: &str) -> Error {
            error_from_handle(db, code)
        }
        
        #[cold]
        #[cfg(feature = "modern_sqlite")] // SQLite >= 3.38.0
        pub unsafe fn error_with_offset(db: *mut ffi::sqlite3, code: c_int, sql: &str) -> Error {
            if db.is_null() {
                error_from_sqlite_code(code, None)
            } else {
                let error = ffi::Error::new(code);
                let msg = errmsg_to_string(ffi::sqlite3_errmsg(db));
                if ffi::ErrorCode::Unknown == error.code {
                    let offset = ffi::sqlite3_error_offset(db);
                    if offset >= 0 {
                        return Error::SqlInputError {
                            error,
                            msg,
                            sql: sql.to_owned(),
                            offset,
                        };
                    }
                }
                Error::SqliteFailure(error, Some(msg))
            }
        }
        
        pub fn check(code: c_int) -> Result<()> {
            if code != crate::ffi::SQLITE_OK {
                Err(error_from_sqlite_code(code, None))
            } else {
                Ok(())
            }
        }
        
        /// Transform Rust error to SQLite error (message and code).
        /// # Safety
        /// This function is unsafe because it uses raw pointer
        pub unsafe fn to_sqlite_error(e: &Error, err_msg: *mut *mut std::os::raw::c_char) -> c_int {
            use crate::util::alloc;
            match e {
                Error::SqliteFailure(err, s) => {
                    if let Some(s) = s {
                        *err_msg = alloc(s);
                    }
                    err.extended_code
                }
                err => {
                    *err_msg = alloc(&err.to_string());
                    ffi::SQLITE_ERROR
                }
            }
        }
    }
    /*
    #[cfg(not(feature = "loadable_extension"))]
    pub mod auto_extension;
    #[cfg(feature = "backup")]
    #[cfg_attr(docsrs, doc(cfg(feature = "backup")))]
    pub mod backup;
    #[cfg(feature = "blob")]
    #[cfg_attr(docsrs, doc(cfg(feature = "blob")))]
    pub mod blob;;
    #[cfg(feature = "collation")]
    #[cfg_attr(docsrs, doc(cfg(feature = "collation")))]
    mod collation;
    
    #[cfg(any(feature = "functions", feature = "vtab"))]
    mod context;
    #[cfg(feature = "functions")]
    #[cfg_attr(docsrs, doc(cfg(feature = "functions")))]
    pub mod functions;
    #[cfg(feature = "hooks")]
    #[cfg_attr(docsrs, doc(cfg(feature = "hooks")))]
    pub mod hooks;
    #[cfg(feature = "limits")]
    #[cfg_attr(docsrs, doc(cfg(feature = "limits")))]
    pub mod limits;
    #[cfg(feature = "load_extension")]
    mod load_extension_guard;
    #[cfg(feature = "serialize")]
    #[cfg_attr(docsrs, doc(cfg(feature = "serialize")))]
    pub mod serialize;
    #[cfg(feature = "session")]
    #[cfg_attr(docsrs, doc(cfg(feature = "session")))]
    pub mod session;
    #[cfg(feature = "trace")]
    #[cfg_attr(docsrs, doc(cfg(feature = "trace")))]
    pub mod trace;
    
    #[cfg(feature = "unlock_notify")]
    mod unlock_notify;
    
    #[cfg(feature = "vtab")]
    #[cfg_attr(docsrs, doc(cfg(feature = "vtab")))]
    pub mod vtab; */

    pub mod auto
    {
        //! Automatic extension loading
        use ::
        {
            *,
        };
        /*
        use super::ffi;
        use crate::error::{check, to_sqlite_error};
        use crate::{Connection, Error, Result};
        use std::os::raw::{c_char, c_int};
        use std::panic::catch_unwind;
        */
        /// Automatic extension initialization routine
        pub type AutoExtension = fn(Connection) -> Result<()>;
        
        /// Raw automatic extension initialization routine
        pub type RawAutoExtension = unsafe extern "C" fn(
            db: *mut ffi::sqlite3,
            pz_err_msg: *mut *mut c_char,
            _: *const ffi::sqlite3_api_routines,
        ) -> c_int;
        
        /// Bridge between `RawAutoExtension` and `AutoExtension`
        ///
        /// # Safety
        /// * Opening a database from an auto-extension handler will lead to
        ///   an endless recursion of the auto-handler triggering itself
        ///   indirectly for each newly-opened database.
        /// * Results are undefined if the given db is closed by an auto-extension.
        /// * The list of auto-extensions should not be manipulated from an auto-extension.
        pub unsafe fn init_auto_extension(
            db: *mut ffi::sqlite3,
            pz_err_msg: *mut *mut c_char,
            ax: AutoExtension,
        ) -> c_int {
            let r = catch_unwind(|| {
                let c = Connection::from_handle(db);
                c.and_then(ax)
            })
            .unwrap_or_else(|_| Err(Error::UnwindingPanic));
            match r {
                Err(e) => to_sqlite_error(&e, pz_err_msg),
                _ => ffi::SQLITE_OK,
            }
        }
        
        /// Register au auto-extension
        ///
        /// # Safety
        /// * Opening a database from an auto-extension handler will lead to
        ///   an endless recursion of the auto-handler triggering itself
        ///   indirectly for each newly-opened database.
        /// * Results are undefined if the given db is closed by an auto-extension.
        /// * The list of auto-extensions should not be manipulated from an auto-extension.
        pub unsafe fn register_auto_extension(ax: RawAutoExtension) -> Result<()> {
            check(ffi::sqlite3_auto_extension(Some(ax)))
        }
        
        /// Unregister the initialization routine
        pub fn cancel_auto_extension(ax: RawAutoExtension) -> bool {
            unsafe { ffi::sqlite3_cancel_auto_extension(Some(ax)) == 1 }
        }
        
        /// Disable all automatic extensions previously registered
        pub fn reset_auto_extension() {
            unsafe { ffi::sqlite3_reset_auto_extension() }
        }

    }
    
    pub mod busy
    {
        //! Busy handler (when the database is locked)
        use ::
        {
            *,
        };
        /*
        use std::mem;
        use std::os::raw::{c_int, c_void};
        use std::panic::catch_unwind;
        use std::ptr;
        use std::time::Duration;
        
        use crate::ffi;
        use crate::{Connection, InnerConnection, Result};
        */
        
        impl Connection {
            /// Set a busy handler that sleeps for a specified amount of time when a
            /// table is locked. The handler will sleep multiple times until at
            /// least "ms" milliseconds of sleeping have accumulated.
            ///
            /// Calling this routine with an argument equal to zero turns off all busy
            /// handlers.
            ///
            /// There can only be a single busy handler for a particular database
            /// connection at any given moment. If another busy handler was defined
            /// (using [`busy_handler`](Connection::busy_handler)) prior to calling this
            /// routine, that other busy handler is cleared.
            ///
            /// Newly created connections currently have a default busy timeout of
            /// 5000ms, but this may be subject to change.
            pub fn busy_timeout(&self, timeout: Duration) -> Result<()> {
                let ms: i32 = timeout
                    .as_secs()
                    .checked_mul(1000)
                    .and_then(|t| t.checked_add(timeout.subsec_millis().into()))
                    .and_then(|t| t.try_into().ok())
                    .expect("too big");
                self.db.borrow_mut().busy_timeout(ms)
            }
        
            /// Register a callback to handle `SQLITE_BUSY` errors.
            ///
            /// If the busy callback is `None`, then `SQLITE_BUSY` is returned
            /// immediately upon encountering the lock. The argument to the busy
            /// handler callback is the number of times that the
            /// busy handler has been invoked previously for the
            /// same locking event. If the busy callback returns `false`, then no
            /// additional attempts are made to access the
            /// database and `SQLITE_BUSY` is returned to the
            /// application. If the callback returns `true`, then another attempt
            /// is made to access the database and the cycle repeats.
            ///
            /// There can only be a single busy handler defined for each database
            /// connection. Setting a new busy handler clears any previously set
            /// handler. Note that calling [`busy_timeout()`](Connection::busy_timeout)
            /// or evaluating `PRAGMA busy_timeout=N` will change the busy handler
            /// and thus clear any previously set busy handler.
            ///
            /// Newly created connections default to a
            /// [`busy_timeout()`](Connection::busy_timeout) handler with a timeout
            /// of 5000ms, although this is subject to change.
            pub fn busy_handler(&self, callback: Option<fn(i32) -> bool>) -> Result<()> {
                unsafe extern "C" fn busy_handler_callback(p_arg: *mut c_void, count: c_int) -> c_int {
                    let handler_fn: fn(i32) -> bool = mem::transmute(p_arg);
                    c_int::from(catch_unwind(|| handler_fn(count)).unwrap_or_default())
                }
                let c = self.db.borrow_mut();
                let r = match callback {
                    Some(f) => unsafe {
                        ffi::sqlite3_busy_handler(c.db(), Some(busy_handler_callback), f as *mut c_void)
                    },
                    None => unsafe { ffi::sqlite3_busy_handler(c.db(), None, ptr::null_mut()) },
                };
                c.decode_result(r)
            }
        }
        
        impl InnerConnection {
            #[inline]
            fn busy_timeout(&mut self, timeout: c_int) -> Result<()> {
                let r = unsafe { ffi::sqlite3_busy_timeout(self.db, timeout) };
                self.decode_result(r)
            }
        }
    }
    
    pub mod cache
    {
        //! Prepared statements cache for faster execution.
        use ::
        {
            *,
        };
        /*
        use crate::raw_statement::RawStatement;
        use crate::{Connection, PrepFlags, Result, Statement};
        use hashlink::LruCache;
        use std::cell::RefCell;
        use std::ops::{Deref, DerefMut};
        use std::sync::Arc;
        */
        impl Connection {
            /// Prepare a SQL statement for execution, returning a previously prepared
            /// (but not currently in-use) statement if one is available. The
            /// returned statement will be cached for reuse by future calls to
            /// [`prepare_cached`](Connection::prepare_cached) once it is dropped.
            ///
            /// ```rust,no_run
            /// # use rusqlite::{Connection, Result};
            /// fn insert_new_people(conn: &Connection) -> Result<()> {
            ///     {
            ///         let mut stmt = conn.prepare_cached("INSERT INTO People (name) VALUES (?1)")?;
            ///         stmt.execute(["Joe Smith"])?;
            ///     }
            ///     {
            ///         // This will return the same underlying SQLite statement handle without
            ///         // having to prepare it again.
            ///         let mut stmt = conn.prepare_cached("INSERT INTO People (name) VALUES (?1)")?;
            ///         stmt.execute(["Bob Jones"])?;
            ///     }
            ///     Ok(())
            /// }
            /// ```
            ///
            /// # Failure
            ///
            /// Will return `Err` if `sql` cannot be converted to a C-compatible string
            /// or if the underlying SQLite call fails.
            #[inline]
            pub fn prepare_cached(&self, sql: &str) -> Result<CachedStatement<'_>> {
                self.cache.get(self, sql)
            }
        
            /// Set the maximum number of cached prepared statements this connection
            /// will hold. By default, a connection will hold a relatively small
            /// number of cached statements. If you need more, or know that you
            /// will not use cached statements, you
            /// can set the capacity manually using this method.
            #[inline]
            pub fn set_prepared_statement_cache_capacity(&self, capacity: usize) {
                self.cache.set_capacity(capacity);
            }
        
            /// Remove/finalize all prepared statements currently in the cache.
            #[inline]
            pub fn flush_prepared_statement_cache(&self) {
                self.cache.flush();
            }
        }
        
        /// Prepared statements LRU cache.
        #[derive(Debug)]
        pub struct StatementCache(RefCell<LruCache<Arc<str>, RawStatement>>);
        
        #[allow(clippy::non_send_fields_in_send_ty)]
        unsafe impl Send for StatementCache {}
        
        /// Cacheable statement.
        ///
        /// Statement will return automatically to the cache by default.
        /// If you want the statement to be discarded, call
        /// [`discard()`](CachedStatement::discard) on it.
        pub struct CachedStatement<'conn> {
            stmt: Option<Statement<'conn>>,
            cache: &'conn StatementCache,
        }
        
        impl<'conn> Deref for CachedStatement<'conn> {
            type Target = Statement<'conn>;
        
            #[inline]
            fn deref(&self) -> &Statement<'conn> {
                self.stmt.as_ref().unwrap()
            }
        }
        
        impl<'conn> DerefMut for CachedStatement<'conn> {
            #[inline]
            fn deref_mut(&mut self) -> &mut Statement<'conn> {
                self.stmt.as_mut().unwrap()
            }
        }
        
        impl Drop for CachedStatement<'_> {
            #[allow(unused_must_use)]
            #[inline]
            fn drop(&mut self) {
                if let Some(stmt) = self.stmt.take() {
                    self.cache.cache_stmt(unsafe { stmt.into_raw() });
                }
            }
        }
        
        impl CachedStatement<'_> {
            #[inline]
            fn new<'conn>(stmt: Statement<'conn>, cache: &'conn StatementCache) -> CachedStatement<'conn> {
                CachedStatement {
                    stmt: Some(stmt),
                    cache,
                }
            }
        
            /// Discard the statement, preventing it from being returned to its
            /// [`Connection`]'s collection of cached statements.
            #[inline]
            pub fn discard(mut self) {
                self.stmt = None;
            }
        }
        
        impl StatementCache {
            /// Create a statement cache.
            #[inline]
            pub fn with_capacity(capacity: usize) -> StatementCache {
                StatementCache(RefCell::new(LruCache::new(capacity)))
            }
        
            #[inline]
            fn set_capacity(&self, capacity: usize) {
                self.0.borrow_mut().set_capacity(capacity);
            }
        
            // Search the cache for a prepared-statement object that implements `sql`.
            // If no such prepared-statement can be found, allocate and prepare a new one.
            //
            // # Failure
            //
            // Will return `Err` if no cached statement can be found and the underlying
            // SQLite prepare call fails.
            fn get<'conn>(
                &'conn self,
                conn: &'conn Connection,
                sql: &str,
            ) -> Result<CachedStatement<'conn>> {
                let trimmed = sql.trim();
                let mut cache = self.0.borrow_mut();
                let stmt = match cache.remove(trimmed) {
                    Some(raw_stmt) => Ok(Statement::new(conn, raw_stmt)),
                    None => conn.prepare_with_flags(trimmed, PrepFlags::SQLITE_PREPARE_PERSISTENT),
                };
                stmt.map(|mut stmt| {
                    stmt.stmt.set_statement_cache_key(trimmed);
                    CachedStatement::new(stmt, self)
                })
            }
        
            // Return a statement to the cache.
            fn cache_stmt(&self, mut stmt: RawStatement) {
                if stmt.is_null() {
                    return;
                }
                let mut cache = self.0.borrow_mut();
                stmt.clear_bindings();
                if let Some(sql) = stmt.statement_cache_key() {
                    cache.insert(sql, stmt);
                } else {
                    debug_assert!(
                        false,
                        "bug in statement cache code, statement returned to cache that without key"
                    );
                }
            }
        
            #[inline]
            fn flush(&self) {
                let mut cache = self.0.borrow_mut();
                cache.clear();
            }
        }
    }
    
    pub mod column
    {
        use ::
        {
            *,
        };
        /*
        use std::str;
        
        use crate::{Error, Result, Statement};
        */
        
        /// Information about a column of a SQLite query.
        #[cfg(feature = "column_decltype")]
        #[cfg_attr(docsrs, doc(cfg(feature = "column_decltype")))]
        #[derive(Debug)]
        pub struct Column<'stmt> {
            name: &'stmt str,
            decl_type: Option<&'stmt str>,
        }
        
        #[cfg(feature = "column_decltype")]
        #[cfg_attr(docsrs, doc(cfg(feature = "column_decltype")))]
        impl Column<'_> {
            /// Returns the name of the column.
            #[inline]
            #[must_use]
            pub fn name(&self) -> &str {
                self.name
            }
        
            /// Returns the type of the column (`None` for expression).
            #[inline]
            #[must_use]
            pub fn decl_type(&self) -> Option<&str> {
                self.decl_type
            }
        }
        
        impl Statement<'_> {
            /// Get all the column names in the result set of the prepared statement.
            ///
            /// If associated DB schema can be altered concurrently, you should make
            /// sure that current statement has already been stepped once before
            /// calling this method.
            pub fn column_names(&self) -> Vec<&str> {
                let n = self.column_count();
                let mut cols = Vec::with_capacity(n);
                for i in 0..n {
                    let s = self.column_name_unwrap(i);
                    cols.push(s);
                }
                cols
            }
        
            /// Return the number of columns in the result set returned by the prepared
            /// statement.
            ///
            /// If associated DB schema can be altered concurrently, you should make
            /// sure that current statement has already been stepped once before
            /// calling this method.
            #[inline]
            pub fn column_count(&self) -> usize {
                self.stmt.column_count()
            }
        
            /// Check that column name reference lifetime is limited:
            /// https://www.sqlite.org/c3ref/column_name.html
            /// > The returned string pointer is valid...
            ///
            /// `column_name` reference can become invalid if `stmt` is reprepared
            /// (because of schema change) when `query_row` is called. So we assert
            /// that a compilation error happens if this reference is kept alive:
            /// ```compile_fail
            /// use rusqlite::{Connection, Result};
            /// fn main() -> Result<()> {
            ///     let db = Connection::open_in_memory()?;
            ///     let mut stmt = db.prepare("SELECT 1 as x")?;
            ///     let column_name = stmt.column_name(0)?;
            ///     let x = stmt.query_row([], |r| r.get::<_, i64>(0))?; // E0502
            ///     assert_eq!(1, x);
            ///     assert_eq!("x", column_name);
            ///     Ok(())
            /// }
            /// ```
            #[inline]
            pub(super) fn column_name_unwrap(&self, col: usize) -> &str {
                // Just panic if the bounds are wrong for now, we never call this
                // without checking first.
                self.column_name(col).expect("Column out of bounds")
            }
        
            /// Returns the name assigned to a particular column in the result set
            /// returned by the prepared statement.
            ///
            /// If associated DB schema can be altered concurrently, you should make
            /// sure that current statement has already been stepped once before
            /// calling this method.
            ///
            /// ## Failure
            ///
            /// Returns an `Error::InvalidColumnIndex` if `idx` is outside the valid
            /// column range for this row.
            ///
            /// # Panics
            ///
            /// Panics when column name is not valid UTF-8.
            #[inline]
            pub fn column_name(&self, col: usize) -> Result<&str> {
                self.stmt
                    .column_name(col)
                    // clippy::or_fun_call (nightly) vs clippy::unnecessary-lazy-evaluations (stable)
                    .ok_or(Error::InvalidColumnIndex(col))
                    .map(|slice| {
                        slice
                            .to_str()
                            .expect("Invalid UTF-8 sequence in column name")
                    })
            }
        
            /// Returns the column index in the result set for a given column name.
            ///
            /// If there is no AS clause then the name of the column is unspecified and
            /// may change from one release of SQLite to the next.
            ///
            /// If associated DB schema can be altered concurrently, you should make
            /// sure that current statement has already been stepped once before
            /// calling this method.
            ///
            /// # Failure
            ///
            /// Will return an `Error::InvalidColumnName` when there is no column with
            /// the specified `name`.
            #[inline]
            pub fn column_index(&self, name: &str) -> Result<usize> {
                let bytes = name.as_bytes();
                let n = self.column_count();
                for i in 0..n {
                    // Note: `column_name` is only fallible if `i` is out of bounds,
                    // which we've already checked.
                    if bytes.eq_ignore_ascii_case(self.stmt.column_name(i).unwrap().to_bytes()) {
                        return Ok(i);
                    }
                }
                Err(Error::InvalidColumnName(String::from(name)))
            }
        
            /// Returns a slice describing the columns of the result of the query.
            ///
            /// If associated DB schema can be altered concurrently, you should make
            /// sure that current statement has already been stepped once before
            /// calling this method.
            #[cfg(feature = "column_decltype")]
            #[cfg_attr(docsrs, doc(cfg(feature = "column_decltype")))]
            pub fn columns(&self) -> Vec<Column> {
                let n = self.column_count();
                let mut cols = Vec::with_capacity(n);
                for i in 0..n {
                    let name = self.column_name_unwrap(i);
                    let slice = self.stmt.column_decltype(i);
                    let decl_type = slice.map(|s| {
                        s.to_str()
                            .expect("Invalid UTF-8 sequence in column declaration")
                    });
                    cols.push(Column { name, decl_type });
                }
                cols
            }
        }
    }
    
    pub mod config
    {
        //! Configure database connections
        use ::
        {
            *,
        };
        /*
        use std::os::raw::c_int;
        
        use crate::error::check;
        use crate::ffi;
        use crate::{Connection, Result};
        */
        
        /// Database Connection Configuration Options
        /// See [Database Connection Configuration Options](https://sqlite.org/c3ref/c_dbconfig_enable_fkey.html) for details.
        #[repr(i32)]
        #[derive(Copy, Clone, Debug)]
        #[allow(non_snake_case, non_camel_case_types)]
        #[non_exhaustive]
        #[allow(clippy::upper_case_acronyms)]
        pub enum DbConfig {
            //SQLITE_DBCONFIG_MAINDBNAME = 1000, /* const char* */
            //SQLITE_DBCONFIG_LOOKASIDE = 1001,  /* void* int int */
            /// Enable or disable the enforcement of foreign key constraints.
            SQLITE_DBCONFIG_ENABLE_FKEY = ffi::SQLITE_DBCONFIG_ENABLE_FKEY,
            /// Enable or disable triggers.
            SQLITE_DBCONFIG_ENABLE_TRIGGER = ffi::SQLITE_DBCONFIG_ENABLE_TRIGGER,
            /// Enable or disable the fts3_tokenizer() function which is part of the
            /// FTS3 full-text search engine extension.
            SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = ffi::SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, // 3.12.0
            //SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005,
            /// In WAL mode, enable or disable the checkpoint operation before closing
            /// the connection.
            SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE = 1006, // 3.16.2
            /// Activates or deactivates the query planner stability guarantee (QPSG).
            SQLITE_DBCONFIG_ENABLE_QPSG = 1007, // 3.20.0
            /// Includes or excludes output for any operations performed by trigger
            /// programs from the output of EXPLAIN QUERY PLAN commands.
            SQLITE_DBCONFIG_TRIGGER_EQP = 1008, // 3.22.0
            /// Activates or deactivates the "reset" flag for a database connection.
            /// Run VACUUM with this flag set to reset the database.
            SQLITE_DBCONFIG_RESET_DATABASE = 1009, // 3.24.0
            /// Activates or deactivates the "defensive" flag for a database connection.
            SQLITE_DBCONFIG_DEFENSIVE = 1010, // 3.26.0
            /// Activates or deactivates the "writable_schema" flag.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_WRITABLE_SCHEMA = 1011, // 3.28.0
            /// Activates or deactivates the legacy behavior of the ALTER TABLE RENAME
            /// command.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_LEGACY_ALTER_TABLE = 1012, // 3.29
            /// Activates or deactivates the legacy double-quoted string literal
            /// misfeature for DML statements only.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_DQS_DML = 1013, // 3.29.0
            /// Activates or deactivates the legacy double-quoted string literal
            /// misfeature for DDL statements.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_DQS_DDL = 1014, // 3.29.0
            /// Enable or disable views.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_ENABLE_VIEW = 1015, // 3.30.0
            /// Activates or deactivates the legacy file format flag.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_LEGACY_FILE_FORMAT = 1016, // 3.31.0
            /// Tells SQLite to assume that database schemas (the contents of the
            /// sqlite_master tables) are untainted by malicious content.
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_TRUSTED_SCHEMA = 1017, // 3.31.0
            /// Sets or clears a flag that enables collection of the
            /// sqlite3_stmt_scanstatus_v2() statistics
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_STMT_SCANSTATUS = 1018, // 3.42.0
            /// Changes the default order in which tables and indexes are scanned
            #[cfg(feature = "modern_sqlite")]
            SQLITE_DBCONFIG_REVERSE_SCANORDER = 1019, // 3.42.0
        }
        
        impl Connection {
            /// Returns the current value of a `config`.
            #[inline]
            pub fn db_config(&self, config: DbConfig) -> Result<bool> {
                let c = self.db.borrow();
                unsafe {
                    let mut val = 0;
                    check(ffi::sqlite3_db_config(
                        c.db(),
                        config as c_int,
                        -1,
                        &mut val,
                    ))?;
                    Ok(val != 0)
                }
            }
        
            /// Make configuration changes to a database connection
            #[inline]
            pub fn set_db_config(&self, config: DbConfig, new_val: bool) -> Result<bool> {
                let c = self.db.borrow_mut();
                unsafe {
                    let mut val = 0;
                    check(ffi::sqlite3_db_config(
                        c.db(),
                        config as c_int,
                        new_val as c_int,
                        &mut val,
                    ))?;
                    Ok(val != 0)
                }
            }
        }
    }
    
    pub mod inner_connection
    {
        use ::
        {
            *,
        };
        /*
        use std::ffi::CStr;
        use std::os::raw::{c_char, c_int};
        #[cfg(feature = "load_extension")]
        use std::path::Path;
        use std::ptr;
        use std::str;
        use std::sync::{Arc, Mutex};
        
        use super::ffi;
        use super::str_for_sqlite;
        use super::{Connection, InterruptHandle, OpenFlags, PrepFlags, Result};
        use crate::error::{error_from_handle, error_from_sqlite_code, error_with_offset, Error};
        use crate::raw_statement::RawStatement;
        use crate::statement::Statement;
        use crate::version_number;
        */
        
        pub struct InnerConnection {
            pub db: *mut ffi::sqlite3,
            // It's unsafe to call `sqlite3_close` while another thread is performing
            // a `sqlite3_interrupt`, and vice versa, so we take this mutex during
            // those functions. This protects a copy of the `db` pointer (which is
            // cleared on closing), however the main copy, `db`, is unprotected.
            // Otherwise, a long-running query would prevent calling interrupt, as
            // interrupt would only acquire the lock after the query's completion.
            interrupt_lock: Arc<Mutex<*mut ffi::sqlite3>>,
            #[cfg(feature = "hooks")]
            pub free_commit_hook: Option<unsafe fn(*mut std::os::raw::c_void)>,
            #[cfg(feature = "hooks")]
            pub free_rollback_hook: Option<unsafe fn(*mut std::os::raw::c_void)>,
            #[cfg(feature = "hooks")]
            pub free_update_hook: Option<unsafe fn(*mut std::os::raw::c_void)>,
            #[cfg(feature = "hooks")]
            pub progress_handler: Option<Box<dyn FnMut() -> bool + Send>>,
            #[cfg(feature = "hooks")]
            pub authorizer: Option<crate::hooks::BoxedAuthorizer>,
            #[cfg(feature = "preupdate_hook")]
            pub free_preupdate_hook: Option<unsafe fn(*mut ::std::os::raw::c_void)>,
            owned: bool,
        }
        
        unsafe impl Send for InnerConnection {}
        
        impl InnerConnection {
            #[allow(clippy::mutex_atomic, clippy::arc_with_non_send_sync)] // See unsafe impl Send / Sync for InterruptHandle
            #[inline]
            pub unsafe fn new(db: *mut ffi::sqlite3, owned: bool) -> InnerConnection {
                InnerConnection {
                    db,
                    interrupt_lock: Arc::new(Mutex::new(db)),
                    #[cfg(feature = "hooks")]
                    free_commit_hook: None,
                    #[cfg(feature = "hooks")]
                    free_rollback_hook: None,
                    #[cfg(feature = "hooks")]
                    free_update_hook: None,
                    #[cfg(feature = "hooks")]
                    progress_handler: None,
                    #[cfg(feature = "hooks")]
                    authorizer: None,
                    #[cfg(feature = "preupdate_hook")]
                    free_preupdate_hook: None,
                    owned,
                }
            }
        
            pub fn open_with_flags(
                c_path: &CStr,
                mut flags: OpenFlags,
                vfs: Option<&CStr>,
            ) -> Result<InnerConnection> {
                ensure_safe_sqlite_threading_mode()?;
        
                let z_vfs = match vfs {
                    Some(c_vfs) => c_vfs.as_ptr(),
                    None => ptr::null(),
                };
        
                // turn on extended results code before opening database to have a better diagnostic if a failure happens
                let exrescode = if version_number() >= 3_037_000 {
                    flags |= OpenFlags::SQLITE_OPEN_EXRESCODE;
                    true
                } else {
                    false // flag SQLITE_OPEN_EXRESCODE is ignored by SQLite version < 3.37.0
                };
        
                unsafe {
                    let mut db: *mut ffi::sqlite3 = ptr::null_mut();
                    let r = ffi::sqlite3_open_v2(c_path.as_ptr(), &mut db, flags.bits(), z_vfs);
                    if r != ffi::SQLITE_OK {
                        let e = if db.is_null() {
                            error_from_sqlite_code(r, Some(c_path.to_string_lossy().to_string()))
                        } else {
                            let mut e = error_from_handle(db, r);
                            if let Error::SqliteFailure(
                                ffi::Error {
                                    code: ffi::ErrorCode::CannotOpen,
                                    ..
                                },
                                Some(msg),
                            ) = e
                            {
                                e = Error::SqliteFailure(
                                    ffi::Error::new(r),
                                    Some(format!("{msg}: {}", c_path.to_string_lossy())),
                                );
                            }
                            ffi::sqlite3_close(db);
                            e
                        };
        
                        return Err(e);
                    }
        
                    // attempt to turn on extended results code; don't fail if we can't.
                    if !exrescode {
                        ffi::sqlite3_extended_result_codes(db, 1);
                    }
        
                    let r = ffi::sqlite3_busy_timeout(db, 5000);
                    if r != ffi::SQLITE_OK {
                        let e = error_from_handle(db, r);
                        ffi::sqlite3_close(db);
                        return Err(e);
                    }
        
                    Ok(InnerConnection::new(db, true))
                }
            }
        
            #[inline]
            pub fn db(&self) -> *mut ffi::sqlite3 {
                self.db
            }
        
            #[inline]
            pub fn decode_result(&self, code: c_int) -> Result<()> {
                unsafe { InnerConnection::decode_result_raw(self.db(), code) }
            }
        
            #[inline]
            unsafe fn decode_result_raw(db: *mut ffi::sqlite3, code: c_int) -> Result<()> {
                if code == ffi::SQLITE_OK {
                    Ok(())
                } else {
                    Err(error_from_handle(db, code))
                }
            }
        
            #[allow(clippy::mutex_atomic)]
            pub fn close(&mut self) -> Result<()> {
                if self.db.is_null() {
                    return Ok(());
                }
                self.remove_hooks();
                self.remove_preupdate_hook();
                let mut shared_handle = self.interrupt_lock.lock().unwrap();
                assert!(
                    !shared_handle.is_null(),
                    "Bug: Somehow interrupt_lock was cleared before the DB was closed"
                );
                if !self.owned {
                    self.db = ptr::null_mut();
                    return Ok(());
                }
                unsafe {
                    let r = ffi::sqlite3_close(self.db);
                    // Need to use _raw because _guard has a reference out, and
                    // decode_result takes &mut self.
                    let r = InnerConnection::decode_result_raw(self.db, r);
                    if r.is_ok() {
                        *shared_handle = ptr::null_mut();
                        self.db = ptr::null_mut();
                    }
                    r
                }
            }
        
            #[inline]
            pub fn get_interrupt_handle(&self) -> InterruptHandle {
                InterruptHandle {
                    db_lock: Arc::clone(&self.interrupt_lock),
                }
            }
        
            #[inline]
            #[cfg(feature = "load_extension")]
            pub unsafe fn enable_load_extension(&mut self, onoff: c_int) -> Result<()> {
                let r = ffi::sqlite3_enable_load_extension(self.db, onoff);
                self.decode_result(r)
            }
        
            #[cfg(feature = "load_extension")]
            pub unsafe fn load_extension(
                &self,
                dylib_path: &Path,
                entry_point: Option<&str>,
            ) -> Result<()> {
                let dylib_str = super::path_to_cstring(dylib_path)?;
                let mut errmsg: *mut c_char = ptr::null_mut();
                let r = if let Some(entry_point) = entry_point {
                    let c_entry = crate::str_to_cstring(entry_point)?;
                    ffi::sqlite3_load_extension(self.db, dylib_str.as_ptr(), c_entry.as_ptr(), &mut errmsg)
                } else {
                    ffi::sqlite3_load_extension(self.db, dylib_str.as_ptr(), ptr::null(), &mut errmsg)
                };
                if r == ffi::SQLITE_OK {
                    Ok(())
                } else {
                    let message = super::errmsg_to_string(errmsg);
                    ffi::sqlite3_free(errmsg.cast::<std::os::raw::c_void>());
                    Err(error_from_sqlite_code(r, Some(message)))
                }
            }
        
            #[inline]
            pub fn last_insert_rowid(&self) -> i64 {
                unsafe { ffi::sqlite3_last_insert_rowid(self.db()) }
            }
        
            pub fn prepare<'a>(
                &mut self,
                conn: &'a Connection,
                sql: &str,
                flags: PrepFlags,
            ) -> Result<Statement<'a>> {
                let mut c_stmt: *mut ffi::sqlite3_stmt = ptr::null_mut();
                let (c_sql, len, _) = str_for_sqlite(sql.as_bytes())?;
                let mut c_tail: *const c_char = ptr::null();
                #[cfg(not(feature = "unlock_notify"))]
                let r = unsafe { self.prepare_(c_sql, len, flags, &mut c_stmt, &mut c_tail) };
                #[cfg(feature = "unlock_notify")]
                let r = unsafe {
                    use crate::unlock_notify;
                    let mut rc;
                    loop {
                        rc = self.prepare_(c_sql, len, flags, &mut c_stmt, &mut c_tail);
                        if !unlock_notify::is_locked(self.db, rc) {
                            break;
                        }
                        rc = unlock_notify::wait_for_unlock_notify(self.db);
                        if rc != ffi::SQLITE_OK {
                            break;
                        }
                    }
                    rc
                };
                // If there is an error, *ppStmt is set to NULL.
                if r != ffi::SQLITE_OK {
                    return Err(unsafe { error_with_offset(self.db, r, sql) });
                }
                // If the input text contains no SQL (if the input is an empty string or a
                // comment) then *ppStmt is set to NULL.
                let tail = if c_tail.is_null() {
                    0
                } else {
                    let n = (c_tail as isize) - (c_sql as isize);
                    if n <= 0 || n >= len as isize {
                        0
                    } else {
                        n as usize
                    }
                };
                Ok(Statement::new(conn, unsafe {
                    RawStatement::new(c_stmt, tail)
                }))
            }
        
            #[inline]
            #[cfg(not(feature = "modern_sqlite"))]
            unsafe fn prepare_(
                &self,
                z_sql: *const c_char,
                n_byte: c_int,
                _: PrepFlags,
                pp_stmt: *mut *mut ffi::sqlite3_stmt,
                pz_tail: *mut *const c_char,
            ) -> c_int {
                ffi::sqlite3_prepare_v2(self.db(), z_sql, n_byte, pp_stmt, pz_tail)
            }
        
            #[inline]
            #[cfg(feature = "modern_sqlite")]
            unsafe fn prepare_(
                &self,
                z_sql: *const c_char,
                n_byte: c_int,
                flags: PrepFlags,
                pp_stmt: *mut *mut ffi::sqlite3_stmt,
                pz_tail: *mut *const c_char,
            ) -> c_int {
                ffi::sqlite3_prepare_v3(self.db(), z_sql, n_byte, flags.bits(), pp_stmt, pz_tail)
            }
        
            #[inline]
            pub fn changes(&self) -> u64 {
                #[cfg(not(feature = "modern_sqlite"))]
                unsafe {
                    ffi::sqlite3_changes(self.db()) as u64
                }
                #[cfg(feature = "modern_sqlite")] // 3.37.0
                unsafe {
                    ffi::sqlite3_changes64(self.db()) as u64
                }
            }
        
            #[inline]
            pub fn total_changes(&self) -> u64 {
                #[cfg(not(feature = "modern_sqlite"))]
                unsafe {
                    ffi::sqlite3_total_changes(self.db()) as u64
                }
                #[cfg(feature = "modern_sqlite")] // 3.37.0
                unsafe {
                    ffi::sqlite3_total_changes64(self.db()) as u64
                }
            }
        
            #[inline]
            pub fn is_autocommit(&self) -> bool {
                unsafe { ffi::sqlite3_get_autocommit(self.db()) != 0 }
            }
        
            pub fn is_busy(&self) -> bool {
                let db = self.db();
                unsafe {
                    let mut stmt = ffi::sqlite3_next_stmt(db, ptr::null_mut());
                    while !stmt.is_null() {
                        if ffi::sqlite3_stmt_busy(stmt) != 0 {
                            return true;
                        }
                        stmt = ffi::sqlite3_next_stmt(db, stmt);
                    }
                }
                false
            }
        
            pub fn cache_flush(&mut self) -> Result<()> {
                crate::error::check(unsafe { ffi::sqlite3_db_cacheflush(self.db()) })
            }
        
            #[cfg(not(feature = "hooks"))]
            #[inline]
            fn remove_hooks(&mut self) {}
        
            #[cfg(not(feature = "preupdate_hook"))]
            #[inline]
            fn remove_preupdate_hook(&mut self) {}
        
            pub fn db_readonly(&self, db_name: super::DatabaseName<'_>) -> Result<bool> {
                let name = db_name.as_cstring()?;
                let r = unsafe { ffi::sqlite3_db_readonly(self.db, name.as_ptr()) };
                match r {
                    0 => Ok(false),
                    1 => Ok(true),
                    -1 => Err(Error::SqliteFailure(
                        ffi::Error::new(ffi::SQLITE_MISUSE),
                        Some(format!("{db_name:?} is not the name of a database")),
                    )),
                    _ => Err(error_from_sqlite_code(
                        r,
                        Some("Unexpected result".to_owned()),
                    )),
                }
            }
        
            #[cfg(feature = "modern_sqlite")] // 3.37.0
            pub fn txn_state(
                &self,
                db_name: Option<super::DatabaseName<'_>>,
            ) -> Result<super::transaction::TransactionState> {
                let r = if let Some(ref name) = db_name {
                    let name = name.as_cstring()?;
                    unsafe { ffi::sqlite3_txn_state(self.db, name.as_ptr()) }
                } else {
                    unsafe { ffi::sqlite3_txn_state(self.db, ptr::null()) }
                };
                match r {
                    0 => Ok(super::transaction::TransactionState::None),
                    1 => Ok(super::transaction::TransactionState::Read),
                    2 => Ok(super::transaction::TransactionState::Write),
                    -1 => Err(Error::SqliteFailure(
                        ffi::Error::new(ffi::SQLITE_MISUSE),
                        Some(format!("{db_name:?} is not the name of a valid schema")),
                    )),
                    _ => Err(error_from_sqlite_code(
                        r,
                        Some("Unexpected result".to_owned()),
                    )),
                }
            }
        
            #[inline]
            #[cfg(feature = "release_memory")]
            pub fn release_memory(&self) -> Result<()> {
                self.decode_result(unsafe { ffi::sqlite3_db_release_memory(self.db) })
            }
        
            #[cfg(feature = "modern_sqlite")] // 3.41.0
            pub fn is_interrupted(&self) -> bool {
                unsafe { ffi::sqlite3_is_interrupted(self.db) == 1 }
            }
        }
        
        impl Drop for InnerConnection {
            #[allow(unused_must_use)]
            #[inline]
            fn drop(&mut self) {
                self.close();
            }
        }
        
        // threading mode checks are not necessary (and do not work) on target
        // platforms that do not have threading (such as webassembly)
        #[cfg(target_arch = "wasm32")]
        fn ensure_safe_sqlite_threading_mode() -> Result<()> {
            Ok(())
        }
        
        #[cfg(not(any(target_arch = "wasm32")))]
        fn ensure_safe_sqlite_threading_mode() -> Result<()> {
            // Ensure SQLite was compiled in threadsafe mode.
            if unsafe { ffi::sqlite3_threadsafe() == 0 } {
                return Err(Error::SqliteSingleThreadedMode);
            }
        
            // Now we know SQLite is _capable_ of being in Multi-thread of Serialized mode,
            // but it's possible someone configured it to be in Single-thread mode
            // before calling into us. That would mean we're exposing an unsafe API via
            // a safe one (in Rust terminology).
            //
            // We can ask SQLite for a mutex and check for
            // the magic value 8. This isn't documented, but it's what SQLite
            // returns for its mutex allocation function in Single-thread mode.
            const SQLITE_SINGLETHREADED_MUTEX_MAGIC: usize = 8;
            let is_singlethreaded = unsafe {
                let mutex_ptr = ffi::sqlite3_mutex_alloc(0);
                let is_singlethreaded = mutex_ptr as usize == SQLITE_SINGLETHREADED_MUTEX_MAGIC;
                ffi::sqlite3_mutex_free(mutex_ptr);
                is_singlethreaded
            };
            if is_singlethreaded {
                Err(Error::SqliteSingleThreadedMode)
            } else {
                Ok(())
            }
        }
    }
    
    pub mod params
    {
        use ::
        {
            *,
        };
        /*
        use crate::{Result, Statement, ToSql};
        */
        
        /// Trait used for [sets of parameter][params] passed into SQL
        /// statements/queries.
        pub trait Params: Sealed {
            #[doc(hidden)]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()>;
        }
        
        impl Sealed for [&(dyn ToSql + Send + Sync); 0] {}
        impl Params for [&(dyn ToSql + Send + Sync); 0] {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.ensure_parameter_count(0)
            }
        }
        
        impl Sealed for &[&dyn ToSql] {}
        impl Params for &[&dyn ToSql] {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.bind_parameters(self)
            }
        }
        
        impl Sealed for &[(&str, &dyn ToSql)] {}
        impl Params for &[(&str, &dyn ToSql)] {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.bind_parameters_named(self)
            }
        }
        
        // Manual impls for the empty and singleton tuple, although the rest are covered
        // by macros.
        impl Sealed for () {}
        impl Params for () {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.ensure_parameter_count(0)
            }
        }
        
        // I'm pretty sure you could tweak the `single_tuple_impl` to accept this.
        impl<T: ToSql> Sealed for (T,) {}
        impl<T: ToSql> Params for (T,) {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.ensure_parameter_count(1)?;
                stmt.raw_bind_parameter(1, self.0)?;
                Ok(())
            }
        }
        
        macro_rules! single_tuple_impl {
            ($count:literal : $(($field:tt $ftype:ident)),* $(,)?) => {
                impl<$($ftype,)*> Sealed for ($($ftype,)*) where $($ftype: ToSql,)* {}
                impl<$($ftype,)*> Params for ($($ftype,)*) where $($ftype: ToSql,)* {
                    fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                        stmt.ensure_parameter_count($count)?;
                        $({
                            debug_assert!($field < $count);
                            stmt.raw_bind_parameter($field + 1, self.$field)?;
                        })+
                        Ok(())
                    }
                }
            }
        }
        
        single_tuple_impl!(2: (0 A), (1 B));
        single_tuple_impl!(3: (0 A), (1 B), (2 C));
        single_tuple_impl!(4: (0 A), (1 B), (2 C), (3 D));
        single_tuple_impl!(5: (0 A), (1 B), (2 C), (3 D), (4 E));
        single_tuple_impl!(6: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F));
        single_tuple_impl!(7: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G));
        single_tuple_impl!(8: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H));
        single_tuple_impl!(9: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I));
        single_tuple_impl!(10: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J));
        single_tuple_impl!(11: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K));
        single_tuple_impl!(12: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L));
        single_tuple_impl!(13: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M));
        single_tuple_impl!(14: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N));
        single_tuple_impl!(15: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N), (14 O));
        single_tuple_impl!(16: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N), (14 O), (15 P));
        
        macro_rules! impl_for_array_ref {
            ($($N:literal)+) => {$(
                // These are already generic, and there's a shedload of them, so lets
                // avoid the compile time hit from making them all inline for now.
                impl<T: ToSql + ?Sized> Sealed for &[&T; $N] {}
                impl<T: ToSql + ?Sized> Params for &[&T; $N] {
                    fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                        stmt.bind_parameters(self)
                    }
                }
                impl<T: ToSql + ?Sized> Sealed for &[(&str, &T); $N] {}
                impl<T: ToSql + ?Sized> Params for &[(&str, &T); $N] {
                    fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                        stmt.bind_parameters_named(self)
                    }
                }
                impl<T: ToSql> Sealed for [T; $N] {}
                impl<T: ToSql> Params for [T; $N] {
                    #[inline]
                    fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                        stmt.bind_parameters(&self)
                    }
                }
            )+};
        }
        
        impl_for_array_ref!(
            1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
            18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
        );
        
        /// Adapter type which allows any iterator over [`ToSql`] values to implement
        /// [`Params`].
        #[derive(Clone, Debug)]
        pub struct ParamsFromIter<I>(I);
        
        /// Constructor function for a [`ParamsFromIter`]. See its documentation for
        /// more.
        #[inline]
        pub fn params_from_iter<I>(iter: I) -> ParamsFromIter<I>
        where
            I: IntoIterator,
            I::Item: ToSql,
        {
            ParamsFromIter(iter)
        }
        
        impl<I> Sealed for ParamsFromIter<I>
        where
            I: IntoIterator,
            I::Item: ToSql,
        {
        }
        
        impl<I> Params for ParamsFromIter<I>
        where
            I: IntoIterator,
            I::Item: ToSql,
        {
            #[inline]
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
                stmt.bind_parameters(self.0)
            }
        }
    }
    
    pub mod pragma
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod raw_statement
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod row
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod statement
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod transaction
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod types
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod version
    {
        use ::
        {
            *,
        };
        /*
        */
    }
    
    pub mod util
    {
        use ::
        {
            *,
        };
        /*
        */
    } pub use util::SmallCString;
    
    // Number of cached prepared statements we'll hold on to.
    const STATEMENT_CACHE_DEFAULT_CAPACITY: usize = 16;
    
    /// A macro making it more convenient to longer lists of
    /// parameters as a `&[&dyn ToSql]`.
    #[macro_export]
    macro_rules! params {
        () => {
            &[] as &[&dyn $crate::ToSql]
        };
        ($($param:expr),+ $(,)?) => {
            &[$(&$param as &dyn $crate::ToSql),+] as &[&dyn $crate::ToSql]
        };
    }
    
    /// A macro making it more convenient to pass lists of named parameters
    /// as a `&[(&str, &dyn ToSql)]`.
    #[macro_export]
    macro_rules! named_params {
        () => {
            &[] as &[(&str, &dyn $crate::ToSql)]
        };
        ($($param_name:literal: $param_val:expr),+ $(,)?) => {
            &[$(($param_name, &$param_val as &dyn $crate::ToSql)),+] as &[(&str, &dyn $crate::ToSql)]
        };
    }
    
    /// Captured identifiers in SQL
    #[cfg(feature = "rusqlite-macros")]
    #[cfg_attr(docsrs, doc(cfg(feature = "rusqlite-macros")))]
    #[macro_export]
    macro_rules! prepare_and_bind {
        ($conn:expr, $sql:literal) => {{
            let mut stmt = $conn.prepare($sql)?;
            $crate::__bind!(stmt $sql);
            stmt
        }};
    }
    
    /// Captured identifiers in SQL
    #[cfg(feature = "rusqlite-macros")]
    #[cfg_attr(docsrs, doc(cfg(feature = "rusqlite-macros")))]
    #[macro_export]
    macro_rules! prepare_cached_and_bind {
        ($conn:expr, $sql:literal) => {{
            let mut stmt = $conn.prepare_cached($sql)?;
            $crate::__bind!(stmt $sql);
            stmt
        }};
    }
    
    /// A typedef of the result returned by many methods.
    pub type Result<T, E = Error> = result::Result<T, E>;
    
    /// See the [method documentation](#tymethod.optional).
    pub trait OptionalExtension<T> {
        fn optional(self) -> Result<Option<T>>;
    }
    
    impl<T> OptionalExtension<T> for Result<T> {
        fn optional(self) -> Result<Option<T>> {
            match self {
                Ok(value) => Ok(Some(value)),
                Err(Error::QueryReturnedNoRows) => Ok(None),
                Err(e) => Err(e),
            }
        }
    }
    
    unsafe fn errmsg_to_string(errmsg: *const c_char) -> String {
        CStr::from_ptr(errmsg).to_string_lossy().into_owned()
    }
    
    fn str_to_cstring(s: &str) -> Result<SmallCString> {
        Ok(SmallCString::new(s)?)
    }
    
    /// Returns `Ok((string ptr, len as c_int, SQLITE_STATIC | SQLITE_TRANSIENT))`
    /// normally.
    fn str_for_sqlite(s: &[u8]) -> Result<(*const c_char, c_int, ffi::sqlite3_destructor_type)> {
        let len = len_as_c_int(s.len())?;
        let (ptr, dtor_info) = if len != 0 {
            (s.as_ptr().cast::<c_char>(), ffi::SQLITE_TRANSIENT())
        } else {
            // Return a pointer guaranteed to live forever
            ("".as_ptr().cast::<c_char>(), ffi::SQLITE_STATIC())
        };
        Ok((ptr, len, dtor_info))
    }
    
    // Helper to cast to c_int safely, returning the correct error type if the cast
    // failed.
    fn len_as_c_int(len: usize) -> Result<c_int> {
        if len >= (c_int::MAX as usize) {
            Err(Error::SqliteFailure(
                ffi::Error::new(ffi::SQLITE_TOOBIG),
                None,
            ))
        } else {
            Ok(len as c_int)
        }
    }
    
    #[cfg(unix)]
    fn path_to_cstring(p: &Path) -> Result<CString> {
        use std::os::unix::ffi::OsStrExt;
        Ok(CString::new(p.as_os_str().as_bytes())?)
    }
    
    #[cfg(not(unix))]
    fn path_to_cstring(p: &Path) -> Result<CString> {
        let s = p.to_str().ok_or_else(|| Error::InvalidPath(p.to_owned()))?;
        Ok(CString::new(s)?)
    }
    
    /// Name for a database within a SQLite connection.
    #[derive(Copy, Clone, Debug)]
    pub enum DatabaseName<'a> {
        /// The main database.
        Main,
    
        /// The temporary database (e.g., any "CREATE TEMPORARY TABLE" tables).
        Temp,
    
        /// A database that has been attached via "ATTACH DATABASE ...".
        Attached(&'a str),
    }
    
    /// Shorthand for [`DatabaseName::Main`].
    pub const MAIN_DB: DatabaseName<'static> = DatabaseName::Main;
    
    /// Shorthand for [`DatabaseName::Temp`].
    pub const TEMP_DB: DatabaseName<'static> = DatabaseName::Temp;
    
    // Currently DatabaseName is only used by the backup and blob mods, so hide
    // this (private) impl to avoid dead code warnings.
    impl DatabaseName<'_> {
        #[inline]
        fn as_cstring(&self) -> Result<SmallCString> {
            use self::DatabaseName::{Attached, Main, Temp};
            match *self {
                Main => str_to_cstring("main"), // TODO C-string literals
                Temp => str_to_cstring("temp"),
                Attached(s) => str_to_cstring(s),
            }
        }
    }
    
    /// A connection to a SQLite database.
    pub struct Connection {
        db: RefCell<InnerConnection>,
        cache: StatementCache,
        transaction_behavior: TransactionBehavior,
    }
    
    unsafe impl Send for Connection {}
    
    impl Drop for Connection {
        #[inline]
        fn drop(&mut self) {
            self.flush_prepared_statement_cache();
        }
    }
    
    impl Connection {
        /// Open a new connection to a SQLite database. If a database does not exist
        /// at the path, one is created.
        #[inline]
        pub fn open<P: AsRef<Path>>(path: P) -> Result<Connection> {
            let flags = OpenFlags::default();
            Connection::open_with_flags(path, flags)
        }
    
        /// Open a new connection to an in-memory SQLite database.
        #[inline]
        pub fn open_in_memory() -> Result<Connection> {
            let flags = OpenFlags::default();
            Connection::open_in_memory_with_flags(flags)
        }
    
        /// Open a new connection to a SQLite database.
        #[inline]
        pub fn open_with_flags<P: AsRef<Path>>(path: P, flags: OpenFlags) -> Result<Connection> {
            let c_path = path_to_cstring(path.as_ref())?;
            InnerConnection::open_with_flags(&c_path, flags, None).map(|db| Connection {
                db: RefCell::new(db),
                cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
                transaction_behavior: TransactionBehavior::Deferred,
            })
        }
    
        /// Open a new connection to a SQLite database using the specific flags and
        /// vfs name.
        #[inline]
        pub fn open_with_flags_and_vfs<P: AsRef<Path>>(
            path: P,
            flags: OpenFlags,
            vfs: &str,
        ) -> Result<Connection> {
            let c_path = path_to_cstring(path.as_ref())?;
            let c_vfs = str_to_cstring(vfs)?;
            InnerConnection::open_with_flags(&c_path, flags, Some(&c_vfs)).map(|db| Connection {
                db: RefCell::new(db),
                cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
                transaction_behavior: TransactionBehavior::Deferred,
            })
        }
    
        /// Open a new connection to an in-memory SQLite database.
        #[inline]
        pub fn open_in_memory_with_flags(flags: OpenFlags) -> Result<Connection> {
            Connection::open_with_flags(":memory:", flags)
        }
    
        /// Open a new connection to an in-memory SQLite database using the specific
        /// flags and vfs name.
        #[inline]
        pub fn open_in_memory_with_flags_and_vfs(flags: OpenFlags, vfs: &str) -> Result<Connection> {
            Connection::open_with_flags_and_vfs(":memory:", flags, vfs)
        }
    
        /// Convenience method to run multiple SQL statements (that cannot take any
        /// parameters).
        pub fn execute_batch(&self, sql: &str) -> Result<()> {
            let mut sql = sql;
            while !sql.is_empty() {
                let stmt = self.prepare(sql)?;
                if !stmt.stmt.is_null() && stmt.step()? && cfg!(feature = "extra_check") {
                    // Some PRAGMA may return rows
                    return Err(Error::ExecuteReturnedResults);
                }
                let tail = stmt.stmt.tail();
                if tail == 0 || tail >= sql.len() {
                    break;
                }
                sql = &sql[tail..];
            }
            Ok(())
        }
    
        /// Convenience method to prepare and execute a single SQL statement.
        #[cfg(feature = "load_extension")]
        #[cfg_attr(docsrs, doc(cfg(feature = "load_extension")))]
        #[inline]
        pub unsafe fn load_extension_enable(&self) -> Result<()> {
            self.db.borrow_mut().enable_load_extension(1)
        }
    
        /// Disable loading of SQLite extensions.
        #[cfg(feature = "load_extension")]
        #[cfg_attr(docsrs, doc(cfg(feature = "load_extension")))]
        #[inline]
        pub fn load_extension_disable(&self) -> Result<()> {
            // It's always safe to turn off extension loading.
            unsafe { self.db.borrow_mut().enable_load_extension(0) }
        }
    
        /// Load the SQLite extension at `dylib_path`. `dylib_path` is passed
        /// through to `sqlite3_load_extension`, which may attempt OS-specific
        /// modifications if the file cannot be loaded directly (for example
        /// converting `"some/ext"` to `"some/ext.so"`, `"some\\ext.dll"`, ...).
        #[cfg(feature = "load_extension")]
        #[cfg_attr(docsrs, doc(cfg(feature = "load_extension")))]
        #[inline]
        pub unsafe fn load_extension<P: AsRef<Path>>(
            &self,
            dylib_path: P,
            entry_point: Option<&str>,
        ) -> Result<()> {
            self.db
                .borrow_mut()
                .load_extension(dylib_path.as_ref(), entry_point)
        }
    
        /// Get access to the underlying SQLite database connection handle.
        #[inline]
        pub unsafe fn handle(&self) -> *mut ffi::sqlite3 {
            self.db.borrow().db()
        }
    
        /// Create a `Connection` from a raw handle.
        #[inline]
        pub unsafe fn from_handle(db: *mut ffi::sqlite3) -> Result<Connection> {
            let db = InnerConnection::new(db, false);
            Ok(Connection {
                db: RefCell::new(db),
                cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
                transaction_behavior: TransactionBehavior::Deferred,
            })
        }
    
        /// Helper to register an SQLite extension written in Rust.
        #[cfg(feature = "loadable_extension")]
        #[cfg_attr(docsrs, doc(cfg(feature = "loadable_extension")))]
        pub unsafe fn extension_init2(
            db: *mut ffi::sqlite3,
            pz_err_msg: *mut *mut c_char,
            p_api: *mut ffi::sqlite3_api_routines,
            init: fn(Connection) -> Result<bool>,
        ) -> c_int {
            if p_api.is_null() {
                return ffi::SQLITE_ERROR;
            }
            match ffi::rusqlite_extension_init2(p_api)
                .map_err(Error::from)
                .and(Connection::from_handle(db))
                .and_then(init)
            {
                Err(err) => to_sqlite_error(&err, pz_err_msg),
                Ok(true) => ffi::SQLITE_OK_LOAD_PERMANENTLY,
                _ => ffi::SQLITE_OK,
            }
        }
    
        /// Create a `Connection` from a raw owned handle.
        #[inline]
        pub unsafe fn from_handle_owned(db: *mut ffi::sqlite3) -> Result<Connection> {
            let db = InnerConnection::new(db, true);
            Ok(Connection {
                db: RefCell::new(db),
                cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
                transaction_behavior: TransactionBehavior::Deferred,
            })
        }
    
        /// Get access to a handle that can be used to interrupt long-running
        /// queries from another thread.
        #[inline]
        pub fn get_interrupt_handle(&self) -> InterruptHandle {
            self.db.borrow().get_interrupt_handle()
        }
    
        #[inline]
        fn decode_result(&self, code: c_int) -> Result<()> {
            self.db.borrow().decode_result(code)
        }
    
        /// Return the number of rows modified, inserted or deleted by the most
        /// recently completed INSERT, UPDATE or DELETE statement on the database
        /// connection.
        #[inline]
        pub fn changes(&self) -> u64 {
            self.db.borrow().changes()
        }
    
        /// Return the total number of rows modified, inserted or deleted by all
        /// completed INSERT, UPDATE or DELETE statements since the database
        /// connection was opened, including those executed as part of trigger programs.
        #[inline]
        pub fn total_changes(&self) -> u64 {
            self.db.borrow().total_changes()
        }
    
        /// Test for auto-commit mode.
        /// Autocommit mode is on by default.
        #[inline]
        pub fn is_autocommit(&self) -> bool {
            self.db.borrow().is_autocommit()
        }
    
        /// Determine if all associated prepared statements have been reset.
        #[inline]
        pub fn is_busy(&self) -> bool {
            self.db.borrow().is_busy()
        }
    
        /// Flush caches to disk mid-transaction
        pub fn cache_flush(&self) -> Result<()> {
            self.db.borrow_mut().cache_flush()
        }
    
        /// Determine if a database is read-only
        pub fn is_readonly(&self, db_name: DatabaseName<'_>) -> Result<bool> {
            self.db.borrow().db_readonly(db_name)
        }
    
        /// Return the schema name for a database connection
        #[cfg(feature = "modern_sqlite")] // 3.39.0
        #[cfg_attr(docsrs, doc(cfg(feature = "modern_sqlite")))]
        pub fn db_name(&self, index: usize) -> Result<String> {
            unsafe {
                let db = self.handle();
                let name = ffi::sqlite3_db_name(db, index as c_int);
                if name.is_null() {
                    Err(Error::InvalidDatabaseIndex(index))
                } else {
                    Ok(CStr::from_ptr(name).to_str()?.to_owned())
                }
            }
        }
    
        /// Determine whether an interrupt is currently in effect
        #[cfg(feature = "modern_sqlite")] // 3.41.0
        #[cfg_attr(docsrs, doc(cfg(feature = "modern_sqlite")))]
        pub fn is_interrupted(&self) -> bool {
            self.db.borrow().is_interrupted()
        }
    }
    
    impl fmt::Debug for Connection {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Connection")
                .field("path", &self.path())
                .finish()
        }
    }
    
    /// Batch iterator
    #[derive(Debug)]
    pub struct Batch<'conn, 'sql> {
        conn: &'conn Connection,
        sql: &'sql str,
        tail: usize,
    }
    
    impl<'conn, 'sql> Batch<'conn, 'sql> {
        /// Constructor
        pub fn new(conn: &'conn Connection, sql: &'sql str) -> Batch<'conn, 'sql> {
            Batch { conn, sql, tail: 0 }
        }
    
        /// Iterates on each batch statements.
        ///
        /// Returns `Ok(None)` when batch is completed.
        #[allow(clippy::should_implement_trait)] // fallible iterator
        pub fn next(&mut self) -> Result<Option<Statement<'conn>>> {
            while self.tail < self.sql.len() {
                let sql = &self.sql[self.tail..];
                let next = self.conn.prepare(sql)?;
                let tail = next.stmt.tail();
                if tail == 0 {
                    self.tail = self.sql.len();
                } else {
                    self.tail += tail;
                }
                if next.stmt.is_null() {
                    continue;
                }
                return Ok(Some(next));
            }
            Ok(None)
        }
    }
    
    impl<'conn> Iterator for Batch<'conn, '_> {
        type Item = Result<Statement<'conn>>;
    
        fn next(&mut self) -> Option<Result<Statement<'conn>>> {
            self.next().transpose()
        }
    }
    
    bitflags::bitflags! {
        /// Flags for opening SQLite database connections.
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[repr(C)]
        pub struct OpenFlags: ::std::os::raw::c_int {
            /// The database is opened in read-only mode.
            /// If the database does not already exist, an error is returned.
            const SQLITE_OPEN_READ_ONLY = ffi::SQLITE_OPEN_READONLY;
            /// The database is opened for reading and writing if possible,
            /// or reading only if the file is write-protected by the operating system.
            const SQLITE_OPEN_READ_WRITE = ffi::SQLITE_OPEN_READWRITE;
            /// The database is created if it does not already exist
            const SQLITE_OPEN_CREATE = ffi::SQLITE_OPEN_CREATE;
            /// The filename can be interpreted as a URI if this flag is set.
            const SQLITE_OPEN_URI = ffi::SQLITE_OPEN_URI;
            /// The database will be opened as an in-memory database.
            const SQLITE_OPEN_MEMORY = ffi::SQLITE_OPEN_MEMORY;
            /// The new database connection will not use a per-connection mutex (the
            /// connection will use the "multi-thread" threading mode, in SQLite
            /// parlance).
            const SQLITE_OPEN_NO_MUTEX = ffi::SQLITE_OPEN_NOMUTEX;
            /// The new database connection will use a per-connection mutex -- the
            /// "serialized" threading mode, in SQLite parlance.
            const SQLITE_OPEN_FULL_MUTEX = ffi::SQLITE_OPEN_FULLMUTEX;
            /// The database is opened with shared cache enabled.
            const SQLITE_OPEN_SHARED_CACHE = 0x0002_0000;
            /// The database is opened shared cache disabled.
            const SQLITE_OPEN_PRIVATE_CACHE = 0x0004_0000;
            /// The database filename is not allowed to be a symbolic link. (3.31.0)
            const SQLITE_OPEN_NOFOLLOW = 0x0100_0000;
            /// Extended result codes. (3.37.0)
            const SQLITE_OPEN_EXRESCODE = 0x0200_0000;
        }
    }
    
    impl Default for OpenFlags {
        #[inline]
        fn default() -> OpenFlags {
            // Note: update the `Connection::open` and top-level `OpenFlags` docs if
            // you change these.
            OpenFlags::SQLITE_OPEN_READ_WRITE
                | OpenFlags::SQLITE_OPEN_CREATE
                | OpenFlags::SQLITE_OPEN_NO_MUTEX
                | OpenFlags::SQLITE_OPEN_URI
        }
    }
    
    bitflags::bitflags! {
        /// Prepare flags. See
        /// [sqlite3_prepare_v3](https://sqlite.org/c3ref/c_prepare_normalize.html) for details.
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
        #[repr(C)]
        pub struct PrepFlags: ::std::os::raw::c_uint {
            /// A hint to the query planner that the prepared statement will be retained for a long time and probably reused many times.
            const SQLITE_PREPARE_PERSISTENT = 0x01;
            /// Causes the SQL compiler to return an error (error code SQLITE_ERROR) if the statement uses any virtual tables.
            const SQLITE_PREPARE_NO_VTAB = 0x04;
        }
    }
    
    /// Allows interrupting a long-running computation.
    pub struct InterruptHandle {
        db_lock: Arc<Mutex<*mut ffi::sqlite3>>,
    }
    
    unsafe impl Send for InterruptHandle {}
    unsafe impl Sync for InterruptHandle {}
    
    impl InterruptHandle {
        /// Interrupt the query currently executing on another thread. This will
        /// cause that query to fail with a `SQLITE3_INTERRUPT` error.
        pub fn interrupt(&self) {
            let db_handle = self.db_lock.lock().unwrap();
            if !db_handle.is_null() {
                unsafe { ffi::sqlite3_interrupt(*db_handle) }
            }
        }
    }
}

pub mod structopt
{
    //! This crate defines the `StructOpt` trait and its custom derive.
    use ::
    {
        *,
    };
    /*
    pub use structopt_derive::*;
    
    use std::ffi::OsString;
    
    /// Re-export of clap
    pub use clap;
    */
    /*
    [dependencies]
    syn = { version = "1", features = ["full"] }
    quote = "1"
    proc-macro2 = "1"
    heck = "0.3.0"
    proc-macro-error = "0.2"
    */
    pub mod derive
    {
        //! This crate is custom derive for `StructOpt`.
        use ::
        {
            *,
        };
        /*
        extern crate proc_macro;
        use crate::{
            attrs::{sub_type, Attrs, CasingStyle, Kind, Parser, Ty},
            spanned::Sp,
        };
        
        use proc_macro2::{Span, TokenStream};
        use proc_macro_error::{call_site_error, filter_macro_errors, span_error};
        use quote::{quote, quote_spanned};
        use syn::{punctuated::Punctuated, spanned::Spanned, token::Comma, *};
        */
        pub mod attrs
        {
            use ::
            {
                *,
            };
            /*
            use crate::spanned::Sp;
            
            use std::env;
            
            use heck::{CamelCase, KebabCase, MixedCase, ShoutySnakeCase, SnakeCase};
            use proc_macro2::{Span, TokenStream};
            use proc_macro_error::{call_site_error, span_error};
            use quote::quote;
            use syn::{
                self, spanned::Spanned, AngleBracketedGenericArguments, Attribute, GenericArgument, Ident,
                LitStr, MetaNameValue, PathArguments, PathSegment, Type::Path, TypePath,
            };
            
            use crate::parse::*;
            */
            #[derive(Clone, Debug)]
            pub enum Kind {
                Arg(Sp<Ty>),
                Subcommand(Sp<Ty>),
                FlattenStruct,
                Skip,
            }
            
            #[derive(Copy, Clone, PartialEq, Debug)]
            pub enum Ty {
                Bool,
                Vec,
                Option,
                OptionOption,
                OptionVec,
                Other,
            }
            
            #[derive(Clone)]
            pub struct Method {
                name: Ident,
                args: TokenStream,
            }
            
            #[derive(Debug, PartialEq, Clone)]
            pub enum Parser {
                FromStr,
                TryFromStr,
                FromOsStr,
                TryFromOsStr,
                FromOccurrences,
            }
            
            /// Defines the casing for the attributes long representation.
            #[derive(Copy, Clone, Debug, PartialEq)]
            pub enum CasingStyle {
                /// Indicate word boundaries with uppercase letter, excluding the first word.
                Camel,
                /// Keep all letters lowercase and indicate word boundaries with hyphens.
                Kebab,
                /// Indicate word boundaries with uppercase letter, including the first word.
                Pascal,
                /// Keep all letters uppercase and indicate word boundaries with underscores.
                ScreamingSnake,
                /// Keep all letters lowercase and indicate word boundaries with underscores.
                Snake,
                /// Use the original attribute name defined in the code.
                Verbatim,
            }
            
            #[derive(Clone)]
            pub struct Attrs {
                name: Sp<String>,
                cased_name: String,
                casing: Sp<CasingStyle>,
                methods: Vec<Method>,
                parser: Sp<(Sp<Parser>, TokenStream)>,
                author: Option<(Ident, LitStr)>,
                about: Option<(Ident, LitStr)>,
                version: Option<(Ident, LitStr)>,
                no_version: Option<Ident>,
                has_custom_parser: bool,
                kind: Sp<Kind>,
            }
            
            impl Parser {
                fn from_ident(ident: Ident) -> Sp<Self> {
                    use Parser::*;
            
                    let p = |kind| Sp::new(kind, ident.span());
                    match &*ident.to_string() {
                        "from_str" => p(FromStr),
                        "try_from_str" => p(TryFromStr),
                        "from_os_str" => p(FromOsStr),
                        "try_from_os_str" => p(TryFromOsStr),
                        "from_occurrences" => p(FromOccurrences),
                        s => span_error!(ident.span(), "unsupported parser `{}`", s),
                    }
                }
            }
            
            impl CasingStyle {
                fn translate(&self, input: &str) -> String {
                    use CasingStyle::*;
            
                    match self {
                        Pascal => input.to_camel_case(),
                        Kebab => input.to_kebab_case(),
                        Camel => input.to_mixed_case(),
                        ScreamingSnake => input.to_shouty_snake_case(),
                        Snake => input.to_snake_case(),
                        Verbatim => String::from(input),
                    }
                }
            
                fn from_lit(name: LitStr) -> Sp<Self> {
                    use CasingStyle::*;
            
                    let normalized = name.value().to_camel_case().to_lowercase();
                    let cs = |kind| Sp::new(kind, name.span());
            
                    match normalized.as_ref() {
                        "camel" | "camelcase" => cs(Camel),
                        "kebab" | "kebabcase" => cs(Kebab),
                        "pascal" | "pascalcase" => cs(Pascal),
                        "screamingsnake" | "screamingsnakecase" => cs(ScreamingSnake),
                        "snake" | "snakecase" => cs(Snake),
                        "verbatim" | "verbatimcase" => cs(Verbatim),
                        s => span_error!(name.span(), "unsupported casing: `{}`", s),
                    }
                }
            }
            
            impl Attrs {
                fn new(name: Sp<String>, casing: Sp<CasingStyle>) -> Self {
                    let cased_name = casing.translate(&name);
            
                    Self {
                        name,
                        cased_name,
                        casing,
                        methods: vec![],
                        parser: Sp::call_site((
                            Sp::call_site(Parser::TryFromStr),
                            quote!(::std::str::FromStr::from_str),
                        )),
                        about: None,
                        author: None,
                        version: None,
                        no_version: None,
            
                        has_custom_parser: false,
                        kind: Sp::call_site(Kind::Arg(Sp::call_site(Ty::Other))),
                    }
                }
            
                /// push `.method("str literal")`
                fn push_str_method(&mut self, name: Sp<String>, arg: Sp<String>) {
                    match (&**name, &**arg) {
                        ("name", _) => {
                            self.cased_name = self.casing.translate(&arg);
                            self.name = arg;
                        }
                        _ => self.methods.push(Method {
                            name: name.as_ident(),
                            args: quote!(#arg),
                        }),
                    }
                }
            
                fn push_attrs(&mut self, attrs: &[Attribute]) {
                    use crate::parse::StructOptAttr::*;
            
                    fn from_lit_or_env(
                        ident: Ident,
                        lit: Option<LitStr>,
                        env_var: &str,
                    ) -> Option<(Ident, LitStr)> {
                        let lit = lit.unwrap_or_else(|| {
                            let gen = env::var(env_var)
                                .unwrap_or_else(|_|
                                 span_error!(ident.span(), "`{}` environment variable is not defined, use `{} = \"{}\"` to set it manually", env_var, env_var, env_var));
                            LitStr::new(&gen, Span::call_site())
                        });
            
                        Some((ident, lit))
                    }
            
                    for attr in parse_structopt_attributes(attrs) {
                        match attr {
                            Short(ident) => {
                                let cased_name = Sp::call_site(self.cased_name.clone());
                                self.push_str_method(ident.into(), cased_name);
                            }
            
                            Long(ident) => {
                                let cased_name = Sp::call_site(self.cased_name.clone());
                                self.push_str_method(ident.into(), cased_name);
                            }
            
                            Subcommand(ident) => {
                                let ty = Sp::call_site(Ty::Other);
                                let kind = Sp::new(Kind::Subcommand(ty), ident.span());
                                self.set_kind(kind);
                            }
            
                            Flatten(ident) => {
                                let kind = Sp::new(Kind::FlattenStruct, ident.span());
                                self.set_kind(kind);
                            }
            
                            Skip(ident) => {
                                let kind = Sp::new(Kind::Skip, ident.span());
                                self.set_kind(kind);
                            }
            
                            NoVersion(ident) => self.no_version = Some(ident),
            
                            About(ident, about) => {
                                self.about = from_lit_or_env(ident, about, "CARGO_PKG_DESCRIPTION")
                            }
            
                            Author(ident, author) => {
                                self.author =
                                    from_lit_or_env(ident, author, "CARGO_PKG_AUTHORS").map(|(ident, lit)| {
                                        let value = lit.value().replace(":", ", ");
                                        (ident.clone(), LitStr::new(&value, ident.span()))
                                    })
                            }
            
                            Version(ident, version) => self.version = Some((ident, version)),
            
                            NameLitStr(name, lit) => {
                                self.push_str_method(name.into(), lit.into());
                            }
            
                            NameExpr(name, expr) => self.methods.push(Method {
                                name: name.into(),
                                args: quote!(#expr),
                            }),
            
                            MethodCall(name, args) => self.methods.push(Method {
                                name: name.into(),
                                args: quote!(#(#args),*),
                            }),
            
                            RenameAll(_, casing_lit) => {
                                self.casing = CasingStyle::from_lit(casing_lit);
                                self.cased_name = self.casing.translate(&self.name);
                            }
            
                            Parse(ident, spec) => {
                                self.has_custom_parser = true;
            
                                self.parser = match spec.parse_func {
                                    None => {
                                        use crate::attrs::Parser::*;
            
                                        let parser: Sp<_> = Parser::from_ident(spec.kind).into();
                                        let function = match *parser {
                                            FromStr | FromOsStr => quote!(::std::convert::From::from),
                                            TryFromStr => quote!(::std::str::FromStr::from_str),
                                            TryFromOsStr => span_error!(
                                                parser.span(),
                                                "cannot omit parser function name with `try_from_os_str`"
                                            ),
                                            FromOccurrences => quote!({ |v| v as _ }),
                                        };
                                        Sp::new((parser, function), ident.span())
                                    }
            
                                    Some(func) => {
                                        let parser: Sp<_> = Parser::from_ident(spec.kind).into();
                                        match func {
                                            syn::Expr::Path(_) => {
                                                Sp::new((parser, quote!(#func)), ident.span())
                                            }
                                            _ => span_error!(
                                                func.span(),
                                                "`parse` argument must be a function path"
                                            ),
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            
                fn push_doc_comment(&mut self, attrs: &[Attribute], name: &str) {
                    let doc_comments = attrs
                        .iter()
                        .filter_map(|attr| {
                            if attr.path.is_ident("doc") {
                                attr.parse_meta().ok()
                            } else {
                                None
                            }
                        })
                        .filter_map(|attr| {
                            use crate::Lit::*;
                            use crate::Meta::*;
                            if let NameValue(MetaNameValue {
                                path, lit: Str(s), ..
                            }) = attr
                            {
                                if !path.is_ident("doc") {
                                    return None;
                                }
                                let value = s.value();
            
                                let text = value
                                    .trim_start_matches("//!")
                                    .trim_start_matches("///")
                                    .trim_start_matches("/*!")
                                    .trim_start_matches("/**")
                                    .trim_end_matches("*/")
                                    .trim();
                                if text.is_empty() {
                                    Some("\n\n".to_string())
                                } else {
                                    Some(text.to_string())
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
            
                    if doc_comments.is_empty() {
                        return;
                    }
            
                    let merged_lines = doc_comments
                        .join(" ")
                        .split('\n')
                        .map(str::trim)
                        .map(str::to_string)
                        .collect::<Vec<_>>()
                        .join("\n");
            
                    let expected_doc_comment_split = if let Some(content) = doc_comments.get(1) {
                        (doc_comments.len() > 2) && (content == "\n\n")
                    } else {
                        false
                    };
            
                    if expected_doc_comment_split {
                        let long_name = Sp::call_site(format!("long_{}", name));
            
                        self.methods.push(Method {
                            name: long_name.as_ident(),
                            args: quote!(#merged_lines),
                        });
                        
                        let short_arg = doc_comments
                            .first()
                            .map(|s| s.trim())
                            .map_or("", |s| s.trim_end_matches('.'));
            
                        self.methods.push(Method {
                            name: Ident::new(name, Span::call_site()),
                            args: quote!(#short_arg),
                        });
                    } else {
                        self.methods.push(Method {
                            name: Ident::new(name, Span::call_site()),
                            args: quote!(#merged_lines),
                        });
                    }
                }
            
                pub fn from_struct(
                    attrs: &[Attribute],
                    name: Sp<String>,
                    argument_casing: Sp<CasingStyle>,
                ) -> Self {
                    let mut res = Self::new(name, argument_casing);
                    res.push_attrs(attrs);
                    res.push_doc_comment(attrs, "about");
            
                    if res.has_custom_parser {
                        span_error!(
                            res.parser.span(),
                            "parse attribute is only allowed on fields"
                        );
                    }
                    match &*res.kind {
                        Kind::Subcommand(_) => {
                            span_error!(res.kind.span(), "subcommand is only allowed on fields")
                        }
                        Kind::FlattenStruct => {
                            span_error!(res.kind.span(), "flatten is only allowed on fields")
                        }
                        Kind::Skip => span_error!(res.kind.span(), "skip is only allowed on fields"),
                        Kind::Arg(_) => res,
                    }
                }
            
                fn ty_from_field(ty: &syn::Type) -> Sp<Ty> {
                    let t = |kind| Sp::new(kind, ty.span());
                    if let Path(TypePath {
                        path: syn::Path { ref segments, .. },
                        ..
                    }) = *ty
                    {
                        match segments.iter().last().unwrap().ident.to_string().as_str() {
                            "bool" => t(Ty::Bool),
                            "Option" => sub_type(ty)
                                .map(Attrs::ty_from_field)
                                .map(|ty| match *ty {
                                    Ty::Option => t(Ty::OptionOption),
                                    Ty::Vec => t(Ty::OptionVec),
                                    _ => t(Ty::Option),
                                })
                                .unwrap_or(t(Ty::Option)),
            
                            "Vec" => t(Ty::Vec),
                            _ => t(Ty::Other),
                        }
                    } else {
                        t(Ty::Other)
                    }
                }
            
                pub fn from_field(field: &syn::Field, struct_casing: Sp<CasingStyle>) -> Self {
                    let name = field.ident.clone().unwrap();
                    let mut res = Self::new(name.into(), struct_casing);
                    res.push_doc_comment(&field.attrs, "help");
                    res.push_attrs(&field.attrs);
            
                    match &*res.kind {
                        Kind::FlattenStruct => {
                            if res.has_custom_parser {
                                span_error!(
                                    res.parser.span(),
                                    "parse attribute is not allowed for flattened entry"
                                );
                            }
                            if !res.methods.is_empty() {
                                span_error!(
                                    res.kind.span(),
                                    "methods and doc comments are not allowed for flattened entry"
                                );
                            }
                        }
                        Kind::Subcommand(_) => {
                            if res.has_custom_parser {
                                span_error!(
                                    res.parser.span(),
                                    "parse attribute is not allowed for subcommand"
                                );
                            }
                            if let Some(m) = res.methods.iter().find(|m| m.name != "help") {
                                span_error!(
                                    m.name.span(),
                                    "methods in attributes are not allowed for subcommand"
                                );
                            }
            
                            let ty = Self::ty_from_field(&field.ty);
                            match *ty {
                                Ty::OptionOption => {
                                    span_error!(
                                        ty.span(),
                                        "Option<Option<T>> type is not allowed for subcommand"
                                    );
                                }
                                Ty::OptionVec => {
                                    span_error!(
                                        ty.span(),
                                        "Option<Vec<T>> type is not allowed for subcommand"
                                    );
                                }
                                _ => (),
                            }
            
                            res.kind = Sp::new(Kind::Subcommand(ty), res.kind.span());
                        }
                        Kind::Skip => {
                            if let Some(m) = res.methods.iter().find(|m| m.name != "help") {
                                span_error!(m.name.span(), "methods are not allowed for skipped fields");
                            }
                        }
                        Kind::Arg(orig_ty) => {
                            let mut ty = Self::ty_from_field(&field.ty);
                            if res.has_custom_parser {
                                match *ty {
                                    Ty::Option | Ty::Vec => (),
                                    _ => ty = Sp::new(Ty::Other, ty.span()),
                                }
                            }
            
                            match *ty {
                                Ty::Bool => {
                                    if let Some(m) = res.find_method("default_value") {
                                        span_error!(m.name.span(), "default_value is meaningless for bool")
                                    }
                                    if let Some(m) = res.find_method("required") {
                                        span_error!(m.name.span(), "required is meaningless for bool")
                                    }
                                }
                                Ty::Option => {
                                    if let Some(m) = res.find_method("default_value") {
                                        span_error!(m.name.span(), "default_value is meaningless for Option")
                                    }
                                    if let Some(m) = res.find_method("required") {
                                        span_error!(m.name.span(), "required is meaningless for Option")
                                    }
                                }
                                Ty::OptionOption => {
                                    // If it's a positional argument.
                                    if !(res.has_method("long") || res.has_method("short")) {
                                        span_error!(
                                            ty.span(),
                                            "Option<Option<T>> type is meaningless for positional argument"
                                        )
                                    }
                                }
                                Ty::OptionVec => {
                                    // If it's a positional argument.
                                    if !(res.has_method("long") || res.has_method("short")) {
                                        span_error!(
                                            ty.span(),
                                            "Option<Vec<T>> type is meaningless for positional argument"
                                        )
                                    }
                                }
            
                                _ => (),
                            }
                            res.kind = Sp::new(Kind::Arg(ty), orig_ty.span());
                        }
                    }
            
                    res
                }
            
                fn set_kind(&mut self, kind: Sp<Kind>) {
                    if let Kind::Arg(_) = *self.kind {
                        self.kind = kind;
                    } else {
                        span_error!(
                            kind.span(),
                            "subcommand, flatten and skip cannot be used together"
                        );
                    }
                }
            
                pub fn has_method(&self, name: &str) -> bool {
                    self.find_method(name).is_some()
                }
            
                pub fn find_method(&self, name: &str) -> Option<&Method> {
                    self.methods.iter().find(|m| m.name == name)
                }
            
                /// generate methods from attributes on top of struct or enum
                pub fn top_level_methods(&self) -> TokenStream {
                    let version = match (&self.no_version, &self.version) {
                        (Some(no_version), Some(_)) => span_error!(
                            no_version.span(),
                            "`no_version` and `version = \"version\"` can't be used together"
                        ),
            
                        (None, Some((_, version))) => quote!(.version(#version)),
            
                        (None, None) => {
                            let version = env::var("CARGO_PKG_VERSION").unwrap_or_else(|_|{
                                call_site_error!("`CARGO_PKG_VERSION` environment variable is not defined, use `version = \"version\" to set it manually or `no_version` to not set it at all")
                            });
                            quote!(.version(#version))
                        }
            
                        (Some(_), None) => TokenStream::new(),
                    };
            
                    let version = Some(version);
                    let author = self
                        .author
                        .as_ref()
                        .map(|(_, version)| quote!(.author(#version)));
                    let about = self
                        .about
                        .as_ref()
                        .map(|(_, version)| quote!(.about(#version)));
            
                    let methods = self
                        .methods
                        .iter()
                        .map(|&Method { ref name, ref args }| quote!( .#name(#args) ))
                        .chain(version)
                        .chain(author)
                        .chain(about);
            
                    quote!( #(#methods)* )
                }
            
                /// generate methods on top of a field
                pub fn field_methods(&self) -> TokenStream {
                    let methods = self
                        .methods
                        .iter()
                        .map(|&Method { ref name, ref args }| quote!( .#name(#args) ));
            
                    quote!( #(#methods)* )
                }
            
                pub fn cased_name(&self) -> String {
                    self.cased_name.to_string()
                }
            
                pub fn parser(&self) -> &(Sp<Parser>, TokenStream) {
                    &self.parser
                }
            
                pub fn kind(&self) -> Sp<Kind> {
                    self.kind.clone()
                }
            
                pub fn casing(&self) -> Sp<CasingStyle> {
                    self.casing.clone()
                }
            }
            
            pub fn sub_type(t: &syn::Type) -> Option<&syn::Type> {
                let segs = match *t {
                    Path(TypePath {
                        path: syn::Path { ref segments, .. },
                        ..
                    }) => segments,
                    _ => return None,
                };
                match *segs.iter().last().unwrap() {
                    PathSegment {
                        arguments:
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }),
                        ..
                    } if args.len() == 1 => {
                        if let GenericArgument::Type(ref ty) = args[0] {
                            Some(ty)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        }
        
        pub mod parse
        {
            use ::
            {
                *,
            };
            /*
            use std::iter::FromIterator;
            
            use proc_macro_error::{span_error, ResultExt};
            use syn::{
                self, parenthesized,
                parse::{Parse, ParseStream},
                parse2,
                punctuated::Punctuated,
                spanned::Spanned,
                Attribute, Expr, ExprLit, Ident, Lit, LitBool, LitStr, Token,
            };
            */
            pub struct StructOptAttributes {
                pub paren_token: syn::token::Paren,
                pub attrs: Punctuated<StructOptAttr, Token![,]>,
            }
            
            impl Parse for StructOptAttributes {
                fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
                    let content;
            
                    Ok(StructOptAttributes {
                        paren_token: parenthesized!(content in input),
                        attrs: content.parse_terminated(StructOptAttr::parse)?,
                    })
                }
            }
            
            pub enum StructOptAttr {
                // single-identifier attributes
                Short(Ident),
                Long(Ident),
                Flatten(Ident),
                Subcommand(Ident),
                Skip(Ident),
                NoVersion(Ident),
            
                // ident [= "string literal"]
                About(Ident, Option<LitStr>),
                Author(Ident, Option<LitStr>),
            
                // ident = "string literal"
                Version(Ident, LitStr),
                RenameAll(Ident, LitStr),
                NameLitStr(Ident, LitStr),
            
                // parse(parser_kind [= parser_func])
                Parse(Ident, ParserSpec),
            
                // ident = arbitrary_expr
                NameExpr(Ident, Expr),
            
                // ident(arbitrary_expr,*)
                MethodCall(Ident, Vec<Expr>),
            }
            
            impl Parse for StructOptAttr {
                fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
                    use self::StructOptAttr::*;
            
                    let name: Ident = input.parse()?;
                    let name_str = name.to_string();
            
                    if input.peek(Token![=]) {
                        let assign_token = input.parse::<Token![=]>()?; // skip '='
            
                        if input.peek(LitStr) {
                            let lit: LitStr = input.parse()?;
                            let lit_str = lit.value();
            
                            let check_empty_lit = |s| {
                                if lit_str.is_empty() {
                                    span_error!(lit.span(), "`#[structopt({} = \"\") is deprecated in structopt 3.0, now it's default behavior", s);
                                }
                            };
            
                            match &*name_str.to_string() {
                                "rename_all" => Ok(RenameAll(name, lit)),
            
                                "version" => {
                                    check_empty_lit("version");
                                    Ok(Version(name, lit))
                                }
            
                                "author" => {
                                    check_empty_lit("author");
                                    Ok(Author(name, Some(lit)))
                                }
            
                                "about" => {
                                    check_empty_lit("about");
                                    Ok(About(name, Some(lit)))
                                }
            
                                _ => Ok(NameLitStr(name, lit)),
                            }
                        } else {
                            match input.parse::<Expr>() {
                                Ok(expr) => Ok(NameExpr(name, expr)),
                                Err(_) => span_error! {
                                    assign_token.span(),
                                    "expected `string literal` or `expression` after `=`"
                                },
                            }
                        }
                    } else if input.peek(syn::token::Paren) {
                        let nested;
                        parenthesized!(nested in input);
            
                        match name_str.as_ref() {
                            "parse" => {
                                let parser_specs: Punctuated<ParserSpec, Token![,]> =
                                    nested.parse_terminated(ParserSpec::parse)?;
            
                                if parser_specs.len() == 1 {
                                    Ok(Parse(name, parser_specs[0].clone()))
                                } else {
                                    span_error!(name.span(), "parse must have exactly one argument")
                                }
                            }
            
                            "raw" => {
                                match nested.parse::<LitBool>() {
                                    Ok(bool_token) => {
                                        let expr = ExprLit { attrs: vec![], lit: Lit::Bool(bool_token) };
                                        let expr = Expr::Lit(expr);
                                        Ok(MethodCall(name, vec![expr]))
                                    }
            
                                    Err(_) => span_error!(name.span(),
                                        "`#[structopt(raw(...))` attributes are deprecated in structopt 3.0, only `raw(true)` and `raw(false)` are allowed")
                                }
                            }
            
                            _ => {
                                let method_args: Punctuated<_, Token![,]> = nested.parse_terminated(Expr::parse)?;
                                Ok(MethodCall(name, Vec::from_iter(method_args)))
                            }
                        }
                    } else {
                        match name_str.as_ref() {
                            "long" => Ok(Long(name)),
                            "short" => Ok(Short(name)),
                            "flatten" => Ok(Flatten(name)),
                            "subcommand" => Ok(Subcommand(name)),
                            "skip" => Ok(Skip(name)),
                            "no_version" => Ok(NoVersion(name)),
            
                            "about" => (Ok(About(name, None))),
                            "author" => (Ok(Author(name, None))),
            
                            "version" => {
                                span_error!(name.span(),
                                "#[structopt(version)] is invalid attribute, structopt 3.0 inherits version from Cargo.toml by default, no attribute needed")
                            },
            
                            _ => span_error!(name.span(), "unexpected attribute: {}", name_str),
                        }
                    }
                }
            }
            
            #[derive(Clone)]
            pub struct ParserSpec {
                pub kind: Ident,
                pub eq_token: Option<Token![=]>,
                pub parse_func: Option<Expr>,
            }
            
            impl Parse for ParserSpec {
                fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
                    let kind = input
                        .parse()
                        .map_err(|_| input.error("parser specification must start with identifier"))?;
                    let eq_token = input.parse()?;
                    let parse_func = match eq_token {
                        None => None,
                        Some(_) => Some(input.parse()?),
                    };
                    Ok(ParserSpec {
                        kind,
                        eq_token,
                        parse_func,
                    })
                }
            }
            
            pub fn parse_structopt_attributes(all_attrs: &[Attribute]) -> Vec<StructOptAttr> {
                all_attrs
                    .iter()
                    .filter(|attr| attr.path.is_ident("structopt"))
                    .flat_map(|attr| {
                        let attrs: StructOptAttributes = parse2(attr.tokens.clone())
                            .map_err(|e| match &*e.to_string() {
                                "unexpected end of input, expected parentheses" => {
                                    let span = attr.path.span();
                                    let patch_msg = "expected parentheses after `structopt`";
                                    syn::Error::new(span, patch_msg)
                                }
                                _ => e,
                            })
                            .unwrap_or_exit();
                        attrs.attrs
                    })
                    .collect()
            }
        }
        
        pub mod spanned
        {
            use ::
            {
                *,
            };
            /*
            use proc_macro2::{Ident, Span, TokenStream};
            use quote::{quote_spanned, ToTokens};
            use std::ops::{Deref, DerefMut};
            use syn::LitStr;
            */
            /// An entity with a span attached.
            #[derive(Debug, Clone)]
            pub struct Sp<T> {
                span: Span,
                val: T,
            }
            
            impl<T> Sp<T> {
                pub fn new(val: T, span: Span) -> Self {
                    Sp { val, span }
                }
            
                pub fn call_site(val: T) -> Self {
                    Sp {
                        val,
                        span: Span::call_site(),
                    }
                }
            
                pub fn span(&self) -> Span {
                    self.span.clone()
                }
            }
            
            impl<T: ToString> Sp<T> {
                pub fn as_ident(&self) -> Ident {
                    Ident::new(&self.to_string(), self.span.clone())
                }
            }
            
            impl<T> Deref for Sp<T> {
                type Target = T;
            
                fn deref(&self) -> &T {
                    &self.val
                }
            }
            
            impl<T> DerefMut for Sp<T> {
                fn deref_mut(&mut self) -> &mut T {
                    &mut self.val
                }
            }
            
            impl From<Ident> for Sp<String> {
                fn from(ident: Ident) -> Self {
                    Sp {
                        val: ident.to_string(),
                        span: ident.span(),
                    }
                }
            }
            
            impl From<LitStr> for Sp<String> {
                fn from(lit: LitStr) -> Self {
                    Sp {
                        val: lit.value(),
                        span: lit.span(),
                    }
                }
            }
            
            impl<'a> From<Sp<&'a str>> for Sp<String> {
                fn from(sp: Sp<&'a str>) -> Self {
                    Sp::new(sp.val.into(), sp.span)
                }
            }
            
            impl<T: PartialEq> PartialEq for Sp<T> {
                fn eq(&self, other: &Sp<T>) -> bool {
                    self.val == other.val
                }
            }
            
            impl<T: AsRef<str>> AsRef<str> for Sp<T> {
                fn as_ref(&self) -> &str {
                    self.val.as_ref()
                }
            }
            
            impl<T: ToTokens> ToTokens for Sp<T> {
                fn to_tokens(&self, stream: &mut TokenStream) {
                    let val = &self.val;
                    let quoted = quote_spanned!(self.span=> #val);
                    stream.extend(quoted);
                }
            }
        }
        
        /// Default casing style for generated arguments.
        const DEFAULT_CASING: CasingStyle = CasingStyle::Kebab;
        
        /// Output for the `gen_xxx()` methods were we need more than a simple stream of tokens.
        struct GenOutput {
            tokens: TokenStream,
            attrs: Attrs,
        }
        
        /// Generates the `StructOpt` impl.
        #[proc_macro_derive(StructOpt, attributes(structopt))]
        pub fn structopt(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            filter_macro_errors! {
                let input: DeriveInput = syn::parse(input).unwrap();
                let gen = impl_structopt(&input);
                gen.into()
            }
        }
        
        /// Generate a block of code to add arguments/subcommands corresponding to
        /// the `fields` to an app.
        fn gen_augmentation(
            fields: &Punctuated<Field, Comma>,
            app_var: &Ident,
            parent_attribute: &Attrs,
        ) -> TokenStream {
            let mut subcmds = fields.iter().filter_map(|field| {
                let attrs = Attrs::from_field(field, parent_attribute.casing());
                if let Kind::Subcommand(ty) = &*attrs.kind() {
                    let subcmd_type = match (**ty, sub_type(&field.ty)) {
                        (Ty::Option, Some(sub_type)) => sub_type,
                        _ => &field.ty,
                    };
                    let required = if **ty == Ty::Option {
                        quote!()
                    } else {
                        quote! {
                            let #app_var = #app_var.setting(
                                ::structopt::clap::AppSettings::SubcommandRequiredElseHelp
                            );
                        }
                    };
        
                    let span = field.span();
                    let ts = quote! {
                        let #app_var = <#subcmd_type>::augment_clap( #app_var );
                        #required
                    };
                    Some((span, ts))
                } else {
                    None
                }
            });
        
            let subcmd = subcmds.next().map(|(_, ts)| ts);
            if let Some((span, _)) = subcmds.next() {
                span_error!(
                    span,
                    "nested subcommands are not allowed, that's the second"
                );
            }
        
            let args = fields.iter().filter_map(|field| {
                let attrs = Attrs::from_field(field, parent_attribute.casing());
                match &*attrs.kind() {
                    Kind::Subcommand(_) | Kind::Skip => None,
                    Kind::FlattenStruct => {
                        let ty = &field.ty;
                        Some(quote! {
                            let #app_var = <#ty>::augment_clap(#app_var);
                            let #app_var = if <#ty>::is_subcommand() {
                                #app_var.setting(::structopt::clap::AppSettings::SubcommandRequiredElseHelp)
                            } else {
                                #app_var
                            };
                        })
                    }
                    Kind::Arg(ty) => {
                        let convert_type = match **ty {
                            Ty::Vec | Ty::Option => sub_type(&field.ty).unwrap_or(&field.ty),
                            Ty::OptionOption | Ty::OptionVec => sub_type(&field.ty).and_then(sub_type).unwrap_or(&field.ty),
                            _ => &field.ty,
                        };
        
                        let occurrences = *attrs.parser().0 == Parser::FromOccurrences;
        
                        let (parser, f) = attrs.parser();
                        let validator = match **parser {
                            Parser::TryFromStr => quote! {
                                .validator(|s| {
                                    #f(&s)
                                    .map(|_: #convert_type| ())
                                    .map_err(|e| e.to_string())
                                })
                            },
                            Parser::TryFromOsStr => quote! {
                                .validator_os(|s| #f(&s).map(|_: #convert_type| ()))
                            },
                            _ => quote!(),
                        };
        
                        let modifier = match **ty {
                            Ty::Bool => quote!( .takes_value(false).multiple(false) ),
                            Ty::Option => quote!( .takes_value(true).multiple(false) #validator ),
                            Ty::OptionOption => {
                                quote! ( .takes_value(true).multiple(false).min_values(0).max_values(1) #validator )
                            }
                            Ty::OptionVec => {
                                quote! ( .takes_value(true).multiple(true).min_values(0) #validator )
                            }
                            Ty::Vec => quote!( .takes_value(true).multiple(true) #validator ),
                            Ty::Other if occurrences => quote!( .takes_value(false).multiple(true) ),
                            Ty::Other => {
                                let required = !attrs.has_method("default_value");
                                quote!( .takes_value(true).multiple(false).required(#required) #validator )
                            }
                        };
        
                        let name = attrs.cased_name();
                        let methods = attrs.field_methods();
        
                        Some(quote! {
                            let #app_var = #app_var.arg(
                                ::structopt::clap::Arg::with_name(#name)
                                    #modifier
                                    #methods
                            );
                        })
                    }
                }
            });
        
            quote! {{
                #( #args )*
                #subcmd
                #app_var
            }}
        }
        
        fn gen_constructor(fields: &Punctuated<Field, Comma>, parent_attribute: &Attrs) -> TokenStream {
            let fields = fields.iter().map(|field| {
                let attrs = Attrs::from_field(field, parent_attribute.casing());
                let field_name = field.ident.as_ref().unwrap();
                let kind = attrs.kind();
                match &*kind {
                    Kind::Subcommand(ty) => {
                        let subcmd_type = match (**ty, sub_type(&field.ty)) {
                            (Ty::Option, Some(sub_type)) => sub_type,
                            _ => &field.ty,
                        };
                        let unwrapper = match **ty {
                            Ty::Option => quote!(),
                            _ => quote!( .unwrap() ),
                        };
                        quote!(#field_name: <#subcmd_type>::from_subcommand(matches.subcommand())#unwrapper)
                    }
                    Kind::FlattenStruct => quote!(#field_name: ::structopt::StructOpt::from_clap(matches)),
                    Kind::Skip => quote_spanned!(kind.span()=> #field_name: Default::default()),
                    Kind::Arg(ty) => {
                        use crate::attrs::Parser::*;
                        let (parser, f) = attrs.parser();
                        let (value_of, values_of, parse) = match **parser {
                            FromStr => (quote!(value_of), quote!(values_of), f.clone()),
                            TryFromStr => (
                                quote!(value_of),
                                quote!(values_of),
                                quote!(|s| #f(s).unwrap()),
                            ),
                            FromOsStr => (quote!(value_of_os), quote!(values_of_os), f.clone()),
                            TryFromOsStr => (
                                quote!(value_of_os),
                                quote!(values_of_os),
                                quote!(|s| #f(s).unwrap()),
                            ),
                            FromOccurrences => (quote!(occurrences_of), quote!(), f.clone()),
                        };
        
                        let occurrences = *attrs.parser().0 == Parser::FromOccurrences;
                        let name = attrs.cased_name();
                        let field_value = match **ty {
                            Ty::Bool => quote!(matches.is_present(#name)),
                            Ty::Option => quote! {
                                matches.#value_of(#name)
                                    .map(#parse)
                            },
                            Ty::OptionOption => quote! {
                                if matches.is_present(#name) {
                                    Some(matches.#value_of(#name).map(#parse))
                                } else {
                                    None
                                }
                            },
                            Ty::OptionVec => quote! {
                                if matches.is_present(#name) {
                                    Some(matches.#values_of(#name)
                                         .map(|v| v.map(#parse).collect())
                                         .unwrap_or_else(Vec::new))
                                } else {
                                    None
                                }
                            },
                            Ty::Vec => quote! {
                                matches.#values_of(#name)
                                    .map(|v| v.map(#parse).collect())
                                    .unwrap_or_else(Vec::new)
                            },
                            Ty::Other if occurrences => quote! {
                                #parse(matches.#value_of(#name))
                            },
                            Ty::Other => quote! {
                                matches.#value_of(#name)
                                    .map(#parse)
                                    .unwrap()
                            },
                        };
        
                        quote!( #field_name: #field_value )
                    }
                }
            });
        
            quote! {{
                #( #fields ),*
            }}
        }
        
        fn gen_from_clap(
            struct_name: &Ident,
            fields: &Punctuated<Field, Comma>,
            parent_attribute: &Attrs,
        ) -> TokenStream {
            let field_block = gen_constructor(fields, parent_attribute);
        
            quote! {
                fn from_clap(matches: &::structopt::clap::ArgMatches) -> Self {
                    #struct_name #field_block
                }
            }
        }
        
        fn gen_clap(attrs: &[Attribute]) -> GenOutput {
            let name = std::env::var("CARGO_PKG_NAME").ok().unwrap_or_default();
        
            let attrs = Attrs::from_struct(attrs, Sp::call_site(name), Sp::call_site(DEFAULT_CASING));
            let tokens = {
                let name = attrs.cased_name();
                let methods = attrs.top_level_methods();
        
                quote!(::structopt::clap::App::new(#name)#methods)
            };
        
            GenOutput { tokens, attrs }
        }
        
        fn gen_clap_struct(struct_attrs: &[Attribute]) -> GenOutput {
            let initial_clap_app_gen = gen_clap(struct_attrs);
            let clap_tokens = initial_clap_app_gen.tokens;
        
            let augmented_tokens = quote! {
                fn clap<'a, 'b>() -> ::structopt::clap::App<'a, 'b> {
                    let app = #clap_tokens;
                    Self::augment_clap(app)
                }
            };
        
            GenOutput {
                tokens: augmented_tokens,
                attrs: initial_clap_app_gen.attrs,
            }
        }
        
        fn gen_augment_clap(fields: &Punctuated<Field, Comma>, parent_attribute: &Attrs) -> TokenStream {
            let app_var = Ident::new("app", Span::call_site());
            let augmentation = gen_augmentation(fields, &app_var, parent_attribute);
            quote! {
                pub fn augment_clap<'a, 'b>(
                    #app_var: ::structopt::clap::App<'a, 'b>
                ) -> ::structopt::clap::App<'a, 'b> {
                    #augmentation
                }
            }
        }
        
        fn gen_clap_enum(enum_attrs: &[Attribute]) -> GenOutput {
            let initial_clap_app_gen = gen_clap(enum_attrs);
            let clap_tokens = initial_clap_app_gen.tokens;
        
            let tokens = quote! {
                fn clap<'a, 'b>() -> ::structopt::clap::App<'a, 'b> {
                    let app = #clap_tokens
                        .setting(::structopt::clap::AppSettings::SubcommandRequiredElseHelp);
                    Self::augment_clap(app)
                }
            };
        
            GenOutput {
                tokens,
                attrs: initial_clap_app_gen.attrs,
            }
        }
        
        fn gen_augment_clap_enum(
            variants: &Punctuated<Variant, Comma>,
            parent_attribute: &Attrs,
        ) -> TokenStream {
            use syn::Fields::*;
        
            let subcommands = variants.iter().map(|variant| {
                let attrs = Attrs::from_struct(
                    &variant.attrs,
                    variant.ident.clone().into(),
                    parent_attribute.casing(),
                );
                let app_var = Ident::new("subcommand", Span::call_site());
                let arg_block = match variant.fields {
                    Named(ref fields) => gen_augmentation(&fields.named, &app_var, &attrs),
                    Unit => quote!( #app_var ),
                    Unnamed(FieldsUnnamed { ref unnamed, .. }) if unnamed.len() == 1 => {
                        let ty = &unnamed[0];
                        quote! {
                            {
                                let #app_var = <#ty>::augment_clap(#app_var);
                                if <#ty>::is_subcommand() {
                                    #app_var.setting(
                                        ::structopt::clap::AppSettings::SubcommandRequiredElseHelp
                                    )
                                } else {
                                    #app_var
                                }
                            }
                        }
                    }
                    Unnamed(..) => call_site_error!("{}: tuple enums are not supported", variant.ident),
                };
        
                let name = attrs.cased_name();
                let from_attrs = attrs.top_level_methods();
        
                quote! {
                    .subcommand({
                        let #app_var = ::structopt::clap::SubCommand::with_name(#name);
                        let #app_var = #arg_block;
                        #app_var#from_attrs
                    })
                }
            });
        
            quote! {
                pub fn augment_clap<'a, 'b>(
                    app: ::structopt::clap::App<'a, 'b>
                ) -> ::structopt::clap::App<'a, 'b> {
                    app #( #subcommands )*
                }
            }
        }
        
        fn gen_from_clap_enum(name: &Ident) -> TokenStream {
            quote! {
                fn from_clap(matches: &::structopt::clap::ArgMatches) -> Self {
                    <#name>::from_subcommand(matches.subcommand())
                        .unwrap()
                }
            }
        }
        
        fn gen_from_subcommand(
            name: &Ident,
            variants: &Punctuated<Variant, Comma>,
            parent_attribute: &Attrs,
        ) -> TokenStream {
            use syn::Fields::*;
        
            let match_arms = variants.iter().map(|variant| {
                let attrs = Attrs::from_struct(
                    &variant.attrs,
                    variant.ident.clone().into(),
                    parent_attribute.casing(),
                );
                let sub_name = attrs.cased_name();
                let variant_name = &variant.ident;
                let constructor_block = match variant.fields {
                    Named(ref fields) => gen_constructor(&fields.named, &attrs),
                    Unit => quote!(),
                    Unnamed(ref fields) if fields.unnamed.len() == 1 => {
                        let ty = &fields.unnamed[0];
                        quote!( ( <#ty as ::structopt::StructOpt>::from_clap(matches) ) )
                    }
                    Unnamed(..) => call_site_error!("{}: tuple enums are not supported", variant.ident),
                };
        
                quote! {
                    (#sub_name, Some(matches)) =>
                        Some(#name :: #variant_name #constructor_block)
                }
            });
        
            quote! {
                pub fn from_subcommand<'a, 'b>(
                    sub: (&'b str, Option<&'b ::structopt::clap::ArgMatches<'a>>)
                ) -> Option<Self> {
                    match sub {
                        #( #match_arms ),*,
                        _ => None
                    }
                }
            }
        }
        
        fn impl_structopt_for_struct(
            name: &Ident,
            fields: &Punctuated<Field, Comma>,
            attrs: &[Attribute],
        ) -> TokenStream {
            let basic_clap_app_gen = gen_clap_struct(attrs);
            let augment_clap = gen_augment_clap(fields, &basic_clap_app_gen.attrs);
            let from_clap = gen_from_clap(name, fields, &basic_clap_app_gen.attrs);
            let paw_impl = gen_paw_impl(name);
        
            let clap_tokens = basic_clap_app_gen.tokens;
            quote! {
                #[allow(unused_variables)]
                impl ::structopt::StructOpt for #name {
                    #clap_tokens
                    #from_clap
                }
        
                #[allow(dead_code, unreachable_code)]
                #[doc(hidden)]
                impl #name {
                    #augment_clap
                    pub fn is_subcommand() -> bool { false }
                }
        
                #paw_impl
            }
        }
        
        fn impl_structopt_for_enum(
            name: &Ident,
            variants: &Punctuated<Variant, Comma>,
            attrs: &[Attribute],
        ) -> TokenStream {
            let basic_clap_app_gen = gen_clap_enum(attrs);
        
            let augment_clap = gen_augment_clap_enum(variants, &basic_clap_app_gen.attrs);
            let from_clap = gen_from_clap_enum(name);
            let from_subcommand = gen_from_subcommand(name, variants, &basic_clap_app_gen.attrs);
            let paw_impl = gen_paw_impl(name);
        
            let clap_tokens = basic_clap_app_gen.tokens;
            quote! {
                impl ::structopt::StructOpt for #name {
                    #clap_tokens
                    #from_clap
                }
        
                #[allow(unused_variables, dead_code, unreachable_code)]
                #[doc(hidden)]
                impl #name {
                    #augment_clap
                    #from_subcommand
                    pub fn is_subcommand() -> bool { true }
                }
        
                #paw_impl
            }
        }
        
        fn impl_structopt(input: &DeriveInput) -> TokenStream {
            use syn::Data::*;
        
            let struct_name = &input.ident;
            match input.data {
                Struct(DataStruct {
                    fields: syn::Fields::Named(ref fields),
                    ..
                }) => impl_structopt_for_struct(struct_name, &fields.named, &input.attrs),
                Enum(ref e) => impl_structopt_for_enum(struct_name, &e.variants, &input.attrs),
                _ => call_site_error!("structopt only supports non-tuple structs and enums"),
            }
        }
    }
    /// A struct that is converted from command line arguments.
    pub trait StructOpt {
        /// Returns the corresponding `clap::App`.
        fn clap<'a, 'b>() -> clap::App<'a, 'b>;
    
        /// Creates the struct from `clap::ArgMatches`.
        fn from_clap(matches: &clap::ArgMatches<'_>) -> Self;
    
        /// Gets the struct from the command line arguments.
        fn from_args() -> Self
        where
            Self: Sized,
        {
            Self::from_clap(&Self::clap().get_matches())
        }
    
        /// Gets the struct from any iterator such as a `Vec` of your making.
        fn from_iter<I>(iter: I) -> Self
        where
            Self: Sized,
            I: IntoIterator,
            I::Item: Into<OsString> + Clone,
        {
            Self::from_clap(&Self::clap().get_matches_from(iter))
        }
    
        /// Gets the struct from any iterator such as a `Vec` of your making.
        fn from_iter_safe<I>(iter: I) -> Result<Self, clap::Error>
        where
            Self: Sized,
            I: IntoIterator,
            I::Item: Into<OsString> + Clone,
        {
            Ok(Self::from_clap(&Self::clap().get_matches_from_safe(iter)?))
        }
    }
}

pub mod uuid
{
    //! Generate and parse universally unique identifiers (UUIDs).
    use ::
    {
        *,
    };
    /*
    */
    pub mod builder
    {
        //! A Builder type for [`Uuid`]s.
        use ::
        {
            *,
        };
        /*
        use crate::{error::*, timestamp, Bytes, Uuid, Variant, Version};
        */
        /// A builder for creating a UUID.
        #[allow(missing_copy_implementations)]
        #[derive(Debug)]
        pub struct Builder(Uuid);
        
        impl Uuid {
            /// The 'nil UUID' (all zeros).
            pub const fn nil() -> Self {
                Uuid::from_bytes([0; 16])
            }
        
            /// The 'max UUID' (all ones).
            #[cfg(uuid_unstable)]
            pub const fn max() -> Self {
                Uuid::from_bytes([0xFF; 16])
            }
        
            /// Creates a UUID from four field values.
            pub const fn from_fields(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Uuid {
                Uuid::from_bytes([
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
        
            /// Creates a UUID from four field values in little-endian order.
            pub const fn from_fields_le(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Uuid {
                Uuid::from_bytes([
                    d1 as u8,
                    (d1 >> 8) as u8,
                    (d1 >> 16) as u8,
                    (d1 >> 24) as u8,
                    (d2) as u8,
                    (d2 >> 8) as u8,
                    d3 as u8,
                    (d3 >> 8) as u8,
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
        
            /// Creates a UUID from a 128bit value.
            pub const fn from_u128(v: u128) -> Self {
                Uuid::from_bytes([
                    (v >> 120) as u8,
                    (v >> 112) as u8,
                    (v >> 104) as u8,
                    (v >> 96) as u8,
                    (v >> 88) as u8,
                    (v >> 80) as u8,
                    (v >> 72) as u8,
                    (v >> 64) as u8,
                    (v >> 56) as u8,
                    (v >> 48) as u8,
                    (v >> 40) as u8,
                    (v >> 32) as u8,
                    (v >> 24) as u8,
                    (v >> 16) as u8,
                    (v >> 8) as u8,
                    v as u8,
                ])
            }
        
            /// Creates a UUID from a 128bit value in little-endian order.
            pub const fn from_u128_le(v: u128) -> Self {
                Uuid::from_bytes([
                    v as u8,
                    (v >> 8) as u8,
                    (v >> 16) as u8,
                    (v >> 24) as u8,
                    (v >> 32) as u8,
                    (v >> 40) as u8,
                    (v >> 48) as u8,
                    (v >> 56) as u8,
                    (v >> 64) as u8,
                    (v >> 72) as u8,
                    (v >> 80) as u8,
                    (v >> 88) as u8,
                    (v >> 96) as u8,
                    (v >> 104) as u8,
                    (v >> 112) as u8,
                    (v >> 120) as u8,
                ])
            }
        
            /// Creates a UUID from two 64bit values.
            pub const fn from_u64_pair(high_bits: u64, low_bits: u64) -> Self {
                Uuid::from_bytes([
                    (high_bits >> 56) as u8,
                    (high_bits >> 48) as u8,
                    (high_bits >> 40) as u8,
                    (high_bits >> 32) as u8,
                    (high_bits >> 24) as u8,
                    (high_bits >> 16) as u8,
                    (high_bits >> 8) as u8,
                    high_bits as u8,
                    (low_bits >> 56) as u8,
                    (low_bits >> 48) as u8,
                    (low_bits >> 40) as u8,
                    (low_bits >> 32) as u8,
                    (low_bits >> 24) as u8,
                    (low_bits >> 16) as u8,
                    (low_bits >> 8) as u8,
                    low_bits as u8,
                ])
            }
        
            /// Creates a UUID using the supplied bytes.
            pub fn from_slice(b: &[u8]) -> Result<Uuid, Error> {
                if b.len() != 16 {
                    return Err(Error(ErrorKind::ByteLength { len: b.len() }));
                }
        
                let mut bytes: Bytes = [0; 16];
                bytes.copy_from_slice(b);
                Ok(Uuid::from_bytes(bytes))
            }
        
            /// Creates a UUID using the supplied bytes in little endian order.
            pub fn from_slice_le(b: &[u8]) -> Result<Uuid, Error> {
                if b.len() != 16 {
                    return Err(Error(ErrorKind::ByteLength { len: b.len() }));
                }
        
                let mut bytes: Bytes = [0; 16];
                bytes.copy_from_slice(b);
                Ok(Uuid::from_bytes_le(bytes))
            }
        
            /// Creates a UUID using the supplied bytes.
            pub const fn from_bytes(bytes: Bytes) -> Uuid {
                Uuid(bytes)
            }
        
            /// Creates a UUID using the supplied bytes in little endian order.
            pub const fn from_bytes_le(b: Bytes) -> Uuid {
                Uuid([
                    b[3], b[2], b[1], b[0], b[5], b[4], b[7], b[6], b[8], b[9], b[10], b[11], b[12], b[13],
                    b[14], b[15],
                ])
            }
        
            /// Creates a reference to a UUID from a reference to the supplied bytes.
            pub fn from_bytes_ref(bytes: &Bytes) -> &Uuid {
                // SAFETY: `Bytes` and `Uuid` have the same ABI
                unsafe { &*(bytes as *const Bytes as *const Uuid) }
            }
        }
        
        impl Builder {
            /// Creates a `Builder` using the supplied bytes.
            pub const fn from_bytes(b: Bytes) -> Self {
                Builder(Uuid::from_bytes(b))
            }
        
            /// Creates a `Builder` using the supplied bytes in little endian order.
            pub const fn from_bytes_le(b: Bytes) -> Self {
                Builder(Uuid::from_bytes_le(b))
            }
        
            /// Creates a `Builder` for a version 1 UUID using the supplied timestamp and node ID.
            pub const fn from_rfc4122_timestamp(ticks: u64, counter: u16, node_id: &[u8; 6]) -> Self {
                Builder(timestamp::encode_rfc4122_timestamp(ticks, counter, node_id))
            }
        
            /// Creates a `Builder` for a version 3 UUID using the supplied MD5 hashed bytes.
            pub const fn from_md5_bytes(md5_bytes: Bytes) -> Self {
                Builder(Uuid::from_bytes(md5_bytes))
                    .with_variant(Variant::RFC4122)
                    .with_version(Version::Md5)
            }
        
            /// Creates a `Builder` for a version 4 UUID using the supplied random bytes.
            pub const fn from_random_bytes(random_bytes: Bytes) -> Self {
                Builder(Uuid::from_bytes(random_bytes))
                    .with_variant(Variant::RFC4122)
                    .with_version(Version::Random)
            }
        
            /// Creates a `Builder` for a version 5 UUID using the supplied SHA-1 hashed bytes.
            pub const fn from_sha1_bytes(sha1_bytes: Bytes) -> Self {
                Builder(Uuid::from_bytes(sha1_bytes))
                    .with_variant(Variant::RFC4122)
                    .with_version(Version::Sha1)
            }
        
            /// Creates a `Builder` for a version 6 UUID using the supplied timestamp and node ID.
            #[cfg(uuid_unstable)]
            pub const fn from_sorted_rfc4122_timestamp(
                ticks: u64,
                counter: u16,
                node_id: &[u8; 6],
            ) -> Self {
                Builder(timestamp::encode_sorted_rfc4122_timestamp(
                    ticks, counter, node_id,
                ))
            }
        
            /// Creates a `Builder` for a version 7 UUID using the supplied Unix timestamp and random bytes.
            #[cfg(uuid_unstable)]
            pub const fn from_unix_timestamp_millis(millis: u64, random_bytes: &[u8; 10]) -> Self {
                Builder(timestamp::encode_unix_timestamp_millis(
                    millis,
                    random_bytes,
                ))
            }
        
            /// Creates a `Builder` for a version 8 UUID using the supplied user-defined bytes.
            #[cfg(uuid_unstable)]
            pub const fn from_custom_bytes(custom_bytes: Bytes) -> Self {
                Builder::from_bytes(custom_bytes)
                    .with_variant(Variant::RFC4122)
                    .with_version(Version::Custom)
            }
        
            /// Creates a `Builder` using the supplied bytes.
            pub fn from_slice(b: &[u8]) -> Result<Self, Error> {
                Ok(Builder(Uuid::from_slice(b)?))
            }
        
            /// Creates a `Builder` using the supplied bytes in little endian order.
            pub fn from_slice_le(b: &[u8]) -> Result<Self, Error> {
                Ok(Builder(Uuid::from_slice_le(b)?))
            }
        
            /// Creates a `Builder` from four field values.
            pub const fn from_fields(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Self {
                Builder(Uuid::from_fields(d1, d2, d3, d4))
            }
        
            /// Creates a `Builder` from four field values.
            pub const fn from_fields_le(d1: u32, d2: u16, d3: u16, d4: &[u8; 8]) -> Self {
                Builder(Uuid::from_fields_le(d1, d2, d3, d4))
            }
        
            /// Creates a `Builder` from a 128bit value.
            pub const fn from_u128(v: u128) -> Self {
                Builder(Uuid::from_u128(v))
            }
        
            /// Creates a UUID from a 128bit value in little-endian order.
            pub const fn from_u128_le(v: u128) -> Self {
                Builder(Uuid::from_u128_le(v))
            }
        
            /// Creates a `Builder` with an initial [`Uuid::nil`].
            pub const fn nil() -> Self {
                Builder(Uuid::nil())
            }
        
            /// Specifies the variant of the UUID.
            pub fn set_variant(&mut self, v: Variant) -> &mut Self {
                *self = Builder(self.0).with_variant(v);
                self
            }
        
            /// Specifies the variant of the UUID.
            pub const fn with_variant(mut self, v: Variant) -> Self {
                let byte = (self.0).0[8];
        
                (self.0).0[8] = match v {
                    Variant::NCS => byte & 0x7f,
                    Variant::RFC4122 => (byte & 0x3f) | 0x80,
                    Variant::Microsoft => (byte & 0x1f) | 0xc0,
                    Variant::Future => byte | 0xe0,
                };
        
                self
            }
        
            /// Specifies the version number of the UUID.
            pub fn set_version(&mut self, v: Version) -> &mut Self {
                *self = Builder(self.0).with_version(v);
                self
            }
        
            /// Specifies the version number of the UUID.
            pub const fn with_version(mut self, v: Version) -> Self {
                (self.0).0[6] = ((self.0).0[6] & 0x0f) | ((v as u8) << 4);
        
                self
            }
        
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid {
                &self.0
            }
        
            /// Convert the builder into a [`Uuid`].
            pub const fn into_uuid(self) -> Uuid {
                self.0
            }
        }
    }
    
    pub mod error
    {
        use ::
        {
            *,
        };
        /*
        */
        /// A general error that can occur when working with UUIDs.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct Error(pub(crate) ErrorKind);
        
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub(crate) enum ErrorKind {
            /// Invalid character in the [`Uuid`] string.
            Char { character: char, index: usize },
            /// A simple [`Uuid`] didn't contain 32 characters.
            SimpleLength { len: usize },
            /// A byte array didn't contain 16 bytes
            ByteLength { len: usize },
            /// A hyphenated [`Uuid`] didn't contain 5 groups
            GroupCount { count: usize },
            /// A hyphenated [`Uuid`] had a group that wasn't the right length
            GroupLength {
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
        
        impl<'a> InvalidUuid<'a> {
            /// Converts the lightweight error type into detailed diagnostics.
            pub fn into_err(self) -> Error {
                let input_str = match std::str::from_utf8(self.0) {
                    Ok(s) => s,
                    Err(_) => return Error(ErrorKind::InvalidUTF8),
                };
        
                let (uuid_str, offset, simple) = match input_str.as_bytes() {
                    [b'{', s @ .., b'}'] => (s, 1, false),
                    [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..] => {
                        (s, "urn:uuid:".len(), false)
                    }
                    s => (s, 0, true),
                };
        
                let mut hyphen_count = 0;
                let mut group_bounds = [0; 4];
                
                let uuid_str = unsafe { ::str::from_utf8_unchecked(uuid_str) };
        
                for (index, character) in uuid_str.char_indices() {
                    let byte = character as u8;
                    if character as u32 - byte as u32 > 0 {
                        // Multibyte char
                        return Error(ErrorKind::Char {
                            character,
                            index: index + offset + 1,
                        });
                    } else if byte == b'-' {
                        // While we search, also count group breaks
                        if hyphen_count < 4 {
                            group_bounds[hyphen_count] = index;
                        }
                        hyphen_count += 1;
                    } else if !matches!(byte, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') {
                        // Non-hex char
                        return Error(ErrorKind::Char {
                            character: byte as char,
                            index: index + offset + 1,
                        });
                    }
                }
        
                if hyphen_count == 0 && simple {
                    Error(ErrorKind::SimpleLength {
                        len: input_str.len(),
                    })
                } else if hyphen_count != 4 {
                    Error(ErrorKind::GroupCount {
                        count: hyphen_count + 1,
                    })
                } else {
                    const BLOCK_STARTS: [usize; 5] = [0, 9, 14, 19, 24];
                    for i in 0..4 {
                        if group_bounds[i] != BLOCK_STARTS[i + 1] - 1 {
                            return Error(ErrorKind::GroupLength {
                                group: i,
                                len: group_bounds[i] - BLOCK_STARTS[i],
                                index: offset + BLOCK_STARTS[i] + 1,
                            });
                        }
                    }
                    
                    Error(ErrorKind::GroupLength {
                        group: 4,
                        len: input_str.len() - BLOCK_STARTS[4],
                        index: offset + BLOCK_STARTS[4] + 1,
                    })
                }
            }
        }
        
        // NOTE: This impl is part of the public API. Breaking changes to it should be carefully considered
        impl fmt::Display for Error {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.0 {
                    ErrorKind::Char {
                        character, index, ..
                    } => {
                        write!(f, "invalid character: expected an optional prefix of `urn:uuid:` followed by [0-9a-fA-F-], found `{}` at {}", character, index)
                    }
                    ErrorKind::SimpleLength { len } => {
                        write!(
                            f,
                            "invalid length: expected length 32 for simple format, found {}",
                            len
                        )
                    }
                    ErrorKind::ByteLength { len } => {
                        write!(f, "invalid length: expected 16 bytes, found {}", len)
                    }
                    ErrorKind::GroupCount { count } => {
                        write!(f, "invalid group count: expected 5, found {}", count)
                    }
                    ErrorKind::GroupLength { group, len, .. } => {
                        let expected = [8, 4, 4, 4, 12][group];
                        write!(
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
        
        impl ::error::Error for Error {}
    }
    
    pub mod parser
    {
        //! [`Uuid`] parsing constructs and utilities.
        use ::
        {
            *,
        };
        /*
        use crate::{
            error::*,
            std::{convert::TryFrom, str},
            Uuid,
        };
        */
        impl str::FromStr for Uuid {
            type Err = Error;
        
            fn from_str(uuid_str: &str) -> Result<Self, Self::Err> {
                Uuid::parse_str(uuid_str)
            }
        }
        
        impl TryFrom<&'_ str> for Uuid {
            type Error = Error;
        
            fn try_from(uuid_str: &'_ str) -> Result<Self, Self::Error> {
                Uuid::parse_str(uuid_str)
            }
        }
        
        impl Uuid {
            /// Parses a `Uuid` from a string of hexadecimal digits with optional
            /// hyphens.
            pub fn parse_str(input: &str) -> Result<Uuid, Error> {
                try_parse(input.as_bytes())
                    .map(Uuid::from_bytes)
                    .map_err(InvalidUuid::into_err)
            }
        
            /// Parses a `Uuid` from a string of hexadecimal digits with optional
            /// hyphens.
            pub const fn try_parse(input: &str) -> Result<Uuid, Error> {
                Self::try_parse_ascii(input.as_bytes())
            }
        
            /// Parses a `Uuid` from a string of hexadecimal digits with optional
            /// hyphens.
            pub const fn try_parse_ascii(input: &[u8]) -> Result<Uuid, Error> {
                match try_parse(input) {
                    Ok(bytes) => Ok(Uuid::from_bytes(bytes)),
                    Err(_) => Err(Error(ErrorKind::Other)),
                }
            }
        }
        
        const fn try_parse(input: &[u8]) -> Result<[u8; 16], InvalidUuid> {
            let result = match (input.len(), input) {
                (32, s) => parse_simple(s),
                (36, s)
                | (38, [b'{', s @ .., b'}'])
                | (45, [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..]) => {
                    parse_hyphenated(s)
                }
                _ => Err(()),
            };
        
            match result {
                Ok(b) => Ok(b),
                Err(()) => Err(InvalidUuid(input)),
            }
        }
        
        #[inline]
        const fn parse_simple(s: &[u8]) -> Result<[u8; 16], ()> {
            if s.len() != 32 {
                return Err(());
            }
        
            let mut buf: [u8; 16] = [0; 16];
            let mut i = 0;
        
            while i < 16 {
                let h1 = HEX_TABLE[s[i * 2] as usize];
                let h2 = HEX_TABLE[s[i * 2 + 1] as usize];
                
                if h1 | h2 == 0xff {
                    return Err(());
                }
                
                buf[i] = SHL4_TABLE[h1 as usize] | h2;
                i += 1;
            }
        
            Ok(buf)
        }
        
        #[inline]
        const fn parse_hyphenated(s: &[u8]) -> Result<[u8; 16], ()> {
            // This length check here removes all other bounds
            // checks in this function
            if s.len() != 36 {
                return Err(());
            }
        
            // We look at two hex-encoded values (4 chars) at a time because
            // that's the size of the smallest group in a hyphenated UUID.
            // The indexes we're interested in are:
            //
            // uuid     : 936da01f-9abd-4d9d-80c7-02af85c822a8
            //            |   |   ||   ||   ||   ||   |   |
            // hyphens  : |   |   8|  13|  18|  23|   |   |
            // positions: 0   4    9   14   19   24  28  32
        
            // First, ensure the hyphens appear in the right places
            match [s[8], s[13], s[18], s[23]] {
                [b'-', b'-', b'-', b'-'] => {}
                _ => return Err(()),
            }
        
            let positions: [u8; 8] = [0, 4, 9, 14, 19, 24, 28, 32];
            let mut buf: [u8; 16] = [0; 16];
            let mut j = 0;
        
            while j < 8 {
                let i = positions[j];
        
                // The decoding here is the same as the simple case
                // We're just dealing with two values instead of one
                let h1 = HEX_TABLE[s[i as usize] as usize];
                let h2 = HEX_TABLE[s[(i + 1) as usize] as usize];
                let h3 = HEX_TABLE[s[(i + 2) as usize] as usize];
                let h4 = HEX_TABLE[s[(i + 3) as usize] as usize];
        
                if h1 | h2 | h3 | h4 == 0xff {
                    return Err(());
                }
        
                buf[j * 2] = SHL4_TABLE[h1 as usize] | h2;
                buf[j * 2 + 1] = SHL4_TABLE[h3 as usize] | h4;
                j += 1;
            }
        
            Ok(buf)
        }
        
        const HEX_TABLE: &[u8; 256] = &{
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
        
        const SHL4_TABLE: &[u8; 256] = &{
            let mut buf = [0; 256];
            let mut i: u8 = 0;
        
            loop {
                buf[i as usize] = i.wrapping_shl(4);
        
                if i == 255 {
                    break buf;
                }
        
                i += 1;
            }
        };
    }
    
    pub mod fmt
    {
        //! Adapters for alternative string formats.
        use ::
        {
            *,
        };
        /*
        use crate::{
            std::{borrow::Borrow, fmt, ptr, str},
            Uuid, Variant,
        };
        */
        impl std::fmt::Debug for Uuid {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(self, f)
            }
        }
        
        impl fmt::Display for Uuid {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(self, f)
            }
        }
        
        impl fmt::Display for Variant {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match *self {
                    Variant::NCS => write!(f, "NCS"),
                    Variant::RFC4122 => write!(f, "RFC4122"),
                    Variant::Microsoft => write!(f, "Microsoft"),
                    Variant::Future => write!(f, "Future"),
                }
            }
        }
        
        impl fmt::LowerHex for Uuid {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(self.as_hyphenated(), f)
            }
        }
        
        impl fmt::UpperHex for Uuid {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::UpperHex::fmt(self.as_hyphenated(), f)
            }
        }
        
        /// Format a [`Uuid`] as a hyphenated string, like
        /// `67e55044-10b1-426f-9247-bb680e5fe0c8`.
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct Hyphenated(Uuid);
        
        /// Format a [`Uuid`] as a simple string, like
        /// `67e5504410b1426f9247bb680e5fe0c8`.
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct Simple(Uuid);
        
        /// Format a [`Uuid`] as a URN string, like
        /// `urn:uuid:67e55044-10b1-426f-9247-bb680e5fe0c8`.
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct Urn(Uuid);
        
        /// Format a [`Uuid`] as a braced hyphenated string, like
        /// `{67e55044-10b1-426f-9247-bb680e5fe0c8}`.
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct Braced(Uuid);
        
        impl Uuid {
            /// Get a [`Hyphenated`] formatter.
            #[inline]
            pub const fn hyphenated(self) -> Hyphenated {
                Hyphenated(self)
            }
        
            /// Get a borrowed [`Hyphenated`] formatter.
            #[inline]
            pub fn as_hyphenated(&self) -> &Hyphenated {
                // SAFETY: `Uuid` and `Hyphenated` have the same ABI
                unsafe { &*(self as *const Uuid as *const Hyphenated) }
            }
        
            /// Get a [`Simple`] formatter.
            #[inline]
            pub const fn simple(self) -> Simple {
                Simple(self)
            }
        
            /// Get a borrowed [`Simple`] formatter.
            #[inline]
            pub fn as_simple(&self) -> &Simple {
                // SAFETY: `Uuid` and `Simple` have the same ABI
                unsafe { &*(self as *const Uuid as *const Simple) }
            }
        
            /// Get a [`Urn`] formatter.
            #[inline]
            pub const fn urn(self) -> Urn {
                Urn(self)
            }
        
            /// Get a borrowed [`Urn`] formatter.
            #[inline]
            pub fn as_urn(&self) -> &Urn {
                // SAFETY: `Uuid` and `Urn` have the same ABI
                unsafe { &*(self as *const Uuid as *const Urn) }
            }
        
            /// Get a [`Braced`] formatter.
            #[inline]
            pub const fn braced(self) -> Braced {
                Braced(self)
            }
        
            /// Get a borrowed [`Braced`] formatter.
            #[inline]
            pub fn as_braced(&self) -> &Braced {
                // SAFETY: `Uuid` and `Braced` have the same ABI
                unsafe { &*(self as *const Uuid as *const Braced) }
            }
        }
        
        const UPPER: [u8; 16] = [
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
        ];
        const LOWER: [u8; 16] = [
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f',
        ];
        
        #[inline]
        const fn format_simple(src: &[u8; 16], upper: bool) -> [u8; 32] {
            let lut = if upper { &UPPER } else { &LOWER };
            let mut dst = [0; 32];
            let mut i = 0;
            while i < 16 {
                let x = src[i];
                dst[i * 2] = lut[(x >> 4) as usize];
                dst[i * 2 + 1] = lut[(x & 0x0f) as usize];
                i += 1;
            }
            dst
        }
        
        #[inline]
        const fn format_hyphenated(src: &[u8; 16], upper: bool) -> [u8; 36] {
            let lut = if upper { &UPPER } else { &LOWER };
            let groups = [(0, 8), (9, 13), (14, 18), (19, 23), (24, 36)];
            let mut dst = [0; 36];
        
            let mut group_idx = 0;
            let mut i = 0;
            while group_idx < 5 {
                let (start, end) = groups[group_idx];
                let mut j = start;
                while j < end {
                    let x = src[i];
                    i += 1;
        
                    dst[j] = lut[(x >> 4) as usize];
                    dst[j + 1] = lut[(x & 0x0f) as usize];
                    j += 2;
                }
                if group_idx < 4 {
                    dst[end] = b'-';
                }
                group_idx += 1;
            }
            dst
        }
        
        #[inline]
        fn encode_simple<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str {
            let buf = &mut buffer[..Simple::LENGTH];
            let dst = buf.as_mut_ptr();
            
            unsafe {
                ptr::write(dst.cast(), format_simple(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }
        
        #[inline]
        fn encode_hyphenated<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str {
            let buf = &mut buffer[..Hyphenated::LENGTH];
            let dst = buf.as_mut_ptr();
            
            unsafe {
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }
        
        #[inline]
        fn encode_braced<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str {
            let buf = &mut buffer[..Braced::LENGTH];
            buf[0] = b'{';
            buf[Braced::LENGTH - 1] = b'}';
            
            unsafe {
                let dst = buf.as_mut_ptr().add(1);
        
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }
        
        #[inline]
        fn encode_urn<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str {
            let buf = &mut buffer[..Urn::LENGTH];
            buf[..9].copy_from_slice(b"urn:uuid:");
            
            unsafe {
                let dst = buf.as_mut_ptr().add(9);
        
                ptr::write(dst.cast(), format_hyphenated(src, upper));
                str::from_utf8_unchecked_mut(buf)
            }
        }
        
        impl Hyphenated {
            /// The length of a hyphenated [`Uuid`] string.
            pub const LENGTH: usize = 36;
        
            /// Creates a [`Hyphenated`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self {
                Hyphenated(uuid)
            }
        
            /// Writes the [`Uuid`] as a lower-case hyphenated string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline]
            pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_hyphenated(self.0.as_bytes(), buffer, false)
            }
        
            /// Writes the [`Uuid`] as an upper-case hyphenated string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline]
            pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_hyphenated(self.0.as_bytes(), buffer, true)
            }
        
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid {
                &self.0
            }
        
            /// Consumes the [`Hyphenated`], returning the underlying [`Uuid`].
            pub const fn into_uuid(self) -> Uuid {
                self.0
            }
        }
        
        impl Braced {
            /// The length of a braced [`Uuid`] string.
            pub const LENGTH: usize = 38;
        
            /// Creates a [`Braced`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self {
                Braced(uuid)
            }
        
            /// Writes the [`Uuid`] as a lower-case hyphenated string surrounded by
            /// braces to `buffer`, and returns the subslice of the buffer that contains
            /// the encoded UUID.
            #[inline]
            pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_braced(self.0.as_bytes(), buffer, false)
            }
        
            /// Writes the [`Uuid`] as an upper-case hyphenated string surrounded by
            /// braces to `buffer`, and returns the subslice of the buffer that contains
            /// the encoded UUID.
            #[inline]
            pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_braced(self.0.as_bytes(), buffer, true)
            }
        
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid {
                &self.0
            }
        
            /// Consumes the [`Braced`], returning the underlying [`Uuid`].
            pub const fn into_uuid(self) -> Uuid {
                self.0
            }
        }
        
        impl Simple {
            /// The length of a simple [`Uuid`] string.
            pub const LENGTH: usize = 32;
        
            /// Creates a [`Simple`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self {
                Simple(uuid)
            }
        
            /// Writes the [`Uuid`] as a lower-case simple string to `buffer`,
            /// and returns the subslice of the buffer that contains the encoded UUID.
            #[inline]
            pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_simple(self.0.as_bytes(), buffer, false)
            }
        
            /// Writes the [`Uuid`] as an upper-case simple string to `buffer`,
            /// and returns the subslice of the buffer that contains the encoded UUID.
            #[inline]
            pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_simple(self.0.as_bytes(), buffer, true)
            }
        
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid {
                &self.0
            }
        
            /// Consumes the [`Simple`], returning the underlying [`Uuid`].
            pub const fn into_uuid(self) -> Uuid {
                self.0
            }
        }
        
        impl Urn {
            /// The length of a URN [`Uuid`] string.
            pub const LENGTH: usize = 45;
        
            /// Creates a [`Urn`] from a [`Uuid`].
            pub const fn from_uuid(uuid: Uuid) -> Self {
                Urn(uuid)
            }
        
            /// Writes the [`Uuid`] as a lower-case URN string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline]
            pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_urn(self.0.as_bytes(), buffer, false)
            }
        
            /// Writes the [`Uuid`] as an upper-case URN string to
            /// `buffer`, and returns the subslice of the buffer that contains the
            /// encoded UUID.
            #[inline]
            pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
                encode_urn(self.0.as_bytes(), buffer, true)
            }
        
            /// Get a reference to the underlying [`Uuid`].
            pub const fn as_uuid(&self) -> &Uuid {
                &self.0
            }
        
            /// Consumes the [`Urn`], returning the underlying [`Uuid`].
            pub const fn into_uuid(self) -> Uuid {
                self.0
            }
        }
        
        macro_rules! impl_fmt_traits {
            ($($T:ident<$($a:lifetime),*>),+) => {$(
                impl<$($a),*> fmt::Display for $T<$($a),*> {
                    #[inline]
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        fmt::LowerHex::fmt(self, f)
                    }
                }
        
                impl<$($a),*> fmt::LowerHex for $T<$($a),*> {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str(self.encode_lower(&mut [0; Self::LENGTH]))
                    }
                }
        
                impl<$($a),*> fmt::UpperHex for $T<$($a),*> {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str(self.encode_upper(&mut [0; Self::LENGTH]))
                    }
                }
        
                impl_fmt_from!($T<$($a),*>);
            )+}
        }
        
        macro_rules! impl_fmt_from {
            ($T:ident<>) => {
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
            ($T:ident<$a:lifetime>) => {
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
        
        impl_fmt_traits! {
            Hyphenated<>,
            Simple<>,
            Urn<>,
            Braced<>
        }
    }
    
    pub mod rng
    {
        use ::
        {
            *,
        };
        /*
        */
        pub(crate) fn bytes() -> [u8; 16] {
            rand::random()
        }
    }
    
    pub mod timestamp
    {
        //! Generating UUIDs from timestamps.
        use ::
        {
            *,
        };
        /*
        use crate::Uuid;
        */
        /// The number of 100 nanosecond ticks between the RFC4122 epoch
        pub const UUID_TICKS_BETWEEN_EPOCHS: u64 = 0x01B2_1DD2_1381_4000;
        
        /// A timestamp that can be encoded into a UUID.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct Timestamp {
            pub(crate) seconds: u64,
            pub(crate) nanos: u32,
        }
        
        impl Timestamp {
            /// Get a timestamp representing the current system time.
            #[cfg(feature = "std")]
            pub fn now(context: impl ClockSequence<Output = u16>) -> Self {
                
                    let _ = context;
        
                let (seconds, nanos) = now();
        
                Timestamp {
                    seconds,
                    nanos,
                }
            }
        
            /// Construct a `Timestamp` from an RFC4122 timestamp and counter, as used
            /// in versions 1 and 6 UUIDs.
            pub const fn from_rfc4122(ticks: u64, counter: u16) -> Self {
                let _ = counter;
                let (seconds, nanos) = Self::rfc4122_to_unix(ticks);
        
                Timestamp {
                    seconds,
                    nanos,
                }
            }
        
            /// Construct a `Timestamp` from a Unix timestamp, as used in version 7 UUIDs..
            pub fn from_unix(context: impl ClockSequence<Output = u16>, seconds: u64, nanos: u32) -> Self 
            {
                let _ = context;
    
                Timestamp { seconds, nanos }
            }
                    
            /// Get the value of the timestamp as a Unix timestamp, as used in version 7 UUIDs.
            pub const fn to_unix(&self) -> (u64, u32) {
                (self.seconds, self.nanos)
            }
        
            const fn rfc4122_to_unix(ticks: u64) -> (u64, u32) {
                (
                    ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) / 10_000_000,
                    (ticks.wrapping_sub(UUID_TICKS_BETWEEN_EPOCHS) % 10_000_000) as u32 * 100,
                )
            }
        
            #[deprecated(note = "use `to_unix` instead; this method will be removed in a future release")]
            /// Get the number of fractional nanoseconds in the Unix timestamp.
            pub const fn to_unix_nanos(&self) -> u32 {
                panic!("`Timestamp::to_unix_nanos` is deprecated and will be removed: use `Timestamp::to_unix` instead")
            }
        }
        
        pub(crate) const fn encode_rfc4122_timestamp(ticks: u64, counter: u16, node_id: &[u8; 6]) -> Uuid {
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
        
        pub(crate) const fn decode_rfc4122_timestamp(uuid: &Uuid) -> (u64, u16) {
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
                
        fn now() -> (u64, u32) {
            let dur = ::time::SystemTime::UNIX_EPOCH.elapsed().expect(
                "Getting elapsed time since UNIX_EPOCH. If this fails, we've somehow violated causality",
            );
        
            (dur.as_secs(), dur.subsec_nanos())
        }
        
        /// A counter that can be used by version 1 and version 6 UUIDs to support
        /// the uniqueness of timestamps.
        pub trait ClockSequence {
            /// The type of sequence returned by this counter.
            type Output;
        
            /// Get the next value in the sequence to feed into a timestamp.
            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output;
        }
        
        impl<'a, T: ClockSequence + ?Sized> ClockSequence for &'a T {
            type Output = T::Output;
            fn generate_sequence(&self, seconds: u64, subsec_nanos: u32) -> Self::Output {
                (**self).generate_sequence(seconds, subsec_nanos)
            }
        }
        
        /// Default implementations for the [`ClockSequence`] trait.
        pub mod context {
            use super::ClockSequence;
        
            #[cfg(any(feature = "v1", feature = "v6"))]
            use atomic::{Atomic, Ordering};
        
            /// An empty counter that will always return the value `0`.
            #[derive(Debug, Clone, Copy, Default)]
            pub struct NoContext;
        
            impl ClockSequence for NoContext {
                type Output = u16;
        
                fn generate_sequence(&self, _seconds: u64, _nanos: u32) -> Self::Output {
                    0
                }
            }
        
            #[cfg(all(any(feature = "v1", feature = "v6"), feature = "std", feature = "rng"))]
            static CONTEXT: Context = Context {
                count: Atomic::new(0),
            };
        
            #[cfg(all(any(feature = "v1", feature = "v6"), feature = "std", feature = "rng"))]
            static CONTEXT_INITIALIZED: Atomic<bool> = Atomic::new(false);
        
            #[cfg(all(any(feature = "v1", feature = "v6"), feature = "std", feature = "rng"))]
            pub(crate) fn shared_context() -> &'static Context {
                if CONTEXT_INITIALIZED
                    .compare_exchange(false, true, Ordering::Relaxed, Ordering::Relaxed)
                    .is_ok()
                {
                    CONTEXT.count.store(crate::rng::u16(), Ordering::Release);
                }
        
                &CONTEXT
            }
        
            /// A thread-safe, wrapping counter that produces 14-bit numbers.
            #[derive(Debug)]
            #[cfg(any(feature = "v1", feature = "v6"))]
            pub struct Context {
                count: Atomic<u16>,
            }
        
            #[cfg(any(feature = "v1", feature = "v6"))]
            impl Context {
                /// Construct a new context that's initialized with the given value.
                pub const fn new(count: u16) -> Self {
                    Self {
                        count: Atomic::<u16>::new(count),
                    }
                }
        
                /// Construct a new context that's initialized with a random value.
                #[cfg(feature = "rng")]
                pub fn new_random() -> Self {
                    Self {
                        count: Atomic::<u16>::new(crate::rng::u16()),
                    }
                }
            }
        
            #[cfg(any(feature = "v1", feature = "v6"))]
            impl ClockSequence for Context {
                type Output = u16;
        
                fn generate_sequence(&self, _seconds: u64, _nanos: u32) -> Self::Output {
                    self.count.fetch_add(1, Ordering::AcqRel) % (u16::MAX >> 2)
                }
            }
        }
    } pub use self::timestamp::{context::NoContext, ClockSequence, Timestamp};

    /// A 128-bit (16 byte) buffer containing the UUID.
    pub type Bytes = [u8; 16];
    
    /// The version of the UUID, denoting the generating algorithm.
    #[derive(Clone, Copy, Debug, PartialEq)]
    #[non_exhaustive]
    #[repr(u8)]
    pub enum Version {
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
        #[cfg(uuid_unstable)]
        SortMac = 6,
        /// Version 7: Timestamp and random.
        #[cfg(uuid_unstable)]
        SortRand = 7,
        /// Version 8: Custom.
        #[cfg(uuid_unstable)]
        Custom = 8,
        /// The "max" (all ones) UUID.
        #[cfg(uuid_unstable)]
        Max = 0xff,
    }
    
    /// The reserved variants of UUIDs.
    #[derive(Clone, Copy, Debug, PartialEq)]
    #[non_exhaustive]
    #[repr(u8)]
    pub enum Variant {
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
    #[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Uuid(Bytes);
    
    impl Uuid {
        /// UUID namespace for Domain Name System (DNS).
        pub const NAMESPACE_DNS: Self = Uuid([
            0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30,
            0xc8,
        ]);
    
        /// UUID namespace for ISO Object Identifiers (OIDs).
        pub const NAMESPACE_OID: Self = Uuid([
            0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30,
            0xc8,
        ]);
    
        /// UUID namespace for Uniform Resource Locators (URLs).
        pub const NAMESPACE_URL: Self = Uuid([
            0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30,
            0xc8,
        ]);
    
        /// UUID namespace for X.500 Distinguished Names (DNs).
        pub const NAMESPACE_X500: Self = Uuid([
            0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30,
            0xc8,
        ]);
    
        /// Returns the variant of the UUID structure.
        pub const fn get_variant(&self) -> Variant {
            match self.as_bytes()[8] {
                x if x & 0x80 == 0x00 => Variant::NCS,
                x if x & 0xc0 == 0x80 => Variant::RFC4122,
                x if x & 0xe0 == 0xc0 => Variant::Microsoft,
                x if x & 0xe0 == 0xe0 => Variant::Future,
                _ => Variant::Future,
            }
        }
    
        /// Returns the version number of the UUID.
        pub const fn get_version_num(&self) -> usize {
            (self.as_bytes()[6] >> 4) as usize
        }
    
        /// Returns the version of the UUID.
        pub const fn get_version(&self) -> Option<Version> {
            match self.get_version_num() {
                0 if self.is_nil() => Some(Version::Nil),
                1 => Some(Version::Mac),
                2 => Some(Version::Dce),
                3 => Some(Version::Md5),
                4 => Some(Version::Random),
                5 => Some(Version::Sha1),
                #[cfg(uuid_unstable)]
                6 => Some(Version::SortMac),
                #[cfg(uuid_unstable)]
                7 => Some(Version::SortRand),
                #[cfg(uuid_unstable)]
                8 => Some(Version::Custom),
                #[cfg(uuid_unstable)]
                0xf => Some(Version::Max),
                _ => None,
            }
        }

        pub fn new_v4() -> Uuid {
            Builder::from_random_bytes(::rng::bytes()).into_uuid()
        }
    
        /// Returns the four field values of the UUID.
        pub fn as_fields(&self) -> (u32, u16, u16, &[u8; 8]) {
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
        pub fn to_fields_le(&self) -> (u32, u16, u16, &[u8; 8]) {
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
        pub const fn as_u128(&self) -> u128 {
            u128::from_be_bytes(*self.as_bytes())
        }
    
        /// Returns a 128bit little-endian value containing the value.
        pub const fn to_u128_le(&self) -> u128 {
            u128::from_le_bytes(*self.as_bytes())
        }
    
        /// Returns two 64bit values containing the value.
        pub const fn as_u64_pair(&self) -> (u64, u64) {
            let value = self.as_u128();
            ((value >> 64) as u64, value as u64)
        }
    
        /// Returns a slice of 16 octets containing the value.
        pub const fn as_bytes(&self) -> &Bytes {
            &self.0
        }
    
        /// Consumes self and returns the underlying byte value of the UUID.
        pub const fn into_bytes(self) -> Bytes {
            self.0
        }
    
        /// Returns the bytes of the UUID in little-endian order.
        pub const fn to_bytes_le(&self) -> Bytes {
            [
                self.0[3], self.0[2], self.0[1], self.0[0], self.0[5], self.0[4], self.0[7], self.0[6],
                self.0[8], self.0[9], self.0[10], self.0[11], self.0[12], self.0[13], self.0[14],
                self.0[15],
            ]
        }
    
        /// Tests if the UUID is nil (all zeros).
        pub const fn is_nil(&self) -> bool {
            self.as_u128() == u128::MIN
        }
    
        /// Tests if the UUID is max (all ones).
        #[cfg(uuid_unstable)]
        pub const fn is_max(&self) -> bool {
            self.as_u128() == u128::MAX
        }
    
        /// A buffer that can be used for `encode_...` calls, that is
        /// guaranteed to be long enough for any of the format adapters.
        pub const fn encode_buffer() -> [u8; fmt::Urn::LENGTH] {
            [0; fmt::Urn::LENGTH]
        }
    
        /// If the UUID is the correct version (v1, v6, or v7) this will return
        /// the timestamp and counter portion parsed from a V1 UUID.
        pub const fn get_timestamp(&self) -> Option<Timestamp> {
            match self.get_version() {
                Some(Version::Mac) => {
                    let (ticks, counter) = timestamp::decode_rfc4122_timestamp(self);
    
                    Some(Timestamp::from_rfc4122(ticks, counter))
                }
                #[cfg(uuid_unstable)]
                Some(Version::SortMac) => {
                    let (ticks, counter) = timestamp::decode_sorted_rfc4122_timestamp(self);
    
                    Some(Timestamp::from_rfc4122(ticks, counter))
                }
                #[cfg(uuid_unstable)]
                Some(Version::SortRand) => {
                    let millis = timestamp::decode_unix_timestamp_millis(self);
    
                    let seconds = millis / 1000;
                    let nanos = ((millis % 1000) * 1_000_000) as u32;
    
                    Some(Timestamp {
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
    
    impl Default for Uuid {
        #[inline]
        fn default() -> Self {
            Uuid::nil()
        }
    }
    
    impl AsRef<[u8]> for Uuid {
        #[inline]
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }
}
/*
yaml-rust = "0.4.0"
*/
pub mod yaml_rust
{
    //! YAML 1.2 implementation in pure Rust.
    use ::
    {
        *,
    };
    /*
    extern crate linked_hash_map;
    */
    pub mod yaml
    {
        use ::
        {
            *,
        };
        /*
        use std::collections::BTreeMap;
        use std::ops::Index;
        use std::string;
        use std::i64;
        use std::f64;
        use std::mem;
        use std::vec;
        use parser::*;
        use scanner::{TScalarStyle, ScanError, TokenType, Marker};
        use linked_hash_map::LinkedHashMap;
        */
        /// A YAML node is stored as this `Yaml` enumeration, which provides an easy way to access your YAML document.
        #[derive(Clone, PartialEq, PartialOrd, Debug, Eq, Ord, Hash)]
        pub enum Yaml {
            /// Float types are stored as String and parsed on demand.
            /// Note that f64 does NOT implement Eq trait and can NOT be stored in BTreeMap.
            Real(string::String),
            /// YAML int is stored as i64.
            Integer(i64),
            /// YAML scalar.
            String(string::String),
            /// YAML bool, e.g. `true` or `false`.
            Boolean(bool),
            /// YAML array, can be accessed as a `Vec`.
            Array(self::Array),
            /// YAML hash, can be accessed as a `LinkedHashMap`.
            ///
            /// Itertion order will match the order of insertion into the map.
            Hash(self::Hash),
            /// Alias, not fully supported yet.
            Alias(usize),
            /// YAML null, e.g. `null` or `~`.
            Null,
            /// Accessing a nonexistent node via the Index trait returns `BadValue`. This
            /// simplifies error handling in the calling code. Invalid type conversion also
            /// returns `BadValue`.
            BadValue,
        }
        
        pub type Array = Vec<Yaml>;
        pub type Hash = LinkedHashMap<Yaml, Yaml>;
        
        // parse f64 as Core schema
        // See: https://github.com/chyh1990/yaml-rust/issues/51
        fn parse_f64(v: &str) -> Option<f64> {
            match v {
                ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Some(f64::INFINITY),
                "-.inf" | "-.Inf" | "-.INF" => Some(f64::NEG_INFINITY),
                ".nan" | "NaN" | ".NAN" => Some(f64::NAN),
                _ => v.parse::<f64>().ok()
            }
        }
        
        pub struct YamlLoader {
            docs: Vec<Yaml>,
            // states
            // (current node, anchor_id) tuple
            doc_stack: Vec<(Yaml, usize)>,
            key_stack: Vec<Yaml>,
            anchor_map: BTreeMap<usize, Yaml>,
        }
        
        impl MarkedEventReceiver for YamlLoader {
            fn on_event(&mut self, ev: Event, _: Marker) {
                // println!("EV {:?}", ev);
                match ev {
                    Event::DocumentStart => {
                        // do nothing
                    },
                    Event::DocumentEnd => {
                        match self.doc_stack.len() {
                            // empty document
                            0 => self.docs.push(Yaml::BadValue),
                            1 => self.docs.push(self.doc_stack.pop().unwrap().0),
                            _ => unreachable!()
                        }
                    },
                    Event::SequenceStart(aid) => {
                        self.doc_stack.push((Yaml::Array(Vec::new()), aid));
                    },
                    Event::SequenceEnd => {
                        let node = self.doc_stack.pop().unwrap();
                        self.insert_new_node(node);
                    },
                    Event::MappingStart(aid) => {
                        self.doc_stack.push((Yaml::Hash(Hash::new()), aid));
                        self.key_stack.push(Yaml::BadValue);
                    },
                    Event::MappingEnd => {
                        self.key_stack.pop().unwrap();
                        let node = self.doc_stack.pop().unwrap();
                        self.insert_new_node(node);
                    },
                    Event::Scalar(v, style, aid, tag) => {
                        let node = if style != TScalarStyle::Plain {
                            Yaml::String(v)
                        } else if let Some(TokenType::Tag(ref handle, ref suffix)) = tag {
                            // XXX tag:yaml.org,2002:
                            if handle == "!!" {
                                match suffix.as_ref() {
                                    "bool" => {
                                        // "true" or "false"
                                        match v.parse::<bool>() {
                                            Err(_) => Yaml::BadValue,
                                            Ok(v) => Yaml::Boolean(v)
                                        }
                                    },
                                    "int" => {
                                        match v.parse::<i64>() {
                                            Err(_) => Yaml::BadValue,
                                            Ok(v) => Yaml::Integer(v)
                                        }
                                    },
                                    "float" => {
                                        match parse_f64(&v) {
                                            Some(_) => Yaml::Real(v),
                                            None => Yaml::BadValue,
                                        }
                                    },
                                    "null" => {
                                        match v.as_ref() {
                                            "~" | "null" => Yaml::Null,
                                            _ => Yaml::BadValue,
                                        }
                                    }
                                    _  => Yaml::String(v),
                                }
                            } else {
                                Yaml::String(v)
                            }
                        } else {
                            // Datatype is not specified, or unrecognized
                            Yaml::from_str(&v)
                        };
        
                        self.insert_new_node((node, aid));
                    },
                    Event::Alias(id) => {
                        let n = match self.anchor_map.get(&id) {
                            Some(v) => v.clone(),
                            None => Yaml::BadValue,
                        };
                        self.insert_new_node((n, 0));
                    }
                    _ => { /* ignore */ }
                }
                // println!("DOC {:?}", self.doc_stack);
            }
        }
        
        impl YamlLoader {
            fn insert_new_node(&mut self, node: (Yaml, usize)) {
                // valid anchor id starts from 1
                if node.1 > 0 {
                    self.anchor_map.insert(node.1, node.0.clone());
                }
                if self.doc_stack.is_empty() {
                    self.doc_stack.push(node);
                } else {
                    let parent = self.doc_stack.last_mut().unwrap();
                    match *parent {
                        (Yaml::Array(ref mut v), _) => v.push(node.0),
                        (Yaml::Hash(ref mut h), _) => {
                            let mut cur_key = self.key_stack.last_mut().unwrap();
                            // current node is a key
                            if cur_key.is_badvalue() {
                                *cur_key = node.0;
                            // current node is a value
                            } else {
                                let mut newkey = Yaml::BadValue;
                                mem::swap(&mut newkey, cur_key);
                                h.insert(newkey, node.0);
                            }
                        },
                        _ => unreachable!(),
                    }
                }
            }
        
            pub fn load_from_str(source: &str) -> Result<Vec<Yaml>, ScanError>{
                let mut loader = YamlLoader {
                    docs: Vec::new(),
                    doc_stack: Vec::new(),
                    key_stack: Vec::new(),
                    anchor_map: BTreeMap::new(),
                };
                let mut parser = Parser::new(source.chars());
                try!(parser.load(&mut loader, true));
                Ok(loader.docs)
            }
        }
        
        macro_rules! define_as (
            ($name:ident, $t:ident, $yt:ident) => (
        pub fn $name(&self) -> Option<$t> {
            match *self {
                Yaml::$yt(v) => Some(v),
                _ => None
            }
        }
            );
        );
        
        macro_rules! define_as_ref (
            ($name:ident, $t:ty, $yt:ident) => (
        pub fn $name(&self) -> Option<$t> {
            match *self {
                Yaml::$yt(ref v) => Some(v),
                _ => None
            }
        }
            );
        );
        
        macro_rules! define_into (
            ($name:ident, $t:ty, $yt:ident) => (
        pub fn $name(self) -> Option<$t> {
            match self {
                Yaml::$yt(v) => Some(v),
                _ => None
            }
        }
            );
        );
        
        impl Yaml {
            define_as!(as_bool, bool, Boolean);
            define_as!(as_i64, i64, Integer);
        
            define_as_ref!(as_str, &str, String);
            define_as_ref!(as_hash, &Hash, Hash);
            define_as_ref!(as_vec, &Array, Array);
        
            define_into!(into_bool, bool, Boolean);
            define_into!(into_i64, i64, Integer);
            define_into!(into_string, String, String);
            define_into!(into_hash, Hash, Hash);
            define_into!(into_vec, Array, Array);
        
            pub fn is_null(&self) -> bool {
                match *self {
                    Yaml::Null => true,
                    _ => false
                }
            }
        
            pub fn is_badvalue(&self) -> bool {
                match *self {
                    Yaml::BadValue => true,
                    _ => false
                }
            }
        
            pub fn is_array(&self) -> bool {
                match *self {
                    Yaml::Array(_) => true,
                    _ => false
                }
            }
        
            pub fn as_f64(&self) -> Option<f64> {
                match *self {
                    Yaml::Real(ref v) => parse_f64(v),
                    _ => None
                }
            }
        
            pub fn into_f64(self) -> Option<f64> {
                match self {
                    Yaml::Real(ref v) => parse_f64(v),
                    _ => None
                }
            }
        }
        
        #[cfg_attr(feature = "cargo-clippy", allow(should_implement_trait))]
        impl Yaml {
            // Not implementing FromStr because there is no possibility of Error.
            // This function falls back to Yaml::String if nothing else matches.
            pub fn from_str(v: &str) -> Yaml {
                if v.starts_with("0x") {
                    let n = i64::from_str_radix(&v[2..], 16);
                    if n.is_ok() {
                        return Yaml::Integer(n.unwrap());
                    }
                }
                if v.starts_with("0o") {
                    let n = i64::from_str_radix(&v[2..], 8);
                    if n.is_ok() {
                        return Yaml::Integer(n.unwrap());
                    }
                }
                if v.starts_with('+') && v[1..].parse::<i64>().is_ok() {
                    return Yaml::Integer(v[1..].parse::<i64>().unwrap());
                }
                match v {
                    "~" | "null" => Yaml::Null,
                    "true" => Yaml::Boolean(true),
                    "false" => Yaml::Boolean(false),
                    _ if v.parse::<i64>().is_ok() => Yaml::Integer(v.parse::<i64>().unwrap()),
                    // try parsing as f64
                    _ if parse_f64(v).is_some() => Yaml::Real(v.to_owned()),
                    _ => Yaml::String(v.to_owned())
                }
            }
        }
        
        static BAD_VALUE: Yaml = Yaml::BadValue;
        impl<'a> Index<&'a str> for Yaml {
            type Output = Yaml;
        
            fn index(&self, idx: &'a str) -> &Yaml {
                let key = Yaml::String(idx.to_owned());
                match self.as_hash() {
                    Some(h) => h.get(&key).unwrap_or(&BAD_VALUE),
                    None => &BAD_VALUE
                }
            }
        }
        
        impl Index<usize> for Yaml {
            type Output = Yaml;
        
            fn index(&self, idx: usize) -> &Yaml {
                if let Some(v) = self.as_vec() {
                    v.get(idx).unwrap_or(&BAD_VALUE)
                } else if let Some(v) = self.as_hash() {
                    let key = Yaml::Integer(idx as i64);
                    v.get(&key).unwrap_or(&BAD_VALUE)
                } else {
                    &BAD_VALUE
                }
            }
        }
        
        impl IntoIterator for Yaml {
            type Item = Yaml;
            type IntoIter = YamlIter;
        
            fn into_iter(self) -> Self::IntoIter {
                YamlIter {
                    yaml: self.into_vec()
                        .unwrap_or_else(Vec::new).into_iter()
                }
            }
        }
        
        pub struct YamlIter {
            yaml: vec::IntoIter<Yaml>,
        }
        
        impl Iterator for YamlIter {
            type Item = Yaml;
        
            fn next(&mut self) -> Option<Yaml> {
                self.yaml.next()
            }
        }
    }
    
    pub mod scanner
    {
        use ::
        {
            *,
        };
        /*
        */
        #[derive(Clone, Copy, PartialEq, Debug, Eq)]
        pub enum TEncoding {
            Utf8
        }
        
        #[derive(Clone, Copy, PartialEq, Debug, Eq)]
        pub enum TScalarStyle {
            Any,
            Plain,
            SingleQuoted,
            DoubleQuoted,
        
            Literal,
            Foled
        }
        
        #[derive(Clone, Copy, PartialEq, Debug, Eq)]
        pub struct Marker {
            index: usize,
            line: usize,
            col: usize,
        }
        
        impl Marker {
            fn new(index: usize, line: usize, col: usize) -> Marker {
                Marker {
                    index: index,
                    line: line,
                    col: col
                }
            }
        
            pub fn index(&self) -> usize {
                self.index
            }
        
            pub fn line(&self) -> usize {
                self.line
            }
        
            pub fn col(&self) -> usize {
                self.col
            }
        }
        
        #[derive(Clone, PartialEq, Debug, Eq)]
        pub struct ScanError {
            mark: Marker,
            info: String,
        }
        
        impl ScanError {
            pub fn new(loc: Marker, info: &str) -> ScanError {
                ScanError {
                    mark: loc,
                    info: info.to_owned()
                }
            }
        
            pub fn marker(&self) -> &Marker {
                &self.mark
            }
        }
        
        impl Error for ScanError {
            fn description(&self) -> &str {
                self.info.as_ref()
            }
        
            fn cause(&self) -> Option<&Error> {
                None
            }
        }
        
        impl fmt::Display for ScanError {
            // col starts from 0
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "{} at line {} column {}", self.info,
                       self.mark.line, self.mark.col + 1)
            }
        }
        
        #[derive(Clone, PartialEq, Debug, Eq)]
        pub enum TokenType {
            NoToken,
            StreamStart(TEncoding),
            StreamEnd,
            /// major, minor
            VersionDirective(u32, u32),
            /// handle, prefix
            TagDirective(String, String),
            DocumentStart,
            DocumentEnd,
            BlockSequenceStart,
            BlockMappingStart,
            BlockEnd,
            FlowSequenceStart,
            FlowSequenceEnd,
            FlowMappingStart,
            FlowMappingEnd,
            BlockEntry,
            FlowEntry,
            Key,
            Value,
            Alias(String),
            Anchor(String),
            /// handle, suffix
            Tag(String, String),
            Scalar(TScalarStyle, String)
        }
        
        #[derive(Clone, PartialEq, Debug, Eq)]
        pub struct Token(pub Marker, pub TokenType);
        
        #[derive(Clone, PartialEq, Debug, Eq)]
        struct SimpleKey {
            possible: bool,
            required: bool,
            token_number: usize,
            mark: Marker,
        }
        
        impl SimpleKey {
            fn new(mark: Marker) -> SimpleKey {
                SimpleKey {
                    possible: false,
                    required: false,
                    token_number: 0,
                    mark: mark,
                }
            }
        }
        
        #[derive(Debug)]
        pub struct Scanner<T> {
            rdr: T,
            mark: Marker,
            tokens: VecDeque<Token>,
            buffer: VecDeque<char>,
            error: Option<ScanError>,
        
            stream_start_produced: bool,
            stream_end_produced: bool,
            simple_key_allowed: bool,
            simple_keys: Vec<SimpleKey>,
            indent: isize,
            indents: Vec<isize>,
            flow_level: usize,
            tokens_parsed: usize,
            token_available: bool,
        }
        
        impl<T: Iterator<Item=char>> Iterator for Scanner<T> {
            type Item = Token;
            fn next(&mut self) -> Option<Token> {
                if self.error.is_some() {
                    return None;
                }
                match self.next_token() {
                    Ok(tok) => tok,
                    Err(e) => {
                        self.error = Some(e);
                        None
                    }
                }
            }
        }
        
        #[inline]
        fn is_z(c: char) -> bool {
            c == '\0'
        }
        #[inline]
        fn is_break(c: char) -> bool {
            c == '\n' || c == '\r'
        }
        #[inline]
        fn is_breakz(c: char) -> bool {
            is_break(c) || is_z(c)
        }
        #[inline]
        fn is_blank(c: char) -> bool {
            c == ' ' || c == '\t'
        }
        #[inline]
        fn is_blankz(c: char) -> bool {
            is_blank(c) || is_breakz(c)
        }
        #[inline]
        fn is_digit(c: char) -> bool {
            c >= '0' && c <= '9'
        }
        #[inline]
        fn is_alpha(c: char) -> bool {
            match c {
                '0'...'9' | 'a'...'z' | 'A'...'Z' => true,
                '_' | '-' => true,
                _ => false
            }
        }
        #[inline]
        fn is_hex(c: char) -> bool {
            (c >= '0' && c <= '9')
                || (c >= 'a' && c <= 'f')
                || (c >= 'A' && c <= 'F')
        }
        #[inline]
        fn as_hex(c: char) -> u32 {
            match c {
                '0'...'9' => (c as u32) - ('0' as u32),
                'a'...'f' => (c as u32) - ('a' as u32) + 10,
                'A'...'F' => (c as u32) - ('A' as u32) + 10,
                _ => unreachable!()
            }
        }
        
        pub type ScanResult = Result<(), ScanError>;
        
        impl<T: Iterator<Item=char>> Scanner<T> {
            /// Creates the YAML tokenizer.
            pub fn new(rdr: T) -> Scanner<T> {
                Scanner {
                    rdr: rdr,
                    buffer: VecDeque::new(),
                    mark: Marker::new(0, 1, 0),
                    tokens: VecDeque::new(),
                    error: None,
        
                    stream_start_produced: false,
                    stream_end_produced: false,
                    simple_key_allowed: true,
                    simple_keys: Vec::new(),
                    indent: -1,
                    indents: Vec::new(),
                    flow_level: 0,
                    tokens_parsed: 0,
                    token_available: false,
                }
            }
            #[inline]
            pub fn get_error(&self) -> Option<ScanError> {
                match self.error {
                    None => None,
                    Some(ref e) => Some(e.clone()),
                }
            }
        
            #[inline]
            fn lookahead(&mut self, count: usize) {
                if self.buffer.len() >= count {
                    return;
                }
                for _ in 0..(count - self.buffer.len()) {
                    self.buffer.push_back(self.rdr.next().unwrap_or('\0'));
                }
            }
            #[inline]
            fn skip(&mut self) {
                let c = self.buffer.pop_front().unwrap();
        
                self.mark.index += 1;
                if c == '\n' {
                    self.mark.line += 1;
                    self.mark.col = 0;
                } else {
                    self.mark.col += 1;
                }
            }
            #[inline]
            fn skip_line(&mut self) {
                if self.buffer[0] == '\r' && self.buffer[1] == '\n' {
                    self.skip();
                    self.skip();
                } else if is_break(self.buffer[0]) {
                    self.skip();
                }
            }
            #[inline]
            fn ch(&self) -> char {
                self.buffer[0]
            }
            #[inline]
            fn ch_is(&self, c: char) -> bool {
                self.buffer[0] == c
            }
            #[allow(dead_code)]
            #[inline]
            fn eof(&self) -> bool {
                self.ch_is('\0')
            }
            #[inline]
            pub fn stream_started(&self) -> bool {
                self.stream_start_produced
            }
            #[inline]
            pub fn stream_ended(&self) -> bool {
                self.stream_end_produced
            }
            #[inline]
            pub fn mark(&self) -> Marker {
                self.mark
            }
            #[inline]
            fn read_break(&mut self, s: &mut String) {
                if self.buffer[0] == '\r' && self.buffer[1] == '\n' {
                    s.push('\n');
                    self.skip();
                    self.skip();
                } else if self.buffer[0] == '\r' || self.buffer[0] == '\n' {
                    s.push('\n');
                    self.skip();
                } else {
                    unreachable!();
                }
            }
            fn insert_token(&mut self, pos: usize, tok: Token) {
                let old_len = self.tokens.len();
                assert!(pos <= old_len);
                self.tokens.push_back(tok);
                for i in 0..old_len - pos {
                    self.tokens.swap(old_len - i, old_len - i - 1);
                }
            }
            fn allow_simple_key(&mut self) {
                    self.simple_key_allowed = true;
            }
            fn disallow_simple_key(&mut self) {
                    self.simple_key_allowed = false;
            }
        
            pub fn fetch_next_token(&mut self) -> ScanResult {
                self.lookahead(1);
                // println!("--> fetch_next_token Cur {:?} {:?}", self.mark, self.ch());
        
                if !self.stream_start_produced {
                    self.fetch_stream_start();
                    return Ok(());
                }
                self.skip_to_next_token();
        
                try!(self.stale_simple_keys());
        
                let mark = self.mark;
                self.unroll_indent(mark.col as isize);
        
                self.lookahead(4);
        
                if is_z(self.ch()) {
                    try!(self.fetch_stream_end());
                    return Ok(());
                }
        
                // Is it a directive?
                if self.mark.col == 0 && self.ch_is('%') {
                    return self.fetch_directive();
                }
        
                if self.mark.col == 0
                    && self.buffer[0] == '-'
                    && self.buffer[1] == '-'
                    && self.buffer[2] == '-'
                    && is_blankz(self.buffer[3]) {
                    try!(self.fetch_document_indicator(TokenType::DocumentStart));
                    return Ok(());
                }
        
                if self.mark.col == 0
                    && self.buffer[0] == '.'
                    && self.buffer[1] == '.'
                    && self.buffer[2] == '.'
                    && is_blankz(self.buffer[3]) {
                    try!(self.fetch_document_indicator(TokenType::DocumentEnd));
                    return Ok(());
                }
        
                let c = self.buffer[0];
                let nc = self.buffer[1];
                match c {
                    '[' => self.fetch_flow_collection_start(TokenType::FlowSequenceStart),
                    '{' => self.fetch_flow_collection_start(TokenType::FlowMappingStart),
                    ']' => self.fetch_flow_collection_end(TokenType::FlowSequenceEnd),
                    '}' => self.fetch_flow_collection_end(TokenType::FlowMappingEnd),
                    ',' => self.fetch_flow_entry(),
                    '-' if is_blankz(nc) => self.fetch_block_entry(),
                    '?' if self.flow_level > 0 || is_blankz(nc) => self.fetch_key(),
                    ':' if self.flow_level > 0 || is_blankz(nc) => self.fetch_value(),
                    // Is it an alias?
                    '*' => self.fetch_anchor(true),
                    // Is it an anchor?
                    '&' => self.fetch_anchor(false),
                    '!' => self.fetch_tag(),
                    // Is it a literal scalar?
                    '|' if self.flow_level == 0 => self.fetch_block_scalar(true),
                    // Is it a folded scalar?
                    '>' if self.flow_level == 0 => self.fetch_block_scalar(false),
                    '\'' => self.fetch_flow_scalar(true),
                    '"' => self.fetch_flow_scalar(false),
                    // plain scalar
                    '-' if !is_blankz(nc) => self.fetch_plain_scalar(),
                    ':' | '?' if !is_blankz(nc) && self.flow_level == 0 => self.fetch_plain_scalar(),
                    '%' | '@' | '`' => Err(ScanError::new(self.mark,
                            &format!("unexpected character: `{}'", c))),
                    _ => self.fetch_plain_scalar(),
                }
            }
        
            pub fn next_token(&mut self) -> Result<Option<Token>, ScanError> {
                if self.stream_end_produced {
                    return Ok(None);
                }
        
                if !self.token_available {
                    try!(self.fetch_more_tokens());
                }
                let t = self.tokens.pop_front().unwrap();
                self.token_available = false;
                self.tokens_parsed += 1;
        
                if let TokenType::StreamEnd = t.1 {
                    self.stream_end_produced = true;
                }
                Ok(Some(t))
            }
        
            pub fn fetch_more_tokens(&mut self) -> ScanResult {
                let mut need_more;
                loop {
                    need_more = false;
                    if self.tokens.is_empty() {
                        need_more = true;
                    } else {
                        try!(self.stale_simple_keys());
                        for sk in &self.simple_keys {
                            if sk.possible && sk.token_number == self.tokens_parsed {
                                need_more = true;
                                break;
                            }
                        }
                    }
        
                    if !need_more { break; }
                    try!(self.fetch_next_token());
                }
                self.token_available = true;
        
                Ok(())
            }
        
            fn stale_simple_keys(&mut self) -> ScanResult {
                for sk in &mut self.simple_keys {
                    if sk.possible && (sk.mark.line < self.mark.line
                        || sk.mark.index + 1024 < self.mark.index) {
                            if sk.required {
                                return Err(ScanError::new(self.mark, "simple key expect ':'"));
                            }
                            sk.possible = false;
                        }
                }
                Ok(())
            }
        
            fn skip_to_next_token(&mut self) {
                loop {
                    self.lookahead(1);
                    // TODO(chenyh) BOM
                    match self.ch() {
                        ' ' => self.skip(),
                        '\t' if self.flow_level > 0 || !self.simple_key_allowed => self.skip(),
                        '\n' | '\r' => {
                            self.lookahead(2);
                            self.skip_line();
                            if self.flow_level == 0 {
                                self.allow_simple_key();
                            }
                        },
                        '#' => while !is_breakz(self.ch()) { self.skip(); self.lookahead(1); },
                        _ => break
                    }
                }
            }
        
            fn fetch_stream_start(&mut self) {
                let mark = self.mark;
                self.indent = -1;
                self.stream_start_produced = true;
                self.allow_simple_key();
                self.tokens.push_back(Token(mark, TokenType::StreamStart(TEncoding::Utf8)));
                self.simple_keys.push(SimpleKey::new(Marker::new(0,0,0)));
            }
        
            fn fetch_stream_end(&mut self) -> ScanResult {
                // force new line
                if self.mark.col != 0 {
                    self.mark.col = 0;
                    self.mark.line += 1;
                }
        
                self.unroll_indent(-1);
                try!(self.remove_simple_key());
                self.disallow_simple_key();
        
                self.tokens.push_back(Token(self.mark, TokenType::StreamEnd));
                Ok(())
            }
        
            fn fetch_directive(&mut self) -> ScanResult {
                self.unroll_indent(-1);
                try!(self.remove_simple_key());
        
                self.disallow_simple_key();
        
                let tok = try!(self.scan_directive());
        
                self.tokens.push_back(tok);
        
                Ok(())
            }
        
            fn scan_directive(&mut self) -> Result<Token, ScanError> {
                let start_mark = self.mark;
                self.skip();
        
                let name = try!(self.scan_directive_name());
                let tok = match name.as_ref() {
                    "YAML" => {
                        try!(self.scan_version_directive_value(&start_mark))
                    },
                    "TAG" => {
                        try!(self.scan_tag_directive_value(&start_mark))
                    },
                    // XXX This should be a warning instead of an error
                    _ => {
                        // skip current line
                        self.lookahead(1);
                        while !is_breakz(self.ch()) {
                            self.skip();
                            self.lookahead(1);
                        }
                        // XXX return an empty TagDirective token
                        Token(start_mark, TokenType::TagDirective(String::new(), String::new()))
                        // return Err(ScanError::new(start_mark,
                        //     "while scanning a directive, found unknown directive name"))
                    }
                };
                self.lookahead(1);
        
                while is_blank(self.ch()) {
                    self.skip();
                    self.lookahead(1);
                }
        
                if self.ch() == '#' {
                    while !is_breakz(self.ch()) {
                        self.skip();
                        self.lookahead(1);
                    }
                }
        
                if !is_breakz(self.ch()) {
                    return Err(ScanError::new(start_mark,
                        "while scanning a directive, did not find expected comment or line break"));
                }
        
                // Eat a line break
                if is_break(self.ch()) {
                    self.lookahead(2);
                    self.skip_line();
                }
        
                Ok(tok)
            }
        
            fn scan_version_directive_value(&mut self, mark: &Marker) -> Result<Token, ScanError> {
                self.lookahead(1);
        
                while is_blank(self.ch()) {
                    self.skip();
                    self.lookahead(1);
                }
        
                let major = try!(self.scan_version_directive_number(mark));
        
                if self.ch() != '.' {
                    return Err(ScanError::new(*mark,
                        "while scanning a YAML directive, did not find expected digit or '.' character"));
                }
        
                self.skip();
        
                let minor = try!(self.scan_version_directive_number(mark));
        
                Ok(Token(*mark, TokenType::VersionDirective(major, minor)))
            }
        
            fn scan_directive_name(&mut self) -> Result<String, ScanError> {
                let start_mark = self.mark;
                let mut string = String::new();
                self.lookahead(1);
                while is_alpha(self.ch()) {
                    string.push(self.ch());
                    self.skip();
                    self.lookahead(1);
                }
        
                if string.is_empty() {
                    return Err(ScanError::new(start_mark,
                            "while scanning a directive, could not find expected directive name"));
                }
        
                if !is_blankz(self.ch()) {
                    return Err(ScanError::new(start_mark,
                            "while scanning a directive, found unexpected non-alphabetical character"));
                }
        
                Ok(string)
            }
        
            fn scan_version_directive_number(&mut self, mark: &Marker) -> Result<u32, ScanError> {
                let mut val = 0u32;
                let mut length = 0usize;
                self.lookahead(1);
                while is_digit(self.ch()) {
                    if length + 1 > 9 {
                        return Err(ScanError::new(*mark,
                            "while scanning a YAML directive, found extremely long version number"));
                    }
                    length += 1;
                    val = val * 10 + ((self.ch() as u32) - ('0' as u32));
                    self.skip();
                    self.lookahead(1);
                }
        
                if length == 0 {
                        return Err(ScanError::new(*mark,
                            "while scanning a YAML directive, did not find expected version number"));
                }
        
                Ok(val)
            }
        
            fn scan_tag_directive_value(&mut self, mark: &Marker) -> Result<Token, ScanError> {
                self.lookahead(1);
                /* Eat whitespaces. */
                while is_blank(self.ch()) {
                    self.skip();
                    self.lookahead(1);
                }
                let handle = try!(self.scan_tag_handle(true, mark));
        
                self.lookahead(1);
                /* Eat whitespaces. */
                while is_blank(self.ch()) {
                    self.skip();
                    self.lookahead(1);
                }
        
                let is_secondary = handle == "!!";
                let prefix = try!(self.scan_tag_uri(true, is_secondary, &String::new(), mark));
        
                self.lookahead(1);
        
                if is_blankz(self.ch()) {
                    Ok(Token(*mark, TokenType::TagDirective(handle, prefix)))
                } else {
                    Err(ScanError::new(*mark,
                        "while scanning TAG, did not find expected whitespace or line break"))
                }
            }
        
            fn fetch_tag(&mut self) -> ScanResult {
                try!(self.save_simple_key());
                self.disallow_simple_key();
        
                let tok = try!(self.scan_tag());
                self.tokens.push_back(tok);
                Ok(())
            }
        
            fn scan_tag(&mut self) -> Result<Token, ScanError> {
                let start_mark = self.mark;
                let mut handle = String::new();
                let mut suffix;
                let mut secondary = false;
        
                // Check if the tag is in the canonical form (verbatim).
                self.lookahead(2);
        
                if self.buffer[1] == '<' {
                    // Eat '!<'
                    self.skip();
                    self.skip();
                    suffix = try!(self.scan_tag_uri(false, false, &String::new(), &start_mark));
        
                    if self.ch() != '>' {
                        return Err(ScanError::new(start_mark,
                            "while scanning a tag, did not find the expected '>'"));
                    }
        
                    self.skip();
                } else {
                    // The tag has either the '!suffix' or the '!handle!suffix'
                    handle = try!(self.scan_tag_handle(false, &start_mark));
                    // Check if it is, indeed, handle.
                    if handle.len() >= 2 && handle.starts_with('!') && handle.ends_with('!') {
                        if handle == "!!" {
                            secondary = true;
                        }
                        suffix = try!(self.scan_tag_uri(false, secondary, &String::new(), &start_mark));
                    } else {
                        suffix = try!(self.scan_tag_uri(false, false, &handle, &start_mark));
                        handle = "!".to_owned();
                        // A special case: the '!' tag.  Set the handle to '' and the
                        // suffix to '!'.
                        if suffix.is_empty() {
                            handle.clear();
                            suffix = "!".to_owned();
                        }
                    }
                }
        
                self.lookahead(1);
                if is_blankz(self.ch()) {
                    // XXX: ex 7.2, an empty scalar can follow a secondary tag
                    Ok(Token(start_mark, TokenType::Tag(handle, suffix)))
                } else {
                    Err(ScanError::new(start_mark,
                        "while scanning a tag, did not find expected whitespace or line break"))
                }
            }
        
            fn scan_tag_handle(&mut self, directive: bool, mark: &Marker) -> Result<String, ScanError> {
                let mut string = String::new();
                self.lookahead(1);
                if self.ch() != '!' {
                    return Err(ScanError::new(*mark,
                        "while scanning a tag, did not find expected '!'"));
                }
        
                string.push(self.ch());
                self.skip();
        
                self.lookahead(1);
                while is_alpha(self.ch()) {
                    string.push(self.ch());
                    self.skip();
                    self.lookahead(1);
                }
        
                // Check if the trailing character is '!' and copy it.
                if self.ch() == '!' {
                    string.push(self.ch());
                    self.skip();
                } else if directive && string != "!" {
                    // It's either the '!' tag or not really a tag handle.  If it's a %TAG
                    // directive, it's an error.  If it's a tag token, it must be a part of
                    // URI.
                    return Err(ScanError::new(*mark,
                        "while parsing a tag directive, did not find expected '!'"));
                }
                Ok(string)
            }
        
            fn scan_tag_uri(&mut self, directive: bool, _is_secondary: bool,
                        head: &str, mark: &Marker) -> Result<String, ScanError> {
                let mut length = head.len();
                let mut string = String::new();
        
                // Copy the head if needed.
                // Note that we don't copy the leading '!' character.
                if length > 1 {
                    string.extend(head.chars().skip(1));
                }
        
                self.lookahead(1);
                /*
                 * The set of characters that may appear in URI is as follows:
                 *
                 *      '0'-'9', 'A'-'Z', 'a'-'z', '_', '-', ';', '/', '?', ':', '@', '&',
                 *      '=', '+', '$', ',', '.', '!', '~', '*', '\'', '(', ')', '[', ']',
                 *      '%'.
                 */
                while match self.ch() {
                    ';' | '/' | '?' | ':' | '@' | '&' => true,
                    '=' | '+' | '$' | ',' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' | '[' | ']' => true,
                    '%' => true,
                    c if is_alpha(c) => true,
                    _ => false
                } {
                    // Check if it is a URI-escape sequence.
                    if self.ch() == '%' {
                        string.push(try!(self.scan_uri_escapes(directive, mark)));
                    } else {
                        string.push(self.ch());
                        self.skip();
                    }
        
                    length += 1;
                    self.lookahead(1);
                }
        
                if length == 0 {
                    return Err(ScanError::new(*mark,
                        "while parsing a tag, did not find expected tag URI"));
                }
        
                Ok(string)
            }
        
            fn scan_uri_escapes(&mut self, _directive: bool, mark: &Marker)
                -> Result<char, ScanError> {
                let mut width = 0usize;
                let mut code = 0u32;
                loop {
                    self.lookahead(3);
        
                    if !(self.ch() == '%'
                         && is_hex(self.buffer[1])
                         && is_hex(self.buffer[2])) {
                        return Err(ScanError::new(*mark,
                            "while parsing a tag, did not find URI escaped octet"));
                    }
        
                    let octet = (as_hex(self.buffer[1]) << 4) + as_hex(self.buffer[2]);
                    if width == 0 {
                        width = match octet {
                            _ if octet & 0x80 == 0x00 => 1,
                            _ if octet & 0xE0 == 0xC0 => 2,
                            _ if octet & 0xF0 == 0xE0 => 3,
                            _ if octet & 0xF8 == 0xF0 => 4,
                            _ => {
                                return Err(ScanError::new(*mark,
                                    "while parsing a tag, found an incorrect leading UTF-8 octet"));
                            }
                        };
                        code = octet;
                    } else {
                        if octet & 0xc0 != 0x80 {
                                return Err(ScanError::new(*mark,
                                    "while parsing a tag, found an incorrect trailing UTF-8 octet"));
                        }
                        code = (code << 8) + octet;
                    }
        
                    self.skip();
                    self.skip();
                    self.skip();
        
                    width -= 1;
                    if width == 0 {
                        break;
                    }
                }
        
                match char::from_u32(code) {
                    Some(ch) => Ok(ch),
                    None => Err(ScanError::new(*mark,
                        "while parsing a tag, found an invalid UTF-8 codepoint"))
                }
            }
        
            fn fetch_anchor(&mut self, alias: bool) -> ScanResult {
                try!(self.save_simple_key());
                self.disallow_simple_key();
        
                let tok = try!(self.scan_anchor(alias));
        
                self.tokens.push_back(tok);
        
                Ok(())
            }
        
            fn scan_anchor(&mut self, alias: bool)
                -> Result<Token, ScanError> {
                let mut string = String::new();
                let start_mark = self.mark;
        
                self.skip();
                self.lookahead(1);
                while is_alpha(self.ch()) {
                    string.push(self.ch());
                    self.skip();
                    self.lookahead(1);
                }
        
                if string.is_empty()
                    || match self.ch() {
                        c if is_blankz(c) => false,
                        '?' | ':' | ',' | ']' | '}' | '%' | '@' | '`' => false,
                        _ => true
                    } {
                    return Err(ScanError::new(start_mark, "while scanning an anchor or alias, did not find expected alphabetic or numeric character"));
                }
        
                if alias {
                    Ok(Token(start_mark, TokenType::Alias(string)))
                } else {
                    Ok(Token(start_mark, TokenType::Anchor(string)))
                }
            }
        
            fn fetch_flow_collection_start(&mut self, tok :TokenType) -> ScanResult {
                // The indicators '[' and '{' may start a simple key.
                try!(self.save_simple_key());
        
                self.increase_flow_level();
        
                self.allow_simple_key();
        
                let start_mark = self.mark;
                self.skip();
        
                self.tokens.push_back(Token(start_mark, tok));
                Ok(())
            }
        
            fn fetch_flow_collection_end(&mut self, tok :TokenType) -> ScanResult {
                try!(self.remove_simple_key());
                self.decrease_flow_level();
        
                self.disallow_simple_key();
        
                let start_mark = self.mark;
                self.skip();
        
                self.tokens.push_back(Token(start_mark, tok));
                Ok(())
            }
        
            fn fetch_flow_entry(&mut self) -> ScanResult {
                try!(self.remove_simple_key());
                self.allow_simple_key();
        
                let start_mark = self.mark;
                self.skip();
        
                self.tokens.push_back(Token(start_mark, TokenType::FlowEntry));
                Ok(())
            }
        
            fn increase_flow_level(&mut self) {
                self.simple_keys.push(SimpleKey::new(Marker::new(0,0,0)));
                self.flow_level += 1;
            }
            fn decrease_flow_level(&mut self) {
                if self.flow_level > 0 {
                    self.flow_level -= 1;
                    self.simple_keys.pop().unwrap();
                }
            }
        
            fn fetch_block_entry(&mut self) -> ScanResult {
                if self.flow_level == 0 {
                    // Check if we are allowed to start a new entry.
                    if !self.simple_key_allowed {
                        return Err(ScanError::new(self.mark,
                                "block sequence entries are not allowed in this context"));
                    }
        
                    let mark = self.mark;
                    // generate BLOCK-SEQUENCE-START if indented
                    self.roll_indent(mark.col, None, TokenType::BlockSequenceStart, mark);
                } else {
                    // - * only allowed in block
                    return Err(ScanError::new(self.mark, r#""-" is only valid inside a block"#))
                }
                try!(self.remove_simple_key());
                self.allow_simple_key();
        
                let start_mark = self.mark;
                self.skip();
        
                self.tokens.push_back(Token(start_mark, TokenType::BlockEntry));
                Ok(())
            }
        
            fn fetch_document_indicator(&mut self, t: TokenType) -> ScanResult {
                self.unroll_indent(-1);
                try!(self.remove_simple_key());
                self.disallow_simple_key();
        
                let mark = self.mark;
        
                self.skip();
                self.skip();
                self.skip();
        
                self.tokens.push_back(Token(mark, t));
                Ok(())
            }
        
            fn fetch_block_scalar(&mut self, literal: bool) -> ScanResult {
                try!(self.save_simple_key());
                self.allow_simple_key();
                let tok = try!(self.scan_block_scalar(literal));
        
                self.tokens.push_back(tok);
                Ok(())
            }
        
            fn scan_block_scalar(&mut self, literal: bool) -> Result<Token, ScanError> {
                let start_mark = self.mark;
                let mut chomping: i32 = 0;
                let mut increment: usize = 0;
                let mut indent: usize = 0;
                let mut trailing_blank: bool;
                let mut leading_blank: bool = false;
        
                let mut string = String::new();
                let mut leading_break = String::new();
                let mut trailing_breaks = String::new();
        
                // skip '|' or '>'
                self.skip();
                self.lookahead(1);
        
                if self.ch() == '+' || self.ch() == '-' {
                    if self.ch() == '+' {
                        chomping = 1;
                    } else {
                        chomping = -1;
                    }
                    self.skip();
                    self.lookahead(1);
                    if is_digit(self.ch()) {
                        if self.ch() == '0' {
                            return Err(ScanError::new(start_mark,
                                    "while scanning a block scalar, found an intendation indicator equal to 0"));
                        }
                        increment = (self.ch() as usize) - ('0' as usize);
                        self.skip();
                    }
                } else if is_digit(self.ch()) {
                    if self.ch() == '0' {
                        return Err(ScanError::new(start_mark,
                                 "while scanning a block scalar, found an intendation indicator equal to 0"));
                    }
        
                    increment = (self.ch() as usize) - ('0' as usize);
                    self.skip();
                    self.lookahead(1);
                    if self.ch() == '+' || self.ch() == '-' {
                        if self.ch() == '+' {
                            chomping = 1;
                        } else {
                            chomping = -1;
                        }
                        self.skip();
                    }
                }
        
                // Eat whitespaces and comments to the end of the line.
                self.lookahead(1);
        
                while is_blank(self.ch()) {
                    self.skip();
                    self.lookahead(1);
                }
        
                if self.ch() == '#' {
                    while !is_breakz(self.ch()) {
                        self.skip();
                        self.lookahead(1);
                    }
                }
        
                // Check if we are at the end of the line.
                if !is_breakz(self.ch()) {
                    return Err(ScanError::new(start_mark,
                            "while scanning a block scalar, did not find expected comment or line break"));
                }
        
                if is_break(self.ch()) {
                    self.lookahead(2);
                    self.skip_line();
                }
        
                if increment > 0 {
                    indent = if self.indent >= 0 { (self.indent + increment as isize) as usize } else { increment }
                }
                // Scan the leading line breaks and determine the indentation level if needed.
                try!(self.block_scalar_breaks(&mut indent, &mut trailing_breaks));
        
                self.lookahead(1);
        
                let start_mark = self.mark;
        
                while self.mark.col == indent && !is_z(self.ch()) {
                    // We are at the beginning of a non-empty line.
                    trailing_blank = is_blank(self.ch());
                    if !literal && !leading_break.is_empty()
                        && !leading_blank && !trailing_blank {
                            if trailing_breaks.is_empty() {
                                string.push(' ');
                            }
                            leading_break.clear();
                    } else {
                        string.push_str(&leading_break);
                        leading_break.clear();
                    }
        
                    string.push_str(&trailing_breaks);
                    trailing_breaks.clear();
        
                    leading_blank = is_blank(self.ch());
        
                    while !is_breakz(self.ch()) {
                        string.push(self.ch());
                        self.skip();
                        self.lookahead(1);
                    }
                    // break on EOF
                    if is_z(self.ch()) { break; }
        
                    self.lookahead(2);
                    self.read_break(&mut leading_break);
        
                    // Eat the following intendation spaces and line breaks.
                    try!(self.block_scalar_breaks(&mut indent, &mut trailing_breaks));
                }
        
                // Chomp the tail.
                if chomping != -1 {
                    string.push_str(&leading_break);
                }
        
                if chomping == 1 {
                    string.push_str(&trailing_breaks);
                }
        
                if literal {
                    Ok(Token(start_mark, TokenType::Scalar(TScalarStyle::Literal, string)))
                } else {
                    Ok(Token(start_mark, TokenType::Scalar(TScalarStyle::Foled, string)))
                }
            }
        
            fn block_scalar_breaks(&mut self, indent: &mut usize, breaks: &mut String) -> ScanResult {
                let mut max_indent = 0;
                loop {
                    self.lookahead(1);
                    while (*indent == 0 || self.mark.col < *indent)
                        && self.buffer[0] == ' ' {
                            self.skip();
                            self.lookahead(1);
                    }
        
                    if self.mark.col > max_indent {
                        max_indent = self.mark.col;
                    }
        
                    // Check for a tab character messing the intendation.
                    if (*indent == 0 || self.mark.col < *indent)
                        && self.buffer[0] == '\t' {
                        return Err(ScanError::new(self.mark,
                                "while scanning a block scalar, found a tab character where an intendation space is expected"));
                    }
        
                    if !is_break(self.ch()) {
                        break;
                    }
        
                    self.lookahead(2);
                    // Consume the line break.
                    self.read_break(breaks);
                }
        
                if *indent == 0 {
                    *indent = max_indent;
                    if *indent < (self.indent + 1) as usize {
                        *indent = (self.indent + 1) as usize;
                    }
                    if *indent < 1 {
                        *indent = 1;
                    }
                }
                Ok(())
            }
        
            fn fetch_flow_scalar(&mut self, single: bool) -> ScanResult {
                try!(self.save_simple_key());
                self.disallow_simple_key();
        
                let tok = try!(self.scan_flow_scalar(single));
        
                self.tokens.push_back(tok);
                Ok(())
            }
        
            fn scan_flow_scalar(&mut self, single: bool) -> Result<Token, ScanError> {
                let start_mark = self.mark;
        
                let mut string = String::new();
                let mut leading_break = String::new();
                let mut trailing_breaks = String::new();
                let mut whitespaces = String::new();
                let mut leading_blanks;
        
                /* Eat the left quote. */
                self.skip();
        
                loop {
                    /* Check for a document indicator. */
                    self.lookahead(4);
        
                    if self.mark.col == 0 &&
                        (((self.buffer[0] == '-') &&
                        (self.buffer[1] == '-') &&
                        (self.buffer[2] == '-')) ||
                        ((self.buffer[0] == '.') &&
                        (self.buffer[1] == '.') &&
                        (self.buffer[2] == '.'))) &&
                        is_blankz(self.buffer[3]) {
                            return Err(ScanError::new(start_mark,
                                "while scanning a quoted scalar, found unexpected document indicator"));
                        }
        
                    if is_z(self.ch()) {
                            return Err(ScanError::new(start_mark,
                                "while scanning a quoted scalar, found unexpected end of stream"));
                    }
        
                    self.lookahead(2);
        
                    leading_blanks = false;
                    // Consume non-blank characters.
        
                    while !is_blankz(self.ch()) {
                        match self.ch() {
                            // Check for an escaped single quote.
                            '\'' if self.buffer[1] == '\'' && single => {
                                string.push('\'');
                                self.skip();
                                self.skip();
                            },
                            // Check for the right quote.
                            '\'' if single => { break; },
                            '"' if !single => { break; },
                            // Check for an escaped line break.
                            '\\' if !single && is_break(self.buffer[1]) => {
                                self.lookahead(3);
                                self.skip();
                                self.skip_line();
                                leading_blanks = true;
                                break;
                            }
                            // Check for an escape sequence.
                            '\\' if !single => {
                                let mut code_length = 0usize;
                                match self.buffer[1] {
                                    '0' => string.push('\0'),
                                    'a' => string.push('\x07'),
                                    'b' => string.push('\x08'),
                                    't' | '\t' => string.push('\t'),
                                    'n' => string.push('\n'),
                                    'v' => string.push('\x0b'),
                                    'f' => string.push('\x0c'),
                                    'r' => string.push('\x0d'),
                                    'e' => string.push('\x1b'),
                                    ' ' => string.push('\x20'),
                                    '"' => string.push('"'),
                                    '\'' => string.push('\''),
                                    '\\' => string.push('\\'),
                                    // NEL (#x85)
                                    'N' => string.push(char::from_u32(0x85).unwrap()),
                                    // #xA0
                                    '_' => string.push(char::from_u32(0xA0).unwrap()),
                                    // LS (#x2028)
                                    'L' => string.push(char::from_u32(0x2028).unwrap()),
                                    // PS (#x2029)
                                    'P' => string.push(char::from_u32(0x2029).unwrap()),
                                    'x' => code_length = 2,
                                    'u' => code_length = 4,
                                    'U' => code_length = 8,
                                    _ => return Err(ScanError::new(start_mark,
                                            "while parsing a quoted scalar, found unknown escape character"))
                                }
                                self.skip();
                                self.skip();
                                // Consume an arbitrary escape code.
                                if code_length > 0 {
                                    self.lookahead(code_length);
                                    let mut value = 0u32;
                                    for i in 0..code_length {
                                        if !is_hex(self.buffer[i]) {
                                            return Err(ScanError::new(start_mark,
                                                "while parsing a quoted scalar, did not find expected hexdecimal number"));
                                        }
                                        value = (value << 4) + as_hex(self.buffer[i]);
                                    }
        
                                    let ch = match char::from_u32(value) {
                                        Some(v) => v,
                                        None => {
                                            return Err(ScanError::new(start_mark,
                                                "while parsing a quoted scalar, found invalid Unicode character escape code"));
                                        }
                                    };
                                    string.push(ch);
        
                                    for _ in 0..code_length {
                                        self.skip();
                                    }
                                }
                            },
                            c => { string.push(c); self.skip(); }
                        }
                        self.lookahead(2);
                    }
                    self.lookahead(1);
                    match self.ch() {
                        '\'' if single => { break; },
                        '"' if !single => { break; },
                        _ => {}
                    }
        
                    // Consume blank characters.
                    while is_blank(self.ch()) || is_break(self.ch()) {
                        if is_blank(self.ch()) {
                            // Consume a space or a tab character.
                            if leading_blanks {
                                self.skip();
                            } else {
                                whitespaces.push(self.ch());
                                self.skip();
                            }
                        } else {
                            self.lookahead(2);
                            // Check if it is a first line break.
                            if leading_blanks {
                                self.read_break(&mut trailing_breaks);
                            } else {
                                whitespaces.clear();
                                self.read_break(&mut leading_break);
                                leading_blanks = true;
                            }
                        }
                        self.lookahead(1);
                    }
                    // Join the whitespaces or fold line breaks.
                    if leading_blanks {
                        if leading_break.is_empty() {
                            string.push_str(&leading_break);
                            string.push_str(&trailing_breaks);
                            trailing_breaks.clear();
                            leading_break.clear();
                        } else {
                            if trailing_breaks.is_empty() {
                                string.push(' ');
                            } else {
                                string.push_str(&trailing_breaks);
                                trailing_breaks.clear();
                            }
                            leading_break.clear();
                        }
                    } else {
                        string.push_str(&whitespaces);
                        whitespaces.clear();
                    }
                } // loop
        
                // Eat the right quote.
                self.skip();
        
                if single {
                    Ok(Token(start_mark, TokenType::Scalar(TScalarStyle::SingleQuoted, string)))
                } else {
                    Ok(Token(start_mark, TokenType::Scalar(TScalarStyle::DoubleQuoted, string)))
                }
            }
        
            fn fetch_plain_scalar(&mut self) -> ScanResult {
                try!(self.save_simple_key());
                self.disallow_simple_key();
        
                let tok = try!(self.scan_plain_scalar());
        
                self.tokens.push_back(tok);
                Ok(())
            }
        
            fn scan_plain_scalar(&mut self) -> Result<Token, ScanError> {
                let indent = self.indent + 1;
                let start_mark = self.mark;
        
                let mut string = String::new();
                let mut leading_break = String::new();
                let mut trailing_breaks = String::new();
                let mut whitespaces = String::new();
                let mut leading_blanks = false;
        
                loop {
                    /* Check for a document indicator. */
                    self.lookahead(4);
        
                    if self.mark.col == 0 &&
                        (((self.buffer[0] == '-') &&
                         (self.buffer[1] == '-') &&
                         (self.buffer[2] == '-')) ||
                            ((self.buffer[0] == '.') &&
                             (self.buffer[1] == '.') &&
                             (self.buffer[2] == '.'))) &&
                            is_blankz(self.buffer[3]) {
                                break;
                            }
        
                    if self.ch() == '#' { break; }
                    while !is_blankz(self.ch()) {
                        if self.flow_level > 0 && self.ch() == ':'
                            && is_blankz(self.ch()) {
                                return Err(ScanError::new(start_mark,
                                                          "while scanning a plain scalar, found unexpected ':'"));
                            }
                        // indicators ends a plain scalar
                        match self.ch() {
                            ':' if is_blankz(self.buffer[1]) => break,
                            ',' | ':' | '?' | '[' | ']' |'{' |'}' if self.flow_level > 0 => break,
                            _ => {}
                        }
        
                        if leading_blanks || !whitespaces.is_empty() {
                            if leading_blanks {
                                if leading_break.is_empty() {
                                    string.push_str(&leading_break);
                                    string.push_str(&trailing_breaks);
                                    trailing_breaks.clear();
                                    leading_break.clear();
                                } else {
                                    if trailing_breaks.is_empty() {
                                        string.push(' ');
                                    } else {
                                        string.push_str(&trailing_breaks);
                                        trailing_breaks.clear();
                                    }
                                    leading_break.clear();
        
                                }
                                leading_blanks = false;
                            } else {
                                string.push_str(&whitespaces);
                                whitespaces.clear();
                            }
                        }
        
                        string.push(self.ch());
                        self.skip();
                        self.lookahead(2);
                    }
                    // is the end?
                    if !(is_blank(self.ch()) || is_break(self.ch())) { break; }
                    self.lookahead(1);
        
                    while is_blank(self.ch()) || is_break(self.ch()) {
                        if is_blank(self.ch()) {
                            if leading_blanks && (self.mark.col as isize) < indent
                                && self.ch() == '\t' {
                                    return Err(ScanError::new(start_mark,
                                        "while scanning a plain scalar, found a tab"));
                            }
        
                            if leading_blanks {
                                self.skip();
                            } else {
                                whitespaces.push(self.ch());
                                self.skip();
                            }
                        } else {
                            self.lookahead(2);
                            // Check if it is a first line break
                            if leading_blanks {
                                self.read_break(&mut trailing_breaks);
                            } else {
                                whitespaces.clear();
                                self.read_break(&mut leading_break);
                                leading_blanks = true;
                            }
                        }
                        self.lookahead(1);
                    }
        
                    // check intendation level
                    if self.flow_level == 0 && (self.mark.col as isize) < indent {
                        break;
                    }
                }
        
                if leading_blanks {
                    self.allow_simple_key();
                }
        
                Ok(Token(start_mark, TokenType::Scalar(TScalarStyle::Plain, string)))
            }
        
            fn fetch_key(&mut self) -> ScanResult {
                let start_mark = self.mark;
                if self.flow_level == 0 {
                    // Check if we are allowed to start a new key (not nessesary simple).
                    if !self.simple_key_allowed {
                        return Err(ScanError::new(self.mark, "mapping keys are not allowed in this context"));
                    }
                    self.roll_indent(start_mark.col, None,
                        TokenType::BlockMappingStart, start_mark);
                }
        
                try!(self.remove_simple_key());
        
                if self.flow_level == 0 {
                    self.allow_simple_key();
                } else {
                    self.disallow_simple_key();
                }
        
                self.skip();
                self.tokens.push_back(Token(start_mark, TokenType::Key));
                Ok(())
            }
        
            fn fetch_value(&mut self) -> ScanResult {
                let sk = self.simple_keys.last().unwrap().clone();
                let start_mark = self.mark;
                if sk.possible {
                    // insert simple key
                    let tok = Token(sk.mark, TokenType::Key);
                    let tokens_parsed = self.tokens_parsed;
                    self.insert_token(sk.token_number - tokens_parsed, tok);
        
                    // Add the BLOCK-MAPPING-START token if needed.
                    self.roll_indent(sk.mark.col, Some(sk.token_number),
                        TokenType::BlockMappingStart, start_mark);
        
                    self.simple_keys.last_mut().unwrap().possible = false;
                    self.disallow_simple_key();
                } else {
                    // The ':' indicator follows a complex key.
                    if self.flow_level == 0 {
                        if !self.simple_key_allowed {
                            return Err(ScanError::new(start_mark,
                                "mapping values are not allowed in this context"));
                        }
        
                        self.roll_indent(start_mark.col, None,
                            TokenType::BlockMappingStart, start_mark);
                    }
        
                    if self.flow_level == 0 {
                        self.allow_simple_key();
                    } else {
                        self.disallow_simple_key();
                    }
                }
                self.skip();
                self.tokens.push_back(Token(start_mark, TokenType::Value));
        
                Ok(())
            }
        
            fn roll_indent(&mut self, col: usize, number: Option<usize>,
                           tok: TokenType, mark: Marker) {
                if self.flow_level > 0 {
                    return;
                }
        
                if self.indent < col as isize {
                    self.indents.push(self.indent);
                    self.indent = col as isize;
                    let tokens_parsed = self.tokens_parsed;
                    match number {
                        Some(n) => self.insert_token(n - tokens_parsed, Token(mark, tok)),
                        None => self.tokens.push_back(Token(mark, tok))
                    }
                }
            }
        
            fn unroll_indent(&mut self, col: isize) {
                if self.flow_level > 0 {
                    return;
                }
                while self.indent > col {
                    self.tokens.push_back(Token(self.mark, TokenType::BlockEnd));
                    self.indent = self.indents.pop().unwrap();
                }
            }
        
            fn save_simple_key(&mut self) -> Result<(), ScanError> {
                let required = self.flow_level > 0 && self.indent == (self.mark.col as isize);
                if self.simple_key_allowed {
                    let mut sk = SimpleKey::new(self.mark);
                    sk.possible = true;
                    sk.required = required;
                    sk.token_number = self.tokens_parsed + self.tokens.len();
        
                    try!(self.remove_simple_key());
        
                    self.simple_keys.pop();
                    self.simple_keys.push(sk);
                }
                Ok(())
            }
        
            fn remove_simple_key(&mut self) -> ScanResult {
                let last = self.simple_keys.last_mut().unwrap();
                if last.possible && last.required {
                    return Err(ScanError::new(self.mark, "simple key expected"));
                }
        
                last.possible = false;
                Ok(())
            }
        
        }
    }
    
    pub mod parser
    {
        use ::
        {
            *,
        };
        /*
        use scanner::*;
        use std::collections::HashMap;
        */
        #[derive(Clone, Copy, PartialEq, Debug, Eq)]
        enum State {
            StreamStart,
            ImplicitDocumentStart,
            DocumentStart,
            DocumentContent,
            DocumentEnd,
            BlockNode,
            // BlockNodeOrIndentlessSequence,
            // FlowNode,
            BlockSequenceFirstEntry,
            BlockSequenceEntry,
            IndentlessSequenceEntry,
            BlockMappingFirstKey,
            BlockMappingKey,
            BlockMappingValue,
            FlowSequenceFirstEntry,
            FlowSequenceEntry,
            FlowSequenceEntryMappingKey,
            FlowSequenceEntryMappingValue,
            FlowSequenceEntryMappingEnd,
            FlowMappingFirstKey,
            FlowMappingKey,
            FlowMappingValue,
            FlowMappingEmptyValue,
            End
        }
        
        /// `Event` is used with the low-level event base parsing API,
        /// see `EventReceiver` trait.
        #[derive(Clone, PartialEq, Debug, Eq)]
        pub enum Event {
            /// Reserved for internal use
            Nothing,
            StreamStart,
            StreamEnd,
            DocumentStart,
            DocumentEnd,
            /// Refer to an anchor ID
            Alias(usize),
            /// Value, style, anchor_id, tag
            Scalar(String, TScalarStyle, usize, Option<TokenType>),
            /// Anchor ID
            SequenceStart(usize),
            SequenceEnd,
            /// Anchor ID
            MappingStart(usize),
            MappingEnd
        }
        
        impl Event {
            fn empty_scalar() -> Event {
                // a null scalar
                Event::Scalar("~".to_owned(), TScalarStyle::Plain, 0, None)
            }
        
            fn empty_scalar_with_anchor(anchor: usize, tag: Option<TokenType>) -> Event {
                Event::Scalar("".to_owned(), TScalarStyle::Plain, anchor, tag)
            }
        }
        
        #[derive(Debug)]
        pub struct Parser<T> {
            scanner: Scanner<T>,
            states: Vec<State>,
            state: State,
            marks: Vec<Marker>,
            token: Option<Token>,
            current: Option<(Event, Marker)>,
            anchors: HashMap<String, usize>,
            anchor_id: usize,
        }
        
        
        pub trait EventReceiver {
            fn on_event(&mut self, ev: Event);
        }
        
        
        pub trait MarkedEventReceiver {
            fn on_event(&mut self, ev: Event, _mark: Marker);
        }
        
        impl<R: EventReceiver> MarkedEventReceiver for R {
            fn on_event(&mut self, ev: Event, _mark: Marker) {
                self.on_event(ev)
            }
        }
        
        pub type ParseResult = Result<(Event, Marker), ScanError>;
        
        impl<T: Iterator<Item=char>> Parser<T> {
            pub fn new(src: T) -> Parser<T> {
                Parser {
                    scanner: Scanner::new(src),
                    states: Vec::new(),
                    state: State::StreamStart,
                    marks: Vec::new(),
                    token: None,
                    current: None,
        
                    anchors: HashMap::new(),
                    // valid anchor_id starts from 1
                    anchor_id: 1,
                }
            }
        
            pub fn peek(&mut self) -> Result<&(Event, Marker), ScanError> {
                match self.current {
                    Some(ref x) => Ok(x),
                    None => {
                        self.current = Some(try!(self.next()));
                        self.peek()
                    }
                }
            }
        
            pub fn next(&mut self) -> ParseResult {
                match self.current {
                    None => self.parse(),
                    Some(_) => {
                        Ok(self.current.take().unwrap())
                    }
                }
            }
        
            fn peek_token(&mut self) -> Result<&Token, ScanError> {
                match self.token {
                    None =>  {
                        self.token = Some(try!(self.scan_next_token()));
                        Ok(self.token.as_ref().unwrap())
                    },
                    Some(ref tok) => Ok(tok)
                }
            }
        
            fn scan_next_token(&mut self) -> Result<Token, ScanError> {
                let token = self.scanner.next();
                match token {
                    None =>
                        match self.scanner.get_error() {
                            None => Err(ScanError::new(self.scanner.mark(), "unexpected eof")),
                            Some(e) => Err(e),
                        },
                    Some(tok) => Ok(tok)
                }
            }
        
            fn fetch_token(&mut self) -> Token {
                self.token.take().expect("fetch_token needs to be preceded by peek_token")
            }
        
        
            fn skip(&mut self) {
                self.token = None;
                //self.peek_token();
            }
            fn pop_state(&mut self) {
                self.state = self.states.pop().unwrap()
            }
            fn push_state(&mut self, state: State) {
                self.states.push(state);
            }
        
            fn parse(&mut self) -> ParseResult {
                if self.state == State::End {
                    return Ok((Event::StreamEnd, self.scanner.mark()));
                }
                let (ev, mark) = try!(self.state_machine());
                // println!("EV {:?}", ev);
                Ok((ev, mark))
            }
        
            pub fn load<R: MarkedEventReceiver>(&mut self, recv: &mut R, multi: bool)
                -> Result<(), ScanError> {
                if !self.scanner.stream_started() {
                    let (ev, mark) = try!(self.next());
                    assert_eq!(ev, Event::StreamStart);
                    recv.on_event(ev, mark);
                }
        
                if self.scanner.stream_ended() {
                    // XXX has parsed?
                    recv.on_event(Event::StreamEnd, self.scanner.mark());
                    return Ok(());
                }
                loop {
                    let (ev, mark) = try!(self.next());
                    if ev == Event::StreamEnd {
                        recv.on_event(ev, mark);
                        return Ok(());
                    }
                    // clear anchors before a new document
                    self.anchors.clear();
                    try!(self.load_document(ev, mark, recv));
                    if !multi {
                        break;
                    }
                }
                Ok(())
            }
        
            fn load_document<R: MarkedEventReceiver>(&mut self, first_ev: Event, mark: Marker, recv: &mut R)
                -> Result<(), ScanError> {
                assert_eq!(first_ev, Event::DocumentStart);
                recv.on_event(first_ev, mark);
        
                let (ev, mark) = try!(self.next());
                try!(self.load_node(ev, mark, recv));
        
                // DOCUMENT-END is expected.
                let (ev, mark) = try!(self.next());
                assert_eq!(ev, Event::DocumentEnd);
                recv.on_event(ev, mark);
        
                Ok(())
            }
        
            fn load_node<R: MarkedEventReceiver>(&mut self, first_ev: Event, mark: Marker, recv: &mut R)
                -> Result<(), ScanError> {
                match first_ev {
                    Event::Alias(..) | Event::Scalar(..) => {
                        recv.on_event(first_ev, mark);
                        Ok(())
                    },
                    Event::SequenceStart(_) => {
                        recv.on_event(first_ev, mark);
                        self.load_sequence(recv)
                    },
                    Event::MappingStart(_) => {
                        recv.on_event(first_ev, mark);
                        self.load_mapping(recv)
                    },
                    _ => { println!("UNREACHABLE EVENT: {:?}", first_ev);
                        unreachable!(); }
                }
            }
        
            fn load_mapping<R: MarkedEventReceiver>(&mut self, recv: &mut R)
                -> Result<(), ScanError> {
                let (mut key_ev, mut key_mark) = try!(self.next());
                while key_ev != Event::MappingEnd {
                    // key
                    try!(self.load_node(key_ev, key_mark, recv));
        
                    // value
                    let (ev, mark) = try!(self.next());
                    try!(self.load_node(ev, mark, recv));
        
                    // next event
                    let (ev, mark) = try!(self.next());
                    key_ev = ev;
                    key_mark = mark;
        
                }
                recv.on_event(key_ev, key_mark);
                Ok(())
            }
        
            fn load_sequence<R: MarkedEventReceiver>(&mut self, recv: &mut R)
                -> Result<(), ScanError> {
                let (mut ev, mut mark) = try!(self.next());
                while ev != Event::SequenceEnd {
                    try!(self.load_node(ev, mark, recv));
        
                    // next event
                    let (next_ev, next_mark) = try!(self.next());
                    ev = next_ev;
                    mark = next_mark;
                }
                recv.on_event(ev, mark);
                Ok(())
            }
        
            fn state_machine(&mut self) -> ParseResult {
                // let next_tok = try!(self.peek_token());
                // println!("cur_state {:?}, next tok: {:?}", self.state, next_tok);
                match self.state {
                    State::StreamStart => self.stream_start(),
        
                    State::ImplicitDocumentStart => self.document_start(true),
                    State::DocumentStart => self.document_start(false),
                    State::DocumentContent => self.document_content(),
                    State::DocumentEnd => self.document_end(),
        
                    State::BlockNode => self.parse_node(true, false),
                    // State::BlockNodeOrIndentlessSequence => self.parse_node(true, true),
                    // State::FlowNode => self.parse_node(false, false),
        
                    State::BlockMappingFirstKey => self.block_mapping_key(true),
                    State::BlockMappingKey => self.block_mapping_key(false),
                    State::BlockMappingValue => self.block_mapping_value(),
        
                    State::BlockSequenceFirstEntry => self.block_sequence_entry(true),
                    State::BlockSequenceEntry => self.block_sequence_entry(false),
        
                    State::FlowSequenceFirstEntry => self.flow_sequence_entry(true),
                    State::FlowSequenceEntry => self.flow_sequence_entry(false),
        
                    State::FlowMappingFirstKey => self.flow_mapping_key(true),
                    State::FlowMappingKey => self.flow_mapping_key(false),
                    State::FlowMappingValue => self.flow_mapping_value(false),
        
                    State::IndentlessSequenceEntry => self.indentless_sequence_entry(),
        
                    State::FlowSequenceEntryMappingKey => self.flow_sequence_entry_mapping_key(),
                    State::FlowSequenceEntryMappingValue => self.flow_sequence_entry_mapping_value(),
                    State::FlowSequenceEntryMappingEnd => self.flow_sequence_entry_mapping_end(),
                    State::FlowMappingEmptyValue => self.flow_mapping_value(true),
        
                    /* impossible */
                    State::End => unreachable!(),
                }
            }
        
            fn stream_start(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::StreamStart(_)) => {
                        self.state = State::ImplicitDocumentStart;
                        self.skip();
                        Ok((Event::StreamStart, mark))
                    },
                    Token(mark, _) => Err(ScanError::new(mark,
                        "did not find expected <stream-start>")),
                }
            }
        
            fn document_start(&mut self, implicit: bool) -> ParseResult {
                if !implicit {
                    while let TokenType::DocumentEnd = try!(self.peek_token()).1 {
                        self.skip();
                    }
                }
        
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::StreamEnd) => {
                        self.state = State::End;
                        self.skip();
                        Ok((Event::StreamEnd, mark))
                    },
                    Token(_, TokenType::VersionDirective(..))
                    | Token(_, TokenType::TagDirective(..))
                    | Token(_, TokenType::DocumentStart) => {
                        // explicit document
                        self._explict_document_start()
                    },
                    Token(mark, _) if implicit => {
                        try!(self.parser_process_directives());
                        self.push_state(State::DocumentEnd);
                        self.state = State::BlockNode;
                        Ok((Event::DocumentStart, mark))
                    },
                    _ => {
                        // explicit document
                        self._explict_document_start()
                    }
                }
            }
        
            fn parser_process_directives(&mut self) -> Result<(), ScanError> {
                loop {
                    match try!(self.peek_token()).1 {
                        TokenType::VersionDirective(_, _) => {
                            // XXX parsing with warning according to spec
                            //if major != 1 || minor > 2 {
                            //    return Err(ScanError::new(tok.0,
                            //        "found incompatible YAML document"));
                            //}
                        },
                        TokenType::TagDirective(..) => {
                            // TODO add tag directive
                        },
                        _ => break
                    }
                    self.skip();
                }
                // TODO tag directive
                Ok(())
            }
        
            fn _explict_document_start(&mut self) -> ParseResult {
                try!(self.parser_process_directives());
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::DocumentStart) => {
                        self.push_state(State::DocumentEnd);
                        self.state = State::DocumentContent;
                        self.skip();
                        Ok((Event::DocumentStart, mark))
                    }    
                    Token(mark, _) => Err(ScanError::new(mark, "did not find expected <document start>"))
                }        
            }
        
            fn document_content(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::VersionDirective(..))
                    | Token(mark, TokenType::TagDirective(..))
                    | Token(mark, TokenType::DocumentStart)
                    | Token(mark, TokenType::DocumentEnd)
                    | Token(mark, TokenType::StreamEnd) => {
                        self.pop_state();
                        // empty scalar
                        Ok((Event::empty_scalar(), mark))
                    },
                    _ => {
                        self.parse_node(true, false)
                    }
                }
            }
        
            fn document_end(&mut self) -> ParseResult {
                let mut _implicit = true;
                let marker: Marker = match *try!(self.peek_token()) {
                    Token(mark, TokenType::DocumentEnd) => {
                        self.skip();
                        _implicit = false;
                        mark
                    },
                    Token(mark, _) => mark
                };
                
                // TODO tag handling
                self.state = State::DocumentStart;
                Ok((Event::DocumentEnd, marker))
            }
        
            fn register_anchor(&mut self, name: String, _: &Marker) -> Result<usize, ScanError> {
                // anchors can be overrided/reused
                // if self.anchors.contains_key(name) {
                //     return Err(ScanError::new(*mark,
                //         "while parsing anchor, found duplicated anchor"));
                // }
                let new_id = self.anchor_id;
                self.anchor_id += 1;
                self.anchors.insert(name, new_id);
                Ok(new_id)
            }
        
            fn parse_node(&mut self, block: bool, indentless_sequence: bool) -> ParseResult {
                let mut anchor_id = 0;
                let mut tag = None;
                match *try!(self.peek_token()) {
                    Token(_, TokenType::Alias(_)) => {
                        self.pop_state();
                        if let Token(mark, TokenType::Alias(name)) = self.fetch_token() {
                            match self.anchors.get(&name) {
                                None => return Err(ScanError::new(mark, "while parsing node, found unknown anchor")),
                                Some(id) => return Ok((Event::Alias(*id), mark))
                            }
                        } else {
                            unreachable!()
                        }
                    },
                    Token(_, TokenType::Anchor(_)) => {
                        if let Token(mark, TokenType::Anchor(name)) = self.fetch_token() {
                            anchor_id = try!(self.register_anchor(name, &mark));
                            if let TokenType::Tag(..) = try!(self.peek_token()).1 {
                                if let tg @ TokenType::Tag(..) = self.fetch_token().1 {
                                    tag = Some(tg);
                                } else {
                                    unreachable!()
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    },
                    Token(_, TokenType::Tag(..)) => {
                        if let tg @ TokenType::Tag(..) = self.fetch_token().1 {
                            tag = Some(tg);
                            if let TokenType::Anchor(_) = try!(self.peek_token()).1 {
                                if let Token(mark, TokenType::Anchor(name)) = self.fetch_token() {
                                    anchor_id = try!(self.register_anchor(name, &mark));
                                } else {
                                    unreachable!()
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    },
                    _ => {}
                }
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::BlockEntry) if indentless_sequence => {
                        self.state = State::IndentlessSequenceEntry;
                        Ok((Event::SequenceStart(anchor_id), mark))
                    },
                    Token(_, TokenType::Scalar(..)) => {
                        self.pop_state();
                        if let Token(mark, TokenType::Scalar(style, v)) = self.fetch_token() {
                            Ok((Event::Scalar(v, style, anchor_id, tag), mark))
                        } else {
                            unreachable!()
                        }
                    },
                    Token(mark, TokenType::FlowSequenceStart) => {
                        self.state = State::FlowSequenceFirstEntry;
                        Ok((Event::SequenceStart(anchor_id), mark))
                    },
                    Token(mark, TokenType::FlowMappingStart) => {
                        self.state = State::FlowMappingFirstKey;
                        Ok((Event::MappingStart(anchor_id), mark))
                    },
                    Token(mark, TokenType::BlockSequenceStart) if block => {
                        self.state = State::BlockSequenceFirstEntry;
                        Ok((Event::SequenceStart(anchor_id), mark))
                    },
                    Token(mark, TokenType::BlockMappingStart) if block => {
                        self.state = State::BlockMappingFirstKey;
                        Ok((Event::MappingStart(anchor_id), mark))
                    },
                    // ex 7.2, an empty scalar can follow a secondary tag
                    Token(mark, _) if tag.is_some() || anchor_id > 0 => {
                        self.pop_state();
                        Ok((Event::empty_scalar_with_anchor(anchor_id, tag), mark))
                    },
                    Token(mark, _) => { Err(ScanError::new(mark, "while parsing a node, did not find expected node content")) }
                }
            }
        
            fn block_mapping_key(&mut self, first: bool) -> ParseResult {
                // skip BlockMappingStart
                if first {
                    let _ = try!(self.peek_token());
                    //self.marks.push(tok.0);
                    self.skip();
                }
                match *try!(self.peek_token()) {
                    Token(_, TokenType::Key) => {
                        self.skip();
                        match *try!(self.peek_token()) {
                            Token(mark, TokenType::Key)
                            | Token(mark, TokenType::Value)
                            | Token(mark, TokenType::BlockEnd) => {
                                self.state = State::BlockMappingValue;
                                // empty scalar
                                Ok((Event::empty_scalar(), mark))
                            }
                            _ => {
                                self.push_state(State::BlockMappingValue);
                                self.parse_node(true, true)
                            }
                        }
                    },
                    // XXX(chenyh): libyaml failed to parse spec 1.2, ex8.18
                    Token(mark, TokenType::Value) => {
                        self.state = State::BlockMappingValue;
                        Ok((Event::empty_scalar(), mark))
                    },
                    Token(mark, TokenType::BlockEnd) => {
                        self.pop_state();
                        self.skip();
                        Ok((Event::MappingEnd, mark))
                    },
                    Token(mark, _) => {
                        Err(ScanError::new(mark, "while parsing a block mapping, did not find expected key"))
                    }
                }
            }
        
            fn block_mapping_value(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(_, TokenType::Value) => {
                        self.skip();
                        match *try!(self.peek_token()) {
                            Token(mark, TokenType::Key)
                            | Token(mark, TokenType::Value)
                            | Token(mark, TokenType::BlockEnd) => {
                                self.state = State::BlockMappingKey;
                                // empty scalar
                                Ok((Event::empty_scalar(), mark))
                            },
                            _ => {
                                self.push_state(State::BlockMappingKey);
                                self.parse_node(true, true)
                            }
                        }
                    },
                    Token(mark, _) => {
                        self.state = State::BlockMappingKey;
                        // empty scalar
                        Ok((Event::empty_scalar(), mark))
                    }
                }
            }
        
            fn flow_mapping_key(&mut self, first: bool) -> ParseResult {
                if first {
                    let _ = try!(self.peek_token());
                    self.skip();
                }
                let marker: Marker = {
                    match *try!(self.peek_token()) {
                        Token(mark, TokenType::FlowMappingEnd) => mark,
                        Token(mark, _) => {
                            if !first {
                                match *try!(self.peek_token()) {
                                    Token(_, TokenType::FlowEntry) => self.skip(),
                                    Token(mark, _) => return Err(ScanError::new(mark,
                                        "while parsing a flow mapping, did not find expected ',' or '}'"))
                                }
                            }
        
                            match *try!(self.peek_token()) {
                                Token(_, TokenType::Key) => {
                                    self.skip();
                                    match *try!(self.peek_token()) {
                                        Token(mark, TokenType::Value)
                                        | Token(mark, TokenType::FlowEntry)
                                        | Token(mark, TokenType::FlowMappingEnd) => {
                                            self.state = State::FlowMappingValue;
                                            return Ok((Event::empty_scalar(), mark));
                                        },
                                        _ => {
                                            self.push_state(State::FlowMappingValue);
                                            return self.parse_node(false, false);
                                        }
                                    }
                                },
                                Token(marker, TokenType::Value) => {
                                    self.state = State::FlowMappingValue;
                                    return Ok((Event::empty_scalar(), marker));
                                },
                                Token(_, TokenType::FlowMappingEnd) => (),
                                _ => {
                                    self.push_state(State::FlowMappingEmptyValue);
                                    return self.parse_node(false, false);
                                }
                            }
        
                            mark
                        }
                    }
                };
        
                self.pop_state();
                self.skip();
                Ok((Event::MappingEnd, marker))
            }
        
            fn flow_mapping_value(&mut self, empty: bool) -> ParseResult {
                let mark: Marker = {
                    if empty {
                        let Token(mark, _) = *try!(self.peek_token());
                        self.state = State::FlowMappingKey;
                        return Ok((Event::empty_scalar(), mark));
                    } else {
                        match *try!(self.peek_token()) {
                            Token(marker, TokenType::Value) => {
                                self.skip();
                                match try!(self.peek_token()).1 {
                                    TokenType::FlowEntry
                                        | TokenType::FlowMappingEnd => { },
                                    _ => {
                                        self.push_state(State::FlowMappingKey);
                                        return self.parse_node(false, false);
                                    }
                                }
                                marker
                            },
                            Token(marker, _) => marker
                        }
                    }
                };
                
                self.state = State::FlowMappingKey;
                Ok((Event::empty_scalar(), mark))
            }
        
            fn flow_sequence_entry(&mut self, first: bool) -> ParseResult {
                // skip FlowMappingStart
                if first {
                    let _ = try!(self.peek_token());
                    //self.marks.push(tok.0);
                    self.skip();
                }
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::FlowSequenceEnd) => {
                        self.pop_state();
                        self.skip();
                        return Ok((Event::SequenceEnd, mark));
                    },
                    Token(_, TokenType::FlowEntry) if !first => {
                        self.skip();
                    },
                    Token(mark, _) if !first => {
                        return Err(ScanError::new(mark,
                                "while parsing a flow sequence, expectd ',' or ']'"));
                    }
                    _ => { /* next */ }
                }
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::FlowSequenceEnd) => {
                        self.pop_state();
                        self.skip();
                        Ok((Event::SequenceEnd, mark))
                    },
                    Token(mark, TokenType::Key) => {
                        self.state = State::FlowSequenceEntryMappingKey;
                        self.skip();
                        Ok((Event::MappingStart(0), mark))
                    }
                    _ => {
                        self.push_state(State::FlowSequenceEntry);
                        self.parse_node(false, false)
                    }
                }
            }
        
            fn indentless_sequence_entry(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(_, TokenType::BlockEntry) => (),
                    Token(mark, _) => {
                        self.pop_state();
                        return Ok((Event::SequenceEnd, mark));
                    }
                }
                self.skip();
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::BlockEntry)
                    | Token(mark, TokenType::Key)
                    | Token(mark, TokenType::Value)
                    | Token(mark, TokenType::BlockEnd) => {
                        self.state = State::IndentlessSequenceEntry;
                        Ok((Event::empty_scalar(), mark))
                    },
                    _ => {
                        self.push_state(State::IndentlessSequenceEntry);
                        self.parse_node(true, false)
                    }
                }
            }
        
            fn block_sequence_entry(&mut self, first: bool) -> ParseResult {
                // BLOCK-SEQUENCE-START
                if first {
                    let _ = try!(self.peek_token());
                    //self.marks.push(tok.0);
                    self.skip();
                }
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::BlockEnd) => {
                        self.pop_state();
                        self.skip();
                        Ok((Event::SequenceEnd, mark))
                    },
                    Token(_, TokenType::BlockEntry) => {
                        self.skip();
                        match *try!(self.peek_token()) {
                            Token(mark, TokenType::BlockEntry)
                            | Token(mark, TokenType::BlockEnd) => {
                                self.state = State::BlockSequenceEntry;
                                Ok((Event::empty_scalar(), mark))
                            },
                            _ => {
                                self.push_state(State::BlockSequenceEntry);
                                self.parse_node(true, false)
                            }
                        }
                    },
                    Token(mark, _) => {
                        Err(ScanError::new(mark,
                                "while parsing a block collection, did not find expected '-' indicator"))
                    }
                }
            }
        
            fn flow_sequence_entry_mapping_key(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(mark, TokenType::Value)
                    | Token(mark, TokenType::FlowEntry)
                    | Token(mark, TokenType::FlowSequenceEnd) => {
                        self.skip();
                        self.state = State::FlowSequenceEntryMappingValue;
                        Ok((Event::empty_scalar(), mark))
                    },
                    _ => {
                        self.push_state(State::FlowSequenceEntryMappingValue);
                        self.parse_node(false, false)
                    }
                }
            }
        
            fn flow_sequence_entry_mapping_value(&mut self) -> ParseResult {
                match *try!(self.peek_token()) {
                    Token(_, TokenType::Value) => {
                            self.skip();
                            self.state = State::FlowSequenceEntryMappingValue;
                            match *try!(self.peek_token()) {
                                Token(mark, TokenType::FlowEntry)
                                | Token(mark, TokenType::FlowSequenceEnd) => {
                                    self.state = State::FlowSequenceEntryMappingEnd;
                                    Ok((Event::empty_scalar(), mark))
                                },
                                _ => {
                                    self.push_state(State::FlowSequenceEntryMappingEnd);
                                    self.parse_node(false, false)
                                }
                            }
                    },
                    Token(mark, _) => {
                        self.state = State::FlowSequenceEntryMappingEnd;
                        Ok((Event::empty_scalar(), mark))
                    }
                }
            }
        
            fn flow_sequence_entry_mapping_end(&mut self) -> ParseResult {
                self.state = State::FlowSequenceEntry;
                Ok((Event::MappingEnd, self.scanner.mark()))
            }
        }
    }
    
    pub mod emitter
    {
        use ::
        {
            *,
        };
        /*
        use std::fmt::{self, Display};
        use std::convert::From;
        use std::error::Error;
        use yaml::{Hash, Yaml};
        */
        #[derive(Copy, Clone, Debug)]
        pub enum EmitError {
                FmtError(fmt::Error),
                BadHashmapKey,
        }
        
        impl Error for EmitError {
            fn description(&self) -> &str {
                match *self {
                    EmitError::FmtError(ref err) => err.description(),
                    EmitError::BadHashmapKey => "bad hashmap key",
                }
            }
        
            fn cause(&self) -> Option<&Error> {
                None
            }
        }
        
        impl Display for EmitError {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    EmitError::FmtError(ref err) => Display::fmt(err, formatter),
                    EmitError::BadHashmapKey => formatter.write_str("bad hashmap key"),
                }
            }
        }
        
        impl From<fmt::Error> for EmitError {
            fn from(f: fmt::Error) -> Self {
                EmitError::FmtError(f)
            }
        }
        
        pub struct YamlEmitter<'a> {
            writer: &'a mut fmt::Write,
            best_indent: usize,
            compact: bool,
        
            level: isize,
        }
        
        pub type EmitResult = Result<(), EmitError>;
        
        // from serialize::json
        fn escape_str(wr: &mut fmt::Write, v: &str) -> Result<(), fmt::Error> {
            try!(wr.write_str("\""));
        
            let mut start = 0;
        
            for (i, byte) in v.bytes().enumerate() {
                let escaped = match byte {
                    b'"' => "\\\"",
                    b'\\' => "\\\\",
                    b'\x00' => "\\u0000",
                    b'\x01' => "\\u0001",
                    b'\x02' => "\\u0002",
                    b'\x03' => "\\u0003",
                    b'\x04' => "\\u0004",
                    b'\x05' => "\\u0005",
                    b'\x06' => "\\u0006",
                    b'\x07' => "\\u0007",
                    b'\x08' => "\\b",
                    b'\t' => "\\t",
                    b'\n' => "\\n",
                    b'\x0b' => "\\u000b",
                    b'\x0c' => "\\f",
                    b'\r' => "\\r",
                    b'\x0e' => "\\u000e",
                    b'\x0f' => "\\u000f",
                    b'\x10' => "\\u0010",
                    b'\x11' => "\\u0011",
                    b'\x12' => "\\u0012",
                    b'\x13' => "\\u0013",
                    b'\x14' => "\\u0014",
                    b'\x15' => "\\u0015",
                    b'\x16' => "\\u0016",
                    b'\x17' => "\\u0017",
                    b'\x18' => "\\u0018",
                    b'\x19' => "\\u0019",
                    b'\x1a' => "\\u001a",
                    b'\x1b' => "\\u001b",
                    b'\x1c' => "\\u001c",
                    b'\x1d' => "\\u001d",
                    b'\x1e' => "\\u001e",
                    b'\x1f' => "\\u001f",
                    b'\x7f' => "\\u007f",
                    _ => { continue; }
                };
        
                if start < i {
                    try!(wr.write_str(&v[start..i]));
                }
        
                try!(wr.write_str(escaped));
        
                start = i + 1;
            }
        
            if start != v.len() {
                try!(wr.write_str(&v[start..]));
            }
        
            try!(wr.write_str("\""));
            Ok(())
        }
        
        impl<'a> YamlEmitter<'a> {
            pub fn new(writer: &'a mut fmt::Write) -> YamlEmitter {
                YamlEmitter {
                    writer: writer,
                    best_indent: 2,
                    compact: true,
        
                    level: -1
                }
            }
        
            /// Set 'compact inline notation' on or off, as described for block
            /// [sequences](http://www.yaml.org/spec/1.2/spec.html#id2797382)
            /// and
            /// [mappings](http://www.yaml.org/spec/1.2/spec.html#id2798057).
            ///
            /// In this form, blocks cannot have any properties (such as anchors
            /// or tags), which should be OK, because this emitter doesn't
            /// (currently) emit those anyways.
            pub fn compact(&mut self, compact: bool) {
              self.compact = compact;
            }
        
            /// Determine if this emitter is using 'compact inline notation'.
            pub fn is_compact(&self) -> bool {
              self.compact
            }
        
            pub fn dump(&mut self, doc: &Yaml) -> EmitResult {
                // write DocumentStart
                try!(write!(self.writer, "---\n"));
                self.level = -1;
                self.emit_node(doc)
            }
        
            fn write_indent(&mut self) -> EmitResult {
                if self.level <= 0 { return Ok(()); }
                for _ in 0..self.level {
                    for _ in 0..self.best_indent {
                        try!(write!(self.writer, " "));
                    }
                }
                Ok(())
            }
        
            fn emit_node(&mut self, node: &Yaml) -> EmitResult {
                match *node {
                    Yaml::Array(ref v) => self.emit_array(v),
                    Yaml::Hash(ref h) => self.emit_hash(h),
                    Yaml::String(ref v) => {
                        if need_quotes(v) {
                            try!(escape_str(self.writer, v));
                        }
                        else {
                            try!(write!(self.writer, "{}", v));
                        }
                        Ok(())
                    },
                    Yaml::Boolean(v) => {
                        if v {
                            try!(self.writer.write_str("true"));
                        } else {
                            try!(self.writer.write_str("false"));
                        }
                        Ok(())
                    },
                    Yaml::Integer(v) => {
                        try!(write!(self.writer, "{}", v));
                        Ok(())
                    },
                    Yaml::Real(ref v) => {
                        try!(write!(self.writer, "{}", v));
                        Ok(())
                    },
                    Yaml::Null | Yaml::BadValue => {
                        try!(write!(self.writer, "~"));
                        Ok(())
                    },
                    // XXX(chenyh) Alias
                    _ => { Ok(()) }
                }
            }
        
            fn emit_array(&mut self, v: &[Yaml]) -> EmitResult {
                if v.is_empty() {
                    try!(write!(self.writer, "[]"));
                } else {
                    self.level += 1;
                    for (cnt, x) in v.iter().enumerate() {
                        if cnt > 0 {
                            try!(write!(self.writer, "\n"));
                            try!(self.write_indent());
                        }
                        try!(write!(self.writer, "-"));
                        try!(self.emit_val(true, x));
                    }
                    self.level -= 1;
                }
                Ok(())
            }
        
            fn emit_hash(&mut self, h: &Hash) -> EmitResult {
                if h.is_empty() {
                    try!(self.writer.write_str("{}"));
                } else {
                    self.level += 1;
                    for (cnt, (k, v)) in h.iter().enumerate() {
                        let complex_key = match *k {
                          Yaml::Hash(_) | Yaml::Array(_) => true,
                          _ => false,
                        };
                        if cnt > 0 {
                            try!(write!(self.writer, "\n"));
                            try!(self.write_indent());
                        }
                        if complex_key {
                          try!(write!(self.writer, "?"));
                          try!(self.emit_val(true, k));
                          try!(write!(self.writer, "\n"));
                          try!(self.write_indent());
                          try!(write!(self.writer, ":"));
                          try!(self.emit_val(true, v));
                        } else {
                          try!(self.emit_node(k));
                          try!(write!(self.writer, ":"));
                          try!(self.emit_val(false, v));
                        }
                    }
                    self.level -= 1;
                }
                Ok(())
            }
        
            /// Emit a yaml as a hash or array value: i.e., which should appear
            /// following a ":" or "-", either after a space, or on a new line.
            /// If `inline` is true, then the preceeding characters are distinct
            /// and short enough to respect the compact flag.
            fn emit_val(&mut self, inline: bool, val: &Yaml) -> EmitResult {
                match *val {
                    Yaml::Array(ref v) => {
                        if (inline && self.compact) || v.is_empty() {
                            try!(write!(self.writer, " "));
                        } else {
                            try!(write!(self.writer, "\n"));
                            self.level += 1;
                            try!(self.write_indent());
                            self.level -= 1;
                        }
                        self.emit_array(v)
                    },
                    Yaml::Hash(ref h) => {
                        if (inline && self.compact) || h.is_empty() {
                            try!(write!(self.writer, " "));
                        } else {
                            try!(write!(self.writer, "\n"));
                            self.level += 1;
                            try!(self.write_indent());
                            self.level -= 1;
                        }
                        self.emit_hash(h)
                    },
                    _ => {
                        try!(write!(self.writer, " "));
                        self.emit_node(val)
                    }
                }
            }
        }
        
        /// Check if the string requires quoting.
        fn need_quotes(string: &str) -> bool {
            fn need_quotes_spaces(string: &str) -> bool {
                string.starts_with(' ')
                    || string.ends_with(' ')
            }
        
            string == ""
            || need_quotes_spaces(string)
            || string.contains(|character: char| {
                match character {
                    ':' | '{' | '}' | '[' | ']' | ',' | '&' | '*' | '#' | '?' | '|' | '-' | '<' | '>' | '=' | '!' | '%' | '@' | '`' | '\"' | '\'' | '\\' | '\0' ... '\x06' | '\t' | '\n' | '\r' | '\x0e' ... '\x1a' | '\x1c' ... '\x1f' => true,
                    _ => false,
                }
            })
            || [// http://yaml.org/type/bool.html
                "y","Y","yes","Yes","YES","n","N","no","No","NO",
                "True", "TRUE", "true", "False", "FALSE", "false",
                "on","On","ON","off","Off","OFF",
                // http://yaml.org/type/null.html
                "null","Null","NULL", "~"
            ].contains(&string)
            || string.starts_with('.')
            || string.parse::<i64>().is_ok()
            || string.parse::<f64>().is_ok()
        }
    }
    
    pub use self::scanner::ScanError;
    pub use self::parser::Event;
    pub use self::yaml::{Yaml, YamlLoader};
    pub use self::emitter::{YamlEmitter, EmitError};
}
/*
*/
pub mod calculator
{
    use ::
    {
        *,
    };
    /*
    // via: https://github.com/pest-parser/book/blob/b6a42eb7/examples/calculator/src/main.rs
    use std::num::Wrapping as W;
    
    use pest::Parser;
    use pest::iterators::{Pair, Pairs};
    use pest::pratt_parser::{Assoc, Op, PrattParser};
    */
    #[derive(Parser)]
    #[grammar = "calculator/grammar.pest"]
    struct Calculator;
    
    lazy_static! {
        static ref PRATT_PARSER: PrattParser<Rule> = {
            use Rule::*;
            use Assoc::*;
    
            PrattParser::new()
                .op(Op::infix(add, Left) | Op::infix(subtract, Left))
                .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
                .op(Op::infix(power, Right))
        };
    }
    
    pub fn eval_int(expression: Pairs<Rule>) -> i64 {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::num => primary.as_str().parse::<i64>().unwrap(),
                Rule::expr => eval_int(primary.into_inner()),
                _ => unreachable!(),
            })
            .map_infix(|lhs: i64, op: Pair<Rule>, rhs: i64| match op.as_rule() {
                Rule::add => (W(lhs) + W(rhs)).0,
                Rule::subtract => (W(lhs) - W(rhs)).0,
                Rule::multiply => (W(lhs) * W(rhs)).0,
                Rule::divide => {
                    if rhs == 0 {
                        (lhs as f64 / 0.0) as i64
                    } else {
                        (W(lhs) / W(rhs)).0
                    }
                }
                Rule::power => lhs.pow(rhs as u32),
                _ => unreachable!(),
            })
            .parse(expression)
    }
    
    pub fn eval_float(expression: Pairs<Rule>) -> f64 {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::num => primary.as_str().parse::<f64>().unwrap(),
                Rule::expr => eval_float(primary.into_inner()),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => lhs + rhs,
                Rule::subtract => lhs - rhs,
                Rule::multiply => lhs * rhs,
                Rule::divide => lhs / rhs,
                Rule::power => lhs.powf(rhs),
                _ => unreachable!(),
            })
            .parse(expression)
    }
    
    pub fn calculate(line: &str) -> Result<pest::iterators::Pairs<'_, Rule>, pest::error::Error<Rule>> {
        Calculator::parse(Rule::calculation, line)
    }
}

pub mod completers
{
    use ::
    {
        *,
    };
    /*
    use std::path::Path;
    use std::sync::Arc;
    
    use lineread::complete::{Completer, Completion};
    use lineread::prompter::Prompter;
    use lineread::terminal::Terminal;
    
    pub mod dots;
    pub mod env;
    pub mod make;
    pub mod path;
    pub mod ssh;
    pub mod utils;
    
    use crate::libs;
    use crate::parsers;
    use crate::shell;
    use crate::tools;
    */
    pub mod dots
    {
        use ::
        {
            *
        };
        /*
        use std::borrow::Cow;
        use std::fs::File;
        use std::io::{Read, Write};
        use std::path::Path;
        
        use lineread::complete::escape;
        use lineread::complete::escaped_word_start;
        use lineread::complete::unescape;
        use lineread::complete::Suffix;
        use lineread::complete::{Completer, Completion};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;
        use yaml_rust::{Yaml, YamlLoader};
        use yaml_rust::yaml::Hash;
        
        use crate::execute;
        use crate::parsers;
        use crate::tools;
        */
        /// Performs completion by searching dotfiles
        pub struct DotsCompleter;
        
        impl<Term: Terminal> Completer<Term> for DotsCompleter {
            fn complete(
                &self,
                word: &str,
                reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let line = reader.buffer();
                Some(complete_dots(line, word))
            }
        
            fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
                escaped_word_start(&line[..end])
            }
        
            fn quote<'a>(&self, word: &'a str) -> Cow<'a, str> {
                escape(word)
            }
        
            fn unquote<'a>(&self, word: &'a str) -> Cow<'a, str> {
                unescape(word)
            }
        }
        
        fn get_dot_file(line: &str) -> (String, String) {
            let args = parsers::parser_line::line_to_plain_tokens(line);
            let dir = tools::get_user_completer_dir();
            let dot_file = format!("{}/{}.yaml", dir, args[0]);
            if !Path::new(&dot_file).exists() {
                return (String::new(), String::new());
            }
            let sub_cmd = if (args.len() >= 3 && !args[1].starts_with('-'))
                || (args.len() >= 2 && !args[1].starts_with('-') && line.ends_with(' '))
            {
                args[1].as_str()
            } else {
                ""
            };
        
            (dot_file, sub_cmd.to_string())
        }
        
        fn handle_lv1_string(res: &mut Vec<Completion>,
                             value: &str, word: &str) {
            if !value.starts_with(word) && !value.starts_with('`') {
                return;
            }
        
            let linfo = parsers::parser_line::parse_line(value);
            let tokens = linfo.tokens;
            if tokens.len() == 1 && tokens[0].0 == "`" {
                log!("run subcmd: {:?}", &tokens[0].1);
                let cr = execute::run(&tokens[0].1);
                let v: Vec<&str> = cr.stdout.split(|c| c == '\n' || c == ' ').collect();
                for s in v {
                    if s.trim().is_empty() {
                        continue;
                    }
                    handle_lv1_string(res, s, word);
                }
                return;
            }
        
            let display = None;
            let suffix = Suffix::Default;
            res.push(Completion {
                completion: value.to_string(),
                display,
                suffix,
            });
        }
        
        fn handle_lv1_hash(res: &mut Vec<Completion>,
                           h: &Hash, word: &str) {
            for v in h.values() {
                if let Yaml::Array(ref arr) = v {
                    for s in arr {
                        if let Yaml::String(value) = s {
                            if !value.starts_with(word) && !value.starts_with('`') {
                                continue;
                            }
                            handle_lv1_string(res, value, word);
                        }
                    }
                }
            }
        }
        
        fn complete_dots(line: &str, word: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            if line.trim().is_empty() {
                return res;
            }
            let (dot_file, sub_cmd) = get_dot_file(line);
            if dot_file.is_empty() {
                return res;
            }
        
            let mut f;
            match File::open(&dot_file) {
                Ok(x) => f = x,
                Err(e) => {
                    println_stderr!("\ncicada: open dot_file error: {:?}", e);
                    return res;
                }
            }
        
            let mut s = String::new();
            match f.read_to_string(&mut s) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!("\ncicada: read_to_string error: {:?}", e);
                    return res;
                }
            }
        
            let docs = match YamlLoader::load_from_str(&s) {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("\ncicada: Bad Yaml file: {}: {:?}", dot_file, e);
                    return res;
                }
            };
        
            for doc in docs.iter() {
                match *doc {
                    Yaml::Array(ref v) => {
                        for x in v {
                            match *x {
                                Yaml::String(ref name) => {
                                    if !sub_cmd.is_empty() {
                                        continue;
                                    }
                                    handle_lv1_string(&mut res, name, word);
                                }
                                Yaml::Hash(ref h) => {
                                    if sub_cmd.is_empty() {
                                        for k in h.keys() {
                                            if let Yaml::String(value) = k {
                                                handle_lv1_string(&mut res, value, word);
                                            }
                                        }
                                    } else {
                                        let key = Yaml::from_str(&sub_cmd);
                                        if !h.contains_key(&key) {
                                            continue;
                                        }
                                        handle_lv1_hash(&mut res, h, word);
                                    }
                                }
                                _ => {
                                    println_stderr!("\nThis yaml file is in bad format: {}", dot_file);
                                }
                            }
                        }
                    }
                    _ => {
                        println_stderr!("\nThis yaml file is in bad format: {}", dot_file);
                    }
                }
            }
            res
        }

    }
    
    pub mod env
    {
        use ::
        {
            *
        };
        /*
        use std::env;
        use std::sync::Arc;
        
        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;
        
        use crate::shell;
        */
        pub struct EnvCompleter {
            pub sh: Arc<shell::Shell>,
        }
        
        impl<Term: Terminal> Completer<Term> for EnvCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap(self.sh.clone());
                match sh {
                    Ok(x) => Some(complete_env(&x, word)),
                    Err(x) => Some(complete_env(&x, word)),
                }
            }
        }
        
        fn complete_env(sh: &shell::Shell, path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            if path.trim().is_empty() {
                return res;
            }
            let mut prefix = path.to_string();
            prefix.remove(0);
        
            for (key, _) in env::vars_os() {
                let env_name = key.to_string_lossy().to_string();
                if env_name.starts_with(&prefix) {
                    res.push(Completion {
                        completion: format!("${}", env_name),
                        display: None,
                        suffix: Suffix::Default,
                    });
                }
            }
        
            // sh.envs is a just clone here; see FIXME in main.rs
            for key in sh.envs.keys() {
                if key.starts_with(&prefix) {
                    res.push(Completion {
                        completion: format!("${}", key),
                        display: None,
                        suffix: Suffix::Default,
                    });
                }
            }
        
            res
        }
    }
    
    pub mod make
    {
        use ::
        {
            *
        };
        /*
        use std::env;
        use std::fs::File;
        use std::io::{BufRead, BufReader, Write};
        
        use regex::Regex;
        
        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;
        */
        pub struct MakeCompleter;
        
        impl<Term: Terminal> Completer<Term> for MakeCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_make(word))
            }
        }
        
        fn handle_file(ci: &mut Vec<Completion>, path: &str, file_path: &str, current_dir: &str) {
            if let Ok(f) = File::open(file_path) {
                let file = BufReader::new(&f);
                let re_cmd = match Regex::new(r"^ *([^ ]+):") {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!("cicada: regex build error: {:?}", e);
                        return;
                    }
                };
        
                let re_include = match Regex::new(r"^ *include  *([^ ]+) *$") {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!("cicada: regex build error: {:?}", e);
                        return;
                    }
                };
        
                for line in file.lines().map_while(Result::ok) {
                    if re_cmd.is_match(&line) {
                        for cap in re_cmd.captures_iter(&line) {
                            if !cap[1].starts_with(path) {
                                continue;
                            }
                            ci.push(Completion {
                                completion: cap[1].to_string(),
                                display: None,
                                suffix: Suffix::Default,
                            });
                        }
                    }
                    if re_include.is_match(&line) {
                        for cap in re_include.captures_iter(&line) {
                            let _file = &cap[1];
                            if _file.contains('/') {
                                handle_file(ci, path, _file, current_dir);
                            } else {
                                let make_file = current_dir.to_owned() + "/" + _file;
                                handle_file(ci, path, &make_file, current_dir);
                            }
                        }
                    }
                }
            }
        }
        
        fn complete_make(path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let current_dir = match env::current_dir() {
                Ok(dir) => match dir.to_str() {
                    Some(s) => s.to_string(),
                    None => {
                        println!("cicada: to_str error");
                        return res;
                    }
                },
                Err(e) => {
                    println!("cicada: get current_dir error: {:?}", e);
                    return res;
                }
            };
        
            let make_file = format!("{}/Makefile", current_dir);
            handle_file(&mut res, path, &make_file, &current_dir);
            res
        }
    }
    
    pub mod path
    {
        use ::
        {
            *
        };
        /*
        use std::collections::HashSet;
        use std::env;
        use std::fs::read_dir;
        use std::io::Write;
        use std::iter::FromIterator;
        use std::os::unix::fs::PermissionsExt;
        use std::path::MAIN_SEPARATOR;
        use std::sync::Arc;
        
        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::terminal::Terminal;
        use lineread::Prompter;
        
        use crate::completers::utils;
        use crate::libs;
        use crate::parsers;
        use crate::shell;
        use crate::tools;
        */
        pub struct BinCompleter {
            pub sh: Arc<shell::Shell>,
        }
        pub struct CdCompleter;
        pub struct PathCompleter;
        
        fn is_env_prefix(line: &str) -> bool {
            libs::re::re_contains(line, r" *\$[a-zA-Z_][A-Za-z0-9_]*")
        }
        
        fn is_pipelined(path: &str) -> bool {
            if !path.contains('|') {
                return false;
            }
            !path.starts_with('"') && !path.starts_with('\'')
        }
        
        impl<Term: Terminal> Completer<Term> for BinCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap(self.sh.clone());
                match sh {
                    Ok(x) => Some(complete_bin(&x, word)),
                    Err(x) => Some(complete_bin(&x, word)),
                }
            }
        }
        
        impl<Term: Terminal> Completer<Term> for PathCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_path(word, false))
            }
        }
        
        impl<Term: Terminal> Completer<Term> for CdCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_path(word, true))
            }
        }
        
        fn needs_expand_home(line: &str) -> bool {
            libs::re::re_contains(line, r"( +~ +)|( +~/)|(^ *~/)|( +~ *$)")
        }
        
        /// Returns a sorted list of paths whose prefix matches the given path.
        pub fn complete_path(word: &str, for_dir: bool) -> Vec<Completion> {
            let is_env = is_env_prefix(word);
            let mut res = Vec::new();
            let linfo = parsers::parser_line::parse_line(word);
            let tokens = linfo.tokens;
            let (path, path_sep) = if tokens.is_empty() {
                (String::new(), String::new())
            } else {
                let (ref _path_sep, ref _path) = tokens[tokens.len() - 1];
                (_path.clone(), _path_sep.clone())
            };
        
            let (_, _dir_orig, _f) = split_pathname(&path, "");
            let dir_orig = if _dir_orig.is_empty() {
                String::new()
            } else {
                _dir_orig.clone()
            };
            let mut path_extended = path.clone();
            if needs_expand_home(&path_extended) {
                utils::expand_home_string(&mut path_extended)
            }
            utils::expand_env_string(&mut path_extended);
        
            let (_, _dir_lookup, file_name) = split_pathname(&path_extended, "");
            let dir_lookup = if _dir_lookup.is_empty() {
                ".".to_string()
            } else {
                _dir_lookup.clone()
            };
            // let dir_lookup = _dir_lookup.unwrap_or(".");
            if let Ok(entries) = read_dir(dir_lookup) {
                for entry in entries.flatten() {
                    let pathbuf = entry.path();
                    let is_dir = pathbuf.is_dir();
                    if for_dir && !is_dir {
                        continue;
                    }
        
                    let entry_name = entry.file_name();
                    // TODO: Deal with non-UTF8 paths in some way
                    if let Ok(_path) = entry_name.into_string() {
                        if _path.starts_with(&file_name) {
                            let (name, display) = if !dir_orig.is_empty() {
                                (
                                    format!("{}{}{}", dir_orig, MAIN_SEPARATOR, _path),
                                    Some(_path),
                                )
                            } else {
                                (_path, None)
                            };
                            let mut name = str::replace(name.as_str(), "//", "/");
                            if path_sep.is_empty() && !is_env {
                                name = tools::escape_path(&name);
                            }
                            let mut quoted = false;
                            if !path_sep.is_empty() {
                                name = tools::wrap_sep_string(&path_sep, &name);
                                quoted = true;
                            }
                            let suffix = if is_dir {
                                if quoted {
                                    name.pop();
                                }
                                Suffix::Some(MAIN_SEPARATOR)
                            } else {
                                Suffix::Default
                            };
                            res.push(Completion {
                                completion: name,
                                display,
                                suffix,
                            });
                        }
                    }
                }
            }
            res.sort_by(|a, b| a.completion.cmp(&b.completion));
            res
        }
        
        // Split optional directory and prefix. (see its test cases for more details)
        fn split_pathname(path: &str, prefix: &str) -> (String, String, String) {
            if is_pipelined(path) {
                let tokens: Vec<&str> = path.rsplitn(2, '|').collect();
                let prefix = format!("{}|", tokens[1]);
                return split_pathname(tokens[0], &prefix);
            }
            match path.rfind('/') {
                Some(pos) => (
                    prefix.to_string(),
                    path[..=pos].to_string(),
                    path[pos + 1..].to_string(),
                ),
                None => (prefix.to_string(), String::new(), path.to_string()),
            }
        }
        
        /// Returns a sorted list of paths whose prefix matches the given path.
        fn complete_bin(sh: &shell::Shell, path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let (prefix, _, fname) = split_pathname(path, "");
            let env_path = match env::var("PATH") {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: env error when complete_bin: {:?}", e);
                    return res;
                }
            };
        
            let mut checker: HashSet<String> = HashSet::new();
        
            // handle alias, builtins, and functions
            for func in sh.funcs.keys() {
                if !func.starts_with(&fname) {
                    continue;
                }
                if checker.contains(func) {
                    continue;
                }
                checker.insert(func.clone());
                res.push(Completion {
                    completion: func.to_owned(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }
            for alias in sh.aliases.keys() {
                if !alias.starts_with(&fname) {
                    continue;
                }
                if checker.contains(alias) {
                    continue;
                }
                checker.insert(alias.clone());
                res.push(Completion {
                    completion: alias.to_owned(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }
        
            let builtins = vec![
                "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
                "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
                "minfd", "set", "unset", "unpath",
            ];
            for item in &builtins {
                if !item.starts_with(&fname) {
                    continue;
                }
                if checker.contains(*item) {
                    continue;
                }
                checker.insert(item.to_string());
                res.push(Completion {
                    completion: item.to_string(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }
        
            let vec_path: Vec<&str> = env_path.split(':').collect();
            let path_list: HashSet<&str> = HashSet::from_iter(vec_path.iter().cloned());
        
            for p in &path_list {
                if let Ok(list) = read_dir(p) {
                    for entry in list.flatten() {
                        if let Ok(name) = entry.file_name().into_string() {
                            if name.starts_with(&fname) {
                                let _mode = match entry.metadata() {
                                    Ok(x) => x,
                                    Err(e) => {
                                        println_stderr!("cicada: metadata error: {:?}", e);
                                        continue;
                                    }
                                };
                                let mode = _mode.permissions().mode();
                                if mode & 0o111 == 0 {
                                    // not binary
                                    continue;
                                }
                                if checker.contains(&name) {
                                    continue;
                                }
        
                                let display = None;
                                let suffix = Suffix::Default;
                                checker.insert(name.clone());
                                // TODO: need to handle quoted: `$ "foo#bar"`
                                let name_e = tools::escape_path(&name);
                                let name_e = format!("{}{}", prefix, name_e);
                                res.push(Completion {
                                    completion: name_e,
                                    display,
                                    suffix,
                                });
                            }
                        }
                    }
                }
            }
            res
        }
    }
    
    pub mod ssh
    {
        use ::
        {
            *
        };
        /*
        use std::fs::File;
        use std::io::{BufRead, BufReader};
        
        use regex::Regex;
        
        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::terminal::Terminal;
        use lineread::Prompter;
        
        use crate::tools;
        */
        pub struct SshCompleter;
        
        impl<Term: Terminal> Completer<Term> for SshCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_ssh(word))
            }
        }
        
        fn complete_ssh(path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let home = tools::get_user_home();
            let ssh_config = home + "/.ssh/config";
            if let Ok(f) = File::open(&ssh_config) {
                let file = BufReader::new(&f);
                let re = match Regex::new(r"^ *(?i)host +([^ ]+)") {
                    Ok(x) => x,
                    Err(e) => {
                        println!("Regex build error: {:?}", e);
                        return res;
                    }
                };
                for line in file.lines().map_while(Result::ok) {
                    if !re.is_match(&line) {
                        continue;
                    }
                    for cap in re.captures_iter(&line) {
                        if !cap[1].starts_with(path) {
                            continue;
                        }
                        res.push(Completion {
                            completion: cap[1].to_string(),
                            display: None,
                            suffix: Suffix::Default,
                        });
                    }
                }
            }
            res
        }
    }
    
    pub mod utils
    {
        use ::
        {
            *
        };
        /*
        use regex::Regex;
        use std::env;
        
        use crate::libs;
        use crate::tools;
        */
        pub fn expand_home_string(text: &mut String) {
            let v = vec![
                r"(?P<head> +)~(?P<tail> +)",
                r"(?P<head> +)~(?P<tail>/)",
                r"^(?P<head> *)~(?P<tail>/)",
                r"(?P<head> +)~(?P<tail> *$)",
            ];
            for item in &v {
                let re;
                if let Ok(x) = Regex::new(item) {
                    re = x;
                } else {
                    return;
                }
                let home = tools::get_user_home();
                let ss = text.clone();
                let to = format!("$head{}$tail", home);
                let result = re.replace_all(ss.as_str(), to.as_str());
                *text = result.to_string();
            }
        }
        
        pub fn expand_env_string(text: &mut String) {
            // expand "$HOME/.local/share" to "/home/tom/.local/share"
            if !text.starts_with('$') {
                return;
            }
            let ptn = r"^\$([A-Za-z_][A-Za-z0-9_]*)";
            let mut env_value = String::new();
            match libs::re::find_first_group(ptn, text) {
                Some(x) => {
                    if let Ok(val) = env::var(&x) {
                        env_value = val;
                    }
                }
                None => {
                    return;
                }
            }
        
            if env_value.is_empty() {
                return;
            }
            let t = text.clone();
            *text = libs::re::replace_all(&t, ptn, &env_value);
        }

    }
        
    pub struct CicadaCompleter {
        pub sh: Arc<shell::Shell>,
    }
    
    fn for_make(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *make ")
    }
    
    fn for_env(line: &str) -> bool {
        libs::re::re_contains(line, r" *\$[_a-zA-Z0-9]*$")
    }
    
    fn for_ssh(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *(ssh|scp).* +[^ \./]+ *$")
    }
    
    fn for_cd(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *cd +")
    }
    
    fn for_bin(line: &str) -> bool {
        let ptn = r"(^ *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)|(^.+\| *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)";
        libs::re::re_contains(line, ptn)
    }
    
    fn for_dots(line: &str) -> bool {
        let args = parsers::parser_line::line_to_plain_tokens(line);
        let len = args.len();
        if len == 0 {
            return false;
        }
        let dir = tools::get_user_completer_dir();
        let dot_file = format!("{}/{}.yaml", dir, args[0]);
        Path::new(dot_file.as_str()).exists()
    }
    
    impl<Term: Terminal> Completer<Term> for CicadaCompleter {
        fn complete(
            &self,
            word: &str,
            reader: &Prompter<Term>,
            start: usize,
            _end: usize,
        ) -> Option<Vec<Completion>> {
            let line = reader.buffer();
    
            let completions: Option<Vec<Completion>>;
            if for_dots(line) {
                let cpl = Arc::new(dots::DotsCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_ssh(line) {
                let cpl = Arc::new(ssh::SshCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_make(line) {
                let cpl = Arc::new(make::MakeCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_bin(line) {
                let cpl = Arc::new(path::BinCompleter {
                    sh: self.sh.clone(),
                });
                completions = cpl.complete(word, reader, start, _end);
            } else if for_env(line) {
                let cpl = Arc::new(env::EnvCompleter {
                    sh: self.sh.clone(),
                });
                completions = cpl.complete(word, reader, start, _end);
            } else if for_cd(line) {
                // `for_cd` should be put a bottom position, so that
                // `cd $SOME_ENV_<TAB>` works as expected.
                let cpl = Arc::new(path::CdCompleter);
                // completions for `cd` should not fail back to path-completion
                return cpl.complete(word, reader, start, _end);
            } else {
                completions = None;
            }
    
            if let Some(x) = completions {
                if !x.is_empty() {
                    return Some(x);
                }
            }
    
            // empty completions should fail back to path-completion,
            // so that `$ make generate /path/to/fi<Tab>` still works.
            let cpl = Arc::new(path::PathCompleter);
            cpl.complete(word, reader, start, _end)
        }
    
        fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
            escaped_word_start(&line[..end])
        }
    }
    
    pub fn escaped_word_start(line: &str) -> usize {
        let mut start_position: usize = 0;
        let mut found_bs = false;
        let mut found_space = false;
        let mut with_quote = false;
        let mut ch_quote = '\0';
        let mut extra_bytes = 0;
        for (i, c) in line.chars().enumerate() {
            if found_space {
                found_space = false;
                start_position = i + extra_bytes;
            }
    
            if c == '\\' {
                found_bs = true;
                continue;
            }
            if c == ' ' && !found_bs && !with_quote {
                found_space = true;
                continue;
            }
    
            if !with_quote && !found_bs && (c == '"' || c == '\'') {
                with_quote = true;
                ch_quote = c;
            } else if with_quote && !found_bs && ch_quote == c {
                with_quote = false;
            }
    
            let bytes_c = c.len_utf8();
            if bytes_c > 1 {
                extra_bytes += bytes_c - 1;
            }
            found_bs = false;
        }
        if found_space {
            start_position = line.len();
        }
        start_position
    }
}

pub mod core
{
    use ::
    {
        *,
    };
    /*
    use std::env;
    use std::ffi::{CStr, CString};
    use std::fs::File;
    use std::io::{Read, Write};
    use std::os::unix::io::FromRawFd;
    use std::os::fd::RawFd;
    use std::process;
    
    use nix::unistd::{execve, ForkResult};
    use libs::pipes::pipe;
    
    use crate::builtins;
    use crate::calculator;
    use crate::jobc;
    use crate::libs;
    use crate::parsers;
    use crate::scripting;
    use crate::shell::{self, Shell};
    use crate::tools;
    use crate::types::{CommandLine, CommandOptions, CommandResult};
    */
    fn try_run_builtin_in_subprocess(
        sh: &mut Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        capture: bool,
    ) -> Option<i32> {
        if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture) {
            return Some(cr.status);
        }
        None
    }
    
    fn try_run_builtin(
        sh: &mut Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        capture: bool,
    ) -> Option<CommandResult> {
        // for builtin, only capture its outputs when it locates at the end
        let capture = capture && idx_cmd + 1 == cl.commands.len();
    
        if idx_cmd >= cl.commands.len() {
            println_stderr!("unexpected error in try_run_builtin");
            return None;
        }
    
        let cmd = &cl.commands[idx_cmd];
        let tokens = cmd.tokens.clone();
        let cname = tokens[0].1.clone();
        if cname == "alias" {
            let cr = builtins::alias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "bg" {
            let cr = builtins::bg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cd" {
            let cr = builtins::cd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cinfo" {
            let cr = builtins::cinfo::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exec" {
            let cr = builtins::exec::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exit" {
            let cr = builtins::exit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "export" {
            let cr = builtins::export::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "fg" {
            let cr = builtins::fg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "history" {
            let cr = builtins::history::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "jobs" {
            let cr = builtins::jobs::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "minfd" {
            let cr = builtins::minfd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "read" {
            let cr = builtins::read::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "set" {
            let cr = builtins::set::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "source" {
            let cr = builtins::source::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "ulimit" {
            let cr = builtins::ulimit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unalias" {
            let cr = builtins::unalias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unset" {
            let cr = builtins::unset::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unpath" {
            let cr = builtins::unpath::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "vox" {
            let cr = builtins::vox::run(sh, cl, cmd, capture);
            return Some(cr);
        }
        None
    }
    
    /// Run a pipeline (e.g. `echo hi | wc -l`)
    /// returns: (is-terminal-given, command-result)
    pub fn run_pipeline(
        sh: &mut shell::Shell,
        cl: &CommandLine,
        tty: bool,
        capture: bool,
        log_cmd: bool,
    ) -> (bool, CommandResult) {
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
    }
    
    /// Run a single command.
    /// e.g. the `sort -k2` part of `ps ax | sort -k2 | head`
    #[allow(clippy::needless_range_loop)]
    #[allow(clippy::too_many_arguments)]
    fn run_single_program(
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
    ) -> i32 {
        let capture = options.capture_output;
        if cl.is_single_and_builtin() {
            if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture) {
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
    
        if cmd.has_here_string() {
            match pipe() {
                Ok(fds) => fds_stdin = Some(fds),
                Err(e) => {
                    println_stderr!("cicada: pipeline4: {}", e);
                    return 1;
                }
            }
        }
    
        match libs::fork::fork() {
            Ok(ForkResult::Child) => {
                unsafe {
                    // child processes need to handle ctrl-Z
                    libc::signal(libc::SIGTSTP, libc::SIG_DFL);
                    libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                }
    
                // close pipes unrelated to current child (left side)
                if idx_cmd > 0 {
                    for i in 0..idx_cmd - 1 {
                        let fds = pipes[i];
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                }
                // close pipes unrelated to current child (right side)
                for i in idx_cmd + 1..pipes_count {
                    let fds = pipes[i];
                    libs::close(fds.0);
                    libs::close(fds.1);
                }
                // close pipe fds for capturing stdout/stderr
                // (they're only used in last child)
                if idx_cmd < pipes_count {
                    if let Some(fds) = fds_capture_stdout {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                    if let Some(fds) = fds_capture_stderr {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                }
    
                if idx_cmd == 0 {
                    unsafe {
                        let pid = libc::getpid();
                        libc::setpgid(0, pid);
                    }
                } else {
                    unsafe {
                        libc::setpgid(0, *pgid);
                    }
                }
    
                // (in child) replace stdin/stdout with read/write ends of pipe
                if idx_cmd > 0 {
                    let fds_prev = pipes[idx_cmd - 1];
                    libs::dup2(fds_prev.0, 0);
                    libs::close(fds_prev.0);
                    libs::close(fds_prev.1);
                }
                if idx_cmd < pipes_count {
                    let fds = pipes[idx_cmd];
                    libs::dup2(fds.1, 1);
                    libs::close(fds.1);
                    libs::close(fds.0);
                }
    
                if cmd.has_redirect_from() {
                    if let Some(redirect_from) = &cmd.redirect_from {
                        let fd = tools::get_fd_from_file(&redirect_from.clone().1);
                        if fd == -1 {
                            process::exit(1);
                        }
    
                        libs::dup2(fd, 0);
                        libs::close(fd);
                    }
                }
    
                if cmd.has_here_string() {
                    if let Some(fds) = fds_stdin {
                        libs::close(fds.1);
                        libs::dup2(fds.0, 0);
                        libs::close(fds.0);
                    }
                }
    
                let mut stdout_redirected = false;
                let mut stderr_redirected = false;
                for item in &cmd.redirects_to {
                    let from_ = &item.0;
                    let op_ = &item.1;
                    let to_ = &item.2;
                    if to_ == "&1" && from_ == "2" {
                        if idx_cmd < pipes_count {
                            libs::dup2(1, 2);
                        } else if !options.capture_output {
                            let fd = libs::dup(1);
                            if fd == -1 {
                                println_stderr!("cicada: dup error");
                                process::exit(1);
                            }
                            libs::dup2(fd, 2);
                        } else {
                            // note: capture output with redirections does not
                            // make much sense
                        }
                    } else if to_ == "&2" && from_ == "1" {
                        if idx_cmd < pipes_count || !options.capture_output {
                            let fd = libs::dup(2);
                            if fd == -1 {
                                println_stderr!("cicada: dup error");
                                process::exit(1);
                            }
                            libs::dup2(fd, 1);
                        } else {
                            // note: capture output with redirections does not
                            // make much sense
                        }
                    } else {
                        let append = op_ == ">>";
                        match tools::create_raw_fd_from_file(to_, append) {
                            Ok(fd) => {
                                if fd == -1 {
                                    println_stderr!("cicada: fork: fd error");
                                    process::exit(1);
                                }
    
                                if from_ == "1" {
                                    libs::dup2(fd, 1);
                                    stdout_redirected = true;
                                } else {
                                    libs::dup2(fd, 2);
                                    stderr_redirected = true;
                                }
                            }
                            Err(e) => {
                                println_stderr!("cicada: fork: {}", e);
                                process::exit(1);
                            }
                        }
                    }
                }
    
                // capture output of last process if needed.
                if idx_cmd == pipes_count && options.capture_output {
                    if !stdout_redirected {
                        if let Some(fds) = fds_capture_stdout {
                            libs::close(fds.0);
                            libs::dup2(fds.1, 1);
                            libs::close(fds.1);
                        }
                    }
                    if !stderr_redirected {
                        if let Some(fds) = fds_capture_stderr {
                            libs::close(fds.0);
                            libs::dup2(fds.1, 2);
                            libs::close(fds.1);
                        }
                    }
                }
    
                if cmd.is_builtin() {
                    if let Some(status) = try_run_builtin_in_subprocess(sh, cl, idx_cmd, capture) {
                        process::exit(status);
                    }
                }
    
                // our strings do not have '\x00' bytes in them,
                // we can use CString::new().expect() safely.
                let mut c_envs: Vec<_> = env::vars()
                    .map(|(k, v)| {
                        CString::new(format!("{}={}", k, v).as_str()).expect("CString error")
                    })
                    .collect();
                for (key, value) in cl.envs.iter() {
                    c_envs.push(
                        CString::new(format!("{}={}", key, value).as_str()).expect("CString error"),
                    );
                }
    
                let program = &cmd.tokens[0].1;
                let path = if program.contains('/') {
                    program.clone()
                } else {
                    libs::path::find_file_in_path(program, true)
                };
                if path.is_empty() {
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
                match execve(&c_program, &c_args, &c_envs) {
                    Ok(_) => {}
                    Err(e) => match e {
                        nix::Error::ENOEXEC => {
                            println_stderr!("cicada: {}: exec format error (ENOEXEC)", program);
                        }
                        nix::Error::ENOENT => {
                            println_stderr!("cicada: {}: file does not exist", program);
                        }
                        nix::Error::EACCES => {
                            println_stderr!("cicada: {}: Permission denied", program);
                        }
                        _ => {
                            println_stderr!("cicada: {}: {:?}", program, e);
                        }
                    },
                }
    
                process::exit(1);
            }
            Ok(ForkResult::Parent { child, .. }) => {
                let pid: i32 = child.into();
                if idx_cmd == 0 {
                    *pgid = pid;
                    unsafe {
                        // we need to wait pgid of child set to itself,
                        // before give terminal to it (for macos).
                        // 1. this loop causes `bash`, `htop` etc to go `T` status
                        //    immediate after start on linux (ubuntu).
                        // 2. but on mac, we need this loop, otherwise commands
                        //    like `vim` will go to `T` status after start.
                        if cfg!(target_os = "macos") {
                            loop {
                                let _pgid = libc::getpgid(pid);
                                if _pgid == pid {
                                    break;
                                }
                            }
                        }
    
                        if sh.has_terminal
                            && options.isatty
                            && !cl.background
                        {
                            *term_given = shell::give_terminal_to(pid);
                        }
                    }
                }
    
                if options.isatty && !options.capture_output {
                    let _cmd = parsers::parser_line::tokens_to_line(&cmd.tokens);
                    sh.insert_job(*pgid, pid, &_cmd, "Running", cl.background);
                }
    
                if let Some(redirect_from) = &cmd.redirect_from {
                    if redirect_from.0 == "<<<" {
                        if let Some(fds) = fds_stdin {
                            unsafe {
                                libs::close(fds.0);
    
                                let mut f = File::from_raw_fd(fds.1);
                                match f.write_all(redirect_from.1.clone().as_bytes()) {
                                    Ok(_) => {}
                                    Err(e) => println_stderr!("cicada: write_all: {}", e),
                                }
                                match f.write_all(b"\n") {
                                    Ok(_) => {}
                                    Err(e) => println_stderr!("cicada: write_all: {}", e),
                                }
                            }
                        }
                    }
                }
    
                // (in parent) close unused pipe ends
                if idx_cmd < pipes_count {
                    let fds = pipes[idx_cmd];
                    libs::close(fds.1);
                }
                if idx_cmd > 0 {
                    // close pipe end only after dupped in the child
                    let fds = pipes[idx_cmd - 1];
                    libs::close(fds.0);
                }
    
                if idx_cmd == pipes_count && options.capture_output {
                    let mut s_out = String::new();
                    let mut s_err = String::new();
    
                    unsafe {
                        if let Some(fds) = fds_capture_stdout {
                            libs::close(fds.1);
    
                            let mut f = File::from_raw_fd(fds.0);
                            match f.read_to_string(&mut s_out) {
                                Ok(_) => {}
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
                            }
                        }
                        if let Some(fds) = fds_capture_stderr {
                            libs::close(fds.1);
                            let mut f_err = File::from_raw_fd(fds.0);
                            match f_err.read_to_string(&mut s_err) {
                                Ok(_) => {}
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
                            }
                        }
                    }
    
                    *cmd_result = CommandResult {
                        gid: *pgid,
                        status: 0,
                        stdout: s_out.clone(),
                        stderr: s_err.clone(),
                    };
                }
    
                pid
            }
    
            Err(_) => {
                println_stderr!("Fork failed");
                *cmd_result = CommandResult::error();
                0
            }
        }
    }
    
    fn try_run_func(
        sh: &mut Shell,
        cl: &CommandLine,
        capture: bool,
        log_cmd: bool,
    ) -> Option<CommandResult> {
        if cl.is_empty() {
            return None;
        }
    
        let command = &cl.commands[0];
        if let Some(func_body) = sh.get_func(&command.tokens[0].1) {
            let mut args = vec!["cicada".to_string()];
            for token in &command.tokens {
                args.push(token.1.to_string());
            }
            if log_cmd {
                log!("run func: {:?}", &args);
            }
            let cr_list = scripting::run_lines(sh, &func_body, &args, capture);
            let mut stdout = String::new();
            let mut stderr = String::new();
            for cr in cr_list {
                stdout.push_str(cr.stdout.trim());
                stdout.push(' ');
                stderr.push_str(cr.stderr.trim());
                stderr.push(' ');
            }
            let mut cr = CommandResult::new();
            cr.stdout = stdout;
            cr.stderr = stderr;
            return Some(cr);
        }
        None
    }
    
    fn try_run_calculator(line: &str, capture: bool) -> Option<CommandResult> {
        if tools::is_arithmetic(line) {
            match run_calculator(line) {
                Ok(result) => {
                    let mut cr = CommandResult::new();
                    if capture {
                        cr.stdout = result.clone();
                    } else {
                        println!("{}", result);
                    }
                    return Some(cr);
                }
                Err(e) => {
                    let mut cr = CommandResult::from_status(0, 1);
                    if capture {
                        cr.stderr = e.to_string();
                    } else {
                        println_stderr!("cicada: calculator: {}", e);
                    }
                    return Some(cr);
                }
            }
        }
        None
    }
    
    pub fn run_calculator(line: &str) -> Result<String, &str> {
        let parse_result = calculator::calculate(line);
        match parse_result {
            Ok(mut calc) => {
                let expr = calc.next().unwrap().into_inner();
    
                if line.contains('.') {
                    Ok(format!("{}", calculator::eval_float(expr)))
                } else {
                    Ok(format!("{}", calculator::eval_int(expr)))
                }
            }
            Err(_) => {
                Err("syntax error")
            }
        }
    }
}

pub mod execute
{
    use ::
    {
        *,
    };
    /*
    use std::collections::HashMap;
    use std::io::{self, Read, Write};
    
    use regex::Regex;
    
    use crate::core;
    use crate::libs;
    use crate::parsers;
    use crate::shell::{self, Shell};
    use crate::types::{CommandLine, CommandResult, Tokens};
    */
    /// Entry point for non-ttys (e.g. Cmd-N on MacVim)
    pub fn run_procs_for_non_tty(sh: &mut Shell) {
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
    
    pub fn run_command_line(sh: &mut Shell, line: &str, tty: bool,
                            capture: bool) -> Vec<CommandResult> {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();
        for token in parsers::parser_line::line_to_cmds(line) {
            if token == ";" || token == "&&" || token == "||" {
                sep = token.clone();
                continue;
            }
            if sep == "&&" && status != 0 {
                break;
            }
            if sep == "||" && status == 0 {
                break;
            }
            let cmd = token.clone();
            let cr = run_proc(sh, &cmd, tty, capture);
            status = cr.status;
            sh.previous_status = status;
            cr_list.push(cr);
        }
        cr_list
    }
    
    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String> {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let ptn_env_exp = r"^([a-zA-Z_][a-zA-Z0-9_]*)=(.*)$";
        let re = Regex::new(ptn_env_exp).unwrap();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !libs::re::re_contains(text, ptn_env_exp) {
                break;
            }
    
            for cap in re.captures_iter(text) {
                let name = cap[1].to_string();
                let value = parsers::parser_line::unquote(&cap[2]);
                envs.insert(name, value);
            }
    
            n += 1;
        }
        if n > 0 {
            tokens.drain(0..n);
        }
        envs
    }
    
    fn line_to_tokens(sh: &mut Shell, line: &str) -> (Tokens, HashMap<String, String>) {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        shell::do_expansion(sh, &mut tokens);
        let envs = drain_env_tokens(&mut tokens);
        (tokens, envs)
    }
    
    fn set_shell_vars(sh: &mut Shell, envs: &HashMap<String, String>) {
        for (name, value) in envs.iter() {
            sh.set_env(name, value);
        }
    }
    
    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    /// example 1: `ls`
    /// example 2: `ls | wc`
    fn run_proc(sh: &mut Shell, line: &str, tty: bool,
                capture: bool) -> CommandResult {
        let log_cmd = !sh.cmd.starts_with(' ');
        match CommandLine::from_line(line, sh) {
            Ok(cl) => {
                if cl.is_empty() {
                    // for commands with only envs, e.g.
                    // $ FOO=1 BAR=2
                    // we need to define these **Shell Variables**.
                    if !cl.envs.is_empty() {
                        set_shell_vars(sh, &cl.envs);
                    }
                    return CommandResult::new();
                }
    
                let (term_given, cr) = core::run_pipeline(sh, &cl, tty, capture, log_cmd);
                if term_given {
                    unsafe {
                        let gid = libc::getpgid(0);
                        shell::give_terminal_to(gid);
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
    
    fn run_with_shell(sh: &mut Shell, line: &str) -> CommandResult {
        let (tokens, envs) = line_to_tokens(sh, line);
        if tokens.is_empty() {
            set_shell_vars(sh, &envs);
            return CommandResult::new();
        }
    
        match CommandLine::from_line(line, sh) {
            Ok(c) => {
                let (term_given, cr) = core::run_pipeline(sh, &c, false, true, false);
                if term_given {
                    unsafe {
                        let gid = libc::getpgid(0);
                        shell::give_terminal_to(gid);
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
    
    pub fn run(line: &str) -> CommandResult {
        let mut sh = Shell::new();
        run_with_shell(&mut sh, line)
    }
}

pub mod history
{
    use ::
    {
        *,
    };
    /*
    use std::collections::HashMap;
    use std::env;
    use std::fs;
    use std::io::Write;
    use std::path::Path;
    
    use lineread::terminal::DefaultTerminal;
    use lineread::Interface;
    use rusqlite::Connection as Conn;
    use rusqlite::Error::SqliteFailure;
    
    use crate::shell;
    use crate::tools;
    */
    fn init_db(hfile: &str, htable: &str) {
        let path = Path::new(hfile);
        if !path.exists() {
            let _parent = match path.parent() {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: history init - no parent found");
                    return;
                }
            };
            let parent = match _parent.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: parent to_str is None");
                    return;
                }
            };
            match fs::create_dir_all(parent) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!("cicada: histdir create error: {}", e);
                    return;
                }
            }
            match fs::File::create(hfile) {
                Ok(_) => {
                    println!("cicada: created history file: {}", hfile);
                }
                Err(e) => {
                    println_stderr!("cicada: history: file create failed: {}", e);
                }
            }
        }
    
        let conn = match Conn::open(hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: history: open db error: {}", e);
                return;
            }
        };
        let sql = format!(
            "
            CREATE TABLE IF NOT EXISTS {}
                (inp TEXT,
                 rtn INTEGER,
                 tsb REAL,
                 tse REAL,
                 sessionid TEXT,
                 out TEXT,
                 info TEXT
                );
        ",
            htable
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => println_stderr!("cicada: history: query error: {}", e),
        }
    }
    
    pub fn init(rl: &mut Interface<DefaultTerminal>) {
        let mut hist_size: usize = 99999;
        if let Ok(x) = env::var("HISTORY_SIZE") {
            if let Ok(y) = x.parse::<usize>() {
                hist_size = y;
            }
        }
        rl.set_history_size(hist_size);
    
        let history_table = get_history_table();
        let hfile = get_history_file();
    
        if !Path::new(&hfile).exists() {
            init_db(&hfile, &history_table);
        }
    
        let mut delete_dups = true;
        if let Ok(x) = env::var("HISTORY_DELETE_DUPS") {
            if x == "0" {
                delete_dups = false;
            }
        }
        if delete_dups {
            delete_duplicated_histories();
        }
    
        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!("SELECT inp FROM {} ORDER BY tsb;", history_table);
        let mut stmt = match conn.prepare(&sql) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: prepare select error: {}", e);
                return;
            }
        };
    
        let rows = match stmt.query_map([], |row| row.get(0)) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: query select error: {}", e);
                return;
            }
        };
    
        let mut dict_helper: HashMap<String, bool> = HashMap::new();
        for x in rows.flatten() {
            let inp: String = x;
            if dict_helper.contains_key(&inp) {
                continue;
            }
            dict_helper.insert(inp.clone(), true);
            rl.add_history(inp.trim().to_string());
        }
    }
    
    pub fn get_history_file() -> String {
        if let Ok(hfile) = env::var("HISTORY_FILE") {
            hfile
        } else if let Ok(d) = env::var("XDG_DATA_HOME") {
            format!("{}/{}", d, "cicada/history.sqlite")
        } else {
            let home = tools::get_user_home();
            format!("{}/{}", home, ".local/share/cicada/history.sqlite")
        }
    }
    
    pub fn get_history_table() -> String {
        if let Ok(hfile) = env::var("HISTORY_TABLE") {
            hfile
        } else {
            String::from("cicada_history")
        }
    }
    
    fn delete_duplicated_histories() {
        let hfile = get_history_file();
        let history_table = get_history_table();
        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!(
            "DELETE FROM {} WHERE rowid NOT IN (
            SELECT MAX(rowid) FROM {} GROUP BY inp)",
            history_table, history_table
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => match e {
                SqliteFailure(ee, msg) => {
                    if ee.extended_code == 5 {
                        log!(
                            "failed to delete dup histories: {}",
                            msg.unwrap_or("db is locked?".to_owned()),
                        );
                        return;
                    }
                    println_stderr!(
                        "cicada: history: delete dups error: {}: {:?}",
                        &ee,
                        &msg
                    );
                }
                _ => {
                    println_stderr!("cicada: history: delete dup error: {}", e);
                }
            },
        }
    }
    
    pub fn add_raw(sh: &shell::Shell, line: &str, status: i32,
                   tsb: f64, tse: f64) {
        let hfile = get_history_file();
        let history_table = get_history_table();
        if !Path::new(&hfile).exists() {
            init_db(&hfile, &history_table);
        }
    
        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!(
            "INSERT INTO \
             {} (inp, rtn, tsb, tse, sessionid, info) \
             VALUES('{}', {}, {}, {}, '{}', 'dir:{}|');",
            history_table,
            str::replace(line.trim(), "'", "''"),
            status,
            tsb,
            tse,
            sh.session_id,
            sh.current_dir,
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => println_stderr!("cicada: history: save error: {}", e),
        }
    }
    
    pub fn add(sh: &shell::Shell, rl: &mut Interface<DefaultTerminal>, line: &str,
               status: i32, tsb: f64, tse: f64) {
        add_raw(sh, line, status, tsb, tse);
        rl.add_history(line.to_string());
    }
}

pub mod highlight
{
    use ::
    {
        *,
    };
    /*
    use std::ops::Range;
    use std::sync::Arc;
    use std::collections::HashSet;
    use std::path::Path;
    use std::env;
    use std::fs;
    use std::sync::Mutex;
    use std::os::unix::fs::PermissionsExt;
    
    use lineread::highlighting::{Highlighter, Style};
    
    use crate::tools;
    use crate::shell;
    use crate::parsers::parser_line;
    */
    pub struct CicadaHighlighter;
    
    // ANSI color codes wrapped with \x01 and \x02 for lineread
    const GREEN: &str = "\x01\x1b[0;32m\x02";
    
    lazy_static! {
        static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
    }
    
    /// Initialize the available commands cache by scanning PATH directories
    pub fn init_command_cache() {
        let commands = scan_available_commands();
        if let Ok(mut cache) = AVAILABLE_COMMANDS.lock() {
            *cache = commands;
        }
    }
    
    /// Update aliases in the highlighter's cache
    pub fn update_aliases(sh: &shell::Shell) {
        if let Ok(mut aliases) = ALIASES.lock() {
            aliases.clear();
            for alias_name in sh.aliases.keys() {
                aliases.insert(alias_name.clone());
            }
        }
    }
    
    fn scan_available_commands() -> HashSet<String> {
        let mut commands = HashSet::new();
    
        if let Ok(path_var) = env::var("PATH") {
            for path in path_var.split(':') {
                if path.is_empty() {
                    continue;
                }
    
                let dir_path = Path::new(path);
                if !dir_path.is_dir() {
                    continue;
                }
    
                if let Ok(entries) = fs::read_dir(dir_path) {
                    for entry in entries.filter_map(Result::ok) {
                        if let Ok(file_type) = entry.file_type() {
                            if file_type.is_file() || file_type.is_symlink() {
                                if let Ok(metadata) = entry.metadata() {
                                    // Check if file is executable
                                    if metadata.permissions().mode() & 0o111 != 0 {
                                        if let Some(name) = entry.file_name().to_str() {
                                            commands.insert(name.to_string());
                                        }
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
    
    fn is_command(word: &str) -> bool {
        if tools::is_builtin(word) {
            return true;
        }
        if let Ok(aliases) = ALIASES.lock() {
            if aliases.contains(word) {
                return true;
            }
        }
        if let Ok(commands) = AVAILABLE_COMMANDS.lock() {
            if commands.contains(word) {
                return true;
            }
        }
        false
    }
    
    fn find_token_range_heuristic(line: &str, start_byte: usize, token: &(String, String)) -> Option<Range<usize>> {
        let (sep, word) = token;
    
        // Find the start of the token, skipping leading whitespace from the search start position
        let mut search_area = &line[start_byte..];
        let token_start_byte = if let Some(non_ws_offset) = search_area.find(|c: char| !c.is_whitespace()) {
            // Calculate the actual byte index of the first non-whitespace character
            start_byte + search_area.char_indices().nth(non_ws_offset).map_or(0, |(idx, _)| idx)
        } else {
            return None; // Only whitespace left
        };
    
        search_area = &line[token_start_byte..];
    
        // Estimate the end byte based on the token structure
        let mut estimated_len = 0;
        let mut current_search_offset = 0;
    
        // Match separator prefix if needed (e.g., `"` or `'`)
        if !sep.is_empty() && search_area.starts_with(sep) {
            estimated_len += sep.len();
            current_search_offset += sep.len();
        }
    
        // Match the word content
        // Use starts_with for a basic check, assuming the word appears next
        if search_area[current_search_offset..].starts_with(word) {
             estimated_len += word.len();
             current_search_offset += word.len();
    
             // Match separator suffix if needed
            if !sep.is_empty() && search_area[current_search_offset..].starts_with(sep) {
                estimated_len += sep.len();
            }
    
            Some(token_start_byte..(token_start_byte + estimated_len))
    
        } else if word.is_empty() && !sep.is_empty() && search_area.starts_with(sep) && search_area[sep.len()..].starts_with(sep) {
             // Handle empty quoted string like "" or ''
             estimated_len += sep.len() * 2;
             Some(token_start_byte..(token_start_byte + estimated_len))
        }
        else {
            // Fallback: Maybe it's just the word without quotes, or a separator like `|`
            if search_area.starts_with(word) {
                 Some(token_start_byte..(token_start_byte + word.len()))
            } else {
                 // Could not reliably map the token back to the original string segment
                 // This might happen with complex escapes or parser ambiguities
                 // As a basic fallback, consume up to the next space or end of line? Unsafe.
                 // Return None to signal failure for this token.
                 None
            }
        }
    }
    
    impl Highlighter for CicadaHighlighter {
        fn highlight(&self, line: &str) -> Vec<(Range<usize>, Style)> {
            let mut styles = Vec::new();
            if line.is_empty() {
                return styles;
            }
    
            let line_info = parser_line::parse_line(line);
            if line_info.tokens.is_empty() {
                // If parser returns no tokens, style whole line as default
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
    
    pub fn create_highlighter() -> Arc<CicadaHighlighter> {
        Arc::new(CicadaHighlighter)
    }
}

pub mod libs
{
    use ::
    {
        *,
    };

    pub mod colored
    {
        use ::
        {
            *,
        };
        // cicada special
        pub const SEQ: &str = "\x01";
        pub const END_SEQ: &str = "\x02";
        pub const ESC: &str = "\x1B";
        
        // Set
        pub const BOLD: &str = "\x01\x1B[1m\x02";
        pub const DIM: &str = "\x01\x1B[2m\x02";
        pub const UNDERLINED: &str = "\x01\x1B[4m\x02";
        pub const BLINK: &str = "\x01\x1B[5m\x02";
        pub const REVERSE: &str = "\x01\x1B[7m\x02";
        pub const HIDDEN: &str = "\x01\x1B[8m\x02";
        
        // Reset
        pub const RESET: &str = "\x01\x1B[0m\x02";
        pub const RESET_BOLD: &str = "\x01\x1B[21m\x02";
        pub const RESET_DIM: &str = "\x01\x1B[22m\x02";
        pub const RESET_UNDERLINED: &str = "\x01\x1B[24m\x02";
        pub const RESET_BLINK: &str = "\x01\x1B[25m\x02";
        pub const RESET_REVERSE: &str = "\x01\x1B[27m\x02";
        pub const RESET_HIDDEN: &str = "\x01\x1B[28m\x02";
        
        // Foreground (text)
        pub const DEFAULT: &str = "\x01\x1B[39m\x02";
        pub const BLACK: &str = "\x01\x1B[30m\x02";
        pub const RED: &str = "\x01\x1B[31m\x02";
        pub const GREEN: &str = "\x01\x1B[32m\x02";
        pub const YELLOW: &str = "\x01\x1B[33m\x02";
        pub const BLUE: &str = "\x01\x1B[34m\x02";
        pub const MAGENTA: &str = "\x01\x1B[35m\x02";
        pub const CYAN: &str = "\x01\x1B[36m\x02";
        pub const GRAY_L: &str = "\x01\x1B[37m\x02";
        
        pub const GRAY_D: &str = "\x01\x1B[90m\x02";
        pub const RED_L: &str = "\x01\x1B[91m\x02";
        pub const GREEN_L: &str = "\x01\x1B[92m\x02";
        pub const YELLOW_L: &str = "\x01\x1B[93m\x02";
        pub const BLUE_L: &str = "\x01\x1B[94m\x02";
        pub const MAGENTA_L: &str = "\x01\x1B[95m\x02";
        pub const CYAN_L: &str = "\x01\x1B[96m\x02";
        pub const WHITE: &str = "\x01\x1B[97m\x02";
        
        pub const BLUE_B: &str = "\x01\x1B[34m\x1B[1m\x02";
        pub const BLACK_B: &str = "\x01\x1B[30m\x1B[1m\x02";
        pub const WHITE_B: &str = "\x01\x1B[97m\x1B[1m\x02";
        pub const RED_B: &str = "\x01\x1B[31m\x1B[1m\x02";
        pub const GREEN_B: &str = "\x01\x1B[32m\x1B[1m\x02";
        
        // Background
        pub const DEFAULT_BG: &str = "\x01\x1B[49m\x02";
        pub const BLACK_BG: &str   = "\x01\x1B[40m\x02";
        pub const RED_BG: &str     = "\x01\x1B[41m\x02";
        pub const GREEN_BG: &str   = "\x01\x1B[42m\x02";
        pub const YELLOW_BG: &str   = "\x01\x1B[43m\x02";
        pub const BLUE_BG: &str    = "\x01\x1B[44m\x02";
        pub const MAGENTA_BG: &str    = "\x01\x1B[45m\x02";
        pub const CYAN_BG: &str    = "\x01\x1B[46m\x02";
        pub const GRAY_L_BG: &str    = "\x01\x1B[47m\x02";
        
        pub const GRAY_D_BG: &str   = "\x01\x1B[100m\x02";
        pub const RED_L_BG: &str   = "\x01\x1B[101m\x02";
        pub const GREEN_L_BG: &str   = "\x01\x1B[102m\x02";
        pub const YELLOW_L_BG: &str   = "\x01\x1B[103m\x02";
        pub const BLUE_L_BG: &str   = "\x01\x1B[104m\x02";
        pub const MAGENTA_L_BG: &str   = "\x01\x1B[105m\x02";
        pub const CYAN_L_BG: &str   = "\x01\x1B[106m\x02";
        pub const WHITE_BG: &str   = "\x01\x1B[107m\x02";
    }
    
    pub mod fork
    {
        use ::
        {
            *,
        };
        /*
        use nix::Result;
        use nix::unistd::{fork as nix_fork, ForkResult};
        */
        // make fork "safe again", in order not to touch the code in core.rs,
        // see https://github.com/nix-rust/nix/issues/586
        // we can have refactorings any time needed.
        pub fn fork() -> Result<ForkResult> {
            unsafe{ nix_fork() }
        }
    }
    
    pub mod os_type
    {
        use ::
        {
            *,
        };
        /*
        use crate::execute;
        */
        pub fn get_os_name() -> String {
            let uname = get_uname();
            if uname.to_lowercase() == "darwin" {
                get_macos_name()
            } else {
                get_other_os_name()
            }
        }
        
        fn get_other_os_name() -> String {
            let mut name = get_release_value("PRETTY_NAME");
            if !name.is_empty() {
                return name;
            }
            name = get_release_value("DISTRIB_DESCRIPTION");
            if !name.is_empty() {
                return name;
            }
            name = get_release_value("IMAGE_DESCRIPTION");
            if !name.is_empty() {
                return name;
            }
            get_uname_mo()
        }
        
        fn get_release_value(ptn: &str) -> String {
            let line = format!(
                "grep -i '{}' /etc/*release* 2>&1 | grep -o '=.*' | tr '\"=' ' '",
                ptn
            );
            let cr = execute::run(&line);
            return cr.stdout.trim().to_string();
        }
        
        fn get_uname() -> String {
            let cr = execute::run("uname");
            return cr.stdout.trim().to_string();
        }
        
        fn get_uname_mo() -> String {
            let cr = execute::run("uname -m -o");
            return cr.stdout.trim().to_string();
        }
        
        fn get_macos_name() -> String {
            let mut os_name = get_osx_codename();
            let ver = get_osx_version();
            if !ver.is_empty() {
                os_name.push(' ');
                os_name.push_str(&ver);
            }
            os_name
        }
        
        fn get_osx_codename() -> String {
            let cr = execute::run("grep -o 'SOFTWARE LICENSE AGREEMENT FOR .*[a-zA-Z]' '/System/Library/CoreServices/Setup Assistant.app/Contents/Resources/en.lproj/OSXSoftwareLicense.rtf' | sed 's/SOFTWARE LICENSE AGREEMENT FOR *//'");
            return cr.stdout.trim().to_string();
        }
        
        fn get_osx_version() -> String {
            let cr = execute::run("sw_vers -productVersion");
            return cr.stdout.trim().to_string();
        }
    }
    
    pub mod path
    {
        use ::
        {
            *,
        };
        /*
        use std::borrow::Cow;
        use std::env;
        use std::fs::read_dir;
        use std::io::{ErrorKind, Write};
        use std::os::unix::fs::PermissionsExt;
        
        use regex::Regex;
        
        use crate::tools;
        */
        pub fn basename(path: &str) -> Cow<'_, str> {
            let mut pieces = path.rsplit('/');
            match pieces.next() {
                Some(p) => p.into(),
                None => path.into(),
            }
        }
        
        pub fn expand_home(text: &str) -> String {
            let mut s: String = text.to_string();
            let v = vec![
                r"(?P<head> +)~(?P<tail> +)",
                r"(?P<head> +)~(?P<tail>/)",
                r"^(?P<head> *)~(?P<tail>/)",
                r"(?P<head> +)~(?P<tail> *$)",
            ];
            for item in &v {
                let re;
                if let Ok(x) = Regex::new(item) {
                    re = x;
                } else {
                    return String::new();
                }
                let home = tools::get_user_home();
                let ss = s.clone();
                let to = format!("$head{}$tail", home);
                let result = re.replace_all(ss.as_str(), to.as_str());
                s = result.to_string();
            }
            s
        }
        
        pub fn find_file_in_path(filename: &str, exec: bool) -> String {
            let env_path = match env::var("PATH") {
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
        
                                if exec {
                                    let _mode = match entry.metadata() {
                                        Ok(x) => x,
                                        Err(e) => {
                                            println_stderr!("cicada: metadata error: {:?}", e);
                                            continue;
                                        }
                                    };
                                    let mode = _mode.permissions().mode();
                                    if mode & 0o111 == 0 {
                                        // not binary
                                        continue;
                                    }
                                }
        
                                return entry.path().to_string_lossy().to_string();
                            }
                        }
                    }
                    Err(e) => {
                        if e.kind() == ErrorKind::NotFound {
                            continue;
                        }
                        log!("cicada: fs read_dir error: {}: {}", p, e);
                    }
                }
            }
            String::new()
        }
        
        pub fn current_dir() -> String {
            let _current_dir = match env::current_dir() {
                Ok(x) => x,
                Err(e) => {
                    log!("cicada: PROMPT: env current_dir error: {}", e);
                    return String::new();
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    log!("cicada: PROMPT: to_str error");
                    return String::new();
                }
            };
        
            current_dir.to_string()
        }
    }
    
    pub mod re
    {
        use ::
        {
            *,
        };
        pub fn find_first_group(ptn: &str, text: &str) -> Option<String> {
            let re = match regex::Regex::new(ptn) {
                Ok(x) => x,
                Err(_) => return None,
            };
            match re.captures(text) {
                Some(caps) => {
                    if let Some(x) = caps.get(1) {
                        return Some(x.as_str().to_owned());
                    }
                }
                None => {
                    return None;
                }
            }
            None
        }
        
        pub fn re_contains(text: &str, ptn: &str) -> bool {
            let re = match regex::Regex::new(ptn) {
                Ok(x) => x,
                Err(e) => {
                    println!("Regex new error: {:?}", e);
                    return false;
                }
            };
            re.is_match(text)
        }
        
        pub fn replace_all(text: &str, ptn: &str, ptn_to: &str) -> String {
            let re = regex::Regex::new(ptn).unwrap();
            let result = re.replace_all(text, ptn_to);
            result.to_string()
        }
    }
    
    pub mod term_size
    {
        use ::
        {
            *,
        };
        /*
        use libc::{c_int, c_ulong, winsize, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO};
        use std::mem::zeroed;
        */
        // Unfortunately the actual command is not standardised...
        #[cfg(any(target_os = "linux", target_os = "android"))]
        static TIOCGWINSZ: c_ulong = 0x5413;
        
        #[cfg(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "dragonfly",
            target_os = "freebsd",
            target_os = "netbsd",
            target_os = "openbsd"
        ))]
        static TIOCGWINSZ: c_ulong = 0x40087468;
        
        #[cfg(target_os = "solaris")]
        static TIOCGWINSZ: c_ulong = 0x5468;
        
        extern "C" {
            fn ioctl(fd: c_int, request: c_ulong, ...) -> c_int;
        }
        
        /// Runs the ioctl command. Returns (0, 0) if all of the streams are not to a terminal, or
        /// there is an error. (0, 0) is an invalid size to have anyway, which is why
        /// it can be used as a nil value.
        unsafe fn get_dimensions_any() -> winsize {
            let mut window: winsize = zeroed();
            let mut result = ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut window);
        
            if result == -1 {
                window = zeroed();
                result = ioctl(STDIN_FILENO, TIOCGWINSZ, &mut window);
                if result == -1 {
                    window = zeroed();
                    result = ioctl(STDERR_FILENO, TIOCGWINSZ, &mut window);
                    if result == -1 {
                        return zeroed();
                    }
                }
            }
            window
        }
        
        /// Query the current processes's output (`stdout`), input (`stdin`), and error (`stderr`) in
        /// that order, in the attempt to dtermine terminal width. If one of those streams is actually
        /// a tty, this function returns its width and height as a number of characters.
        ///
        /// # Errors
        ///
        /// If *all* of the streams are not ttys or return any errors this function will return `None`.
        ///
        /// # Example
        ///
        /// To get the dimensions of your terminal window, simply use the following:
        ///
        /// ```ignore
        /// if let Some((w, h)) = term_size::dimensions() {
        ///     println!("Width: {}\nHeight: {}", w, h);
        /// } else {
        ///     println!("Unable to get term size :(")
        /// }
        /// ```
        pub fn dimensions() -> Option<(usize, usize)> {
            let w = unsafe { get_dimensions_any() };
        
            if w.ws_col == 0 || w.ws_row == 0 {
                None
            } else {
                Some((w.ws_col as usize, w.ws_row as usize))
            }
        }
    }
    
    pub mod progopts
    {
        use ::
        {
            *,
        };
        
        pub fn is_login(args: &[String]) -> bool {
            if !args.is_empty() && args[0].starts_with("-") {
                return true;
            }
        
            if args.len() > 1 && (args[1] == "--login" || args[1] == "-l") {
                return true;
            }
        
            if let Ok(term_program) = std::env::var("TERM_PROGRAM") {
                if term_program == "vscode" {
                    return true;
                }
            }
        
            false
        }
        
        pub fn is_script(args: &[String]) -> bool {
            args.len() > 1 && !args[1].starts_with("-")
        }
        
        pub fn is_command_string(args: &[String]) -> bool {
            args.len() > 1 && args[1] == "-c"
        }
        
        pub fn is_non_tty() -> bool {
            unsafe { libc::isatty(0) == 0 }
        }
    }
    
    pub mod pipes
    {
        use ::
        {
            *,
        };
        /*
        use std::os::fd::RawFd;
        use nix::Error;
        use std::mem;
        use libc::c_int;
        */
        pub fn pipe() -> std::result::Result<(RawFd, RawFd), Error> {
            let mut fds = mem::MaybeUninit::<[c_int; 2]>::uninit();
            let res = unsafe { libc::pipe(fds.as_mut_ptr() as *mut c_int) };
            Error::result(res)?;
            unsafe { Ok((fds.assume_init()[0], fds.assume_init()[1])) }
        }
    }
        
    pub fn close(fd: i32) { unsafe { libc::close(fd); } }
    
    pub fn dup(fd: i32) -> i32 { unsafe { libc::dup(fd) } }
    
    pub fn dup2(src: i32, dst: i32) { unsafe { libc::dup2(src, dst); } }
}

pub mod parsers
{
    use ::
    {
        *,
    };
    
    pub mod parser_line
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
        pub fn line_to_plain_tokens(line: &str) -> Vec<String> {
            let mut result = Vec::new();
            let linfo = parse_line(line);
            for (_, r) in linfo.tokens {
                result.push(r.clone());
            }
            result
        }
        
        pub fn tokens_to_args(tokens: &Tokens) -> Vec<String> {
            let mut result = Vec::new();
            for s in tokens {
                result.push(s.1.clone());
            }
            result
        }
        
        pub fn tokens_to_line(tokens: &Tokens) -> String {
            let mut result = String::new();
            for t in tokens {
                if t.0.is_empty() {
                    result.push_str(&t.1);
                } else {
                    let s = tools::wrap_sep_string(&t.0, &t.1);
                    result.push_str(&s);
                }
                result.push(' ');
            }
            if result.ends_with(' ') {
                let len = result.len();
                result.truncate(len - 1);
            }
            result
        }
        
        /// Parse command line for multiple commands. Examples:
        /// >>> line_to_cmds("echo foo && echo bar; echo end");
        /// vec!["echo foo", "&&", "echo bar", ";", "echo end"]
        /// >>> line_to_cmds("man awk | grep version");
        /// vec!["man awk | grep version"]
        pub fn line_to_cmds(line: &str) -> Vec<String> {
            // Special characters: http://tldp.org/LDP/abs/html/special-chars.html
            let mut result = Vec::new();
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
            result
        }
        
        /// parse command line to tokens
        /// >>> parse_line("echo 'hi yoo' | grep \"hi\"");
        /// LineInfo {
        ///    tokens: vec![
        ///        ("", "echo"),
        ///        ("'", "hi yoo"),
        ///        ("", "|"),
        ///        ("", "grep"),
        ///        ("\"", "hi"),
        ///    ],
        ///    is_complete: true
        /// }
        // #[allow(clippy::cyclomatic_complexity)]
        pub fn parse_line(line: &str) -> LineInfo {
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
        
        pub fn tokens_to_redirections(tokens: &Tokens) -> Result<(Tokens, Vec<Redirection>), String> {
            let mut tokens_new = Vec::new();
            let mut redirects = Vec::new();
            let mut to_be_continued = false;
            let mut to_be_continued_s1 = String::new();
            let mut to_be_continued_s2 = String::new();
        
            for token in tokens {
                let sep = &token.0;
                if !sep.is_empty() && !to_be_continued {
                    tokens_new.push(token.clone());
                    continue;
                }
                let word = &token.1;
        
                if to_be_continued {
                    if sep.is_empty() && word.starts_with('&') {
                        return Err(String::from("bad redirection syntax near &"));
                    }
        
                    let s3 = word.to_string();
                    if libs::re::re_contains(&to_be_continued_s1, r"^\d+$") {
                        if to_be_continued_s1 != "1" && to_be_continued_s1 != "2" {
                            return Err(String::from("Bad file descriptor #3"));
                        }
                        let s1 = to_be_continued_s1.clone();
                        let s2 = to_be_continued_s2.clone();
                        redirects.push((s1, s2, s3));
                    } else {
                        if !to_be_continued_s1.is_empty() {
                            tokens_new.push((sep.clone(), to_be_continued_s1.to_string()));
                        }
                        redirects.push(("1".to_string(), to_be_continued_s2.clone(), s3));
                    }
        
                    to_be_continued = false;
                    continue;
                }
        
                let ptn1 = r"^([^>]*)(>>?)([^>]+)$";
                let ptn2 = r"^([^>]*)(>>?)$";
                if !libs::re::re_contains(word, r">") {
                    tokens_new.push(token.clone());
                } else if libs::re::re_contains(word, ptn1) {
                    let re;
                    if let Ok(x) = Regex::new(ptn1) {
                        re = x;
                    } else {
                        return Err(String::from("Failed to build Regex"));
                    }
        
                    if let Some(caps) = re.captures(word) {
                        let s1 = caps.get(1).unwrap().as_str();
                        let s2 = caps.get(2).unwrap().as_str();
                        let s3 = caps.get(3).unwrap().as_str();
                        if s3.starts_with('&') && s3 != "&1" && s3 != "&2" {
                            return Err(String::from("Bad file descriptor #1"));
                        }
        
                        if libs::re::re_contains(s1, r"^\d+$") {
                            if s1 != "1" && s1 != "2" {
                                return Err(String::from("Bad file descriptor #2"));
                            }
                            redirects.push((s1.to_string(), s2.to_string(), s3.to_string()));
                        } else {
                            if !s1.is_empty() {
                                tokens_new.push((sep.clone(), s1.to_string()));
                            }
                            redirects.push((String::from("1"), s2.to_string(), s3.to_string()));
                        }
                    }
                } else if libs::re::re_contains(word, ptn2) {
                    let re;
                    if let Ok(x) = Regex::new(ptn2) {
                        re = x;
                    } else {
                        return Err(String::from("Failed to build Regex"));
                    }
        
                    if let Some(caps) = re.captures(word) {
                        let s1 = caps.get(1).unwrap().as_str();
                        let s2 = caps.get(2).unwrap().as_str();
        
                        to_be_continued = true;
                        to_be_continued_s1 = s1.to_string();
                        to_be_continued_s2 = s2.to_string();
                    }
                }
            }
        
            if to_be_continued {
                return Err(String::from("redirection syntax error"));
            }
        
            Ok((tokens_new, redirects))
        }
        
        pub fn unquote(text: &str) -> String {
            let mut new_str = String::from(text);
            for &c in ['"', '\''].iter() {
                if text.starts_with(c) && text.ends_with(c) {
                    new_str.remove(0);
                    new_str.pop();
                    break;
                }
            }
            new_str
        }
    }
    
    pub mod locust
    {
        use ::
        {
            *,
        };
        /*
        use pest::Parser;
        use pest::iterators::Pairs;
        use pest::error::Error; */
        #[derive(Parser)]
        #[grammar = "parsers/grammar.pest"]
        struct Locust;
        
        pub fn parse_lines(lines: &str) -> Result<Pairs<crate::parsers::locust::Rule>, Error<crate::parsers::locust::Rule>> {
            Locust::parse(Rule::EXP, lines)
        }

    }
}

pub mod prompt
{
    use ::
    {
        *,
    };
    /*
    
    use crate::libs;
    use crate::shell;
    
    use self::main::get_prompt_string;
    use self::main::render_prompt;
    pub use self::multilines::EnterFunction;
    */
    mod main
    {
        use ::
        {
            *,
        };
        /*
        use std::env;
        
        use crate::execute;
        use crate::libs;
        use crate::shell;
        */
        const DEFAULT_PROMPT: &str = "${COLOR_STATUS}$USER${RESET}\
            @${COLOR_STATUS}$HOSTNAME${RESET}: \
            ${COLOR_STATUS}$CWD${RESET}$ ";
        use super::preset::apply_preset_item;
        use super::preset::apply_pyenv;
        
        fn is_prefix_char(c: char) -> bool {
            c == '[' || c == '{'
        }
        
        fn is_suffix_char(c: char) -> bool {
            c == ']' || c == '}'
        }
        
        fn is_prompt_item_char(c: char, token: &str) -> bool {
            let s = c.to_string();
            if token.is_empty() {
                libs::re::re_contains(&s, r#"^[a-zA-Z_]$"#)
            } else {
                libs::re::re_contains(&s, r#"^[a-zA-Z0-9_]$"#)
            }
        }
        
        pub fn get_prompt_string() -> String {
            if let Ok(x) = env::var("PROMPT") {
                return x;
            }
            DEFAULT_PROMPT.to_string()
        }
        
        fn apply_prompt_item(sh: &shell::Shell, result: &mut String, token: &str) {
            if let Some(x) = sh.get_env(token) {
                result.push_str(&x);
                return;
            }
            apply_preset_item(sh, result, token);
        }
        
        fn apply_command(result: &mut String, token: &str, prefix: &str, suffix: &str) {
            let cr = execute::run(token);
            let output = cr.stdout.trim();
            if !output.is_empty() {
                result.push_str(prefix);
                result.push_str(output);
                result.push_str(suffix);
            }
        }
        
        pub fn render_prompt(sh: &shell::Shell, ps: &str) -> String {
            let mut prompt = String::new();
            apply_pyenv(&mut prompt);
        
            let mut met_dollar = false;
            let mut met_brace = false;
            let mut met_paren = false;
            let mut token = String::new();
            let mut prefix = String::new();
            let mut suffix = String::new();
            for c in ps.chars() {
                if met_dollar {
                    if c == '(' && !met_brace && !met_paren {
                        met_paren = true;
                        continue;
                    }
                    if c == ')' && met_paren {
                        apply_command(&mut prompt, &token, &prefix, &suffix);
                        token.clear();
                        prefix.clear();
                        suffix.clear();
                        met_dollar = false;
                        met_paren = false;
                        continue;
                    }
                    if c == '{' && !met_brace && !met_paren {
                        met_brace = true;
                        continue;
                    } else if c == '}' && met_brace {
                        apply_prompt_item(sh, &mut prompt, &token);
                        token.clear();
                        met_dollar = false;
                        met_brace = false;
                        continue;
                    } else if c == '$' {
                        if token.is_empty() {
                            // to make single $ as a plain $
                            prompt.push('$');
                            met_dollar = true;
                            continue;
                        } else {
                            apply_prompt_item(sh, &mut prompt, &token);
                            token.clear();
                            // met_dollar is still true
                            continue;
                        }
                    } else if met_paren {
                        if is_prefix_char(c) {
                            prefix.push(c);
                        } else if is_suffix_char(c) {
                            suffix.push(c);
                        } else {
                            token.push(c);
                        }
                        continue;
                    } else if is_prompt_item_char(c, &token) {
                        token.push(c);
                        continue;
                    } else if token.is_empty() {
                        prompt.push('$');
                        prompt.push(c);
                        met_dollar = false;
                        continue;
                    }
                }
        
                if c == '$' {
                    met_dollar = true;
                    continue;
                }
        
                if !token.is_empty() {
                    apply_prompt_item(sh, &mut prompt, &token);
                    token.clear();
                }
                prompt.push(c);
                met_dollar = false;
            }
        
            if !token.is_empty() {
                apply_prompt_item(sh, &mut prompt, &token);
                met_dollar = false;
            }
        
            if met_dollar {
                // for cases like PROMPT='$$'
                prompt.push('$');
            }
        
            if prompt.trim().is_empty() {
                return format!("cicada-{} >> ", env!("CARGO_PKG_VERSION"));
            }
            prompt
        }
    }
    
    mod preset
    {
        use ::
        {
            *,
        };
        /*
        use std::env;
        use std::fs::File;
        use std::io::{Read, Write};
        use std::path::Path;
        
        use crate::libs;
        use crate::shell;
        use crate::tools;
        */
        fn apply_seq(prompt: &mut String) {
            prompt.push_str(libs::colored::SEQ);
        }
        
        fn apply_end_seq(prompt: &mut String) {
            prompt.push_str(libs::colored::END_SEQ);
        }
        
        fn apply_esc(prompt: &mut String) {
            prompt.push_str(libs::colored::ESC);
        }
        
        fn apply_underlined(prompt: &mut String) {
            prompt.push_str(libs::colored::UNDERLINED);
        }
        
        fn apply_user(prompt: &mut String) {
            let username = tools::get_user_name();
            prompt.push_str(&username);
        }
        
        fn apply_black(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK);
        }
        
        fn apply_black_b(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK_B);
        }
        
        fn apply_black_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK_BG);
        }
        
        fn apply_blue(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE);
        }
        
        fn apply_blue_b(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_B);
        }
        
        fn apply_blue_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_BG);
        }
        
        fn apply_bold(prompt: &mut String) {
            prompt.push_str(libs::colored::BOLD);
        }
        
        fn apply_green(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN);
        }
        
        fn apply_green_b(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_B);
        }
        
        fn apply_green_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_BG);
        }
        
        fn apply_red(prompt: &mut String) {
            prompt.push_str(libs::colored::RED);
        }
        
        fn apply_red_b(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_B);
        }
        
        fn apply_red_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_BG);
        }
        
        fn apply_white(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE);
        }
        
        fn apply_white_b(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE_B);
        }
        
        fn apply_white_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE_BG);
        }
        
        fn apply_hidden(prompt: &mut String) {
            prompt.push_str(libs::colored::HIDDEN);
        }
        
        fn apply_reset(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET);
        }
        
        fn apply_reverse(prompt: &mut String) {
            prompt.push_str(libs::colored::REVERSE);
        }
        
        fn apply_dim(prompt: &mut String) {
            prompt.push_str(libs::colored::DIM);
        }
        
        fn apply_blink(prompt: &mut String) {
            prompt.push_str(libs::colored::BLINK);
        }
        
        fn apply_reset_underlined(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_UNDERLINED);
        }
        
        fn apply_reset_dim(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_DIM);
        }
        
        fn apply_reset_reverse(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_REVERSE);
        }
        
        fn apply_reset_hidden(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_HIDDEN);
        }
        
        fn apply_reset_blink(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_BLINK);
        }
        
        fn apply_reset_bold(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_BOLD);
        }
        
        fn apply_default(prompt: &mut String) {
            prompt.push_str(libs::colored::DEFAULT);
        }
        
        fn apply_default_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::DEFAULT_BG);
        }
        
        fn apply_cyan(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN);
        }
        
        fn apply_cyan_l(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_L);
        }
        
        fn apply_cyan_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_BG);
        }
        
        fn apply_cyan_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_L_BG);
        }
        
        fn apply_red_l(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_L);
        }
        
        fn apply_red_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_L_BG);
        }
        
        fn apply_green_l(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_L);
        }
        
        fn apply_green_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_L_BG);
        }
        
        fn apply_gray_l(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_L);
        }
        
        fn apply_gray_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_L_BG);
        }
        
        fn apply_gray_d(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_D);
        }
        
        fn apply_gray_d_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_D_BG);
        }
        
        fn apply_magenta(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA);
        }
        
        fn apply_magenta_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_BG);
        }
        
        fn apply_magenta_l(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_L);
        }
        
        fn apply_magenta_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_L_BG);
        }
        
        fn apply_yellow(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW);
        }
        
        fn apply_yellow_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_BG);
        }
        
        fn apply_yellow_l(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_L);
        }
        
        fn apply_yellow_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_L_BG);
        }
        
        fn apply_blue_l(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_L);
        }
        
        fn apply_blue_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_L_BG);
        }
        
        fn apply_color_status(sh: &shell::Shell, prompt: &mut String) {
            if sh.previous_status == 0 {
                prompt.push_str(libs::colored::GREEN_B);
            } else {
                prompt.push_str(libs::colored::RED_B);
            }
        }
        
        fn _find_git_root() -> String {
            let current_dir = libs::path::current_dir();
            let dir_git = format!("{}/.git", current_dir);
            if Path::new(&dir_git).exists() {
                return current_dir;
            }
        
            let mut _dir = current_dir.clone();
            while Path::new(&_dir).parent().is_some() {
                match Path::new(&_dir).parent() {
                    Some(p) => {
                        _dir = p.to_string_lossy().to_string();
                        let dir_git = format!("{}/.git", _dir);
                        if Path::new(&dir_git).exists() {
                            return _dir;
                        }
                    }
                    None => {
                        break;
                    }
                }
            }
        
            String::new()
        }
        
        fn apply_gitbr(prompt: &mut String) {
            let git_root = _find_git_root();
            if git_root.is_empty() {
                return;
            }
        
            let file_head = format!("{}/.git/HEAD", git_root);
            if !Path::new(&file_head).exists() {
                return;
            }
        
            let mut file;
            match File::open(&file_head) {
                Ok(x) => file = x,
                Err(e) => {
                    println!("cicada: .git/HEAD err: {:?}", e);
                    return;
                }
            }
            let mut text = String::new();
            match file.read_to_string(&mut text) {
                Ok(_) => {}
                Err(e) => {
                    println!("cicada: read_to_string error: {:?}", e);
                    return;
                }
            }
        
            if let Some(branch) = libs::re::find_first_group(r"^[a-z]+: ?[a-z]+/[a-z]+/(.+)$", text.trim())
            {
                apply_blue_b(prompt);
                if let Ok(x) = env::var("CICADA_GITBR_PREFIX") {
                    prompt.push_str(&x);
                }
        
                let _len_default: i32 = 32;
                let mut len_max = if let Ok(x) = env::var("CICADA_GITBR_MAX_LEN") {
                    match x.parse::<i32>() {
                        Ok(n) => n,
                        Err(_) => _len_default,
                    }
                } else {
                    _len_default
                };
                if len_max <= 0 {
                    len_max = _len_default;
                }
        
                if branch.len() as i32 <= len_max {
                    prompt.push_str(&branch);
                } else {
                    let len = branch.len() as i32;
                    let offset = (len - len_max + 2) as usize;
                    let branch_short = format!("..{}", &branch[offset..]);
                    prompt.push_str(&branch_short);
                }
                if let Ok(x) = env::var("CICADA_GITBR_SUFFIX") {
                    prompt.push_str(&x);
                }
                apply_reset(prompt);
            }
        }
        
        fn apply_cwd(prompt: &mut String) {
            let _current_dir = match env::current_dir() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: PROMPT: env current_dir error: {}", e);
                    return;
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: PROMPT: to_str error");
                    return;
                }
            };
            let _tokens: Vec<&str> = current_dir.split('/').collect();
        
            let last = match _tokens.last() {
                Some(x) => x,
                None => {
                    log!("cicada: PROMPT: token last error");
                    return;
                }
            };
        
            let home = tools::get_user_home();
            let pwd = if last.is_empty() {
                "/"
            } else if current_dir == home {
                "~"
            } else {
                last
            };
            prompt.push_str(pwd);
        }
        
        fn apply_hostname(prompt: &mut String) {
            let hostname = tools::get_hostname();
            prompt.push_str(&hostname);
        }
        
        fn apply_newline(prompt: &mut String) {
            prompt.push('\n');
        }
        
        pub fn apply_pyenv(prompt: &mut String) {
            if let Ok(x) = env::var("VIRTUAL_ENV") {
                if !x.is_empty() {
                    let _tokens: Vec<&str> = x.split('/').collect();
                    let env_name = match _tokens.last() {
                        Some(x) => x,
                        None => {
                            log!("prompt token last error");
                            return;
                        }
                    };
        
                    apply_blue_b(prompt);
                    prompt.push('(');
                    prompt.push_str(env_name);
                    prompt.push(')');
                    apply_reset(prompt);
                }
            }
        }
        
        pub fn apply_preset_item(sh: &shell::Shell, prompt: &mut String, token: &str) {
            match token.to_ascii_lowercase().as_ref() {
                "black" => apply_black(prompt),
                "black_b" => apply_black_b(prompt),
                "black_bg" => apply_black_bg(prompt),
                "blink" => apply_blink(prompt),
                "blue" => apply_blue(prompt),
                "blue_b" => apply_blue_b(prompt),
                "blue_bg" => apply_blue_bg(prompt),
                "blue_l" => apply_blue_l(prompt),
                "blue_l_bg" => apply_blue_l_bg(prompt),
                "bold" => apply_bold(prompt),
                "color_status" => apply_color_status(sh, prompt),
                "cwd" => apply_cwd(prompt),
                "cyan" => apply_cyan(prompt),
                "cyan_bg" => apply_cyan_bg(prompt),
                "cyan_l" => apply_cyan_l(prompt),
                "cyan_l_bg" => apply_cyan_l_bg(prompt),
                "default" => apply_default(prompt),
                "default_bg" => apply_default_bg(prompt),
                "dim" => apply_dim(prompt),
                "end_seq" => apply_end_seq(prompt),
                "esc" => apply_esc(prompt),
                "gitbr" => apply_gitbr(prompt),
                "gray_d" => apply_gray_d(prompt),
                "gray_d_bg" => apply_gray_d_bg(prompt),
                "gray_l" => apply_gray_l(prompt),
                "gray_l_bg" => apply_gray_l_bg(prompt),
                "green" => apply_green(prompt),
                "green_b" => apply_green_b(prompt),
                "green_bg" => apply_green_bg(prompt),
                "green_l" => apply_green_l(prompt),
                "green_l_bg" => apply_green_l_bg(prompt),
                "hidden" => apply_hidden(prompt),
                "hostname" => apply_hostname(prompt),
                "magenta" => apply_magenta(prompt),
                "magenta_bg" => apply_magenta_bg(prompt),
                "magenta_l" => apply_magenta_l(prompt),
                "magenta_l_bg" => apply_magenta_l_bg(prompt),
                "newline" => apply_newline(prompt),
                "red" => apply_red(prompt),
                "red_b" => apply_red_b(prompt),
                "red_bg" => apply_red_bg(prompt),
                "red_l" => apply_red_l(prompt),
                "red_l_bg" => apply_red_l_bg(prompt),
                "reset" => apply_reset(prompt),
                "reset_blink" => apply_reset_blink(prompt),
                "reset_bold" => apply_reset_bold(prompt),
                "reset_dim" => apply_reset_dim(prompt),
                "reset_hidden" => apply_reset_hidden(prompt),
                "reset_reverse" => apply_reset_reverse(prompt),
                "reset_underlined" => apply_reset_underlined(prompt),
                "reverse" => apply_reverse(prompt),
                "seq" => apply_seq(prompt),
                "underlined" => apply_underlined(prompt),
                "user" => apply_user(prompt),
                "white" => apply_white(prompt),
                "white_b" => apply_white_b(prompt),
                "white_bg" => apply_white_bg(prompt),
                "yellow" => apply_yellow(prompt),
                "yellow_bg" => apply_yellow_bg(prompt),
                "yellow_l" => apply_yellow_l(prompt),
                "yellow_l_bg" => apply_yellow_l_bg(prompt),
                _ => (),
            }
        }
    }
    
    mod multilines
    {
        use ::
        {
            *,
        };
        /*
        use std::io;
        use lineread::{Function, Prompter, Terminal};
        
        use crate::parsers::parser_line;
        */
        pub struct EnterFunction;
        
        impl<T: Terminal> Function<T> for EnterFunction 
        {
            fn execute(&self, prompter: &mut Prompter<T>, count: i32, _ch: char) -> io::Result<()> {
                let buf = prompter.buffer();
                let linfo = parser_line::parse_line(buf);
                if linfo.is_complete {
                    prompter.accept_input()
                } else if count > 0 {
                    match prompter.insert(count as usize, '\n') {
                        Ok(_) => {},
                        Err(e) => {
                            println!("sub-prompt error: {}", e);
                        }
                    }
                    prompter.insert_str(">> ")
                } else {
                    Ok(())
                }
            }
        }
    }

    fn get_prompt_len(prompt: &str) -> i32 
    {
        let mut count = 0;
        let mut met_x01 = false;
        for c in prompt.chars() {
            if c == '\x01' {
                met_x01 = true;
                continue;
            } else if c == '\x02' {
                met_x01 = false;
                continue;
            }
            if !met_x01 {
                count += 1;
            }
        }
        count
    }
    
    pub fn get_prompt(sh: &shell::Shell) -> String {
        let ps = get_prompt_string();
        let mut prompt = render_prompt(sh, &ps);
        if let Some((w, _h)) = libs::term_size::dimensions() {
            if get_prompt_len(&prompt) > (w / 2) as i32
                && !libs::re::re_contains(&ps, r#"(?i)\$\{?newline.\}?"#)
            {
                prompt.push_str("\n$ ");
            }
        } else {
            log!("ERROR: Failed to get term size");
        }
        prompt
    }
}

pub mod rcfile
{
    use ::
    {
        *,
    };
    /*
    use std::path::Path;
    
    use crate::scripting;
    use crate::shell;
    use crate::tools;
    */
    pub fn get_rc_file() -> String {
        let dir_config = tools::get_config_dir();
        let rc_file = format!("{}/cicadarc", dir_config);
        if Path::new(&rc_file).exists() {
            return rc_file;
        }
    
        // fail back to $HOME/.cicadarc
        let home = tools::get_user_home();
        let rc_file_home = format!("{}/{}", home, ".cicadarc");
        if Path::new(&rc_file_home).exists() {
            return rc_file_home;
        }
    
        // use std path if both absent
        rc_file
    }
    
    pub fn load_rc_files(sh: &mut shell::Shell) {
        let rc_file = get_rc_file();
        if !Path::new(&rc_file).exists() {
            return;
        }
    
        let args = vec!["source".to_string(), rc_file];
        scripting::run_script(sh, &args);
    }
}

pub mod run
{
    use ::
    {
        *,
    };
    /*
    use regex::Regex;
    
    use crate::shell;
    use crate::tools;
    use crate::types::{Command, CommandLine, CommandResult};
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::builtins::utils::print_stdout_with_capture;

    use crate::builtins::utils::print_stderr_with_capture;
    use crate::jobc;
    use crate::libc;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use std::env;
    use std::path::Path;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::parsers;
    use crate::shell;
    use crate::tools;
    use crate::types::{Command, CommandLine, CommandResult};

    use crate::builtins::utils::print_stdout_with_capture;
    use crate::history;
    use crate::libs;
    use crate::rcfile;
    use crate::shell::Shell;
    use crate::types::{Command, CommandLine, CommandResult};

    use exec;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::parsers;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    #![allow(unreachable_code)]
    use std::process;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use regex::Regex;
    use std::env;
    
    use crate::libs;
    use crate::parsers;
    use crate::tools;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use libc;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::jobc;
    use crate::shell::{self, Shell};
    use crate::types::{CommandResult, CommandLine, Command};

    use std::path::Path;
    
    use rusqlite::Connection as Conn;
    use structopt::StructOpt;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::builtins::utils::print_stdout_with_capture;
    use crate::ctime;
    use crate::history;
    use crate::parsers;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use crate::builtins::utils::print_stdout_with_capture;
    use crate::jobc;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use std::io;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::libs::re::re_contains;
    use crate::types::{CommandResult, CommandLine, Command};
    use crate::tools;

    use structopt::StructOpt;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::builtins::utils::print_stdout_with_capture;
    use crate::parsers;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use crate::builtins::utils::print_stderr_with_capture;
    use crate::parsers;
    use crate::scripting;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use clap::{Parser, CommandFactory};
    use std::io::Error;
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::builtins::utils::print_stdout_with_capture;
    use crate::parsers;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use crate::builtins::utils::print_stderr_with_capture;
    use crate::shell::Shell;
    use crate::types::{CommandResult, CommandLine, Command};

    use std::fs::File;
    use std::io::Write;
    use std::os::unix::io::{FromRawFd, RawFd};
    
    use errno::errno;
    
    use crate::tools;
    use crate::types::{Command, CommandLine, CommandResult, Redirection};

    use std::env;
    use std::fs;
    use std::path::Path;
    
    use crate::builtins::utils::print_stderr_with_capture;
    use crate::builtins::utils::print_stdout_with_capture;
    use crate::execute;
    use crate::parsers;
    use crate::shell::{self, Shell};
    use crate::types::{self, CommandResult, CommandLine, Command};
    */
    #[derive(Debug, StructOpt)]
    #[structopt(name = "history", about = "History in cicada shell")]
    struct OptMainHistory 
    {
        #[structopt(short, long, help = "For current session only")]
        session: bool,
    
        #[structopt(short, long, help = "Search old items first")]
        asc: bool,
    
        #[structopt(short, long, help = "For current directory only")]
        pwd: bool,
    
        #[structopt(short, long, help = "Only show ROWID")]
        only_id: bool,
    
        #[structopt(short, long, help = "Do not show ROWID")]
        no_id: bool,
    
        #[structopt(short="d", long, help = "Show date")]
        show_date: bool,
    
        #[structopt(short, long, default_value = "20")]
        limit: i32,
    
        #[structopt(name = "PATTERN", default_value = "", help = "You can use % to match anything")]
        pattern: String,
    
        #[structopt(subcommand)]
        cmd: Option<SubCommand>
    }

    #[derive(Debug, StructOpt)]
    #[structopt(name = "set", about = "Set shell options (BETA)")]
    struct OptMainSet 
    {
        #[structopt(short, help = "exit on error status")]
        exit_on_error: bool,
    }

    #[derive(Parser)]
    #[command(name = "ulimit", about = "show / modify shell resource limits")]
    #[allow(non_snake_case)]
    struct App 
    {
        #[arg(short, help = "All current limits are reported.")]
        a: bool,
        #[arg(short, value_name = "NEW VALUE", help = "The maximum number of open file descriptors.")]
        n: Option<Option<u64>>,
        #[arg(short, value_name = "NEW VALUE", help = "The maximum size of core files created.")]
        c: Option<Option<u64>>,
        #[arg(short = 'S', help = "Set a soft limit for the given resource. (default)")]
        S: bool,
        #[arg(short = 'H', help = "Set a hard limit for the given resource.")]
        H: bool,
    }
    
    #[derive(StructOpt, Debug)]
    enum SubCommand 
    {
        #[structopt(about="Add new item into history")]
        Add 
        {
            #[structopt(short="t", long, help = "Specify a timestamp for the new item")]
            timestamp: Option<f64>,
    
            #[structopt(name="INPUT", help = "input to be added into history")]
            input: String,
        },
        #[structopt(about="Delete item from history")]
        Delete 
        {
            #[structopt(name="ROWID", help = "Row IDs of item to delete")]
            rowid: Vec<usize>,
        }
    }
    
    pub fn alias(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();
    
        if tokens.len() == 1 {
            return show_alias_list(sh, cmd, cl, capture);
        }
    
        if tokens.len() > 2 {
            let info = "alias syntax error: usage: alias foo='echo foo'";
            print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
            return cr;
        }
    
        let input = &tokens[1].1;
        let re_single_read = Regex::new(r"^[a-zA-Z0-9_\.-]+$").unwrap();
        if re_single_read.is_match(input) {
            return show_single_alias(sh, input, cmd, cl, capture);
        }
    
        let re_to_add = Regex::new(r"^([a-zA-Z0-9_\.-]+)=(.*)$").unwrap();
        for cap in re_to_add.captures_iter(input) {
            let name = tools::unquote(&cap[1]);
            // due to limitation of `parses::parser_line`,
            // `alias foo-bar='foo bar'` will become 'foo-bar=foo bar'
            // while `alias foo_bar='foo bar'` keeps foo_bar='foo bar'
            let value = if cap[2].starts_with('"') || cap[2].starts_with('\'') {
                tools::unquote(&cap[2])
            } else {
                cap[2].to_string()
            };
            sh.add_alias(name.as_str(), value.as_str());
        }
    
        CommandResult::new()
    }
    
    pub fn bg(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();
    
        if sh.jobs.is_empty() {
            let info = "cicada: bg: no job found";
            print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
            return cr;
        }
    
        let mut job_id = -1;
        if tokens.len() == 1 {
            if let Some((gid, _)) = sh.jobs.iter().next() {
                job_id = *gid;
            }
        }
    
        if tokens.len() >= 2 {
            let mut job_str = tokens[1].1.clone();
            if job_str.starts_with("%") {
                job_str = job_str.trim_start_matches('%').to_string();
            }
    
            match job_str.parse::<i32>() {
                Ok(n) => job_id = n,
                Err(_) => {
                    let info = "cicada: bg: invalid job id";
                    print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            }
        }
        if job_id == -1 {
            let info = "cicada: bg: not such job";
            print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
            return cr;
        }
    
        let gid: i32;
    
        {
            let mut result = sh.get_job_by_id(job_id);
            // fall back to find job by using prcess group id
            if result.is_none() {
                result = sh.get_job_by_gid(job_id);
            }
    
            match result {
                Some(job) => {
                    unsafe {
                        libc::killpg(job.gid, libc::SIGCONT);
                        gid = job.gid;
                        if job.status == "Running" {
                            let info = format!("cicada: bg: job {} already in background", job.id);
                            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                            return cr;
                        }
                    }
    
                    let info_cmd = format!("[{}]  {} &", job.id, job.cmd);
                    print_stderr_with_capture(&info_cmd, &mut cr, cl, cmd, capture);
                    cr.status = 0;
                }
                None => {
                    let info = "cicada: bg: not such job";
                    print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            }
        }
    
        jobc::mark_job_as_running(sh, gid, true);
        cr
    }
    
    pub fn cd(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();
        let args = parsers::parser_line::tokens_to_args(&tokens);
    
        if args.len() > 2 {
            let info = "cicada: cd: too many argument";
            print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
            return cr;
        }
    
        let str_current_dir = tools::get_current_dir();
    
        let mut dir_to = if args.len() == 1 {
            let home = tools::get_user_home();
            home.to_string()
        } else {
            args[1..].join("")
        };
    
        if dir_to == "-" {
            if sh.previous_dir.is_empty() {
                let info = "no previous dir";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }
            dir_to = sh.previous_dir.clone();
        } else if !dir_to.starts_with('/') {
            dir_to = format!("{}/{}", str_current_dir, dir_to);
        }
    
        if !Path::new(&dir_to).exists() {
            let info = format!("cicada: cd: {}: No such file or directory", &args[1]);
            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
            return cr;
        }
    
        match Path::new(&dir_to).canonicalize() {
            Ok(p) => {
                dir_to = p.as_path().to_string_lossy().to_string();
            }
            Err(e) => {
                let info = format!("cicada: cd: error: {}", e);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }
        }
    
        match env::set_current_dir(&dir_to) {
            Ok(_) => {
                sh.current_dir = dir_to.clone();
                if str_current_dir != dir_to {
                    sh.previous_dir = str_current_dir.clone();
                    env::set_var("PWD", &sh.current_dir);
                };
                cr.status = 0;
                cr
            }
            Err(e) => {
                let info = format!("cicada: cd: {}", e);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                cr
            }
        }
    }
    
    pub fn c(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult // cinfo
    {
        let mut info = vec![];
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        info.push(("version", VERSION));
    
        let os_name = libs::os_type::get_os_name();
        info.push(("os-name", &os_name));
    
        let hfile = history::get_history_file();
        info.push(("history-file", &hfile));
    
        let rcf = rcfile::get_rc_file();
        info.push(("rc-file", &rcf));
    
        let git_hash = env!("GIT_HASH");
        if !git_hash.is_empty() {
            info.push(("git-commit", env!("GIT_HASH")));
        }
    
        let git_branch = env!("GIT_BRANCH");
        let mut branch = String::new();
        if !git_branch.is_empty() {
            branch.push_str(git_branch);
            let git_status = env!("GIT_STATUS");
            if git_status != "0" {
                branch.push_str(" (dirty)");
            }
            info.push(("git-branch", &branch));
        }
    
        info.push(("built-with", env!("BUILD_RUSTC_VERSION")));
        info.push(("built-at", env!("BUILD_DATE")));
    
        let mut lines = Vec::new();
        for (k, v) in &info {
            // longest key above is 12-char length
            lines.push(format!("{: >12}: {}", k, v));
        }
        let buffer = lines.join("\n");
        let mut cr = CommandResult::new();
        print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
        cr
    }

    pub fn exe(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult // exec
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();
        let args = parsers::parser_line::tokens_to_args(&tokens);
        let len = args.len();
        if len == 1 {
         print_stderr_with_capture("invalid usage", &mut cr, cl, cmd, capture);
         return cr;
        }

        let mut _cmd = exec::Command::new(&args[1]);
        let err = _cmd.args(&args[2..len]).exec();
        let info = format!("cicada: exe: {}", err);
        print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
        cr
    }

    pub fn exit(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();
        if tokens.len() > 2 {
         let info = "cicada: exit: too many arguments";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        if tokens.len() == 2 {
         let _code = &tokens[1].1;
         match _code.parse::<i32>() {
             Ok(x) => {
                 process::exit(x);
             }
             Err(_) => {
                 let info = format!("cicada: exit: {}: numeric argument required", _code);
                 print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                 process::exit(255);
             }
         }
        }

        for (_i, job) in sh.jobs.iter() {
         if !job.cmd.starts_with("nohup ") {
             let mut info = String::new();
             info.push_str("There are background jobs.");
             info.push_str("Run `jobs` to see details; `exit 1` to force quit.");
             print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
             return cr;
         }
        }

        process::exit(0);
        cr
    }

    pub fn export(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();

        let re_name_ptn = Regex::new(r"^([a-zA-Z_][a-zA-Z0-9_]*)=(.*)$").unwrap();
        for (_, text) in tokens.iter() {
         if text == "export" {
             continue;
         }

         if !tools::is_env(text) {
             let mut info = String::new();
             info.push_str("export: invalid command\n");
             info.push_str("usage: export XXX=YYY");
             print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
             return cr;
         }

         if !re_name_ptn.is_match(text) {
             let mut info = String::new();
             info.push_str("export: invalid command\n");
             info.push_str("usage: export XXX=YYY ZZ=123");
             print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
             return cr;
         }

         for cap in re_name_ptn.captures_iter(text) {
             let name = cap[1].to_string();
             let token = parsers::parser_line::unquote(&cap[2]);
             let value = libs::path::expand_home(&token);
             env::set_var(name, &value);
         }
        }
        cr
    }

    pub fn fg(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();

        if sh.jobs.is_empty() {
         let info = "cicada: fg: no job found";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        let mut job_id = -1;
        if tokens.len() == 1 {
         if let Some((gid, _)) = sh.jobs.iter().next() {
             job_id = *gid;
         }
        }

        if tokens.len() >= 2 {
         let mut job_str = tokens[1].1.clone();
         if job_str.starts_with("%") {
             job_str = job_str.trim_start_matches('%').to_string();
         }

         match job_str.parse::<i32>() {
             Ok(n) => job_id = n,
             Err(_) => {
                 let info = "cicada: fg: invalid job id";
                 print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                 return cr;
             }
         }
        }

        if job_id == -1 {
         let info = "cicada: not job id found";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        let gid: i32;
        let pid_list: Vec<i32>;

        {
         let mut result = sh.get_job_by_id(job_id);
         // fall back to find job by using prcess group id
         if result.is_none() {
             result = sh.get_job_by_gid(job_id);
         }

         match result {
             Some(job) => {
                 print_stderr_with_capture(&job.cmd, &mut cr, cl, cmd, capture);
                 cr.status = 0;

                 unsafe {
                     if !shell::give_terminal_to(job.gid) {
                         return CommandResult::error();
                     }

                     libc::killpg(job.gid, libc::SIGCONT);
                     pid_list = job.pids.clone();
                     gid = job.gid;
                 }
             }
             None => {
                 let info = "cicada: fg: no such job";
                 print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                 return cr;
             }
         }
        }

        unsafe {
         jobc::mark_job_as_running(sh, gid, false);

         let cr = jobc::wait_fg_job(sh, gid, &pid_list);

         let gid_shell = libc::getpgid(0);
         if !shell::give_terminal_to(gid_shell) {
             log!("failed to give term to back to shell : {}", gid_shell);
         }

         cr
        }
    }

    pub fn history(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let hfile = history::get_history_file();
        let path = Path::new(hfile.as_str());
        if !path.exists() {
        let info = "no history file";
        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
        return cr;
        }
        let conn = match Conn::open(&hfile) {
        Ok(x) => x,
        Err(e) => {
            let info = format!("history: sqlite error: {:?}", e);
            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
            return cr;
        }
        };

        let tokens = cmd.tokens.clone();
        let args = parsers::parser_line::tokens_to_args(&tokens);

        let show_usage = args.len() > 1 && (args[1] == "-h" || args[1] == "--help");
        let opt = OptMain::from_iter_safe(args);
        match opt {
        Ok(opt) => {
            match opt.cmd {
                Some(SubCommand::Delete {rowid: rowids}) => {
                    let mut _count = 0;
                    for rowid in rowids {
                        let _deleted = delete_history_item(&conn, rowid);
                        if _deleted {
                            _count += 1;
                        }
                    }
                    if _count > 0 {
                        let info = format!("deleted {} items", _count);
                        print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                    }
                    cr
                }
                Some(SubCommand::Add {timestamp: ts, input}) => {
                    let ts = ts.unwrap_or(0 as f64);
                    add_history(sh, ts, &input);
                    cr
                }
                None => {
                    let (str_out, str_err) = list_current_history(sh, &conn, &opt);
                    if !str_out.is_empty() {
                        print_stdout_with_capture(&str_out, &mut cr, cl, cmd, capture);
                    }
                    if !str_err.is_empty() {
                        print_stderr_with_capture(&str_err, &mut cr, cl, cmd, capture);
                    }
                    cr
                }
            }
        }
        Err(e) => {
            let info = format!("{}", e);
            if show_usage {
                print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                cr.status = 0;
            } else {
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                cr.status = 1;
            }
            cr
        }
        }
    }

    pub fn jobs(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        if sh.jobs.is_empty() {
        return cr;
        }

        // update status of jobs if any
        jobc::try_wait_bg_jobs(sh, false, false);

        let mut lines = Vec::new();
        let jobs = sh.jobs.clone();
        let no_trim = cmd.tokens.len() >= 2 && cmd.tokens[1].1 == "-f";
        for (_i, job) in jobs.iter() {
        let line = jobc::get_job_line(job, !no_trim);
        lines.push(line);
        }
        let buffer = lines.join("\n");

        print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
        cr
    }

    pub fn minfd(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();

        let fd = nix::fcntl::open(
        "/dev/null",
        nix::fcntl::OFlag::empty(),
        nix::sys::stat::Mode::empty()
        );
        match fd {
        Ok(fd) => {
            let info = format!("{}", fd);
            print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
            unsafe { libc::close(fd); }
        }
        Err(e) => {
            println_stderr!("cicada: minfd: error: {}", e);
        }
        }

        cr
    }

    pub fn read(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();

        let name_list: Vec<String>;
        if tokens.len() <= 1 {
        name_list = vec!["REPLY".to_string()];
        } else {
        name_list = tokens[1..].iter().map(|x| x.1.clone()).collect();
        if let Some(id_) = _find_invalid_identifier(&name_list) {
            let info = format!("cicada: read: `{}': not a valid identifier", id_);
            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
            return cr;
        }
        }

        let mut buffer = String::new();

        if cmd.has_here_string() {
        if let Some(redirect_from) = &cmd.redirect_from {
            buffer.push_str(&redirect_from.1);
            buffer.push('\n');
        }
        } else {
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {}
            Err(e) => {
                let info = format!("cicada: read: error in reading stdin: {:?}", e);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }
        }
        }

        let envs = cl.envs.clone();
        let value_list = tools::split_into_fields(sh, buffer.trim(), &envs);

        let idx_2rd_last = name_list.len() - 1;
        for i in 0..idx_2rd_last {
        let name = name_list.get(i);
        if name.is_none() {
            let info = "cicada: read: name index error";
            print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
            return cr;
        }
        let name = name.unwrap();

        let value = value_list.get(i).unwrap_or(&String::new()).clone();
        sh.set_env(name, &value);
        }

        let name_last = &name_list[idx_2rd_last];
        let value_left: String = if value_list.len() > idx_2rd_last {
        value_list[idx_2rd_last..].join(" ")
        } else {
        String::new()
        };
        sh.set_env(name_last, &value_left);
        cr
    }

    pub fn set(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = &cmd.tokens;
        let args = parsers::parser_line::tokens_to_args(tokens);
        let show_usage = args.len() > 1 && (args[1] == "-h" || args[1] == "--help");

        let opt = OptMain::from_iter_safe(args);
        match opt {
        Ok(opt) => {
            if opt.exit_on_error {
                sh.exit_on_error = true;
                cr
            } else {
                let info = "cicada: set: option not implemented";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                cr
            }
        }
        Err(e) => {
            let info = format!("{}", e);
            if show_usage {
                print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                cr.status = 0;
            } else {
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                cr.status = 1;
            }
            cr
        }
        }
    }

    pub fn source(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = &cmd.tokens;
        let args = parsers::parser_line::tokens_to_args(tokens);

        if args.len() < 2 {
        let info = "cicada: source: no file specified";
        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
        return cr;
        }

        let status = scripting::run_script(sh, &args);
        cr.status = status;
        cr
    }

    pub fn ulimit(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = &cmd.tokens;
        let args = parsers::parser_line::tokens_to_args(tokens);
    
        if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
            App::command().print_help().unwrap();
            println!();
            return cr;
        }
    
        let app = App::parse_from(args);
    
        if app.H && app.S {
            println!("cicada: ulimit: Cannot both hard and soft.");
            cr.status = 1;
            return cr;
        }
    
        let mut all_stdout = String::new();
        let mut all_stderr = String::new();
    
        if app.a {
            report_all(&app, &mut all_stdout, &mut all_stderr);
        } else if handle_limit(app.n, "open_files", app.H, &mut all_stdout, &mut all_stderr)
            || handle_limit(app.c, "core_file_size", app.H, &mut all_stdout, &mut all_stderr) {
        } else {
            report_all(&app, &mut all_stdout, &mut all_stderr);
        }
    
        if !all_stdout.is_empty() {
            print_stdout_with_capture(&all_stdout, &mut cr, cl, cmd, capture);
        }
        if !all_stderr.is_empty() {
            print_stderr_with_capture(&all_stderr, &mut cr, cl, cmd, capture);
        }
    
        cr
    }

    pub fn unalias(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();

        if tokens.len() != 2 {
         let info = "cicada: unalias: syntax error";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        let input = &tokens[1].1;
        if !sh.remove_alias(input) {
         let info = format!("cicada: unalias: {}: not found", input);
         print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
         return cr;
        }
        cr
    }

    pub fn unpath(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();

        if tokens.len() != 2 {
         let info = "cicada: unpath: syntax error";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        let input = &tokens[1].1;
        sh.remove_path(input);
        cr
    }

    pub fn unset(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let tokens = cmd.tokens.clone();
        let mut cr = CommandResult::new();

        if tokens.len() != 2 {
         let info = "cicada: unset: syntax error";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         return cr;
        }

        let input = &tokens[1].1;
        if !sh.remove_env(input) {
         let info = format!("cicada: unset: invalid varname: {:?}", input);
         print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
         return cr;
        }
        cr
    }

    pub fn vox(sh: &mut shell::Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
    {
        let mut cr = CommandResult::new();
        let tokens = cmd.tokens.clone();
        let args = parsers::parser_line::tokens_to_args(&tokens);
        let len = args.len();
        let subcmd = if len > 1 { &args[1] } else { "" };

        if len == 1 || (len == 2 && subcmd == "ls") {
         match get_all_venvs() {
             Ok(venvs) => {
                 let info = venvs.join("\n");
                 print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                 return cr;
             }
             Err(reason) => {
                 print_stderr_with_capture(&reason, &mut cr, cl, cmd, capture);
                 return cr;
             }
         }
        }

        if len == 3 && subcmd == "create" {
         let pybin = match env::var("VIRTUALENV_PYBIN") {
             Ok(x) => x,
             Err(_) => "python3".to_string(),
         };
         let dir_venv = get_envs_home();
         let venv_name = args[2].to_string();
         let line = format!("{} -m venv \"{}/{}\"", pybin, dir_venv, venv_name);
         print_stderr_with_capture(&line, &mut cr, cl, cmd, capture);
         let cr_list = execute::run_command_line(sh, &line, false, false);
         return cr_list[0].clone();
        }

        if len == 3 && subcmd == "enter" {
         let _err = enter_env(sh, args[2].as_str());
         if !_err.is_empty() {
             print_stderr_with_capture(&_err, &mut cr, cl, cmd, capture);
         }
         cr
        } else if len == 2 && subcmd == "exit" {
         let _err = exit_env(sh);
         if !_err.is_empty() {
             print_stderr_with_capture(&_err, &mut cr, cl, cmd, capture);
         }
         cr
        } else {
         let info = "cicada: vox: invalid option";
         print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
         cr
        }
    }
    
    pub fn print_stdout(info: &str, cmd: &Command, cl: &CommandLine)
    {
        let fd = _get_dupped_stdout_fd(cmd, cl);
        if fd == -1 {
            return;
        }
    
        unsafe {
            let mut f = File::from_raw_fd(fd);
            let info = info.trim_end_matches('\n');
            match f.write_all(info.as_bytes()) {
                Ok(_) => {},
                Err(e) => {
                    println_stderr!("write_all: error: {}", e);
                }
            }
            if !info.is_empty() {
                match f.write_all(b"\n") {
                    Ok(_) => {},
                    Err(e) => {
                        println_stderr!("write_all: error: {}", e);
                    }
                }
            }
        }
    }
    
    pub fn print_stderr(info: &str, cmd: &Command, cl: &CommandLine) 
    {
        let fd = _get_dupped_stderr_fd(cmd, cl);
        if fd == -1 {
            return;
        }
    
        unsafe {
            let mut f = File::from_raw_fd(fd);
            let info = info.trim_end_matches('\n');
            match f.write_all(info.as_bytes()) {
                Ok(_) => (),
                Err(e) => {
                    println_stderr!("write_all: error: {}", e);
                }
            }
    
            if !info.is_empty() {
                match f.write_all(b"\n") {
                    Ok(_) => (),
                    Err(e) => {
                        println_stderr!("write_all: error: {}", e);
                    }
                }
            }
        }
    }
    
    pub fn print_stderr_with_capture( info: &str, cr: &mut CommandResult, cl: &CommandLine, cmd: &Command, capture: bool )
    {
        cr.status = 1;
        if capture {
            cr.stderr = info.to_string();
        } else {
            print_stderr(info, cmd, cl);
        }
    }
    
    pub fn print_stdout_with_capture(info: &str, cr: &mut CommandResult, cl: &CommandLine, cmd: &Command, capture: bool)
    {
        cr.status = 0;
        if capture {
            cr.stdout = info.to_string();
        } else {
            print_stdout(info, cmd, cl);
        }
    }
    
    fn add_history(sh: &Shell, ts: f64, input: &str) 
    {
        let (tsb, tse) = (ts, ts + 1.0);
        history::add_raw(sh, input, 0, tsb, tse);
    }

    fn list_current_history(sh: &Shell, conn: &Conn, opt: &OptMain) -> (String, String) 
    {
        let mut result_stderr = String::new();
        let result_stdout = String::new();

        let history_table = history::get_history_table();
        let mut sql = format!("SELECT ROWID, inp, tsb FROM {} WHERE ROWID > 0",
                          history_table);
        if !opt.pattern.is_empty() {
        sql = format!("{} AND inp LIKE '%{}%'", sql, opt.pattern)
        }
        if opt.session {
        sql = format!("{} AND sessionid = '{}'", sql, sh.session_id)
        }
        if opt.pwd {
        sql = format!("{} AND info like '%dir:{}|%'", sql, sh.current_dir)
        }

        if opt.asc {
        sql = format!("{} ORDER BY tsb", sql);
        } else {
        sql = format!("{} order by tsb desc", sql);
        };
        sql = format!("{} limit {} ", sql, opt.limit);

        let mut stmt = match conn.prepare(&sql) {
        Ok(x) => x,
        Err(e) => {
            let info = format!("history: prepare select error: {:?}", e);
            result_stderr.push_str(&info);
            return (result_stdout, result_stderr);
        }
        };

        let mut rows = match stmt.query([]) {
        Ok(x) => x,
        Err(e) => {
            let info = format!("history: query error: {:?}", e);
            result_stderr.push_str(&info);
            return (result_stdout, result_stderr);
        }
        };

        let mut lines = Vec::new();
        loop {
        match rows.next() {
            Ok(_rows) => {
                if let Some(row) = _rows {
                    let row_id: i32 = match row.get(0) {
                        Ok(x) => x,
                        Err(e) => {
                            let info = format!("history: error: {:?}", e);
                            result_stderr.push_str(&info);
                            return (result_stdout, result_stderr);
                        }
                    };
                    let inp: String = match row.get(1) {
                        Ok(x) => x,
                        Err(e) => {
                            let info = format!("history: error: {:?}", e);
                            result_stderr.push_str(&info);
                            return (result_stdout, result_stderr);
                        }
                    };

                    if opt.no_id {
                        lines.push(inp.to_string());
                    } else if opt.only_id {
                        lines.push(row_id.to_string());
                    } else if opt.show_date {
                        let tsb: f64 = match row.get(2) {
                            Ok(x) => x,
                            Err(e) => {
                                let info = format!("history: error: {:?}", e);
                                result_stderr.push_str(&info);
                                return (result_stdout, result_stderr);
                            }
                        };
                        let dt = ctime::DateTime::from_timestamp(tsb);
                        lines.push(format!("{}: {}: {}", row_id, dt, inp));
                    } else {
                        lines.push(format!("{}: {}", row_id, inp));
                    }
                } else {
                    break;
                }
            }
            Err(e) => {
                let info = format!("history: rows next error: {:?}", e);
                result_stderr.push_str(&info);
                return (result_stdout, result_stderr);
            }
        }
        }

        if !opt.asc {
        lines.reverse();
        }

        let buffer = lines.join("\n");

        (buffer, result_stderr)
    }

    fn delete_history_item(conn: &Conn, rowid: usize) -> bool 
    {
        let history_table = history::get_history_table();
        let sql = format!("DELETE from {} where rowid = {}", history_table, rowid);
        match conn.execute(&sql, []) {
        Ok(_) => true,
        Err(e) => {
            log!("history: error when delete: {:?}", e);
            false
        }
        }
    }
    
    fn show_alias_list(sh: &shell::Shell, cmd: &Command, cl: &CommandLine, capture: bool) -> CommandResult
    {
        let mut lines = Vec::new();
        for (name, value) in sh.get_alias_list() {
            let line = format!("alias {}='{}'", name, value);
            lines.push(line);
        }
        let buffer = lines.join("\n");
        let mut cr = CommandResult::new();
        print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
        cr
    }
    
    fn show_single_alias(sh: &shell::Shell, name_to_find: &str, cmd: &Command, cl: &CommandLine, capture: bool) -> CommandResult 
    {
        let mut cr = CommandResult::new();
        if let Some(content) = sh.get_alias_content(name_to_find) {
            let info = format!("alias {}='{}'", name_to_find, content);
            print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
        } else {
            let info = format!("cicada: alias: {}: not found", name_to_find);
            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
        }
        cr
    }

    fn _find_invalid_identifier(name_list: &Vec<String>) -> Option<String> 
    {
        for id_ in name_list {
            if !re_contains(id_, r"^[a-zA-Z_][a-zA-Z0-9_]*$") {
                return Some(id_.to_string());
            }
        }
        None
    }

    fn set_limit(limit_name: &str, value: u64, for_hard: bool) -> String
    {
        let limit_id = match limit_name {
            "open_files" => libc::RLIMIT_NOFILE,
            "core_file_size" => libc::RLIMIT_CORE,
            _ => return String::from("invalid limit name"),
        };
    
        let mut rlp = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
    
        unsafe {
            if libc::getrlimit(limit_id, &mut rlp) != 0 {
                return format!("cicada: ulimit: error getting limit: {}", Error::last_os_error());
            }
        }
    
        // to support armv7-linux-gnueabihf & 32-bit musl systems
        if for_hard {
            #[cfg(all(target_pointer_width = "32", target_env = "gnu"))]
            { rlp.rlim_max = value as u32; }
            #[cfg(not(all(target_pointer_width = "32", target_env = "gnu")))]
            { rlp.rlim_max = value; }
        } else {
            #[cfg(all(target_pointer_width = "32", target_env = "gnu"))]
            { rlp.rlim_cur = value as u32; }
            #[cfg(not(all(target_pointer_width = "32", target_env = "gnu")))]
            { rlp.rlim_cur = value; }
        }
    
        unsafe {
            if libc::setrlimit(limit_id, &rlp) != 0 {
                return format!("cicada: ulimit: error setting limit: {}", Error::last_os_error());
            }
        }
    
        String::new()
    }
    
    fn get_limit(limit_name: &str, single_print: bool, for_hard: bool) -> (String, String)
    {
        let (desc, limit_id) = match limit_name {
            "open_files" => ("open files", libc::RLIMIT_NOFILE),
            "core_file_size" => ("core file size", libc::RLIMIT_CORE),
            _ => return (String::new(), String::from("ulimit: error: invalid limit name")),
        };
    
        let mut rlp = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
    
        let mut result_stdout = String::new();
        let mut result_stderr = String::new();
    
        unsafe {
            if libc::getrlimit(limit_id, &mut rlp) != 0 {
                result_stderr.push_str(&format!("error getting limit: {}", Error::last_os_error()));
                return (result_stdout, result_stderr);
            }
    
            let to_print = if for_hard { rlp.rlim_max } else { rlp.rlim_cur };
    
            let info = if to_print == libc::RLIM_INFINITY {
                if single_print { "unlimited\n".to_string() } else { format!("{}\t\tunlimited\n", desc) }
            } else if single_print {
                format!("{}\n", to_print)
            } else {
                format!("{}\t\t{}\n", desc, to_print)
            };
    
            result_stdout.push_str(&info);
        }
    
        (result_stdout, result_stderr)
    }
    
    fn report_all(app: &App, all_stdout: &mut String, all_stderr: &mut String)
    {
        for limit_name in &["open_files", "core_file_size"] {
            let (out, err) = get_limit(limit_name, false, app.H);
            all_stdout.push_str(&out);
            all_stderr.push_str(&err);
        }
    }
    
    fn handle_limit
    (
        limit_option: Option<Option<u64>>,
        limit_name: &str,
        for_hard: bool,
        all_stdout: &mut String,
        all_stderr: &mut String
    ) -> bool 
    {
        match limit_option {
            None => false,
            Some(None) => {
                let (out, err) = get_limit(limit_name, true, for_hard);
                all_stdout.push_str(&out);
                all_stderr.push_str(&err);
                true
            }
            Some(Some(value)) => {
                let err = set_limit(limit_name, value, for_hard);
                if !err.is_empty() {
                    all_stderr.push_str(&err);
                }
                true
            }
        }
    }
    /// Helper function to get (stdout, stderr) pairs for redirections
    fn _get_std_fds(redirects: &[Redirection]) -> (Option<RawFd>, Option<RawFd>) {
        if redirects.is_empty() {
            return (None, None);
        }
    
        let mut fd_out = None;
        let mut fd_err = None;
    
        for i in 0..redirects.len() {
            let item = &redirects[i];
            if item.0 == "1" {
                // 1>&2
                let mut _fd_candidate = None;
    
                if item.2 == "&2" {
                    let (_fd_out, _fd_err) = _get_std_fds(&redirects[i+1..]);
                    if let Some(fd) = _fd_err {
                        _fd_candidate = Some(fd);
                    } else {
                        _fd_candidate = unsafe { Some(libc::dup(2)) };
                    }
                } else {  // 1> foo.log
                    let append = item.1 == ">>";
                    if let Ok(fd) = tools::create_raw_fd_from_file(&item.2, append) {
                        _fd_candidate = Some(fd);
                    }
                }
    
                // for command like this: `alias > a.txt > b.txt > c.txt`,
                // we need to return the last one, but close the previous two.
                if let Some(fd) = fd_out {
                    unsafe { libc::close(fd); }
                }
    
                fd_out = _fd_candidate;
            }
    
            if item.0 == "2" {
                // 2>&1
                let mut _fd_candidate = None;
    
                if item.2 == "&1" {
                    if let Some(fd) = fd_out {
                        _fd_candidate = unsafe { Some(libc::dup(fd)) };
                    }
                } else {  // 2>foo.log
                    let append = item.1 == ">>";
                    if let Ok(fd) = tools::create_raw_fd_from_file(&item.2, append) {
                        _fd_candidate = Some(fd);
                    }
                }
    
                if let Some(fd) = fd_err {
                    unsafe { libc::close(fd); }
                }
    
                fd_err = _fd_candidate;
            }
        }
    
        (fd_out, fd_err)
    }
    
    fn _get_dupped_stdout_fd(cmd: &Command, cl: &CommandLine) -> RawFd {
        // if with pipeline, e.g. `history | grep foo`, then we don't need to
        // dup stdout since it is running in a sperated process, whose fd can
        // be dropped after use.
        if cl.with_pipeline() {
            return 1;
        }
    
        let (_fd_out, _fd_err) = _get_std_fds(&cmd.redirects_to);
        if let Some(fd) = _fd_err {
            unsafe { libc::close(fd); }
        }
        if let Some(fd) = _fd_out {
            fd
        } else {
            let fd = unsafe { libc::dup(1) };
            if fd == -1 {
                let eno = errno();
                println_stderr!("cicada: dup: {}", eno);
            }
            fd
        }
    }
    
    fn _get_dupped_stderr_fd(cmd: &Command, cl: &CommandLine) -> RawFd {
        if cl.with_pipeline() {
            return 2;
        }
    
        let (_fd_out, _fd_err) = _get_std_fds(&cmd.redirects_to);
        if let Some(fd) = _fd_out {
            unsafe { libc::close(fd); }
        }
    
        if let Some(fd) = _fd_err {
            fd
        } else {
            let fd = unsafe { libc::dup(2) };
            if fd == -1 {
                let eno = errno();
                println_stderr!("cicada: dup: {}", eno);
            }
            fd
        }
    }

    fn in_env() -> bool { env::var("VIRTUAL_ENV").map_or(false, |x| !x.is_empty()) }
    
    fn get_envs_home() -> String { env::var("VIRTUALENV_HOME").unwrap_or_default() }
    
    fn get_all_venvs() -> Result<Vec<String>, String>
    {
        let home_envs = get_envs_home();
        if home_envs.is_empty() {
            let info = String::from("you need to set VIRTUALENV_HOME to use vox");
            return Err(info);
        }
        if !Path::new(home_envs.as_str()).exists() {
            match fs::create_dir_all(home_envs.as_str()) {
                Ok(_) => {}
                Err(e) => {
                    let info = format!("fs create_dir_all failed: {:?}", e);
                    return Err(info);
                }
            }
        }
    
        let mut venvs = Vec::new();
        let pdir = home_envs.clone();
        if let Ok(list) = fs::read_dir(home_envs) {
            for ent in list.flatten() {
                let ent_name = ent.file_name();
                if let Ok(path) = ent_name.into_string() {
                    let full_path = format!("{}/{}/bin/activate", pdir, path);
                    if !Path::new(full_path.as_str()).exists() {
                        continue;
                    }
                    venvs.push(path);
                }
            }
        }
    
        Ok(venvs)
    }
    
    fn enter_env(sh: &Shell, path: &str) -> String
    {
        if in_env() {
            return "vox: already in env".to_string();
        }
    
        let home_envs = get_envs_home();
        let full_path = format!("{}/{}/bin/activate", home_envs, path);
        if !Path::new(full_path.as_str()).exists() {
            return format!("no such env: {}", full_path);
        }
    
        let path_env = format!("{}/{}", home_envs, path);
        env::set_var("VIRTUAL_ENV", &path_env);
        let path_new = String::from("${VIRTUAL_ENV}/bin:$PATH");
        let mut tokens: types::Tokens = Vec::new();
        tokens.push((String::new(), path_new));
        shell::expand_env(sh, &mut tokens);
        env::set_var("PATH", &tokens[0].1);
        String::new()
    }
    
    fn exit_env(sh: &Shell) -> String
    {
        if !in_env() {
            return String::from("vox: not in an env");
        }
    
        let env_path = match env::var("PATH") {
            Ok(x) => x,
            Err(_) => {
                return String::from("vox: cannot read PATH env");
            }
        };
    
        let mut _tokens: Vec<&str> = env_path.split(':').collect();
        let mut path_virtual_env = String::from("${VIRTUAL_ENV}/bin");
        // shell::extend_env(sh, &mut path_virtual_env);
        let mut tokens: types::Tokens = Vec::new();
        tokens.push((String::new(), path_virtual_env));
        shell::expand_env(sh, &mut tokens);
        path_virtual_env = tokens[0].1.clone();
        _tokens
            .iter()
            .position(|&n| n == path_virtual_env)
            .map(|e| _tokens.remove(e));
        let env_path_new = _tokens.join(":");
        env::set_var("PATH", &env_path_new);
        env::set_var("VIRTUAL_ENV", "");
    
        String::new()
    }
}

pub mod scripting
{
    use ::
    {
        *,
    };
    /*
    use std::fs::File;
    use std::io::{Read, Write, ErrorKind};
    use std::path::Path;
    
    use pest::iterators::Pair;
    use regex::{Regex, RegexBuilder};
    
    use crate::execute;
    use crate::libs;
    use crate::parsers;
    use crate::shell;
    use crate::types;
    use crate::types::CommandResult;
    */
    pub fn run_script(sh: &mut shell::Shell, args: &Vec<String>) -> i32 {
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
    
    pub fn run_lines(sh: &mut shell::Shell,
                     lines: &str,
                     args: &Vec<String>,
                     capture: bool) -> Vec<CommandResult> {
        let mut cr_list = Vec::new();
        match parsers::locust::parse_lines(lines) {
            Ok(pairs_exp) => {
                for pair in pairs_exp {
                    let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, false, capture);
                    cr_list.append(&mut _cr_list);
                }
            }
            Err(e) => {
                println_stderr!("syntax error: {:?}", e);
                return cr_list;
            }
        }
        cr_list
    }
    
    fn expand_args(line: &str, args: &[String]) -> String {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        parsers::parser_line::tokens_to_line(&tokens)
    }
    
    fn expand_line_to_toknes(line: &str,
                             args: &[String],
                             sh: &mut shell::Shell) -> types::Tokens {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        shell::do_expansion(sh, &mut tokens);
        tokens
    }
    
    fn is_args_in_token(token: &str) -> bool {
        libs::re::re_contains(token, r"\$\{?[0-9@]+\}?")
    }
    
    fn expand_args_for_single_token(token: &str, args: &[String]) -> String {
        let re = Regex::new(r"^(.*?)\$\{?([0-9]+|@)\}?(.*)$").unwrap();
        if !re.is_match(token) {
            return token.to_string();
        }
    
        let mut result = String::new();
        let mut _token = token.to_string();
        let mut _head = String::new();
        let mut _output = String::new();
        let mut _tail = String::new();
        loop {
            if !re.is_match(&_token) {
                if !_token.is_empty() {
                    result.push_str(&_token);
                }
                break;
            }
            for cap in re.captures_iter(&_token) {
                _head = cap[1].to_string();
                _tail = cap[3].to_string();
                let _key = cap[2].to_string();
                if _key == "@" {
                    result.push_str(format!("{}{}", _head, args[1..].join(" ")).as_str());
                } else if let Ok(arg_idx) = _key.parse::<usize>() {
                    if arg_idx < args.len() {
                        result.push_str(format!("{}{}", _head, args[arg_idx]).as_str());
                    } else {
                        result.push_str(&_head);
                    }
                } else {
                    result.push_str(&_head);
                }
            }
    
            if _tail.is_empty() {
                break;
            }
            _token = _tail.clone();
        }
        result
    }
    
    fn expand_args_in_tokens(tokens: &mut types::Tokens, args: &[String]) {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
    
        for (sep, token) in tokens.iter() {
            if sep == "`" || sep == "'" || !is_args_in_token(token) {
                idx += 1;
                continue;
            }
    
            let _token = expand_args_for_single_token(token, args);
            buff.push((idx, _token));
            idx += 1;
        }
    
        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }
    
    fn run_exp_test_br(sh: &mut shell::Shell,
                       pair_br: Pair<parsers::locust::Rule>,
                       args: &Vec<String>,
                       in_loop: bool,
                       capture: bool) -> (Vec<CommandResult>, bool, bool, bool) {
        let mut cr_list = Vec::new();
        let pairs = pair_br.into_inner();
        let mut test_pass = false;
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::IF_HEAD ||
                    rule == parsers::locust::Rule::IF_ELSEIF_HEAD ||
                    rule == parsers::locust::Rule::WHILE_HEAD {
                let pairs_test: Vec<Pair<parsers::locust::Rule>> =
                    pair.into_inner().collect();
                let pair_test = &pairs_test[0];
                let line = pair_test.as_str().trim();
                let line_new = expand_args(line, &args[1..]);
                let mut _cr_list = execute::run_command_line(sh, &line_new, true, capture);
                if let Some(last) = _cr_list.last() {
                    if last.status == 0 {
                        test_pass = true;
                    }
                }
                continue;
            }
    
            if rule == parsers::locust::Rule::KW_ELSE {
                test_pass = true;
                continue;
            }
    
            if rule == parsers::locust::Rule::EXP_BODY {
                if !test_pass {
                    return (cr_list, false, false, false);
                }
                let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);
                // branch executed successfully
                return (cr_list, true, _cont, _brk);
            }
    
            unreachable!();
        }
        (cr_list, test_pass, false, false)
    }
    
    fn run_exp_if(sh: &mut shell::Shell,
                  pair_if: Pair<parsers::locust::Rule>,
                  args: &Vec<String>,
                  in_loop: bool,
                  capture: bool) -> (Vec<CommandResult>, bool, bool) {
        let mut cr_list = Vec::new();
        let pairs = pair_if.into_inner();
        let mut met_continue = false;
        let mut met_break = false;
        for pair in pairs {
            let (mut _cr_list, passed, _cont, _brk) = run_exp_test_br(sh, pair, args, in_loop, capture);
            met_continue = _cont;
            met_break = _brk;
            cr_list.append(&mut _cr_list);
            // break at first successful branch
            if passed {
                break;
            }
        }
        (cr_list, met_continue, met_break)
    }
    
    fn get_for_result_from_init(sh: &mut shell::Shell,
                                pair_init: Pair<parsers::locust::Rule>,
                                args: &[String]) -> Vec<String> {
        let mut result: Vec<String> = Vec::new();
        let pairs = pair_init.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::TEST {
                let line = pair.as_str().trim();
                let tokens = expand_line_to_toknes(line, &args[1..], sh);
                for (sep, token) in tokens {
                    if sep.is_empty() {
                        for x in token.split_whitespace() {
                            result.push(x.to_string());
                        }
                    } else {
                        result.push(token.clone());
                    }
                }
            }
        }
        result
    }
    
    fn get_for_result_list(sh: &mut shell::Shell,
                           pair_head: Pair<parsers::locust::Rule>,
                           args: &[String]) -> Vec<String> {
        let pairs = pair_head.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT {
                return get_for_result_from_init(sh, pair, args);
            }
        }
        Vec::new()
    }
    
    fn get_for_var_name(pair_head: Pair<parsers::locust::Rule>) -> String {
        let pairs = pair_head.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT {
                let pairs_init = pair.into_inner();
                for pair_init in pairs_init {
                    let rule_init = pair_init.as_rule();
                    if rule_init == parsers::locust::Rule::FOR_VAR {
                        let line = pair_init.as_str().trim();
                        return line.to_string();
                    }
                }
            }
        }
        String::new()
    }
    
    fn run_exp_for(sh: &mut shell::Shell,
                   pair_for: Pair<parsers::locust::Rule>,
                   args: &Vec<String>,
                   capture: bool) -> Vec<CommandResult> {
        let mut cr_list = Vec::new();
        let pairs = pair_for.into_inner();
        let mut result_list: Vec<String> = Vec::new();
        let mut var_name: String = String::new();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_HEAD {
                var_name = get_for_var_name(pair.clone());
                result_list = get_for_result_list(sh, pair.clone(), args);
                continue;
            }
            if rule == parsers::locust::Rule::EXP_BODY {
                for value in &result_list {
                    sh.set_env(&var_name, value);
                    let (mut _cr_list, _cont, _brk) = run_exp(
                        sh, pair.clone(), args, true, capture);
                    cr_list.append(&mut _cr_list);
                    if _brk {
                        break;
                    }
                }
            }
        }
        cr_list
    }
    
    fn run_exp_while(sh: &mut shell::Shell,
                     pair_while: Pair<parsers::locust::Rule>,
                     args: &Vec<String>,
                     capture: bool) -> Vec<CommandResult> {
        let mut cr_list = Vec::new();
        loop {
            let (mut _cr_list, passed, _cont, _brk) = run_exp_test_br(sh, pair_while.clone(), args, true, capture);
            cr_list.append(&mut _cr_list);
            if !passed || _brk {
                break;
            }
        }
        cr_list
    }
    
    fn run_exp(sh: &mut shell::Shell,
               pair_in: Pair<parsers::locust::Rule>,
               args: &Vec<String>,
               in_loop: bool,
               capture: bool) -> (Vec<CommandResult>, bool, bool) {
        let mut cr_list = Vec::new();
        let pairs = pair_in.into_inner();
        for pair in pairs {
            let line = pair.as_str().trim();
            if line.is_empty() {
                continue;
            }
    
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::CMD {
                if line == "continue" {
                    if in_loop {
                        return (cr_list, true, false);
                    } else {
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
            } else if rule == parsers::locust::Rule::EXP_IF {
                let (mut _cr_list, _cont, _brk) = run_exp_if(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);
                if _cont {
                    return (cr_list, true, false);
                }
                if _brk {
                    return (cr_list, false, true);
                }
            } else if rule == parsers::locust::Rule::EXP_FOR {
                let mut _cr_list = run_exp_for(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            } else if rule == parsers::locust::Rule::EXP_WHILE {
                let mut _cr_list = run_exp_while(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            }
        }
        (cr_list, false, false)
    }
}

pub mod shell
{
    use ::
    {
        *,
    };
    /*
    use errno::errno;
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

pub mod signals
{
    use ::
    {
        *,
    };
    /*
    use errno::{errno, set_errno};
    use nix::sys::signal;
    use nix::sys::wait::{WaitPidFlag as WF, WaitStatus as WS, waitpid};
    use nix::unistd::Pid;
    use std::sync::Mutex;
    use std::collections::{HashMap, HashSet};
    */
    lazy_static! {
        static ref REAP_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
        static ref STOP_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        static ref CONT_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        static ref KILL_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
    }
    
    pub fn killed_map_insert(pid: i32, sig: i32) {
        if let Ok(mut m) = KILL_MAP.try_lock() {
            m.insert(pid, sig);
        }
    }
    
    pub fn killed_map_pop(pid: i32) -> Option<i32> {
        if let Ok(mut m) = KILL_MAP.try_lock() {
            m.remove(&pid)
        } else {
            None
        }
    }
    
    pub fn insert_cont_map(pid: i32) {
        if let Ok(mut m) = CONT_MAP.try_lock() {
            m.insert(pid);
        }
    }
    
    pub fn pop_cont_map(pid: i32) -> bool {
        match CONT_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }
    
    pub fn insert_stopped_map(pid: i32) {
        if let Ok(mut m) = STOP_MAP.try_lock() {
            m.insert(pid);
        }
    }
    
    pub fn pop_stopped_map(pid: i32) -> bool {
        match STOP_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }
    
    pub fn insert_reap_map(pid: i32, status: i32) {
        if let Ok(mut m) = REAP_MAP.try_lock() {
            m.insert(pid, status);
        }
    }
    
    pub fn pop_reap_map(pid: i32) -> Option<i32> {
        match REAP_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => None,
        }
    }
    
    pub fn block_signals() {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_BLOCK, Some(&sigset), None) {
            Ok(_) => {},
            Err(e) => {
                log!("sigprocmask block error: {:?}", e);
            }
        }
    }
    
    pub fn unblock_signals() {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_UNBLOCK, Some(&sigset), None) {
            Ok(_) => {},
            Err(e) => {
                log!("sigprocmask unblock error: {:?}", e);
            }
        }
    }
    
    #[allow(unreachable_patterns)]
    pub extern "C" fn handle_sigchld(_sig: i32) {
        let saved_errno = errno();
        let options = Some(WF::WUNTRACED | WF::WNOHANG | WF::WCONTINUED);
        loop {
            match waitpid(Pid::from_raw(-1), options) {
                Ok(WS::Exited(pid, status)) => {
                    insert_reap_map(i32::from(pid), status);
                }
                Ok(WS::Stopped(pid, _sig)) => {
                    insert_stopped_map(i32::from(pid));
                }
                Ok(WS::Continued(pid)) => {
                    // NOTE: SIGCHLD generated by SIGCONT is not reliable
                    // on Mac (both for signal handler or sync waitpid).
                    insert_cont_map(i32::from(pid));
                }
                Ok(WS::Signaled(pid, sig, _core_dumped)) => {
                    killed_map_insert(i32::from(pid), sig as i32);
                }
                Ok(WS::StillAlive) => {
                    break;
                }
                Ok(_others) => {
                    // log!("sigchld others: {:?}", _others);
                }
                Err(e) => {
                    if e == nix::Error::ECHILD {
                        break;
                    }
    
                    log!("chld waitpid error: {:?}", e);
                    break;
                }
            }
        }
    
        set_errno(saved_errno);
    }
    
    pub fn setup_sigchld_handler() {
        let sigset = signal::SigSet::empty();
        let handler = signal::SigHandler::Handler(handle_sigchld);
        // automatically restart system calls interrupted by this signal handler
        let flags = signal::SaFlags::SA_RESTART;
        let sa = signal::SigAction::new(handler, flags, sigset);
        unsafe {
            match signal::sigaction(signal::SIGCHLD, &sa) {
                Ok(_) => {},
                Err(e) => {
                    log!("sigaction error: {:?}", e);
                }
            }
        }
    }
}

pub mod types
{
    use ::
    {
        *,
    };
    /*
    use regex::Regex;
    use std::collections::{HashMap, HashSet};
    use std::fmt;
    
    use crate::parsers;
    use crate::parsers::parser_line::tokens_to_redirections;
    use crate::shell;
    use crate::libs;
    use crate::tools;
    */
    
    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub struct WaitStatus(i32, i32, i32);
    
    impl WaitStatus {
        pub fn from_exited(pid: i32, status: i32) -> Self {
            WaitStatus(pid, 0, status)
        }
    
        pub fn from_signaled(pid: i32, sig: i32) -> Self {
            WaitStatus(pid, 1, sig)
        }
    
        pub fn from_stopped(pid: i32, sig: i32) -> Self {
            WaitStatus(pid, 2, sig)
        }
    
        pub fn from_continuted(pid: i32) -> Self {
            WaitStatus(pid, 3, 0)
        }
    
        pub fn from_others() -> Self {
            WaitStatus(0, 9, 9)
        }
    
        pub fn from_error(errno: i32) -> Self {
            WaitStatus(0, 255, errno)
        }
    
        pub fn empty() -> Self {
            WaitStatus(0, 0, 0)
        }
    
        pub fn is_error(&self) -> bool {
            self.1 == 255
        }
    
        pub fn is_others(&self) -> bool {
            self.1 == 9
        }
    
        pub fn is_signaled(&self) -> bool {
            self.1 == 1
        }
    
        pub fn get_errno(&self) -> nix::Error {
            nix::Error::from_raw(self.2)
        }
    
        pub fn is_exited(&self) -> bool {
            self.0 != 0 && self.1 == 0
        }
    
        pub fn is_stopped(&self) -> bool {
            self.1 == 2
        }
    
        pub fn is_continued(&self) -> bool {
            self.1 == 3
        }
    
        pub fn get_pid(&self) -> i32 {
            self.0
        }
    
        fn _get_signaled_status(&self) -> i32 {
            self.2 + 128
        }
    
        pub fn get_signal(&self) -> i32 {
            self.2
        }
    
        pub fn get_name(&self) -> String {
            if self.is_exited() {
                "Exited".to_string()
            } else if self.is_stopped() {
                "Stopped".to_string()
            } else if self.is_continued() {
                "Continued".to_string()
            } else if self.is_signaled() {
                "Signaled".to_string()
            } else if self.is_others() {
                "Others".to_string()
            } else if self.is_error() {
                "Error".to_string()
            } else {
                format!("unknown: {}", self.2)
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
    
    impl fmt::Debug for WaitStatus {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    pub struct LineInfo {
        // e.g. echo 'foo
        // is not a completed line, need to turn to multiple-line mode.
        pub tokens: Tokens,
        pub is_complete: bool,
    }
    
    impl LineInfo {
        pub fn new(tokens: Tokens) -> Self {
            LineInfo { tokens, is_complete: true }
        }
    }
    
    ///
    /// command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    /// Command {
    ///     tokens: [("", "ls"), ("", "-G"), ("\'", "foo bar")],
    ///     redirects_to: [
    ///         ("2", ">", "&1"),
    ///         ("1", ">", "/dev/null"),
    ///     ],
    ///     redirect_from: Some(("<", "one-file")),
    /// }
    ///
    #[derive(Debug)]
    pub struct Command {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }
    
    #[derive(Debug)]
    pub struct CommandLine {
        pub line: String,
        pub commands: Vec<Command>,
        pub envs: HashMap<String, String>,
        pub background: bool,
    }
    
    impl Command {
        pub fn from_tokens(tokens: Tokens) -> Result<Command, String> {
            let mut tokens_new = tokens.clone();
            let mut redirects_from_type = String::new();
            let mut redirects_from_value = String::new();
            let mut has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");
    
            let mut len = tokens_new.len();
            while has_redirect_from {
                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<") {
                    redirects_from_type = "<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }
                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<<<") {
                    redirects_from_type = "<<<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }
    
                has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");
            }
    
            let tokens_final;
            let redirects_to;
            match tokens_to_redirections(&tokens_new) {
                Ok((_tokens, _redirects_to)) => {
                    tokens_final = _tokens;
                    redirects_to = _redirects_to;
                }
                Err(e) => {
                    return Err(e);
                }
            }
    
            let redirect_from = if redirects_from_type.is_empty() {
                None
            } else {
                Some((redirects_from_type, redirects_from_value))
            };
    
            Ok(Command{
                tokens: tokens_final,
                redirects_to,
                redirect_from,
            })
        }
    
        pub fn has_redirect_from(&self) -> bool {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<"
        }
    
        pub fn has_here_string(&self) -> bool {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<<<"
        }
    
        pub fn is_builtin(&self) -> bool {
            tools::is_builtin(&self.tokens[0].1)
        }
    }
    
    #[derive(Debug, Clone, Default)]
    pub struct Job {
        pub cmd: String,
        pub id: i32,
        pub gid: i32,
        pub pids: Vec<i32>,
        pub pids_stopped: HashSet<i32>,
        pub status: String,
        pub is_bg: bool,
    }
    
    impl Job {
        pub fn all_members_stopped(&self) -> bool {
            for pid in &self.pids {
                if !self.pids_stopped.contains(pid) {
                    return false;
                }
            }
            true
        }
    
        pub fn all_members_running(&self) -> bool {
            self.pids_stopped.is_empty()
        }
    }
    
    #[allow(dead_code)]
    #[derive(Clone, Debug, Default)]
    pub struct CommandResult {
        pub gid: i32,
        pub status: i32,
        pub stdout: String,
        pub stderr: String,
    }
    
    impl CommandResult {
        pub fn new() -> CommandResult {
            CommandResult {
                gid: 0,
                status: 0,
                stdout: String::new(),
                stderr: String::new(),
            }
        }
    
        pub fn from_status(gid: i32, status: i32) -> CommandResult {
            CommandResult {
                gid,
                status,
                stdout: String::new(),
                stderr: String::new(),
            }
        }
    
        pub fn error() -> CommandResult {
            CommandResult {
                gid: 0,
                status: 1,
                stdout: String::new(),
                stderr: String::new(),
            }
        }
    }
    
    #[allow(dead_code)]
    #[derive(Clone, Debug, Default)]
    pub struct CommandOptions {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }
    
    fn split_tokens_by_pipes(tokens: &[Token]) -> Vec<Tokens> {
        let mut cmd = Vec::new();
        let mut cmds = Vec::new();
        for token in tokens {
            let sep = &token.0;
            let value = &token.1;
            if sep.is_empty() && value == "|" {
                if cmd.is_empty() {
                    return Vec::new();
                }
                cmds.push(cmd.clone());
                cmd = Vec::new();
            } else {
                cmd.push(token.clone());
            }
        }
        if cmd.is_empty() {
            return Vec::new();
        }
        cmds.push(cmd.clone());
        cmds
    }
    
    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String> {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !libs::re::re_contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") {
                break;
            }
    
            for cap in re.captures_iter(text) {
                let name = cap[1].to_string();
                let value = parsers::parser_line::unquote(&cap[2]);
                envs.insert(name, value);
            }
    
            n += 1;
        }
        if n > 0 {
            tokens.drain(0..n);
        }
        envs
    }
    
    impl CommandLine {
        pub fn from_line(line: &str, sh: &mut shell::Shell) -> Result<CommandLine, String> {
            let linfo = parsers::parser_line::parse_line(line);
            let mut tokens = linfo.tokens;
            shell::do_expansion(sh, &mut tokens);
            let envs = drain_env_tokens(&mut tokens);
    
            let mut background = false;
            let len = tokens.len();
            if len > 1 && tokens[len - 1].1 == "&" {
                background = true;
                tokens.pop();
            }
    
            let mut commands = Vec::new();
            for sub_tokens in split_tokens_by_pipes(&tokens) {
                match Command::from_tokens(sub_tokens) {
                    Ok(c) => {
                        commands.push(c);
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
    
            Ok(CommandLine{
                line: line.to_string(),
                commands,
                envs,
                background,
            })
        }
    
        pub fn is_empty(&self) -> bool {
            self.commands.is_empty()
        }
    
        pub fn with_pipeline(&self) -> bool {
            self.commands.len() > 1
        }
    
        pub fn is_single_and_builtin(&self) -> bool {
            self.commands.len() == 1 && self.commands[0].is_builtin()
        }
    }
}
// #[allow(clippy::cast_lossless)]
fn main() {
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);

        // ignore SIGTSTP (ctrl-Z) for the shell itself
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);
    }

    tools::init_path_env();

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
///////////////////////////////////////////////////////////////////////////////////////////////////
// 22838
