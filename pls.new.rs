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
/**/
pub use std::{ * };

/**/
#[macro_use] extern crate lazy_static;
/**/
extern crate clap;
extern crate getrandom;
extern crate libc;
extern crate nix;
extern crate regex;
extern crate time;
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
    use ::
    {
        time::OffsetDateTime,
        *,
    };
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
/**/
pub mod builtins
{
    pub mod alias
    {
        use ::
        {
            regex::{ Regex },
            types::{ Command, CommandLine, CommandResult },
            *,
        };
        
        pub fn run( sh:&mut shell::Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            if tokens.len() == 1
            { return show_alias_list( sh, cmd, cl, capture ); }

            if tokens.len() > 2
            {
                let info = "alias syntax error:usage:alias foo='echo foo'";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let input = &tokens[1].1;
            let re_single_read = Regex::new( r"^[a-zA-Z0-9_\.-]+$" ).unwrap();
            if re_single_read.is_match( input ) { return show_single_alias( sh, input, cmd, cl, capture ); }

            let re_to_add = Regex::new( r"^( [a-zA-Z0-9_\.-]+ )=( .* )$" ).unwrap();
            for cap in re_to_add.captures_iter( input )
            {
                let name = str::unquote( &cap[1] );
                let value = if cap[2].starts_with( '"' ) || cap[2].starts_with( '\'' )
                { str::unquote( &cap[2] ) }

                else
                { cap[2].to_string() };

                sh.add_alias( name.as_str(), value.as_str() );
            }

            CommandResult::new()
        }
        
        pub fn show_alias_list( sh:&shell::Shell, cmd:&Command, cl:&CommandLine, capture:bool ) -> CommandResult
        {
            let mut lines = Vec::new();
            for ( name, value ) in sh.get_alias_list()
            {
                let line = format!( "alias {}='{}'", name, value );
                lines.push( line );
            }

            let buffer = lines.join( "\n" );
            let mut cr = CommandResult::new();
            print_stdout_with_capture( &buffer, &mut cr, cl, cmd, capture );
            cr
        }
        
        pub fn show_single_alias( sh:&shell::Shell, named:&str, cmd:&Command, cl:&CommandLine, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            if let Some( content ) = sh.get_alias_content( named )
            {
                let info = format!( "alias {}='{}'", named, content );
                print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
            }
            else
            {
                let info = format!( "pls:alias:{}:not found", named );
                print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
            }
            cr
        }
    }
    
    pub mod bg
    {
        use ::
        {
            c::{ job },
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty()
            {
                let info = "pls:bg:no job found";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let mut job_id = -1;
            if tokens.len() == 1
            {
                if let Some( ( gid, _ ) ) = sh.jobs.iter().next()
                { job_id = *gid; }
            }

            if tokens.len() >= 2
            {
                let mut job_str = tokens[1].1.clone();
                if job_str.starts_with( "%" )
                {
                    job_str = job_str.trim_start_matches( '%' ).to_string();
                }

                match job_str.parse::<i32>()
                {
                    Ok( n ) => job_id = n,
                    Err( _ ) =>
                    {
                        let info = "pls:bg:invalid job id";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }
            
            if job_id == -1
            {
                let info = "pls:bg:not such job";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let gid:i32;

            {
                let mut result = sh.get_job_by_id( job_id );
                if result.is_none() {
                    result = sh.get_job_by_gid( job_id );
                }

                match result
                {
                    Some( job ) =>
                    unsafe 
                    {
                        libc::killpg( job.gid, libc::SIGCONT );
                        gid = job.gid;
                        if job.status == "Running"
                        {
                            let info = format!( "pls:bg:job {} already in background", job.id );
                            print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                            return cr;
                        }

                        let info_cmd = format!( "[{}]  {} &", job.id, job.cmd );
                        print_stderr_with_capture( &info_cmd, &mut cr, cl, cmd, capture );
                        cr.status = 0;
                    }
                    
                    None =>
                    {
                        let info = "pls:bg:not such job";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }

            c::job::mark_job_as_running( sh, gid, true );
            cr
        }
    }
    
    pub mod cd
    {
        use ::
        {
            path::{ Path },
            types::{Command, CommandLine, CommandResult},
            *,
        };

        pub fn run( sh:&mut shell::Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();
            let args = parsers::line::tokens_to_args( &tokens );

            if args.len() > 2 {
                let info = "pls:cd:too many argument";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let str_current_dir = get::current_directory();

            let mut dir_to = if args.len() == 1 {
                let home = get::user_home();
                home.to_string()
            } else {
                args[1..].join( "" )
            };

            if dir_to == "-" {
                if sh.previous_dir.is_empty() {
                    let info = "no previous dir";
                    print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                    return cr;
                }
                dir_to = sh.previous_dir.clone();
            } else if !dir_to.starts_with( '/' ) {
                dir_to = format!( "{}/{}", str_current_dir, dir_to );
            }

            if !Path::new( &dir_to ).exists()
            {
                let info = format!( "pls:cd:{}:No such file or directory", &args[1] );
                print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                return cr;
            }

            match Path::new( &dir_to ).canonicalize()
            {
                Ok( p ) =>
        {                   dir_to = p.as_path().to_string_lossy().to_string();
                }
                Err( e ) =>
        {                   let info = format!( "pls:cd:error:{}", e );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }
            }

            match env::set_current_dir( &dir_to )
            {
                Ok( _ ) =>
        {                   sh.current_dir = dir_to.clone();
                    if str_current_dir != dir_to {
                        sh.previous_dir = str_current_dir.clone();
                        env::set_var( "PWD", &sh.current_dir );
                    };
                    cr.status = 0;
                    cr
                }
                Err( e ) =>
        {                   let info = format!( "pls:cd:{}", e );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    cr
                }
            }
        }
    }
    
    pub mod cinfo
    {
        use ::
        {
            shell::{ Shell },
            types::{ Command, CommandLine, CommandResult }, 
            *,
        };

        pub fn run( _sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut info = vec![];
            const VERSION:&str = "0.0.0"; //env!( "CARGO_PKG_VERSION" );
            info.push( ( "version", VERSION ) );

            let os_name = get::os_name();
            info.push( ( "os-name", &os_name ) );

            let hfile = get::history_file();
            info.push( ( "history-file", &hfile ) );

            let rcf = rc::file::get_rc_file();
            info.push( ( "rc-file", &rcf ) );

            let git_hash = std::env::var( "GIT_HASH" ); // env!( "GIT_HASH" );
            if !git_hash.is_empty()
            {
                //info.push( ( "git-commit", env!( "GIT_HASH" ) ) );
                info.push( ( "git-commit", env::var( "GIT_HASH" ) ) );
            }

            let git_branch = env::var( "GIT_BRANCH" ); //env!( "GIT_BRANCH" );
            let mut branch = String::new();
            if !git_branch.is_empty()
            {
                branch.push_str( git_branch );
                let git_status = env::var( "GIT_STATUS" ); //env!( "GIT_STATUS" );
                if git_status != "0" { branch.push_str( " ( dirty )" ); }
                info.push( ( "git-branch", &branch ) );
            }

            //info.push( ( "built-with", env!( "BUILD_RUSTC_VERSION" ) ) );
            info.push( ( "built-with", env::var( "BUILD_RUSTC_VERSION" ) ) );
            //info.push( ( "built-at", env!( "BUILD_DATE" ) ) );
            info.push( ( "built-with", env::var( "BUILD_DATE" ) ) );

            let mut lines = Vec::new();
            for ( k, v ) in &info {
                // longest key above is 12-char length
                lines.push( format!( "{:>12}:{}", k, v ) );
            }
            let buffer = lines.join( "\n" );
            let mut cr = CommandResult::new();
            print_stdout_with_capture( &buffer, &mut cr, cl, cmd, capture );
            cr
        }
    }
    
    pub mod exec
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *,
        };
        /**/
        pub fn run( _sh:&Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            let args = parsers::line::tokens_to_args( &tokens );
            let len = args.len();
            if len == 1
            {
                print_stderr_with_capture( "invalid usage", &mut cr, cl, cmd, capture );
                return cr;
            }

            let mut _cmd = execute::Command::new( &args[1] );
            let err = _cmd.args( &args[2..len] ).exec();
            let info = format!( "pls:exec:{}", err );
            print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
            cr
        }
    }
    
    pub mod exit
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };
        /**/
        pub fn run( sh:&Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            if tokens.len() > 2 {
                let info = "pls:exit:too many arguments";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            if tokens.len() == 2 {
                let _code = &tokens[1].1;
                match _code.parse::<i32>() {
                    Ok( x ) =>
        {                       process::exit( x );
                    }
                    Err( _ ) =>
        {                       let info = format!( "pls:exit:{}:numeric argument required", _code );
                        print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                        process::exit( 255 );
                    }
                }
            }

            for ( _i, job ) in sh.jobs.iter() {
                if !job.cmd.starts_with( "nohup " ) {
                    let mut info = String::new();
                    info.push_str( "There are background jobs." );
                    info.push_str( "Run `jobs` to see details; `exit 1` to force quit." );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }
            }

            process::exit( 0 );
            cr
        }
    }
    
    pub mod export
    {
        use ::
        {
            regex::{ Regex },
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *,
        };

        pub fn run( _sh:&Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            let re_name_ptn = Regex::new( r"^( [a-zA-Z_][a-zA-Z0-9_]* )=( .* )$" ).unwrap();
            for ( _, text ) in tokens.iter() {
                if text == "export" {
                    continue;
                }

                if !is::environment( text ) {
                    let mut info = String::new();
                    info.push_str( "export:invalid command\n" );
                    info.push_str( "usage:export XXX=YYY" );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }

                if !re_name_ptn.is_match( text ) {
                    let mut info = String::new();
                    info.push_str( "export:invalid command\n" );
                    info.push_str( "usage:export XXX=YYY ZZ=123" );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }

                for cap in re_name_ptn.captures_iter( text ) {
                    let name = cap[1].to_string();
                    let token = parsers::line::unquote( &cap[2] );
                    let value = expand::home( &token );
                    env::set_var( name, &value );
                }
            }
            cr
        }
    }
    
    pub mod fg
    {
        use ::
        {
            c::{ job },
            shell::{ self, Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty()
            {
                let info = "pls:fg:no job found";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let mut job_id = -1;
            if tokens.len() == 1 {
                if let Some( ( gid, _ ) ) = sh.jobs.iter().next() {
                    job_id = *gid;
                }
            }

            if tokens.len() >= 2 {
                let mut job_str = tokens[1].1.clone();
                if job_str.starts_with( "%" ) {
                    job_str = job_str.trim_start_matches( '%' ).to_string();
                }

                match job_str.parse::<i32>() {
                    Ok( n ) => job_id = n,
                    Err( _ ) =>
        {                       let info = "pls:fg:invalid job id";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }

            if job_id == -1 {
                let info = "pls:not job id found";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let gid:i32;
            let pid_list:Vec<i32>;

            {
                let mut result = sh.get_job_by_id( job_id );
                // fall back to find job by using prcess group id
                if result.is_none() {
                    result = sh.get_job_by_gid( job_id );
                }

                match result {
                    Some( job ) =>
        {                       print_stderr_with_capture( &job.cmd, &mut cr, cl, cmd, capture );
                        cr.status = 0;

                        unsafe {
                            if !shell::give_terminal_to( job.gid ) {
                                return CommandResult::error();
                            }

                            libc::killpg( job.gid, libc::SIGCONT );
                            pid_list = job.pids.clone();
                            gid = job.gid;
                        }
                    }
                    None =>
        {                       let info = "pls:fg:no such job";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }

            unsafe {
                c::job::mark_job_as_running( sh, gid, false );

                let cr = c::job::wait_fg_job( sh, gid, &pid_list );

                let gid_shell = libc::getpgid( 0 );
                if !shell::give_terminal_to( gid_shell ) {
                    //log!( "failed to give term to back to shell :{}", gid_shell );
                }

                cr
            }
        }
    }
    
    pub mod history
    {
        use ::
        {
            c::{ time }, 
            path::{ Path },
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };


        /*
        #[derive( Debug, StructOpt )]
        #[structopt( name = "history", about = "History in pls shell" )]
        struct OptMain {
            #[structopt( short, long, help = "For current session only" )]
            session:bool,

            #[structopt( short, long, help = "Search old items first" )]
            asc:bool,

            #[structopt( short, long, help = "For current directory only" )]
            pwd:bool,

            #[structopt( short, long, help = "Only show ROWID" )]
            only_id:bool,

            #[structopt( short, long, help = "Do not show ROWID" )]
            no_id:bool,

            #[structopt( short="d", long, help = "Show date" )]
            show_date:bool,

            #[structopt( short, long, default_value = "20" )]
            limit:i32,

            #[structopt( name = "PATTERN", default_value = "", help = "You can use % to match anything" )]
            pattern:String,

            #[structopt( subcommand )]
            cmd:Option<SubCommand>
        }
        */
        #[derive( Debug )]
        pub struct OptMain();
        /*
        #[derive( StructOpt, Debug )]
        enum SubCommand {
            #[structopt( about="Add new item into history" )]
            Add {
                #[structopt( short="t", long, help = "Specify a timestamp for the new item" )]
                timestamp:Option<f64>,

                #[structopt( name="INPUT", help = "input to be added into history" )]
                input:String,
            },
            #[structopt( about="Delete item from history" )]
            Delete {
                #[structopt( name="ROWID", help = "Row IDs of item to delete" )]
                rowid:Vec<usize>,
            }
        }
        */
        #[derive( Debug )]
        pub enum SubCommand
        {
            Add,
            Delete,
        }

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let hfile = get::history_file();
            let path = Path::new( hfile.as_str() );
            if !path.exists()
            {
                let info = "no history file";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }
            let connection = match Connection::open( &hfile )
            {
                Ok( x ) => x,
                Err( e ) =>
        {                   let info = format!( "history:sqlite error:{:?}", e );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }
            };

            let tokens = cmd.tokens.clone();
            let args = parsers::line::tokens_to_args( &tokens );

            let show_usage = args.len() > 1 && ( args[1] == "-h" || args[1] == "--help" );
            let opt = OptMain::from_iter_safe( args );
            match opt
            {
                Ok( opt ) =>
        {                   match opt.cmd {
                        Some( SubCommand::Delete {rowid:rowids} ) =>
        {                           let mut _count = 0;
                            for rowid in rowids {
                                let _deleted = delete_history_item( &connection, rowid );
                                if _deleted {
                                    _count += 1;
                                }
                            }
                            if _count > 0 {
                                let info = format!( "deleted {} items", _count );
                                print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                            }
                            cr
                        }
                        Some( SubCommand::Add {timestamp:ts, input} ) =>
        {                           let ts = ts.unwrap_or( 0 as f64 );
                            add_history( sh, ts, &input );
                            cr
                        }
                        None =>
        {                           let ( str_out, str_err ) = list_current_history( sh, &connection, &opt );
                            if !str_out.is_empty() {
                                print_stdout_with_capture( &str_out, &mut cr, cl, cmd, capture );
                            }
                            if !str_err.is_empty() {
                                print_stderr_with_capture( &str_err, &mut cr, cl, cmd, capture );
                            }
                            cr
                        }
                    }
                }
                Err( e ) =>
        {                   let info = format!( "{}", e );
                    if show_usage {
                        print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                        cr.status = 0;
                    } else {
                        print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                        cr.status = 1;
                    }
                    cr
                }
            }
        }

        pub fn add_history( sh:&Shell, ts:f64, input:&str )
        {
            let ( tsb, tse ) = ( ts, ts + 1.0 );
            history::add_raw( sh, input, 0, tsb, tse );
        }

        pub fn list_current_history( sh:&Shell, connection:&Connection, opt:&OptMain ) -> ( String, String )
        {
            let mut result_stderr = String::new();
            let result_stdout = String::new();

            let history_table = get::history_table();
            let mut sql = format!( "SELECT ROWID, inp, tsb FROM {} WHERE ROWID > 0",
                                  history_table );
            if !opt.pattern.is_empty() {
                sql = format!( "{} AND inp LIKE '%{}%'", sql, opt.pattern )
            }
            if opt.session {
                sql = format!( "{} AND sessionid = '{}'", sql, sh.session_id )
            }
            if opt.pwd {
                sql = format!( "{} AND info like '%dir:{}|%'", sql, sh.current_dir )
            }

            if opt.asc {
                sql = format!( "{} ORDER BY tsb", sql );
            } else {
                sql = format!( "{} order by tsb desc", sql );
            };
            sql = format!( "{} limit {} ", sql, opt.limit );

            let mut stmt = match connection.prepare( &sql )
            {
                Ok( x ) => x,
                Err( e ) =>
        {                   let info = format!( "history:prepare select error:{:?}", e );
                    result_stderr.push_str( &info );
                    return ( result_stdout, result_stderr );
                }
            };

            let mut rows = match stmt.query( [] )
            {
                Ok( x ) => x,
                Err( e ) =>
        {                   let info = format!( "history:query error:{:?}", e );
                    result_stderr.push_str( &info );
                    return ( result_stdout, result_stderr );
                }
            };

            let mut lines = Vec::new();
            loop {
                match rows.next() {
                    Ok( _rows ) =>
        {                       if let Some( row ) = _rows {
                            let row_id:i32 = match row.get( 0 ) {
                                Ok( x ) => x,
                                Err( e ) =>
        {                                   let info = format!( "history:error:{:?}", e );
                                    result_stderr.push_str( &info );
                                    return ( result_stdout, result_stderr );
                                }
                            };
                            let inp:String = match row.get( 1 ) {
                                Ok( x ) => x,
                                Err( e ) =>
        {                                   let info = format!( "history:error:{:?}", e );
                                    result_stderr.push_str( &info );
                                    return ( result_stdout, result_stderr );
                                }
                            };

                            if opt.no_id {
                                lines.push( inp.to_string() );
                            } else if opt.only_id {
                                lines.push( row_id.to_string() );
                            } else if opt.show_date {
                                let tsb:f64 = match row.get( 2 ) {
                                    Ok( x ) => x,
                                    Err( e ) =>
        {                                       let info = format!( "history:error:{:?}", e );
                                        result_stderr.push_str( &info );
                                        return ( result_stdout, result_stderr );
                                    }
                                };
                                let dt = c::time::DateTime::from_timestamp( tsb );
                                lines.push( format!( "{}:{}:{}", row_id, dt, inp ) );
                            } else {
                                lines.push( format!( "{}:{}", row_id, inp ) );
                            }
                        } else {
                            break;
                        }
                    }
                    Err( e ) =>
        {                       let info = format!( "history:rows next error:{:?}", e );
                        result_stderr.push_str( &info );
                        return ( result_stdout, result_stderr );
                    }
                }
            }

            let buffer = lines.join( "\n" );

            ( buffer, result_stderr )
        }

        fn delete_history_item( connection:&Connection, rowid:usize ) -> bool
        {
            let history_table = get::history_table();
            let sql = format!( "DELETE from {} where rowid = {}", history_table, rowid );
            match connection.execute( &sql, [] )
            {
                Ok( _ ) => true,
                Err( e ) =>
                {
                    //log!( "history:error when delete:{:?}", e );
                    false
                }
            }
        }
    }
    
    pub mod jobs
    {
        use ::
        {
            c::{ job },
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            if sh.jobs.is_empty() {
                return cr;
            }

            // update status of jobs if any
            c::job::try_wait_bg_jobs( sh, false, false );

            let mut lines = Vec::new();
            let jobs = sh.jobs.clone();
            let no_trim = cmd.tokens.len() >= 2 && cmd.tokens[1].1 == "-f";
            for ( _i, job ) in jobs.iter()
            {
                let line = get::job_line( job, !no_trim );
                lines.push( line );
            }
            let buffer = lines.join( "\n" );

            print_stdout_with_capture( &buffer, &mut cr, cl, cmd, capture );
            cr
        }

    }
    /**/
    pub mod read
    {
        use ::
        {
            libs::re::re_contains,
            shell::{ Shell }, 
            types::{ CommandResult, CommandLine, Command },
            *,
        };

        fn _find_invalid_identifier( name_list:&Vec<String> ) -> Option<String>
        {
            for id_ in name_list
            {
                if !re_contains( id_, r"^[a-zA-Z_][a-zA-Z0-9_]*$" ) {
                    return Some( id_.to_string() );
                }
            }
            None
        }

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            let name_list:Vec<String>;
            if tokens.len() <= 1 {
                name_list = vec!["REPLY".to_string( )];
            } else {
                name_list = tokens[1..].iter().map( |x| x.1.clone() ).collect();
                if let Some( id_ ) = _find_invalid_identifier( &name_list ) {
                    let info = format!( "pls:read:`{}':not a valid identifier", id_ );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }
            }

            let mut buffer = String::new();

            if cmd.has_here_string() {
                if let Some( redirect_from ) = &cmd.redirect_from {
                    buffer.push_str( &redirect_from.1 );
                    buffer.push( '\n' );
                }
            } else {
                match io::stdin().read_line( &mut buffer ) {
                    Ok( _ ) => {}
                    Err( e ) =>
        {                       let info = format!( "pls:read:error in reading stdin:{:?}", e );
                        print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }

            let envs = cl.envs.clone();
            let value_list = str::split_into_fields( sh, buffer.trim(), &envs );

            let idx_2rd_last = name_list.len() - 1;
            for i in 0..idx_2rd_last {
                let name = name_list.get( i );
                if name.is_none() {
                    let info = "pls:read:name index error";
                    print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                    return cr;
                }
                let name = name.unwrap();

                let value = value_list.get( i ).unwrap_or( &String::new() ).clone();
                sh.set_env( name, &value );
            }

            let name_last = &name_list[idx_2rd_last];
            let value_left:String = if value_list.len() > idx_2rd_last {
                value_list[idx_2rd_last..].join( " " )
            } else {
                String::new()
            };
            sh.set_env( name_last, &value_left );
            cr
        }
    }
    /**/
    pub mod source
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *,
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parsers::line::tokens_to_args( tokens );

            if args.len() < 2 {
                let info = "pls:source:no file specified";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let status = scripts::run_script( sh, &args );
            cr.status = status;
            cr
        }
    }
    /**/
    pub mod unalias
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = "pls:unalias:syntax error";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_alias( input )
            {
                let info = format!( "pls:unalias:{}:not found", input );
                print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                return cr;
            }
            cr
        }
    }
    /**/
    pub mod vox
    {
        use ::
        {
            path::{ Path },
            shell::{ self, Shell },
            types::{ self, CommandResult, CommandLine, Command },
            *
        };

        fn in_env() -> bool
        {
            env::var( "VIRTUAL_ENV" ).map_or( false, |x| !x.is_empty() )
        }

        fn enter_env( sh:&Shell, path:&str ) -> String
        {
            if in_env() {
                return "vox:already in env".to_string();
            }

            let home_envs = get::virtual_environments_home();
            let full_path = format!( "{}/{}/bin/activate", home_envs, path );
            if !Path::new( full_path.as_str() ).exists() {
                return format!( "no such env:{}", full_path );
            }

            let path_env = format!( "{}/{}", home_envs, path );
            env::set_var( "VIRTUAL_ENV", &path_env );
            let path_new = String::from( "${VIRTUAL_ENV}/bin:$PATH" );
            let mut tokens:types::Tokens = Vec::new();
            tokens.push( ( String::new(), path_new ) );
            expand::environment( sh, &mut tokens );
            env::set_var( "PATH", &tokens[0].1 );
            String::new()
        }

        fn exit_env( sh:&Shell ) -> String
        {
            if !in_env() {
                return String::from( "vox:not in an env" );
            }

            let env_path = match env::var( "PATH" )
            {
                Ok( x ) => x,
                Err( _ ) =>
        {                   return String::from( "vox:cannot read PATH env" );
                }
            };

            let mut _tokens:Vec<&str> = env_path.split( ':' ).collect();
            let mut path_virtual_env = String::from( "${VIRTUAL_ENV}/bin" );
            // shell::extend_env( sh, &mut path_virtual_env );
            let mut tokens:types::Tokens = Vec::new();
            tokens.push( ( String::new(), path_virtual_env ) );
            expand::environment( sh, &mut tokens );
            path_virtual_env = tokens[0].1.clone();
            _tokens
                .iter()
                .position( |&n| n == path_virtual_env )
                .map( |e| _tokens.remove( e ) );
            let env_path_new = _tokens.join( ":" );
            env::set_var( "PATH", &env_path_new );
            env::set_var( "VIRTUAL_ENV", "" );

            String::new()
        }

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            let args = parsers::line::tokens_to_args( &tokens );
            let len = args.len();
            let subcmd = if len > 1 { &args[1] } else { "" };

            if len == 1 || ( len == 2 && subcmd == "ls" ) {
                match get::virtual_environments() {
                    Ok( venvs ) =>
        {                       let info = venvs.join( "\n" );
                        print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                    Err( reason ) =>
        {                       print_stderr_with_capture( &reason, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                }
            }

            if len == 3 && subcmd == "create" {
                let pybin = match env::var( "VIRTUALENV_PYBIN" ) {
                    Ok( x ) => x,
                    Err( _ ) => "python3".to_string(),
                };
                let dir_venv = get::virtual_environments_home();
                let venv_name = args[2].to_string();
                let line = format!( "{} -m venv \"{}/{}\"", pybin, dir_venv, venv_name );
                print_stderr_with_capture( &line, &mut cr, cl, cmd, capture );
                let cr_list = execute::run_command_line( sh, &line, false, false );
                return cr_list[0].clone();
            }

            if len == 3 && subcmd == "enter" {
                let _err = enter_env( sh, args[2].as_str() );
                if !_err.is_empty() {
                    print_stderr_with_capture( &_err, &mut cr, cl, cmd, capture );
                }
                cr
            } else if len == 2 && subcmd == "exit" {
                let _err = exit_env( sh );
                if !_err.is_empty() {
                    print_stderr_with_capture( &_err, &mut cr, cl, cmd, capture );
                }
                cr
            } else {
                let info = "pls:vox:invalid option";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                cr
            }
        }
    }
    /**/
    pub mod ulimit
    {
        use ::
        {
            clap::{Parser, CommandFactory},
            io::{ Error },
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *,
        };

        #[derive( Parser )]
        #[command( name = "ulimit", about = "show / modify shell resource limits" )]
        #[allow( non_snake_case )]
        struct App
        {
            #[arg( short, help = "All current limits are reported." )]
            a:bool,
            #[arg( short, value_name = "NEW VALUE", help = "The maximum number of open file descriptors." )]
            n:Option<Option<u64>>,
            #[arg( short, value_name = "NEW VALUE", help = "The maximum size of core files created." )]
            c:Option<Option<u64>>,
            #[arg( short = 'S', help = "Set a soft limit for the given resource. ( default )" )]
            S:bool,
            #[arg( short = 'H', help = "Set a hard limit for the given resource." )]
            H:bool,
        }

        pub fn run( _sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parsers::line::tokens_to_args( tokens );

            if args.contains( &"--help".to_string() ) || args.contains( &"-h".to_string() ) {
                App::command().print_help().unwrap();
                println!();
                return cr;
            }

            let app = App::parse_from( args );

            if app.H && app.S {
                println!( "pls:ulimit:Cannot both hard and soft." );
                cr.status = 1;
                return cr;
            }

            let mut all_stdout = String::new();
            let mut all_stderr = String::new();

            if app.a {
                report_all( &app, &mut all_stdout, &mut all_stderr );
            } else if handle_limit( app.n, "open_files", app.H, &mut all_stdout, &mut all_stderr )
                || handle_limit( app.c, "core_file_size", app.H, &mut all_stdout, &mut all_stderr ) {
            } else {
                report_all( &app, &mut all_stdout, &mut all_stderr );
            }

            if !all_stdout.is_empty() {
                print_stdout_with_capture( &all_stdout, &mut cr, cl, cmd, capture );
            }
            if !all_stderr.is_empty() {
                print_stderr_with_capture( &all_stderr, &mut cr, cl, cmd, capture );
            }

            cr
        }

        fn set_limit( limit_name:&str, value:u64, for_hard:bool ) -> String
        {
            let limit_id = match limit_name {
                "open_files" => libc::RLIMIT_NOFILE,
                "core_file_size" => libc::RLIMIT_CORE,
                _ => return String::from( "invalid limit name" ),
            };

            let mut rlp = libc::rlimit { rlim_cur:0, rlim_max:0 };

            unsafe {
                if libc::getrlimit( limit_id, &mut rlp ) != 0 {
                    return format!( "pls:ulimit:error getting limit:{}", Error::last_os_error() );
                }
            }

            // to support armv7-linux-gnueabihf & 32-bit musl systems
            if for_hard {
                #[cfg( all( target_pointer_width = "32", target_env = "gnu" ) )]
                { rlp.rlim_max = value as u32; }
                #[cfg( not( all( target_pointer_width = "32", target_env = "gnu" ) ) )]
                { rlp.rlim_max = value; }
            } else {
                #[cfg( all( target_pointer_width = "32", target_env = "gnu" ) )]
                { rlp.rlim_cur = value as u32; }
                #[cfg( not( all( target_pointer_width = "32", target_env = "gnu" ) ) )]
                { rlp.rlim_cur = value; }
            }

            unsafe {
                if libc::setrlimit( limit_id, &rlp ) != 0 {
                    return format!( "pls:ulimit:error setting limit:{}", Error::last_os_error() );
                }
            }

            String::new()
        }

        fn report_all( app:&App, all_stdout:&mut String, all_stderr:&mut String )
        {
            for limit_name in &["open_files", "core_file_size"]
            {
                let ( out, err ) = get::limit( limit_name, false, app.H );
                all_stdout.push_str( &out );
                all_stderr.push_str( &err );
            }
        }

        fn handle_limit
        ( 
            limit_option:Option<Option<u64>>,
            limit_name:&str,
            for_hard:bool,
            all_stdout:&mut String,
            all_stderr:&mut String
        ) -> bool
        {
            match limit_option
            {
                None => false,
                Some( None ) =>
                {
                    let ( out, err ) = get::limit( limit_name, true, for_hard );
                    all_stdout.push_str( &out );
                    all_stderr.push_str( &err );
                    true
                }
                Some( Some( value ) ) =>
                {
                    let err = set_limit( limit_name, value, for_hard );
                    if !err.is_empty() {
                        all_stderr.push_str( &err );
                    }
                    true
                }
            }
        }
    }
    /**/
    pub mod minfd
    {
        use::
        {
            io::{ Write },
            shell::{ Shell },
            types::{CommandResult, CommandLine, Command},
            *
        };

        pub fn run( _sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();

            let fd = nix::fcntl::open( 
                "/dev/null",
                nix::fcntl::OFlag::empty(),
                nix::sys::stat::Mode::empty()
            );
            match fd
            {
                Ok( fd ) =>
                {
                    let info = format!( "{}", fd );
                    print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                    unsafe { libc::close( fd ); }
                }
                Err( e ) =>
                {
                    println_stderr!( "pls:minfd:error:{}", e );
                }
            }

            cr
        }

    }
    /**/
    pub mod set
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *,
        };

        /*
        #[derive( Debug, StructOpt )]
        #[structopt( name = "set", about = "Set shell options ( BETA )" )]
        struct OptMain {
            #[structopt( short, help = "exit on error status" )]
            exit_on_error:bool,
        }
        */
        #[derive( Debug )]
        struct OptMain();


        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parsers::line::tokens_to_args( tokens );
            let show_usage = args.len() > 1 && ( args[1] == "-h" || args[1] == "--help" );

            let opt = OptMain::from_iter_safe( args );
            match opt {
                Ok( opt ) =>
        {                   if opt.exit_on_error {
                        sh.exit_on_error = true;
                        cr
                    } else {
                        let info = "pls:set:option not implemented";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        cr
                    }
                }
                Err( e ) =>
        {                   let info = format!( "{}", e );
                    if show_usage {
                        print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                        cr.status = 0;
                    } else {
                        print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                        cr.status = 1;
                    }
                    cr
                }
            }
        }
    }
    /**/
    pub mod unpath
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = "pls:unpath:syntax error";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let input = &tokens[1].1;
            sh.remove_path( input );
            cr
        }
    }
    /**/
    pub mod unset
    {
        use ::
        {
            shell::{ Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = "pls:unset:syntax error";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_env( input )
            {
                let info = format!( "pls:unset:invalid varname:{:?}", input );
                print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                return cr;
            }
            cr
        }
    }
}
pub mod c
{
    pub mod time
    {
        use ::
        {
            time::{ OffsetDateTime },
            *,
        };

        #[derive( Debug, PartialEq, Eq )]
        pub struct DateTime 
        {
            odt:OffsetDateTime,
        }

        impl DateTime
        {
            pub fn now() -> Self
            {
                let odt:OffsetDateTime = match OffsetDateTime::now_local()
                {
                    Ok( dt ) => dt,
                    Err( _ ) => OffsetDateTime::now_utc(),
                };
                DateTime { odt }
            }

            pub fn from_timestamp( ts:f64 ) -> Self
            {
                let dummy_now = Self::now();
                let offset_seconds = dummy_now.odt.offset().whole_minutes() * 60;
                let ts_nano = ( ts + offset_seconds as f64 ) * 1000000000.0;
                let odt:OffsetDateTime = match OffsetDateTime::from_unix_timestamp_nanos( ts_nano as i128 )
                {
                    Ok( x ) => x,
                    Err( _ ) => OffsetDateTime::now_utc(),
                };
                DateTime { odt }
            }

            pub fn unix_timestamp( &self ) -> f64 { self.odt.unix_timestamp_nanos() as f64 / 1000000000.0 }
        }

        impl ::fmt::Display for DateTime
        {
            fn fmt( &self, f:&mut ::fmt::Formatter<'_> ) -> ::fmt::Result
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
                    self.odt.millisecond()
                )
            }
        }
    }
    /**/
    pub mod job
    {
        use ::
        {
            io::{ Write },
            nix::
            {
                sys::
                {
                    signal::{ Signal },
                    wait::{ waitpid, WaitPidFlag as WF, WaitStatus as WS },
                },
                unistd::{ Pid },
            },
            types::{ self, CommandResult },
            *,
        };

        pub fn print_job( job:&types::Job )
        {
            let line = get::job_line( job, true );
            println_stderr!( "{}", line );
        }

        pub fn mark_job_as_done( sh:&mut shell::Shell, gid:i32, pid:i32, reason:&str )
        {
            if let Some( mut job ) = sh.remove_pid_from_job( gid, pid ) {
                job.status = reason.to_string();
                if job.is_bg {
                    println_stderr!( "" );
                    print_job( &job );
                }
            }
        }

        pub fn mark_job_as_stopped( sh:&mut shell::Shell, gid:i32, report:bool )
        {
            sh.mark_job_as_stopped( gid );
            
            if !report { return; }
            
            if let Some( job ) = sh.get_job_by_gid( gid )
            {
                println_stderr!( "" );
                print_job( job );
            }
        }

        pub fn mark_job_member_stopped( sh:&mut shell::Shell, pid:i32, gid:i32, report:bool )
        {
            let _gid = if gid == 0 { unsafe { libc::getpgid( pid ) } } 
            else { gid };

            if let Some( job ) = sh.mark_job_member_stopped( pid, gid )
            {
                if job.all_members_stopped() { mark_job_as_stopped( sh, gid, report ); }
            }
        }

        pub fn mark_job_member_continued( sh:&mut shell::Shell, pid:i32, gid:i32 )
        {
            let _gid = if gid == 0 {
                unsafe { libc::getpgid( pid ) }
            } else {
                gid
            };

            if let Some( job ) = sh.mark_job_member_continued( pid, gid ) {
                if job.all_members_running() {
                    mark_job_as_running( sh, gid, true );
                }
            }
        }

        pub fn mark_job_as_running( sh:&mut shell::Shell, gid:i32, bg:bool )
        {
            sh.mark_job_as_running( gid, bg );
        }

        pub fn waitpidx( wpid:i32, block:bool ) -> types::WaitStatus
        {
            let options = if block {
                Some( WF::WUNTRACED | WF::WCONTINUED )
            } else {
                Some( WF::WUNTRACED | WF::WCONTINUED | WF::WNOHANG )
            };
            match waitpid( Pid::from_raw( wpid ), options ) {
                Ok( WS::Exited( pid, status ) ) =>
        {                   let pid = i32::from( pid );
                    types::WaitStatus::from_exited( pid, status )
                }
                Ok( WS::Stopped( pid, sig ) ) =>
        {                   let pid = i32::from( pid );
                    types::WaitStatus::from_stopped( pid, sig as i32 )
                }
                Ok( WS::Continued( pid ) ) =>
        {                   let pid = i32::from( pid );
                    types::WaitStatus::from_continuted( pid )
                }
                Ok( WS::Signaled( pid, sig, _core_dumped ) ) =>
        {                   let pid = i32::from( pid );
                    types::WaitStatus::from_signaled( pid, sig as i32 )
                }
                Ok( WS::StillAlive ) =>
        {                   types::WaitStatus::empty()
                }
                Ok( _others ) =>
        {                   // this is for PtraceEvent and PtraceSyscall on Linux,
                    // unreachable on other platforms.
                    types::WaitStatus::from_others()
                }
                Err( e ) =>
        {                   types::WaitStatus::from_error( e as i32 )
                }
            }
        }

        pub fn wait_fg_job( sh:&mut shell::Shell, gid:i32, pids:&[i32] ) -> CommandResult
        {
            let mut cmd_result = CommandResult::new();
            let mut count_waited = 0;
            let count_child = pids.len();
            if count_child == 0 {
                return cmd_result;
            }
            let pid_last = pids.last().unwrap();

            loop {
                let ws = waitpidx( -1, true );
                // here when we calling waitpidx(), all signals should have
                // been masked. There should no errors ( ECHILD/EINTR etc ) happen.
                if ws.is_error() {
                    let err = ws.get_errno();
                    if err == nix::Error::ECHILD {
                        break;
                    }

                    //log!( "jobc unexpected waitpid error:{}", err );
                    cmd_result = CommandResult::from_status( gid, err as i32 );
                    break;
                }

                let pid = ws.get_pid();
                let is_a_fg_child = pids.contains( &pid );
                if is_a_fg_child && !ws.is_continued() {
                    count_waited += 1;
                }

                if ws.is_exited() {
                    if is_a_fg_child {
                        mark_job_as_done( sh, gid, pid, "Done" );
                    } else {
                        let status = ws.get_status();
                        signals::insert_reap_map( pid, status );
                    }
                } else if ws.is_stopped() {
                    if is_a_fg_child {
                        // for stop signal of fg job ( current job )
                        // i.e. Ctrl-Z is pressed on the fg job
                        mark_job_member_stopped( sh, pid, gid, true );
                    } else {
                        // for stop signal of bg jobs
                        signals::insert_stopped_map( pid );
                        mark_job_member_stopped( sh, pid, 0, false );
                    }
                } else if ws.is_continued() {
                    if !is_a_fg_child {
                        signals::insert_cont_map( pid );
                    }
                    continue;
                } else if ws.is_signaled() {
                    if is_a_fg_child {
                        mark_job_as_done( sh, gid, pid, "Killed" );
                    } else {
                        signals::killed_map_insert( pid, ws.get_signal() );
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

        pub fn try_wait_bg_jobs( sh:&mut shell::Shell, report:bool, sig_handler_enabled:bool )
        {
            if sh.jobs.is_empty() {
                return;
            }

            if !sig_handler_enabled {
                // we need to wait pids in case CICADA_ENABLE_SIG_HANDLER=0
                signals::handle_sigchld( Signal::SIGCHLD as i32 );
            }

            let jobs = sh.jobs.clone();
            for ( _i, job ) in jobs.iter() {
                for pid in job.pids.iter() {
                    if let Some( _status ) = signals::pop_reap_map( *pid ) {
                        mark_job_as_done( sh, job.gid, *pid, "Done" );
                        continue;
                    }

                    if let Some( sig ) = signals::killed_map_pop( *pid ) {
                        let reason = if sig == Signal::SIGQUIT as i32 {
                            format!( "Quit:{}", sig )
                        } else if sig == Signal::SIGINT as i32 {
                            format!( "Interrupt:{}", sig )
                        } else if sig == Signal::SIGKILL as i32 {
                            format!( "Killed:{}", sig )
                        } else if sig == Signal::SIGTERM as i32 {
                            format!( "Terminated:{}", sig )
                        } else {
                            format!( "Killed:{}", sig )
                        };
                        mark_job_as_done( sh, job.gid, *pid, &reason );
                        continue;
                    }

                    if signals::pop_stopped_map( *pid ) {
                        mark_job_member_stopped( sh, *pid, job.gid, report );
                    } else if signals::pop_cont_map( *pid ) {
                        mark_job_member_continued( sh, *pid, job.gid );
                    }
                }
            }
        }
    }
}
/**/
pub mod completers
{
    use ::
    {
        completers::{ utils },
        path::{ Path },
        prompt::lines::
        {
            complete::{Completer, Completion,},
            prompter::Prompter,
            terminal::Terminal,
        },
        sync::{ Arc },
        *,
    };

    pub mod dots
    {
        use ::
        {
            borrow::Cow,
            fs::File,
            io::{Read, Write},
            path::Path,
            prompt::lines::
            {
                complete::{ Completer, Completion, escape, escaped_word_start, unescape, Suffix },
                prompter::Prompter,
                terminal::Terminal,
            },
            *,
        };        
        /// Performs completion by searching dotfiles
        pub struct DotsCompleter;

        impl<Term: Terminal> Completer<Term> for DotsCompleter
        {
            fn complete( &self, word: &str, reader: &Prompter<Term>, _start: usize, _end: usize ) -> 
            Option<Vec<Completion>>
            {
                let line = reader.buffer();
                Some(complete_dots(line, word))
            }

            fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize
            { escaped_word_start(&line[..end]) }

            fn quote<'a>(&self, word: &'a str) -> Cow<'a, str> { escape(word) }

            fn unquote<'a>(&self, word: &'a str) -> Cow<'a, str> { unescape(word) }
        }

        fn get_dot_file(line: &str) -> (String, String)
        {
            let args = parsers::parser_line::line_to_plain_tokens(line);
            let dir = tools::get_user_completer_dir();
            let dot_file = format!("{}/{}.yaml", dir, args[0]);

            if !Path::new(&dot_file).exists() { return (String::new(), String::new()); }

            let sub_cmd = if 
            (
                args.len() >= 3 
                && !args[1].starts_with('-')
            ) 
            ||
            (
                args.len() >= 2 
                && !args[1].starts_with('-') 
                && line.ends_with(' ')
            )
            { args[1].as_str() }
            
            else { "" };

            (dot_file, sub_cmd.to_string())
        }

        fn handle_lv1_string(res: &mut Vec<Completion>, value: &str, word: &str)
        {
            if !value.starts_with(word) && !value.starts_with('`') { return; }

            let linfo = parsers::parser_line::parse_line(value);
            let tokens = linfo.tokens;
            
            if tokens.len() == 1 && tokens[0].0 == "`"
            {
                log!("run subcmd: {:?}", &tokens[0].1);
                let cr = execute::run(&tokens[0].1);
                let v: Vec<&str> = cr.stdout.split(|c| c == '\n' || c == ' ').collect();
                
                for s in v
                {
                    if s.trim().is_empty() {
                        continue;
                    }
                    handle_lv1_string(res, s, word);
                }

                return;
            }

            let display = None;
            let suffix = Suffix::Default;
            res.push(Completion
            {
                completion: value.to_string(),
                display,
                suffix,
            });
        }

        fn handle_lv1_hash(res: &mut Vec<Completion>, h: &Hash, word: &str)
        {
            for v in h.values()
            {
                if let Yaml::Array(ref arr) = v
                {
                    for s in arr
                    {
                        if let Yaml::String(value) = s
                        {
                            if !value.starts_with(word) && !value.starts_with('`') { continue; }
                            handle_lv1_string(res, value, word);
                        }
                    }
                }
            }
        }

        fn complete_dots(line: &str, word: &str) -> Vec<Completion>
        {
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
            prompt::lines::
            {
                complete::{Completer, Completion, Suffix},
                prompter::{ Prompter },
                terminal::{ Terminal },
            },
            sync::{ Arc },
            *,
        };

        pub struct EnvCompleter
        {
            pub sh: Arc<shell::Shell>,
        }

        impl<Term: Terminal> Completer<Term> for EnvCompleter
        {
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

        fn complete_env(sh: &shell::Shell, path: &str) -> Vec<Completion>
        {
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
            fs::{ File },
            io::{BufRead, BufReader, Write},
            prompt::lines::
            {
                complete::{Completer, Completion, Suffix},
                prompter::{ Prompter },
                terminal::{ Terminal },
            },
            regex::{ Regex },
            *,
        };
        
        pub struct MakeCompleter;

        impl<Term: Terminal> Completer<Term> for MakeCompleter 
        {
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

        fn handle_file(ci: &mut Vec<Completion>, path: &str, file_path: &str, current_dir: &str) 
        {
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

        fn complete_make(path: &str) -> Vec<Completion> 
        {
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
            collections::{ HashSet },
            completers::{ utils },
            fs::{ read_dir },
            io::{ Write },
            iter::{ FromIterator },
            path::{ MAIN_SEPARATOR },
            prompt::lines::
            {
                complete::{Completer, Completion, Suffix},
                prompter::{ Prompter },
                terminal::{ Terminal },
            },
            os::unix::fs::{ PermissionsExt },
            sync::{ Arc },
            *,
        };

        pub struct BinCompleter
        {
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
        fn complete_bin(sh: &shell::Shell, path: &str) -> Vec<Completion> 
        {
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
            fs::{ File },
            io::{BufRead, BufReader},
            prompt::lines::
            {
                complete::{Completer, Completion, Suffix},
                prompter::{ Prompter },
                terminal::{ Terminal },
            },
            regex::{ Regex },
            *,
        };

        pub struct SshCompleter;

        impl<Term: Terminal> Completer<Term> for SshCompleter 
        {
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

        fn complete_ssh(path: &str) -> Vec<Completion> 
        {
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
            regex::{ Regex },
            *,
        };

        pub fn expand_home_string(text: &mut String)
        {
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

        pub fn expand_env_string(text: &mut String)
        {
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
    
    pub struct CicadaCompleter
    {
        pub sh: Arc<shell::Shell>,
    }

    fn for_make(line: &str) -> bool { libs::re::re_contains(line, r"^ *make ") }

    fn for_env(line: &str) -> bool { libs::re::re_contains(line, r" *\$[_a-zA-Z0-9]*$") }

    fn for_ssh(line: &str) -> bool { libs::re::re_contains(line, r"^ *(ssh|scp).* +[^ \./]+ *$") }

    fn for_cd(line: &str) -> bool { libs::re::re_contains(line, r"^ *cd +") }

    fn for_bin(line: &str) -> bool
    {
        let ptn = r"(^ *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)|(^.+\| *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)";
        libs::re::re_contains(line, ptn)
    }

    fn for_dots(line: &str) -> bool
    {
        let args = parsers::parser_line::line_to_plain_tokens(line);
        let len = args.len();

        if len == 0 { return false; }

        let dir = tools::get_user_completer_dir();
        let dot_file = format!("{}/{}.yaml", dir, args[0]);
        Path::new(dot_file.as_str()).exists()
    }

    impl<Term: Terminal> Completer<Term> for CicadaCompleter
    {
        fn complete( &self, word:&str, reader:&Prompter<Term>, start:usize, _end:usize ) -> Option<Vec<Completion>>
        {
            let line = reader.buffer();
            let completions: Option<Vec<Completion>>;

            if for_dots(line)
            {
                let cpl = Arc::new(dots::DotsCompleter);
                completions = cpl.complete(word, reader, start, _end);
            }
            
            else if for_ssh(line)
            {
                let cpl = Arc::new(ssh::SshCompleter);
                completions = cpl.complete(word, reader, start, _end);
            }
            
            else if for_make(line)
            {
                let cpl = Arc::new(make::MakeCompleter);
                completions = cpl.complete(word, reader, start, _end);
            }
            
            else if for_bin(line)
            {
                let cpl = Arc::new(path::BinCompleter
                {
                    sh: self.sh.clone(),
                });

                completions = cpl.complete(word, reader, start, _end);
            }
            
            else if for_env(line)
            {
                let cpl = Arc::new(env::EnvCompleter
                {
                    sh: self.sh.clone(),
                });

                completions = cpl.complete(word, reader, start, _end);
            }

            else if for_cd(line)
            {
                let cpl = Arc::new(path::CdCompleter);
                return cpl.complete(word, reader, start, _end);
            }

            else { completions = None; }

            if let Some(x) = completions
            {
                if !x.is_empty() { return Some(x); }
            }
            
            let cpl = Arc::new(path::PathCompleter);
            cpl.complete(word, reader, start, _end)
        }

        fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
            escaped_word_start(&line[..end])
        }
    }

    pub fn escaped_word_start(line: &str) -> usize
    {
        let mut start_position: usize = 0;
        let mut found_bs = false;
        let mut found_space = false;
        let mut with_quote = false;
        let mut ch_quote = '\0';
        let mut extra_bytes = 0;

        for (i, c) in line.chars().enumerate()
        {
            if found_space
            {
                found_space = false;
                start_position = i + extra_bytes;
            }

            if c == '\\'
            {
                found_bs = true;
                continue;
            }

            if c == ' ' && !found_bs && !with_quote
            {
                found_space = true;
                continue;
            }

            if !with_quote && !found_bs && (c == '"' || c == '\'')
            {
                with_quote = true;
                ch_quote = c;
            }

            else if with_quote && !found_bs && ch_quote == c { with_quote = false; }

            let bytes_c = c.len_utf8();

            if bytes_c > 1 { extra_bytes += bytes_c - 1; }

            found_bs = false;
        }

        if found_space { start_position = line.len(); }
        
        start_position
    }
}
/**/
pub mod execute
{
    use ::
    {
        collections::{ HashMap },
        io::{self, Read, Write},
        regex::{ Regex },
        shell::{self, Shell},
        types::{CommandLine, CommandResult, Tokens},
        *,
    };
    /// Entry point for non-ttys (e.g. Cmd-N on MacVim)
    pub fn run_procs_for_non_tty(sh: &mut Shell)
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

            Err(e) => { println!("cicada: stdin.read_to_string() failed: {:?}", e); }
        }
    }

    pub fn run_command_line(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();

        for token in parsers::parser_line::line_to_cmds(line)
        {
            if token == ";" || token == "&&" || token == "||"
            {
                sep = token.clone();
                continue;
            }

            if sep == "&&" && status != 0 { break; }

            if sep == "||" && status == 0 { break; }

            let cmd = token.clone();
            let cr = run_proc(sh, &cmd, tty, capture);
            status = cr.status;
            sh.previous_status = status;
            cr_list.push(cr);
        }

        cr_list
    }

    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String>
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let ptn_env_exp = r"^([a-zA-Z_][a-zA-Z0-9_]*)=(.*)$";
        let re = Regex::new(ptn_env_exp).unwrap();

        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !libs::re::re_contains(text, ptn_env_exp) { break; }

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

    fn line_to_tokens(sh: &mut Shell, line: &str) -> (Tokens, HashMap<String, String>)
    {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        shell::do_expansion(sh, &mut tokens);
        let envs = drain_env_tokens(&mut tokens);

        (tokens, envs)
    }

    fn set_shell_vars(sh: &mut Shell, envs: &HashMap<String, String>)
    {
        for (name, value) in envs.iter()
        {
            sh.set_env(name, value);
        }
    }
    
    fn run_proc(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> CommandResult
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
                    unsafe
                    {
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

    fn run_with_shell(sh: &mut Shell, line: &str) -> CommandResult
    {
        let (tokens, envs) = line_to_tokens(sh, line);
        
        if tokens.is_empty()
        {
            set_shell_vars(sh, &envs);
            return CommandResult::new();
        }

        match CommandLine::from_line(line, sh)
        {
            Ok(c) =>
            {
                let (term_given, cr) = core::run_pipeline(sh, &c, false, true, false);
                if term_given
                {
                    unsafe
                    {
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

    pub fn run(line: &str) -> CommandResult
    {
        let mut sh = Shell::new();
        run_with_shell(&mut sh, line)
    }
}
/**/
pub mod highlight
{
    use ::
    {
        ops::Range,
        sync::Arc,
        collections::HashSet,
        path::Path,
        parsers::parser_line,
        prompt::lines::highlighting::{Highlighter, Style},
        sync::Mutex,
        os::unix::fs::PermissionsExt,
        *,
    };

    #[derive(Clone)]
    pub struct CicadaHighlighter;

    // ANSI color codes wrapped with \x01 and \x02 for lineread
    const GREEN: &str = "\x01\x1b[0;32m\x02";

    lazy_static! 
    {
        static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
    }
    /// Initialize the available commands cache by scanning PATH directories
    pub fn init_command_cache() 
    {
        let commands = scan_available_commands();
        if let Ok(mut cache) = AVAILABLE_COMMANDS.lock() { *cache = commands; }
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
                                        if let Some( name ) = entry.file_name().to_str()
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

    fn is_command(word: &str) -> bool
    {
        if tools::is_builtin(word) { return true; }

        if let Ok(aliases) = ALIASES.lock()
        {
            if aliases.contains(word) { return true; }
        }

        if let Ok(commands) = AVAILABLE_COMMANDS.lock()
        {
            if commands.contains(word) { return true; }
        }

        false
    }

    fn find_token_range_heuristic(line: &str, start_byte: usize, token: &(String, String)) -> Option<Range<usize>>
    {
        let (sep, word) = token;        
        let mut search_area = &line[start_byte..];
        let token_start_byte = if let Some(non_ws_offset) = search_area.find(|c: char| !c.is_whitespace())
        { start_byte + search_area.char_indices().nth(non_ws_offset).map_or(0, |(idx, _)| idx) }
        else { return None; };

        search_area = &line[token_start_byte..];
        let mut estimated_len = 0;
        let mut current_search_offset = 0;
        
        if !sep.is_empty() && search_area.starts_with(sep)
        {
            estimated_len += sep.len();
            current_search_offset += sep.len();
        }
        
        if search_area[current_search_offset..].starts_with(word)
        {
            estimated_len += word.len();
            current_search_offset += word.len();
            if !sep.is_empty() && search_area[current_search_offset..].starts_with(sep)
            { estimated_len += sep.len(); }

            Some(token_start_byte..(token_start_byte + estimated_len))
        }
        
        else if word.is_empty() 
        && !sep.is_empty() 
        && search_area.starts_with(sep) 
        && search_area[sep.len()..].starts_with(sep)
        {
            estimated_len += sep.len() * 2;
            Some(token_start_byte..(token_start_byte + estimated_len))
        }

        else
        {
            if search_area.starts_with(word) { Some(token_start_byte..(token_start_byte + word.len())) }            
            else { None }
        }
    }

    impl Highlighter for CicadaHighlighter
    {
        fn highlight(&self, line: &str) -> Vec<(Range<usize>, Style)>
        {
            let mut styles = Vec::new();

            if line.is_empty() { return styles; }

            let line_info = parser_line::parse_line(line);
            
            if line_info.tokens.is_empty()
            {
                styles.push((0..line.len(), Style::Default));
                return styles;
            }

            let mut current_byte_idx = 0;
            let mut is_start_of_segment = true;

            for token in &line_info.tokens
            {
                match find_token_range_heuristic(line, current_byte_idx, token)
                {
                    Some(token_range) =>
                    {
                        if token_range.start > current_byte_idx
                        { styles.push((current_byte_idx..token_range.start, Style::Default)); }

                        let (_sep, word) = token;
                        let mut current_token_style = Style::Default;

                        if is_start_of_segment && !word.is_empty()
                        {
                            if is_command(word) { current_token_style = Style::AnsiColor(GREEN.to_string()); }
                            
                            is_start_of_segment = false;
                        }

                        styles.push((token_range.clone(), current_token_style));
                        
                        if ["|", "&&", "||", ";"].contains(&word.as_str()) { is_start_of_segment = true; }

                        current_byte_idx = token_range.end;
                    }

                    None =>
                    {
                        if current_byte_idx < line.len()
                        { styles.push((current_byte_idx..line.len(), Style::Default)); }

                        current_byte_idx = line.len();
                        break;
                    }
                }
            }
            
            if current_byte_idx < line.len() { styles.push((current_byte_idx..line.len(), Style::Default)); }

            styles
        }
    }

    pub fn create_highlighter() -> Arc<CicadaHighlighter>
    {
        Arc::new(CicadaHighlighter)
    }
}
/**/
pub mod history
{
    use ::
    {
        collections::{ HashMap },
        io::{ Write },
        path::{ Path },
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
    pub fn init_db(hfile: &str, htable: &str)
    {
        
    }

    //pub fn init(rl: &mut Interface<DefaultTerminal>)
    pub fn init()
    {

    }

    pub fn get_history_file() -> String
    {
        if let Ok(hfile) = env::var("HISTORY_FILE")
        {
            hfile
        }
        
        else if let Ok(d) = env::var("XDG_DATA_HOME")
        {
            format!("{}/{}", d, "cicada/history.sqlite")
        }
        
        else
        {
            let home = tools::get_user_home();
            format!("{}/{}", home, ".local/share/cicada/history.sqlite")
        }
    }

    pub fn get_history_table() -> String
    {
        if let Ok(hfile) = env::var("HISTORY_TABLE"){ hfile }

        else { String::from("cicada_history") }
    }

    fn delete_duplicated_histories()
    {
        let hfile = get_history_file();
        let history_table = get_history_table();
        let conn = match Conn::open(&hfile)
        {
            Ok(x) => x,
            Err(e) =>
            {
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };

        let sql = format
        (
            "DELETE FROM {} WHERE rowid NOT IN (
            SELECT MAX(rowid) FROM {} GROUP BY inp)",
            history_table, history_table
        );

        match conn.execute(&sql, [])
        {
            Ok(_) => {}
            Err(e) => match e
            {
                SqliteFailure(ee, msg) =>
                {
                    if ee.extended_code == 5
                    {
                        log!(
                            "failed to delete dup histories: {}",
                            msg.unwrap_or("db is locked?".to_owned()),
                        );
                        return;
                    }
                    println_stderr!
                    (
                        "cicada: history: delete dups error: {}: {:?}",
                        &ee,
                        &msg
                    );
                }
                _ =>
                {
                    println_stderr!("cicada: history: delete dup error: {}", e);
                }
            },
        }
    }

    pub fn add_raw(sh: &shell::Shell, line: &str, status: i32, tsb: f64, tse: f64)
    {
        let hfile = get_history_file();
        let history_table = get_history_table();
        if !Path::new(&hfile).exists()
        {
            init_db(&hfile, &history_table);
        }

        let conn = match Conn::open(&hfile)
        {
            Ok(x) => x,
            Err(e) => 
            {
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };

        let sql = format!
        (
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

        match conn.execute(&sql, [])
        {
            Ok(_) => {}
            Err(e) => println_stderr!("cicada: history: save error: {}", e),
        }
    }

    pub fn add(sh: &shell::Shell, rl: &mut Interface<DefaultTerminal>, line: &str, status: i32, tsb: f64, tse: f64)
    {
        add_raw(sh, line, status, tsb, tse);
        rl.add_history(line.to_string());
    }
}
/**/
pub mod jobc
{
    use ::
    {
        io::{ Write },
        nix::
        {
            sys::
            {
                signal::Signal,
                wait::waitpid,
                wait::WaitPidFlag as WF,
                wait::WaitStatus as WS,
            },
            unistd::{ Pid },
        },
        types::{ self, CommandResult },
        *,
    };
    
    pub fn get_job_line(job: &types::Job, trim: bool) -> String
    {
        let mut cmd = job.cmd.clone();
        if trim && cmd.len() > 50
        {
            cmd.truncate(50);
            cmd.push_str(" ...");
        }
        
        let _cmd = if job.is_bg && job.status == "Running" { format!("{} &", cmd) }
        else { cmd };

        format!("[{}] {}  {}   {}", job.id, job.gid, job.status, _cmd)
    }

    pub fn print_job(job: &types::Job)
    {
        let line = get_job_line(job, true);
        println_stderr!("{}", line);
    }

    pub fn mark_job_as_done(sh: &mut shell::Shell, gid: i32, pid: i32, reason: &str)
    {
        if let Some(mut job) = sh.remove_pid_from_job(gid, pid)
        {
            job.status = reason.to_string();
            if job.is_bg
            {
                println_stderr!("");
                print_job(&job);
            }
        }
    }

    pub fn mark_job_as_stopped(sh: &mut shell::Shell, gid: i32, report: bool)
    {
        sh.mark_job_as_stopped(gid);
        if !report { return; }
        
        if let Some(job) = sh.get_job_by_gid(gid)
        {
            println_stderr!("");
            print_job(job);
        }
    }

    pub fn mark_job_member_stopped(sh: &mut shell::Shell, pid: i32, gid: i32, report: bool)
    {
        let _gid = if gid == 0 { unsafe { libc::getpgid(pid) } }
        else { gid };

        if let Some(job) = sh.mark_job_member_stopped(pid, gid)
        {
            if job.all_members_stopped() { mark_job_as_stopped(sh, gid, report); }
        }
    }

    pub fn mark_job_member_continued(sh: &mut shell::Shell, pid: i32, gid: i32)
    {
        let _gid = if gid == 0 { unsafe { libc::getpgid(pid) } }
        else { gid };

        if let Some(job) = sh.mark_job_member_continued(pid, gid)
        {
            if job.all_members_running() { mark_job_as_running(sh, gid, true); }
        }
    }

    pub fn mark_job_as_running(sh: &mut shell::Shell, gid: i32, bg: bool) { sh.mark_job_as_running(gid, bg); }
    
    pub fn waitpidx(wpid: i32, block: bool) -> types::WaitStatus
    {
        let options = if block { Some(WF::WUNTRACED | WF::WCONTINUED) }
        else { Some(WF::WUNTRACED | WF::WCONTINUED | WF::WNOHANG) };

        match waitpid(Pid::from_raw(wpid), options)
        {
            Ok(WS::Exited(pid, status)) =>
            {
                let pid = i32::from(pid);
                types::WaitStatus::from_exited(pid, status)
            }

            Ok(WS::Stopped(pid, sig)) =>
            {
                let pid = i32::from(pid);
                types::WaitStatus::from_stopped(pid, sig as i32)
            }

            Ok(WS::Continued(pid)) =>
            {
                let pid = i32::from(pid);
                types::WaitStatus::from_continuted(pid)
            }
            
            Ok(WS::Signaled(pid, sig, _core_dumped)) =>
            {
                let pid = i32::from(pid);
                types::WaitStatus::from_signaled(pid, sig as i32)
            }

            Ok(WS::StillAlive) => { types::WaitStatus::empty() }

            Ok(_others) => { types::WaitStatus::from_others() }

            Err(e) => { types::WaitStatus::from_error(e as i32) }
        }
    }

    pub fn wait_fg_job(sh: &mut shell::Shell, gid: i32, pids: &[i32]) -> CommandResult
    {
        let mut cmd_result = CommandResult::new();
        let mut count_waited = 0;
        let count_child = pids.len();
        if count_child == 0 { return cmd_result; }

        let pid_last = pids.last().unwrap();

        loop
        {
            let ws = waitpidx(-1, true);
            
            if ws.is_error()
            {
                let err = ws.get_errno();
                if err == nix::Error::ECHILD { break; }
                
                log!("jobc unexpected waitpid error: {}", err);
                cmd_result = CommandResult::from_status(gid, err as i32);
                break;
            }

            let pid = ws.get_pid();
            let is_a_fg_child = pids.contains(&pid);
            if is_a_fg_child && !ws.is_continued() { count_waited += 1; }

            if ws.is_exited()
            {
                if is_a_fg_child { mark_job_as_done(sh, gid, pid, "Done"); }
                else
                {
                    let status = ws.get_status();
                    signals::insert_reap_map(pid, status);
                }
            }

            else if ws.is_stopped()
            {
                if is_a_fg_child { mark_job_member_stopped(sh, pid, gid, true); }
                else 
                {
                    signals::insert_stopped_map(pid);
                    mark_job_member_stopped(sh, pid, 0, false);
                }
            }
            
            else if ws.is_continued()
            {
                if !is_a_fg_child { signals::insert_cont_map(pid); }
                continue;
            }

            else if ws.is_signaled()
            {
                if is_a_fg_child { mark_job_as_done(sh, gid, pid, "Killed"); }
                else { signals::killed_map_insert(pid, ws.get_signal()); }
            }

            if is_a_fg_child && pid == *pid_last
            {
                let status = ws.get_status();
                cmd_result.status = status;
            }

            if count_waited >= count_child { break; }
        }

        cmd_result
    }

    pub fn try_wait_bg_jobs(sh: &mut shell::Shell, report: bool, sig_handler_enabled: bool)
    {
        if sh.jobs.is_empty() { return; }

        if !sig_handler_enabled { signals::handle_sigchld(Signal::SIGCHLD as i32); }

        let jobs = sh.jobs.clone();
        
        for (_i, job) in jobs.iter()
        {
            for pid in job.pids.iter()
            {
                if let Some(_status) = signals::pop_reap_map(*pid)
                {
                    mark_job_as_done(sh, job.gid, *pid, "Done");
                    continue;
                }

                if let Some(sig) = signals::killed_map_pop(*pid)
                {
                    let reason = if sig == Signal::SIGQUIT as i32 { format!("Quit: {}", sig) }                    
                    else if sig == Signal::SIGINT as i32 { format!("Interrupt: {}", sig) }                    
                    else if sig == Signal::SIGKILL as i32 { format!("Killed: {}", sig) }                    
                    else if sig == Signal::SIGTERM as i32 { format!("Terminated: {}", sig) }                    
                    else { format!("Killed: {}", sig) };

                    mark_job_as_done(sh, job.gid, *pid, &reason);
                    continue;
                }

                if signals::pop_stopped_map(*pid) { mark_job_member_stopped(sh, *pid, job.gid, report); }
                else if signals::pop_cont_map(*pid) { mark_job_member_continued(sh, *pid, job.gid); }
            }
        }
    }
}
/**/
pub mod libs
{
    pub mod colored
    {
        //! Setting prompt in crate lineread needs wrap every SEQ chars
        //! with prefixing with '\x01' and suffix with '\x02'.
        //! Color Reference: https://misc.flogisoft.com/bash/tip_colors_and_formatting

        /// cicada special
        pub const SEQ: &str = "\x01";
        pub const END_SEQ: &str = "\x02";
        pub const ESC: &str = "\x1B";
        pub const BOLD: &str = "\x01\x1B[1m\x02";
        pub const DIM: &str = "\x01\x1B[2m\x02";
        pub const UNDERLINED: &str = "\x01\x1B[4m\x02";
        pub const BLINK: &str = "\x01\x1B[5m\x02";
        pub const REVERSE: &str = "\x01\x1B[7m\x02";
        pub const HIDDEN: &str = "\x01\x1B[8m\x02";
        pub const RESET: &str = "\x01\x1B[0m\x02";
        pub const RESET_BOLD: &str = "\x01\x1B[21m\x02";
        pub const RESET_DIM: &str = "\x01\x1B[22m\x02";
        pub const RESET_UNDERLINED: &str = "\x01\x1B[24m\x02";
        pub const RESET_BLINK: &str = "\x01\x1B[25m\x02";
        pub const RESET_REVERSE: &str = "\x01\x1B[27m\x02";
        pub const RESET_HIDDEN: &str = "\x01\x1B[28m\x02";
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
            nix::
            {
                unistd::{fork as nix_fork, ForkResult},
                Result,
            },
            *,
        };
        
        pub fn fork() -> Result<ForkResult> { unsafe{ nix_fork() } }
    }

    pub mod os_type
    {
        use ::
        {
            *,
        };

        pub fn get_os_name() -> String
        {
            let uname = get_uname();
            if uname.to_lowercase() == "darwin" { get_macos_name() }
            else { get_other_os_name() }
        }

        fn get_other_os_name() -> String 
        {
            let mut name = get_release_value("PRETTY_NAME");
            if !name.is_empty() { return name; }

            name = get_release_value("DISTRIB_DESCRIPTION");
            if !name.is_empty() { return name; }

            name = get_release_value("IMAGE_DESCRIPTION");
            if !name.is_empty() { return name; }

            get_uname_mo()
        }

        fn get_release_value(ptn: &str) -> String 
        {
            let line = format!( "grep -i '{}' /etc/*release* 2>&1 | grep -o '=.*' | tr '\"=' ' '", ptn );
            let cr = execute::run(&line);
            return cr.stdout.trim().to_string();
        }

        fn get_uname() -> String 
        {
            let cr = execute::run("uname");
            return cr.stdout.trim().to_string();
        }

        fn get_uname_mo() -> String 
        {
            let cr = execute::run("uname -m -o");
            return cr.stdout.trim().to_string();
        }

        fn get_macos_name() -> String 
        {
            let mut os_name = get_osx_codename();
            let ver = get_osx_version();
            if !ver.is_empty()
            {
                os_name.push(' ');
                os_name.push_str(&ver);
            }

            os_name
        }

        fn get_osx_codename() -> String 
        {
            let cr = execute::run("grep -o 'SOFTWARE LICENSE AGREEMENT FOR .*[a-zA-Z]' '/System/Library/CoreServices/Setup Assistant.app/Contents/Resources/en.lproj/OSXSoftwareLicense.rtf' | sed 's/SOFTWARE LICENSE AGREEMENT FOR *//'");
            return cr.stdout.trim().to_string();
        }

        fn get_osx_version() -> String 
        {
            let cr = execute::run("sw_vers -productVersion");
            return cr.stdout.trim().to_string();
        }
    }

    pub mod path
    {
        use ::
        {
            borrow::{ Cow },
            fs::{ read_dir },
            io::{ ErrorKind, Write },
            os::unix::fs::{ PermissionsExt },
            regex::{ Regex },
            *,
        };

        pub fn basename(path: &str) -> Cow<'_, str>
        {
            let mut pieces = path.rsplit('/');
            match pieces.next()
            {
                Some(p) => p.into(),
                None => path.into(),
            }
        }

        pub fn expand_home(text: &str) -> String
        {
            let mut s: String = text.to_string();
            let v = vec!
            [
                r"(?P<head> +)~(?P<tail> +)",
                r"(?P<head> +)~(?P<tail>/)",
                r"^(?P<head> *)~(?P<tail>/)",
                r"(?P<head> +)~(?P<tail> *$)",
            ];

            for item in &v
            {
                let re;

                if let Ok(x) = Regex::new(item) { re = x; }                
                else { return String::new(); }
                
                let home = tools::get_user_home();
                let ss = s.clone();
                let to = format!("$head{}$tail", home);
                let result = re.replace_all(ss.as_str(), to.as_str());
                s = result.to_string();
            }

            s
        }

        pub fn find_file_in_path(filename: &str, exec: bool) -> String
        {
            let env_path = match env::var("PATH")
            {
                Ok(x) => x,
                Err(e) =>
                {
                    println_stderr!("cicada: error with env PATH: {:?}", e);
                    return String::new();
                }
            };

            let vec_path: Vec<&str> = env_path.split(':').collect();
            
            for p in &vec_path
            {
                match read_dir(p)
                {
                    Ok(list) =>
                    {
                        for entry in list.flatten()
                        {
                            if let Ok(name) = entry.file_name().into_string()
                            {
                                if name != filename { continue; }

                                if exec
                                {
                                    let _mode = match entry.metadata()
                                    {
                                        Ok(x) => x,
                                        Err(e) =>
                                        {
                                            println_stderr!("cicada: metadata error: {:?}", e);
                                            continue;
                                        }
                                    };

                                    let mode = _mode.permissions().mode();
                                    
                                    if mode & 0o111 == 0 { continue; }
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
            }

            String::new()
        }

        pub fn current_dir() -> String
        {
            let _current_dir = match env::current_dir()
            {
                Ok(x) => x,
                Err(e) =>
                {
                    log!("cicada: PROMPT: env current_dir error: {}", e);
                    return String::new();
                }
            };

            let current_dir = match _current_dir.to_str()
            {
                Some(x) => x,
                None =>
                {
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

    pub mod term_size
    {
        use ::
        {
            libc::{ c_int, c_ulong, winsize, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO },
            mem::{ zeroed },
            *,
        };
        
        #[cfg(any(target_os = "linux", target_os = "android"))]
        static TIOCGWINSZ: c_ulong = 0x5413;

        #[cfg(any
        (
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

        extern "C" 
        {
            fn ioctl(fd: c_int, request: c_ulong, ...) -> c_int;
        }
        /// Runs the ioctl command. Returns (0, 0) if all of the streams are not to a terminal, or there is an error.
        unsafe fn get_dimensions_any() -> winsize
        {
            let mut window: winsize = zeroed();
            let mut result = ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut window);

            if result == -1
            {
                window = zeroed();
                result = ioctl(STDIN_FILENO, TIOCGWINSZ, &mut window);

                if result == -1 
                {
                    window = zeroed();
                    result = ioctl(STDERR_FILENO, TIOCGWINSZ, &mut window);

                    if result == -1 { return zeroed(); }
                }
            }

            window
        }
        /// Query the current processes's stdout, stdin, and stderr in that order, 
        /// in the attempt to dtermine terminal width.
        pub fn dimensions() -> Option<(usize, usize)>
        {
            let w = unsafe { get_dimensions_any() };

            if w.ws_col == 0 || w.ws_row == 0 { None }
            else { Some((w.ws_col as usize, w.ws_row as usize)) }
        }
    }

    pub mod progopts
    {
        use ::
        {
            *,
        };

        pub fn is_login(args: &[String]) -> bool
        {

            if !args.is_empty() && args[0].starts_with("-") { return true; }

            if args.len() > 1 && (args[1] == "--login" || args[1] == "-l") { return true; }

            if let Ok(term_program) = env::var("TERM_PROGRAM")
            {
                if term_program == "vscode" { return true; }
            }

            false
        }

        pub fn is_script(args: &[String]) -> bool { args.len() > 1 && !args[1].starts_with("-") }

        pub fn is_command_string(args: &[String]) -> bool { args.len() > 1 && args[1] == "-c" }

        pub fn is_non_tty() -> bool { unsafe { libc::isatty(0) == 0 } }
    }

    pub mod pipes
    {
        use ::
        {
            libc::{ c_int },
            nix::{ Error },
            os::fd::{ RawFd },
            *,
        };
        
        pub fn pipe() -> std::result::Result<(RawFd, RawFd), Error>
        {
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
/**/
pub mod parsers
{
    pub mod parser_line
    {
        use ::
        {
            regex::{ Regex },
            types::{LineInfo, Redirection, Tokens},
            *,
        };
        
        pub fn line_to_plain_tokens(line: &str) -> Vec<String>
        {
            let mut result = Vec::new();
            let linfo = parse_line(line);
            
            for (_, r) in linfo.tokens
            {
                result.push(r.clone());
            }
            
            result
        }

        pub fn tokens_to_args(tokens: &Tokens) -> Vec<String>
        {
            let mut result = Vec::new();
            
            for s in tokens
            {
                result.push(s.1.clone());
            }
            
            result
        }

        pub fn tokens_to_line(tokens: &Tokens) -> String
        {
            let mut result = String::new();
            
            for t in tokens
            {
                if t.0.is_empty() { result.push_str(&t.1); }                
                else
                {
                    let s = tools::wrap_sep_string(&t.0, &t.1);
                    result.push_str(&s);
                }

                result.push(' ');
            }
            
            if result.ends_with(' ')
            {
                let len = result.len();
                result.truncate(len - 1);
            }
            
            result
        }
        /// Parse command line for multiple commands.
        pub fn line_to_cmds(line: &str) -> Vec<String>
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
                
                if c == '&' || c == '|' 
                {
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
                
                if c == ';' 
                {
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

            if !token.is_empty() { result.push(token.trim().to_string()); }
            
            result
        }
        /// parse command line to tokens
        pub fn parse_line(line: &str) -> LineInfo
        {
            let mut result = Vec::new();
            if tools::is_arithmetic(line)
            {
                for x in line.split(' ')
                {
                    result.push((String::from(""), x.to_string()));
                }
                
                return LineInfo::new(result);
            }

            let mut sep = String::new();
            let mut sep_second = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let mut met_parenthesis = false;
            let mut new_round = true;
            let mut skip_next = false;
            let mut has_dollar = false;
            let mut parens_left_ignored = false;
            let mut sep_made = String::new();
            let mut semi_ok = false;
            let count_chars = line.chars().count();

            for (i, c) in line.chars().enumerate()
            {
                if skip_next
                {
                    skip_next = false;
                    continue;
                }

                
                if has_backslash && sep.is_empty() && (c == '>' || c == '<') 
                {
                    sep_made = String::from("'");
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                
                if has_backslash && sep == "\"" && c != '\"' 
                {
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                
                if has_backslash 
                {
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

                
                if c == '$' 
                {
                    has_dollar = true;
                }
                
                if c == '(' && sep.is_empty() 
                {
                    if !has_dollar && token.is_empty() {
                        parens_left_ignored = true;
                        continue;
                    }
                    met_parenthesis = true;
                }
                
                if c == ')' 
                {
                    if parens_left_ignored && !has_dollar {
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

                
                if c == '\\' 
                {
                    if sep == "'" || !sep_second.is_empty() {
                        token.push(c)
                    } else {
                        has_backslash = true;
                    }
                    continue;
                }

                
                if new_round 
                {
                    if c == ' ' {
                        continue;
                    } else if c == '"' || c == '\'' || c == '`' {
                        sep = c.to_string();
                        new_round = false;
                        continue;
                    }

                    sep = String::new();

                    if c == '#' {
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

                
                if c == '|' && !has_backslash 
                {
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

                
                if c == ' ' 
                {
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

                
                if c == '\'' || c == '"' || c == '`' 
                {
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
            
            if !token.is_empty() || semi_ok 
            {
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

        pub fn tokens_to_redirections(tokens: &Tokens) -> Result<(Tokens, Vec<Redirection>), String>
        {
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

        pub fn unquote(text: &str) -> String 
        {
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

    }
}
/**/
pub mod prompt
{
    use ::
    {
        prompt::
        {
            main::{ get_prompt_string, render_prompt },
            multilines::{ EnterFunction },
        },
        *,
    };
    /**/
    /*
    linereader v0.0.0*/
    pub mod lines
    {

    }
    /**/
    mod main
    {
        use ::
        {
            prompt::preset::{ apply_preset_item, apply_pyenv },
            *,
        };

        pub const DEFAULT_PROMPT: &str = "${COLOR_STATUS}$USER${RESET}@${COLOR_STATUS}$HOSTNAME${RESET}: \
        ${COLOR_STATUS}$CWD${RESET}$ ";

        fn is_prefix_char(c: char) -> bool { c == '[' || c == '{' }

        fn is_suffix_char(c: char) -> bool { c == ']' || c == '}' }

        fn is_prompt_item_char(c: char, token: &str) -> bool
        {
            let s = c.to_string();
            if token.is_empty() { libs::re::re_contains(&s, r#"^[a-zA-Z_]$"#) }
            else { libs::re::re_contains(&s, r#"^[a-zA-Z0-9_]$"#) }
        }

        pub fn get_prompt_string() -> String
        {
            if let Ok(x) = env::var("PROMPT")
            {
                return x;
            }

            DEFAULT_PROMPT.to_string()
        }

        fn apply_prompt_item(sh: &shell::Shell, result: &mut String, token: &str)
        {
            if let Some(x) = sh.get_env(token)
            {
                result.push_str(&x);
                return;
            }

            apply_preset_item(sh, result, token);
        }

        fn apply_command(result: &mut String, token: &str, prefix: &str, suffix: &str)
        {
            let cr = execute::run(token);
            let output = cr.stdout.trim();
            
            if !output.is_empty()
            {
                result.push_str(prefix);
                result.push_str(output);
                result.push_str(suffix);
            }
        }

        pub fn render_prompt(sh: &shell::Shell, ps: &str) -> String
        {
            let mut prompt = String::new();
            apply_pyenv(&mut prompt);
            let mut met_dollar = false;
            let mut met_brace = false;
            let mut met_paren = false;
            let mut token = String::new();
            let mut prefix = String::new();
            let mut suffix = String::new();

            for c in ps.chars()
            {
                if met_dollar
                {
                    if c == '(' && !met_brace && !met_paren
                    {
                        met_paren = true;
                        continue;
                    }
                    
                    if c == ')' && met_paren
                    {
                        apply_command(&mut prompt, &token, &prefix, &suffix);
                        token.clear();
                        prefix.clear();
                        suffix.clear();
                        met_dollar = false;
                        met_paren = false;
                        continue;
                    }
                    
                    if c == '{' && !met_brace && !met_paren
                    {
                        met_brace = true;
                        continue;
                    }
                    
                    else if c == '}' && met_brace
                    {
                        apply_prompt_item(sh, &mut prompt, &token);
                        token.clear();
                        met_dollar = false;
                        met_brace = false;
                        continue;
                    }
                    
                    else if c == '$'
                    {
                        if token.is_empty()
                        {
                            prompt.push('$');
                            met_dollar = true;
                            continue;
                        }
                        
                        else
                        {
                            apply_prompt_item(sh, &mut prompt, &token);
                            token.clear();
                            continue;
                        }
                    }
                    
                    else if met_paren
                    {
                        if is_prefix_char(c) { prefix.push(c); }
                        else if is_suffix_char(c) { suffix.push(c); }
                        else { token.push(c); }

                        continue;
                    }
                    
                    else if is_prompt_item_char(c, &token)
                    {
                        token.push(c);
                        continue;
                    }
                    
                    else if token.is_empty()
                    {
                        prompt.push('$');
                        prompt.push(c);
                        met_dollar = false;
                        continue;
                    }
                }

                if c == '$'
                {
                    met_dollar = true;
                    continue;
                }

                if !token.is_empty()
                {
                    apply_prompt_item(sh, &mut prompt, &token);
                    token.clear();
                }

                prompt.push(c);
                met_dollar = false;
            }

            if !token.is_empty()
            {
                apply_prompt_item(sh, &mut prompt, &token);
                met_dollar = false;
            }

            if met_dollar { prompt.push('$'); }

            if prompt.trim().is_empty() { return format!("cicada-{} >> ", "25.1.1" ); }

            prompt
        }
    }
    /**/
    mod multilines
    {
        use ::
        {
            parsers::parser_line::{ self },
            prompt::lines::{ Function, Prompter, Terminal },
            *,
        };

        pub struct EnterFunction;

        impl<T: Terminal> Function<T> for EnterFunction
        {
            fn execute(&self, prompter: &mut Prompter<T>, count: i32, _ch: char) -> io::Result<()>
            {
                let buf = prompter.buffer();
                let linfo = parser_line::parse_line( buf );
                if linfo.is_complete
                {
                    prompter.accept_input()
                }

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
    /**/
    mod preset
    {
        use ::
        {
            fs::{ File },
            io::{ Read, Write },
            path::{ Path },
            *,
        };
        
        fn apply_seq(prompt: &mut String) { prompt.push_str(libs::colored::SEQ); }
        fn apply_end_seq(prompt: &mut String) { prompt.push_str(libs::colored::END_SEQ); }
        fn apply_esc(prompt: &mut String) { prompt.push_str(libs::colored::ESC); }
        fn apply_underlined(prompt: &mut String) { prompt.push_str(libs::colored::UNDERLINED); }
        fn apply_user(prompt: &mut String)
        {
            let username = tools::get_user_name();
            prompt.push_str(&username);
        }
        fn apply_black(prompt: &mut String) { prompt.push_str(libs::colored::BLACK); }
        fn apply_black_b(prompt: &mut String) { prompt.push_str(libs::colored::BLACK_B); }
        fn apply_black_bg(prompt: &mut String) { prompt.push_str(libs::colored::BLACK_BG); }
        fn apply_blue(prompt: &mut String) { prompt.push_str(libs::colored::BLUE); }
        fn apply_blue_b(prompt: &mut String) { prompt.push_str(libs::colored::BLUE_B); }
        fn apply_blue_bg(prompt: &mut String) { prompt.push_str(libs::colored::BLUE_BG); }
        fn apply_bold(prompt: &mut String) { prompt.push_str(libs::colored::BOLD); }
        fn apply_green(prompt: &mut String) { prompt.push_str(libs::colored::GREEN); }
        fn apply_green_b(prompt: &mut String) { prompt.push_str(libs::colored::GREEN_B); }
        fn apply_green_bg(prompt: &mut String) { prompt.push_str(libs::colored::GREEN_BG); }
        fn apply_red(prompt: &mut String) { prompt.push_str(libs::colored::RED); }
        fn apply_red_b(prompt: &mut String) { prompt.push_str(libs::colored::RED_B); }
        fn apply_red_bg(prompt: &mut String) { prompt.push_str(libs::colored::RED_BG); }
        fn apply_white(prompt: &mut String) { prompt.push_str(libs::colored::WHITE); }
        fn apply_white_b(prompt: &mut String) { prompt.push_str(libs::colored::WHITE_B); }
        fn apply_white_bg(prompt: &mut String) { prompt.push_str(libs::colored::WHITE_BG); }
        fn apply_hidden(prompt: &mut String) { prompt.push_str(libs::colored::HIDDEN); }
        fn apply_reset(prompt: &mut String) { prompt.push_str(libs::colored::RESET); }
        fn apply_reverse(prompt: &mut String) { prompt.push_str(libs::colored::REVERSE); }
        fn apply_dim(prompt: &mut String) { prompt.push_str(libs::colored::DIM); }
        fn apply_blink(prompt: &mut String) { prompt.push_str(libs::colored::BLINK); }
        fn apply_reset_underlined(prompt: &mut String) { prompt.push_str(libs::colored::RESET_UNDERLINED); }
        fn apply_reset_dim(prompt: &mut String) { prompt.push_str(libs::colored::RESET_DIM); }
        fn apply_reset_reverse(prompt: &mut String) { prompt.push_str(libs::colored::RESET_REVERSE); }
        fn apply_reset_hidden(prompt: &mut String) { prompt.push_str(libs::colored::RESET_HIDDEN); }
        fn apply_reset_blink(prompt: &mut String) { prompt.push_str(libs::colored::RESET_BLINK); }
        fn apply_reset_bold(prompt: &mut String) { prompt.push_str(libs::colored::RESET_BOLD); }
        fn apply_default(prompt: &mut String) { prompt.push_str(libs::colored::DEFAULT); }
        fn apply_default_bg(prompt: &mut String) { prompt.push_str(libs::colored::DEFAULT_BG); }
        fn apply_cyan(prompt: &mut String) { prompt.push_str(libs::colored::CYAN); }
        fn apply_cyan_l(prompt: &mut String) { prompt.push_str(libs::colored::CYAN_L); }
        fn apply_cyan_bg(prompt: &mut String) { prompt.push_str(libs::colored::CYAN_BG); }
        fn apply_cyan_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::CYAN_L_BG); }
        fn apply_red_l(prompt: &mut String) { prompt.push_str(libs::colored::RED_L); }
        fn apply_red_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::RED_L_BG); }
        fn apply_green_l(prompt: &mut String) { prompt.push_str(libs::colored::GREEN_L); }
        fn apply_green_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::GREEN_L_BG); }
        fn apply_gray_l(prompt: &mut String) { prompt.push_str(libs::colored::GRAY_L); }
        fn apply_gray_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::GRAY_L_BG); }
        fn apply_gray_d(prompt: &mut String) { prompt.push_str(libs::colored::GRAY_D); }
        fn apply_gray_d_bg(prompt: &mut String) { prompt.push_str(libs::colored::GRAY_D_BG); }
        fn apply_magenta(prompt: &mut String) { prompt.push_str(libs::colored::MAGENTA); }
        fn apply_magenta_bg(prompt: &mut String) { prompt.push_str(libs::colored::MAGENTA_BG); }
        fn apply_magenta_l(prompt: &mut String) { prompt.push_str(libs::colored::MAGENTA_L); }
        fn apply_magenta_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::MAGENTA_L_BG); }
        fn apply_yellow(prompt: &mut String) { prompt.push_str(libs::colored::YELLOW); }
        fn apply_yellow_bg(prompt: &mut String) { prompt.push_str(libs::colored::YELLOW_BG); }
        fn apply_yellow_l(prompt: &mut String) { prompt.push_str(libs::colored::YELLOW_L); }
        fn apply_yellow_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::YELLOW_L_BG); }
        fn apply_blue_l(prompt: &mut String) { prompt.push_str(libs::colored::BLUE_L); }
        fn apply_blue_l_bg(prompt: &mut String) { prompt.push_str(libs::colored::BLUE_L_BG); }
        fn apply_color_status(sh: &shell::Shell, prompt: &mut String)
        {
            if sh.previous_status == 0 { prompt.push_str(libs::colored::GREEN_B); }
            else { prompt.push_str(libs::colored::RED_B); }
        }

        fn _find_git_root() -> String
        {
            let current_dir = libs::path::current_dir();
            let dir_git = format!("{}/.git", current_dir);
            if Path::new(&dir_git).exists() { return current_dir; }

            let mut _dir = current_dir.clone();
            while Path::new(&_dir).parent().is_some()
            {
                match Path::new(&_dir).parent()
                {
                    Some(p) =>
                    {
                        _dir = p.to_string_lossy().to_string();
                        let dir_git = format!("{}/.git", _dir);
                        if Path::new(&dir_git).exists() { return _dir; }
                    }

                    None => { break; }
                }
            }

            String::new()
        }

        fn apply_gitbr(prompt: &mut String)
        {
            let git_root = _find_git_root();
            if git_root.is_empty() { return; }

            let file_head = format!("{}/.git/HEAD", git_root);
            if !Path::new(&file_head).exists() { return; }

            let mut file;
            match File::open(&file_head)
            {
                Ok(x) => file = x,
                Err(e) =>
                {
                    println!("cicada: .git/HEAD err: {:?}", e);
                    return;
                }
            }

            let mut text = String::new();
            match file.read_to_string(&mut text)
            {
                Ok(_) => {}
                Err(e) =>
                {
                    println!("cicada: read_to_string error: {:?}", e);
                    return;
                }
            }

            if let Some(branch) = libs::re::find_first_group(r"^[a-z]+: ?[a-z]+/[a-z]+/(.+)$", text.trim())
            {
                apply_blue_b(prompt);
                if let Ok(x) = env::var("CICADA_GITBR_PREFIX") { prompt.push_str(&x); }

                let _len_default: i32 = 32;
                let mut len_max = if let Ok(x) = env::var("CICADA_GITBR_MAX_LEN")
                {
                    match x.parse::<i32>()
                    {
                        Ok(n) => n,
                        Err(_) => _len_default,
                    }
                }                
                else { _len_default };

                if len_max <= 0 { len_max = _len_default; }

                if branch.len() as i32 <= len_max { prompt.push_str(&branch); }
                else
                {
                    let len = branch.len() as i32;
                    let offset = (len - len_max + 2) as usize;
                    let branch_short = format!("..{}", &branch[offset..]);
                    prompt.push_str(&branch_short);
                }

                if let Ok(x) = env::var("CICADA_GITBR_SUFFIX") { prompt.push_str(&x); }

                apply_reset(prompt);
            }
        }

        fn apply_cwd(prompt: &mut String)
        {
            let _current_dir = match env::current_dir()
            {
                Ok(x) => x,
                Err(e) =>
                {
                    println_stderr!("cicada: PROMPT: env current_dir error: {}", e);
                    return;
                }
            };
            
            let current_dir = match _current_dir.to_str()
            {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: PROMPT: to_str error");
                    return;
                }
            };

            let _tokens: Vec<&str> = current_dir.split('/').collect();
            let last = match _tokens.last()
            {
                Some(x) => x,
                None =>
                {
                    log!("cicada: PROMPT: token last error");
                    return;
                }
            };

            let home = tools::get_user_home();
            
            let pwd = if last.is_empty()
            {
                "/"
            }
            else if current_dir == home { "~" }
            else { last };

            prompt.push_str(pwd);
        }

        fn apply_hostname(prompt: &mut String)
        {
            let hostname = tools::get_hostname();
            prompt.push_str(&hostname);
        }

        fn apply_newline(prompt: &mut String) { prompt.push('\n'); }

        pub fn apply_pyenv(prompt: &mut String)
        {
            if let Ok(x) = env::var("VIRTUAL_ENV")
            {
                if !x.is_empty()
                {
                    let _tokens: Vec<&str> = x.split('/').collect();
                    let env_name = match _tokens.last()
                    {
                        Some(x) => x,
                        None =>
                        {
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

        pub fn apply_preset_item(sh: &shell::Shell, prompt: &mut String, token: &str)
        {
            match token.to_ascii_lowercase().as_ref()
            {
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
    /**/
    fn get_prompt_len( prompt:&str ) -> i32
    {
        let mut count = 0;
        let mut met_x01 = false;
        
        for c in prompt.chars()
        {
            if c == '\x01'
            {
                met_x01 = true;
                continue;
            }
            
            else if c == '\x02'
            {
                met_x01 = false;
                continue;
            }

            if !met_x01 { count += 1; }
        }

        count
    }

    pub fn get_prompt( sh:&shell::Shell ) -> String
    {
        let ps = get_prompt_string();
        let mut prompt = render_prompt(sh, &ps);
        if let Some((w, _h)) = libs::term_size::dimensions()
        {
            if get_prompt_len(&prompt) > (w / 2) as i32 && !libs::re::re_contains(&ps, r#"(?i)\$\{?newline.\}?"#)
            { prompt.push_str("\n$ "); }
        }

        else { log!("ERROR: Failed to get term size"); }
        
        prompt
    }
}
/**/
pub mod rcfile
{
    use ::
    {
        path::{ Path },
        *,
    };

    pub fn get_rc_file() -> String
    {
        let dir_config = tools::get_config_dir();
        let rc_file = format!( "{}/cicadarc", dir_config );

        if Path::new(&rc_file).exists() { return rc_file; }

        let home = tools::get_user_home();
        let rc_file_home = format!( "{}/{}", home, ".cicadarc" );
        
        if Path::new( &rc_file_home ).exists() { return rc_file_home; }
        
        rc_file
    }

    pub fn load_rc_files( sh:&mut shell::Shell )
    {
        let rc_file = get_rc_file();

        if !Path::new( &rc_file ).exists() { return; }

        let args = vec![ "source".to_string(), rc_file ];
        scripting::run_script( sh, &args );
    }

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
    fmt::{ * },
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
// 9431
