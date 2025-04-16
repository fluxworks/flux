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
#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
/**/
extern crate clap;
extern crate getrandom;
extern crate libc;
extern crate nix;
extern crate regex;
extern crate time;
extern crate unicode_width;
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

            let status = scripting::run_script( sh, &args );
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
    /**/
    pub mod utils
    {
        use ::
        {
            error::{ errno },
            fs::{ File },
            io::{ Write },
            os::unix::io::{FromRawFd, RawFd},
            types::{Command, CommandLine, CommandResult, Redirection},
            *,
        };
        /// Helper function to get (stdout, stderr) pairs for redirections,
        fn _get_std_fds(redirects: &[Redirection]) -> (Option<RawFd>, Option<RawFd>)
        {
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

        pub fn print_stdout(info: &str, cmd: &Command, cl: &CommandLine) {
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

        pub fn print_stderr(info: &str, cmd: &Command, cl: &CommandLine) {
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

        pub fn print_stderr_with_capture(info: &str, cr: &mut CommandResult,
                                        cl: &CommandLine, cmd: &Command,
                                        capture: bool) {
            cr.status = 1;
            if capture {
                cr.stderr = info.to_string();
            } else {
                print_stderr(info, cmd, cl);
            }
        }

        pub fn print_stdout_with_capture(info: &str, cr: &mut CommandResult,
                                        cl: &CommandLine, cmd: &Command,
                                        capture: bool) {
            cr.status = 0;
            if capture {
                cr.stdout = info.to_string();
            } else {
                print_stdout(info, cmd, cl);
            }
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
pub mod char
{
    //! Provides utilities for manipulating character values
    pub use std::char::{ * };
    // This is technically configurable on Unix.
    pub const EOF: char = '\x04';
    /// Character value generated by the Escape key
    pub const ESCAPE: char = '\x1b';
    /// Character value generated by the Backspace key.
    #[cfg(unix)]
    pub const DELETE: char = RUBOUT;
    /// Character value generated by the Backspace key
    #[cfg(windows)]
    pub const DELETE: char = '\x08';
    /// Character value generated by the Backspace key on some systems
    pub const RUBOUT: char = '\x7f';
    pub const CTRL_BIT: u8 = 0x40;
    pub const CTRL_MASK: u8 = 0x1f;
    /// Returns a character name as a key sequence, e.g. `Control-x` or `Meta-x`.
    pub fn parse_char_name(name: &str) -> Option<String> 
    {
        let name_lc = name.to_lowercase();

        let is_ctrl = contains_any(&name_lc, &["c-", "ctrl-", "control-"]);
        let is_meta = contains_any(&name_lc, &["m-", "meta-"]);

        let name = match name_lc.rfind('-') {
            Some(pos) => &name_lc[pos + 1..],
            None => &name_lc[..]
        };

        let ch = match name {
            "del" | "rubout"  => DELETE,
            "esc" | "escape"  => ESCAPE,
            "lfd" | "newline" => '\n',
            "ret" | "return"  => '\r',
            "spc" | "space"   => ' ',
            "tab"             => '\t',
            s if !s.is_empty() => s.chars().next().unwrap(),
            _ => return None
        };

        let ch = match (is_ctrl, is_meta) {
            (true,  true)  => meta(ctrl(ch)),
            (true,  false) => ctrl(ch).to_string(),
            (false, true)  => meta(ch),
            (false, false) => ch.to_string(),
        };

        Some(ch)
    }
    /// Returns a character sequence escaped for user-facing display.
    pub fn escape_sequence(s: &str) -> String
    {
        let mut res = String::with_capacity(s.len());

        for ch in s.chars() {
            match ch {
                ESCAPE => res.push_str(r"\e"),
                RUBOUT => res.push_str(r"\C-?"),
                '\\' => res.push_str(r"\\"),
                '\'' => res.push_str(r"\'"),
                '"' => res.push_str(r#"\""#),
                ch if is_ctrl(ch) => {
                    res.push_str(r"\C-");
                    res.push(unctrl_lower(ch));
                }
                ch => res.push(ch)
            }
        }

        res
    }
    /// Returns a meta sequence for the given character.
    pub fn meta(ch: char) -> String 
    {
        let mut s = String::with_capacity(ch.len_utf8() + 1);
        s.push(ESCAPE);
        s.push(ch);
        s
    }

    fn contains_any(s: &str, strs: &[&str]) -> bool { strs.iter().any(|a| s.contains(a)) }
    /// Returns whether the character is printable.
    pub fn is_printable(c: char) -> bool { c == '\t' || c == '\n' || !(c == '\0' || is_ctrl(c)) }
    /// Returns whether the given character is a control character.
    pub fn is_ctrl(c: char) -> bool
    {
        const CTRL_MAX: u32 = 0x1f;
        c != '\0' && c as u32 <= CTRL_MAX
    }
    /// Returns a control character for the given character.
    pub fn ctrl(c: char) -> char { ((c as u8) & CTRL_MASK) as char }
    /// Returns the printable character corresponding to the given control character.
    pub fn unctrl(c: char) -> char { ((c as u8) | CTRL_BIT) as char }
    /// Returns the lowercase character corresponding to the given control character.
    pub fn unctrl_lower(c: char) -> char { unctrl(c).to_ascii_lowercase() }
    #[inline] pub fn char_width(ch: char) -> Option<usize>
    {
        use unicode_width::UnicodeWidthChar;
        ch.width()
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
pub mod completers
{
    use ::
    {
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

        fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize
        { escaped_word_start(&line[..end]) }
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

                let (term_given, cr) = shell::run_pipeline(sh, &cl, tty, capture, log_cmd);
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
                let (term_given, cr) = shell::run_pipeline(sh, &c, false, true, false);
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
/*
glob v0.3.0*/
pub mod glob
{
    //! Support for matching file paths against Unix shell style patterns.
    use ::
    {
        error::Error,
        glob::
        {
            CharSpecifier::{CharRange, SingleChar},
            MatchResult::{EntirePatternDoesntMatch, Match, SubPatternDoesntMatch},
            PatternToken::AnyExcept,
            PatternToken::{AnyChar, AnyRecursiveSequence, AnySequence, AnyWithin, Char},
        },
        path::{self, Component, Path, PathBuf},
        result::{ Result, },
        str::FromStr,
        *,
    };
    /// An iterator that yields `Path`s from the filesystem that match a particular pattern.
    pub struct Paths
    {
        dir_patterns: Vec<Pattern>,
        require_dir: bool,
        options: MatchOptions,
        todo: Vec<Result<(PathBuf, usize), GlobError>>,
        scope: Option<PathBuf>,
    }
    /// Return an iterator that produces all the `Path`s that match the given pattern using default match options, 
    /// which may be absolute or relative to the current working directory.
    pub fn glob(pattern: &str) -> Result<Paths, PatternError> { glob_with(pattern, MatchOptions::new()) }
    /// Return an iterator that produces all the `Path`s that match the given
    /// pattern using the specified match options, which may be absolute or relative
    /// to the current working directory.
    pub fn glob_with(pattern: &str, options: MatchOptions) -> Result<Paths, PatternError> 
    {
        fn check_windows_verbatim(_: &Path) -> bool { false }
        
        fn to_scope(p: &Path) -> PathBuf { p.to_path_buf() }
        
        if let Err(err) = Pattern::new(pattern) { return Err(err); }

        let mut components = Path::new(pattern).components().peekable();
        
        loop
        {
            match components.peek()
            {
                Some(&Component::Prefix(..)) | Some(&Component::RootDir) => { components.next(); }
                _ => break,
            }
        }

        let rest = components.map(|s| s.as_os_str()).collect::<PathBuf>();
        let normalized_pattern = Path::new(pattern).iter().collect::<PathBuf>();
        let root_len = normalized_pattern.to_str().unwrap().len() - rest.to_str().unwrap().len();
        
        let root = if root_len > 0 { Some(Path::new(&pattern[..root_len])) }
        else { None };

        if root_len > 0 && check_windows_verbatim(root.unwrap())
        {
            return Ok
            (
                Paths
                {
                    dir_patterns: Vec::new(),
                    require_dir: false,
                    options,
                    todo: Vec::new(),
                    scope: None,
                }
            );
        }

        let scope = root.map_or_else(|| PathBuf::from("."), to_scope);
        let mut dir_patterns = Vec::new();
        let components = pattern[cmp::min(root_len, pattern.len())..].split_terminator(path::is_separator);

        for component in components
        {
            dir_patterns.push(Pattern::new(component)?);
        }

        if root_len == pattern.len()
        {
            dir_patterns.push(Pattern
            {
                original: "".to_string(),
                tokens: Vec::new(),
                is_recursive: false,
            });
        }

        let last_is_separator = pattern.chars().next_back().map(path::is_separator);
        let require_dir = last_is_separator == Some(true);
        let todo = Vec::new();

        Ok(Paths
        {
            dir_patterns,
            require_dir,
            options,
            todo,
            scope: Some(scope),
        })
    }
    /// A glob iteration error.
    #[derive(Debug)]
    pub struct GlobError 
    {
        path: PathBuf,
        error: io::Error,
    }

    impl GlobError 
    {
        /// The Path that the error corresponds to.
        pub fn path(&self) -> &Path { &self.path }
        /// The error in question.
        pub fn error(&self) -> &io::Error { &self.error }
        /// Consumes self, returning the _raw_ underlying `io::Error`
        pub fn into_error(self) -> io::Error { self.error }
    }

    impl Error for GlobError
    {
        fn description(&self) -> &str { self.error.description() }
        fn cause(&self) -> Option<&Error> { Some(&self.error) }
    }

    impl ::fmt::Display for GlobError 
    {
        fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result
        {
            write!
            (
                f, "attempting to read `{}` resulted in an error: {}", self.path.display(), self.error
            )
        }
    }

    fn is_dir(p: &Path) -> bool { fs::metadata(p).map(|m| m.is_dir()).unwrap_or(false) }
    /// An alias for a glob iteration result.
    pub type GlobResult = Result<PathBuf, GlobError>;

    impl Iterator for Paths 
    {
        type Item = GlobResult;

        fn next(&mut self) -> Option<GlobResult>
        {
            if let Some(scope) = self.scope.take()
            {
                if !self.dir_patterns.is_empty()
                {
                    assert!(self.dir_patterns.len() < !0 as usize);
                    fill_todo(&mut self.todo, &self.dir_patterns, 0, &scope, self.options);
                }
            }

            loop
            {
                if self.dir_patterns.is_empty() || self.todo.is_empty() { return None; }

                let (path, mut idx) = match self.todo.pop().unwrap()
                {
                    Ok(pair) => pair,
                    Err(e) => return Some(Err(e)),
                };
                
                if idx == !0 as usize
                {
                    if self.require_dir && !is_dir(&path) { continue; }
                    return Some(Ok(path));
                }

                if self.dir_patterns[idx].is_recursive
                {
                    let mut next = idx;
                    
                    while (next + 1) < self.dir_patterns.len() && self.dir_patterns[next + 1].is_recursive
                    {
                        next += 1;
                    }

                    if is_dir(&path)
                    {
                        fill_todo
                        (
                            &mut self.todo,
                            &self.dir_patterns,
                            next,
                            &path,
                            self.options,
                        );

                        if next == self.dir_patterns.len() - 1 { return Some(Ok(path)); }
                        else { idx = next + 1; }
                    }
                    else if next == self.dir_patterns.len() - 1 { continue; }
                    else { idx = next + 1; }
                }
                
                if self.dir_patterns[idx].matches_with(
                {
                    match path.file_name().and_then(|s| s.to_str())
                    {
                        None => continue,
                        Some(x) => x,
                    }
                }, self.options, )
                {
                    if idx == self.dir_patterns.len() - 1
                    {
                        if !self.require_dir || is_dir(&path) { return Some(Ok(path)); }
                    }
                    else
                    {
                        fill_todo
                        (
                            &mut self.todo,
                            &self.dir_patterns,
                            idx + 1,
                            &path,
                            self.options,
                        );
                    }
                }
            }
        }
    }
    /// A pattern parsing error.
    #[derive(Debug)]
    pub struct PatternError 
    {
        /// The approximate character index of where the error occurred.
        pub pos: usize,
        /// A message describing the error.
        pub msg: &'static str,
    }

    impl Error for PatternError 
    {
        fn description(&self) -> &str { self.msg }
    }

    impl ::fmt::Display for PatternError 
    {
        fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result 
        {
            write!
            (
                f,
                "Pattern syntax error near position {}: {}",
                self.pos, self.msg
            )
        }
    }
    /// A compiled Unix shell style pattern.
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
    pub struct Pattern
    {
        original: String,
        tokens: Vec<PatternToken>,
        is_recursive: bool,
    }
    /// Show the original glob pattern.
    impl ::fmt::Display for Pattern
    {
        fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result { self.original.fmt(f) }
    }

    impl FromStr for Pattern
    {
        type Err = PatternError;
        fn from_str(s: &str) -> Result<Self, PatternError> { Self::new(s) }
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    enum PatternToken
    {
        Char(char),
        AnyChar,
        AnySequence,
        AnyRecursiveSequence,
        AnyWithin(Vec<CharSpecifier>),
        AnyExcept(Vec<CharSpecifier>),
    }

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    enum CharSpecifier 
    {
        SingleChar(char),
        CharRange(char, char),
    }

    #[derive(Copy, Clone, PartialEq)]
    enum MatchResult 
    {
        Match,
        SubPatternDoesntMatch,
        EntirePatternDoesntMatch,
    }

    const ERROR_WILDCARDS: &str = "wildcards are either regular `*` or recursive `**`";
    const ERROR_RECURSIVE_WILDCARDS: &str = "recursive wildcards must form a single path component";
    const ERROR_INVALID_RANGE: &str = "invalid range pattern";

    impl Pattern
    {
        /// This function compiles Unix shell style patterns.
        pub fn new(pattern: &str) -> Result<Self, PatternError>
        {
            let chars = pattern.chars().collect::<Vec<_>>();
            let mut tokens = Vec::new();
            let mut is_recursive = false;
            let mut i = 0;

            while i < chars.len()
            {
                match chars[i]
                {
                    '?' =>
                    {
                        tokens.push(AnyChar);
                        i += 1;
                    }

                    '*' =>
                    {
                        let old = i;
                        
                        while i < chars.len() && chars[i] == '*'
                        {
                            i += 1;
                        }

                        let count = i - old;

                        if count > 2
                        {
                            return Err(PatternError
                            {
                                pos: old + 2,
                                msg: ERROR_WILDCARDS,
                            });
                        }
                        
                        else if count == 2
                        {
                            let is_valid = if i == 2 || path::is_separator(chars[i - count - 1])
                            {
                                if i < chars.len() && path::is_separator(chars[i])
                                {
                                    i += 1;
                                    true
                                }
                                
                                else if i == chars.len()
                                {
                                    true
                                }
                                
                                else
                                {
                                    return Err(PatternError
                                    {
                                        pos: i,
                                        msg: ERROR_RECURSIVE_WILDCARDS,
                                    });
                                }
                            }
                            
                            else
                            {
                                return Err(PatternError
                                {
                                    pos: old - 1,
                                    msg: ERROR_RECURSIVE_WILDCARDS,
                                });
                            };

                            let tokens_len = tokens.len();

                            if is_valid 
                            {
                                if !(tokens_len > 1 && tokens[tokens_len - 1] == AnyRecursiveSequence)
                                {
                                    is_recursive = true;
                                    tokens.push(AnyRecursiveSequence);
                                }
                            }
                        }
                        else { tokens.push(AnySequence); }
                    }

                    '[' =>
                    {
                        if i + 4 <= chars.len() && chars[i + 1] == '!'
                        {
                            match chars[i + 3..].iter().position(|x| *x == ']')
                            {
                                None => (),
                                Some(j) =>
                                {
                                    let chars = &chars[i + 2..i + 3 + j];
                                    let cs = parse_char_specifiers(chars);
                                    tokens.push(AnyExcept(cs));
                                    i += j + 4;
                                    continue;
                                }
                            }
                        }
                        
                        else if i + 3 <= chars.len() && chars[i + 1] != '!'
                        {
                            match chars[i + 2..].iter().position(|x| *x == ']')
                            {
                                None => (),
                                Some(j) =>
                                {
                                    let cs = parse_char_specifiers(&chars[i + 1..i + 2 + j]);
                                    tokens.push(AnyWithin(cs));
                                    i += j + 3;
                                    continue;
                                }
                            }
                        }

                        return Err(PatternError
                        {
                            pos: i,
                            msg: ERROR_INVALID_RANGE,
                        });
                    }

                    c =>
                    {
                        tokens.push(Char(c));
                        i += 1;
                    }
                }
            }

            Ok(Self
            {
                tokens,
                original: pattern.to_string(),
                is_recursive,
            })
        }
        /// Escape metacharacters within the given string by surrounding them in brackets. 
        pub fn escape(s: &str) -> String
        {
            let mut escaped = String::new();
            for c in s.chars()
            {
                match c
                {
                    '?' | '*' | '[' | ']' =>
                    {
                        escaped.push('[');
                        escaped.push(c);
                        escaped.push(']');
                    }

                    c => { escaped.push(c); }
                }
            }
            escaped
        }
        /// Return if the given `str` matches this `Pattern` using the default match options (i.e. `MatchOptions::new()`).
        pub fn matches(&self, str: &str) -> bool { self.matches_with(str, MatchOptions::new()) }
        /// Return if the given `Path`, when converted to a `str`, 
        /// matches this `Pattern` using the default match options (i.e. `MatchOptions::new()`).
        pub fn matches_path(&self, path: &Path) -> bool { path.to_str().map_or(false, |s| self.matches(s)) }
        /// Return if the given `str` matches this `Pattern` using the specified match options.
        pub fn matches_with(&self, str: &str, options: MatchOptions) -> bool
        { self.matches_from(true, str.chars(), 0, options) == Match }
        /// Return if the given `Path`, when converted to a `str`,  
        /// matches this `Pattern` using the specified match options.
        pub fn matches_path_with(&self, path: &Path, options: MatchOptions) -> bool
        {
            path.to_str().map_or(false, |s| self.matches_with(s, options))
        }
        /// Access the original glob pattern.
        pub fn as_str(&self) -> &str { &self.original }

        fn matches_from
        (
            &self,
            mut follows_separator: bool,
            mut file: std::str::Chars,
            i: usize,
            options: MatchOptions,
        ) -> MatchResult
        {
            for (ti, token) in self.tokens[i..].iter().enumerate()
            {
                match *token
                {
                    AnySequence | AnyRecursiveSequence =>
                    {
                        debug_assert!(match *token
                        {
                            AnyRecursiveSequence => follows_separator,
                            _ => true,
                        });

                        match self.matches_from(follows_separator, file.clone(), i + ti + 1, options)
                        {
                            SubPatternDoesntMatch => (),
                            m => return m,
                        };

                        while let Some(c) = file.next()
                        {
                            if follows_separator && options.require_literal_leading_dot && c == '.'
                            { return SubPatternDoesntMatch; }

                            follows_separator = path::is_separator(c);
                            
                            match *token
                            {
                                AnyRecursiveSequence if !follows_separator => continue,
                                AnySequence if options.require_literal_separator && follows_separator =>
                                {
                                    return SubPatternDoesntMatch
                                }
                                _ => (),
                            }

                            match self.matches_from
                            (
                                follows_separator,
                                file.clone(),
                                i + ti + 1,
                                options,
                            )
                            {
                                SubPatternDoesntMatch => (),
                                m => return m,
                            }
                        }
                    }
                    _ => {
                        let c = match file.next() {
                            Some(c) => c,
                            None => return EntirePatternDoesntMatch,
                        };

                        let is_sep = path::is_separator(c);

                        if !match *token {
                            AnyChar | AnyWithin(..) | AnyExcept(..)
                                if (options.require_literal_separator && is_sep)
                                    || (follows_separator
                                        && options.require_literal_leading_dot
                                        && c == '.') =>
                            {
                                false
                            }
                            AnyChar => true,
                            AnyWithin(ref specifiers) => in_char_specifiers(&specifiers, c, options),
                            AnyExcept(ref specifiers) => !in_char_specifiers(&specifiers, c, options),
                            Char(c2) => chars_eq(c, c2, options.case_sensitive),
                            AnySequence | AnyRecursiveSequence => unreachable!(),
                        } {
                            return SubPatternDoesntMatch;
                        }
                        follows_separator = is_sep;
                    }
                }
            }
            
            if file.next().is_none() {
                Match
            } else {
                SubPatternDoesntMatch
            }
        }
    }

    fn fill_todo(
        todo: &mut Vec<Result<(PathBuf, usize), GlobError>>,
        patterns: &[Pattern],
        idx: usize,
        path: &Path,
        options: MatchOptions,
    ) {
        fn pattern_as_str(pattern: &Pattern) -> Option<String> {
            let mut s = String::new();
            for token in &pattern.tokens {
                match *token {
                    Char(c) => s.push(c),
                    _ => return None,
                }
            }

            Some(s)
        }

        let add = |todo: &mut Vec<_>, next_path: PathBuf| {
            if idx + 1 == patterns.len() {
                todo.push(Ok((next_path, !0 as usize)));
            } else {
                fill_todo(todo, patterns, idx + 1, &next_path, options);
            }
        };

        let pattern = &patterns[idx];
        let is_dir = is_dir(path);
        let curdir = path == Path::new(".");
        match pattern_as_str(pattern) {
            Some(s) => {
                let special = "." == s || ".." == s;
                let next_path = if curdir {
                    PathBuf::from(s)
                } else {
                    path.join(&s)
                };
                if (special && is_dir) || (!special && fs::metadata(&next_path).is_ok()) {
                    add(todo, next_path);
                }
            }
            None if is_dir => {
                let dirs = fs::read_dir(path).and_then(|d| {
                    d.map(|e| {
                        e.map(|e| {
                            if curdir {
                                PathBuf::from(e.path().file_name().unwrap())
                            } else {
                                e.path()
                            }
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
                });
                match dirs {
                    Ok(mut children) => {
                        children.sort_by(|p1, p2| p2.file_name().cmp(&p1.file_name()));
                        todo.extend(children.into_iter().map(|x| Ok((x, idx))));
                        
                        if !pattern.tokens.is_empty() && pattern.tokens[0] == Char('.') {
                            for &special in &[".", ".."] {
                                if pattern.matches_with(special, options) {
                                    add(todo, path.join(special));
                                }
                            }
                        }
                    }
                    Err(e) => {
                        todo.push(Err(GlobError {
                            path: path.to_path_buf(),
                            error: e,
                        }));
                    }
                }
            }
            None => {
            }
        }
    }

    fn parse_char_specifiers(s: &[char]) -> Vec<CharSpecifier> {
        let mut cs = Vec::new();
        let mut i = 0;
        while i < s.len() {
            if i + 3 <= s.len() && s[i + 1] == '-' {
                cs.push(CharRange(s[i], s[i + 2]));
                i += 3;
            } else {
                cs.push(SingleChar(s[i]));
                i += 1;
            }
        }
        cs
    }

    fn in_char_specifiers(specifiers: &[CharSpecifier], c: char, options: MatchOptions) -> bool {
        for &specifier in specifiers.iter() {
            match specifier {
                SingleChar(sc) => {
                    if chars_eq(c, sc, options.case_sensitive) {
                        return true;
                    }
                }
                CharRange(start, end) => {
                    if !options.case_sensitive && c.is_ascii() && start.is_ascii() && end.is_ascii() {
                        let start = start.to_ascii_lowercase();
                        let end = end.to_ascii_lowercase();

                        let start_up = start.to_uppercase().next().unwrap();
                        let end_up = end.to_uppercase().next().unwrap();
                        if start != start_up && end != end_up {
                            let c = c.to_ascii_lowercase();
                            if c >= start && c <= end {
                                return true;
                            }
                        }
                    }

                    if c >= start && c <= end {
                        return true;
                    }
                }
            }
        }

        false
    }
    /// A helper function to determine if two chars are (possibly case-insensitively) equal.
    fn chars_eq(a: char, b: char, case_sensitive: bool) -> bool {
        if cfg!(windows) && path::is_separator(a) && path::is_separator(b) {
            true
        } else if !case_sensitive && a.is_ascii() && b.is_ascii() {
            a.to_ascii_lowercase() == b.to_ascii_lowercase()
        } else {
            a == b
        }
    }
    /// Configuration options to modify the behaviour of `Pattern::matches_with(..)`.
    #[allow(missing_copy_implementations)]
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct MatchOptions 
    {
        /// Whether or not patterns should be matched in a case-sensitive manner.
        pub case_sensitive: bool,
        /// Whether or not path-component separator characters (e.g. `/` on
        /// Posix) must be matched by a literal `/`, rather than by `*` or `?` or
        /// `[...]`.
        pub require_literal_separator: bool,

        /// Whether or not paths that contain components that start with a `.`
        /// will require that `.` appears literally in the pattern; `*`, `?`, `**`,
        /// or `[...]` will not match.
        pub require_literal_leading_dot: bool,
    }

    impl MatchOptions 
    {
        /// Constructs a new `MatchOptions` with default field values.
        pub fn new() -> Self 
        {
            Self 
            {
                case_sensitive: true,
                require_literal_separator: false,
                require_literal_leading_dot: false,
            }
        }
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
pub mod iter
{
    use ::
    {
        fmt::{ * },
        hash::{ Hash },
    };
    pub use std::iter::{ * };

    /// A trait which parser rules must implement.
    pub trait RuleType: Copy + Debug + Eq + Hash + Ord {}

    impl<T: Copy + Debug + Eq + Hash + Ord> RuleType for T {}
    /// Types and iterators for parser output.
    /*
    mod flat_pairs
    mod line_index;
    mod pair;
    pub(crate) mod pairs;
    mod queueable_token;
    mod tokens;

    pub use self::flat_pairs::FlatPairs;
    pub use self::pair::Pair;
    pub use self::pairs::Pairs;
    pub(crate) use self::queueable_token::QueueableToken;
    pub use self::tokens::Tokens;
    */
    pub mod line_index
    {
        #[derive(Clone)]
        pub struct LineIndex
        {
            /// Offset (bytes) the the beginning of each line, zero-based
            line_offsets: Vec<usize>,
        }

        impl LineIndex
        {
            pub fn new(text: &str) -> LineIndex
            {
                let mut line_offsets: Vec<usize> = vec![0];
                let mut offset = 0;

                for c in text.chars()
                {
                    offset += c.len_utf8();
                    if c == '\n' { line_offsets.push(offset); }
                }

                LineIndex { line_offsets }
            }
            /// Returns (line, col) of pos.
            pub fn line_col(&self, input: &str, pos: usize) -> (usize, usize) 
            {
                let line = self.line_offsets.partition_point(|&it| it <= pos) - 1;
                let first_offset = self.line_offsets[line];
                let line_str = &input[first_offset..pos];
                let col = line_str.chars().count();
                (line + 1, col + 1)
            }
        }
    }

    pub mod pair
    {
        use ::
        {
            borrow::{ Borrow },
            hash::{ Hash, Hasher },
            iter::
            { 
                line_index::{ LineIndex },
                pairs::{ self, Pairs },
                queueable_token::{ QueueableToken },
                RuleType 
            },
            rc::{ Rc },
            string::{ String },
            vec::{ Vec },
            *,
        };
        /*
        use super::tokens::{self, Tokens};
        use crate::span::Span;
        */
        /// A matching pair of [`Token`]s and everything between them.
        #[derive(Clone)]
        pub struct Pair<'i, R>
        {
            queue: Rc<Vec<QueueableToken<'i, R>>>,
            input: &'i str,
            /// Token index into `queue`.
            start: usize,
            line_index: Rc<LineIndex>,
        }

        pub fn new<'i, R: RuleType>
        ( queue:Rc<Vec<QueueableToken<'i, R>>>, input:&'i str, line_index:Rc<LineIndex>, start:usize ) -> 
        Pair<'i, R>
        {
            Pair
            {
                queue,
                input,
                start,
                line_index,
            }
        }
        /*
        impl<'i, R: RuleType> Pair<'i, R> {
            /// Returns the `Rule` of the `Pair`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     a
            /// }
            ///
            /// let input = "";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::a ...
            /// #     state.rule(Rule::a, |s| Ok(s))
            /// }).unwrap().next().unwrap();
            ///
            /// assert_eq!(pair.as_rule(), Rule::a);
            /// ```
            #[inline]
            pub fn as_rule(&self) -> R {
                match self.queue[self.pair()] {
                    QueueableToken::End { rule, .. } => rule,
                    _ => unreachable!(),
                }
            }

            /// Captures a slice from the `&str` defined by the token `Pair`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     ab
            /// }
            ///
            /// let input = "ab";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::ab ...
            /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
            /// }).unwrap().next().unwrap();
            ///
            /// assert_eq!(pair.as_str(), "ab");
            /// ```
            #[inline]
            pub fn as_str(&self) -> &'i str {
                let start = self.pos(self.start);
                let end = self.pos(self.pair());

                // Generated positions always come from Positions and are UTF-8 borders.
                &self.input[start..end]
            }

            /// Returns the input string of the `Pair`.
            ///
            /// This function returns the input string of the `Pair` as a `&str`. This is the source string
            /// from which the `Pair` was created. The returned `&str` can be used to examine the contents of
            /// the `Pair` or to perform further processing on the string.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     ab
            /// }
            ///
            /// // Example: Get input string from a Pair
            ///
            /// let input = "ab";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::ab ...
            /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
            /// }).unwrap().next().unwrap();
            ///
            /// assert_eq!(pair.as_str(), "ab");
            /// assert_eq!(input, pair.get_input());
            /// ```
            pub fn get_input(&self) -> &'i str {
                self.input
            }

            /// Returns the `Span` defined by the `Pair`, consuming it.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     ab
            /// }
            ///
            /// let input = "ab";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::ab ...
            /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
            /// }).unwrap().next().unwrap();
            ///
            /// assert_eq!(pair.into_span().as_str(), "ab");
            /// ```
            #[inline]
            #[deprecated(since = "2.0.0", note = "Please use `as_span` instead")]
            pub fn into_span(self) -> Span<'i> {
                self.as_span()
            }

            /// Returns the `Span` defined by the `Pair`, **without** consuming it.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     ab
            /// }
            ///
            /// let input = "ab";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::ab ...
            /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
            /// }).unwrap().next().unwrap();
            ///
            /// assert_eq!(pair.as_span().as_str(), "ab");
            /// ```
            #[inline]
            pub fn as_span(&self) -> Span<'i> {
                let start = self.pos(self.start);
                let end = self.pos(self.pair());

                Span::new_internal(self.input, start, end)
            }

            /// Get current node tag
            #[inline]
            pub fn as_node_tag(&self) -> Option<&str> {
                match &self.queue[self.pair()] {
                    QueueableToken::End { tag, .. } => tag.as_ref().map(|x| x.borrow()),
                    _ => None,
                }
            }

            /// Returns the inner `Pairs` between the `Pair`, consuming it.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     a
            /// }
            ///
            /// let input = "";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::a ...
            /// #     state.rule(Rule::a, |s| Ok(s))
            /// }).unwrap().next().unwrap();
            ///
            /// assert!(pair.into_inner().next().is_none());
            /// ```
            #[inline]
            pub fn into_inner(self) -> Pairs<'i, R> {
                let pair = self.pair();

                pairs::new(
                    self.queue,
                    self.input,
                    Some(self.line_index),
                    self.start + 1,
                    pair,
                )
            }

            /// Returns the `Tokens` for the `Pair`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use std::rc::Rc;
            /// # use pest;
            /// # #[allow(non_camel_case_types)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// enum Rule {
            ///     a
            /// }
            ///
            /// let input = "";
            /// let pair = pest::state(input, |state| {
            ///     // generating Token pair with Rule::a ...
            /// #     state.rule(Rule::a, |s| Ok(s))
            /// }).unwrap().next().unwrap();
            /// let tokens: Vec<_> = pair.tokens().collect();
            ///
            /// assert_eq!(tokens.len(), 2);
            /// ```
            #[inline]
            pub fn tokens(self) -> Tokens<'i, R> {
                let end = self.pair();

                tokens::new(self.queue, self.input, self.start, end + 1)
            }

            /// Generates a string that stores the lexical information of `self` in
            /// a pretty-printed JSON format.
            #[cfg(feature = "pretty-print")]
            pub fn to_json(&self) -> String {
                ::serde_json::to_string_pretty(self).expect("Failed to pretty-print Pair to json.")
            }

            /// Returns the `line`, `col` of this pair start.
            pub fn line_col(&self) -> (usize, usize) {
                let pos = self.pos(self.start);
                self.line_index.line_col(self.input, pos)
            }

            fn pair(&self) -> usize {
                match self.queue[self.start] {
                    QueueableToken::Start {
                        end_token_index, ..
                    } => end_token_index,
                    _ => unreachable!(),
                }
            }

            fn pos(&self, index: usize) -> usize {
                match self.queue[index] {
                    QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                        input_pos
                    }
                }
            }
        }

        impl<'i, R: RuleType> Pairs<'i, R> {
            /// Create a new `Pairs` iterator containing just the single `Pair`.
            pub fn single(pair: Pair<'i, R>) -> Self {
                let end = pair.pair();
                pairs::new(
                    pair.queue,
                    pair.input,
                    Some(pair.line_index),
                    pair.start,
                    end,
                )
            }
        }

        impl<'i, R: RuleType> fmt::Debug for Pair<'i, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let pair = &mut f.debug_struct("Pair");
                pair.field("rule", &self.as_rule());
                // In order not to break compatibility
                if let Some(s) = self.as_node_tag() {
                    pair.field("node_tag", &s);
                }
                pair.field("span", &self.as_span())
                    .field("inner", &self.clone().into_inner().collect::<Vec<_>>())
                    .finish()
            }
        }

        impl<'i, R: RuleType> fmt::Display for Pair<'i, R> 
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                let rule = self.as_rule();
                let start = self.pos(self.start);
                let end = self.pos(self.pair());
                let mut pairs = self.clone().into_inner().peekable();

                if pairs.peek().is_none() {
                    write!(f, "{:?}({}, {})", rule, start, end)
                } 
                else
                {
                     write!
                     (
                        f, 
                        "{:?}({}, {}, [{}])",
                        rule,
                        start,
                        end,
                        pairs
                        .map(|pair| format!("{}", pair))
                        .collect::<Vec<_>>()
                        .join(", ")
                    )
                }
            }
        }

        impl<'i, R: PartialEq> PartialEq for Pair<'i, R>
        {
            fn eq(&self, other: &Pair<'i, R>) -> bool {
                Rc::ptr_eq(&self.queue, &other.queue)
                    && ptr::eq(self.input, other.input)
                    && self.start == other.start
            }
        }

        impl<'i, R: Eq> Eq for Pair<'i, R> {}

        impl<'i, R: Hash> Hash for Pair<'i, R> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                (&*self.queue as *const Vec<QueueableToken<'i, R>>).hash(state);
                (self.input as *const str).hash(state);
                self.start.hash(state);
            }
        }
        
        impl<'i, R: RuleType> ::serde::Serialize for Pair<'i, R> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: ::serde::Serializer,
            {
                let start = self.pos(self.start);
                let end = self.pos(self.pair());
                let rule = format!("{:?}", self.as_rule());
                let inner = self.clone().into_inner();

                let mut ser = serializer.serialize_struct("Pairs", 3)?;
                ser.serialize_field("pos", &(start, end))?;
                ser.serialize_field("rule", &rule)?;

                if inner.peek().is_none() {
                    ser.serialize_field("inner", &self.as_str())?;
                } else {
                    ser.serialize_field("inner", &inner)?;
                }

                ser.end()
            }
        }
        */
    }

    pub mod pairs
    {
        use ::
        {
            hash::{Hash, Hasher},
            iter::
            {
                Filter,
                line_index::{ LineIndex },
                queueable_token::{ QueueableToken },
                RuleType,
            },
            vec::{ Vec },
            rc::{ Rc },
            string::{ String },
            *,
        };
        /*
        use super::flat_pairs::{self, FlatPairs};
        use super::line_index::LineIndex;
        use super::pair::{self, Pair};
        use super::queueable_token::QueueableToken;
        use super::tokens::{self, Tokens};
        use crate::RuleType;
        */
        /// An iterator over [`Pair`]s.
        #[derive(Clone)]
        pub struct Pairs<'i, R>
        {
            queue: Rc<Vec<QueueableToken<'i, R>>>,
            input: &'i str,
            start: usize,
            end: usize,
            pairs_count: usize,
            line_index: Rc<LineIndex>,
        }

        pub fn new<'i, R: RuleType>
        (
            queue: Rc<Vec<QueueableToken<'i, R>>>,
            input: &'i str,
            line_index: Option<Rc<LineIndex>>,
            start: usize,
            end: usize,
        ) -> Pairs<'i, R> 
        {
            let line_index = match line_index
            {
                Some(line_index) => line_index,
                None =>
                {
                    let last_input_pos = queue
                    .last()
                    .map(|token| match *token
                    {
                        QueueableToken::Start { input_pos, .. }
                        | QueueableToken::End { input_pos, .. } => input_pos,
                    }).unwrap_or(0);

                    Rc::new(LineIndex::new(&input[..last_input_pos]))
                }
            };

            let mut pairs_count = 0;
            let mut cursor = start;
            while cursor < end
            {
                cursor = match queue[cursor]
                {
                    QueueableToken::Start 
                    {
                        end_token_index, ..
                    } => end_token_index,
                    _ => unreachable!(),
                } + 1;
                pairs_count += 1;
            }

            Pairs
            {
                queue,
                input,
                start,
                end,
                pairs_count,
                line_index,
            }
        }
        /*
            impl<'i, R: RuleType> Pairs<'i, R>
            {
                /// Captures a slice from the `&str` defined by the starting position of the first token `Pair`
                /// and the ending position of the last token `Pair` of the `Pairs`.
                #[inline] pub fn as_str(&self) -> &'i str
                {
                    if self.start < self.end
                    {
                        let start = self.pos(self.start);
                        let end = self.pos(self.end - 1);
                        &self.input[start..end]
                    }
                    else { "" }
                }
                /// Returns the input string of `Pairs`.
                pub fn get_input(&self) -> &'i str { self.input }
                /// Captures inner token `Pair`s and concatenates resulting `&str`s.
                #[inline] pub fn concat(&self) -> String
                {
                    self.clone().fold(String::new(), |string, pair| string + pair.as_str())
                }
                /// Flattens the `Pairs`.
                #[inline] pub fn flatten(self) -> FlatPairs<'i, R>
                {
                    flat_pairs::new
                    (
                        self.queue,
                        self.input,
                        self.line_index,
                        self.start,
                        self.end,
                    )
                }

                /// Finds the first pair that has its node or branch tagged with the provided
                /// label. Searches in the flattened [`Pairs`] iterator.
                ///
                /// # Examples
                ///
                /// Try to recognize the branch between add and mul
                /// ```
                /// use pest::{state, ParseResult, ParserState};
                /// #[allow(non_camel_case_types)]
                /// #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                /// enum Rule {
                ///     number, // 0..9
                ///     add,    // num + num
                ///     mul,    // num * num
                /// }
                /// fn mark_branch(
                ///     state: Box<ParserState<'_, Rule>>,
                /// ) -> ParseResult<Box<ParserState<'_, Rule>>> {
                ///     expr(state, Rule::mul, "*")
                ///         .and_then(|state| state.tag_node("mul"))
                ///         .or_else(|state| expr(state, Rule::add, "+"))
                ///         .and_then(|state| state.tag_node("add"))
                /// }
                /// fn expr<'a>(
                ///     state: Box<ParserState<'a, Rule>>,
                ///     r: Rule,
                ///     o: &'static str,
                /// ) -> ParseResult<Box<ParserState<'a, Rule>>> {
                ///     state.rule(r, |state| {
                ///         state.sequence(|state| {
                ///             number(state)
                ///                 .and_then(|state| state.tag_node("lhs"))
                ///                 .and_then(|state| state.match_string(o))
                ///                 .and_then(number)
                ///                 .and_then(|state| state.tag_node("rhs"))
                ///         })
                ///     })
                /// }
                /// fn number(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
                ///     state.rule(Rule::number, |state| state.match_range('0'..'9'))
                /// }
                /// let input = "1+2";
                /// let pairs = state(input, mark_branch).unwrap();
                /// assert_eq!(pairs.find_first_tagged("add").unwrap().as_rule(), Rule::add);
                /// assert_eq!(pairs.find_first_tagged("mul"), None);
                /// ```
                #[inline]
                pub fn find_first_tagged(&self, tag: &'i str) -> Option<Pair<'i, R>> {
                    self.clone().find_tagged(tag).next()
                }

                /// Returns the iterator over pairs that have their node or branch tagged
                /// with the provided label. The iterator is built from a flattened [`Pairs`] iterator.
                ///
                /// # Examples
                ///
                /// Try to recognize the node between left and right hand side
                /// ```
                /// use pest::{state, ParseResult, ParserState};
                /// #[allow(non_camel_case_types)]
                /// #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                /// enum Rule {
                ///     number, // 0..9
                ///     add,    // num + num
                ///     mul,    // num * num
                /// }
                /// fn mark_branch(
                ///     state: Box<ParserState<'_, Rule>>,
                /// ) -> ParseResult<Box<ParserState<'_, Rule>>> {
                ///     expr(state, Rule::mul, "*")
                ///         .and_then(|state| state.tag_node("mul"))
                ///         .or_else(|state| expr(state, Rule::add, "+"))
                ///         .and_then(|state| state.tag_node("add"))
                /// }
                /// fn expr<'a>(
                ///     state: Box<ParserState<'a, Rule>>,
                ///     r: Rule,
                ///     o: &'static str,
                /// ) -> ParseResult<Box<ParserState<'a, Rule>>> {
                ///     state.rule(r, |state| {
                ///         state.sequence(|state| {
                ///             number(state)
                ///                 .and_then(|state| state.tag_node("lhs"))
                ///                 .and_then(|state| state.match_string(o))
                ///                 .and_then(number)
                ///                 .and_then(|state| state.tag_node("rhs"))
                ///         })
                ///     })
                /// }
                /// fn number(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
                ///     state.rule(Rule::number, |state| state.match_range('0'..'9'))
                /// }
                ///
                /// let input = "1+2";
                /// let pairs = state(input, mark_branch).unwrap();
                /// let mut left_numbers = pairs.find_tagged("lhs");
                /// assert_eq!(left_numbers.next().unwrap().as_str(), "1");
                /// assert_eq!(left_numbers.next(), None);
                /// ```
                #[inline]
                pub fn find_tagged(
                    self,
                    tag: &'i str,
                ) -> Filter<FlatPairs<'i, R>, impl FnMut(&Pair<'i, R>) -> bool + 'i> {
                    self.flatten()
                        .filter(move |pair: &Pair<'i, R>| matches!(pair.as_node_tag(), Some(nt) if nt == tag))
                }

                /// Returns the `Tokens` for the `Pairs`.
                ///
                /// # Examples
                ///
                /// ```
                /// # use std::rc::Rc;
                /// # use pest;
                /// # #[allow(non_camel_case_types)]
                /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                /// enum Rule {
                ///     a
                /// }
                ///
                /// let input = "";
                /// let pairs = pest::state(input, |state| {
                ///     // generating Token pair with Rule::a ...
                /// #     state.rule(Rule::a, |s| Ok(s))
                /// }).unwrap();
                /// let tokens: Vec<_> = pairs.tokens().collect();
                ///
                /// assert_eq!(tokens.len(), 2);
                /// ```
                #[inline]
                pub fn tokens(self) -> Tokens<'i, R> {
                    tokens::new(self.queue, self.input, self.start, self.end)
                }

                /// Peek at the first inner `Pair` without changing the position of this iterator.
                #[inline]
                pub fn peek(&self) -> Option<Pair<'i, R>> {
                    if self.start < self.end {
                        Some(pair::new(
                            Rc::clone(&self.queue),
                            self.input,
                            Rc::clone(&self.line_index),
                            self.start,
                        ))
                    } else {
                        None
                    }
                }

                /// Generates a string that stores the lexical information of `self` in
                /// a pretty-printed JSON format.
                #[cfg(feature = "pretty-print")]
                pub fn to_json(&self) -> String {
                    ::serde_json::to_string_pretty(self).expect("Failed to pretty-print Pairs to json.")
                }

                fn pair(&self) -> usize {
                    match self.queue[self.start] {
                        QueueableToken::Start {
                            end_token_index, ..
                        } => end_token_index,
                        _ => unreachable!(),
                    }
                }

                fn pair_from_end(&self) -> usize {
                    match self.queue[self.end - 1] {
                        QueueableToken::End {
                            start_token_index, ..
                        } => start_token_index,
                        _ => unreachable!(),
                    }
                }

                fn pos(&self, index: usize) -> usize {
                    match self.queue[index] {
                        QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                            input_pos
                        }
                    }
                }
            }

            impl<'i, R: RuleType> ExactSizeIterator for Pairs<'i, R>
            {
                #[inline]
                fn len(&self) -> usize {
                    self.pairs_count
                }
            }

            impl<'i, R: RuleType> Iterator for Pairs<'i, R>
            {
                type Item = Pair<'i, R>;

                fn next(&mut self) -> Option<Self::Item> {
                    let pair = self.peek()?;

                    self.start = self.pair() + 1;
                    self.pairs_count -= 1;
                    Some(pair)
                }

                fn size_hint(&self) -> (usize, Option<usize>) {
                    let len = <Self as ExactSizeIterator>::len(self);
                    (len, Some(len))
                }
            }

            impl<'i, R: RuleType> DoubleEndedIterator for Pairs<'i, R>
            {
                fn next_back(&mut self) -> Option<Self::Item> {
                    if self.end <= self.start {
                        return None;
                    }

                    self.end = self.pair_from_end();
                    self.pairs_count -= 1;

                    let pair = pair::new(
                        Rc::clone(&self.queue),
                        self.input,
                        Rc::clone(&self.line_index),
                        self.end,
                    );

                    Some(pair)
                }
            }

            impl<'i, R: RuleType> fmt::Debug for Pairs<'i, R>
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.debug_list().entries(self.clone()).finish()
                }
            }

            impl<'i, R: RuleType> fmt::Display for Pairs<'i, R>
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "[{}]",
                        self.clone()
                            .map(|pair| format!("{}", pair))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }

            impl<'i, R: PartialEq> PartialEq for Pairs<'i, R>
            {
                fn eq(&self, other: &Pairs<'i, R>) -> bool {
                    Rc::ptr_eq(&self.queue, &other.queue)
                        && ptr::eq(self.input, other.input)
                        && self.start == other.start
                        && self.end == other.end
                }
            }

            impl<'i, R: Eq> Eq for Pairs<'i, R> {}

            impl<'i, R: Hash> Hash for Pairs<'i, R>
            {
                fn hash<H: Hasher>(&self, state: &mut H) {
                    (&*self.queue as *const Vec<QueueableToken<'i, R>>).hash(state);
                    (self.input as *const str).hash(state);
                    self.start.hash(state);
                    self.end.hash(state);
                }
            }
        */
    }

    pub mod queueable_token
    {
        #[derive(Debug)]
        pub enum QueueableToken<'i, R>
        {
            Start
            {
                /// Queue (as a vec) contains both `Start` token and `End` for the same rule.
                end_token_index: usize,
                /// Position from which rule was tried to parse (or successfully parsed).
                input_pos: usize,
            },
            End {
                /// Queue (as a vec) contains both `Start` token and `End` for the same rule.
                start_token_index: usize,
                rule: R,
                tag: Option<&'i str>,
                /// Position at which successfully parsed rule finished (ended).
                input_pos: usize,
            },
        }
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
        prompt::lines::
        {
            interface::{ Interface },
            terminal::{ DefaultTerminal },
        },
        *,
    };
    /*
    use rusqlite::Connection as Conn;
    use rusqlite::Error::SqliteFailure;
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
/* mortal v.0.0.0 */
pub mod mortal
{
    pub mod sequence
    {
        //! Utilities for manipulating raw input sequences
        use ::
        {
            iter::FromIterator,
            mem::replace,
            *,
        };
        /// Contains a set of string sequences, mapped to a value.
        #[derive(Clone, Debug, Default)]
        pub struct SequenceMap<K, V>
        {
            sequences: Vec<(K, V)>,
        }
        /// Represents the result of a `SequenceMap::find` operation.
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum FindResult<V>
        {
            /// No contained sequences begin with the provided input sequence.
            NotFound,
            /// One or more sequences begin with the provided input sequence,
            /// but the sequence does not represent a complete sequence.
            Incomplete,
            /// A sequence was found exactly matching the input sequence;
            /// additionally, one or more sequences begin with the input sequence.
            Undecided(V),
            /// A sequence was found exactly matching the input sequence;
            /// no additional partially-matching sequences exist.
            Found(V),
        }

        impl<'a, V: Clone> FindResult<&'a V> 
        {
            /// Maps `FindResult<&V>` to `FindResult<V>` by cloning the contents of the result value.
            pub fn cloned(self) -> FindResult<V> 
            {
                match self 
                {
                    FindResult::NotFound => FindResult::NotFound,
                    FindResult::Incomplete => FindResult::Incomplete,
                    FindResult::Undecided(v) => FindResult::Undecided(v.clone()),
                    FindResult::Found(v) => FindResult::Found(v.clone()),
                }
            }
        }

        impl<K: AsRef<str>, V> SequenceMap<K, V> 
        {
            /// Creates an empty `SequenceMap`.
            pub fn new() -> SequenceMap<K, V> { SequenceMap::with_capacity(0) }
            /// Creates an empty `SequenceMap` with allocated capacity for `n` elements.
            pub fn with_capacity(n: usize) -> SequenceMap<K, V>
            {
                SequenceMap
                {
                    sequences: Vec::with_capacity(n),
                }
            }
            /// Returns a slice of all contained sequences, sorted by key.
            pub fn sequences(&self) -> &[(K, V)] { &self.sequences }
            /// Returns a mutable slice of all contained sequences, sorted by key.
            pub fn sequences_mut(&mut self) -> &mut [(K, V)] { &mut self.sequences }
            /// Returns an `Entry` for the given key.
            pub fn entry(&mut self, key: K) -> Entry<K, V>
            {
                match self.search(key.as_ref())
                {
                    Ok(n) => Entry::Occupied(OccupiedEntry
                    {
                        map: self,
                        index: n,
                    }),
                    
                    Err(n) => Entry::Vacant(VacantEntry
                    {
                        map: self,
                        key,
                        index: n,
                    })
                }
            }
            /// Performs a search for a partial or complete sequence match.
            pub fn find(&self, key: &str) -> FindResult<&V>
            {
                let (n, found) = match self.search(key)
                {
                    Ok(n) => (n, true),
                    Err(n) => (n, false)
                };

                let incomplete = self.sequences.get(n + (found as usize))
                .map_or(false, |&(ref next, _)| next.as_ref().starts_with(key));

                match (found, incomplete)
                {
                    (false, false) => FindResult::NotFound,
                    (false, true) => FindResult::Incomplete,
                    (true, false) => FindResult::Found(&self.sequences[n].1),
                    (true, true) => FindResult::Undecided(&self.sequences[n].1),
                }
            }
            /// Returns the corresponding value for the given sequence.
            pub fn get(&self, key: &str) -> Option<&V>
            {
                match self.search(key)
                {
                    Ok(n) => Some(&self.sequences[n].1),
                    Err(_) => None
                }
            }
            /// Returns a mutable reference to the corresponding value for the given sequence.
            pub fn get_mut(&mut self, key: &str) -> Option<&mut V>
            {
                match self.search(key)
                {
                    Ok(n) => Some(&mut self.sequences[n].1),
                    Err(_) => None
                }
            }
            /// Inserts a key-value pair into the map.
            pub fn insert(&mut self, key: K, value: V) -> Option<V>
            {
                match self.search(key.as_ref())
                {
                    Ok(n) => Some(replace(&mut self.sequences[n], (key, value)).1),
                    Err(n) =>
                    {
                        self.sequences.insert(n, (key, value));
                        None
                    }
                }
            }
            /// Removes a key-value pair from the map.
            pub fn remove(&mut self, key: &str) -> Option<(K, V)>
            {
                match self.search(key)
                {
                    Ok(n) => Some(self.sequences.remove(n)),
                    Err(_) => None
                }
            }

            fn search(&self, key: &str) -> Result<usize, usize>
            { self.sequences.binary_search_by_key(&key, |&(ref k, _)| &k.as_ref()) }
        }

        impl<K: AsRef<str>, V> From<Vec<(K, V)>> for SequenceMap<K, V>
        {
            /// Creates a `SequenceMap` from a `Vec` of key-value pairs.
            fn from(mut sequences: Vec<(K, V)>) -> SequenceMap<K, V>
            {
                sequences.sort_by(|a, b| a.0.as_ref().cmp(b.0.as_ref()));
                sequences.dedup_by(|a, b| a.0.as_ref() == b.0.as_ref());
                SequenceMap{sequences}
            }
        }

        impl<K: AsRef<str>, V> FromIterator<(K, V)> for SequenceMap<K, V>
        {
            /// Creates a `SequenceMap` from an iterator of key-value pairs.
            fn from_iter<I: IntoIterator<Item=(K, V)>>(iter: I) -> Self
            {
                let iter = iter.into_iter();
                let mut map = SequenceMap::with_capacity(iter.size_hint().0);

                for (k, v) in iter
                {
                    map.insert(k, v);
                }

                map
            }
        }
        /// A view into a single entry of a `SequenceMap`, which may be either occupied or vacant.
        pub enum Entry<'a, K: 'a, V: 'a>
        {
            /// An occupied entry
            Occupied(OccupiedEntry<'a, K, V>),
            /// A vacant entry
            Vacant(VacantEntry<'a, K, V>),
        }
        /// A view into an occupied entry in a `SequenceMap`.
        pub struct OccupiedEntry<'a, K: 'a, V: 'a>
        {
            map: &'a mut SequenceMap<K, V>,
            index: usize,
        }
        /// A view into a vacant entry in a `SequenceMap`.
        pub struct VacantEntry<'a, K: 'a, V: 'a>
        {
            map: &'a mut SequenceMap<K, V>,
            key: K,
            index: usize,
        }

        impl<'a, K, V> Entry<'a, K, V>
        {
            /// Provides in-place mutable access to an occupied entry before any potential inserts into the map.
            pub fn and_modify<F: FnOnce(&mut V)>(self, f: F) -> Self
            {
                match self
                {
                    Entry::Occupied(mut ent) =>
                    {
                        f(ent.get_mut());
                        Entry::Occupied(ent)
                    }
                    Entry::Vacant(ent) => Entry::Vacant(ent)
                }
            }

            /// Returns a mutable reference to entry value, inserting the provided default if the entry is vacant.
            pub fn or_insert(self, default: V) -> &'a mut V 
            {
                match self
                {
                    Entry::Occupied(ent) => ent.into_mut(),
                    Entry::Vacant(ent) => ent.insert(default)
                }
            }
            /// Returns a mutable reference to the entry value, 
            /// inserting a value using the provided closure if the entry is vacant.
            pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V
            {
                match self
                {
                    Entry::Occupied(ent) => ent.into_mut(),
                    Entry::Vacant(ent) => ent.insert(default())
                }
            }
            /// Returns a borrowed reference to the entry key.
            pub fn key(&self) -> &K
            {
                match *self
                {
                    Entry::Occupied(ref ent) => ent.key(),
                    Entry::Vacant(ref ent) => ent.key(),
                }
            }
        }

        impl<'a, K, V> OccupiedEntry<'a, K, V>
        {
            /// Returns a borrowed reference to the entry key.
            pub fn key(&self) -> &K { &self.map.sequences[self.index].0 }
            /// Returns a borrowed reference to the entry value.
            pub fn get(&self) -> &V { &self.map.sequences[self.index].1 }
            /// Returns a mutable reference to the entry value.
            pub fn get_mut(&mut self) -> &mut V { &mut self.map.sequences[self.index].1 }
            /// Converts the `OccupiedEntry` into a mut reference whose lifetime is bound to the `SequenceMap`.
            pub fn into_mut(self) -> &'a mut V { &mut self.map.sequences[self.index].1 }
            /// Replaces the entry value with the given value, returning the previous value.
            pub fn insert(&mut self, value: V) -> V { replace(self.get_mut(), value) }
            /// Removes the entry and returns the value.
            pub fn remove(self) -> V { self.map.sequences.remove(self.index).1 }
            /// Removes the entry and returns the key-value pair.
            pub fn remove_entry(self) -> (K, V) { self.map.sequences.remove(self.index) }
        }

        impl<'a, K, V> VacantEntry<'a, K, V>
        {
            /// Returns a borrowed reference to the entry key.
            pub fn key(&self) -> &K { &self.key }
            /// Consumes the `VacantEntry` and returns ownership of the key.
            pub fn into_key(self) -> K { self.key }
            /// Consumes the `VacantEntry` and inserts a value,
            /// returning a mutable reference to its place in the `SequenceMap`.
            pub fn insert(self, value: V) -> &'a mut V
            {
                self.map.sequences.insert(self.index, (self.key, value));
                &mut self.map.sequences[self.index].1
            }
        }

        impl<'a, K: fmt::Debug, V: fmt::Debug> fmt::Debug for Entry<'a, K, V>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                match *self
                {
                    Entry::Occupied(ref ent) => f.debug_tuple("Entry").field(ent).finish(),
                    Entry::Vacant(ref ent) => f.debug_tuple("Entry").field(ent).finish()
                }
            }
        }

        impl<'a, K: fmt::Debug, V: fmt::Debug> fmt::Debug for OccupiedEntry<'a, K, V>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                f.debug_struct("OccupiedEntry")
                .field("key", self.key())
                .field("value", self.get())
                .finish()
            }
        }

        impl<'a, K: fmt::Debug, V> fmt::Debug for VacantEntry<'a, K, V>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                f.debug_tuple("VacantEntry").field(self.key()).finish()
            }
        }
    }
    pub use self::sequence::{ FindResult, SequenceMap };
}
/**/
pub mod option
{
    pub use std::option::
    {
        Option::{ self, * },
        *,
    };
    /// Extension trait providing additional methods for `Option`.
    pub trait OptionExt<T>
    {
        /// Returns `true` if the option is a [`Some`] value containing the given value.
        #[must_use]
        fn contains<U>(&self, x: &U) -> bool where U: PartialEq<T>;

        /// Returns the result from applying the function `f` to the contained value if the option is [`Some`],
        /// or returns provided `default` value if the option is [`None`].
        #[must_use]
        fn map_or2<U, F: FnOnce(T) -> U>(self, f: F, default: U) -> U;

        /// Returns the result from applying the function `f` to the contained value if the option is [`Some`],
        /// or returns the result from evaluating the provided function `default` if the option is [`None`].
        #[must_use]
        fn map_or_else2<U, F: FnOnce(T) -> U, D: FnOnce() -> U>(self, f: F, default: D) -> U;
    }

    impl<T> OptionExt<T> for Option<T>
    {
        fn contains<U>(&self, x: &U) -> bool where U: PartialEq<T>
        {
            match *self
            {
                Some(ref y) => x == y,
                None => false,
            }
        }

        #[inline] fn map_or2<U, F: FnOnce(T) -> U>(self, f: F, default: U) -> U { self.map_or(default, f) }

        #[inline] fn map_or_else2<U, F: FnOnce(T) -> U, D: FnOnce() -> U>(self, f: F, default: D) -> U
        { self.map_or_else(default, f) }
    }
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
        #[derive( Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub enum Rule
        {
            CMD,
            FOR_HEAD,
            FOR_INIT,
            FOR_VAR,
            EXP_BODY,
            EXP_FOR,
            EXP_IF,
            EXP_WHILE,                        
            IF_HEAD,
            IF_ELSEIF_HEAD,
            KW_ELSE,
            TEST,
            WHILE_HEAD,
            WHILE_BODY,
        }
    }
}
/**/
pub mod path
{
    pub use std::path::{ * };
    
    pub mod system
    {
        use ::
        {
            collections::{ HashMap },
            ffi::{ OsString },
            io::{ self, Read },
            option::{ OptionExt },
            os::unix::ffi::OsStringExt,
            path::{ Path, PathBuf },
            *,
        };

        pub mod unix
        {
            use ::
            {
                ffi::{ CStr, OsString },
                os::unix::ffi::{ OsStringExt },
                path::{ * },
                *,
            };
            
            pub fn home_dir() -> Option<PathBuf> 
            {
                return env::var_os("HOME")
                    .and_then(|h| if h.is_empty() { None } else { Some(h) })
                    .or_else(|| unsafe { fallback() })
                    .map(PathBuf::from);
                    
                unsafe fn fallback() -> Option<OsString> {
                    let amt = match libc::sysconf(libc::_SC_GETPW_R_SIZE_MAX) {
                        n if n < 0 => 512 as usize,
                        n => n as usize,
                    };
                    let mut buf = Vec::with_capacity(amt);
                    let mut passwd: libc::passwd = mem::zeroed();
                    let mut result = ptr::null_mut();
                    match libc::getpwuid_r(
                        libc::getuid(),
                        &mut passwd,
                        buf.as_mut_ptr(),
                        buf.capacity(),
                        &mut result,
                    ) {
                        0 if !result.is_null() => {
                            let ptr = passwd.pw_dir as *const _;
                            let bytes = CStr::from_ptr(ptr).to_bytes();
                            if bytes.is_empty() {
                                None
                            } else {
                                Some(OsStringExt::from_vec(bytes.to_vec()))
                            }
                        }
                        _ => None,
                    }
                }
            }
            
            pub fn user_dir(user_dir_name: &str) -> Option<PathBuf>
            {
                None
            }
            
            pub fn cache_dir()        -> Option<PathBuf> { env::var_os("XDG_CACHE_HOME").and_then( super::is_absolute_path ).or_else(|| home_dir().map(|h| h.join(".cache"))) }
            pub fn config_dir()       -> Option<PathBuf> { env::var_os("XDG_CONFIG_HOME").and_then( super::is_absolute_path ).or_else(|| home_dir().map(|h| h.join(".config"))) }
            pub fn config_local_dir() -> Option<PathBuf> { config_dir() }
            pub fn data_dir()         -> Option<PathBuf> { env::var_os("XDG_DATA_HOME").and_then( super::is_absolute_path ).or_else(|| home_dir().map(|h| h.join(".local/share"))) }
            pub fn data_local_dir()   -> Option<PathBuf> { data_dir() }
            pub fn preference_dir()   -> Option<PathBuf> { config_dir() }
            pub fn runtime_dir()      -> Option<PathBuf> { env::var_os("XDG_RUNTIME_DIR").and_then( super::is_absolute_path ) }
            pub fn state_dir()        -> Option<PathBuf> { env::var_os("XDG_STATE_HOME").and_then( super::is_absolute_path ).or_else(|| home_dir().map(|h| h.join(".local/state"))) }
            pub fn executable_dir()   -> Option<PathBuf> { env::var_os("XDG_BIN_HOME").and_then( super::is_absolute_path ).or_else(|| home_dir().map(|h| h.join(".local/bin"))) }
            pub fn audio_dir()        -> Option<PathBuf> { user_dir("MUSIC") }
            pub fn desktop_dir()      -> Option<PathBuf> { user_dir("DESKTOP") }
            pub fn document_dir()     -> Option<PathBuf> { user_dir("DOCUMENTS") }
            pub fn download_dir()     -> Option<PathBuf> { user_dir("DOWNLOAD") }
            pub fn font_dir()         -> Option<PathBuf> { data_dir().map(|d| d.join("fonts")) }
            pub fn picture_dir()      -> Option<PathBuf> { user_dir("PICTURES") }
            pub fn public_dir()       -> Option<PathBuf> { user_dir("PUBLICSHARE") }
            pub fn template_dir()     -> Option<PathBuf> { user_dir("TEMPLATES") }
            pub fn video_dir()        -> Option<PathBuf> { user_dir("VIDEOS") }
        }

        pub mod windows
        {
            use ::
            {
                ffi::{ c_void, OsString },
                os::windows::ffi::{ OsStringExt },
                path::{ * },
                *,
            };
            /*
            use super::windows::Win32::UI::Shell;
            */

            pub fn home_dir()         -> Option<PathBuf> { known_folder_profile() }

            pub fn cache_dir()        -> Option<PathBuf> { data_local_dir() }
            pub fn config_dir()       -> Option<PathBuf> { known_folder_roaming_app_data() }
            pub fn config_local_dir() -> Option<PathBuf> { known_folder_local_app_data() }
            pub fn data_dir()         -> Option<PathBuf> { known_folder_roaming_app_data() }
            pub fn data_local_dir()   -> Option<PathBuf> { known_folder_local_app_data() }
            pub fn executable_dir()   -> Option<PathBuf> { None }
            pub fn preference_dir()   -> Option<PathBuf> { known_folder_local_app_data() }
            pub fn runtime_dir()      -> Option<PathBuf> { None }
            pub fn state_dir()        -> Option<PathBuf> { None }

            pub fn audio_dir()        -> Option<PathBuf> { known_folder_music() }
            pub fn desktop_dir()      -> Option<PathBuf> { known_folder_desktop() }
            pub fn document_dir()     -> Option<PathBuf> { known_folder_documents() }
            pub fn download_dir()     -> Option<PathBuf> { known_folder_downloads() }
            pub fn font_dir()         -> Option<PathBuf> { None }
            pub fn picture_dir()      -> Option<PathBuf> { known_folder_pictures() }
            pub fn public_dir()       -> Option<PathBuf> { known_folder_public()}
            pub fn template_dir()     -> Option<PathBuf> { known_folder_templates() }
            pub fn video_dir()        -> Option<PathBuf> { known_folder_videos() }
            
            pub fn known_folder(folder_id: windows::core::GUID) -> Option<PathBuf>
            {
                None
                /*
                unsafe
                {
                    let mut path_ptr: windows::core::PWSTR = std::ptr::null_mut();
                    let result = Shell::SHGetKnownFolderPath(
                        &folder_id,
                        0,
                        std::ptr::null_mut(),
                        &mut path_ptr
                    );
                    if result == 0 {
                        let len = windows::Win32::Globalization::lstrlenW(path_ptr) as usize;
                        let path = slice::from_raw_parts(path_ptr, len);
                        let ostr: OsString = OsStringExt::from_wide(path);
                        windows::Win32::System::Com::CoTaskMemFree(path_ptr as *const c_void);
                        Some(PathBuf::from(ostr))
                    } else {
                        windows::Win32::System::Com::CoTaskMemFree(path_ptr as *const c_void);
                        None
                    }
                }
                */
            }

            pub fn known_folder_profile() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Profile)
                None
            }

            pub fn known_folder_roaming_app_data() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_RoamingAppData)
                None
            }

            pub fn known_folder_local_app_data() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_LocalAppData)
                None
            }

            pub fn known_folder_music() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Music)
                None
            }

            pub fn known_folder_desktop() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Desktop)
                None
            }

            pub fn known_folder_documents() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Documents)
                None
            }

            pub fn known_folder_downloads() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Downloads)
                None
            }

            pub fn known_folder_pictures() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Pictures)
                None
            }

            pub fn known_folder_public() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Public)
                None
            }
            pub fn known_folder_templates() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Templates)
                None
            }
            pub fn known_folder_videos() -> Option<PathBuf> {
                // known_folder(Shell::FOLDERID_Videos)
                None
            }
        }
        /// Returns all XDG user directories obtained from $(XDG_CONFIG_HOME)/user-dirs.dirs.
        pub fn all(home_dir_path: &Path, user_dir_file_path: &Path) -> HashMap<String, PathBuf>
        {
            let bytes = read_all(user_dir_file_path).unwrap_or(Vec::new());
            parse_user_dirs(home_dir_path, None, &bytes)
        }
        /// Returns a single XDG user directory obtained from $(XDG_CONFIG_HOME)/user-dirs.dirs.
        pub fn single(home_dir_path: &Path, user_dir_file_path: &Path, user_dir_name: &str) -> HashMap<String, PathBuf>
        {
            let bytes = read_all(user_dir_file_path).unwrap_or(Vec::new());
            parse_user_dirs(home_dir_path, Some(user_dir_name), &bytes)
        }
        
        pub fn is_absolute_path(path: OsString) -> Option<PathBuf> 
        {
            let path = PathBuf::from(path);
            if path.is_absolute() {
                Some(path)
            } else {
                None
            }
        }

        fn parse_user_dirs(home_dir: &Path, user_dir: Option<&str>, bytes: &[u8]) -> HashMap<String, PathBuf>
        {
            let mut user_dirs = HashMap::new();

            for line in bytes.split(|b| *b == b'\n')
            {
                let mut single_dir_found = false;
                let (key, value) = match split_once(line, b'=') {
                    Some(kv) => kv,
                    None => continue,
                };

                let key = trim_blank(key);
                let key = if key.starts_with(b"XDG_") && key.ends_with(b"_DIR") {
                    match str::from_utf8(&key[4..key.len()-4]) {
                        Ok(key) =>
                            if user_dir.contains(&key) {
                                single_dir_found = true;
                                key
                            } else if user_dir.is_none() {
                                key
                            } else {
                                continue
                            },
                        Err(_)  => continue,
                    }
                } else {
                    continue
                };

                // xdg-user-dirs-update uses double quotes and we don't support anything else.
                let value = trim_blank(value);
                let mut value = if value.starts_with(b"\"") && value.ends_with(b"\"") {
                    &value[1..value.len()-1]
                } else {
                    continue
                };

                // Path should be either relative to the home directory or absolute.
                let is_relative = if value == b"$HOME/" {
                    // "Note: To disable a directory, point it to the homedir."
                    // Source: https://www.freedesktop.org/wiki/Software/xdg-user-dirs/
                    // Additionally directory is reassigned to homedir when removed.
                    continue
                } else if value.starts_with(b"$HOME/") {
                    value = &value[b"$HOME/".len()..];
                    true
                } else if value.starts_with(b"/") {
                    false
                } else {
                    continue
                };

                let value = OsString::from_vec(shell_unescape(value));

                let path = if is_relative {
                    let mut path = PathBuf::from(&home_dir);
                    path.push(value);
                    path
                } else {
                    PathBuf::from(value)
                };

                user_dirs.insert(key.to_owned(), path);
                if single_dir_found {
                    break;
                }
            }

            user_dirs
        }
        /// Reads the entire contents of a file into a byte vector.
        fn read_all(path: &Path) -> io::Result<Vec<u8>> 
        {
            let mut file = fs::File::open(path)?;
            let mut bytes = Vec::with_capacity(1024);
            file.read_to_end(&mut bytes)?;
            Ok(bytes)
        }
        /// Returns bytes before and after first occurrence of separator.
        fn split_once(bytes: &[u8], separator: u8) -> Option<(&[u8], &[u8])> 
        {
            bytes.iter().position(|b| *b == separator).map(|i| {
                (&bytes[..i], &bytes[i+1..])
            })
        }
        /// Returns a slice with leading and trailing <blank> characters removed.
        fn trim_blank(bytes: &[u8]) -> &[u8] 
        {
            // Trim leading <blank> characters.
            let i = bytes.iter().cloned().take_while(|b| *b == b' ' || *b == b'\t').count();
            let bytes = &bytes[i..];

            // Trim trailing <blank> characters.
            let i = bytes.iter().cloned().rev().take_while(|b| *b == b' ' || *b == b'\t').count();
            &bytes[..bytes.len()-i]
        }
        /// Unescape bytes escaped with POSIX shell double-quotes rules (as used by xdg-user-dirs-update).
        fn shell_unescape(escaped: &[u8]) -> Vec<u8> 
        {
            // We assume that byte string was created by xdg-user-dirs-update which
            // escapes all characters that might potentially have special meaning,
            // so there is no need to check if backslash is actually followed by
            // $ ` " \ or a <newline>.

            let mut unescaped: Vec<u8> = Vec::with_capacity(escaped.len());
            let mut i = escaped.iter().cloned();

            while let Some(b) = i.next() {
                if b == b'\\' {
                    if let Some(b) = i.next() {
                        unescaped.push(b);
                    }
                } else {
                    unescaped.push(b);
                }
            }

            unescaped
        }
    }
    #[cfg(unix)] pub use self::system::unix as sys;
    #[cfg(windows)] pub use self::system::windows as sys;
    pub use self::sys::{ * };
}
/**/
pub mod prompt
{
    use ::
    {
        prompt::
        {
            main::{ get_prompt_string, render_prompt },
        },
        *,
    };
    /*
    linereader v0.0.0*/
    pub mod lines
    {
        //     use lineread::{Command, Interface, ReadResult};
        pub mod command
        {
            //! Defines the set of line editing commands
            use ::
            {
                borrow::Cow::{ self, Borrowed, Owned },
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
                                Command::Macro(ref s) => write!(f, "\"{}\"", ::char::escape_sequence(s) )
                            }
                        }
                    }

                    impl Command
                    {
                        /// Constructs a command from a `'static str` reference.
                        pub fn from_str(name: &'static str) -> Command
                        {
                            Command::opt_from_str(name).unwrap_or_else(|| Command::Custom(Borrowed(name)))
                        }
                        /// Constructs a command from a non-`'static` string-like type.
                        pub fn from_string<T>(name: T) -> Command where 
                        T: AsRef<str> + Into<String>
                        {
                            Command::opt_from_str(name.as_ref()).unwrap_or_else(|| Command::Custom(Owned(name.into())))
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

            /// Describes the category of a command.
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
        }
        pub use self::command::{ Command };

        pub mod complete
        {
            //! Provides utilities for implementing word completion
            use ::
            {
                borrow::Cow::{ self, Borrowed, Owned },
                fs::{ read_dir },
                path::{is_separator, MAIN_SEPARATOR},
                prompt::lines::{ Prompter, Terminals },
                *,
            };
            /// Represents a single possible completion
            #[derive(Clone, Debug)]
            pub struct Completion
            {
                /// Whole completion text
                pub completion: String,
                /// Listing display string; `None` if matches completion
                pub display: Option<String>,
                /// Completion suffix; replaces append character
                pub suffix: Suffix,
            }
            /// Specifies an optional suffix to override the default value
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub enum Suffix
            {
                /// Use the default suffix
                Default,
                /// Use no suffix
                None,
                /// Use the given suffix
                Some(char),
            }

            impl Completion
            {
                /// Returns a simple `Completion` value, with display string matching
                /// completion and using the default completion suffix.
                pub fn simple(s: String) -> Completion
                {
                    Completion{
                        completion: s,
                        display: None,
                        suffix: Suffix::default(),
                    }
                }

                /// Returns the full completion string, including suffix, using the given
                /// default suffix if one is not assigned to this completion.
                pub fn completion(&self, def_suffix: Option<char>) -> Cow<str> {
                    let mut s = Borrowed(&self.completion[..]);

                    if let Some(suffix) = self.suffix.with_default(def_suffix) {
                        s.to_mut().push(suffix);
                    }

                    s
                }

                /// Returns the display string, including suffix
                pub fn display(&self) -> Cow<str> {
                    let mut s = Borrowed(self.display_str());

                    if let Suffix::Some(suffix) = self.suffix {
                        s.to_mut().push(suffix);
                    }

                    s
                }

                /// Returns the number of characters displayed
                pub fn display_chars(&self) -> usize {
                    let n = self.display_str().chars().count();
                    n + if self.suffix.is_some() { 1 } else { 0 }
                }

                fn display_str(&self) -> &str {
                    match self.display {
                        Some(ref dis) => dis,
                        None => &self.completion
                    }
                }
            }

            impl Suffix {
                /// Returns whether the `Suffix` value is the `Default` variant.
                pub fn is_default(&self) -> bool {
                    match *self {
                        Suffix::Default => true,
                        _ => false
                    }
                }

                /// Returns whether the `Suffix` value is the `Some(_)` variant.
                pub fn is_some(&self) -> bool {
                    match *self {
                        Suffix::Some(_) => true,
                        _ => false
                    }
                }

                /// Returns whether the `Suffix` value is the `None` variant.
                pub fn is_none(&self) -> bool {
                    match *self {
                        Suffix::None => true,
                        _ => false
                    }
                }

                /// Returns an `Option<char>`, using the given value in place of `Default`.
                pub fn with_default(self, default: Option<char>) -> Option<char> {
                    match self {
                        Suffix::None => None,
                        Suffix::Some(ch) => Some(ch),
                        Suffix::Default => default
                    }
                }
            }

            impl Default for Suffix {
                fn default() -> Suffix {
                    Suffix::Default
                }
            }

            /// Performs completion for `Prompter` when triggered by a user input sequence
            pub trait Completer<Term: Terminals>: Send + Sync {
                /// Returns the set of possible completions for the prefix `word`.
                fn complete(&self, word: &str, prompter: &Prompter<Term>,
                    start: usize, end: usize) -> Option<Vec<Completion>>;

                /// Returns the starting position of the word under the cursor.
                ///
                /// The default implementation uses `Prompter::word_break_chars()` to
                /// detect the start of a word.
                fn word_start(&self, line: &str, end: usize, prompter: &Prompter<Term>) -> usize {
                    word_break_start(&line[..end], prompter.word_break_chars())
                }

                /// Quotes a possible completion for insertion into input.
                ///
                /// The default implementation returns the word, as is.
                fn quote<'a>(&self, word: &'a str) -> Cow<'a, str> { Borrowed(word) }

                /// Unquotes a piece of user input before searching for completions.
                ///
                /// The default implementation returns the word, as is.
                fn unquote<'a>(&self, word: &'a str) -> Cow<'a, str> { Borrowed(word) }
            }

            /// `Completer` type that performs no completion
            ///
            /// This is the default `Completer` for a new `Prompter` instance.
            pub struct DummyCompleter;

            impl<Term: Terminals> Completer<Term> for DummyCompleter {
                fn complete(&self, _word: &str, _reader: &Prompter<Term>,
                        _start: usize, _end: usize) -> Option<Vec<Completion>> { None }
            }

            /// Performs completion by searching for filenames matching the word prefix.
            pub struct PathCompleter;

            impl<Term: Terminals> Completer<Term> for PathCompleter
            {
                fn complete(&self, word: &str, _reader: &Prompter<Term>, _start: usize, _end: usize)
                        -> Option<Vec<Completion>> {
                    Some(complete_path(word))
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

            /// Returns a sorted list of paths whose prefix matches the given path.
            pub fn complete_path(path: &str) -> Vec<Completion>
            {
                let (base_dir, fname) = split_path(path);
                let mut res = Vec::new();

                let lookup_dir = base_dir.unwrap_or(".");

                if let Ok(list) = read_dir(lookup_dir)
                {
                    for ent in list
                    {
                        if let Ok(ent) = ent
                        {
                            let ent_name = ent.file_name();

                            // TODO: Deal with non-UTF8 paths in some way
                            if let Ok(path) = ent_name.into_string()
                            {
                                if path.starts_with(fname)
                                {
                                    let (name, display) = if let Some(dir) = base_dir
                                    {
                                        (format!("{}{}{}", dir, MAIN_SEPARATOR, path),
                                            Some(path))
                                    } else { (path, None) };

                                    let is_dir = ent.metadata().ok().map_or(false, |m| m.is_dir());

                                    let suffix = if is_dir { Suffix::Some(MAIN_SEPARATOR) }
                                    
                                    else { Suffix::Default };

                                    res.push(Completion
                                    {
                                        completion: name,
                                        display: display,
                                        suffix: suffix,
                                    });
                                }
                            }
                        }
                    }
                }

                res.sort_by(|a, b| a.display_str().cmp(b.display_str()));
                res
            }

            /// Returns the start position of the word that ends at the end of the string.
            pub fn word_break_start(s: &str, word_break: &str) -> usize
            {
                let mut start = s.len();

                for (idx, ch) in s.char_indices().rev() {
                    if word_break.contains(ch) {
                        break;
                    }
                    start = idx;
                }

                start
            }

            /// Returns the start position of a word with non-word characters escaped by backslash (`\\`).
            pub fn escaped_word_start(s: &str) -> usize
            {
                let mut chars = s.char_indices().rev();
                let mut start = s.len();

                while let Some((idx, ch)) = chars.next() {
                    if needs_escape(ch) {
                        let n = {
                            let mut n = 0;

                            loop {
                                let mut clone = chars.clone();

                                let ch = match clone.next() {
                                    Some((_, ch)) => ch,
                                    None => break
                                };

                                if ch == '\\' {
                                    chars = clone;
                                    n += 1;
                                } else {
                                    break;
                                }
                            }

                            n
                        };

                        if n % 2 == 0 {
                            break;
                        }
                    }

                    start = idx;
                }

                start
            }

            /// Escapes a word by prefixing a backslash (`\\`) to non-word characters.
            pub fn escape(s: &str) -> Cow<str> 
            {
                let n = s.chars().filter(|&ch| needs_escape(ch)).count();

                if n == 0 {
                    Borrowed(s)
                } else {
                    let mut res = String::with_capacity(s.len() + n);

                    for ch in s.chars() {
                        if needs_escape(ch) {
                            res.push('\\');
                        }
                        res.push(ch);
                    }

                    Owned(res)
                }
            }

            /// Unescapes a word by removing the backslash (`\\`) from escaped characters.
            pub fn unescape(s: &str) -> Cow<str>
            {
                if s.contains('\\') {
                    let mut res = String::with_capacity(s.len());
                    let mut chars = s.chars();

                    while let Some(ch) = chars.next() {
                        if ch == '\\' {
                            if let Some(ch) = chars.next() {
                                res.push(ch);
                            }
                        } else {
                            res.push(ch);
                        }
                    }

                    Owned(res)
                } else {
                    Borrowed(s)
                }
            }

            fn needs_escape(ch: char) -> bool
            {
                match ch
                {
                    ' ' | '\t' | '\n' | '\\' => true,
                    _ => false
                }
            }

            fn split_path(path: &str) -> (Option<&str>, &str)
            {
                match path.rfind(is_separator)
                {
                    Some(pos) => (Some(&path[..pos]), &path[pos + 1..]),
                    None => (None, path)
                }
            }
        }
        pub use self::complete::{ Completion };

        pub mod function
        {
            //! Provides the `Function` trait for implementing custom `Prompter` commands
            use ::
            {
                prompt::lines::
                {
                    command::Category,
                    prompter::Prompter,
                    terminal::Terminal,
                },
                *,
            };
            /// Implements custom functionality for a `Prompter` command
            pub trait Function<Term: Terminal>: Send + Sync 
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
                { self(prompter, count, ch) }
            }
        }
        pub use self::function::{ Function };

        pub mod inputrc
        {
            //! Parses configuration files in the format of GNU Readline `inputrc`
            use ::
            {
                char::{ from_u32, ctrl, meta, parse_char_name },
                fs::{ File },
                io::{ stderr, Read, Write },
                path::{ Path },
                prompt::lines::{ Command },
                str::{Chars, Lines},
                *,
            };
            /// Parsed configuration directive
            #[derive(Clone, Debug)]
            pub enum Directive
            {
                /// Bind construct; `"input-sequence": command-or-macro`
                Bind(String, Command),
                /// Conditional construct;
                /// (`$if name=value` or `$if value`) *directives*
                /// (optional `$else` *directives*) `$endif`
                Conditional
                {
                    /// Value name; if `None`, value refers to application name
                    name: Option<String>,
                    /// Value to compare
                    value: String,
                    /// Group of directives evaluated when condition is true
                    then_group: Vec<Directive>,
                    /// Group of directives evaluated when condition is false
                    else_group: Vec<Directive>,
                },
                /// Set variable; `set name value`
                SetVariable(String, String),
            }

            /// Parses the named file and returns contained directives.
            ///
            /// If the file cannot be opened, `None` is returned and an error is printed
            /// to `stderr`. If any errors are encountered during parsing, they are printed
            /// to `stderr`.
            pub fn parse_file<P: ?Sized>(filename: &P) -> Option<Vec<Directive>>
                    where P: AsRef<Path> {
                let filename = filename.as_ref();

                let mut f = match File::open(filename) {
                    Ok(f) => f,
                    Err(e) => {
                        let _ = writeln!(stderr(), "linefeed: {}: {}", filename.display(), e);
                        return None;
                    }
                };

                let mut buf = String::new();

                if let Err(e) = f.read_to_string(&mut buf) {
                    let _ = writeln!(stderr(), "{}: {}", filename.display(), e);
                    return None;
                }

                Some(parse_text(filename, &buf))
            }

            /// Parses some text and returns contained directives.
            ///
            /// If any errors are encountered during parsing, they are printed to `stderr`.
            pub fn parse_text<P: ?Sized>(name: &P, line: &str) -> Vec<Directive>
                    where P: AsRef<Path> {
                let mut p = Parser::new(name.as_ref(), line);
                p.parse()
            }

            struct Parser<'a> {
                lines: Lines<'a>,
                filename: &'a Path,
                line_num: usize,
            }

            enum Token<'a> {
                /// Colon; `:`
                Colon,
                /// Equal; `=`
                Equal,
                /// Conditional or other special directive; `$word`
                SpecialWord(&'a str),
                /// Double-quoted string; `"foo"`
                String(String),
                /// Bare word; `foo`
                Word(&'a str),
                /// Invalid token
                Invalid,
            }

            impl<'a> Parser<'a> {
                pub fn new(filename: &'a Path, text: &'a str) -> Parser<'a> {
                    Parser{
                        lines: text.lines(),
                        filename: filename,
                        line_num: 0,
                    }
                }

                fn next_line(&mut self) -> Option<&'a str> {
                    self.lines.next().map(|line| {
                        self.line_num += 1;
                        line.trim()
                    })
                }

                fn parse(&mut self) -> Vec<Directive> {
                    let mut dirs = Vec::new();

                    while let Some(line) = self.next_line() {
                        if line.starts_with('#') {
                            continue;
                        }

                        let mut tokens = Tokens::new(line);

                        if let Some(Token::SpecialWord("include")) = tokens.next() {
                            let path = tokens.line;

                            if let Some(d) = parse_file(Path::new(path)) {
                                dirs.extend(d);
                            }

                            continue;
                        }

                        if let Some(dir) = self.parse_line(line) {
                            dirs.push(dir);
                        }
                    }

                    dirs
                }

                fn parse_conditional(&mut self) -> (Vec<Directive>, Vec<Directive>) {
                    let mut then_group = Vec::new();
                    let mut else_group = Vec::new();
                    let mut parse_else = false;

                    loop {
                        let line = match self.next_line() {
                            Some(line) => line,
                            None => {
                                self.error("missing $endif directive");
                                break;
                            }
                        };

                        if line.starts_with('#') {
                            continue;
                        }

                        let mut tokens = Tokens::new(line);

                        let start = match tokens.next() {
                            Some(tok) => tok,
                            None => continue
                        };

                        match start {
                            Token::SpecialWord("else") => {
                                if parse_else {
                                    self.error("duplicate $else directive");
                                } else {
                                    parse_else = true;
                                }
                            }
                            Token::SpecialWord("endif") => {
                                break;
                            }
                            _ => {
                                if let Some(dir) = self.parse_line(line) {
                                    if parse_else {
                                        else_group.push(dir);
                                    } else {
                                        then_group.push(dir);
                                    }
                                }
                            }
                        }
                    }

                    (then_group, else_group)
                }

                fn parse_line(&mut self, line: &str) -> Option<Directive> {
                    let mut tokens = Tokens::new(line);

                    let start = tokens.next()?;

                    let dir = match start {
                        Token::SpecialWord("if") => {
                            let name = match tokens.next() {
                                Some(Token::Word(w)) => w,
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            };

                            let (name, value) = match tokens.next() {
                                Some(Token::Equal) => {
                                    let value = match tokens.next() {
                                        Some(Token::Word(w)) => w,
                                        None => "",
                                        _ => {
                                            self.invalid();
                                            return None;
                                        }
                                    };

                                    (Some(name), value)
                                }
                                None => (None, name),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            };

                            let (then_group, else_group) = self.parse_conditional();

                            Directive::Conditional{
                                name: name.map(|s| s.to_owned()),
                                value: value.to_owned(),
                                then_group: then_group,
                                else_group: else_group,
                            }
                        }
                        Token::SpecialWord("else") => {
                            self.error("$else without matching $if directive");
                            return None;
                        }
                        Token::SpecialWord("endif") => {
                            self.error("$endif without matching $if directive");
                            return None;
                        }
                        Token::String(seq) => {
                            match tokens.next() {
                                Some(Token::Colon) => (),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            }

                            match tokens.next() {
                                Some(Token::Word(value)) =>
                                    Directive::Bind(seq, Command::from_string(value)),
                                Some(Token::String(out)) =>
                                    Directive::Bind(seq, Command::Macro(out.to_owned().into())),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            }
                        }
                        Token::Word("set") => {
                            let name = match tokens.next() {
                                Some(Token::Word(w)) => w,
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            };

                            let rest = tokens.line;

                            let value = match tokens.next() {
                                Some(Token::String(s)) => s,
                                Some(Token::Word(_)) => rest.to_owned(),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            };

                            Directive::SetVariable(name.to_owned(), value)
                        }
                        Token::Word(name) => {
                            match tokens.next() {
                                Some(Token::Colon) => (),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            }

                            let seq = match parse_char_name(name) {
                                Some(seq) => seq,
                                None => {
                                    self.invalid();
                                    return None;
                                }
                            };

                            match tokens.next() {
                                Some(Token::Word(value)) =>
                                    Directive::Bind(seq, Command::from_string(value)),
                                Some(Token::String(macro_seq)) =>
                                    Directive::Bind(seq, Command::Macro(macro_seq.to_owned().into())),
                                _ => {
                                    self.invalid();
                                    return None;
                                }
                            }
                        }
                        _ => {
                            self.invalid();
                            return None;
                        }
                    };

                    Some(dir)
                }

                fn error(&self, msg: &str) {
                    let _ = writeln!(stderr(),
                        "linefeed: {} line {}: {}", self.filename.display(), self.line_num, msg);
                }

                fn invalid(&self) {
                    self.error("invalid directive");
                }
            }

            struct Tokens<'a> {
                line: &'a str,
            }

            impl<'a> Tokens<'a> {
                fn new(line: &str) -> Tokens {
                    Tokens{
                        line: line,
                    }
                }
            }

            impl<'a> Iterator for Tokens<'a> {
                type Item = Token<'a>;

                fn next(&mut self) -> Option<Token<'a>> {
                    let ch = self.line.chars().next()?;

                    let tok = match ch {
                        ':' => {
                            self.line = self.line[1..].trim_start();
                            Token::Colon
                        }
                        '=' => {
                            self.line = self.line[1..].trim_start();
                            Token::Equal
                        }
                        '$' => {
                            let (word, rest) = parse_word(&self.line[1..]);
                            self.line = rest.trim_start();
                            Token::SpecialWord(word)
                        }
                        '"' => {
                            let (tok, rest) = parse_string(self.line);
                            self.line = rest.trim_start();
                            tok
                        }
                        _ => {
                            let (word, rest) = parse_word(self.line);
                            self.line = rest.trim_start();
                            Token::Word(word)
                        }
                    };

                    Some(tok)
                }
            }

            fn parse_escape(chars: &mut Chars) -> Option<String> {
                let ch = chars.next()?;

                let esc = match ch {
                    'C'  => {
                        match chars.next() {
                            Some('-') => (),
                            _ => return None
                        }
                        ctrl(chars.next()?)
                    }
                    'M'  => {
                        match chars.next() {
                            Some('-') => (),
                            _ => return None
                        }
                        return Some(meta(chars.next()?));
                    }
                    'e'  => '\x1b',
                    '\\' => '\\',
                    '"'  => '"',
                    '\'' => '\'',
                    'a'  => '\x07',
                    'b'  => '\x08',
                    'd'  => '\x7f',
                    'f'  => '\x0c',
                    'n'  => '\n',
                    'r'  => '\r',
                    't'  => '\t',
                    'u'  => {
                        match chars.next() {
                            Some('{') => (),
                            _ => return None
                        }

                        let mut n = 0;

                        for _ in 0..6 {
                            match chars.clone().next().and_then(|ch| ch.to_digit(16)) {
                                Some(digit) => {
                                    chars.next();
                                    n *= 16;
                                    n += digit;
                                }
                                None => break
                            }
                        }

                        match chars.next() {
                            Some('}') => (),
                            _ => return None
                        }

                        from_u32(n)?
                    }
                    'v'  => '\x0b',
                    'x'  => {
                        let mut n = 0;

                        for _ in 0..2 {
                            // Peek the next character
                            let digit = chars.clone().next()?.to_digit(16)? as u8;

                            // Consume if valid
                            chars.next();

                            n <<= 4;
                            n |= digit;
                        }

                        n as char
                    }
                    '0' ..= '3' => {
                        let mut n = ch as u8 - b'0';

                        for _ in 0..2 {
                            // Peek the next character
                            let digit = chars.clone().next()?.to_digit(8)? as u8;

                            // Consume if valid
                            chars.next();

                            n <<= 3;
                            n |= digit;
                        }

                        n as char
                    }
                    _ => return None
                };

                Some(esc.to_string())
            }

            fn parse_string(s: &str) -> (Token, &str) {
                let mut chars = s.chars();
                let mut res = String::new();

                // Skip open quote
                chars.next();

                while let Some(ch) = chars.next() {
                    match ch {
                        '"' => return (Token::String(res), chars.as_str()),
                        '\\' => {
                            match parse_escape(&mut chars) {
                                Some(esc) => {
                                    res.push_str(&esc);
                                }
                                None => break
                            }
                        }
                        ch => res.push(ch)
                    }
                }

                (Token::Invalid, "")
            }

            fn parse_word(s: &str) -> (&str, &str) {
                let mut chars = s.char_indices();

                loop {
                    let mut clone = chars.clone();

                    match clone.next() {
                        Some((ind, ch)) if ch == ':' || ch == '"' || ch == '=' ||
                                ch.is_whitespace() => {
                            return (&s[..ind], &s[ind..]);
                        }
                        None => {
                            return (s, "");
                        }
                        _ => ()
                    }

                    chars = clone;
                }
            }
        }
        pub use self::inputrc::{ Directive };

        pub mod interface
        {
            // Provides the main interface to interactive input reader
            use ::
            {
                borrow::{ Cow },
                fs::{File, OpenOptions},
                io::{ self, BufRead, BufReader, BufWriter, Read as _, Seek, SeekFrom, Write as _, },
                path::{ Path },
                prompt::lines::
                {
                    function::{ Function },
                    reader::{Read, Reader, ReadLock, ReadResult},
                    terminal::{ DefaultTerminal, Terminals }
                },
                sync::{ Arc, Mutex, MutexGuard },
                time::{ Duration },
                *,
            };
            /*
                use crate::command::Command;
                use crate::complete::{Completer};
                use crate::function::Function;
                use crate::inputrc::Directive;
                use crate::reader::{Read, Reader, ReadLock, ReadResult};
                use crate::terminal::{DefaultTerminal, Signal, Terminal};
                use crate::variables::Variable;
                use crate::writer::{Write, Writer, WriteLock};
            */
            /// The main interface to input reading and other terminal operations.
            pub struct Interface<Term: Terminals>
            {
                term: Term,
                write: Mutex<Write>,
                read: Mutex<Read<Term>>,
            }
            /*
                impl Interface<DefaultTerminal>
                {
                    /// Creates a new `Interface` with the given application name.
                    ///
                    /// `application` is a string containing the name of the application.
                    /// This can be used in user configurations to specify behavior for
                    /// particular applications.
                    ///
                    /// The default terminal interface is used.
                    pub fn new<T>(application: T) -> io::Result<Interface<DefaultTerminal>>
                            where T: Into<Cow<'static, str>> {
                        let term = DefaultTerminal::new()?;
                        Interface::with_term(application, term)
                    }
                }

                impl<Term: Terminal> Interface<Term>
                {
                    /// Creates a new `Interface` instance with a particular terminal implementation.
                    ///
                    /// To use the default terminal interface, call `Interface::new` instead.
                    pub fn with_term<T>(application: T, term: Term) -> io::Result<Interface<Term>>
                            where T: Into<Cow<'static, str>> {
                        let size = term.lock_write().size()?;
                        let read = Read::new(&term, application.into());

                        Ok(Interface{
                            term: term,
                            write: Mutex::new(Write::new(size)),
                            read: Mutex::new(read),
                        })
                    }

                    /// Acquires the read lock and returns a `Reader` instance.
                    ///
                    /// The `Reader` instance allows exclusive access to variables, bindings,
                    /// and command implementations.
                    pub fn lock_reader(&self) -> Reader<Term> {
                        Reader::new(self, self.lock_read())
                    }

                    /// Acquires the write lock and returns a `Writer` instance.
                    ///
                    /// If a `read_line` call is in progress, this method will move the cursor
                    /// to a new line after the prompt, allowing output to be written without
                    /// corrupting the prompt text. The prompt will be redrawn when the `Writer`
                    /// instance is dropped.
                    ///
                    /// To instead erase the prompt and write text, use [`lock_writer_erase`].
                    ///
                    /// [`lock_writer_erase`]: #method.lock_writer_erase
                    pub fn lock_writer_append(&self) -> io::Result<Writer<Term>> {
                        Writer::with_lock(self.lock_write(), false)
                    }

                    /// Acquires the write lock and returns a `Writer` instance.
                    ///
                    /// If a `read_line` call is in progress, this method will erase the prompt,
                    /// allowing output to be written without corrupting the prompt text.
                    /// The prompt will be redrawn when the `Writer` instance is dropped.
                    ///
                    /// To instead write text after the prompt, use [`lock_writer_append`].
                    ///
                    /// [`lock_writer_append`]: #method.lock_writer_append
                    pub fn lock_writer_erase(&self) -> io::Result<Writer<Term>> {
                        Writer::with_lock(self.lock_write(), true)
                    }

                    fn lock_read(&self) -> ReadLock<Term> {
                        ReadLock::new(
                            self.term.lock_read(),
                            self.read.lock().expect("Interface::lock_read"))
                    }

                    pub fn lock_write(&self) -> WriteLock<Term> {
                        WriteLock::new(
                            self.term.lock_write(),
                            self.write.lock().expect("Interface::lock_write"))
                    }

                    pub fn lock_write_data(&self) -> MutexGuard<Write> {
                        self.write.lock().expect("Interface::lock_write_data")
                    }
                }

                /// ## Locking
                ///
                /// The following methods internally acquire the read lock.
                ///
                /// The lock is released before the method returns.
                ///
                /// If the read lock is already held, e.g. because a `read_line` call is in
                /// progress, the method will block until the lock is released.
                impl<Term: Terminal> Interface<Term>
                {
                    /// Interactively reads a line from the terminal device.
                    ///
                    /// User input is collected until one of the following conditions is met:
                    ///
                    /// * If the user issues an end-of-file, `ReadResult::Eof` is returned.
                    /// * When the user inputs a newline (`'\n'`), the resulting input
                    ///   (not containing a trailing newline character) is returned as
                    ///   `ReadResult::Input(_)`.
                    /// * When a reported signal (see [`set_report_signal`]) is received,
                    ///   it is returned as `ReadResult::Signal(_)`. The `read_line` operation may
                    ///   then be either resumed with another call to `read_line` or ended by
                    ///   calling [`cancel_read_line`].
                    ///
                    /// [`cancel_read_line`]: #method.cancel_read_line
                    /// [`set_report_signal`]: #method.set_report_signal
                    pub fn read_line(&self) -> io::Result<ReadResult> {
                        self.lock_reader().read_line()
                    }

                    /// Performs one step of the interactive `read_line` loop.
                    ///
                    /// This method can be used to drive the `read_line` process asynchronously.
                    /// It will wait for input only up to the specified duration, then process
                    /// any available input from the terminal.
                    ///
                    /// If the user completes the input process, `Ok(Some(result))` is returned.
                    /// Otherwise, `Ok(None)` is returned to indicate that the interactive loop
                    /// may continue.
                    ///
                    /// The interactive prompt may be cancelled prematurely using the
                    /// [`cancel_read_line`] method.
                    ///
                    /// See [`read_line`] for details on the return value.
                    ///
                    /// [`cancel_read_line`]: #method.cancel_read_line
                    /// [`read_line`]: #method.read_line
                    pub fn read_line_step(&self, timeout: Option<Duration>)
                            -> io::Result<Option<ReadResult>> {
                        self.lock_reader().read_line_step(timeout)
                    }

                    /// Cancels an in-progress `read_line` operation.
                    ///
                    /// This method will reset internal data structures to their original state
                    /// and move the terminal cursor to a new, empty line.
                    ///
                    /// This method is called to prematurely end the interactive loop when
                    /// using the [`read_line_step`] method.
                    ///
                    /// It is not necessary to call this method if using the [`read_line`] method.
                    ///
                    /// [`read_line`]: #method.read_line
                    /// [`read_line_step`]: #method.read_line_step
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

                    /// Returns the value of the named variable or `None`
                    /// if no such variable exists.
                    pub fn get_variable(&self, name: &str) -> Option<Variable> {
                        self.lock_reader().get_variable(name)
                    }

                    /// Sets the value of the named variable and returns the previous
                    /// value.
                    ///
                    /// If `name` does not refer to a variable or the `value` is not
                    /// a valid value for the variable, `None` is returned.
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
                    ///
                    /// Returns the previously bound command.
                    pub fn bind_sequence<T>(&self, seq: T, cmd: Command) -> Option<Command>
                            where T: Into<Cow<'static, str>> {
                        self.lock_reader().bind_sequence(seq, cmd)
                    }

                    /// Binds a sequence to a command, if and only if the given sequence
                    /// is not already bound to a command.
                    ///
                    /// Returns `true` if a new binding was created.
                    pub fn bind_sequence_if_unbound<T>(&self, seq: T, cmd: Command) -> bool
                            where T: Into<Cow<'static, str>> {
                        self.lock_reader().bind_sequence_if_unbound(seq, cmd)
                    }

                    /// Removes a binding for the given sequence.
                    ///
                    /// Returns the previously bound command.
                    pub fn unbind_sequence(&self, seq: &str) -> Option<Command> {
                        self.lock_reader().unbind_sequence(seq)
                    }

                    /// Defines a named function to which sequences may be bound.
                    ///
                    /// The name should consist of lowercase ASCII letters and numbers,
                    /// containing no spaces, with words separated by hyphens. However,
                    /// this is not a requirement.
                    ///
                    /// Returns the function previously defined with the same name.
                    pub fn define_function<T>(&self, name: T, cmd: Arc<dyn Function<Term>>)
                            -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
                        self.lock_reader().define_function(name, cmd)
                    }

                    /// Removes a function defined with the given name.
                    ///
                    /// Returns the defined function.
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
                ///
                /// The following methods internally acquire the write lock.
                ///
                /// The lock is released before the method returns.
                ///
                /// If the write lock is already held, the method will block until it is released.
                impl<Term: Terminal> Interface<Term> 
                {
                    /// Returns the current input buffer.
                    pub fn buffer(&self) -> String {
                        self.lock_write().buffer.to_owned()
                    }

                    /// Returns the current number of history entries.
                    pub fn history_len(&self) -> usize {
                        self.lock_write().history_len()
                    }

                    /// Returns the maximum number of history entries.
                    ///
                    /// Not to be confused with [`history_len`], which returns the *current*
                    /// number of history entries.
                    ///
                    /// [`history_len`]: #method.history_len
                    pub fn history_size(&self) -> usize {
                        self.lock_write().history_size()
                    }

                    /// Save history entries to the specified file.
                    ///
                    /// If the file does not exist, it is created and all history entries are
                    /// written to the new file.
                    ///
                    /// If the file does exist, entries added since the last call to `save_history`
                    /// (or since the start of the application) are appended to the named file.
                    ///
                    /// If the file would contain more than `self.history_size()` entries,
                    /// it is first truncated, discarding the oldest entries.
                    pub fn save_history<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
                        let path = path.as_ref();
                        let mut w = self.lock_write();

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
                        let mut writer = self.lock_write();

                        let file = File::open(&path)?;
                        let rdr = BufReader::new(file);

                        for line in rdr.lines() {
                            writer.add_history(line?);
                        }

                        writer.reset_new_history();

                        Ok(())
                    }

                    /// Writes formatted text to the terminal display.
                    ///
                    /// This method enables `Interface` to be used as the receiver to
                    /// the [`writeln!`] macro.
                    ///
                    /// If the text contains any unprintable characters (e.g. escape sequences),
                    /// those characters will be escaped before printing.
                    ///
                    /// # Notes
                    ///
                    /// If this method is called during a [`read_line`] call, the prompt will
                    /// first be erased, then restored after the given string is printed.
                    /// Therefore, the written text should end with a newline. If the `writeln!`
                    /// macro is used, a newline is automatically added to the end of the text.
                    ///
                    /// To instead write text after the prompt, use [`lock_writer_append`].
                    ///
                    /// [`read_line`]: #method.read_line
                    /// [`writeln!`]: https://doc.rust-lang.org/std/macro.writeln.html
                    /// [`lock_writer_append`]: #method.lock_writer_append
                    pub fn write_fmt(&self, args: fmt::Arguments) -> io::Result<()> {
                        let s = args.to_string();
                        self.write_str(&s)
                    }

                    fn write_str(&self, line: &str) -> io::Result<()> {
                        self.lock_writer_erase()?.write_str(line)
                    }
                }

                /// ## Locking
                ///
                /// The following methods internally acquire both the read and write locks.
                ///
                /// The locks are released before the method returns.
                ///
                /// If either lock is already held, the method will block until it is released.
                impl<Term: Terminal> Interface<Term> 
                {
                    /// Sets the prompt that will be displayed when `read_line` is called.
                    ///
                    /// # Notes
                    ///
                    /// If `prompt` contains any terminal escape sequences (e.g. color codes),
                    /// such escape sequences should be immediately preceded by the character
                    /// `'\x01'` and immediately followed by the character `'\x02'`.
                    pub fn set_prompt(&self, prompt: &str) -> io::Result<()> {
                        self.lock_reader().set_prompt(prompt)
                    }

                    /// Sets the input buffer to the given string.
                    ///
                    /// # Notes
                    ///
                    /// To prevent invalidating the cursor, this method sets the cursor
                    /// position to the end of the new buffer.
                    pub fn set_buffer(&self, buf: &str) -> io::Result<()> {
                        self.lock_reader().set_buffer(buf)
                    }

                    /// Sets the cursor position in the input buffer.
                    ///
                    /// # Panics
                    ///
                    /// If the given position is out of bounds or not on a `char` boundary.
                    pub fn set_cursor(&self, pos: usize) -> io::Result<()> {
                        self.lock_reader().set_cursor(pos)
                    }

                    // History methods don't appear to require a read lock, but do acquire
                    // it nonetheless because any operation that truncates history may interefere
                    // with an ongoing `read_line` call. Therefore, the read lock is acquired
                    // to check whether a `read_line` call is in progress.

                    /// Adds a line to history.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn add_history(&self, line: String) {
                        self.lock_reader().add_history(line);
                    }

                    /// Adds a line to history, unless it is identical to the most recent entry.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn add_history_unique(&self, line: String) {
                        self.lock_reader().add_history_unique(line);
                    }

                    /// Removes all history entries.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn clear_history(&self) {
                        self.lock_reader().clear_history();
                    }

                    /// Removes the history entry at the given index.
                    ///
                    /// If the index is out of bounds, this method has no effect.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn remove_history(&self, idx: usize) {
                        self.lock_reader().remove_history(idx);
                    }

                    /// Sets the maximum number of history entries.
                    ///
                    /// If `n` is less than the current number of history entries,
                    /// the oldest entries are truncated to meet the given requirement.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn set_history_size(&self, n: usize) {
                        self.lock_reader().set_history_size(n);
                    }

                    /// Truncates history to the only the most recent `n` entries.
                    ///
                    /// If a `read_line` call is in progress, this method has no effect.
                    pub fn truncate_history(&self, n: usize) {
                        self.lock_reader().truncate_history(n);
                    }
                }
            */
        }
        pub use self::interface::{ Interface };

        pub mod prompter
        {
            //! Provides access to prompt input state
            use ::
            {
                char::{ is_ctrl, is_printable, DELETE, EOF },
                mem::replace,
                mortal::{ FindResult },
                ops::Range,
                prompt::lines::
                {
                    command::{ Category, Command },
                    function::{ Function },
                    reader::{ BindingIter, InputState, ReadLock, ReadResult },
                },
                sync::Arc,
                time::Instant,
                *,
            };
            /* 
            use crate::chars::{is_ctrl, is_printable, DELETE, EOF};
            use crate::complete::Completion;
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
                pub(crate) read: &'a mut ReadLock<'b, Term>,
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
                ///
                /// This method will erase the prompt, allowing output to be written
                /// without corrupting the prompt text. The prompt will be redrawn
                /// when the `Writer` instance is dropped.
                ///
                /// To instead write text after the prompt, use [`writer_append`].
                ///
                /// [`writer_append`]: #method.writer_append
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
                        InputState::CompleteIntro =>
                        {
                            match ch
                            {
                                'y' | 'Y' | ' ' =>
                                {
                                    self.write.write_str("\n")?;
                                    self.show_completions_page(0)?;
                                }

                                '\r' | '\n' =>
                                {
                                    self.write.write_str("\n")?;
                                    self.show_completions_line(0)?;
                                }

                                'q' | 'Q' | 'n' | 'N' | DELETE =>
                                {
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
                ///
                /// # Panics
                ///
                /// If the given position is out of bounds or is not aligned to `char` boundaries.
                pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
                    self.write.set_cursor(pos)
                }
                /// Sets the prompt that will be displayed when `read_line` is called.
                ///
                /// # Notes
                ///
                /// If `prompt` contains any terminal escape sequences (e.g. color codes),
                /// such escape sequences should be immediately preceded by the character
                /// `'\x01'` and immediately followed by the character `'\x02'`.
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
                ///
                /// If the user is not editing a line of history, `None` is returned.
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
                /// Execute  SelfInsert(...) on the first character in the input sequence, if it is printable.
                fn insert_first_char(&mut self) -> io::Result<()>
                {
                    let (first, rest) =
                    {
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
                    use super::command::Command::*;

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
                ///
                /// This method may be called by a [`Function`] implementation, immediately
                /// before ending execution, in order to simulate the `accept-line` command;
                /// e.g. to implement a command that extends the default behavior of the
                /// `accept-line` action.
                ///
                /// Behavior of this method is undefined if called outside of a `Function`
                /// implementation.
                ///
                /// [`Function`]: ../function/trait.Function.html
                pub fn accept_input(&mut self) -> io::Result<()> {
                    self.write.move_to_end()?;
                    self.write.write_str("\n")?;
                    self.read.input_accepted = true;
                    self.write.is_prompt_drawn = false;
                    Ok(())
                }

                /// Moves the cursor to the given position, waits for 500 milliseconds
                /// (or until next user input), then restores the original cursor position.
                ///
                /// # Panics
                ///
                /// If the given position is out of bounds or is not aligned to `char` boundaries.
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
                        // TODO: Replace borrowed data in `Table` with owned data.
                        // Then, store table here to avoid regenerating column widths
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
                ///
                /// # Panics
                ///
                /// If the given range is out of bounds or is not aligned to `char` boundaries.
                pub fn delete_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
                    self.write.delete_range(range)
                }

                /// Deletes a range from the buffer and adds the removed text to the
                /// kill ring.
                ///
                /// # Panics
                ///
                /// If the given range is out of bounds or is not aligned to `char` boundaries.
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
                /// The cursor is placed at the end of the new location of `src`.
                ///
                /// # Panics
                ///
                /// If `src` and `dest` overlap, are out of bounds,
                /// or are not aligned to `char` boundaries.
                pub fn transpose_range(&mut self, src: Range<usize>, dest: Range<usize>)
                        -> io::Result<()> {
                    self.write.transpose_range(src, dest)
                }

                /// Insert text from the front of the kill ring at the current cursor position.
                /// The cursor is placed at the end of the new text.
                pub fn yank(&mut self) -> io::Result<()> {
                    if let Some(kill) = self.read.kill_ring.front().cloned() {
                        let start = self.write.cursor;
                        self.read.last_yank = Some((start, start + kill.len()));

                        self.insert_str(&kill)?;
                    }

                    Ok(())
                }

                /// Rotates the kill ring and replaces yanked text with the new front.
                ///
                /// If the previous operation was not `yank`, this has no effect.
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
                ///
                /// The cursor position remains the same.
                pub fn insert(&mut self, n: usize, ch: char) -> io::Result<()> {
                    if n != 0 {
                        let s = repeat_char(ch, n);
                        self.insert_str(&s)?;
                    }

                    Ok(())
                }

                /// Insert a string at the current cursor position.
                ///
                /// The cursor is placed at the end of the new string.
                pub fn insert_str(&mut self, s: &str) -> io::Result<()> {
                    self.write.insert_str(s)
                }

                /// Replaces a range in the buffer and redraws.
                ///
                /// The cursor is placed at the start of the range.
                pub fn replace_str_backward<R: RangeArgument<usize>>(&mut self,
                        range: R, s: &str) -> io::Result<()> {
                    self.replace_str_impl(range, s)?;
                    let len = self.write.buffer.len();
                    self.write.move_from(len)
                }

                /// Replaces a range in the buffer and redraws.
                ///
                /// The cursor is placed at the end of the new string.
                pub fn replace_str_forward<R: RangeArgument<usize>>(&mut self,
                        range: R, s: &str) -> io::Result<()> {
                    self.replace_str_impl(range, s)?;
                    self.write.cursor += s.len();
                    let len = self.write.buffer.len();
                    self.write.move_from(len)
                }

                /// Replaces a range in the buffer and redraws.
                ///
                /// The cursor position is set to start of range, on-screen cursor remains
                /// at end of buffer.
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
        pub use self::prompter::{ Prompter };

        pub mod reader
        {
            //! Provides access to terminal read operations
            use ::
            {
                borrow::Cow,
                collections::{HashMap, VecDeque},
                mem::replace,
                mortal::SequenceMap,
                ops::{Deref, DerefMut},
                path::{Path, PathBuf},
                prompt::lines::
                {
                    command::{ Category, Command },
                    function::{ Function },
                    interface::{ Interface },
                    terminal::{ RawRead, Signal, SignalSet, Size, Terminals, TerminalReader },
                },
                sync::{Arc, MutexGuard},
                time::{Duration, Instant},
                *,
            };
            /*
            use crate::complete::{Completer, Completion, DummyCompleter};
            use crate::function::Function;
            use crate::inputrc::{parse_file, Directive};
            use crate::interface::Interface;
            use crate::prompter::Prompter;
            use crate::sys::path::{env_init_file, system_init_file, user_init_file};
            use crate::terminal::{
                RawRead, Signal, SignalSet, Size,
                Terminal, TerminalReader,
            };
            use crate::util::{first_char, match_name};
            use crate::variables::{Variable, Variables, VariableIter};
            */
            /// Default set of string characters
            pub const STRING_CHARS: &str = "\"'";
            /// Default set of word break characters
            pub const WORD_BREAK_CHARS: &str = " \t\n\"\\'`@$><=;|&{(";
            /// Indicates the start of a series of invisible characters in the prompt
            pub const START_INVISIBLE: char = '\x01';
            /// Indicates the end of a series of invisible characters in the prompt
            pub const END_INVISIBLE: char = '\x02';
            /// Maximum size of kill ring
            const MAX_KILLS: usize = 10;
            /// Provides access to data related to reading and processing user input.
            pub struct Reader<'a, Term: 'a + Terminals> 
            {
                iface: &'a Interface<Term>,
                lock: ReadLock<'a, Term>,
            }

            pub struct Read<Term: Terminals> 
            {
                /// Application name
                pub application: Cow<'static, str>,

                /// Pending input
                pub input_buffer: Vec<u8>,
                /// Pending macro sequence
                pub macro_buffer: String,
                pub bindings: SequenceMap<Cow<'static, str>, Command>,
                pub functions: HashMap<Cow<'static, str>, Arc<dyn Function<Term>>>,
                /// Current input sequence
                pub sequence: String,
                /// Whether newline has been received
                pub input_accepted: bool,
                /// Whether overwrite mode is currently active
                pub overwrite_mode: bool,
                /// Characters appended while in overwrite mode
                pub overwritten_append: usize,
                /// Characters overwritten in overwrite mode
                pub overwritten_chars: String,
                /// Configured completer
                pub completer: Arc<dyn Completer<Term>>,
                /// Character appended to completions
                pub completion_append_character: Option<char>,
                /// Current set of possible completions
                pub completions: Option<Vec<Completion>>,
                /// Current "menu-complete" entry being viewed:
                pub completion_index: usize,
                /// Start of the completed word
                pub completion_start: usize,
                /// Start of the inserted prefix of a completed word
                pub completion_prefix: usize,
                pub string_chars: Cow<'static, str>,
                pub word_break: Cow<'static, str>,
                pub last_cmd: Category,
                pub last_yank: Option<(usize, usize)>,
                pub kill_ring: VecDeque<String>,
                pub catch_signals: bool,
                pub ignore_signals: SignalSet,
                pub report_signals: SignalSet,
                pub last_resize: Option<Size>,
                pub last_signal: Option<Signal>,
                variables: Variables,
                pub state: InputState,
                pub max_wait_duration: Option<Duration>,
            }

            pub struct ReadLock<'a, Term: 'a + Terminals> 
            {
                term: Box<dyn TerminalReader<Term> + 'a>,
                data: MutexGuard<'a, Read<Term>>,
            }

            /// Returned from [`read_line`] to indicate user input
            #[derive(Debug)]
            pub enum ReadResult 
            {
                /// User issued end-of-file
                Eof,
                /// User input received
                Input(String),
                /// Reported signal was received
                Signal(Signal),
            }

            #[derive(Copy, Clone, Debug)]
            pub(crate) enum InputState 
            {
                Inactive,
                NewSequence,
                ContinueSequence{
                    expiry: Option<Instant>,
                },
                Number,
                CharSearch{
                    n: usize,
                    backward: bool,
                },
                TextSearch,
                CompleteIntro,
                CompleteMore(usize),
                QuotedInsert(usize),
            }

            impl<'a, Term: 'a + Terminals> Reader<'a, Term> 
            {
                pub fn new(iface: &'a Interface<Term>, lock: ReadLock<'a, Term>)
                        -> Reader<'a, Term> {
                    Reader{iface, lock}
                }
                /// Interactively reads a line from the terminal device.
                pub fn read_line(&mut self) -> io::Result<ReadResult> {
                    loop {
                        if let Some(res) = self.read_line_step(None)? {
                            return Ok(res);
                        }
                    }
                }
                /// Performs one step of the interactive `read_line` loop.
                pub fn read_line_step(&mut self, timeout: Option<Duration>)
                        -> io::Result<Option<ReadResult>> {
                    self.initialize_read_line()?;

                    let state = self.prepare_term()?;
                    let res = self.read_line_step_impl(timeout);
                    self.lock.term.restore(state)?;

                    res
                }

                /// Cancels an in-progress `read_line` operation.
                pub fn cancel_read_line(&mut self) -> io::Result<()> {
                    self.end_read_line()
                }

                fn initialize_read_line(&mut self) -> io::Result<()> {
                    if !self.lock.is_active() {
                        self.prompter().start_read_line()?;
                    }
                    Ok(())
                }

                fn read_line_step_impl(&mut self, timeout: Option<Duration>)
                        -> io::Result<Option<ReadResult>> {
                    let do_read = if self.lock.is_input_available() {
                        self.lock.term.wait_for_input(Some(Duration::from_secs(0)))?
                    } else {
                        let timeout = limit_duration(timeout, self.lock.max_wait_duration);
                        self.lock.term.wait_for_input(timeout)?
                    };

                    if do_read {
                        self.lock.read_input()?;
                    }

                    if let Some(size) = self.lock.take_resize() {
                        self.handle_resize(size)?;
                    }

                    if let Some(sig) = self.lock.take_signal() {
                        if self.lock.report_signals.contains(sig) {
                            return Ok(Some(ReadResult::Signal(sig)));
                        }
                        if !self.lock.ignore_signals.contains(sig) {
                            self.handle_signal(sig)?;
                        }
                    }

                    // Acquire the write lock and process all available input
                    {
                        let mut prompter = self.prompter();
                        prompter.check_expire_timeout()?;
                        let mut macro_len = prompter.read.data.macro_buffer.len();

                        while prompter.read.is_input_available() 
                        {
                            if let Some(ch) = prompter.read.read_char()? {
                                if let Some(r) = prompter.handle_input(ch)? {
                                    prompter.end_read_line()?;
                                    return Ok(Some(r));
                                }
                            }

                            let new_macro_len = prompter.read.data.macro_buffer.len();

                            if new_macro_len != 0 && new_macro_len >= macro_len {
                                break;
                            }

                            macro_len = new_macro_len;
                        }
                    }

                    Ok(None)
                }

                fn end_read_line(&mut self) -> io::Result<()> {
                    if self.lock.is_active() {
                        self.prompter().end_read_line()?;
                    }
                    Ok(())
                }

                fn prepare_term(&mut self) -> io::Result<Term::PrepareState> {
                    if self.read_next_raw() {
                        self.lock.term.prepare(true, SignalSet::new())
                    } else {
                        let mut signals = self.lock.report_signals.union(self.lock.ignore_signals);

                        if self.lock.catch_signals {
                            signals.insert(Signal::Interrupt);
                        }

                        let block_signals = !self.lock.catch_signals;

                        self.lock.term.prepare(block_signals, signals)
                    }
                }

                fn read_next_raw(&self) -> bool {
                    match self.lock.state {
                        InputState::QuotedInsert(_) => true,
                        _ => false
                    }
                }
                /// Sets the input buffer to the given string.
                pub fn set_buffer(&mut self, buf: &str) -> io::Result<()> {
                    if self.lock.is_active() {
                        self.prompter().set_buffer(buf)
                    } else {
                        self.iface.lock_write_data().set_buffer(buf);
                        Ok(())
                    }
                }
                /// Sets the cursor position in the input buffer.
                pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
                    if self.lock.is_active() {
                        self.prompter().set_cursor(pos)
                    } else {
                        self.iface.lock_write_data().set_cursor(pos);
                        Ok(())
                    }
                }
                /// Sets the prompt that will be displayed when `read_line` is called.
                pub fn set_prompt(&mut self, prompt: &str) -> io::Result<()> {
                    self.prompter().set_prompt(prompt)
                }
                /// Adds a line to history.
                pub fn add_history(&self, line: String) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().add_history(line);
                    }
                }
                /// Adds a line to history, unless it is identical to the most recent entry.
                pub fn add_history_unique(&self, line: String) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().add_history_unique(line);
                    }
                }
                /// Removes all history entries.
                pub fn clear_history(&self) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().clear_history();
                    }
                }
                /// Removes the history entry at the given index.
                pub fn remove_history(&self, idx: usize) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().remove_history(idx);
                    }
                }
                /// Sets the maximum number of history entries.
                pub fn set_history_size(&self, n: usize) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().set_history_size(n);
                    }
                }
                /// Truncates history to the only the most recent `n` entries.
                pub fn truncate_history(&self, n: usize) {
                    if !self.lock.is_active() {
                        self.iface.lock_write().truncate_history(n);
                    }
                }
                /// Returns the application name
                pub fn application(&self) -> &str {
                    &self.lock.application
                }
                /// Sets the application name
                pub fn set_application<T>(&mut self, application: T)
                        where T: Into<Cow<'static, str>> {
                    self.lock.application = application.into();
                }
                /// Returns a reference to the current completer instance.
                pub fn completer(&self) -> &Arc<dyn Completer<Term>> {
                    &self.lock.completer
                }
                /// Replaces the current completer, returning the previous instance.
                pub fn set_completer(&mut self, completer: Arc<dyn Completer<Term>>)
                        -> Arc<dyn Completer<Term>> {
                    replace(&mut self.lock.completer, completer)
                }
                /// Returns the value of the named variable or `None` if no such variable exists.
                pub fn get_variable(&self, name: &str) -> Option<Variable> {
                    self.lock.get_variable(name)
                }
                /// Sets the value of the named variable and returns the previous value.
                pub fn set_variable(&mut self, name: &str, value: &str) -> Option<Variable> {
                    self.lock.set_variable(name, value)
                }
                /// Returns an iterator over stored variables.
                pub fn variables(&self) -> VariableIter {
                    self.lock.variables.iter()
                }
                /// Returns whether to "blink" matching opening parenthesis character 
                /// when a closing parenthesis character is entered.
                pub fn blink_matching_paren(&self) -> bool {
                    self.lock.blink_matching_paren
                }
                /// Sets the `blink-matching-paren` variable.
                pub fn set_blink_matching_paren(&mut self, set: bool) {
                    self.lock.blink_matching_paren = set;
                }
                /// Returns whether `linefeed` will catch certain signals.
                pub fn catch_signals(&self) -> bool {
                    self.lock.catch_signals
                }
                /// Sets whether `linefeed` will catch certain signals.
                pub fn set_catch_signals(&mut self, enabled: bool) {
                    self.lock.catch_signals = enabled;
                }
                /// Returns whether the given `Signal` is ignored.
                pub fn ignore_signal(&self, signal: Signal) -> bool {
                    self.lock.ignore_signals.contains(signal)
                }
                /// Sets whether the given `Signal` will be ignored.
                pub fn set_ignore_signal(&mut self, signal: Signal, set: bool) {
                    if set {
                        self.lock.ignore_signals.insert(signal);
                        self.lock.report_signals.remove(signal);
                    } else {
                        self.lock.ignore_signals.remove(signal);
                    }
                }
                /// Returns whether the given `Signal` is to be reported.
                pub fn report_signal(&self, signal: Signal) -> bool {
                    self.lock.report_signals.contains(signal)
                }
                /// Sets whether to report the given `Signal`.
                pub fn set_report_signal(&mut self, signal: Signal, set: bool) {
                    if set {
                        self.lock.report_signals.insert(signal);
                        self.lock.ignore_signals.remove(signal);
                    } else {
                        self.lock.report_signals.remove(signal);
                    }
                }
                /// Returns whether Tab completion is disabled.
                pub fn disable_completion(&self) -> bool {
                    self.lock.disable_completion
                }
                /// Sets the `disable-completion` variable.
                pub fn set_disable_completion(&mut self, disable: bool) {
                    self.lock.disable_completion = disable;
                }
                /// When certain control characters are pressed, a character sequence
                /// equivalent to this character will be echoed.
                pub fn echo_control_characters(&self) -> bool {
                    self.lock.echo_control_characters
                }
                /// Sets the `echo-control-characters` variable.
                pub fn set_echo_control_characters(&mut self, echo: bool) {
                    self.lock.echo_control_characters = echo;
                }
                /// Returns the character, if any, that is appended to a successful completion.
                pub fn completion_append_character(&self) -> Option<char> {
                    self.lock.completion_append_character
                }
                /// Sets the character, if any, that is appended to a successful completion.
                pub fn set_completion_append_character(&mut self, ch: Option<char>) {
                    self.lock.completion_append_character = ch;
                }
                /// Returns the width of completion listing display.
                pub fn completion_display_width(&self) -> usize {
                    self.lock.completion_display_width
                }
                /// Sets the `completion-display-width` variable.
                pub fn set_completion_display_width(&mut self, n: usize) {
                    self.lock.completion_display_width = n;
                }
                /// Returns the minimum number of completion items that require user confirmation before listing.
                pub fn completion_query_items(&self) -> usize {
                    self.lock.completion_query_items
                }
                /// Sets the `completion-query-items` variable.
                pub fn set_completion_query_items(&mut self, n: usize) {
                    self.lock.completion_query_items = n;
                }
                /// Returns the timeout to wait for further user input when an ambiguous
                /// sequence has been entered. If the value is `None`, wait is indefinite.
                pub fn keyseq_timeout(&self) -> Option<Duration> {
                    self.lock.keyseq_timeout
                }
                /// Sets the `keyseq-timeout` variable.
                pub fn set_keyseq_timeout(&mut self, timeout: Option<Duration>) {
                    self.lock.keyseq_timeout = timeout;
                }
                /// Returns whether to list possible completions one page at a time.
                pub fn page_completions(&self) -> bool {
                    self.lock.page_completions
                }
                /// Sets the `page-completions` variable.
                pub fn set_page_completions(&mut self, set: bool) {
                    self.lock.page_completions = set;
                }
                /// Returns whether to list completions horizontally, rather than down the screen.
                pub fn print_completions_horizontally(&self) -> bool {
                    self.lock.print_completions_horizontally
                }
                /// Sets the `print-completions-horizontally` variable.
                pub fn set_print_completions_horizontally(&mut self, set: bool) {
                    self.lock.print_completions_horizontally = set;
                }
                /// Returns the set of characters that delimit strings.
                pub fn string_chars(&self) -> &str {
                    &self.lock.string_chars
                }
                /// Sets the set of characters that delimit strings.
                pub fn set_string_chars<T>(&mut self, chars: T)
                        where T: Into<Cow<'static, str>> {
                    self.lock.string_chars = chars.into();
                }
                /// Returns the set of characters that indicate a word break.
                pub fn word_break_chars(&self) -> &str {
                    &self.lock.word_break
                }
                /// Sets the set of characters that indicate a word break.
                pub fn set_word_break_chars<T>(&mut self, chars: T)
                        where T: Into<Cow<'static, str>> {
                    self.lock.word_break = chars.into();
                }
                /// Returns an iterator over bound sequences
                pub fn bindings(&self) -> BindingIter {
                    self.lock.bindings()
                }
                /// Binds a sequence to a command.
                pub fn bind_sequence<T>(&mut self, seq: T, cmd: Command) -> Option<Command>
                        where T: Into<Cow<'static, str>> {
                    self.lock.bind_sequence(seq, cmd)
                }
                /// Binds a sequence to a command, if and only if the given sequence
                /// is not already bound to a command.
                pub fn bind_sequence_if_unbound<T>(&mut self, seq: T, cmd: Command) -> bool
                        where T: Into<Cow<'static, str>> {
                    self.lock.bind_sequence_if_unbound(seq, cmd)
                }
                /// Removes a binding for the given sequence.
                pub fn unbind_sequence(&mut self, seq: &str) -> Option<Command> {
                    self.lock.unbind_sequence(seq)
                }
                /// Defines a named function to which sequences may be bound.
                pub fn define_function<T>(&mut self, name: T, cmd: Arc<dyn Function<Term>>)
                        -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
                    self.lock.define_function(name, cmd)
                }
                /// Removes a function defined with the given name.
                pub fn remove_function(&mut self, name: &str) -> Option<Arc<dyn Function<Term>>> {
                    self.lock.remove_function(name)
                }

                pub fn evaluate_directives(&mut self, term: &Term, dirs: Vec<Directive>) {
                    self.lock.data.evaluate_directives(term, dirs)
                }

                pub fn evaluate_directive(&mut self, term: &Term, dir: Directive) {
                    self.lock.data.evaluate_directive(term, dir)
                }

                fn prompter<'b>(&'b mut self) -> Prompter<'b, 'a, Term> {
                    Prompter::new(
                        &mut self.lock,
                        self.iface.lock_write())
                }

                fn handle_resize(&mut self, size: Size) -> io::Result<()> {
                    self.prompter().handle_resize(size)
                }

                fn handle_signal(&mut self, sig: Signal) -> io::Result<()> {
                    self.prompter().handle_signal(sig)
                }
            }

            impl<'a, Term: 'a + Terminals> ReadLock<'a, Term> {
                pub fn new(term: Box<dyn TerminalReader<Term> + 'a>, data: MutexGuard<'a, Read<Term>>)
                        -> ReadLock<'a, Term> {
                    ReadLock{term, data}
                }

                /// Reads the next character of input.
                ///
                /// Performs a non-blocking read from the terminal, if necessary.
                ///
                /// If non-input data was received (e.g. a signal) or insufficient input
                /// is available, `Ok(None)` is returned.
                pub fn read_char(&mut self) -> io::Result<Option<char>> {
                    if let Some(ch) = self.macro_pop() {
                        Ok(Some(ch))
                    } else if let Some(ch) = self.decode_input()? {
                        Ok(Some(ch))
                    } else {
                        Ok(None)
                    }
                }

                fn read_input(&mut self) -> io::Result<()> {
                    match self.term.read(&mut self.data.input_buffer)? {
                        RawRead::Bytes(_) => (),
                        RawRead::Resize(new_size) => {
                            self.last_resize = Some(new_size);
                        }
                        RawRead::Signal(sig) => {
                            self.last_signal = Some(sig);
                        }
                    }

                    Ok(())
                }

                fn is_input_available(&self) -> bool {
                    !self.data.macro_buffer.is_empty() || match self.peek_input() {
                        Ok(Some(_)) | Err(_) => true,
                        Ok(None) => false
                    }
                }

                fn macro_pop(&mut self) -> Option<char> {
                    if self.data.macro_buffer.is_empty() {
                        None
                    } else {
                        Some(self.data.macro_buffer.remove(0))
                    }
                }

                fn decode_input(&mut self) -> io::Result<Option<char>> {
                    let res = self.peek_input();

                    if let Ok(Some(ch)) = res {
                        self.data.input_buffer.drain(..ch.len_utf8());
                    }

                    res
                }

                fn peek_input(&self) -> io::Result<Option<char>> {
                    if self.data.input_buffer.is_empty() {
                        Ok(None)
                    } else {
                        first_char(&self.data.input_buffer)
                    }
                }

                pub fn reset_data(&mut self) {
                    self.data.reset_data();
                }
            }

            impl<'a, Term: 'a + Terminals> Deref for ReadLock<'a, Term> {
                type Target = Read<Term>;

                fn deref(&self) -> &Read<Term> {
                    &self.data
                }
            }

            impl<'a, Term: 'a + Terminals> DerefMut for ReadLock<'a, Term> {
                fn deref_mut(&mut self) -> &mut Read<Term> {
                    &mut self.data
                }
            }

            impl<Term: Terminals> Deref for Read<Term> {
                type Target = Variables;

                fn deref(&self) -> &Variables {
                    &self.variables
                }
            }

            impl<Term: Terminals> DerefMut for Read<Term> {
                fn deref_mut(&mut self) -> &mut Variables {
                    &mut self.variables
                }
            }

            impl<Term: Terminals> Read<Term> {
                pub fn new(term: &Term, application: Cow<'static, str>) -> Read<Term> {
                    let mut r = Read{
                        application,

                        bindings: default_bindings(),
                        functions: HashMap::new(),

                        input_buffer: Vec::new(),
                        macro_buffer: String::new(),

                        sequence: String::new(),
                        input_accepted: false,

                        overwrite_mode: false,
                        overwritten_append: 0,
                        overwritten_chars: String::new(),

                        completer: Arc::new(DummyCompleter),
                        completion_append_character: Some(' '),
                        completions: None,
                        completion_index: 0,
                        completion_start: 0,
                        completion_prefix: 0,

                        string_chars: STRING_CHARS.into(),
                        word_break: WORD_BREAK_CHARS.into(),

                        last_cmd: Category::Other,
                        last_yank: None,
                        kill_ring: VecDeque::with_capacity(MAX_KILLS),

                        catch_signals: true,
                        ignore_signals: SignalSet::new(),
                        report_signals: SignalSet::new(),
                        last_resize: None,
                        last_signal: None,

                        variables: Variables::default(),

                        state: InputState::Inactive,
                        max_wait_duration: None,
                    };

                    r.read_init(term);
                    r
                }

                pub fn bindings(&self) -> BindingIter {
                    BindingIter(self.bindings.sequences().iter())
                }

                pub fn variables(&self) -> VariableIter {
                    self.variables.iter()
                }

                fn take_resize(&mut self) -> Option<Size> {
                    self.last_resize.take()
                }

                fn take_signal(&mut self) -> Option<Signal> {
                    self.last_signal.take()
                }

                pub fn queue_input(&mut self, seq: &str) {
                    self.macro_buffer.insert_str(0, seq);
                }

                pub fn is_active(&self) -> bool {
                    match self.state {
                        InputState::Inactive => false,
                        _ => true
                    }
                }

                pub fn reset_data(&mut self) {
                    self.state = InputState::NewSequence;
                    self.input_accepted = false;
                    self.overwrite_mode = false;
                    self.overwritten_append = 0;
                    self.overwritten_chars.clear();
                    self.sequence.clear();

                    self.completions = None;

                    self.last_cmd = Category::Other;
                    self.last_yank = None;

                    self.last_resize = None;
                    self.last_signal = None;
                }

                pub fn bind_sequence<T>(&mut self, seq: T, cmd: Command) -> Option<Command>
                        where T: Into<Cow<'static, str>> {
                    self.bindings.insert(seq.into(), cmd)
                }

                pub fn bind_sequence_if_unbound<T>(&mut self, seq: T, cmd: Command) -> bool
                        where T: Into<Cow<'static, str>> {
                    use mortal::sequence::Entry;

                    match self.bindings.entry(seq.into()) {
                        Entry::Occupied(_) => false,
                        Entry::Vacant(ent) => {
                            ent.insert(cmd);
                            true
                        }
                    }
                }

                pub fn unbind_sequence(&mut self, seq: &str) -> Option<Command> {
                    self.bindings.remove(seq)
                        .map(|(_, cmd)| cmd)
                }

                pub fn define_function<T>(&mut self, name: T, cmd: Arc<dyn Function<Term>>)
                        -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
                    self.functions.insert(name.into(), cmd)
                }

                pub fn remove_function(&mut self, name: &str) -> Option<Arc<dyn Function<Term>>> {
                    self.functions.remove(name)
                }

                fn read_init(&mut self, term: &Term) {
                    if let Some(path) = env_init_file() {
                        // If `INPUTRC` is present, even if invalid, parse nothing else.
                        // Thus, an empty `INPUTRC` will inhibit loading configuration.
                        self.read_init_file_if_exists(term, Some(path));
                    } else {
                        if !self.read_init_file_if_exists(term, user_init_file()) {
                            self.read_init_file_if_exists(term, system_init_file());
                        }
                    }
                }

                fn read_init_file_if_exists(&mut self, term: &Term, path: Option<PathBuf>) -> bool {
                    match path {
                        Some(ref path) if path.exists() => {
                            self.read_init_file(term, path);
                            true
                        }
                        _ => false
                    }
                }

                fn read_init_file(&mut self, term: &Term, path: &Path) {
                    if let Some(dirs) = parse_file(path) {
                        self.evaluate_directives(term, dirs);
                    }
                }

                /// Evaluates a series of configuration directives.
                pub fn evaluate_directives(&mut self, term: &Term, dirs: Vec<Directive>) {
                    for dir in dirs {
                        self.evaluate_directive(term, dir);
                    }
                }

                /// Evaluates a single configuration directive.
                pub fn evaluate_directive(&mut self, term: &Term, dir: Directive) {
                    match dir {
                        Directive::Bind(seq, cmd) => {
                            self.bind_sequence(seq, cmd);
                        }
                        Directive::Conditional{name, value, then_group, else_group} => {
                            let name = name.as_ref().map(|s| &s[..]);

                            if self.eval_condition(term, name, &value) {
                                self.evaluate_directives(term, then_group);
                            } else {
                                self.evaluate_directives(term, else_group);
                            }
                        }
                        Directive::SetVariable(name, value) => {
                            self.set_variable(&name, &value);
                        }
                    }
                }

                fn eval_condition(&self, term: &Term, name: Option<&str>, value: &str) -> bool {
                    match name {
                        None => self.application == value,
                        Some("lib") => value == "linefeed",
                        Some("mode") => value == "emacs",
                        Some("term") => self.term_matches(term, value),
                        _ => false
                    }
                }

                fn term_matches(&self, term: &Term, value: &str) -> bool {
                    match_name(term.name(), value)
                }
            }
            /// Iterator over `Reader` bindings
            pub struct BindingIter<'a>(slice::Iter<'a, (Cow<'static, str>, Command)>);

            impl<'a> ExactSizeIterator for BindingIter<'a> {}

            impl<'a> Iterator for BindingIter<'a> {
                type Item = (&'a str, &'a Command);

                #[inline]
                fn next(&mut self) -> Option<Self::Item> {
                    self.0.next().map(|&(ref s, ref cmd)| (&s[..], cmd))
                }

                #[inline]
                fn nth(&mut self, n: usize) -> Option<Self::Item> {
                    self.0.nth(n).map(|&(ref s, ref cmd)| (&s[..], cmd))
                }

                #[inline]
                fn size_hint(&self) -> (usize, Option<usize>) {
                    self.0.size_hint()
                }
            }

            impl<'a> DoubleEndedIterator for BindingIter<'a> {
                #[inline]
                fn next_back(&mut self) -> Option<Self::Item> {
                    self.0.next_back().map(|&(ref s, ref cmd)| (&s[..], cmd))
                }
            }

            fn default_bindings() -> SequenceMap<Cow<'static, str>, Command>
            {
                use super::command::Command::*;

                SequenceMap::from(vec![
                    // Carriage return and line feed
                    ("\r".into(), AcceptLine),
                    ("\n".into(), AcceptLine),

                    // Possible sequences for arrow keys, Home, End
                    ("\x1b[A".into(), PreviousHistory),
                    ("\x1b[B".into(), NextHistory),
                    ("\x1b[C".into(), ForwardChar),
                    ("\x1b[D".into(), BackwardChar),
                    ("\x1b[H".into(), BeginningOfLine),
                    ("\x1b[F".into(), EndOfLine),

                    // More possible sequences for arrow keys, Home, End
                    ("\x1bOA".into(), PreviousHistory),
                    ("\x1bOB".into(), NextHistory),
                    ("\x1bOC".into(), ForwardChar),
                    ("\x1bOD".into(), BackwardChar),
                    ("\x1bOH".into(), BeginningOfLine),
                    ("\x1bOF".into(), EndOfLine),

                    // Possible sequences for Insert, Delete
                    ("\x1b[2~".into(), OverwriteMode),
                    ("\x1b[3~".into(), DeleteChar),

                    // Basic commands
                    ("\x01"    .into(), BeginningOfLine),           // Ctrl-A
                    ("\x02"    .into(), BackwardChar),              // Ctrl-B
                    ("\x04"    .into(), DeleteChar),                // Ctrl-D
                    ("\x05"    .into(), EndOfLine),                 // Ctrl-E
                    ("\x06"    .into(), ForwardChar),               // Ctrl-F
                    ("\x07"    .into(), Abort),                     // Ctrl-G
                    ("\x08"    .into(), BackwardDeleteChar),        // Ctrl-H
                    ("\x0b"    .into(), KillLine),                  // Ctrl-K
                    ("\x0c"    .into(), ClearScreen),               // Ctrl-L
                    ("\x0e"    .into(), NextHistory),               // Ctrl-N
                    ("\x10"    .into(), PreviousHistory),           // Ctrl-P
                    ("\x12"    .into(), ReverseSearchHistory),      // Ctrl-R
                    ("\x14"    .into(), TransposeChars),            // Ctrl-T
                    ("\x15"    .into(), BackwardKillLine),          // Ctrl-U
                    ("\x16"    .into(), QuotedInsert),              // Ctrl-V
                    ("\x17"    .into(), UnixWordRubout),            // Ctrl-W
                    ("\x19"    .into(), Yank),                      // Ctrl-Y
                    ("\x1d"    .into(), CharacterSearch),           // Ctrl-]
                    ("\x7f"    .into(), BackwardDeleteChar),        // Rubout
                    ("\x1b\x08".into(), BackwardKillWord),          // Escape, Ctrl-H
                    ("\x1b\x1d".into(), CharacterSearchBackward),   // Escape, Ctrl-]
                    ("\x1b\x7f".into(), BackwardKillWord),          // Escape, Rubout
                    ("\x1bb"   .into(), BackwardWord),              // Escape, b
                    ("\x1bd"   .into(), KillWord),                  // Escape, d
                    ("\x1bf"   .into(), ForwardWord),               // Escape, f
                    ("\x1bt"   .into(), TransposeWords),            // Escape, t
                    ("\x1by"   .into(), YankPop),                   // Escape, y
                    ("\x1b#"   .into(), InsertComment),             // Escape, #
                    ("\x1b<"   .into(), BeginningOfHistory),        // Escape, <
                    ("\x1b>"   .into(), EndOfHistory),              // Escape, >

                    // Completion commands
                    ("\t"   .into(), Complete),             // Tab
                    ("\x1b?".into(), PossibleCompletions),  // Escape, ?
                    ("\x1b*".into(), InsertCompletions),    // Escape, *

                    // Digit commands
                    ("\x1b-".into(), DigitArgument),    // Escape, -
                    ("\x1b0".into(), DigitArgument),    // Escape, 0
                    ("\x1b1".into(), DigitArgument),    // Escape, 1
                    ("\x1b2".into(), DigitArgument),    // Escape, 2
                    ("\x1b3".into(), DigitArgument),    // Escape, 3
                    ("\x1b4".into(), DigitArgument),    // Escape, 4
                    ("\x1b5".into(), DigitArgument),    // Escape, 5
                    ("\x1b6".into(), DigitArgument),    // Escape, 6
                    ("\x1b7".into(), DigitArgument),    // Escape, 7
                    ("\x1b8".into(), DigitArgument),    // Escape, 8
                    ("\x1b9".into(), DigitArgument),    // Escape, 9
                ])
            }

            fn limit_duration(dur: Option<Duration>, max: Option<Duration>) -> Option<Duration> {
                match (dur, max) {
                    (dur, None) | (None, dur) => dur,
                    (Some(dur), Some(max)) => Some(dur.min(max)),
                }
            }
        }
        pub use self::reader::{ Reader };

        pub mod system
        {
            pub mod unix
            {
                //! Unix platform support
                pub mod path
                {
                    use ::
                    {
                        env::{ var_os },
                        path::{ PathBuf, home_dir },
                        *,
                    };
                    
                    pub fn env_init_file() -> Option<PathBuf> { var_os("INPUTRC").map(PathBuf::from) }

                    pub fn system_init_file() -> Option<PathBuf> { Some(PathBuf::from("/etc/inputrc")) }

                    pub fn user_init_file() -> Option<PathBuf> { home_dir().map(|p| p.join(".inputrc")) }
                }

                mod terminal
                {
                    use ::
                    {
                        time::{ Duration },
                        *,
                    };
                    /*
                    use std::io;
                    use std::time::Duration;

                    use mortal::{Event, TerminalReadGuard};
                    use mortal::unix::TerminalExt;

                    use crate::terminal::RawRead;
                    */

                    pub fn terminal_read(term: &mut TerminalReadGuard, buf: &mut Vec<u8>) -> io::Result<RawRead>
                    {
                        let mut buffer = [0; 1024];

                        match term.read_raw(&mut buffer, Some(Duration::new(0, 0)))?
                        {
                            None => Ok(RawRead::Bytes(0)),
                            Some(Event::Raw(n)) =>
                            {
                                buf.extend(&buffer[..n]);
                                Ok(RawRead::Bytes(n))
                            }
                            Some(Event::Resize(size)) => Ok(RawRead::Resize(size)),
                            Some(Event::Signal(sig)) => Ok(RawRead::Signal(sig)),
                            _ => unreachable!()
                        }
                    }
                }
                pub use self::terminal::terminal_read;
            }

            pub mod windows
            {
                
            }
        
        }
        #[cfg(unix)] pub use self::system::unix as sys;
        #[cfg(windows)] pub use self::system::windows as sys;

        pub mod table
        {
            //! Provides utilities for formatting strings in a table
            use ::
            {
                cmp::{ min },
                *,
            };
            
            const COL_SPACE: usize = 2;

            /// Represents a table of strings, formatted into rows and columns.
            pub struct Table<'a, S: 'a>
            {
                strings: &'a [S],
                sizes: Option<&'a [usize]>,
                offset: usize,
                per_col: usize,
                rows: usize,
                horizontal: bool,
            }

            impl<'a, S: 'a + AsRef<str>> Table<'a, S>
            {
                /// Constructs a new table from the given set of strings, using the given column sizes.
                pub fn new(strs: &'a [S], mut sizes: Option<&'a [usize]>, horizontal: bool) -> Table<'a, S>
                {
                    if let Some(sz) = sizes {
                        if sz.is_empty() {
                            sizes = None;
                        }
                    }

                    let n_strs = strs.len();
                    let n_cols = sizes.map_or(1, |sz| sz.len());

                    let rows = n_strs / n_cols + (n_strs % n_cols != 0) as usize;

                    Table{
                        strings: strs,
                        sizes: sizes,
                        offset: 0,
                        per_col: (strs.len() + (n_cols - 1)) / n_cols,
                        rows: rows,
                        horizontal: horizontal,
                    }
                }

                /// Returns whether more lines are present in the table.
                pub fn has_more(&self) -> bool {
                    self.offset < self.rows
                }

                fn num_cols(&self) -> usize {
                    self.sizes.map_or(1, |sz| sz.len())
                }
            }

            impl<'a, S: 'a + AsRef<str>> Iterator for Table<'a, S> {
                type Item = Line<'a, S>;

                fn next(&mut self) -> Option<Line<'a, S>> {
                    if self.offset == self.rows {
                        return None;
                    }

                    let n = self.num_cols();

                    let (start, end, stride) = if self.horizontal {
                        let start = self.offset * n;
                        let end = min(self.strings.len(), start + n);
                        (start, end, 1)
                    } else {
                        let start = self.offset;
                        let end = min(self.strings.len(), start + self.per_col * n);
                        (start, end, self.per_col)
                    };

                    self.offset += 1;

                    Some(Line{
                        strings: &self.strings[start..end],
                        sizes: self.sizes,
                        stride: stride,
                        offset: 0,
                    })
                }
            }

            /// Represents a single line of the table
            ///
            /// A `Line` is an `Iterator` yielding `(usize, &str)` elements, describing
            /// the width and content of each cell in a given row.
            pub struct Line<'a, S: 'a> {
                strings: &'a [S],
                sizes: Option<&'a [usize]>,
                stride: usize,
                offset: usize,
            }

            impl<'a, S: 'a + AsRef<str>> Iterator for Line<'a, S> {
                type Item = (usize, &'a str);

                fn next(&mut self) -> Option<(usize, &'a str)> {
                    let s = self.strings.get(self.offset * self.stride)?.as_ref();

                    let width = self.sizes.and_then(|sz| sz.get(self.offset).cloned())
                        .unwrap_or_else(|| s.chars().count());

                    self.offset += 1;

                    Some((width, s))
                }
            }

            /// Formats a series of strings into columns, fitting within a given screen width.
            /// Returns the size of each resulting column, including spacing.
            ///
            /// If the strings cannot be formatted into columns (e.g. one or more strings
            /// are longer than the screen width) or the result would be only one column,
            /// `None` is returned.
            pub fn format_columns<S: AsRef<str>>(strs: &[S], screen_width: usize,
                    horizontal: bool) -> Option<Vec<usize>> {
                if strs.is_empty() {
                    return None;
                }

                let n_strs = strs.len();

                let (mut min_len, mut max_len) = min_max(strs.iter().map(|s| s.as_ref().chars().count()));

                if min_len == 0 { min_len = 1; }
                if max_len == 0 { max_len = 1; }

                let mut min_cols = min(n_strs, screen_width / max_len);
                let max_cols = min(n_strs, screen_width / min_len);

                if min_cols <= 1 {
                    // No point in checking whether text can fit within one column
                    min_cols = 2;
                }

                if max_cols <= 1 {
                    return None;
                }

                let mut col_sizes = if min_cols == max_cols {
                    vec![vec![0; max_cols]]
                } else {
                    (min_cols..max_cols + 1)
                        .map(|n| vec![0; n]).collect::<Vec<_>>()
                };

                for (i, s) in strs.iter().enumerate() {
                    let len = s.as_ref().chars().count();

                    for cols in &mut col_sizes {
                        let n_cols = cols.len();

                        let col = if horizontal {
                            i % n_cols
                        } else {
                            let per_col = (n_strs + (n_cols - 1)) / n_cols;
                            i / per_col
                        };

                        let real_len = if col == n_cols - 1 { len } else { len + COL_SPACE };

                        if real_len > cols[col] {
                            cols[col] = real_len;
                        }
                    }
                }

                for cols in col_sizes.into_iter().rev() {
                    if cols.iter().fold(0, |a, b| a + b) <= screen_width {
                        return Some(cols);
                    }
                }

                None
            }

            fn min_max<I>(iter: I) -> (usize, usize) where I: Iterator<Item=usize> {
                let mut min = usize::max_value();
                let mut max = 0;

                for n in iter {
                    if n < min {
                        min = n;
                    }
                    if n + COL_SPACE > max {
                        max = n + COL_SPACE;
                    }
                }

                (min, max)
            }
        }
        pub use self::table::{ Table };

        pub mod terminal
        {
            //! Provides a low-level terminal interface
            use ::
            {
                time::{ Duration },
                *,
            };
            /*
            use mortal::{self, PrepareConfig, PrepareState, TerminalReadGuard, TerminalWriteGuard};
            use crate::sys;
            pub use mortal::{CursorMode, Signal, SignalSet, Size};

            DefaultTerminal
                Terminal + Terminals

            */
            /// Defines a low-level interface to the terminal
            pub trait Terminals: Sized + Send + Sync
            {
                /// Returned by `prepare`; passed to `restore` to restore state.
                type PrepareState;
                /*
                /// Holds an exclusive read lock and provides read operations
                type Reader: TerminalReader;
                /// Holds an exclusive write lock and provides write operations
                type Writer: TerminalWriter;
                */
                /// Returns the name of the terminal.
                fn name(&self) -> &str;
                /// Acquires a lock on terminal read operations and returns a value holding
                /// that lock and granting access to such operations.
                fn lock_read<'a>(&'a self) -> Box<dyn TerminalReader<Self> + 'a>;
                /// Acquires a lock on terminal write operations and returns a value holding
                /// that lock and granting access to such operations.
                fn lock_write<'a>(&'a self) -> Box<dyn TerminalWriter<Self> + 'a>;
            }
            /// Holds a lock on `Terminal` read operations
            pub trait TerminalReader<Term:Terminals> 
            {
                /// Prepares the terminal for line reading and editing operations.
                fn prepare(&mut self, block_signals: bool, report_signals: SignalSet) 
                -> io::Result<Term::PrepareState>;
                /// Like `prepare`, but called when the write lock is already held.
                unsafe fn prepare_with_lock
                (&mut self, lock: &mut dyn TerminalWriter<Term>, block_signals: bool, report_signals: SignalSet)
                -> io::Result<Term::PrepareState>;
                /// Restores the terminal state using the given state data.
                fn restore(&mut self, state: Term::PrepareState) -> io::Result<()>;
                /// Like `restore`, but called when the write lock is already held.
                unsafe fn restore_with_lock
                (&mut self, lock: &mut dyn TerminalWriter<Term>, state: Term::PrepareState) -> io::Result<()>;
                /// Reads some input from the terminal and appends it to the given buffer.
                fn read(&mut self, buf: &mut Vec<u8>) -> io::Result<RawRead>;
                /// Waits `timeout` for user input. If `timeout` is `None`, waits indefinitely.
                fn wait_for_input(&mut self, timeout: Option<Duration>) -> io::Result<bool>;
            }
            /// Holds a lock on `Terminal` write operations
            pub trait TerminalWriter<Term: Terminals> 
            {
                /// Returns the size of the terminal window
                fn size(&self) -> io::Result<Size>;
                /// Presents a clear terminal screen, with cursor at first row, first column.
                fn clear_screen(&mut self) -> io::Result<()>;
                /// Clears characters on the line occupied by the cursor, beginning with the
                /// cursor and ending at the end of the line.
                fn clear_to_screen_end(&mut self) -> io::Result<()>;
                /// Moves the cursor up `n` cells; `n` may be zero.
                fn move_up(&mut self, n: usize) -> io::Result<()>;
                /// Moves the cursor down `n` cells; `n` may be zero.
                fn move_down(&mut self, n: usize) -> io::Result<()>;
                /// Moves the cursor left `n` cells; `n` may be zero.
                fn move_left(&mut self, n: usize) -> io::Result<()>;
                /// Moves the cursor right `n` cells; `n` may be zero.
                fn move_right(&mut self, n: usize) -> io::Result<()>;
                /// Moves the cursor to the first column of the current line
                fn move_to_first_column(&mut self) -> io::Result<()>;
                /// Set the current cursor mode
                fn set_cursor_mode(&mut self, mode: CursorMode) -> io::Result<()>;
                /// Writes output to the terminal.
                fn write(&mut self, s: &str) -> io::Result<()>;
                /// Flushes any currently buffered output data.
                fn flush(&mut self) -> io::Result<()>;
            }
            /// Represents the result of a `Terminal` read operation
            pub enum RawRead 
            {
                /// `n` bytes were read from the device
                Bytes(usize),
                /// The terminal window was resized
                Resize(Size),
                /// A signal was received while waiting for input
                Signal(Signal),
            }
            /// Represents the size of a terminal window.
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub struct Size
            {
                /// Number of lines in the terminal
                pub lines: usize,
                /// Number of columns in the terminal
                pub columns: usize,
            }

            impl Size
            {
                /// Returns the total number of cells in a terminal of the given size.
                #[inline] pub fn area(&self) -> usize
                {
                    self.checked_area().unwrap_or_else( || panic!("overflow in Size::area {:?}", self) )
                }
                /// Returns the total number of cells in a terminal of the given size.
                #[inline] pub fn checked_area(&self) -> Option<usize> { self.lines.checked_mul(self.columns) }
            }
            /// Represents a color attribute applied to text foreground or background.
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub enum Color
            {
                /// Black
                Black,
                /// Blue
                Blue,
                /// Cyan
                Cyan,
                /// Green
                Green,
                /// Magenta
                Magenta,
                /// Red
                Red,
                /// White
                White,
                /// Yellow
                Yellow,
            }
            bitflags!
            {
                /// Represents a set of style attributes applied to text.
                #[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
                pub struct Style:u8
                {
                    /// Bold
                    const BOLD      = 1 << 0;
                    /// Italic
                    const ITALIC    = 1 << 1;
                    /// Reverse
                    const REVERSE   = 1 << 2;
                    /// Underline
                    const UNDERLINE = 1 << 3;
                }
            }

            pub mod system
            {
                pub mod unix
                {
                    pub mod terminal
                    {
                        pub struct Terminal 
                        {
                            info: Database,
                            out_fd: RawFd,
                            in_fd: RawFd,
                            owned_fd: bool,
                            sequences: SeqMap,
                            reader: Mutex<Reader>,
                            writer: Mutex<Writer>,
                        }
                    }
                }

                pub mod windows
                {
                    use ::
                    {
                        libc::{ c_void, c_ulong, c_ushort },
                        *,
                    };

                    pub type HANDLE = *mut c_void;
                    pub type WORD = c_ushort;
                    pub type DWORD = c_ulong;

                    pub mod terminal
                    {
                        use ::
                        {
                            sync::{ LockResult, Mutex, MutexGuard, TryLockResult },
                            *,
                        };

                        use super::{ * };
                        
                        
                        pub struct Terminal
                        {
                            in_handle: HANDLE,
                            default_attrs: WORD,
                            old_out_mode: DWORD,
                            reader: Mutex<Reader>,
                            writer: Mutex<Writer>,
                        }

                        unsafe impl Send for Terminal {}
                        unsafe impl Sync for Terminal {}

                        struct Reader
                        {
                            always_track_motion: bool,
                            prev_buttons: DWORD,
                        }

                        struct Writer
                        {
                            out_handle: HANDLE,
                            fg: Option<Color>,
                            bg: Option<Color>,
                            style: Style,
                        }
                    }
                }
            }
            #[cfg(unix)] pub use self::system::unix as sys;
            #[cfg(windows)] pub use self::system::windows as sys;
            pub use self::sys::terminal::Terminal as SystemTerminal;            
            /// Provides concurrent read and write access to a terminal device
            pub struct Terminal( pub SystemTerminal );
            /// Default `Terminal` interface
            pub struct DefaultTerminal( Terminal );            
            /*
            impl DefaultTerminal {
                /// Opens access to the terminal device associated with standard output.
                pub fn new() -> io::Result<DefaultTerminal> {
                    mortal::Terminal::new().map(DefaultTerminal)
                }

                /// Opens access to the terminal device associated with standard error.
                pub fn stderr() -> io::Result<DefaultTerminal> {
                    mortal::Terminal::stderr().map(DefaultTerminal)
                }

                unsafe fn cast_writer<'a>(writer: &'a mut dyn TerminalWriter<Self>)
                        -> &'a mut TerminalWriteGuard<'a> {
                    &mut *(writer as *mut _ as *mut TerminalWriteGuard)
                }
            }

            impl Terminal for DefaultTerminal {
                type PrepareState = PrepareState;

                fn name(&self) -> &str {
                    self.0.name()
                }

                fn lock_read<'a>(&'a self) -> Box<dyn TerminalReader<Self> + 'a> {
                    Box::new(self.0.lock_read().unwrap())
                }

                fn lock_write<'a>(&'a self) -> Box<dyn TerminalWriter<Self> + 'a> {
                    Box::new(self.0.lock_write().unwrap())
                }
            }

            impl<'a> TerminalReader<DefaultTerminal> for TerminalReadGuard<'a> {
                fn prepare(&mut self, block_signals: bool, report_signals: SignalSet)
                        -> io::Result<PrepareState> {
                    self.prepare(PrepareConfig{
                        block_signals,
                        enable_control_flow: !block_signals,
                        enable_keypad: false,
                        report_signals,
                        .. PrepareConfig::default()
                    })
                }

                unsafe fn prepare_with_lock(&mut self,
                        lock: &mut dyn TerminalWriter<DefaultTerminal>,
                        block_signals: bool, report_signals: SignalSet)
                        -> io::Result<PrepareState> {
                    let lock = DefaultTerminal::cast_writer(lock);

                    self.prepare_with_lock(lock, PrepareConfig{
                        block_signals,
                        enable_control_flow: !block_signals,
                        enable_keypad: false,
                        report_signals,
                        .. PrepareConfig::default()
                    })
                }

                fn restore(&mut self, state: PrepareState) -> io::Result<()> {
                    self.restore(state)
                }

                unsafe fn restore_with_lock(&mut self,
                        lock: &mut dyn TerminalWriter<DefaultTerminal>, state: PrepareState)
                        -> io::Result<()> {
                    let lock = DefaultTerminal::cast_writer(lock);
                    self.restore_with_lock(lock, state)
                }

                fn read(&mut self, buf: &mut Vec<u8>) -> io::Result<RawRead> {
                    sys::terminal_read(self, buf)
                }

                fn wait_for_input(&mut self, timeout: Option<Duration>) -> io::Result<bool> {
                    self.wait_event(timeout)
                }

            }

            impl<'a> TerminalWriter<DefaultTerminal> for TerminalWriteGuard<'a> {
                fn size(&self) -> io::Result<Size> {
                    self.size()
                }

                fn clear_screen(&mut self) -> io::Result<()> {
                    self.clear_screen()
                }

                fn clear_to_screen_end(&mut self) -> io::Result<()> {
                    self.clear_to_screen_end()
                }

                fn move_up(&mut self, n: usize) -> io::Result<()> {
                    self.move_up(n)
                }
                fn move_down(&mut self, n: usize) -> io::Result<()> {
                    self.move_down(n)
                }
                fn move_left(&mut self, n: usize) -> io::Result<()> {
                    self.move_left(n)
                }
                fn move_right(&mut self, n: usize) -> io::Result<()> {
                    self.move_right(n)
                }

                fn move_to_first_column(&mut self) -> io::Result<()> {
                    self.move_to_first_column()
                }

                fn set_cursor_mode(&mut self, mode: CursorMode) -> io::Result<()> {
                    self.set_cursor_mode(mode)
                }

                fn write(&mut self, s: &str) -> io::Result<()> {
                    self.write_str(s)
                }

                fn flush(&mut self) -> io::Result<()> {
                    self.flush()
                }
            }
            */
        }
        pub use self::terminal::{ Terminals };

        pub mod util
        {
            //! Provides miscellaneous utilities
            use ::
            {
                borrow::{ Cow },
                ops::{ Range, RangeFrom, RangeFull, RangeTo },
                str::{ from_utf8, from_utf8_unchecked },
                *,
            };
            
            pub fn filter_visible(s: &str) -> Cow<str>
            {
                use super::reader::{ START_INVISIBLE, END_INVISIBLE };

                if !s.contains(START_INVISIBLE) {
                    return Cow::Borrowed(s);
                }

                let mut virt = String::new();
                let mut ignore = false;

                for ch in s.chars() {
                    if ch == START_INVISIBLE {
                        ignore = true;
                    } else if ch == END_INVISIBLE {
                        ignore = false;
                    } else if !ignore {
                        virt.push(ch);
                    }
                }

                Cow::Owned(virt)
            }

            /// Returns the longest common prefix of a set of strings.
            ///
            /// If no common prefix exists, `None` is returned.
            pub fn longest_common_prefix<'a, I, S>(iter: I) -> Option<&'a str> where
                    I: IntoIterator<Item=&'a S>,
                    S: 'a + ?Sized + AsRef<str>,
                    {
                let mut iter = iter.into_iter();

                let mut pfx = iter.next()?.as_ref();

                for s in iter {
                    let s = s.as_ref();

                    let n = pfx.chars().zip(s.chars())
                        .take_while(|&(a, b)| a == b)
                        .map(|(ch, _)| ch.len_utf8()).sum();

                    if n == 0 {
                        return None;
                    } else {
                        pfx = &pfx[..n];
                    }
                }

                Some(pfx)
            }

            /// Returns a string consisting of a `char`, repeated `n` times.
            pub fn repeat_char(ch: char, n: usize) -> String {
                let mut buf = [0; 4];
                let s = ch.encode_utf8(&mut buf);

                s.repeat(n)
            }

            /// Implemented for built-in range types
            // Waiting for stabilization of `std` trait of the same name
            pub trait RangeArgument<T> {
                /// Returns the start of range, if present.
                fn start(&self) -> Option<&T> { None }

                /// Returns the end of range, if present.
                fn end(&self) -> Option<&T> { None }
            }

            impl<T> RangeArgument<T> for Range<T> {
                fn start(&self) -> Option<&T> { Some(&self.start) }

                fn end(&self) -> Option<&T> { Some(&self.end) }
            }

            impl<T> RangeArgument<T> for RangeFrom<T> {
                fn start(&self) -> Option<&T> { Some(&self.start) }
            }

            impl<T> RangeArgument<T> for RangeTo<T> {
                fn end(&self) -> Option<&T> { Some(&self.end) }
            }

            impl<T> RangeArgument<T> for RangeFull {}

            pub fn backward_char(n: usize, s: &str, cur: usize) -> usize {
                let mut chars = s[..cur].char_indices()
                    .filter(|&(_, ch)| !is_combining_mark(ch));
                let mut res = cur;

                for _ in 0..n {
                    match chars.next_back() {
                        Some((idx, _)) => res = idx,
                        None => return 0
                    }
                }

                res
            }

            pub fn forward_char(n: usize, s: &str, cur: usize) -> usize {
                let mut chars = s[cur..].char_indices()
                    .filter(|&(_, ch)| !is_combining_mark(ch));

                for _ in 0..n {
                    match chars.next() {
                        Some(_) => (),
                        None => return s.len()
                    }
                }

                match chars.next() {
                    Some((idx, _)) => cur + idx,
                    None => s.len()
                }
            }

            pub fn backward_search_char(n: usize, buf: &str, mut cur: usize, ch: char) -> Option<usize> {
                let mut pos = None;

                for _ in 0..n {
                    match buf[..cur].rfind(ch) {
                        Some(p) => {
                            cur = p;
                            pos = Some(cur);
                        }
                        None => break
                    }
                }

                pos
            }

            pub fn forward_search_char(n: usize, buf: &str, mut cur: usize, ch: char) -> Option<usize> {
                let mut pos = None;

                for _ in 0..n {
                    // Skip past the character under the cursor
                    let off = match buf[cur..].chars().next() {
                        Some(ch) => ch.len_utf8(),
                        None => break
                    };

                    match buf[cur + off..].find(ch) {
                        Some(p) => {
                            cur += off + p;
                            pos = Some(cur);
                        }
                        None => break
                    }
                }

                pos
            }

            pub fn backward_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
                let mut chars = buf[..cur].char_indices().rev();

                for _ in 0..n {
                    drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                    drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                }

                match chars.next() {
                    Some((ind, ch)) => ind + ch.len_utf8(),
                    None => 0
                }
            }

            pub fn forward_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
                let mut chars = buf[cur..].char_indices();

                for _ in 0..n {
                    drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                    drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                }

                match chars.next() {
                    Some((ind, _)) => cur + ind,
                    None => buf.len()
                }
            }

            pub fn back_n_words(n: usize, buf: &str, cur: usize, word_break: &str) -> Range<usize> {
                let prev = backward_word(1, buf, cur, word_break);
                let end = word_end(&buf, prev, word_break);

                if n > 1 {
                    let start = backward_word(n - 1, buf, prev, word_break);
                    start..end
                } else {
                    prev..end
                }
            }

            pub fn forward_n_words(n: usize, buf: &str, cur: usize, word_break: &str) -> Range<usize> {
                let start = next_word(1, buf, cur, word_break);

                if n > 1 {
                    let last = next_word(n - 1, buf, start, word_break);
                    let end = word_end(buf, last, word_break);
                    start..end
                } else {
                    let end = word_end(buf, start, word_break);
                    start..end
                }
            }

            /// Returns the first character in the buffer, if it contains any valid characters.
            pub fn first_char(buf: &[u8]) -> io::Result<Option<char>> {
                match from_utf8(buf) {
                    Ok(s) => Ok(s.chars().next()),
                    Err(e) => {
                        if e.error_len().is_some() {
                            return Err(io::Error::new(io::ErrorKind::InvalidData,
                                "invalid utf-8 input received"));
                        }

                        let valid = e.valid_up_to();

                        let s = unsafe { from_utf8_unchecked(&buf[..valid]) };
                        Ok(s.chars().next())
                    }
                }
            }

            pub fn first_word(buf: &str, word_break: &str) -> Option<usize> {
                let mut chars = buf.char_indices();

                drop_while(&mut chars, |(_, ch)| word_break.contains(ch));

                chars.next().map(|(idx, _)| idx)
            }

            pub fn word_start(buf: &str, cur: usize, word_break: &str) -> usize {
                let fwd = match buf[cur..].chars().next() {
                    Some(ch) => word_break.contains(ch),
                    None => return buf.len()
                };

                if fwd {
                    next_word(1, buf, cur, word_break)
                } else {
                    let mut chars = buf[..cur].char_indices().rev();

                    drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));

                    match chars.next() {
                        Some((idx, ch)) => idx + ch.len_utf8(),
                        None => 0
                    }
                }
            }

            pub fn next_word(n: usize, buf: &str, cur: usize, word_break: &str) -> usize {
                let mut chars = buf[cur..].char_indices();

                for _ in 0..n {
                    drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                    drop_while(&mut chars, |(_, ch)| word_break.contains(ch));
                    if chars.clone().next().is_none() { break; }
                }

                match chars.next() {
                    Some((idx, _)) => cur + idx,
                    None => buf.len()
                }
            }

            pub fn word_end(buf: &str, cur: usize, word_break: &str) -> usize {
                let mut chars = buf[cur..].char_indices();

                drop_while(&mut chars, |(_, ch)| !word_break.contains(ch));

                match chars.next() {
                    Some((idx, _)) => cur + idx,
                    None => buf.len()
                }
            }

            pub fn drop_while<I, T, F>(iter: &mut I, mut f: F)
                    where I: Iterator<Item=T> + Clone, F: FnMut(T) -> bool {
                loop {
                    let mut clone = iter.clone();

                    match clone.next() {
                        None => break,
                        Some(t) => {
                            if f(t) {
                                *iter = clone;
                            } else {
                                break;
                            }
                        }
                    }
                }
            }

            pub fn get_open_paren(ch: char) -> Option<char> {
                match ch {
                    ')' => Some('('),
                    ']' => Some('['),
                    '}' => Some('{'),
                    _ => None
                }
            }

            pub fn find_matching_paren(s: &str, quotes: &str, open: char, close: char) -> Option<usize>
            {
                let mut chars = s.char_indices().rev();
                let mut level = 0;
                let mut string_delim = None;

                while let Some((ind, ch)) = chars.next() {
                    if string_delim == Some(ch) {
                        string_delim = None;
                    } else if quotes.contains(ch) {
                        string_delim = Some(ch);
                    } else if string_delim.is_none() && ch == close {
                        level += 1;
                    } else if string_delim.is_none() && ch == open {
                        level -= 1;

                        if level == 0 {
                            return Some(ind);
                        }
                    }
                }

                None
            }

            pub fn is_combining_mark(ch: char) -> bool
            {
                //use mortal::util::is_combining_mark;
                is_combining_mark(ch)
            }

            pub fn is_wide(ch: char) -> bool
            {
                //use mortal::util::char_width;
                char_width(ch) == Some(2)
            }

            pub fn match_name(name: &str, value: &str) -> bool
            {
                name == value || (name.starts_with(value) && name.as_bytes()[value.len()] == b'-')
            }
        }

        pub mod writer
        {
            //! Provides access to terminal write operations
            use ::
            {
                borrow::Cow::{self, Borrowed, Owned},
                char::{is_ctrl, unctrl, ESCAPE, RUBOUT},
                collections::{vec_deque, VecDeque},
                iter::{repeat, Skip},
                ops::{Deref, DerefMut, Range},
                sync::MutexGuard,
                time::{Duration, Instant},
                *,
            };

            use super::
            {
                reader::{START_INVISIBLE, END_INVISIBLE},
                terminal::{CursorMode, Size, Terminal, TerminalWriter},
                util::
                {
                    backward_char, forward_char, backward_search_char, forward_search_char,
                    filter_visible, is_combining_mark, is_wide, RangeArgument,
                },
            };
            /// Duration to wait for input when "blinking"
            pub const BLINK_DURATION: Duration = Duration::from_millis(500);

            const COMPLETE_MORE: &'static str = "--More--";
            /// Default maximum history size
            const MAX_HISTORY: usize = !0;
            /// Tab column interval
            const TAB_STOP: usize = 8;
            // Length of "(arg: "
            const PROMPT_NUM_PREFIX: usize = 6;
            // Length of ") "
            const PROMPT_NUM_SUFFIX: usize = 2;
            // Length of "(i-search)`"
            const PROMPT_SEARCH_PREFIX: usize = 11;
            // Length of "failed "
            const PROMPT_SEARCH_FAILED_PREFIX: usize = 7;
            // Length of "reverse-"
            const PROMPT_SEARCH_REVERSE_PREFIX: usize = 8;
            // Length of "': "
            const PROMPT_SEARCH_SUFFIX: usize = 3;
            /// Provides an interface to write line-by-line output to the terminal device.
            pub struct Writer<'a, 'b: 'a, Term: 'b + Terminal> 
            {
                write: WriterImpl<'a, 'b, Term>,
            }

            enum WriterImpl<'a, 'b: 'a, Term: 'b + Terminal> 
            {
                Mutex(WriteLock<'b, Term>),
                MutRef(&'a mut WriteLock<'b, Term>),
            }

            pub struct Write 
            {
                /// Input buffer
                pub buffer: String,
                /// Original buffer entered before searching through history
                pub backup_buffer: String,
                /// Position of the cursor
                pub cursor: usize,
                /// Position of the cursor if currently performing a blink
                blink: Option<Blink>,
                /// Stored history entries
                pub history: VecDeque<String>,
                /// History entry currently being edited;
                /// `None` if the new buffer is being edited
                pub history_index: Option<usize>,
                /// Maximum size of history
                history_size: usize,
                /// Number of history entries added since last loading history
                history_new_entries: usize,
                /// Whether the prompt is drawn; i.e. a `read_line` operation is in progress
                pub is_prompt_drawn: bool,
                /// Portion of prompt up to and including the final newline
                pub prompt_prefix: String,
                prompt_prefix_len: usize,
                /// Portion of prompt after the final newline
                pub prompt_suffix: String,
                prompt_suffix_len: usize,
                /// Current type of prompt
                pub prompt_type: PromptType,
                /// Whether a search in progress is a reverse search
                pub reverse_search: bool,
                /// Whether a search in progress has failed to find a match
                pub search_failed: bool,
                /// Current search string
                pub search_buffer: String,
                /// Last search string
                pub last_search: String,
                /// Selected history entry prior to a history search
                pub prev_history: Option<usize>,
                /// Position of the cursor prior to a history search
                pub prev_cursor: usize,
                /// Numerical argument
                pub input_arg: Digit,
                /// Whether a numerical argument was supplied
                pub explicit_arg: bool,
                /// Terminal size as of last draw operation
                pub screen_size: Size,
            }

            pub(crate) struct WriteLock<'a, Term: 'a + Terminal>
            {
                term: Box<dyn TerminalWriter<Term> + 'a>,
                data: MutexGuard<'a, Write>,
            }

            impl<'a, Term: Terminal> WriteLock<'a, Term>
            {
                pub fn new(term: Box<dyn TerminalWriter<Term> + 'a>, data: MutexGuard<'a, Write>)
                        -> WriteLock<'a, Term> {
                    WriteLock{term, data}
                }

                pub fn size(&self) -> io::Result<Size> {
                    self.term.size()
                }

                pub fn flush(&mut self) -> io::Result<()> {
                    self.term.flush()
                }

                pub fn update_size(&mut self) -> io::Result<()> {
                    let size = self.size()?;
                    self.screen_size = size;
                    Ok(())
                }

                pub fn blink(&mut self, pos: usize) -> io::Result<()> {
                    self.expire_blink()?;

                    let orig = self.cursor;
                    self.move_to(pos)?;
                    self.cursor = orig;

                    let expiry = Instant::now() + BLINK_DURATION;

                    self.blink = Some(Blink{
                        pos,
                        expiry,
                    });

                    Ok(())
                }

                pub fn check_expire_blink(&mut self, now: Instant) -> io::Result<bool> {
                    if let Some(blink) = self.data.blink {
                        if now >= blink.expiry {
                            self.expire_blink()?;
                        }
                    }

                    Ok(self.blink.is_none())
                }

                pub fn expire_blink(&mut self) -> io::Result<()> {
                    if let Some(blink) = self.data.blink.take() {
                        self.move_from(blink.pos)?;
                    }

                    Ok(())
                }

                pub fn set_prompt(&mut self, prompt: &str) -> io::Result<()> {
                    self.expire_blink()?;

                    let redraw = self.is_prompt_drawn && self.prompt_type.is_normal();

                    if redraw {
                        self.clear_full_prompt()?;
                    }

                    self.data.set_prompt(prompt);

                    if redraw {
                        self.draw_prompt()?;
                    }

                    Ok(())
                }

                /// Draws the prompt and current input, assuming the cursor is at column 0
                pub fn draw_prompt(&mut self) -> io::Result<()> {
                    self.draw_prompt_prefix()?;
                    self.draw_prompt_suffix()
                }

                pub fn draw_prompt_prefix(&mut self) -> io::Result<()> {
                    match self.prompt_type {
                        // Prefix is not drawn when completions are shown
                        PromptType::CompleteMore => Ok(()),
                        _ => {
                            let pfx = self.prompt_prefix.clone();
                            self.draw_raw_prompt(&pfx)
                        }
                    }
                }

                pub fn draw_prompt_suffix(&mut self) -> io::Result<()> {
                    match self.prompt_type {
                        PromptType::Normal => {
                            let sfx = self.prompt_suffix.clone();
                            self.draw_raw_prompt(&sfx)?;
                        }
                        PromptType::Number => {
                            let n = self.input_arg.to_i32();
                            let s = format!("(arg: {}) ", n);
                            self.draw_text(0, &s)?;
                        }
                        PromptType::Search => {
                            let pre = match (self.reverse_search, self.search_failed) {
                                (false, false) => "(i-search)",
                                (false, true)  => "(failed i-search)",
                                (true,  false) => "(reverse-i-search)",
                                (true,  true)  => "(failed reverse-i-search)",
                            };

                            let ent = self.get_history(self.history_index).to_owned();
                            let s = format!("{}`{}': {}", pre, self.search_buffer, ent);

                            self.draw_text(0, &s)?;
                            let pos = self.cursor;

                            let (lines, cols) = self.move_delta(ent.len(), pos, &ent);
                            return self.move_rel(lines, cols);
                        }
                        PromptType::CompleteIntro(n) => {
                            return self.term.write(&complete_intro(n));
                        }
                        PromptType::CompleteMore => {
                            return self.term.write(COMPLETE_MORE);
                        }
                    }

                    self.draw_buffer(0)?;
                    let len = self.buffer.len();
                    self.move_from(len)
                }

                pub fn redraw_prompt(&mut self, new_prompt: PromptType) -> io::Result<()> {
                    self.clear_prompt()?;
                    self.prompt_type = new_prompt;
                    self.draw_prompt_suffix()
                }

                /// Draws a portion of the buffer, starting from the given cursor position
                pub fn draw_buffer(&mut self, pos: usize) -> io::Result<()> {
                    let (_, col) = self.line_col(pos);

                    let buf = self.buffer[pos..].to_owned();
                    self.draw_text(col, &buf)?;
                    Ok(())
                }

                /// Draw some text with the cursor beginning at the given column.
                fn draw_text(&mut self, start_col: usize, text: &str) -> io::Result<()> {
                    self.draw_text_impl(start_col, text, Display{
                        allow_tab: true,
                        allow_newline: true,
                        .. Display::default()
                    }, false)
                }

                fn draw_raw_prompt(&mut self, text: &str) -> io::Result<()> {
                    self.draw_text_impl(0, text, Display{
                        allow_tab: true,
                        allow_newline: true,
                        allow_escape: true,
                    }, true)
                }

                fn draw_text_impl(&mut self, start_col: usize, text: &str, disp: Display,
                        handle_invisible: bool) -> io::Result<()> {
                    let width = self.screen_size.columns;
                    let mut col = start_col;
                    let mut out = String::with_capacity(text.len());

                    let mut clear = false;
                    let mut hidden = false;

                    for ch in text.chars() {
                        if handle_invisible && ch == START_INVISIBLE {
                            hidden = true;
                        } else if handle_invisible && ch == END_INVISIBLE {
                            hidden = false;
                        } else if hidden {
                            // Render the character, but assume it has 0 width.
                            out.push(ch);
                        } else {
                            for ch in display(ch, disp) {
                                if ch == '\t' {
                                    let n = TAB_STOP - (col % TAB_STOP);

                                    if col + n > width {
                                        let pre = width - col;
                                        out.extend(repeat(' ').take(pre));
                                        out.push_str(" \r");
                                        out.extend(repeat(' ').take(n - pre));
                                        col = n - pre;
                                    } else {
                                        out.extend(repeat(' ').take(n));
                                        col += n;

                                        if col == width {
                                            out.push_str(" \r");
                                            col = 0;
                                        }
                                    }
                                } else if ch == '\n' {
                                    if !clear {
                                        self.term.write(&out)?;
                                        out.clear();
                                        self.term.clear_to_screen_end()?;
                                        clear = true;
                                    }

                                    out.push('\n');
                                    col = 0;
                                } else if is_combining_mark(ch) {
                                    out.push(ch);
                                } else if is_wide(ch) {
                                    if col == width - 1 {
                                        out.push_str("  \r");
                                        out.push(ch);
                                        col = 2;
                                    } else {
                                        out.push(ch);
                                        col += 2;
                                    }
                                } else {
                                    out.push(ch);
                                    col += 1;

                                    if col == width {
                                        // Space pushes the cursor to the next line,
                                        // CR brings back to the start of the line.
                                        out.push_str(" \r");
                                        col = 0;
                                    }
                                }
                            }
                        }
                    }

                    if col == width {
                        out.push_str(" \r");
                    }

                    self.term.write(&out)
                }

                pub fn set_buffer(&mut self, buf: &str) -> io::Result<()> {
                    self.expire_blink()?;

                    self.move_to(0)?;
                    self.buffer.clear();
                    self.buffer.push_str(buf);
                    self.new_buffer()
                }

                pub fn set_cursor(&mut self, pos: usize) -> io::Result<()> {
                    self.expire_blink()?;

                    if !self.buffer.is_char_boundary(pos) {
                        panic!("invalid cursor position {} in buffer {:?}",
                            pos, self.buffer);
                    }

                    self.move_to(pos)
                }

                pub fn set_cursor_mode(&mut self, mode: CursorMode) -> io::Result<()> {
                    self.term.set_cursor_mode(mode)
                }

                pub fn history_len(&self) -> usize {
                    self.history.len()
                }

                pub fn history_size(&self) -> usize {
                    self.history_size
                }

                pub fn set_history_size(&mut self, n: usize) {
                    self.history_size = n;
                    self.truncate_history(n);
                }

                pub fn write_str(&mut self, s: &str) -> io::Result<()> {
                    self.term.write(s)
                }

                pub fn start_history_search(&mut self, reverse: bool) -> io::Result<()> {
                    self.search_buffer = self.buffer[..self.cursor].to_owned();

                    self.continue_history_search(reverse)
                }

                pub fn continue_history_search(&mut self, reverse: bool) -> io::Result<()> {
                    if let Some(idx) = self.find_history_search(reverse) {
                        self.set_history_entry(Some(idx));

                        let pos = self.cursor;
                        let end = self.buffer.len();

                        self.draw_buffer(pos)?;
                        self.clear_to_screen_end()?;
                        self.move_from(end)?;
                    }

                    Ok(())
                }

                fn find_history_search(&self, reverse: bool) -> Option<usize> {
                    let len = self.history.len();
                    let idx = self.history_index.unwrap_or(len);

                    if reverse {
                        self.history.iter().rev().skip(len - idx)
                            .position(|ent| ent.starts_with(&self.search_buffer))
                            .map(|pos| idx - (pos + 1))
                    } else {
                        self.history.iter().skip(idx + 1)
                            .position(|ent| ent.starts_with(&self.search_buffer))
                            .map(|pos| idx + (pos + 1))
                    }
                }

                pub fn start_search_history(&mut self, reverse: bool) -> io::Result<()> {
                    self.reverse_search = reverse;
                    self.search_failed = false;
                    self.search_buffer.clear();
                    self.prev_history = self.history_index;
                    self.prev_cursor = self.cursor;

                    self.redraw_prompt(PromptType::Search)
                }

                pub fn continue_search_history(&mut self, reverse: bool) -> io::Result<()> {
                    self.reverse_search = reverse;
                    self.search_failed = false;

                    {
                        let data = &mut *self.data;
                        data.search_buffer.clone_from(&data.last_search);
                    }

                    self.search_history_step()
                }

                pub fn end_search_history(&mut self) -> io::Result<()> {
                    self.redraw_prompt(PromptType::Normal)
                }

                pub fn abort_search_history(&mut self) -> io::Result<()> {
                    self.clear_prompt()?;

                    let ent = self.prev_history;
                    self.set_history_entry(ent);
                    self.cursor = self.prev_cursor;

                    self.prompt_type = PromptType::Normal;
                    self.draw_prompt_suffix()
                }

                fn show_search_match(&mut self, next_match: Option<(Option<usize>, usize)>)
                        -> io::Result<()> {
                    self.clear_prompt()?;

                    if let Some((idx, pos)) = next_match {
                        self.search_failed = false;
                        self.set_history_entry(idx);
                        self.cursor = pos;
                    } else {
                        self.search_failed = true;
                    }

                    self.prompt_type = PromptType::Search;
                    self.draw_prompt_suffix()
                }

                pub fn search_history_update(&mut self) -> io::Result<()> {
                    // Search for the next match, perhaps including the current position
                    let next_match = if self.reverse_search {
                        self.search_history_backward(&self.search_buffer, true)
                    } else {
                        self.search_history_forward(&self.search_buffer, true)
                    };

                    self.show_search_match(next_match)
                }

                fn search_history_step(&mut self) -> io::Result<()> {
                    if self.search_buffer.is_empty() {
                        return self.redraw_prompt(PromptType::Search);
                    }

                    // Search for the next match
                    let next_match = if self.reverse_search {
                        self.search_history_backward(&self.search_buffer, false)
                    } else {
                        self.search_history_forward(&self.search_buffer, false)
                    };

                    self.show_search_match(next_match)
                }

                fn search_history_backward(&self, s: &str, include_cur: bool)
                        -> Option<(Option<usize>, usize)> {
                    let mut idx = self.history_index;
                    let mut pos = Some(self.cursor);

                    if include_cur && !self.search_failed {
                        if let Some(p) = pos {
                            if self.get_history(idx).is_char_boundary(p + s.len()) {
                                pos = Some(p + s.len());
                            }
                        }
                    }

                    loop {
                        let line = self.get_history(idx);

                        match line[..pos.unwrap_or(line.len())].rfind(s) {
                            Some(found) => {
                                pos = Some(found);
                                break;
                            }
                            None => {
                                match idx {
                                    Some(0) => return None,
                                    Some(n) => {
                                        idx = Some(n - 1);
                                        pos = None;
                                    }
                                    None => {
                                        if self.history.is_empty() {
                                            return None;
                                        } else {
                                            idx = Some(self.history.len() - 1);
                                            pos = None;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    pos.map(|pos| (idx, pos))
                }

                fn search_history_forward(&self, s: &str, include_cur: bool)
                        -> Option<(Option<usize>, usize)> {
                    let mut idx = self.history_index;
                    let mut pos = Some(self.cursor);

                    if !include_cur {
                        if let Some(p) = pos {
                            pos = Some(forward_char(1, self.get_history(idx), p));
                        }
                    }

                    loop {
                        let line = self.get_history(idx);

                        match line[pos.unwrap_or(0)..].find(s) {
                            Some(found) => {
                                pos = pos.map(|n| n + found).or(Some(found));
                                break;
                            }
                            None => {
                                if let Some(n) = idx {
                                    if n + 1 == self.history.len() {
                                        idx = None;
                                    } else {
                                        idx = Some(n + 1);
                                    }
                                    pos = None;
                                } else {
                                    return None;
                                }
                            }
                        }
                    }

                    pos.map(|pos| (idx, pos))
                }

                pub fn add_history(&mut self, line: String) {
                    if self.history.len() == self.history_size {
                        self.history.pop_front();
                    }

                    self.history.push_back(line);
                    self.history_new_entries = self.history.len()
                        .min(self.history_new_entries + 1);
                }

                pub fn add_history_unique(&mut self, line: String) {
                    let is_duplicate = self.history.back().map_or(false, |ent| *ent == line);

                    if !is_duplicate {
                        self.add_history(line);
                    }
                }

                pub fn clear_history(&mut self) {
                    self.truncate_history(0);
                    self.history_new_entries = 0;
                }

                pub fn remove_history(&mut self, n: usize) {
                    if n < self.history.len() {
                        let first_new = self.history.len() - self.history_new_entries;

                        if n >= first_new {
                            self.history_new_entries -= 1;
                        }

                        self.history.remove(n);
                    }
                }

                pub fn truncate_history(&mut self, n: usize) {
                    let len = self.history.len();

                    if n < len {
                        let _ = self.history.drain(..len - n);
                        self.history_new_entries = self.history_new_entries.max(n);
                    }
                }

                pub fn next_history(&mut self, n: usize) -> io::Result<()> {
                    if let Some(old) = self.history_index {
                        let new = old.saturating_add(n);

                        if new >= self.history.len() {
                            self.select_history_entry(None)?;
                        } else {
                            self.select_history_entry(Some(new))?;
                        }
                    }

                    Ok(())
                }

                pub fn prev_history(&mut self, n: usize) -> io::Result<()> {
                    if !self.history.is_empty() && self.history_index != Some(0) {
                        let new = if let Some(old) = self.history_index {
                            old.saturating_sub(n)
                        } else {
                            self.history.len().saturating_sub(n)
                        };

                        self.select_history_entry(Some(new))?;
                    }

                    Ok(())
                }

                pub fn select_history_entry(&mut self, new: Option<usize>) -> io::Result<()> {
                    if new != self.history_index {
                        self.move_to(0)?;
                        self.set_history_entry(new);
                        self.new_buffer()?;
                    }

                    Ok(())
                }

                pub fn set_history_entry(&mut self, new: Option<usize>) {
                    let old = self.history_index;

                    if old != new {
                        let data = &mut *self.data;
                        data.history_index = new;

                        if let Some(old) = old {
                            data.history[old].clone_from(&data.buffer);
                        } else {
                            swap(&mut data.buffer, &mut data.backup_buffer);
                        }

                        if let Some(new) = new {
                            data.buffer.clone_from(&data.history[new]);
                        } else {
                            data.buffer.clear();
                            swap(&mut data.buffer, &mut data.backup_buffer);
                        }
                    }
                }

                fn get_history(&self, n: Option<usize>) -> &str {
                    if self.history_index == n {
                        &self.buffer
                    } else if let Some(n) = n {
                        &self.history[n]
                    } else {
                        &self.backup_buffer
                    }
                }

                pub fn backward_char(&mut self, n: usize) -> io::Result<()> {
                    let pos = backward_char(n, &self.buffer, self.cursor);
                    self.move_to(pos)
                }

                pub fn forward_char(&mut self, n: usize) -> io::Result<()> {
                    let pos = forward_char(n, &self.buffer, self.cursor);
                    self.move_to(pos)
                }

                pub fn backward_search_char(&mut self, n: usize, ch: char) -> io::Result<()> {
                    if let Some(pos) = backward_search_char(n, &self.buffer, self.cursor, ch) {
                        self.move_to(pos)?;
                    }

                    Ok(())
                }

                pub fn forward_search_char(&mut self, n: usize, ch: char) -> io::Result<()> {
                    if let Some(pos) = forward_search_char(n, &self.buffer, self.cursor, ch) {
                        self.move_to(pos)?;
                    }

                    Ok(())
                }

                /// Deletes a range from the buffer; the cursor is moved to the end
                /// of the given range.
                pub fn delete_range<R: RangeArgument<usize>>(&mut self, range: R) -> io::Result<()> {
                    let start = range.start().cloned().unwrap_or(0);
                    let end = range.end().cloned().unwrap_or_else(|| self.buffer.len());

                    self.move_to(start)?;

                    let _ = self.buffer.drain(start..end);

                    self.draw_buffer(start)?;
                    self.term.clear_to_screen_end()?;
                    let len = self.buffer.len();
                    self.move_from(len)?;

                    Ok(())
                }

                pub fn insert_str(&mut self, s: &str) -> io::Result<()> {
                    // If the string insertion moves a combining character,
                    // we must redraw starting from the character before the cursor.
                    let moves_combining = match self.buffer[self.cursor..].chars().next() {
                        Some(ch) if is_combining_mark(ch) => true,
                        _ => false
                    };

                    let cursor = self.cursor;
                    self.buffer.insert_str(cursor, s);

                    if moves_combining && cursor != 0 {
                        let pos = backward_char(1, &self.buffer, self.cursor);
                        // Move without updating the cursor
                        let (lines, cols) = self.move_delta(cursor, pos, &self.buffer);
                        self.move_rel(lines, cols)?;
                        self.draw_buffer(pos)?;
                    } else {
                        self.draw_buffer(cursor)?;
                    }

                    self.cursor += s.len();

                    let len = self.buffer.len();
                    self.move_from(len)
                }

                pub fn transpose_range(&mut self, src: Range<usize>, dest: Range<usize>)
                        -> io::Result<()> {
                    // Ranges must not overlap
                    assert!(src.end <= dest.start || src.start >= dest.end);

                    // Final cursor position
                    let final_cur = if src.start < dest.start {
                        dest.end
                    } else {
                        dest.start + (src.end - src.start)
                    };

                    let (left, right) = if src.start < dest.start {
                        (src, dest)
                    } else {
                        (dest, src)
                    };

                    self.move_to(left.start)?;

                    let a = self.buffer[left.clone()].to_owned();
                    let b = self.buffer[right.clone()].to_owned();

                    let _ = self.buffer.drain(right.clone());
                    self.buffer.insert_str(right.start, &a);

                    let _ = self.buffer.drain(left.clone());
                    self.buffer.insert_str(left.start, &b);

                    let cursor = self.cursor;
                    self.draw_buffer(cursor)?;
                    self.term.clear_to_screen_end()?;

                    self.cursor = final_cur;
                    let len = self.buffer.len();
                    self.move_from(len)
                }

                fn prompt_suffix_length(&self) -> usize {
                    match self.prompt_type {
                        PromptType::Normal => self.prompt_suffix_len,
                        PromptType::Number => {
                            let n = number_len(self.input_arg.to_i32());
                            PROMPT_NUM_PREFIX + PROMPT_NUM_SUFFIX + n
                        }
                        PromptType::Search => {
                            let mut prefix = PROMPT_SEARCH_PREFIX;

                            if self.reverse_search {
                                prefix += PROMPT_SEARCH_REVERSE_PREFIX;
                            }
                            if self.search_failed {
                                prefix += PROMPT_SEARCH_FAILED_PREFIX;
                            }

                            let n = self.display_size(&self.search_buffer, prefix);
                            prefix + n + PROMPT_SEARCH_SUFFIX
                        }
                        PromptType::CompleteIntro(n) => complete_intro(n).len(),
                        PromptType::CompleteMore => COMPLETE_MORE.len(),
                    }
                }

                fn line_col(&self, pos: usize) -> (usize, usize) {
                    let prompt_len = self.prompt_suffix_length();

                    match self.prompt_type {
                        PromptType::CompleteIntro(_) |
                        PromptType::CompleteMore => {
                            let width = self.screen_size.columns;
                            (prompt_len / width, prompt_len % width)
                        }
                        _ => self.line_col_with(pos, &self.buffer, prompt_len)
                    }
                }

                fn line_col_with(&self, pos: usize, buf: &str, start_col: usize) -> (usize, usize) {
                    let width = self.screen_size.columns;
                    if width == 0 {
                        return (0, 0);
                    }

                    let n = start_col + self.display_size(&buf[..pos], start_col);

                    (n / width, n % width)
                }

                pub fn clear_screen(&mut self) -> io::Result<()> {
                    self.term.clear_screen()?;
                    self.draw_prompt()?;

                    Ok(())
                }

                pub fn clear_to_screen_end(&mut self) -> io::Result<()> {
                    self.term.clear_to_screen_end()
                }

                /// Draws a new buffer on the screen. Cursor position is assumed to be `0`.
                pub fn new_buffer(&mut self) -> io::Result<()> {
                    self.draw_buffer(0)?;
                    self.cursor = self.buffer.len();

                    self.term.clear_to_screen_end()?;

                    Ok(())
                }

                pub fn clear_full_prompt(&mut self) -> io::Result<()> {
                    let prefix_lines = self.prompt_prefix_len / self.screen_size.columns;
                    let (line, _) = self.line_col(self.cursor);
                    self.term.move_up(prefix_lines + line)?;
                    self.term.move_to_first_column()?;
                    self.term.clear_to_screen_end()
                }

                pub(crate) fn clear_prompt(&mut self) -> io::Result<()> {
                    let (line, _) = self.line_col(self.cursor);

                    self.term.move_up(line)?;
                    self.term.move_to_first_column()?;
                    self.term.clear_to_screen_end()
                }

                /// Move back to true cursor position from some other position
                pub fn move_from(&mut self, pos: usize) -> io::Result<()> {
                    let (lines, cols) = self.move_delta(pos, self.cursor, &self.buffer);
                    self.move_rel(lines, cols)
                }

                pub fn move_to(&mut self, pos: usize) -> io::Result<()> {
                    if pos != self.cursor {
                        let (lines, cols) = self.move_delta(self.cursor, pos, &self.buffer);
                        self.move_rel(lines, cols)?;
                        self.cursor = pos;
                    }

                    Ok(())
                }

                pub fn move_to_end(&mut self) -> io::Result<()> {
                    let pos = self.buffer.len();
                    self.move_to(pos)
                }

                pub fn move_right(&mut self, n: usize) -> io::Result<()> {
                    self.term.move_right(n)
                }

                /// Moves from `old` to `new` cursor position, using the given buffer
                /// as current input.
                fn move_delta(&self, old: usize, new: usize, buf: &str) -> (isize, isize) {
                    let prompt_len = self.prompt_suffix_length();
                    let (old_line, old_col) = self.line_col_with(old, buf, prompt_len);
                    let (new_line, new_col) = self.line_col_with(new, buf, prompt_len);

                    (new_line as isize - old_line as isize,
                    new_col as isize - old_col as isize)
                }

                fn move_rel(&mut self, lines: isize, cols: isize) -> io::Result<()> {
                    if lines > 0 {
                        self.term.move_down(lines as usize)?;
                    } else if lines < 0 {
                        self.term.move_up((-lines) as usize)?;
                    }

                    if cols > 0 {
                        self.term.move_right(cols as usize)?;
                    } else if cols < 0 {
                        self.term.move_left((-cols) as usize)?;
                    }

                    Ok(())
                }

                pub fn reset_data(&mut self) {
                    self.data.reset_data();
                }

                pub fn set_digit_from_char(&mut self, ch: char) {
                    let digit = match ch {
                        '-' => Digit::NegNone,
                        '0' ..= '9' => Digit::from(ch),
                        _ => Digit::None
                    };

                    self.input_arg = digit;
                    self.explicit_arg = true;
                }
            }

            #[derive(Copy, Clone)]
            struct Blink 
            {
                pos: usize,
                expiry: Instant,
            }

            impl<'a, 'b: 'a, Term: 'b + Terminal> Writer<'a, 'b, Term>
            {
                fn new(mut write: WriterImpl<'a, 'b, Term>, clear: bool) -> io::Result<Self> {
                    write.expire_blink()?;

                    if write.is_prompt_drawn {
                        if clear {
                            write.clear_full_prompt()?;
                        } else {
                            write.move_to_end()?;
                            write.write_str("\n")?;
                        }
                    }

                    Ok(Writer{write})
                }

                pub(crate) fn with_lock(write: WriteLock<'b, Term>, clear: bool) -> io::Result<Self> {
                    Writer::new(WriterImpl::Mutex(write), clear)
                }

                pub(crate) fn with_ref(write: &'a mut WriteLock<'b, Term>, clear: bool) -> io::Result<Self> {
                    Writer::new(WriterImpl::MutRef(write), clear)
                }

                /// Returns an iterator over history entries.
                pub fn history(&self) -> HistoryIter {
                    self.write.history()
                }

                /// Writes some text to the terminal device.
                ///
                /// Before the `Writer` is dropped, any output written should be followed
                /// by a newline. A newline is automatically written if the `writeln!`
                /// macro is used.
                pub fn write_str(&mut self, s: &str) -> io::Result<()> {
                    self.write.write_str(s)
                }

                /// Writes formatted text to the terminal display.
                ///
                /// This method enables `Interface` to be used as the receiver to
                /// the [`writeln!`] macro.
                ///
                /// If the text contains any unprintable characters (e.g. escape sequences),
                /// those characters will be escaped before printing.
                ///
                /// [`read_line`]: ../interface/struct.Interface.html#method.read_line
                /// [`writeln!`]: https://doc.rust-lang.org/std/macro.writeln.html
                pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
                    let s = args.to_string();
                    self.write_str(&s)
                }
            }

            impl<'a, 'b: 'a, Term: 'b + Terminal> Drop for Writer<'a, 'b, Term> {
                fn drop(&mut self) {
                    if self.write.is_prompt_drawn {
                        // There's not really anything useful to be done with this error.
                        let _ = self.write.draw_prompt();
                    }
                }
            }

            impl<'a, Term: 'a + Terminal> Deref for WriteLock<'a, Term> {
                type Target = Write;

                fn deref(&self) -> &Write {
                    &self.data
                }
            }

            impl<'a, Term: 'a + Terminal> DerefMut for WriteLock<'a, Term> {
                fn deref_mut(&mut self) -> &mut Write {
                    &mut self.data
                }
            }

            impl Write {
                pub fn new(screen_size: Size) -> Write {
                    Write{
                        buffer: String::new(),
                        backup_buffer: String::new(),
                        cursor: 0,
                        blink: None,

                        history: VecDeque::new(),
                        history_index: None,
                        history_size: MAX_HISTORY,
                        history_new_entries: 0,

                        is_prompt_drawn: false,

                        prompt_prefix: String::new(),
                        prompt_prefix_len: 0,
                        prompt_suffix: String::new(),
                        prompt_suffix_len: 0,

                        prompt_type: PromptType::Normal,

                        reverse_search: false,
                        search_failed: false,
                        search_buffer: String::new(),
                        last_search: String::new(),
                        prev_history: None,
                        prev_cursor: !0,

                        input_arg: Digit::None,
                        explicit_arg: false,

                        screen_size,
                    }
                }

                pub fn history(&self) -> HistoryIter {
                    HistoryIter(self.history.iter())
                }

                pub fn new_history(&self) -> Skip<HistoryIter> {
                    let first_new = self.history.len() - self.history_new_entries;
                    self.history().skip(first_new)
                }

                pub fn new_history_entries(&self) -> usize {
                    self.history_new_entries
                }

                pub fn reset_data(&mut self) {
                    self.buffer.clear();
                    self.backup_buffer.clear();
                    self.cursor = 0;
                    self.history_index = None;

                    self.prompt_type = PromptType::Normal;

                    self.input_arg = Digit::None;
                    self.explicit_arg = false;
                }

                pub fn reset_new_history(&mut self) {
                    self.history_new_entries = 0;
                }

                pub fn set_buffer(&mut self, buf: &str) {
                    self.buffer.clear();
                    self.buffer.push_str(buf);
                    self.cursor = buf.len();
                }

                pub fn set_cursor(&mut self, pos: usize) {
                    if !self.buffer.is_char_boundary(pos) {
                        panic!("invalid cursor position {} in buffer {:?}",
                            pos, self.buffer);
                    }

                    self.cursor = pos;
                }

                pub fn set_prompt(&mut self, prompt: &str) {
                    let (pre, suf) = match prompt.rfind('\n') {
                        Some(pos) => (&prompt[..pos + 1], &prompt[pos + 1..]),
                        None => (&prompt[..0], prompt)
                    };

                    self.prompt_prefix = pre.to_owned();
                    self.prompt_suffix = suf.to_owned();

                    let pre_virt = filter_visible(pre);
                    self.prompt_prefix_len = self.display_size(&pre_virt, 0);

                    let suf_virt = filter_visible(suf);
                    self.prompt_suffix_len = self.display_size(&suf_virt, 0);
                }

                pub fn display_size(&self, s: &str, start_col: usize) -> usize {
                    let width = self.screen_size.columns;
                    let mut col = start_col;

                    let disp = Display{
                        allow_tab: true,
                        allow_newline: true,
                        .. Display::default()
                    };

                    for ch in s.chars().flat_map(|ch| display(ch, disp)) {
                        let n = match ch {
                            '\n' => width - (col % width),
                            '\t' => TAB_STOP - (col % TAB_STOP),
                            ch if is_combining_mark(ch) => 0,
                            ch if is_wide(ch) => {
                                if col % width == width - 1 {
                                    // Can't render a fullwidth character into last column
                                    3
                                } else {
                                    2
                                }
                            }
                            _ => 1
                        };

                        col += n;
                    }

                    col - start_col
                }
            }

            /// Maximum value of digit input
            const NUMBER_MAX: i32 = 1_000_000;

            #[derive(Copy, Clone, Debug)]
            pub(crate) enum Digit {
                None,
                NegNone,
                Num(i32),
                NegNum(i32),
            }

            impl Digit {
                pub fn input(&mut self, n: i32) {
                    match *self {
                        Digit::None => *self = Digit::Num(n),
                        Digit::NegNone => *self = Digit::NegNum(n),
                        Digit::Num(ref mut m) | Digit::NegNum(ref mut m) => {
                            *m *= 10;
                            *m += n;
                        }
                    }
                }

                pub fn is_out_of_bounds(&self) -> bool {
                    match *self {
                        Digit::Num(n) | Digit::NegNum(n) if n > NUMBER_MAX => true,
                        _ => false
                    }
                }

                pub fn to_i32(&self) -> i32 {
                    match *self {
                        Digit::None => 1,
                        Digit::NegNone => -1,
                        Digit::Num(n) => n,
                        Digit::NegNum(n) => -n,
                    }
                }
            }

            impl From<char> for Digit {
                /// Convert a decimal digit character to a `Digit` value.
                ///
                /// The input must be in the range `'0' ..= '9'`.
                fn from(ch: char) -> Digit {
                    let n = (ch as u8) - b'0';
                    Digit::Num(n as i32)
                }
            }

            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub(crate) enum PromptType {
                Normal,
                Number,
                Search,
                CompleteIntro(usize),
                CompleteMore,
            }

            impl PromptType {
                pub(crate) fn is_normal(&self) -> bool {
                    *self == PromptType::Normal
                }
            }

            impl<'a, 'b, Term: 'b + Terminal> Deref for WriterImpl<'a, 'b, Term> {
                type Target = WriteLock<'b, Term>;

                fn deref(&self) -> &WriteLock<'b, Term> {
                    match *self {
                        WriterImpl::Mutex(ref m) => m,
                        WriterImpl::MutRef(ref m) => m,
                    }
                }
            }

            impl<'a, 'b: 'a, Term: 'b + Terminal> DerefMut for WriterImpl<'a, 'b, Term> {
                fn deref_mut(&mut self) -> &mut WriteLock<'b, Term> {
                    match *self {
                        WriterImpl::Mutex(ref mut m) => m,
                        WriterImpl::MutRef(ref mut m) => m,
                    }
                }
            }

            /// Iterator over `Interface` history entries
            pub struct HistoryIter<'a>(vec_deque::Iter<'a, String>);

            impl<'a> ExactSizeIterator for HistoryIter<'a> {}

            impl<'a> Iterator for HistoryIter<'a> {
                type Item = &'a str;

                #[inline]
                fn next(&mut self) -> Option<&'a str> {
                    self.0.next().map(|s| &s[..])
                }

                #[inline]
                fn nth(&mut self, n: usize) -> Option<&'a str> {
                    self.0.nth(n).map(|s| &s[..])
                }

                #[inline]
                fn size_hint(&self) -> (usize, Option<usize>) {
                    self.0.size_hint()
                }
            }

            impl<'a> DoubleEndedIterator for HistoryIter<'a> {
                #[inline]
                fn next_back(&mut self) -> Option<&'a str> {
                    self.0.next_back().map(|s| &s[..])
                }
            }

            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub(crate) enum DisplaySequence {
                Char(char),
                Escape(char),
                End,
            }

            impl Iterator for DisplaySequence {
                type Item = char;

                fn next(&mut self) -> Option<char> {
                    use self::DisplaySequence::*;

                    let (res, next) = match *self {
                        Char(ch) => (ch, End),
                        Escape(ch) => ('^', Char(ch)),
                        End => return None
                    };

                    *self = next;
                    Some(res)
                }

                fn size_hint(&self) -> (usize, Option<usize>) {
                    use self::DisplaySequence::*;

                    let n = match *self {
                        Char(_) => 1,
                        Escape(_) => 2,
                        End => 0,
                    };

                    (n, Some(n))
                }
            }

            #[derive(Copy, Clone, Debug, Default)]
            pub(crate) struct Display {
                allow_tab: bool,
                allow_newline: bool,
                allow_escape: bool,
            }

            pub(crate) fn display(ch: char, style: Display) -> DisplaySequence {
                match ch {
                    '\t' if style.allow_tab => DisplaySequence::Char(ch),
                    '\n' if style.allow_newline => DisplaySequence::Char(ch),
                    ESCAPE if style.allow_escape => DisplaySequence::Char(ch),
                    '\0' => DisplaySequence::Escape('@'),
                    RUBOUT => DisplaySequence::Escape('?'),
                    ch if is_ctrl(ch) => DisplaySequence::Escape(unctrl(ch)),
                    ch => DisplaySequence::Char(ch)
                }
            }

            pub(crate) fn display_str<'a>(s: &'a str, style: Display) -> Cow<'a, str> {
                if s.chars().all(|ch| display(ch, style) == DisplaySequence::Char(ch)) {
                    Borrowed(s)
                } else {
                    Owned(s.chars().flat_map(|ch| display(ch, style)).collect())
                }
            }

            fn complete_intro(n: usize) -> String {
                format!("Display all {} possibilities? (y/n)", n)
            }

            fn number_len(n: i32) -> usize {
                match n {
                    -1_000_000              => 8,
                    -  999_999 ..= -100_000 => 7,
                    -   99_999 ..= - 10_000 => 6,
                    -    9_999 ..= -  1_000 => 5,
                    -      999 ..= -    100 => 4,
                    -       99 ..= -     10 => 3,
                    -        9 ..= -      1 => 2,
                            0 ..=        9 => 1,
                            10 ..=       99 => 2,
                        100 ..=      999 => 3,
                        1_000 ..=    9_999 => 4,
                        10_000 ..=   99_999 => 5,
                    100_000 ..=  999_999 => 6,
                    1_000_000              => 7,
                    _ => unreachable!()
                }
            }
        }
        pub use self::writer::{ Writer };
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
            prompt::lines::{ Function, Prompter, Terminals },
            *,
        };

        pub struct EnterFunction;

        impl<T: Terminals> Function<T> for EnterFunction
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
    pub use self::multilines::{ EnterFunction };
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
    
    pub mod v1 {/* UNSUPPORTED */}
    mod v3 {/* UNSUPPORTED */}

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
    mod v5 {/* UNSUPPORTED */}
    mod v6 {/* UNSUPPORTED */}
    mod v7 {/* UNSUPPORTED */}
    mod v8 {/* UNSUPPORTED */}

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
        iter::{ pair::Pair },
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
        }
        */
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
        error::{ errno },
        ffi::{ CStr, CString },
        fs::{ File, },
        io::{Read, Write},
        libs::pipes::{ pipe },
        nix::unistd::{ execve, ForkResult },
        os::
        { 
            fd::{ RawFd },
            unix::io::FromRawFd
        },
        regex::{ Regex },
        types::{ self, CommandLine, CommandOptions, CommandResult },
        uuid::{ Uuid },
        *,
    };
    
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
        /// Update existing <ENVVAR> if such name exists in ENVs, 
        /// or define a local *Shell Variable*, which would not be exported into child processes.
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
                            let (term_given, cr) = run_pipeline(sh, &c, true, true, false);
                            if term_given
                            {
                                unsafe 
                                {
                                    let gid = libc::getpgid(0);
                                    give_terminal_to(gid);
                                } 
                            }

                            cr
                        }

                        Err(e) =>
                        {
                            println_stderr!("cicada: {}", e);
                            continue;
                        }
                    };

                    let output_txt = cmd_result.stdout.trim();

                    let ptn = r"(?P<head>[^\$]*)\$\(.+\)(?P<tail>.*)";
                    let re;

                    if let Ok(x) = Regex::new(ptn) { re = x; }                    
                    else { return; }

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
                        let (term_given, _cr) = run_pipeline(sh, &c, true, true, false);
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
                                let (term_given, _cr) = run_pipeline(sh, &c, true, true, false);
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

    fn try_run_builtin_in_subprocess( sh:&mut Shell, cl:&CommandLine, idx_cmd:usize, capture:bool ) -> Option<i32>
    {
        if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture) { return Some(cr.status); }
        None
    }

    fn try_run_builtin( sh:&mut Shell, cl:&CommandLine,  idx_cmd:usize, capture:bool ) -> Option<CommandResult>
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
        match cname
        {
            "alias" => 
            {
                let cr = builtins::alias::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "bg" => 
            {                
                let cr = builtins::bg::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "cd" => 
            {                
                let cr = builtins::cd::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "cinfo" => 
            {
                let cr = builtins::cinfo::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "exit" => 
            {
                let cr = builtins::exit::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "export" => 
            {
                let cr = builtins::export::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "fg" => 
            {
                let cr = builtins::fg::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "history" => 
            {
                let cr = builtins::history::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "jobs" => 
            {
                let cr = builtins::jobs::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "minfd" => 
            {
                let cr = builtins::minfd::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "read" => 
            {
                let cr = builtins::read::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "set" => 
            {
                let cr = builtins::set::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "source" => 
            {
                let cr = builtins::source::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "ulimit" => 
            {
                let cr = builtins::ulimit::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "unalias" => 
            {
                let cr = builtins::unalias::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "unset" => 
            {
                let cr = builtins::unset::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "unpath" => 
            {
                let cr = builtins::unpath::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            "vox" => 
            {
                let cr = builtins::vox::run(sh, cl, cmd, capture);
                return Some(cr);
            }
            _ => { return None; }
        }
    }
    /// Run a pipeline (e.g. `echo hi | wc -l`); returns: (is-terminal-given, command-result)
    pub fn run_pipeline( sh: &mut shell::Shell, cl: &CommandLine, tty: bool, capture: bool, log_cmd: bool ) ->
    ( bool, CommandResult )
    {
        let mut term_given = false;
        if cl.background && capture
        {
            println_stderr!("cicada: cannot capture output of background cmd");
            return (term_given, CommandResult::error());
        }
        
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
        let isatty = if tty { unsafe { libc::isatty(1) == 1 } } else { false };
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

        if cl.is_single_and_builtin()
        {
            return (false, cmd_result);
        }

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

    /// Run a single command.
    /// e.g. the `sort -k2` part of `ps ax | sort -k2 | head`
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
            match pipe()
            {
                Ok(fds) => fds_stdin = Some(fds),
                Err(e) =>
                {
                    println_stderr!("cicada: pipeline4: {}", e);
                    return 1;
                }
            }
        }

        match libs::fork::fork()
        {
            Ok(ForkResult::Child) =>
            {
                unsafe
                {
                    libc::signal(libc::SIGTSTP, libc::SIG_DFL);
                    libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                }
                
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
                    unsafe
                    {
                        let pid = libc::getpid();
                        libc::setpgid(0, pid);
                    }
                } 

                else { unsafe { libc::setpgid(0, *pgid); } }
                
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
                        if fd == -1
                        {
                            process::exit(1);
                        }

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
                        if idx_cmd < pipes_count { libs::dup2(1, 2); }
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

    fn try_run_func
    (
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
    //! Contains types relating to operating system signals
    use ::
    {
        collections::{ HashMap, HashSet },
        error::{ errno, set_errno },
        iter::{ FromIterator },
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
    
    pub const NUM_SIGNALS: u8 = 6;
    
    macro_rules! impl_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) => {
            impl ops::$tr for SignalSet {
                type Output = SignalSet;

                fn $tr_meth(self, rhs: SignalSet) -> SignalSet {
                    self.$method(rhs)
                }
            }
        }
    }

    macro_rules! impl_mut_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) =>
        {
            impl ops::$tr for SignalSet {
                fn $tr_meth(&mut self, rhs: SignalSet) {
                    *self = self.$method(rhs);
                }
            }
        }
    }

    macro_rules! impl_unary_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) =>
        {
            impl ops::$tr for SignalSet {
                type Output = SignalSet;

                fn $tr_meth(self) -> SignalSet {
                    self.$method()
                }
            }
        }
    }

    /// Signal received through a terminal device
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum Signal
    {
        /// Break signal (`CTRL_BREAK_EVENT`); Windows only
        Break,
        /// Continue signal (`SIGCONT`); Unix only
        Continue,
        /// Interrupt signal (`SIGINT` on Unix, `CTRL_C_EVENT` on Windows)
        Interrupt,
        /// Terminal window resize (`SIGWINCH` on Unix,
        /// `WINDOW_BUFFER_SIZE_EVENT` on Windows)
        ///
        /// When this signal is received, it will be translated into an
        /// `Event::Resize(_)` value containing the new size of the terminal.
        Resize,
        /// Suspend signal (`SIGTSTP`); Unix only
        Suspend,
        /// Quit signal (`SIGQUIT`); Unix only
        Quit,
    }
    
    impl Signal
    {
        fn as_bit(&self) -> u8 { 1 << (*self as u8) }
        fn all_bits() -> u8 { (1 << NUM_SIGNALS) - 1 }
    }

    impl ops::BitOr for Signal
    {
        type Output = SignalSet;
        fn bitor(self, rhs: Signal) -> SignalSet
        {
            let mut set = SignalSet::new();
            set.insert(self);
            set.insert(rhs);
            set
        }
    }

    impl ops::Not for Signal
    {
        type Output = SignalSet;
        fn not(self) -> SignalSet { !SignalSet::from(self) }
    }
    /// Represents a set of `Signal` values
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct SignalSet(u8);

    impl SignalSet
    {
        /// Returns an empty `SignalSet`.
        pub fn new() -> SignalSet { SignalSet(0) }
        /// Returns a `SignalSet` containing all available signals.
        pub fn all() -> SignalSet { SignalSet(Signal::all_bits()) }
        /// Returns whether this set contains the given `Signal`.
        pub fn contains(&self, sig: Signal) -> bool { self.0 & sig.as_bit() != 0 }
        /// Returns whether this set contains all signals present in another set.
        pub fn contains_all(&self, other: SignalSet) -> bool { self.0 & other.0 == other.0 }
        /// Returns whether this set contains any signals present in another set.
        pub fn intersects(&self, other: SignalSet) -> bool { self.0 & other.0 != 0 }
        /// Returns whether this set contains any signals.
        pub fn is_empty(&self) -> bool { self.0 == 0 }
        /// Inserts a `Signal` into this set.
        pub fn insert(&mut self, sig: Signal) { self.0 |= sig.as_bit(); }
        /// Removes a `Signal` from this set.
        pub fn remove(&mut self, sig: Signal) { self.0 &= !sig.as_bit(); }
        /// Sets whether this set contains the given `Signal`.
        pub fn set(&mut self, sig: Signal, set: bool)
        {
            if set { self.insert(sig); }
            else { self.remove(sig); }
        }
        /// Returns the difference of two sets.
        pub fn difference(&self, other: SignalSet) -> SignalSet { SignalSet(self.0 & !other.0) }
        /// Returns the symmetric difference of two sets.
        pub fn symmetric_difference(&self, other: SignalSet) -> SignalSet { SignalSet(self.0 ^ other.0) }
        /// Returns the intersection of two sets.
        pub fn intersection(&self, other: SignalSet) -> SignalSet { SignalSet(self.0 & other.0) }
        /// Returns the union of two sets.
        pub fn union(&self, other: SignalSet) -> SignalSet { SignalSet(self.0 | other.0) }
        /// Returns the inverse of the set.
        pub fn inverse(&self) -> SignalSet { SignalSet(!self.0 & Signal::all_bits()) }
    }

    impl fmt::Debug for SignalSet
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
        {
            const SIGNALS: &[Signal] = &
            [
                Signal::Break,
                Signal::Continue,
                Signal::Interrupt,
                Signal::Resize,
                Signal::Suspend,
                Signal::Quit,
            ];

            let mut first = true;

            f.write_str("SignalSet(")?;

            for &sig in SIGNALS
            {
                if self.contains(sig)
                {
                    if !first { f.write_str(" | ")?; }
                    write!(f, "{:?}", sig)?;
                    first = false;
                }
            }

            f.write_str(")")
        }
    }

    impl From<Signal> for SignalSet
    {
        fn from(sig: Signal) -> SignalSet
        {
            let mut set = SignalSet::new();
            set.insert(sig);
            set
        }
    }

    impl Extend<Signal> for SignalSet
    {
        fn extend<I: IntoIterator<Item=Signal>>(&mut self, iter: I)
        {
            for sig in iter
            {
                self.insert(sig);
            }
        }
    }

    impl FromIterator<Signal> for SignalSet
    {
        fn from_iter<I: IntoIterator<Item=Signal>>(iter: I) -> SignalSet
        {
            let mut set = SignalSet::new();
            set.extend(iter);
            set
        }
    }

    impl_op!{ BitAnd, bitand, intersection }
    impl_op!{ BitOr, bitor, union }
    impl_op!{ BitXor, bitxor, symmetric_difference }
    impl_op!{ Sub, sub, difference }

    impl_unary_op!{ Not, not, inverse }

    impl_mut_op!{ BitAndAssign, bitand_assign, intersection }
    impl_mut_op!{ BitOrAssign, bitor_assign, union }
    impl_mut_op!{ BitXorAssign, bitxor_assign, symmetric_difference }
    impl_mut_op!{ SubAssign, sub_assign, difference }

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
        let saved_errno = error::errno();
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
                Ok(_others)                              => { log!("sigchld others: {:?}", _others); }
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
pub use std::{ *, };
/// Represents an error calling `exec`.
pub use ::types::CommandResult;
pub use ::types::LineInfo;
/**/
use ::
{
    fmt::{ * },
    io::{ stderr, Write },
    sync::{ Arc },
};
/// Parse a command to tokens.
pub fn parse_line(cmd: &str) -> LineInfo { parsers::parser_line::parse_line( cmd ) }
/// Run a command or a pipeline.
pub fn run( line:&str ) -> CommandResult { execute::run( line ) }

pub unsafe fn domain()
{
    use ::
    {
        prompt::lines::
        {
            command::{ Command },
            interface::{ Interface },
            reader::{ ReadResult, },
        },
    };

    tools::init_path_env();
    let mut sh = shell::Shell::new();
    let args: Vec<String> = env::args().collect();

    if libs::progopts::is_login(&args)
    {
        rcfile::load_rc_files(&mut sh);
        sh.is_login = true;
    }
    
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

        if sig_handler_enabled { signals::unblock_signals(); }

        match rl.read_line()
        {
            Ok(ReadResult::Input(line)) =>
            {
                if sig_handler_enabled { signals::block_signals(); }

                let line = shell::trim_multiline_prompts(&line);
                if line.trim() == ""
                {
                    jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                    continue;
                }
                sh.cmd = line.clone();

                let tsb = ctime::DateTime::now().unix_timestamp();
                let mut line = line.clone();
                tools::extend_bangbang(&sh, &mut line);

                let mut status = 0;
                let cr_list = execute::run_command_line(&mut sh, &line, true, false);

                if let Some(last) = cr_list.last() { status = last.status; }

                let tse = ctime::DateTime::now().unix_timestamp();

                if !sh.cmd.starts_with(' ') && line != sh.previous_cmd {
                    history::add(&sh, &mut rl, &line, status, tsb, tse);
                    sh.previous_cmd = line.clone();
                }

                if tools::is_shell_altering_command(&line) 
                {
                    rl.set_completer(Arc::new(completers::CicadaCompleter
                    {
                        sh: Arc::new(sh.clone()),
                    }));
                    
                    highlight::update_aliases(&sh);
                }

                jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                continue;
            }
            Ok(ReadResult::Eof) => 
            {
                if let Ok(x) = env::var("NO_EXIT_ON_CTRL_D") {
                    if x == "1" {
                        println!();
                    }
                } else {
                    println!("exit");
                    break;
                }
            }

            Ok(ReadResult::Signal(s)) => { println_stderr!("readline signal: {:?}", s); }

            Err(e) =>
            {
                println_stderr!("readline error: {}", e);
                unsafe
                {
                    let gid = libc::getpgid(0);
                    shell::give_terminal_to(gid);
                }
            }
        }

        if sig_handler_enabled { signals::block_signals(); }
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
// 19666
