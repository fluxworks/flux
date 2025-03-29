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
    unknown_lints,
    unused_imports,
    unused_variables,
    unreachable_code,    
 )]
/*
External Macros */
#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
//#[macro_use] extern crate pest_derive;
/*
External Crates */
extern crate clap;
extern crate fnv;
extern crate libc;
extern crate memchr;
extern crate nix;
extern crate phf;
extern crate regex as re;
extern crate smallvec;
extern crate time as timing;
extern crate unicode_normalization;
extern crate unicode_width;
/*
extern crate errno;
extern crate exec;
extern crate glob;
extern crate linefeed;
extern crate pest;
extern crate rusqlite;
extern crate yaml_rust;
*/
use ::
{
    io::{ Write },
    prompt::lines::{ Command, Interface, ReadResult },
    sync::{ Arc },
};
/*
Macros */
#[macro_use] mod macros
{
    use ::
    {
        collections::{ HashMap },
        fs::{ OpenOptions },
        io::{ Write },
        os::unix::io::{ IntoRawFd },
        path::{ Path, PathBuf },
        *
    };

    pub use std::
    {
        assert_eq, assert_ne, debug_assert, debug_assert_eq, debug_assert_ne, unreachable, unimplemented, write, 
        writeln, todo,
    };

    #[macro_export] macro_rules! println_stderr
    {
        ( $fmt:expr ) =>
        ( 
            match writeln!( &mut ::io::stderr(), $fmt )
            {
                Ok( _ ) => {}
                Err( e ) => println!( "write to stderr failed:{:?}", e )
            }
 );

        ( $fmt:expr, $( $arg:tt )* ) =>
        ( 
            match writeln!( &mut ::io::stderr(), $fmt, $( $arg )* )
            {
                Ok( _ ) => {}
                Err( e ) => println!( "write to stderr failed:{:?}", e )
            }
 );
    }
}

pub mod alloc { pub use std::alloc::{ * }; }
pub mod any   { pub use std::arch::{ * }; }
pub mod arch  { pub use std::arch::{ * }; }
pub mod array { pub use std::array::{ * }; }
pub mod ascii
{
    pub use std::ascii::{ * };

    const NONE:u8 = 0b000000;
    const PRINT:u8 = 0b000001;
    const SPACE:u8 = 0b000010;
    const CONTROL:u8 = 0b000100;
    const PIPE:u8 = 0b001000;
    const COMMA:u8 = 0b010000;
    const EOL:u8 = 0b100000;
    
    static ASCII:[u8; 256] =
    [
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, SPACE, EOL, NONE, NONE, EOL, NONE, NONE, NONE, NONE, 
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, PRINT | SPACE, PRINT, PRINT,
        PRINT | CONTROL, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT | COMMA | CONTROL, PRINT, PRINT,
        PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT | CONTROL,
        PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
        PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
        PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
        PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT | PIPE,
        PRINT, PRINT, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
        NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
    ];

    #[inline( always )] pub fn is_ws( ch:u8 ) -> bool { unsafe { ASCII.get_unchecked( ch as usize ) & SPACE == SPACE } }

    #[inline( always )] pub fn is_eol( ch:u8 ) -> bool { unsafe { ASCII.get_unchecked( ch as usize ) & EOL == EOL } }

    #[inline( always )] pub fn is_printable_no_pipe( ch:u8 ) -> bool 
    { unsafe { ASCII.get_unchecked( ch as usize ) & ( PRINT | PIPE ) == PRINT } }

    #[inline( always )] pub fn is_printable_no_comma( ch:u8 ) -> bool 
    { unsafe { ASCII.get_unchecked( ch as usize ) & ( PRINT | COMMA ) == PRINT } }

    #[inline( always )] pub fn is_printable_no_control( ch:u8 ) -> bool
    {
        unsafe { ASCII.get_unchecked( ch as usize ) & ( PRINT | CONTROL ) == PRINT }
    }
}
pub mod borrow { pub use std::borrow::{ * }; }
pub mod boxed { pub use std::boxed::{ * }; }
pub mod builtins
{
    pub mod alias
    {
        use ::
        {
            regex::{ Regex },
            types::{ Command, CommandLine, CommandResult },
            print::{ print_stderr_with_capture, print_stdout_with_capture },
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
            print::{ print_stderr_with_capture },
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
            print::{ print_stderr_with_capture },
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

            if !Path::new( &dir_to ).exists() {
                let info = format!( "pls:cd:{}:No such file or directory", &args[1] );
                print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                return cr;
            }

            match Path::new( &dir_to ).canonicalize()
            {
                Ok( p ) => {
                    dir_to = p.as_path().to_string_lossy().to_string();
                }
                Err( e ) => {
                    let info = format!( "pls:cd:error:{}", e );
                    print_stderr_with_capture( &info, &mut cr, cl, cmd, capture );
                    return cr;
                }
            }

            match env::set_current_dir( &dir_to )
            {
                Ok( _ ) => {
                    sh.current_dir = dir_to.clone();
                    if str_current_dir != dir_to {
                        sh.previous_dir = str_current_dir.clone();
                        env::set_var( "PWD", &sh.current_dir );
                    };
                    cr.status = 0;
                    cr
                }
                Err( e ) => {
                    let info = format!( "pls:cd:{}", e );
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
            print::{ print_stdout_with_capture },
            shell::{ Shell },
            types::{ Command, CommandLine, CommandResult },
            rc::{ file },
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
            print::{ print_stderr_with_capture },
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

            let mut _cmd = exec::Command::new( &args[1] );
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
            print::{ print_stderr_with_capture },
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
                    Ok( x ) => {
                        process::exit( x );
                    }
                    Err( _ ) => {
                        let info = format!( "pls:exit:{}:numeric argument required", _code );
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
            print::{ print_stderr_with_capture },
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
            print::{ print_stderr_with_capture },
            shell::{ self, Shell },
            types::{ CommandResult, CommandLine, Command },
            *
        };

        pub fn run( sh:&mut Shell, cl:&CommandLine, cmd:&Command, capture:bool ) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty() {
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
                    Err( _ ) => {
                        let info = "pls:fg:invalid job id";
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
                    Some( job ) => {
                        print_stderr_with_capture( &job.cmd, &mut cr, cl, cmd, capture );
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
                    None => {
                        let info = "pls:fg:no such job";
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
            print::{ print_stderr_with_capture, print_stdout_with_capture, },
            //rusqlite::{ Connection as Conn },
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
            if !path.exists() {
                let info = "no history file";
                print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                return cr;
            }
            let conn = match Conn::open( &hfile )
            {
                Ok( x ) => x,
                Err( e ) => {
                    let info = format!( "history:sqlite error:{:?}", e );
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
                Ok( opt ) => {
                    match opt.cmd {
                        Some( SubCommand::Delete {rowid:rowids} ) => {
                            let mut _count = 0;
                            for rowid in rowids {
                                let _deleted = delete_history_item( &conn, rowid );
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
                        Some( SubCommand::Add {timestamp:ts, input} ) => {
                            let ts = ts.unwrap_or( 0 as f64 );
                            add_history( sh, ts, &input );
                            cr
                        }
                        None => {
                            let ( str_out, str_err ) = list_current_history( sh, &conn, &opt );
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
                Err( e ) => {
                    let info = format!( "{}", e );
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

        pub fn list_current_history( sh:&Shell, conn:&Conn, opt:&OptMain ) -> ( String, String )
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

            let mut stmt = match conn.prepare( &sql )
            {
                Ok( x ) => x,
                Err( e ) => {
                    let info = format!( "history:prepare select error:{:?}", e );
                    result_stderr.push_str( &info );
                    return ( result_stdout, result_stderr );
                }
            };

            let mut rows = match stmt.query( [] )
            {
                Ok( x ) => x,
                Err( e ) => {
                    let info = format!( "history:query error:{:?}", e );
                    result_stderr.push_str( &info );
                    return ( result_stdout, result_stderr );
                }
            };

            let mut lines = Vec::new();
            loop {
                match rows.next() {
                    Ok( _rows ) => {
                        if let Some( row ) = _rows {
                            let row_id:i32 = match row.get( 0 ) {
                                Ok( x ) => x,
                                Err( e ) => {
                                    let info = format!( "history:error:{:?}", e );
                                    result_stderr.push_str( &info );
                                    return ( result_stdout, result_stderr );
                                }
                            };
                            let inp:String = match row.get( 1 ) {
                                Ok( x ) => x,
                                Err( e ) => {
                                    let info = format!( "history:error:{:?}", e );
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
                                    Err( e ) => {
                                        let info = format!( "history:error:{:?}", e );
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
                    Err( e ) => {
                        let info = format!( "history:rows next error:{:?}", e );
                        result_stderr.push_str( &info );
                        return ( result_stdout, result_stderr );
                    }
                }
            }

            let buffer = lines.join( "\n" );

            ( buffer, result_stderr )
        }

        fn delete_history_item( conn:&Conn, rowid:usize ) -> bool
        {
            let history_table = get::history_table();
            let sql = format!( "DELETE from {} where rowid = {}", history_table, rowid );
            match conn.execute( &sql, [] )
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
            print::{ print_stdout_with_capture },
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
            for ( _i, job ) in jobs.iter() {
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
            print::{ print_stderr_with_capture },
            shell::{ Shell },
            regex::{ re_contains },
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
                name_list = vec!["REPLY".to_string()];
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
                    Err( e ) => {
                        let info = format!( "pls:read:error in reading stdin:{:?}", e );
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
            print::{ print_stderr_with_capture },
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
            print::{ print_stderr_with_capture },
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
            if !sh.remove_alias( input ) {
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
            print::{ print_stderr_with_capture, print_stdout_with_capture, },
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
                Err( _ ) => {
                    return String::from( "vox:cannot read PATH env" );
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
                    Ok( venvs ) => {
                        let info = venvs.join( "\n" );
                        print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                        return cr;
                    }
                    Err( reason ) => {
                        print_stderr_with_capture( &reason, &mut cr, cl, cmd, capture );
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
                let cr_list = now::run_command_line( sh, &line, false, false );
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
            print::{ print_stderr_with_capture, print_stdout_with_capture },
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
                Some( Some( value ) ) => {
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
            print::{ print_stdout_with_capture },
            shell::{ Shell },
            types::{CommandResult, CommandLine, Command},
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
                Ok( fd ) => {
                    let info = format!( "{}", fd );
                    print_stdout_with_capture( &info, &mut cr, cl, cmd, capture );
                    unsafe { libc::close( fd ); }
                }
                Err( e ) => {
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
            print::{ print_stderr_with_capture, print_stdout_with_capture },
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
                Ok( opt ) => {
                    if opt.exit_on_error {
                        sh.exit_on_error = true;
                        cr
                    } else {
                        let info = "pls:set:option not implemented";
                        print_stderr_with_capture( info, &mut cr, cl, cmd, capture );
                        cr
                    }
                }
                Err( e ) => {
                    let info = format!( "{}", e );
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
            print::{ print_stderr_with_capture },
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
            print::{ print_stderr_with_capture },
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
            if !sh.remove_env( input ) {
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
                timing::{ OffsetDateTime },
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

            pub fn unix_timestamp( &self ) -> f64
            {
                self.odt.unix_timestamp_nanos() as f64 / 1000000000.0
            }
        }

        impl fmt::Display for DateTime
        {
            fn fmt( &self, f:&mut fmt::Formatter<'_> ) -> fmt::Result
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
            if !report {
                return;
            }

            // add an extra line to separate output of fg commands if any.
            if let Some( job ) = sh.get_job_by_gid( gid ) {
                println_stderr!( "" );
                print_job( job );
            }
        }

        pub fn mark_job_member_stopped( sh:&mut shell::Shell, pid:i32, gid:i32, report:bool )
        {
            let _gid = if gid == 0 {
                unsafe { libc::getpgid( pid ) }
            } else {
                gid
            };

            if let Some( job ) = sh.mark_job_member_stopped( pid, gid ) {
                if job.all_members_stopped() {
                    mark_job_as_stopped( sh, gid, report );
                }
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
                Ok( WS::Exited( pid, status ) ) => {
                    let pid = i32::from( pid );
                    types::WaitStatus::from_exited( pid, status )
                }
                Ok( WS::Stopped( pid, sig ) ) => {
                    let pid = i32::from( pid );
                    types::WaitStatus::from_stopped( pid, sig as i32 )
                }
                Ok( WS::Continued( pid ) ) => {
                    let pid = i32::from( pid );
                    types::WaitStatus::from_continuted( pid )
                }
                Ok( WS::Signaled( pid, sig, _core_dumped ) ) => {
                    let pid = i32::from( pid );
                    types::WaitStatus::from_signaled( pid, sig as i32 )
                }
                Ok( WS::StillAlive ) => {
                    types::WaitStatus::empty()
                }
                Ok( _others ) => {
                    // this is for PtraceEvent and PtraceSyscall on Linux,
                    // unreachable on other platforms.
                    types::WaitStatus::from_others()
                }
                Err( e ) => {
                    types::WaitStatus::from_error( e as i32 )
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
pub mod cell { pub use std::cell::{ * }; }
pub mod char
{
    pub use std::char::{ * };

    /// Returns the width of a character in the terminal.
    #[inline] pub fn char_width( ch:char ) -> Option<usize>
    {
        use unicode_width::UnicodeWidthChar;
        ch.width()
    }
}
pub mod clone { pub use std::clone::{ * }; }
pub mod cmp { pub use std::cmp::{ * }; }
pub mod collections { pub use std::collections::{ * }; }
pub mod completers
{
    use ::
    {
        path::{ Path },
        regex::{ Regex },
        slice::{ unescape },
        sync::{ Arc },
        *,
    };
    /*
    use linefeed::complete::{Completer, Completion};
    use linefeed::prompter::Prompter;
    use linefeed::terminal::Terminal;
    */
    pub mod dots
    {
        /*
        use linefeed::complete::escape;
        use linefeed::complete::escaped_word_start;
        use linefeed::complete::unescape;
        use linefeed::complete::Suffix;
        use linefeed::complete::{Completer, Completion};
        use linefeed::prompter::Prompter;
        use linefeed::terminal::Terminal;
        use yaml_rust::{Yaml, YamlLoader};
        use yaml_rust::yaml::Hash;
        */
        use ::
        {
            borrow::{ Cow },
            fs::{ File },
            hash::{ Hash },
            io::{ Read, Write },
            path::{ Path },
            slice::{ unescape },
            *,
        };

        /// Performs completion by searching dotfiles
        pub struct DotsCompleter;

        impl<Term:Terminal> Completer<Term> for DotsCompleter
        {
            fn complete
            ( 
                &self,
                word:&str,
                reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>>
            {
                let line = reader.buffer();
                Some( complete_dots( line, word ) )
            }

            fn word_start( &self, line:&str, end:usize, _reader:&Prompter<Term> ) -> usize
            {
                completers::escaped_word_start( &line[..end] )
            }

            fn quote<'a>( &self, word:&'a str ) -> Cow<'a, str>
            {
                regex::escape( word )
            }

            fn unquote<'a>( &self, word:&'a str ) -> Cow<'a, str>
            {
                unescape( word )
            }
        }

        fn get_dot_file( line:&str ) -> ( String, String )
        {
            let args = parsers::line::line_to_plain_tokens( line );
            let dir = get::user_completer_directory();
            let dot_file = format!( "{}/{}.yaml", dir, args[0] );
            if !Path::new( &dot_file ).exists() {
                return ( String::new(), String::new() );
            }
            let sub_cmd = if ( args.len() >= 3 && !args[1].starts_with( '-' ) )
                || ( args.len() >= 2 && !args[1].starts_with( '-' ) && line.ends_with( ' ' ) )
            {
                args[1].as_str()
            } else {
                ""
            };

            ( dot_file, sub_cmd.to_string() )
        }

        fn handle_lv1_string( res:&mut Vec<Completion>, value:&str, word:&str )
        {
            if !value.starts_with( word ) && !value.starts_with( '`' ) {
                return;
            }

            let linfo = parsers::line::parse_line( value );
            let tokens = linfo.tokens;
            if tokens.len() == 1 && tokens[0].0 == "`" {
                //log!( "run subcmd:{:?}", &tokens[0].1 );
                let cr = now::run( &tokens[0].1 );
                let v:Vec<&str> = cr.stdout.split( |c| c == '\n' || c == ' ' ).collect();
                for s in v {
                    if s.trim().is_empty() {
                        continue;
                    }
                    handle_lv1_string( res, s, word );
                }
                return;
            }

            let display = None;
            let suffix = Suffix::Default;
            res.push( Completion {
                completion:value.to_string(),
                display,
                suffix,
            } );
        }

        fn handle_lv1_hash( res:&mut Vec<Completion>, h:&Hash, word:&str )
        {
            for v in h.values() {
                if let Yaml::Array( ref arr ) = v {
                    for s in arr {
                        if let Yaml::String( value ) = s {
                            if !value.starts_with( word ) && !value.starts_with( '`' ) {
                                continue;
                            }
                            handle_lv1_string( res, value, word );
                        }
                    }
                }
            }
        }

        fn complete_dots( line:&str, word:&str ) -> Vec<Completion>
        {
            let mut res = Vec::new();
            if line.trim().is_empty() {
                return res;
            }
            let ( dot_file, sub_cmd ) = get_dot_file( line );
            if dot_file.is_empty() {
                return res;
            }

            let mut f;
            match File::open( &dot_file ) {
                Ok( x ) => f = x,
                Err( e ) => {
                    println_stderr!( "\ncicada:open dot_file error:{:?}", e );
                    return res;
                }
            }

            let mut s = String::new();
            match f.read_to_string( &mut s ) {
                Ok( _ ) => {}
                Err( e ) => {
                    println_stderr!( "\ncicada:read_to_string error:{:?}", e );
                    return res;
                }
            }

            let docs = match YamlLoader::load_from_str( &s ) {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "\ncicada:Bad Yaml file:{}:{:?}", dot_file, e );
                    return res;
                }
            };

            for doc in docs.iter() {
                match *doc {
                    Yaml::Array( ref v ) => {
                        for x in v {
                            match *x {
                                Yaml::String( ref name ) => {
                                    if !sub_cmd.is_empty() {
                                        continue;
                                    }
                                    handle_lv1_string( &mut res, name, word );
                                }
                                Yaml::Hash( ref h ) => {
                                    if sub_cmd.is_empty() {
                                        for k in h.keys() {
                                            if let Yaml::String( value ) = k {
                                                handle_lv1_string( &mut res, value, word );
                                            }
                                        }
                                    } else {
                                        let key = Yaml::from_str( &sub_cmd );
                                        if !h.contains_key( &key ) {
                                            continue;
                                        }
                                        handle_lv1_hash( &mut res, h, word );
                                    }
                                }
                                _ => {
                                    println_stderr!( "\nThis yaml file is in bad format:{}", dot_file );
                                }
                            }
                        }
                    }
                    _ => {
                        println_stderr!( "\nThis yaml file is in bad format:{}", dot_file );
                    }
                }
            }
            res
        }
    }

    pub mod environment
    {
        /*
        use linefeed::complete::{Completer, Completion, Suffix};
        use linefeed::prompter::Prompter;
        use linefeed::terminal::Terminal;
        */
        use ::
        {
            sync::{ Arc },
            *,
        };

        pub struct EnvCompleter
        {
            pub sh:Arc<shell::Shell>,
        }

        impl<Term:Terminal> Completer<Term> for EnvCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap( self.sh.clone() );
                match sh {
                    Ok( x ) => Some( complete_env( &x, word ) ),
                    Err( x ) => Some( complete_env( &x, word ) ),
                }
            }
        }

        fn complete_env( sh:&shell::Shell, path:&str ) -> Vec<Completion>
        {
            let mut res = Vec::new();
            if path.trim().is_empty() {
                return res;
            }
            let mut prefix = path.to_string();
            prefix.remove( 0 );

            for ( key, _ ) in env::vars_os() {
                let env_name = key.to_string_lossy().to_string();
                if env_name.starts_with( &prefix ) {
                    res.push( Completion {
                        completion:format!( "${}", env_name ),
                        display:None,
                        suffix:Suffix::Default,
                    } );
                }
            }

            // sh.envs is a just clone here; see FIXME in main.rs
            for key in sh.envs.keys() {
                if key.starts_with( &prefix ) {
                    res.push( Completion {
                        completion:format!( "${}", key ),
                        display:None,
                        suffix:Suffix::Default,
                    } );
                }
            }

            res
        }
    }

    pub mod make
    {
        /*
        use linefeed::complete::{Completer, Completion, Suffix};
        use linefeed::prompter::Prompter;
        use linefeed::terminal::Terminal; */
        use ::
        {
            fs::{ File },
            io::{ BufRead, BufReader, Write },
            regex::{ Regex },
            *
        };

        pub struct MakeCompleter;

        impl<Term:Terminal> Completer<Term> for MakeCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                Some( complete_make( word ) )
            }
        }

        fn handle_file( ci:&mut Vec<Completion>, path:&str, file_path:&str, current_dir:&str )
        {
            if let Ok( f ) = File::open( file_path ) {
                let file = BufReader::new( &f );
                let re_cmd = match Regex::new( r"^ *( [^ ]+ ):" ) {
                    Ok( x ) => x,
                    Err( e ) => {
                        println_stderr!( "pls:regex build error:{:?}", e );
                        return;
                    }
                };

                let re_include = match Regex::new( r"^ *include  *( [^ ]+ ) *$" ) {
                    Ok( x ) => x,
                    Err( e ) => {
                        println_stderr!( "pls:regex build error:{:?}", e );
                        return;
                    }
                };

                for line in file.lines().map_while( Result::ok ) {
                    if re_cmd.is_match( &line ) {
                        for cap in re_cmd.captures_iter( &line ) {
                            if !cap[1].starts_with( path ) {
                                continue;
                            }
                            ci.push( Completion {
                                completion:cap[1].to_string(),
                                display:None,
                                suffix:Suffix::Default,
                            } );
                        }
                    }
                    if re_include.is_match( &line ) {
                        for cap in re_include.captures_iter( &line ) {
                            let _file = &cap[1];
                            if _file.contains( '/' ) {
                                handle_file( ci, path, _file, current_dir );
                            } else {
                                let make_file = current_dir.to_owned() + "/" + _file;
                                handle_file( ci, path, &make_file, current_dir );
                            }
                        }
                    }
                }
            }
        }

        fn complete_make( path:&str ) -> Vec<Completion>
        {
            let mut res = Vec::new();
            let current_dir = match env::current_dir() {
                Ok( dir ) => match dir.to_str() {
                    Some( s ) => s.to_string(),
                    None => {
                        println!( "pls:to_str error" );
                        return res;
                    }
                },
                Err( e ) => {
                    println!( "pls:get current_dir error:{:?}", e );
                    return res;
                }
            };

            let make_file = format!( "{}/Makefile", current_dir );
            handle_file( &mut res, path, &make_file, &current_dir );
            res
        }
    }

    pub mod path
    {
        /*
        use linefeed::complete::{Completer, Completion, Suffix};
        use linefeed::terminal::Terminal;
        use linefeed::Prompter; */
        use ::
        {
            collections::{ HashSet },
            fs::{ read_dir },
            io::{ Write },
            iter::{ FromIterator },
            os::unix::fs::{ PermissionsExt },
            path::{ MAIN_SEPARATOR },
            sync::{ Arc },
            *,
        };

        pub struct BinCompleter
        {
            pub sh:Arc<shell::Shell>,
        }

        pub struct CdCompleter;
        pub struct PathCompleter;

        impl<Term:Terminal> Completer<Term> for BinCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap( self.sh.clone() );
                match sh {
                    Ok( x ) => Some( complete_bin( &x, word ) ),
                    Err( x ) => Some( complete_bin( &x, word ) ),
                }
            }
        }

        impl<Term:Terminal> Completer<Term> for PathCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                Some( complete_path( word, false ) )
            }
        }

        impl<Term:Terminal> Completer<Term> for CdCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                Some( complete_path( word, true ) )
            }
        }

        fn needs_expand_home( line:&str ) -> bool
        {
            regex::re_contains( line, r"( +~ + )|( +~/ )|( ^ *~/ )|( +~ *$ )" )
        }
        /// Returns a sorted list of paths whose prefix matches the given path.
        pub fn complete_path( word:&str, for_dir:bool ) -> Vec<Completion>
        {
            let is_env = is::env_prefix( word );
            let mut res = Vec::new();
            let linfo = parsers::line::parse_line( word );
            let tokens = linfo.tokens;
            let ( path, path_sep ) = if tokens.is_empty() {
                ( String::new(), String::new() )
            } else {
                let ( ref _path_sep, ref _path ) = tokens[tokens.len() - 1];
                ( _path.clone(), _path_sep.clone() )
            };

            let ( _, _dir_orig, _f ) = split_pathname( &path, "" );
            let dir_orig = if _dir_orig.is_empty() {
                String::new()
            } else {
                _dir_orig.clone()
            };
            let mut path_extended = path.clone();
            if needs_expand_home( &path_extended ) { expand::home_string( &mut path_extended ) }
            expand::environment_string( &mut path_extended );

            let ( _, _dir_lookup, file_name ) = split_pathname( &path_extended, "" );
            let dir_lookup = if _dir_lookup.is_empty() {
                ".".to_string()
            } else {
                _dir_lookup.clone()
            };
            // let dir_lookup = _dir_lookup.unwrap_or( "." );
            if let Ok( entries ) = read_dir( dir_lookup ) {
                for entry in entries.flatten() {
                    let pathbuf = entry.path();
                    let is_dir = pathbuf.is_dir();
                    if for_dir && !is_dir {
                        continue;
                    }

                    let entry_name = entry.file_name();
                    // TODO:Deal with non-UTF8 paths in some way
                    if let Ok( _path ) = entry_name.into_string() {
                        if _path.starts_with( &file_name ) {
                            let ( name, display ) = if !dir_orig.is_empty() {
                                ( 
                                    format!( "{}{}{}", dir_orig, MAIN_SEPARATOR, _path ),
                                    Some( _path ),
 )
                            } else {
                                ( _path, None )
                            };
                            let mut name = str::replace( name.as_str(), "//", "/" );
                            if path_sep.is_empty() && !is_env {
                                name = path::escape_path( &name );
                            }
                            let mut quoted = false;
                            if !path_sep.is_empty() {
                                name = str::wrap_sep_string( &path_sep, &name );
                                quoted = true;
                            }
                            let suffix = if is_dir {
                                if quoted {
                                    name.pop();
                                }
                                Suffix::Some( MAIN_SEPARATOR )
                            } else {
                                Suffix::Default
                            };
                            res.push( Completion {
                                completion:name,
                                display,
                                suffix,
                            } );
                        }
                    }
                }
            }
            res.sort_by( |a, b| a.completion.cmp( &b.completion ) );
            res
        }

        // Split optional directory and prefix. ( see its test cases for more details )
        fn split_pathname( path:&str, prefix:&str ) -> ( String, String, String )
        {
            if is::pipelined( path ) {
                let tokens:Vec<&str> = path.rsplitn( 2, '|' ).collect();
                let prefix = format!( "{}|", tokens[1] );
                return split_pathname( tokens[0], &prefix );
            }
            match path.rfind( '/' ) {
                Some( pos ) => ( 
                    prefix.to_string(),
                    path[..=pos].to_string(),
                    path[pos + 1..].to_string(),
 ),
                None => ( prefix.to_string(), String::new(), path.to_string() ),
            }
        }
        /// Returns a sorted list of paths whose prefix matches the given path.
        fn complete_bin( sh:&shell::Shell, path:&str ) -> Vec<Completion>
        {
            let mut res = Vec::new();
            let ( prefix, _, fname ) = split_pathname( path, "" );
            let env_path = match env::var( "PATH" ) {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "pls:env error when complete_bin:{:?}", e );
                    return res;
                }
            };

            let mut checker:HashSet<String> = HashSet::new();

            // handle alias, builtins, and functions
            for func in sh.funcs.keys() {
                if !func.starts_with( &fname ) {
                    continue;
                }
                if checker.contains( func ) {
                    continue;
                }
                checker.insert( func.clone() );
                res.push( Completion {
                    completion:func.to_owned(),
                    display:None,
                    suffix:Suffix::Default,
                } );
            }
            for alias in sh.alias.keys() {
                if !alias.starts_with( &fname ) {
                    continue;
                }
                if checker.contains( alias ) {
                    continue;
                }
                checker.insert( alias.clone() );
                res.push( Completion {
                    completion:alias.to_owned(),
                    display:None,
                    suffix:Suffix::Default,
                } );
            }

            let builtins = vec![
                "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
                "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
                "minfd", "set", "unset", "unpath",
            ];
            for item in &builtins {
                if !item.starts_with( &fname ) {
                    continue;
                }
                if checker.contains( *item ) {
                    continue;
                }
                checker.insert( item.to_string() );
                res.push( Completion {
                    completion:item.to_string(),
                    display:None,
                    suffix:Suffix::Default,
                } );
            }

            let vec_path:Vec<&str> = env_path.split( ':' ).collect();
            let path_list:HashSet<&str> = HashSet::from_iter( vec_path.iter().cloned() );

            for p in &path_list {
                if let Ok( list ) = read_dir( p ) {
                    for entry in list.flatten() {
                        if let Ok( name ) = entry.file_name().into_string() {
                            if name.starts_with( &fname ) {
                                let _mode = match entry.metadata() {
                                    Ok( x ) => x,
                                    Err( e ) => {
                                        println_stderr!( "pls:metadata error:{:?}", e );
                                        continue;
                                    }
                                };
                                let mode = _mode.permissions().mode();
                                if mode & 0o111 == 0 {
                                    // not binary
                                    continue;
                                }
                                if checker.contains( &name ) {
                                    continue;
                                }

                                let display = None;
                                let suffix = Suffix::Default;
                                checker.insert( name.clone() );
                                // TODO:need to handle quoted:`$ "foo#bar"`
                                let name_e = path::escape_path( &name );
                                let name_e = format!( "{}{}", prefix, name_e );
                                res.push( Completion {
                                    completion:name_e,
                                    display,
                                    suffix,
                                } );
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
        /*
        use linefeed::complete::{Completer, Completion, Suffix};
        use linefeed::terminal::Terminal;
        use linefeed::Prompter;
        */
        use ::
        {
            fs::{ File },
            io::{ BufRead, BufReader },
            regex::{ Regex },
            *,
        };

        pub struct SshCompleter;

        impl<Term:Terminal> Completer<Term> for SshCompleter
        {
            fn complete( 
                &self,
                word:&str,
                _reader:&Prompter<Term>,
                _start:usize,
                _end:usize,
 ) -> Option<Vec<Completion>> {
                Some( complete_ssh( word ) )
            }
        }

        fn complete_ssh( path:&str ) -> Vec<Completion>
        {
            let mut res = Vec::new();
            let home = get::user_home();
            let ssh_config = home + "/.ssh/config";
            if let Ok( f ) = File::open( &ssh_config ) {
                let file = BufReader::new( &f );
                let re = match Regex::new( r"^ *( ?i )host +( [^ ]+ )" ) {
                    Ok( x ) => x,
                    Err( e ) => {
                        println!( "Regex build error:{:?}", e );
                        return res;
                    }
                };
                for line in file.lines().map_while( Result::ok ) {
                    if !re.is_match( &line ) {
                        continue;
                    }
                    for cap in re.captures_iter( &line ) {
                        if !cap[1].starts_with( path ) {
                            continue;
                        }
                        res.push( Completion {
                            completion:cap[1].to_string(),
                            display:None,
                            suffix:Suffix::Default,
                        } );
                    }
                }
            }
            res
        }
    }

    pub struct CicadaCompleter
    {
        pub sh:Arc<shell::Shell>,
    }

    impl<Term:Terminal> Completer<Term> for CicadaCompleter
    {
        fn complete
        ( 
            &self,
            word:&str,
            reader:&Prompter<Term>,
            start:usize,
            _end:usize,
 ) -> Option<Vec<Completion>>
        {
            let line = reader.buffer();
            let completions:Option<Vec<Completion>>;
            if for_dots( line )
            {
                let cpl = Arc::new( dots::DotsCompleter );
                completions = cpl.complete( word, reader, start, _end );
            }
            else if for_ssh( line )
            {
                let cpl = Arc::new( ssh::SshCompleter );
                completions = cpl.complete( word, reader, start, _end );
            }

            else if for_make( line )
            {
                let cpl = Arc::new( make::MakeCompleter );
                completions = cpl.complete( word, reader, start, _end );
            }
            else if for_bin( line )
            {
                let cpl = Arc::new( path::BinCompleter {
                    sh:self.sh.clone(),
                } );
                completions = cpl.complete( word, reader, start, _end );
            }
            else if for_env( line )
            {
                let cpl = Arc::new( completers::environment::EnvCompleter {
                    sh:self.sh.clone(),
                } );
                completions = cpl.complete( word, reader, start, _end );
            }
            else if for_cd( line )
            {
                // `for_cd` should be put a bottom position, so that
                // `cd $SOME_ENV_<TAB>` works as expected.
                let cpl = Arc::new( path::CdCompleter );
                // completions for `cd` should not fail back to path-completion
                return cpl.complete( word, reader, start, _end );
            }
            else { completions = None; }

            if let Some( x ) = completions
            {
                if !x.is_empty()
                {
                    return Some( x );
                }
            }

            // empty completions should fail to path-completion, so that
            // `$ make generate /path/to/fi<Tab>` still works.
            let cpl = Arc::new( path::PathCompleter );
            cpl.complete( word, reader, start, _end )
        }

        fn word_start( &self, line:&str, end:usize, _reader:&Prompter<Term> ) -> usize
        { escaped_word_start( &line[..end] ) }
    }

    pub fn for_make( line:&str ) -> bool
    {
        regex::re_contains( line, r"^ *make " )
    }

    pub fn for_env( line:&str ) -> bool
    {
        regex::re_contains( line, r" *\$[_a-zA-Z0-9]*$" )
    }

    pub fn for_ssh( line:&str ) -> bool
    {
        regex::re_contains( line, r"^ *( ssh|scp ).* +[^ \./]+ *$" )
    }

    pub fn for_cd( line:&str ) -> bool
    {
        regex::re_contains( line, r"^ *cd +" )
    }

    pub fn for_bin( line:&str ) -> bool
    {
        let ptn = r"( ^ *( sudo|which|nohup )? *[a-zA-Z0-9_\.-]+$ )|( ^.+\| *( sudo|which|nohup )? *[a-zA-Z0-9_\.-]+$ )";
        regex::re_contains( line, ptn )
    }

    pub fn for_dots( line:&str ) -> bool
    {
        let args = parsers::line::line_to_plain_tokens( line );
        let len = args.len();
        if len == 0 { return false; }
        let dir = get::user_completer_directory();
        let dot_file = format!( "{}/{}.yaml", dir, args[0] );
        Path::new( dot_file.as_str() ).exists()
    }

    pub fn escaped_word_start( line:&str ) -> usize
    {
        let mut start_position:usize = 0;
        let mut found_bs = false;
        let mut found_space = false;
        let mut with_quote = false;
        let mut ch_quote = '\0';
        let mut extra_bytes = 0;
        for ( i, c ) in line.chars().enumerate()
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

            if !with_quote && !found_bs && ( c == '"' || c == '\'' )
            {
                with_quote = true;
                ch_quote = c;
            }

            else if with_quote && !found_bs && ch_quote == c
            {
                with_quote = false;
            }

            let bytes_c = c.len_utf8();
            if bytes_c > 1 { extra_bytes += bytes_c - 1; }
            found_bs = false;
        }
        
        if found_space { start_position = line.len(); }
        start_position
    }
}
pub mod convert { pub use std::convert::{ * }; }
pub mod default { pub use std::default::{ * }; }
pub mod env
{
    pub use std::env::{ * };
    use ::
    {
        *
    };
    /* init_path_env( ... ) */
    pub fn initialize_path_environment()
    {
        // order matters. took from `runc spec`
        let mut paths:Vec<String> = vec![];
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
            if path::Path::new( x ).exists() { paths.push( x.to_string() ); }
        }

        if let Ok( env_path ) = var( "PATH" )
        {
            for x in env_path.split( ":" )
            {
                if !paths.contains( &x.to_string() ) { paths.push( x.to_string() ); }
            }
        }

        let paths = paths.join( ":" );
        set_var( "PATH", paths );
    }
    /* env_args_to_command_line( ... ) -> String */
    pub fn arguments_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = args();
        if env_args.len() <= 1
        {
            return result;
        }
        for ( i, arg ) in env_args.enumerate()
        {
            if i == 0 || arg == "-c" { continue; }
            result.push_str( arg.as_str() );
        }
        result
    }
}
pub mod expand
{
    use ::
    {
        shell::{ brace_getitem, Shell },
        regex::{ Regex },
        *
    };
    /*
    pub fn expand_alias( ... )*/
    pub fn alias( sh:&Shell, tokens:&mut types::Tokens )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();
        let mut is_head = true;
        for ( sep, text ) in tokens.iter()
        {
            if sep.is_empty() && text == "|" {
                is_head = true;
                idx += 1;
                continue;
            }
            if is_head && text == "xargs" {
                idx += 1;
                continue;
            }

            if !is_head || !sh.is_alias( text ) {
                idx += 1;
                is_head = false;
                continue;
            }

            if let Some( value ) = sh.get_alias_content( text ) {
                buff.push( ( idx, value.clone() ) );
            }

            idx += 1;
            is_head = false;
        }

        for ( i, text ) in buff.iter().rev()
        {
            let linfo = parsers::line::parse_line( text );
            let tokens_ = linfo.tokens;
            tokens.remove( *i );
            for item in tokens_.iter().rev()
            {
                tokens.insert( *i, item.clone() );
            }
        }
    }
    /*
    pub fn expand_arguments( ... )*/
    pub fn arguments( line:&str, args:&[String] ) -> String
    {
        let linfo = parsers::line::parse_line( line );
        let mut tokens = linfo.tokens;
        arguments_in_tokens( &mut tokens, args );
        parsers::line::tokens_to_line( &tokens )
    }
    /*
    pub fn expand_args_for_single_token( ... )*/
    pub fn arguments_for_token( token:&str, args:&[String] ) -> String
    {
        let re = Regex::new( r"^( .*? )\$\{?( [0-9]+|@ )\}?( .* )$" ).unwrap();
        if !re.is_match( token ) { return token.to_string(); }

        let mut result = String::new();
        let mut _token = token.to_string();
        let mut _head = String::new();
        let mut _output = String::new();
        let mut _tail = String::new();
        loop
        {
            if !re.is_match( &_token )
            {
                if !_token.is_empty()
                {
                    result.push_str( &_token );
                }
                break;
            }

            for cap in re.captures_iter( &_token )
            {
                _head = cap[1].to_string();
                _tail = cap[3].to_string();
                let _key = cap[2].to_string();
                if _key == "@"
                { result.push_str( format!( "{}{}", _head, args[1..].join( " " ) ).as_str() ); } 
                
                else if let Ok( arg_idx ) = _key.parse::<usize>()
                {
                    if arg_idx < args.len()
                    {
                        result.push_str( format!( "{}{}", _head, args[arg_idx] ).as_str() );
                    } 
                    else
                    {
                        result.push_str( &_head );
                    }
                } 
                else
                { result.push_str( &_head ); }
            }

            if _tail.is_empty() {
                break;
            }
            _token = _tail.clone();
        }
        result
    }
    /*
    pub fn expand_args_in_tokens( ... )*/
    pub fn arguments_in_tokens( tokens:&mut types::Tokens, args:&[String] )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();

        for ( sep, token ) in tokens.iter()
        {
            if sep == "`" || sep == "'" || !is::arguments_in_token( token )
            {
                idx += 1;
                continue;
            }

            let _token = arguments_for_token( token, args );
            buff.push( ( idx, _token ) );
            idx += 1;
        }

        for ( i, text ) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }
    /*
    pub fn expand_brace( ... )*/
    pub fn brace( tokens:&mut types::Tokens )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();
        for ( sep, token ) in tokens.iter()
        {
            if !sep.is_empty() || !is::brace_expandable( token )
            {
                idx += 1;
                continue;
            }

            let mut result:Vec<String> = Vec::new();
            let items = brace_getitem( token, 0 );
            for x in items.0
            { result.push( x.clone() ); }
            buff.push( ( idx, result ) );
            idx += 1;
        }

        for ( i, items ) in buff.iter().rev()
        {
            tokens.remove( *i );
            for ( j, token ) in items.iter().enumerate()
            {
                let sep = if token.contains( ' ' ) { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }
    /*
    pub fn expand_brace_range( ... )*/
    pub fn brace_range( tokens:&mut types::Tokens )
    {
        let re;
        if let Ok( x ) = Regex::new( r#"\{( -?[0-9]+ )\.\.( -?[0-9]+ )( \.\. )?( [0-9]+ )?\}"# ) { re = x; }

        else
        {
            println_stderr!( "pls:re new error" );
            return;
        }

        let mut idx:usize = 0;
        let mut buff:Vec<( usize, Vec<String> )> = Vec::new();
        for ( sep, token ) in tokens.iter()
        {
            if !sep.is_empty() || !re.is_match( token )
            {
                idx += 1;
                continue;
            }

            // safe to unwrap here, since the `is_match` above already validated
            let caps = re.captures( token ).unwrap();
            let start = match caps[1].to_string().parse::<i32>()
            {
                Ok( x ) => x,
                Err( e ) =>
                {
                    println_stderr!( "pls:{}", e );
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>()
            {
                Ok( x ) => x,
                Err( e ) =>
                {
                    println_stderr!( "pls:{}", e );
                    return;
                }
            };

            // incr is always positive
            let mut incr = if caps.get( 4 ).is_none() { 1 }
            else
            {
                match caps[4].to_string().parse::<i32>()
                {
                    Ok( x ) => x,
                    Err( e ) =>
                    {
                        println_stderr!( "pls:{}", e );
                        return;
                    }
                }
            };

            if incr <= 1
            {
                incr = 1;
            }

            let mut result:Vec<String> = Vec::new();
            let mut n = start;
            if start > end
            {
                while n >= end
                {
                    result.push( format!( "{}", n ) );
                    n -= incr;
                }
            }

            else
            {
                while n <= end
                {
                    result.push( format!( "{}", n ) );
                    n += incr;
                }
            }

            buff.push( ( idx, result ) );
            idx += 1;
        }

        for ( i, items ) in buff.iter().rev()
        {
            tokens.remove( *i );
            for ( j, token ) in items.iter().enumerate()
            {
                let sep = if token.contains( ' ' ) { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }
    /*
    pub fn expand_env( ... )*/
    pub fn environment( sh:&Shell, tokens:&mut types::Tokens )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();

        for ( sep, token ) in tokens.iter()
        {
            if sep == "`" || sep == "'"
            {
                idx += 1;
                continue;
            }

            if !is::environment_in_token( token )
            {
                idx += 1;
                continue;
            }

            let mut _token = token.clone();
            while is::environment_in_token( &_token ) 
            { _token = single_environment( sh, &_token ); }
            buff.push( ( idx, _token ) );
            idx += 1;
        }

        for ( i, text ) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }
    /*
    pub fn expand_env_string( ... )*/
    pub fn environment_string( text:&mut String )
    {
        // expand "$HOME/.local/share" to "/home/tom/.local/share"
        if !text.starts_with( '$' ) { return; }

        let ptn = r"^\$( [A-Za-z_][A-Za-z0-9_]* )";
        let mut env_value = String::new();
        match regex::find_first_group( ptn, text )
        {
            Some( x ) =>
            {
                if let Ok( val ) = env::var( &x )
                { env_value = val; }
            }
            None => { return; }
        }

        if env_value.is_empty() { return; }
        let t = text.clone();
        *text = regex::replace_all( &t, ptn, &env_value );
    }
    /*
    pub fn expand_glob( ... )*/
    pub fn glob( tokens:&mut types::Tokens )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();
        for ( sep, text ) in tokens.iter()
        {
            if !sep.is_empty() || !is::globable( text )
            {
                idx += 1;
                continue;
            }

            let mut result:Vec<String> = Vec::new();
            let item = text.as_str();
            if !item.contains( '*' ) || item.trim().starts_with( '\'' ) || item.trim().starts_with( '"' )
            {
                result.push( item.to_string() );
            }

            else
            {
                let _basename = path::basename( item );
                let show_hidden = _basename.starts_with( ".*" );
                match glob::glob( item )
                {
                    Ok( paths ) =>
                    {
                        let mut is_empty = true;
                        for entry in paths
                        {
                            match entry
                            {
                                Ok( path ) =>
                                {
                                    let file_path = path.to_string_lossy();
                                    let _basename = path::basename( &file_path );
                                    if _basename == ".." || _basename == "." { continue; }
                                    if _basename.starts_with( '.' ) && !show_hidden { continue; }
                                    result.push( file_path.to_string() );
                                    is_empty = false;
                                }

                                Err( e ) => { /*log!( "glob error:{:?}", e );*/ }
                            }
                        }

                        if is_empty  { result.push( item.to_string() ); }
                    }

                    Err( e ) =>
                    {
                        println!( "glob error:{:?}", e );
                        result.push( item.to_string() );
                        return;
                    }
                }
            }

            buff.push( ( idx, result ) );
            idx += 1;
        }

        for ( i, result ) in buff.iter().rev()
        {
            tokens.remove( *i );
            for ( j, token ) in result.iter().enumerate()
            {
                let sep = if token.contains( ' ' ) { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }
    /*
    pub fn expand_arguments( ... )*/
    pub fn home( text:&str ) -> String
    {
        let mut s:String = text.to_string();
        let v = vec!
        [
            r"( ?P<head> + )~( ?P<tail> + )",
            r"( ?P<head> + )~( ?P<tail>/ )",
            r"^( ?P<head> * )~( ?P<tail>/ )",
            r"( ?P<head> + )~( ?P<tail> *$ )",
        ];

        for item in &v
        {
            let re;
            if let Ok( x ) = Regex::new( item ) { re = x; }
            else { return String::new(); }
            let home = get::user_home();
            let ss = s.clone();
            let to = format!( "$head{}$tail", home );
            let result = re.replace_all( ss.as_str(), to.as_str() );
            s = result.to_string();
        }
        s
    }
    /*
    pub fn expands_home( ... )*/
    pub fn house( tokens:&mut types::Tokens )
    {
        let mut idx:usize = 0;
        let mut buff = Vec::new();
        for ( sep, text ) in tokens.iter()
        {
            if !sep.is_empty() || !text.starts_with( "~" )
            {
                idx += 1;
                continue;
            }

            let mut s:String = text.clone();
            let ptn = r"^~( ?P<tail>.* )";
            let re = Regex::new( ptn ).expect( "invalid re ptn" );
            let home = get::user_home();
            let ss = s.clone();
            let to = format!( "{}$tail", home );
            let result = re.replace_all( ss.as_str(), to.as_str() );
            s = result.to_string();

            buff.push( ( idx, s.clone() ) );
            idx += 1;
        }

        for ( i, text ) in buff.iter().rev()
        {
            tokens[*i].1 = text.to_string();
        }
    }
    /*
    pub fn expand_home_string( ... )*/
    pub fn home_string( text:&mut String )
    {
        let v = vec!
        [
            r"( ?P<head> + )~( ?P<tail> + )",
            r"( ?P<head> + )~( ?P<tail>/ )",
            r"^( ?P<head> * )~( ?P<tail>/ )",
            r"( ?P<head> + )~( ?P<tail> *$ )",
        ];

        for item in &v
        {
            let re;
            if let Ok( x ) = Regex::new( item ) { re = x; }
            else { return; }

            let home = get::user_home();
            let ss = text.clone();
            let to = format!( "$head{}$tail", home );
            let result = re.replace_all( ss.as_str(), to.as_str() );
            *text = result.to_string();
        }
    }
    /*
    pub fn expand_line_to_toknes( ... )*/
    pub fn line_to_tokens( line:&str, args:&[String], sh:&mut shell::Shell ) -> types::Tokens
    {
        let linfo = parsers::line::parse_line( line );
        let mut tokens = linfo.tokens;
        arguments_in_tokens( &mut tokens, args );
        shell::do_expansion( sh, &mut tokens );
        tokens
    }
    /*
    pub fn expand_one_env( ... )*/
    pub fn single_environment( sh:&Shell, token:&str ) -> String
    {
        unsafe
        {
            // do not combine these two into one:`\{?..\}?`,
            // otherwize `}` in `{print $NF}` would gone.
            let re1 = Regex::new( r"^( .*? )\$( [A-Za-z0-9_]+|\$|\? )( .* )$" ).unwrap();
            let re2 = Regex::new( r"( .*? )\$\{( [A-Za-z0-9_]+|\$|\? )\}( .* )$" ).unwrap();
            if !re1.is_match( token ) && !re2.is_match( token ){ return token.to_string(); }

            let mut result = String::new();
            let match_re1 = re1.is_match( token );
            let match_re2 = re2.is_match( token );
            if !match_re1 && !match_re2 { return token.to_string(); }

            let cap_results = if match_re1 { re1.captures_iter( token ) } 
                            else { re2.captures_iter( token ) };

            for cap in cap_results
            {
                let head = cap[1].to_string();
                let tail = cap[3].to_string();
                let key = cap[2].to_string();
                if key == "?"
                { result.push_str( format!( "{}{}", head, sh.previous_status ).as_str() ); } 
                else if key == "$" 
                {
                    let val = libc::getpid();
                    result.push_str( format!( "{}{}", head, val ).as_str() );
                } 
                else if let Ok( val ) = env::var( &key )
                { result.push_str( format!( "{}{}", head, val ).as_str() ); }
                else if let Some( val ) = sh.get_env( &key )
                { result.push_str( format!( "{}{}", head, val ).as_str() ); } 
                else
                { result.push_str( &head ); }

                result.push_str( &tail );
            }

            result
        }
    }
}
pub mod error
{
    pub use std::error::{ * };
    /*
    errno v0.3.10 */
    pub mod no
    {
        //! Cross-platform interface to the `errno` variable.
        use ::
        {
            error::{ Error },
            *,
        };

        mod sys
        {
            //! Implementation of `errno` functionality for Unix systems.
            use ::
            {
                libc::{self, c_int, size_t, strerror_r, strlen},
                *,
            };

            use super::Errno;            
            
            pub const STRERROR_NAME:&str = "strerror_r";            

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
                    any( target_os = "solaris", target_os = "illumos" ),
                    link_name = "___errno"
 )]
                #[cfg_attr( target_os = "haiku", link_name = "_errnop" )]
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
                #[cfg_attr( target_os = "aix", link_name = "_Errno" )]
                #[cfg_attr( target_os = "nto", link_name = "__get_errno_ptr" )]
                fn errno_location() -> *mut c_int;
            }

            fn from_utf8_lossy( input:&[u8] ) -> &str
            {
                match str::from_utf8( input )
                {
                    Ok( valid ) => valid,
                    Err( error ) => unsafe { str::from_utf8_unchecked( &input[..error.valid_up_to()] ) },
                }
            }

            pub fn with_description<F, T>( err:Errno, callback:F ) -> T where
            F:FnOnce( Result<&str, Errno> ) -> T,
            {
                unsafe 
                {
                    let mut buf = [0u8; 1024];
                    let c_str = 
                    {
                        let rc = strerror_r( err.0, buf.as_mut_ptr() as *mut _, buf.len() as size_t );
                        if rc != 0
                        {
                            let fm_err = match rc < 0
                            {
                                true => errno(),
                                false => Errno( rc ),
                            };                            
                            if fm_err != Errno( libc::ERANGE ){ return callback( Err( fm_err ) ); }
                        }
                        let c_str_len = strlen( buf.as_ptr() as *const _ );
                        &buf[..c_str_len]
                    };
                    callback( Ok( from_utf8_lossy( c_str ) ) )
                }
            }
            
            pub fn errno() -> Errno
            { unsafe { Errno( *errno_location() ) } }

            pub fn set_errno( Errno( errno ):Errno )
            { unsafe { *errno_location() = errno; } }
        }
        /// Wraps a platform-specific error code.
        #[derive( Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash )]
        pub struct Errno( pub i32 );

        impl fmt::Debug for Errno
        {
            fn fmt( &self, fmt:&mut fmt::Formatter ) -> fmt::Result
            {
                sys::with_description( *self, |desc|
                {
                    fmt.debug_struct( "Errno" )
                    .field( "code", &self.0 )
                    .field( "description", &desc.ok() )
                    .finish()
                } )
            }
        }

        impl fmt::Display for Errno
        {
            fn fmt( &self, fmt:&mut fmt::Formatter ) -> fmt::Result
            {
                sys::with_description( *self, |desc| match desc
                {
                    Ok( desc ) => fmt.write_str( desc ),
                    Err( fm_err ) => write!
                    ( 
                        fmt,
                        "OS error {} ( {} returned error {} )",
                        self.0,
                        sys::STRERROR_NAME,
                        fm_err.0
 ),
                } )
            }
        }

        impl From<Errno> for i32
        {
            fn from( e:Errno ) -> Self { e.0 }
        }
        
        impl Error for Errno
        {
            #[allow( deprecated )] fn description( &self ) -> &str { "system error" }
        }
        
        impl From<Errno> for io::Error
        {
            fn from( errno:Errno ) -> Self
            { io::Error::from_raw_os_error( errno.0 ) }
        }
        /// Returns the platform-specific value of `errno`.
        pub fn errno() -> Errno
        { sys::errno() }
        /// Sets the platform-specific value of `errno`.
        pub fn set_errno( err:Errno )
        { sys::set_errno( err ) }
    }
}
pub mod f32 { pub use std::f32::{ * }; }
pub mod f64 { pub use std::f64::{ * }; }
pub mod ffi { pub use std::ffi::{ * }; }
pub mod fmt { pub use std::fmt::{ * }; }
pub mod fs
{
    pub use std::fs::{ * };
    /**/
    pub fn create_raw_fd_from_file( file_name:&str, append:bool ) -> Result<i32, String>
    {
        let mut oos = OpenOptions::new();
        if append { oos.append( true ); }
        else
        {
            oos.write( true );
            oos.truncate( true );
        }
        match oos.create( true ).open( file_name )
        {
            Ok( x ) =>
            {
                let fd = x.into_raw_fd();
                Ok( fd )
            }

            Err( e ) => Err( format!( "{}", e ) ),
        }
    }
    /**/
    pub fn close( fd:i32 )
    { unsafe { libc::close( fd ); } }
    /**/
    pub fn dup( fd:i32 ) -> i32
    { unsafe { libc::dup( fd ) } }
    /**/
    pub fn dup2( src:i32, dst:i32 )
    { unsafe { libc::dup2( src, dst ); } }
}
pub mod futures { pub use std::future::{ * }; }
/*
State Reading */
pub mod get
{
    use ::
    {
        error::{ no },
        libc::{ c_int, c_ulong, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO,  winsize },
        os::
        {
            fd::{ RawFd },
        },
        parsers::{ self },
        path::{ Path, PathBuf },
        types::{ CommandLine, Redirection },
        *
    };
    
    #[cfg( any( target_os = "linux", target_os = "android" ) )]
    static TIOCGWINSZ:c_ulong = 0x5413;

    #[cfg
    ( 
        any
        ( 
            target_os = "macos",
            target_os = "ios",
            target_os = "dragonfly",
            target_os = "freebsd",
            target_os = "netbsd",
            target_os = "openbsd"
 )
 )]
    static TIOCGWINSZ:c_ulong = 0x40087468;

    #[cfg( target_os = "solaris" )]
    static TIOCGWINSZ:c_ulong = 0x5468;

    extern "C"
    {
        fn gethostname( name:*mut libc::c_char, size:libc::size_t ) -> libc::c_int;
        fn ioctl( fd:c_int, request:c_ulong, ... ) -> libc::c_int;
    }
    /*
    get_user_home( ... ) -> String */
    pub fn user_home() -> String
    {
        match env::var( "HOME" )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println_stderr!( "pls:env HOME error:{}", e );
                String::new()
            }
        }
    }
    /*
    get_config_dir( ... ) -> String */
    pub fn configuration_directory() -> String
    {
        if let Ok( x ) = env::var( "XDG_CONFIG_HOME" ) { format!( "{}/pls", x ) }
        else
        {
            let home = user_home();
            format!( "{}/.config/pls", home )
        }
    }
    /*
    get_user_completer_dir( ... ) -> String */
    pub fn user_completer_directory() -> String
    {
        let dir_config = ::get::configuration_directory();
        format!( "{}/completers", dir_config )
    }
    /*
    get_fd_from_file( ... ) -> i32 */
    pub fn descriptor_from_file( file_name:&str ) -> i32
    {
        let path = path::Path::new( file_name );
        let display = path.display();
        let file = match fs::File::open( path )
        {
            Err( why ) =>
            {
                println_stderr!( "pls:{}:{}", display, why );
                return -1;
            }

            Ok( file ) => file,
        };

        file.into_raw_fd()
    }
    /*
    get_current_dir( ... ) -> String */
    pub fn current_directory() -> String
    {
        let mut current_dir = path::PathBuf::new();
        match env::current_dir()
        {
            Ok( x ) => current_dir = x,
            Err( e ) =>
            {
                println_stderr!( "env current_dir() failed:{}", e );
            }
        }

        let mut str_current_dir = "";
        match current_dir.to_str()
        {
            Some( x ) => str_current_dir = x,
            None => { println_stderr!( "current_dir to str failed." ); }
        }

        str_current_dir.to_string()
    }
    /*
    get_hostname( ... ) -> String
    https://gist.github.com/conradkleinespel/6c8174aee28fa22bfe26 */
    pub fn hostname() -> String
    {
        unsafe
        {
            let len = 255;
            let mut buf = Vec::<u8>::with_capacity( len );
            let ptr = buf.as_mut_slice().as_mut_ptr();
            let err = gethostname( ptr as *mut libc::c_char, len as libc::size_t ) as i32;

            match err
            {
                0 =>
                {
                    let real_len;
                    let mut i = 0;
                    loop
                    {
                        let byte = unsafe { *( ( ( ptr as u64 ) + ( i as u64 ) ) as *const u8 ) };
                        if byte == 0
                        {
                            real_len = i;
                            break;
                        }

                        i += 1;
                    }
                    buf.set_len( real_len );
                    String::from_utf8_lossy( buf.as_slice() ).into_owned()
                }
                _ => String::from( "unknown" ),
            }
        }
    }
    /*
    get_limit( ... ) -> ( String, String ) */
    pub fn limit( limit_name:&str, single_print:bool, for_hard:bool ) -> ( String, String )
    {
        unsafe
        {
            let ( desc, limit_id ) = match limit_name
            {
                "open_files" => ( "open files", libc::RLIMIT_NOFILE ),
                "core_file_size" => ( "core file size", libc::RLIMIT_CORE ),
                _ => return ( String::new(), String::from( "ulimit:error:invalid limit name" ) ),
            };

            let mut rlp = libc::rlimit { rlim_cur:0, rlim_max:0 };
            let mut result_stdout = String::new();
            let mut result_stderr = String::new();            
            if libc::getrlimit( limit_id, &mut rlp ) != 0
            {
                result_stderr.push_str( &format!( "error getting limit:{}", Error::last_os_error() ) );
                return ( result_stdout, result_stderr );
            }

            let to_print = if for_hard { rlp.rlim_max } else { rlp.rlim_cur };
            let info = if to_print == libc::RLIM_INFINITY 
            { if single_print { "unlimited\n".to_string() } else { format!( "{}\t\tunlimited\n", desc ) } }
            else if single_print
            { format!( "{}\n", to_print ) }
            else
            { format!( "{}\t\t{}\n", desc, to_print ) };

            result_stdout.push_str( &info );
            ( result_stdout, result_stderr )
        }

        
    }
    /*
    get_user_name( ... ) -> String */
    pub fn username() -> String
    {
        match env::var( "USER" )
        {
            Ok( x ) => { return x; }
            Err( e ) =>
            {
                //log!( "pls:env USER error:{}", e );
            }
        }
        let cmd_result = now::run( "whoami" );
        return cmd_result.stdout.trim().to_string();
    }
    /*
    get_envs_home( ... ) -> String */
    pub fn virtual_environments_home() -> String
    {
        env::var( "VIRTUALENV_HOME" ).unwrap_or_default()
    }
    /*
    get_all_venvs( ... ) -> Result<Vec<String>, String> */
    pub fn virtual_environments() -> Result<Vec<String>, String>
    {
        let home_envs = virtual_environments_home();
        if home_envs.is_empty()
        {
            let info = String::from( "you need to set VIRTUALENV_HOME to use vox" );
            return Err( info );
        }

        if !Path::new( home_envs.as_str() ).exists()
        {
            match fs::create_dir_all( home_envs.as_str() )
            {
                Ok( _ ) => {}
                Err( e ) =>
                {
                    let info = format!( "fs create_dir_all failed:{:?}", e );
                    return Err( info );
                }
            }
        }

        let mut venvs = Vec::new();
        let pdir = home_envs.clone();
        if let Ok( list ) = fs::read_dir( home_envs )
        {
            for ent in list.flatten()
            {
                let ent_name = ent.file_name();
                if let Ok( path ) = ent_name.into_string()
                {
                    let full_path = format!( "{}/{}/bin/activate", pdir, path );
                    if !Path::new( full_path.as_str() ).exists(){ continue; }
                    venvs.push( path );
                }
            }
        }

        Ok( venvs )
    }
    /*
    _get_std_fds( ... ) -> ( Option<RawFd>, Option<RawFd> ) */
    pub fn std_fds( redirects:&[Redirection] ) -> ( Option<RawFd>, Option<RawFd> )
    {
        if redirects.is_empty(){ return ( None, None ); }
        let mut fd_out = None;
        let mut fd_err = None;

        for i in 0..redirects.len()
        {
            let item = &redirects[i];
            if item.0 == "1"
            {
                let mut _fd_candidate = None;
                if item.2 == "&2"
                {
                    let ( _fd_out, _fd_err ) = std_fds( &redirects[i+1..] );
                    if let Some( fd ) = _fd_err {
                        _fd_candidate = Some( fd );
                    } else {
                        _fd_candidate = unsafe { Some( libc::dup( 2 ) ) };
                    }
                }

                else
                {  
                    let append = item.1 == ">>";
                    if let Ok( fd ) = fs::create_raw_fd_from_file( &item.2, append )
                    { _fd_candidate = Some( fd ); }
                }
                /*
                For command like this:`alias > a.txt > b.txt > c.txt`, 
                we need to return the last one, but close the previous two. */
                if let Some( fd ) = fd_out { unsafe { libc::close( fd ); } }
                fd_out = _fd_candidate;
            }

            if item.0 == "2" {
                let mut _fd_candidate = None;

                if item.2 == "&1" {
                    if let Some( fd ) = fd_out {
                        _fd_candidate = unsafe { Some( libc::dup( fd ) ) };
                    }
                } else {
                    let append = item.1 == ">>";
                    if let Ok( fd ) = fs::create_raw_fd_from_file( &item.2, append ) {
                        _fd_candidate = Some( fd );
                    }
                }

                if let Some( fd ) = fd_err {
                    unsafe { libc::close( fd ); }
                }

                fd_err = _fd_candidate;
            }
        }

        ( fd_out, fd_err )
    }
    /*
    _get_dupped_stdout_fd( ... ) -> Result<Vec<String>, String> */
    pub fn dupped_stdout_fd( cmd:&Command, cl:&CommandLine ) -> RawFd
    {
        /*
        If with pipeline, e.g. `history | grep foo`,
        Then we don't need to dup stdout since it is running in a sperated process. */
        unsafe
        {
            if cl.with_pipeline() { return 1; }
            let ( _fd_out, _fd_err ) = get::std_fds( &cmd.redirects_to );
            if let Some( fd ) = _fd_err { libc::close( fd ); }
            if let Some( fd ) = _fd_out { fd }
            else
            {
                let fd = libc::dup( 1 );
                if fd == -1
                {
                    let eno = no::errno();
                    println_stderr!( "pls:dup:{}", eno );
                }
                fd
            }
        }
    }
    /*
    _get_dupped_stderr_fd( ... ) -> Result<Vec<String>, String> */
    pub fn dupped_stderr_fd( cmd:&Command, cl:&CommandLine ) -> RawFd
    {
        unsafe
        {
            if cl.with_pipeline(){ return 2; }
            let ( _fd_out, _fd_err ) = get::std_fds( &cmd.redirects_to );
            if let Some( fd ) = _fd_out { libc::close( fd ); }
            if let Some( fd ) = _fd_err { fd } 
            else
            {
                let fd = libc::dup( 2 );
                if fd == -1
                {
                    let eno = no::errno();
                    println_stderr!( "pls:dup:{}", eno );
                }
                fd
            }
        }
    }
    /*
    get_osx_version( ... ) -> String */
    pub fn osx_version() -> String
    {
        let cr = now::run( "sw_vers -productVersion" );
        return cr.stdout.trim().to_string();
    }
    /*
    get_osx_codename( ... ) -> String */
    pub fn osx_codename() -> String
    {
        let cr = now::run( "grep -o 'SOFTWARE LICENSE AGREEMENT FOR .*[a-zA-Z]' '/System/Library/CoreServices/Setup Assistant.app/Contents/Resources/en.lproj/OSXSoftwareLicense.rtf' | sed 's/SOFTWARE LICENSE AGREEMENT FOR *//'" );
        return cr.stdout.trim().to_string();
    }
    /*
    get_macos_name( ... ) -> String */
    pub fn macos_name() -> String
    {
        let mut os_name = osx_codename();
        let ver = osx_version();
        if !ver.is_empty()
        {
            os_name.push( ' ' );
            os_name.push_str( &ver );
        }
        os_name
    }
    /*
    get_uname_mo( ... ) -> String */
    pub fn uname_mo() -> String
    {
        let cr = now::run( "uname -m -o" );
        return cr.stdout.trim().to_string();
    }
    /*
    get_uname( ... ) -> String */
    pub fn uname() -> String
    {
        let cr = now::run( "uname" );
        return cr.stdout.trim().to_string();
    }
    /*
    get_release_value( ... ) -> String */
    pub fn release_value( ptn:&str ) -> String
    {
        let line = format!( 
            "grep -i '{}' /etc/*release* 2>&1 | grep -o '=.*' | tr '\"=' ' '",
            ptn
 );
        let cr = now::run( &line );
        return cr.stdout.trim().to_string();
    }
    /*
    get_other_os_name( ... ) -> String */
    pub fn other_os_name() -> String
    {
        let mut name = release_value( "PRETTY_NAME" );
        if !name.is_empty() {
            return name;
        }
        name = release_value( "DISTRIB_DESCRIPTION" );
        if !name.is_empty() {
            return name;
        }
        name = release_value( "IMAGE_DESCRIPTION" );
        if !name.is_empty() {
            return name;
        }
        uname_mo()
    }
    /*
    get_os_name( ... ) -> String */
    pub fn os_name() -> String
    {
        let uname = uname();
        if uname.to_lowercase() == "darwin" {
           macos_name()
        } else {
            other_os_name()
        }
    }
    /*
    get_dimensions_any( ... ) -> winsize
    Runs the ioctl command. 
    Returns ( 0, 0 ) if all of the streams are not to a terminal, or there is an error.
    ( 0, 0 ) is an invalid size to have anyway, which is why it can be used as a nil value. */
    unsafe fn any_dimensions() -> winsize
    {
        let mut window:winsize = ::mem::zeroed();
        let mut result = ioctl( STDOUT_FILENO, TIOCGWINSZ, &mut window );
        if result == -1
        {
            window = ::mem::zeroed();
            result = ioctl( STDIN_FILENO, TIOCGWINSZ, &mut window );
            if result == -1
            {
                window = ::mem::zeroed();
                result = ioctl( STDERR_FILENO, TIOCGWINSZ, &mut window );
                if result == -1
                {
                    return ::mem::zeroed();
                }
            }
        }
        window
    }
    /*
    Query the current processes's output ( `stdout` ), input ( `stdin` ), and error ( `stderr` ) in that order,
    in the attempt to dtermine terminal width. */
    pub fn dimensions() -> Option<( usize, usize )>
    {
        let w = unsafe { any_dimensions() };
        if w.ws_col == 0 || w.ws_row == 0
        { None }
        else
        { Some( ( w.ws_col as usize, w.ws_row as usize ) ) }
    }
    /*
    get_prompt_len( ... ) -> i32 */
    pub fn prompt_len( prompt:&str ) -> i32
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
    /*
    get_prompt( ... ) -> String */
    pub fn prompt( sh:&shell::Shell ) -> String
    {
        let ps = prompt_string();
        let mut prompt = prompt::main::render_prompt( sh, &ps );
        if let Some( ( w, _h ) ) = dimensions()
        {
            if prompt_len( &prompt ) > ( w / 2 ) as i32 && !regex::re_contains( &ps, r#"( ?i )\$\{?newline.\}?"# )
            {
                prompt.push_str( "\n$ " );
            }
        } 
        else 
        {
            //log!( "ERROR:Failed to get term size" );
        }
        prompt
    }
    /*
    get_prompt_string( ... ) -> String */
    pub fn prompt_string() -> String
    {
        if let Ok( x ) = env::var( "PROMPT" ) { return x; }
        DEFAULT_PROMPT.to_string()
    }
    /*
    get_for_result_from_init( ... ) -> Vec<String> */
    pub fn for_result_from_init
    ( 
        sh:&mut shell::Shell,
        pair_init:Pair<parsers::locust::Rule>,
        args:&[String]
 ) -> Vec<String>
    {
        let mut result:Vec<String> = Vec::new();
        /*
        let pairs = pair_init.into_inner();
        for pair in pairs
        {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::TEST
            {
                let line = pair.as_str().trim();
                let tokens = expand_line_to_toknes( line, &args[1..], sh );
                for ( sep, token ) in tokens {
                    if sep.is_empty() {
                        for x in token.split_whitespace() {
                            result.push( x.to_string() );
                        }
                    } else {
                        result.push( token.clone() );
                    }
                }
            }
        }
        */
        result
    }
    /*
    get_for_result_list( ... ) -> Vec<String> */
    pub fn for_result_list
    ( 
        sh:&mut shell::Shell,
        pair_head:Pair<parsers::locust::Rule>,
        args:&[String]
 ) -> Vec<String>
    {
        /*
        let pairs = pair_head.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT {
                return get_for_result_from_init( sh, pair, args );
            }
        }
        */
        Vec::new()
    }
    /*
    get_for_var_name( ... ) -> String */
    pub fn for_var_name( pair_head:Pair<parsers::locust::Rule> ) -> String
    {
        /*
        let pairs = pair_head.into_inner();
        for pair in pairs
        {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT
            {
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
        */
        String::new()
    }
    /*
    get::history_file( ... ) -> String */
    pub fn history_file() -> String
    {
        if let Ok( hfile ) = env::var( "HISTORY_FILE" ){ hfile } 
        else if let Ok( d ) = env::var( "XDG_DATA_HOME" ){ format!( "{}/{}", d, "pls/history.sqlite" ) } 
        else
        {
            let home = get::user_home();
            format!( "{}/{}", home, ".local/share/pls/history.sqlite" )
        }
    }
    /*
    get_history_table( ... ) -> String */
    pub fn history_table() -> String
    {
        if let Ok( hfile ) = env::var( "HISTORY_TABLE" ){ hfile } 
        else { String::from( "cicada_history" ) }
    }
    /*
    get_job_line( ... ) -> String */
    pub fn job_line( job:&types::Job, trim:bool ) -> String
    {
        let mut cmd = job.cmd.clone();
        if trim && cmd.len() > 50
        {
            cmd.truncate( 50 );
            cmd.push_str( " ..." );
        }

        let _cmd = if job.is_bg && job.status == "Running"
        {
            format!( "{} &", cmd )
        }
        else { cmd };
        format!( "[{}] {}  {}   {}", job.id, job.gid, job.status, _cmd )
    }
}
/*
glob v0.0.0*/
pub mod glob
{

}
pub mod hint { pub use std::hint::{ * }; }
pub mod history
{
    /*
    use linefeed::terminal::DefaultTerminal;
    use linefeed::Interface;
    use rusqlite::Connection as Conn;
    use rusqlite::Error::SqliteFailure;
    */
    use ::
    {
        collections::{ HashMap },
        io::{ Write },
        path::{ Path },
        *,
    };

    pub fn init_db( hfile:&str, htable:&str )
    {
        let path = Path::new( hfile );
        if !path.exists() {
            let _parent = match path.parent() {
                Some( x ) => x,
                None => {
                    println_stderr!( "pls:history init - no parent found" );
                    return;
                }
            };
            let parent = match _parent.to_str() {
                Some( x ) => x,
                None => {
                    println_stderr!( "pls:parent to_str is None" );
                    return;
                }
            };
            match fs::create_dir_all( parent ) {
                Ok( _ ) => {}
                Err( e ) => {
                    println_stderr!( "pls:histdir create error:{}", e );
                    return;
                }
            }
            match fs::File::create( hfile ) {
                Ok( _ ) => {
                    println!( "pls:created history file:{}", hfile );
                }
                Err( e ) => {
                    println_stderr!( "pls:history:file create failed:{}", e );
                }
            }
        }

        let conn = match Conn::open( hfile ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:history:open db error:{}", e );
                return;
            }
        };
        let sql = format!( 
            "
            CREATE TABLE IF NOT EXISTS {}
                ( inp TEXT,
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
        match conn.execute( &sql, [] ) {
            Ok( _ ) => {}
            Err( e ) => println_stderr!( "pls:history:query error:{}", e ),
        }
    }

    pub fn init( rl:&mut Interface<DefaultTerminal> )
    {
        let mut hist_size:usize = 99999;
        if let Ok( x ) = env::var( "HISTORY_SIZE" ) {
            if let Ok( y ) = x.parse::<usize>() {
                hist_size = y;
            }
        }
        rl.set_history_size( hist_size );

        let history_table = get::history_table();
        let hfile = get::history_file();

        if !Path::new( &hfile ).exists() {
            init_db( &hfile, &history_table );
        }

        let mut delete_dups = true;
        if let Ok( x ) = env::var( "HISTORY_DELETE_DUPS" ) {
            if x == "0" {
                delete_dups = false;
            }
        }
        if delete_dups {
            delete_duplicated_histories();
        }

        let conn = match Conn::open( &hfile ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:history:conn error:{}", e );
                return;
            }
        };
        let sql = format!( "SELECT inp FROM {} ORDER BY tsb;", history_table );
        let mut stmt = match conn.prepare( &sql ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:prepare select error:{}", e );
                return;
            }
        };

        let rows = match stmt.query_map( [], |row| row.get( 0 ) ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:query select error:{}", e );
                return;
            }
        };

        let mut dict_helper:HashMap<String, bool> = HashMap::new();
        for x in rows.flatten() {
            let inp:String = x;
            if dict_helper.contains_key( &inp ) {
                continue;
            }
            dict_helper.insert( inp.clone(), true );
            rl.add_history( inp.trim().to_string() );
        }
    }

    pub fn delete_duplicated_histories()
    {
        let hfile = get::history_file();
        let history_table = get::history_table();
        let conn = match Conn::open( &hfile ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:history:conn error:{}", e );
                return;
            }
        };
        let sql = format!( 
            "DELETE FROM {} WHERE rowid NOT IN ( 
            SELECT MAX( rowid ) FROM {} GROUP BY inp )",
            history_table, history_table
 );
        match conn.execute( &sql, [] ) {
            Ok( _ ) => {}
            Err( e ) => match e {
                SqliteFailure( ee, msg ) => {
                    if ee.extended_code == 5 {
                        /*log!( 
                            "failed to delete dup histories:{}",
                            msg.unwrap_or( "db is locked?".to_owned() ),
 );*/
                        return;
                    }
                    println_stderr!( 
                        "pls:history:delete dups error:{}:{:?}",
                        &ee,
                        &msg
 );
                }
                _ => {
                    println_stderr!( "pls:history:delete dup error:{}", e );
                }
            },
        }
    }

    pub fn add_raw( sh:&shell::Shell, line:&str, status:i32, tsb:f64, tse:f64 )
    {
        let hfile = get::history_file();
        let history_table = get::history_table();
        if !Path::new( &hfile ).exists() {
            init_db( &hfile, &history_table );
        }

        let conn = match Conn::open( &hfile ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:history:conn error:{}", e );
                return;
            }
        };
        let sql = format!( 
            "INSERT INTO \
             {} ( inp, rtn, tsb, tse, sessionid, info ) \
             VALUES( '{}', {}, {}, {}, '{}', 'dir:{}|' );",
            history_table,
            str::replace( line.trim(), "'", "''" ),
            status,
            tsb,
            tse,
            sh.session_id,
            sh.current_dir,
 );
        match conn.execute( &sql, [] ) {
            Ok( _ ) => {}
            Err( e ) => println_stderr!( "pls:history:save error:{}", e ),
        }
    }

    pub fn add( sh:&shell::Shell, rl:&mut Interface<DefaultTerminal>, line:&str, status:i32, tsb:f64, tse:f64 )
    {
        add_raw( sh, line, status, tsb, tse );
        rl.add_history( line.to_string() );
    }
}
pub mod io { pub use std::io::{ * }; }
pub mod hash { pub use std::hash::{ * }; }
/*
State Testing */
pub mod is
{
    use ::
    {
        char::{ char_width },
        regex::{ re_contains, Regex },
        *,
    };
    /*
    is_arithmetic( ... ) -> bool*/
    pub fn arithmetic( line:&str ) -> bool
    {
        if !re_contains( line, r"[0-9]+" )
        { return false; }

        if !re_contains( line, r"\+|\-|\*|/|\^" )
        { return false; }

        re_contains( line, r"^[ 0-9\.\( \ )\+\-\*/\^]+[\.0-9 \ )]$" )
    }
    /*
    is_builtin( ... ) -> bool*/
    pub fn builtin( s:&str ) -> bool
    {
        let builtins =
        [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
            "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
            "minfd", "set", "unset", "unpath",
        ];

        builtins.contains( &s )
    }
    /*
    is_env( ... ) -> bool*/
    pub fn environment( line:&str ) -> bool
    { re_contains( line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$" ) }
    /*
    is_shell_altering_command( ... ) -> bool*/
    pub fn shell_altering_command( line:&str ) -> bool
    {
        let line = line.trim();
        if re_contains( line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$" )
        { return true; }

        line.starts_with( "alias " )
        || line.starts_with( "export " )
        || line.starts_with( "unalias " )
        || line.starts_with( "unset " )
        || line.starts_with( "source " )
    }
    /*
    is_signal_handler_enabled( ... ) -> bool*/
    pub fn signal_handler_enabled() -> bool
    {
        env::var( "CICADA_ENABLE_SIG_HANDLER" ).map_or( false, |x| x == "1" )
    }
    /*
    is_login( ... ) -> bool */
    pub fn login( args:&[String] ) -> bool
    {
        if !args.is_empty() && args[0].starts_with( "-" ) {
            return true;
        }

        if args.len() > 1 && ( args[1] == "--login" || args[1] == "-l" ) {
            return true;
        }

        false
    }
    /*
    is_script( ... ) -> bool */
    pub fn script( args:&[String] ) -> bool
    { args.len() > 1 && !args[1].starts_with( "-" ) }
    /*
    is_command_string( ... ) -> bool */
    pub fn command_string( args:&[String] ) -> bool
    { args.len() > 1 && args[1] == "-c" }
    /*
    is_non_tty( ... ) -> bool */
    pub fn non_tty() -> bool
    { unsafe { libc::isatty( 0 ) == 0 } }
    /*
    is::prefix_char( ... ) -> bool */
    pub fn prefix_char( c:char ) -> bool
    {
        c == '[' || c == '{'
    }
    /*
    is::suffix_char( ... ) -> bool */
    pub fn suffix_char( c:char ) -> bool
    {
        c == ']' || c == '}'
    }
    /*
    is::prompt_item_char( ... ) -> bool */
    pub fn prompt_item_char( c:char, token:&str ) -> bool
    {
        let s = c.to_string();
        if token.is_empty() { regex::re_contains( &s, r#"^[a-zA-Z_]$"# ) }
        else { regex::re_contains( &s, r#"^[a-zA-Z0-9_]$"# ) }
    }
    /*
    is_args_in_token( ... ) -> bool */
    pub fn arguments_in_token( token:&str ) -> bool
    {
        ::regex::re_contains( token, r"\$\{?[0-9@]+\}?" )
    }
    /*
    needs_globbing( ... ) -> bool */
    pub fn globable ( line:&str ) -> bool
    {
        let re = Regex::new( r"\*+" ).expect( "Invalid regex ptn" );
        re.is_match( line )
    }
    /*
    need_expand_brace( ... ) -> bool */
    pub fn brace_expandable( line:&str ) -> bool
    {
        regex::re_contains( line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"# )
    }
    /*
    env_in_token( ... ) -> bool */
    pub fn environment_in_token( token:&str ) -> bool
    {
        if regex::re_contains( token, r"\$\{?[\$\?]\}?" ){ return true; }
        let ptn_env_name = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let ptn_env = format!( r"\$\{{?{}\}}?", ptn_env_name );
        if !regex::re_contains( token, &ptn_env ) { return false; }
        
        let ptn_cmd_sub1 = format!( r"^{}=`.*`$", ptn_env_name );
        let ptn_cmd_sub2 = format!( r"^{}=\$\( .*\ )$", ptn_env_name );
        if regex::re_contains( token, &ptn_cmd_sub1 )
        || regex::re_contains( token, &ptn_cmd_sub2 )
        || regex::re_contains( token, r"^\$\( .+\ )$" )
        {
            return false;
        }
        
        let ptn_env = format!( r"='.*\$\{{?{}\}}?.*'$", ptn_env_name );
        !regex::re_contains( token, &ptn_env )
    }
    /*
    is_env_prefix( ... ) -> bool */
    pub fn env_prefix( line:&str ) -> bool
    { ::regex::re_contains( line, r" *\$[a-zA-Z_][A-Za-z0-9_]*" ) }
    /*
    is_pipelined( ... ) -> bool */
    pub fn pipelined( path:&str ) -> bool
    {
        if !path.contains( '|' ) { return false; }
        !path.starts_with( '"' ) && !path.starts_with( '\'' )
    }
    /*
    is_visible( ... ) -> bool */
    pub fn visible( ch:char ) -> bool
    {
        match ch
        {
            '\t' | '\r' | '\n' => true,
            _ => char_width( ch ).unwrap_or( 0 ) != 0
        }
    }
    /*
    is_combining_mark( ... ) -> bool */
    #[inline] pub fn combining_mark( ch:char ) -> bool
    { unicode_normalization::char::is_combining_mark( ch ) }



            fn is_flag( i:u8 ) -> bool
            {
                i == b' ' || i == b'-' || i == b'+' || i == b'#'
            }
}
pub mod iter { pub use std::iter::{ * }; }
pub mod marker { pub use std::marker::{ * }; }
pub mod mem { pub use std::mem::{ * }; }
/*
mortal v0.0.0*/
pub mod mortal
{

}
pub mod net { pub use std::net::{ * }; }
/*
nom v7.1.3*/
pub mod nom
{
    //! # nom, eating data byte by byte
    pub use self::bits::*;
    pub use self::internal::*;
    pub use self::traits::*;
    pub use self::str::*;
    /**/
    #[macro_use] mod macros
    {
        macro_rules! succ
        ( 
            ( 0, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 1, $( $rest )* ) );
            ( 1, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 2, $( $rest )* ) );
            ( 2, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 3, $( $rest )* ) );
            ( 3, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 4, $( $rest )* ) );
            ( 4, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 5, $( $rest )* ) );
            ( 5, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 6, $( $rest )* ) );
            ( 6, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 7, $( $rest )* ) );
            ( 7, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 8, $( $rest )* ) );
            ( 8, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 9, $( $rest )* ) );
            ( 9, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 10, $( $rest )* ) );
            ( 10, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 11, $( $rest )* ) );
            ( 11, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 12, $( $rest )* ) );
            ( 12, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 13, $( $rest )* ) );
            ( 13, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 14, $( $rest )* ) );
            ( 14, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 15, $( $rest )* ) );
            ( 15, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 16, $( $rest )* ) );
            ( 16, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 17, $( $rest )* ) );
            ( 17, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 18, $( $rest )* ) );
            ( 18, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 19, $( $rest )* ) );
            ( 19, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 20, $( $rest )* ) );
            ( 20, $submac:ident ! ( $( $rest:tt )* ) ) => ( $submac!( 21, $( $rest )* ) );
        );
    }
    /**/
    #[macro_use] pub mod error
    {
        //! Error management
        use super::internal::{Err, IResult, Parser};
        use ::
        {
            error::{ Error as StdError },
            fmt::{ self, Debug, Display, Write },
            nom::{ HexDisplay, traits::{ Offset } },
            ops::{ Deref, DerefMut },
            string::{ String },
            *,
        };
        /// Creates a parse error from a `nom::ErrorKind` and the position in the input
        #[macro_export( local_inner_macros )] macro_rules! error_position
        (( $input:expr, $code:expr ) => ({ $crate::nom::error::make_error( $input, $code ) }););
        /// Creates a parse error from a `nom::ErrorKind`, the position in the input and the next error in the parsing tree
        #[macro_export( local_inner_macros )] macro_rules! error_node_position
        (( $input:expr, $code:expr, $next:expr ) => ({ $crate::nom::error::append_error( $input, $code, $next ) } ););
        /// This trait must be implemented by the error type of a nom parser.
        pub trait ParseError<I>:Sized
        {
            /// Creates an error from the input position and an [ErrorKind]
            fn from_error_kind( input:I, kind:ErrorKind ) -> Self;
            /// Combines an existing error with a new one created from the input position and an [ErrorKind].
            fn append( input:I, kind:ErrorKind, other:Self ) -> Self;
            /// Creates an error from an input position and an expected character
            fn from_char( input:I, _:char ) -> Self { Self::from_error_kind( input, ErrorKind::Char ) }
            /// Combining two existing errors, this is used to compare errors generated in various branches of `alt`.
            fn or( self, other:Self ) -> Self { other }
        }
        /// This trait is required by the `context` combinator to add a static string to an existing error
        pub trait ContextError<I>:Sized
        {
            /// Creates a new error from an input position, a static string and an existing error.
            fn add_context( _input:I, _ctx:&'static str, other:Self ) -> Self { other }
        }
        /// Required by the `map_res` combinator to integrate error types from external functions.
        pub trait FromExternalError<I, E>
        {
            /// Creates a new error from an input position, an [ErrorKind] indicating the
            /// wrapping parser, and an external error
            fn from_external_error( input:I, kind:ErrorKind, e:E ) -> Self;
        }
        /// default error type, only contains the error' location and code
        #[derive( Debug, PartialEq )]
        pub struct Error<I>
        {
            /// position of the error in the input data
            pub input:I,
            /// nom error code
            pub code:ErrorKind,
        }

        impl<I> Error<I>
        {
            /// creates a new basic error
            pub fn new( input:I, code:ErrorKind ) -> Error<I> { Error { input, code } }
        }

        impl<I> ParseError<I> for Error<I>
        {
            fn from_error_kind( input:I, kind:ErrorKind ) -> Self { Error { input, code:kind } }
            fn append( _:I, _:ErrorKind, other:Self ) -> Self { other }
        }

        impl<I> ContextError<I> for Error<I> {}

        impl<I, E> FromExternalError<I, E> for Error<I>
        {
            /// Create a new error from an input position and an external error
            fn from_external_error( input:I, kind:ErrorKind, _e:E ) -> Self { Error { input, code:kind } }
        }
        /// The Display implementation allows the std::error::Error implementation
        impl<I:Debug + Display> StdError for Error<I> {}
        impl<I:Display> Display for Error<I>
        {
            fn fmt( &self, f:&mut fmt::Formatter<'_> ) -> fmt::Result { write!(f, "error {:?} @:{}", self.code, self.input) }
        }
        /// Indicates which parser returned an error
        #[derive( Clone, Copy, Debug, Eq, Hash, PartialEq )]
        pub enum ErrorKind
        {
            Tag,
            MapRes,
            MapOpt,
            Alt,
            IsNot,
            IsA,
            SeparatedList,
            SeparatedNonEmptyList,
            Many0,
            Many1,
            ManyTill,
            Count,
            TakeUntil,
            LengthValue,
            TagClosure,
            Alpha,
            Digit,
            HexDigit,
            OctDigit,
            AlphaNumeric,
            Space,
            MultiSpace,
            LengthValueFn,
            Eof,
            Switch,
            TagBits,
            OneOf,
            NoneOf,
            Char,
            CrLf,
            RegexpMatch,
            RegexpMatches,
            RegexpFind,
            RegexpCapture,
            RegexpCaptures,
            TakeWhile1,
            Complete,
            Fix,
            Escaped,
            EscapedTransform,
            NonEmpty,
            ManyMN,
            Not,
            Permutation,
            Verify,
            TakeTill1,
            TakeWhileMN,
            TooLarge,
            Many0Count,
            Many1Count,
            Float,
            Satisfy,
            Fail,
        }

        impl ErrorKind
        {
            /// Converts an ErrorKind to a text description
            pub fn description( &self ) -> &str 
            {
                match *self
                {
                    ErrorKind::Tag                   => "Tag",
                    ErrorKind::MapRes                => "Map on Result",
                    ErrorKind::MapOpt                => "Map on Option",
                    ErrorKind::Alt                   => "Alternative",
                    ErrorKind::IsNot                 => "IsNot",
                    ErrorKind::IsA                   => "IsA",
                    ErrorKind::SeparatedList         => "Separated list",
                    ErrorKind::SeparatedNonEmptyList => "Separated non empty list",
                    ErrorKind::Many0                 => "Many0",
                    ErrorKind::Many1                 => "Many1",
                    ErrorKind::Count                 => "Count",
                    ErrorKind::TakeUntil             => "Take until",
                    ErrorKind::LengthValue           => "Length followed by value",
                    ErrorKind::TagClosure            => "Tag closure",
                    ErrorKind::Alpha                 => "Alphabetic",
                    ErrorKind::Digit                 => "Digit",
                    ErrorKind::AlphaNumeric          => "AlphaNumeric",
                    ErrorKind::Space                 => "Space",
                    ErrorKind::MultiSpace            => "Multiple spaces",
                    ErrorKind::LengthValueFn         => "LengthValueFn",
                    ErrorKind::Eof                   => "End of file",
                    ErrorKind::Switch                => "Switch",
                    ErrorKind::TagBits               => "Tag on bitstream",
                    ErrorKind::OneOf                 => "OneOf",
                    ErrorKind::NoneOf                => "NoneOf",
                    ErrorKind::Char                  => "Char",
                    ErrorKind::CrLf                  => "CrLf",
                    ErrorKind::RegexpMatch           => "RegexpMatch",
                    ErrorKind::RegexpMatches         => "RegexpMatches",
                    ErrorKind::RegexpFind            => "RegexpFind",
                    ErrorKind::RegexpCapture         => "RegexpCapture",
                    ErrorKind::RegexpCaptures        => "RegexpCaptures",
                    ErrorKind::TakeWhile1            => "TakeWhile1",
                    ErrorKind::Complete              => "Complete",
                    ErrorKind::Fix                   => "Fix",
                    ErrorKind::Escaped               => "Escaped",
                    ErrorKind::EscapedTransform      => "EscapedTransform",
                    ErrorKind::NonEmpty              => "NonEmpty",
                    ErrorKind::ManyMN                => "Many( m, n )",
                    ErrorKind::HexDigit              => "Hexadecimal Digit",
                    ErrorKind::OctDigit              => "Octal digit",
                    ErrorKind::Not                   => "Negation",
                    ErrorKind::Permutation           => "Permutation",
                    ErrorKind::ManyTill              => "ManyTill",
                    ErrorKind::Verify                => "predicate verification",
                    ErrorKind::TakeTill1             => "TakeTill1",
                    ErrorKind::TakeWhileMN           => "TakeWhileMN",
                    ErrorKind::TooLarge              => "Needed data size is too large",
                    ErrorKind::Many0Count            => "Count occurrence of >=0 patterns",
                    ErrorKind::Many1Count            => "Count occurrence of >=1 patterns",
                    ErrorKind::Float                 => "Float",
                    ErrorKind::Satisfy               => "Satisfy",
                    ErrorKind::Fail                  => "Fail",
                }
            }
        }
        
        impl<I> ParseError<I> for ( I, ErrorKind )
        {
            fn from_error_kind( input:I, kind:ErrorKind ) -> Self { ( input, kind ) }
            fn append( _:I, _:ErrorKind, other:Self ) -> Self { other }
        }

        impl<I> ContextError<I> for ( I, ErrorKind ) {}
        impl<I, E> FromExternalError<I, E> for ( I, ErrorKind )
        {
            fn from_external_error( input:I, kind:ErrorKind, _e:E ) -> Self { ( input, kind ) }
        }

        impl<I> ParseError<I> for ()
        {
            fn from_error_kind( _:I, _:ErrorKind ) -> Self {}
            fn append( _:I, _:ErrorKind, _:Self ) -> Self {}
        }

        impl<I> ContextError<I> for () {}

        impl<I, E> FromExternalError<I, E> for ()
        {
            fn from_external_error( _input:I, _kind:ErrorKind, _e:E ) -> Self {}
        }
        /// Creates an error from the input position and an [ErrorKind]
        pub fn make_error<I, E:ParseError<I>>( input:I, kind:ErrorKind ) -> E
        {
            E::from_error_kind( input, kind )
        }
        /// Combines an existing error with a new one created from the input position and an [ErrorKind].
        pub fn append_error<I, E:ParseError<I>>( input:I, kind:ErrorKind, other:E ) -> E { E::append( input, kind, other ) }
        /// This error type accumulates errors and their position when backtracking through a parse tree.
        #[derive( Clone, Debug, PartialEq )]
        pub struct VerboseError<I>
        {
            /// List of errors accumulated by `VerboseError`, containing the affected part of input data, and some context
            pub errors:Vec<( I, VerboseErrorKind )>
        }
        /// Error context for `VerboseError`
        #[derive( Clone, Debug, PartialEq )]
        pub enum VerboseErrorKind
        {
            /// Static string added by the `context` function
            Context( &'static str ),
            /// Indicates which character was expected by the `char` function
            Char( char ),
            /// Error kind given by various nom parsers
            Nom( ErrorKind ),
        }
        
        impl<I> ParseError<I> for VerboseError<I>
        {
            fn from_error_kind( input:I, kind:ErrorKind ) -> Self
            {
                VerboseError
                {
                    errors:vec![( input, VerboseErrorKind::Nom( kind ) )],
                }
            }

            fn append( input:I, kind:ErrorKind, mut other:Self ) -> Self
            {
                other.errors.push( ( input, VerboseErrorKind::Nom( kind ) ) );
                other
            }

            fn from_char( i:I, c:char ) -> Self { VerboseError { errors:vec![( i, VerboseErrorKind::Char( c ) )], } }
        }
        
        impl<I> ContextError<I> for VerboseError<I>
        {
            fn add_context( input:I, ctx:&'static str, mut other:Self ) -> Self
            {
                other.errors.push( ( input, VerboseErrorKind::Context( ctx ) ) );
                other
            }
        }
        
        impl<I, E> FromExternalError<I, E> for VerboseError<I>
        {
            /// Create a new error from an input position and an external error
            fn from_external_error( input:I, kind:ErrorKind, _e:E ) -> Self { Self::from_error_kind( input, kind ) }
        }
        
        impl<I:Display> Display for VerboseError<I>
        {
            fn fmt( &self, f:&mut fmt::Formatter<'_> ) -> fmt::Result
            {
                writeln!( f, "Parse error:" )?;
                for ( input, error ) in &self.errors
                {
                    match error
                    {
                        VerboseErrorKind::Nom( e ) => writeln!( f, "{:?} at:{}", e, input )?,
                        VerboseErrorKind::Char( c ) => writeln!( f, "expected '{}' at:{}", c, input )?,
                        VerboseErrorKind::Context( s ) => writeln!( f, "in section '{}', at:{}", s, input )?,
                    }
                }
                Ok( () )
            }
        }
        
        impl<I:Debug + Display> StdError for VerboseError<I> {}
        /// Create a new error from an input position, a static string and an existing error.
        pub fn context<I:Clone, E:ContextError<I>, F, O> ( ctx:&'static str, mut f:F ) -> impl FnMut( I ) -> IResult<I, O, E>
        where F:Parser<I, O, E>,
        {
            move |i:I| match f.parse( i.clone() )
            {
                Ok( o ) => Ok( o ),
                Err( Err::Incomplete( i ) ) => Err( Err::Incomplete( i ) ),
                Err( Err::Error( e ) ) => Err( Err::Error( E::add_context( i, ctx, e ) ) ),
                Err( Err::Failure( e ) ) => Err( Err::Failure( E::add_context( i, ctx, e ) ) ),
            }
        }
        /// Transforms a `VerboseError` into a trace with input position information
        pub fn convert_error<I:Deref<Target = str>>( input:I, e:VerboseError<I>, ) -> String
        {
            let mut result = String::new();
            for ( i, ( substring, kind ) ) in e.errors.iter().enumerate()
            {
                let offset = input.offset( substring );
                if input.is_empty()
                {
                    match kind
                    {
                        VerboseErrorKind::Char( c ) => { write!( &mut result, "{}:expected '{}', got empty input\n\n", i, c ) }
                        VerboseErrorKind::Context( s ) => write!( &mut result, "{}:in {}, got empty input\n\n", i, s ),
                        VerboseErrorKind::Nom( e ) => write!( &mut result, "{}:in {:?}, got empty input\n\n", i, e ),
                    }
                }
                else
                {
                    let p = &input.as_bytes()[..offset];

                    // Count the number of newlines in the first `offset` bytes of input
                    let line_no = p.iter().filter( |&&b| b == b'\n' ).count() + 1;

                    // Find the line that includes the subslice:
                    // Find the *last* newline before the substring starts
                    let line_begin = p.iter().rev().position( |&b| b == b'\n' ).map( |pos| offset - pos ).unwrap_or( 0 );

                    // Find the full line after that newline
                    let line = input[line_begin..].lines().next().unwrap_or( &input[line_begin..] ).trim_end();

                    // The ( 1-indexed ) column number is the offset of our substring into that line
                    let column_number = line.offset( substring ) + 1;

                    match kind
                    {
                        VerboseErrorKind::Char( c ) =>
                        {
                            if let Some( found ) = substring.chars().next()
                            {
                                write!
                                (
                                    &mut result,
                                    "{i}:@line {line_no}:\n{line}\n{caret:>column$}\nneeds '{needs}', found {found}\n\n",
                                    i = i,
                                    line_no = line_no,
                                    line = line,
                                    caret = '^',
                                    column = column_number,
                                    needs = c,
                                    found = found,
                                )
                            } 
                            else
                            {
                                write!
                                ( 
                                    &mut result,
                                    "{i}:@line {line_no}:\n{line}\n{caret:>column$}\nneeds '{needs}', got end of input\n\n",
                                    i = i,
                                    line_no = line_no,
                                    line = line,
                                    caret = '^',
                                    column = column_number,
                                    needs = c,
                                )
                            }
                        }

                        VerboseErrorKind::Context( s ) =>
                        {
                            write!
                            (
                                &mut result,
                                "{i}:@line {line_no}, in {ctx}:\n{line}\n{caret:>column$}\n\n",
                                i = i,
                                line_no = line_no,
                                ctx = s,
                                line = line,
                                caret = '^',
                                column = column_number,
                            )
                        }
                        
                        VerboseErrorKind::Nom( e ) =>
                        {
                            write!
                            (
                                &mut result,
                                "{i}:@line {line_no}, in {nom_err:?}:\n{line}\n{caret:>column$}\n\n",
                                i = i,
                                line_no = line_no,
                                nom_err = e,
                                line = line,
                                caret = '^',
                                column = column_number,
                            )
                        }
                    }
                }
                .unwrap(); // Because `write!` to a `String` is infallible, this `unwrap` is fine.
            }

            result
        }
        /// Converts an ErrorKind to a number
        pub fn error_to_u32( e:&ErrorKind ) -> u32
        {
            match *e
            {
                ErrorKind::Tag                   => 1,
                ErrorKind::MapRes                => 2,
                ErrorKind::MapOpt                => 3,
                ErrorKind::Alt                   => 4,
                ErrorKind::IsNot                 => 5,
                ErrorKind::IsA                   => 6,
                ErrorKind::SeparatedList         => 7,
                ErrorKind::SeparatedNonEmptyList => 8,
                ErrorKind::Many1                 => 9,
                ErrorKind::Count                 => 10,
                ErrorKind::TakeUntil             => 12,
                ErrorKind::LengthValue           => 15,
                ErrorKind::TagClosure            => 16,
                ErrorKind::Alpha                 => 17,
                ErrorKind::Digit                 => 18,
                ErrorKind::AlphaNumeric          => 19,
                ErrorKind::Space                 => 20,
                ErrorKind::MultiSpace            => 21,
                ErrorKind::LengthValueFn         => 22,
                ErrorKind::Eof                   => 23,
                ErrorKind::Switch                => 27,
                ErrorKind::TagBits               => 28,
                ErrorKind::OneOf                 => 29,
                ErrorKind::NoneOf                => 30,
                ErrorKind::Char                  => 40,
                ErrorKind::CrLf                  => 41,
                ErrorKind::RegexpMatch           => 42,
                ErrorKind::RegexpMatches         => 43,
                ErrorKind::RegexpFind            => 44,
                ErrorKind::RegexpCapture         => 45,
                ErrorKind::RegexpCaptures        => 46,
                ErrorKind::TakeWhile1            => 47,
                ErrorKind::Complete              => 48,
                ErrorKind::Fix                   => 49,
                ErrorKind::Escaped               => 50,
                ErrorKind::EscapedTransform      => 51,
                ErrorKind::NonEmpty              => 56,
                ErrorKind::ManyMN                => 57,
                ErrorKind::HexDigit              => 59,
                ErrorKind::OctDigit              => 61,
                ErrorKind::Many0                 => 62,
                ErrorKind::Not                   => 63,
                ErrorKind::Permutation           => 64,
                ErrorKind::ManyTill              => 65,
                ErrorKind::Verify                => 66,
                ErrorKind::TakeTill1             => 67,
                ErrorKind::TakeWhileMN           => 69,
                ErrorKind::TooLarge              => 70,
                ErrorKind::Many0Count            => 71,
                ErrorKind::Many1Count            => 72,
                ErrorKind::Float                 => 73,
                ErrorKind::Satisfy               => 74,
                ErrorKind::Fail                  => 75,
            }
        }
        /// Prints a message and the input if the parser fails.
        pub fn dbg_dmp<'a, F, O, E:Debug>( f:F, ctx:&'static str ) -> impl Fn( &'a [u8] ) -> IResult<&'a [u8], O, E> where
        F:Fn( &'a [u8] ) -> IResult<&'a [u8], O, E>,
        {
            move |i:&'a [u8]| match f( i )
            {
                Err( e ) =>
                {
                    println!( "{}:Error( {:?} ) at:\n{}", ctx, e, i.to_hex( 8 ) );
                    Err( e )
                }
                a => a,
            }
        }
    }
    /**/
    pub mod branch
    {
        //! Choice combinators
        use ::
        {
            nom::
            {
                error::{ ErrorKind, ParseError },
                internal::{ Err, Mode, Parser },
            },
            marker::{ PhantomData },
            *,
        };
        
        macro_rules! alt_trait
        (
            ($first:ident $second:ident $($id: ident)+) => ( alt_trait!(__impl $first $second; $($id)+); );
            (__impl $($current:ident)*; $head:ident $($id: ident)+) => 
            (
                alt_trait_impl!($($current)*);
                alt_trait!(__impl $($current)* $head; $($id)+);
            );
            (__impl $($current:ident)*; $head:ident) =>
            (
                alt_trait_impl!($($current)*);
                alt_trait_impl!($($current)* $head);
            );
        );

        macro_rules! alt_trait_impl
        (
            ($($id:ident)+) =>
            (
                impl
                <
                 Input: Clone, Output, Error: ParseError<Input>,
                 $($id: Parser<Input, Output = Output, Error = Error>),+
                > Parser<Input> for Choice< ( $($id),+ )>
                {
                    type Output = Output;
                    type Error = Error;

                    #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, input: Input ) -> 
                    crate::nom::PResult<OM, Input, Self::Output, Self::Error>
                    {
                        match self.parser.0.process::<OM>(input.clone())
                        {
                            Ok(res) => Ok(res),
                            Err(Err::Failure(e))=> Err(Err::Failure(e)),
                            Err(Err::Incomplete(i))=> Err(Err::Incomplete(i)),
                            Err(Err::Error(e)) => alt_trait_inner!(1, self, input, e, $($id)+),
                        }
                    }
                }
            );
        );

        macro_rules! alt_trait_inner
        (
            ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => 
            (
                match $self.parser.$it.process::<OM>($input.clone())
                {
                    Ok(res) => Ok(res),
                    Err(Err::Failure(e))=>Err(Err::Failure(e)),
                    Err(Err::Incomplete(i))=> Err(Err::Incomplete(i)),
                    Err(Err::Error(e)) => 
                    {
                        succ!
                        (
                            $it, 
                            alt_trait_inner!
                            (
                                $self, 
                                $input, 
                                <OM::Error as crate::nom::Mode>::combine($err, e, |e1, e2| e1.or(e2)), 
                                $($id)+
                            )
                        )
                    }
                }
            );

            ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident) =>
            (
                Err( Err::Error( <OM::Error as crate::nom::Mode>::map
                (
                    $err,
                    |err| Error::append($input, ErrorKind::Alt, err)
                )))
            );
        );

        macro_rules! permutation_trait
        (
            (
                $name1:ident $ty1:ident $item1:ident
                $name2:ident $ty2:ident $item2:ident
                $($name3:ident $ty3:ident $item3:ident)*
            ) =>
            ( permutation_trait!(__impl $name1 $ty1 $item1, $name2 $ty2 $item2; $($name3 $ty3 $item3)*); );

            (
                __impl $($name:ident $ty:ident $item:ident),+;
                $name1:ident $ty1:ident $item1:ident $($name2:ident $ty2:ident $item2:ident)*
            ) =>
            (
                permutation_trait_impl!($($name $ty $item),+);
                permutation_trait!(__impl $($name $ty $item),+ , $name1 $ty1 $item1; $($name2 $ty2 $item2)*);
            );

            (__impl $($name:ident $ty:ident $item:ident),+;) =>
            ( permutation_trait_impl!($($name $ty $item),+); );
        );

        macro_rules! permutation_trait_impl
        (
            ($($name:ident $ty:ident $item:ident),+) =>
            (
                impl<Input, Error, $($ty),+ , $($name),+> Parser<Input> for Permutation< ( $($name),+ ), Error> where
                Input: Clone,
                Error: ParseError<Input>,
                $($name: Parser<Input, Output = $ty, Error = Error>),+
                {
                    type Output = ( $($ty),+ );
                    type Error = Error;
                    #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, mut input: Input )
                    -> crate::nom::PResult<OM, Input, Self::Output, Self::Error>
                    {
                        let mut res = OM::Output::bind(|| ($(Option::<$ty>::None),+));
                        $(let mut $item = false;)+
                        loop
                        {
                            let mut err: Option<<OM::Error as Mode>::Output<Error>> = None;
                            permutation_trait_inner!(0, self, input, res,  err, $($item)+);
                            if let Some(err) = err 
                            {
                                return Err(Err::Error(OM::Error::map
                                (err, |err| Error::append(input, ErrorKind::Permutation, err))));
                            }

                            return Ok((input,OM::Output::map(res, |res|
                            {
                                match res
                                {
                                    ($(Some($item)),+) =>  ($($item),+),
                                    _ => unreachable!(),
                                }
                            })))
                        }
                    }
                }
            );
        );

        macro_rules! permutation_trait_inner
        (
            ($it:tt, $self:expr, $input:ident, $res:expr,  $err:expr, $head:ident $($item:ident)*) =>
            (
                if !$head
                {
                    match $self.parser.$it.process::<OM>($input.clone())
                    {
                        Ok((i, o)) =>
                        {
                            $input = i;
                            $res = OM::Output::combine($res, o, |mut res, o | {res.$it = Some(o);res });
                            $head = true;
                            continue;
                        }
                        Err(Err::Error(e)) =>
                        {
                            $err = Some(match $err {
                                None => e,
                                Some(err) => OM::Error::combine(err, e, |err, e| err.or(e))
                            });
                        }
                        Err(e) => return Err(e),
                    };
                }
                succ!($it, permutation_trait_inner!($self, $input, $res, $err, $($item)*));
            );

            ($it:tt, $self:expr, $input:ident, $res:expr, $err:expr,) => ();
        );
        /// Tests a list of parsers one by one until one succeeds.
        pub fn alt<List>(l: List) -> Choice<List>
        {
            Choice { parser: l }
        }
        /// Applies a list of parsers in any order.
        pub fn permutation<I: Clone, E: ParseError<I>, List>(list: List) -> Permutation<List, E>
        {
            Permutation
            {
                parser: list,
                e: PhantomData,
            }
        }
        /// Wrapping structure for the [alt()] combinator implementation
        pub struct Choice<T>
        {
            parser: T,
        }
        
        alt_trait!(A B C D E F G H I J K L M N O P Q R S T U);
        // Manually implement Alt for (A,), the 1-tuple type
        impl
        <
         Input, 
         Output, 
         Error: ParseError<Input>, 
         A: Parser<Input, Output = Output, 
         Error = Error>> Parser<Input> for Choice<(A,)
        >
        {
            type Output = Output;
            type Error = Error;
            #[inline] fn process<OM: crate::nom::OutputMode>( &mut self, input: Input ) -> 
            crate::nom::PResult<OM, Input, Self::Output, Self::Error> 
            { self.parser.0.process::<OM>(input) }
        }

        impl
        <
         const N: usize,
         Input: Clone,
         Output,
         Error: ParseError<Input>,
         A: Parser<Input, Output = Output, Error = Error>,
        > Parser<Input> for Choice<[A; N]>
        {
            type Output = Output;
            type Error = Error;

            #[inline] fn process<OM: crate::nom::OutputMode>( &mut self, input: Input ) -> 
            crate::nom::PResult<OM, Input, Self::Output, Self::Error>
            {
                let mut error = None;
                for branch in &mut self.parser
                {
                    match branch.process::<OM>(input.clone())
                    {
                        Err(Err::Error(e)) => match error
                        {
                            None => error = Some(e),
                            Some(err) => error = Some(OM::Error::combine(err, e, |e1, e2| e1.or(e2))),
                        },
                        res => return res,
                    }
                }

                match error
                {
                    Some(e) => Err(Err::Error(OM::Error::map(e, |err| { Error::append(input, ErrorKind::Alt, err) }))),
                    None => Err(Err::Error(OM::Error::bind(|| { Error::from_error_kind(input, ErrorKind::Alt) }))),
                }
            }
        }

        impl
        <
         Input: Clone,
         Output,
         Error: ParseError<Input>,
         A: Parser<Input, Output = Output, Error = Error>,
        > Parser<Input> for Choice<&mut [A]>
        {
            type Output = Output;
            type Error = Error;
            #[inline] fn process<OM: crate::nom::OutputMode>( &mut self, input: Input ) -> 
            crate::nom::PResult<OM, Input, Self::Output, Self::Error>
            {
                let mut error = None;
                for branch in self.parser.iter_mut()
                {
                    match branch.process::<OM>(input.clone())
                    {
                        Err(Err::Error(e)) => match error
                        {
                            None => error = Some(e),
                            Some(err) => error = Some(OM::Error::combine(err, e, |e1, e2| e1.or(e2))),
                        },
                        res => return res,
                    }
                }

                match error
                {
                    Some(e) => Err(Err::Error(OM::Error::map(e, |err| { Error::append(input, ErrorKind::Alt, err) }))),
                    None => Err(Err::Error(OM::Error::bind(|| { Error::from_error_kind(input, ErrorKind::Alt) }))),
                }
            }
        }
        /// Wrapping structure for the [permutation] combinator implementation
        pub struct Permutation<T, Error>
        {
            parser: T,
            e: PhantomData<Error>,
        }

        permutation_trait!
        (
            FnA A a
            FnB B b
            FnC C c
            FnD D d
            FnE E e
            FnF F f
            FnG G g
            FnH H h
            FnI I i
            FnJ J j
            FnK K k
            FnL L l
            FnM M m
            FnN N n
            FnO O o
            FnP P p
            FnQ Q q
            FnR R r
            FnS S s
            FnT T t
            FnU U u
        );
    }
    /*
    */
    pub mod combinator
    {
        //! General purpose combinators
        use ::
        {
            borrow::{ Borrow },
            boxed::{ Box },
            convert::{ Into },
            fmt::{ Debug },
            mem::{ transmute },
            ops::{ Range, RangeFrom, RangeTo },
            nom::
            {
                error::{ErrorKind, FromExternalError, ParseError},
                internal::{ * },
                traits::{AsChar, Compare, CompareResult, Input, Offset, ParseTo},
            },
            marker::{ PhantomData },
            *,
        };
        /// Return the remaining input.
        #[inline] pub fn rest<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where 
        T: Input 
        { Ok(input.take_split(input.input_len())) }
        /// Return the length of the remaining input.
        #[inline] pub fn rest_len<T, E: ParseError<T>>(input: T) -> IResult<T, usize, E> where
        T: Input,
        {
            let len = input.input_len();
            Ok((input, len))
        }
        /// Maps a function on the result of a parser.
        pub fn map<I, O, E: ParseError<I>, F, G>(parser: F, f: G) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: FnMut(<F as Parser<I>>::Output) -> O,
        { parser.map(f) }
        /// Applies a function returning a `Result` over the result of a parser.
        pub fn map_res
        <
         I: Clone, 
         O, 
         E: ParseError<I> + FromExternalError<I, E2>, 
         E2, 
         F, 
         G
        >( parser: F, f: G ) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: FnMut(<F as Parser<I>>::Output) -> Result<O, E2>,
        { parser.map_res(f) }
        /// Applies a function returning an `Option` over the result of a parser.
        pub fn map_opt<I:Clone, O, E: ParseError<I>, F, G>( parser: F, f: G ) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: FnMut(<F as Parser<I>>::Output) -> Option<O>,
        { parser.map_opt(f) }
        /// Applies a parser over the result of another one.
        pub fn map_parser
        <
         I, 
         O, 
         E: ParseError<I>, 
         F, 
         G
        >( parser: F, applied_parser: G ) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: Parser<<F as Parser<I>>::Output, Output = O, Error = E>,
        { parser.and_then(applied_parser) }
        /// Creates a new parser from the output of the first parser, then apply that parser over the rest of the input.
        pub fn flat_map
        <
         I, 
         O, 
         E: ParseError<I>, 
         F, 
         G, 
         H
        >
        ( parser: F, applied_parser: G ) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: FnMut(<F as Parser<I>>::Output) -> H,
        H: Parser<I, Output = O, Error = E>,
        { parser.flat_map(applied_parser) }
        /// Optional parser, will return `None` on [`Err::Error`].
        pub fn opt<I: Clone, E: ParseError<I>, F>
        ( f: F ) -> impl Parser<I, Output = Option<<F as Parser<I>>::Output>, Error = E> where
        F: Parser<I, Error = E>
        { Opt { parser: f } }
        /// Parser implementation for [opt]
        pub struct Opt<F>
        {
            parser: F,
        }

        impl<I, F: Parser<I>> Parser<I> for Opt<F> where
        I: Clone,
        {
            type Output = Option<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self
                .parser
                .process::<OutputM<OM::Output, Check, OM::Incomplete>>(input)
                {
                    Ok((i, o)) => Ok((i, OM::Output::map(o, Some))),
                    Err(Err::Error(_)) => Ok((i, OM::Output::bind(|| None))),
                    Err(Err::Failure(e)) => Err(Err::Failure(e)),
                    Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                }
            }
        }
        /// Calls the parser if the condition is met.
        pub fn cond<I, E: ParseError<I>, F>( b:bool, f:F ) -> 
        impl Parser<I, Output = Option<<F as Parser<I>>::Output>, Error = E> where
        F: Parser<I, Error = E>,
        {
            Cond 
            { 
                parser: if b { Some(f) } else { None } 
            }
        }
        /// Parser implementation for [cond]
        pub struct Cond<F>
        {
            parser: Option<F>,
        }

        impl<I, F> Parser<I> for Cond<F> where
        F: Parser<I>,
        {
            type Output = Option<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match &mut self.parser
                {
                    None => Ok((input, OM::Output::bind(|| None))),
                    Some(f) => f
                    .process::<OM>(input)
                    .map(|(i, o)| (i, OM::Output::map(o, Some))),
                }
            }
        }
        /// Tries to apply its parser without consuming the input.
        pub fn peek<I: Clone, F>( parser: F ) -> 
        impl Parser<I, Output = <F as Parser<I>>::Output, Error = <F as Parser<I>>::Error> where
        F: Parser<I>
        { Peek { parser } }
        /// Parsr implementation for [peek]
        pub struct Peek<F>
        {
            parser: F,
        }

        impl<I, F> Parser<I> for Peek<F> where
        I: Clone,
        F: Parser<I>,
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self.parser.process::<OM>(input)
                {
                    Ok((_, o)) => Ok((i, o)),
                    Err(e) => Err(e),
                }
            }
        }
        /// Returns its input if it is at the end of input data.
        pub fn eof<I: Input + Clone, E: ParseError<I>>(input: I) -> IResult<I, I, E>
        {
            if input.input_len() == 0
            {
                let clone = input.clone();
                Ok((input, clone))
            } 
            else { Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))) }
        }
        /// Transforms Incomplete into `Error`.
        pub fn complete<I: Clone, O, E: ParseError<I>, F>( parser: F ) -> impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Output = O, Error = E>,
        { MakeComplete { parser } }
        /// Parser implementation for [complete]
        pub struct MakeComplete<F>
        {
            parser: F
        }

        impl<I, F> Parser<I> for MakeComplete<F> where
        I: Clone,
        F: Parser<I>,
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self
                .parser
                .process::<OutputM<OM::Output, OM::Error, Complete>>(input)
                {
                    Err(Err::Incomplete(_)) => Err(Err::Error(OM::Error::bind(|| 
                    { <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::Complete) }))),
                    Err(e) => Err(e),
                    Ok(o) => Ok(o),
                }
            }
        }
        /// Succeeds if all the input has been consumed by its child parser.
        pub fn all_consuming<I, E: ParseError<I>, F>( parser: F ) -> 
        impl Parser<I, Output = <F as Parser<I>>::Output, Error = E> where
        I: Input,
        F: Parser<I, Error = E>,
        { AllConsuming { parser } }
        /// Parser implementation for [all_consuming]
        pub struct AllConsuming<F>
        {
            parser: F,
        }

        impl<I, F> Parser<I> for AllConsuming<F> where
        I: Input,
        F: Parser<I>,
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (input, res) = self.parser.process::<OM>(input)?;
                if input.input_len() == 0 { Ok((input, res)) } 
                else
                { Err(Err::Error(OM::Error::bind(|| { <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Eof)}))) }
            }
        }
        /// Returns the result of the child parser if it satisfies a verification function.
        pub fn verify
        <
         I: Clone, 
         O2, 
         E: ParseError<I>, 
         F, 
         G
        >( first: F, second: G ) -> 
        impl Parser<I, Output = <F as Parser<I>>::Output, Error = E> where
        F: Parser<I, Error = E>,
        G: Fn(&O2) -> bool,
        <F as Parser<I>>::Output: Borrow<O2>,
        O2: ?Sized,
        {
            Verify
            {
                first,
                second,
                o2: PhantomData,
            }
        }
        /// Parser iplementation for verify
        pub struct Verify<F, G, O2: ?Sized>
        {
            first: F,
            second: G,
            o2: PhantomData<O2>,
        }

        impl<I, F: Parser<I>, G, O2> Parser<I> for Verify<F, G, O2> where
        I: Clone,
        G: Fn(&O2) -> bool,
        <F as Parser<I>>::Output: Borrow<O2>,
        O2: ?Sized,
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, o) = self.first.process::<OutputM<Emit, OM::Error, OM::Incomplete>>(input.clone())?;
                if (self.second)(o.borrow()) { Ok((i, OM::Output::bind(|| o))) } 
                else { Err(Err::Error(OM::Error::bind(move ||
                {
                    let e: ErrorKind = ErrorKind::Verify;
                    <F as Parser<I>>::Error::from_error_kind(input, e) 
                })))}
            }
        }
        /// Returns the provided value if the child parser succeeds.
        pub fn value<I, O1: Clone, E: ParseError<I>, F>( val: O1, parser: F ) -> 
        impl Parser<I, Output = O1, Error = E> where
        F: Parser<I, Error = E>,
        { parser.map(move |_| val.clone()) }
        /// Succeeds if the child parser returns an error.
        pub fn not<I: Clone, E: ParseError<I>, F>(parser: F) -> impl Parser<I, Output = (), Error = E> where
        F: Parser<I, Error = E>
        { Not { parser } }
        /// Parser implementation for [not]
        pub struct Not<F>
        { 
            parser: F,
        }

        impl<I, F> Parser<I> for Not<F> where
        I: Clone,
        F: Parser<I>,
        {
            type Output = ();
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self.parser.process::<OM>(input)
                {
                    Ok(_) => Err(Err::Error(OM::Error::bind(||
                    { <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::Not) }))),
                    Err(Err::Error(_)) => Ok((i, OM::Output::bind(|| ()))),
                    Err(e) => Err(e),
                }
            }
        }
        /// If the child parser was successful, return the consumed input as produced value.
        pub fn recognize<I: Clone + Offset + Input, E: ParseError<I>, F>( parser: F ) -> 
        impl Parser<I, Output = I, Error = E> where
        F: Parser<I, Error = E>
        { Recognize { parser } }
        /// Parser implementation for [recognize]
        pub struct Recognize<F> 
        {
            parser: F,
        }

        impl<I, F> Parser<I> for Recognize<F> where
        I: Clone + Offset + Input,
        F: Parser<I>
        {
            type Output = I;
            type Error = <F as Parser<I>>::Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self
                .parser
                .process::<OutputM<Check, OM::Error, OM::Incomplete>>(i)
                {
                    Ok((i, _)) =>
                    {
                        let index = input.offset(&i);
                        Ok((i, OM::Output::bind(|| input.take(index))))
                    }
                    Err(e) => Err(e),
                }
            }
        }
        /// If the child parser was successful, return the consumed input with the output as a tuple.
        pub fn consumed<I, F, E>( parser: F ) -> impl Parser<I, Output = (I, <F as Parser<I>>::Output), Error = E> where
        I: Clone + Offset + Input,
        E: ParseError<I>,
        F: Parser<I, Error = E>,
        { Consumed { parser } }
        /// Parser implementation for [consumed]
        pub struct Consumed<F> 
        {
            parser: F
        }

        impl<I, F> Parser<I> for Consumed<F> where
        I: Clone + Offset + Input,
        F: Parser<I>,
        {
            type Output = (I, <F as Parser<I>>::Output);
            type Error = <F as Parser<I>>::Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let i = input.clone();
                match self.parser.process::<OM>(i)
                {
                    Ok((remaining, result)) =>
                    {
                        let index = input.offset(&remaining);
                        Ok((
                            remaining,
                            OM::Output::map(result, |res|
                            {
                                let consumed = input.take(index);
                                (consumed, res)
                            }),
                        ))
                    }
                    Err(e) => Err(e),
                }
            }
        }
        /// Transforms an [`Err::Error`] (recoverable) to [`Err::Failure`] (unrecoverable).
        pub fn cut<I, E: ParseError<I>, F>( parser:F ) -> impl Parser<I, Output = <F as Parser<I>>::Output, Error = E> where
        F: Parser<I, Error = E>
        { Cut { parser } }
        /// Parser implementation for [cut]
        pub struct Cut<F>
        {
            parser: F
        }

        impl<I, F> Parser<I> for Cut<F> where
        F: Parser<I>,
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match self.parser.process::<OutputM<OM::Output, Emit, OM::Incomplete>>(input)
                {
                    Err(Err::Error(e)) => Err(Err::Failure(e)),
                    Err(Err::Failure(e)) => Err(Err::Failure(e)),
                    Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                    Ok((i, o)) => Ok((i, o)),
                }
            }
        }
        /// Automatically converts the child parser's result to another type.
        pub fn into<I, O1, O2, E1, E2, F>(parser: F) -> impl Parser<I, Output = O2, Error = E2> where
        O2: From<O1>,
        E2: From<E1>,
        E1: ParseError<I>,
        E2: ParseError<I>,
        F: Parser<I, Output = O1, Error = E1>,
        { parser.into::<O2, E2>() }
        /// Creates an iterator from input data and a parser.
        pub fn iterator<Input, Error, F>(input: Input, f: F) -> ParserIterator<Input, Error, F> where
        F: Parser<Input>,
        Error: ParseError<Input>,
        {
            ParserIterator
            {
                iterator: f,
                input,
                state: Some(State::Running),
            }
        }
        /// Main structure associated to the [iterator] function.
        pub struct ParserIterator<I, E, F>
        {
            iterator: F,
            input: I,
            state: Option<State<E>>
        }

        impl<I: Clone, E, F> ParserIterator<I, E, F>
        {
            /// Returns the remaining input if parsing was successful, or the error if we encountered an error.
            pub fn finish(mut self) -> IResult<I, (), E>
            {
                match self.state.take().unwrap()
                {
                    State::Running | State::Done => Ok((self.input, ())),
                    State::Failure(e) => Err(Err::Failure(e)),
                    State::Incomplete(i) => Err(Err::Incomplete(i)),
                }
            }
        }

        impl<Input, Output, Error, F> core::iter::Iterator for ParserIterator<Input, Error, F> where
        F: Parser<Input, Output = Output, Error = Error>,
        Input: Clone,
        {
            type Item = Output;
            fn next(&mut self) -> Option<Self::Item>
            {
                if let State::Running = self.state.take().unwrap()
                {
                    let input = self.input.clone();
                    match (self.iterator).parse(input)
                    {
                        Ok((i, o)) =>
                        {
                            self.input = i;
                            self.state = Some(State::Running);
                            Some(o)
                        }
                        Err(Err::Error(_)) =>
                        {
                            self.state = Some(State::Done);
                            None
                        }
                        Err(Err::Failure(e)) =>
                        {
                            self.state = Some(State::Failure(e));
                            None
                        }
                        Err(Err::Incomplete(i)) =>
                        {
                            self.state = Some(State::Incomplete(i));
                            None
                        }
                    }
                }
                else { None }
            }
        }

        enum State<E>
        {
            Running,
            Done,
            Failure(E),
            Incomplete(Needed),
        }
        /// A parser which always succeeds with given value without consuming any input.
        pub fn success<I, O: Clone, E: ParseError<I>>(val: O) -> impl Parser<I, Output = O, Error = E>
        {
            Success
            {
                val,
                e: PhantomData,
            }
        }
        /// Parser implementation for [success]
        pub struct Success<O: Clone, E>
        {
            val: O,
            e: PhantomData<E>,
        }

        impl<I, O, E> Parser<I> for Success<O, E> where
        O: Clone,
        E: ParseError<I>,
        {
            type Output = O;
            type Error = E;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            { Ok((input, OM::Output::bind(|| self.val.clone()))) }
        }
        /// A parser which always fails.
        pub fn fail<I, O, E: ParseError<I>>() -> impl Parser<I, Output = O, Error = E>
        {
            Fail 
            {
                o: PhantomData,
                e: PhantomData,
            }
        }
        /// Parser implementation for [fail]
        pub struct Fail<O, E>
        {
            o: PhantomData<O>,
            e: PhantomData<E>,
        }

        impl<I, O, E> Parser<I> for Fail<O, E> where
        E: ParseError<I>,
        {
            type Output = O;
            type Error = E;
            fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error>
            { Err(Err::Error(OM::Error::bind(|| { E::from_error_kind(input, ErrorKind::Fail) }))) }
        }
    }
    /*
    */
    pub mod internal
    {
        //! Basic types to build the parsers
        use ::
        {
            borrow::{ ToOwned },
            error::{ Error },
            nom::
            {
                error::{ self, ErrorKind, FromExternalError, ParseError },
                internal::{ Needed::* },
            },
            marker::{ PhantomData },
            num::{ NonZeroUsize },
            string::{ String },
            vec::{ Vec },
            *,
        };

        macro_rules! impl_parser_for_tuple
        {
            ($($parser:ident $output:ident),+) => 
            (
                impl<I, $($output),+, E: ParseError<I>, $($parser),+> Parser<I> for ($($parser),+,) where
                $($parser: Parser<I, Output = $output, Error = E>),+
                {
                    type Output = ($($output),+,);
                    type Error = E;
                    #[inline(always)] fn process<P: OutputMode>(&mut self, i: I) -> PResult<P, I, Self::Output, Self::Error>
                    {
                        let ($(ref mut $parser),+,) = *self;
                        $(let(i, $output) = $parser.process::<OutputM<Emit, P::Error, P::Incomplete>>(i)?;)+
                        Ok((i, P::Output::bind(|| ($($output),+,))))
                    }
                }
            )
        }

        macro_rules! impl_parser_for_tuples
        {
            ($parser1:ident $output1:ident, $($parser:ident $output:ident),+) => 
            { impl_parser_for_tuples!(__impl $parser1 $output1; $($parser $output),+); };
            (__impl $($parser:ident $output:ident),+; $parser1:ident $output1:ident $(,$parser2:ident $output2:ident)*) =>
            {
                impl_parser_for_tuple!($($parser $output),+);
                impl_parser_for_tuples!(__impl $($parser $output),+, $parser1 $output1; $($parser2 $output2),*);
            };
            (__impl $($parser:ident $output:ident),+;) => { impl_parser_for_tuple!($($parser $output),+); }
        }
        /// Holds the result of parsing functions
        pub type IResult<I, O, E = error::Error<I>> = Result<(I, O), Err<E>>;
        /// Helper trait to convert a parser's result to a more manageable type
        pub trait Finish<I, O, E> 
        {
            /// Converts the parser's result to a type that is more consumable by error management libraries.
            fn finish(self) -> Result<(I, O), E>;
        }

        impl<I, O, E> Finish<I, O, E> for IResult<I, O, E>
        {
            fn finish(self) -> Result<(I, O), E>
            {
                match self
                {
                    Ok(res) => Ok(res),
                    Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
                    Err(Err::Incomplete(_)) =>
                    {
                        panic!(r#"Cannot call `finish()` on `Err(Err::Incomplete(_))`: 
            This result means that the parser does not have enough data to decide;
            you should gather more data and try to reapply the parser instead"#)
                    }
                }
            }
        }

        /// Contains information on needed data if a parser returned `Incomplete`
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        pub enum Needed
        {
            /// Needs more data, but we do not know how much
            Unknown,
            /// Contains the required data size in bytes
            Size(NonZeroUsize),
        }

        impl Needed
        {
            /// Creates `Needed` instance, returns `Needed::Unknown` if the argument is zero
            pub fn new(s: usize) -> Self
            {
                match NonZeroUsize::new(s)
                {
                    Some(sz) => Needed::Size(sz),
                    None => Needed::Unknown,
                }
            }
            /// Indicates if we know how many bytes we need
            pub fn is_known(&self) -> bool { *self != Unknown }
            /// Maps a `Needed` to `Needed` by applying a function to a contained `Size` value.
            #[inline] pub fn map<F: Fn(NonZeroUsize) -> usize>(self, f: F) -> Needed
            {
                match self
                {
                    Unknown => Unknown,
                    Size(n) => Needed::new(f(n)),
                }
            }
        }
        /// The `Err` enum indicates the parser was not successful.
        #[derive(Debug, Clone, PartialEq)]
        pub enum Err<Failure, Error = Failure>
        {
            /// There was not enough data
            Incomplete(Needed),
            /// The parser had an error (recoverable)
            Error(Error),
            /// The parser had an unrecoverable error: we got to the right branch and we know other branches won't work, 
            /// so backtrack as fast as possible
            Failure(Failure),
        }

        impl<E> Err<E>
        {
            /// Tests if the result is Incomplete
            pub fn is_incomplete(&self) -> bool { matches!(self, Err::Incomplete(..)) }
            /// Applies the given function to the inner error
            pub fn map<E2, F>(self, f: F) -> Err<E2> where F: FnOnce(E) -> E2
            {
                match self
                {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure(t) => Err::Failure(f(t)),
                    Err::Error(t) => Err::Error(f(t)),
                }
            }
            /// Automatically converts between errors if the underlying type supports it
            pub fn convert<F>(e: Err<F>) -> Self where E: From<F>{ e.map( convert::Into::into ) }
        }

        impl<T> Err<(T, ErrorKind)>
        {
            /// Maps `Err<(T, ErrorKind)>` to `Err<(U, ErrorKind)>` with the given `F: T -> U`
            pub fn map_input<U, F>(self, f: F) -> Err<(U, ErrorKind)> where F: FnOnce(T) -> U
            {
                match self
                {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure((input, k)) => Err::Failure((f(input), k)),
                    Err::Error((input, k)) => Err::Error((f(input), k)),
                }
            }
        }

        impl<T> Err<error::Error<T>>
        {
            /// Maps `Err<error::Error<T>>` to `Err<error::Error<U>>` with the given `F: T -> U`
            pub fn map_input<U, F>(self, f: F) -> Err<error::Error<U>> where F: FnOnce(T) -> U
            {
                match self 
                {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure(error::Error { input, code }) => 
                    Err::Failure(error::Error 
                    {
                        input: f(input),
                        code,
                    }),
                    Err::Error(error::Error { input, code }) => 
                    Err::Error(error::Error 
                    {
                        input: f(input),
                        code,
                    }),
                }
            }
        }
        
        impl Err<(&[u8], ErrorKind)>
        {
            /// Obtaining ownership
            pub fn to_owned(self) -> Err<(Vec<u8>, ErrorKind)> { self.map_input(ToOwned::to_owned) }
        }
        
        impl Err<(&str, ErrorKind)>
        {
            /// Obtaining ownership
            pub fn to_owned(self) -> Err<(String, ErrorKind)> { self.map_input(ToOwned::to_owned) }
        }
        
        impl Err<error::Error<&[u8]>>
        {
            /// Obtaining ownership
            pub fn to_owned(self) -> Err<error::Error<Vec<u8>>> { self.map_input(ToOwned::to_owned) }
        }
        
        impl Err<error::Error<&str>>
        {
            /// Obtaining ownership
            pub fn to_owned(self) -> Err<error::Error<String>> { self.map_input(ToOwned::to_owned) }
        }

        impl<E: Eq> Eq for Err<E> {}

        impl<E> fmt::Display for Err<E> where E: fmt::Debug
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                match self
                {
                    Err::Incomplete(Needed::Size(u)) => write!(f, "Parsing requires {} bytes/chars", u),
                    Err::Incomplete(Needed::Unknown) => write!(f, "Parsing requires more data"),
                    Err::Failure(c) => write!(f, "Parsing Failure: {:?}", c),
                    Err::Error(c) => write!(f, "Parsing Error: {:?}", c),
                }
            }
        }
        
        impl<E> Error for Err<E> where E: fmt::Debug 
        { 
            fn source(&self) -> Option<&(dyn Error + 'static)> { None } 
        }
        /// Parser mode: influences how combinators build values.
        pub trait Mode
        {
            /// The output type that may be generated
            type Output<T>;
            /// Produces a value
            fn bind<T, F: FnOnce() -> T>(f: F) -> Self::Output<T>;
            /// Applies a function over the produced value
            fn map<T, U, F: FnOnce(T) -> U>(x: Self::Output<T>, f: F) -> Self::Output<U>;
            /// Combines two values generated by previous parsers
            fn combine<T, U, V, F: FnOnce(T, U) -> V>( x: Self::Output<T>, y: Self::Output<U>, f: F ) -> Self::Output<V>;
        }
        /// Produces a value. This is the default behaviour for parsers.
        pub struct Emit;
        impl Mode for Emit
        {
            type Output<T> = T;

            #[inline(always)] fn bind<T, F: FnOnce() -> T>(f: F) -> Self::Output<T> { f() }

            #[inline(always)]
            fn map<T, U, F: FnOnce(T) -> U>(x: Self::Output<T>, f: F) -> Self::Output<U> { f(x) }

            #[inline(always)] fn combine<T, U, V, F: FnOnce(T, U) -> V>
            ( x: Self::Output<T>, y: Self::Output<U>, f: F ) -> Self::Output<V> { f(x, y) }
        }
        /// Applies the parser, but do not a produce a value.
        pub struct Check;
        impl Mode for Check
        {
            type Output<T> = ();
            #[inline(always)] fn bind<T, F: FnOnce() -> T>(_: F) -> Self::Output<T> {}
            #[inline(always)] fn map<T, U, F: FnOnce(T) -> U>(_: Self::Output<T>, _: F) -> Self::Output<U> {}
            #[inline(always)] fn combine<T, U, V, F: FnOnce(T, U) -> V>
            ( _: Self::Output<T>, _: Self::Output<U>, _: F ) -> Self::Output<V> {}
        }
        /// Parser result type.
        pub type PResult<OM, I, O, E> = 
        Result<(I, <<OM as OutputMode>::Output as Mode>::Output<O>), Err<E, <<OM as OutputMode>::Error as Mode>::Output<E>>>;
        /// Trait Defining the parser's execution.
        pub trait OutputMode 
        {
            /// Defines the [Mode] for the output type. [Emit] will generate the value, 
            /// [Check] will apply the parser but will only generate `()` if successful.
            type Output: Mode;
            /// Defines the [Mode] for the output type. [Emit] will generate the value, [Check] will
            /// apply the parser but will only generate `()` if an error happened.
            type Error: Mode;
            /// Indicates whether the input data is "complete", ie we already have the entire data in the
            /// buffer, or if it is "streaming", where more data can be added later in the buffer.
            type Incomplete: IsStreaming;
        }
        /// Specifies the behaviour when a parser encounters an error that could be due to partial ata
        pub trait IsStreaming
        {
            /// Called by parsers on partial data errors.
            fn incomplete<E, F: FnOnce() -> E>(needed: Needed, err_f: F) -> Err<E>;
            /// Indicates whether the data is in streaming mode or not
            fn is_streaming() -> bool;
        }
        /// Indicates that the input data is streaming: more data may be available later
        pub struct Streaming;

        impl IsStreaming for Streaming
        {
            fn incomplete<E, F: FnOnce() -> E>(needed: Needed, _err_f: F) -> Err<E> { Err::Incomplete(needed) }
            #[inline] fn is_streaming() -> bool { true }
        }
        /// Indicates that the input data is complete: no more data may be added later
        pub struct Complete;

        impl IsStreaming for Complete
        {
            fn incomplete<E, F: FnOnce() -> E>(_needed: Needed, err_f: F) -> Err<E> { Err::Error(err_f()) }
            #[inline] fn is_streaming() -> bool { false }
        }
        /// Holds the parser execution modifiers: output [Mode], error [Mode] and streaming behaviour for input data.
        pub struct OutputM<M: Mode, EM: Mode, S: IsStreaming>
        {
            m: PhantomData<M>,
            em: PhantomData<EM>,
            s: PhantomData<S>,
        }

        impl<M: Mode, EM: Mode, S: IsStreaming> OutputMode for OutputM<M, EM, S> 
        {
            type Output = M;
            type Error = EM;
            type Incomplete = S;
        }
        /// All nom parsers implement this trait
        pub trait Parser<Input> 
        {
            /// Type of the produced value
            type Output;
            /// Error type of this parser
            type Error: ParseError<Input>;
            /// A parser takes input type, and returns `Result` with either the remaining input and output value or error.
            #[inline] fn parse(&mut self, input: Input) -> IResult<Input, Self::Output, Self::Error>
            { self.process::<OutputM<Emit, Emit, Streaming>>(input) }
            /// A parser takes in input type, and returns a `Result` containing
            /// either the remaining input and the output value, or an error
            #[inline] fn parse_complete(&mut self, input: Input) -> IResult<Input, Self::Output, Self::Error>
            { self.process::<OutputM<Emit, Emit, Complete>>(input) }
            /// A parser takes in input type, and returns a `Result` containing
            /// either the remaining input and the output value, or an error
            fn process<OM: OutputMode>( &mut self, input: Input ) -> PResult<OM, Input, Self::Output, Self::Error>;
            /// Maps a function over the result of a parser
            fn map<G, O2>(self, g: G) -> Map<Self, G> where
            G: FnMut(Self::Output) -> O2,
            Self: core::marker::Sized 
            { Map { f: self, g } }
            /// Applies a function returning a `Result` over the result of a parser.
            fn map_res<G, O2, E2>(self, g: G) -> MapRes<Self, G> where
            G: FnMut(Self::Output) -> Result<O2, E2>,
            Self::Error: FromExternalError<Input, E2>,
            Self: core::marker::Sized,
            { MapRes { f: self, g } }
            /// Applies a function returning an `Option` over the result of a parser.
            fn map_opt<G, O2>(self, g: G) -> MapOpt<Self, G> where
            G: FnMut(Self::Output) -> Option<O2>,
            Self: core::marker::Sized,
            { MapOpt { f: self, g } }
            /// Creates a second parser from the output of the first one, then apply over the rest of the input
            fn flat_map<G, H>(self, g: G) -> FlatMap<Self, G> where
            G: FnMut(Self::Output) -> H,
            H: Parser<Input, Error = Self::Error>,
            Self: core::marker::Sized,
            { FlatMap { f: self, g } }
            /// Applies a second parser over the output of the first one
            fn and_then<G>(self, g: G) -> AndThen<Self, G> where
            G: Parser<Self::Output, Error = Self::Error>,
            Self: core::marker::Sized,
            { AndThen { f: self, g } }
            /// Applies a second parser after the first one, return their results as a tuple
            fn and<G, O2>(self, g: G) -> And<Self, G> where
            G: Parser<Input, Output = O2, Error = Self::Error>,
            Self: core::marker::Sized,
            { And { f: self, g } }
            /// Applies a second parser over the input if the first one failed
            fn or<G>(self, g: G) -> Or<Self, G> where
            G: Parser<Input, Output = Self::Output, Error = Self::Error>,
            Self: core::marker::Sized,
            { Or { f: self, g } }
            /// Automatically converts the parser's output and error values to another type, 
            /// as long as they implement the `From` trait
            fn into<O2: From<Self::Output>, E2: From<Self::Error>>(self) -> Into<Self, O2, E2> where
            Self: core::marker::Sized,
            {
                Into 
                {
                    f: self,
                    phantom_out2: core::marker::PhantomData,
                    phantom_err2: core::marker::PhantomData,
                }
            }
        }

        impl<I, O, E: ParseError<I>, F> Parser<I> for F where
        F: FnMut(I) -> IResult<I, O, E>,
        {
            type Output = O;
            type Error = E;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, o) = self(i).map_err(|e| match e
                {
                    Err::Incomplete(i) => Err::Incomplete(i),
                    Err::Error(e) => Err::Error(OM::Error::bind(|| e)),
                    Err::Failure(e) => Err::Failure(e),
                })?;
                Ok((i, OM::Output::bind(|| o)))
            }
        }

        impl_parser_for_tuples!
        (
            P1 O1, P2 O2, P3 O3, P4 O4, P5 O5, P6 O6, P7 O7, P8 O8, P9 O9, P10 O10, P11 O11, P12 O12, P13 O13, P14 O14, 
            P15 O15, P16 O16, P17 O17, P18 O18, P19 O19, P20 O20, P21 O21
        );
        /// Implementation of `Parser::map`
        pub struct Map<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, O2, E: ParseError<I>, F: Parser<I, Error = E>, G: FnMut(<F as Parser<I>>::Output) -> O2>
        Parser<I> for Map<F, G>
        {
            type Output = O2;
            type Error = E;

            #[inline(always)] fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match self.f.process::<OM>(i)
                {
                    Err(e) => Err(e),
                    Ok((i, o)) => Ok((i, OM::Output::map(o, |o| (self.g)(o)))),
                }
            }
        }
        /// Implementation of `Parser::map_res`
        pub struct MapRes<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, O2, E2, F, G> Parser<I> for MapRes<F, G> where
        I: Clone,
        <F as Parser<I>>::Error: FromExternalError<I, E2>,
        F: Parser<I>,
        G: FnMut(<F as Parser<I>>::Output) -> Result<O2, E2>,
        {
            type Output = O2;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (input, o1) = self.f.process::<OutputM<Emit, OM::Error, OM::Incomplete>>(i.clone())?;
                match (self.g)(o1)
                {
                    Ok(o2) => Ok((input, OM::Output::bind(|| o2))),
                    Err(e) => Err(Err::Error(OM::Error::bind(|| 
                    { <F as Parser<I>>::Error::from_external_error(i, ErrorKind::MapRes, e) }))),
                }
            }
        }
        /// Implementation of `Parser::map_opt`
        pub struct MapOpt<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, O2, F, G> Parser<I> for MapOpt<F, G> where
        I: Clone,
        F: Parser<I>,
        G: FnMut(<F as Parser<I>>::Output) -> Option<O2>,
        {
            type Output = O2;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (input, o1) = self.f.process::<OutputM<Emit, OM::Error, OM::Incomplete>>(i.clone())?;
                match (self.g)(o1)
                {
                    Some(o2) => Ok((input, OM::Output::bind(|| o2))),
                    None => Err(Err::Error(OM::Error::bind(||
                    { <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::MapOpt) }))),
                }
            }
        }
        /// Implementation of `Parser::flat_map`
        pub struct FlatMap<F, G>
        {
            f: F,
            g: G,
        }

        impl
        <
         I, 
         E: ParseError<I>, 
         F: Parser<I, Error = E>, 
         G: FnMut(<F as Parser<I>>::Output) -> H, 
         H: Parser<I, Error = E>
        > Parser<I> for FlatMap<F, G>
        {
            type Output = <H as Parser<I>>::Output;
            type Error = E;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (input, o1) = self.f.process::<OutputM<Emit, OM::Error, OM::Incomplete>>(i)?;
                (self.g)(o1).process::<OM>(input)
            }
        }
        /// Implementation of `Parser::and_then`
        pub struct AndThen<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, F: Parser<I>, G: Parser<<F as Parser<I>>::Output, Error = <F as Parser<I>>::Error>> Parser<I> 
        for AndThen<F, G>
        {
            type Output = <G as Parser<<F as Parser<I>>::Output>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (input, o1) = self.f.process::<OutputM<Emit, OM::Error, OM::Incomplete>>(i)?;
                let (_, o2) = self.g.process::<OM>(o1)?;
                Ok((input, o2))
            }
        }
        /// Implementation of `Parser::and`
        pub struct And<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, E: ParseError<I>, F: Parser<I, Error = E>, G: Parser<I, Error = E>> Parser<I>
        for And<F, G>
        {
            type Output = (<F as Parser<I>>::Output, <G as Parser<I>>::Output);
            type Error = E;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, o1) = self.f.process::<OM>(i)?;
                let (i, o2) = self.g.process::<OM>(i)?;
                Ok((i, OM::Output::combine(o1, o2, |o1, o2| (o1, o2))))
            }
        }
        /// Implementation of `Parser::or`
        pub struct Or<F, G>
        {
            f: F,
            g: G,
        }

        impl
        <
         I: Clone,
         O,
         E: ParseError<I>,
         F: Parser<I, Output = O, Error = E>,
         G: Parser<I, Output = O, Error = E>,
        > Parser<I> for Or<F, G>
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match self.f.process::<OM>(i.clone())
                {
                    Err(Err::Error(e1)) => match self.g.process::<OM>(i)
                    {
                        Err(Err::Error(e2)) => Err(Err::Error(OM::Error::combine(e1, e2, |e1, e2| e1.or(e2)))),
                        res => res,
                    },
                    res => res,
                }
            }
        }
        /// Implementation of `Parser::into`
        pub struct Into<F, O2, E2>
        {
            f: F,
            phantom_out2: core::marker::PhantomData<O2>,
            phantom_err2: core::marker::PhantomData<E2>,
        }

        impl
        <
         I,
         O2: From<<F as Parser<I>>::Output>,
         E2: crate::nom::error::ParseError<I> + From<<F as Parser<I>>::Error>,
         F: Parser<I>,
        > Parser<I> for Into<F, O2, E2>
        {
            type Output = O2;
            type Error = E2;
            fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match self.f.process::<OM>(i)
                {
                    Ok((i, o)) => Ok((i, OM::Output::map(o, |o| o.into()))),
                    Err(Err::Error(e)) => Err(Err::Error(OM::Error::map(e, |e| e.into()))),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.into())),
                    Err(Err::Incomplete(e)) => Err(Err::Incomplete(e)),
                }
            }
        }
        /// Alternate between two Parser implementations with the same result type.
        pub enum Either<F, G>
        {
            Left(F),
            Right(G),
        }

        impl
        <
         I,
         F: Parser<I>,
         G: Parser<I, Output = <F as Parser<I>>::Output, Error = <F as Parser<I>>::Error>,
        > Parser<I> for Either<F, G>
        {
            type Output = <F as Parser<I>>::Output;
            type Error = <F as Parser<I>>::Error;
            #[inline] fn process<OM: OutputMode>(&mut self, i: I) -> PResult<OM, I, Self::Output, Self::Error>
            {
                match self
                {
                    Either::Left(f) => f.process::<OM>(i),
                    Either::Right(g) => g.process::<OM>(i),
                }
            }
        }
    }
    /*
    */
    pub mod multi
    {
        //! Combinators applying their child parser multiple times
        use ::
        {
            marker::{ PhantomData },
            mem::{ size_of },
            nom::
            {
                bytes::{ take },
                error::{ ErrorKind, ParseError },
                internal::{ Err, Needed, Parser },
                Check, Emit, Input, Mode, NomRange, OutputM, OutputMode,
            },
            num::{ NonZeroUsize },
            vec::{ Vec },
            *,
        };

        /// Don't pre-allocate more than 64KiB when calling `Vec::with_capacity`.
        const MAX_INITIAL_CAPACITY_BYTES: usize = 65536;
        /// Repeats the embedded parser, gathering the results in a `Vec`.
        pub fn many0<I, F>( parser:F ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = <F as Parser<I>>::Error> where
        I: Clone + Input,
        F: Parser<I>,
        {
            Many0 { parser }
        }
        /// Parser implementation for the [many0] combinator
        pub struct Many0<F>
        {
            parser: F,
        }
        
        impl<I, F> Parser<I> for Many0<F> where
        I: Clone + Input,
        F: Parser<I>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>
            (
                &mut self,
                mut i: I,
            ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut acc = OM::Output::bind(|| Vec::with_capacity(4));
                loop
                {
                    let len = i.input_len();
                    match self
                    .parser
                    .process::<OutputM<OM::Output, Check, OM::Incomplete>>(i.clone())
                    {
                        Err(Err::Error(_)) => return Ok((i, acc)),
                        Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                        Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                        Ok((i1, o)) => 
                        {
                            // infinite loop check: the parser must always consume
                            if i1.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::Many0)
                                })));
                            }

                            i = i1;
                            acc = OM::Output::combine(acc, o, |mut acc, o|
                            {
                                acc.push(o);
                                acc
                            })
                        }
                    }
                }
            }
        }

        /// Runs the embedded parser, gathering the results in a `Vec`.
        pub fn many1<I, F>( parser:F ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = <F as Parser<I>>::Error> where
        I: Clone + Input,
        F: Parser<I>,
        {
            Many1 { parser }
        }
        /// Parser implementation for the [many1] combinator
        pub struct Many1<F>
        {
            parser:F,
        }
        
        impl<I, F> Parser<I> for Many1<F> where
        I: Clone + Input,
        F: Parser<I>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut i: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match self.parser.process::<OutputM<OM::Output, Emit, OM::Incomplete>>(i.clone())
                {
                    Err(Err::Error(err)) => Err(Err::Error(OM::Error::bind(|| 
                    {
                        <F as Parser<I>>::Error::append(i, ErrorKind::Many1, err)
                    }))),
                    Err(Err::Failure(e)) => Err(Err::Failure(e)),
                    Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                    Ok((i1, o)) =>
                    {
                        let mut acc = OM::Output::map(o, |o|
                        {
                            let mut acc = Vec::with_capacity(4);
                            acc.push(o);
                            acc
                        });
                        i = i1;
                        loop 
                        {
                            let len = i.input_len();
                            match self.parser.process::<OutputM<OM::Output, Check, OM::Incomplete>>(i.clone())
                            {
                                Err(Err::Error(_)) => return Ok((i, acc)),
                                Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                                Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                                Ok((i1, o)) => 
                                {
                                    // infinite loop check: the parser must always consume
                                    if i1.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::Many0)
                                        })));
                                    }

                                    i = i1;
                                    acc = OM::Output::combine(acc, o, |mut acc, o|
                                    {
                                        acc.push(o);
                                        acc
                                    })
                                }
                            }
                        }
                    }
                }
            }
        }
        /// Applies the parser `f` until the parser `g` produces a result.
        pub fn many_till<I, E, F, G>( f: F, g: G ) -> 
        impl Parser<I, Output = (Vec<<F as Parser<I>>::Output>, <G as Parser<I>>::Output), Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            ManyTill
            {
                f,
                g,
                e: PhantomData,
            }
        }
        /// Parser implementation for the [many_till] combinator
        pub struct ManyTill<F, G, E>
        {
            f: F,
            g: G,
            e: PhantomData<E>
        }
        
        impl<I, F, G, E> Parser<I> for ManyTill<F, G, E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            type Output = (Vec<<F as Parser<I>>::Output>, <G as Parser<I>>::Output);
            type Error = E;
            fn process<OM: OutputMode>( &mut self, mut i: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut res = OM::Output::bind(crate::lib::std::vec::Vec::new);
                loop
                {
                    let len = i.input_len();
                    match self
                    .g
                    .process::<OutputM<OM::Output, Check, OM::Incomplete>>(i.clone())
                    {
                        Ok((i1, o)) => return Ok((i1, OM::Output::combine(res, o, |res, o| (res, o)))),
                        Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                        Err(Err::Incomplete(i)) => return Err(Err::Incomplete(i)),
                        Err(Err::Error(_)) => 
                        {
                            match self.f.process::<OM>(i.clone())
                            {
                                Err(Err::Error(err)) =>
                                {
                                    return Err(Err::Error(OM::Error::map(err, |err|
                                    {
                                        E::append(i, ErrorKind::ManyTill, err)
                                    })))
                                }
                                Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                                Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                                Ok((i1, o)) => 
                                {
                                    if i1.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            E::from_error_kind(i, ErrorKind::Many0)
                                        })));
                                    }

                                    i = i1;
                                    res = OM::Output::combine(res, o, |mut acc, o|
                                    {
                                        acc.push(o);
                                        acc
                                    })
                                }
                            }
                        }
                    }
                }
            }
        }
        /// Alternates between two parsers to produce a list of elements.
        pub fn separated_list0<I, E, F, G>( sep: G, f: F ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            SeparatedList0
            {
                parser: f,
                separator: sep,
            }
        }
        /// Parser implementation for the [separated_list0] combinator
        pub struct SeparatedList0<F, G>
        {
            parser: F,
            separator: G,
        }
        
        impl<I, E: ParseError<I>, F, G> Parser<I> for SeparatedList0<F, G> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut i: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut res = OM::Output::bind( Vec::new );
                match self
                .parser
                .process::<OutputM<OM::Output, Check, OM::Incomplete>>(i.clone())
                {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                    Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                    Ok((i1, o)) =>
                    {
                        res = OM::Output::combine(res, o, |mut res, o|
                        {
                            res.push(o);
                            res
                        });
                        i = i1;
                    }
                }

                loop
                {
                    let len = i.input_len();
                    match self
                    .separator
                    .process::<OutputM<Check, Check, OM::Incomplete>>(i.clone())
                    {
                        Err(Err::Error(_)) => return Ok((i, res)),
                        Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                        Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                        Ok((i1, _)) =>
                        {
                            match self
                            .parser
                            .process::<OutputM<OM::Output, Check, OM::Incomplete>>(i1.clone())
                            {
                                Err(Err::Error(_)) => return Ok((i, res)),
                                Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                                Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                                Ok((i2, o)) =>
                                {
                                    // infinite loop check: the parser must always consume
                                    if i2.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::SeparatedList)
                                        })));
                                    }

                                    res = OM::Output::combine(res, o, |mut res, o|
                                    {
                                        res.push(o);
                                        res
                                    });

                                    i = i2;
                                }
                            }
                        }
                    }
                }
            }
        }
        /// Alternates between two parsers to produce a list of elements until [`Err::Error`].
        pub fn separated_list1<I, E, F, G>( separator: G, parser: F ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            SeparatedList1 { parser, separator }
        }
        /// Parser implementation for the [separated_list1] combinator
        pub struct SeparatedList1<F, G>
        {
            parser: F,
            separator: G,
        }
        
        impl<I, E: ParseError<I>, F, G> Parser<I> for SeparatedList1<F, G> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut i: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut res = OM::Output::bind( Vec::new );
                match self.parser.process::<OM>(i.clone())
                {
                    Err(e) => return Err(e),
                    Ok((i1, o)) =>
                    {
                        res = OM::Output::combine(res, o, |mut res, o|
                        {
                            res.push(o);
                            res
                        });
                        i = i1;
                    }
                }

                loop
                {
                    let len = i.input_len();
                    match self
                    .separator
                    .process::<OutputM<Check, Check, OM::Incomplete>>(i.clone())
                    {
                        Err(Err::Error(_)) => return Ok((i, res)),
                        Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                        Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                        Ok((i1, _)) => 
                        {
                            match self
                            .parser
                            .process::<OutputM<OM::Output, Check, OM::Incomplete>>(i1.clone())
                            {
                                Err(Err::Error(_)) => return Ok((i, res)),
                                Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                                Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
                                Ok((i2, o)) =>
                                {
                                    // infinite loop check: the parser must always consume
                                    if i2.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::SeparatedList)
                                        })));
                                    }

                                    res = OM::Output::combine(res, o, |mut res, o|
                                    {
                                        res.push(o);
                                        res
                                    });
                                    i = i2;
                                }
                            }
                        }
                    }
                }
            }
        }
        /// Repeats the embedded parser `m..=n` times.
        pub fn many_m_n<I, E, F>( min: usize, max: usize, parser: F ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            ManyMN { parser, min, max }
        }
        /// Parser implementation for the [many_m_n] combinator
        pub struct ManyMN<F>
        {
            parser: F,
            min: usize,
            max: usize,
        }
        
        impl<I, F> Parser<I> for ManyMN<F> where
        I: Clone + Input,
        F: Parser<I>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if self.min > self.max
                { return Err(Err::Failure(<F as Parser<I>>::Error::from_error_kind( input, ErrorKind::ManyMN ))); }

                let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES / size_of::<<F as Parser<I>>::Output>().max(1);
                let mut res = OM::Output::bind(||
                {
                    Vec::with_capacity(self.min.min(max_initial_capacity))
                });

                for count in 0..self.max
                {
                    let len = input.input_len();
                    match self.parser.process::<OM>(input.clone())
                    {
                        Ok((tail, value)) =>
                        {
                            // infinite loop check: the parser must always consume
                            if tail.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::ManyMN)
                                })));
                            }

                            res = OM::Output::combine(res, value, |mut res, value|
                            {
                                res.push(value);
                                res
                            });
                            input = tail;
                        }

                        Err(Err::Error(e)) =>
                        {
                        if count < self.min
                        {
                            return Err(Err::Error(OM::Error::map(e, |e|
                            {
                                <F as Parser<I>>::Error::append(input, ErrorKind::ManyMN, e)
                            })));
                        } else { return Ok((input, res)); }
                        }

                        Err(e) => { return Err(e); }
                    }
                }

                Ok((input, res))
            }
        }

        /// Repeats the embedded parser, counting the results.
        pub fn many0_count<I, E, F>(parser: F) -> impl Parser<I, Output = usize, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            Many0Count { parser }
        }
        /// Parser implementation for the [many0_count] combinator
        pub struct Many0Count<F>
        {
            parser: F,
        }

        impl<I, F> Parser<I> for Many0Count<F> where
        I: Clone + Input,
        F: Parser<I>,
        {
            type Output = usize;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut count = 0;
                loop
                {
                    let input_ = input.clone();
                    let len = input.input_len();
                    match self
                    .parser
                    .process::<OutputM<Check, Check, OM::Incomplete>>(input_)
                    {
                        Ok((i, _)) =>
                        {
                            // infinite loop check: the parser must always consume
                            if i.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many0Count)
                                })));
                            }

                            input = i;
                            count += 1;
                        }

                        Err(Err::Error(_)) => return Ok((input, OM::Output::bind(|| count))),
                        Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                        Err(Err::Incomplete(i)) => return Err(Err::Incomplete(i)),
                    }
                }
            }
        }
        /// Runs the embedded parser, counting the results.
        pub fn many1_count<I, E, F>(parser: F) -> impl Parser<I, Output = usize, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            Many1Count { parser }
        }
        /// Parser implementation for the [many1_count] combinator
        pub struct Many1Count<F>
        {
            parser:F
        }

        impl<I, F> Parser<I> for Many1Count<F> where
        I: Clone + Input,
        F: Parser<I>,
        {
            type Output = usize;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut count = 0;
                match self
                .parser
                .process::<OutputM<Check, Check, OM::Incomplete>>(input.clone())
                {
                    Err(Err::Error(_)) => Err(Err::Error(OM::Error::bind(move ||
                    {
                        <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many1Count)
                    }))),
                    Err(Err::Failure(e)) => Err(Err::Failure(e)),
                    Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                    Ok((mut input, _)) =>
                    {
                        count += 1;
                        loop
                        {
                            let input_ = input.clone();
                            let len = input.input_len();
                            match self
                            .parser
                            .process::<OutputM<Check, Check, OM::Incomplete>>(input_)
                            {
                                Ok((i, _)) =>
                                {
                                    if i.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many1Count)
                                        })));
                                    }
                                    input = i;
                                    count += 1;
                                }

                                Err(Err::Error(_)) => return Ok((input, OM::Output::bind(|| count))),
                                Err(Err::Failure(e)) => return Err(Err::Failure(e)),
                                Err(Err::Incomplete(i)) => return Err(Err::Incomplete(i)),
                            }
                        }
                    }
                }
            }
        }
        /// Runs the embedded parser `count` times, gathering the results in a `Vec`
        pub fn count<I, F>( parser: F, count: usize ) -> 
        impl Parser<I, Output = Vec<<F as Parser<I>>::Output>, Error = <F as Parser<I>>::Error>
        where
        I: Clone,
        F: Parser<I>,
        {
            Count { parser, count }
        }
        /// Parser implementation for the [count] combinator
        pub struct Count<F>
        {
            parser: F,
            count: usize,
        }

        impl<I, F> Parser<I> for Count<F> where
        I: Clone,
        F: Parser<I>,
        {
            type Output = Vec<<F as Parser<I>>::Output>;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut input = i.clone();
                let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES / size_of::<<F as Parser<I>>::Output>().max(1);
                let mut res = OM::Output::bind(||
                {
                    Vec::with_capacity(self.count.min(max_initial_capacity))
                });

                for _ in 0..self.count
                {
                    let input_ = input.clone();
                    match self.parser.process::<OM>(input_)
                    {
                        Ok((i, o)) => 
                        {
                            res = OM::Output::combine(res, o, |mut res, o|
                            {
                                res.push(o);
                                res
                            });
                            input = i;
                        }

                        Err(Err::Error(e)) =>
                        {
                            return Err(Err::Error(OM::Error::map(e, |e|
                            {
                                <F as Parser<I>>::Error::append(i, ErrorKind::Count, e)
                            })));
                        }

                        Err(e) => { return Err(e); }
                    }
                }

                Ok((input, res))
            }
        }
        /// Runs the embedded parser repeatedly, filling the given slice with results.
        pub fn fill<'a, I, E, F>( parser: F, buf: &'a mut [<F as Parser<I>>::Output] ) -> 
        impl Parser<I, Output = (), Error = E> + 'a where
        I: Clone,
        F: Parser<I, Error = E> + 'a,
        E: ParseError<I>,
        {
            Fill { parser, buf }
        }
        /// Parser implementation for the [fill] combinator
        pub struct Fill<'a, F, O>
        {
            parser: F,
            buf: &'a mut [O],
        }

        impl<'a, I, F, O> Parser<I> for Fill<'a, F, O> where
        I: Clone,
        F: Parser<I, Output = O>,
        {
            type Output = ();
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut input = i.clone();
                for elem in self.buf.iter_mut()
                {
                    let input_ = input.clone();
                    match self.parser.process::<OM>(input_)
                    {
                        Ok((i, o)) =>
                        {
                            OM::Output::map(o, |o| *elem = o);
                            input = i;
                        }

                        Err(Err::Error(e)) =>
                        {
                            return Err(Err::Error(OM::Error::map(e, |e|
                            {
                                <F as Parser<I>>::Error::append(i, ErrorKind::Count, e)
                            })));
                        }
                        
                        Err(e) => { return Err(e); }
                    }
                }

                Ok((input, OM::Output::bind(|| ())))
            }
        }
        /// Repeats the embedded parser, calling `g` to gather the results.
        pub fn fold_many0<I, E, F, G, H, R> ( parser:F, init:H, g:G ) -> impl Parser<I, Output = R, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
        {
            FoldMany0
            {
                parser,
                g,
                init,
                r: PhantomData,
            }
        }
        /// Parser implementation for the [fold_many0] combinator
        pub struct FoldMany0<F, G, Init, R>
        {
            parser: F,
            g: G,
            init: Init,
            r: PhantomData<R>,
        }

        impl<I, F, G, Init, R> Parser<I> for FoldMany0<F, G, Init, R> where
        I: Clone + Input,
        F: Parser<I>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        Init: FnMut() -> R,
        {
            type Output = R;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut res = OM::Output::bind(|| (self.init)());
                let mut input = i;
                loop
                {
                    let i_ = input.clone();
                    let len = input.input_len();
                    match self.parser.process::<OM>(i_)
                    {
                        Ok((i, o)) =>
                        {
                            if i.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many0)
                                })));
                            }

                            res = OM::Output::combine(res, o, |res, o| (self.g)(res, o));
                            input = i;
                        }
                        Err(Err::Error(_)) => { return Ok((input, res)); }
                        Err(e) => { return Err(e); }
                    }
                }
            }
        }
        /// Repeats the embedded parser, calling `g` to gather the results.
        pub fn fold_many1<I, E, F, G, H, R>( parser: F, init: H, g: G ) -> impl Parser<I, Output = R, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
        {
            FoldMany1
            {
                parser,
                g,
                init,
                r: PhantomData,
            }
        }
        /// Parser implementation for the [fold_many1] combinator
        pub struct FoldMany1<F, G, Init, R>
        {
            parser:F,
            g:G,
            init:Init,
            r:PhantomData<R>,
        }

        impl<I, F, G, Init, R> Parser<I> for FoldMany1<F, G, Init, R> where
        I: Clone + Input,
        F: Parser<I>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        Init: FnMut() -> R,
        {
            type Output = R;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut res = OM::Output::bind(|| (self.init)());
                let input = i.clone();
                match self.parser.process::<OM>(input)
                {
                    Err(Err::Error(_)) => Err(Err::Error(OM::Error::bind(||
                    {
                        <F as Parser<I>>::Error::from_error_kind(i, ErrorKind::Many1)
                    }))),
                    Err(e) => Err(e),
                    Ok((i1, o1)) =>
                    {
                        res = OM::Output::combine(res, o1, |res, o| (self.g)(res, o));
                        let mut input = i1;
                        loop
                        {
                            let i_ = input.clone();
                            let len = input.input_len();
                            match self.parser.process::<OM>(i_)
                            {
                                Ok((i, o)) =>
                                {
                                    if i.input_len() == len
                                    {
                                        return Err(Err::Error(OM::Error::bind(||
                                        {
                                            <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many1)
                                        })));
                                    }

                                    res = OM::Output::combine(res, o, |res, o| (self.g)(res, o));
                                    input = i;
                                }

                                Err(Err::Error(_)) => { return Ok((input, res)); }
                                Err(e) => { return Err(e); }
                            }
                        }
                    }
                }
            }
        }
        /// Repeats the embedded parser `m..=n` times, calling `g` to gather the results.
        pub fn fold_many_m_n<I, E, F, G, H, R>( min:usize, max:usize, parser:F, init:H, g:G ) -> 
        impl Parser<I, Output = R, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
        {
            FoldManyMN
            {
                parser,
                g,
                init,
                min,
                max,
                r:PhantomData,
            }
        }
        /// Parser implementation for the [fold_many_m_n] combinator
        pub struct FoldManyMN<F, G, Init, R>
        {
            parser:F,
            g:G,
            init:Init,
            r:PhantomData<R>,
            min:usize,
            max:usize,
        }

        impl<I, F, G, Init, R> Parser<I> for FoldManyMN<F, G, Init, R> where
        I: Clone + Input,
        F: Parser<I>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        Init: FnMut() -> R,
        {
            type Output = R;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if self.min > self.max
                {
                    return Err(Err::Error(OM::Error::bind(||
                    {
                        <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::ManyMN)
                    })));
                }

                let mut res = OM::Output::bind(|| (self.init)());
                for count in 0..self.max
                {
                    let len = input.input_len();
                    match self.parser.process::<OM>(input.clone())
                    {
                        Ok((tail, value)) =>
                        {
                            if tail.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(tail, ErrorKind::ManyMN)
                                })));
                            }

                            res = OM::Output::combine(res, value, |res, o| (self.g)(res, o));
                            input = tail;
                        }
                        
                        Err(Err::Error(err)) =>
                        {
                            if count < self.min
                            {
                                return Err(Err::Error(OM::Error::map(err, |err|
                                {
                                    <F as Parser<I>>::Error::append(input, ErrorKind::ManyMN, err)
                                })));
                            } 
                            else { break; }
                        }
                        
                        Err(e) => return Err(e),
                    }
                }

                Ok((input, res))
            }
        }
        /// Gets a number from the parser and returns a subslice of the input of that size.
        pub fn length_data<I, E, F>(f: F) -> impl Parser<I, Output = I, Error = E> where
        I: Input,
        <F as Parser<I>>::Output: ToUsize,
        F: Parser<I, Error = E>,
        E: ParseError<I>,
        { f.flat_map(|size| take(size)) }
        /// Gets a number from the first parser, takes a subslice of the input of that size, 
        /// then applies the second parser on that subslice.
        pub fn length_value<I, E, F, G>( f: F, g: G ) -> impl Parser<I, Output = <G as Parser<I>>::Output, Error = E> where
        I: Clone + Input,
        <F as Parser<I>>::Output: ToUsize,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            LengthValue
            {
                length:f,
                parser:g,
                e:PhantomData,
            }
        }
        /// Parser implementation for the [length_value] combinator
        pub struct LengthValue<F, G, E>
        {
            length:F,
            parser:G,
            e:PhantomData<E>,
        }

        impl<I, F, G, E> Parser<I> for LengthValue<F, G, E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        <F as Parser<I>>::Output: ToUsize,
        E: ParseError<I>,
        {
            type Output = <G as Parser<I>>::Output;
            type Error = E;
            fn process<OM: OutputMode>( &mut self, input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, length) = self
                .length
                .process::<OutputM<Emit, OM::Error, OM::Incomplete>>(input)?;
                let length:usize = length.to_usize();

                if let Some(needed) = length
                .checked_sub(i.input_len())
                .and_then(NonZeroUsize::new)
                {
                    Err(Err::Incomplete(Needed::Size(needed)))
                }
                else
                {
                    let (rest, i) = i.take_split(length);
                    match self.parser.process::<OM>(i.clone())
                    {
                        Err(Err::Incomplete(_)) => Err(Err::Error(OM::Error::bind(||
                        {
                            E::from_error_kind(i, ErrorKind::Complete)
                        }))),
                        Err(e) => Err(e),
                        Ok((_, o)) => Ok((rest, o)),
                    }
                }
            }
        }
        /// Gets a number from the first parser, then applies the second parser that many times.
        pub fn length_count<I, E, F, G>( f: F, g: G ) -> 
        impl Parser<I, Output = Vec<<G as Parser<I>>::Output>, Error = E> where
        I: Clone,
        <F as Parser<I>>::Output: ToUsize,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        E: ParseError<I>,
        {
            LengthCount
            {
                length: f,
                parser: g,
                e: PhantomData,
            }
        }
        /// Parser implementation for the [length_count] combinator
        pub struct LengthCount<F, G, E>
        {
            length: F,
            parser: G,
            e: PhantomData<E>,
        }
        
        impl<I, F, G, E> Parser<I> for LengthCount<F, G, E> where
        I: Clone,
        F: Parser<I, Error = E>,
        G: Parser<I, Error = E>,
        <F as Parser<I>>::Output: ToUsize,
        E: ParseError<I>,
        {
            type Output = Vec<<G as Parser<I>>::Output>;
            type Error = E;
            fn process<OM: OutputMode>
            (
                &mut self,
                input: I,
            ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match self
                .length
                .process::<OutputM<Emit, OM::Error, OM::Incomplete>>(input)
                {
                    Err(e) => Err(e),
                    Ok((i, count)) =>
                    {
                        let count = count.to_usize();
                        let mut input = i.clone();
                        let max_init_capacity = MAX_INITIAL_CAPACITY_BYTES / size_of::<<F as Parser<I>>::Output>().max(1);
                        let mut res = OM::Output::bind(|| 
                        {
                            Vec::with_capacity(count.min( max_init_capacity ))
                        });

                        for _ in 0..count
                        {
                            let input_ = input.clone();
                            match self.parser.process::<OM>(input_)
                            {
                                Ok((i, o)) =>
                                {
                                    res = OM::Output::combine(res, o, |mut res, o|
                                    {
                                        res.push(o);
                                        res
                                    });
                                    input = i;
                                }
                                
                                Err(Err::Error(e)) =>
                                {
                                    return Err(Err::Error(OM::Error::map(e, |e|
                                    {
                                        <F as Parser<I>>::Error::append(i, ErrorKind::Count, e)
                                    })));
                                }

                                Err(e) => { return Err(e); }
                            }
                        }

                        Ok((input, res))
                    }
                }
            }
        }
        /// Repeats the embedded parser and collects the results in a type implementing `Extend + Default`.
        pub fn many<I, E, Collection, F, G>( range: G, parser: F ) -> impl Parser<I, Output = Collection, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        Collection: Extend<<F as Parser<I>>::Output> + Default,
        E: ParseError<I>,
        G: NomRange<usize>,
        {
            Many 
            {
                parser,
                range,
                c: PhantomData,
            }
        }
        /// Parser implementation for the [many] combinator
        pub struct Many<F, R, Collection>
        {
            parser:F,
            range:R,
            c:PhantomData<Collection>,
        }

        impl<I, F, R, Collection> Parser<I> for Many<F, R, Collection> where
        I: Clone + Input,
        F: Parser<I>,
        Collection: Extend<<F as Parser<I>>::Output> + Default,
        R: NomRange<usize>,
        {
            type Output = Collection;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if self.range.is_inverted()
                { return Err(Err::Failure(<F as Parser<I>>::Error::from_error_kind( input, ErrorKind::Many ))); }

                let mut res = OM::Output::bind(Collection::default);

                for count in self.range.bounded_iter()
                {
                    let len = input.input_len();
                    match self.parser.process::<OM>(input.clone())
                    {
                        Ok((tail, value)) => 
                        {
                            if tail.input_len() == len
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(input, ErrorKind::Many)
                                })));
                            }

                            res = OM::Output::combine(res, value, |mut res, value|
                            {
                                res.extend(Some(value));
                                res
                            });
                            input = tail;
                        }

                        Err(Err::Error(e)) =>
                        {
                            if !self.range.contains(&count)
                            {
                                return Err(Err::Error(OM::Error::map(e, |e|
                                {
                                    <F as Parser<I>>::Error::append(input, ErrorKind::Many, e)
                                })));
                            } 
                            else { return Ok((input, res)); }
                        }
                        
                        Err(e) => { return Err(e); }
                    }
                }

                Ok((input, res))
            }
        }
        /// Applies a parser and accumulates the results using a given function and initial value.
        pub fn fold<I, E, F, G, H, J, R>( range: J, parser: F, init: H, fold: G ) -> 
        impl Parser<I, Output = R, Error = E> where
        I: Clone + Input,
        F: Parser<I, Error = E>,
        G: FnMut(R, <F as Parser<I>>::Output) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
        J: NomRange<usize>,
        {
            Fold
            {
                parser,
                init,
                fold,
                range,
            }
        }
        /// Parser implementation for the [fold] combinator
        pub struct Fold<F, G, H, Range>
        {
            parser:F,
            init:H,
            fold:G,
            range:Range,
        }

        impl<I, F, G, H, Range, Res> Parser<I> for Fold<F, G, H, Range> where
        I: Clone + Input,
        F: Parser<I>,
        G: FnMut(Res, <F as Parser<I>>::Output) -> Res,
        H: FnMut() -> Res,
        Range: NomRange<usize>,
        {
            type Output = Res;
            type Error = <F as Parser<I>>::Error;
            fn process<OM: OutputMode>( &mut self, mut input: I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if self.range.is_inverted()
                {
                    return Err(Err::Failure(<F as Parser<I>>::Error::from_error_kind( input, ErrorKind::Fold )));
                }

                let mut acc = OM::Output::bind(|| (self.init)());

                for count in self.range.saturating_iter()
                {
                    let len = input.input_len();
                    match self.parser.process::<OM>(input.clone())
                    {
                        Ok((tail, value)) =>
                        {
                            if tail.input_len() == len 
                            {
                                return Err(Err::Error(OM::Error::bind(||
                                {
                                    <F as Parser<I>>::Error::from_error_kind(tail, ErrorKind::Fold)
                                })));
                            }

                            acc = OM::Output::combine(acc, value, |acc, value| (self.fold)(acc, value));
                            input = tail;
                        }

                        Err(Err::Error(err)) =>
                        {
                            if !self.range.contains(&count)
                            {
                                return Err(Err::Error(OM::Error::map(err, |err|
                                {
                                    <F as Parser<I>>::Error::append(input, ErrorKind::Fold, err)
                                })));
                            }
                            else { break; }
                        }

                        Err(e) => return Err(e),
                    }
                }

                Ok((input, acc))
            }
        }
    }
    /*
    */
    pub mod sequence
    {
        //! Combinators applying parsers in sequence
        use ::
        {
            nom::
            {
                error::{ ParseError },
                internal::{ IResult, Parser },
                Check, OutputM, OutputMode, PResult,
            },
            *,
        };

        macro_rules! tuple_trait
        (
            ($name1:ident $ty1:ident, $name2: ident $ty2:ident, $($name:ident $ty:ident),*) => 
            ( tuple_trait!(__impl $name1 $ty1, $name2 $ty2; $($name $ty),*); );

            (__impl $($name:ident $ty: ident),+; $name1:ident $ty1:ident, $($name2:ident $ty2:ident),*) =>
            (
                tuple_trait_impl!($($name $ty),+);
                tuple_trait!(__impl $($name $ty),+ , $name1 $ty1; $($name2 $ty2),*);
            );

            (__impl $($name:ident $ty: ident),+; $name1:ident $ty1:ident) =>
            (
                tuple_trait_impl!($($name $ty),+);
                tuple_trait_impl!($($name $ty),+, $name1 $ty1);
            );
        );

        macro_rules! tuple_trait_impl
        (
            ($($name:ident $ty: ident),+) => 
            (
                impl
                <
                    Input: Clone, $($ty),+ , Error: ParseError<Input>,
                    $($name: Parser<Input, Output = $ty, Error = Error>),+
                > Tuple<Input, ( $($ty),+ ), Error> for ( $($name),+ )
                {
                    fn parse_tuple(&mut self, input: Input) -> IResult<Input, ( $($ty),+ ), Error>
                    { tuple_trait_inner!(0, self, input, (), $($name)+) }
                }
            );
        );

        macro_rules! tuple_trait_inner
        (
            ($it:tt, $self:expr, $input:expr, (), $head:ident $($id:ident)+) => 
            ({
                let (i, o) = $self.$it.parse($input)?;
                succ!($it, tuple_trait_inner!($self, i, ( o ), $($id)+))
            });

            ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident $($id:ident)+) => 
            ({
                let (i, o) = $self.$it.parse($input)?;
                succ!($it, tuple_trait_inner!($self, i, ($($parsed)* , o), $($id)+))
            });

            ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident) => 
            ({
                let (i, o) = $self.$it.parse($input)?;
                Ok((i, ($($parsed)* , o)))
            });
        );
        /// Gets an object from the first parser, then gets another object from the second parser.
        pub fn pair<I, O1, O2, E:ParseError<I>, F, G>( first:F, second:G ) -> 
        impl Parser<I, Output = (O1, O2), Error = E> where
        F: Parser<I, Output = O1, Error = E>,
        G: Parser<I, Output = O2, Error = E>
        { first.and( second ) }
        /// Matches an object from the first parser and discards it,
        /// then gets an object from the second parser.
        pub fn preceded<I, O, E: ParseError<I>, F, G>( f:F g:G ) -> 
        impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: Parser<I, Output = O, Error = E>,
        {
            Preceded
            {
                f,
                g,
            }
        }
        
        pub struct Preceded<F, G>
        {
            f: F,
            g: G,
        }

        impl<I, E:ParseError<I>, F: Parser<I, Error = E>, G:Parser<I, Error = E>> Parser<I> for Preceded<F, G>
        {
            type Output = <G as Parser<I>>::Output;
            type Error = E;
            #[inline(always)] fn process<OM:OutputMode>(&mut self, i: I) -> 
            PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, _) = self.f.process::<OutputM<Check, OM::Error, OM::Incomplete>>(i)?;
                let (i, o2) = self.g.process::<OM>(i)?;
                Ok((i, o2))
            }
        }
        /// Gets an object from the first parser,
        /// then matches an object from the second parser and discards it.
        pub fn terminated<I, O, E: ParseError<I>, F, G>( f: F, g:G ) -> 
        impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Output = O, Error = E>,
        G: Parser<I, Error = E>,
        {
            Terminated
            {
                f,
                g,
            }
        }
        
        pub struct Terminated<F, G>
        {
            f:F,
            g:G,
        }

        impl<I, E:ParseError<I>, F:Parser<I, Error = E>, G:Parser<I, Error = E>> 
        Parser<I> for Terminated<F, G>
        {
            type Output = <F as Parser<I>>::Output;
            type Error = E;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, i: I) ->
            PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, o1) = self.f.process::<OM>(i)?;
                let (i, _) = self.g.process::<OutputM<Check, OM::Error, OM::Incomplete>>(i)?;
                Ok((i, o1))
            }
        }
        /// Gets an object from the first parser, then matches an object from the sep_parser and discards it,
        /// then gets another object from the second parser.
        pub fn separated_pair<I, O1, O2, E: ParseError<I>, F, G, H>( first:F, sep:G, second:H ) -> 
        impl Parser<I, Output = (O1, O2), Error = E> where
        F: Parser<I, Output = O1, Error = E>,
        G: Parser<I, Error = E>,
        H: Parser<I, Output = O2, Error = E>,
        { first.and( preceded( sep, second ) ) }
        /// Matches an object from the first parser and discards it,
        /// then gets an object from the second parser,
        /// and finally matches an object from the third parser and discards it.
        pub fn delimited<I, O, E: ParseError<I>, F, G, H>( first:F, second:G, third:H ) -> 
        impl Parser<I, Output = O, Error = E> where
        F: Parser<I, Error = E>,
        G: Parser<I, Output = O, Error = E>,
        H: Parser<I, Error = E>
        { preceded( first, terminated( second, third ) ) }
        /// Helper trait for the tuple combinator.
        ///
        /// This trait is implemented for tuples of parsers of up to 21 elements.
        pub trait Tuple<I, O, E>
        {
            /// Parses the input and returns a tuple of results of each parser.
            fn parse_tuple(&mut self, input: I) -> IResult<I, O, E>;
        }
        
        impl<Input, Output, Error: ParseError<Input>, F: Parser<Input, Output = Output, Error = Error>>
        Tuple<Input, (Output,), Error> for (F,)
        {
            fn parse_tuple(&mut self, input:Input) -> IResult<Input, (Output,), Error>
            {
                self.0.parse(input).map(|(i, o)| (i, (o,)))
            }
        }

        tuple_trait!(FnA A, FnB B, FnC C, FnD D, FnE E, FnF F, FnG G, FnH H, FnI I, FnJ J, FnK K, FnL L,
        FnM M, FnN N, FnO O, FnP P, FnQ Q, FnR R, FnS S, FnT T, FnU U);
        // Special case: implement `Tuple` for `()`, the unit type.
        // This can come up in macros which accept a variable number of arguments.
        // Literally, `()` is an empty tuple, so it should simply parse nothing.
        impl<I, E: ParseError<I>> Tuple<I, (), E> for () 
        {
            fn parse_tuple(&mut self, input: I) -> IResult<I, (), E> { Ok((input, ())) }
        }
        ///Applies a tuple of parsers one by one and returns their results as a tuple.
        pub fn tuple<I, O, E: ParseError<I>, List:Tuple<I, O, E>>( mut l: List ) -> 
        impl FnMut(I) -> IResult<I, O, E> 
        { move |i: I| l.parse_tuple(i) }
    }
    /*
    */
    mod traits
    {
        //! Traits input types have to implement to work with nom combinators.
        use ::
        {
            iter::{ Copied, Enumerate },
            ops::{ Range, RangeFrom, RangeFull, RangeTo },
            nom::
            {
                error::{ self, ErrorKind, ParseError },
                internal::{ Err, IResult, Needed },
            },
            slice::{ Iter },
            str::{ Chars, CharIndices, from_utf8, FromStr },
            string::{ String },
            vec::{ Vec },
            *,
        };
        static CHARS: &[u8] = b"0123456789abcdef";

        macro_rules! as_bytes_array_impls
        {
            ($($N:expr)+) =>
            {
                $( 
                    impl<'a> AsBytes for &'a [u8; $N] { #[inline(always)] fn as_bytes(&self) -> &[u8] { *self } }
                    impl AsBytes for [u8; $N] { #[inline(always)] fn as_bytes(&self) -> &[u8] { self } }
                )+
            };
        }
        
        macro_rules! impl_fn_slice
        {
            ( $ty:ty ) => { fn slice(&self, range: $ty) -> Self { &self[range] } };
        }

        macro_rules! slice_range_impl
        {
            ( [ $for_t:ident ], $ty:ty ) => { impl<'a, $for_t> Slice<$ty> for &'a [$for_t] { impl_fn_slice!($ty); } };
            ( $for_t:ty, $ty:ty ) => { impl<'a> Slice<$ty> for &'a $for_t { impl_fn_slice!($ty); } };
        }

        macro_rules! slice_ranges_impl
        {
            ( [ $for_type:ident ] ) =>
            {
                slice_range_impl! {[$for_type], Range<usize>}
                slice_range_impl! {[$for_type], RangeTo<usize>}
                slice_range_impl! {[$for_type], RangeFrom<usize>}
                slice_range_impl! {[$for_type], RangeFull}
            };
            ( $for_type:ty ) =>
            {
                slice_range_impl! {$for_type, Range<usize>}
                slice_range_impl! {$for_type, RangeTo<usize>}
                slice_range_impl! {$for_type, RangeFrom<usize>}
                slice_range_impl! {$for_type, RangeFull}
            };
        }
        
        macro_rules! array_impls
        {
            ($($N:expr)+) =>
            {
                $
                ( 
                    impl InputLength for [u8; $N] { #[inline] fn input_len(&self) -> usize { self.len() } }
                    impl<'a> InputLength for &'a [u8; $N] { #[inline] fn input_len(&self) -> usize { self.len() } }
                    impl<'a> InputIter for &'a [u8; $N]
                    {
                        type Item = u8;
                        type Iter = Enumerate<Self::IterElem>;
                        type IterElem = Copied<Iter<'a, u8>>;
                        fn iter_indices(&self) -> Self::Iter { (&self[..]).iter_indices() }
                        fn iter_elements(&self) -> Self::IterElem { (&self[..]).iter_elements() }
                        fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::Item) -> bool 
                        { (&self[..]).position(predicate) }
                        fn slice_index(&self, count: usize) -> Result<usize, Needed> { (&self[..]).slice_index(count) }
                    }
                    impl<'a> Compare<[u8; $N]> for &'a [u8]
                    {
                        #[inline(always)] fn compare(&self, t: [u8; $N]) -> CompareResult { self.compare(&t[..]) }
                        #[inline(always)] fn compare_no_case(&self, t: [u8;$N]) -> CompareResult { self.compare_no_case(&t[..]) }
                    }
                    impl<'a,'b> Compare<&'b [u8; $N]> for &'a [u8]
                    {
                        #[inline(always)] fn compare(&self, t: &'b [u8; $N]) -> CompareResult { self.compare(&t[..]) }
                        #[inline(always)] fn compare_no_case(&self, t: &'b [u8;$N]) -> CompareResult { self.compare_no_case(&t[..]) }
                    }
                    impl FindToken<u8> for [u8; $N] { fn find_token(&self, token: u8) -> bool { memchr::memchr(token, &self[..]).is_some() } }
                    impl<'a> FindToken<&'a u8> for [u8; $N] { fn find_token(&self, token: &u8) -> bool { self.find_token(*token) } }
                )+
            };
        }
        /// Abstract method to calculate the input length
        pub trait InputLength
        {
            /// Calculates the input length, as indicated by its name, and the name of the trait itself
            fn input_len(&self) -> usize;
        }

        impl<'a, T> InputLength for &'a [T] { #[inline] fn input_len(&self) -> usize { self.len() } }

        impl<'a> InputLength for &'a str { #[inline] fn input_len(&self) -> usize { self.len() } }

        impl<'a> InputLength for (&'a [u8], usize) 
        {
            #[inline] fn input_len(&self) -> usize
            {
                /*
                println!("bit input length for ({:?}, {}):", self.0, self.1);
                println!("-> {}", self.0.len() * 8 - self.1); */
                self.0.len() * 8 - self.1
            }
        }
        /// Useful functions to calculate the offset between slices and show a hexdump of a slice
        pub trait Offset
        {
            /// Offset between the first byte of self and the first byte of the argument
            fn offset(&self, second: &Self) -> usize;
        }

        impl Offset for [u8]
        {
            fn offset(&self, second: &Self) -> usize
            {
                let fst = self.as_ptr();
                let snd = second.as_ptr();
                snd as usize - fst as usize
            }
        }

        impl<'a> Offset for &'a [u8]
        {
            fn offset(&self, second: &Self) -> usize
            {
                let fst = self.as_ptr();
                let snd = second.as_ptr();
                snd as usize - fst as usize
            }
        }

        impl Offset for str
        {
            fn offset(&self, second: &Self) -> usize
            {
                let fst = self.as_ptr();
                let snd = second.as_ptr();
                snd as usize - fst as usize
            }
        }

        impl<'a> Offset for &'a str
        {
            fn offset(&self, second: &Self) -> usize
            {
                let fst = self.as_ptr();
                let snd = second.as_ptr();
                snd as usize - fst as usize
            }
        }
        /// Helper trait for types that can be viewed as a byte slice
        pub trait AsBytes
        {
            /// Casts the input type to a byte slice
            fn as_bytes(&self) -> &[u8];
        }

        impl<'a> AsBytes for &'a str { #[inline(always)] fn as_bytes(&self) -> &[u8] { (*self).as_bytes() } }

        impl AsBytes for str { #[inline(always)] fn as_bytes(&self) -> &[u8] { self.as_ref() } }

        impl<'a> AsBytes for &'a [u8] { #[inline(always)] fn as_bytes(&self) -> &[u8] { *self } }

        impl AsBytes for [u8] { #[inline(always)] fn as_bytes(&self) -> &[u8] { self } }

        as_bytes_array_impls! { 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 }

        /// Transforms common types to a char for basic token parsing
        pub trait AsChar
        {
            /// makes a char from self
            fn as_char(self) -> char;
            /// Tests that self is an alphabetic character
            fn is_alpha(self) -> bool;
            /// Tests that self is an alphabetic character
            /// or a decimal digit
            fn is_alphanum(self) -> bool;
            /// Tests that self is a decimal digit
            fn is_dec_digit(self) -> bool;
            /// Tests that self is an hex digit
            fn is_hex_digit(self) -> bool;
            /// Tests that self is an octal digit
            fn is_oct_digit(self) -> bool;
            /// Gets the len in bytes for self
            fn len(self) -> usize;
        }

        impl AsChar for u8
        {
            #[inline] fn as_char(self) -> char { self as char }
            #[inline] fn is_alpha(self) -> bool { (self >= 0x41 && self <= 0x5A) || (self >= 0x61 && self <= 0x7A) }
            #[inline] fn is_alphanum(self) -> bool { self.is_alpha() || self.is_dec_digit() }
            #[inline] fn is_dec_digit(self) -> bool { self >= 0x30 && self <= 0x39 }
            #[inline] fn is_hex_digit(self) -> bool 
            { (self >= 0x30 && self <= 0x39) || (self >= 0x41 && self <= 0x46) || (self >= 0x61 && self <= 0x66) }
            #[inline] fn is_oct_digit(self) -> bool { self >= 0x30 && self <= 0x37 }
            #[inline] fn len(self) -> usize { 1 }
        }

        impl<'a> AsChar for &'a u8
        {
            #[inline] fn as_char(self) -> char { *self as char }
            #[inline] fn is_alpha(self) -> bool { (*self >= 0x41 && *self <= 0x5A) || (*self >= 0x61 && *self <= 0x7A) }
            #[inline] fn is_alphanum(self) -> bool { self.is_alpha() || self.is_dec_digit() }
            #[inline] fn is_dec_digit(self) -> bool { *self >= 0x30 && *self <= 0x39 }
            #[inline] fn is_hex_digit(self) -> bool 
            { (*self >= 0x30 && *self <= 0x39) || (*self >= 0x41 && *self <= 0x46) || (*self >= 0x61 && *self <= 0x66) }
            #[inline] fn is_oct_digit(self) -> bool { *self >= 0x30 && *self <= 0x37 }
            #[inline] fn len(self) -> usize { 1 }
        }

        impl AsChar for char
        {
            #[inline] fn as_char(self) -> char { self }
            #[inline] fn is_alpha(self) -> bool { self.is_ascii_alphabetic() }
            #[inline] fn is_alphanum(self) -> bool { self.is_alpha() || self.is_dec_digit() }
            #[inline] fn is_dec_digit(self) -> bool { self.is_ascii_digit() }
            #[inline] fn is_hex_digit(self) -> bool { self.is_ascii_hexdigit() }
            #[inline] fn is_oct_digit(self) -> bool { self.is_digit(8) }
            #[inline] fn len(self) -> usize { self.len_utf8() }
        }

        impl<'a> AsChar for &'a char
        {
            #[inline] fn as_char(self) -> char { *self }
            #[inline] fn is_alpha(self) -> bool { self.is_ascii_alphabetic() }
            #[inline] fn is_alphanum(self) -> bool { self.is_alpha() || self.is_dec_digit() }
            #[inline] fn is_dec_digit(self) -> bool { self.is_ascii_digit() }
            #[inline] fn is_hex_digit(self) -> bool { self.is_ascii_hexdigit() }
            #[inline] fn is_oct_digit(self) -> bool { self.is_digit(8) }
            #[inline] fn len(self) -> usize { self.len_utf8() }
        }
        /// Abstracts common iteration operations on the input type
        pub trait InputIter
        {
            /// The current input type is a sequence of that `Item` type.
            type Item;
            /// An iterator over the input type, producing the item and its position for use with [Slice].
            type Iter: Iterator<Item = (usize, Self::Item)>;
            /// An iterator over the input type, producing the item
            type IterElem: Iterator<Item = Self::Item>;
            /// Returns an iterator over the elements and their byte offsets
            fn iter_indices(&self) -> Self::Iter;
            /// Returns an iterator over the elements
            fn iter_elements(&self) -> Self::IterElem;
            /// Finds the byte position of the element
            fn position<P>(&self, predicate: P) -> Option<usize> where
            P: Fn(Self::Item) -> bool;
            /// Get the byte offset from the element's position in the stream
            fn slice_index(&self, count: usize) -> Result<usize, Needed>;
        }
        /// Abstracts slicing operations
        pub trait InputTake: Sized 
        {
            /// Returns a slice of `count` bytes. panics if count > length
            fn take(&self, count: usize) -> Self;
            /// Split the stream at the `count` byte offset. panics if count > length
            fn take_split(&self, count: usize) -> (Self, Self);
        }

        impl<'a> InputIter for &'a [u8]
        {
            type Item = u8;
            type Iter = Enumerate<Self::IterElem>;
            type IterElem = Copied<Iter<'a, u8>>;
            #[inline] fn iter_indices(&self) -> Self::Iter { self.iter_elements().enumerate() }
            #[inline] fn iter_elements(&self) -> Self::IterElem { self.iter().copied() }
            #[inline] fn position<P>(&self, predicate: P) -> Option<usize> where
            P: Fn(Self::Item) -> bool
            { self.iter().position(|b| predicate(*b)) }
            #[inline] fn slice_index(&self, count: usize) -> Result<usize, Needed>
            { if self.len() >= count { Ok(count) } else { Err(Needed::new(count - self.len())) } }
        }

        impl<'a> InputTake for &'a [u8]
        {
            #[inline] fn take(&self, count: usize) -> Self { &self[0..count] }
            #[inline] fn take_split(&self, count: usize) -> (Self, Self)
            {
                let (prefix, suffix) = self.split_at(count);
                (suffix, prefix)
            }
        }

        impl<'a> InputIter for &'a str
        {
            type Item = char;
            type Iter = CharIndices<'a>;
            type IterElem = Chars<'a>;
            #[inline] fn iter_indices(&self) -> Self::Iter { self.char_indices() }
            #[inline] fn iter_elements(&self) -> Self::IterElem { self.chars() }
            fn position<P>(&self, predicate: P) -> Option<usize> where
            P: Fn(Self::Item) -> bool
            {
                for (o, c) in self.char_indices()
                {
                    if predicate(c) { return Some(o); }
                }
                None
            }
            #[inline] fn slice_index(&self, count: usize) -> Result<usize, Needed>
            {
                let mut cnt = 0;
                for (index, _) in self.char_indices()
                {
                    if cnt == count { return Ok(index); }
                    cnt += 1;
                }
                if cnt == count { return Ok(self.len()); }
                Err(Needed::Unknown)
            }
        }

        impl<'a> InputTake for &'a str
        {
            #[inline] fn take(&self, count: usize) -> Self { &self[..count] }
            // return byte index
            #[inline] fn take_split(&self, count: usize) -> (Self, Self)
            {
                let (prefix, suffix) = self.split_at(count);
                (suffix, prefix)
            }
        }
        /// Dummy trait used for default implementations (currently only used for `InputTakeAtPosition` and `Compare`).
        pub trait UnspecializedInput {}

        /// Methods to take as much input as possible until the provided function returns true for the current element.
        pub trait InputTakeAtPosition: Sized
        {
            /// The current input type is a sequence of that `Item` type.
            type Item;

            /// Looks for the first element of the input type in which the condition returns true, 
            /// and returns the input up to this position.
            /// *streaming version*: If no element is found matching the condition, this will return `Incomplete`
            fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where 
            P: Fn(Self::Item) -> bool;

            /// Looks for the first element of the input type for which the condition returns true
            /// and returns the input up to this position.
            ///
            /// Fails if the produced slice is empty.
            ///
            /// *streaming version*: If no element is found matching the condition, this will return `Incomplete`
            fn split_at_position1<P, E: ParseError<Self>> ( &self, predicate: P, e: ErrorKind, ) -> IResult<Self, Self, E> 
            where P: Fn(Self::Item) -> bool;

            /// Looks for the first element of the input type for which the condition returns true,
            /// and returns the input up to this position.
            ///
            /// *complete version*: If no element is found matching the condition, this will return the whole input
            fn split_at_position_complete<P, E: ParseError<Self>>( &self, predicate: P, ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool;

            /// Looks for the first element of the input type for which the condition returns true
            /// and returns the input up to this position.
            ///
            /// Fails if the produced slice is empty.
            ///
            /// *complete version*: If no element is found matching the condition, this will return the whole input
            fn split_at_position1_complete<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind, ) 
            -> IResult<Self, Self, E> where
            P: Fn(Self::Item) -> bool;
        }

        impl<Type: InputLength + InputIter + InputTake + Clone + UnspecializedInput> InputTakeAtPosition for Type
        {
            type Item = <Type as InputIter>::Item;

            fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool,
            {
                match self.position(predicate)
                {
                    Some(n) => Ok(self.take_split(n)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position1<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind, ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool,
            {
                match self.position(predicate)
                {
                    Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
                    Some(n) => Ok(self.take_split(n)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position_complete<P, E: ParseError<Self>>( &self, predicate: P, ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool,
            {
                match self.split_at_position(predicate)
                {
                    Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
                    res => res,
                }
            }

            fn split_at_position1_complete<P, E: ParseError<Self>>
            ( &self, predicate: P, e: ErrorKind ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool,
            {
                match self.split_at_position1(predicate, e)
                {
                    Err(Err::Incomplete(_)) =>
                    {
                        if self.input_len() == 0 { Err(Err::Error(E::from_error_kind(self.clone(), e))) } 
                        else { Ok(self.take_split(self.input_len())) }
                    }
                    res => res,
                }
            }
        }

        impl<'a> InputTakeAtPosition for &'a [u8]
        {
            type Item = u8;

            fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where
            P: Fn(Self::Item) -> bool,
            {
                match self.iter().position(|c| predicate(*c))
                {
                    Some(i) => Ok(self.take_split(i)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position1<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool,
            {
                match self.iter().position(|c| predicate(*c))
                {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    Some(i) => Ok(self.take_split(i)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position_complete<P, E: ParseError<Self>>( &self, predicate: P ) -> IResult<Self, Self, E> where
            P: Fn(Self::Item) -> bool,
            {
                match self.iter().position(|c| predicate(*c))
                {
                    Some(i) => Ok(self.take_split(i)),
                    None => Ok(self.take_split(self.input_len())),
                }
            }

            fn split_at_position1_complete<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind ) 
            -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool
            {
                match self.iter().position(|c| predicate(*c))
                {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    Some(i) => Ok(self.take_split(i)),
                    None => 
                    {
                        if self.is_empty() { Err(Err::Error(E::from_error_kind(self, e))) } 
                        else { Ok(self.take_split(self.input_len())) }
                    }
                }
            }
        }

        impl<'a> InputTakeAtPosition for &'a str
        {
            type Item = char;
            fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> 
            where P: Fn(Self::Item) -> bool,
            {
                match self.find(predicate)
                {
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position1<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind ) -> IResult<Self, Self, E> 
            where P: Fn(Self::Item) -> bool
            {
                match self.find(predicate)
                {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => Err(Err::Incomplete(Needed::new(1))),
                }
            }

            fn split_at_position_complete<P, E: ParseError<Self>>( &self, predicate: P ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool
            {
                match self.find(predicate)
                {
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => unsafe
                    {
                        Ok
                        ((
                            self.get_unchecked(self.len()..),
                            self.get_unchecked(..self.len()),
                        ))
                    }
                }
            }

            fn split_at_position1_complete<P, E: ParseError<Self>>( &self, predicate: P, e: ErrorKind ) -> IResult<Self, Self, E>
            where P: Fn(Self::Item) -> bool
            {
                unsafe
                {
                    match self.find(predicate)
                    {
                        Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                        Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                        None =>
                        {
                            if self.is_empty(){ Err(Err::Error(E::from_error_kind(self, e))) } 
                            else { Ok(( self.get_unchecked(self.len()..), self.get_unchecked(..self.len()) )) }
                        }
                    }
                }
            }
        }

        /// Indicates whether a comparison was successful, an error, or if more data was needed
        #[derive(Debug, PartialEq)]
        pub enum CompareResult
        {
            /// Comparison was successful
            Ok,
            /// We need more data to be sure
            Incomplete,
            /// Comparison failed
            Error
        }

        /// Abstracts comparison operations
        pub trait Compare<T>
        {
            /// Compares self to another value for equality
            fn compare(&self, t: T) -> CompareResult;
            /// Compares self to another value for equality independently of the case.
            fn compare_no_case(&self, t: T) -> CompareResult;
        }

        impl<'a, 'b> Compare<&'b [u8]> for &'a [u8] 
        {
            #[inline(always)] fn compare(&self, t: &'b [u8]) -> CompareResult
            {
                let pos = self.iter().zip(t.iter()).position(|(a, b)| a != b);
                match pos
                {
                    Some(_) => CompareResult::Error,
                    None => { if self.len() >= t.len() { CompareResult::Ok } else { CompareResult::Incomplete } }
                }
            }

            #[inline(always)] fn compare_no_case(&self, t: &'b [u8]) -> CompareResult
            {
                if self.iter().zip(t).any(|(a, b)| lowercase_byte(*a) != lowercase_byte(*b)) { CompareResult::Error } 
                else if self.len() < t.len(){ CompareResult::Incomplete } 
                else { CompareResult::Ok }
            }
        }

        impl
        <
        T: InputLength + InputIter<Item = u8> + InputTake + UnspecializedInput, 
        O: InputLength + InputIter<Item = u8> + InputTake,
        > Compare<O> for T
        {
            #[inline(always)] fn compare(&self, t: O) -> CompareResult
            {
                let pos = self.iter_elements().zip(t.iter_elements()).position(|(a, b)| a != b);
                match pos
                {
                    Some(_) => CompareResult::Error,
                    None =>
                    {
                        if self.input_len() >= t.input_len(){ CompareResult::Ok } else { CompareResult::Incomplete }
                    }
                }
            }

            #[inline(always)] fn compare_no_case(&self, t: O) -> CompareResult
            {
                if self.iter_elements().zip(t.iter_elements()).any(|(a, b)| lowercase_byte(a) != lowercase_byte(b))
                { CompareResult::Error } 
                else if self.input_len() < t.input_len() { CompareResult::Incomplete } 
                else { CompareResult::Ok }
            }
        }

        impl<'a, 'b> Compare<&'b str> for &'a [u8]
        {
            #[inline(always)] fn compare(&self, t: &'b str) -> CompareResult { self.compare(AsBytes::as_bytes(t)) }
            #[inline(always)] fn compare_no_case(&self, t: &'b str) -> CompareResult 
            { self.compare_no_case(AsBytes::as_bytes(t)) }
        }

        impl<'a, 'b> Compare<&'b str> for &'a str
        {
            #[inline(always)]
            fn compare(&self, t: &'b str) -> CompareResult {
                self.as_bytes().compare(t.as_bytes())
            }

            //FIXME: this version is too simple and does not use the current locale
            #[inline(always)] fn compare_no_case(&self, t: &'b str) -> CompareResult
            {
                let pos = self.chars().zip(t.chars()).position(|(a, b)| a.to_lowercase().ne(b.to_lowercase()));
                match pos
                {
                    Some(_) => CompareResult::Error,
                    None => { if self.len() >= t.len(){ CompareResult::Ok } else { CompareResult::Incomplete } }
                }
            }
        }

        impl<'a, 'b> Compare<&'b [u8]> for &'a str
        {
            #[inline(always)] fn compare(&self, t: &'b [u8]) -> CompareResult { AsBytes::as_bytes(self).compare(t) }
            #[inline(always)] fn compare_no_case(&self, t: &'b [u8]) -> CompareResult
            { AsBytes::as_bytes(self).compare_no_case(t) }
        }
        /// Look for a token in self
        pub trait FindToken<T> 
        {
            /// Returns true if self contains the token
            fn find_token(&self, token: T) -> bool;
        }

        impl<'a> FindToken<u8> for &'a [u8]
        { fn find_token(&self, token: u8) -> bool { memchr::memchr(token, self).is_some() } }

        impl<'a> FindToken<u8> for &'a str { fn find_token(&self, token: u8) -> bool { self.as_bytes().find_token(token) } }

        impl<'a, 'b> FindToken<&'a u8> for &'b [u8]{ fn find_token(&self, token: &u8) -> bool { self.find_token(*token) } }

        impl<'a, 'b> FindToken<&'a u8> for &'b str 
        { fn find_token(&self, token: &u8) -> bool { self.as_bytes().find_token(token) } }

        impl<'a> FindToken<char> for &'a [u8] 
        { fn find_token(&self, token: char) -> bool { self.iter().any(|i| *i == token as u8) } }

        impl<'a> FindToken<char> for &'a str
        { fn find_token(&self, token: char) -> bool { self.chars().any(|i| i == token) } }

        impl<'a> FindToken<char> for &'a [char]
        { fn find_token(&self, token: char) -> bool { self.iter().any(|i| *i == token) } }

        impl<'a, 'b> FindToken<&'a char> for &'b [char] 
        { fn find_token(&self, token: &char) -> bool { self.find_token(*token) } }

        /// Look for a substring in self
        pub trait FindSubstring<T>
        {
            /// Returns the byte position of the substring if it is found
            fn find_substring(&self, substr: T) -> Option<usize>;
        }

        impl<'a, 'b> FindSubstring<&'b [u8]> for &'a [u8]
        {
            fn find_substring(&self, substr: &'b [u8]) -> Option<usize>
            {
                if substr.len() > self.len() { return None; }

                let (&substr_first, substr_rest) = match substr.split_first()
                {
                    Some(split) => split,
                    None => return Some(0),
                };
                if substr_rest.is_empty() { return memchr::memchr(substr_first, self); }

                let mut offset = 0;
                let haystack = &self[..self.len() - substr_rest.len()];
                while let Some(position) = memchr::memchr(substr_first, &haystack[offset..])
                {
                    offset += position;
                    let next_offset = offset + 1;
                    if &self[next_offset..][..substr_rest.len()] == substr_rest { return Some(offset); }
                    offset = next_offset;
                }

                None
            }
        }

        impl<'a, 'b> FindSubstring<&'b str> for &'a [u8] 
        { fn find_substring(&self, substr: &'b str) -> Option<usize> { self.find_substring(AsBytes::as_bytes(substr)) } }

        impl<'a, 'b> FindSubstring<&'b str> for &'a str 
        { fn find_substring(&self, substr: &'b str) -> Option<usize> { self.find(substr) } }

        /// Used to integrate `str`'s `parse()` method
        pub trait ParseTo<R>
        {
            /// Succeeds if `parse()` succeeded. 
            /// The byte slice implementation will first convert it to a `&str`, then apply the `parse()` function.
            fn parse_to(&self) -> Option<R>;
        }

        impl<'a, R: FromStr> ParseTo<R> for &'a [u8] 
        { fn parse_to(&self) -> Option<R> { from_utf8(self).ok().and_then(|s| s.parse().ok()) } }

        impl<'a, R: FromStr> ParseTo<R> for &'a str { fn parse_to(&self) -> Option<R> { self.parse().ok() } }

        /// Slicing operations using ranges.
        pub trait Slice<R>
        {
            /// Slices self according to the range argument
            fn slice(&self, range: R) -> Self;
        }

        slice_ranges_impl! {str}
        slice_ranges_impl! {[T]}
        array_impls! { 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 }

        /// Abstracts something which can extend an `Extend`.
        pub trait ExtendInto
        {
            /// The current input type is a sequence of that `Item` type.
            type Item;
            /// The type that will be produced
            type Extender;
            /// Create a new `Extend` of the correct type
            fn new_builder(&self) -> Self::Extender;
            /// Accumulate the input into an accumulator
            fn extend_into(&self, acc: &mut Self::Extender);
        }
        
        impl ExtendInto for [u8]
        {
            type Item = u8;
            type Extender = Vec<u8>;
            #[inline] fn new_builder(&self) -> Vec<u8> { Vec::new() }
            #[inline] fn extend_into(&self, acc: &mut Vec<u8>) { acc.extend(self.iter().cloned()); }
        }
        
        impl ExtendInto for &[u8]
        {
            type Item = u8;
            type Extender = Vec<u8>;
            #[inline] fn new_builder(&self) -> Vec<u8> { Vec::new() }
            #[inline] fn extend_into(&self, acc: &mut Vec<u8>) { acc.extend_from_slice(self); }
        }
        
        impl ExtendInto for str
        {
            type Item = char;
            type Extender = String;
            #[inline] fn new_builder(&self) -> String { String::new() }
            #[inline] fn extend_into(&self, acc: &mut String) { acc.push_str(self); }
        }
        
        impl ExtendInto for &str
        {
            type Item = char;
            type Extender = String;
            #[inline] fn new_builder(&self) -> String { String::new() }
            #[inline] fn extend_into(&self, acc: &mut String) { acc.push_str(self); }
        }
        
        impl ExtendInto for char
        {
            type Item = char;
            type Extender = String;
            #[inline] fn new_builder(&self) -> String { String::new() }
            #[inline] fn extend_into(&self, acc: &mut String) { acc.push(*self); }
        }
        /// Helper trait to convert numbers to usize.
        pub trait ToUsize
        {
            /// converts self to usize
            fn to_usize(&self) -> usize;
        }

        impl ToUsize for u8 { #[inline] fn to_usize(&self) -> usize { *self as usize } }

        impl ToUsize for u16 { #[inline] fn to_usize(&self) -> usize { *self as usize } }

        impl ToUsize for usize { #[inline] fn to_usize(&self) -> usize { *self } }
        
        impl ToUsize for u64 { #[inline] fn to_usize(&self) -> usize { *self as usize } }
        /// Equivalent From implementation to avoid orphan rules in bits parsers
        pub trait ErrorConvert<E>
        {
            /// Transform to another error type
            fn convert(self) -> E;
        }

        impl<I> ErrorConvert<(I, ErrorKind)> for ((I, usize), ErrorKind)
        { 
            fn convert(self) -> (I, ErrorKind) { ((self.0).0, self.1) } 
        }

        impl<I> ErrorConvert<((I, usize), ErrorKind)> for (I, ErrorKind)
        { 
            fn convert(self) -> ((I, usize), ErrorKind) { ((self.0, 0), self.1) }
        }
        
        impl<I> ErrorConvert<error::Error<I>> for error::Error<(I, usize)>
        {
            fn convert(self) -> error::Error<I>
            {
                error::Error
                {
                    input: self.input.0,
                    code: self.code,
                }
            }
        }

        impl<I> ErrorConvert<error::Error<(I, usize)>> for error::Error<I>
        {
            fn convert(self) -> error::Error<(I, usize)>
            {
                error::Error
                {
                    input: (self.input, 0),
                    code: self.code,
                }
            }
        }
        
        impl<I> ErrorConvert<error::VerboseError<I>> for error::VerboseError<(I, usize)>
        {
            fn convert(self) -> error::VerboseError<I>
            {
                error::VerboseError
                {
                    errors: self.errors.into_iter().map(|(i, e)| (i.0, e)).collect(),
                }
            }
        }
        
        impl<I> ErrorConvert<error::VerboseError<(I, usize)>> for error::VerboseError<I>
        {
            fn convert(self) -> error::VerboseError<(I, usize)>
            {
                error::VerboseError
                {
                    errors: self.errors.into_iter().map(|(i, e)| ((i, 0), e)).collect(),
                }
            }
        }

        impl ErrorConvert<()> for () { fn convert(self) {} }

        /// Helper trait to show a byte slice as a hex dump
        pub trait HexDisplay
        {
            /// Converts the value of `self` to a hex dump, returning the owned `String`.
            fn to_hex(&self, chunk_size: usize) -> String;

            /// Converts the value of `self` to a hex dump beginning at `from` address, returning the owned `String`.
            fn to_hex_from(&self, chunk_size: usize, from: usize) -> String;
        }
        
        impl HexDisplay for [u8]
        {
            fn to_hex(&self, chunk_size: usize) -> String { self.to_hex_from(chunk_size, 0) }
            fn to_hex_from(&self, chunk_size: usize, from: usize) -> String
            {
                let mut v = Vec::with_capacity(self.len() * 3);
                let mut i = from;
                for chunk in self.chunks(chunk_size)
                {
                    let s = format!("{:08x}", i);
                    for &ch in s.as_bytes().iter()
                    { v.push(ch); }
                    
                    v.push(b'\t');
                    i += chunk_size;
                    for &byte in chunk
                    {
                        v.push(CHARS[(byte >> 4) as usize]);
                        v.push(CHARS[(byte & 0xf) as usize]);
                        v.push(b' ');
                    }
                    
                    if chunk_size > chunk.len()
                    {
                        for j in 0..(chunk_size - chunk.len())
                        {
                            v.push(b' ');
                            v.push(b' ');
                            v.push(b' ');
                        }
                    }                    
                    
                    v.push(b'\t');

                    for &byte in chunk
                    {
                        if (byte >= 32 && byte <= 126) || byte >= 128  { v.push(byte); } else { v.push(b'.'); }
                    }

                    v.push(b'\n');
                }

                String::from_utf8_lossy(&v[..]).into_owned()
            }
        }
        
        impl HexDisplay for str
        {
            fn to_hex(&self, chunk_size: usize) -> String { self.to_hex_from(chunk_size, 0) }
            fn to_hex_from(&self, chunk_size: usize, from: usize) -> String { self.as_bytes().to_hex_from(chunk_size, from) }
        }
        
        fn lowercase_byte(c: u8) -> u8
        {
            match c
            {
                b'A'..=b'Z' => c - b'A' + b'a',
                _ => c,
            }
        }
    }
    /*
    */
    pub mod bits
    {
        //! Bit level parsers
        use ::
        {
            nom::
            {
                error::{ ErrorKind, ParseError },
                internal::{ Err, IResult, Needed, Parser },
                traits::{ ErrorConvert },
                Input,
            },
            *,
        };

        pub mod complete
        {
            //! Bit level parsers
            use ::
            {
                nom::
                {
                    error::{ ErrorKind, ParseError },
                    internal::{ Err, IResult },
                    traits::{ Input, ToUsize },
                },
                ops::{ AddAssign, Div, Shl, Shr },
                *,
            };
            /// Generates a parser taking `count` bits.
            pub fn take<I, O, C, E: ParseError<(I, usize)>>( count: C ) -> 
            impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
            I: Input<Item = u8>,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
            {
                let count = count.to_usize();
                move |(input, bit_offset): (I, usize)|
                {
                    if count == 0 { Ok((( input, bit_offset ), 0u8.into() )) } 
                    else if input.input_len() * 8 < count + bit_offset 
                    {
                        Err(Err::Error(E::from_error_kind
                        (
                            (input, bit_offset),
                            ErrorKind::Eof,
                        )))
                    }
                    else
                    {
                        let cnt = (count + bit_offset).div(8);
                        let mut acc: O = 0_u8.into();
                        let mut offset: usize = bit_offset;
                        let mut remaining: usize = count;
                        let mut end_offset: usize = 0;
                        
                        for byte in input.iter_elements().take(cnt + 1)
                        {
                            if remaining == 0 { break; }

                            let val: O = if offset == 0 { byte.into() }
                            else { ( ( byte << offset) >> offset ).into() };

                            if remaining < 8 - offset
                            {
                                acc += val >> (8 - offset - remaining);
                                end_offset = remaining + offset;
                                break;
                            }
                            else
                            {
                                acc += val << (remaining - (8 - offset));
                                remaining -= 8 - offset;
                                offset = 0;
                            }
                        }

                        Ok(((input.take_from(cnt), end_offset), acc))
                    }
                }
            }
            /// Generates a parser taking `count` bits and comparing them to `pattern`
            pub fn tag<I, O, C, E: ParseError<(I, usize)>>( pattern: O, count: C ) -> 
            impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
            I: Input<Item = u8> + Clone,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O> + PartialEq,
            {
                let count = count.to_usize();
                move |input: (I, usize)|
                {
                    let inp = input.clone();
                    take(count)(input).and_then(|(i, o)|
                    {
                        if pattern == o{ Ok((i, o)) }
                        else { Err(Err::Error(error_position!(inp, ErrorKind::TagBits))) }
                    })
                }
            }
            /// Parses one specific bit as a bool.
            pub fn bool<I, E: ParseError<(I, usize)>>(input:(I, usize)) -> IResult<(I, usize), bool, E> where
            I: Input<Item = u8>,
            {
                let (res, bit): (_, u32) = take(1usize)(input)?;
                Ok((res, bit != 0))
            }
        }

        pub mod streaming
        {
            //! Bit level parsers
            use::
            {
                nom::
                {
                    error::{ ErrorKind, ParseError },
                    internal::{ Err, IResult, Needed },
                    traits::{ Input, ToUsize },
                },
                ops::{ AddAssign, Div, Shl, Shr },
                *,
            };
            /// Generates a parser taking `count` bits
            pub fn take<I, O, C, E: ParseError<(I, usize)>>( count:C ) -> 
            impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
            I: Input<Item = u8>,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
            {
                let c = count.to_usize();
                move |(input, bit_offset): (I, usize)|
                {
                    if c == 0 { Ok(((input, bit_offset), 0u8.into())) } 
                    else
                    {
                        let cnt = (c + bit_offset).div(8);
                        if input.input_len() * 8 < c + bit_offset { Err(Err::Incomplete(Needed::new(c))) } 
                        else
                        {
                            let mut acc: O = 0_u8.into();
                            let mut offset: usize = bit_offset;
                            let mut remaining: usize = c;
                            let mut end_offset: usize = 0;

                            for byte in input.iter_elements().take(cnt + 1)
                            {
                                if remaining == 0 { break; }
                                let val: O = if offset == 0 { byte.into() }
                                else { ((byte << offset) >> offset).into() };

                                if remaining < 8 - offset
                                {
                                    acc += val >> (8 - offset - remaining);
                                    end_offset = remaining + offset;
                                    break;
                                }
                                
                                else
                                {
                                    acc += val << (remaining - (8 - offset));
                                    remaining -= 8 - offset;
                                    offset = 0;
                                }
                            }

                            Ok( ( ( input.take_from(cnt), end_offset ), acc ) )
                        }
                    }
                }
            }
            /// Generates a parser taking `count` bits and comparing them to `pattern`
            pub fn tag<I, O, C, E: ParseError<(I, usize)>>( pattern:O, count:C ) -> 
            impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
            I: Input<Item = u8> + Clone,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O> + PartialEq,
            {
                let count = count.to_usize();
                move |input: (I, usize)|
                {
                    let inp = input.clone();
                    take(count)(input).and_then(|(i, o)|
                    {
                        if pattern == o { Ok((i, o)) }
                        else { Err(Err::Error(error_position!(inp, ErrorKind::TagBits))) }
                    })
                }
            }
            /// Parses one specific bit as a bool.
            pub fn bool<I, E: ParseError<(I, usize)>>(input:(I, usize)) -> IResult<(I, usize), bool, E> where
            I: Input<Item = u8>,
            {
                let (res, bit): (_, u32) = take(1usize)(input)?;
                Ok((res, bit != 0))
            }
        }
        /// Converts a byte-level input to a bit-level input, for consumption by a parser that uses bits.
        pub fn bits<I, O, E1, E2, P>(mut parser: P) -> impl FnMut(I) -> IResult<I, O, E2> where
        E1: ParseError<(I, usize)> + ErrorConvert<E2>,
        E2: ParseError<I>,
        I: Input,
        P: Parser<(I, usize), Output = O, Error = E1>,
        {
            move |input: I| match parser.parse((input, 0))
            {
                Ok(((rest, offset), result)) =>
                {
                    let remaining_bytes_index = offset / 8 + if offset % 8 == 0 { 0 } else { 1 };
                    Ok((rest.take_from(remaining_bytes_index), result))
                }
                Err(Err::Incomplete(n)) => Err(Err::Incomplete(n.map(|u| u.get() / 8 + 1))),
                Err(Err::Error(e)) => Err(Err::Error(e.convert())),
                Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
            }
        }
        /// Counterpart to `bits`, `bytes` transforms its bit stream input into a byte slice for 
        /// the underlying parser, allowing byte-slice parsers to work on bit streams.
        pub fn bytes<I, O, E1, E2, P>( mut p:P ) -> impl FnMut((I, usize)) -> IResult<(I, usize), O, E2> where
        E1: ParseError<I> + ErrorConvert<E2>,
        E2: ParseError<(I, usize)>,
        I: Input + Clone,
        P: Parser<I, Output = O, Error = E1>,
        {
            move |(input, offset): (I, usize)|
            {
                let inner = if offset % 8 != 0 { input.take_from(1 + offset / 8) } 
                else { input.take_from(offset / 8) };
                let i = (input, offset);
                match p.parse(inner)
                {
                    Ok((rest, res)) => Ok(((rest, 0), res)),
                    Err(Err::Incomplete(Needed::Unknown)) => Err(Err::Incomplete(Needed::Unknown)),
                    Err(Err::Incomplete(Needed::Size(sz))) => Err(match sz.get().checked_mul(8)
                    {
                        Some(v) => Err::Incomplete(Needed::new(v)),
                        None => Err::Failure(E2::from_error_kind(i, ErrorKind::TooLarge)),
                    }),
                    Err(Err::Error(e)) => Err(Err::Error(e.convert())),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
                }
            }
        }
    }
    /*
    */
    pub mod bytes
    {
        //! Parsers recognizing bytes streams
        use ::
        {
            marker::{ PhantomData },
            nom::
            {
                error::{ ErrorKind, ParseError },
                internal::{ Err, Needed, Parser },
                traits::{ Compare, CompareResult },
                AsChar, Check, ExtendInto, FindSubstring, FindToken, Input, IsStreaming, Mode, OutputM, 
                OutputMode, ToUsize
            },
            result::Result::{ * },
            *,
        };

        pub mod complete
        {
            //! Parsers recognizing bytes streams, complete input version
            use ::
            {
                nom::
                {
                    error::{ ParseError },
                    internal::{ IResult, Parser },
                    traits::{ Compare, FindSubstring, FindToken, ToUsize },
                    Complete, Emit, Input, OutputM,
                },
                marker::{ PhantomData },
                *,
            };
            /// Recognizes a pattern.
            pub fn tag<T, I, Error: ParseError<I>>(tag: T) -> impl Fn(I) -> IResult<I, I, Error> where
            I: Input + Compare<T>,
            T: Input + Clone
            {
                move |i: I|
                {
                    let mut parser = super::Tag
                    {
                        tag: tag.clone(),
                        e: PhantomData,
                    };

                    parser.process::<OutputM<Emit, Emit, Complete>>(i)
                }
            }
            /// Recognizes a case insensitive pattern.
            pub fn tag_no_case<T, I, Error: ParseError<I>>(tag: T) -> impl Fn(I) -> IResult<I, I, Error> where
            I: Input + Compare<T>,
            T: Input + Clone
            {
                move |i: I|
                {
                    let mut parser = super::TagNoCase
                    {
                        tag: tag.clone(),
                        e: PhantomData,
                    };

                    parser.process::<OutputM<Emit, Emit, Complete>>(i)
                }
            }
            /// Parse till certain characters are met.
            pub fn is_not<T, I, Error: ParseError<I>>(arr: T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            T: FindToken<<I as Input>::Item>
            {
                let mut parser = super::is_not(arr);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest input slice (at least 1) that matches the pattern.
            pub fn is_a<T, I, Error: ParseError<I>>(arr: T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            T: FindToken<<I as Input>::Item>
            {
                let mut parser = super::is_a(arr);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest input slice (if any) that matches the predicate.
            pub fn take_while<F, I, Error: ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest (at least 1) input slice that matches the predicate.
            pub fn take_while1<F, I, Error:ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while1(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest (m <= len <= n) input slice that matches the predicate.
            pub fn take_while_m_n<F, I, Error: ParseError<I>>
            (
                m:usize,
                n:usize,
                cond:F
            ) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while_m_n(m, n, cond);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest input slice (if any) till a predicate is met.
            pub fn take_till<F, I, Error: ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I:Input,
            F:Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_till(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the longest (at least 1) input slice till a predicate is met.
            pub fn take_till1<F, I, Error: ParseError<I>>(cond: F) -> 
            impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_till1(cond);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns an input slice containing the first N input elements (Input[..N]).
            pub fn take<C, I, Error: ParseError<I>>(count: C) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            C: ToUsize,
            {
                let mut parser = super::take(count);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the input slice up to the first occurrence of the pattern.
            pub fn take_until<T, I, Error: ParseError<I>>(t:T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input + FindSubstring<T>,
            T: Input + Clone,
            {
                let mut parser = super::take_until(t);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Returns the non empty input slice up to the first occurrence of the pattern.
            pub fn take_until1<T, I, Error: ParseError<I>>(t:T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input + FindSubstring<T>,
            T: Input + Clone,
            {
                let mut parser = super::take_until1(t);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Matches a byte string with escaped characters.
            pub fn escaped<'a, I, Error, F, G>( normal:F, control_char:char, escapable:G ) -> 
            impl FnMut(I) -> IResult<I, I, Error> where
            I: Clone + crate::nom::traits::Offset + Input + 'a,
            <I as Input>::Item: crate::nom::traits::AsChar,
            F: Parser<I, Error = Error>,
            G: Parser<I, Error = Error>,
            Error: ParseError<I>,
            {
                let mut parser = super::escaped(normal, control_char, escapable);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Matches a byte string with escaped characters.
            pub fn escaped_transform<I, Error, F, G, O1, O2, ExtendItem, Output>
            ( normal:F, control_char:char, transform:G ) -> 
            impl FnMut(I) -> IResult<I, Output, Error> where
            I: Clone + crate::nom::traits::Offset + Input,
            I: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O1: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O2: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            <I as Input>::Item: crate::nom::traits::AsChar,
            F: Parser<I, Output = O1, Error = Error>,
            G: Parser<I, Output = O2, Error = Error>,
            Error: ParseError<I>,
            {
                let mut parser = super::escaped_transform(normal, control_char, transform);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
        }

        pub mod streaming
        {
            //! Parsers recognizing bytes streams, streaming version
            use ::
            {
                marker::{ PhantomData },
                nom::
                {
                    error::{ ParseError },
                    internal::{IResult, Parser},
                    traits::{ Compare, FindSubstring, FindToken, ToUsize },
                    Emit, Input, OutputM, Streaming,
                },
                *,
            };
            /// Recognizes a pattern.
            pub fn tag<T, I, Error: ParseError<I>>(tag: T) -> impl Fn(I) -> IResult<I, I, Error> where
            I: Input + Compare<T>,
            T: Input + Clone
            {
                move |i: I|
                {
                    let mut parser = super::Tag
                    {
                        tag: tag.clone(),
                        e: PhantomData,
                    };
                    parser.process::<OutputM<Emit, Emit, Streaming>>(i)
                }
            }
            /// Recognizes a case insensitive pattern.
            pub fn tag_no_case<T, I, Error: ParseError<I>>(tag: T) -> 
            impl Fn(I) -> IResult<I, I, Error> where
            I: Input + Compare<T>,
            T: Input + Clone
            {
                move |i: I|
                {
                    let mut parser = super::TagNoCase
                    {
                        tag: tag.clone(),
                        e: PhantomData,
                    };
                    parser.process::<OutputM<Emit, Emit, Streaming>>(i)
                }
            }
            /// Parse till certain characters are met.
            pub fn is_not<T, I, Error: ParseError<I>>(arr: T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            T: FindToken<<I as Input>::Item>
            {
                let mut parser = super::is_not(arr);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest input slice (at least 1) that matches the pattern.
            pub fn is_a<T, I, Error: ParseError<I>>(arr: T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            T: FindToken<<I as Input>::Item>
            {
                let mut parser = super::is_a(arr);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest input slice (if any) that matches the predicate.
            pub fn take_while<F, I, Error: ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest (at least 1) input slice that matches the predicate.
            pub fn take_while1<F, I, Error:ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while1(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
            pub fn take_while_m_n<F, I, Error: ParseError<I>>( m:usize, n:usize, cond:F ) -> 
            impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_while_m_n(m, n, cond);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest input slice (if any) till a predicate is met.
            pub fn take_till<F, I, Error: ParseError<I>>(cond: F) -> 
            impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_till(cond);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the longest (at least 1) input slice till a predicate is met.
            pub fn take_till1<F, I, Error: ParseError<I>>(cx:F) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            F: Fn(<I as Input>::Item) -> bool
            {
                let mut parser = super::take_till1(cx);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns an input slice containing the first N input elements (Input[..N]).
            pub fn take<C, I, Error: ParseError<I>>(count: C) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input,
            C: ToUsize
            {
                let mut parser = super::take(count);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the input slice up to the first occurrence of the pattern.
            pub fn take_until<T, I, Error: ParseError<I>>(t:T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input + FindSubstring<T>,
            T: Clone,
            {
                let mut parser = super::take_until(t);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Returns the non empty input slice up to the first occurrence of the pattern.
            pub fn take_until1<T, I, Error: ParseError<I>>(t:T) -> impl FnMut(I) -> IResult<I, I, Error> where
            I: Input + FindSubstring<T>,
            T: Clone
            {
                let mut parser = super::take_until1(t);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Matches a byte string with escaped characters.
            pub fn escaped<I, Error, F, G>( normal:F, control_char:char, escapable:G ) -> 
            impl FnMut(I) -> IResult<I, I, Error> where
            I: Input + Clone + crate::nom::traits::Offset,
            <I as Input>::Item: crate::nom::traits::AsChar,
            F: Parser<I, Error = Error>,
            G: Parser<I, Error = Error>,
            Error: ParseError<I>
            {
                let mut parser = super::escaped(normal, control_char, escapable);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Matches a byte string with escaped characters.
            pub fn escaped_transform<I, Error, F, G, O1, O2, ExtendItem, Output>
            ( normal:F, control_char:char, transform:G ) -> 
            impl FnMut(I) -> IResult<I, Output, Error> where
            I: Clone + crate::nom::traits::Offset + Input,
            I: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O1: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O2: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            <I as Input>::Item: crate::nom::traits::AsChar,
            F: Parser<I, Output = O1, Error = Error>,
            G: Parser<I, Output = O2, Error = Error>,
            Error: ParseError<I>
            {
                let mut parser = super::escaped_transform(normal, control_char, transform);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
        }
        /// Recognizes a pattern.
        pub fn tag<T, I, Error: ParseError<I>>( tag:T ) -> impl Parser<I, Output = I, Error = Error> where
        I: Input + Compare<T>,
        T: Input + Clone
        {
            Tag
            {
                tag,
                e:PhantomData,
            }
        }
        /// Tag implementation
        pub struct Tag<T, E>
        {
            tag: T,
            e:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, T> Parser<I> for Tag<T, Error> where
        I: Input + Compare<T>,
        T: Input + Clone
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>( &mut self, i:I ) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let tag_len = self.tag.input_len();
                let t = self.tag.clone();
                match i.compare(t)
                {
                    CompareResult::Ok => Ok((i.take_from(tag_len), OM::Output::bind(|| i.take(tag_len)))),
                    CompareResult::Incomplete =>
                    {
                        if OM::Incomplete::is_streaming() 
                        {
                            Err(Err::Incomplete(Needed::new(tag_len - i.input_len())))
                        } 
                        
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                let e: ErrorKind = ErrorKind::Tag;
                                Error::from_error_kind(i, e)
                            })))
                        }
                    }

                    CompareResult::Error => Err(Err::Error(OM::Error::bind(||
                    {
                        let e: ErrorKind = ErrorKind::Tag;
                        Error::from_error_kind(i, e)
                    }))),
                }
            }
        }
        /// Recognizes a case insensitive pattern.
        pub fn tag_no_case<T, I, Error:ParseError<I>>(tag: T) -> impl Parser<I, Output = I, Error = Error>
        where
        I: Input + Compare<T>,
        T: Input + Clone
        {
            TagNoCase
            {
                tag,
                e:PhantomData,
            }
        }
        /// Case insensitive Tag implementation
        pub struct TagNoCase<T, E>
        {
            tag:T,
            e:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, T> Parser<I> for TagNoCase<T, Error> where
        I: Input + Compare<T>,
        T: Input + Clone
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let tag_len = self.tag.input_len();
                let t = self.tag.clone();
                match i.compare_no_case(t)
                {
                    CompareResult::Ok => Ok((i.take_from(tag_len), OM::Output::bind(|| i.take(tag_len)))),
                    CompareResult::Incomplete =>
                    {
                        if OM::Incomplete::is_streaming()
                        { Err(Err::Incomplete(Needed::new(tag_len - i.input_len()))) } 
                        
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                let e: ErrorKind = ErrorKind::Tag;
                                Error::from_error_kind(i, e)
                            })))
                        }
                    }
                    
                    CompareResult::Error => Err(Err::Error(OM::Error::bind(||
                    {
                        let e: ErrorKind = ErrorKind::Tag;
                        Error::from_error_kind(i, e)
                    }))),
                }
            }
        }
        /// Parser wrapper for `split_at_position`
        pub struct SplitPosition<F, E>
        {
            predicate:F,
            error:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, F> Parser<I> for SplitPosition<F, Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            type Output = I;
            type Error = Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            { i.split_at_position_mode::<OM, _, _>(|c| (self.predicate)(c)) }
        }
        /// Parser wrapper for `split_at_position1`
        pub struct SplitPosition1<F, E>
        {
            e:ErrorKind,
            predicate:F,
            error:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, F> Parser<I> for SplitPosition1<F, Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool
        {
            type Output = I;
            type Error = Error;
            #[inline(always)] fn process<OM: OutputMode>(&mut self, i: I) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            { i.split_at_position_mode1::<OM, _, _>(|c| (self.predicate)(c), self.e) }
        }
        /// Parse till certain characters are met.
        pub fn is_not<T, I, Error: ParseError<I>>(arr: T) -> impl Parser<I, Output = I, Error = Error> where
        I: Input,
        T: FindToken<<I as Input>::Item>,
        {
            SplitPosition1
            {
                e: ErrorKind::IsNot,
                predicate: move |c| arr.find_token(c),
                error: PhantomData,
            }
        }
        /// Returns the longest input slice (at least 1) that matches the pattern.
        pub fn is_a<T, I, Error: ParseError<I>>(arr: T) -> impl Parser<I, Output = I, Error = Error> where
        I: Input,
        T: FindToken<<I as Input>::Item>,
        {
            SplitPosition1
            {
                e:ErrorKind::IsA,
                predicate: move |c| !arr.find_token(c),
                error:PhantomData,
            }
        }
        /// Returns the longest input slice (if any) that matches the predicate.
        pub fn take_while<F, I, Error: ParseError<I>>(cond: F) -> impl Parser<I, Output = I, Error = Error>
        where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            SplitPosition
            {
                predicate: move |c| !cond(c),
                error: PhantomData,
            }
        }
        /// Returns the longest (at least 1) input slice that matches the predicate.
        pub fn take_while1<F, I, Error: ParseError<I>>(cond: F) -> impl Parser<I, Output = I, Error = Error>
        where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            SplitPosition1
            {
                e: ErrorKind::TakeWhile1,
                predicate: move |c| !cond(c),
                error: PhantomData,
            }
        }
        /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
        pub fn take_while_m_n<F, I, Error: ParseError<I>>( m: usize, n: usize, predicate: F ) -> 
        impl Parser<I, Output = I, Error = Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            TakeWhileMN
            {
                m,
                n,
                predicate,
                e:PhantomData,
            }
        }
        /// Parser implementation for [take_while_m_n]
        pub struct TakeWhileMN<F, E>
        {
            m: usize,
            n: usize,
            predicate: F,
            e:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, F> Parser<I> for TakeWhileMN<F, Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut count = 0;
                for (i, (index, item)) in input.iter_indices().enumerate()
                {
                    if i == self.n
                    {
                        return Ok
                        ((
                            input.take_from(index),
                            OM::Output::bind(|| input.take(index)),
                        ));
                    }

                    if !(self.predicate)(item)
                    {
                        if i >= self.m
                        {
                            return Ok
                            ((
                                input.take_from(index),
                                OM::Output::bind(|| input.take(index)),
                            ));
                        }

                        else
                        {
                            return Err
                            (
                                Err::Error
                                (
                                    OM::Error::bind(|| 
                                    {
                                        Error::from_error_kind(input, ErrorKind::TakeWhileMN) 
                                    })
                                )
                            );
                        }
                    }
                    count += 1;
                }

                let input_len = input.input_len();
                if OM::Incomplete::is_streaming()
                {
                    let needed = if self.m > input_len { self.m - input_len } else { 1 };
                    Err(Err::Incomplete(Needed::new(needed)))
                }

                else if count >= self.m
                {
                    Ok
                    ((
                        input.take_from(input_len),
                        OM::Output::bind(|| input.take(input_len)),
                    ))
                }
                
                else
                {
                    Err(Err::Error(OM::Error::bind(||
                    {
                        Error::from_error_kind(input, ErrorKind::TakeWhileMN)
                    })))
                }
            }
        }
        /// Returns the longest input slice (if any) till a predicate is met.
        pub fn take_till<F, I, Error: ParseError<I>>(cond: F) -> 
        impl Parser<I, Output = I, Error = Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            SplitPosition
            {
                predicate: cond,
                error: PhantomData,
            }
        }
        /// Returns the longest (at least 1) input slice till a predicate is met.
        pub fn take_till1<F, I, Error: ParseError<I>>(cond: F) -> 
        impl Parser<I, Output = I, Error = Error> where
        I: Input,
        F: Fn(<I as Input>::Item) -> bool,
        {
            SplitPosition1
            {
                e: ErrorKind::TakeTill1,
                predicate: cond,
                error: PhantomData,
            }
        }
        /// Returns an input slice containing the first N input elements (Input[..N]).
        pub fn take<C, I, Error: ParseError<I>>(count: C) -> impl Parser<I, Output = I, Error = Error> where
        I: Input,
        C: ToUsize,
        {
            Take
            {
                length: count.to_usize(),
                e: PhantomData,
            }
        }
        /// Parser implementation for [take]
        pub struct Take<E>
        {
            length: usize,
            e: PhantomData<E>,
        }

        impl<I, Error: ParseError<I>> Parser<I> for Take<Error> where
        I: Input,
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match i.slice_index(self.length)
                {
                    Err(needed) =>
                    {
                        if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(needed)) }
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                let e: ErrorKind = ErrorKind::Eof;
                                Error::from_error_kind(i, e)
                            })))
                        }
                    }
                    Ok(index) => Ok((i.take_from(index), OM::Output::bind(|| i.take(index)))),
                }
            }
        }
        /// Returns the input slice up to the first occurrence of the pattern.
        pub fn take_until<T, I, Error: ParseError<I>>(tag: T) -> impl Parser<I, Output = I, Error = Error>
        where
        I: Input + FindSubstring<T>,
        T: Clone,
        {
            TakeUntil
            {
                tag,
                e:PhantomData,
            }
        }
        /// Parser implementation for [take_until]
        pub struct TakeUntil<T, E> 
        {
            tag: T,
            e: PhantomData<E>,
        }

        impl<I, T, Error: ParseError<I>> Parser<I> for TakeUntil<T, Error> where
        I: Input + FindSubstring<T>,
        T: Clone,
        {
            type Output = I;
            type Error = Error;

            fn process<OM: OutputMode>(&mut self, i: I) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match i.find_substring(self.tag.clone())
                {
                    None =>
                    {
                        if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(Needed::Unknown)) } 
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                let e: ErrorKind = ErrorKind::TakeUntil;
                                Error::from_error_kind(i, e)
                            })))
                        }
                    }
                    Some(index) => Ok((i.take_from(index), OM::Output::bind(|| i.take(index)))),
                }
            }
        }
        /// Returns the non empty input slice up to the first occurrence of the pattern.
        pub fn take_until1<T, I, Error: ParseError<I>>(tag: T) -> 
        impl Parser<I, Output = I, Error = Error> where
        I: Input + FindSubstring<T>,
        T: Clone,
        {
            TakeUntil1
            {
                tag,
                e:PhantomData,
            }
        }
        /// Parser implementation for take_until1
        pub struct TakeUntil1<T, E>
        {
            tag:T,
            e:PhantomData<E>,
        }

        impl<I, T, Error: ParseError<I>> Parser<I> for TakeUntil1<T, Error> where
        I: Input + FindSubstring<T>,
        T: Clone,
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>(&mut self, i: I) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match i.find_substring(self.tag.clone())
                {
                    None =>
                    {
                        if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(Needed::Unknown)) } 
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                let e: ErrorKind = ErrorKind::TakeUntil;
                                Error::from_error_kind(i, e)
                            })))
                        }
                    }

                    Some(0) => Err(Err::Error(OM::Error::bind(||
                    {
                        Error::from_error_kind(i, ErrorKind::TakeUntil)
                    }))),

                    Some(index) => Ok((i.take_from(index), OM::Output::bind(|| i.take(index)))),
                }
            }
        }
        /// Matches a byte string with escaped characters.
        pub fn escaped<I, Error, F, G>( normal:F, control_char:char, escapable:G ) -> 
        impl Parser<I, Output = I, Error = Error> where
        I: Input + Clone + crate::nom::traits::Offset,
        <I as Input>::Item: crate::nom::traits::AsChar,
        F: Parser<I, Error = Error>,
        G: Parser<I, Error = Error>,
        Error: ParseError<I>,
        {
            Escaped
            {
                normal,
                escapable,
                control_char,
                e:PhantomData,
            }
        }
        /// Parser implementation for [escaped]
        pub struct Escaped<F, G, E>
        {
            normal: F,
            escapable: G,
            control_char: char,
            e:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>, F, G> Parser<I> for Escaped<F, G, Error> where
        I: Input + Clone + crate::nom::traits::Offset,
        <I as Input>::Item: crate::nom::traits::AsChar,
        F: Parser<I, Error = Error>,
        G: Parser<I, Error = Error>,
        Error: ParseError<I>,
        {
            type Output = I;
            type Error = Error;
            fn process<OM: OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut i = input.clone();

                while i.input_len() > 0
                {
                    let current_len = i.input_len();

                    match self
                    .normal
                    .process::<OutputM<Check, Check, OM::Incomplete>>(i.clone())
                    {
                        Ok((i2, _)) =>
                        {
                            if i2.input_len() == 0
                            {
                                if OM::Incomplete::is_streaming(){ return Err(Err::Incomplete(Needed::Unknown)); } 
                                else
                                {
                                    let index = input.input_len();
                                    return Ok
                                    ((
                                        input.take_from(index),
                                        OM::Output::bind(|| input.take(index)),
                                    ));
                                }
                            }
                            
                            else if i2.input_len() == current_len
                            {
                                let index = input.offset(&i2);
                                return Ok((
                                input.take_from(index),
                                OM::Output::bind(|| input.take(index)),
                                ));
                            } else {
                                i = i2;
                            }
                        }
                        Err(Err::Error(_)) => {
                        // unwrap() should be safe here since index < $i.input_len()
                        if i.iter_elements().next().unwrap().as_char() == self.control_char {
                            let next = self.control_char.len_utf8();
                            if next >= i.input_len() {
                            if OM::Incomplete::is_streaming() {
                                return Err(Err::Incomplete(Needed::new(1)));
                            } else {
                                return Err(Err::Error(OM::Error::bind(|| {
                                Error::from_error_kind(input, ErrorKind::Escaped)
                                })));
                            }
                            } else {
                            match self
                                .escapable
                                .process::<OutputM<Check, OM::Error, OM::Incomplete>>(i.take_from(next))
                            {
                                Ok((i2, _)) => {
                                if i2.input_len() == 0 {
                                    if OM::Incomplete::is_streaming() {
                                    return Err(Err::Incomplete(Needed::Unknown));
                                    } else {
                                    let index = input.input_len();
                                    return Ok((
                                        input.take_from(index),
                                        OM::Output::bind(|| input.take(index)),
                                    ));
                                    }
                                } else {
                                    i = i2;
                                }
                                }
                                Err(e) => return Err(e),
                            }
                            }
                        } else {
                            let index = input.offset(&i);
                            if index == 0 {
                            return Err(Err::Error(OM::Error::bind(|| {
                                Error::from_error_kind(input, ErrorKind::Escaped)
                            })));
                            } else {
                            return Ok((
                                input.take_from(index),
                                OM::Output::bind(|| input.take(index)),
                            ));
                            }
                        }
                        }
                        Err(Err::Failure(e)) => {
                        return Err(Err::Failure(e));
                        }
                        Err(Err::Incomplete(i)) => {
                        return Err(Err::Incomplete(i));
                        }
                    }
                }

                if OM::Incomplete::is_streaming() {
                Err(Err::Incomplete(Needed::Unknown))
                } else {
                let index = input.input_len();
                Ok((
                    input.take_from(index),
                    OM::Output::bind(|| input.take(index)),
                ))
                }
            }
        }
        /// Matches a byte string with escaped characters.
        pub fn escaped_transform<I, Error, F, G, ExtendItem, Output> 
        ( normal: F, control_char:char, transform:G ) -> 
        impl Parser<I, Output = Output, Error = Error> where
        I: Clone + crate::nom::traits::Offset + Input,
        I: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <F as Parser<I>>::Output: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <G as Parser<I>>::Output: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <I as Input>::Item: crate::nom::traits::AsChar,
        F: Parser<I, Error = Error>,
        G: Parser<I, Error = Error>,
        Error: ParseError<I>,
        {
            EscapedTransform
            {
                normal,
                control_char,
                transform,
                e: PhantomData,
                extend: PhantomData,
                o: PhantomData,
            }
        }
        /// Parser implementation for [escaped_transform]
        pub struct EscapedTransform<F, G, E, ExtendItem, Output>
        {
            normal: F,
            transform: G,
            control_char: char,
            e: PhantomData<E>,
            extend: PhantomData<ExtendItem>,
            o: PhantomData<Output>,
        }

        impl<I, Error: ParseError<I>, F, G, ExtendItem, Output> Parser<I>
        for EscapedTransform<F, G, Error, ExtendItem, Output>
        where
        I: Clone + crate::nom::traits::Offset + Input,
        I: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <F as Parser<I>>::Output: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <G as Parser<I>>::Output: crate::nom::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
        <I as Input>::Item: crate::nom::traits::AsChar,
        F: Parser<I, Error = Error>,
        G: Parser<I, Error = Error>,
        Error: ParseError<I>,
        {
            type Output = Output;
            type Error = Error;
            fn process<OM: OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let mut index = 0;
                let mut res = OM::Output::bind(|| input.new_builder());

                while index < input.input_len()
                {
                    let current_len = input.input_len();
                    let remainder = input.take_from(index);

                    match self.normal.process::<OM>(remainder.clone())
                    {
                        Ok((i2, o)) =>
                        {
                            res = OM::Output::combine(o, res, |o, mut res|
                            {
                                o.extend_into(&mut res);
                                res
                            });

                            if i2.input_len() == 0
                            {
                                if OM::Incomplete::is_streaming()
                                { return Err(Err::Incomplete(Needed::Unknown)); }                                 
                                else
                                {
                                    let index = input.input_len();
                                    return Ok((input.take_from(index), res));
                                }
                            }
                            else if i2.input_len() == current_len { return Ok((remainder, res)); } 
                            else { index = input.offset(&i2); }
                        }

                        Err(Err::Error(_)) =>
                        {
                            if remainder.iter_elements().next().unwrap().as_char() == self.control_char
                            {
                                let next = index + self.control_char.len_utf8();
                                let input_len = input.input_len();

                                if next >= input_len {
                                if OM::Incomplete::is_streaming() {
                                    return Err(Err::Incomplete(Needed::Unknown));
                                } else {
                                    return Err(Err::Error(OM::Error::bind(|| {
                                    Error::from_error_kind(remainder, ErrorKind::EscapedTransform)
                                    })));
                                }
                                } else {
                                match self.transform.process::<OM>(input.take_from(next)) {
                                    Ok((i2, o)) => {
                                    res = OM::Output::combine(o, res, |o, mut res| {
                                        o.extend_into(&mut res);
                                        res
                                    });
                                    if i2.input_len() == 0 {
                                        if OM::Incomplete::is_streaming() {
                                        return Err(Err::Incomplete(Needed::Unknown));
                                        } else {
                                        return Ok((input.take_from(input.input_len()), res));
                                        }
                                    } else {
                                        index = input.offset(&i2);
                                    }
                                    }
                                    Err(Err::Error(e)) => return Err(Err::Error(e)),
                                    Err(Err::Failure(e)) => {
                                    return Err(Err::Failure(e));
                                    }
                                    Err(Err::Incomplete(i)) => {
                                    return Err(Err::Incomplete(i));
                                    }
                                }
                                }
                            }
                            
                            else
                            {
                                if index == 0
                                { 
                                    return Err
                                    (
                                        Err::Error(OM::Error::bind(|| {Error::from_error_kind(remainder, ErrorKind::EscapedTransform)}))
                                    ); 
                                }

                                return Ok((remainder, res));
                            }
                        }

                        Err(Err::Failure(e)) => { return Err(Err::Failure(e)); }
                        Err(Err::Incomplete(i)) => { return Err(Err::Incomplete(i)); }
                    }
                }

                if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(Needed::Unknown)) }
                else { Ok((input.take_from(index), res)) }
            }
        }
    }
    /*
    */
    pub mod character
    {
        //! Character specific parsers and combinators.
        use ::
        {
            marker::{ PhantomData },
            nom::
            {
                error::{ ErrorKind, ParseError },
                AsChar, Err, FindToken, IResult, Input, IsStreaming, Mode, Needed, Parser,
            },
            *,
        };

        pub mod complete
        {
            //! Character specific parsers and combinators, complete input version.
            use ::
            {
                nom::
                {
                    branch::{ alt },
                    bytes::complete::{ tag },
                    combinator::{ opt, value },
                    error::{ ErrorKind, ParseError },
                    internal::{ Err, IResult },
                    traits::{ AsChar, Compare, CompareResult, FindToken, Input },
                    Complete, Emit, OutputM, Parser,
                },
                *,
            };
            
            macro_rules! ints
            {
                ($($t:tt)+) =>
                {
                    $(
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E> where
                        T: Input  + Clone,
                        <T as Input>::Item: AsChar,
                        T: for <'a> Compare<&'a[u8]>
                        {
                            let (i, sign) = sign(input.clone())?;
                            if i.input_len() == 0 
                            { return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))); }

                            let mut value: $t = 0;
                            if sign
                            {
                                let mut pos = 0;
                                for c in i.iter_elements()
                                {
                                    match c.as_char().to_digit(10)
                                    {
                                        None =>
                                        {
                                            if pos == 0
                                            {
                                                return Err
                                                (
                                                    Err::Error
                                                    (
                                                        E::from_error_kind(input, ErrorKind::Digit)
                                                    )
                                                );
                                            } 
                                            else { return Ok((i.take_from(pos), value)); }
                                        },

                                        Some(d) => match value.checked_mul(10)
                                        .and_then(|v| v.checked_add(d as $t))
                                        {
                                            None =>
                                            { 
                                                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)))
                                            },
                                            Some(v) =>
                                            {
                                                pos += c.len();
                                                value = v;
                                            },
                                        }
                                    }
                                }
                            } 
                            else
                            {
                                let mut pos = 0;
                                    for c in i.iter_elements() {
                                        match c.as_char().to_digit(10) {
                                            None => {
                                                if pos == 0 {
                                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                                } else {
                                                    return Ok((i.take_from(pos), value));
                                                }
                                            },
                                            Some(d) => match value.checked_mul(10).and_then(|v| v.checked_sub(d as $t)) {
                                                None => return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))),
                                                Some(v) => {
                                                pos += c.len();
                                                value = v;
                                                },
                                            }
                                        }
                                    }
                            }

                            Ok((i.take_from(i.input_len()), value))
                        }
                    )+
                }
            }
            
            macro_rules! uints
            {
                ($($t:tt)+) =>
                {
                    $(
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E> where
                        T: Input ,
                        <T as Input>::Item: AsChar
                        {
                            let i = input;
                            if i.input_len() == 0
                            { return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit))); }

                            let mut value: $t = 0;
                            let mut pos = 0;

                            for c in i.iter_elements()
                            {
                                match c.as_char().to_digit(10)
                                {
                                    None =>
                                    {
                                        if pos == 0 
                                        { return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit))); } 
                                        else { return Ok((i.take_from(pos), value)); }
                                    },

                                    Some(d) => match value.checked_mul(10)
                                    .and_then(|v| v.checked_add(d as $t)) 
                                    {
                                        None =>
                                        {
                                            return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)))
                                        },
                                        Some(v) =>
                                        {
                                            pos += c.len();
                                            value = v;
                                        },
                                    }
                                }
                            }

                            Ok((i.take_from(i.input_len()), value))
                        }
                    )+
                }
            }
            /// Recognizes one character.
            pub fn char<I, Error: ParseError<I>>(c: char) -> impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            {
                let mut parser = super::char(c);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Recognizes one character and checks that it satisfies a predicate.
            pub fn satisfy<F, I, Error: ParseError<I>>(p:F) -> impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            F: Fn(char) -> bool
            {
                let mut parser = super::satisfy(p);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Recognizes one of the provided characters.
            pub fn one_of<I, T, Error: ParseError<I>>(l:T) -> impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            T: FindToken<char>
            {
                let mut parser = super::one_of(l);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Recognizes a character that is not in the provided characters.
            pub fn none_of<I, T, Error: ParseError<I>>(l:T) -> impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            T: FindToken<char>
            {
                let mut parser = super::none_of(l);
                move |i: I| parser.process::<OutputM<Emit, Emit, Complete>>(i)
            }
            /// Recognizes the string "\r\n".
            pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str>
            {
                match input.compare("\r\n")
                {
                    CompareResult::Ok => Ok(input.take_split(2)),
                    _ =>
                    {
                        let e: ErrorKind = ErrorKind::CrLf;
                        Err(Err::Error(E::from_error_kind(input, e)))
                    }
                }
            }
            /// Recognizes a string of any char except '\r\n' or '\n'.
            pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str>,
            <T as Input>::Item: AsChar
            {
                match input.position(|item|
                {
                    let c = item.as_char();
                    c == '\r' || c == '\n'
                })
                {
                    None => Ok(input.take_split(input.input_len())),
                    Some(index) =>
                    {
                        let mut it = input.take_from(index).iter_elements();
                        let nth = it.next().unwrap().as_char();
                        if nth == '\r'
                        {
                            let sliced = input.take_from(index);
                            let comp = sliced.compare("\r\n");
                            match comp
                            {
                                CompareResult::Ok => Ok(input.take_split(index)),
                                _ =>
                                {
                                    let e: ErrorKind = ErrorKind::Tag;
                                    Err(Err::Error(E::from_error_kind(input, e)))
                                }
                            }
                        } 
                        else { Ok(input.take_split(index)) }
                    }
                }
            }
            /// Recognizes an end of line (both '\n' and '\r\n').
            pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str>,
            {
                match input.compare("\n")
                {
                    CompareResult::Ok => Ok(input.take_split(1)),
                    CompareResult::Incomplete => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                    CompareResult::Error => match input.compare("\r\n")
                    {
                        CompareResult::Ok => Ok(input.take_split(2)),
                        _ => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                    },
                }
            }
            /// Matches a newline character '\n'.
            pub fn newline<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            { char('\n')(input) }
            /// Matches a tab character '\t'.
            pub fn tab<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            { char('\t')(input) }
            /// Matches one byte as a character. Note that the input type will
            /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
            pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                let mut it = input.iter_elements();
                match it.next()
                {
                    None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
                    Some(c) => Ok((input.take_from(c.len()), c.as_char())),
                }
            }
            /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
            pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_alpha()) }
            /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
            pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_alpha(), ErrorKind::Alpha) }
            /// Recognizes zero or more ASCII numerical characters: 0-9.
            pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_dec_digit()) }
            /// Recognizes one or more ASCII numerical characters: 0-9.
            pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_dec_digit(), ErrorKind::Digit) }
            /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
            pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_hex_digit()) }
            /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
            pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_hex_digit(), ErrorKind::HexDigit) }
            /// Recognizes zero or more octal characters: 0-7.
            pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_oct_digit()) }
            /// Recognizes one or more octal characters: 0-7.
            pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_oct_digit(), ErrorKind::OctDigit) }
            /// Recognizes zero or more binary characters: 0-1.
            pub fn bin_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_bin_digit()) }
            /// Recognizes one or more binary characters: 0-1.
            pub fn bin_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_bin_digit(), ErrorKind::BinDigit) }
            /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
            pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position_complete(|item| !item.is_alphanum()) }
            /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
            pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1_complete(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric) }
            /// Recognizes zero or more spaces and tabs.
            pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar + Clone
            {
                input.split_at_position_complete(|item| 
                {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t')
                })
            }
            /// Recognizes one or more spaces and tabs.
            pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position1_complete
                (
                    |item|
                    {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    },
                    ErrorKind::Space,
                )
            }
            /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
            pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position_complete(|item|
                {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                })
            }
            /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
            pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position1_complete
                (
                    |item| 
                    {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    },
                    ErrorKind::MultiSpace,
                )
            }

            pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E> where
            T: Clone + Input,
            T: for<'a> Compare<&'a [u8]>
            {
                let (i, opt_sign) = 
                opt(alt((value(false, tag(&b"-"[..])), value(true, tag(&b"+"[..]))))).parse(input)?;
                let sign = opt_sign.unwrap_or(true);
                Ok((i, sign))
            }

            ints! { i8 i16 i32 i64 i128 isize }
            uints! { u8 u16 u32 u64 u128 usize }
        }

        pub mod streaming
        {
            //! Character specific parsers and combinators, streaming version.
            use ::
            {
                nom::
                {
                    bytes::streaming::{ tag },
                    branch::{ alt },
                    combinator::{ opt, value },
                    error::{ ErrorKind, ParseError },
                    internal::{ Err, IResult, Needed },
                    traits::{ AsChar, FindToken, Input },
                    traits::{ Compare, CompareResult },
                    Emit, OutputM, Parser, Streaming,
                },
                *,
            };
            
            macro_rules! ints
            {
                ($($t:tt)+) =>
                {
                    $(
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E> where
                        T: Input +  Clone,
                        <T as Input>::Item: AsChar,
                        T: for <'a> Compare<&'a[u8]>
                        {
                            let (i, sign) = sign(input.clone())?;
                            if i.input_len() == 0 { return Err(Err::Incomplete(Needed::new(1))); }

                            let mut value: $t = 0;
                            if sign
                            {
                                let mut pos = 0;
                                for c in i.iter_elements()
                                {
                                    match c.as_char().to_digit(10)
                                    {
                                        None =>
                                        {
                                            if pos == 0
                                            {
                                                return Err(Err::Error(E::from_error_kind
                                                (
                                                    input, ErrorKind::Digit
                                                )));
                                            }
                                            else { return Ok((i.take_from(pos), value)); }
                                        },

                                        Some(d) => match value.checked_mul(10)
                                        .and_then(|v| v.checked_add(d as $t))
                                        {
                                            None => return Err(Err::Error(E::from_error_kind
                                            (
                                                input, ErrorKind::Digit
                                            ))),

                                            Some(v) =>
                                            {
                                                pos += c.len();
                                                value = v;
                                            },
                                        }
                                    }
                                }
                            }

                            else
                            {
                                let mut pos = 0;
                                for c in i.iter_elements()
                                {
                                    match c.as_char().to_digit(10)
                                    {
                                        None =>
                                        {
                                            if pos == 0
                                            {
                                                return Err(Err::Error(E::from_error_kind
                                                (
                                                    input, ErrorKind::Digit
                                                )));
                                            }
                                            else { return Ok((i.take_from(pos), value)); }
                                        },

                                        Some(d) => match value.checked_mul(10)
                                        .and_then(|v| v.checked_sub(d as $t))
                                        {
                                            None => return Err(Err::Error(E::from_error_kind
                                            (
                                                input, ErrorKind::Digit
                                            ))),

                                            Some(v) =>
                                            {
                                                pos += c.len();
                                                value = v;
                                            },
                                        }
                                    }
                                }
                            }

                            Err(Err::Incomplete(Needed::new(1)))
                        }
                    )+
                }
            }
            
            macro_rules! uints
            {
                ($($t:tt)+) =>
                {
                    $(
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E>
                        where
                        T: Input ,
                        <T as Input>::Item: AsChar,
                        {
                            let i = input;

                            if i.input_len() == 0 {
                                return Err(Err::Incomplete(Needed::new(1)));
                            }

                            let mut value: $t = 0;
                            let mut pos = 0;
                            for c in i.iter_elements() {
                                match c.as_char().to_digit(10) {
                                    None => {
                                        if pos == 0 {
                                            return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
                                        } else {
                                            return Ok((i.take_from(pos), value));
                                        }
                                    },
                                    Some(d) => match value.checked_mul(10).and_then(|v| v.checked_add(d as $t)) {
                                        None => return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit))),
                                        Some(v) => {
                                        pos += c.len();
                                        value = v;
                                        },
                                    }
                                }
                            }

                            Err(Err::Incomplete(Needed::new(1)))
                        }
                    )+
                }
            }
            /// Recognizes one character.
            pub fn char<I, Error: ParseError<I>>(c: char) -> impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            {
                let mut parser = super::char(c);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Recognizes one character and checks that it satisfies a predicate.
            pub fn satisfy<F, I, Error: ParseError<I>>(cond: F) -> 
            impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            F: Fn(char) -> bool
            {
                let mut parser = super::satisfy(cond);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Recognizes one of the provided characters.
            pub fn one_of<I, T, Error: ParseError<I>>(list: T) -> 
            impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            T: FindToken<char>
            {
                let mut parser = super::one_of(list);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Recognizes a character that is not in the provided characters.
            pub fn none_of<I, T, Error: ParseError<I>>(list: T) -> 
            impl FnMut(I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar,
            T: FindToken<char>
            {
                let mut parser = super::none_of(list);
                move |i: I| parser.process::<OutputM<Emit, Emit, Streaming>>(i)
            }
            /// Recognizes the string "\r\n".
            pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str
            {
                match input.compare("\r\n")
                {
                    CompareResult::Ok => Ok(input.take_split(2)),
                    CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                    CompareResult::Error =>
                    {
                        let e: ErrorKind = ErrorKind::CrLf;
                        Err(Err::Error(E::from_error_kind(input, e)))
                    }
                }
            }
            /// Recognizes a string of any char except '\r\n' or '\n'.
            pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str>,
            <T as Input>::Item: AsChar
            {
                match input.position(|item|
                {
                    let c = item.as_char();
                    c == '\r' || c == '\n'
                })
                {
                    None => Err(Err::Incomplete(Needed::Unknown)),
                    Some(index) =>
                    {
                        let mut it = input.take_from(index).iter_elements();
                        let nth = it.next().unwrap().as_char();
                        if nth == '\r'
                        {
                            let sliced = input.take_from(index);
                            let comp = sliced.compare("\r\n");
                            match comp
                            {
                                CompareResult::Incomplete => Err(Err::Incomplete(Needed::Unknown)),
                                CompareResult::Error => 
                                {
                                    let e: ErrorKind = ErrorKind::Tag;
                                    Err(Err::Error(E::from_error_kind(input, e)))
                                }
                                CompareResult::Ok => Ok(input.take_split(index)),
                            }
                        } 
                        else { Ok(input.take_split(index)) }
                    }
                }
            }
            /// Recognizes an end of line (both '\n' and '\r\n').
            pub fn line_ending<T, E: ParseError<T>>(i: T) -> IResult<T, T, E> where
            T: Input,
            T: Compare<&'static str>,
            {
                match i.compare("\n")
                {
                    CompareResult::Ok => Ok(i.take_split(1)),
                    CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(1))),
                    CompareResult::Error =>
                    {
                        match i.compare("\r\n")
                        {
                            CompareResult::Ok => Ok(i.take_split(2)),
                            CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                            CompareResult::Error => Err(Err::Error(E::from_error_kind(i, ErrorKind::CrLf))),
                        }
                    }
                }
            }
            /// Matches a newline character '\\n'.
            pub fn newline<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            { char('\n')(input) }
            /// Matches a tab character '\t'.
            pub fn tab<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error> where
            I: Input,
            <I as Input>::Item: AsChar
            { char('\t')(input) }
            /// Matches one element as a character.
            pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                let mut it = input.iter_elements();
                match it.next()
                {
                    None => Err(Err::Incomplete(Needed::new(1))),
                    Some(c) => Ok((input.take_from(c.len()), c.as_char())),
                }
            }
            /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
            pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar,
            { input.split_at_position(|item| !item.is_alpha()) }
            /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
            pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_alpha(), ErrorKind::Alpha) }
            /// Recognizes zero or more ASCII numerical characters: 0-9.
            pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position(|item| !item.is_dec_digit()) }
            /// Recognizes one or more ASCII numerical characters: 0-9.
            pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_dec_digit(), ErrorKind::Digit) }
            /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
            pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position(|item| !item.is_hex_digit()) }
            /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
            pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_hex_digit(), ErrorKind::HexDigit) }
            /// Recognizes zero or more octal characters: 0-7.
            pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position(|item| !item.is_oct_digit()) }
            /// Recognizes one or more octal characters: 0-7.
            pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_oct_digit(), ErrorKind::OctDigit) }
            /// Recognizes zero or more binary characters: 0-1.
            pub fn bin_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position(|item| !item.is_bin_digit()) }
            /// Recognizes one or more binary characters: 0-1.
            pub fn bin_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_bin_digit(), ErrorKind::BinDigit) }
            /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
            pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position(|item| !item.is_alphanum()) }
            /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
            pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            { input.split_at_position1(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric) }
            /// Recognizes zero or more spaces and tabs.
            pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position(|item|
                {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t')
                })
            }
            /// Recognizes one or more spaces and tabs.
            pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position1
                (
                    |item|
                    {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    },
                    ErrorKind::Space,
                )
            }
            /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
            pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position(|item|
                {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                })
            }
            /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
            pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Input,
            <T as Input>::Item: AsChar
            {
                input.split_at_position1
                (
                    |item|
                    {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    },
                    ErrorKind::MultiSpace,
                )
            }

            pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E> where
            T: Clone + Input,
            T: for<'a> Compare<&'a [u8]>
            {
                let (i, opt_sign) = opt(alt((value(false, tag(&b"-"[..])), value(true, tag(&b"+"[..])))))
                .parse(input)?;
                let sign = opt_sign.unwrap_or(true);
                Ok((i, sign))
            }

            ints! { i8 i16 i32 i64 i128 isize }

            uints! { u8 u16 u32 u64 u128 usize }
        }

        #[inline] pub fn is_alphabetic(chr: u8) -> bool { matches!(chr, 0x41..=0x5A | 0x61..=0x7A) }

        #[inline] pub fn is_digit(chr: u8) -> bool { matches!(chr, 0x30..=0x39) }

        #[inline] pub fn is_hex_digit(chr: u8) -> bool { matches!(chr, 0x30..=0x39 | 0x41..=0x46 | 0x61..=0x66) }

        #[inline] pub fn is_oct_digit(chr: u8) -> bool { matches!(chr, 0x30..=0x37) }
        /// Tests if byte is ASCII binary digit: 0-1.
        #[inline] pub fn is_bin_digit(chr: u8) -> bool { matches!(chr, 0x30..=0x31) }

        #[inline] pub fn is_alphanumeric(chr: u8) -> bool { AsChar::is_alphanum(chr) }

        #[inline] pub fn is_space(chr: u8) -> bool { chr == b' ' || chr == b'\t' }

        #[inline] pub fn is_newline(chr: u8) -> bool { chr == b'\n' }
        /// Recognizes one character.
        pub fn char<I, Error: ParseError<I>>(c: char) -> impl Parser<I, Output = char, Error = Error> where
        I: Input,
        <I as Input>::Item: AsChar
        {
            Char { c, e: PhantomData }
        }
        /// Parser implementation for [char()]
        pub struct Char<E>
        {
            c:char,
            e:PhantomData<E>,
        }

        impl<I, Error: ParseError<I>> Parser<I> for Char<Error> where
        I: Input,
        <I as Input>::Item: AsChar
        {
            type Output = char;
            type Error = Error;
            #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, i: I ) 
            -> crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match (i).iter_elements().next().map(|t|
                {
                    let b = t.as_char() == self.c;
                    (&self.c, b)
                })
                {
                    None =>
                    {
                        if OM::Incomplete::is_streaming() { Err(Err::Incomplete(Needed::new(self.c.len() - i.input_len()))) } 
                        else { Err(Err::Error(OM::Error::bind(|| Error::from_char(i, self.c)))) }
                    }
                    Some((_, false)) => Err(Err::Error(OM::Error::bind(|| Error::from_char(i, self.c)))),
                    Some((c, true)) => Ok((i.take_from(c.len()), OM::Output::bind(|| c.as_char()))),
                }
            }
        }
        /// Recognizes one character and checks that it satisfies a predicate.
        pub fn satisfy<F, I, Error: ParseError<I>>( predicate: F ) -> 
        impl Parser<I, Output = char, Error = Error> where
        I: Input,
        <I as Input>::Item: AsChar,
        F: Fn(char) -> bool
        {
            Satisfy
            {
                predicate,
                make_error: |i: I| Error::from_error_kind(i, ErrorKind::Satisfy),
            }
        }
        /// Parser implementation for [satisfy]
        pub struct Satisfy<F, MakeError>
        {
            predicate: F,
            make_error: MakeError,
        }

        impl<I, Error: ParseError<I>, F, MakeError> Parser<I> for Satisfy<F, MakeError> where
        I: Input,
        <I as Input>::Item: AsChar,
        F: Fn(char) -> bool,
        MakeError: Fn(I) -> Error
        {
            type Output = char;
            type Error = Error;
            #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, i: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                match (i).iter_elements().next().map(|t|
                {
                    let c = t.as_char();
                    let b = (self.predicate)(c);
                    (c, b)
                })
                {
                    None =>
                    {
                        if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(Needed::Unknown)) } 
                        else { Err(Err::Error(OM::Error::bind(|| (self.make_error)(i)))) }
                    }
                    Some((_, false)) => Err(Err::Error(OM::Error::bind(|| (self.make_error)(i)))),
                    Some((c, true)) => Ok((i.take_from(c.len()), OM::Output::bind(|| c.as_char()))),
                }
            }
        }
        /// Recognizes one of the provided characters.
        pub fn one_of<I, T, Error: ParseError<I>>(l:T) -> impl Parser<I, Output = char, Error = Error> where
        I: Input,
        <I as Input>::Item: AsChar
        T: FindToken<char>
        {
            Satisfy
            {
                predicate: move |c: char| l.find_token(c),
                make_error: move |i| Error::from_error_kind(i, ErrorKind::OneOf),
            }
        }
        /// Recognizes a character that is not in the provided characters.
        pub fn none_of<I, T, Error: ParseError<I>>(l:T) -> impl Parser<I, Output = char, Error = Error> where
        I: Input,
        <I as Input>::Item: AsChar
        T: FindToken<char>
        {
            Satisfy
            {
                predicate: move |c: char| !l.find_token(c),
                make_error: move |i| Error::from_error_kind(i, ErrorKind::NoneOf),
            }
        }
        /// Matches one byte as a character. Note that the input type will
        /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
        pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E> where
        T: Input,
        <T as Input>::Item: AsChar
        {
            let mut it = input.iter_elements();
            match it.next()
            {
                None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
                Some(c) => Ok((input.take_from(c.len()), c.as_char())),
            }
        }
        /// Parser implementation for char
        pub struct AnyChar<E>
        {
            e: PhantomData<E>,
        }

        impl<I, Error: ParseError<I>> Parser<I> for AnyChar<Error> where
        I: Input,
        <I as Input>::Item: AsChar
        {
            type Output = char;
            type Error = Error;
            fn process<OM: crate::nom::OutputMode>( &mut self, i: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error> 
            {
                match (i).iter_elements().next()
                {
                    None =>
                    {
                        if OM::Incomplete::is_streaming() { Err(Err::Incomplete(Needed::new(1))) } 
                        else
                        {
                            Err(Err::Error(OM::Error::bind(||
                            {
                                Error::from_error_kind(i, ErrorKind::Eof)
                            })))
                        }
                    }
                    Some(c) => Ok((i.take_from(c.len()), OM::Output::bind(|| c.as_char()))),
                }
            }
        }
        /// Recognizes one or more ASCII numerical characters: 0-9.
        pub fn digit1<T, E: ParseError<T>>() -> impl Parser<T, Output = T, Error = E> where
        T: Input,
        <T as Input>::Item: AsChar
        { Digit1 { e: PhantomData } }
        /// Parser implementation for [digit1]
        pub struct Digit1<E>
        {
            e:PhantomData<E>,
        }

        impl<I: Input, E: ParseError<I>> Parser<I> for Digit1<E> where
        <I as Input>::Item: AsChar
        {
            type Output = I;
            type Error = E;
            #[inline] fn process<OM: crate::nom::OutputMode>( &mut self, input: I ) ->
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            { input.split_at_position_mode1::<OM, _, _>(|item| !item.is_dec_digit(), ErrorKind::Digit) }
        }
        /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
        pub fn multispace0<T, E: ParseError<T>>() -> impl Parser<T, Output = T, Error = E> where
        T: Input,
        <T as Input>::Item: AsChar
        {
            MultiSpace0 { e: PhantomData }
        }
        /// Parser implementation for [multispace0()]
        pub struct MultiSpace0<E>
        {
            e: PhantomData<E>,
        }

        impl<I, Error: ParseError<I>> Parser<I> for MultiSpace0<Error> where
        I: Input,
        <I as Input>::Item: AsChar
        {
            type Output = I;
            type Error = Error;
            fn process<OM: crate::nom::OutputMode>( &mut self, i: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                i.split_at_position_mode::<OM, _, _>(|item|
                {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                })
            }
        }
    }
    /*
    */
    mod str
    {
        
    }
    /*
    */
    pub mod number
    {
        //! Parsers recognizing numbers
        use ::
        {
            marker::{ PhantomData },
            nom::
            {
                branch::{ alt },
                character::{ char, digit1 },
                combinator::{ cut, map, opt, recognize },
                error::{ make_error, ErrorKind, ParseError },
                sequence::{ pair },
                AsBytes, AsChar, Compare, Either, Emit, Err, Input, IsStreaming, Mode, Needed, Offset, OutputM, Parser,
            },
            ops::{ Add, Shl },
            *,
        };

        pub mod complete
        {
            //! Parsers recognizing numbers, complete input version
            use ::
            {
                nom::
                {
                    branch::{ alt },
                    bytes::complete::{ tag },
                    character::complete::{ char, digit1, sign },
                    combinator::{ cut, map, opt, recognize },
                    error::{ ErrorKind, ParseError },
                    internal::{ * },
                    sequence::{ pair },
                    traits::{ AsBytes, AsChar, Compare, Input, Offset },
                },
                ops::{ Add, Shl },
                *,
            };
            /// Recognizes an unsigned 1 byte integer.
            #[inline] pub fn be_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E> where
            I: Input<Item = u8>
            { be_uint(input, 1) }
            /// Recognizes a big endian unsigned 2 bytes integer.
            #[inline] pub fn be_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E> where
            I: Input<Item = u8>
            { be_uint(input, 2) }
            /// Recognizes a big endian unsigned 3 byte integer.
            #[inline] pub fn be_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E> where
            I: Input<Item = u8>
            { be_uint(input, 3) }
            /// Recognizes a big endian unsigned 4 bytes integer.
            #[inline] pub fn be_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E> where
            I: Input<Item = u8>
            { be_uint(input, 4) }
            /// Recognizes a big endian unsigned 8 bytes integer.
            #[inline] pub fn be_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E> where
            I: Input<Item = u8>
            { be_uint(input, 8) }
            /// Recognizes a big endian unsigned 16 bytes integer.
            #[inline] pub fn be_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E> where
            I: Input<Item = u8>
            { be_uint(input, 16) }

            #[inline] fn be_uint<I, Uint, E: ParseError<I>>(input: I, bound: usize) -> IResult<I, Uint, E> where
            I: Input<Item = u8>
            Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>
            { super::be_uint(bound).parse_complete(input) }
            /// Recognizes a signed 1 byte integer.
            #[inline] pub fn be_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E> where
            I: Input<Item = u8>
            { be_u8.map(|x| x as i8).parse(input) }
            /// Recognizes a big endian signed 2 bytes integer.
            #[inline] pub fn be_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E> where
            I: Input<Item = u8>
            { be_u16.map(|x| x as i16).parse(input) }
            /// Recognizes a big endian signed 3 bytes integer.
            #[inline] pub fn be_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E> where
            I: Input<Item = u8>
            {
                be_u24
                .map(|x| 
                {
                    if x & 0x80_00_00 != 0  { (x | 0xff_00_00_00) as i32 } 
                    else { x as i32 }
                })
                .parse(input)
            }
            /// Recognizes a big endian signed 4 bytes integer.
            #[inline] pub fn be_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E> where
            I: Input<Item = u8>
            { be_u32.map(|x| x as i32).parse(input) }
            /// Recognizes a big endian signed 8 bytes integer.
            #[inline] pub fn be_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E> where
            I: Input<Item = u8>
            { be_u64.map(|x| x as i64).parse(input) }
            /// Recognizes a big endian signed 16 bytes integer.
            #[inline] pub fn be_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E> where
            I: Input<Item = u8>
            { be_u128.map(|x| x as i128).parse(input) }
            /// Recognizes an unsigned 1 byte integer.
            #[inline] pub fn le_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E> where
            I: Input<Item = u8>
            { le_uint(input, 1) }
            /// Recognizes a little endian unsigned 2 bytes integer.
            #[inline] pub fn le_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E> where
            I: Input<Item = u8>
            { le_uint(input, 2) }
            /// Recognizes a little endian unsigned 3 byte integer.
            #[inline] pub fn le_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E> where
            I: Input<Item = u8>
            { le_uint(input, 3) }
            /// Recognizes a little endian unsigned 4 bytes integer.
            #[inline] pub fn le_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E> where
            I: Input<Item = u8>
            { le_uint(input, 4) }
            /// Recognizes a little endian unsigned 8 bytes integer.
            #[inline] pub fn le_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E> where
            I: Input<Item = u8>
            { le_uint(input, 8) }
            /// Recognizes a little endian unsigned 16 bytes integer.
            #[inline] pub fn le_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E> where
            I: Input<Item = u8>
            { le_uint(input, 16) }

            #[inline] fn le_uint<I, Uint, E: ParseError<I>>(input: I, bound: usize) -> IResult<I, Uint, E> where
            I: Input<Item = u8>
            Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>,
            { super::le_uint(bound).parse_complete(input) }
            /// Recognizes a signed 1 byte integer.
            #[inline] pub fn le_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E> where
            I: Input<Item = u8>
            { be_u8.map(|x| x as i8).parse(input) }
            /// Recognizes a little endian signed 2 bytes integer.
            #[inline] pub fn le_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E> where
            I: Input<Item = u8>
            { le_u16.map(|x| x as i16).parse(input) }
            /// Recognizes a little endian signed 3 bytes integer.
            #[inline] pub fn le_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E> where
            I: Input<Item = u8>
            {
                le_u24
                .map(|x| 
                {
                    if x & 0x80_00_00 != 0 { (x | 0xff_00_00_00) as i32 } 
                    else { x as i32 }
                })
                .parse(input)
            }
            /// Recognizes a little endian signed 4 bytes integer.
            #[inline]
            pub fn le_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
            where
            I: Input<Item = u8>,
            {
            le_u32.map(|x| x as i32).parse(input)
            }
            /// Recognizes a little endian signed 8 bytes integer.
            #[inline]
            pub fn le_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E>
            where
            I: Input<Item = u8>,
            {
            le_u64.map(|x| x as i64).parse(input)
            }
            /// Recognizes a little endian signed 16 bytes integer.
            #[inline]
            pub fn le_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E>
            where
            I: Input<Item = u8>,
            {
            le_u128.map(|x| x as i128).parse(input)
            }
            /// Recognizes an unsigned 1 byte integer.
            #[inline]
            pub fn u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
            where
            I: Input<Item = u8>,
            {
            super::u8().parse_complete(input)
            }
            /// Recognizes an unsigned 2 bytes integer.
            #[inline]
            pub fn u16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, u16, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::u16(endian).parse_complete(input)
            }
            /// Recognizes an unsigned 3 byte integer.
            #[inline]
            pub fn u24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, u32, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::u24(endian).parse_complete(input)
            }
            /// Recognizes an unsigned 4 byte integer.
            #[inline]
            pub fn u32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, u32, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::u32(endian).parse_complete(input)
            }
            /// Recognizes an unsigned 8 byte integer.
            #[inline]
            pub fn u64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, u64, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::u64(endian).parse_complete(input)
            }
            /// Recognizes an unsigned 16 byte integer.
            #[inline]
            pub fn u128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, u128, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::u128(endian).parse_complete(input)
            }
            /// Recognizes a signed 1 byte integer.
            #[inline]
            pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E>
            where
            I: Input<Item = u8>,
            {
            super::u8().map(|x| x as i8).parse_complete(i)
            }
            /// Recognizes a signed 2 byte integer
            #[inline]
            pub fn i16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, i16, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::i16(endian).parse_complete(input)
            }
            /// Recognizes a signed 3 byte integer
            #[inline]
            pub fn i24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, i32, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::i24(endian).parse_complete(input)
            }
            /// Recognizes a signed 4 byte integer
            #[inline]
            pub fn i32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, i32, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::i32(endian).parse_complete(input)
            }
            /// Recognizes a signed 8 byte integer
            #[inline]
            pub fn i64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, i64, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::i64(endian).parse_complete(input)
            }
            /// Recognizes a signed 16 byte integer
            #[inline]
            pub fn i128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
            ) -> impl Fn(I) -> IResult<I, i128, E>
            where
            I: Input<Item = u8>,
            {
            move |input| super::i128(endian).parse_complete(input)
            }
            /// Recognizes a big endian 4 bytes floating point number.
            #[inline]
            pub fn be_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
            where
            I: Input<Item = u8>,
            {
            match be_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
            }
            /// Recognizes a big endian 8 bytes floating point number.
            #[inline]
            pub fn be_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
            where
            I: Input<Item = u8>,
            {
            match be_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
            }
            /// Recognizes a little endian 4 bytes floating point number.
            #[inline]
            pub fn le_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
            where
            I: Input<Item = u8>,
            {
            match le_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
            }
            /// Recognizes a little endian 8 bytes floating point number.
            #[inline]
            pub fn le_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
            where
            I: Input<Item = u8>,
            {
            match le_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
            }
            /// Recognizes a 4 byte floating point number
            #[inline]
            pub fn f32<I, E: ParseError<I>>(endian: crate::number::Endianness) -> fn(I) -> IResult<I, f32, E>
            where
            I: Input<Item = u8>,
            {
            match endian {
                crate::number::Endianness::Big => be_f32,
                crate::number::Endianness::Little => le_f32,
                #[cfg(target_endian = "big")]
                crate::number::Endianness::Native => be_f32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f32,
            }
            }
            /// Recognizes an 8 byte floating point number
            #[inline]
            pub fn f64<I, E: ParseError<I>>(endian: crate::number::Endianness) -> fn(I) -> IResult<I, f64, E>
            where
            I: Input<Item = u8>,
            {
            match endian {
                crate::number::Endianness::Big => be_f64,
                crate::number::Endianness::Little => le_f64,
                #[cfg(target_endian = "big")]
                crate::number::Endianness::Native => be_f64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f64,
            }
            }
            /// Recognizes a hex-encoded integer.
            #[inline]
            pub fn hex_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
            where
            I: Input,
            <I as Input>::Item: AsChar,
            I: AsBytes,
            {
            let e: ErrorKind = ErrorKind::IsA;
            let (i, o) = input.split_at_position1_complete(
                |c| {
                let c = c.as_char();
                !"0123456789abcdefABCDEF".contains(c)
                },
                e,
            )?;
            
            let (remaining, parsed) = if o.input_len() <= 8 {
                (i, o)
            } else {
                input.take_split(8)
            };

            let res = parsed
                .as_bytes()
                .iter()
                .rev()
                .enumerate()
                .map(|(k, &v)| {
                let digit = v as char;
                digit.to_digit(16).unwrap_or(0) << (k * 4)
                })
                .sum();

            Ok((remaining, res))
            }
            /// Recognizes floating point number in a byte string and returns the corresponding slice.
            pub fn recognize_float<T, E:ParseError<T>>(input: T) -> IResult<T, T, E>
            where
            T: Clone + Offset,
            T: Input,
            <T as Input>::Item: AsChar,
            {
            recognize((
                opt(alt((char('+'), char('-')))),
                alt((
                    map((digit1, opt(pair(char('.'), opt(digit1)))), |_| ()),
                    map((char('.'), digit1), |_| ())
                )),
                opt((
                    alt((char('e'), char('E'))),
                    opt(alt((char('+'), char('-')))),
                    cut(digit1)
                ))
            )).parse(input)
            }
            
            pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
            where
            T: Clone + Offset,
            T: Input + Compare<&'static str>,
            <T as Input>::Item: AsChar,
            {
            alt((
                |i: T| {
                recognize_float::<_, E>(i.clone()).map_err(|e| match e {
                    crate::Err::Error(_) => crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)),
                    crate::Err::Failure(_) => crate::Err::Failure(E::from_error_kind(i, ErrorKind::Float)),
                    crate::Err::Incomplete(needed) => crate::Err::Incomplete(needed),
                })
                },
                |i: T| {
                crate::bytes::complete::tag_no_case::<_, _, E>("nan")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
                |i: T| {
                crate::bytes::complete::tag_no_case::<_, _, E>("infinity")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
                |i: T| {
                crate::bytes::complete::tag_no_case::<_, _, E>("inf")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
            ))
            .parse(input)
            }
            /// Recognizes a floating point number in text format
            pub fn recognize_float_parts<T, E: ParseError<T>>(input: T) -> IResult<T, (bool, T, T, i32), E>
            where
            T: Clone + Offset,
            T: Input,
            <T as Input>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
            T: AsBytes,
            {
            let (i, sign) = sign(input.clone())?;
            let (i, zeroes) = match i.as_bytes().iter().position(|c| *c != b'0') {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };
            
            let (i, mut integer) = match i
                .as_bytes()
                .iter()
                .position(|c| !(*c >= b'0' && *c <= b'9'))
            {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };

            if integer.input_len() == 0 && zeroes.input_len() > 0 {
                // keep the last zero if integer is empty
                integer = zeroes.take_from(zeroes.input_len() - 1);
            }

            let (i, opt_dot) = opt(tag(&b"."[..])).parse(i)?;
            let (i, fraction) = if opt_dot.is_none() {
                let i2 = i.clone();
                (i2, i.take(0))
            } else {
                // match number, trim right zeroes
                let mut zero_count = 0usize;
                let mut position = None;
                for (pos, c) in i.as_bytes().iter().enumerate() {
                if *c >= b'0' && *c <= b'9' {
                    if *c == b'0' {
                    zero_count += 1;
                    } else {
                    zero_count = 0;
                    }
                } else {
                    position = Some(pos);
                    break;
                }
                }

                #[allow(clippy::or_fun_call)]
                let position = position.unwrap_or(i.input_len());

                let index = if zero_count == 0 {
                position
                } else if zero_count == position {
                position - zero_count + 1
                } else {
                position - zero_count
                };

                (i.take_from(position), i.take(index))
            };

            if integer.input_len() == 0 && fraction.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Float)));
            }

            let i2 = i.clone();
            let (i, e) = match i.as_bytes().iter().next() {
                Some(b'e') => (i.take_from(1), true),
                Some(b'E') => (i.take_from(1), true),
                _ => (i, false),
            };

            let (i, exp) = if e {
                cut(crate::character::complete::i32).parse(i)?
            } else {
                (i2, 0)
            };

            Ok((i, (sign, integer, fraction, exp)))
            }

            use crate::traits::ParseTo;

            /// Recognizes floating point number in text format and returns a f32.
            pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E>
            where
            T: Clone + Offset + ParseTo<f32> + Compare<&'static str>,
            T: Input,
            <T as Input>::Item: AsChar,
            <T as Input>::Iter: Clone,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
            {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => Err(crate::Err::Error(E::from_error_kind(
                i,
                crate::error::ErrorKind::Float,
                ))),
            }
            }
            /// Recognizes floating point number in text format and returns a f64.
            pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E>
            where
            T: Clone + Offset + ParseTo<f64> + Compare<&'static str>,
            T: Input,
            <T as Input>::Item: AsChar,
            <T as Input>::Iter: Clone,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
            {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => Err(crate::Err::Error(E::from_error_kind(
                i,
                crate::error::ErrorKind::Float,
                ))),
            }
            }
        }

        pub mod streaming
        {
            
        }
        /// Configurable endianness
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        pub enum Endianness
        {
            /// Big endian
            Big,
            /// Little endian
            Little,
            /// Will match the host's endianness
            Native,
        }
        /// Creates a big endian unsigned integer parser.
        #[inline] fn be_uint<I, Uint, E: ParseError<I>>(bound: usize) -> impl Parser<I, Output = Uint, Error = E> where
        I: Input<Item = u8>,
        Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>
        {
            BeUint
            {
                bound,
                e: PhantomData,
                u: PhantomData,
            }
        }
        /// Big endian unsigned integer parser
        struct BeUint<Uint, E>
        {
            bound:usize,
            e:PhantomData<E>,
            u:PhantomData<Uint>,
        }

        impl<I, Uint, E: ParseError<I>> Parser<I> for BeUint<Uint, E> where
        I: Input<Item = u8>,
        Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>
        {
            type Output = Uint;
            type Error = E;
            #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if input.input_len() < self.bound
                {
                    if OM::Incomplete::is_streaming(){ Err(Err::Incomplete(Needed::new(self.bound - input.input_len()))) }
                    else
                    {
                        Err(Err::Error(OM::Error::bind(||
                        {
                            make_error(input, ErrorKind::Eof)
                        })))
                    }
                }

                else
                {
                    let res = OM::Output::bind(||
                    {
                        let mut res = Uint::default();
                        if self.bound > 1
                        {
                            for byte in input.iter_elements().take(self.bound)
                            {
                                res = (res << 8) + byte.into();
                            }
                        }
                        
                        else
                        {
                            for byte in input.iter_elements().take(self.bound)
                            {
                                res = byte.into();
                            }
                        }

                        res
                    });

                    Ok((input.take_from(self.bound), res))
                }
            }
        }
        /// Recognizes an unsigned 1 byte integer.
        #[inline] pub fn be_u8<I, E: ParseError<I>>() -> impl Parser<I, Output = u8, Error = E> where
        I: Input<Item = u8>
        { be_uint(1) }
        /// Recognizes a big endian unsigned 2 bytes integer.
        #[inline] pub fn be_u16<I, E: ParseError<I>>() -> impl Parser<I, Output = u16, Error = E> where
        I: Input<Item = u8>
        { be_uint(2) }
        /// Recognizes a big endian unsigned 3 byte integer.
        #[inline] pub fn be_u24<I, E: ParseError<I>>() -> impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        { be_uint(3) }
        /// Recognizes a big endian unsigned 4 bytes integer.
        #[inline] pub fn be_u32<I, E: ParseError<I>>() -> impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        { be_uint(4) }
        /// Recognizes a big endian unsigned 8 bytes integer.
        #[inline] pub fn be_u64<I, E: ParseError<I>>() -> impl Parser<I, Output = u64, Error = E> where
        I: Input<Item = u8>
        { be_uint(8) }
        /// Recognizes a big endian unsigned 16 bytes integer.
        #[inline] pub fn be_u128<I, E: ParseError<I>>() -> impl Parser<I, Output = u128, Error = E> where
        I: Input<Item = u8>
        { be_uint(16) }
        /// Recognizes a signed 1 byte integer.
        #[inline] pub fn be_i8<I, E: ParseError<I>>() -> impl Parser<I, Output = i8, Error = E> where
        I: Input<Item = u8>
        { be_u8().map(|x| x as i8) }
        /// Recognizes a big endian signed 2 bytes integer.
        #[inline] pub fn be_i16<I, E: ParseError<I>>() -> impl Parser<I, Output = i16, Error = E> where
        I: Input<Item = u8>
        { be_u16().map(|x| x as i16) }
        /// Recognizes a big endian signed 3 bytes integer.
        #[inline] pub fn be_i24<I, E: ParseError<I>>() -> impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        {
            be_u24().map(|x| 
            {
                if x & 0x80_00_00 != 0 { (x | 0xff_00_00_00) as i32 }
                else { x as i32 }
            })
        }
        /// Recognizes a big endian signed 4 bytes integer.
        #[inline] pub fn be_i32<I, E: ParseError<I>>() -> impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        { be_u32().map(|x| x as i32) }
        /// Recognizes a big endian signed 8 bytes integer.
        #[inline] pub fn be_i64<I, E: ParseError<I>>() -> impl Parser<I, Output = i64, Error = E> where
        I: Input<Item = u8>
        { be_u64().map(|x| x as i64) }
        /// Recognizes a big endian signed 16 bytes integer.
        #[inline] pub fn be_i128<I, E: ParseError<I>>() -> impl Parser<I, Output = i128, Error = E> where
        I: Input<Item = u8>
        { be_u128().map(|x| x as i128) }
        /// creates a little endian unsigned integer parser.
        #[inline] fn le_uint<I, Uint, E: ParseError<I>>(bound: usize) -> impl Parser<I, Output = Uint, Error = E> where
        I: Input<Item = u8>,
        Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>
        {
            LeUint
            {
                bound,
                e: PhantomData,
                u: PhantomData,
            }
        }
        /// Little endian unsigned integer parser
        struct LeUint<Uint, E>
        {
            bound: usize,
            e: PhantomData<E>,
            u: PhantomData<Uint>,
        }

        impl<I, Uint, E: ParseError<I>> Parser<I> for LeUint<Uint, E> where
        I: Input<Item = u8>,
        Uint: Default + Shl<u8, Output = Uint> + Add<Uint, Output = Uint> + From<u8>
        {
            type Output = Uint;
            type Error = E;
            #[inline(always)] fn process<OM: crate::nom::OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                if input.input_len() < self.bound
                {
                    if OM::Incomplete::is_streaming()
                    {
                        Err(Err::Incomplete(Needed::new(self.bound - input.input_len())))
                    } 
                    else
                    {
                        Err(Err::Error(OM::Error::bind(||
                        {
                            make_error(input, ErrorKind::Eof)
                        })))
                    }
                }
                
                else
                {
                    let res = OM::Output::bind(||
                    {
                        let mut res = Uint::default();
                        for (index, byte) in input.iter_elements().take(self.bound).enumerate()
                        {
                            res = res + (Uint::from(byte) << (8 * index as u8));
                        }
                        res
                    });
                    Ok((input.take_from(self.bound), res))
                }
            }
        }
        /// Recognizes an unsigned 1 byte integer.
        #[inline] pub fn le_u8<I, E: ParseError<I>>() -> impl Parser<I, Output = u8, Error = E> where
        I: Input<Item = u8>
        { le_uint(1) }
        /// Recognizes a little endian unsigned 2 bytes integer.
        #[inline] pub fn le_u16<I, E: ParseError<I>>() -> impl Parser<I, Output = u16, Error = E> where
        I: Input<Item = u8>
        { le_uint(2) }
        /// Recognizes a little endian unsigned 3 bytes integer.
        #[inline] pub fn le_u24<I, E: ParseError<I>>() -> impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        { le_uint(3) }
        /// Recognizes a little endian unsigned 4 bytes integer.
        #[inline] pub fn le_u32<I, E: ParseError<I>>() -> impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        { le_uint(4) }
        /// Recognizes a little endian unsigned 8 bytes integer.
        #[inline] pub fn le_u64<I, E: ParseError<I>>() -> impl Parser<I, Output = u64, Error = E> where
        I: Input<Item = u8>
        { le_uint(8) }
        /// Recognizes a little endian unsigned 16 bytes integer.
        #[inline] pub fn le_u128<I, E: ParseError<I>>() -> impl Parser<I, Output = u128, Error = E> where
        I: Input<Item = u8>
        { le_uint(16) }
        /// Recognizes a signed 1 byte integer.
        #[inline] pub fn le_i8<I, E: ParseError<I>>() -> impl Parser<I, Output = i8, Error = E> where
        I: Input<Item = u8>
        { le_u8().map(|x| x as i8) }
        /// Recognizes a little endian signed 2 bytes integer.
        #[inline] pub fn le_i16<I, E: ParseError<I>>() -> impl Parser<I, Output = i16, Error = E> where
        I: Input<Item = u8>
        { le_u16().map(|x| x as i16) }
        /// Recognizes a little endian signed 3 bytes integer.
        #[inline] pub fn le_i24<I, E: ParseError<I>>() -> impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        {
            le_u24().map(|x|
            {
                if x & 0x80_00_00 != 0 { (x | 0xff_00_00_00) as i32 } 
                else { x as i32 }
            })
        }
        /// Recognizes a little endian signed 4 bytes integer.
        #[inline] pub fn le_i32<I, E: ParseError<I>>() -> impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        { le_u32().map(|x| x as i32) }
        /// Recognizes a little endian signed 8 bytes integer.
        #[inline] pub fn le_i64<I, E: ParseError<I>>() -> impl Parser<I, Output = i64, Error = E> where
        I: Input<Item = u8>
        { le_u64().map(|x| x as i64) }
        /// Recognizes a little endian signed 16 bytes integer.
        #[inline] pub fn le_i128<I, E: ParseError<I>>() -> impl Parser<I, Output = i128, Error = E> where
        I: Input<Item = u8>
        { le_u128().map(|x| x as i128) }
        /// Recognizes an unsigned 1 byte integer.
        #[inline] pub fn u8<I, E: ParseError<I>>() -> impl Parser<I, Output = u8, Error = E> where
        I: Input<Item = u8>
        { be_u8() }
        /// Recognizes an unsigned 2 bytes integer.
        #[inline] pub fn u16<I, E: ParseError<I>>( endian:crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = u16, Error = E> where
        I: Input<Item = u8>
        {
            match endian 
            {
                crate::nom::number::Endianness::Big => Either::Left(be_u16()),
                crate::nom::number::Endianness::Little => Either::Right(le_u16()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_u16()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_u16()),
            }
        }
        /// Recognizes an unsigned 3 byte integer.
        #[inline] pub fn u24<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_u24()),
                crate::nom::number::Endianness::Little => Either::Right(le_u24()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_u24()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_u24()),
            }
        }
        /// Recognizes an unsigned 4 byte integer.
        #[inline] pub fn u32<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = u32, Error = E> where
        I: Input<Item = u8>
        {
            match endian 
            {
                crate::nom::number::Endianness::Big => Either::Left(be_u32()),
                crate::nom::number::Endianness::Little => Either::Right(le_u32()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_u32()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_u32()),
            }
        }
        /// Recognizes an unsigned 8 byte integer.
        #[inline] pub fn u64<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = u64, Error = E> where
        I: Input<Item = u8>
        {
            match endian 
            {
                crate::nom::number::Endianness::Big => Either::Left(be_u64()),
                crate::nom::number::Endianness::Little => Either::Right(le_u64()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_u64()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_u64()),
            }
        }
        /// Recognizes an unsigned 16 byte integer.
        #[inline] pub fn u128<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = u128, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_u128()),
                crate::nom::number::Endianness::Little => Either::Right(le_u128()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_u128()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_u128()),
            }
        }
        /// Recognizes a signed 1 byte integer.
        #[inline] pub fn i8<I, E: ParseError<I>>() -> impl Parser<I, Output = i8, Error = E> where
        I: Input<Item = u8>
        { u8().map(|x| x as i8) }
        /// Recognizes a signed 2 byte integer.
        #[inline] pub fn i16<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = i16, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_i16()),
                crate::nom::number::Endianness::Little => Either::Right(le_i16()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_i16()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_i16()),
            }
        }
        /// Recognizes a signed 3 byte integer.
        #[inline] pub fn i24<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_i24()),
                crate::nom::number::Endianness::Little => Either::Right(le_i24()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_i24()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_i24()),
            }
        }
        /// Recognizes a signed 4 byte integer.
        #[inline] pub fn i32<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = i32, Error = E> where
        I: Input<Item = u8>
        {
            match endian 
            {
                crate::nom::number::Endianness::Big => Either::Left(be_i32()),
                crate::nom::number::Endianness::Little => Either::Right(le_i32()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_i32()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_i32()),
            }
        }
        /// Recognizes a signed 8 byte integer.
        #[inline] pub fn i64<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> impl Parser<I, Output = i64, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_i64()),
                crate::nom::number::Endianness::Little => Either::Right(le_i64()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_i64()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_i64()),
            }
        }
        /// Recognizes a signed 16 byte integer.
        #[inline] pub fn i128<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = i128, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_i128()),
                crate::nom::number::Endianness::Little => Either::Right(le_i128()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_i128()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_i128()),
            }
        }
        /// Recognizes a big endian 4 bytes floating point number.
        #[inline] pub fn be_f32<I, E: ParseError<I>>() -> impl Parser<I, Output = f32, Error = E> where
        I: Input<Item = u8>
        { be_u32().map(f32::from_bits) }
        /// Recognizes a big endian 8 bytes floating point number.
        #[inline] pub fn be_f64<I, E: ParseError<I>>() -> impl Parser<I, Output = f64, Error = E> where
        I: Input<Item = u8>
        { be_u64().map(f64::from_bits) }
        /// Recognizes a little endian 4 bytes floating point number.
        #[inline] pub fn le_f32<I, E: ParseError<I>>() -> impl Parser<I, Output = f32, Error = E> where
        I: Input<Item = u8>
        { le_u32().map(f32::from_bits) }
        /// Recognizes a little endian 8 bytes floating point number.
        #[inline] pub fn le_f64<I, E: ParseError<I>>() -> impl Parser<I, Output = f64, Error = E> where
        I: Input<Item = u8>
        { le_u64().map(f64::from_bits) }
        /// Recognizes a 4 byte floating point number.
        #[inline] pub fn f32<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> impl Parser<I, Output = f32, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_f32()),
                crate::nom::number::Endianness::Little => Either::Right(le_f32()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_f32()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_f32()),
            }
        }
        /// Recognizes an 8 byte floating point number.
        #[inline] pub fn f64<I, E: ParseError<I>>( endian: crate::nom::number::Endianness ) -> 
        impl Parser<I, Output = f64, Error = E> where
        I: Input<Item = u8>
        {
            match endian
            {
                crate::nom::number::Endianness::Big => Either::Left(be_f64()),
                crate::nom::number::Endianness::Little => Either::Right(le_f64()),
                #[cfg(target_endian = "big")]
                crate::nom::number::Endianness::Native => Either::Left(be_f64()),
                #[cfg(target_endian = "little")]
                crate::nom::number::Endianness::Native => Either::Right(le_f64()),
            }
        }
        /// Recognizes a floating point number in text format and returns the corresponding part of the input.
        pub fn recognize_float<T, E:ParseError<T>>() -> impl Parser<T, Output=T,Error= E> where
        T: Clone + Offset,
        T: Input,
        <T as Input>::Item: AsChar
        {
            recognize
            ((
                opt(alt((char('+'), char('-')))),
                alt
                ((
                    map((digit1(), opt(pair(char('.'), opt(digit1())))), |_| ()),
                    map((char('.'), digit1()), |_| ())
                )),
                opt
                ((
                    alt((char('e'), char('E'))),
                    opt(alt((char('+'), char('-')))),
                    cut(digit1())
                ))
            ))
        }
        /// Float number text parser that also recognizes "nan", "infinity" and "inf" (case insensitive)
        pub fn recognize_float_or_exceptions<T, E: ParseError<T>>() -> impl Parser<T, Output = T, Error = E> where
        T: Clone + Offset,
        T: Input + Compare<&'static str>,
        <T as Input>::Item: AsChar
        {
            alt
            ((
                recognize_float::<_, E>(),
                |i: T| 
                {
                    crate::nom::bytes::streaming::tag_no_case::<_, _, E>("nan")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
                |i: T|
                {
                    crate::nom::bytes::streaming::tag_no_case::<_, _, E>("infinity")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
                |i: T|
                {
                    crate::nom::bytes::streaming::tag_no_case::<_, _, E>("inf")(i.clone())
                    .map_err(|_| crate::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                },
            ))
        }
        /// Single precision floating point number parser from text
        pub fn float<T, E: ParseError<T>>() -> impl Parser<T, Output = f32, Error = E> where
        T: Clone + Offset,
        T: Input + crate::traits::ParseTo<f32> + Compare<&'static str>,
        <T as Input>::Item: AsChar + Clone,
        T: AsBytes,
        T: for<'a> Compare<&'a [u8]>
        {
            Float
            {
                o: PhantomData,
                e: PhantomData,
            }
        }
        /// Double precision floating point number parser from text
        pub fn double<T, E: ParseError<T>>() -> impl Parser<T, Output = f64, Error = E> where
        T: Clone + Offset,
        T: Input + crate::traits::ParseTo<f64> + Compare<&'static str>,
        <T as Input>::Item: AsChar + Clone,
        T: AsBytes,
        T: for<'a> Compare<&'a [u8]>
        {
            Float
            {
                o: PhantomData,
                e: PhantomData,
            }
        }
        /// F64 parser from text
        struct Float<O, E>
        {
            o: PhantomData<O>,
            e: PhantomData<E>,
        }

        impl<I, O, E: ParseError<I>> Parser<I> for Float<O, E> where
        I: Clone + Offset,
        I: Input + crate::traits::ParseTo<O> + Compare<&'static str>,
        <I as Input>::Item: AsChar + Clone,
        I: AsBytes,
        I: for<'a> Compare<&'a [u8]>
        {
            type Output = O;
            type Error = E;
            fn process<OM: crate::nom::OutputMode>( &mut self, input: I ) -> 
            crate::nom::PResult<OM, I, Self::Output, Self::Error>
            {
                let (i, s) = recognize_float_or_exceptions().process::<OutputM<Emit, OM::Error, OM::Incomplete>>(input)?;
                match s.parse_to()
                {
                    Some(f) => Ok((i, OM::Output::bind(|| f))),
                    None => Err(crate::nom::Err::Error(OM::Error::bind(||
                    {
                        E::from_error_kind(i, crate::nom::error::ErrorKind::Float)
                    }))),
                }
            }
        }
    }
}
/* aka pub mod execute */
pub mod now
{
    use ::
    {
        collections::{ HashMap },
        io::{ self, Read, Write },
        regex::{ Regex },
        shell::{ self, Shell },
        types::{ CommandLine, CommandResult, Tokens },
        *,
    };

    /// Entry point for non-ttys ( e.g. Cmd-N on MacVim )
    pub fn run_procs_for_non_tty( sh:&mut Shell )
    {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        match handle.read_to_string( &mut buffer ) {
            Ok( _ ) => {
               // log!( "run non tty command:{}", &buffer );
                run_command_line( sh, &buffer, false, false );
            }
            Err( e ) => {
                println!( "pls:stdin.read_to_string() failed:{:?}", e );
            }
        }
    }

    pub fn run_command_line( sh:&mut Shell, line:&str, tty:bool, capture:bool ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();
        for token in parsers::line::line_to_cmds( line ) {
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
            let cr = run_proc( sh, &cmd, tty, capture );
            status = cr.status;
            sh.previous_status = status;
            cr_list.push( cr );
        }
        cr_list
    }

    fn drain_env_tokens( tokens:&mut Tokens ) -> HashMap<String, String>
    {
        let mut envs:HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let ptn_env_exp = r"^( [a-zA-Z_][a-zA-Z0-9_]* )=( .* )$";
        let re = Regex::new( ptn_env_exp ).unwrap();
        for ( sep, text ) in tokens.iter() {
            if !sep.is_empty() || !regex::re_contains( text, ptn_env_exp ) {
                break;
            }

            for cap in re.captures_iter( text ) {
                let name = cap[1].to_string();
                let value = parsers::line::unquote( &cap[2] );
                envs.insert( name, value );
            }

            n += 1;
        }
        if n > 0 {
            tokens.drain( 0..n );
        }
        envs
    }

    fn line_to_tokens( sh:&mut Shell, line:&str ) -> ( Tokens, HashMap<String, String> )
    {
        let linfo = parsers::line::parse_line( line );
        let mut tokens = linfo.tokens;
        shell::do_expansion( sh, &mut tokens );
        let envs = drain_env_tokens( &mut tokens );
        ( tokens, envs )
    }

    fn set_shell_vars( sh:&mut Shell, envs:&HashMap<String, String> )
    {
        for ( name, value ) in envs.iter() {
            sh.set_env( name, value );
        }
    }

    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    /// example 1:`ls`
    /// example 2:`ls | wc`
    fn run_proc( sh:&mut Shell, line:&str, tty:bool, capture:bool ) -> CommandResult
    {
        let log_cmd = !sh.cmd.starts_with( ' ' );
        match CommandLine::from_line( line, sh ) {
            Ok( cl ) => {
                if cl.is_empty() {
                    if !cl.envs.is_empty() {
                        set_shell_vars( sh, &cl.envs );
                    }
                    return CommandResult::new();
                }

                let ( term_given, cr ) = shell::run_pipeline( sh, &cl, tty, capture, log_cmd );
                if term_given {
                    unsafe {
                        let gid = libc::getpgid( 0 );
                        shell::give_terminal_to( gid );
                    }
                }

                cr
            }
            Err( e ) => {
                println_stderr!( "pls:{}", e );
                CommandResult::from_status( 0, 1 )
            }
        }
    }

    fn run_with_shell( sh:&mut Shell, line:&str ) -> CommandResult
    {
        let ( tokens, envs ) = line_to_tokens( sh, line );
        if tokens.is_empty() {
            set_shell_vars( sh, &envs );
            return CommandResult::new();
        }

        match CommandLine::from_line( line, sh ) {
            Ok( c ) => {
                let ( term_given, cr ) = shell::run_pipeline( sh, &c, false, true, false );
                if term_given {
                    unsafe {
                        let gid = libc::getpgid( 0 );
                        shell::give_terminal_to( gid );
                    }
                }

                cr
            }
            Err( e ) => {
                println_stderr!( "pls:{}", e );
                CommandResult::from_status( 0, 1 )
            }
        }
    }

    pub fn run( line:&str ) -> CommandResult
    {
        let mut sh = Shell::new();
        run_with_shell( &mut sh, line )
    }
}
pub mod num { pub use std::num::{ * }; }
pub mod ops { pub use std::ops::{ * }; }
pub mod option
{
    pub use std::option::{ * };
    pub use self::Option::{ * };
    /*
    option-ext v0.0.0*/
    pub mod implementation
    {
        use super::OptionExt;

        impl<T> OptionExt<T> for Option<T>
        {
            fn contains<U>( &self, x:&U ) -> bool where
            U:PartialEq<T> 
            {
                match *self
                {
                    Some( ref y ) => x == y,
                    None => false,
                }
            }

            #[inline] fn map_or2<U, F:FnOnce( T ) -> U>( self, f:F, default:U ) -> U 
            { self.map_or( default, f ) }

            #[inline] fn map_or_else2<U, F:FnOnce( T ) -> U, D:FnOnce() -> U>( self, f:F, default:D ) -> U 
            { self.map_or_else( default, f ) }
        }
    }

    /// Extension trait providing additional methods for `Option`.
    pub trait OptionExt<T>
    {
        /// Returns `true` if the option is a [`Some`] value containing the given value.
        #[must_use] fn contains<U>( &self, x:&U ) -> bool where U:PartialEq<T>;

        /// Returns the result from applying the function `f` to the contained value if the option is [`Some`], 
        /// or returns provided `default` value if the option is [`None`].
        #[must_use] fn map_or2<U, F:FnOnce( T ) -> U>( self, f:F, default:U ) -> U;

        /// Returns the result from applying the function `f` to the contained value if the option is [`Some`],
        /// or returns the result from evaluating the provided function `default` if the option is [`None`].
        #[must_use] fn map_or_else2<U, F:FnOnce( T ) -> U, D:FnOnce() -> U>( self, f:F, default:D ) -> U;
    }
}
pub mod os { pub use std::os::{ * }; }
pub mod panic { pub use std::panic::{ * }; }
pub mod parsers
{
    pub mod line
    {
        use ::
        {
            regex::{ Regex },
            types::{LineInfo, Redirection, Tokens},
            *,
        };

        pub fn line_to_plain_tokens( line:&str ) -> Vec<String>
        {
            let mut result = Vec::new();
            let linfo = parse_line( line );
            for ( _, r ) in linfo.tokens {
                result.push( r.clone() );
            }
            result
        }

        pub fn tokens_to_args( tokens:&Tokens ) -> Vec<String>
        {
            let mut result = Vec::new();
            for s in tokens {
                result.push( s.1.clone() );
            }
            result
        }

        pub fn tokens_to_line( tokens:&Tokens ) -> String
        {
            let mut result = String::new();
            for t in tokens {
                if t.0.is_empty() {
                    result.push_str( &t.1 );
                } else {
                    let s = str::wrap_sep_string( &t.0, &t.1 );
                    result.push_str( &s );
                }
                result.push( ' ' );
            }
            if result.ends_with( ' ' ) {
                let len = result.len();
                result.truncate( len - 1 );
            }
            result
        }
        /// Parse command line for multiple commands.
        pub fn line_to_cmds( line:&str ) -> Vec<String>
        {
            let mut result = Vec::new();
            let mut sep = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let len = line.chars().count();
            for ( i, c ) in line.chars().enumerate() {
                if has_backslash {
                    token.push( '\\' );
                    token.push( c );
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
                        token.push( c );
                        continue;
                    }
                }
                if c == '\'' || c == '"' || c == '`' {
                    if sep.is_empty() {
                        sep.push( c );
                        token.push( c );
                        continue;
                    } else if sep == c.to_string() {
                        token.push( c );
                        sep = String::new();
                        continue;
                    } else {
                        token.push( c );
                        continue;
                    }
                }
                if c == '&' || c == '|' {
                    if sep.is_empty() {
                        if i + 1 == len {
                            token.push( c );
                            continue;
                        } else {
                            let c_next = match line.chars().nth( i + 1 ) {
                                Some( x ) => x,
                                None => {
                                    println!( "chars nth error - should never happen" );
                                    continue;
                                }
                            };

                            if c_next != c {
                                token.push( c );
                                continue;
                            }
                        }
                    }

                    if sep.is_empty() {
                        sep.push( c );
                        continue;
                    } else if c.to_string() == sep {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push( _token );
                        }
                        token = String::new();
                        result.push( format!( "{}{}", sep, sep ) );
                        sep = String::new();
                        continue;
                    } else {
                        token.push( c );
                        continue;
                    }
                }
                if c == ';' {
                    if sep.is_empty() {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push( _token );
                        }
                        result.push( String::from( ";" ) );
                        token = String::new();
                        continue;
                    } else {
                        token.push( c );
                        continue;
                    }
                }
                token.push( c );
            }
            if !token.is_empty() {
                result.push( token.trim().to_string() );
            }
            result
        }
        /// Parse command line to tokens.
        pub fn parse_line( line:&str ) -> LineInfo
        {
            let mut result = Vec::new();
            if is::arithmetic( line ) {
                for x in line.split( ' ' ) {
                    result.push( ( String::from( "" ), x.to_string() ) );
                }
                return LineInfo::new( result );
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
            for ( i, c ) in line.chars().enumerate() {
                if skip_next {
                    skip_next = false;
                    continue;
                }

                if has_backslash && sep.is_empty() && ( c == '>' || c == '<' ) {
                    sep_made = String::from( "'" );
                    token.push( c );
                    has_backslash = false;
                    continue;
                }

                if has_backslash && sep == "\"" && c != '\"' {
                    token.push( '\\' );
                    token.push( c );
                    has_backslash = false;
                    continue;
                }

                if has_backslash {
                    if new_round && sep.is_empty() && ( c == '|' || c == '$' ) && token.is_empty() {
                        sep = String::from( "\\" );
                        token = format!( "{}", c );
                    } else {
                        token.push( c );
                    }
                    new_round = false;
                    has_backslash = false;
                    continue;
                }

                if c == '$' {
                    has_dollar = true;
                }
                
                if c == '(' && sep.is_empty() {
                    if !has_dollar && token.is_empty() {
                        parens_left_ignored = true;
                        continue;
                    }
                    met_parenthesis = true;
                }
                if c == ')' {
                    if parens_left_ignored && !has_dollar {
                        if i == count_chars - 1 ||
                                ( i + 1 < count_chars &&
                                 line.chars().nth( i + 1 ).unwrap() == ' ' ) {
                            continue;
                        }
                    }
                    if sep.is_empty() {
                        met_parenthesis = false;
                    }
                }

                if c == '\\' {
                    if sep == "'" || !sep_second.is_empty() {
                        token.push( c )
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
                        break;
                    }

                    if c == '|' {
                        if i + 1 < count_chars && line.chars().nth( i + 1 ).unwrap() == '|' {
                            result.push( ( String::from( "" ), "||".to_string() ) );
                            skip_next = true;
                        } else {
                            result.push( ( String::from( "" ), "|".to_string() ) );
                        }
                        new_round = true;
                        continue;
                    }

                    token.push( c );
                    new_round = false;
                    continue;
                }

                if c == '|' && !has_backslash {
                    if semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push( ( sep_made.to_string(), token ) );
                            sep_made = String::new();
                        } else {
                            result.push( ( sep.to_string(), token ) );
                        }
                        result.push( ( String::from( "" ), "|".to_string() ) );
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        continue;
                    } else if !met_parenthesis && sep_second.is_empty() && sep.is_empty() {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push( ( sep_made.to_string(), token ) );
                            sep_made = String::new();
                        } else {
                            result.push( ( String::from( "" ), token ) );
                        }
                        result.push( ( String::from( "" ), "|".to_string() ) );
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
                            result.push( ( sep_made.to_string(), token ) );
                            sep_made = String::new();
                        } else {
                            result.push( ( sep.to_string(), token ) );
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
                        token.push( c );
                        continue;
                    }

                    if met_parenthesis {
                        token.push( c );
                        continue;
                    }

                    if sep == "\\" {
                        result.push( ( String::from( "\\" ), token ) );
                        token = String::new();
                        new_round = true;
                        continue;
                    }

                    if sep.is_empty() {
                        if sep_second.is_empty() {
                            if sep.is_empty() && !sep_made.is_empty() {
                                result.push( ( sep_made.clone(), token ) );
                                sep_made = String::new();
                            } else {
                                result.push( ( String::from( "" ), token ) );
                            }
                            token = String::new();
                            new_round = true;
                            continue;
                        } else {
                            token.push( c );
                            continue;
                        }
                    } else {
                        token.push( c );
                        continue;
                    }
                }

                if c == '\'' || c == '"' || c == '`' {
                    if has_backslash {
                        has_backslash = false;
                        token.push( c );
                        continue;
                    }

                    if sep != c.to_string() && semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push( ( sep_made.to_string(), token ) );
                            sep_made = String::new();
                        } else {
                            result.push( ( sep.to_string(), token ) );
                        }
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                    }

                    if sep != c.to_string() && met_parenthesis {
                        token.push( c );
                        continue;
                    }
                    if sep.is_empty() && !sep_second.is_empty() && sep_second != c.to_string() {
                        token.push( c );
                        continue;
                    }

                    if sep.is_empty() {
                        let is_an_env = regex::re_contains( &token, r"^[a-zA-Z0-9_]+=.*$" );
                        if !is_an_env && ( c == '\'' || c == '"' ) {
                            sep = c.to_string();
                            continue;
                        }

                        token.push( c );
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
                        token.push( c );
                    }
                } else {
                    if has_backslash {
                        has_backslash = false;
                        if sep == "\"" || sep == "'" {
                            token.push( '\\' );
                        }
                    }
                    token.push( c );
                }
            }
            if !token.is_empty() || semi_ok {
                if sep.is_empty() && !sep_made.is_empty() {
                    result.push( ( sep_made.clone(), token ) );
                } else {
                    result.push( ( sep.clone(), token ) );
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

            LineInfo { tokens:result, is_complete:is_line_complete }
        }

        pub fn tokens_to_redirections( tokens:&Tokens ) -> Result<( Tokens, Vec<Redirection> ), String>
        {
            let mut tokens_new = Vec::new();
            let mut redirects = Vec::new();
            let mut to_be_continued = false;
            let mut to_be_continued_s1 = String::new();
            let mut to_be_continued_s2 = String::new();

            for token in tokens {
                let sep = &token.0;
                if !sep.is_empty() && !to_be_continued {
                    tokens_new.push( token.clone() );
                    continue;
                }
                let word = &token.1;

                if to_be_continued {
                    if sep.is_empty() && word.starts_with( '&' ) {
                        return Err( String::from( "bad redirection syntax near &" ) );
                    }

                    let s3 = word.to_string();
                    if regex::re_contains( &to_be_continued_s1, r"^\d+$" ) {
                        if to_be_continued_s1 != "1" && to_be_continued_s1 != "2" {
                            return Err( String::from( "Bad file descriptor #3" ) );
                        }
                        let s1 = to_be_continued_s1.clone();
                        let s2 = to_be_continued_s2.clone();
                        redirects.push( ( s1, s2, s3 ) );
                    } else {
                        if !to_be_continued_s1.is_empty() {
                            tokens_new.push( ( sep.clone(), to_be_continued_s1.to_string() ) );
                        }
                        redirects.push( ( "1".to_string(), to_be_continued_s2.clone(), s3 ) );
                    }

                    to_be_continued = false;
                    continue;
                }

                let ptn1 = r"^( [^>]* )( >>? )( [^>]+ )$";
                let ptn2 = r"^( [^>]* )( >>? )$";
                if !regex::re_contains( word, r">" ) {
                    tokens_new.push( token.clone() );
                } else if regex::re_contains( word, ptn1 ) {
                    let re;
                    if let Ok( x ) = Regex::new( ptn1 ) {
                        re = x;
                    } else {
                        return Err( String::from( "Failed to build Regex" ) );
                    }

                    if let Some( caps ) = re.captures( word ) {
                        let s1 = caps.get( 1 ).unwrap().as_str();
                        let s2 = caps.get( 2 ).unwrap().as_str();
                        let s3 = caps.get( 3 ).unwrap().as_str();
                        if s3.starts_with( '&' ) && s3 != "&1" && s3 != "&2" {
                            return Err( String::from( "Bad file descriptor #1" ) );
                        }

                        if regex::re_contains( s1, r"^\d+$" ) {
                            if s1 != "1" && s1 != "2" {
                                return Err( String::from( "Bad file descriptor #2" ) );
                            }
                            redirects.push( ( s1.to_string(), s2.to_string(), s3.to_string() ) );
                        } else {
                            if !s1.is_empty() {
                                tokens_new.push( ( sep.clone(), s1.to_string() ) );
                            }
                            redirects.push( ( String::from( "1" ), s2.to_string(), s3.to_string() ) );
                        }
                    }
                } else if regex::re_contains( word, ptn2 ) {
                    let re;
                    if let Ok( x ) = Regex::new( ptn2 ) {
                        re = x;
                    } else {
                        return Err( String::from( "Failed to build Regex" ) );
                    }

                    if let Some( caps ) = re.captures( word ) {
                        let s1 = caps.get( 1 ).unwrap().as_str();
                        let s2 = caps.get( 2 ).unwrap().as_str();

                        to_be_continued = true;
                        to_be_continued_s1 = s1.to_string();
                        to_be_continued_s2 = s2.to_string();
                    }
                }
            }

            if to_be_continued {
                return Err( String::from( "redirection syntax error" ) );
            }

            Ok( ( tokens_new, redirects ) )
        }

        pub fn unquote( text:&str ) -> String
        {
            let mut new_str = String::from( text );
            for &c in ['"', '\''].iter() {
                if text.starts_with( c ) && text.ends_with( c ) {
                    new_str.remove( 0 );
                    new_str.pop();
                    break;
                }
            }
            new_str
        }
    }

    pub mod locust
    {
        /*
        use pest::Parser;
        use pest::iterators::Pairs;
        use pest::error::Error;

        #[derive( Parser )]
        #[grammar = "parsers/grammar.pest"]
        struct Locust;

        pub fn parse_lines( lines:&str ) -> 
        Result<Pairs<crate::parsers::locust::Rule>, Error<crate::parsers::locust::Rule>> {
            Locust::parse( Rule::EXP, lines )
        }
        */
        pub struct Pairs();
        /**/
        pub fn parse_lines( lines:&str ) -> Result<Pairs<crate::parsers::locust::Rule>, Error<crate::parsers::locust::Rule>>
        {
            //Locust::parse( Rule::EXP, lines )
            Ok( Pairs() )
        }


        #[derive( Debug )]
        pub enum Rule
        {
            If,
            IfElse,
        }
    }
    /*
    Fast, minimal float-parsing algorithm.*/
    pub mod float
    {

        #[macro_use] pub mod upper
        {
            use super::
            {
                bigint::{ nonzero, rview },
                limb::{ Limb, LIMB_BITS },
            };
            /// Extract the hi bits from the buffer.
           #[macro_export] macro_rules! upper
            {
                ( @1 $self:ident, $rview:ident, $t:ident, $fn:ident ) =>
                {{
                    $fn( $rview[0] as $t )
                }};
                
                ( @2 $self:ident, $rview:ident, $t:ident, $fn:ident ) =>
                {{
                    let r0 = $rview[0] as $t;
                    let r1 = $rview[1] as $t;
                    $fn( r0, r1 )
                }};
                
                ( @nonzero2 $self:ident, $rview:ident, $t:ident, $fn:ident ) =>
                {{
                    let ( v, n ) = upper!( @2 $self, $rview, $t, $fn );
                    ( v, n || nonzero( $self, 2 ) )
                }};
                
                ( @3 $self:ident, $rview:ident, $t:ident, $fn:ident ) =>
                {{
                    let r0 = $rview[0] as $t;
                    let r1 = $rview[1] as $t;
                    let r2 = $rview[2] as $t;
                    $fn( r0, r1, r2 )
                }};
                
                ( @nonzero3 $self:ident, $rview:ident, $t:ident, $fn:ident ) =>
                {{
                    let ( v, n ) = upper!( @3 $self, $rview, $t, $fn );
                    ( v, n || nonzero( $self, 3 ) )
                }};
            }
            /// Shift 64-bit integer to high 64-bits.
            #[inline] pub fn u64_to_hi64_1( r0:u64 ) -> ( u64, bool )
            {
                let ls = r0.leading_zeros();
                ( r0 << ls, false )
            }
            /// Shift 2 64-bit integers to high 64-bits.
            #[inline] pub fn u64_to_hi64_2( r0:u64, r1:u64 ) -> ( u64, bool )
            {
                let ls = r0.leading_zeros();
                let rs = 64 - ls;
                let v = match ls
                {
                    0 => r0,
                    _ => ( r0 << ls ) | ( r1 >> rs ),
                };
                let n = r1 << ls != 0;
                ( v, n )
            }            
            /// Shift 32-bit integer to high 64-bits.
            #[inline] pub fn u32_to_hi64_1( r0:u32 ) -> ( u64, bool ) 
            { u64_to_hi64_1( r0 as u64 ) }
            /// Shift 2 32-bit integers to high 64-bits.
            #[inline] pub fn u32_to_hi64_2( r0:u32, r1:u32 ) -> ( u64, bool )
            {
                let r0 = ( r0 as u64 ) << 32;
                let r1 = r1 as u64;
                u64_to_hi64_1( r0 | r1 )
            }
            /// Shift 3 32-bit integers to high 64-bits.
            #[inline] pub fn u32_to_hi64_3( r0:u32, r1:u32, r2:u32 ) -> ( u64, bool )
            {
                let r0 = r0 as u64;
                let r1 = ( r1 as u64 ) << 32;
                let r2 = r2 as u64;
                u64_to_hi64_2( r0, r1 | r2 )
            }

            /// Get the high 64 bits from the vector.
            #[inline( always )] pub fn upper( x:&[Limb] ) -> ( u64, bool ) 
            {
                let rslc = rview( x );
                match x.len()
                {
                    0 => ( 0, false ),
                    1 if LIMB_BITS == 32 => upper!( @1 x, rslc, u32, u32_to_hi64_1 ),
                    1 => upper!( @1 x, rslc, u64, u64_to_hi64_1 ),
                    2 if LIMB_BITS == 32 => upper!( @2 x, rslc, u32, u32_to_hi64_2 ),
                    2 => upper!( @2 x, rslc, u64, u64_to_hi64_2 ),
                    _ if LIMB_BITS == 32 => upper!( @nonzero3 x, rslc, u32, u32_to_hi64_3 ),
                    _ => upper!( @nonzero2 x, rslc, u64, u64_to_hi64_2 ),
                }
            }
        }

        pub mod bellerophon
        {
            
            //! An implementation of Clinger's Bellerophon algorithm.
            use super::extended::ExtendedFloat;
            use super::mask::{lower_n_halfway, lower_n_mask};
            use super::num::Float;
            use super::number::Number;
            use super::rounding::{round, round_nearest_tie_even};
            use super::table::BASE10_POWERS;
            
            /// Core implementation of the Bellerophon algorithm.
            pub fn bellerophon<F:Float>( num:&Number ) -> ExtendedFloat
            {
                let fp_zero = ExtendedFloat
                {
                    mant:0,
                    exp:0,
                };
                
                let fp_inf = ExtendedFloat
                {
                    mant:0,
                    exp:F::INFINITE_POWER,
                };

                if num.mantissa == 0 || num.exponent <= -0x1000 { return fp_zero; } 
                else if num.exponent >= 0x1000 { return fp_inf; }

                let exponent = num.exponent as i32 + BASE10_POWERS.bias;
                let small_index = exponent % BASE10_POWERS.step;
                let large_index = exponent / BASE10_POWERS.step;
                if exponent < 0 { return fp_zero; }
                if large_index as usize >= BASE10_POWERS.large.len() { return fp_inf; }
                
                let mut errors:u32 = 0;
                if num.many_digits { errors += error_halfscale(); }

                let mut fp = ExtendedFloat
                {
                    mant:num.mantissa,
                    exp:0,
                };
                match fp.mant.overflowing_mul( BASE10_POWERS.get_small_int( small_index as usize ) )
                {
                    ( _, true ) =>
                    {
                        normalize( &mut fp );
                        fp = mul( &fp, &BASE10_POWERS.get_small( small_index as usize ) );
                        errors += error_halfscale();
                    },
                    
                    ( mant, false ) =>
                    {
                        fp.mant = mant;
                        normalize( &mut fp );
                    },
                }
                
                fp = mul( &fp, &BASE10_POWERS.get_large( large_index as usize ) );
                if errors > 0
                {
                    errors += 1;
                }

                errors += error_halfscale();                
                let shift = normalize( &mut fp );
                errors <<= shift;
                fp.exp += F::EXPONENT_BIAS;                
                if -fp.exp + 1 > 65 { return fp_zero; }                
                if !error_is_accurate::<F>( errors, &fp )
                {
                    fp.exp += F::INVALID_FP;
                    return fp;
                }
                
                if -fp.exp + 1 == 65 { return fp_zero; }
                round::<F, _>( &mut fp, |f, s| 
                {
                    round_nearest_tie_even( f, s, |is_odd, is_halfway, is_above|
                    {
                        is_above || ( is_odd && is_halfway )
                    } );
                } );
                fp
            }
            /// Get the full error scale.
            #[inline( always )] const fn error_scale() -> u32 { 8 }
            /// Get the half error scale.
            #[inline( always )] const fn error_halfscale() -> u32 { error_scale() / 2 }
            /// Determine if the number of errors is tolerable for float precision.
            fn error_is_accurate<F:Float>( errors:u32, fp:&ExtendedFloat ) -> bool
            {
                debug_assert!( fp.exp >= -64 );
                let mantissa_shift = 64 - F::MANTISSA_SIZE - 1;
                let extrabits = match fp.exp <= -mantissa_shift 
                {
                    true => 1 - fp.exp,
                    false => 64 - F::MANTISSA_SIZE - 1,
                };
                let maskbits = extrabits as u64;
                let errors = errors as u64;
                if extrabits > 64 { !fp.mant.overflowing_add( errors ).1 } 
                else
                {
                    let mask = lower_n_mask( maskbits );
                    let extra = fp.mant & mask;
                    let halfway = lower_n_halfway( maskbits );
                    let cmp1 = halfway.wrapping_sub( errors ) < extra;
                    let cmp2 = extra < halfway.wrapping_add( errors );
                    !( cmp1 && cmp2 )
                }
            }
            /// Normalize float-point number.
            pub fn normalize( fp:&mut ExtendedFloat ) -> i32
            {
                if fp.mant != 0
                {
                    let shift = fp.mant.leading_zeros() as i32;
                    fp.mant <<= shift;
                    fp.exp -= shift;
                    shift
                } 
                else { 0 }
            }
            /// Multiply two normalized extended-precision floats, as if by `a*b`.
            pub fn mul( x:&ExtendedFloat, y:&ExtendedFloat ) -> ExtendedFloat
            {
                debug_assert!( x.mant >> 32 != 0 );
                debug_assert!( y.mant >> 32 != 0 );

                const LOMASK:u64 = 0xffff_ffff;
                let x1 = x.mant >> 32;
                let x0 = x.mant & LOMASK;
                let y1 = y.mant >> 32;
                let y0 = y.mant & LOMASK;
                let x1_y0 = x1 * y0;
                let x0_y1 = x0 * y1;
                let x0_y0 = x0 * y0;
                let x1_y1 = x1 * y1;
                let mut tmp = ( x1_y0 & LOMASK ) + ( x0_y1 & LOMASK ) + ( x0_y0 >> 32 );
                tmp += 1 << ( 32 - 1 );
                ExtendedFloat
                {
                    mant:x1_y1 + ( x1_y0 >> 32 ) + ( x0_y1 >> 32 ) + ( tmp >> 32 ),
                    exp:x.exp + y.exp + 64,
                }
            }
            /// Precalculated powers of base N for the Bellerophon algorithm.
            pub struct BellerophonPowers
            {
                pub small:&'static [u64],
                pub large:&'static [u64],
                pub small_int:&'static [u64],
                pub step:i32,
                pub bias:i32,
                /// ceil( log2( radix ) ) scaled as a multiplier.
                pub log2:i64,
                /// Bitshift for the log2 multiplier.
                pub log2_shift:i32,
            }

            /// Allow indexing of values without bounds checking
            impl BellerophonPowers
            {
                #[inline] pub fn get_small( &self, index:usize ) -> ExtendedFloat
                {
                    let mant = self.small[index];
                    let exp = ( 1 - 64 ) + ( ( self.log2 * index as i64 ) >> self.log2_shift );
                    ExtendedFloat
                    {
                        mant,
                        exp:exp as i32,
                    }
                }

                #[inline] pub fn get_large( &self, index:usize ) -> ExtendedFloat
                {
                    let mant = self.large[index];
                    let biased_e = index as i64 * self.step as i64 - self.bias as i64;
                    let exp = ( 1 - 64 ) + ( ( self.log2 * biased_e ) >> self.log2_shift );
                    ExtendedFloat
                    {
                        mant,
                        exp:exp as i32,
                    }
                }

                #[inline] pub fn get_small_int( &self, index:usize ) -> u64 { self.small_int[index] }
            }
        }

        pub mod bigint
        {
            //! A simple big-integer type for slow path algorithms.
            use super::
            {
                heapvec::{ HeapVec },
                num::{ int_pow_fast_path, FastPathRadix },
            };

            use ::
            {
                *,
            };
            /// Number of bits in a Bigint.
            pub const BIGINT_BITS:usize = 4000;

            /// The number of limbs for the bigint.
            pub const BIGINT_LIMBS:usize = BIGINT_BITS / LIMB_BITS;

            pub type VecType = HeapVec;

            /// Storage for a big integer type.
            #[derive( Clone, PartialEq, Eq )]
            pub struct Bigint
            {
                /// Significant digits for the float, stored in a big integer in LE order.
                pub data:VecType,
            }
            
            impl Bigint
            {
                /// Construct a bigint representing 0.
                #[inline( always )]
                pub fn new() -> Self
                {
                    Self { data:VecType::new(), }
                }

                /// Construct a bigint from an integer.
                #[inline( always )] pub fn from_u64( value:u64 ) -> Self
                {
                    Self
                    {
                        data:VecType::from_u64( value ),
                    }
                }

                #[inline( always )] pub fn hi64( &self ) -> ( u64, bool )
                {
                    self.data.hi64()
                }

                /// Multiply and assign as if by exponentiation by a power.
                #[inline]
                pub fn pow( &mut self, base:u32, exp:u32 ) -> Option<()> {
                    debug_assert!( base == 2 || base == 5 || base == 10 );
                    if base % 5 == 0 {
                        pow( &mut self.data, exp )?;
                    }
                    if base % 2 == 0 {
                        shl( &mut self.data, exp as usize )?;
                    }
                    Some( () )
                }

                /// Calculate the bit-length of the big-integer.
                #[inline]
                pub fn bit_length( &self ) -> u32 {
                    bit_length( &self.data )
                }
            }

            impl ops::MulAssign<&Bigint> for Bigint {
                fn mul_assign( &mut self, rhs:&Bigint ) {
                    self.data *= &rhs.data;
                }
            }

            /// REVERSE VIEW

            /// Reverse, immutable view of a sequence.
            pub struct ReverseView<'a, T:'a> {
                inner:&'a [T],
            }

            impl<'a, T> ops::Index<usize> for ReverseView<'a, T> {
                type Output = T;

                #[inline]
                fn index( &self, index:usize ) -> &T {
                    let len = self.inner.len();
                    &( *self.inner )[len - index - 1]
                }
            }

            /// Create a reverse view of the vector for indexing.
            #[inline]
            pub fn rview( x:&[Limb] ) -> ReverseView<Limb> {
                ReverseView {
                    inner:x,
                }
            }

            /// Compare `x` to `y`, in little-endian order.
            #[inline]
            pub fn compare( x:&[Limb], y:&[Limb] ) -> cmp::Ordering {
                match x.len().cmp( &y.len() ) {
                    cmp::Ordering::Equal => {
                        let iter = x.iter().rev().zip( y.iter().rev() );
                        for ( &xi, yi ) in iter {
                            match xi.cmp( yi ) {
                                cmp::Ordering::Equal => (),
                                ord => return ord,
                            }
                        }
                        
                        cmp::Ordering::Equal
                    },
                    ord => ord,
                }
            }

            /// Normalize the integer, so any leading zero values are removed.
            #[inline]
            pub fn normalize( x:&mut VecType ) {
                while let Some( &value ) = x.get( x.len().wrapping_sub( 1 ) ) {
                    if value == 0 {
                        unsafe { x.set_len( x.len() - 1 ) };
                    } else {
                        break;
                    }
                }
            }

            /// Get if the big integer is normalized.
            #[inline]
            #[allow( clippy::match_like_matches_macro )]
            pub fn is_normalized( x:&[Limb] ) -> bool {
                match x.get( x.len().wrapping_sub( 1 ) ) {
                    Some( &0 ) => false,
                    _ => true,
                }
            }

            /// Create StackVec from u64 value.
            #[inline( always )]
            #[allow( clippy::branches_sharing_code )]
            pub fn from_u64( x:u64 ) -> VecType {
                let mut vec = VecType::new();
                debug_assert!( vec.capacity() >= 2 );
                if LIMB_BITS == 32 {
                    vec.try_push( x as Limb ).unwrap();
                    vec.try_push( ( x >> 32 ) as Limb ).unwrap();
                } else {
                    vec.try_push( x as Limb ).unwrap();
                }
                vec.normalize();
                vec
            }
            /// Check if any of the remaining bits are non-zero.
            #[inline]
            pub fn nonzero( x:&[Limb], rindex:usize ) -> bool {
                debug_assert!( rindex <= x.len() );

                let len = x.len();
                let slc = &x[..len - rindex];
                slc.iter().rev().any( |&x| x != 0 )
            }

            /// Shift 32-bit integer to high 64-bits.
            #[inline]
            pub fn u32_to_hi64_1( r0:u32 ) -> ( u64, bool ) {
                u64_to_hi64_1( r0 as u64 )
            }

            /// Shift 2 32-bit integers to high 64-bits.
            #[inline]
            pub fn u32_to_hi64_2( r0:u32, r1:u32 ) -> ( u64, bool ) {
                let r0 = ( r0 as u64 ) << 32;
                let r1 = r1 as u64;
                u64_to_hi64_1( r0 | r1 )
            }

            /// Shift 3 32-bit integers to high 64-bits.
            #[inline]
            pub fn u32_to_hi64_3( r0:u32, r1:u32, r2:u32 ) -> ( u64, bool ) {
                let r0 = r0 as u64;
                let r1 = ( r1 as u64 ) << 32;
                let r2 = r2 as u64;
                u64_to_hi64_2( r0, r1 | r2 )
            }

            /// Shift 64-bit integer to high 64-bits.
            #[inline]
            pub fn u64_to_hi64_1( r0:u64 ) -> ( u64, bool ) {
                let ls = r0.leading_zeros();
                ( r0 << ls, false )
            }

            /// Shift 2 64-bit integers to high 64-bits.
            #[inline]
            pub fn u64_to_hi64_2( r0:u64, r1:u64 ) -> ( u64, bool ) {
                let ls = r0.leading_zeros();
                let rs = 64 - ls;
                let v = match ls {
                    0 => r0,
                    _ => ( r0 << ls ) | ( r1 >> rs ),
                };
                let n = r1 << ls != 0;
                ( v, n )
            }

            /// Extract the hi bits from the buffer.
            macro_rules! hi {
                ( @1 $self:ident, $rview:ident, $t:ident, $fn:ident ) => {{
                    $fn( $rview[0] as $t )
                }};
                
                ( @2 $self:ident, $rview:ident, $t:ident, $fn:ident ) => {{
                    let r0 = $rview[0] as $t;
                    let r1 = $rview[1] as $t;
                    $fn( r0, r1 )
                }};
                
                ( @nonzero2 $self:ident, $rview:ident, $t:ident, $fn:ident ) => {{
                    let ( v, n ) = hi!( @2 $self, $rview, $t, $fn );
                    ( v, n || nonzero( $self, 2 ) )
                }};
                
                ( @3 $self:ident, $rview:ident, $t:ident, $fn:ident ) => {{
                    let r0 = $rview[0] as $t;
                    let r1 = $rview[1] as $t;
                    let r2 = $rview[2] as $t;
                    $fn( r0, r1, r2 )
                }};
                
                ( @nonzero3 $self:ident, $rview:ident, $t:ident, $fn:ident ) => {{
                    let ( v, n ) = hi!( @3 $self, $rview, $t, $fn );
                    ( v, n || nonzero( $self, 3 ) )
                }};
            }

            /// Get the high 64 bits from the vector.
            #[inline( always )]
            pub fn hi64( x:&[Limb] ) -> ( u64, bool ) {
                let rslc = rview( x );
                match x.len() {
                    0 => ( 0, false ),
                    1 if LIMB_BITS == 32 => hi!( @1 x, rslc, u32, u32_to_hi64_1 ),
                    1 => hi!( @1 x, rslc, u64, u64_to_hi64_1 ),
                    2 if LIMB_BITS == 32 => hi!( @2 x, rslc, u32, u32_to_hi64_2 ),
                    2 => hi!( @2 x, rslc, u64, u64_to_hi64_2 ),
                    _ if LIMB_BITS == 32 => hi!( @nonzero3 x, rslc, u32, u32_to_hi64_3 ),
                    _ => hi!( @nonzero2 x, rslc, u64, u64_to_hi64_2 ),
                }
            }

            /// MulAssign by a power of 5.
            pub fn pow( x:&mut VecType, mut exp:u32 ) -> Option<()>
            {
                let small_step = if LIMB_BITS == 32 {
                    13
                } else {
                    27
                };
                let max_native = ( 5 as Limb ).pow( small_step );
                while exp >= small_step {
                    small_mul( x, max_native )?;
                    exp -= small_step;
                }
                if exp != 0 {
                    let small_power = unsafe { int_pow_fast_path( exp as usize, FastPathRadix::Five ) };
                    small_mul( x, small_power as Limb )?;
                }
                Some( () )
            }

            /// Add two small integers and return the resulting value and if overflow happens.
            #[inline( always )]
            pub fn scalar_add( x:Limb, y:Limb ) -> ( Limb, bool ) {
                x.overflowing_add( y )
            }

            /// Multiply two small integers ( with carry ) ( and return the overflow contribution ).
            #[inline( always )]
            pub fn scalar_mul( x:Limb, y:Limb, carry:Limb ) -> ( Limb, Limb ) {
                let z:Wide = ( x as Wide ) * ( y as Wide ) + ( carry as Wide );
                ( z as Limb, ( z >> LIMB_BITS ) as Limb )
            }

            /// Add small integer to bigint starting from offset.
            #[inline]
            pub fn small_add_from( x:&mut VecType, y:Limb, start:usize ) -> Option<()> {
                let mut index = start;
                let mut carry = y;
                while carry != 0 && index < x.len() {
                    let result = scalar_add( x[index], carry );
                    x[index] = result.0;
                    carry = result.1 as Limb;
                    index += 1;
                }
                
                if carry != 0 {
                    x.try_push( carry )?;
                }
                Some( () )
            }

            /// Add small integer to bigint.
            #[inline( always )]
            pub fn small_add( x:&mut VecType, y:Limb ) -> Option<()> {
                small_add_from( x, y, 0 )
            }

            /// Multiply bigint by small integer.
            #[inline]
            pub fn small_mul( x:&mut VecType, y:Limb ) -> Option<()> {
                let mut carry = 0;
                for xi in x.iter_mut() {
                    let result = scalar_mul( *xi, y, carry );
                    *xi = result.0;
                    carry = result.1;
                }
                
                if carry != 0 {
                    x.try_push( carry )?;
                }
                Some( () )
            }

            /// Add bigint to bigint starting from offset.
            pub fn large_add_from( x:&mut VecType, y:&[Limb], start:usize ) -> Option<()> {
                if y.len() > x.len().saturating_sub( start ) {
                    x.try_resize( y.len() + start, 0 )?;
                }
                
                let mut carry = false;
                for ( index, &yi ) in y.iter().enumerate() {
                    let xi = x.get_mut( start + index ).unwrap();                    
                    let result = scalar_add( *xi, yi );
                    *xi = result.0;
                    let mut tmp = result.1;
                    if carry {
                        let result = scalar_add( *xi, 1 );
                        *xi = result.0;
                        tmp |= result.1;
                    }
                    carry = tmp;
                }
                
                if carry {
                    small_add_from( x, 1, y.len() + start )?;
                }
                Some( () )
            }

            /// Add bigint to bigint.
            #[inline( always )]
            pub fn large_add( x:&mut VecType, y:&[Limb] ) -> Option<()> {
                large_add_from( x, y, 0 )
            }

            /// Grade-school multiplication algorithm.
            /// In short, Karatsuba multiplication is never worthwhile for out use-case.
            pub fn long_mul( x:&[Limb], y:&[Limb] ) -> Option<VecType>
            {
                let mut z = VecType::try_from( x )?;
                if !y.is_empty() {
                    let y0 = y[0];
                    small_mul( &mut z, y0 )?;

                    for ( index, &yi ) in y.iter().enumerate().skip( 1 ) {
                        if yi != 0 {
                            let mut zi = VecType::try_from( x )?;
                            small_mul( &mut zi, yi )?;
                            large_add_from( &mut z, &zi, index )?;
                        }
                    }
                }

                z.normalize();
                Some( z )
            }

            /// Multiply bigint by bigint using grade-school multiplication algorithm.
            #[inline( always )]
            pub fn large_mul( x:&mut VecType, y:&[Limb] ) -> Option<()> {
                if y.len() == 1 {
                    small_mul( x, y[0] )?;
                } else {
                    *x = long_mul( y, x )?;
                }
                Some( () )
            }
            
            /// Shift-left `n` bits inside a buffer.
            #[inline]
            pub fn shl_bits( x:&mut VecType, n:usize ) -> Option<()>
            {
                debug_assert!( n != 0 );
                debug_assert!( n < LIMB_BITS );
                let rshift = LIMB_BITS - n;
                let lshift = n;
                let mut prev:Limb = 0;
                for xi in x.iter_mut() {
                    let tmp = *xi;
                    *xi <<= lshift;
                    *xi |= prev >> rshift;
                    prev = tmp;
                }

                // Always push the carry, even if it creates a non-normal result.
                let carry = prev >> rshift;
                if carry != 0 {
                    x.try_push( carry )?;
                }

                Some( () )
            }

            /// Shift-left `n` limbs inside a buffer.
            #[inline]
            pub fn shl_limbs( x:&mut VecType, n:usize ) -> Option<()> {
                debug_assert!( n != 0 );
                if n + x.len() > x.capacity() {
                    None
                } else if !x.is_empty() {
                    let len = n + x.len();
                    unsafe {
                        let src = x.as_ptr();
                        let dst = x.as_mut_ptr().add( n );
                        ptr::copy( src, dst, x.len() );
                        ptr::write_bytes( x.as_mut_ptr(), 0, n );
                        x.set_len( len );
                    }
                    Some( () )
                } else {
                    Some( () )
                }
            }

            /// Shift-left buffer by n bits.
            #[inline]
            pub fn shl( x:&mut VecType, n:usize ) -> Option<()> {
                let rem = n % LIMB_BITS;
                let div = n / LIMB_BITS;
                if rem != 0 {
                    shl_bits( x, rem )?;
                }
                if div != 0 {
                    shl_limbs( x, div )?;
                }
                Some( () )
            }

            /// Get number of leading zero bits in the storage.
            #[inline]
            pub fn leading_zeros( x:&[Limb] ) -> u32 {
                let length = x.len();
                if let Some( &value ) = x.get( length.wrapping_sub( 1 ) ) {
                    value.leading_zeros()
                } else {
                    0
                }
            }

            /// Calculate the bit-length of the big-integer.
            #[inline]
            pub fn bit_length( x:&[Limb] ) -> u32 {
                let nlz = leading_zeros( x );
                LIMB_BITS as u32 * x.len() as u32 - nlz
            }

            ///  Type for a single limb of the big integer.
            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub type Limb = u64;
            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub type Wide = u128;
            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub const LIMB_BITS:usize = 64;

            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub type Limb = u32;
            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub type Wide = u64;
            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub const LIMB_BITS:usize = 32;
        }

        pub mod extended
        {
            use super::num::Float;

            /// Extended precision floating-point type.
            #[derive( Clone, Copy, Debug, PartialEq, Eq )]
            pub struct ExtendedFloat 
            {
                /// Mantissa for the extended-precision float.
                pub mant:u64,
                /// Binary exponent for the extended-precision float.
                pub exp:i32,
            }

            /// Converts an `ExtendedFloat` to the closest machine float type.
            #[inline( always )] pub fn extended_to_float<F:Float>( x:ExtendedFloat ) -> F
            {
                let mut word = x.mant;
                word |= ( x.exp as u64 ) << F::MANTISSA_SIZE;
                F::from_bits( word )
            }
        }

        pub mod fpu
        {
            //! Platform-specific, assembly instructions to avoid rounding on architectures with FPUs.
            mod fpu_precision
            {
                pub fn set_precision<T>(){}
            }
        }

        pub mod heapvec
        {
            use ::
            {
                cmp::{ Ord, Ordering, PartialOrd },
                ops::{ Deref, DerefMut, MulAssign },
                parsers::float::
                {
                    bigint::{ long_mul },
                    limb::{ self, BIGINT_LIMBS, Limb, LIMB_BITS, Wide },
                    upper::{ self },
                },
                *
            };
            /// Simple heap vector implementation.
            #[derive( Clone )] pub struct HeapVec
            {
                /// The heap-allocated buffer for the elements.
                data:Vec<Limb>,
            }
            
            impl HeapVec
            {
                /// Construct an empty vector.
                pub const fn new() -> Self
                {
                    Self
                    {
                        data:Vec::with_capacity( limb::BIGINT_LIMBS ),
                    }
                }

                /// Construct a vector from an existing slice.
                #[inline] pub fn try_from( x:&[Limb] ) -> Option<Self>
                {
                    let mut vec = Self::new();
                    vec.try_extend( x )?;
                    Some( vec )
                }

                /// Sets the length of a vector.
                #[inline] pub unsafe fn set_len( &mut self, len:usize )
                {
                    unsafe
                    {
                        debug_assert!( len <= BIGINT_LIMBS );
                        self.data.set_len( len )
                    };
                }

                /// The number of elements stored in the vector.
                #[inline] pub fn len( &self ) -> usize
                { self.data.len() }

                /// If the vector is empty.
                #[inline] pub fn is_empty( &self ) -> bool
                { self.len() == 0 }

                /// The number of items the vector can hold.
                #[inline] pub fn capacity( &self ) -> usize
                { self.data.capacity() }

                /// Append an item to the vector.
                #[inline] pub fn try_push( &mut self, value:Limb ) -> Option<()>
                {
                    self.data.push( value );
                    Some( () )
                }

                /// Remove an item from the end of the vector and return it, or None if empty.
                #[inline] pub fn pop( &mut self ) -> Option<Limb>
                { self.data.pop() }

                /// Copy elements from a slice and append them to the vector.
                #[inline] pub fn try_extend( &mut self, slc:&[Limb] ) -> Option<()>
                {
                    self.data.extend_from_slice( slc );
                    Some( () )
                }

                /// Try to resize the buffer.
                #[inline] pub fn try_resize( &mut self, len:usize, value:Limb ) -> Option<()>
                {
                    self.data.resize( len, value );
                    Some( () )
                }

                /// Get the high 64 bits from the vector.
                #[inline( always )] pub fn hi64( &self ) -> ( u64, bool )
                { upper::upper( &self.data ) }
                
                /// Create StackVec from u64 value.
                #[inline( always )] pub fn from_u64( x:u64 ) -> Self
                { from_u64( x ) }
                
                /// Normalize the integer, so any leading zero values are removed.
                #[inline] pub fn normalize( &mut self )
                { normalize( self ) }
                /// Get if the big integer is normalized.
                #[inline] pub fn is_normalized( &self ) -> bool
                { is_normalized( self ) }
                /// AddAssign small integer.
                #[inline] pub fn add_small( &mut self, y:Limb ) -> Option<()>
                { small_add( self, y ) }
                /// MulAssign small integer.
                #[inline]
                pub fn mul_small( &mut self, y:Limb ) -> Option<()>
                { small_mul( self, y ) }
            }

            impl Eq for HeapVec {}
            
            impl PartialEq for HeapVec
            {
                #[inline] fn eq( &self, other:&Self ) -> bool
                {
                    self.len() == other.len() && self.deref() == other.deref()
                }
            }

            impl PartialOrd for HeapVec
            {
                #[inline] fn partial_cmp( &self, other:&Self ) -> Option<cmp::Ordering>
                { Some( compare( self, other ) ) }
            }

            impl Ord for HeapVec
            {
                #[inline] fn cmp( &self, other:&Self ) -> cmp::Ordering
                { compare( self, other ) }
            }

            impl Deref for HeapVec
            {
                type Target = [ Limb ];
                #[inline] fn deref( &self ) -> &[ Limb ]
                { &self.data }
            }

            impl DerefMut for HeapVec
            {
                #[inline] fn deref_mut( &mut self ) -> &mut [ Limb ]
                { &mut self.data }
            }

            impl MulAssign<&[ Limb ]> for HeapVec
            {
                #[inline] fn mul_assign( &mut self, rhs:&[ Limb ] )
                { large_mul( self, rhs ).unwrap(); }
            }
            /// Compare `x` to `y`, in little-endian order.
            #[inline] pub fn compare( x:&[Limb], y:&[Limb] ) -> cmp::Ordering
            {
                match x.len().cmp( &y.len() )
                {
                    Ordering::Equal =>
                    {
                        let iter = x.iter().rev().zip( y.iter().rev() );
                        for ( &xi, yi ) in iter
                        {
                            match xi.cmp( yi )
                            {
                                Ordering::Equal => (),
                                ord => return ord,
                            }
                        }
                        
                        Ordering::Equal
                    },
                    ord => ord,
                }
            }
            /// Create StackVec from u64 value.
            #[inline( always )] pub fn from_u64( x:u64 ) -> HeapVec
            {
                let mut vec = HeapVec::new();
                debug_assert!( vec.capacity() >= 2 );
                if LIMB_BITS == 32
                {
                    vec.try_push( x as Limb ).unwrap();
                    vec.try_push( ( x >> 32 ) as Limb ).unwrap();
                } 
                else { vec.try_push( x as Limb ).unwrap(); }
                vec.normalize();
                vec
            }
            /// Normalize the integer, so any leading zero values are removed.
            #[inline] pub fn normalize( x:&mut HeapVec )
            {
                unsafe
                {
                    while let Some( &value ) = x.get( x.len().wrapping_sub( 1 ) )
                    {
                        if value == 0 { x.set_len( x.len() - 1 ); } 
                        else { break; }
                    }
                }
            }
            /// Get if the big integer is normalized.
            #[inline] pub fn is_normalized( x:&[Limb] ) -> bool
            {
                match x.get( x.len().wrapping_sub( 1 ) )
                {
                    Some( &0 ) => false,
                    _ => true,
                }
            }
            /// Add small integer to bigint starting from offset.
            #[inline] pub fn small_add_from( x:&mut HeapVec, y:Limb, start:usize ) -> Option<()>
            {
                let mut index = start;
                let mut carry = y;
                while carry != 0 && index < x.len()
                {
                    let result = scalar_add( x[index], carry );
                    x[index] = result.0;
                    carry = result.1 as Limb;
                    index += 1;
                }
                if carry != 0 { x.try_push( carry )?; }
                Some( () )
            }
            /// Add small integer to bigint.
            #[inline( always )] pub fn small_add( x:&mut HeapVec, y:Limb ) -> Option<()>
            { small_add_from( x, y, 0 ) }
            /// Multiply bigint by small integer.
            #[inline] pub fn small_mul( x:&mut HeapVec, y:Limb ) -> Option<()>
            {
                let mut carry = 0;
                for xi in x.iter_mut()
                {
                    let result = scalar_mul( *xi, y, carry );
                    *xi = result.0;
                    carry = result.1;
                }                
                if carry != 0 { x.try_push( carry )?; }
                Some( () )
            }
            /// Multiply bigint by bigint using grade-school multiplication algorithm.
            #[inline( always )] pub fn large_mul( x:&mut HeapVec, y:&[Limb] ) -> Option<()>
            {
                if y.len() == 1 { small_mul( x, y[0] )?; }
                else
                { *x = long_mul( y, x )?; }
                Some( () )
            }
            /// Add two small integers and return the resulting value and if overflow happens.
            #[inline( always )] pub fn scalar_add( x:Limb, y:Limb ) -> ( Limb, bool )
            { x.overflowing_add( y ) }

            /// Multiply two small integers ( with carry ) ( and return the overflow contribution ).
            #[inline( always )] pub fn scalar_mul( x:Limb, y:Limb, carry:Limb ) -> ( Limb, Limb )
            {
                let z:Wide = ( x as Wide ) * ( y as Wide ) + ( carry as Wide );
                ( z as Limb, ( z >> LIMB_BITS ) as Limb )
            }            
        }

        pub mod libm
        {
            //! A small number of math routines for floats and doubles.
            macro_rules! i 
            {
                ( $array:ident, $index:expr ) => 
                {
                    unsafe { *$array.get_unchecked( $index ) }
                };
            }

            pub fn powf( x:f32, y:f32 ) -> f32 
            {
                const BP:[f32; 2] = [1.0, 1.5];
                const DP_H:[f32; 2] = [0.0, 5.84960938e-01];
                const DP_L:[f32; 2] = [0.0, 1.56322085e-06];
                const TWO24:f32 = 16777216.0;
                const HUGE:f32 = 1.0e30;
                const TINY:f32 = 1.0e-30;
                const L1:f32 = 6.0000002384e-01;
                const L2:f32 = 4.2857143283e-01;
                const L3:f32 = 3.3333334327e-01;
                const L4:f32 = 2.7272811532e-01;
                const L5:f32 = 2.3066075146e-01;
                const L6:f32 = 2.0697501302e-01;
                const P1:f32 = 1.6666667163e-01;
                const P2:f32 = -2.7777778450e-03;
                const P3:f32 = 6.6137559770e-05;
                const P4:f32 = -1.6533901999e-06;
                const P5:f32 = 4.1381369442e-08;
                const LG2:f32 = 6.9314718246e-01;
                const LG2_H:f32 = 6.93145752e-01;
                const LG2_L:f32 = 1.42860654e-06;
                const OVT:f32 = 4.2995665694e-08;
                const CP:f32 = 9.6179670095e-01;
                const CP_H:f32 = 9.6191406250e-01;
                const CP_L:f32 = -1.1736857402e-04;
                const IVLN2:f32 = 1.4426950216e+00;
                const IVLN2_H:f32 = 1.4426879883e+00;
                const IVLN2_L:f32 = 7.0526075433e-06;

                let mut z:f32;
                let mut ax:f32;
                let z_h:f32;
                let z_l:f32;
                let mut p_h:f32;
                let mut p_l:f32;
                let y1:f32;
                let mut t1:f32;
                let t2:f32;
                let mut r:f32;
                let s:f32;
                let mut sn:f32;
                let mut t:f32;
                let mut u:f32;
                let mut v:f32;
                let mut w:f32;
                let i:i32;
                let mut j:i32;
                let mut k:i32;
                let mut yisint:i32;
                let mut n:i32;
                let hx:i32;
                let hy:i32;
                let mut ix:i32;
                let iy:i32;
                let mut is:i32;

                hx = x.to_bits() as i32;
                hy = y.to_bits() as i32;
                ix = hx & 0x7fffffff;
                iy = hy & 0x7fffffff;
                
                if iy == 0 {
                    return 1.0;
                }
                
                if hx == 0x3f800000 {
                    return 1.0;
                }
                
                if ix > 0x7f800000 || iy > 0x7f800000 {
                    return x + y;
                }
                
                yisint = 0;
                if hx < 0 {
                    if iy >= 0x4b800000 {
                        yisint = 2;
                    } else if iy >= 0x3f800000 {
                        k = ( iy >> 23 ) - 0x7f;
                        j = iy >> ( 23 - k );
                        if ( j << ( 23 - k ) ) == iy {
                            yisint = 2 - ( j & 1 );
                        }
                    }
                }

                if iy == 0x7f800000 {
                    if ix == 0x3f800000 {
                        return 1.0;
                    } else if ix > 0x3f800000 {
                        return if hy >= 0 {
                            y
                        } else {
                            0.0
                        };
                    } else {
                        return if hy >= 0 {
                            0.0
                        } else {
                            -y
                        };
                    }
                }

                if iy == 0x3f800000
                {
                    return if hy >= 0 { x } 
                    else { 1.0 / x };
                }

                if hy == 0x40000000 { return x * x; }

                if hy == 0x3f000000 && hx >= 0 { return sqrtf( x ); }

                ax = fabsf( x );
                if ix == 0x7f800000 || ix == 0 || ix == 0x3f800000
                {
                    z = ax;
                    if hy < 0 { z = 1.0 / z; }
                    if hx < 0
                    {
                        if ( ( ix - 0x3f800000 ) | yisint ) == 0 { z = ( z - z ) / ( z - z ); }
                        else if yisint == 1 { z = -z; }
                    }
                    return z;
                }

                sn = 1.0;
                if hx < 0 {
                    if yisint == 0 {
                        return ( x - x ) / ( x - x );
                    }

                    if yisint == 1 {
                        sn = -1.0;
                    }
                }                

                if iy > 0x4d000000 {
                    if ix < 0x3f7ffff8 {
                        return if hy < 0 {
                            sn * HUGE * HUGE
                        } else {
                            sn * TINY * TINY
                        };
                    }

                    if ix > 0x3f800007
                    {
                        return if hy > 0
                        { sn * HUGE * HUGE } 
                        else 
                        { sn * TINY * TINY };
                    }
                    
                    t = ax - 1.;
                    w = ( t * t ) * ( 0.5 - t * ( 0.333333333333 - t * 0.25 ) );
                    u = IVLN2_H * t;
                    v = t * IVLN2_L - w * IVLN2;
                    t1 = u + v;
                    is = t1.to_bits() as i32;
                    t1 = f32::from_bits( is as u32 & 0xfffff000 );
                    t2 = v - ( t1 - u );
                } else {
                    let mut s2:f32;
                    let mut s_h:f32;
                    let s_l:f32;
                    let mut t_h:f32;
                    let mut t_l:f32;
                    n = 0;
                    if ix < 0x00800000 {
                        ax *= TWO24;
                        n -= 24;
                        ix = ax.to_bits() as i32;
                    }
                    n += ( ( ix ) >> 23 ) - 0x7f;
                    j = ix & 0x007fffff;
                    ix = j | 0x3f800000;
                    if j <= 0x1cc471 {
                        k = 0;
                    } else if j < 0x5db3d7 {
                        k = 1;
                    } else {
                        k = 0;
                        n += 1;
                        ix -= 0x00800000;
                    }
                    ax = f32::from_bits( ix as u32 );
                    u = ax - i!( BP, k as usize );
                    v = 1.0 / ( ax + i!( BP, k as usize ) );
                    s = u * v;
                    s_h = s;
                    is = s_h.to_bits() as i32;
                    s_h = f32::from_bits( is as u32 & 0xfffff000 );
                    is = ( ( ( ix as u32 >> 1 ) & 0xfffff000 ) | 0x20000000 ) as i32;
                    t_h = f32::from_bits( is as u32 + 0x00400000 + ( ( k as u32 ) << 21 ) );
                    t_l = ax - ( t_h - i!( BP, k as usize ) );
                    s_l = v * ( ( u - s_h * t_h ) - s_h * t_l );
                    s2 = s * s;
                    r = s2 * s2 * ( L1 + s2 * ( L2 + s2 * ( L3 + s2 * ( L4 + s2 * ( L5 + s2 * L6 ) ) ) ) );
                    r += s_l * ( s_h + s );
                    s2 = s_h * s_h;
                    t_h = 3.0 + s2 + r;
                    is = t_h.to_bits() as i32;
                    t_h = f32::from_bits( is as u32 & 0xfffff000 );
                    t_l = r - ( ( t_h - 3.0 ) - s2 );
                    u = s_h * t_h;
                    v = s_l * t_h + t_l * s;
                    p_h = u + v;
                    is = p_h.to_bits() as i32;
                    p_h = f32::from_bits( is as u32 & 0xfffff000 );
                    p_l = v - ( p_h - u );
                    z_h = CP_H * p_h;
                    z_l = CP_L * p_h + p_l * CP + i!( DP_L, k as usize );
                    t = n as f32;
                    t1 = ( ( z_h + z_l ) + i!( DP_H, k as usize ) ) + t;
                    is = t1.to_bits() as i32;
                    t1 = f32::from_bits( is as u32 & 0xfffff000 );
                    t2 = z_l - ( ( ( t1 - t ) - i!( DP_H, k as usize ) ) - z_h );
                };

                is = y.to_bits() as i32;
                y1 = f32::from_bits( is as u32 & 0xfffff000 );
                p_l = ( y - y1 ) * t1 + y * t2;
                p_h = y1 * t1;
                z = p_l + p_h;
                j = z.to_bits() as i32;
                if j > 0x43000000 {
                    return sn * HUGE * HUGE;
                } else if j == 0x43000000 {
                    if p_l + OVT > z - p_h {
                        return sn * HUGE * HUGE;
                    }
                } else if ( j & 0x7fffffff ) > 0x43160000 {
                    return sn * TINY * TINY; 
                } else if j as u32 == 0xc3160000
                        && p_l <= z - p_h
                {
                    return sn * TINY * TINY;
                }
                
                i = j & 0x7fffffff;
                k = ( i >> 23 ) - 0x7f;
                n = 0;
                if i > 0x3f000000 {
                    n = j + ( 0x00800000 >> ( k + 1 ) );
                    k = ( ( n & 0x7fffffff ) >> 23 ) - 0x7f;
                    t = f32::from_bits( n as u32 & !( 0x007fffff >> k ) );
                    n = ( ( n & 0x007fffff ) | 0x00800000 ) >> ( 23 - k );
                    if j < 0 {
                        n = -n;
                    }
                    p_h -= t;
                }
                t = p_l + p_h;
                is = t.to_bits() as i32;
                t = f32::from_bits( is as u32 & 0xffff8000 );
                u = t * LG2_H;
                v = ( p_l - ( t - p_h ) ) * LG2 + t * LG2_L;
                z = u + v;
                w = v - ( z - u );
                t = z * z;
                t1 = z - t * ( P1 + t * ( P2 + t * ( P3 + t * ( P4 + t * P5 ) ) ) );
                r = ( z * t1 ) / ( t1 - 2.0 ) - ( w + z * w );
                z = 1.0 - ( r - z );
                j = z.to_bits() as i32;
                j += n << 23;
                if ( j >> 23 ) <= 0 {
                    z = scalbnf( z, n );
                } else {
                    z = f32::from_bits( j as u32 );
                }
                sn * z
            }
            
            pub fn sqrtf( x:f32 ) -> f32
            {
                unsafe
                {
                    use ::arch::x86_64::*;
                    let m = _mm_set_ss( x );
                    let m_sqrt = _mm_sqrt_ss( m );
                    _mm_cvtss_f32( m_sqrt )
                }
            }
            
            pub fn fabsf( x:f32 ) -> f32 
            { f32::from_bits( x.to_bits() & 0x7fffffff ) }

            pub fn scalbnf( mut x:f32, mut n:i32 ) -> f32
            {
                let x1p127 = f32::from_bits( 0x7f000000 );
                let x1p_126 = f32::from_bits( 0x800000 );
                let x1p24 = f32::from_bits( 0x4b800000 );

                if n > 127
                {
                    x *= x1p127;
                    n -= 127;
                    if n > 127 {
                        x *= x1p127;
                        n -= 127;
                        if n > 127 {
                            n = 127;
                        }
                    }
                }
                else if n < -126
                {
                    x *= x1p_126 * x1p24;
                    n += 126 - 24;
                    if n < -126 {
                        x *= x1p_126 * x1p24;
                        n += 126 - 24;
                        if n < -126 {
                            n = -126;
                        }
                    }
                }
                x * f32::from_bits( ( ( 0x7f + n ) as u32 ) << 23 )
            }

            pub fn powd( x:f64, y:f64 ) -> f64 
            {
                const BP:[f64; 2] = [1.0, 1.5];
                const DP_H:[f64; 2] = [0.0, 5.84962487220764160156e-01];
                const DP_L:[f64; 2] = [0.0, 1.35003920212974897128e-08];
                const TWO53:f64 = 9007199254740992.0;
                const HUGE:f64 = 1.0e300;
                const TINY:f64 = 1.0e-300;
                
                const L1:f64 = 5.99999999999994648725e-01;
                const L2:f64 = 4.28571428578550184252e-01;
                const L3:f64 = 3.33333329818377432918e-01;
                const L4:f64 = 2.72728123808534006489e-01;
                const L5:f64 = 2.30660745775561754067e-01;
                const L6:f64 = 2.06975017800338417784e-01;
                const P1:f64 = 1.66666666666666019037e-01;
                const P2:f64 = -2.77777777770155933842e-03;
                const P3:f64 = 6.61375632143793436117e-05;
                const P4:f64 = -1.65339022054652515390e-06;
                const P5:f64 = 4.13813679705723846039e-08;
                const LG2:f64 = 6.93147180559945286227e-01;
                const LG2_H:f64 = 6.93147182464599609375e-01;
                const LG2_L:f64 = -1.90465429995776804525e-09;
                const OVT:f64 = 8.0085662595372944372e-017;
                const CP:f64 = 9.61796693925975554329e-01;
                const CP_H:f64 = 9.61796700954437255859e-01;
                const CP_L:f64 = -7.02846165095275826516e-09;
                const IVLN2:f64 = 1.44269504088896338700e+00;
                const IVLN2_H:f64 = 1.44269502162933349609e+00;
                const IVLN2_L:f64 = 1.92596299112661746887e-08;

                let t1:f64;
                let t2:f64;
                let ( hx, lx ):( i32, u32 ) = ( ( x.to_bits() >> 32 ) as i32, x.to_bits() as u32 );
                let ( hy, ly ):( i32, u32 ) = ( ( y.to_bits() >> 32 ) as i32, y.to_bits() as u32 );
                let mut ix:i32 = ( hx & 0x7fffffff ) as i32;
                let iy:i32 = ( hy & 0x7fffffff ) as i32;
                if ( ( iy as u32 ) | ly ) == 0 { return 1.0; }
                if hx == 0x3ff00000 && lx == 0 { return 1.0; }
                if ix > 0x7ff00000
                || ( ix == 0x7ff00000 && lx != 0 )
                || iy > 0x7ff00000
                || ( iy == 0x7ff00000 && ly != 0 )
                { return x + y; }
                
                let mut yisint:i32 = 0;
                let mut k:i32;
                let mut j:i32;
                if hx < 0
                {
                    if iy >= 0x43400000 { yisint = 2; } 
                    else if iy >= 0x3ff00000 
                    {
                        k = ( iy >> 20 ) - 0x3ff;
                        if k > 20
                        {
                            j = ( ly >> ( 52 - k ) ) as i32;

                            if ( j << ( 52 - k ) ) == ( ly as i32 ) {
                                yisint = 2 - ( j & 1 );
                            }
                        }
                        
                        else if ly == 0
                        {
                            j = iy >> ( 20 - k );

                            if ( j << ( 20 - k ) ) == iy {
                                yisint = 2 - ( j & 1 );
                            }
                        }
                    }
                }

                if ly == 0
                {
                    if iy == 0x7ff00000
                    {
                        return if ( ( ix - 0x3ff00000 ) | ( lx as i32 ) ) == 0 { 1.0 } 
                        else if ix >= 0x3ff00000
                        {
                            if hy >= 0 { y }  else { 0.0 }
                        } 
                        else
                        {
                            if hy >= 0 { 0.0 } else { -y }
                        };
                    }

                    if iy == 0x3ff00000
                    {
                        return if hy >= 0 { x } else { 1.0 / x };
                    }

                    if hy == 0x40000000 
                    { return x * x; }

                    if hy == 0x3fe00000
                    {
                        if hx >= 0 { return sqrtd( x ); }
                    }
                }

                let mut ax:f64 = fabsd( x );
                if lx == 0
                {
                    if ix == 0x7ff00000 || ix == 0 || ix == 0x3ff00000 
                    {
                        let mut z:f64 = ax;
                        if hy < 0 { z = 1.0 / z; }
                        if hx < 0 
                        {
                            if ( ( ix - 0x3ff00000 ) | yisint ) == 0 { z = ( z - z ) / ( z - z ); } else if yisint == 1 { z = -z; }
                        }
                        return z;
                    }
                }

                let mut s:f64 = 1.0;
                if hx < 0 
                {
                    if yisint == 0  { return ( x - x ) / ( x - x ); }
                    if yisint == 1 { s = -1.0; }
                }
                
                if iy > 0x41e00000
                {
                    
                    if iy > 0x43f00000 
                    {
                        if ix <= 0x3fefffff
                        { return if hy < 0 { HUGE * HUGE } else { TINY * TINY }; }

                        if ix >= 0x3ff00000
                        { return if hy > 0 { HUGE * HUGE } else { TINY * TINY }; }
                    }
                    
                    if ix < 0x3fefffff
                    { return if hy < 0 { s * HUGE * HUGE } else { s * TINY * TINY }; }

                    if ix > 0x3ff00000
                    { return if hy > 0 { s * HUGE * HUGE } else { s * TINY * TINY }; }
                    
                    let t:f64 = ax - 1.0;
                    let w:f64 = ( t * t ) * ( 0.5 - t * ( 0.3333333333333333333333 - t * 0.25 ) );
                    let u:f64 = IVLN2_H * t;
                    let v:f64 = t * IVLN2_L - w * IVLN2;
                    t1 = with_set_low_word( u + v, 0 );
                    t2 = v - ( t1 - u );
                }
                
                else
                {
                    let mut n:i32 = 0;

                    if ix < 0x00100000 {
                        ax *= TWO53;
                        n -= 53;
                        ix = get_high_word( ax ) as i32;
                    }

                    n += ( ix >> 20 ) - 0x3ff;
                    j = ix & 0x000fffff;
                    
                    let k:i32;
                    ix = j | 0x3ff00000;
                    if j <= 0x3988E {
                        k = 0;
                    } else if j < 0xBB67A {
                        k = 1;
                    } else {
                        k = 0;
                        n += 1;
                        ix -= 0x00100000;
                    }

                    ax = with_set_high_word( ax, ix as u32 );                    
                    let u:f64 = ax - i!( BP, k as usize );
                    let v:f64 = 1.0 / ( ax + i!( BP, k as usize ) );
                    let ss:f64 = u * v;
                    let s_h = with_set_low_word( ss, 0 );                    
                    let t_h:f64 = with_set_high_word( 0.0, ( ( ix as u32 >> 1 ) | 0x20000000 ) + 0x00080000 + ( ( k as u32 ) << 18 ), );
                    let t_l:f64 = ax - ( t_h - i!( BP, k as usize ) );
                    let s_l:f64 = v * ( ( u - s_h * t_h ) - s_h * t_l );                    
                    let s2:f64 = ss * ss;
                    let mut r:f64 = s2 * s2 * ( L1 + s2 * ( L2 + s2 * ( L3 + s2 * ( L4 + s2 * ( L5 + s2 * L6 ) ) ) ) );
                    r += s_l * ( s_h + ss );
                    let s2:f64 = s_h * s_h;
                    let t_h:f64 = with_set_low_word( 3.0 + s2 + r, 0 );
                    let t_l:f64 = r - ( ( t_h - 3.0 ) - s2 );                    
                    let u:f64 = s_h * t_h;
                    let v:f64 = s_l * t_h + t_l * ss;                    
                    let p_h:f64 = with_set_low_word( u + v, 0 );
                    let p_l = v - ( p_h - u );
                    let z_h:f64 = CP_H * p_h;
                    let z_l:f64 = CP_L * p_h + p_l * CP + i!( DP_L, k as usize );                    
                    let t:f64 = n as f64;
                    t1 = with_set_low_word( ( ( z_h + z_l ) + i!( DP_H, k as usize ) ) + t, 0 );
                    t2 = z_l - ( ( ( t1 - t ) - i!( DP_H, k as usize ) ) - z_h );
                }
                
                let y1:f64 = with_set_low_word( y, 0 );
                let p_l:f64 = ( y - y1 ) * t1 + y * t2;
                let mut p_h:f64 = y1 * t1;
                let z:f64 = p_l + p_h;
                let mut j:i32 = ( z.to_bits() >> 32 ) as i32;
                let i:i32 = z.to_bits() as i32;

                if j >= 0x40900000
                {
                    if ( j - 0x40900000 ) | i != 0 { return s * HUGE * HUGE; }
                    if p_l + OVT > z - p_h { return s * HUGE * HUGE; }
                }
                
                else if ( j & 0x7fffffff ) >= 0x4090cc00
                {
                    if ( ( ( j as u32 ) - 0xc090cc00 ) | ( i as u32 ) ) != 0 { return s * TINY * TINY; }
                    if p_l <= z - p_h { return s * TINY * TINY; }
                }
                
                let i:i32 = j & ( 0x7fffffff as i32 );
                k = ( i >> 20 ) - 0x3ff;
                let mut n:i32 = 0;

                if i > 0x3fe00000
                {
                    n = j + ( 0x00100000 >> ( k + 1 ) );
                    k = ( ( n & 0x7fffffff ) >> 20 ) - 0x3ff;
                    let t:f64 = with_set_high_word( 0.0, ( n & !( 0x000fffff >> k ) ) as u32 );
                    n = ( ( n & 0x000fffff ) | 0x00100000 ) >> ( 20 - k );
                    if j < 0 { n = -n; }
                    p_h -= t;
                }

                let t:f64 = with_set_low_word( p_l + p_h, 0 );
                let u:f64 = t * LG2_H;
                let v:f64 = ( p_l - ( t - p_h ) ) * LG2 + t * LG2_L;
                let mut z:f64 = u + v;
                let w:f64 = v - ( z - u );
                let t:f64 = z * z;
                let t1:f64 = z - t * ( P1 + t * ( P2 + t * ( P3 + t * ( P4 + t * P5 ) ) ) );
                let r:f64 = ( z * t1 ) / ( t1 - 2.0 ) - ( w + z * w );
                z = 1.0 - ( r - z );
                j = get_high_word( z ) as i32;
                j += n << 20;

                if ( j >> 20 ) <= 0 { z = scalbnd( z, n ); } 
                else { z = with_set_high_word( z, j as u32 ); }
                s * z
            }

            /// Absolute value ( magnitude ) ( f64 )
            pub fn fabsd( x:f64 ) -> f64 
            { f64::from_bits( x.to_bits() & ( u64::MAX / 2 ) ) }

            pub fn scalbnd( x:f64, mut n:i32 ) -> f64
            {
                let x1p1023 = f64::from_bits( 0x7fe0000000000000 );
                let x1p53 = f64::from_bits( 0x4340000000000000 );
                let x1p_1022 = f64::from_bits( 0x0010000000000000 );
                let mut y = x;

                if n > 1023
                {
                    y *= x1p1023;
                    n -= 1023;
                    if n > 1023
                    {
                        y *= x1p1023;
                        n -= 1023;
                        if n > 1023 { n = 1023; }
                    }
                }
                else if n < -1022
                {
                    y *= x1p_1022 * x1p53;
                    n += 1022 - 53;
                    if n < -1022
                    {
                        y *= x1p_1022 * x1p53;
                        n += 1022 - 53;
                        if n < -1022 { n = -1022; }
                    }
                }
                y * f64::from_bits( ( ( 0x3ff + n ) as u64 ) << 52 )
            }

            pub fn sqrtd( x:f64 ) -> f64 
            {
                unsafe 
                {
                    use ::arch::x86_64::*;
                    let m = _mm_set_sd( x );
                    let m_sqrt = _mm_sqrt_pd( m );
                    _mm_cvtsd_f64( m_sqrt )
                }
            }

            #[inline] fn get_high_word( x:f64 ) -> u32
            { ( x.to_bits() >> 32 ) as u32 }

            #[inline] fn with_set_high_word( f:f64, hi:u32 ) -> f64
            {
                let mut tmp = f.to_bits();
                tmp &= 0x00000000_ffffffff;
                tmp |= ( hi as u64 ) << 32;
                f64::from_bits( tmp )
            }

            #[inline] fn with_set_low_word( f:f64, lo:u32 ) -> f64
            {
                let mut tmp = f.to_bits();
                tmp &= 0xffffffff_00000000;
                tmp |= lo as u64;
                f64::from_bits( tmp )
            }
        }

        pub mod limb
        {
            /// Number of bits in a Bigint.
            pub const BIGINT_BITS:usize = 4000;

            /// The number of limbs for the bigint.
            pub const BIGINT_LIMBS:usize = BIGINT_BITS / LIMB_BITS;

            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub type Limb = u64;
            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub type Wide = u128;
            #[cfg( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) )]
            pub const LIMB_BITS:usize = 64;

            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub type Limb = u32;
            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub type Wide = u64;
            #[cfg( not( all( target_pointer_width = "64", not( target_arch = "sparc" ) ) ) )]
            pub const LIMB_BITS:usize = 32;
        }
        
        pub mod mask
        {
            //! Utilities to generate bitmasks.

            /// Generate a bitwise mask for the lower `n` bits.
            #[inline] pub fn lower_n_mask( n:u64 ) -> u64
            {
                debug_assert!( n <= 64, "lower_n_mask() overflow in shl." );
                match n == 64
                {
                    true => 0xffff_ffff_ffff_ffff,
                    false => ( 1 << n ) - 1,
                }
            }

            /// Calculate the halfway point for the lower `n` bits.
            #[inline] pub fn lower_n_halfway( n:u64 ) -> u64
            {
                debug_assert!( n <= 64, "lower_n_halfway() overflow in shl." );
                match n == 0
                {
                    true => 0,
                    false => nth_bit( n - 1 ),
                }
            }

            /// Calculate a scalar factor of 2 above the halfway point.
            #[inline] pub fn nth_bit( n:u64 ) -> u64
            {
                debug_assert!( n < 64, "nth_bit() overflow in shl." );
                1 << n
            }
        }

        pub mod num
        {
            use ::
            {
                *,
            };

            /// Generic floating-point type, to be used in generic code for parsing.
            pub trait Float:
            Sized
            + Copy
            + PartialEq
            + PartialOrd
            + Send
            + Sync
            + ops::Add<Output = Self>
            + ops::AddAssign
            + ops::Div<Output = Self>
            + ops::DivAssign
            + ops::Mul<Output = Self>
            + ops::MulAssign
            + ops::Rem<Output = Self>
            + ops::RemAssign
            + ops::Sub<Output = Self>
            + ops::SubAssign
            + ops::Neg<Output = Self>
            {
                /// Maximum number of digits that can contribute in the mantissa.
                const MAX_DIGITS:usize;
                /// Bitmask for the sign bit.
                const SIGN_MASK:u64;
                /// Bitmask for the exponent, including the hidden bit.
                const EXPONENT_MASK:u64;
                /// Bitmask for the hidden bit in exponent, which is an implicit 1 in the fraction.
                const HIDDEN_BIT_MASK:u64;
                /// Bitmask for the mantissa ( fraction ), excluding the hidden bit.
                const MANTISSA_MASK:u64;
                /// Size of the significand ( mantissa ) without hidden bit.
                const MANTISSA_SIZE:i32;
                /// Bias of the exponet
                const EXPONENT_BIAS:i32;
                /// Exponent portion of a denormal float.
                const DENORMAL_EXPONENT:i32;
                /// Maximum exponent value in float.
                const MAX_EXPONENT:i32;
                /// Mask to determine if a full-carry occurred ( 1 in bit above hidden bit ).
                const CARRY_MASK:u64;
                /// Bias for marking an invalid extended float.
                const INVALID_FP:i32 = -0x8000;
                // Maximum mantissa for the fast-path ( `1 << 53` for f64 ).
                const MAX_MANTISSA_FAST_PATH:u64 = 2_u64 << Self::MANTISSA_SIZE;
                // Largest exponent value `( 1 << EXP_BITS ) - 1`.
                const INFINITE_POWER:i32 = Self::MAX_EXPONENT + Self::EXPONENT_BIAS;
                const MIN_EXPONENT_ROUND_TO_EVEN:i32;
                const MAX_EXPONENT_ROUND_TO_EVEN:i32;
                /// Minimum normal exponent value `-( 1 << ( EXPONENT_SIZE - 1 ) ) + 1`.
                const MINIMUM_EXPONENT:i32;
                /// Smallest decimal exponent for a non-zero value.
                const SMALLEST_POWER_OF_TEN:i32;
                /// Largest decimal exponent for a non-infinite value.
                const LARGEST_POWER_OF_TEN:i32;
                /// Minimum exponent that for a fast path case, or `-⌊( MANTISSA_SIZE+1 )/log2( 10 )⌋`
                const MIN_EXPONENT_FAST_PATH:i32;
                /// Maximum exponent that for a fast path case, or `⌊( MANTISSA_SIZE+1 )/log2( 5 )⌋`
                const MAX_EXPONENT_FAST_PATH:i32;
                /// Maximum exponent that can be represented for a disguised-fast path case.
                const MAX_EXPONENT_DISGUISED_FAST_PATH:i32;
                /// Convert 64-bit integer to float.
                fn from_u64( u:u64 ) -> Self;
                fn from_bits( u:u64 ) -> Self;
                fn to_bits( self ) -> u64;

                /// Get a small power-of-radix for fast-path multiplication.
                unsafe fn pow_fast_path( exponent:usize ) -> Self;

                /// Returns true if the float is a denormal.
                #[inline] fn is_denormal( self ) -> bool { self.to_bits() & Self::EXPONENT_MASK == 0 }

                /// Get exponent component from the float.
                #[inline] fn exponent( self ) -> i32
                {
                    if self.is_denormal() { return Self::DENORMAL_EXPONENT; }
                    let bits = self.to_bits();
                    let biased_e:i32 = ( ( bits & Self::EXPONENT_MASK ) >> Self::MANTISSA_SIZE ) as i32;
                    biased_e - Self::EXPONENT_BIAS
                }

                /// Get mantissa ( significand ) component from float.
                #[inline] fn mantissa( self ) -> u64
                {
                    let bits = self.to_bits();
                    let s = bits & Self::MANTISSA_MASK;
                    if !self.is_denormal(){ s + Self::HIDDEN_BIT_MASK } 
                    else { s }
                }
            }

            impl Float for f32
            {
                const MAX_DIGITS:usize = 114;
                const SIGN_MASK:u64 = 0x80000000;
                const EXPONENT_MASK:u64 = 0x7F800000;
                const HIDDEN_BIT_MASK:u64 = 0x00800000;
                const MANTISSA_MASK:u64 = 0x007FFFFF;
                const MANTISSA_SIZE:i32 = 23;
                const EXPONENT_BIAS:i32 = 127 + Self::MANTISSA_SIZE;
                const DENORMAL_EXPONENT:i32 = 1 - Self::EXPONENT_BIAS;
                const MAX_EXPONENT:i32 = 0xFF - Self::EXPONENT_BIAS;
                const CARRY_MASK:u64 = 0x1000000;
                const MIN_EXPONENT_ROUND_TO_EVEN:i32 = -17;
                const MAX_EXPONENT_ROUND_TO_EVEN:i32 = 10;
                const MINIMUM_EXPONENT:i32 = -127;
                const SMALLEST_POWER_OF_TEN:i32 = -65;
                const LARGEST_POWER_OF_TEN:i32 = 38;
                const MIN_EXPONENT_FAST_PATH:i32 = -10;
                const MAX_EXPONENT_FAST_PATH:i32 = 10;
                const MAX_EXPONENT_DISGUISED_FAST_PATH:i32 = 17;

                #[inline( always )] unsafe fn pow_fast_path( exponent:usize ) -> Self
                { return powf( 10.0f32, exponent as f32 ); }

                #[inline] fn from_u64( u:u64 ) -> f32
                { u as _ }

                #[inline] fn from_bits( u:u64 ) -> f32
                {
                    debug_assert!( u <= 0xffff_ffff );
                    f32::from_bits( u as u32 )
                }

                #[inline] fn to_bits( self ) -> u64
                { f32::to_bits( self ) as u64 }
            }

            impl Float for f64
            {
                const MAX_DIGITS:usize = 769;
                const SIGN_MASK:u64 = 0x8000000000000000;
                const EXPONENT_MASK:u64 = 0x7FF0000000000000;
                const HIDDEN_BIT_MASK:u64 = 0x0010000000000000;
                const MANTISSA_MASK:u64 = 0x000FFFFFFFFFFFFF;
                const MANTISSA_SIZE:i32 = 52;
                const EXPONENT_BIAS:i32 = 1023 + Self::MANTISSA_SIZE;
                const DENORMAL_EXPONENT:i32 = 1 - Self::EXPONENT_BIAS;
                const MAX_EXPONENT:i32 = 0x7FF - Self::EXPONENT_BIAS;
                const CARRY_MASK:u64 = 0x20000000000000;
                const MIN_EXPONENT_ROUND_TO_EVEN:i32 = -4;
                const MAX_EXPONENT_ROUND_TO_EVEN:i32 = 23;
                const MINIMUM_EXPONENT:i32 = -1023;
                const SMALLEST_POWER_OF_TEN:i32 = -342;
                const LARGEST_POWER_OF_TEN:i32 = 308;
                const MIN_EXPONENT_FAST_PATH:i32 = -22;
                const MAX_EXPONENT_FAST_PATH:i32 = 22;
                const MAX_EXPONENT_DISGUISED_FAST_PATH:i32 = 37;

                #[inline( always )]
                unsafe fn pow_fast_path( exponent:usize ) -> Self 
                {
                    return powd( 10.0f64, exponent as f64 );
                }

                #[inline]
                fn from_u64( u:u64 ) -> f64 {
                    u as _
                }

                #[inline]
                fn from_bits( u:u64 ) -> f64 {
                    f64::from_bits( u )
                }

                #[inline]
                fn to_bits( self ) -> u64 {
                    f64::to_bits( self )
                }
            }

            #[inline( always )] pub fn powf( x:f32, y:f32 ) -> f32
            { x.powf( y ) }

            #[inline( always )] pub fn powd( x:f64, y:f64 ) -> f64
            { x.powf( y ) }

            pub( crate ) enum FastPathRadix
            {
                Five,
                Ten,
            }

            impl From<FastPathRadix> for u64
            {
                fn from( radix:FastPathRadix ) -> u64
                {
                    match radix
                    {
                        FastPathRadix::Five => 5,
                        FastPathRadix::Ten => 10,
                    }
                }
            }

            /// Get a small, integral power-of-radix for fast-path multiplication.
            #[inline( always )]
            pub( crate ) unsafe fn int_pow_fast_path( exponent:usize, radix:FastPathRadix ) -> u64
            {
                return u64::from( radix ).pow( exponent as u32 );
            }
        }

        pub mod number
        {
            //! Representation of a float as the significant digits and exponent.
            /*
            #![doc( hidden )]

            #[cfg( feature = "nightly" )]
            use crate::fpu::set_precision;
            */
            use ::
            {
                *,
            };
            use super::num::{int_pow_fast_path, FastPathRadix, Float};

            /// Representation of a number as the significant digits and exponent.
            #[derive( Clone, Copy, Debug, Default, PartialEq, Eq )]
            pub struct Number 
            {
                /// The exponent of the float, scaled to the mantissa.
                pub exponent:i32,
                /// The significant digits of the float.
                pub mantissa:u64,
                /// If the significant digits were truncated.
                pub many_digits:bool,
            }

            impl Number 
            {
                /// Detect if the float can be accurately reconstructed from native floats.
                #[inline]
                pub fn is_fast_path<F:Float>( &self ) -> bool 
                {
                    F::MIN_EXPONENT_FAST_PATH <= self.exponent
                    && self.exponent <= F::MAX_EXPONENT_DISGUISED_FAST_PATH
                    && self.mantissa <= F::MAX_MANTISSA_FAST_PATH
                    && !self.many_digits
                }

                /// The fast path algorithmn using machine-sized integers and floats.
                pub fn try_fast_path<F:Float>( &self ) -> Option<F>
                {
                    /*
                    // The fast path crucially depends on arithmetic being rounded to the correct number of bits
                    // without any intermediate rounding. On x86 ( without SSE or SSE2 ) this requires the precision
                    // of the x87 FPU stack to be changed so that it directly rounds to 64/32 bit.
                    // The `set_precision` function takes care of setting the precision on architectures which
                    // require setting it by changing the global state ( like the control word of the x87 FPU ).
                    #[cfg( feature = "nightly" )]
                    let _cw = set_precision::<F>();
                    */
                    if self.is_fast_path::<F>() 
                    {
                        let max_exponent = F::MAX_EXPONENT_FAST_PATH;
                        Some( if self.exponent <= max_exponent 
                        {
                            let value = F::from_u64( self.mantissa );
                            if self.exponent < 0 { value / unsafe { F::pow_fast_path( ( -self.exponent ) as _ ) } }
                            else { value * unsafe { F::pow_fast_path( self.exponent as _ ) } }
                        }
                        else
                        {
                            let shift = self.exponent - max_exponent;
                            let int_power = unsafe { int_pow_fast_path( shift as usize, FastPathRadix::Ten ) };
                            let mantissa = self.mantissa.checked_mul( int_power )?;
                            if mantissa > F::MAX_MANTISSA_FAST_PATH { return None; }
                            F::from_u64( mantissa ) * unsafe { F::pow_fast_path( max_exponent as _ ) }
                        } )
                    }
                    
                    else { None }
                }
            }
        }

        pub mod parse
        {
            //! Parse byte iterators to float.
            use super::bellerophon::bellerophon;
            use super::extended::{extended_to_float, ExtendedFloat};
            use super::num::Float;
            use super::number::Number;
            use super::slow::slow;

            /// Try to parse the significant digits quickly.
            #[inline] fn parse_number_fast<'a, Iter1, Iter2>
            ( 
                integer:Iter1,
                fraction:Iter2,
                exponent:i32,
 ) -> Option<Number> where
            Iter1:Iterator<Item = &'a u8>,
            Iter2:Iterator<Item = &'a u8>,
            {
                let mut num = Number::default();
                let mut integer_count:usize = 0;
                let mut fraction_count:usize = 0;
                for &c in integer 
                {
                    integer_count += 1;
                    let digit = c - b'0';
                    num.mantissa = num.mantissa.wrapping_mul( 10 ).wrapping_add( digit as u64 );
                }

                for &c in fraction
                {
                    fraction_count += 1;
                    let digit = c - b'0';
                    num.mantissa = num.mantissa.wrapping_mul( 10 ).wrapping_add( digit as u64 );
                }

                if integer_count + fraction_count <= 19 
                {
                    num.exponent = exponent.saturating_sub( fraction_count as i32 );
                    Some( num )
                } 
                else { None }
            }

            /// Parse the significant digits of the float and adjust the exponent.
            #[inline] fn parse_number<'a, I1, I2>( mut integer:I1, mut fraction:I2, exponent:i32 ) -> Number where 
            I1:Iterator<Item = &'a u8> + Clone, I2:Iterator<Item = &'a u8> + Clone,
            {
                if let Some( num ) = parse_number_fast( integer.clone(), fraction.clone(), exponent ){ return num; }
                let mut num = Number::default();
                let mut count = 0;
                while let Some( &c ) = integer.next()
                {
                    count += 1;
                    if count == 20
                    {
                        num.many_digits = true;
                        num.exponent = exponent.saturating_add( into_i32( 1 + integer.count() ) );
                        return num;
                    } 
                    else
                    {
                        let digit = c - b'0';
                        num.mantissa = num.mantissa * 10 + digit as u64;
                    }
                }
                
                let mut fraction_count:usize = 0;
                if count == 0 
                {
                    for &c in &mut fraction 
                    {
                        fraction_count += 1;
                        if c != b'0'
                        {
                            count += 1;
                            let digit = c - b'0';
                            num.mantissa = num.mantissa * 10 + digit as u64;
                            break;
                        }
                    }
                }

                for c in fraction
                {
                    fraction_count += 1;
                    count += 1;
                    if count == 20
                    {
                        num.many_digits = true;
                        num.exponent = exponent.saturating_sub( fraction_count as i32 - 1 );
                        return num;
                    } 
                    else
                    {
                        let digit = c - b'0';
                        num.mantissa = num.mantissa * 10 + digit as u64;
                    }
                }

                num.exponent = exponent.saturating_sub( fraction_count as i32 );
                num
            }

            /// Parse float from extracted float components.
            pub fn parse_float<'a, F, Iter1, Iter2>( integer:Iter1, fraction:Iter2, exponent:i32 ) -> F where
            F:Float,
            Iter1:Iterator<Item = &'a u8> + Clone,
            Iter2:Iterator<Item = &'a u8> + Clone,
            {
                let num = parse_number( integer.clone(), fraction.clone(), exponent );
                if let Some( value ) = num.try_fast_path() { return value; }

                let mut fp = moderate_path::<F>( &num );
                if fp.exp < 0
                {
                    fp.exp -= F::INVALID_FP;
                    fp = slow::<F, _, _>( num, fp, integer, fraction );
                }
                
                extended_to_float::<F>( fp )
            }

            /// Wrapper for different moderate-path algorithms..
            #[inline] pub fn moderate_path<F:Float>( num:&Number ) -> ExtendedFloat 
            { return bellerophon::<F>( num ); }

            /// Convert usize into i32 without overflow.
            #[inline] fn into_i32( value:usize ) -> i32
            {
                if value > i32::max_value() as usize { i32::max_value() }
                else { value as i32 }
            }

            // Add digit to mantissa.
            #[inline] pub fn add_digit( value:u64, digit:u8 ) -> Option<u64>
            { value.checked_mul( 10 )?.checked_add( digit as u64 ) }
        }

        pub mod rounding
        {
            //! Defines rounding schemes for floating-point numbers.
            use super::extended::ExtendedFloat;
            use super::mask::{lower_n_halfway, lower_n_mask};
            use super::num::Float;
            
            /// Round an extended-precision float to the nearest machine float.
            #[inline] pub fn round<F, Cb>( fp:&mut ExtendedFloat, cb:Cb ) where
            F:Float,
            Cb:Fn( &mut ExtendedFloat, i32 ),
            {
                let fp_inf = ExtendedFloat
                {
                    mant:0,
                    exp:F::INFINITE_POWER,
                };
                
                let mantissa_shift = 64 - F::MANTISSA_SIZE - 1;
                if -fp.exp >= mantissa_shift 
                {
                    let shift = -fp.exp + 1;
                    debug_assert!( shift <= 65 );
                    cb( fp, shift.min( 64 ) );
                    fp.exp = ( fp.mant >= F::HIDDEN_BIT_MASK ) as i32;
                    return;
                }
                cb( fp, mantissa_shift );
                let carry_mask = F::CARRY_MASK;
                if fp.mant & carry_mask == carry_mask 
                {
                    fp.mant >>= 1;
                    fp.exp += 1;
                }

                if fp.exp >= F::INFINITE_POWER 
                {
                    *fp = fp_inf;
                    return;
                }

                fp.mant &= F::MANTISSA_MASK;
            }

            /// Shift right N-bytes and round towards a direction.
            pub fn round_nearest_tie_even<Cb>( fp:&mut ExtendedFloat, shift:i32, cb:Cb ) where
            Cb:Fn( bool, bool, bool ) -> bool,
            {
                debug_assert!( shift <= 64 );
                let mask = lower_n_mask( shift as u64 );
                let halfway = lower_n_halfway( shift as u64 );
                let truncated_bits = fp.mant & mask;
                let is_above = truncated_bits > halfway;
                let is_halfway = truncated_bits == halfway;
                fp.mant = match shift == 64 
                {
                    true => 0,
                    false => fp.mant >> shift,
                };
                fp.exp += shift;
                let is_odd = fp.mant & 1 == 1;
                fp.mant += cb( is_odd, is_halfway, is_above ) as u64;
            }
            
            pub fn round_down( fp:&mut ExtendedFloat, shift:i32 ) 
            {
                fp.mant = match shift == 64 
                {
                    true => 0,
                    false => fp.mant >> shift,
                };
                fp.exp += shift;
            }
        }

        pub mod slow
        {
            //! Slow, fallback cases where we cannot unambiguously round a float.
            use super::bigint::{Bigint, Limb, LIMB_BITS};
            use super::extended::{extended_to_float, ExtendedFloat};
            use super::num::{int_pow_fast_path, FastPathRadix, Float};
            use super::number::Number;
            use super::rounding::{round, round_down, round_nearest_tie_even};
            use ::
            {
                *,
            };

            /// Add a digit to the temporary value.
            macro_rules! add_digit
            {
                ( $c:ident, $value:ident, $counter:ident, $count:ident ) =>
                {{
                    let digit = $c - b'0';
                    $value *= 10 as Limb;
                    $value += digit as Limb;
                    $counter += 1;
                    $count += 1;
                }};
            }

            /// Add a temporary value to our mantissa.
            macro_rules! add_temporary
            {
                ( @mul $result:ident, $power:expr, $value:expr ) =>
                {
                    $result.data.mul_small( $power ).unwrap();
                    $result.data.add_small( $value ).unwrap();
                };
                
                ( $format:ident, $result:ident, $counter:ident, $value:ident ) =>
                {
                    if $counter != 0
                    {
                        let small_power = unsafe { int_pow_fast_path( $counter, FastPathRadix::Ten ) };
                        add_temporary!( @mul $result, small_power as Limb, $value );
                        $counter = 0;
                        $value = 0;
                    }
                };

                // Add a temporary where we won't read the counter results internally.
                ( @end $format:ident, $result:ident, $counter:ident, $value:ident ) =>
                {
                    if $counter != 0
                    {
                        let small_power = unsafe { int_pow_fast_path( $counter, FastPathRadix::Ten ) };
                        add_temporary!( @mul $result, small_power as Limb, $value );
                    }
                };

                // Add the maximum native value.
                ( @max $format:ident, $result:ident, $counter:ident, $value:ident, $max:ident ) =>
                {
                    add_temporary!( @mul $result, $max, $value );
                    $counter = 0;
                    $value = 0;
                };
            }

            /// Round-up a truncated value.
            macro_rules! round_up_truncated
            {
                ( $format:ident, $result:ident, $count:ident ) =>
                {{
                    add_temporary!( @mul $result, 10, 1 );
                    $count += 1;
                }};
            }

            /// Check and round-up the fraction if any non-zero digits exist.
            macro_rules! round_up_nonzero
            {
                ( $format:ident, $iter:expr, $result:ident, $count:ident ) => 
                {{
                    for &digit in $iter 
                    {
                        if digit != b'0' 
                        {
                            round_up_truncated!( $format, $result, $count );
                            return ( $result, $count );
                        }
                    }
                }};
            }

            /// Parse the significant digits and biased, binary exponent of a float.
            #[inline] pub fn slow<'a, F, Iter1, Iter2>
            ( 
                num:Number,
                fp:ExtendedFloat,
                integer:Iter1,
                fraction:Iter2,
 ) -> ExtendedFloat where
            F:Float,
            Iter1:Iterator<Item = &'a u8> + Clone,
            Iter2:Iterator<Item = &'a u8> + Clone,
            {
                debug_assert!( fp.mant & ( 1 << 63 ) != 0 );
                let sci_exp = scientific_exponent( &num );
                let ( bigmant, digits ) = parse_mantissa( integer, fraction, F::MAX_DIGITS );
                let exponent = sci_exp + 1 - digits as i32;
                if exponent >= 0 { positive_digit_comp::<F>( bigmant, exponent ) }
                else { negative_digit_comp::<F>( bigmant, fp, exponent ) }
            }

            /// Generate the significant digits with a positive exponent relative to mantissa.
            pub fn positive_digit_comp<F:Float>( mut bigmant:Bigint, exponent:i32 ) -> ExtendedFloat
            {
                bigmant.pow( 10, exponent as u32 ).unwrap();
                let ( mant, is_truncated ) = bigmant.hi64();
                let exp = bigmant.bit_length() as i32 - 64 + F::EXPONENT_BIAS;
                let mut fp = ExtendedFloat
                {
                    mant,
                    exp,
                };

                // Shift the digits into position and determine if we need to round-up.
                round::<F, _>( &mut fp, |f, s|
                {
                    round_nearest_tie_even( f, s, |is_odd, is_halfway, is_above|
                    {
                        is_above || ( is_halfway && is_truncated ) || ( is_odd && is_halfway )
                    } );
                } );
                fp
            }

            /// Generate the significant digits with a negative exponent relative to mantissa.
            pub fn negative_digit_comp<F:Float>
            ( 
                bigmant:Bigint,
                mut fp:ExtendedFloat,
                exponent:i32,
 ) -> ExtendedFloat
            {
                debug_assert!( fp.mant & ( 1 << 63 ) != 0 );
                let mut real_digits = bigmant;
                let real_exp = exponent;
                debug_assert!( real_exp < 0 );
                let mut b = fp;
                round::<F, _>( &mut b, round_down );
                let b = extended_to_float::<F>( b );
                let theor = bh( b );
                let mut theor_digits = Bigint::from_u64( theor.mant );
                let theor_exp = theor.exp;
                let binary_exp = theor_exp - real_exp;
                let halfradix_exp = -real_exp;
                if halfradix_exp != 0 
                {
                    theor_digits.pow( 5, halfradix_exp as u32 ).unwrap();
                }
                
                if binary_exp > 0
                { theor_digits.pow( 2, binary_exp as u32 ).unwrap(); }

                else if binary_exp < 0
                { real_digits.pow( 2, ( -binary_exp ) as u32 ).unwrap(); }
                
                let ord = real_digits.data.cmp( &theor_digits.data );
                round::<F, _>( &mut fp, |f, s|
                {
                    round_nearest_tie_even( f, s, |is_odd, _, _|
                    {
                        match ord
                        {
                            cmp::Ordering::Greater => true,
                            cmp::Ordering::Less => false,
                            cmp::Ordering::Equal if is_odd => true,
                            cmp::Ordering::Equal => false,
                        }
                    } );
                } );

                fp
            }

            /// Parse the full mantissa into a big integer.
            pub fn parse_mantissa<'a, Iter1, Iter2>
            ( 
                mut integer:Iter1,
                mut fraction:Iter2,
                max_digits:usize,
 ) -> ( Bigint, usize ) where
            Iter1:Iterator<Item = &'a u8> + Clone,
            Iter2:Iterator<Item = &'a u8> + Clone,
            {
                let mut counter:usize = 0;
                let mut count:usize = 0;
                let mut value:Limb = 0;
                let mut result = Bigint::new();                
                let step:usize = if LIMB_BITS == 32 { 9 } else { 19 };
                let max_native = ( 10 as Limb ).pow( step as u32 );                
                'integer:loop 
                {
                    while counter < step && count < max_digits 
                    {
                        if let Some( &c ) = integer.next(){ add_digit!( c, value, counter, count ); } else { break 'integer; }
                    }

                    // Check if we've exhausted our max digits.
                    if count == max_digits 
                    {
                        add_temporary!( @end format, result, counter, value );
                        round_up_nonzero!( format, integer, result, count );
                        round_up_nonzero!( format, fraction, result, count );
                        return ( result, count );
                    }
                    else 
                    { add_temporary!( @max format, result, counter, value, max_native ); }
                }
                
                if count == 0 
                {
                    for &c in &mut fraction 
                    {
                        if c != b'0' 
                        {
                            add_digit!( c, value, counter, count );
                            break;
                        }
                    }
                }
                
                'fraction:loop 
                {
                    while counter < step && count < max_digits 
                    {
                        if let Some( &c ) = fraction.next(){ add_digit!( c, value, counter, count ); } else { break 'fraction; }
                    }
                    
                    if count == max_digits 
                    {
                        add_temporary!( @end format, result, counter, value );
                        round_up_nonzero!( format, fraction, result, count );
                        return ( result, count );
                    } else { add_temporary!( @max format, result, counter, value, max_native ); }
                }
                
                add_temporary!( @end format, result, counter, value );
                ( result, count )
            }
            /// Calculate the scientific exponent from a `Number` value.
            #[inline] pub fn scientific_exponent( num:&Number ) -> i32
            {
                let mut mantissa = num.mantissa;
                let mut exponent = num.exponent;
                while mantissa >= 10000
                {
                    mantissa /= 10000;
                    exponent += 4;
                }
                while mantissa >= 100
                {
                    mantissa /= 100;
                    exponent += 2;
                }
                while mantissa >= 10
                {
                    mantissa /= 10;
                    exponent += 1;
                }
                exponent as i32
            }

            /// Calculate `b` from a a representation of `b` as a float.
            #[inline] pub fn b<F:Float>( float:F ) -> ExtendedFloat 
            {
                ExtendedFloat
                {
                    mant:float.mantissa(),
                    exp:float.exponent(),
                }
            }

            /// Calculate `b+h` from a a representation of `b` as a float.
            #[inline] pub fn bh<F:Float>( float:F ) -> ExtendedFloat
            {
                let fp = b( float );
                ExtendedFloat
                {
                    mant:( fp.mant << 1 ) + 1,
                    exp:fp.exp - 1,
                }
            }
        }
        
        pub mod table
        {
            use super::{ bellerophon::BellerophonPowers };
            
            pub const BASE10_POWERS:BellerophonPowers = BellerophonPowers
            {
                small:&BASE10_SMALL_MANTISSA,
                large:&BASE10_LARGE_MANTISSA,
                small_int:&BASE10_SMALL_INT_POWERS,
                step:BASE10_STEP,
                bias:BASE10_BIAS,
                log2:BASE10_LOG2_MULT,
                log2_shift:BASE10_LOG2_SHIFT,
            };
            
            const BASE10_SMALL_MANTISSA:[u64; 10] =
            [
                9223372036854775808, 
                11529215046068469760,
                14411518807585587200,
                18014398509481984000,
                11258999068426240000,
                14073748835532800000,
                17592186044416000000,
                10995116277760000000,
                13743895347200000000,
                17179869184000000000,
            ];

            const BASE10_LARGE_MANTISSA:[u64; 66] = 
            [
                11555125961253852697, 
                13451937075301367670, 
                15660115838168849784, 
                18230774251475056848, 
                10611707258198326947, 
                12353653155963782858, 
                14381545078898527261, 
                16742321987285426889, 
                9745314011399999080,  
                11345038669416679861, 
                13207363278391631158, 
                15375394465392026070, 
                17899314949046850752, 
                10418772551374772303, 
                12129047596099288555, 
                14120069793541087484, 
                16437924692338667210, 
                9568131466127621947,  
                11138771039116687545, 
                12967236152753102995, 
                15095849699286165408, 
                17573882009934360870, 
                10229345649675443343, 
                11908525658859223294, 
                13863348470604074297, 
                16139061738043178685, 
                9394170331095332911,  
                10936253623915059621, 
                12731474852090538039, 
                14821387422376473014, 
                17254365866976409468, 
                10043362776618689222, 
                11692013098647223345, 
                13611294676837538538, 
                15845632502852867518, 
                9223372036854775808,  
                10737418240000000000, 
                12500000000000000000, 
                14551915228366851806, 
                16940658945086006781, 
                9860761315262647567,  
                11479437019748901445, 
                13363823550460978230, 
                15557538194652854267, 
                18111358157653424735, 
                10542197943230523224, 
                12272733663244316382, 
                14287342391028437277, 
                16632655625031838749, 
                9681479787123295682,  
                11270725851789228247, 
                13120851772591970218, 
                15274681817498023410, 
                17782069995880619867, 
                10350527006597618960, 
                12049599325514420588, 
                14027579833653779454, 
                16330252207878254650, 
                9505457831475799117,  
                11065809325636130661, 
                12882297539194266616, 
                14996968138956309548, 
                17458768723248864463, 
                10162340898095201970, 
                11830521861667747109, 
                13772540099066387756, 
            ];
            const BASE10_SMALL_INT_POWERS:[u64; 10] = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000];
            const BASE10_STEP:i32 = 10;
            const BASE10_BIAS:i32 = 350;
            const BASE10_LOG2_MULT:i64 = 217706;
            const BASE10_LOG2_SHIFT:i32 = 16;
        }
        
        pub use self::num::Float;
        pub use self::parse::parse_float;
    }
}
pub mod path
{
    pub use std::path::{ * };
    use ::
    {
        borrow::{ Cow },        
        fs::{ read_dir },
        io::{ ErrorKind, Write },
        os::unix::fs::{ PermissionsExt },
        regex::{ re_contains, Regex },
        *,
    };
    /*
    dirs     v0.0.0
    dirs-sys v0.0.0*/
    pub mod sys
    {
        use ::
        {
            ffi::{ OsString },
            path::{ PathBuf },
            *
        };
        pub fn cache_dir() -> Option<PathBuf> { env::var_os( "XDG_CACHE_HOME" ) .and_then( is_absolute_path ).or_else( || home_dir().map( |h| h.join( ".cache" ) ) ) }
        pub fn config_dir() -> Option<PathBuf> { env::var_os( "XDG_CONFIG_HOME" ).and_then( is_absolute_path ).or_else( || home_dir().map( |h| h.join( ".config" ) ) ) }
        pub fn config_local_dir() -> Option<PathBuf> { config_dir() }
        pub fn data_dir() -> Option<PathBuf> { env::var_os( "XDG_DATA_HOME" )  .and_then( is_absolute_path ).or_else( || home_dir().map( |h| h.join( ".local/share" ) ) ) }
        pub fn data_local_dir() -> Option<PathBuf> { data_dir() }
        pub fn preference_dir() -> Option<PathBuf> { config_dir() }
        pub fn runtime_dir() -> Option<PathBuf> { env::var_os( "XDG_RUNTIME_DIR" ).and_then( is_absolute_path ) }
        pub fn state_dir() -> Option<PathBuf> { env::var_os( "XDG_STATE_HOME" ) .and_then( is_absolute_path ).or_else( || home_dir().map( |h| h.join( ".local/state" ) ) ) }
        pub fn executable_dir() -> Option<PathBuf> { env::var_os( "XDG_BIN_HOME" ).and_then( is_absolute_path ).or_else( || home_dir().map( |h| h.join( ".local/bin" ) ) ) }

        pub fn audio_dir() -> Option<PathBuf> { user_dir( "MUSIC" ) }
        pub fn desktop_dir() -> Option<PathBuf> { user_dir( "DESKTOP" ) }
        pub fn document_dir() -> Option<PathBuf> { user_dir( "DOCUMENTS" ) }
        pub fn download_dir() -> Option<PathBuf> { user_dir( "DOWNLOAD" ) }
        pub fn font_dir() -> Option<PathBuf> { data_dir().map( |d| d.join( "fonts" ) ) }
        pub fn picture_dir() -> Option<PathBuf> { user_dir( "PICTURES" ) }
        pub fn public_dir() -> Option<PathBuf> { user_dir( "PUBLICSHARE" ) }
        pub fn template_dir() -> Option<PathBuf> { user_dir( "TEMPLATES" ) }
        pub fn video_dir() -> Option<PathBuf> { user_dir( "VIDEOS" ) }
        
        pub fn is_absolute_path( path:OsString ) -> Option<PathBuf>
        {
            let path = PathBuf::from( path );
            if path.is_absolute()
            { Some( path ) } 
            else 
            { None }
        }

        #[cfg( all( unix, not( target_os = "redox" ) ) )]
        mod target_unix_not_redox
        {
            use ::
            {
                ffi::{ CStr, OsString },
                os::unix::ffi::{ OsStringExt },
                path::{ PathBuf },
                *,
            };
            
            // https://github.com/rust-lang/rust/blob/2682b88c526d493edeb2d3f2df358f44db69b73f/library/std/src/sys/unix/os.rs#L595
            pub fn home_dir() -> Option<PathBuf>
            {
                return env::var_os( "HOME" )
                .and_then( |h| if h.is_empty() { None } else { Some( h ) } )
                .or_else( || unsafe { fallback() } )
                .map( PathBuf::from );
                
                #[cfg( any( target_os = "android", target_os = "ios", target_os = "emscripten" ) )]
                unsafe fn fallback() -> Option<OsString>
                { None }

                #[cfg( not( any( target_os = "android", target_os = "ios", target_os = "emscripten" ) ) )]
                unsafe fn fallback() -> Option<OsString>
                {
                    let amt = match libc::sysconf( libc::_SC_GETPW_R_SIZE_MAX )
                    {
                        n if n < 0 => 512 as usize,
                        n => n as usize,
                    };

                    let mut buf = Vec::with_capacity( amt );
                    let mut passwd:libc::passwd = mem::zeroed();
                    let mut result = ptr::null_mut();
                    match libc::getpwuid_r
                    ( 
                        libc::getuid(),
                        &mut passwd,
                        buf.as_mut_ptr(),
                        buf.capacity(),
                        &mut result,
 )
                    {
                        0 if !result.is_null() =>
                        {
                            let ptr = passwd.pw_dir as *const _;
                            let bytes = CStr::from_ptr( ptr ).to_bytes();
                            if bytes.is_empty() { None }
                            else { Some( OsStringExt::from_vec( bytes.to_vec() ) ) }
                        }
                        _ => None,
                    }
                }
            }

        }

        #[cfg( all( unix, not( target_os = "redox" ) ) )]
        pub use self::target_unix_not_redox::home_dir;

        #[cfg( target_os = "redox" )]
        extern crate redox_users;

        #[cfg( target_os = "redox" )]
        mod target_redox
        {
            use ::
            {
                path::{ PathBuf },
                *,
            };
            use super::redox_users::{All, AllUsers, Config};

            pub fn home_dir() -> Option<PathBuf>
            {
                let current_uid = redox_users::get_uid().ok()?;
                let users = AllUsers::basic( Config::default() ).ok()?;
                let user = users.get_by_id( current_uid )?;
                Some( PathBuf::from( user.home.clone() ) )
            }
        }

        #[cfg( target_os = "redox" )]
        pub use self::target_redox::home_dir;

        #[cfg( all( unix, not( any( target_os = "macos", target_os = "ios" ) ) ) )]
        mod xdg_user_dirs
        {
            use ::
            {
                collections::{ HashMap },
                ffi::{ OsString },
                io::{ self, Read },
                option::{ OptionExt },
                os::unix::ffi::{ OsStringExt },
                path::{ Path, PathBuf },
                *
            };

            /// Returns all XDG user directories obtained from $( XDG_CONFIG_HOME )/user-dirs.dirs.
            pub fn all( home_dir_path:&Path, user_dir_file_path:&Path ) -> HashMap<String, PathBuf>
            {
                let bytes = read_all( user_dir_file_path ).unwrap_or( Vec::new() );
                parse_user_dirs( home_dir_path, None, &bytes )
            }

            /// Returns a single XDG user directory obtained from $( XDG_CONFIG_HOME )/user-dirs.dirs.
            pub fn single( home_dir_path:&Path, user_dir_file_path:&Path, user_dir_name:&str ) -> HashMap<String, PathBuf>
            {
                let bytes = read_all( user_dir_file_path ).unwrap_or( Vec::new() );
                parse_user_dirs( home_dir_path, Some( user_dir_name ), &bytes )
            }

            fn parse_user_dirs( home_dir:&Path, user_dir:Option<&str>, bytes:&[u8] ) -> HashMap<String, PathBuf>
            {
                let mut user_dirs = HashMap::new();
                for line in bytes.split( |b| *b == b'\n' )
                {
                    let mut single_dir_found = false;
                    let ( key, value ) = 
                    match split_once( line, b'=' )
                    {
                        Some( kv ) => kv,
                        None => continue,
                    };

                    let key = trim_blank( key );
                    let key = if key.starts_with( b"XDG_" ) && key.ends_with( b"_DIR" )
                    {
                        match str::from_utf8( &key[4..key.len()-4] )
                        {
                            Ok( key ) =>
                            {
                                if user_dir.contains( &key )
                                {
                                    single_dir_found = true;
                                    key
                                } 
                                else if user_dir.is_none() { key } 
                                else { continue }
                            }                               
                            Err( _ )  => continue,
                        }
                    } 
                    else { continue };
                    
                    let value = trim_blank( value );
                    let mut value = if value.starts_with( b"\"" ) && value.ends_with( b"\"" )
                    { &value[1..value.len()-1] } 
                    else { continue };
                    
                    let is_relative = if value == b"$HOME/" { continue } 
                    else if value.starts_with( b"$HOME/" )
                    {
                        value = &value[b"$HOME/".len()..];
                        true
                    }
                    else if value.starts_with( b"/" ){ false }
                    else { continue };

                    let value = OsString::from_vec( shell_unescape( value ) );
                    let path = if is_relative
                    {
                        let mut path = PathBuf::from( &home_dir );
                        path.push( value );
                        path
                    }
                    else { PathBuf::from( value ) };

                    user_dirs.insert( key.to_owned(), path );
                    if single_dir_found
                    { break; }
                }
                user_dirs
            }

            /// Reads the entire contents of a file into a byte vector.
            fn read_all( path:&Path ) -> io::Result<Vec<u8>>
            {
                let mut file = fs::File::open( path )?;
                let mut bytes = Vec::with_capacity( 1024 );
                file.read_to_end( &mut bytes )?;
                Ok( bytes )
            }

            /// Returns bytes before and after first occurrence of separator.
            fn split_once( bytes:&[u8], separator:u8 ) -> Option<( &[u8], &[u8] )>
            {
                bytes.iter()
                .position( |b| *b == separator )
                .map( |i| 
                {
                    ( &bytes[..i], &bytes[i+1..] )
                } )
            }

            /// Returns a slice with leading and trailing <blank> characters removed.
            fn trim_blank( bytes:&[u8] ) -> &[u8]
            {
                let i = bytes.iter().cloned().take_while( |b| *b == b' ' || *b == b'\t' ).count();
                let bytes = &bytes[i..];
                let i = bytes.iter().cloned().rev().take_while( |b| *b == b' ' || *b == b'\t' ).count();
                &bytes[..bytes.len()-i]
            }

            /// Unescape bytes escaped with POSIX shell double-quotes rules ( as used by xdg-user-dirs-update ).
            fn shell_unescape( escaped:&[u8] ) -> Vec<u8>
            {
                let mut unescaped:Vec<u8> = Vec::with_capacity( escaped.len() );
                let mut i = escaped.iter().cloned();
                while let Some( b ) = i.next()
                {
                    if b == b'\\' { if let Some( b ) = i.next() { unescaped.push( b ); } }
                    else { unescaped.push( b ); }
                }
                unescaped
            }
        }

        #[cfg( all( unix, not( any( target_os = "macos", target_os = "ios" ) ) ) )]
        mod target_unix_not_mac
        {
            use ::
            {
                collections::{ HashMap },
                path::{ Path, PathBuf },
                *
            };
            use super::{home_dir, is_absolute_path};
            use super::xdg_user_dirs;

            fn user_dir_file( home_dir:&Path ) -> PathBuf
            {
                env::var_os( "XDG_CONFIG_HOME" )
                .and_then( is_absolute_path )
                .unwrap_or_else( || home_dir.join( ".config" ) )
                .join( "user-dirs.dirs" )
            }
            
            pub fn user_dir( user_dir_name:&str ) -> Option<PathBuf>
            {
                if let Some( home_dir ) = home_dir()
                { xdg_user_dirs::single( &home_dir, &user_dir_file( &home_dir ), user_dir_name ).remove( user_dir_name ) }
                else
                { None }
            }

            pub fn user_dirs( home_dir_path:&Path ) -> HashMap<String, PathBuf>
            { xdg_user_dirs::all( home_dir_path, &user_dir_file( home_dir_path ) ) }
        }

        #[cfg( all( unix, not( any( target_os = "macos", target_os = "ios" ) ) ) )]
        pub use self::target_unix_not_mac::{user_dir, user_dirs};

        #[cfg( target_os = "windows" )]
        extern crate windows_sys as windows;

        #[cfg( target_os = "windows" )]
        mod target_windows
        {
            use ::
            {
                ffi::{ c_void, OsString },
                os::windows::ffi::OsStringExt,
                path::{ PathBuf },
                *
            };
            use super::windows::Win32::UI::Shell;

            pub fn known_folder( folder_id:windows::core::GUID ) -> Option<PathBuf>
            {
                unsafe
                {
                    let mut path_ptr:windows::core::PWSTR = ptr::null_mut();
                    let result = Shell::SHGetKnownFolderPath
                    ( 
                        &folder_id,
                        0,
                        std::ptr::null_mut(),
                        &mut path_ptr
 );
                    if result == 0
                    {
                        let len = windows::Win32::Globalization::lstrlenW( path_ptr ) as usize;
                        let path = slice::from_raw_parts( path_ptr, len );
                        let ostr:OsString = OsStringExt::from_wide( path );
                        windows::Win32::System::Com::CoTaskMemFree( path_ptr as *const c_void );
                        Some( PathBuf::from( ostr ) )
                    } 
                    else
                    {
                        windows::Win32::System::Com::CoTaskMemFree( path_ptr as *const c_void );
                        None
                    }
                }
            }

            pub fn known_folder_profile() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Profile ) }

            pub fn known_folder_roaming_app_data() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_RoamingAppData ) }

            pub fn known_folder_local_app_data() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_LocalAppData ) }

            pub fn known_folder_music() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Music ) }

            pub fn known_folder_desktop() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Desktop ) }

            pub fn known_folder_documents() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Documents ) }

            pub fn known_folder_downloads() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Downloads ) }

            pub fn known_folder_pictures() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Pictures ) }

            pub fn known_folder_public() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Public ) }

            pub fn known_folder_templates() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Templates ) }
           
            pub fn known_folder_videos() -> Option<PathBuf>
            { known_folder( Shell::FOLDERID_Videos ) }
        }

        #[cfg( target_os = "windows" )] pub use self::target_windows::
        {
            known_folder, known_folder_profile, known_folder_roaming_app_data, known_folder_local_app_data, 
            known_folder_music, known_folder_desktop, known_folder_documents, known_folder_downloads,
            known_folder_pictures, known_folder_public, known_folder_templates, known_folder_videos
        };
    }
    /*
    dir v0.0.0*/
    /// Returns the path to the user's home directory.
    pub fn home_dir() -> Option<PathBuf>
    { sys::home_dir() }
    /// Returns the path to the user's cache directory.
    pub fn cache_dir() -> Option<PathBuf>
    { sys::cache_dir() }
    /// Returns the path to the user's config directory.
    pub fn config_dir() -> Option<PathBuf>
    { sys::config_dir() }
    /// Returns the path to the user's local config directory.
    pub fn config_local_dir() -> Option<PathBuf>
    { sys::config_local_dir() }
    /// Returns the path to the user's data directory.
    pub fn data_dir() -> Option<PathBuf>
    { sys::data_dir() }
    /// Returns the path to the user's local data directory.
    pub fn data_local_dir() -> Option<PathBuf>
    { sys::data_local_dir() }
    /// Returns the path to the user's executable directory.
    pub fn executable_dir() -> Option<PathBuf>
    { sys::executable_dir() }
    /// Returns the path to the user's preference directory.
    pub fn preference_dir() -> Option<PathBuf>
    { sys::preference_dir() }
    /// Returns the path to the user's runtime directory.
    pub fn runtime_dir() -> Option<PathBuf>
    { sys::runtime_dir() }
    /// Returns the path to the user's state directory.
    pub fn state_dir() -> Option<PathBuf>
    { sys::state_dir() }
    /// Returns the path to the user's audio directory.
    pub fn audio_dir() -> Option<PathBuf>
    { sys::audio_dir() }
    /// Returns the path to the user's desktop directory.
    pub fn desktop_dir() -> Option<PathBuf>
    { sys::desktop_dir() }
    /// Returns the path to the user's document directory.
    pub fn document_dir() -> Option<PathBuf>
    { sys::document_dir() }
    /// Returns the path to the user's download directory.
    pub fn download_dir() -> Option<PathBuf>
    { sys::download_dir() }
    /// Returns the path to the user's font directory.
    pub fn font_dir() -> Option<PathBuf>
    { sys::font_dir() }
    /// Returns the path to the user's picture directory.
    pub fn picture_dir() -> Option<PathBuf>
    { sys::picture_dir() }
    /// Returns the path to the user's public directory.
    pub fn public_dir() -> Option<PathBuf>
    { sys::public_dir() }
    /// Returns the path to the user's template directory.
    pub fn template_dir() -> Option<PathBuf>
    { sys::template_dir() }
    /// Returns the path to the user's video directory.
    pub fn video_dir() -> Option<PathBuf>
    { sys::video_dir() }
    /**/
    pub fn escape_path( path:&str ) -> String
    {
        let re = Regex::new( r##"( ?P<c>[!\( \ )<>,\?\]\[\{\} \\'"`*\^#|$&;] )"## ).unwrap();
        return re.replace_all( path, "\\$c" ).to_string();
    }
    /**/
    pub fn extend_bangbang( sh:&shell::Shell, line:&mut String )
    {
        if !re_contains( line, r"!!" ) {
            return;
        }
        if sh.previous_cmd.is_empty() {
            return;
        }

        let re = Regex::new( r"!!" ).unwrap();
        let mut replaced = false;
        let mut new_line = String::new();
        let linfo = parsers::line::parse_line( line );
        for ( sep, token ) in linfo.tokens {
            if !sep.is_empty() {
                new_line.push_str( &sep );
            }

            if re_contains( &token, r"!!" ) && sep != "'" {
                let line2 = token.clone();
                let result = re.replace_all( &line2, sh.previous_cmd.as_str() );
                new_line.push_str( &result );
                replaced = true;
            } else {
                new_line.push_str( &token );
            }

            if !sep.is_empty() {
                new_line.push_str( &sep );
            }
            new_line.push( ' ' );
        }

        *line = new_line.trim_end().to_string();
        if replaced {
            println!( "{}", line );
        }
    }
    /**/
    pub fn basename( path:&str ) -> Cow<'_, str>
    {
        let mut pieces = path.rsplit( '/' );
        match pieces.next() {
            Some( p ) => p.into(),
            None => path.into(),
        }
    }
    /**/
    pub fn find_file_in_path( filename:&str, exec:bool ) -> String
    {
        let env_path = match env::var( "PATH" ) {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "pls:error with env PATH:{:?}", e );
                return String::new();
            }
        };
        let vec_path:Vec<&str> = env_path.split( ':' ).collect();
        for p in &vec_path {
            match read_dir( p ) {
                Ok( list ) => {
                    for entry in list.flatten() {
                        if let Ok( name ) = entry.file_name().into_string() {
                            if name != filename {
                                continue;
                            }

                            if exec {
                                let _mode = match entry.metadata() {
                                    Ok( x ) => x,
                                    Err( e ) => {
                                        println_stderr!( "pls:metadata error:{:?}", e );
                                        continue;
                                    }
                                };
                                let mode = _mode.permissions().mode();
                                if mode & 0o111 == 0 {
                                    continue;
                                }
                            }

                            return entry.path().to_string_lossy().to_string();
                        }
                    }
                }
                Err( e ) => {
                    if e.kind() == ErrorKind::NotFound {
                        continue;
                    }
                    //log!( "pls:fs read_dir error:{}:{}", p, e );
                }
            }
        }
        String::new()
    }
    /**/
    pub fn current_dir() -> String
    {
        let _current_dir = match env::current_dir() {
            Ok( x ) => x,
            Err( e ) => {
                //log!( "pls:PROMPT:env current_dir error:{}", e );
                return String::new();
            }
        };
        let current_dir = match _current_dir.to_str() {
            Some( x ) => x,
            None => {
                //log!( "pls:PROMPT:to_str error" );
                return String::new();
            }
        };

        current_dir.to_string()
    }
}
/*
pest v0.0.0*/
pub mod pest
{
    /*
    pest-derive v0.0.0*/
    pub mod derive
    {

    }
}
pub mod pin { pub use std::pin::{ * }; }
pub mod print
{
    use ::
    {
        error::{ no }, //use errno::errno;
        fs::{ File },
        io::{ Write },
        os::unix::io::{ FromRawFd, RawFd },
        types::{ Command, CommandLine, CommandResult, Redirection },
        *,
    };
    /**/
    pub fn print_stdout( info:&str, cmd:&Command, cl:&CommandLine )
    {
        let fd = get::dupped_stdout_fd( cmd, cl );
        if fd == -1 {
            return;
        }

        unsafe {
            let mut f = File::from_raw_fd( fd );
            let info = info.trim_end_matches( '\n' );
            match f.write_all( info.as_bytes() ) {
                Ok( _ ) => {},
                Err( e ) => {
                    println_stderr!( "write_all:error:{}", e );
                }
            }
            if !info.is_empty() {
                match f.write_all( b"\n" ) {
                    Ok( _ ) => {},
                    Err( e ) => {
                        println_stderr!( "write_all:error:{}", e );
                    }
                }
            }
        }
    }
    /**/
    pub fn print_stderr( info:&str, cmd:&Command, cl:&CommandLine )
    {
        let fd = get::dupped_stderr_fd( cmd, cl );
        if fd == -1 {
            return;
        }

        unsafe {
            let mut f = File::from_raw_fd( fd );
            let info = info.trim_end_matches( '\n' );
            match f.write_all( info.as_bytes() ) {
                Ok( _ ) => (),
                Err( e ) => {
                    println_stderr!( "write_all:error:{}", e );
                }
            }

            if !info.is_empty() {
                match f.write_all( b"\n" ) {
                    Ok( _ ) => (),
                    Err( e ) => {
                        println_stderr!( "write_all:error:{}", e );
                    }
                }
            }
        }
    }

    pub fn print_stderr_with_capture( info:&str, cr:&mut CommandResult, cl:&CommandLine, cmd:&Command, capture:bool )
    {
        cr.status = 1;
        if capture {
            cr.stderr = info.to_string();
        } else {
            print_stderr( info, cmd, cl );
        }
    }

    pub fn print_stdout_with_capture( info:&str, cr:&mut CommandResult, cl:&CommandLine, cmd:&Command, capture:bool )
    {
        cr.status = 0;
        if capture {
            cr.stdout = info.to_string();
        } else {
            print_stdout( info, cmd, cl );
        }
    }
}
pub mod process
{
    pub use std::process::{ * };
    use ::
    {
        libc::{ c_int },
        nix::
        {
            unistd::{ fork as nix_fork, ForkResult },
            Error, Result
        },
        os::fd::{RawFd},
        *,
    };
    /*
    Make fork "safe again", in order not to touch the code in core.rs,
    https://github.com/nix-rust/nix/issues/586     */
    pub fn fork() -> Result<ForkResult>
    {
        unsafe{ nix_fork() }
    }
    /**/
    pub fn pipe() -> ::result::Result<( RawFd, RawFd ), Error>
    {
        let mut fds = mem::MaybeUninit::<[c_int; 2]>::uninit();
        let res = unsafe { libc::pipe( fds.as_mut_ptr() as *mut c_int ) };
        Error::result( res )?;
        unsafe { Ok( ( fds.assume_init()[0], fds.assume_init()[1] ) ) }
    }
}
pub mod prompt
{
    use ::
    {
        shell::{ self },
        *
    };

    use self::main::render_prompt;
    pub use self::multilines::EnterFunction;
    /*
    linefeed v0.0.0 */
    pub mod lines
    {

    }

    pub mod main
    {
        use ::{ * };

        const DEFAULT_PROMPT:&str =  "${COLOR_STATUS}$USER${RESET}@${COLOR_STATUS}$HOSTNAME${RESET}:${COLOR_STATUS}$CWD${RESET}$ ";
        use super::preset::apply_preset_item;
        use super::preset::apply_pyenv;

        pub fn render_prompt( sh:&shell::Shell, ps:&str ) -> String
        {
            let mut prompt = String::new();
            apply_pyenv( &mut prompt );

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
                        apply_command( &mut prompt, &token, &prefix, &suffix );
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
                        apply_prompt_item( sh, &mut prompt, &token );
                        token.clear();
                        met_dollar = false;
                        met_brace = false;
                        continue;
                    } else if c == '$' {
                        if token.is_empty() {
                            prompt.push( '$' );
                            met_dollar = true;
                            continue;
                        } else {
                            apply_prompt_item( sh, &mut prompt, &token );
                            token.clear();
                            continue;
                        }
                    } else if met_paren {
                        if is::prefix_char( c ) {
                            prefix.push( c );
                        } else if is::suffix_char( c ) {
                            suffix.push( c );
                        } else {
                            token.push( c );
                        }
                        continue;
                    } else if is::prompt_item_char( c, &token ) {
                        token.push( c );
                        continue;
                    } else if token.is_empty() {
                        prompt.push( '$' );
                        prompt.push( c );
                        met_dollar = false;
                        continue;
                    }
                }

                if c == '$' {
                    met_dollar = true;
                    continue;
                }

                if !token.is_empty() {
                    apply_prompt_item( sh, &mut prompt, &token );
                    token.clear();
                }
                prompt.push( c );
                met_dollar = false;
            }

            if !token.is_empty() {
                apply_prompt_item( sh, &mut prompt, &token );
                met_dollar = false;
            }

            if met_dollar {
                prompt.push( '$' );
            }

            if prompt.trim().is_empty() {
                //return format!( "pls-{} >> ", env!( "CARGO_PKG_VERSION" ) );
                return format!( "pls-{} >> ", ::env::var( "CARGO_PKG_VERSION" ) );
            }
            prompt
        }

        fn apply_prompt_item( sh:&shell::Shell, result:&mut String, token:&str )
        {
            if let Some( x ) = sh.get_env( token ) {
                result.push_str( &x );
                return;
            }
            apply_preset_item( sh, result, token );
        }

        fn apply_command( result:&mut String, token:&str, prefix:&str, suffix:&str )
        {
            let cr = now::run( token );
            let output = cr.stdout.trim();
            if !output.is_empty() {
                result.push_str( prefix );
                result.push_str( output );
                result.push_str( suffix );
            }
        }
    }

    pub mod preset
    {
        use ::
        {
            fs::{ File },
            io::{Read, Write},
            path::{ Path },
            *,
        };

        pub fn apply_seq( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::SEQ );
        }

        pub fn apply_end_seq( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::END_SEQ );
        }

        pub fn apply_esc( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::ESC );
        }

        pub fn apply_underlined( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::UNDERLINED );
        }

        pub fn apply_user( prompt:&mut String )
        {
            let username = get::username();
            prompt.push_str( &username );
        }

        pub fn apply_black( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLACK );
        }

        pub fn apply_black_b( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLACK_B );
        }

        pub fn apply_black_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLACK_BG );
        }

        pub fn apply_blue( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLUE );
        }

        pub fn apply_blue_b( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLUE_B );
        }

        pub fn apply_blue_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLUE_BG );
        }

        pub fn apply_bold( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BOLD );
        }

        pub fn apply_green( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GREEN );
        }

        pub fn apply_green_b( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GREEN_B );
        }

        pub fn apply_green_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GREEN_BG );
        }

        pub fn apply_red( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RED );
        }

        pub fn apply_red_b( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RED_B );
        }

        pub fn apply_red_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RED_BG );
        }

        pub fn apply_white( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::WHITE );
        }

        pub fn apply_white_b( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::WHITE_B );
        }

        pub fn apply_white_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::WHITE_BG );
        }

        pub fn apply_hidden( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::HIDDEN );
        }

        pub fn apply_reset( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET );
        }

        pub fn apply_reverse( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::REVERSE );
        }

        pub fn apply_dim( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::DIM );
        }

        pub fn apply_blink( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLINK );
        }

        pub fn apply_reset_underlined( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_UNDERLINED );
        }

        pub fn apply_reset_dim( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_DIM );
        }

        pub fn apply_reset_reverse( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_REVERSE );
        }

        pub fn apply_reset_hidden( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_HIDDEN );
        }

        pub fn apply_reset_blink( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_BLINK );
        }

        pub fn apply_reset_bold( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RESET_BOLD );
        }

        pub fn apply_default( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::DEFAULT );
        }

        pub fn apply_default_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::DEFAULT_BG );
        }

        pub fn apply_cyan( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::CYAN );
        }

        pub fn apply_cyan_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::CYAN_L );
        }

        pub fn apply_cyan_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::CYAN_BG );
        }

        pub fn apply_cyan_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::CYAN_L_BG );
        }

        pub fn apply_red_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RED_L );
        }

        pub fn apply_red_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::RED_L_BG );
        }

        pub fn apply_green_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GREEN_L );
        }

        pub fn apply_green_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GREEN_L_BG );
        }

        pub fn apply_gray_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GRAY_L );
        }

        pub fn apply_gray_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GRAY_L_BG );
        }

        pub fn apply_gray_d( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GRAY_D );
        }

        pub fn apply_gray_d_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::GRAY_D_BG );
        }

        pub fn apply_magenta( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::MAGENTA );
        }

        pub fn apply_magenta_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::MAGENTA_BG );
        }

        pub fn apply_magenta_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::MAGENTA_L );
        }

        pub fn apply_magenta_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::MAGENTA_L_BG );
        }

        pub fn apply_yellow( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::YELLOW );
        }

        pub fn apply_yellow_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::YELLOW_BG );
        }

        pub fn apply_yellow_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::YELLOW_L );
        }

        pub fn apply_yellow_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::YELLOW_L_BG );
        }

        pub fn apply_blue_l( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLUE_L );
        }

        pub fn apply_blue_l_bg( prompt:&mut String )
        {
            prompt.push_str( prompt::colored::BLUE_L_BG );
        }

        pub fn apply_color_status( sh:&shell::Shell, prompt:&mut String )
        {
            if sh.previous_status == 0 {
                prompt.push_str( prompt::colored::GREEN_B );
            } else {
                prompt.push_str( prompt::colored::RED_B );
            }
        }

        pub fn _find_git_root() -> String
        {
            let current_dir = path::current_dir();
            let dir_git = format!( "{}/.git", current_dir );
            if Path::new( &dir_git ).exists() {
                return current_dir;
            }

            let mut _dir = current_dir.clone();
            while Path::new( &_dir ).parent().is_some() {
                match Path::new( &_dir ).parent() {
                    Some( p ) => {
                        _dir = p.to_string_lossy().to_string();
                        let dir_git = format!( "{}/.git", _dir );
                        if Path::new( &dir_git ).exists() {
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

        pub fn apply_gitbr( prompt:&mut String )
        {
            let git_root = _find_git_root();
            if git_root.is_empty() {
                return;
            }

            let file_head = format!( "{}/.git/HEAD", git_root );
            if !Path::new( &file_head ).exists() {
                return;
            }

            let mut file;
            match File::open( &file_head ) {
                Ok( x ) => file = x,
                Err( e ) => {
                    println!( "pls:.git/HEAD err:{:?}", e );
                    return;
                }
            }
            let mut text = String::new();
            match file.read_to_string( &mut text ) {
                Ok( _ ) => {}
                Err( e ) => {
                    println!( "pls:read_to_string error:{:?}", e );
                    return;
                }
            }

            if let Some( branch ) = regex::find_first_group( r"^[a-z]+:?[a-z]+/[a-z]+/( .+ )$", text.trim() )
            {
                apply_blue_b( prompt );
                if let Ok( x ) = env::var( "CICADA_GITBR_PREFIX" ) {
                    prompt.push_str( &x );
                }

                let _len_default:i32 = 32;
                let mut len_max = if let Ok( x ) = env::var( "CICADA_GITBR_MAX_LEN" ) {
                    match x.parse::<i32>() {
                        Ok( n ) => n,
                        Err( _ ) => _len_default,
                    }
                } else {
                    _len_default
                };
                if len_max <= 0 {
                    len_max = _len_default;
                }

                if branch.len() as i32 <= len_max {
                    prompt.push_str( &branch );
                } else {
                    let len = branch.len() as i32;
                    let offset = ( len - len_max + 2 ) as usize;
                    let branch_short = format!( "..{}", &branch[offset..] );
                    prompt.push_str( &branch_short );
                }
                if let Ok( x ) = env::var( "CICADA_GITBR_SUFFIX" ) {
                    prompt.push_str( &x );
                }
                apply_reset( prompt );
            }
        }

        pub fn apply_cwd( prompt:&mut String )
        {
            let _current_dir = match env::current_dir() {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "pls:PROMPT:env current_dir error:{}", e );
                    return;
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some( x ) => x,
                None => {
                    println_stderr!( "pls:PROMPT:to_str error" );
                    return;
                }
            };
            let _tokens:Vec<&str> = current_dir.split( '/' ).collect();

            let last = match _tokens.last() {
                Some( x ) => x,
                None => {
                    //log!( "pls:PROMPT:token last error" );
                    return;
                }
            };

            let home = get::user_home();
            let pwd = if last.is_empty() {
                "/"
            } else if current_dir == home {
                "~"
            } else {
                last
            };
            prompt.push_str( pwd );
        }

        pub fn apply_hostname( prompt:&mut String )
        {
            let hostname = get::hostname();
            prompt.push_str( &hostname );
        }

        pub fn apply_newline( prompt:&mut String )
        {
            prompt.push( '\n' );
        }

        pub fn apply_pyenv( prompt:&mut String )
        {
            if let Ok( x ) = env::var( "VIRTUAL_ENV" ) {
                if !x.is_empty() {
                    let _tokens:Vec<&str> = x.split( '/' ).collect();
                    let env_name = match _tokens.last() {
                        Some( x ) => x,
                        None => {
                            //log!( "prompt token last error" );
                            return;
                        }
                    };

                    apply_blue_b( prompt );
                    prompt.push( '(' );
                    prompt.push_str( env_name );
                    prompt.push( ')' );
                    apply_reset( prompt );
                }
            }
        }

        pub fn apply_preset_item( sh:&shell::Shell, prompt:&mut String, token:&str )
        {
            match token.to_ascii_lowercase().as_ref() {
                "black" => apply_black( prompt ),
                "black_b" => apply_black_b( prompt ),
                "black_bg" => apply_black_bg( prompt ),
                "blink" => apply_blink( prompt ),
                "blue" => apply_blue( prompt ),
                "blue_b" => apply_blue_b( prompt ),
                "blue_bg" => apply_blue_bg( prompt ),
                "blue_l" => apply_blue_l( prompt ),
                "blue_l_bg" => apply_blue_l_bg( prompt ),
                "bold" => apply_bold( prompt ),
                "color_status" => apply_color_status( sh, prompt ),
                "cwd" => apply_cwd( prompt ),
                "cyan" => apply_cyan( prompt ),
                "cyan_bg" => apply_cyan_bg( prompt ),
                "cyan_l" => apply_cyan_l( prompt ),
                "cyan_l_bg" => apply_cyan_l_bg( prompt ),
                "default" => apply_default( prompt ),
                "default_bg" => apply_default_bg( prompt ),
                "dim" => apply_dim( prompt ),
                "end_seq" => apply_end_seq( prompt ),
                "esc" => apply_esc( prompt ),
                "gitbr" => apply_gitbr( prompt ),
                "gray_d" => apply_gray_d( prompt ),
                "gray_d_bg" => apply_gray_d_bg( prompt ),
                "gray_l" => apply_gray_l( prompt ),
                "gray_l_bg" => apply_gray_l_bg( prompt ),
                "green" => apply_green( prompt ),
                "green_b" => apply_green_b( prompt ),
                "green_bg" => apply_green_bg( prompt ),
                "green_l" => apply_green_l( prompt ),
                "green_l_bg" => apply_green_l_bg( prompt ),
                "hidden" => apply_hidden( prompt ),
                "hostname" => apply_hostname( prompt ),
                "magenta" => apply_magenta( prompt ),
                "magenta_bg" => apply_magenta_bg( prompt ),
                "magenta_l" => apply_magenta_l( prompt ),
                "magenta_l_bg" => apply_magenta_l_bg( prompt ),
                "newline" => apply_newline( prompt ),
                "red" => apply_red( prompt ),
                "red_b" => apply_red_b( prompt ),
                "red_bg" => apply_red_bg( prompt ),
                "red_l" => apply_red_l( prompt ),
                "red_l_bg" => apply_red_l_bg( prompt ),
                "reset" => apply_reset( prompt ),
                "reset_blink" => apply_reset_blink( prompt ),
                "reset_bold" => apply_reset_bold( prompt ),
                "reset_dim" => apply_reset_dim( prompt ),
                "reset_hidden" => apply_reset_hidden( prompt ),
                "reset_reverse" => apply_reset_reverse( prompt ),
                "reset_underlined" => apply_reset_underlined( prompt ),
                "reverse" => apply_reverse( prompt ),
                "seq" => apply_seq( prompt ),
                "underlined" => apply_underlined( prompt ),
                "user" => apply_user( prompt ),
                "white" => apply_white( prompt ),
                "white_b" => apply_white_b( prompt ),
                "white_bg" => apply_white_bg( prompt ),
                "yellow" => apply_yellow( prompt ),
                "yellow_bg" => apply_yellow_bg( prompt ),
                "yellow_l" => apply_yellow_l( prompt ),
                "yellow_l_bg" => apply_yellow_l_bg( prompt ),
                _ => (),
            }
        }
    }

    pub mod multilines
    {
        use ::
        {
            parsers::line::{ self },
            prompt::lines::{ Function, Prompter, Terminal },
            *,
        };

        pub struct EnterFunction;

        impl<T:Terminal> Function<T> for EnterFunction
        {
            fn execute( &self, prompter:&mut Prompter<T>, count:i32, _ch:char ) -> io::Result<()> {
                let buf = prompter.buffer();
                let linfo = parsers::line::parse_line( buf );
                if linfo.is_complete {
                    prompter.accept_input()
                } else if count > 0 {
                    match prompter.insert( count as usize, '\n' ) {
                        Ok( _ ) => {},
                        Err( e ) => {
                            println!( "sub-prompt error:{}", e );
                        }
                    }
                    prompter.insert_str( ">> " )
                } else {
                    Ok( () )
                }
            }
        }
    }

    pub mod colored
    {
        pub const SEQ:&str = "\x01";
        pub const END_SEQ:&str = "\x02";
        pub const ESC:&str = "\x1B";
        pub const BOLD:&str = "\x01\x1B[1m\x02";
        pub const DIM:&str = "\x01\x1B[2m\x02";
        pub const UNDERLINED:&str = "\x01\x1B[4m\x02";
        pub const BLINK:&str = "\x01\x1B[5m\x02";
        pub const REVERSE:&str = "\x01\x1B[7m\x02";
        pub const HIDDEN:&str = "\x01\x1B[8m\x02";
        pub const RESET:&str = "\x01\x1B[0m\x02";
        pub const RESET_BOLD:&str = "\x01\x1B[21m\x02";
        pub const RESET_DIM:&str = "\x01\x1B[22m\x02";
        pub const RESET_UNDERLINED:&str = "\x01\x1B[24m\x02";
        pub const RESET_BLINK:&str = "\x01\x1B[25m\x02";
        pub const RESET_REVERSE:&str = "\x01\x1B[27m\x02";
        pub const RESET_HIDDEN:&str = "\x01\x1B[28m\x02";
        pub const DEFAULT:&str = "\x01\x1B[39m\x02";
        pub const BLACK:&str = "\x01\x1B[30m\x02";
        pub const RED:&str = "\x01\x1B[31m\x02";
        pub const GREEN:&str = "\x01\x1B[32m\x02";
        pub const YELLOW:&str = "\x01\x1B[33m\x02";
        pub const BLUE:&str = "\x01\x1B[34m\x02";
        pub const MAGENTA:&str = "\x01\x1B[35m\x02";
        pub const CYAN:&str = "\x01\x1B[36m\x02";
        pub const GRAY_L:&str = "\x01\x1B[37m\x02";
        pub const GRAY_D:&str = "\x01\x1B[90m\x02";
        pub const RED_L:&str = "\x01\x1B[91m\x02";
        pub const GREEN_L:&str = "\x01\x1B[92m\x02";
        pub const YELLOW_L:&str = "\x01\x1B[93m\x02";
        pub const BLUE_L:&str = "\x01\x1B[94m\x02";
        pub const MAGENTA_L:&str = "\x01\x1B[95m\x02";
        pub const CYAN_L:&str = "\x01\x1B[96m\x02";
        pub const WHITE:&str = "\x01\x1B[97m\x02";
        pub const BLUE_B:&str = "\x01\x1B[34m\x1B[1m\x02";
        pub const BLACK_B:&str = "\x01\x1B[30m\x1B[1m\x02";
        pub const WHITE_B:&str = "\x01\x1B[97m\x1B[1m\x02";
        pub const RED_B:&str = "\x01\x1B[31m\x1B[1m\x02";
        pub const GREEN_B:&str = "\x01\x1B[32m\x1B[1m\x02";
        pub const DEFAULT_BG:&str = "\x01\x1B[49m\x02";
        pub const BLACK_BG:&str   = "\x01\x1B[40m\x02";
        pub const RED_BG:&str     = "\x01\x1B[41m\x02";
        pub const GREEN_BG:&str   = "\x01\x1B[42m\x02";
        pub const YELLOW_BG:&str   = "\x01\x1B[43m\x02";
        pub const BLUE_BG:&str    = "\x01\x1B[44m\x02";
        pub const MAGENTA_BG:&str    = "\x01\x1B[45m\x02";
        pub const CYAN_BG:&str    = "\x01\x1B[46m\x02";
        pub const GRAY_L_BG:&str    = "\x01\x1B[47m\x02";
        pub const GRAY_D_BG:&str   = "\x01\x1B[100m\x02";
        pub const RED_L_BG:&str   = "\x01\x1B[101m\x02";
        pub const GREEN_L_BG:&str   = "\x01\x1B[102m\x02";
        pub const YELLOW_L_BG:&str   = "\x01\x1B[103m\x02";
        pub const BLUE_L_BG:&str   = "\x01\x1B[104m\x02";
        pub const MAGENTA_L_BG:&str   = "\x01\x1B[105m\x02";
        pub const CYAN_L_BG:&str   = "\x01\x1B[106m\x02";
        pub const WHITE_BG:&str   = "\x01\x1B[107m\x02";
    }
}
pub mod ptr { pub use std::ptr::{ * }; }
pub mod rc
{
    pub use std::rc::{ * };
    /* ./rcfile.rs */
    pub mod file
    {
        use ::
        {
            path::{ Path },
            *,
        };

        pub fn get_rc_file() -> String
        {
            let dir_config = get::configuration_directory();
            let rc_file = format!( "{}/cicadarc", dir_config );
            if Path::new( &rc_file ).exists() {
                return rc_file;
            }
            
            let home = get::user_home();
            let rc_file_home = format!( "{}/{}", home, ".cicadarc" );
            if Path::new( &rc_file_home ).exists() {
                return rc_file_home;
            }
            
            rc_file
        }

        pub fn load_rc_files( sh:&mut shell::Shell )
        {
            let rc_file = get_rc_file();
            if !Path::new( &rc_file ).exists() {
                return;
            }

            let args = vec!["source".to_string(), rc_file];
            scripts::run_script( sh, &args );
        }
    }
}
/*
regex v1.5.6 */
pub mod regex
{
    pub use re::{ * };

    pub fn find_first_group( ptn:&str, text:&str ) -> Option<String>
    {
        let rex = match re::Regex::new( ptn )
        {
            Ok( x ) => x,
            Err( _ ) => return None,
        };

        match rex.captures( text )
        {
            Some( caps ) => {
                if let Some( x ) = caps.get( 1 ) {
                    return Some( x.as_str().to_owned() );
                }
            }
            None => {
                return None;
            }
        }
        None
    }

    pub fn re_contains( text:&str, ptn:&str ) -> bool
    {
        let rex = match re::Regex::new( ptn )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println!( "Regex new error:{:?}", e );
                return false;
            }
        };

        rex.is_match( text )
    }

    pub fn replace_all( text:&str, ptn:&str, ptn_to:&str ) -> String
    {
        let rex = re::Regex::new( ptn ).unwrap();
        let result = rex.replace_all( text, ptn_to );
        result.to_string()
    }
}
pub mod result { pub use std::result::{ * }; }
pub mod rust { pub use std::prelude::v1::{ * }; }
pub mod scripts
{
    // use pest::iterators::Pair;
    use ::
    {
        fs::{ File },
        io::{ Read, Write, ErrorKind },
        path::{ Path },
        regex::{ Regex, RegexBuilder },
        types::{ self, CommandResult },
        *,
    };

    pub fn run_script( sh:&mut shell::Shell, args:&Vec<String> ) -> i32
    {
        let src_file = &args[1];
        let full_src_file:String;
        if src_file.contains( '/' ) {
            full_src_file = src_file.clone();
        } else {
            let full_path = path::find_file_in_path( src_file, false );
            if full_path.is_empty() {
                // not in PATH and not in current work directory
                if !Path::new( src_file ).exists() {
                    println_stderr!( "pls:{}:no such file", src_file );
                    return 1;
                }
                full_src_file = format!( "./{}", src_file );
            } else {
                full_src_file = full_path.clone();
            }
        }

        if !Path::new( &full_src_file ).exists() {
            println_stderr!( "pls:{}:no such file", src_file );
            return 1;
        }
        if Path::new( &full_src_file ).is_dir() {
            println_stderr!( "pls:{}:is a directory", src_file );
            return 1;
        }

        let mut file;
        match File::open( &full_src_file ) {
            Ok( x ) => file = x,
            Err( e ) => {
                println_stderr!( "pls:{}:failed to open file - {:?}", &full_src_file, e.kind() );
                return 1;
            }
        }
        let mut text = String::new();
        match file.read_to_string( &mut text ) {
            Ok( _ ) => {}
            Err( e ) => {
                match e.kind() {
                    ErrorKind::InvalidData => {
                        println_stderr!( "pls:{}:not a valid script file", &full_src_file );
                    }
                    _ => {
                        println_stderr!( "pls:{}:error:{:?}", &full_src_file, e );
                    }
                }
                return 1;
            }
        }

        if text.contains( "\\\n" ) {
            let re = RegexBuilder::new( r#"( [ \t]*\\\n[ \t]+ )|( [ \t]+\\\n[ \t]* )"# )
                .multi_line( true ).build().unwrap();
            text = re.replace_all( &text, " " ).to_string();

            let re = RegexBuilder::new( r#"\\\n"# ).multi_line( true ).build().unwrap();
            text = re.replace_all( &text, "" ).to_string();
        }

        let re_func_head = Regex::new( r"^function ( [a-zA-Z_-][a-zA-Z0-9_-]* ) *( ?:\( \ ) )? *\{$" ).unwrap();
        let re_func_tail = Regex::new( r"^\}$" ).unwrap();
        let mut text_new = String::new();
        let mut enter_func = false;
        let mut func_name = String::new();
        let mut func_body = String::new();
        for line in text.clone().lines() {
            if re_func_head.is_match( line.trim() ) {
                enter_func = true;
                let cap = re_func_head.captures( line.trim() ).unwrap();
                func_name = cap[1].to_string();
                func_body = String::new();
                continue;
            }
            if re_func_tail.is_match( line.trim() ) {
                sh.set_func( &func_name, &func_body );
                enter_func = false;
                continue;
            }
            if enter_func {
                func_body.push_str( line );
                func_body.push( '\n' );
            } else {
                text_new.push_str( line );
                text_new.push( '\n' );
            }
        }

        let mut status = 0;
        let cr_list = run_lines( sh, &text_new, args, false );
        if let Some( last ) = cr_list.last() {
            status = last.status;
        }
        /* FIXME:
        We probably need to fix the issue in the `set` builtin, 
        which currently set `exit_on_error` at the shell session level.
        We should instead set in a script-level.
        Here is a work-around ugly fix. */
        sh.exit_on_error = false;
        status
    }

    pub fn run_lines( sh:&mut shell::Shell, lines:&str, args:&Vec<String>, capture:bool ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        match parsers::locust::parse_lines( lines ) {
            Ok( pairs_exp ) => {
                for pair in pairs_exp {
                    let ( mut _cr_list, _cont, _brk ) = run_exp( sh, pair, args, false, capture );
                    cr_list.append( &mut _cr_list );
                }
            }
            Err( e ) => {
                println_stderr!( "syntax error:{:?}", e );
                return cr_list;
            }
        }
        cr_list
    }

    pub fn run_exp_test_br
    ( 
        sh:&mut shell::Shell,
        pair_br:Pair<parsers::locust::Rule>,
        args:&Vec<String>,
        in_loop:bool,
        capture:bool
 ) -> ( Vec<CommandResult>, bool, bool, bool )
    {
        let mut cr_list = Vec::new();
        let pairs = pair_br.into_inner();
        let mut test_pass = false;
        /*
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::IF_HEAD ||
                    rule == parsers::locust::Rule::IF_ELSEIF_HEAD ||
                    rule == parsers::locust::Rule::WHILE_HEAD {
                let pairs_test:Vec<Pair<parsers::locust::Rule>> =
                    pair.into_inner().collect();
                let pair_test = &pairs_test[0];
                let line = pair_test.as_str().trim();
                let line_new = expand_args( line, &args[1..] );
                let mut _cr_list = now::run_command_line( sh, &line_new, true, capture );
                if let Some( last ) = _cr_list.last() {
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
                    return ( cr_list, false, false, false );
                }
                let ( mut _cr_list, _cont, _brk ) = run_exp( sh, pair, args, in_loop, capture );
                cr_list.append( &mut _cr_list );
                
                return ( cr_list, true, _cont, _brk );
            }

            unreachable!();
        }
        */
        ( cr_list, test_pass, false, false )
    }

    pub fn run_exp_if
    ( 
        sh:&mut shell::Shell,
        pair_if:Pair<parsers::locust::Rule>,
        args:&Vec<String>,
        in_loop:bool,
        capture:bool
 ) -> ( Vec<CommandResult>, bool, bool )
    {
        let mut cr_list = Vec::new();
        let pairs = pair_if.into_inner();
        let mut met_continue = false;
        let mut met_break = false;
        /*
        for pair in pairs
        {
            let ( mut _cr_list, passed, _cont, _brk ) = run_exp_test_br( sh, pair, args, in_loop, capture );
            met_continue = _cont;
            met_break = _brk;
            cr_list.append( &mut _cr_list );
            if passed {
                break;
            }
        }
        */
        ( cr_list, met_continue, met_break )
    }

    pub fn run_exp_for
    ( 
        sh:&mut shell::Shell,
        pair_for:Pair<parsers::locust::Rule>,
        args:&Vec<String>,
        capture:bool
 ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        /*
        let pairs = pair_for.into_inner();
        let mut result_list:Vec<String> = Vec::new();
        let mut var_name:String = String::new();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_HEAD {
                var_name = get_for_var_name( pair.clone() );
                result_list = get_for_result_list( sh, pair.clone(), args );
                continue;
            }
            if rule == parsers::locust::Rule::EXP_BODY {
                for value in &result_list {
                    sh.set_env( &var_name, value );
                    let ( mut _cr_list, _cont, _brk ) = run_exp( 
                        sh, pair.clone(), args, true, capture );
                    cr_list.append( &mut _cr_list );
                    if _brk {
                        break;
                    }
                }
            }
        }*/
        cr_list
    }

    pub fn run_exp_while
    ( 
        sh:&mut shell::Shell,
        pair_while:Pair<parsers::locust::Rule>,
        args:&Vec<String>,
        capture:bool
 ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        loop
        {
            let ( mut _cr_list, passed, _cont, _brk ) = run_exp_test_br( sh, pair_while.clone(), args, true, capture );
            cr_list.append( &mut _cr_list );
            if !passed || _brk {
                break;
            }
        }
        cr_list
    }

    pub fn run_exp
    ( 
        sh:&mut shell::Shell,
        pair_in:Pair<parsers::locust::Rule>,
        args:&Vec<String>,
        in_loop:bool,
        capture:bool
 ) -> ( Vec<CommandResult>, bool, bool )
    {
        let mut cr_list = Vec::new();
        /*
        let pairs = pair_in.into_inner();
        for pair in pairs
        {
            let line = pair.as_str().trim();
            if line.is_empty() {
                continue;
            }

            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::CMD {
                if line == "continue" {
                    if in_loop {
                        return ( cr_list, true, false );
                    } else {
                        println_stderr!( "pls:continue:only meaningful in loops" );
                        continue;
                    }
                }
                if line == "break" {
                    if in_loop {
                        return ( cr_list, false, true );
                    } else {
                        println_stderr!( "pls:break:only meaningful in loops" );
                        continue;
                    }
                }

                let line_new = expand_args( line, &args[1..] );
                let mut _cr_list = now::run_command_line( sh, &line_new, true, capture );
                cr_list.append( &mut _cr_list );
                if let Some( last ) = cr_list.last() {
                    let status = last.status;
                    if status != 0 && sh.exit_on_error {
                        return ( cr_list, false, false );
                    }
                }
            } else if rule == parsers::locust::Rule::EXP_IF {
                let ( mut _cr_list, _cont, _brk ) = run_exp_if( sh, pair, args, in_loop, capture );
                cr_list.append( &mut _cr_list );
                if _cont {
                    return ( cr_list, true, false );
                }
                if _brk {
                    return ( cr_list, false, true );
                }
            } else if rule == parsers::locust::Rule::EXP_FOR {
                let mut _cr_list = run_exp_for( sh, pair, args, capture );
                cr_list.append( &mut _cr_list );
            } else if rule == parsers::locust::Rule::EXP_WHILE {
                let mut _cr_list = run_exp_while( sh, pair, args, capture );
                cr_list.append( &mut _cr_list );
            }
        }
        */
        ( cr_list, false, false )
    }
}
mod shell
{
    // uuid::Uuid
    use ::
    {
        collections::{ HashMap, HashSet },
        error::{ no },
        ffi::{ CStr, CString },
        fs::{ File },
        io::{ Read, Write },
        os::
        {
            fd::RawFd,
            unix::io::FromRawFd,
        },
        nix::unistd::{ execve, ForkResult },
        regex::{ Regex },
        types::{ self, CommandLine, CommandOptions, CommandResult },
        *,
    };
    /**/
    pub fn try_run_builtin_in_subprocess
    ( 
        sh:&mut Shell,
        cl:&CommandLine,
        idx_cmd:usize,
        capture:bool,
 ) -> Option<i32>
    {
        if let Some( cr ) = try_run_builtin( sh, cl, idx_cmd, capture ) {
            return Some( cr.status );
        }
        None
    }
    /**/
    pub fn try_run_builtin
    ( 
        sh:&mut Shell,
        cl:&CommandLine,
        idx_cmd:usize,
        capture:bool,
 ) -> Option<CommandResult>
    {
        let capture = capture && idx_cmd + 1 == cl.commands.len();

        if idx_cmd >= cl.commands.len() {
            println_stderr!( "unexpected error in try_run_builtin" );
            return None;
        }

        let cmd = &cl.commands[idx_cmd];
        let tokens = cmd.tokens.clone();
        let cname = tokens[0].1.clone();
        if cname == "alias" {
            let cr = builtins::alias::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "bg" {
            let cr = builtins::bg::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "cd" {
            let cr = builtins::cd::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "cinfo" {
            let cr = builtins::cinfo::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "exec" {
            let cr = builtins::exec::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "exit" {
            let cr = builtins::exit::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "export" {
            let cr = builtins::export::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "fg" {
            let cr = builtins::fg::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "history" {
            let cr = builtins::history::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "jobs" {
            let cr = builtins::jobs::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "minfd" {
            let cr = builtins::minfd::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "read" {
            let cr = builtins::read::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "set" {
            let cr = builtins::set::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "source" {
            let cr = builtins::source::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "ulimit" {
            let cr = builtins::ulimit::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unalias" {
            let cr = builtins::unalias::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unset" {
            let cr = builtins::unset::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unpath" {
            let cr = builtins::unpath::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "vox" {
            let cr = builtins::vox::run( sh, cl, cmd, capture );
            return Some( cr );
        }
        None
    }
    /// Run a pipeline ( e.g. `echo hi | wc -l` )
    /// returns:( is-terminal-given, command-result )
    pub fn run_pipeline
    ( 
        sh:&mut shell::Shell,
        cl:&CommandLine,
        tty:bool,
        capture:bool,
        log_cmd:bool,
 ) -> ( bool, CommandResult )
    {
        let mut term_given = false;
        if cl.background && capture {
            println_stderr!( "pls:cannot capture output of background cmd" );
            return ( term_given, CommandResult::error() );
        }
        /*
        if let Some( cr ) = try_run_calculator( &cl.line, capture ) { return ( term_given, cr ); }
        FIXME:move func-run into run single command */
        if let Some( cr ) = try_run_func( sh, cl, capture, log_cmd ){ return ( term_given, cr ); }

        if log_cmd
        {
            //log!( "run:{}", cl.line );
        }

        let length = cl.commands.len();
        if length == 0
        {
            println!( "pls:invalid command:cmds with empty length" );
            return ( false, CommandResult::error() );
        }

        let mut pipes = Vec::new();
        let mut errored_pipes = false;
        for _ in 0..length - 1
        {
            match process::pipe()
            {
                Ok( fds ) => pipes.push( fds ),
                Err( e ) =>
                {
                    errored_pipes = true;
                    println_stderr!( "pls:pipeline1:{}", e );
                    break;
                }
            }
        }

        if errored_pipes
        {
            for fds in pipes {
                fs::close( fds.0 );
                fs::close( fds.1 );
            }
            return ( false, CommandResult::error() );
        }

        if pipes.len() + 1 != length
        {
            println!( "pls:invalid command:unmatched pipes count" );
            return ( false, CommandResult::error() );
        }

        let mut pgid:i32 = 0;
        let mut fg_pids:Vec<i32> = Vec::new();
        let isatty = if tty { unsafe { libc::isatty( 1 ) == 1 } } else { false };
        let options = CommandOptions
        {
            isatty,
            capture_output:capture,
            background:cl.background,
            envs:cl.envs.clone(),
        };
        let mut fds_capture_stdout = None;
        let mut fds_capture_stderr = None;
        if capture
        {
            match process::pipe()
            {
                Ok( fds ) => fds_capture_stdout = Some( fds ),
                Err( e ) =>
                {
                    println_stderr!( "pls:pipeline2:{}", e );
                    return ( false, CommandResult::error() );
                }
            }
            match process::pipe()
            {
                Ok( fds ) => fds_capture_stderr = Some( fds ),
                Err( e ) =>
                {
                    if let Some( fds ) = fds_capture_stdout
                    {
                        fs::close( fds.0 );
                        fs::close( fds.1 );
                    }
                    println_stderr!( "pls:pipeline3:{}", e );
                    return ( false, CommandResult::error() );
                }
            }
        }

        let mut cmd_result = CommandResult::new();
        for i in 0..length
        {
            let child_id:i32 = run_single_program
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

            if child_id > 0 && !cl.background
            {
                fg_pids.push( child_id );
            }
        }

        if cl.is_single_and_builtin(){ return ( false, cmd_result ); }

        if cl.background
        {
            if let Some( job ) = sh.get_job_by_gid( pgid ){ println_stderr!( "[{}] {}", job.id, job.gid ); }
        }

        if !fg_pids.is_empty()
        {
            let _cr = c::job::wait_fg_job( sh, pgid, &fg_pids );
            /*
            For capture commands, e.g. `echo foo` in `echo "hello $( echo foo )",
            the cmd_result is already built in loop calling run_single_program() above.*/
            if !capture { cmd_result = _cr; }
        }
        ( term_given, cmd_result )
    }
    /// Run a single command.
    /// e.g. the `sort -k2` part of `ps ax | sort -k2 | head`
    pub fn run_single_program
    ( 
        sh:&mut shell::Shell,
        cl:&CommandLine,
        idx_cmd:usize,
        options:&CommandOptions,
        pgid:&mut i32,
        term_given:&mut bool,
        cmd_result:&mut CommandResult,
        pipes:&[( RawFd, RawFd )],
        fds_capture_stdout:&Option<( RawFd, RawFd )>,
        fds_capture_stderr:&Option<( RawFd, RawFd )>,
 ) -> i32
    {
        unsafe
        {
            let capture = options.capture_output;
            if cl.is_single_and_builtin() {
                if let Some( cr ) = try_run_builtin( sh, cl, idx_cmd, capture ) {
                    *cmd_result = cr;
                    return unsafe { libc::getpid() };
                }

                println_stderr!( "pls:error when run singler builtin" );
                //log!( "error when run singler builtin:{:?}", cl );
                return 1;
            }

            let pipes_count = pipes.len();
            let mut fds_stdin = None;
            let cmd = cl.commands.get( idx_cmd ).unwrap();

            if cmd.has_here_string() {
                match process::pipe() {
                    Ok( fds ) => fds_stdin = Some( fds ),
                    Err( e ) => {
                        println_stderr!( "pls:pipeline4:{}", e );
                        return 1;
                    }
                }
            }

            match process::fork()
            {
                Ok( ForkResult::Child ) =>
                {
                    unsafe
                    {
                        libc::signal( libc::SIGTSTP, libc::SIG_DFL );
                        libc::signal( libc::SIGQUIT, libc::SIG_DFL );
                    }
                    
                    if idx_cmd > 0
                    {
                        for i in 0..idx_cmd - 1
                        {
                            let fds = pipes[i];
                            fs::close( fds.0 );
                            fs::close( fds.1 );
                        }
                    }
                    
                    for i in idx_cmd + 1..pipes_count
                    {
                        let fds = pipes[i];
                        fs::close( fds.0 );
                        fs::close( fds.1 );
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        if let Some( fds ) = fds_capture_stdout
                        {
                            fs::close( fds.0 );
                            fs::close( fds.1 );
                        }
                        if let Some( fds ) = fds_capture_stderr
                        {
                            fs::close( fds.0 );
                            fs::close( fds.1 );
                        }
                    }

                    if idx_cmd == 0
                    {
                        let pid = libc::getpid();
                        libc::setpgid( 0, pid );
                    }
                    else { libc::setpgid( 0, *pgid ); }
                    
                    if idx_cmd > 0
                    {
                        let fds_prev = pipes[idx_cmd - 1];
                        fs::dup2( fds_prev.0, 0 );
                        fs::close( fds_prev.0 );
                        fs::close( fds_prev.1 );
                    }

                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        fs::dup2( fds.1, 1 );
                        fs::close( fds.1 );
                        fs::close( fds.0 );
                    }

                    if cmd.has_redirect_from()
                    {
                        if let Some( redirect_from ) = &cmd.redirect_from
                        {
                            let fd = get::descriptor_from_file( &redirect_from.clone().1 );
                            if fd == -1 { process::exit( 1 ); }
                            fs::dup2( fd, 0 );
                            fs::close( fd );
                        }
                    }

                    if cmd.has_here_string()
                    {
                        if let Some( fds ) = fds_stdin
                        {
                            fs::close( fds.1 );
                            fs::dup2( fds.0, 0 );
                            fs::close( fds.0 );
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
                                fs::dup2( 1, 2 );
                            }
                            else if !options.capture_output
                            {
                                let fd = fs::dup( 1 );
                                if fd == -1 {
                                    println_stderr!( "pls:dup error" );
                                    process::exit( 1 );
                                }
                                fs::dup2( fd, 2 );
                            }
                            else
                            {
                                
                            }
                        }
                        else if to_ == "&2" && from_ == "1"
                        {
                            if idx_cmd < pipes_count || !options.capture_output
                            {
                                let fd = fs::dup( 2 );
                                if fd == -1
                                {
                                    println_stderr!( "pls:dup error" );
                                    process::exit( 1 );
                                }
                                fs::dup2( fd, 1 );
                            }
                            else
                            {
                                
                            }
                        }

                        else
                        {
                            let append = op_ == ">>";
                            match fs::create_raw_fd_from_file( to_, append )
                            {
                                Ok( fd ) =>
                                {
                                    if fd == -1
                                    {
                                        println_stderr!( "pls:fork:fd error" );
                                        process::exit( 1 );
                                    }

                                    if from_ == "1"
                                    {
                                        fs::dup2( fd, 1 );
                                        stdout_redirected = true;
                                    }
                                    else
                                    {
                                        fs::dup2( fd, 2 );
                                        stderr_redirected = true;
                                    }
                                }
                                Err( e ) =>
                                {
                                    println_stderr!( "pls:fork:{}", e );
                                    process::exit( 1 );
                                }
                            }
                        }
                    }
                    
                    if idx_cmd == pipes_count && options.capture_output
                    {
                        if !stdout_redirected
                        {
                            if let Some( fds ) = fds_capture_stdout
                            {
                                fs::close( fds.0 );
                                fs::dup2( fds.1, 1 );
                                fs::close( fds.1 );
                            }
                        }

                        if !stderr_redirected
                        {
                            if let Some( fds ) = fds_capture_stderr
                            {
                                fs::close( fds.0 );
                                fs::dup2( fds.1, 2 );
                                fs::close( fds.1 );
                            }
                        }
                    }

                    if cmd.is_builtin()
                    {
                        if let Some( status ) = try_run_builtin_in_subprocess( sh, cl, idx_cmd, capture )
                        { process::exit( status ); }
                    }
                    /*
                    Our strings do not have '\x00' bytes in them, we can use CString::new().expect() safely. */
                    let mut c_envs:Vec<_> = env::vars()
                            .map( |( k, v )|
                            {
                                CString::new( format!( "{}={}", k, v ).as_str() ).expect( "CString error" )
                            } )
                            .collect();

                    for ( key, value ) in cl.envs.iter()
                    {
                        c_envs.push( CString::new( format!( "{}={}", key, value ).as_str() ).expect( "CString error" ), );
                    }

                    let program = &cmd.tokens[0].1;
                    let path = if program.contains( '/' ) { program.clone() }
                    else { path::find_file_in_path( program, true ) };

                    if path.is_empty()
                    {
                        println_stderr!( "pls:{}:command not found", program );
                        process::exit( 127 );
                    }

                    let c_program = CString::new( path.as_str() ).expect( "CString::new failed" );
                    let c_args:Vec<_> = cmd
                        .tokens
                        .iter()
                        .map( |x| CString::new( x.1.as_str() )
                        .expect( "CString error" ) )
                        .collect();

                    let c_args:Vec<&CStr> = c_args.iter().map( |x| x.as_c_str() ).collect();
                    let c_envs:Vec<&CStr> = c_envs.iter().map( |x| x.as_c_str() ).collect();
                    match execve( &c_program, &c_args, &c_envs )
                    {
                        Ok( _ ) => {}
                        Err( e ) => match e
                        {
                            nix::Error::ENOEXEC => { println_stderr!( "pls:{}:exec format error ( ENOEXEC )", program ); }
                            nix::Error::ENOENT => { println_stderr!( "pls:{}:file does not exist", program ); }
                            nix::Error::EACCES => { println_stderr!( "pls:{}:Permission denied", program ); }
                            _ => { println_stderr!( "pls:{}:{:?}", program, e ); }
                        },
                    }

                    process::exit( 1 );
                }

                Ok( ForkResult::Parent { child, .. } ) =>
                {
                    let pid:i32 = child.into();
                    if idx_cmd == 0
                    {
                        *pgid = pid;
                        /*
                        We need to wait pgid of child set to itself, before give terminal to it ( for macos ).
                        1. This loop causes `bash`, `htop` etc to go `T` status immediate after start on linux ( ubuntu ).
                        2. But on mac, we need this loop, otherwise commands like `vim` will go to `T` status after start. */
                        if cfg!( target_os = "macos" )
                        {
                            loop
                            {
                                let _pgid = libc::getpgid( pid );
                                if _pgid == pid { break; }
                            }
                        }

                        if sh.has_terminal && options.isatty && !cl.background
                        {
                            *term_given = shell::give_terminal_to( pid );
                        }
                    }

                    if options.isatty && !options.capture_output
                    {
                        let _cmd = parsers::line::tokens_to_line( &cmd.tokens );
                        sh.insert_job( *pgid, pid, &_cmd, "Running", cl.background );
                    }

                    if let Some( redirect_from ) = &cmd.redirect_from
                    {
                        if redirect_from.0 == "<<<"
                        {
                            if let Some( fds ) = fds_stdin
                            {
                                fs::close( fds.0 );
                                let mut f = File::from_raw_fd( fds.1 );
                                match f.write_all( redirect_from.1.clone().as_bytes() )
                                {
                                    Ok( _ ) => {}
                                    Err( e ) => println_stderr!( "pls:write_all:{}", e ),
                                }
                                match f.write_all( b"\n" )
                                {
                                    Ok( _ ) => {}
                                    Err( e ) => println_stderr!( "pls:write_all:{}", e ),
                                }
                            }
                        }
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        fs::close( fds.1 );
                    }
                    
                    if idx_cmd > 0
                    {
                        let fds = pipes[idx_cmd - 1];
                        fs::close( fds.0 );
                    }

                    if idx_cmd == pipes_count && options.capture_output
                    {
                        let mut s_out = String::new();
                        let mut s_err = String::new();

                        if let Some( fds ) = fds_capture_stdout
                        {
                                fs::close( fds.1 );
                                let mut f = File::from_raw_fd( fds.0 );
                                match f.read_to_string( &mut s_out )
                                {
                                    Ok( _ ) => {}
                                    Err( e ) => println_stderr!( "pls:readstr:{}", e ),
                                }
                        }

                        if let Some( fds ) = fds_capture_stderr
                        {
                            fs::close( fds.1 );
                            let mut f_err = File::from_raw_fd( fds.0 );
                            match f_err.read_to_string( &mut s_err )
                            {
                                Ok( _ ) => {}
                                Err( e ) => println_stderr!( "pls:readstr:{}", e ),
                            }
                        }

                        *cmd_result = CommandResult
                        {
                            gid:*pgid,
                            status:0,
                            stdout:s_out.clone(),
                            stderr:s_err.clone(),
                        };
                    }

                    pid
                }

                Err( _ ) =>
                {
                    println_stderr!( "Fork failed" );
                    *cmd_result = CommandResult::error();
                    0
                }
            }
        }
    }
    /**/
    pub fn try_run_func
    ( 
        sh:&mut Shell,
        cl:&CommandLine,
        capture:bool,
        log_cmd:bool,
 ) -> Option<CommandResult>
    {
        if cl.is_empty() {
            return None;
        }

        let command = &cl.commands[0];
        if let Some( func_body ) = sh.get_func( &command.tokens[0].1 ) {
            let mut args = vec!["pls".to_string()];
            for token in &command.tokens {
                args.push( token.1.to_string() );
            }
            if log_cmd {
                //log!( "run func:{:?}", &args );
            }
            let cr_list = scripts::run_lines( sh, &func_body, &args, capture );
            let mut stdout = String::new();
            let mut stderr = String::new();
            for cr in cr_list {
                stdout.push_str( cr.stdout.trim() );
                stdout.push( ' ' );
                stderr.push_str( cr.stderr.trim() );
                stderr.push( ' ' );
            }
            let mut cr = CommandResult::new();
            cr.stdout = stdout;
            cr.stderr = stderr;
            return Some( cr );
        }
        None
    }

    #[derive( Debug, Clone )]
    pub struct Shell
    {
        pub jobs:HashMap<i32, types::Job>,
        pub alias:HashMap<String, String>,
        pub envs:HashMap<String, String>,
        pub funcs:HashMap<String, String>,
        pub cmd:String,
        pub current_dir:String,
        pub previous_dir:String,
        pub previous_cmd:String,
        pub previous_status:i32,
        pub is_login:bool,
        pub exit_on_error:bool,
        pub has_terminal:bool,
        pub session_id:String,
    }

    impl Shell
    {
        pub fn new() -> Shell
        {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = get::current_directory();
            // TODO:the shell proc may have terminal later
            // e.g. $ pls foo.sh &
            // then with a $ fg
            let has_terminal = proc_has_terminal();
            let ( session_id, _ ) = uuid.split_at( 13 );
            Shell {
                jobs:HashMap::new(),
                alias:HashMap::new(),
                envs:HashMap::new(),
                funcs:HashMap::new(),
                cmd:String::new(),
                current_dir:current_dir.clone(),
                previous_dir:String::new(),
                previous_cmd:String::new(),
                previous_status:0,
                is_login:false,
                exit_on_error:false,
                has_terminal,
                session_id:session_id.to_string(),
            }
        }

        pub fn insert_job( &mut self, gid:i32, pid:i32, cmd:&str, status:&str, bg:bool )
        {
            let mut i = 1;
            loop {
                let mut indexed_job_missing = false;
                if let Some( x ) = self.jobs.get_mut( &i ) {
                    if x.gid == gid {
                        x.pids.push( pid );
                        x.cmd = format!( "{} | {}", x.cmd, cmd );
                        return;
                    }
                } else {
                    indexed_job_missing = true;
                }

                if indexed_job_missing {
                    self.jobs.insert( 
                        i,
                        types::Job {
                            cmd:cmd.to_string(),
                            id:i,
                            gid,
                            pids:vec![pid],
                            pids_stopped:HashSet::new(),
                            status:status.to_string(),
                            is_bg:bg,
                        },
 );
                    return;
                }
                i += 1;
            }
        }

        pub fn get_job_by_id( &self, job_id:i32 ) -> Option<&types::Job>
        {
            self.jobs.get( &job_id )
        }

        pub fn mark_job_member_continued( &mut self, pid:i32, gid:i32 ) -> Option<&types::Job>
        {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i ) {
                    if job.gid == gid {
                        job.pids_stopped.remove( &pid );
                        idx_found = i;
                        break;
                    }
                }


                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get( &idx_found )
        }

        pub fn mark_job_member_stopped( &mut self, pid:i32, gid:i32 ) -> Option<&types::Job>
        {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i ) {
                    if job.gid == gid {
                        job.pids_stopped.insert( pid );
                        idx_found = i;
                        break;
                    }
                }


                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get( &idx_found )
        }

        pub fn get_job_by_gid( &self, gid:i32 ) -> Option<&types::Job>
        {
            if self.jobs.is_empty() {
                return None;
            }

            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get( &i ) {
                    if x.gid == gid {
                        return Some( x );
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
            None
        }

        pub fn mark_job_as_running( &mut self, gid:i32, bg:bool )
        {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i ) {
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

        pub fn mark_job_as_stopped( &mut self, gid:i32 )
        {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get_mut( &i ) {
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

        pub fn remove_pid_from_job( &mut self, gid:i32, pid:i32 ) -> Option<types::Job>
        {
            if self.jobs.is_empty() {
                return None;
            }

            let mut empty_pids = false;
            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get_mut( &i ) {
                    if x.gid == gid {
                        if let Ok( i_pid ) = x.pids.binary_search( &pid ) {
                            x.pids.remove( i_pid );
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
                return self.jobs.remove( &i );
            }
            None
        }
        /*
        Update existing *ENV Variable* if such name exists in ENVs,
        so we define a local *Shell Variable*, which would not be exported into child processes. */
        pub fn set_env( &mut self, name:&str, value:&str )
        {
            if env::var( name ).is_ok()
            {
                env::set_var( name, value );
            }
            else
            {
                self.envs.insert( name.to_string(), value.to_string() );
            }
        }
        /// get *Shell Variable*, or *ENV Variable*.
        pub fn get_env( &self, name:&str ) -> Option<String>
        {
            match self.envs.get( name ) {
                Some( x ) => Some( x.to_string() ),
                None => {
                    match env::var( name ) {
                        Ok( x ) => Some( x ),
                        Err( _ ) => None,
                    }
                }
            }
        }
        /// Remove environment variable, function from the environment of the currently running process.
        pub fn remove_env( &mut self, name:&str ) -> bool
        {
            let ptn_env = Regex::new( r"^[a-zA-Z_][a-zA-Z0-9_-]*$" ).unwrap();
            if !ptn_env.is_match( name ) {
                return false;
            }

            env::remove_var( name );
            self.envs.remove( name );
            self.remove_func( name );
            true
        }

        pub fn remove_path( &mut self, path:&str )
        {
            if let Ok( paths ) = env::var( "PATH" ) {
                let mut paths_new:Vec<&str> = paths.split( ":" ).collect();
                paths_new.retain( |&x| x != path );
                env::set_var( "PATH", paths_new.join( ":" ).as_str() );
            }
        }

        pub fn remove_func( &mut self, name:&str )
        {
            self.funcs.remove( name );
        }

        pub fn set_func( &mut self, name:&str, value:&str )
        {
            self.funcs.insert( name.to_string(), value.to_string() );
        }

        pub fn get_func( &self, name:&str ) -> Option<String>
        {
            self.funcs.get( name ).map( |x| x.to_string() )
        }

        pub fn get_alias_list( &self ) -> Vec<( String, String )>
        {
            let mut result = Vec::new();
            for ( name, value ) in &self.alias {
                result.push( ( name.clone(), value.clone() ) );
            }
            result
        }

        pub fn add_alias( &mut self, name:&str, value:&str )
        {
            self.alias.insert( name.to_string(), value.to_string() );
        }

        pub fn is_alias( &self, name:&str ) -> bool
        {
            self.alias.contains_key( name )
        }

        pub fn remove_alias( &mut self, name:&str ) -> bool {
            let opt = self.alias.remove( name );
            opt.is_some()
        }

        pub fn get_alias_content( &self, name:&str ) -> Option<String> {
            let result = match self.alias.get( name ) {
                Some( x ) => x.to_string(),
                None => String::new(),
            };
            if result.is_empty() {
                None
            } else {
                Some( result )
            }
        }
    }

    pub unsafe fn give_terminal_to( gid:i32 ) -> bool
    {
        let mut mask:libc::sigset_t = mem::zeroed();
        let mut old_mask:libc::sigset_t = mem::zeroed();

        libc::sigemptyset( &mut mask );
        libc::sigaddset( &mut mask, libc::SIGTSTP );
        libc::sigaddset( &mut mask, libc::SIGTTIN );
        libc::sigaddset( &mut mask, libc::SIGTTOU );
        libc::sigaddset( &mut mask, libc::SIGCHLD );

        let rcode = libc::pthread_sigmask( libc::SIG_BLOCK, &mask, &mut old_mask );
        if rcode != 0 {
           // log!( "failed to call pthread_sigmask" );
        }
        let rcode = libc::tcsetpgrp( 1, gid );
        let given;
        if rcode == -1 {
            given = false;
            let e = no::errno();
            let code = e.0;
           // log!( "error in give_terminal_to() {}:{}", code, e );
        } else {
            given = true;
        }
        let rcode = libc::pthread_sigmask( libc::SIG_SETMASK, &old_mask, &mut mask );
        if rcode != 0 { /*log!( "failed to call pthread_sigmask" );*/ }
        given
    }

    pub fn brace_getitem( s:&str, depth:i32 ) -> ( Vec<String>, String )
    {
        let mut out:Vec<String> = vec![String::new()];
        let mut ss = s.to_string();
        let mut tmp;
        while !ss.is_empty()
        {
            let c = match ss.chars().next()
            {
                Some( x ) => x,
                None => { return ( out, ss ); }
            };
            if depth > 0 && ( c == ',' || c == '}' ) { return ( out, ss ); }

            if c == '{'
            {
                let mut sss = ss.clone();
                sss.remove( 0 );
                let result_groups = brace_getgroup( &sss, depth + 1 );
                if let Some( ( out_group, s_group ) ) = result_groups
                {
                    let mut tmp_out = Vec::new();
                    for x in out.iter()
                    {
                        for y in out_group.iter()
                        {
                            let item = format!( "{}{}", x, y );
                            tmp_out.push( item );
                        }
                    }
                    out = tmp_out;
                    ss = s_group.clone();
                    continue;
                }
            }
            // FIXME:here we mean more than one char.
            if c == '\\' && ss.len() > 1
            {
                ss.remove( 0 );
                let c;
                match ss.chars().next()
                {
                    Some( x ) => c = x,
                    None => { return ( out, ss ) }
                }

                tmp = format!( "\\{}", c );
            }
            else { tmp = c.to_string(); }

            let mut result = Vec::new();
            for x in out.iter()
            {
                let item = format!( "{}{}", x, tmp );
                result.push( item );
            }
            out = result;
            ss.remove( 0 );
        }
        ( out, ss )
    }

    pub fn brace_getgroup( s:&str, depth:i32 ) -> Option<( Vec<String>, String )>
    {
        let mut out:Vec<String> = Vec::new();
        let mut comma = false;
        let mut ss = s.to_string();
        while !ss.is_empty()
        {
            let ( g, sss ) = brace_getitem( ss.as_str(), depth );
            ss = sss.clone();
            if ss.is_empty() { break; }

            for x in g.iter() { out.push( x.clone() ); }
            let c = match ss.chars().next()
            {
                Some( x ) => x,
                None => { break; }
            };

            if c == '}'
            {
                let mut sss = ss.clone();
                sss.remove( 0 );
                if comma { return Some( ( out, sss ) ); }

                let mut result = Vec::new();
                for x in out.iter()
                {
                    let item = format!( "{{{}}}", x );
                    result.push( item );
                }

                return Some( ( result, ss ) );
            }

            if c == ','
            {
                comma = true;
                ss.remove( 0 );
            }
        }

        None
    }

    pub fn should_do_dollar_command_extension( line:&str ) -> bool
    {
        regex::re_contains( line, r"\$\( [^\ )]+\ )" ) &&
        !regex::re_contains( line, r"='.*\$\( [^\ )]+\ ).*'$" )
    }

    pub fn do_command_substitution_for_dollar( sh:&mut Shell, tokens:&mut types::Tokens )
    {
        unsafe
        {
            let mut idx:usize = 0;
            let mut buff:HashMap<usize, String> = HashMap::new();

            for ( sep, token ) in tokens.iter()
            {
                if sep == "'" || sep == "\\" || !should_do_dollar_command_extension( token )
                {
                    idx += 1;
                    continue;
                }

                let mut line = token.to_string();
                loop
                {
                    if !should_do_dollar_command_extension( &line ) { break; }

                    let ptn_cmd = r"\$\( ( .+ )\ )";
                    let cmd = match regex::find_first_group( ptn_cmd, &line )
                    {
                        Some( x ) => x,
                        None =>
                        {
                            println_stderr!( "pls:calculator:no first group" );
                            return;
                        }
                    };

                    let cmd_result = match CommandLine::from_line( &cmd, sh )
                    {
                        Ok( c ) =>
                        {
                            //log!( "run subcmd dollar:{:?}", &cmd );
                            let ( term_given, cr ) = shell::run_pipeline( sh, &c, true, true, false );
                            if term_given
                            {
                                let gid = libc::getpgid( 0 );
                                give_terminal_to( gid );
                            }

                            cr
                        }

                        Err( e ) =>
                        {
                            println_stderr!( "pls:{}", e );
                            continue;
                        }
                    };

                    let output_txt = cmd_result.stdout.trim();
                    let ptn = r"( ?P<head>[^\$]* )\$\( .+\ )( ?P<tail>.* )";
                    let re;

                    if let Ok( x ) = Regex::new( ptn ) { re = x; }
                    else { return; }

                    let to = format!( "${{head}}{}${{tail}}", output_txt );
                    let line_ = line.clone();
                    let result = re.replace( &line_, to.as_str() );
                    line = result.to_string();
                }

                buff.insert( idx, line.clone() );
                idx += 1;
            }

            for ( i, text ) in buff.iter() { tokens[*i].1 = text.to_string(); }
        }
    }

    pub fn do_command_substitution_for_dot( sh:&mut Shell, tokens:&mut types::Tokens )
    {
        unsafe
        {
            let mut idx:usize = 0;
            let mut buff:HashMap<usize, String> = HashMap::new();
            for ( sep, token ) in tokens.iter()
            {
                let new_token:String;
                if sep == "`"
                {
                   // log!( "run subcmd dot1:{:?}", token );
                    let cr = match CommandLine::from_line( token, sh )
                    {
                        Ok( c ) =>
                        {
                            let ( term_given, _cr ) = shell::run_pipeline( sh, &c, true, true, false );
                            if term_given
                            {
                                let gid = libc::getpgid( 0 );
                                give_terminal_to( gid );
                            }

                            _cr
                        }

                        Err( e ) =>
                        {
                            println_stderr!( "pls:{}", e );
                            continue;
                        }
                    };

                    new_token = cr.stdout.trim().to_string();

                }
                else if sep == "\"" || sep.is_empty()
                {
                    let re;
                    if let Ok( x ) = Regex::new( r"^( [^`]* )`( [^`]+ )`( .* )$" ) { re = x; }
                    else
                    {
                        println_stderr!( "pls:re new error" );
                        return;
                    }

                    if !re.is_match( token )
                    {
                        idx += 1;
                        continue;
                    }
                    let mut _token = token.clone();
                    let mut _item = String::new();
                    let mut _head = String::new();
                    let mut _output = String::new();
                    let mut _tail = String::new();
                    loop
                    {
                        if !re.is_match( &_token )
                        {
                            if !_token.is_empty() { _item = format!( "{}{}", _item, _token ); }
                            break;
                        }

                        for cap in re.captures_iter( &_token )
                        {
                            _head = cap[1].to_string();
                            _tail = cap[3].to_string();
                           // log!( "run subcmd dot2:{:?}", &cap[2] );

                            let cr = match CommandLine::from_line( &cap[2], sh )
                            {
                                Ok( c ) =>
                                {
                                    let ( term_given, _cr ) = shell::run_pipeline( sh, &c, true, true, false );
                                    if term_given
                                    {
                                        let gid = libc::getpgid( 0 );
                                        give_terminal_to( gid );
                                    }

                                    _cr
                                }

                                Err( e ) =>
                                {
                                    println_stderr!( "pls:{}", e );
                                    continue;
                                }
                            };

                            _output = cr.stdout.trim().to_string();
                        }

                        _item = format!( "{}{}{}", _item, _head, _output );
                        if _tail.is_empty() { break; }
                        _token = _tail.clone();
                    }
                    new_token = _item;
                }
                else
                {
                    idx += 1;
                    continue;
                }

                buff.insert( idx, new_token.clone() );
                idx += 1;
            }

            for ( i, text ) in buff.iter() { tokens[*i].1 = text.to_string(); }
        }
    }

    pub fn do_command_substitution( sh:&mut Shell, tokens:&mut types::Tokens )
    {
        do_command_substitution_for_dot( sh, tokens );
        do_command_substitution_for_dollar( sh, tokens );
    }

    pub fn do_expansion( sh:&mut Shell, tokens:&mut types::Tokens )
    {
        let line = parsers::line::tokens_to_line( tokens );
        if is::arithmetic( &line ) { return; }
        if tokens.len() >= 2 && tokens[0].1 == "export" && tokens[1].1.starts_with( "PROMPT=" ) { return; }

        expand::alias( sh, tokens );
        expand::home( tokens );
        expand::environment( sh, tokens );
        expand::brace( tokens );
        expand::glob( tokens );
        do_command_substitution( sh, tokens );
        expand::brace_range( tokens );
    }

    pub fn trim_multiline_prompts( line:&str ) -> String
    {
        /*
        Remove sub-prompts from multiple line mode:
            1. assuming '\n' char cannot be typed manually?
            2. `>>` is defined as `src/prompt/multilines.rs` */
        let line_new = regex::replace_all( line, r"\\\n>> ", "" );
        let line_new = regex::replace_all( &line_new, r"\| *\n>> ", "| " );
        regex::replace_all( &line_new, r"( ?P<NEWLINE>\n )>> ", "$NEWLINE" )
    }

    pub fn proc_has_terminal() -> bool
    {
        unsafe
        {
            let tgid = libc::tcgetpgrp( 0 );
            let pgid = libc::getpgid( 0 );
            tgid == pgid
        }
    }
}
mod signals
{
    use ::
    {
        collections::{ HashMap, HashSet },
        error::no::{ errno, set_errno },
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
        static ref REAP_MAP:Mutex<HashMap<i32, i32>> = Mutex::new( HashMap::new() );
        static ref STOP_MAP:Mutex<HashSet<i32>> = Mutex::new( HashSet::new() );
        static ref CONT_MAP:Mutex<HashSet<i32>> = Mutex::new( HashSet::new() );
        static ref KILL_MAP:Mutex<HashMap<i32, i32>> = Mutex::new( HashMap::new() );
    }

    pub fn killed_map_insert( pid:i32, sig:i32 )
    { if let Ok( mut m ) = KILL_MAP.try_lock() { m.insert( pid, sig ); } }

    pub fn killed_map_pop( pid:i32 ) -> Option<i32>
    { if let Ok( mut m ) = KILL_MAP.try_lock() { m.remove( &pid ) } else { None } }

    pub fn insert_cont_map( pid:i32 ) 
    { if let Ok( mut m ) = CONT_MAP.try_lock() { m.insert( pid ); } }

    pub fn pop_cont_map( pid:i32 ) -> bool
    {
        match CONT_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => false,
        }
    }

    pub fn insert_stopped_map( pid:i32 )
    { if let Ok( mut m ) = STOP_MAP.try_lock() { m.insert( pid ); } }

    pub fn pop_stopped_map( pid:i32 ) -> bool
    {
        match STOP_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => false,
        }
    }

    pub fn insert_reap_map( pid:i32, status:i32 )
    { if let Ok( mut m ) = REAP_MAP.try_lock() { m.insert( pid, status ); } }

    pub fn pop_reap_map( pid:i32 ) -> Option<i32>
    {
        match REAP_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => None,
        }
    }

    pub fn block_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add( signal::SIGCHLD );
        match signal::sigprocmask( signal::SigmaskHow::SIG_BLOCK, Some( &sigset ), None )
        {
            Ok( _ ) => {},
            Err( e ) => 
            { 
                // log!( "sigprocmask block error:{:?}", e );
            }
        }
    }

    pub fn unblock_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add( signal::SIGCHLD );
        match signal::sigprocmask( signal::SigmaskHow::SIG_UNBLOCK, Some( &sigset ), None )
        {
            Ok( _ ) => {},
            Err( e ) => 
            { 
                // log!( "sigprocmask unblock error:{:?}", e );
            }
        }
    }
    
    pub extern fn handle_sigchld( _sig:i32 )
    {
        let saved_errno = errno();
        let options = Some( WF::WUNTRACED | WF::WNOHANG | WF::WCONTINUED );
        loop
        {
            match waitpid( Pid::from_raw( -1 ), options )
            {
                Ok( WS::Exited( pid, status ) ) => { insert_reap_map( i32::from( pid ), status ); }
                Ok( WS::Stopped( pid, _sig ) ) => { insert_stopped_map( i32::from( pid ) ); }
                Ok( WS::Continued( pid ) ) => { insert_cont_map( i32::from( pid ) ); }
                Ok( WS::Signaled( pid, sig, _core_dumped ) ) => { killed_map_insert( i32::from( pid ), sig as i32 ); }
                Ok( WS::StillAlive ) => { break; }
                Ok( _others ) => { /* log!( "sigchld others:{:?}", _others ); */ }
                Err( e ) =>
                {
                    if e == nix::Error::ECHILD { break; } //log!( "chld waitpid error:{:?}", e );
                    break;
                }
            }
        }
        set_errno( saved_errno );
    }

    pub fn setup_sigchld_handler()
    {
        unsafe
        {
            let sigset = signal::SigSet::empty();
            let handler = signal::SigHandler::Handler( handle_sigchld );
            let flags = signal::SaFlags::SA_RESTART;
            let sa = signal::SigAction::new( handler, flags, sigset );
            match signal::sigaction( signal::SIGCHLD, &sa )
            {
                Ok( _ ) => {},
                Err( e ) => { log!( "sigaction error:{:?}", e ); }
            }
        }
    }
}
pub mod slice
{
    pub use std::slice::{ * };
    use ::
    {
        borrow::{ Cow },
        *,
    };

    pub fn ws( input:&[u8] ) -> IResult<&[u8], char>
    { alt( ( char( ' ' ), char( '\t' ) ) )( input ) }

    pub fn end( input:&[u8] ) -> IResult<&[u8], &[u8]>
    { alt( ( eof, eol ) )( input ) }
   /*
   */
   #[inline] pub fn number( i:&[u8] ) -> i32
    {
        let mut n:i32 = 0;
        for &ch in i
        {
            let d = ( ch as i32 ).wrapping_sub( b'0' as i32 );
            if d <= 9
            { n = n.saturating_mul( 10 ).saturating_add( d ); }
        }

        n
    }
    /**/
    pub fn unescape( i:&[u8] ) -> Cow<[u8]>
    {
    }
}
pub mod str
{
    pub use std::str::{ * };
    use ::
    {
        borrow::{ Borrow, BorrowMut, Cow },
        boxed::{ Box },
        collections::{ HashMap },
        cmp::{ Ordering },
        ffi::{OsStr, OsString},
        fmt,
        hash::{ Hash, Hasher },
        iter::{ FromIterator },
        marker::{ PhantomData },
        smallvec::{ Array, SmallVec },
        *,
    };
    /**/
    macro_rules! impl_index_str
    {
        ( $index_type:ty ) => 
        {
            impl<A:Array<Item = u8>> ops::Index<$index_type> for SmallString<A>
            {
                type Output = str;
                #[inline] fn index( &self, index:$index_type ) -> &str { &self.as_str()[index] }
            }

            impl<A:Array<Item = u8>> ops::IndexMut<$index_type> for SmallString<A>
            {
                #[inline] fn index_mut( &mut self, index:$index_type ) -> &mut str { &mut self.as_mut_str()[index] }
            }
        };
    }

    macro_rules! eq_str
    {
        ( $rhs:ty ) =>
        {
            impl<'a, A:Array<Item = u8>> PartialEq<$rhs> for SmallString<A>
            {
                #[inline] fn eq( &self, rhs:&$rhs ) -> bool { &self[..] == &rhs[..] }
                #[inline] fn ne( &self, rhs:&$rhs ) -> bool { &self[..] != &rhs[..] }
            }
        };
    }
    /*
    */
    pub fn unquote( s:&str ) -> String
    {
        let args = parsers::line::line_to_plain_tokens( s );
        if args.is_empty() { return String::new(); }
        args[0].clone()
    }
    /**/
    pub fn split_into_fields( sh:&shell::Shell, line:&str, envs:&HashMap<String, String>, ) -> Vec<String>
    {
        let ifs_chars;

        if envs.contains_key( "IFS" ) { ifs_chars = envs[&"IFS".to_string()].chars().collect(); }
        else if let Some( x ) = sh.get_env( "IFS" ) { ifs_chars = x.chars().collect(); }
        else if let Ok( x ) = env::var( "IFS" ) { ifs_chars = x.chars().collect(); }
        else { ifs_chars = vec![]; }

        if ifs_chars.is_empty()
        {
            return line
            .split( &[' ', '\t', '\n'][..] )
            .map( |x| x.to_string() )
            .collect();
        }
        else { return line.split( &ifs_chars[..] ).map( |x| x.to_string() ).collect(); }
    }
    /**/
    pub fn wrap_sep_string( sep:&str, s:&str ) -> String
    {
        let mut _token = String::new();
        let mut met_subsep = false;
        let mut previous_subsep = 'N';
        for c in s.chars()
        {
            if sep.is_empty() && ( c == '`' || c == '"' )
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
            if c.to_string() == sep { _token.push( '\\' ); }
            if c == ' ' && sep.is_empty() && !met_subsep
            {
                _token.push( '\\' );
            }
            _token.push( c );
        }
        format!( "{}{}{}", sep, _token, sep )
    }
    /*
    smallstr v0.0.0 
    Implements `SmallString`, a `String`-like container for small strings */
    /// A `String`-like container that can store a small number of bytes inline.
    #[derive( Clone, Default )]
    pub struct SmallString<A:Array<Item = u8>>
    {
        data:SmallVec<A>,
    }

    impl<A:Array<Item = u8>> SmallString<A>
    {
        /// Construct an empty string.
        #[inline] pub fn new() -> SmallString<A>
        {
            SmallString
            {
                data:SmallVec::new(),
            }
        }
        /// Construct an empty string with enough capacity pre-allocated to store at least `n` bytes.
        #[inline] pub fn with_capacity( n:usize ) -> SmallString<A>
        {
            SmallString
            {
                data:SmallVec::with_capacity( n ),
            }
        }
        /// Construct a `SmallString` by copying data from a `&str`.
        #[inline] pub fn from_str( s:&str ) -> SmallString<A>
        {
            SmallString
            {
                data:SmallVec::from_slice( s.as_bytes() ),
            }
        }
        /// Construct a `SmallString` by using an existing allocation.
        #[inline] pub fn from_string( s:String ) -> SmallString<A>
        {
            SmallString
            {
                data:SmallVec::from_vec( s.into_bytes() ),
            }
        }
        /// Constructs a new `SmallString` on the stack using UTF-8 bytes.
        #[inline] pub fn from_buf( buf:A ) -> Result<SmallString<A>, FromUtf8Error<A>>
        {
            let data = SmallVec::from_buf( buf );

            match str::from_utf8( &data )
            {
                Ok( _ ) => Ok( SmallString { data } ),
                Err( error ) =>
                {
                    let buf = data.into_inner().ok().unwrap();
                    Err( FromUtf8Error { buf, error } )
                }
            }
        }
        /// Constructs a new `SmallString` on stack using provided array without checking that it contains valid UTF-8.
        #[inline] pub unsafe fn from_buf_unchecked( buf:A ) -> SmallString<A>
        {
            SmallString
            {
                data:SmallVec::from_buf( buf ),
            }
        }
        /// The maximum number of bytes this string can hold inline.
        #[inline] pub fn inline_size( &self ) -> usize
        { A::size() }
        /// Returns the length of this string, in bytes.
        #[inline] pub fn len( &self ) -> usize
        { self.data.len() }
        /// Returns `true` if this string is empty.
        #[inline] pub fn is_empty( &self ) -> bool
        { self.data.is_empty() }
        /// Returns the number of bytes this string can hold without reallocating.
        #[inline] pub fn capacity( &self ) -> usize
        { self.data.capacity() }
        /// Returns `true` if the data has spilled into a separate heap-allocated buffer.
        #[inline] pub fn spilled( &self ) -> bool
        { self.data.spilled() }
        /// Empties the string and returns an iterator over its former contents.
        pub fn drain( &mut self ) -> Drain
        {
            unsafe
            {
                let len = self.len();
                self.data.set_len( 0 );
                let ptr = self.as_ptr();
                let slice = slice::from_raw_parts( ptr, len );
                let s = str::from_utf8_unchecked( slice );
                Drain { iter:s.chars() }
            }
        }
        /// Appends the given `char` to the end of this string.
        #[inline] pub fn push( &mut self, ch:char )
        {
            match ch.len_utf8() 
            {
                1 => self.data.push( ch as u8 ),
                _ => self.push_str( ch.encode_utf8( &mut [0; 4] ) ),
            }
        }
        /// Appends the given string slice to the end of this string.
        #[inline] pub fn push_str( &mut self, s:&str )
        { self.data.extend_from_slice( s.as_bytes() ); }
        /// Removes the last character from this string and returns it.
        #[inline] pub fn pop( &mut self ) -> Option<char>
        {
            unsafe
            {
                match self.chars().next_back()
                {
                    Some( ch ) =>
                    {
                        let new_len = self.len() - ch.len_utf8();
                        self.data.set_len( new_len );
                        Some( ch )
                    },
                    None => None,
                }
            }
        }
        /// Reallocates to set the new capacity to `new_cap`.
        #[inline] pub fn grow( &mut self, new_cap:usize )
        { self.data.grow( new_cap ); }
        /// Ensures that this string's capacity is at least `additional` bytes larger than its length.
        #[inline] pub fn reserve( &mut self, additional:usize )
        { self.data.reserve( additional ); }
        /// Ensures that this string's capacity is `additional` bytes larger than its length.
        #[inline] pub fn reserve_exact( &mut self, additional:usize )
        { self.data.reserve( additional ); }
        /// Shrink the capacity of the string as much as possible.
        #[inline] pub fn shrink_to_fit( &mut self )
        { self.data.shrink_to_fit(); }
        /// Shorten the string, keeping the first `len` bytes.
        #[inline] pub fn truncate( &mut self, len:usize )
        {
            assert!( self.is_char_boundary( len ) );
            self.data.truncate( len );
        }
        /// Extracts a string slice containing the entire string.
        #[inline] pub fn as_str( &self ) -> &str
        { self }
        /// Extracts a string slice containing the entire string.
        #[inline] pub fn as_mut_str( &mut self ) -> &mut str
        { self }
        /// Removes all contents of the string.
        #[inline] pub fn clear( &mut self )
        { self.data.clear(); }
        /// Removes a `char` from this string at a byte position and returns it.
        #[inline] pub fn remove( &mut self, idx:usize ) -> char
        {
            unsafe
            {
                let ch = match self[idx..].chars().next()
                {
                    Some( ch ) => ch,
                    None => panic!( "cannot remove a char from the end of a string" ),
                };
                let ch_len = ch.len_utf8();
                let next = idx + ch_len;
                let len = self.len();                
                ptr::copy
                (
                    self.as_ptr().add( next ),
                    self.as_mut_ptr().add( idx ),
                    len - next,
                );
                self.data.set_len( len - ch_len );
                ch
            }
        }
        /// Inserts a `char` into this string at the given byte position.
        #[inline] pub fn insert( &mut self, idx:usize, ch:char )
        {
            assert!( self.is_char_boundary( idx ) );
            match ch.len_utf8()
            {
                1 => self.data.insert( idx, ch as u8 ),
                _ => self.insert_str( idx, ch.encode_utf8( &mut [0; 4] ) ),
            }
        }
        /// Inserts a `&str` into this string at the given byte position.
        #[inline] pub fn insert_str( &mut self, idx:usize, s:&str )
        {
            unsafe
            {
                assert!( self.is_char_boundary( idx ) );
                let len = self.len();
                let amt = s.len();
                self.data.reserve( amt );                
                ptr::copy
                ( 
                    self.as_ptr().add( idx ),
                    self.as_mut_ptr().add( idx + amt ),
                    len - idx,
                );
                ptr::copy_nonoverlapping( s.as_ptr(), self.as_mut_ptr().add( idx ), amt );
                self.data.set_len( len + amt );
            }
        }
        /// Returns a mutable reference to the contents of the `SmallString`.
        #[inline] pub unsafe fn as_mut_vec( &mut self ) -> &mut SmallVec<A>
        { &mut self.data }
        /// Converts the `SmallString` into a `String`, without reallocating if it has already spilled onto the heap.
        #[inline] pub fn into_string( self ) -> String
        { unsafe { String::from_utf8_unchecked( self.data.into_vec() ) } }
        /// Converts the `SmallString` into a `Box<str>`, without reallocating if it has already spilled onto the heap.
        #[inline] pub fn into_boxed_str( self ) -> Box<str>
        { self.into_string().into_boxed_str() }
        /// Convert the `SmallString` into `A`, if possible. Otherwise, return `Err( self )`.
        #[inline] pub fn into_inner( self ) -> Result<A, Self>
        { self.data.into_inner().map_err( |data| SmallString { data } ) }
        /// Retains only the characters specified by the predicate.
        #[inline] pub fn retain<F:FnMut( char ) -> bool>( &mut self, mut f:F )
        {
            unsafe
            {            
                struct SetLenOnDrop<'a, A:Array<Item = u8>>
                {
                    s:&'a mut SmallString<A>,
                    idx:usize,
                    del_bytes:usize,
                }

                impl<'a, A:Array<Item = u8>> Drop for SetLenOnDrop<'a, A>
                {
                    fn drop( &mut self )
                    {
                        let new_len = self.idx - self.del_bytes;
                        debug_assert!( new_len <= self.s.len() );
                        self.s.data.set_len( new_len );
                    }
                }

                let len = self.len();
                let mut guard = SetLenOnDrop
                {
                    s:self,
                    idx:0,
                    del_bytes:0,
                };

                while guard.idx < len
                {
                    let ch = guard
                    .s
                    .get_unchecked( guard.idx..len )
                    .chars()
                    .next()
                    .unwrap();
                    let ch_len = ch.len_utf8();

                    if !f( ch ) { guard.del_bytes += ch_len; }
                    else if guard.del_bytes > 0
                    {
                        ptr::copy
                        (
                            guard.s.data.as_ptr().add( guard.idx ),
                            guard.s.data.as_mut_ptr().add( guard.idx - guard.del_bytes ),
                            ch_len,
                        );
                    }
                    
                    guard.idx += ch_len;
                }

                drop( guard );
            }
        }

        fn as_mut_ptr( &mut self ) -> *mut u8 { self.as_ptr() as *mut u8 }
    }

    impl<A:Array<Item = u8>> ops::Deref for SmallString<A>
    {
        type Target = str;
        #[inline] fn deref( &self ) -> &str
        {
            unsafe 
            { 
                let bytes:&[u8] = &self.data;
                str::from_utf8_unchecked( bytes ) 
            }
        }
    }

    impl<A:Array<Item = u8>> ops::DerefMut for SmallString<A>
    {
        #[inline] fn deref_mut( &mut self ) -> &mut str
        {
            unsafe
            {
                let bytes:&mut [u8] = &mut self.data;
                str::from_utf8_unchecked_mut( bytes ) 
            }
        }
    }

    impl<A:Array<Item = u8>> AsRef<str> for SmallString<A>
    {
        #[inline] fn as_ref( &self ) -> &str { self }
    }

    impl<A:Array<Item = u8>> AsMut<str> for SmallString<A>
    {
        #[inline] fn as_mut( &mut self ) -> &mut str { self }
    }

    impl<A:Array<Item = u8>> Borrow<str> for SmallString<A>
    {
        #[inline] fn borrow( &self ) -> &str { self }
    }

    impl<A:Array<Item = u8>> BorrowMut<str> for SmallString<A>
    {
        #[inline] fn borrow_mut( &mut self ) -> &mut str { self }
    }

    impl<A:Array<Item = u8>> AsRef<[u8]> for SmallString<A>
    {
        #[inline] fn as_ref( &self ) -> &[u8] { self.data.as_ref() }
    }

    impl<A:Array<Item = u8>> fmt::Write for SmallString<A>
    {
        #[inline] fn write_str( &mut self, s:&str ) -> fmt::Result
        {
            self.push_str( s );
            Ok( () )
        }

        #[inline] fn write_char( &mut self, ch:char ) -> fmt::Result
        {
            self.push( ch );
            Ok( () )
        }
    }
    
    struct SmallStringVisitor<A>
    {
        phantom:PhantomData<A>,
    }
    
    impl<'de, A:Array<Item = u8>> Visitor<'de> for SmallStringVisitor<A>
    {
        type Value = SmallString<A>;
        fn expecting( &self, f:&mut fmt::Formatter ) -> fmt::Result { f.write_str( "a string" ) }
        fn visit_str<E:Error>( self, v:&str ) -> Result<Self::Value, E> { Ok( v.into() ) }
        fn visit_string<E:Error>( self, v:String ) -> Result<Self::Value, E> { Ok( v.into() ) }
    }

    impl<A:Array<Item = u8>> From<char> for SmallString<A>
    {
        #[inline] fn from( ch:char ) -> SmallString<A> { SmallString::from_str( ch.encode_utf8( &mut [0; 4] ) ) }
    }

    impl<'a, A:Array<Item = u8>> From<&'a str> for SmallString<A>
    {
        #[inline] fn from( s:&str ) -> SmallString<A> { SmallString::from_str( s ) }
    }

    impl<A:Array<Item = u8>> From<Box<str>> for SmallString<A>
    {
        #[inline] fn from( s:Box<str> ) -> SmallString<A> { SmallString::from_string( s.into() ) }
    }

    impl<A:Array<Item = u8>> From<String> for SmallString<A>
    {
        #[inline] fn from( s:String ) -> SmallString<A> { SmallString::from_string( s ) }
    }

    impl<'a, A:Array<Item = u8>> From<Cow<'a, str>> for SmallString<A>
    {
        fn from( value:Cow<'a, str> ) -> Self
        {
            match value
            {
                Cow::Borrowed( s ) => Self::from_str( s ),
                Cow::Owned( s ) => Self::from_string( s ),
            }
        }
    }

    impl_index_str!( ops::Range<usize> );
    impl_index_str!( ops::RangeFrom<usize> );
    impl_index_str!( ops::RangeTo<usize> );
    impl_index_str!( ops::RangeFull );

    impl<A:Array<Item = u8>> FromIterator<char> for SmallString<A>
    {
        fn from_iter<I:IntoIterator<Item = char>>( iter:I ) -> SmallString<A>
        {
            let mut s = SmallString::new();
            s.extend( iter );
            s
        }
    }

    impl<'a, A:Array<Item = u8>> FromIterator<&'a char> for SmallString<A>
    {
        fn from_iter<I:IntoIterator<Item = &'a char>>( iter:I ) -> SmallString<A>
        {
            let mut s = SmallString::new();
            s.extend( iter.into_iter().cloned() );
            s
        }
    }

    impl<'a, A:Array<Item = u8>> FromIterator<Cow<'a, str>> for SmallString<A>
    {
        fn from_iter<I:IntoIterator<Item = Cow<'a, str>>>( iter:I ) -> SmallString<A>
        {
            let mut s = SmallString::new();
            s.extend( iter );
            s
        }
    }

    impl<'a, A:Array<Item = u8>> FromIterator<&'a str> for SmallString<A>
    {
        fn from_iter<I:IntoIterator<Item = &'a str>>( iter:I ) -> SmallString<A>
        {
            let mut s = SmallString::new();
            s.extend( iter );
            s
        }
    }

    impl<A:Array<Item = u8>> FromIterator<String> for SmallString<A>
    {
        fn from_iter<I:IntoIterator<Item = String>>( iter:I ) -> SmallString<A>
        {
            let mut s = SmallString::new();
            s.extend( iter );
            s
        }
    }

    impl<A:Array<Item = u8>> Extend<char> for SmallString<A>
    {
        fn extend<I:IntoIterator<Item = char>>( &mut self, iter:I )
        {
            let iter = iter.into_iter();
            let ( lo, _ ) = iter.size_hint();
            self.reserve( lo );
            for ch in iter
            {
                self.push( ch );
            }
        }
    }

    impl<'a, A:Array<Item = u8>> Extend<&'a char> for SmallString<A>
    {
        fn extend<I:IntoIterator<Item = &'a char>>( &mut self, iter:I ){ self.extend( iter.into_iter().cloned() ); }
    }

    impl<'a, A:Array<Item = u8>> Extend<Cow<'a, str>> for SmallString<A>
    {
        fn extend<I:IntoIterator<Item = Cow<'a, str>>>( &mut self, iter:I )
        {
            for s in iter
            {
                self.push_str( &s );
            }
        }
    }

    impl<'a, A:Array<Item = u8>> Extend<&'a str> for SmallString<A>
    {
        fn extend<I:IntoIterator<Item = &'a str>>( &mut self, iter:I )
        {
            for s in iter
            {
                self.push_str( s );
            }
        }
    }

    impl<A:Array<Item = u8>> Extend<String> for SmallString<A>
    {
        fn extend<I:IntoIterator<Item = String>>( &mut self, iter:I )
        {
            for s in iter
            {
                self.push_str( &s );
            }
        }
    }

    impl<A:Array<Item = u8>> fmt::Debug for SmallString<A>
    {
        #[inline] fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result { fmt::Debug::fmt( &**self, f ) }
    }

    impl<A:Array<Item = u8>> fmt::Display for SmallString<A>
    {
        #[inline] fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result { fmt::Display::fmt( &**self, f ) }
    }

    eq_str!( str );
    eq_str!( &'a str );
    eq_str!( String );
    eq_str!( Cow<'a, str> );
    
    impl<A:Array<Item = u8>> PartialEq<OsStr> for SmallString<A>
    {
        #[inline] fn eq( &self, rhs:&OsStr ) -> bool{ &self[..] == rhs }
        #[inline] fn ne( &self, rhs:&OsStr ) -> bool{ &self[..] != rhs }
    }
    
    impl<'a, A:Array<Item = u8>> PartialEq<&'a OsStr> for SmallString<A>
    {
        #[inline] fn eq( &self, rhs:&&OsStr ) -> bool { &self[..] == *rhs }
        #[inline] fn ne( &self, rhs:&&OsStr ) -> bool { &self[..] != *rhs }
    }
    
    impl<A:Array<Item = u8>> PartialEq<OsString> for SmallString<A>
    {
        #[inline] fn eq( &self, rhs:&OsString ) -> bool { &self[..] == rhs }
        #[inline] fn ne( &self, rhs:&OsString ) -> bool { &self[..] != rhs }
    }
    
    impl<'a, A:Array<Item = u8>> PartialEq<Cow<'a, OsStr>> for SmallString<A>
    {
        #[inline] fn eq( &self, rhs:&Cow<OsStr> ) -> bool { self[..] == **rhs }
        #[inline] fn ne( &self, rhs:&Cow<OsStr> ) -> bool { self[..] != **rhs }
    }

    impl<A, B> PartialEq<SmallString<B>> for SmallString<A> where
    A:Array<Item = u8>,
    B:Array<Item = u8>,
    {
        #[inline] fn eq( &self, rhs:&SmallString<B> ) -> bool { &self[..] == &rhs[..] }
        #[inline] fn ne( &self, rhs:&SmallString<B> ) -> bool { &self[..] != &rhs[..] }
    }

    impl<A:Array<Item = u8>> Eq for SmallString<A> {}

    impl<A:Array<Item = u8>> PartialOrd for SmallString<A>
    {
        #[inline] fn partial_cmp( &self, rhs:&SmallString<A> ) -> Option<Ordering> { self[..].partial_cmp( &rhs[..] ) }
    }

    impl<A:Array<Item = u8>> Ord for SmallString<A>
    {
        #[inline] fn cmp( &self, rhs:&SmallString<A> ) -> Ordering { self[..].cmp( &rhs[..] ) }
    }

    impl<A:Array<Item = u8>> Hash for SmallString<A>
    {
        #[inline] fn hash<H:Hasher>( &self, state:&mut H ){ self[..].hash( state ) }
    }
    /// A draining iterator for `SmallString`.
    pub struct Drain<'a>
    {
        iter:Chars<'a>,
    }

    impl<'a> Iterator for Drain<'a>
    {
        type Item = char;
        #[inline] fn next( &mut self ) -> Option<char> { self.iter.next() }
        #[inline] fn size_hint( &self ) -> ( usize, Option<usize> ) { self.iter.size_hint() }
    }

    impl<'a> DoubleEndedIterator for Drain<'a>
    {
        #[inline] fn next_back( &mut self ) -> Option<char> { self.iter.next_back() }
    }
    /// A possible error value when creating a `SmallString` from a byte array.
    #[derive( Debug )] pub struct FromUtf8Error<A:Array<Item = u8>>
    {
        buf:A,
        error:Utf8Error,
    }

    impl<A:Array<Item = u8>> FromUtf8Error<A>
    {
        /// Returns the slice of `[u8]` bytes that were attempted to convert to a `SmallString`.
        #[inline] pub fn as_bytes( &self ) -> &[u8]
        {
            unsafe 
            {
                let ptr = &self.buf as *const _ as *const u8;
                slice::from_raw_parts( ptr, A::size() ) 
            }
        }
        /// Returns the byte array that was attempted to convert into a `SmallString`.
        #[inline] pub fn into_buf( self ) -> A { self.buf }
        /// Returns the `Utf8Error` to get more details about the conversion failure.
        #[inline] pub fn utf8_error( &self ) -> Utf8Error { self.error }
    }

    impl<A:Array<Item = u8>> fmt::Display for FromUtf8Error<A>
    {
        #[inline] fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result
        { fmt::Display::fmt( &self.error, f ) }
    }
}
pub mod string { pub use std::string::{ * }; }
pub mod sync
{
    pub use std::sync::{ atomic as libatom, * };
    pub mod atomic
    {
        pub use std::sync::atomic::{ * };
    }
}
pub mod task { pub use std::task::{ * }; }
#[macro_use] pub mod thread { pub use std::thread::{ * }; }
pub mod terminal
{
    /*
    terminfo v0.9.0 */
    pub mod info
    {
        /// String capability expansion.
        #[macro_use] pub mod expand
        {

        }

        mod error
        {

        }
        pub use self::error::{ Error, Result };
        /// Parsers for various formats.
        mod parser
        {

        }
        pub use self::expand::{ Expand };
        /// Standard terminal capabilities.
        pub mod capability
        {

        }
        pub use self::capability::{ Capability, Value };

        mod database
        {

        }
        pub use self::database::{ Database };
        /// Constants to deal with name differences across terminfo and termcap.
        pub mod names
        {

        }
    }
}
pub mod time
{
    pub use std::time::{ * };
    pub mod extension
    {
        pub use timing::{ * };
    }
}
mod types
{
    use ::
    {
        collections::{ HashMap, HashSet },
        parsers::{ self, line::{ tokens_to_redirections }, },
        regex::{ Regex },
        *,
    };

    pub type Token = ( String, String );
    pub type Tokens = Vec<Token>;
    pub type Redirection = ( String, String, String );

    #[derive( Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash )]
    pub struct WaitStatus( i32, i32, i32 );

    impl WaitStatus
    {
        pub fn from_exited( pid:i32, status:i32 ) -> Self {
            WaitStatus( pid, 0, status )
        }

        pub fn from_signaled( pid:i32, sig:i32 ) -> Self {
            WaitStatus( pid, 1, sig )
        }

        pub fn from_stopped( pid:i32, sig:i32 ) -> Self {
            WaitStatus( pid, 2, sig )
        }

        pub fn from_continuted( pid:i32 ) -> Self {
            WaitStatus( pid, 3, 0 )
        }

        pub fn from_others() -> Self {
            WaitStatus( 0, 9, 9 )
        }

        pub fn from_error( errno:i32 ) -> Self {
            WaitStatus( 0, 255, errno )
        }

        pub fn empty() -> Self {
            WaitStatus( 0, 0, 0 )
        }

        pub fn is_error( &self ) -> bool {
            self.1 == 255
        }

        pub fn is_others( &self ) -> bool {
            self.1 == 9
        }

        pub fn is_signaled( &self ) -> bool {
            self.1 == 1
        }

        pub fn get_errno( &self ) -> nix::Error {
            nix::Error::from_raw( self.2 )
        }

        pub fn is_exited( &self ) -> bool {
            self.0 != 0 && self.1 == 0
        }

        pub fn is_stopped( &self ) -> bool {
            self.1 == 2
        }

        pub fn is_continued( &self ) -> bool {
            self.1 == 3
        }

        pub fn get_pid( &self ) -> i32 {
            self.0
        }

        fn _get_signaled_status( &self ) -> i32 {
            self.2 + 128
        }

        pub fn get_signal( &self ) -> i32
        {
            self.2
        }

        pub fn get_name( &self ) -> String
        {
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
                format!( "unknown:{}", self.2 )
            }
        }

        pub fn get_status( &self ) -> i32
        {
            if self.is_exited() {
                self.2
            } else {
                self._get_signaled_status()
            }
        }
    }

    impl fmt::Debug for WaitStatus
    {
        fn fmt( &self, f:&mut fmt::Formatter<'_> ) -> fmt::Result {
            let mut formatter = f.debug_struct( "WaitStatus" );
            formatter.field( "pid", &self.0 );
            let name = self.get_name();
            formatter.field( "name", &name );
            formatter.field( "ext", &self.2 );
            formatter.finish()
        }
    }

    #[derive( Debug )]
    pub struct LineInfo
    {
        pub tokens:Tokens,
        pub is_complete:bool,
    }

    impl LineInfo
    {
        pub fn new( tokens:Tokens ) -> Self
        { LineInfo { tokens, is_complete:true } }
    }
    /*
    command line:`ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    Command
    {
        tokens:[( "", "ls" ), ( "", "-G" ), ( "\'", "foo bar" )],
        redirects_to:
        [
            ( "2", ">", "&1" ),
            ( "1", ">", "/dev/null" ),
        ],
        redirect_from:Some( ( "<", "one-file" ) ),
    } */
    #[derive( Debug )]
    pub struct Command
    {
        pub tokens:Tokens,
        pub redirects_to:Vec<Redirection>,
        pub redirect_from:Option<Token>,
    }

    #[derive( Debug )]
    pub struct CommandLine
    {
        pub line:String,
        pub commands:Vec<Command>,
        pub envs:HashMap<String, String>,
        pub background:bool,
    }

    impl Command
    {
        pub fn from_tokens( tokens:Tokens ) -> Result<Command, String>
        {
            let mut tokens_new = tokens.clone();
            let mut redirects_from_type = String::new();
            let mut redirects_from_value = String::new();
            let mut has_redirect_from = tokens_new.iter().any( |x| x.1 == "<" || x.1 == "<<<" );
            let mut len = tokens_new.len();

            while has_redirect_from
            {
                if let Some( idx ) = tokens_new.iter().position( |x| x.1 == "<" )
                {
                    redirects_from_type = "<".to_string();
                    tokens_new.remove( idx );
                    len -= 1;
                    if len > idx
                    {
                        redirects_from_value = tokens_new.remove( idx ).1;
                        len -= 1;
                    }
                }

                if let Some( idx ) = tokens_new.iter().position( |x| x.1 == "<<<" )
                {
                    redirects_from_type = "<<<".to_string();
                    tokens_new.remove( idx );
                    len -= 1;
                    if len > idx
                    {
                        redirects_from_value = tokens_new.remove( idx ).1;
                        len -= 1;
                    }
                }

                has_redirect_from = tokens_new.iter().any( |x| x.1 == "<" || x.1 == "<<<" );
            }

            let tokens_final;
            let redirects_to;
            match tokens_to_redirections( &tokens_new )
            {
                Ok( ( _tokens, _redirects_to ) ) =>
                {
                    tokens_final = _tokens;
                    redirects_to = _redirects_to;
                }

                Err( e ) => { return Err( e ); }
            }

            let redirect_from = if redirects_from_type.is_empty()
            { None }
            else
            { Some( ( redirects_from_type, redirects_from_value ) ) };

            Ok
            ( 
                Command
                {
                    tokens:tokens_final,
                    redirects_to,
                    redirect_from,
                }
 )
        }

        pub fn has_redirect_from( &self ) -> bool
        {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<"
        }

        pub fn has_here_string( &self ) -> bool
        {
            self.redirect_from.is_some() &&
            self.redirect_from.clone().unwrap().0 == "<<<"
        }

        pub fn is_builtin( &self ) -> bool
        { is::builtin( &self.tokens[0].1 ) }
    }

    #[derive( Debug, Clone, Default )]
    pub struct Job
    {
        pub cmd:String,
        pub id:i32,
        pub gid:i32,
        pub pids:Vec<i32>,
        pub pids_stopped:HashSet<i32>,
        pub status:String,
        pub is_bg:bool,
    }

    impl Job
    {
        pub fn all_members_stopped( &self ) -> bool
        {
            for pid in &self.pids
            {
                if !self.pids_stopped.contains( pid )
                {
                    return false;
                }
            }

            true
        }

        pub fn all_members_running( &self ) -> bool
        { self.pids_stopped.is_empty() }
    }

    #[derive( Clone, Debug, Default )]
    pub struct CommandResult
    {
        pub gid:i32,
        pub status:i32,
        pub stdout:String,
        pub stderr:String,
    }

    impl CommandResult
    {
        pub fn new() -> CommandResult
        {
            CommandResult
            {
                gid:0,
                status:0,
                stdout:String::new(),
                stderr:String::new(),
            }
        }

        pub fn from_status( gid:i32, status:i32 ) -> CommandResult
        {
            CommandResult
            {
                gid,
                status,
                stdout:String::new(),
                stderr:String::new(),
            }
        }

        pub fn error() -> CommandResult
        {
            CommandResult
            {
                gid:0,
                status:1,
                stdout:String::new(),
                stderr:String::new(),
            }
        }
    }

    #[derive( Clone, Debug, Default )]
    pub struct CommandOptions
    {
        pub background:bool,
        pub isatty:bool,
        pub capture_output:bool,
        pub envs:HashMap<String, String>,
    }

    fn split_tokens_by_pipes( tokens:&[Token] ) -> Vec<Tokens>
    {
        let mut cmd = Vec::new();
        let mut cmds = Vec::new();
        for token in tokens
        {
            let sep = &token.0;
            let value = &token.1;
            if sep.is_empty() && value == "|"
            {
                if cmd.is_empty()
                {
                    return Vec::new();
                }
                cmds.push( cmd.clone() );
                cmd = Vec::new();
            }
            else
            {
                cmd.push( token.clone() );
            }
        }
        if cmd.is_empty() { return Vec::new(); }
        cmds.push( cmd.clone() );
        cmds
    }

    fn drain_env_tokens( tokens:&mut Tokens ) -> HashMap<String, String>
    {
        let mut envs:HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new( r"^( [a-zA-Z0-9_]+ )=( .* )$" ).unwrap();
        for ( sep, text ) in tokens.iter()
        {
            if !sep.is_empty() || !regex::re_contains( text, r"^( [a-zA-Z0-9_]+ )=( .* )$" ) { break; }
            for cap in re.captures_iter( text )
            {
                let name = cap[1].to_string();
                let value = parsers::line::unquote( &cap[2] );
                envs.insert( name, value );
            }

            n += 1;
        }

        if n > 0 { tokens.drain( 0..n ); }
        envs
    }

    impl CommandLine
    {
        pub fn from_line( line:&str, sh:&mut shell::Shell ) -> Result<CommandLine, String>
        {
            let linfo = parsers::line::parse_line( line );
            let mut tokens = linfo.tokens;
            shell::do_expansion( sh, &mut tokens );
            let envs = drain_env_tokens( &mut tokens );

            let mut background = false;
            let len = tokens.len();
            if len > 1 && tokens[len - 1].1 == "&"
            {
                background = true;
                tokens.pop();
            }

            let mut commands = Vec::new();
            for sub_tokens in split_tokens_by_pipes( &tokens )
            {
                match Command::from_tokens( sub_tokens )
                {
                    Ok( c ) => { commands.push( c ); }
                    Err( e ) => { return Err( e ); }
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

        pub fn is_empty( &self ) -> bool
        { self.commands.is_empty() }

        pub fn with_pipeline( &self ) -> bool
        { self.commands.len() > 1 }

        pub fn is_single_and_builtin( &self ) -> bool
        { self.commands.len() == 1 && self.commands[0].is_builtin() }
    }
}
pub mod vec { pub use std::vec::{ * }; }
pub unsafe fn domain()
{
    env::initialize_path_environment();
    let mut sh = shell::Shell::new();
    let args:Vec<String> = env::args().collect();
    if is::login( &args )
    {
        rc::file::load_rc_files( &mut sh );
        sh.is_login = true;
    }

    if is::script( &args )
    {
        let status = scripts::run_script( &mut sh, &args ); // log!( "run script:{:?} ", &args );
        ::process::exit( status );
    }

    if is::command_string( &args )
    {
        let line = env::arguments_to_command_line(); // log!( "run with -c args:{}", &line );
        now::run_command_line( &mut sh, &line, false, false );
        ::process::exit( sh.previous_status );
    }

    if is::non_tty()
    {
        now::run_procs_for_non_tty( &mut sh );
        return;
    }

    let mut rl;
    match Interface::new( "pls" )
    {
        Ok( x ) => { rl = x; }
        Err( e ) =>
        {
            println!( "pls:linefeed error:{}", e );
            return;
        }
    }

    rl.define_function( "enter-function", Arc::new( prompt::EnterFunction ) );
    rl.bind_sequence( "\r", Command::from_str( "enter-function" ) );

    history::init( &mut rl );
    rl.set_completer
    ( 
        Arc::new
        ( 
            completers::CicadaCompleter
            {
                sh:Arc::new( sh.clone() ),
            }
        )
    );

    let sig_handler_enabled = is::signal_handler_enabled();
    if sig_handler_enabled
    {
        signals::setup_sigchld_handler();
        signals::block_signals();
    }

    loop
    {
        let prompt = get::prompt( &sh );
        match rl.set_prompt( &prompt )
        {
            Ok( _ ) => {}
            Err( e ) => { println_stderr!( "pls:prompt error:{}", e ); }
        }

        if sig_handler_enabled { signals::unblock_signals(); }

        match rl.read_line()
        {
            Ok( ReadResult::Input( line ) ) =>
            {
                if sig_handler_enabled { signals::block_signals(); }

                let line = shell::trim_multiline_prompts( &line );
                if line.trim() == ""
                {
                    c::job::try_wait_bg_jobs( &mut sh, true, sig_handler_enabled );
                    continue;
                }
                sh.cmd = line.clone();

                let tsb = c::time::DateTime::now().unix_timestamp();
                let mut line = line.clone();
                path::extend_bangbang( &sh, &mut line );

                let mut status = 0; //log!( "run now::run_command_line:{}", line );

                let cr_list = now::run_command_line( &mut sh, &line, true, false );
                if let Some( last ) = cr_list.last() { status = last.status; }

                let tse = c::time::DateTime::now().unix_timestamp();
                if !sh.cmd.starts_with( ' ' ) && line != sh.previous_cmd
                {
                    history::add( &sh, &mut rl, &line, status, tsb, tse );
                    sh.previous_cmd = line.clone();
                }

                if is::shell_altering_command( &line )
                {
                    rl.set_completer
                    ( 
                        Arc::new
                        ( 
                            completers::CicadaCompleter
                            {
                                sh:Arc::new( sh.clone() ),
                            }
                        )
                    );
                }

                c::job::try_wait_bg_jobs( &mut sh, true, sig_handler_enabled );
                continue;
            }

            Ok( ReadResult::Eof ) =>
            {
                if let Ok( x ) = env::var( "NO_EXIT_ON_CTRL_D" )
                {
                    if x == "1"
                    {
                        println!();
                    }
                }

                else
                {
                    println!( "exit" );
                    break;
                }
            }

            Ok( ReadResult::Signal( s ) ) => { println_stderr!( "readline signal:{:?}", s ); }

            Err( e ) =>
            {
                println_stderr!( "readline error:{}", e );
                let gid = libc::getpgid( 0 );
                shell::give_terminal_to( gid );
            }
        }

        if sig_handler_enabled { signals::block_signals(); }
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
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 20963
