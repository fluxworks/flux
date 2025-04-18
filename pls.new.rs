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
extern crate num_traits;
extern crate time as timed;
extern crate rand;
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
        ( $fmt:expr ) =>
        ( 
            let log_file = if let Ok( x ) = ::env::var( "CICADA_LOG_FILE" )
            { x.clone() } 
            else { String::new() };

            if !log_file.is_empty()
            {

                let msg = $fmt;
                match  OpenOptions::new().append( true ).create( true ).open( &log_file )
                {
                    Ok( mut cfile ) =>
                    {
                        let pid = $crate::get::pid();
                        let now = $crate::time::c::DateTime::now();
                        let msg = format!( "[{}][{}] {}", now, pid, msg );
                        let msg = if msg.ends_with( '\n' )
            { msg } else { format!( "{}\n", msg ) };

                        match cfile.write_all( msg.as_bytes() )
                        {
                            Ok( _ ) => {}
                            Err( _ ) => println!( "tlog: write_all error" )
                        }
                    }
                    Err( _ ) => println!( "tlog: open file error" ),
                }
            }
       );

        ( $fmt:expr, $( $arg:tt )* ) =>
        ( 
            let msg = format!( $fmt, $( $arg )* );
            log!( &msg );
       );
    }

    #[macro_export] macro_rules! println_stderr
    {
        ( $fmt:expr ) =>
        ( 
            match writeln!( &mut $crate::io::stderr(), $fmt )
            {
                Ok( _ ) => {}
                Err( e ) => println!( "write to stderr failed: {:?}", e )
            }
       );

        ( $fmt:expr, $( $arg:tt )* ) =>
        ( 
            match writeln!( &mut $crate::io::stderr(), $fmt, $( $arg )* )
            {
                Ok( _ ) => {}
                Err( e ) => println!( "write to stderr failed: {:?}", e )
            }
       );
    }

    #[macro_export] macro_rules! map
    {
        {} =>
        {
            ::collections::HashMap::new()
        };
        
        { $( $key:expr => $value:expr ),+ , } =>
        {
            map!{ $( $key => $value ),+ }
        };

        { $( $key:expr => $value:expr ),* } =>
        {
            {
                let mut _map = ::collections::HashMap::new();
                $( 
                    let _ = _map.insert( $key, $value );
                )*
                _map
            }
        };
    }
    /// Given an int, creates and returns a `BigInt`.
    #[macro_export] macro_rules! int
    {
        ($int:expr) => 
        {{
            use ::num::big::BigInt;
            let _b: BigInt = $int.into();
            _b
        }};
    }
    /// Given two ints, creates and returns a `BigRational`.
    #[macro_export] macro_rules! frac
    {
        ($int1:expr, $int2:expr) =>
        {{
            ::num::rational::BigRational::new($int1.into(), $int2.into())
        }};
    }
    /// Converts each element of a list to a `Value` and returns an `Arr` containing a vector of the values.
    #[macro_export] macro_rules! arr
    {
        [] => { $crate::database::arrays::Arr::from_vec(vec![]).unwrap() };
        
        [ $( $elem:expr ),+ , ] =>
        {
            try_arr![ $( $elem ),+ ].unwrap()
        };

        [ $( $elem:expr ),+ ] =>
        {
            try_arr![ $( $elem ),+ ].unwrap()
        };
    }
    /// Converts each element of a list to a `Value` and returns an `Arr` containing a vector of the values.
    #[macro_export] macro_rules! try_arr
    {
        [ $( $elem:expr ),+ , ] =>
        {
            try_arr![ $( $elem ),+ ]
        };
        
        [ $( $elem:expr ),+ ] =>
        {
            {
                $crate::database::arrays::Arr::from_vec(vec![ $( $elem.into() ),+ ])
            }
        };
    }
    /// Converts each element  of a list to `Value`s and returns a `Tup` containing a vector of the values.
    #[macro_export] macro_rules! tup
    {
        ( $( $elem:expr ),* , ) =>
        {
            tup!( $( $elem ),* )
        };

        ( $( $elem:expr ),* ) => {
            {
                $crate::database::tuples::Tup::from_vec(vec![ $( $elem.into() ),+ ])
            }
        };
    }
    /// Given a list of field/value pairs, returns an `Obj` containing each pair.
    #[macro_export] macro_rules! obj
    {
        {} =>
        {
            $crate::database::objects::Obj::from_map_unchecked( ::collections::HashMap::new())
        };

        { $( $field:expr => $inner:expr ),+ , } =>
        {
            try_obj!{ $( $field => $inner ),+ }.unwrap()
        };

        { $( $field:expr => $inner:expr ),+ } =>
        {
            try_obj!{ $( $field => $inner ),+ }.unwrap()
        };
    }
    /// Given a list of field to `Value` pairs, returns an `Obj` with the fields and values.
    #[macro_export] macro_rules! try_obj
    {
        { $( $field:expr => $inner:expr ),+ , } =>
        {
            try_obj!{ $( $field => $inner ),* };
        };

        { $( $field:expr => $inner:expr ),+ } =>
        {
            {
                use $crate::database::objects::Obj;

                let mut _map = ::collections::HashMap::new();
                let mut _parent: Option<$crate::database::value::Value> = None;

                $(
                    if $field == "^" { _parent = Some($inner.into()); }
                    else { _map.insert($field.into(), $inner.into()); }
                )*

                match _parent
                {
                    Some(parent) => match parent.get_obj() {
                        Ok(parent) => Obj::from_map_with_parent(_map, parent),
                        e @ Err(_) => e,
                    }
                    None => Obj::from_map(_map),
                }
            }
        };
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

    use ::
    {
        borrow::{ Cow },
        ops::{ Range, RangeFrom, RangeFull, RangeTo },
        str::{ from_utf8, from_utf8_unchecked },
        *,
    };
    /*
    pub fn repeat_char( ch: char, n: usize ) -> String */
    /// Returns a string consisting of a `char`, repeated `n` times.
    pub fn repeat( ch: char, n: usize ) -> String
    {
        let mut buf = [0; 4];
        let s = ch.encode_utf8( &mut buf );
        s.repeat( n )
    }
    /*
    pub fn forward_char( n:usize, s:&str, cur: usize ) -> usize */
    pub fn forward( n:usize, s:&str, cur: usize ) -> usize
    {
        let mut chars = s[cur..].char_indices()
            .filter( |&( _, ch )| !is_combining_mark( ch ) );

        for _ in 0..n {
            match chars.next()
            {
                Some( _ ) => (),
                None => return s.len()
            }
        }

        match chars.next()
            {
            Some( ( idx, _ ) ) => cur + idx,
            None => s.len()
        }
    }
    /*
    pub fn forward_search_char( n:usize, buf:&str, mut cur:usize, ch: char ) -> Option<usize> */
    pub fn forward_search( n:usize, buf:&str, mut cur:usize, ch: char ) -> Option<usize>
    {
        let mut pos = None;

        for _ in 0..n
        {
            let off = match buf[cur..].chars().next()
            {
                Some( ch ) => ch.len_utf8(),
                None => break
            };

            match buf[cur + off..].find( ch )
            {
                Some( p ) =>
                {
                    cur += off + p;
                    pos = Some( cur );
                }

                None => break
            }
        }

        pos
    }
    /*
    pub fn backward_char( n:usize, s:&str, cur: usize ) -> usize */
    pub fn backward( n:usize, s:&str, cur: usize ) -> usize
    {
        let mut chars = s[..cur].char_indices()
            .filter( |&( _, ch )| !is_combining_mark( ch ) );
        let mut res = cur;

        for _ in 0..n {
            match chars.next_back()
            {
                Some( ( idx, _ ) ) => res = idx,
                None => return 0
            }
        }

        res
    }
    /*
    pub fn backward_search_char( n:usize, buf:&str, mut cur:usize, ch: char ) -> Option<usize> */
    pub fn backward_search( n:usize, buf:&str, mut cur:usize, ch: char ) -> Option<usize>
    {
        let mut pos = None;

        for _ in 0..n
        {
            match buf[..cur].rfind( ch )
            {
                Some( p ) =>
                {
                    cur = p;
                    pos = Some( cur );
                }

                None => break
            }
        }

        pos
    }
    /// Returns the first character in the buffer, if it contains any valid characters.
    pub fn first_char( buf:&[u8] ) -> io::Result<Option<char>>
    {
        match from_utf8( buf )
        {
            Ok( s ) => Ok( s.chars().next() ),
            Err( e ) =>
            {
                if e.error_len().is_some()
                {
                    return Err( io::Error::new( io::ErrorKind::InvalidData, "invalid utf-8 input received" ) );
                }

                let valid = e.valid_up_to();
                let s = unsafe { from_utf8_unchecked( &buf[..valid] ) };
                Ok( s.chars().next() )
            }
        }
    }
    /// Returns a character sequence escaped( `\e` ) for user-facing display.
    pub fn escape_sequence(s: &str) -> String
    {
        let mut res = String::with_capacity(s.len());

        for ch in s.chars()
        {
            match ch
            {
                ESCAPE => res.push_str(r"\e"),
                RUBOUT => res.push_str(r"\C-?"),
                '\\' => res.push_str(r"\\"),
                '\'' => res.push_str(r"\'"),
                '"' => res.push_str(r#"\""#),
                
                ch if is_ctrl(ch) =>
                {
                    res.push_str(r"\C-");
                    res.push(unctrl_lower(ch));
                }

                ch => res.push(ch)
            }
        }

        res
    }
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
    
    use ::
    {
        iter::{ FromIterator },
        mem::{ replace },
        *,
    };
    /* Utilities for manipulating raw input sequences. */

    /// Contains a set of string sequences, mapped to a value.
    #[derive( Clone, Debug, Default )]
    pub struct SequenceMap<K, V>
    {
        sequences: Vec<( K, V )>,
    }
    /// Represents the result of a `SequenceMap::find` operation.
    #[derive( Copy, Clone, Debug, Eq, PartialEq )]
    pub enum FindResult<V>
    {
        /// No contained sequences begin with the provided input sequence.
        NotFound,
        /// One or more sequences begin with the provided input sequence,
        /// but the sequence does not represent a complete sequence.
        Incomplete,
        /// A sequence was found exactly matching the input sequence;
        /// additionally, one or more sequences begin with the input sequence.
        Undecided( V ),
        /// A sequence was found exactly matching the input sequence;
        /// no additional partially-matching sequences exist.
        Found( V ),
    }

    impl<'a, V: Clone> FindResult<&'a V>
    {
        /// Maps `FindResult<&V>` to `FindResult<V>` by cloning the contents of the result value.
        pub fn cloned( self ) -> FindResult<V>
        {
            match self
            {
                FindResult::NotFound => FindResult::NotFound,
                FindResult::Incomplete => FindResult::Incomplete,
                FindResult::Undecided( v ) => FindResult::Undecided( v.clone() ),
                FindResult::Found( v ) => FindResult::Found( v.clone() ),
            }
        }
    }

    impl<K: AsRef<str>, V> SequenceMap<K, V>
    {
        /// Creates an empty `SequenceMap`.
        pub fn new() -> SequenceMap<K, V> { SequenceMap::with_capacity( 0 ) }
        /// Creates an empty `SequenceMap` with allocated capacity for `n` elements.
        pub fn with_capacity( n: usize ) -> SequenceMap<K, V>
        {
            SequenceMap
            {
                sequences: Vec::with_capacity( n ),
            }
        }
        /// Returns a slice of all contained sequences, sorted by key.
        pub fn sequences( &self ) -> &[( K, V )] { &self.sequences }
        /// Returns a mutable slice of all contained sequences, sorted by key.
        pub fn sequences_mut( &mut self ) -> &mut [( K, V )] { &mut self.sequences }
        /// Returns an `Entry` for the given key.
        pub fn entry( &mut self, key: K ) -> Entry<K, V>
        {
            match self.search( key.as_ref() )
            {
                Ok( n ) => Entry::Occupied( OccupiedEntry
                {
                    map: self,
                    index: n,
                } ),

                Err( n ) => Entry::Vacant( VacantEntry
                {
                    map: self,
                    key,
                    index: n,
                } )
            }
        }
        /// Performs a search for a partial or complete sequence match.
        pub fn find( &self, key:&str ) -> FindResult<&V>
        {
            let ( n, found ) = match self.search( key )
            {
                Ok( n ) => ( n, true ),
                Err( n ) => ( n, false )
            };

            let incomplete = self.sequences.get( n + ( found as usize ) )
                .map_or( false, |&( ref next, _ )| next.as_ref().starts_with( key ) );

            match ( found, incomplete )
            {
                ( false, false ) => FindResult::NotFound,
                ( false, true ) => FindResult::Incomplete,
                ( true, false ) => FindResult::Found( &self.sequences[n].1 ),
                ( true, true ) => FindResult::Undecided( &self.sequences[n].1 ),
            }
        }

        /// Returns the corresponding value for the given sequence.
        pub fn get( &self, key:&str ) -> Option<&V> {
            match self.search( key )
            {
                Ok( n ) => Some( &self.sequences[n].1 ),
                Err( _ ) => None
            }
        }

        /// Returns a mutable reference to the corresponding value for the given sequence.
        pub fn get_mut( &mut self, key:&str ) -> Option<&mut V> {
            match self.search( key )
            {
                Ok( n ) => Some( &mut self.sequences[n].1 ),
                Err( _ ) => None
            }
        }

        /// Inserts a key-value pair into the map.
        ///
        /// If the key already exists in the map, the new value will replace the old
        /// value and the old value will be returned.
        pub fn insert( &mut self, key: K, value: V ) -> Option<V> {
            match self.search( key.as_ref() )
            {
                Ok( n ) => Some( replace( &mut self.sequences[n], ( key, value ) ).1 ),
                Err( n ) => {
                    self.sequences.insert( n, ( key, value ) );
                    None
                }
            }
        }

        /// Removes a key-value pair from the map.
        pub fn remove( &mut self, key:&str ) -> Option<( K, V )> {
            match self.search( key )
            {
                Ok( n ) => Some( self.sequences.remove( n ) ),
                Err( _ ) => None
            }
        }

        fn search( &self, key:&str ) -> Result<usize, usize> {
            self.sequences.binary_search_by_key( &key, |&( ref k, _ )| &k.as_ref() )
        }
    }

    impl<K: AsRef<str>, V> From<Vec<( K, V )>> for SequenceMap<K, V>
    {
        /// Creates a `SequenceMap` from a `Vec` of key-value pairs.
        fn from( mut sequences: Vec<( K, V )> ) -> SequenceMap<K, V>
        {
            sequences.sort_by( |a, b| a.0.as_ref().cmp( b.0.as_ref() ) );
            sequences.dedup_by( |a, b| a.0.as_ref() == b.0.as_ref() );
            SequenceMap{sequences}
        }
    }

    impl<K: AsRef<str>, V> FromIterator<( K, V )> for SequenceMap<K, V>
    {
        /// Creates a `SequenceMap` from an iterator of key-value pairs.
        fn from_iter<I: IntoIterator<Item=( K, V )>>( iter: I ) -> Self
        {
            let iter = iter.into_iter();
            let mut map = SequenceMap::with_capacity( iter.size_hint().0 );

            for ( k, v ) in iter
            {
                map.insert( k, v );
            }

            map
        }
    }
    /// A view into a single entry of a `SequenceMap`, which may be either occupied or vacant.
    pub enum Entry<'a, K: 'a, V: 'a>
    {
        /// An occupied entry
        Occupied( OccupiedEntry<'a, K, V> ),
        /// A vacant entry
        Vacant( VacantEntry<'a, K, V> ),
    }

    impl<'a, K, V> Entry<'a, K, V>
    {
        /// Provides in-place mutable access to an occupied entry before any potential inserts into the map.
        pub fn and_modify<F: FnOnce( &mut V )>( self, f: F ) -> Self
        {
            match self
            {
                Entry::Occupied( mut ent ) =>
                {
                    f( ent.get_mut() );
                    Entry::Occupied( ent )
                }

                Entry::Vacant( ent ) => Entry::Vacant( ent )
            }
        }
        /// Returns a mutable reference to the entry value, inserting the provided default if the entry is vacant.
        pub fn or_insert( self, default: V ) -> &'a mut V
        {
            match self
            {
                Entry::Occupied( ent ) => ent.into_mut(),
                Entry::Vacant( ent ) => ent.insert( default )
            }
        }
        /// Returns a mutable reference to the entry value,
        /// inserting a value using the provided closure if the entry is vacant.
        pub fn or_insert_with<F: FnOnce() -> V>( self, default: F ) -> &'a mut V
        {
            match self
            {
                Entry::Occupied( ent ) => ent.into_mut(),
                Entry::Vacant( ent ) => ent.insert( default() )
            }
        }
        /// Returns a borrowed reference to the entry key.
        pub fn key( &self ) -> &K
        {
            match *self
            {
                Entry::Occupied( ref ent ) => ent.key(),
                Entry::Vacant( ref ent ) => ent.key(),
            }
        }
    }

    impl<'a, K: fmt::Debug, V: fmt::Debug> fmt::Debug for Entry<'a, K, V>
    {
        fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result
        {
            match *self
            {
                Entry::Occupied( ref ent ) =>
                f.debug_tuple( "Entry" )
                .field( ent )
                .finish(),

                Entry::Vacant( ref ent ) =>
                f.debug_tuple( "Entry" )
                .field( ent )
                .finish()
            }
        }
    }
    /// A view into an occupied entry in a `SequenceMap`.
    pub struct OccupiedEntry<'a, K: 'a, V: 'a>
    {
        map:&'a mut SequenceMap<K, V>,
        index:usize,
    }

    impl<'a, K, V> OccupiedEntry<'a, K, V>
    {
        /// Returns a borrowed reference to the entry key.
        pub fn key( &self ) -> &K { &self.map.sequences[self.index].0 }
        /// Returns a borrowed reference to the entry value.
        pub fn get( &self ) -> &V { &self.map.sequences[self.index].1 }
        /// Returns a mutable reference to the entry value.
        pub fn get_mut( &mut self ) -> &mut V { &mut self.map.sequences[self.index].1 }
        /// Converts the `OccupiedEntry` into a mutable reference whose lifetime is bound to the `SequenceMap`.
        pub fn into_mut( self ) -> &'a mut V { &mut self.map.sequences[self.index].1 }
        /// Replaces the entry value with the given value, returning the previous value.
        pub fn insert( &mut self, value: V ) -> V { replace( self.get_mut(), value ) }
        /// Removes the entry and returns the value.
        pub fn remove( self ) -> V { self.map.sequences.remove( self.index ).1 }
        /// Removes the entry and returns the key-value pair.
        pub fn remove_entry( self ) -> ( K, V )
            { self.map.sequences.remove( self.index ) }
    }

    impl<'a, K: fmt::Debug, V: fmt::Debug> fmt::Debug for OccupiedEntry<'a, K, V>
    {
        fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result
        {
            f.debug_struct( "OccupiedEntry" )
            .field( "key", self.key() )
            .field( "value", self.get() )
            .finish()
        }
    }
    /// A view into a vacant entry in a `SequenceMap`.
    pub struct VacantEntry<'a, K: 'a, V: 'a>
    {
        map:&'a mut SequenceMap<K, V>,
        key: K,
        index:usize,
    }

    impl<'a, K, V> VacantEntry<'a, K, V>
    {
        /// Returns a borrowed reference to the entry key.
        pub fn key( &self ) -> &K { &self.key }
        /// Consumes the `VacantEntry` and returns ownership of the key.
        pub fn into_key( self ) -> K { self.key }
        /// Consumes the `VacantEntry` and inserts a value, returning a mutable
        /// reference to its place in the `SequenceMap`.
        pub fn insert( self, value: V ) -> &'a mut V
        {
            self.map.sequences.insert( self.index, ( self.key, value ) );
            &mut self.map.sequences[self.index].1
        }
    }

    impl<'a, K: fmt::Debug, V> fmt::Debug for VacantEntry<'a, K, V>
    {
        fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result
        {
            f.debug_tuple( "VacantEntry" )
            .field( self.key() )
            .finish()
        }
    }
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
            #[derive( Clone, Debug, Eq, PartialEq )]
            pub enum Command
            {
                $( #[$meta] $name , )+
                /// Custom application-defined command
                Custom( Cow<'static, str> ),
                /// Execute a given key sequence
                Macro( Cow<'static, str> ),
            }

            /// List of all command names
            pub static COMMANDS:&[&str] = &[ $( $str ),+ ];

            impl fmt::Display for Command
            {
                fn fmt( &self, f:&mut fmt::Formatter ) -> fmt::Result
                {
                    match *self
                    {
                        $( Command::$name => f.write_str( $str ) , )+
                        Command::Custom( ref s ) => f.write_str( s ),
                        Command::Macro( ref s ) => write!( f, "\"{}\"",
                            escape_sequence( s ) )
                    }
                }
            }

            impl Command
            {
                /// Constructs a command from a `'static str` reference.
                pub fn from_str( name:&'static str ) -> Command {
                    Command::opt_from_str( name )
                        .unwrap_or_else( || Command::Custom( Borrowed( name ) ) )
                }
                /// Constructs a command from a non-`'static` string-like type.
                pub fn from_string<T>( name:T ) -> Command where
                T: AsRef<str> + Into<String>
                {
                    Command::opt_from_str( name.as_ref() )
                        .unwrap_or_else( || Command::Custom( Owned( name.into() ) ) )
                }

                fn opt_from_str( s:&str ) -> Option<Command>
                {
                    match s
                    {
                        $( $str => Some( Command::$name ), )+
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
    #[derive( Copy, Clone, Debug, Eq, PartialEq )]
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
        pub fn category( &self ) -> Category
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
        fn default() -> Self { Command::Custom( Borrowed( "" ) ) }
    }

    pub mod api
    {
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
}
/// API for connecting to a flat file database
pub mod connect
{

}
/// Traits for conversions between types.
pub mod convert
{
    pub use std::convert::{ * };
}
/// Flat file database using the OVER data format
pub mod database
{
    use ::
    {
        fs::{ File },
        io::{ Write },
        *,
    };
    /// Indent step in .over files.
    pub const INDENT_STEP: usize = 4;

    pub mod arrays
    {
        //! An [`Arr`] container which can hold an arbitrary number of elements of a single type.
        use ::
        {
            *,
        };
        /*
        use crate::parse::format::Format;
        use crate::types::Type;
        use crate::value::Value;
        use crate::{OverError, OverResult, INDENT_STEP};
        use std::fmt;
        use std::slice::Iter;
        use std::sync::Arc;
        */
        #[derive(Clone, Debug)]
        struct ArrInner
        {
            vec: Vec<Value>,
            inner_t: Type,
        }
        /// `Arr` struct.
        #[derive(Clone, Debug)]
        pub struct Arr
        {
            inner: Arc<ArrInner>,
        }

        impl Arr
        {
            /// Returns a new `Arr` from the given vector of `Value`s.
            pub fn from_vec(vec: Vec<Value>) -> OverResult<Arr> {
                let mut tcur = Type::Any;
                let mut has_any = true;

                for value in &vec {
                    let tnew = value.get_type();

                    if has_any {
                        match Type::most_specific(&tcur, &tnew) {
                            Some((t, any)) => {
                                tcur = t;
                                has_any = any;
                            }
                            None => return Err(OverError::ArrTypeMismatch(tcur, tnew)),
                        }
                    } else if tcur != tnew {
                        return Err(OverError::ArrTypeMismatch(tcur, tnew));
                    }
                }

                Ok(Arr {
                    inner: Arc::new(ArrInner { vec, inner_t: tcur }),
                })
            }

            /// Returns a new `Arr` from the given vector of `Value`s without checking whether every value
            /// in `vec` is the same type.
            ///
            /// It is much faster than the safe version, [`from_vec`], if you know every element in `vec` is
            /// of type `inner_t`.
            pub fn from_vec_unchecked(vec: Vec<Value>, inner_t: Type) -> Arr {
                Arr {
                    inner: Arc::new(ArrInner { vec, inner_t }),
                }
            }

            /// Returns a reference to the inner vec of this `Arr`.
            pub fn vec_ref(&self) -> &Vec<Value> {
                &self.inner.vec
            }

            /// Iterates over each `Value` in `self`, applying `Fn` `f`.
            pub fn with_each<F>(&self, mut f: F)
            where
                F: FnMut(&Value),
            {
                for value in &self.inner.vec {
                    f(value)
                }
            }

            /// Gets the value at `index`.
            /// Returns an error if `index` is out of bounds.
            pub fn get(&self, index: usize) -> OverResult<Value> {
                if index >= self.inner.vec.len() {
                    Err(OverError::ArrOutOfBounds(index))
                } else {
                    Ok(self.inner.vec[index].clone())
                }
            }

            /// Returns the type of all elements in this `Arr`.
            pub fn inner_type(&self) -> Type {
                self.inner.inner_t.clone()
            }

            /// Returns the length of this `Arr`.
            pub fn len(&self) -> usize {
                self.inner.vec.len()
            }

            /// Returns whether this `Arr` is empty.
            pub fn is_empty(&self) -> bool {
                self.inner.vec.is_empty()
            }

            /// Returns whether `self` and `other` point to the same data.
            pub fn ptr_eq(&self, other: &Self) -> bool {
                Arc::ptr_eq(&self.inner, &other.inner)
            }

            /// Returns an iterator over the Arr.
            pub fn iter(&self) -> Iter<Value> {
                self.vec_ref().iter()
            }
        }

        impl Default for Arr
        {
            fn default() -> Self {
                Self::from_vec_unchecked(vec![], Type::Any)
            }
        }

        impl fmt::Display for Arr
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.format(true, INDENT_STEP))
            }
        }

        impl PartialEq for Arr
        {
            fn eq(&self, other: &Self) -> bool {
                // Quickly return false if the types don't match.
                if self.inner.inner_t != other.inner.inner_t {
                    return false;
                }

                self.inner.vec == other.inner.vec
            }
        }
    }

    pub mod objects
    {
        //! A hashmap of keys to values, where values can be any type, including other objects.
        use ::
        {
            *,
        };
        /*
        use crate::arr::Arr;
        use crate::error::OverError;
        use crate::parse;
        use crate::parse::format::Format;
        use crate::tup::Tup;
        use crate::types::Type;
        use crate::util::{is_digit, write_file_str};
        use crate::value::Value;
        use crate::{OverResult, INDENT_STEP};
        use num_bigint::BigInt;
        use num_rational::BigRational;
        use num_traits::Zero;
        use std::collections::hash_map::{Iter, Keys, Values};
        use std::collections::HashMap;
        use std::str::FromStr;
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;
        use std::{convert, fmt, io}; */

        lazy_static!
        {
            static ref CUR_ID: AtomicUsize = AtomicUsize::new(0);
        }
        
        macro_rules! get_fn
        {
            ( $doc:expr, $name:tt, $type:ty ) =>
            {
                #[doc=$doc]
                pub fn $name(&self, field: &str) -> OverResult<$type>
                {
                    match self.get(field)
                    {
                        Some(value) =>
                        {
                            match value.$name()
                            {
                                Ok(result) => Ok(result),
                                e @ Err(_) => e,
                            }
                        }
                        
                        None => Err(OverError::FieldNotFound(field.into())),
                    }
                }
            }
        }

        #[derive(Clone, Debug)]
        struct ObjInner
        {
            map: HashMap<String, Value>,
            parent: Option<Obj>,
            id: usize,
        }
        /// `Obj` struct.
        #[derive(Clone, Debug)]
        pub struct Obj
        {
            inner: Arc<ObjInner>,
        }

        impl Obj
        {
            /// Returns a new `Obj` created from the given `HashMap`.
            pub fn from_map(obj_map: HashMap<String, Value>) -> OverResult<Obj>
            {
                for field in obj_map.keys()
                {
                    if !Self::is_valid_field(field) { return Err(OverError::InvalidFieldName((*field).clone())); }
                }

                let id = get_id();

                Ok(Obj
                {
                    inner: Arc::new(ObjInner
                    {
                        map: obj_map,
                        parent: None,
                        id,
                    }),
                })
            }
            /// Returns a new `Obj` created from the given `HashMap` with given `parent`.
            pub fn from_map_with_parent(obj_map: HashMap<String, Value>, parent: Obj) -> OverResult<Obj>
            {
                for field in obj_map.keys()
                {
                    if !Self::is_valid_field(field) { return Err(OverError::InvalidFieldName(field.clone())); }
                }

                let id = get_id();

                Ok(Obj
                {
                    inner: Arc::new(ObjInner
                    {
                        map: obj_map,
                        parent: Some(parent),
                        id,
                    }),
                })
            }
            /// Returns a new `Obj` created from the given `HashMap`.
            pub fn from_map_unchecked(obj_map: HashMap<String, Value>) -> Obj
            {
                let id = get_id();

                Obj
                {
                    inner: Arc::new(ObjInner
                    {
                        map: obj_map,
                        parent: None,
                        id,
                    }),
                }
            }
            /// Returns a new `Obj` created from the given `HashMap` with given `parent`.
            pub fn from_map_with_parent_unchecked(obj_map: HashMap<String, Value>, parent: Obj) -> Obj
            {
                let id = get_id();

                Obj
                {
                    inner: Arc::new(ObjInner
                    {
                        map: obj_map,
                        parent: Some(parent),
                        id,
                    }),
                }
            }
            /// Returns the ID of this `Obj`.
            pub fn id(&self) -> usize { self.inner.id }
            /// Returns a reference to the inner map of this `Obj`.
            pub fn map_ref(&self) -> &HashMap<String, Value> { &self.inner.map }
            /// Returns a new `Obj` loaded from a file.
            pub fn from_file(path: &str) -> OverResult<Obj> { Ok(parse::load_from_file(path)?) }
            /// Writes this `Obj` to given file in `.over` representation.
            ///
            /// # Notes
            /// Note that the fields of the `Obj` will be output in an unpredictable order.
            /// Also note that shorthand in the original file, including variables and file includes,
            /// is not preserved when parsing the file, and will not appear when writing to another file.
            pub fn write_to_file(&self, path: &str) -> OverResult<()>
            {
                write_file_str(path, &self.write_str())?;
                Ok(())
            }
            /// Writes this `Obj` to a `String`.
            pub fn write_str(&self) -> String { self.format(false, 0) }
            /// Iterates over each `(String, Value)` pair in `self`, applying `f`.
            pub fn with_each<F>(&self, mut f: F) where
            F: FnMut(&String, &Value)
            {
                for (field, value) in &self.inner.map
                {
                    f(field, value)
                }
            }
            /// Returns the number of fields for this `Obj` (parent fields not included).
            pub fn len(&self) -> usize { self.inner.map.len() }
            /// Returns whether this `Obj` is empty.
            pub fn is_empty(&self) -> bool { self.inner.map.is_empty() }
            /// Returns whether `self` and `other` point to the same data.
            pub fn ptr_eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.inner, &other.inner) }
            /// Returns true if this `Obj` contains `field`.
            pub fn contains(&self, field: &str) -> bool { self.inner.map.contains_key(field) }
            /// Gets the `Value` associated with `field`.
            pub fn get(&self, field: &str) -> Option<Value>
            {
                match self.inner.map.get(field)
                {
                    Some(value) => Some(value.clone()),
                    None => match self.inner.parent
                    {
                        Some(ref parent) => parent.get(field),
                        None => None,
                    },
                }
            }
            /// Gets the `Value` associated with `field` and the `Obj` where it was found.
            pub fn get_with_source(&self, field: &str) -> Option<(Value, Obj)>
            {
                match self.inner.map.get(field)
                {
                    Some(value) => Some((value.clone(), self.clone())),
                    None => match self.inner.parent
                    {
                        Some(ref parent) => parent.get_with_source(field),
                        None => None,
                    },
                }
            }
            get_fn!
            (
                "Returns the `bool` found at `field`.",
                get_bool,
                bool
            );
            get_fn!
            (
                "Returns the `BigInt` found at `field`.",
                get_int,
                BigInt
            );
            get_fn!
            (
                "Returns the `BigRational` found at `field`.",
                get_frac,
                BigRational
            );
            get_fn!
            (
                "Returns the `char` found at `field`.",
                get_char,
                char
            );
            get_fn!
            (
                "Returns the `String` found at `field`.",
                get_str,
                String
            );
            get_fn!
            (
                "Returns the `Arr` found at `field`.",
                get_arr,
                Arr
            );
            get_fn!
            (
                "Returns the `Tup` found at `field`.",
                get_tup,
                Tup
            );
            get_fn!
            (
                "Returns the `Obj` found at `field`.",
                get_obj,
                Obj
            );
            /// Returns whether this `Obj` has a parent.
            pub fn has_parent(&self) -> bool { self.inner.parent.is_some() }
            /// Returns the parent for this `Obj`.
            pub fn get_parent(&self) -> Option<Obj>
            {
                match self.inner.parent
                {
                    Some(ref parent) => Some(parent.clone()),
                    None => None,
                }
            }
            /// Returns true if `field` is a valid field name for an `Obj`.
            pub fn is_valid_field(field: &str) -> bool
            {
                let mut first = true;

                for ch in field.chars()
                {
                    if first
                    {
                        if !Self::is_valid_field_char(ch, true) { return false; }
                        first = false;
                    }

                    else if !Self::is_valid_field_char(ch, false) { return false; }
                }

                true
            }
            /// Returns true if the given char is valid for a field, depending on whether its the first char or not.
            pub fn is_valid_field_char(ch: char, first: bool) -> bool
            {
                match ch
                {
                    ch if ch.is_alphabetic() => true,
                    ch if is_digit(ch) => !first,
                    '_' => true,
                    '^' => first,
                    _ => false,
                }
            }
            /// An iterator visiting all fields (keys) in arbitrary order.
            pub fn keys(&self) -> Keys<String, Value> { self.map_ref().keys() }
            /// An iterator visiting all values in arbitrary order.
            pub fn values(&self) -> Values<String, Value> { self.map_ref().values() }
            /// An iterator visiting all field-value pairs in arbitrary order.
            pub fn iter(&self) -> Iter<String, Value> { self.map_ref().iter() }
        }

        impl Default for Obj
        {
            fn default() -> Self { Self::from_map_unchecked(map! {}) }
        }

        impl fmt::Display for Obj
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                write!(f, "{}", self.format(true, INDENT_STEP))
            }
        }

        impl FromStr for Obj
        {
            type Err = OverError;

            fn from_str(s: &str) -> Result<Self, Self::Err> { Ok(parse::load_from_str(s)?) }
        }
        /// For two Objs to be equal, the following two checks must pass:
        /// 1. If either Obj has a parent, then both must have parents and the parents must be equal.
        /// 2. The two Objs must have all the same fields pointing to the same values.
        impl PartialEq for Obj
        {
            fn eq(&self, other: &Self) -> bool
            {
                let inner = &self.inner;
                let other_inner = &other.inner;
                
                if inner.parent.is_some() && other_inner.parent.is_some()
                {
                    let parent = self.get_parent().unwrap();
                    let other_parent = other.get_parent().unwrap();
                    if parent != other_parent { return false; }
                }
                
                else if !(inner.parent.is_none() && other_inner.parent.is_none()) { return false; }
                
                inner.map == other_inner.map
            }
        }

        fn get_id() -> usize { CUR_ID.fetch_add(1, Ordering::Relaxed) }
    }

    pub mod tuples
    {
        //! A tuple container which can hold elements of different types.
        use ::
        {
            *,
        };
        /*
        use crate::parse::format::Format;
        use crate::types::Type;
        use crate::value::Value;
        use crate::{OverError, OverResult, INDENT_STEP};
        use std::fmt;
        use std::slice::Iter;
        use std::sync::Arc; */

        #[derive(Clone, Debug)]
        struct TupInner
        {
            vec: Vec<Value>,
            inner_tvec: Vec<Type>,
        }
        /// `Tup` struct.
        #[derive(Clone, Debug)]
        pub struct Tup
        {
            inner: Arc<TupInner>,
        }

        impl Tup
        {
            /// Returns a new `Tup` from the given vector of `Value`s.
            pub fn from_vec(values: Vec<Value>) -> Tup
            {
                let tvec: Vec<Type> = values.iter().map(|val| val.get_type()).collect();
                Tup
                {
                    inner: Arc::new(TupInner
                    {
                        vec: values,
                        inner_tvec: tvec,
                    }),
                }
            }
            /// Returns a reference to the inner vec of this `Tup`.
            pub fn vec_ref(&self) -> &Vec<Value> { &self.inner.vec }
            /// Iterates over each `Value` in `self`, applying `Fn` `f`.
            pub fn with_each<F>(&self, mut f: F) where
            F: FnMut(&Value)
            {
                for value in &self.inner.vec
                {
                    f(value)
                }
            }
            /// Gets the value at `index`.
            pub fn get(&self, index: usize) -> OverResult<Value>
            {
                if index >= self.inner.vec.len() { Err(OverError::TupOutOfBounds(index)) }
                else { Ok(self.inner.vec[index].clone()) }
            }
            /// Returns the type vector of this `Tup`.
            pub fn inner_type_vec(&self) -> Vec<Type> { self.inner.inner_tvec.clone() }
            /// Returns the length of this `Tup`.
            pub fn len(&self) -> usize { self.inner.vec.len() }
            /// Returns whether this `Tup` is empty.
            pub fn is_empty(&self) -> bool { self.inner.vec.is_empty() }
            /// Returns whether `self` and `other` point to the same data.
            pub fn ptr_eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.inner, &other.inner) }
            /// Returns an iterator over the Tup.
            pub fn iter(&self) -> Iter<Value> { self.vec_ref().iter() }
        }

        impl Default for Tup
        {
            fn default() -> Self { Self::from_vec(vec![]) }
        }

        impl fmt::Display for Tup
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(true, INDENT_STEP)) }
        }

        impl From<Vec<Value>> for Tup
        {
            fn from(vec: Vec<Value>) -> Self { Self::from_vec(vec) }
        }

        impl PartialEq for Tup
        {
            fn eq(&self, other: &Self) -> bool
            {
                if self.inner.inner_tvec != other.inner.inner_tvec { return false; }

                self.inner.vec == other.inner.vec
            }
        }
    }
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
            if Path::new( x ).exists()
            { paths.push( x.to_string() ); }
        }

        if let Ok( env_path ) = var( "PATH" )
        {
            for x in env_path.split( ":" )
            {
                if !paths.contains( &x.to_string() )
            { paths.push( x.to_string() ); }
            }
        }

        let paths = paths.join( ":" );
        set_var( "PATH", paths );
    }
    // pub fn env_args_to_command_line() -> String
    pub fn args_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = args();
        
        if env_args.len() <= 1 { return result; }

        for ( i, arg ) in env_args.enumerate()
        {
            if i == 0 || arg == "-c" { continue; }

            result.push_str( arg.as_str() );
        }

        result
    }

    pub fn find_file_in_path( filename:&str, exec:bool ) -> String
    {
        let env_path = match var( "PATH" )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println_stderr!( "cicada: error with env PATH: {:?}", e );
                return String::new();
            }
        };

        let vec_path: Vec<&str> = env_path.split( ':' ).collect();
        for p in &vec_path
        {
            match read_dir( p )
            {
                Ok( list ) =>
                {
                    for entry in list.flatten()
                    {
                        if let Ok( name ) = entry.file_name().into_string()
                        {
                            if name != filename { continue; }

                            if exec
                            {
                                let _mode = match entry.metadata()
                                {
                                    Ok( x ) => x,
                                    Err( e ) =>
                                    {
                                        println_stderr!( "cicada: metadata error: {:?}", e );
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

                Err( e ) =>
                {
                    if e.kind() == ErrorKind::NotFound { continue; }

                    log!( "cicada: fs read_dir error: {}: {}", p, e );
                }
            }
        }

        String::new()
    }
}
/// Expansions
pub mod expand
{
    use ::
    {
        regex::{ Regex },
        shell::{ Shell },
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
    /*
    pub fn basename( path:&str ) -> Cow<'_, str> */
    pub fn basename( path:&str ) -> Cow<'_, str>
    {
        let mut pieces = path.rsplit( '/' );
        match pieces.next()
            {
            Some( p ) => p.into(),
            None => path.into(),
        }
    }
    /*
    pub fn expand_home( text:&str ) -> String */
    pub fn house( text:&str ) -> String
    {
        let mut s: String = text.to_string();
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
            if let Ok( x ) = Regex::new( item )
            { re = x; }
            else { return String::new(); }

            let home = tools::get_user_home();
            let ss = s.clone();
            let to = format!( "$head{}$tail", home );
            let result = re.replace_all( ss.as_str(), to.as_str() );
            s = result.to_string();
        }
        s
    }
    /*
    pub fn expand_glob( tokens:&mut types::Tokens )  */
    pub fn glob( tokens:&mut types::Tokens ) 
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for ( sep, text ) in tokens.iter()
        {
            if !sep.is_empty() || !needs_globbing( text )
            {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let item = text.as_str();

            if !item.contains( '*' ) || item.trim().starts_with( '\'' ) || item.trim().starts_with( '"' )
            {
                result.push( item.to_string() );
            }
            
            else
            {
                let _basename = basename( item );
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
                                    let _basename = basename( &file_path );

                                    if _basename == ".." || _basename == "." { continue; }

                                    if _basename.starts_with( '.' ) && !show_hidden {  continue; }

                                    result.push( file_path.to_string() );
                                    is_empty = false;
                                }

                                Err( e ) => { log!( "glob error: {:?}", e ); }
                            }
                        }

                        if is_empty { result.push( item.to_string() ); }
                    }
                    Err( e ) => {
                        println!( "glob error: {:?}", e );
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
                let sep = if token.contains( ' ' )
            { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }

    fn expand_one_env( sh:&Shell, token:&str ) -> String 
    {
        let re1 = Regex::new( r"^( .*? )\$( [A-Za-z0-9_]+|\$|\? )( .* )$" ).unwrap();
        let re2 = Regex::new( r"( .*? )\$\{( [A-Za-z0-9_]+|\$|\? )\}( .* )$" ).unwrap();
        if !re1.is_match( token ) && !re2.is_match( token )
            { return token.to_string(); }

        let mut result = String::new();
        let match_re1 = re1.is_match( token );
        let match_re2 = re2.is_match( token );
        if !match_re1 && !match_re2 {
            return token.to_string();
        }

        let cap_results = if match_re1 {
            re1.captures_iter( token )
        } else {
            re2.captures_iter( token )
        };

        for cap in cap_results {
            let head = cap[1].to_string();
            let tail = cap[3].to_string();
            let key = cap[2].to_string();
            if key == "?" {
                result.push_str( format!( "{}{}", head, sh.previous_status ).as_str() );
            } else if key == "$" {
                unsafe {
                    let val = libc::getpid();
                    result.push_str( format!( "{}{}", head, val ).as_str() );
                }
            } else if let Ok( val ) = env::var( &key )
            {
                result.push_str( format!( "{}{}", head, val ).as_str() );
            } else if let Some( val ) = sh.get_env( &key )
            {
                result.push_str( format!( "{}{}", head, val ).as_str() );
            } else {
                result.push_str( &head );
            }
            result.push_str( &tail );
        }

        result
    }
    /*
    expand_brace( tokens:&mut types::Tokens ) */
    fn brace( tokens:&mut types::Tokens )
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for ( sep, token ) in tokens.iter()
            {
            if !sep.is_empty() || !need_expand_brace( token )
            {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let items = brace_getitem( token, 0 );
            for x in items.0 {
                result.push( x.clone() );
            }
            buff.push( ( idx, result ) );
            idx += 1;
        }

        for ( i, items ) in buff.iter().rev()
            {
            tokens.remove( *i );
            for ( j, token ) in items.iter().enumerate()
            {
                let sep = if token.contains( ' ' )
            { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }
    /*
    expand_brace_range( tokens:&mut types::Tokens ) */
    pub fn brace_range( tokens:&mut types::Tokens )
    {
        let re;
        if let Ok( x ) = Regex::new( r#"\{( -?[0-9]+ )\.\.( -?[0-9]+ )( \.\. )?( [0-9]+ )?\}"# )
            {
            re = x;
        } else {
            println_stderr!( "cicada: re new error" );
            return;
        }

        let mut idx: usize = 0;
        let mut buff: Vec<( usize, Vec<String> )> = Vec::new();
        for ( sep, token ) in tokens.iter()
            {
            if !sep.is_empty() || !re.is_match( token )
            {
                idx += 1;
                continue;
            }
            
            let caps = re.captures( token ).unwrap();

            let start = match caps[1].to_string().parse::<i32>()
            {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "cicada: {}", e );
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>()
            {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "cicada: {}", e );
                    return;
                }
            };
            
            let mut incr = if caps.get( 4 ).is_none()
            {
                1
            } else {
                match caps[4].to_string().parse::<i32>()
            {
                    Ok( x ) => x,
                    Err( e ) => {
                        println_stderr!( "cicada: {}", e );
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
                    result.push( format!( "{}", n ) );
                    n -= incr;
                }
            } else {
                while n <= end {
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
                let sep = if token.contains( ' ' )
            { "\"" } else { "" };
                tokens.insert( *i + j, ( sep.to_string(), token.clone() ) );
            }
        }
    }
    /*
    fn expand_alias( sh:&Shell, tokens:&mut types::Tokens ) */
    pub fn alias( sh:&Shell, tokens:&mut types::Tokens )
    {
        let mut idx: usize = 0;
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

            if !is_head || !sh.is_alias( text )
            {
                idx += 1;
                is_head = false;
                continue;
            }

            if let Some( value ) = sh.get_alias_content( text )
            {
                buff.push( ( idx, value.clone() ) );
            }

            idx += 1;
            is_head = false;
        }

        for ( i, text ) in buff.iter().rev()
            {
            let linfo = parsers::lines::line( text );
            let tokens_ = linfo.tokens;
            tokens.remove( *i );
            for item in tokens_.iter().rev()
            {
                tokens.insert( *i, item.clone() );
            }
        }
    }
    /*
    fn expand_home( tokens:&mut types::Tokens ) */
    pub fn home( tokens:&mut types::Tokens )
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for ( sep, text ) in tokens.iter()
            {
            if !sep.is_empty() || !text.starts_with( "~" )
            {
                idx += 1;
                continue;
            }

            let mut s: String = text.clone();
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
    pub fn expand_env( sh:&Shell, tokens:&mut types::Tokens ) */
    pub fn env( sh:&Shell, tokens:&mut types::Tokens )
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for ( sep, token ) in tokens.iter()
            {
            if sep == "`" || sep == "'" {
                idx += 1;
                continue;
            }

            if !env_in_token( token )
            {
                idx += 1;
                continue;
            }

            let mut _token = token.clone();
            while env_in_token( &_token )
            {
                _token = expand_one_env( sh, &_token );
            }
            buff.push( ( idx, _token ) );
            idx += 1;
        }

        for ( i, text ) in buff.iter().rev()
            {
            tokens[*i].1 = text.to_string();
        }
    }
    /*
    pub fn extend_bangbang( sh:&shell::Shell, line:&mut String ) */
    pub fn bangbang( sh:&shell::Shell, line:&mut String )
    {
        if !re_contains( line, r"!!" )
            {
            return;
        }
        if sh.previous_cmd.is_empty()
            {
            return;
        }

        let re = Regex::new( r"!!" ).unwrap();
        let mut replaced = false;
        let mut new_line = String::new();
        let linfo = parsers::line::parse( line );
        for ( sep, token ) in linfo.tokens {
            if !sep.is_empty()
            {
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

            if !sep.is_empty()
            {
                new_line.push_str( &sep );
            }
            new_line.push( ' ' );
        }

        *line = new_line.trim_end().to_string();
        // print full line after extending
        if replaced {
            println!( "{}", line );
        }
    }
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

            pub fn with_description<F, T>( err: Errno, callback: F ) -> T where
            F: FnOnce( Result<&str, Errno> ) -> T,
            {
                let mut buf = [0u8; 1024];
                let c_str = unsafe {
                    let rc = strerror_r( err.0, buf.as_mut_ptr() as *mut _, buf.len() as size_t );
                    if rc != 0
                    {
                        let fm_err = match rc < 0 {
                            true => errno(),
                            false => Errno( rc ),
                        };
                        if fm_err != Errno( libc::ERANGE )
            {
                            return callback( Err( fm_err ) );
                        }
                    }
                    let c_str_len = strlen( buf.as_ptr() as *const _ );
                    &buf[..c_str_len]
                };
                callback( Ok( from_utf8_lossy( c_str ) ) )
            }
            
            pub fn errno() -> Errno { unsafe { Errno( *errno_location() ) } }

            pub fn set_errno( Errno( errno ): Errno )
            { unsafe { *errno_location() = errno; } }
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

            pub const STRERROR_NAME:&str = "FormatMessageW";
            pub const FORMAT_MESSAGE_FROM_SYSTEM: FORMAT_MESSAGE_OPTIONS = FORMAT_MESSAGE_OPTIONS( 4096u32 );
            pub const FORMAT_MESSAGE_IGNORE_INSERTS: FORMAT_MESSAGE_OPTIONS = FORMAT_MESSAGE_OPTIONS( 512u32 );
            
            pub type WIN32_ERROR = u32;
            
            pub type PWSTR = *mut u16;

            #[repr( transparent )] #[derive( Clone, Copy, Debug, Default, Eq, PartialEq )]
            pub struct FORMAT_MESSAGE_OPTIONS( pub u32 );
            
            #[inline] pub unsafe fn GetLastError() -> WIN32_ERROR { 0 }
            #[inline] pub unsafe fn SetLastError( dwerrcode:WIN32_ERROR )
            { }
            #[inline] pub unsafe fn FormatMessageW
            ( 
                dwflags: FORMAT_MESSAGE_OPTIONS, 
                lpsource:Option<*const ::ffi::c_void>, 
                dwmessageid: u32, 
                dwlanguageid: u32, 
                lpbuffer:PWSTR, 
                nsize: u32, 
                arguments:Option<*const *const i8>
           ) -> u32
            {
                0
            }

            fn from_utf16_lossy<'a>( input:&[u16], output:&'a mut [u8] ) -> &'a str
            {
                let mut output_len = 0;
                for c in char::decode_utf16( input.iter().copied().take_while( |&x| x != 0 ) )
                    .map( |x| x.unwrap_or( REPLACEMENT_CHARACTER ) )
                {
                    let c_len = c.len_utf8();
                    if c_len > output.len() - output_len {
                        break;
                    }
                    c.encode_utf8( &mut output[output_len..] );
                    output_len += c_len;
                }
                unsafe { str::from_utf8_unchecked( &output[..output_len] ) }
            }

            pub fn with_description<F, T>( err: Errno, callback: F ) -> T where
            F: FnOnce( Result<&str, Errno> ) -> T
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
                        return callback( Err( fm_err ) );
                    }

                    let mut msg = [0u8; 2048];
                    let msg = from_utf16_lossy( &buf[..res as usize], &mut msg[..] );
                    callback( Ok( msg.trim_end() ) )
                }
            }
            
            pub fn errno() -> Errno { unsafe { Errno( GetLastError() as i32 ) } }

            pub fn set_errno( Errno( errno ): Errno )
            { unsafe { SetLastError( errno as WIN32_ERROR ) } }
        }
    }
    #[cfg( unix )] pub use self::imply::unix as sys;
    #[cfg( windows )] pub use self::imply::windows as sys;
    /// OVER Error Handling
    pub mod over
    {
        //! Error module.
        use ::
        {
            *,
        };
        /*
        use crate::parse::error::ParseError;
        use crate::types::Type;
        use std::error::Error;
        use std::fmt;
        use std::io;
        */

        pub mod parse
        {
            //! Module for parse errors.
            use ::
            {
                *,
            };
            /*
            #![allow(missing_docs)]

            use super::misc::format_char;
            use super::ParseResult;
            use super::MAX_DEPTH;
            use crate::types::Type;
            use crate::OverError;
            use num_bigint::{BigInt, ParseBigIntError};
            use std::error::Error;
            use std::fmt;
            use std::io;
            use std::num::ParseIntError;
            */

            pub fn parse_err<T>(file: Option<String>, kind: ParseErrorKind) -> ParseResult<T> {
                Err(ParseError { file, kind })
            }

            /// Error kind.
            #[derive(Debug)]
            pub enum ParseErrorKind {
                BinaryOperatorError(Type, Type, char, usize, usize),
                CyclicInclude(String, usize, usize),
                DuplicateField(String, usize, usize),
                DuplicateGlobal(String, usize, usize),
                ExpectedType(Type, Type, usize, usize),
                GlobalNotFound(String, usize, usize),
                InvalidIndex(BigInt, usize, usize),
                InvalidClosingBracket(Option<char>, char, usize, usize),
                InvalidDot(Type, usize, usize),
                InvalidEscapeChar(char, usize, usize),
                InvalidFieldChar(char, usize, usize),
                InvalidFieldName(String, usize, usize),
                InvalidIncludeChar(char, usize, usize),
                InvalidIncludePath(String, usize, usize),
                InvalidIncludeToken(Type, usize, usize),
                InvalidNumeric(usize, usize),
                InvalidValue(String, usize, usize),
                InvalidValueChar(char, usize, usize),
                MaxDepth(usize, usize),
                UnaryOperatorError(Type, char, usize, usize),
                UnexpectedEnd(usize),
                VariableNotFound(String, usize, usize),

                IoError(String),
                OverError(String),
                ParseIntError(String),
            }

            /// Parse error.
            #[derive(Debug)]
            pub struct ParseError {
                /// The file this error occurred in.
                pub file: Option<String>,
                /// Error kind.
                pub kind: ParseErrorKind,
            }

            impl fmt::Display for ParseError {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    use self::ParseErrorKind::*;

                    if let Some(ref file) = (*self).file {
                        write!(f, "{}: ", file)?;
                    }

                    match (*self).kind {
                        BinaryOperatorError(ref expected, ref found, ref op, ref line, ref col) => write!(
                            f,
                            "Could not apply operator {} on types {} and {} at line {}, column {}",
                            op, expected, found, line, col,
                        ),
                        CyclicInclude(ref file, ref line, ref col) => write!(
                            f,
                            "Tried to cyclically include file \"{}\" at line {}, column {}",
                            file, line, col
                        ),
                        DuplicateField(ref field, ref line, ref col) => write!(
                            f,
                            "Duplicate field \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        DuplicateGlobal(ref field, ref line, ref col) => write!(
                            f,
                            "Duplicate global \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        ExpectedType(ref expected, ref found, ref line, ref col) => write!(
                            f,
                            "Expected {} at line {}, column {}; found {}",
                            expected, line, col, found
                        ),
                        GlobalNotFound(ref var, ref line, ref col) => write!(
                            f,
                            "Global \"{}\" at line {}, column {} could not be found",
                            var, line, col
                        ),
                        InvalidClosingBracket(ref expected, ref found, ref line, ref col) => write!(
                            f,
                            "Invalid closing bracket '{}' at line {}, column {}; expected {}",
                            found,
                            line,
                            col,
                            match *expected {
                                Some(ch) => format!("'{}'", ch),
                                None => String::from("none"),
                            }
                        ),
                        InvalidDot(ref t, ref line, ref col) => write!(
                            f,
                            "Invalid use of dot notation on value of type {} at line {}, column {}; \
                            value must be an Obj, Arr, or Tup.",
                            t, line, col
                        ),
                        InvalidEscapeChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid escape character '\\{}' at line {}, column {}. \
                            If you meant to write a backslash, use '\\\\'",
                            format_char(*ch),
                            line,
                            col
                        ),
                        InvalidFieldChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid character '{}' for field at line {}, column {}",
                            format_char(*ch),
                            line,
                            col
                        ),
                        InvalidFieldName(ref field, ref line, ref col) => write!(
                            f,
                            "Invalid field name \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        InvalidIncludeChar(ref found, ref line, ref col) => write!(
                            f,
                            "Invalid include token character \'{}\' at line {}, column {}",
                            found, line, col
                        ),
                        InvalidIncludePath(ref path, ref line, ref col) => write!(
                            f,
                            "Invalid include path \"{}\" at line {}, column {}",
                            path, line, col
                        ),
                        InvalidIncludeToken(ref t, ref line, ref col) => write!(
                            f,
                            "Invalid value of type \"{}\" at line {}, column {}; \
                            must be either a Str value or one of the tokens \
                            \"Obj\", \"Arr\", \"Tup\", or \"Str\"",
                            t, line, col
                        ),
                        InvalidIndex(ref index, ref line, ref col) => write!(
                            f,
                            "Invalid index {} at line {}, column {}",
                            index, line, col
                        ),
                        InvalidNumeric(ref line, ref col) => {
                            write!(f, "Invalid numeric value at line {}, column {}", line, col)
                        }
                        InvalidValue(ref value, ref line, ref col) => write!(
                            f,
                            "Invalid value \"{}\" at line {}, column {}",
                            value, line, col
                        ),
                        InvalidValueChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid character '{}' for value at line {}, column {}",
                            format_char(*ch),
                            line,
                            col
                        ),
                        MaxDepth(ref line, ref col) => write!(
                            f,
                            "Exceeded maximum recursion depth ({}) at line {}, column {}",
                            MAX_DEPTH, line, col
                        ),
                        UnaryOperatorError(ref found, ref op, ref line, ref col) => write!(
                            f,
                            "Could not apply operator {} on type {} at line {}, column {}",
                            op, found, line, col,
                        ),
                        UnexpectedEnd(ref line) => write!(f, "Unexpected end at line {}", line,),
                        VariableNotFound(ref var, ref line, ref col) => write!(
                            f,
                            "Variable \"{}\" at line {}, column {} could not be found",
                            var, line, col
                        ),

                        IoError(ref error) | OverError(ref error) | ParseIntError(ref error) => {
                            write!(f, "{}", error)
                        }
                    }
                }
            }

            impl Error for ParseError {
                fn description(&self) -> &str {
                    use self::ParseErrorKind::*;

                    match (*self).kind {
                        BinaryOperatorError(_, _, _, _, _) | UnaryOperatorError(_, _, _, _) => {
                            "Could not apply operator"
                        }

                        CyclicInclude(_, _, _) => "Tried to cyclically include file",
                        DuplicateField(_, _, _) => "Duplicate field",
                        DuplicateGlobal(_, _, _) => "Duplicate global",
                        ExpectedType(_, _, _, _) => "Expected different type",
                        GlobalNotFound(_, _, _) => "Global could not be found",
                        InvalidClosingBracket(_, _, _, _) => "Invalid closing bracket",
                        InvalidDot(_, _, _) => "Invalid use of dot notation",
                        InvalidEscapeChar(_, _, _) => "Invalid escape character",
                        InvalidFieldChar(_, _, _) => "Invalid character for field",
                        InvalidFieldName(_, _, _) => "Invalid field name",
                        InvalidIncludeChar(_, _, _) => "Invalid include character",
                        InvalidIncludePath(_, _, _) => "Invalid include path",
                        InvalidIncludeToken(_, _, _) => "Invalid include token",
                        InvalidIndex(_, _, _) => "Invalid index",
                        InvalidNumeric(_, _) => "Invalid numeric value",
                        InvalidValue(_, _, _) => "Invalid value",
                        InvalidValueChar(_, _, _) => "Invalid character for value",
                        MaxDepth(_, _) => "Exceeded maximum depth for a container",
                        UnexpectedEnd(_) => "Unexpected end when reading value",
                        VariableNotFound(_, _, _) => "Variable could not be found",

                        IoError(ref error) | OverError(ref error) | ParseIntError(ref error) => error,
                    }
                }
            }

            impl ParseError {
                /// Convert an `OverError` to a `ParseError` given line and column numbers.
                pub fn from_over(e: &OverError, file: Option<String>, line: usize, col: usize) -> Self {
                    ParseError {
                        file,
                        kind: ParseErrorKind::OverError(format!("{} at line {}, col {}", e, line, col)),
                    }
                }
            }

            impl From<io::Error> for ParseError {
                fn from(e: io::Error) -> Self {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::IoError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseIntError> for ParseError {
                fn from(e: ParseIntError) -> Self {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseBigIntError> for ParseError {
                fn from(e: ParseBigIntError) -> Self {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }
        }
        use self::parse::ParseError;
        /// Result type for this crate.
        pub type OverResult<T> = Result<T, OverError>;
        /// The fabulous OVER error type.
        #[derive(Debug, PartialEq, Eq)]
        pub enum OverError {
            ArrOutOfBounds(usize),
            ArrTypeMismatch(Type, Type),
            FieldNotFound(String),
            InvalidFieldName(String),
            NoParentFound,
            ParseError(String),
            TupOutOfBounds(usize),
            TupTypeMismatch(Type, Type, usize),
            TypeMismatch(Type, Type),

            IoError(String),
        }

        impl fmt::Display for OverError {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                use self::OverError::*;

                match *self {
                    ArrOutOfBounds(ref index) => write!(f, "Arr index {} out of bounds", index),
                    ArrTypeMismatch(ref expected, ref found) => write!(
                        f,
                        "Arr inner types do not match: expected {}, found {}",
                        expected, found
                    ),
                    FieldNotFound(ref field) => write!(f, "Field not found: \"{}\"", field),
                    InvalidFieldName(ref field) => write!(f, "Invalid field name: \"{}\"", field),
                    NoParentFound => write!(f, "No parent found for this obj"),
                    TupOutOfBounds(ref index) => write!(f, "Tup index {} out of bounds", index),
                    TupTypeMismatch(ref expected, ref found, ref index) => write!(
                        f,
                        "Tup inner types do not match at index {}: expected {}, found {}",
                        index, expected, found
                    ),
                    TypeMismatch(ref expected, ref found) => {
                        write!(f, "Type mismatch: expected {}, found {}", expected, found)
                    }

                    ParseError(ref error) | IoError(ref error) => write!(f, "{}", error),
                }
            }
        }

        impl Error for OverError {
            fn description(&self) -> &str {
                use self::OverError::*;

                match *self {
                    ArrOutOfBounds(_) => "Arr index out of bounds",
                    ArrTypeMismatch(_, _) => "Arr inner types do not match",
                    FieldNotFound(_) => "Field not found",
                    InvalidFieldName(_) => "Invalid field name",
                    NoParentFound => "No parent found for this obj",
                    TupOutOfBounds(_) => "Tup index out of bounds",
                    TupTypeMismatch(_, _, _) => "Tup inner types do not match",
                    TypeMismatch(_, _) => "Type mismatch",

                    ParseError(ref error) | IoError(ref error) => error,
                }
            }
        }

        impl From<io::Error> for OverError {
            fn from(e: io::Error) -> Self {
                OverError::IoError(format!("{}", e))
            }
        }

        impl From<ParseError> for OverError {
            fn from(e: ParseError) -> Self {
                OverError::ParseError(format!("{}", e))
            }
        }
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
                Err( fm_err ) => write!( 
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
        fn from( e: Errno ) -> Self { e.0 }
    }
    
    impl Error for Errno
    {
        #[allow( deprecated )] fn description( &self ) -> &str { "system error" }
    }
    
    impl From<Errno> for io::Error
    {
        fn from( errno: Errno ) -> Self { io::Error::from_raw_os_error( errno.0 ) }
    }
    /// Returns the platform-specific value of `errno`.
    pub fn errno() -> Errno { sys::errno() }
    /// Sets the platform-specific value of `errno`.
    pub fn set_errno( err: Errno ) { sys::set_errno( err ) }
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
        fn execute( &self, prompter:&mut Prompter<Term>, count: i32, ch: char ) -> io::Result<()>;
        /// Returns the command category.
        fn category( &self ) -> Category { Category::Other }
    }

    impl<F, Term: Terminal> Function<Term> for F where
    F: Send + Sync,
    F: Fn( &mut Prompter<Term>, i32, char ) -> io::Result<()>
    {
        fn execute( &self, prompter:&mut Prompter<Term>, count: i32, ch: char ) -> io::Result<()>
        {
            self( prompter, count, ch )
        }
    }

    pub struct EnterFunction;

    impl<T: Terminals> Function<T> for EnterFunction
    {
        fn execute( &self, prompter:&mut Prompter<T>, count: i32, _ch: char ) -> io::Result<()>
        {
            let buf = prompter.buffer();
            let linfo = parsers::line::parse( buf );
            
            if linfo.is_complete { prompter.accept_input() }
            else if count > 0
            {
                match prompter.insert( count as usize, '\n' )
                {
                    Ok( _ ) => {},
                    Err( e ) => { println!( "sub-prompt error: {}", e ); }
                }

                prompter.insert_str( ">> " )
            }
            else { Ok( () ) }
        }
    }
}
/// Utilities for formatting and printing Strings.
pub mod fmt
{
    pub use std::fmt::{ * };
    use ::
    {
        cmp::{ min },
    };
    /*
    pub fn format_columns<S: AsRef<str>>( strs:&[S], screen_width:usize, horizontal:bool ) -> Option<Vec<usize>> */
    /// Formats a series of strings into columns, fitting within a given screen width.
    pub fn columns<S: AsRef<str>>( strs:&[S], screen_width:usize, horizontal:bool ) -> Option<Vec<usize>>
    {
        if strs.is_empty()
            { return None; }

        let n_strs = strs.len();
        let ( mut min_len, mut max_len ) = min_max( strs.iter().map( |s| s.as_ref().chars().count() ) );

        if min_len == 0 { min_len = 1; }
        if max_len == 0 { max_len = 1; }

        let mut min_cols = min( n_strs, screen_width / max_len );
        let max_cols = min( n_strs, screen_width / min_len );

        if min_cols <= 1 { min_cols = 2; }

        if max_cols <= 1 { return None; }

        let mut col_sizes = if min_cols == max_cols { vec![vec![0; max_cols]] }
        else
        {
            ( min_cols..max_cols + 1 ).map( |n| vec![0; n] ).collect::<Vec<_>>()
        };

        for ( i, s ) in strs.iter().enumerate()
        {
            let len = s.as_ref().chars().count();

            for cols in &mut col_sizes
            {
                let n_cols = cols.len();
                let col = if horizontal
                {
                    i % n_cols
                }
                else
                {
                    let per_col = ( n_strs + ( n_cols - 1 ) ) / n_cols;
                    i / per_col
                };

                let real_len = if col == n_cols - 1 { len } else { len + COL_SPACE };

                if real_len > cols[col] { cols[col] = real_len; }
            }
        }

        for cols in col_sizes.into_iter().rev()
        {
            if cols.iter().fold( 0, |a, b| a + b ) <= screen_width { return Some( cols ); }
        }

        None
    }
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
        let rc_file = format!( "{}/cicadarc", dir_config );
        
        if Path::new( &rc_file ).exists()
            { return rc_file; }
        
        let home = tools::get_user_home();
        let rc_file_home = format!( "{}/{}", home, ".cicadarc" );
        
        if Path::new( &rc_file_home ).exists()
            { return rc_file_home; }
        
        rc_file
    }

    pub fn load_rc_files( sh:&mut shell::Shell )
    {
        let rc_file = get_rc_file();

        if !Path::new( &rc_file ).exists()
            { return; }

        let args = vec!["source".to_string(), rc_file];
        scripts::run( sh, &args );
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
        borrow::{ Cow },
        env::{ var },
        ops::{Range, RangeFrom, RangeFull, RangeTo},
        str::{from_utf8, from_utf8_unchecked},
        *,
    };
    /*
    pub fn getpid() -> i32 */
    pub fn pid() -> i32 { unsafe { libc::getpid() } }
    /*
    pub fn get_user_name() -> String */
    pub fn username() -> String
    {
        match var( "USER" )
        {
            Ok( x ) => { return x; }
            Err( e ) => { log!( "cicada: env USER error: {}", e ); }
        }

        let cmd_result = now::run( "whoami" );
        return cmd_result.stdout.trim().to_string();
    }
    /*
    pub fn get_user_home() -> String */
    pub fn user_home() -> String
    {
        match env::var( "HOME" )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println_stderr!( "cicada: env HOME error: {}", e );
                String::new()
            }
        }
    }
    /*
    fn brace_getgroup( ... ) -> Option<( Vec<String>, String )> */
    pub fn braces( s:&str, depth: i32 ) -> Option<( Vec<String>, String )>
    {
        let mut out: Vec<String> = Vec::new();
        let mut comma = false;
        let mut ss = s.to_string();

        while !ss.is_empty()
        {
            let ( g, sss ) = braced( ss.as_str(), depth );
            ss = sss.clone();
            if ss.is_empty()
            { break; }

            for x in g.iter()
            {
                out.push( x.clone() );
            }

            let c = match ss.chars().next()
            {
                Some( x ) => x,
                None =>
                {
                    break;
                }
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
    /*
    fn brace_getitem( ... ) -> ( Vec<String>, String ) */
    fn braced( s:&str, depth: i32 ) -> ( Vec<String>, String )
    {
        let mut out: Vec<String> = vec![String::new()];
        let mut ss = s.to_string();
        let mut tmp;
        
        while !ss.is_empty()
        {
            let c = match ss.chars().next()
            {
                Some( x ) => x,
                None => { return ( out, ss ); }
            };

            if depth > 0 && ( c == ',' || c == '}' )
            { return ( out, ss ); }

            if c == '{'
            {
                let mut sss = ss.clone();
                sss.remove( 0 );
                let result_groups = braces( &sss, depth + 1 );
                
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
    
    pub fn backward_word( n:usize, buf:&str, cur:usize, word_break:&str ) -> usize
    {
        let mut chars = buf[..cur].char_indices().rev();

        for _ in 0..n
        {
            drop_while( &mut chars, |( _, ch )| word_break.contains( ch ) );
            if chars.clone().next().is_none()
            { break; }
            drop_while( &mut chars, |( _, ch )| !word_break.contains( ch ) );
            if chars.clone().next().is_none()
            { break; }
        }

        match chars.next()
        {
            Some( ( ind, ch ) ) => ind + ch.len_utf8(),
            None => 0
        }
    }

    pub fn forward_word( n:usize, buf:&str, cur:usize, word_break:&str ) -> usize
    {
        let mut chars = buf[cur..].char_indices();

        for _ in 0..n
        {
            drop_while( &mut chars, |( _, ch )| word_break.contains( ch ) );
            if chars.clone().next().is_none()
            { break; }
            drop_while( &mut chars, |( _, ch )| !word_break.contains( ch ) );
            if chars.clone().next().is_none()
            { break; }
        }

        match chars.next()
        {
            Some( ( ind, _ ) ) => cur + ind,
            None => buf.len()
        }
    }

    pub fn back_n_words( n:usize, buf:&str, cur:usize, word_break:&str ) -> Range<usize>
    {
        let prev = backward_word( 1, buf, cur, word_break );
        let end = word_end( &buf, prev, word_break );

        if n > 1
        {
            let start = backward_word( n - 1, buf, prev, word_break );
            start..end
        }

        else { prev..end }
    }

    pub fn forward_n_words( n:usize, buf:&str, cur:usize, word_break:&str ) -> Range<usize>
    {
        let start = next_word( 1, buf, cur, word_break );

        if n > 1
        {
            let last = next_word( n - 1, buf, start, word_break );
            let end = word_end( buf, last, word_break );
            start..end
        }
        
        else
        {
            let end = word_end( buf, start, word_break );
            start..end
        }
    }
    /// Returns the longest common prefix of a set of strings.
    pub fn longest_common_prefix<'a, I, S>( iter: I ) -> Option<&'a str> where
    I: IntoIterator<Item=&'a S>,
    S: 'a + ?Sized + AsRef<str>
    {
        let mut iter = iter.into_iter();
        let mut pfx = iter.next()?.as_ref();

        for s in iter
        {
            let s = s.as_ref();

            let n = pfx.chars().zip( s.chars() )
            .take_while( |&( a, b )| a == b )
            .map( |( ch, _ )| ch.len_utf8() ).sum();

            if n == 0 { return None; }
            else { pfx = &pfx[..n]; }
        }

        Some( pfx )
    }

    pub fn word_start( buf:&str, cur:usize, word_break:&str ) -> usize
    {
        let fwd = match buf[cur..].chars().next()
        {
            Some( ch ) => word_break.contains( ch ),
            None => return buf.len()
        };

        if fwd
        {
            next_word( 1, buf, cur, word_break )
        }
        
        else
        {
            let mut chars = buf[..cur].char_indices().rev();
            drop_while( &mut chars, |( _, ch )| !word_break.contains( ch ) );

            match chars.next()
            {
                Some( ( idx, ch ) ) => idx + ch.len_utf8(),
                None => 0
            }
        }
    }

    pub fn word_end( buf:&str, cur:usize, word_break:&str ) -> usize
    {
        let mut chars = buf[cur..].char_indices();
        drop_while( &mut chars, |( _, ch )| !word_break.contains( ch ) );

        match chars.next()
        {
            Some( ( idx, _ ) ) => cur + idx,
            None => buf.len()
        }
    }
    /*
    pub fn get_escape_char(ch: char) -> Option<char> */
    /// If `ch` preceded by a backslash together form an escape character, then return this char.
    pub fn escape_char(ch: char) -> Option<char>
    {
        match ch
        {
            '\\' => Some('\\'),
            '"' => Some('"'),
            '\'' => Some('\''),
            '$' => Some('$'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            _ => None,
        }
    }
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
/// Input History
pub mod history
{
    use ::
    {
        collections::{ HashMap },
        terminal::{ DefaultTerminal, Interface },
        io::{ Write },
        path::{ Path },
        *,
    };
    /*
    use rusqlite::Connection as Conn;
    use rusqlite::Error::SqliteFailure;

    use crate::shell;
    use crate::tools;
    */
    fn init_db( hfile:&str, htable:&str )
    {
        let path = Path::new( hfile );
        if !path.exists()
            {
            let _parent = match path.parent()
            {
                Some( x ) => x,
                None => {
                    println_stderr!( "cicada: history init - no parent found" );
                    return;
                }
            };
            let parent = match _parent.to_str()
            {
                Some( x ) => x,
                None => {
                    println_stderr!( "cicada: parent to_str is None" );
                    return;
                }
            };
            match fs::create_dir_all( parent )
            {
                Ok( _ ) => {}
                Err( e ) => {
                    println_stderr!( "cicada: histdir create error: {}", e );
                    return;
                }
            }
            match fs::File::create( hfile )
            {
                Ok( _ ) => {
                    println!( "cicada: created history file: {}", hfile );
                }
                Err( e ) => {
                    println_stderr!( "cicada: history: file create failed: {}", e );
                }
            }
        }

        let conn = match Connection::open( hfile )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: history: open db error: {}", e );
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
        match conn.execute( &sql, [] )
            {
            Ok( _ ) => {}
            Err( e ) => println_stderr!( "cicada: history: query error: {}", e ),
        }
    }

    pub fn init( rl:&mut Interface<DefaultTerminal> )
            {
        let mut hist_size: usize = 99999;
        if let Ok( x ) = env::var( "HISTORY_SIZE" )
            {
            if let Ok( y ) = x.parse::<usize>()
            {
                hist_size = y;
            }
        }
        rl.set_history_size( hist_size );

        let history_table = get_history_table();
        let hfile = get_history_file();

        if !Path::new( &hfile ).exists()
            {
            init_db( &hfile, &history_table );
        }

        let mut delete_dups = true;
        if let Ok( x ) = env::var( "HISTORY_DELETE_DUPS" )
            {
            if x == "0" {
                delete_dups = false;
            }
        }
        if delete_dups {
            delete_duplicated_histories();
        }

        let conn = match Connection::open( &hfile )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: history: conn error: {}", e );
                return;
            }
        };
        let sql = format!( "SELECT inp FROM {} ORDER BY tsb;", history_table );
        let mut stmt = match conn.prepare( &sql )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: prepare select error: {}", e );
                return;
            }
        };

        let rows = match stmt.query_map( [], |row| row.get( 0 ) )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: query select error: {}", e );
                return;
            }
        };

        let mut dict_helper: HashMap<String, bool> = HashMap::new();
        for x in rows.flatten()
            {
            let inp: String = x;
            if dict_helper.contains_key( &inp )
            {
                continue;
            }
            dict_helper.insert( inp.clone(), true );
            rl.add_history( inp.trim().to_string() );
        }
    }

    pub fn get_history_file() -> String {
        if let Ok( hfile ) = env::var( "HISTORY_FILE" )
            {
            hfile
        } else if let Ok( d ) = env::var( "XDG_DATA_HOME" )
            {
            format!( "{}/{}", d, "cicada/history.sqlite" )
        } else {
            let home = get::user_home();
            format!( "{}/{}", home, ".local/share/cicada/history.sqlite" )
        }
    }

    pub fn get_history_table() -> String {
        if let Ok( hfile ) = env::var( "HISTORY_TABLE" )
            {
            hfile
        } else {
            String::from( "cicada_history" )
        }
    }

    fn delete_duplicated_histories()
            {
        let hfile = get_history_file();
        let history_table = get_history_table();
        let conn = match Connection::open( &hfile )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: history: conn error: {}", e );
                return;
            }
        };
        let sql = format!( 
            "DELETE FROM {} WHERE rowid NOT IN ( 
            SELECT MAX( rowid ) FROM {} GROUP BY inp )",
            history_table, history_table
       );
        match conn.execute( &sql, [] )
            {
            Ok( _ ) => {}
            Err( e ) => match e {
                SqliteFailure( ee, msg ) => {
                    if ee.extended_code == 5 {
                        log!( 
                            "failed to delete dup histories: {}",
                            msg.unwrap_or( "db is locked?".to_owned() ),
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
                    println_stderr!( "cicada: history: delete dup error: {}", e );
                }
            },
        }
    }

    pub fn add_raw( sh:&shell::Shell, line:&str, status: i32,
                tsb: f64, tse: f64 )
            {
        let hfile = get_history_file();
        let history_table = get_history_table();
        if !Path::new( &hfile ).exists()
            {
            init_db( &hfile, &history_table );
        }

        let conn = match Connection::open( &hfile )
            {
            Ok( x ) => x,
            Err( e ) => {
                println_stderr!( "cicada: history: conn error: {}", e );
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
        match conn.execute( &sql, [] )
            {
            Ok( _ ) => {}
            Err( e ) => println_stderr!( "cicada: history: save error: {}", e ),
        }
    }

    pub fn add( sh:&shell::Shell, rl:&mut Interface<DefaultTerminal>, line:&str,
            status: i32, tsb: f64, tse: f64 )
            {
        add_raw( sh, line, status, tsb, tse );
        rl.add_history( line.to_string() );
    }
}
/// State Verification
pub mod is
{
    use ::
    {
        env::{ var },
        regex::{ contains, Regex },
        *,
    };
    /*
    is_command_string( ... ) -> bool */
    pub fn command_string( args:&[String] ) -> bool { args.len() > 1 && args[1] == "-c" }
    /*
    is_login( ... ) -> bool */
    pub fn login( args:&[String] ) -> bool
    {
        if !args.is_empty() && args[0].starts_with( "-" )
        { return true; }

        if args.len() > 1 && ( args[1] == "--login" || args[1] == "-l" )
        { return true; }

        if let Ok( term_program ) = var( "TERM_PROGRAM" )
        { if term_program == "vscode" { return true; } }

        false
    }
    /*
    is_non_tty( ... ) -> bool */
    pub fn non_tty() -> bool { unsafe { libc::isatty( 0 ) == 0 } }
    /*
    is_shell_altering_command( ... ) -> bool */
    pub fn shell_altering_command( line:&str ) -> bool
    {
        let line = line.trim();
        
        if contains( line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$" )
            { return true; }
        
        line.starts_with( "alias " )
        || line.starts_with( "export " )
        || line.starts_with( "unalias " )
        || line.starts_with( "unset " )
        || line.starts_with( "source " )
    }
    /*
    is_script( ... ) -> bool */
    pub fn script( args:&[String] ) -> bool { args.len() > 1 && !args[1].starts_with( "-" ) }
    /*
    is_signal_handler_enabled( ... ) -> bool */
    pub fn signal_handler_enabled() -> bool { var( "CICADA_ENABLE_SIG_HANDLER" ).map_or( false, |x| x == "1" ) }
    /*
    is_prefix_char( ... ) -> bool */
    fn prefix_char( c: char ) -> bool { c == '[' || c == '{' }
    /*
    is_suffix_char( ... ) -> bool */
    fn suffix_char( c: char ) -> bool { c == ']' || c == '}' }
    /*
    is_prompt_item_char( ... ) -> bool */
    fn prompt_item_char( c: char, token:&str ) -> bool 
    {
        let s = c.to_string();

        if token.is_empty()
            { contains( &s, r#"^[a-zA-Z_]$"# ) } 
        else { contains( &s, r#"^[a-zA-Z0-9_]$"# ) }
    }
    /*
    needs_globbing( ... ) -> bool */
    fn globable( line:&str ) -> bool 
    {
        let re = Regex::new( r"\*+" ).expect( "Invalid regex ptn" );
        re.is_match( line )
    }
    /*
    should_do_dollar_command_extension( ... ) -> bool */
    fn dollar_command_extensible( line:&str ) -> bool 
    { contains( line, r"\$\( [^\ )]+\ )" ) && !contains( line, r"='.*\$\( [^\ )]+\ ).*'$" ) }
    /*
    need_expand_brace( ... ) -> bool */
    fn brace_expandable( line:&str ) -> bool { contains( line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"# ) }
    /*
    is_digit( ... ) -> bool */
    /// Returns true if `ch` is an ASCII decimal digit.
    pub fn digit( ch: char ) -> bool
    {
        match ch
        {
            '0'...'9' => true,
            _ => false,
        }
    }
    /*
    is_value_end_char( ... ) -> bool */
    /// Returns true if this character signifies the legal end of a value.
    pub fn value_end_char( ch: char ) -> bool { is_whitespace( ch ) || is_end_delimiter( ch ) || is_operator( ch ) }
    /*
    is_whitespace( ... ) -> bool */
    /// Returns true if the character is either whitespace or '#' ( start of a comment ).
    pub fn whitespace( ch: char ) -> bool { ch.is_whitespace() || ch == '#' }
    /*
    is_end_delimiter( ... ) -> bool */
    pub fn end_delimiter( ch: char ) -> bool
    {
        match ch
        {
            ')' | ']' | '}' | '>' => true,
            _ => false,
        }
    }
    /*
    is_numeric_char( ... ) -> bool */
    pub fn numeric_char( ch: char ) -> bool
    {
        match ch
        {
            _ch if is_digit( _ch ) => true,
            '.' | ',' => true,
            _ => false,
        }
    }
    /*
    is_priority_operator( ... ) -> bool */
    pub fn priority_operator( ch: char ) -> bool
    {
        match ch
        {
            '*' | '/' | '%' => true,
            _ => false,
        }
    }
    /*
    is_operator( ... ) -> bool */
    pub fn operator( ch: char ) -> bool
    {
        match ch
        {
            '+' | '-' | '*' | '/' | '%' => true,
            _ => false,
        }
    }
    /*
    is_reserved( ... ) -> bool */
    pub fn reserved( field: &str ) -> bool
    {
        match field
        {
            "@" | "null" | "true" | "false" | "Obj" | "Str" | "Arr" | "Tup" => true,
            _ => false,
        }
    }
}
/// Traits, helpers, and type definitions for core I/O functionality.
pub mod io
{
    pub use std::io::{ * };

    pub mod reader
    {
        //! Provides access to terminal read operations
        use ::
        {
            borrow::{ Cow },
            collections::{ HashMap, SequenceMap, VecDeque },
            command::{ Category, Command },
            ffi::{ Function },
            mem::{ replace },
            ops::{ Deref, DerefMut },
            path::{ Path, PathBuf },
            signals::{ Signal },
            sync::{ Arc, MutexGuard },
            terminal::
            { 
                Interface,
                size::Size, 
                Terminals,
            },
            time::{ Duration, Instant },
            *,
        };
        /*
        use crate::complete::{Completer, Completion, DummyCompleter};
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
        pub const STRING_CHARS:&str = "\"'";
        /// Default set of word break characters
        pub const WORD_BREAK_CHARS:&str = " \t\n\"\\'`@$><=;|&{( ";
        /// Indicates the start of a series of invisible characters in the prompt
        pub const START_INVISIBLE: char = '\x01';
        /// Indicates the end of a series of invisible characters in the prompt
        pub const END_INVISIBLE: char = '\x02';
        /// Maximum size of kill ring
        const MAX_KILLS: usize = 10;
        /// Represents the result of a `Terminal` read operation
        pub enum RawRead
        {
            /// `n` bytes were read from the device
            Bytes( usize ),
            /// The terminal window was resized
            Resize( Size ),
            /// A signal was received while waiting for input
            Signal( Signal ),
        }

        /// Provides access to data related to reading and processing user input.
        pub struct Reader<'a, Term: 'a + Terminals>
        {
            iface:&'a Interface<Term>,
            lock:ReadLock<'a, Term>,
        }

        pub( crate ) struct Read<Term: Terminals>
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
            pub input_accepted:bool,

            /// Whether overwrite mode is currently active
            pub overwrite_mode:bool,
            /// Characters appended while in overwrite mode
            pub overwritten_append:usize,
            /// Characters overwritten in overwrite mode
            pub overwritten_chars: String,

            /// Configured completer
            pub completer: Arc<dyn Completer<Term>>,
            /// Character appended to completions
            pub completion_append_character:Option<char>,
            /// Current set of possible completions
            pub completions:Option<Vec<Completion>>,
            /// Current "menu-complete" entry being viewed:
            pub completion_index:usize,
            /// Start of the completed word
            pub completion_start:usize,
            /// Start of the inserted prefix of a completed word
            pub completion_prefix:usize,

            pub string_chars: Cow<'static, str>,
            pub word_break: Cow<'static, str>,

            pub last_cmd: Category,
            pub last_yank:Option<( usize, usize )>,
            pub kill_ring: VecDeque<String>,

            pub catch_signals:bool,
            pub ignore_signals: SignalSet,
            pub report_signals: SignalSet,
            pub last_resize:Option<Size>,
            pub last_signal:Option<Signal>,

            variables: Variables,

            pub state: InputState,
            pub max_wait_duration:Option<Duration>,
        }

        pub( crate ) struct ReadLock<'a, Term: 'a + Terminal> {
            term: Box<dyn TerminalReader<Term> + 'a>,
            data: MutexGuard<'a, Read<Term>>,
        }

        /// Returned from [`read_line`] to indicate user input
        ///
        /// [`read_line`]: ../interface/struct.Interface.html#method.read_line
        #[derive( Debug )]
        pub enum ReadResult {
            /// User issued end-of-file
            Eof,
            /// User input received
            Input( String ),
            /// Reported signal was received
            Signal( Signal ),
        }

        #[derive( Copy, Clone, Debug )]
        pub( crate ) enum InputState {
            Inactive,
            NewSequence,
            ContinueSequence{
                expiry:Option<Instant>,
            },
            Number,
            CharSearch{
                n:usize,
                backward:bool,
            },
            TextSearch,
            CompleteIntro,
            CompleteMore( usize ),
            QuotedInsert( usize ),
        }

        impl<'a, Term: 'a + Terminal> Reader<'a, Term> {
            pub( crate ) fn new( iface:&'a Interface<Term>, lock:ReadLock<'a, Term> )
                    -> Reader<'a, Term> {
                Reader{iface, lock}
            }

            /// Interactively reads a line from the terminal device.
            ///
            /// User input is collected until one of the following conditions is met:
            ///
            /// * If the user issues an end-of-file, `ReadResult::Eof` is returned.
            /// * When the user inputs a newline ( `'\n'` ), the resulting input
            ///   ( not containing a trailing newline character ) is returned as
            ///   `ReadResult::Input( _ )`.
            /// * When a reported signal ( see [`set_report_signal`] ) is received,
            ///   it is returned as `ReadResult::Signal( _ )`. The `read_line` operation may
            ///   then be either resumed with another call to `read_line` or ended by
            ///   calling [`cancel_read_line`].
            ///
            /// [`cancel_read_line`]: #method.cancel_read_line
            /// [`set_report_signal`]: #method.set_report_signal
            pub fn read_line( &mut self ) -> io::Result<ReadResult> {
                loop {
                    if let Some( res ) = self.read_line_step( None )? {
                        return Ok( res );
                    }
                }
            }

            /// Performs one step of the interactive `read_line` loop.
            ///
            /// This method can be used to drive the `read_line` process asynchronously.
            /// It will wait for input only up to the specified duration, then process
            /// any available input from the terminal.
            ///
            /// If the user completes the input process, `Ok( Some( result ) )` is returned.
            /// Otherwise, `Ok( None )` is returned to indicate that the interactive loop
            /// may continue.
            ///
            /// The interactive prompt may be cancelled prematurely using the
            /// [`cancel_read_line`] method.
            ///
            /// See [`read_line`] for details on the return value.
            ///
            /// [`cancel_read_line`]: #method.cancel_read_line
            /// [`read_line`]: #method.read_line
            pub fn read_line_step( &mut self, timeout:Option<Duration> )
                    -> io::Result<Option<ReadResult>> {
                self.initialize_read_line()?;

                let state = self.prepare_term()?;
                let res = self.read_line_step_impl( timeout );
                self.lock.term.restore( state )?;

                res
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
            pub fn cancel_read_line( &mut self ) -> io::Result<()> {
                self.end_read_line()
            }

            fn initialize_read_line( &mut self ) -> io::Result<()> {
                if !self.lock.is_active()
            {
                    self.prompter().start_read_line()?;
                }
                Ok( () )
            }

            fn read_line_step_impl( &mut self, timeout:Option<Duration> )
                    -> io::Result<Option<ReadResult>> {
                let do_read = if self.lock.is_input_available()
            {
                    // This branch will be taken only if a macro has buffered some input.
                    // We check for input with a zero duration to see if the user has
                    // entered Ctrl-C, e.g. to interrupt an infinitely recursive macro.
                    self.lock.term.wait_for_input( Some( Duration::from_secs( 0 ) ) )?
                } else {
                    let timeout = limit_duration( timeout, self.lock.max_wait_duration );
                    self.lock.term.wait_for_input( timeout )?
                };

                if do_read {
                    self.lock.read_input()?;
                }

                if let Some( size ) = self.lock.take_resize()
            {
                    self.handle_resize( size )?;
                }

                if let Some( sig ) = self.lock.take_signal()
            {
                    if self.lock.report_signals.contains( sig )
            {
                        return Ok( Some( ReadResult::Signal( sig ) ) );
                    }
                    if !self.lock.ignore_signals.contains( sig )
            {
                        self.handle_signal( sig )?;
                    }
                }

                // Acquire the write lock and process all available input
                {
                    let mut prompter = self.prompter();

                    prompter.check_expire_timeout()?;

                    // If the macro buffer grows in size while input is being processed,
                    // we end this step and let the caller try again. This is to allow
                    // reading Ctrl-C to interrupt ( perhaps infinite ) macro execution.
                    let mut macro_len = prompter.read.data.macro_buffer.len();

                    while prompter.read.is_input_available()
            {
                        if let Some( ch ) = prompter.read.read_char()? {
                            if let Some( r ) = prompter.handle_input( ch )? {
                                prompter.end_read_line()?;
                                return Ok( Some( r ) );
                            }
                        }

                        let new_macro_len = prompter.read.data.macro_buffer.len();

                        if new_macro_len != 0 && new_macro_len >= macro_len {
                            break;
                        }

                        macro_len = new_macro_len;
                    }
                }

                Ok( None )
            }

            fn end_read_line( &mut self ) -> io::Result<()> {
                if self.lock.is_active()
            {
                    self.prompter().end_read_line()?;
                }
                Ok( () )
            }

            fn prepare_term( &mut self ) -> io::Result<Term::PrepareState> {
                if self.read_next_raw()
            {
                    self.lock.term.prepare( true, SignalSet::new() )
                } else {
                    let mut signals = self.lock.report_signals.union( self.lock.ignore_signals );

                    if self.lock.catch_signals {
                        // Ctrl-C is always intercepted ( unless we're catching no signals ).
                        // By default, lineread handles it by clearing the current input state.
                        signals.insert( Signal::Interrupt );
                    }

                    let block_signals = !self.lock.catch_signals;

                    self.lock.term.prepare( block_signals, signals )
                }
            }

            fn read_next_raw( &self ) -> bool {
                match self.lock.state {
                    InputState::QuotedInsert( _ ) => true,
                    _ => false
                }
            }

            /// Sets the input buffer to the given string.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// # Notes
            ///
            /// To prevent invalidating the cursor, this method sets the cursor
            /// position to the end of the new buffer.
            pub fn set_buffer( &mut self, buf:&str ) -> io::Result<()> {
                if self.lock.is_active()
            {
                    self.prompter().set_buffer( buf )
                } else {
                    self.iface.lock_write_data().set_buffer( buf );
                    Ok( () )
                }
            }

            /// Sets the cursor position in the input buffer.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// # Panics
            ///
            /// If the given position is out of bounds or not on a `char` boundary.
            pub fn set_cursor( &mut self, pos: usize ) -> io::Result<()> {
                if self.lock.is_active()
            {
                    self.prompter().set_cursor( pos )
                } else {
                    self.iface.lock_write_data().set_cursor( pos );
                    Ok( () )
                }
            }

            /// Sets the prompt that will be displayed when `read_line` is called.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// # Notes
            ///
            /// If `prompt` contains any terminal escape sequences ( e.g. color codes ),
            /// such escape sequences should be immediately preceded by the character
            /// `'\x01'` and immediately followed by the character `'\x02'`.
            pub fn set_prompt( &mut self, prompt:&str ) -> io::Result<()> {
                self.prompter().set_prompt( prompt )
            }

            /// Adds a line to history.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn add_history( &self, line: String )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.add_history( line );
                    }
                }
            }

            /// Adds a line to history, unless it is identical to the most recent entry.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn add_history_unique( &self, line: String )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.add_history_unique( line );
                    }
                }
            }

            /// Removes all history entries.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn clear_history( &self )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.clear_history();
                    }
                }
            }

            /// Removes the history entry at the given index.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If the index is out of bounds, this method has no effect.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn remove_history( &self, idx: usize )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.remove_history( idx );
                    }
                }
            }

            /// Sets the maximum number of history entries.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If `n` is less than the current number of history entries,
            /// the oldest entries are truncated to meet the given requirement.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn set_history_size( &self, n: usize )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.set_history_size( n );
                    }
                }
            }

            /// Truncates history to the only the most recent `n` entries.
            ///
            /// This method internally acquires the `Interface` write lock.
            ///
            /// If a `read_line` call is in progress, this method has no effect.
            pub fn truncate_history( &self, n: usize )
            {
                if !self.lock.is_active()
            {
                    if let Ok( mut lock ) = self.iface.lock_write()
            {
                        lock.truncate_history( n );
                    }
                }
            }

            /// Returns the application name
            pub fn application( &self ) -> &str {
                &self.lock.application
            }

            /// Sets the application name
            pub fn set_application<T>( &mut self, application:T )
                    where T: Into<Cow<'static, str>> {
                self.lock.application = application.into();
            }

            /// Returns a reference to the current completer instance.
            pub fn completer( &self ) -> &Arc<dyn Completer<Term>> {
                &self.lock.completer
            }

            /// Replaces the current completer, returning the previous instance.
            pub fn set_completer( &mut self, completer: Arc<dyn Completer<Term>> )
                    -> Arc<dyn Completer<Term>> {
                replace( &mut self.lock.completer, completer )
            }

            /// Returns the value of the named variable or `None`
            /// if no such variable exists.
            pub fn get_variable( &self, name:&str ) -> Option<Variable> {
                self.lock.get_variable( name )
            }

            /// Sets the value of the named variable and returns the previous
            /// value.
            ///
            /// If `name` does not refer to a variable or the `value` is not
            /// a valid value for the variable, `None` is returned.
            pub fn set_variable( &mut self, name:&str, value:&str ) -> Option<Variable> {
                self.lock.set_variable( name, value )
            }

            /// Returns an iterator over stored variables.
            pub fn variables( &self ) -> VariableIter {
                self.lock.variables.iter()
            }

            /// Returns whether to "blink" matching opening parenthesis character
            /// when a closing parenthesis character is entered.
            ///
            /// The default value is `false`.
            pub fn blink_matching_paren( &self ) -> bool {
                self.lock.blink_matching_paren
            }

            /// Sets the `blink-matching-paren` variable.
            pub fn set_blink_matching_paren( &mut self, set:bool )
            {
                self.lock.blink_matching_paren = set;
            }

            /// Returns whether `lineread` will catch certain signals.
            pub fn catch_signals( &self ) -> bool {
                self.lock.catch_signals
            }

            /// Sets whether `lineread` will catch certain signals.
            ///
            /// This setting is `true` by default. It can be disabled to allow the
            /// host program to handle signals itself.
            pub fn set_catch_signals( &mut self, enabled:bool )
            {
                self.lock.catch_signals = enabled;
            }

            /// Returns whether the given `Signal` is ignored.
            pub fn ignore_signal( &self, signal: Signal ) -> bool {
                self.lock.ignore_signals.contains( signal )
            }

            /// Sets whether the given `Signal` will be ignored.
            pub fn set_ignore_signal( &mut self, signal: Signal, set:bool )
            {
                if set {
                    self.lock.ignore_signals.insert( signal );
                    self.lock.report_signals.remove( signal );
                } else {
                    self.lock.ignore_signals.remove( signal );
                }
            }

            /// Returns whether the given `Signal` is to be reported.
            pub fn report_signal( &self, signal: Signal ) -> bool {
                self.lock.report_signals.contains( signal )
            }

            /// Sets whether to report the given `Signal`.
            ///
            /// When a reported signal is received via the terminal, it will be returned
            /// from `Interface::read_line` as `Ok( Signal( signal ) )`.
            pub fn set_report_signal( &mut self, signal: Signal, set:bool )
            {
                if set {
                    self.lock.report_signals.insert( signal );
                    self.lock.ignore_signals.remove( signal );
                } else {
                    self.lock.report_signals.remove( signal );
                }
            }

            /// Returns whether Tab completion is disabled.
            ///
            /// The default value is `false`.
            pub fn disable_completion( &self ) -> bool {
                self.lock.disable_completion
            }

            /// Sets the `disable-completion` variable.
            pub fn set_disable_completion( &mut self, disable:bool )
            {
                self.lock.disable_completion = disable;
            }

            /// When certain control characters are pressed, a character sequence
            /// equivalent to this character will be echoed.
            ///
            /// The default value is `true`.
            pub fn echo_control_characters( &self ) -> bool {
                self.lock.echo_control_characters
            }

            /// Sets the `echo-control-characters` variable.
            pub fn set_echo_control_characters( &mut self, echo:bool )
            {
                self.lock.echo_control_characters = echo;
            }

            /// Returns the character, if any, that is appended to a successful completion.
            pub fn completion_append_character( &self ) -> Option<char> {
                self.lock.completion_append_character
            }

            /// Sets the character, if any, that is appended to a successful completion.
            pub fn set_completion_append_character( &mut self, ch:Option<char> )
            {
                self.lock.completion_append_character = ch;
            }

            /// Returns the width of completion listing display.
            ///
            /// If this value is greater than the terminal width, terminal width is used
            /// instead.
            ///
            /// The default value is equal to `usize::max_value()`.
            pub fn completion_display_width( &self ) -> usize {
                self.lock.completion_display_width
            }

            /// Sets the `completion-display-width` variable.
            pub fn set_completion_display_width( &mut self, n: usize )
            {
                self.lock.completion_display_width = n;
            }

            /// Returns the minimum number of completion items that require user
            /// confirmation before listing.
            ///
            /// The default value is `100`.
            pub fn completion_query_items( &self ) -> usize {
                self.lock.completion_query_items
            }

            /// Sets the `completion-query-items` variable.
            pub fn set_completion_query_items( &mut self, n: usize )
            {
                self.lock.completion_query_items = n;
            }

            /// Returns the timeout to wait for further user input when an ambiguous
            /// sequence has been entered. If the value is `None`, wait is indefinite.
            ///
            /// The default value 500 milliseconds.
            pub fn keyseq_timeout( &self ) -> Option<Duration> {
                self.lock.keyseq_timeout
            }

            /// Sets the `keyseq-timeout` variable.
            pub fn set_keyseq_timeout( &mut self, timeout:Option<Duration> )
            {
                self.lock.keyseq_timeout = timeout;
            }

            /// Returns whether to list possible completions one page at a time.
            ///
            /// The default value is `true`.
            pub fn page_completions( &self ) -> bool {
                self.lock.page_completions
            }

            /// Sets the `page-completions` variable.
            pub fn set_page_completions( &mut self, set:bool )
            {
                self.lock.page_completions = set;
            }

            /// Returns whether to list completions horizontally, rather than down
            /// the screen.
            ///
            /// The default value is `false`.
            pub fn print_completions_horizontally( &self ) -> bool {
                self.lock.print_completions_horizontally
            }

            /// Sets the `print-completions-horizontally` variable.
            pub fn set_print_completions_horizontally( &mut self, set:bool )
            {
                self.lock.print_completions_horizontally = set;
            }

            /// Returns the set of characters that delimit strings.
            pub fn string_chars( &self ) -> &str {
                &self.lock.string_chars
            }

            /// Sets the set of characters that delimit strings.
            pub fn set_string_chars<T>( &mut self, chars:T )
                    where T: Into<Cow<'static, str>> {
                self.lock.string_chars = chars.into();
            }

            /// Returns the set of characters that indicate a word break.
            pub fn word_break_chars( &self ) -> &str {
                &self.lock.word_break
            }

            /// Sets the set of characters that indicate a word break.
            pub fn set_word_break_chars<T>( &mut self, chars:T )
                    where T: Into<Cow<'static, str>> {
                self.lock.word_break = chars.into();
            }

            /// Returns an iterator over bound sequences
            pub fn bindings( &self ) -> BindingIter {
                self.lock.bindings()
            }

            /// Binds a sequence to a command.
            ///
            /// Returns the previously bound command.
            pub fn bind_sequence<T>( &mut self, seq:T, cmd: Command ) -> Option<Command>
                    where T: Into<Cow<'static, str>> {
                self.lock.bind_sequence( seq, cmd )
            }

            /// Binds a sequence to a command, if and only if the given sequence
            /// is not already bound to a command.
            ///
            /// Returns `true` if a new binding was created.
            pub fn bind_sequence_if_unbound<T>( &mut self, seq:T, cmd: Command ) -> bool
                    where T: Into<Cow<'static, str>> {
                self.lock.bind_sequence_if_unbound( seq, cmd )
            }

            /// Removes a binding for the given sequence.
            ///
            /// Returns the previously bound command.
            pub fn unbind_sequence( &mut self, seq:&str ) -> Option<Command> {
                self.lock.unbind_sequence( seq )
            }

            /// Defines a named function to which sequences may be bound.
            ///
            /// The name should consist of lowercase ASCII letters and numbers,
            /// containing no spaces, with words separated by hyphens. However,
            /// this is not a requirement.
            ///
            /// Returns the function previously defined with the same name.
            pub fn define_function<T>( &mut self, name:T, cmd: Arc<dyn Function<Term>> )
                    -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
                self.lock.define_function( name, cmd )
            }

            /// Removes a function defined with the given name.
            ///
            /// Returns the defined function.
            pub fn remove_function( &mut self, name:&str ) -> Option<Arc<dyn Function<Term>>> {
                self.lock.remove_function( name )
            }

            pub( crate ) fn evaluate_directives( &mut self, term:&Term, dirs: Vec<Directive> )
            {
                self.lock.data.evaluate_directives( term, dirs )
            }

            pub( crate ) fn evaluate_directive( &mut self, term:&Term, dir: Directive )
            {
                self.lock.data.evaluate_directive( term, dir )
            }

            fn prompter<'b>( &'b mut self ) -> Prompter<'b, 'a, Term> {
                Prompter::new( 
                    &mut self.lock,
                    self.iface.lock_write().expect( "Failed to acquire write lock" ) )
            }

            fn handle_resize( &mut self, size:Size ) -> io::Result<()> {
                self.prompter().handle_resize( size )
            }

            fn handle_signal( &mut self, sig: Signal ) -> io::Result<()> {
                self.prompter().handle_signal( sig )
            }
        }

        impl<'a, Term: 'a + Terminal> ReadLock<'a, Term> {
            pub fn new( term: Box<dyn TerminalReader<Term> + 'a>, data: MutexGuard<'a, Read<Term>> )
                    -> ReadLock<'a, Term> {
                ReadLock{term, data}
            }

            /// Reads the next character of input.
            ///
            /// Performs a non-blocking read from the terminal, if necessary.
            ///
            /// If non-input data was received ( e.g. a signal ) or insufficient input
            /// is available, `Ok( None )` is returned.
            pub fn read_char( &mut self ) -> io::Result<Option<char>> {
                if let Some( ch ) = self.macro_pop()
            {
                    Ok( Some( ch ) )
                } else if let Some( ch ) = self.decode_input()? {
                    Ok( Some( ch ) )
                } else {
                    Ok( None )
                }
            }

            fn read_input( &mut self ) -> io::Result<()> {
                match self.term.read( &mut self.data.input_buffer )? {
                    RawRead::Bytes( _ ) => (),
                    RawRead::Resize( new_size ) => {
                        self.last_resize = Some( new_size );
                    }
                    RawRead::Signal( sig ) => {
                        self.last_signal = Some( sig );
                    }
                }

                Ok( () )
            }

            fn is_input_available( &self ) -> bool {
                !self.data.macro_buffer.is_empty() || match self.peek_input()
            {
                    Ok( Some( _ ) ) | Err( _ ) => true,
                    Ok( None ) => false
                }
            }

            fn macro_pop( &mut self ) -> Option<char> {
                if self.data.macro_buffer.is_empty()
            {
                    None
                } else {
                    Some( self.data.macro_buffer.remove( 0 ) )
                }
            }

            fn decode_input( &mut self ) -> io::Result<Option<char>> {
                let res = self.peek_input();

                if let Ok( Some( ch ) ) = res {
                    self.data.input_buffer.drain( ..ch.len_utf8() );
                }

                res
            }

            fn peek_input( &self ) -> io::Result<Option<char>> {
                if self.data.input_buffer.is_empty()
            {
                    Ok( None )
                } else {
                    first_char( &self.data.input_buffer )
                }
            }

            pub fn reset_data( &mut self )
            {
                self.data.reset_data();
            }
        }

        impl<'a, Term: 'a + Terminal> Deref for ReadLock<'a, Term> {
            type Target = Read<Term>;

            fn deref( &self ) -> &Read<Term> {
                &self.data
            }
        }

        impl<'a, Term: 'a + Terminal> DerefMut for ReadLock<'a, Term> {
            fn deref_mut( &mut self ) -> &mut Read<Term> {
                &mut self.data
            }
        }

        impl<Term: Terminal> Deref for Read<Term> {
            type Target = Variables;

            fn deref( &self ) -> &Variables {
                &self.variables
            }
        }

        impl<Term: Terminal> DerefMut for Read<Term> {
            fn deref_mut( &mut self ) -> &mut Variables {
                &mut self.variables
            }
        }

        impl<Term: Terminal> Read<Term> {
            pub fn new( term:&Term, application: Cow<'static, str> ) -> Read<Term> {
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

                    completer: Arc::new( DummyCompleter ),
                    completion_append_character: Some( ' ' ),
                    completions: None,
                    completion_index: 0,
                    completion_start: 0,
                    completion_prefix: 0,

                    string_chars: STRING_CHARS.into(),
                    word_break: WORD_BREAK_CHARS.into(),

                    last_cmd: Category::Other,
                    last_yank: None,
                    kill_ring: VecDeque::with_capacity( MAX_KILLS ),

                    catch_signals: true,
                    ignore_signals: SignalSet::new(),
                    report_signals: SignalSet::new(),
                    last_resize: None,
                    last_signal: None,

                    variables: Variables::default(),

                    state: InputState::Inactive,
                    max_wait_duration: None,
                };

                r.read_init( term );
                r
            }

            pub fn bindings( &self ) -> BindingIter {
                BindingIter( self.bindings.sequences().iter() )
            }

            pub fn variables( &self ) -> VariableIter {
                self.variables.iter()
            }

            fn take_resize( &mut self ) -> Option<Size> {
                self.last_resize.take()
            }

            fn take_signal( &mut self ) -> Option<Signal> {
                self.last_signal.take()
            }

            pub fn queue_input( &mut self, seq:&str )
            {
                self.macro_buffer.insert_str( 0, seq );
            }

            pub fn is_active( &self ) -> bool {
                match self.state {
                    InputState::Inactive => false,
                    _ => true
                }
            }

            pub fn reset_data( &mut self )
            {
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

            pub fn bind_sequence<T>( &mut self, seq:T, cmd: Command ) -> Option<Command>
                    where T: Into<Cow<'static, str>> {
                self.bindings.insert( seq.into(), cmd )
            }

            pub fn bind_sequence_if_unbound<T>( &mut self, seq:T, cmd: Command ) -> bool
                    where T: Into<Cow<'static, str>> {
                use mortal::sequence::Entry;

                match self.bindings.entry( seq.into() )
            {
                    Entry::Occupied( _ ) => false,
                    Entry::Vacant( ent ) => {
                        ent.insert( cmd );
                        true
                    }
                }
            }

            pub fn unbind_sequence( &mut self, seq:&str ) -> Option<Command> {
                self.bindings.remove( seq )
                    .map( |( _, cmd )| cmd )
            }

            pub fn define_function<T>( &mut self, name:T, cmd: Arc<dyn Function<Term>> )
                    -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
                self.functions.insert( name.into(), cmd )
            }

            pub fn remove_function( &mut self, name:&str ) -> Option<Arc<dyn Function<Term>>> {
                self.functions.remove( name )
            }

            fn read_init( &mut self, term:&Term )
            {
                if let Some( path ) = env_init_file()
            {
                    // If `INPUTRC` is present, even if invalid, parse nothing else.
                    // Thus, an empty `INPUTRC` will inhibit loading configuration.
                    self.read_init_file_if_exists( term, Some( path ) );
                } else {
                    if !self.read_init_file_if_exists( term, user_init_file() )
            {
                        self.read_init_file_if_exists( term, system_init_file() );
                    }
                }
            }

            fn read_init_file_if_exists( &mut self, term:&Term, path:Option<PathBuf> ) -> bool {
                match path {
                    Some( ref path ) if path.exists() => {
                        self.read_init_file( term, path );
                        true
                    }
                    _ => false
                }
            }

            fn read_init_file( &mut self, term:&Term, path:&Path )
            {
                if let Some( dirs ) = parse_file( path )
            {
                    self.evaluate_directives( term, dirs );
                }
            }

            /// Evaluates a series of configuration directives.
            pub( crate ) fn evaluate_directives( &mut self, term:&Term, dirs: Vec<Directive> )
            {
                for dir in dirs {
                    self.evaluate_directive( term, dir );
                }
            }

            /// Evaluates a single configuration directive.
            pub( crate ) fn evaluate_directive( &mut self, term:&Term, dir: Directive )
            {
                match dir {
                    Directive::Bind( seq, cmd ) => {
                        self.bind_sequence( seq, cmd );
                    }
                    Directive::Conditional{name, value, then_group, else_group} => {
                        let name = name.as_ref().map( |s| &s[..] );

                        if self.eval_condition( term, name, &value )
            {
                            self.evaluate_directives( term, then_group );
                        } else {
                            self.evaluate_directives( term, else_group );
                        }
                    }
                    Directive::SetVariable( name, value ) => {
                        self.set_variable( &name, &value );
                    }
                }
            }

            fn eval_condition( &self, term:&Term, name:Option<&str>, value:&str ) -> bool {
                match name {
                    None => self.application == value,
                    Some( "lib" ) => value == "lineread",
                    Some( "mode" ) => value == "emacs",
                    Some( "term" ) => self.term_matches( term, value ),
                    _ => false
                }
            }

            fn term_matches( &self, term:&Term, value:&str ) -> bool {
                match_name( term.name(), value )
            }
        }

        /// Iterator over `Reader` bindings
        pub struct BindingIter<'a>( slice::Iter<'a, ( Cow<'static, str>, Command )> );

        impl<'a> ExactSizeIterator for BindingIter<'a> {}

        impl<'a> Iterator for BindingIter<'a> {
            type Item = ( &'a str, &'a Command );

            #[inline]
            fn next( &mut self ) -> Option<Self::Item> {
                self.0.next().map( |&( ref s, ref cmd )| ( &s[..], cmd ) )
            }

            #[inline]
            fn nth( &mut self, n: usize ) -> Option<Self::Item> {
                self.0.nth( n ).map( |&( ref s, ref cmd )| ( &s[..], cmd ) )
            }

            #[inline]
            fn size_hint( &self ) -> ( usize, Option<usize> )
            {
                self.0.size_hint()
            }
        }

        impl<'a> DoubleEndedIterator for BindingIter<'a> {
            #[inline]
            fn next_back( &mut self ) -> Option<Self::Item> {
                self.0.next_back().map( |&( ref s, ref cmd )| ( &s[..], cmd ) )
            }
        }

        fn default_bindings() -> SequenceMap<Cow<'static, str>, Command> {
            use crate::command::Command::*;

            SequenceMap::from( vec![
                // Carriage return and line feed
                ( "\r".into(), AcceptLine ),
                ( "\n".into(), AcceptLine ),

                // Possible sequences for arrow keys, Home, End
                ( "\x1b[A".into(), PreviousHistory ),
                ( "\x1b[B".into(), NextHistory ),
                ( "\x1b[C".into(), ForwardChar ),
                ( "\x1b[D".into(), BackwardChar ),
                ( "\x1b[H".into(), BeginningOfLine ),
                ( "\x1b[F".into(), EndOfLine ),

                // More possible sequences for arrow keys, Home, End
                ( "\x1bOA".into(), PreviousHistory ),
                ( "\x1bOB".into(), NextHistory ),
                ( "\x1bOC".into(), ForwardChar ),
                ( "\x1bOD".into(), BackwardChar ),
                ( "\x1bOH".into(), BeginningOfLine ),
                ( "\x1bOF".into(), EndOfLine ),

                // Possible sequences for Insert, Delete
                ( "\x1b[2~".into(), OverwriteMode ),
                ( "\x1b[3~".into(), DeleteChar ),

                // Basic commands
                ( "\x01"    .into(), BeginningOfLine ),           // Ctrl-A
                ( "\x02"    .into(), BackwardChar ),              // Ctrl-B
                ( "\x04"    .into(), DeleteChar ),                // Ctrl-D
                ( "\x05"    .into(), EndOfLine ),                 // Ctrl-E
                ( "\x06"    .into(), ForwardChar ),               // Ctrl-F
                ( "\x07"    .into(), Abort ),                     // Ctrl-G
                ( "\x08"    .into(), BackwardDeleteChar ),        // Ctrl-H
                ( "\x0b"    .into(), KillLine ),                  // Ctrl-K
                ( "\x0c"    .into(), ClearScreen ),               // Ctrl-L
                ( "\x0e"    .into(), NextHistory ),               // Ctrl-N
                ( "\x10"    .into(), PreviousHistory ),           // Ctrl-P
                ( "\x12"    .into(), ReverseSearchHistory ),      // Ctrl-R
                ( "\x14"    .into(), TransposeChars ),            // Ctrl-T
                ( "\x15"    .into(), BackwardKillLine ),          // Ctrl-U
                ( "\x16"    .into(), QuotedInsert ),              // Ctrl-V
                ( "\x17"    .into(), UnixWordRubout ),            // Ctrl-W
                ( "\x19"    .into(), Yank ),                      // Ctrl-Y
                ( "\x1d"    .into(), CharacterSearch ),           // Ctrl-]
                ( "\x7f"    .into(), BackwardDeleteChar ),        // Rubout
                ( "\x1b\x08".into(), BackwardKillWord ),          // Escape, Ctrl-H
                ( "\x1b\x1d".into(), CharacterSearchBackward ),   // Escape, Ctrl-]
                ( "\x1b\x7f".into(), BackwardKillWord ),          // Escape, Rubout
                ( "\x1bb"   .into(), BackwardWord ),              // Escape, b
                ( "\x1bd"   .into(), KillWord ),                  // Escape, d
                ( "\x1bf"   .into(), ForwardWord ),               // Escape, f
                ( "\x1bt"   .into(), TransposeWords ),            // Escape, t
                ( "\x1by"   .into(), YankPop ),                   // Escape, y
                ( "\x1b#"   .into(), InsertComment ),             // Escape, #
                ( "\x1b<"   .into(), BeginningOfHistory ),        // Escape, <
                ( "\x1b>"   .into(), EndOfHistory ),              // Escape, >

                // Completion commands
                ( "\t"   .into(), Complete ),             // Tab
                ( "\x1b?".into(), PossibleCompletions ),  // Escape, ?
                ( "\x1b*".into(), InsertCompletions ),    // Escape, *

                // Digit commands
                ( "\x1b-".into(), DigitArgument ),    // Escape, -
                ( "\x1b0".into(), DigitArgument ),    // Escape, 0
                ( "\x1b1".into(), DigitArgument ),    // Escape, 1
                ( "\x1b2".into(), DigitArgument ),    // Escape, 2
                ( "\x1b3".into(), DigitArgument ),    // Escape, 3
                ( "\x1b4".into(), DigitArgument ),    // Escape, 4
                ( "\x1b5".into(), DigitArgument ),    // Escape, 5
                ( "\x1b6".into(), DigitArgument ),    // Escape, 6
                ( "\x1b7".into(), DigitArgument ),    // Escape, 7
                ( "\x1b8".into(), DigitArgument ),    // Escape, 8
                ( "\x1b9".into(), DigitArgument ),    // Escape, 9
            ] )
        }

        fn limit_duration( dur:Option<Duration>, max:Option<Duration> ) -> Option<Duration> {
            match ( dur, max )
            {
                ( dur, None ) | ( None, dur ) => dur,
                ( Some( dur ), Some( max ) ) => Some( dur.min( max ) ),
            }
        }
        /// Reads a file and returns its contents in a string.
        pub fn read_file_str( fname: &str ) -> io::Result<String>
        {
            // Open a file in read-only mode
            let mut file = File::open( fname )?;

            let mut contents = String::new();
            let _ = file.read_to_string( &mut contents )?;

            Ok( contents )
        }
    }

    pub mod writer
    {
        //! Provides access to terminal write operations
        use ::
        {
            *,
        };
        /*
        use std::borrow::Cow::{self, Borrowed, Owned};
        use std::collections::{vec_deque, VecDeque};
        use std::fmt;
        use std::io;
        use std::iter::{repeat, Skip};
        use std::mem::swap;
        use std::ops::{Deref, DerefMut, Range};
        use std::sync::MutexGuard;
        use std::time::{Duration, Instant};

        use crate::chars::{is_ctrl, unctrl, ESCAPE, RUBOUT};
        use crate::reader::{START_INVISIBLE, END_INVISIBLE};
        use crate::terminal::{CursorMode, Size, Terminal, TerminalWriter};
        use crate::util::{
            backward_char, forward_char, backward_search_char, forward_search_char,
            filter_visible, is_combining_mark, is_wide, RangeArgument,
        };
        */
        /// Duration to wait for input when "blinking"
        pub( crate ) const BLINK_DURATION: Duration = Duration::from_millis( 500 );

        const COMPLETE_MORE:&'static str = "--More--";

        /// Default maximum history size
        const MAX_HISTORY: usize = !0;

        /// Tab column interval
        const TAB_STOP: usize = 8;

        // Length of "( arg: "
        const PROMPT_NUM_PREFIX: usize = 6;
        // Length of " ) "
        const PROMPT_NUM_SUFFIX: usize = 2;

        // Length of "( i-search )`"
        const PROMPT_SEARCH_PREFIX: usize = 11;
        // Length of "failed "
        const PROMPT_SEARCH_FAILED_PREFIX: usize = 7;
        // Length of "reverse-"
        const PROMPT_SEARCH_REVERSE_PREFIX: usize = 8;
        // Length of "': "
        const PROMPT_SEARCH_SUFFIX: usize = 3;

        /// Provides an interface to write line-by-line output to the terminal device.
        ///
        /// Holds a lock on terminal write operations.
        /// See [`Interface`] for more information about concurrent operations.
        ///
        /// An instance of this type can be constructed using either the
        /// [`Interface::lock_writer_append`] or the [`Interface::lock_writer_erase`]
        /// method.
        ///
        /// [`Interface`]: ../interface/struct.Interface.html
        /// [`Interface::lock_writer_append`]: ../interface/struct.Interface.html#method.lock_writer_append
        /// [`Interface::lock_writer_erase`]: ../interface/struct.Interface.html#method.lock_writer_erase
        pub struct Writer<'a, 'b: 'a, Term: 'b + Terminal> {
            write: WriterImpl<'a, 'b, Term>,
        }

        enum WriterImpl<'a, 'b: 'a, Term: 'b + Terminal> {
            Mutex( WriteLock<'b, Term> ),
            MutRef( &'a mut WriteLock<'b, Term> ),
        }

        pub( crate ) struct Write {
            /// Input buffer
            pub buffer: String,
            /// Original buffer entered before searching through history
            pub backup_buffer: String,
            /// Position of the cursor
            pub cursor:usize,
            /// Position of the cursor if currently performing a blink
            blink:Option<Blink>,

            /// Stored history entries
            pub history: VecDeque<String>,
            /// History entry currently being edited;
            /// `None` if the new buffer is being edited
            pub history_index:Option<usize>,
            /// Maximum size of history
            history_size:usize,
            /// Number of history entries added since last loading history
            history_new_entries:usize,

            /// Whether the prompt is drawn; i.e. a `read_line` operation is in progress
            pub is_prompt_drawn:bool,

            /// Portion of prompt up to and including the final newline
            pub prompt_prefix: String,
            prompt_prefix_len:usize,
            /// Portion of prompt after the final newline
            pub prompt_suffix: String,
            prompt_suffix_len:usize,

            /// Current type of prompt
            pub prompt_type: PromptType,

            /// Whether a search in progress is a reverse search
            pub reverse_search:bool,
            /// Whether a search in progress has failed to find a match
            pub search_failed:bool,
            /// Current search string
            pub search_buffer: String,
            /// Last search string
            pub last_search: String,
            /// Selected history entry prior to a history search
            pub prev_history:Option<usize>,
            /// Position of the cursor prior to a history search
            pub prev_cursor:usize,

            /// Numerical argument
            pub input_arg: Digit,
            /// Whether a numerical argument was supplied
            pub explicit_arg:bool,

            /// Terminal size as of last draw operation
            pub screen_size:Size,
        }

        pub( crate ) struct WriteLock<'a, Term: 'a + Terminal> {
            term: Box<dyn TerminalWriter<Term> + 'a>,
            data: MutexGuard<'a, Write>,
        }

        impl<'a, Term: Terminal> WriteLock<'a, Term> {
            pub fn new( term: Box<dyn TerminalWriter<Term> + 'a>, data: MutexGuard<'a, Write> )
                    -> WriteLock<'a, Term> {
                WriteLock{term, data}
            }

            pub fn size( &self ) -> io::Result<Size> {
                self.term.size()
            }

            pub fn flush( &mut self ) -> io::Result<()> {
                self.term.flush()
            }

            pub fn update_size( &mut self ) -> io::Result<()> {
                let size = self.size()?;
                self.screen_size = size;
                Ok( () )
            }

            pub fn blink( &mut self, pos: usize ) -> io::Result<()> {
                self.expire_blink()?;

                let orig = self.cursor;
                self.move_to( pos )?;
                self.cursor = orig;

                let expiry = Instant::now() + BLINK_DURATION;

                self.blink = Some( Blink{
                    pos,
                    expiry,
                } );

                Ok( () )
            }

            pub fn check_expire_blink( &mut self, now: Instant ) -> io::Result<bool> {
                if let Some( blink ) = self.data.blink {
                    if now >= blink.expiry {
                        self.expire_blink()?;
                    }
                }

                Ok( self.blink.is_none() )
            }

            pub fn expire_blink( &mut self ) -> io::Result<()> {
                if let Some( blink ) = self.data.blink.take()
            {
                    self.move_from( blink.pos )?;
                }

                Ok( () )
            }

            pub fn set_prompt( &mut self, prompt:&str ) -> io::Result<()> {
                self.expire_blink()?;

                let redraw = self.is_prompt_drawn && self.prompt_type.is_normal();

                if redraw {
                    self.clear_full_prompt()?;
                }

                self.data.set_prompt( prompt );

                if redraw {
                    self.draw_prompt()?;
                }

                Ok( () )
            }

            /// Draws the prompt and current input, assuming the cursor is at column 0
            pub fn draw_prompt( &mut self ) -> io::Result<()> {
                self.draw_prompt_prefix()?;
                self.draw_prompt_suffix()
            }

            pub fn draw_prompt_prefix( &mut self ) -> io::Result<()> {
                match self.prompt_type {
                    // Prefix is not drawn when completions are shown
                    PromptType::CompleteMore => Ok( () ),
                    _ => {
                        let pfx = self.prompt_prefix.clone();
                        self.draw_raw_prompt( &pfx )
                    }
                }
            }

            pub fn draw_prompt_suffix( &mut self ) -> io::Result<()> {
                match self.prompt_type {
                    PromptType::Normal => {
                        let sfx = self.prompt_suffix.clone();
                        self.draw_raw_prompt( &sfx )?;
                    }
                    PromptType::Number => {
                        let n = self.input_arg.to_i32();
                        let s = format!( "( arg: {} ) ", n );
                        self.draw_text( 0, &s )?;
                    }
                    PromptType::Search => {
                        let pre = match ( self.reverse_search, self.search_failed )
            {
                            ( false, false ) => "( i-search )",
                            ( false, true )  => "( failed i-search )",
                            ( true,  false ) => "( reverse-i-search )",
                            ( true,  true )  => "( failed reverse-i-search )",
                        };

                        let ent = self.get_history( self.history_index ).to_owned();
                        let s = format!( "{}`{}': {}", pre, self.search_buffer, ent );

                        self.draw_text( 0, &s )?;
                        let pos = self.cursor;

                        let ( lines, cols ) = self.move_delta( ent.len(), pos, &ent );
                        return self.move_rel( lines, cols );
                    }
                    PromptType::CompleteIntro( n ) => {
                        return self.term.write( &complete_intro( n ) );
                    }
                    PromptType::CompleteMore => {
                        return self.term.write( COMPLETE_MORE );
                    }
                }

                self.draw_buffer( 0 )?;
                let len = self.buffer.len();
                self.move_from( len )
            }

            pub fn redraw_prompt( &mut self, new_prompt: PromptType ) -> io::Result<()> {
                self.clear_prompt()?;
                self.prompt_type = new_prompt;
                self.draw_prompt_suffix()
            }

            /// Draws a portion of the buffer, starting from the given cursor position
            pub fn draw_buffer( &mut self, pos: usize ) -> io::Result<()> {
                let ( _, col ) = self.line_col( pos );

                let buf = self.buffer[pos..].to_owned();
                self.draw_text( col, &buf )?;
                Ok( () )
            }

            /// Draw some text with the cursor beginning at the given column.
            fn draw_text( &mut self, start_col:usize, text:&str ) -> io::Result<()> {
                self.draw_text_impl( start_col, text, Display{
                    allow_tab: true,
                    allow_newline: true,
                    .. Display::default()
                }, false )
            }

            fn draw_raw_prompt( &mut self, text:&str ) -> io::Result<()> {
                self.draw_text_impl( 0, text, Display{
                    allow_tab: true,
                    allow_newline: true,
                    allow_escape: true,
                }, true )
            }

            fn draw_text_impl( &mut self, start_col:usize, text:&str, disp: Display,
                    handle_invisible:bool ) -> io::Result<()> {
                let width = self.screen_size.columns;
                let mut col = start_col;
                let mut out = String::with_capacity( text.len() );

                let mut clear = false;
                let mut hidden = false;

                for ch in text.chars()
            {
                    if handle_invisible && ch == START_INVISIBLE {
                        hidden = true;
                    } else if handle_invisible && ch == END_INVISIBLE {
                        hidden = false;
                    } else if hidden {
                        // Render the character, but assume it has 0 width.
                        out.push( ch );
                    } else {
                        for ch in display( ch, disp )
            {
                            if ch == '\t' {
                                let n = TAB_STOP - ( col % TAB_STOP );

                                if col + n > width {
                                    let pre = width - col;
                                    out.extend( repeat( ' ' ).take( pre ) );
                                    out.push_str( " \r" );
                                    out.extend( repeat( ' ' ).take( n - pre ) );
                                    col = n - pre;
                                } else {
                                    out.extend( repeat( ' ' ).take( n ) );
                                    col += n;

                                    if col == width {
                                        out.push_str( " \r" );
                                        col = 0;
                                    }
                                }
                            } else if ch == '\n' {
                                if !clear {
                                    self.term.write( &out )?;
                                    out.clear();
                                    self.term.clear_to_screen_end()?;
                                    clear = true;
                                }

                                out.push( '\n' );
                                col = 0;
                            } else if is_combining_mark( ch )
            {
                                out.push( ch );
                            } else if is_wide( ch )
            {
                                if col == width - 1 {
                                    out.push_str( "  \r" );
                                    out.push( ch );
                                    col = 2;
                                } else {
                                    out.push( ch );
                                    col += 2;
                                }
                            } else {
                                out.push( ch );
                                col += 1;

                                if col == width {
                                    // Space pushes the cursor to the next line,
                                    // CR brings back to the start of the line.
                                    out.push_str( " \r" );
                                    col = 0;
                                }
                            }
                        }
                    }
                }

                if col == width {
                    out.push_str( " \r" );
                }

                self.term.write( &out )
            }

            pub fn set_buffer( &mut self, buf:&str ) -> io::Result<()> {
                self.expire_blink()?;

                self.move_to( 0 )?;
                self.buffer.clear();
                self.buffer.push_str( buf );
                self.new_buffer()
            }

            pub fn set_cursor( &mut self, pos: usize ) -> io::Result<()> {
                self.expire_blink()?;

                if !self.buffer.is_char_boundary( pos )
            {
                    panic!( "invalid cursor position {} in buffer {:?}",
                        pos, self.buffer );
                }

                self.move_to( pos )
            }

            pub fn set_cursor_mode( &mut self, mode: CursorMode ) -> io::Result<()> {
                self.term.set_cursor_mode( mode )
            }

            pub fn history_len( &self ) -> usize {
                self.history.len()
            }

            pub fn history_size( &self ) -> usize {
                self.history_size
            }

            pub fn set_history_size( &mut self, n: usize )
            {
                self.history_size = n;
                self.truncate_history( n );
            }

            pub fn write_str( &mut self, s:&str ) -> io::Result<()> {
                self.term.write( s )
            }

            pub fn start_history_search( &mut self, reverse:bool ) -> io::Result<()> {
                self.search_buffer = self.buffer[..self.cursor].to_owned();

                self.continue_history_search( reverse )
            }

            pub fn continue_history_search( &mut self, reverse:bool ) -> io::Result<()> {
                if let Some( idx ) = self.find_history_search( reverse )
            {
                    self.set_history_entry( Some( idx ) );

                    let pos = self.cursor;
                    let end = self.buffer.len();

                    self.draw_buffer( pos )?;
                    self.clear_to_screen_end()?;
                    self.move_from( end )?;
                }

                Ok( () )
            }

            fn find_history_search( &self, reverse:bool ) -> Option<usize> {
                let len = self.history.len();
                let idx = self.history_index.unwrap_or( len );

                if reverse {
                    self.history.iter().rev().skip( len - idx )
                        .position( |ent| ent.starts_with( &self.search_buffer ) )
                        .map( |pos| idx - ( pos + 1 ) )
                } else {
                    self.history.iter().skip( idx + 1 )
                        .position( |ent| ent.starts_with( &self.search_buffer ) )
                        .map( |pos| idx + ( pos + 1 ) )
                }
            }

            pub fn start_search_history( &mut self, reverse:bool ) -> io::Result<()> {
                self.reverse_search = reverse;
                self.search_failed = false;
                self.search_buffer.clear();
                self.prev_history = self.history_index;
                self.prev_cursor = self.cursor;

                self.redraw_prompt( PromptType::Search )
            }

            pub fn continue_search_history( &mut self, reverse:bool ) -> io::Result<()> {
                self.reverse_search = reverse;
                self.search_failed = false;

                {
                    let data = &mut *self.data;
                    data.search_buffer.clone_from( &data.last_search );
                }

                self.search_history_step()
            }

            pub fn end_search_history( &mut self ) -> io::Result<()> {
                self.redraw_prompt( PromptType::Normal )
            }

            pub fn abort_search_history( &mut self ) -> io::Result<()> {
                self.clear_prompt()?;

                let ent = self.prev_history;
                self.set_history_entry( ent );
                self.cursor = self.prev_cursor;

                self.prompt_type = PromptType::Normal;
                self.draw_prompt_suffix()
            }

            fn show_search_match( &mut self, next_match:Option<( Option<usize>, usize )> )
                    -> io::Result<()> {
                self.clear_prompt()?;

                if let Some( ( idx, pos ) ) = next_match {
                    self.search_failed = false;
                    self.set_history_entry( idx );
                    self.cursor = pos;
                } else {
                    self.search_failed = true;
                }

                self.prompt_type = PromptType::Search;
                self.draw_prompt_suffix()
            }

            pub fn search_history_update( &mut self ) -> io::Result<()> {
                // Search for the next match, perhaps including the current position
                let next_match = if self.reverse_search {
                    self.search_history_backward( &self.search_buffer, true )
                } else {
                    self.search_history_forward( &self.search_buffer, true )
                };

                self.show_search_match( next_match )
            }

            fn search_history_step( &mut self ) -> io::Result<()> {
                if self.search_buffer.is_empty()
            {
                    return self.redraw_prompt( PromptType::Search );
                }

                // Search for the next match
                let next_match = if self.reverse_search {
                    self.search_history_backward( &self.search_buffer, false )
                } else {
                    self.search_history_forward( &self.search_buffer, false )
                };

                self.show_search_match( next_match )
            }

            fn search_history_backward( &self, s:&str, include_cur:bool )
                    -> Option<( Option<usize>, usize )> {
                let mut idx = self.history_index;
                let mut pos = Some( self.cursor );

                if include_cur && !self.search_failed {
                    if let Some( p ) = pos {
                        if self.get_history( idx ).is_char_boundary( p + s.len() )
            {
                            pos = Some( p + s.len() );
                        }
                    }
                }

                loop {
                    let line = self.get_history( idx );

                    match line[..pos.unwrap_or( line.len() )].rfind( s )
            {
                        Some( found ) => {
                            pos = Some( found );
                            break;
                        }
                        None => {
                            match idx {
                                Some( 0 ) => return None,
                                Some( n ) => {
                                    idx = Some( n - 1 );
                                    pos = None;
                                }
                                None => {
                                    if self.history.is_empty()
            {
                                        return None;
                                    } else {
                                        idx = Some( self.history.len() - 1 );
                                        pos = None;
                                    }
                                }
                            }
                        }
                    }
                }

                pos.map( |pos| ( idx, pos ) )
            }

            fn search_history_forward( &self, s:&str, include_cur:bool )
                    -> Option<( Option<usize>, usize )> {
                let mut idx = self.history_index;
                let mut pos = Some( self.cursor );

                if !include_cur {
                    if let Some( p ) = pos {
                        pos = Some( forward_char( 1, self.get_history( idx ), p ) );
                    }
                }

                loop {
                    let line = self.get_history( idx );

                    match line[pos.unwrap_or( 0 )..].find( s )
            {
                        Some( found ) => {
                            pos = pos.map( |n| n + found ).or( Some( found ) );
                            break;
                        }
                        None => {
                            if let Some( n ) = idx {
                                if n + 1 == self.history.len()
            {
                                    idx = None;
                                } else {
                                    idx = Some( n + 1 );
                                }
                                pos = None;
                            } else {
                                return None;
                            }
                        }
                    }
                }

                pos.map( |pos| ( idx, pos ) )
            }

            pub fn add_history( &mut self, line: String )
            {
                if self.history.len() == self.history_size {
                    self.history.pop_front();
                }

                self.history.push_back( line );
                self.history_new_entries = self.history.len()
                    .min( self.history_new_entries + 1 );
            }

            pub fn add_history_unique( &mut self, line: String )
            {
                let is_duplicate = self.history.back().map_or( false, |ent| *ent == line );

                if !is_duplicate {
                    self.add_history( line );
                }
            }

            pub fn clear_history( &mut self )
            {
                self.truncate_history( 0 );
                self.history_new_entries = 0;
            }

            pub fn remove_history( &mut self, n: usize )
            {
                if n < self.history.len()
            {
                    let first_new = self.history.len() - self.history_new_entries;

                    if n >= first_new {
                        self.history_new_entries -= 1;
                    }

                    self.history.remove( n );
                }
            }

            pub fn truncate_history( &mut self, n: usize )
            {
                let len = self.history.len();

                if n < len {
                    let _ = self.history.drain( ..len - n );
                    self.history_new_entries = self.history_new_entries.max( n );
                }
            }

            pub fn next_history( &mut self, n: usize ) -> io::Result<()> {
                if let Some( old ) = self.history_index {
                    let new = old.saturating_add( n );

                    if new >= self.history.len()
            {
                        self.select_history_entry( None )?;
                    } else {
                        self.select_history_entry( Some( new ) )?;
                    }
                }

                Ok( () )
            }

            pub fn prev_history( &mut self, n: usize ) -> io::Result<()> {
                if !self.history.is_empty() && self.history_index != Some( 0 )
            {
                    let new = if let Some( old ) = self.history_index {
                        old.saturating_sub( n )
                    } else {
                        self.history.len().saturating_sub( n )
                    };

                    self.select_history_entry( Some( new ) )?;
                }

                Ok( () )
            }

            pub fn select_history_entry( &mut self, new:Option<usize> ) -> io::Result<()> {
                if new != self.history_index {
                    self.move_to( 0 )?;
                    self.set_history_entry( new );
                    self.new_buffer()?;
                }

                Ok( () )
            }

            pub fn set_history_entry( &mut self, new:Option<usize> )
            {
                let old = self.history_index;

                if old != new {
                    let data = &mut *self.data;
                    data.history_index = new;

                    if let Some( old ) = old {
                        data.history[old].clone_from( &data.buffer );
                    } else {
                        swap( &mut data.buffer, &mut data.backup_buffer );
                    }

                    if let Some( new ) = new {
                        data.buffer.clone_from( &data.history[new] );
                    } else {
                        data.buffer.clear();
                        swap( &mut data.buffer, &mut data.backup_buffer );
                    }
                }
            }

            fn get_history( &self, n:Option<usize> ) -> &str {
                if self.history_index == n {
                    &self.buffer
                } else if let Some( n ) = n {
                    &self.history[n]
                } else {
                    &self.backup_buffer
                }
            }

            pub fn backward_char( &mut self, n: usize ) -> io::Result<()> {
                let pos = backward_char( n, &self.buffer, self.cursor );
                self.move_to( pos )
            }

            pub fn forward_char( &mut self, n: usize ) -> io::Result<()> {
                let pos = forward_char( n, &self.buffer, self.cursor );
                self.move_to( pos )
            }

            pub fn backward_search_char( &mut self, n:usize, ch: char ) -> io::Result<()> {
                if let Some( pos ) = backward_search_char( n, &self.buffer, self.cursor, ch )
            {
                    self.move_to( pos )?;
                }

                Ok( () )
            }

            pub fn forward_search_char( &mut self, n:usize, ch: char ) -> io::Result<()> {
                if let Some( pos ) = forward_search_char( n, &self.buffer, self.cursor, ch )
            {
                    self.move_to( pos )?;
                }

                Ok( () )
            }

            /// Deletes a range from the buffer; the cursor is moved to the end
            /// of the given range.
            pub fn delete_range<R: RangeArgument<usize>>( &mut self, range: R ) -> io::Result<()> {
                let start = range.start().cloned().unwrap_or( 0 );
                let end = range.end().cloned().unwrap_or_else( || self.buffer.len() );

                self.move_to( start )?;

                let _ = self.buffer.drain( start..end );

                self.draw_buffer( start )?;
                self.term.clear_to_screen_end()?;
                let len = self.buffer.len();
                self.move_from( len )?;

                Ok( () )
            }

            pub fn insert_str( &mut self, s:&str ) -> io::Result<()> {
                // If the string insertion moves a combining character,
                // we must redraw starting from the character before the cursor.
                let moves_combining = match self.buffer[self.cursor..].chars().next()
            {
                    Some( ch ) if is_combining_mark( ch ) => true,
                    _ => false
                };

                let cursor = self.cursor;
                self.buffer.insert_str( cursor, s );

                if moves_combining && cursor != 0 {
                    let pos = backward_char( 1, &self.buffer, self.cursor );
                    // Move without updating the cursor
                    let ( lines, cols ) = self.move_delta( cursor, pos, &self.buffer );
                    self.move_rel( lines, cols )?;
                    self.draw_buffer( pos )?;
                } else {
                    self.draw_buffer( cursor )?;
                }

                self.cursor += s.len();

                let len = self.buffer.len();
                self.move_from( len )
            }

            pub fn transpose_range( &mut self, src: Range<usize>, dest: Range<usize> )
                    -> io::Result<()> {
                // Ranges must not overlap
                assert!( src.end <= dest.start || src.start >= dest.end );

                // Final cursor position
                let final_cur = if src.start < dest.start {
                    dest.end
                } else {
                    dest.start + ( src.end - src.start )
                };

                let ( left, right ) = if src.start < dest.start {
                    ( src, dest )
                } else {
                    ( dest, src )
                };

                self.move_to( left.start )?;

                let a = self.buffer[left.clone()].to_owned();
                let b = self.buffer[right.clone()].to_owned();

                let _ = self.buffer.drain( right.clone() );
                self.buffer.insert_str( right.start, &a );

                let _ = self.buffer.drain( left.clone() );
                self.buffer.insert_str( left.start, &b );

                let cursor = self.cursor;
                self.draw_buffer( cursor )?;
                self.term.clear_to_screen_end()?;

                self.cursor = final_cur;
                let len = self.buffer.len();
                self.move_from( len )
            }

            fn prompt_suffix_length( &self ) -> usize {
                match self.prompt_type {
                    PromptType::Normal => self.prompt_suffix_len,
                    PromptType::Number => {
                        let n = number_len( self.input_arg.to_i32() );
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

                        let n = self.display_size( &self.search_buffer, prefix );
                        prefix + n + PROMPT_SEARCH_SUFFIX
                    }
                    PromptType::CompleteIntro( n ) => complete_intro( n ).len(),
                    PromptType::CompleteMore => COMPLETE_MORE.len(),
                }
            }

            fn line_col( &self, pos: usize ) -> ( usize, usize )
            {
                let prompt_len = self.prompt_suffix_length();

                match self.prompt_type {
                    PromptType::CompleteIntro( _ ) |
                    PromptType::CompleteMore => {
                        let width = self.screen_size.columns;
                        ( prompt_len / width, prompt_len % width )
                    }
                    _ => self.line_col_with( pos, &self.buffer, prompt_len )
                }
            }

            fn line_col_with( &self, pos:usize, buf:&str, start_col: usize ) -> ( usize, usize )
            {
                let width = self.screen_size.columns;
                if width == 0 {
                    return ( 0, 0 );
                }

                let n = start_col + self.display_size( &buf[..pos], start_col );

                ( n / width, n % width )
            }

            pub fn clear_screen( &mut self ) -> io::Result<()> {
                self.term.clear_screen()?;
                self.draw_prompt()?;

                Ok( () )
            }

            pub fn clear_to_screen_end( &mut self ) -> io::Result<()> {
                self.term.clear_to_screen_end()
            }

            /// Draws a new buffer on the screen. Cursor position is assumed to be `0`.
            pub fn new_buffer( &mut self ) -> io::Result<()> {
                self.draw_buffer( 0 )?;
                self.cursor = self.buffer.len();

                self.term.clear_to_screen_end()?;

                Ok( () )
            }

            pub fn clear_full_prompt( &mut self ) -> io::Result<()> {
                let prefix_lines = self.prompt_prefix_len / self.screen_size.columns;
                let ( line, _ ) = self.line_col( self.cursor );
                self.term.move_up( prefix_lines + line )?;
                self.term.move_to_first_column()?;
                self.term.clear_to_screen_end()
            }

            pub( crate ) fn clear_prompt( &mut self ) -> io::Result<()> {
                let ( line, _ ) = self.line_col( self.cursor );

                self.term.move_up( line )?;
                self.term.move_to_first_column()?;
                self.term.clear_to_screen_end()
            }

            /// Move back to true cursor position from some other position
            pub fn move_from( &mut self, pos: usize ) -> io::Result<()> {
                let ( lines, cols ) = self.move_delta( pos, self.cursor, &self.buffer );
                self.move_rel( lines, cols )
            }

            pub fn move_to( &mut self, pos: usize ) -> io::Result<()> {
                if pos != self.cursor {
                    let ( lines, cols ) = self.move_delta( self.cursor, pos, &self.buffer );
                    self.move_rel( lines, cols )?;
                    self.cursor = pos;
                }

                Ok( () )
            }

            pub fn move_to_end( &mut self ) -> io::Result<()> {
                let pos = self.buffer.len();
                self.move_to( pos )
            }

            pub fn move_right( &mut self, n: usize ) -> io::Result<()> {
                self.term.move_right( n )
            }

            /// Moves from `old` to `new` cursor position, using the given buffer
            /// as current input.
            fn move_delta( &self, old:usize, new:usize, buf:&str ) -> ( isize, isize )
            {
                let prompt_len = self.prompt_suffix_length();
                let ( old_line, old_col ) = self.line_col_with( old, buf, prompt_len );
                let ( new_line, new_col ) = self.line_col_with( new, buf, prompt_len );

                ( new_line as isize - old_line as isize,
                new_col as isize - old_col as isize )
            }

            fn move_rel( &mut self, lines: isize, cols: isize ) -> io::Result<()> {
                if lines > 0 {
                    self.term.move_down( lines as usize )?;
                } else if lines < 0 {
                    self.term.move_up( ( -lines ) as usize )?;
                }

                if cols > 0 {
                    self.term.move_right( cols as usize )?;
                } else if cols < 0 {
                    self.term.move_left( ( -cols ) as usize )?;
                }

                Ok( () )
            }

            pub fn reset_data( &mut self )
            {
                self.data.reset_data();
            }

            pub fn set_digit_from_char( &mut self, ch: char )
            {
                let digit = match ch {
                    '-' => Digit::NegNone,
                    '0' ..= '9' => Digit::from( ch ),
                    _ => Digit::None
                };

                self.input_arg = digit;
                self.explicit_arg = true;
            }
        }

        #[derive( Copy, Clone )]
        struct Blink {
            pos:usize,
            expiry: Instant,
        }

        impl<'a, 'b: 'a, Term: 'b + Terminal> Writer<'a, 'b, Term> {
            fn new( mut write: WriterImpl<'a, 'b, Term>, clear:bool ) -> io::Result<Self> {
                write.expire_blink()?;

                if write.is_prompt_drawn {
                    if clear {
                        write.clear_full_prompt()?;
                    } else {
                        write.move_to_end()?;
                        write.write_str( "\n" )?;
                    }
                }

                Ok( Writer{write} )
            }

            pub( crate ) fn with_lock( write: WriteLock<'b, Term>, clear:bool ) -> io::Result<Self> {
                Writer::new( WriterImpl::Mutex( write ), clear )
            }

            pub( crate ) fn with_ref( write:&'a mut WriteLock<'b, Term>, clear:bool ) -> io::Result<Self> {
                Writer::new( WriterImpl::MutRef( write ), clear )
            }

            /// Returns an iterator over history entries.
            pub fn history( &self ) -> HistoryIter {
                self.write.history()
            }

            /// Writes some text to the terminal device.
            ///
            /// Before the `Writer` is dropped, any output written should be followed
            /// by a newline. A newline is automatically written if the `writeln!`
            /// macro is used.
            pub fn write_str( &mut self, s:&str ) -> io::Result<()> {
                self.write.write_str( s )
            }

            /// Writes formatted text to the terminal display.
            ///
            /// This method enables `Interface` to be used as the receiver to
            /// the [`writeln!`] macro.
            ///
            /// If the text contains any unprintable characters ( e.g. escape sequences ),
            /// those characters will be escaped before printing.
            ///
            /// [`read_line`]: ../interface/struct.Interface.html#method.read_line
            /// [`writeln!`]: https://doc.rust-lang.org/std/macro.writeln.html
            pub fn write_fmt( &mut self, args:fmt::Arguments ) -> io::Result<()> {
                let s = args.to_string();
                self.write_str( &s )
            }
        }

        impl<'a, 'b: 'a, Term: 'b + Terminal> Drop for Writer<'a, 'b, Term> {
            fn drop( &mut self )
            {
                if self.write.is_prompt_drawn {
                    // There's not really anything useful to be done with this error.
                    let _ = self.write.draw_prompt();
                }
            }
        }

        impl<'a, Term: 'a + Terminal> Deref for WriteLock<'a, Term> {
            type Target = Write;

            fn deref( &self ) -> &Write {
                &self.data
            }
        }

        impl<'a, Term: 'a + Terminal> DerefMut for WriteLock<'a, Term> {
            fn deref_mut( &mut self ) -> &mut Write {
                &mut self.data
            }
        }

        impl Write {
            pub fn new( screen_size:Size ) -> Write {
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

            pub fn history( &self ) -> HistoryIter {
                HistoryIter( self.history.iter() )
            }

            pub fn new_history( &self ) -> Skip<HistoryIter> {
                let first_new = self.history.len() - self.history_new_entries;
                self.history().skip( first_new )
            }

            pub fn new_history_entries( &self ) -> usize {
                self.history_new_entries
            }

            pub fn reset_data( &mut self )
            {
                self.buffer.clear();
                self.backup_buffer.clear();
                self.cursor = 0;
                self.history_index = None;

                self.prompt_type = PromptType::Normal;

                self.input_arg = Digit::None;
                self.explicit_arg = false;
            }

            pub fn reset_new_history( &mut self )
            {
                self.history_new_entries = 0;
            }

            pub fn set_buffer( &mut self, buf:&str )
            {
                self.buffer.clear();
                self.buffer.push_str( buf );
                self.cursor = buf.len();
            }

            pub fn set_cursor( &mut self, pos: usize )
            {
                if !self.buffer.is_char_boundary( pos )
            {
                    panic!( "invalid cursor position {} in buffer {:?}",
                        pos, self.buffer );
                }

                self.cursor = pos;
            }

            pub fn set_prompt( &mut self, prompt:&str )
            {
                let ( pre, suf ) = match prompt.rfind( '\n' )
            {
                    Some( pos ) => ( &prompt[..pos + 1], &prompt[pos + 1..] ),
                    None => ( &prompt[..0], prompt )
                };

                self.prompt_prefix = pre.to_owned();
                self.prompt_suffix = suf.to_owned();

                let pre_virt = filter_visible( pre );
                self.prompt_prefix_len = self.display_size( &pre_virt, 0 );

                let suf_virt = filter_visible( suf );
                self.prompt_suffix_len = self.display_size( &suf_virt, 0 );
            }

            pub fn display_size( &self, s:&str, start_col: usize ) -> usize {
                let width = self.screen_size.columns;
                let mut col = start_col;

                let disp = Display{
                    allow_tab: true,
                    allow_newline: true,
                    .. Display::default()
                };

                for ch in s.chars().flat_map( |ch| display( ch, disp ) )
            {
                    let n = match ch {
                        '\n' => width - ( col % width ),
                        '\t' => TAB_STOP - ( col % TAB_STOP ),
                        ch if is_combining_mark( ch ) => 0,
                        ch if is_wide( ch ) => {
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

        #[derive( Copy, Clone, Debug )]
        pub( crate ) enum Digit {
            None,
            NegNone,
            Num( i32 ),
            NegNum( i32 ),
        }

        impl Digit {
            pub fn input( &mut self, n: i32 )
            {
                match *self {
                    Digit::None => *self = Digit::Num( n ),
                    Digit::NegNone => *self = Digit::NegNum( n ),
                    Digit::Num( ref mut m ) | Digit::NegNum( ref mut m ) => {
                        *m *= 10;
                        *m += n;
                    }
                }
            }

            pub fn is_out_of_bounds( &self ) -> bool {
                match *self {
                    Digit::Num( n ) | Digit::NegNum( n ) if n > NUMBER_MAX => true,
                    _ => false
                }
            }

            pub fn to_i32( &self ) -> i32 {
                match *self {
                    Digit::None => 1,
                    Digit::NegNone => -1,
                    Digit::Num( n ) => n,
                    Digit::NegNum( n ) => -n,
                }
            }
        }

        impl From<char> for Digit {
            /// Convert a decimal digit character to a `Digit` value.
            ///
            /// The input must be in the range `'0' ..= '9'`.
            fn from( ch: char ) -> Digit {
                let n = ( ch as u8 ) - b'0';
                Digit::Num( n as i32 )
            }
        }

        #[derive( Copy, Clone, Debug, Eq, PartialEq )]
        pub( crate ) enum PromptType {
            Normal,
            Number,
            Search,
            CompleteIntro( usize ),
            CompleteMore,
        }

        impl PromptType {
            pub( crate ) fn is_normal( &self ) -> bool {
                *self == PromptType::Normal
            }
        }

        impl<'a, 'b, Term: 'b + Terminal> Deref for WriterImpl<'a, 'b, Term> {
            type Target = WriteLock<'b, Term>;

            fn deref( &self ) -> &WriteLock<'b, Term> {
                match *self {
                    WriterImpl::Mutex( ref m ) => m,
                    WriterImpl::MutRef( ref m ) => m,
                }
            }
        }

        impl<'a, 'b: 'a, Term: 'b + Terminal> DerefMut for WriterImpl<'a, 'b, Term> {
            fn deref_mut( &mut self ) -> &mut WriteLock<'b, Term> {
                match *self {
                    WriterImpl::Mutex( ref mut m ) => m,
                    WriterImpl::MutRef( ref mut m ) => m,
                }
            }
        }

        /// Iterator over `Interface` history entries
        pub struct HistoryIter<'a>( vec_deque::Iter<'a, String> );

        impl<'a> ExactSizeIterator for HistoryIter<'a> {}

        impl<'a> Iterator for HistoryIter<'a> {
            type Item = &'a str;

            #[inline]
            fn next( &mut self ) -> Option<&'a str> {
                self.0.next().map( |s| &s[..] )
            }

            #[inline]
            fn nth( &mut self, n: usize ) -> Option<&'a str> {
                self.0.nth( n ).map( |s| &s[..] )
            }

            #[inline]
            fn size_hint( &self ) -> ( usize, Option<usize> )
            {
                self.0.size_hint()
            }
        }

        impl<'a> DoubleEndedIterator for HistoryIter<'a> {
            #[inline]
            fn next_back( &mut self ) -> Option<&'a str> {
                self.0.next_back().map( |s| &s[..] )
            }
        }

        #[derive( Copy, Clone, Debug, Eq, PartialEq )]
        pub( crate ) enum DisplaySequence {
            Char( char ),
            Escape( char ),
            End,
        }

        impl Iterator for DisplaySequence {
            type Item = char;

            fn next( &mut self ) -> Option<char> {
                use self::DisplaySequence::*;

                let ( res, next ) = match *self {
                    Char( ch ) => ( ch, End ),
                    Escape( ch ) => ( '^', Char( ch ) ),
                    End => return None
                };

                *self = next;
                Some( res )
            }

            fn size_hint( &self ) -> ( usize, Option<usize> )
            {
                use self::DisplaySequence::*;

                let n = match *self {
                    Char( _ ) => 1,
                    Escape( _ ) => 2,
                    End => 0,
                };

                ( n, Some( n ) )
            }
        }

        #[derive( Copy, Clone, Debug, Default )]
        pub( crate ) struct Display {
            allow_tab:bool,
            allow_newline:bool,
            allow_escape:bool,
        }

        pub( crate ) fn display( ch: char, style: Display ) -> DisplaySequence {
            match ch {
                '\t' if style.allow_tab => DisplaySequence::Char( ch ),
                '\n' if style.allow_newline => DisplaySequence::Char( ch ),
                ESCAPE if style.allow_escape => DisplaySequence::Char( ch ),
                '\0' => DisplaySequence::Escape( '@' ),
                RUBOUT => DisplaySequence::Escape( '?' ),
                ch if is_ctrl( ch ) => DisplaySequence::Escape( unctrl( ch ) ),
                ch => DisplaySequence::Char( ch )
            }
        }

        pub( crate ) fn display_str<'a>( s:&'a str, style: Display ) -> Cow<'a, str> {
            if s.chars().all( |ch| display( ch, style ) == DisplaySequence::Char( ch ) )
            {
                Borrowed( s )
            } else {
                Owned( s.chars().flat_map( |ch| display( ch, style ) ).collect() )
            }
        }

        fn complete_intro( n: usize ) -> String {
            format!( "Display all {} possibilities? ( y/n )", n )
        }

        fn number_len( n: i32 ) -> usize {
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
    pub fn run_command_line( sh:&mut Shell, line:&str, tty:bool, capture:bool ) -> Vec<CommandResult>
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();

        for token in parsers::line::to_cmds( line )
        {
            if token == ";" || token == "&&" || token == "||"
            {
                sep = token.clone();
                continue;
            }

            if sep == "&&" && status != 0 { break; }
            
            if sep == "||" && status == 0 { break; }

            let cmd = token.clone();
            let cr = run_procedure( sh, &cmd, tty, capture );
            status = cr.status;
            sh.previous_status = status;
            cr_list.push( cr );
            
        }

        cr_list
    }
    /// Run a single command.
    fn run_single_program
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

            if cl.is_single_and_builtin()
            {
                if let Some( cr ) = try_run_builtin( sh, cl, idx_cmd, capture )
                {
                    *cmd_result = cr;
                    return unsafe { libc::getpid() };
                }

                println_stderr!( "cicada: error when run singler builtin" );
                log!( "error when run singler builtin: {:?}", cl );
                return 1;
            }

            let pipes_count = pipes.len();
            let mut fds_stdin = None;
            let cmd = cl.commands.get( idx_cmd ).unwrap();

            if cmd.has_here_string()
            {
                match process::pipe()
                {
                    Ok( fds ) => fds_stdin = Some( fds ),
                    Err( e ) =>
                    {
                        println_stderr!( "cicada: pipeline4: {}", e );
                        return 1;
                    }
                }
            }

            match process::fork()
            {
                Ok( ForkResult::Child ) =>
                {
                    libc::signal( libc::SIGTSTP, libc::SIG_DFL );
                    libc::signal( libc::SIGQUIT, libc::SIG_DFL );
                    
                    if idx_cmd > 0
                    {
                        for i in 0..idx_cmd - 1
                        {
                            let fds = pipes[i];
                            libs::close( fds.0 );
                            libs::close( fds.1 );
                        }
                    }
                    
                    for i in idx_cmd + 1..pipes_count
                    {
                        let fds = pipes[i];
                        libs::close( fds.0 );
                        libs::close( fds.1 );
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        if let Some( fds ) = fds_capture_stdout
                        {
                            libs::close( fds.0 );
                            libs::close( fds.1 );
                        }

                        if let Some( fds ) = fds_capture_stderr
                        {
                            libs::close( fds.0 );
                            libs::close( fds.1 );
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
                        libs::dup2( fds_prev.0, 0 );
                        libs::close( fds_prev.0 );
                        libs::close( fds_prev.1 );
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        libs::dup2( fds.1, 1 );
                        libs::close( fds.1 );
                        libs::close( fds.0 );
                    }

                    if cmd.has_redirect_from()
                    {
                        if let Some( redirect_from ) = &cmd.redirect_from
                        {
                            let fd = tools::get_fd_from_file( &redirect_from.clone().1 );
                            if fd == -1 { process::exit( 1 ); }

                            libs::dup2( fd, 0 );
                            libs::close( fd );
                        }
                    }

                    if cmd.has_here_string()
                    {
                        if let Some( fds ) = fds_stdin
                        {
                            libs::close( fds.1 );
                            libs::dup2( fds.0, 0 );
                            libs::close( fds.0 );
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
                                libs::dup2( 1, 2 );
                            }
                            
                            else if !options.capture_output
                            {
                                let fd = libs::dup( 1 );
                                if fd == -1
                                {
                                    println_stderr!( "cicada: dup error" );
                                    process::exit( 1 );
                                }

                                libs::dup2( fd, 2 );
                            }
                            
                            else { }
                        }
                        
                        else if to_ == "&2" && from_ == "1"
                        {
                            if idx_cmd < pipes_count || !options.capture_output
                            {
                                let fd = libs::dup( 2 );
                                if fd == -1
                                {
                                    println_stderr!( "cicada: dup error" );
                                    process::exit( 1 );
                                }

                                libs::dup2( fd, 1 );
                            }
                            
                            else { }
                        } 
                        
                        else
                        {
                            let append = op_ == ">>";
                            match tools::create_raw_fd_from_file( to_, append )
                            {
                                Ok( fd ) =>
                                {
                                    if fd == -1
                                    {
                                        println_stderr!( "cicada: fork: fd error" );
                                        process::exit( 1 );
                                    }

                                    if from_ == "1"
                                    {
                                        libs::dup2( fd, 1 );
                                        stdout_redirected = true;
                                    }
                                    
                                    else
                                    {
                                        libs::dup2( fd, 2 );
                                        stderr_redirected = true;
                                    }
                                }

                                Err( e ) =>
                                {
                                    println_stderr!( "cicada: fork: {}", e );
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
                                libs::close( fds.0 );
                                libs::dup2( fds.1, 1 );
                                libs::close( fds.1 );
                            }
                        }

                        if !stderr_redirected
                        {
                            if let Some( fds ) = fds_capture_stderr
                            {
                                libs::close( fds.0 );
                                libs::dup2( fds.1, 2 );
                                libs::close( fds.1 );
                            }
                        }
                    }

                    if cmd.is_builtin()
                    {
                        if let Some( status ) = try_run_builtin_in_subprocess( sh, cl, idx_cmd, capture )
                        { process::exit( status ); }
                    }
                    
                    let mut c_envs: Vec<_> = vars().map( |( k, v )|
                    {
                        CString::new( format!( "{}={}", k, v ).as_str() ).expect( "CString error" )
                    } ).collect();

                    for ( key, value ) in cl.envs.iter()
                    {
                        c_envs.push( CString::new( format!( "{}={}", key, value ).as_str() ).expect( "CString error" ), );
                    }

                    let program = &cmd.tokens[0].1;
                    let path = if program.contains( '/' )
            {
                        program.clone()
                    }
                    
                    else {  libs::path::find_file_in_path( program, true ) };

                    if path.is_empty()
                    {
                        println_stderr!( "cicada: {}: command not found", program );
                        process::exit( 127 );
                    }

                    let c_program = CString::new( path.as_str() ).expect( "CString::new failed" );
                    let c_args: Vec<_> = cmd
                    .tokens
                    .iter()
                    .map( |x| CString::new( x.1.as_str() ).expect( "CString error" ) )
                    .collect();

                    let c_args: Vec<&CStr> = c_args.iter().map( |x| x.as_c_str() ).collect();
                    let c_envs: Vec<&CStr> = c_envs.iter().map( |x| x.as_c_str() ).collect();
                    match execve( &c_program, &c_args, &c_envs )
                    {
                        Ok( _ ) => {}
                        Err( e ) => match e
                        {
                            nix::Error::ENOEXEC => 
                            { println_stderr!( "cicada: {}: exec format error ( ENOEXEC )", program ); }
                            
                            nix::Error::ENOENT => 
                            { println_stderr!( "cicada: {}: file does not exist", program ); }
                            
                            nix::Error::EACCES => 
                            { println_stderr!( "cicada: {}: Permission denied", program ); }

                            _ => { println_stderr!( "cicada: {}: {:?}", program, e ); }
                        },
                    }

                    process::exit( 1 );
                }

                Ok( ForkResult::Parent { child, .. } ) =>
                {
                    let pid: i32 = child.into();
                    if idx_cmd == 0
                    {
                        *pgid = pid;
                        if sh.has_terminal && options.isatty && !cl.background 
                        { *term_given = shell::give_terminal_to( pid ); }
                    }

                    if options.isatty && !options.capture_output
                    {
                        let _cmd = parsers::parser_line::tokens_to_line( &cmd.tokens );
                        sh.insert_job( *pgid, pid, &_cmd, "Running", cl.background );
                    }

                    if let Some( redirect_from ) = &cmd.redirect_from
                    {
                        if redirect_from.0 == "<<<"
                        {
                            if let Some( fds ) = fds_stdin
                            {
                                unsafe
                                {
                                    libs::close( fds.0 );

                                    let mut f = File::from_raw_fd( fds.1 );
                                    
                                    match f.write_all( redirect_from.1.clone().as_bytes() )
                                    {
                                        Ok( _ ) => {}
                                        Err( e ) => println_stderr!( "cicada: write_all: {}", e ),
                                    }
                                    
                                    match f.write_all( b"\n" )
                                    {
                                        Ok( _ ) => {}
                                        Err( e ) => println_stderr!( "cicada: write_all: {}", e ),
                                    }
                                }
                            }
                        }
                    }
                    
                    if idx_cmd < pipes_count
                    {
                        let fds = pipes[idx_cmd];
                        libs::close( fds.1 );
                    }
                    
                    if idx_cmd > 0
                    {
                        let fds = pipes[idx_cmd - 1];
                        libs::close( fds.0 );
                    }

                    if idx_cmd == pipes_count && options.capture_output
                    {
                        let mut s_out = String::new();
                        let mut s_err = String::new();

                        if let Some( fds ) = fds_capture_stdout
                        {
                            libs::close( fds.1 );

                            let mut f = File::from_raw_fd( fds.0 );
                            match f.read_to_string( &mut s_out )
                            {
                                Ok( _ ) => {}
                                Err( e ) => println_stderr!( "cicada: readstr: {}", e ),
                            }
                        }
                        
                        if let Some( fds ) = fds_capture_stderr
                        {
                            libs::close( fds.1 );
                            let mut f_err = File::from_raw_fd( fds.0 );
                            match f_err.read_to_string( &mut s_err )
                            {
                                Ok( _ ) => {}
                                Err( e ) => println_stderr!( "cicada: readstr: {}", e ),
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

                Err( _ ) =>
                {
                    println_stderr!( "Fork failed" );
                    *cmd_result = CommandResult::error();
                    0
                }
            }
        }
    }
    /// Run a pipeline ( e.g. `echo hi | wc -l` )
    /// returns: ( is-terminal-given, command-result )
    pub fn run_pipeline( sh:&mut shell::Shell, cl:&CommandLine, tty:bool, capture:bool, log_cmd:bool ) -> 
    ( bool, CommandResult )
    {
        unsafe
        {
            let mut term_given = false;
        
            if cl.background && capture
            {
                println_stderr!( "cicada: cannot capture output of background cmd" );
                return ( term_given, CommandResult::error() );
            }

            if let Some( cr ) = try_run_calculator( &cl.line, capture )
            { return ( term_given, cr ); }
            
            if let Some( cr ) = try_run_func( sh, cl, capture, log_cmd )
            { return ( term_given, cr ); }

            if log_cmd { log!( "run: {}", cl.line ); }

            let length = cl.commands.len();
            
            if length == 0
            {
                println!( "cicada: invalid command: cmds with empty length" );
                return ( false, CommandResult::error() );
            }

            let mut pipes = Vec::new();
            let mut errored_pipes = false;
            
            for _ in 0..length - 1
            {
                match pipe()
                {
                    Ok( fds ) => pipes.push( fds ),
                    Err( e ) =>
                    {
                        errored_pipes = true;
                        println_stderr!( "cicada: pipeline1: {}", e );
                        break;
                    }
                }
            }

            if errored_pipes
            {
                for fds in pipes
                {
                    libs::close( fds.0 );
                    libs::close( fds.1 );
                }
                
                return ( false, CommandResult::error() );
            }

            if pipes.len() + 1 != length
            {
                println!( "cicada: invalid command: unmatched pipes count" );
                return ( false, CommandResult::error() );
            }

            let mut pgid: i32 = 0;
            let mut fg_pids: Vec<i32> = Vec::new();

            let isatty = if tty { libc::isatty( 1 ) == 1 } 
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
                    Ok( fds ) => fds_capture_stdout = Some( fds ),
                    Err( e ) =>
                    {
                        println_stderr!( "cicada: pipeline2: {}", e );
                        return ( false, CommandResult::error() );
                    }
                }

                match pipe()
                {
                    Ok( fds ) => fds_capture_stderr = Some( fds ),
                    Err( e ) =>
                    {
                        if let Some( fds ) = fds_capture_stdout
                        {
                            libs::close( fds.0 );
                            libs::close( fds.1 );
                        }

                        println_stderr!( "cicada: pipeline3: {}", e );
                        return ( false, CommandResult::error() );
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

                if child_id > 0 && !cl.background { fg_pids.push( child_id ); }
            }

            if cl.is_single_and_builtin()
            { return ( false, cmd_result ); }

            if cl.background
            {
                if let Some( job ) = sh.get_job_by_gid( pgid )
            { println_stderr!( "[{}] {}", job.id, job.gid ); }
            }

            if !fg_pids.is_empty()
            {
                let _cr = jobc::wait_fg_job( sh, pgid, &fg_pids );
                
                if !capture { cmd_result = _cr; }
            }
            
            ( term_given, cmd_result )
        }
    }
    /// Entry point for non-ttys ( e.g. Cmd-N on MacVim )
    pub fn run_procedures_for_non_tty( sh:&mut Shell )
    {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        match handle.read_to_string( &mut buffer )
        {
            Ok( _ ) =>
            {
                log!( "run non tty command: {}", &buffer );
                run_command_line( sh, &buffer, false, false );
            }
            
            Err( e ) =>
            {
                println!( "cicada: stdin.read_to_string() failed: {:?}", e );
            }
        }
    }
    //  fn run_proc( ... ) -> CommandResult
    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    fn run_procedure( sh:&mut Shell, line:&str, tty:bool, capture:bool ) -> CommandResult
    {
        unsafe
        {   
            let log_cmd = !sh.cmd.starts_with( ' ' );
            match CommandLine::from_line( line, sh )
            {
                Ok( cl ) =>
                {
                    if cl.is_empty()
                    {
                        if !cl.envs.is_empty()
            { set_shell_vars( sh, &cl.envs ); }

                        return CommandResult::new();
                    }

                    let ( term_given, cr ) = core::run_pipeline( sh, &cl, tty, capture, log_cmd );
                    if term_given
                    {
                        unsafe {
                            let gid = libc::getpgid( 0 );
                            shell::give_terminal_to( gid );
                        }
                    }

                    cr
                }
                
                Err( e ) =>
                {
                    println_stderr!( "cicada: {}", e );
                    CommandResult::from_status( 0, 1 )
                }
            }
        }
    }
    
    fn try_run_builtin( sh:&mut Shell, cl:&CommandLine, idx_cmd:usize, capture:bool ) -> Option<CommandResult>
    {
        let capture = capture && idx_cmd + 1 == cl.commands.len();

        if idx_cmd >= cl.commands.len()
        {
            println_stderr!( "unexpected error in try_run_builtin" );
            return None;
        }

        let cmd = &cl.commands[idx_cmd];
        let tokens = cmd.tokens.clone();
        let cname = tokens[0].1.clone();
        
        if cname == "alias" {
            let cr = ffi::alias::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "bg" {
            let cr = ffi::bg::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "cd" {
            let cr = ffi::cd::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "cinfo" {
            let cr = ffi::cinfo::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "exec" {
            let cr = ffi::exec::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "exit" {
            let cr = ffi::exit::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "export" {
            let cr = ffi::export::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "fg" {
            let cr = ffi::fg::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "history" {
            let cr = ffi::history::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "jobs" {
            let cr = ffi::jobs::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "minfd" {
            let cr = ffi::minfd::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "read" {
            let cr = ffi::read::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "set" {
            let cr = ffi::set::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "source" {
            let cr = ffi::source::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "ulimit" {
            let cr = ffi::ulimit::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unalias" {
            let cr = ffi::unalias::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unset" {
            let cr = ffi::unset::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "unpath" {
            let cr = ffi::unpath::run( sh, cl, cmd, capture );
            return Some( cr );
        } else if cname == "vox" {
            let cr = ffi::vox::run( sh, cl, cmd, capture );
            return Some( cr );
        }
        None
    }

    fn run_with_shell( sh:&mut Shell, line:&str ) -> CommandResult
    {
        unsafe
        {
            let ( tokens, envs ) = line_to_tokens( sh, line );

            if tokens.is_empty()
            {
                set_shell_vars( sh, &envs );
                return CommandResult::new();
            }

            match CommandLine::from_line( line, sh )
            {
                Ok( c ) =>
                {
                    let ( term_given, cr ) = run_pipeline( sh, &c, false, true, false );
                    
                    if term_given
                    {
                        let gid = libc::getpgid( 0 );
                        shell::give_terminal_to( gid );    
                    }

                    cr
                }

                Err( e ) =>
                {
                    println_stderr!( "cicada: {}", e );
                    CommandResult::from_status( 0, 1 )
                }
            }
        }

        
    }

    pub fn run( line:&str ) -> CommandResult
    {
        let mut sh = Shell::new();
        run_with_shell( &mut sh, line )
    }
}
/// Additional functionality for numerics.
pub mod num
{
    pub use std::num::{ * };
    /*
    use num_bigint::BigInt;
    use num_bigint::ParseBigIntError;
    */
    pub mod big
    {
        //! A Big integer (signed version: `BigInt`, unsigned version: `BigUint`).
        use ::
        {
            error::{ Error },
            *,
        };

        #[macro_use] mod macros
        {
            macro_rules! forward_val_val_binop
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_val_val_binop_commutative
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            if self.capacity() >= other.capacity() {
                                $imp::$method(self, &other)
                            } else {
                                $imp::$method(other, &self)
                            }
                        }
                    }
                };
            }

            macro_rules! forward_ref_val_binop
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl<'a> $imp<$res> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_val_binop_commutative 
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl<'a> $imp<$res> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(other, self)
                        }
                    }
                };
            }

            macro_rules! forward_val_ref_binop
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl<'a> $imp<&'a $res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(&self, other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_ref_binop
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl<'a, 'b> $imp<&'b $res> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(self.clone(), other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_ref_binop_commutative
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl<'a, 'b> $imp<&'b $res> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            if self.len() >= other.len() {
                                $imp::$method(self.clone(), other)
                            } else {
                                $imp::$method(other.clone(), self)
                            }
                        }
                    }
                };
            }

            macro_rules! forward_val_assign
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        #[inline]
                        fn $method(&mut self, other: $res) {
                            self.$method(&other);
                        }
                    }
                };
            }

            macro_rules! forward_val_assign_scalar
            {
                (impl $imp:ident for $res:ty, $scalar:ty, $method:ident) => {
                    impl $imp<$res> for $scalar {
                        #[inline]
                        fn $method(&mut self, other: $res) {
                            self.$method(&other);
                        }
                    }
                };
            }

            /// Used if val_val_binop is already implemented and the reversed order is required
            macro_rules! forward_scalar_val_val_binop_commutative
            {
                (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) =>
                {
                    impl $imp<$res> for $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(other, self)
                        }
                    }
                };
            }
            
            macro_rules! forward_scalar_val_val_binop_to_ref_val 
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl $imp<$scalar> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $scalar) -> $res {
                            $imp::$method(&self, other)
                        }
                    }

                    impl $imp<$res> for $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_ref_ref_binop_to_ref_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl<'a, 'b> $imp<&'b $scalar> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self, *other)
                        }
                    }

                    impl<'a, 'b> $imp<&'a $res> for &'b $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(*self, other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_val_ref_binop_to_ref_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl<'a> $imp<&'a $scalar> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(&self, *other)
                        }
                    }

                    impl<'a> $imp<$res> for &'a $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(*self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_val_ref_binop_to_val_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl<'a> $imp<&'a $scalar> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self, *other)
                        }
                    }

                    impl<'a> $imp<$res> for &'a $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(*self, other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_ref_val_binop_to_val_val
            {
                (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) => {
                    impl<'a> $imp<$scalar> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $scalar) -> $res {
                            $imp::$method(self.clone(), other)
                        }
                    }

                    impl<'a> $imp<&'a $res> for $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(self, other.clone())
                        }
                    }
                };
            }

            macro_rules! forward_scalar_ref_ref_binop_to_val_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl<'a, 'b> $imp<&'b $scalar> for &'a $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self.clone(), *other)
                        }
                    }

                    impl<'a, 'b> $imp<&'a $res> for &'b $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(*self, other.clone())
                        }
                    }
                };
            }

            macro_rules! promote_scalars
            {
                (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
                    $(
                        forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);

                        impl $imp<$scalar> for $res {
                            type Output = $res;

                            #[cfg_attr(feature = "cargo-clippy", allow(renamed_and_removed_lints))]
                            #[cfg_attr(feature = "cargo-clippy", allow(cast_lossless))]
                            #[inline]
                            fn $method(self, other: $scalar) -> $res {
                                $imp::$method(self, other as $promo)
                            }
                        }

                        impl $imp<$res> for $scalar {
                            type Output = $res;

                            #[cfg_attr(feature = "cargo-clippy", allow(renamed_and_removed_lints))]
                            #[cfg_attr(feature = "cargo-clippy", allow(cast_lossless))]
                            #[inline]
                            fn $method(self, other: $res) -> $res {
                                $imp::$method(self as $promo, other)
                            }
                        }
                    )*
                }
            }

            macro_rules! promote_scalars_assign
            {
                (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
                    $(
                        impl $imp<$scalar> for $res {
                            #[cfg_attr(feature = "cargo-clippy", allow(renamed_and_removed_lints))]
                            #[cfg_attr(feature = "cargo-clippy", allow(cast_lossless))]
                            #[inline]
                            fn $method(&mut self, other: $scalar) {
                                self.$method(other as $promo);
                            }
                        }
                    )*
                }
            }

            macro_rules! promote_unsigned_scalars
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars!(impl $imp<u32> for $res, $method, u8, u16);
                    promote_scalars!(impl $imp<UsizePromotion> for $res, $method, usize);
                }
            }

            macro_rules! promote_unsigned_scalars_assign
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars_assign!(impl $imp<u32> for $res, $method, u8, u16);
                    promote_scalars_assign!(impl $imp<UsizePromotion> for $res, $method, usize);
                }
            }

            macro_rules! promote_signed_scalars
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars!(impl $imp<i32> for $res, $method, i8, i16);
                    promote_scalars!(impl $imp<IsizePromotion> for $res, $method, isize);
                }
            }

            macro_rules! promote_signed_scalars_assign
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars_assign!(impl $imp<i32> for $res, $method, i8, i16);
                    promote_scalars_assign!(impl $imp<IsizePromotion> for $res, $method, isize);
                }
            }
            
            macro_rules! forward_all_binop_to_ref_ref
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop!(impl $imp for $res, $method);
                    forward_val_ref_binop!(impl $imp for $res, $method);
                    forward_ref_val_binop!(impl $imp for $res, $method);
                };
            }
            
            macro_rules! forward_all_binop_to_val_ref
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop!(impl $imp for $res, $method);
                    forward_ref_val_binop!(impl $imp for $res, $method);
                    forward_ref_ref_binop!(impl $imp for $res, $method);
                };
            }
            
            macro_rules! forward_all_binop_to_val_ref_commutative
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop_commutative!(impl $imp for $res, $method);
                    forward_ref_val_binop_commutative!(impl $imp for $res, $method);
                    forward_ref_ref_binop_commutative!(impl $imp for $res, $method);
                };
            }

            macro_rules! forward_all_scalar_binop_to_ref_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_val_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_val_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! forward_all_scalar_binop_to_val_val
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_val_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! forward_all_scalar_binop_to_val_val_commutative
            {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_val_binop_commutative!(impl $imp<$scalar> for $res, $method);
                    forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! promote_all_scalars
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_unsigned_scalars!(impl $imp for $res, $method);
                    promote_signed_scalars!(impl $imp for $res, $method);
                }
            }

            macro_rules! promote_all_scalars_assign
            {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_unsigned_scalars_assign!(impl $imp for $res, $method);
                    promote_signed_scalars_assign!(impl $imp for $res, $method);
                }
            }

            macro_rules! impl_sum_iter_type
            {
                ($res:ty) => {
                    impl<T> Sum<T> for $res
                    where
                        $res: Add<T, Output = $res>,
                    {
                        fn sum<I>(iter: I) -> Self
                        where
                            I: Iterator<Item = T>,
                        {
                            iter.fold(Zero::zero(), <$res>::add)
                        }
                    }
                };
            }

            macro_rules! impl_product_iter_type
            {
                ($res:ty) => {
                    impl<T> Product<T> for $res
                    where
                        $res: Mul<T, Output = $res>,
                    {
                        fn product<I>(iter: I) -> Self
                        where
                            I: Iterator<Item = T>,
                        {
                            iter.fold(One::one(), <$res>::mul)
                        }
                    }
                };
            }
        }

        pub mod bigint
        {
            use ::
            {
                *,
            };
            /*
            use std::ascii::AsciiExt;
            use std::cmp::Ordering::{self, Equal, Greater, Less};
            use std::default::Default;
            use std::fmt;
            use std::iter::{Product, Sum};
            use std::mem;
            use std::ops::{
                Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
                Mul, MulAssign, Neg, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
            };
            use std::str::{self, FromStr};
            use std::{i128, u128};
            use std::{i64, u64};

            #[cfg(feature = "serde")]
            use serde;

            use integer::{Integer, Roots};
            use traits::{
                CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Num, One, Pow, Signed,
                ToPrimitive, Zero,
            };

            use self::Sign::{Minus, NoSign, Plus};

            use super::ParseBigIntError;
            use big_digit::{self, BigDigit, DoubleBigDigit};
            use biguint;
            use biguint::to_str_radix_reversed;
            use biguint::{BigUint, IntDigits};

            use IsizePromotion;
            use UsizePromotion;
            */
            macro_rules! pow_impl
            {
                ($T:ty) => {
                    impl<'a> Pow<$T> for &'a BigInt {
                        type Output = BigInt;

                        #[inline]
                        fn pow(self, rhs: $T) -> BigInt {
                            BigInt::from_biguint(powsign(self.sign, &rhs), (&self.data).pow(rhs))
                        }
                    }

                    impl<'a, 'b> Pow<&'b $T> for &'a BigInt {
                        type Output = BigInt;

                        #[inline]
                        fn pow(self, rhs: &$T) -> BigInt {
                            BigInt::from_biguint(powsign(self.sign, rhs), (&self.data).pow(rhs))
                        }
                    }
                };
            }
            
            macro_rules! bigint_add
            {
                ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) => {
                    match ($a.sign, $b.sign) {
                        (_, NoSign) => $a_owned,
                        (NoSign, _) => $b_owned,
                        // same sign => keep the sign with the sum of magnitudes
                        (Plus, Plus) | (Minus, Minus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                        // opposite signs => keep the sign of the larger with the difference of magnitudes
                        (Plus, Minus) | (Minus, Plus) => match $a.data.cmp(&$b.data) {
                            Less => BigInt::from_biguint($b.sign, $b_data - $a_data),
                            Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                            Equal => Zero::zero(),
                        },
                    }
                };
            }

            macro_rules! impl_to_bigint
            {
                ($T:ty, $from_ty:path) =>
                {
                    impl ToBigInt for $T {
                        #[inline]
                        fn to_bigint(&self) -> Option<BigInt> {
                            $from_ty(*self)
                        }
                    }
                };
            }

            macro_rules! impl_bigint_from_uint
            {
                ($T:ty) => {
                    impl From<$T> for BigInt {
                        #[inline]
                        fn from(n: $T) -> Self {
                            BigInt::from(n as u64)
                        }
                    }
                };
            }

            macro_rules! impl_bigint_from_int
            {
                ($T:ty) => {
                    impl From<$T> for BigInt {
                        #[inline]
                        fn from(n: $T) -> Self {
                            BigInt::from(n as i64)
                        }
                    }
                };
            }
            
            macro_rules! bigint_sub
            {
                ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) =>
                {
                    match ($a.sign, $b.sign) {
                        (_, NoSign) => $a_owned,
                        (NoSign, _) => -$b_owned,
                        (Plus, Minus) | (Minus, Plus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                        (Plus, Plus) | (Minus, Minus) => match $a.data.cmp(&$b.data) {
                            Less => BigInt::from_biguint(-$a.sign, $b_data - $a_data),
                            Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                            Equal => Zero::zero(),
                        },
                    }
                };
            }
            /// A Sign is a `BigInt`'s composing element.
            #[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug, Hash)]
            pub enum Sign
            {
                Minus,
                NoSign,
                Plus,
            }

            impl Neg for Sign
            {
                type Output = Sign;
                /// Negate Sign value.
                #[inline] fn neg(self) -> Sign
                {
                    match self {
                        Minus => Plus,
                        NoSign => NoSign,
                        Plus => Minus,
                    }
                }
            }

            impl Mul<Sign> for Sign
            {
                type Output = Sign;

                #[inline]
                fn mul(self, other: Sign) -> Sign {
                    match (self, other) {
                        (NoSign, _) | (_, NoSign) => NoSign,
                        (Plus, Plus) | (Minus, Minus) => Plus,
                        (Plus, Minus) | (Minus, Plus) => Minus,
                    }
                }
            }

            /// A big signed integer type.
            #[derive(Clone, Debug, Hash)]
            pub struct BigInt 
            {
                sign: Sign,
                data: BigUint,
            }

            /// Return the magnitude of a `BigInt`.
            pub fn magnitude(i: &BigInt) -> &BigUint
            {
                &i.data
            }

            /// Return the owned magnitude of a `BigInt`.
            pub fn into_magnitude(i: BigInt) -> BigUint 
            {
                i.data
            }

            impl PartialEq for BigInt 
            {
                #[inline]
                fn eq(&self, other: &BigInt) -> bool {
                    self.cmp(other) == Equal
                }
            }

            impl Eq for BigInt {}

            impl PartialOrd for BigInt 
            {
                #[inline]
                fn partial_cmp(&self, other: &BigInt) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for BigInt 
            {
                #[inline]
                fn cmp(&self, other: &BigInt) -> Ordering {
                    let scmp = self.sign.cmp(&other.sign);
                    if scmp != Equal {
                        return scmp;
                    }

                    match self.sign {
                        NoSign => Equal,
                        Plus => self.data.cmp(&other.data),
                        Minus => other.data.cmp(&self.data),
                    }
                }
            }

            impl Default for BigInt 
            {
                #[inline]
                fn default() -> BigInt {
                    Zero::zero()
                }
            }

            impl fmt::Display for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "", &self.data.to_str_radix(10))
                }
            }

            impl fmt::Binary for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0b", &self.data.to_str_radix(2))
                }
            }

            impl fmt::Octal for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0o", &self.data.to_str_radix(8))
                }
            }

            impl fmt::LowerHex for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0x", &self.data.to_str_radix(16))
                }
            }

            impl fmt::UpperHex for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    let mut s = self.data.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(!self.is_negative(), "0x", &s)
                }
            }
            
            #[inline] fn negate_carry(a: BigDigit, acc: &mut DoubleBigDigit) -> BigDigit 
            {
                *acc += DoubleBigDigit::from(!a);
                let lo = *acc as BigDigit;
                *acc >>= big_digit::BITS;
                lo
            }

            impl Not for BigInt 
            {
                type Output = BigInt;

                fn not(mut self) -> BigInt {
                    match self.sign {
                        NoSign | Plus => {
                            self.data += 1u32;
                            self.sign = Minus;
                        }
                        Minus => {
                            self.data -= 1u32;
                            self.sign = if self.data.is_zero() { NoSign } else { Plus };
                        }
                    }
                    self
                }
            }

            impl<'a> Not for &'a BigInt 
            {
                type Output = BigInt;

                fn not(self) -> BigInt {
                    match self.sign {
                        NoSign | Plus => BigInt::from_biguint(Minus, &self.data + 1u32),
                        Minus => BigInt::from_biguint(Plus, &self.data - 1u32),
                    }
                }
            }
            
            fn bitand_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
            {
                let mut carry_b = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai &= twos_b;
                }
                debug_assert!(b.len() > a.len() || carry_b == 0);
            }
            
            fn bitand_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
            {
                let mut carry_a = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    *ai = twos_a & bi;
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                if a.len() > b.len() {
                    a.truncate(b.len());
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().cloned());
                }
            }
            
            fn bitand_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
            {
                let mut carry_a = 1;
                let mut carry_b = 1;
                let mut carry_and = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai = negate_carry(twos_a & twos_b, &mut carry_and);
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                debug_assert!(b.len() > a.len() || carry_b == 0);
                if a.len() > b.len() {
                    for ai in a[b.len()..].iter_mut() {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a, &mut carry_and);
                    }
                    debug_assert!(carry_a == 0);
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().map(|&bi| {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        negate_carry(twos_b, &mut carry_and)
                    }));
                    debug_assert!(carry_b == 0);
                }
                if carry_and != 0 {
                    a.push(1);
                }
            }

            forward_val_val_binop!(impl BitAnd for BigInt, bitand);
            forward_ref_val_binop!(impl BitAnd for BigInt, bitand);
            
            impl<'a, 'b> BitAnd<&'b BigInt> for &'a BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn bitand(self, other: &BigInt) -> BigInt {
                    match (self.sign, other.sign) {
                        (NoSign, _) | (_, NoSign) => BigInt::from_slice(NoSign, &[]),
                        (Plus, Plus) => BigInt::from_biguint(Plus, &self.data & &other.data),
                        (Plus, Minus) => self.clone() & other,
                        (Minus, Plus) => other.clone() & self,
                        (Minus, Minus) => {
                            // forward to val-ref, choosing the larger to clone
                            if self.len() >= other.len() {
                                self.clone() & other
                            } else {
                                other.clone() & self
                            }
                        }
                    }
                }
            }

            impl<'a> BitAnd<&'a BigInt> for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn bitand(mut self, other: &BigInt) -> BigInt {
                    self &= other;
                    self
                }
            }

            forward_val_assign!(impl BitAndAssign for BigInt, bitand_assign);

            impl<'a> BitAndAssign<&'a BigInt> for BigInt 
            {
                fn bitand_assign(&mut self, other: &BigInt) {
                    match (self.sign, other.sign) {
                        (NoSign, _) => {}
                        (_, NoSign) => self.assign_from_slice(NoSign, &[]),
                        (Plus, Plus) => {
                            self.data &= &other.data;
                            if self.data.is_zero() {
                                self.sign = NoSign;
                            }
                        }
                        (Plus, Minus) => {
                            bitand_pos_neg(self.digits_mut(), other.digits());
                            self.normalize();
                        }
                        (Minus, Plus) => {
                            bitand_neg_pos(self.digits_mut(), other.digits());
                            self.sign = Plus;
                            self.normalize();
                        }
                        (Minus, Minus) => {
                            bitand_neg_neg(self.digits_mut(), other.digits());
                            self.normalize();
                        }
                    }
                }
            }
            
            fn bitor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit])
            {
                let mut carry_b = 1;
                let mut carry_or = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai = negate_carry(*ai | twos_b, &mut carry_or);
                }
                debug_assert!(b.len() > a.len() || carry_b == 0);
                if a.len() > b.len() {
                    a.truncate(b.len());
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().map(|&bi| {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        negate_carry(twos_b, &mut carry_or)
                    }));
                    debug_assert!(carry_b == 0);
                }
                
                debug_assert!(carry_or == 0);
            }
            
            fn bitor_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
            {
                let mut carry_a = 1;
                let mut carry_or = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    *ai = negate_carry(twos_a | bi, &mut carry_or);
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                if a.len() > b.len() {
                    for ai in a[b.len()..].iter_mut() {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a, &mut carry_or);
                    }
                    debug_assert!(carry_a == 0);
                }
                
                debug_assert!(carry_or == 0);
            }
            
            fn bitor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
            {
                let mut carry_a = 1;
                let mut carry_b = 1;
                let mut carry_or = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai = negate_carry(twos_a | twos_b, &mut carry_or);
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                debug_assert!(b.len() > a.len() || carry_b == 0);
                if a.len() > b.len() {
                    a.truncate(b.len());
                }
                
                debug_assert!(carry_or == 0);
            }

            forward_val_val_binop!(impl BitOr for BigInt, bitor);
            forward_ref_val_binop!(impl BitOr for BigInt, bitor);
            
            impl<'a, 'b> BitOr<&'b BigInt> for &'a BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn bitor(self, other: &BigInt) -> BigInt {
                    match (self.sign, other.sign) {
                        (NoSign, _) => other.clone(),
                        (_, NoSign) => self.clone(),
                        (Plus, Plus) => BigInt::from_biguint(Plus, &self.data | &other.data),
                        (Plus, Minus) => other.clone() | self,
                        (Minus, Plus) => self.clone() | other,
                        (Minus, Minus) => {
                            
                            if self.len() <= other.len() {
                                self.clone() | other
                            } else {
                                other.clone() | self
                            }
                        }
                    }
                }
            }

            impl<'a> BitOr<&'a BigInt> for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn bitor(mut self, other: &BigInt) -> BigInt {
                    self |= other;
                    self
                }
            }

            forward_val_assign!(impl BitOrAssign for BigInt, bitor_assign);

            impl<'a> BitOrAssign<&'a BigInt> for BigInt 
            {
                fn bitor_assign(&mut self, other: &BigInt) {
                    match (self.sign, other.sign) {
                        (_, NoSign) => {}
                        (NoSign, _) => self.assign_from_slice(other.sign, other.digits()),
                        (Plus, Plus) => self.data |= &other.data,
                        (Plus, Minus) => {
                            bitor_pos_neg(self.digits_mut(), other.digits());
                            self.sign = Minus;
                            self.normalize();
                        }
                        (Minus, Plus) => {
                            bitor_neg_pos(self.digits_mut(), other.digits());
                            self.normalize();
                        }
                        (Minus, Minus) => {
                            bitor_neg_neg(self.digits_mut(), other.digits());
                            self.normalize();
                        }
                    }
                }
            }
            
            fn bitxor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit])
            {
                let mut carry_b = 1;
                let mut carry_xor = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                }
                debug_assert!(b.len() > a.len() || carry_b == 0);
                if a.len() > b.len() {
                    for ai in a[b.len()..].iter_mut() {
                        let twos_b = !0;
                        *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                    }
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().map(|&bi| {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        negate_carry(twos_b, &mut carry_xor)
                    }));
                    debug_assert!(carry_b == 0);
                }
                if carry_xor != 0 {
                    a.push(1);
                }
            }
            
            fn bitxor_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit])
            {
                let mut carry_a = 1;
                let mut carry_xor = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    *ai = negate_carry(twos_a ^ bi, &mut carry_xor);
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                if a.len() > b.len() {
                    for ai in a[b.len()..].iter_mut() {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a, &mut carry_xor);
                    }
                    debug_assert!(carry_a == 0);
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().map(|&bi| {
                        let twos_a = !0;
                        negate_carry(twos_a ^ bi, &mut carry_xor)
                    }));
                }
                if carry_xor != 0 {
                    a.push(1);
                }
            }
            
            fn bitxor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit])
            {
                let mut carry_a = 1;
                let mut carry_b = 1;
                for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                    let twos_a = negate_carry(*ai, &mut carry_a);
                    let twos_b = negate_carry(bi, &mut carry_b);
                    *ai = twos_a ^ twos_b;
                }
                debug_assert!(a.len() > b.len() || carry_a == 0);
                debug_assert!(b.len() > a.len() || carry_b == 0);
                if a.len() > b.len() {
                    for ai in a[b.len()..].iter_mut() {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        let twos_b = !0;
                        *ai = twos_a ^ twos_b;
                    }
                    debug_assert!(carry_a == 0);
                } else if b.len() > a.len() {
                    let extra = &b[a.len()..];
                    a.extend(extra.iter().map(|&bi| {
                        let twos_a = !0;
                        let twos_b = negate_carry(bi, &mut carry_b);
                        twos_a ^ twos_b
                    }));
                    debug_assert!(carry_b == 0);
                }
            }

            forward_all_binop_to_val_ref_commutative!(impl BitXor for BigInt, bitxor);

            impl<'a> BitXor<&'a BigInt> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn bitxor(mut self, other: &BigInt) -> BigInt {
                    self ^= other;
                    self
                }
            }

            forward_val_assign!(impl BitXorAssign for BigInt, bitxor_assign);

            impl<'a> BitXorAssign<&'a BigInt> for BigInt
            {
                fn bitxor_assign(&mut self, other: &BigInt) {
                    match (self.sign, other.sign) {
                        (_, NoSign) => {}
                        (NoSign, _) => self.assign_from_slice(other.sign, other.digits()),
                        (Plus, Plus) => {
                            self.data ^= &other.data;
                            if self.data.is_zero() {
                                self.sign = NoSign;
                            }
                        }
                        (Plus, Minus) => {
                            bitxor_pos_neg(self.digits_mut(), other.digits());
                            self.sign = Minus;
                            self.normalize();
                        }
                        (Minus, Plus) => {
                            bitxor_neg_pos(self.digits_mut(), other.digits());
                            self.normalize();
                        }
                        (Minus, Minus) => {
                            bitxor_neg_neg(self.digits_mut(), other.digits());
                            self.sign = Plus;
                            self.normalize();
                        }
                    }
                }
            }

            impl FromStr for BigInt
            {
                type Err = ParseBigIntError;
                #[inline] fn from_str(s: &str) -> Result<BigInt, ParseBigIntError> { BigInt::from_str_radix(s, 10) }
            }

            impl Num for BigInt
            {
                type FromStrRadixErr = ParseBigIntError;

                /// Creates and initializes a BigInt.
                #[inline]
                fn from_str_radix(mut s: &str, radix: u32) -> Result<BigInt, ParseBigIntError> {
                    let sign = if s.starts_with('-') {
                        let tail = &s[1..];
                        if !tail.starts_with('+') {
                            s = tail
                        }
                        Minus
                    } else {
                        Plus
                    };
                    let bu = BigUint::from_str_radix(s, radix)?;
                    Ok(BigInt::from_biguint(sign, bu))
                }
            }

            impl Shl<usize> for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn shl(mut self, rhs: usize) -> BigInt {
                    self <<= rhs;
                    self
                }
            }

            impl<'a> Shl<usize> for &'a BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn shl(self, rhs: usize) -> BigInt {
                    BigInt::from_biguint(self.sign, &self.data << rhs)
                }
            }

            impl ShlAssign<usize> for BigInt 
            {
                #[inline]
                fn shl_assign(&mut self, rhs: usize) {
                    self.data <<= rhs;
                }
            }
            
            fn shr_round_down(i: &BigInt, rhs: usize) -> bool
            {
                i.is_negative()
                    && biguint::trailing_zeros(&i.data)
                        .map(|n| n < rhs)
                        .unwrap_or(false)
            }

            impl Shr<usize> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn shr(mut self, rhs: usize) -> BigInt {
                    self >>= rhs;
                    self
                }
            }

            impl<'a> Shr<usize> for &'a BigInt
            {
                type Output = BigInt;

                #[inline]
                fn shr(self, rhs: usize) -> BigInt {
                    let round_down = shr_round_down(self, rhs);
                    let data = &self.data >> rhs;
                    BigInt::from_biguint(self.sign, if round_down { data + 1u8 } else { data })
                }
            }

            impl ShrAssign<usize> for BigInt 
            {
                #[inline]
                fn shr_assign(&mut self, rhs: usize) {
                    let round_down = shr_round_down(self, rhs);
                    self.data >>= rhs;
                    if round_down {
                        self.data += 1u8;
                    } else if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Zero for BigInt
            {
                #[inline]
                fn zero() -> BigInt {
                    BigInt::from_biguint(NoSign, Zero::zero())
                }

                #[inline]
                fn set_zero(&mut self) {
                    self.data.set_zero();
                    self.sign = NoSign;
                }

                #[inline]
                fn is_zero(&self) -> bool {
                    self.sign == NoSign
                }
            }

            impl One for BigInt
            {
                #[inline]
                fn one() -> BigInt {
                    BigInt::from_biguint(Plus, One::one())
                }

                #[inline]
                fn set_one(&mut self) {
                    self.data.set_one();
                    self.sign = Plus;
                }

                #[inline]
                fn is_one(&self) -> bool {
                    self.sign == Plus && self.data.is_one()
                }
            }

            impl Signed for BigInt 
            {
                #[inline]
                fn abs(&self) -> BigInt {
                    match self.sign {
                        Plus | NoSign => self.clone(),
                        Minus => BigInt::from_biguint(Plus, self.data.clone()),
                    }
                }

                #[inline]
                fn abs_sub(&self, other: &BigInt) -> BigInt {
                    if *self <= *other {
                        Zero::zero()
                    } else {
                        self - other
                    }
                }

                #[inline]
                fn signum(&self) -> BigInt {
                    match self.sign {
                        Plus => BigInt::from_biguint(Plus, One::one()),
                        Minus => BigInt::from_biguint(Minus, One::one()),
                        NoSign => Zero::zero(),
                    }
                }

                #[inline]
                fn is_positive(&self) -> bool {
                    self.sign == Plus
                }

                #[inline]
                fn is_negative(&self) -> bool {
                    self.sign == Minus
                }
            }

            /// Help function for pow.
            #[inline] fn powsign<T: Integer>(sign: Sign, other: &T) -> Sign 
            {
                if other.is_zero() {
                    Plus
                } else if sign != Minus {
                    sign
                } else if other.is_odd() {
                    sign
                } else {
                    -sign
                }
            }

            pow_impl!(u8);
            pow_impl!(u16);
            pow_impl!(u32);
            pow_impl!(u64);
            pow_impl!(usize);
            
            pow_impl!(u128);
            pow_impl!(BigUint);
            
            #[inline] fn i32_abs_as_u32(a: i32) -> u32
            {
                if a == i32::min_value() {
                    a as u32
                } else {
                    a.abs() as u32
                }
            }
            
            #[inline] fn i64_abs_as_u64(a: i64) -> u64
            {
                if a == i64::min_value() {
                    a as u64
                } else {
                    a.abs() as u64
                }
            }
            
            #[inline] fn i128_abs_as_u128(a: i128) -> u128
            {
                if a == i128::min_value() { a as u128 }
                else { a.abs() as u128 }
            }

            impl<'a, 'b> Add<&'b BigInt> for &'a BigInt
            {
                type Output = BigInt;

                #[inline]
                fn add(self, other: &BigInt) -> BigInt {
                    bigint_add!(
                        self,
                        self.clone(),
                        &self.data,
                        other,
                        other.clone(),
                        &other.data
                    )
                }
            }

            impl<'a> Add<BigInt> for &'a BigInt 
            {
                type Output = BigInt;

                #[inline] fn add(self, other: BigInt) -> BigInt
                { bigint_add!(self, self.clone(), &self.data, other, other, other.data) }
            }

            impl<'a> Add<&'a BigInt> for BigInt 
            {
                type Output = BigInt;

                #[inline] fn add(self, other: &BigInt) -> BigInt
                { bigint_add!(self, self, self.data, other, other.clone(), &other.data) }
            }

            impl Add<BigInt> for BigInt 
            {
                type Output = BigInt;
                #[inline] fn add(self, other: BigInt) -> BigInt
                { bigint_add!(self, self, self.data, other, other, other.data) }
            }

            impl<'a> AddAssign<&'a BigInt> for BigInt 
            {
                #[inline] fn add_assign(&mut self, other: &BigInt)
                {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n + other;
                }
            }

            forward_val_assign!(impl AddAssign for BigInt, add_assign);

            promote_all_scalars!(impl Add for BigInt, add);
            promote_all_scalars_assign!(impl AddAssign for BigInt, add_assign);
            forward_all_scalar_binop_to_val_val_commutative!(impl Add<u32> for BigInt, add);
            forward_all_scalar_binop_to_val_val_commutative!(impl Add<u64> for BigInt, add);
            forward_all_scalar_binop_to_val_val_commutative!(impl Add<u128> for BigInt, add);

            impl Add<u32> for BigInt 
            {
                type Output = BigInt;

                #[inline] fn add(self, other: u32) -> BigInt
                {
                    match self.sign
                    {
                        NoSign => From::from(other),
                        Plus => BigInt::from_biguint(Plus, self.data + other),
                        Minus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Less => BigInt::from_biguint(Plus, other - self.data),
                            Greater => BigInt::from_biguint(Minus, self.data - other),
                        },
                    }
                }
            }

            impl AddAssign<u32> for BigInt
            {
                #[inline] fn add_assign(&mut self, other: u32)
                {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n + other;
                }
            }

            impl Add<u64> for BigInt
            {
                type Output = BigInt;

                #[inline] fn add(self, other: u64) -> BigInt
                {
                    match self.sign
                    {
                        NoSign => From::from(other),
                        Plus => BigInt::from_biguint(Plus, self.data + other),
                        Minus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Less => BigInt::from_biguint(Plus, other - self.data),
                            Greater => BigInt::from_biguint(Minus, self.data - other),
                        },
                    }
                }
            }

            impl AddAssign<u64> for BigInt 
            {
                #[inline] fn add_assign(&mut self, other: u64)
                {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n + other;
                }
            }
            
            impl Add<u128> for BigInt 
            {
                type Output = BigInt;

                #[inline] fn add(self, other: u128) -> BigInt
                {
                    match self.sign {
                        NoSign => From::from(other),
                        Plus => BigInt::from_biguint(Plus, self.data + other),
                        Minus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Less => BigInt::from_biguint(Plus, other - self.data),
                            Greater => BigInt::from_biguint(Minus, self.data - other),
                        },
                    }
                }
            }
            
            impl AddAssign<u128> for BigInt 
            {
                #[inline]
                fn add_assign(&mut self, other: u128) {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n + other;
                }
            }

            forward_all_scalar_binop_to_val_val_commutative!(impl Add<i32> for BigInt, add);
            forward_all_scalar_binop_to_val_val_commutative!(impl Add<i64> for BigInt, add);
            forward_all_scalar_binop_to_val_val_commutative!(impl Add<i128> for BigInt, add);

            impl Add<i32> for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn add(self, other: i32) -> BigInt {
                    if other >= 0 {
                        self + other as u32
                    } else {
                        self - i32_abs_as_u32(other)
                    }
                }
            }

            impl AddAssign<i32> for BigInt 
            {
                #[inline]
                fn add_assign(&mut self, other: i32) {
                    if other >= 0 {
                        *self += other as u32;
                    } else {
                        *self -= i32_abs_as_u32(other);
                    }
                }
            }

            impl Add<i64> for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn add(self, other: i64) -> BigInt {
                    if other >= 0 {
                        self + other as u64
                    } else {
                        self - i64_abs_as_u64(other)
                    }
                }
            }

            impl AddAssign<i64> for BigInt
            {
                #[inline] fn add_assign(&mut self, other: i64)
                {
                    if other >= 0 {
                        *self += other as u64;
                    } else {
                        *self -= i64_abs_as_u64(other);
                    }
                }
            }
            
            impl Add<i128> for BigInt
            {
                type Output = BigInt;

                #[inline] fn add(self, other: i128) -> BigInt
                {
                    if other >= 0 {
                        self + other as u128
                    } else {
                        self - i128_abs_as_u128(other)
                    }
                }
            }
            
            impl AddAssign<i128> for BigInt 
            {
                #[inline]
                fn add_assign(&mut self, other: i128) {
                    if other >= 0 {
                        *self += other as u128;
                    } else {
                        *self -= i128_abs_as_u128(other);
                    }
                }
            }

            impl<'a, 'b> Sub<&'b BigInt> for &'a BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: &BigInt) -> BigInt {
                    bigint_sub!(
                        self,
                        self.clone(),
                        &self.data,
                        other,
                        other.clone(),
                        &other.data
                    )
                }
            }

            impl<'a> Sub<BigInt> for &'a BigInt
            {
                type Output = BigInt;

                #[inline] fn sub(self, other: BigInt) -> BigInt
                {
                    bigint_sub!(self, self.clone(), &self.data, other, other, other.data)
                }
            }

            impl<'a> Sub<&'a BigInt> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: &BigInt) -> BigInt {
                    bigint_sub!(self, self, self.data, other, other.clone(), &other.data)
                }
            }

            impl Sub<BigInt> for BigInt
            {
                type Output = BigInt;

                #[inline] fn sub(self, other: BigInt) -> BigInt
                { bigint_sub!(self, self, self.data, other, other, other.data) }
            }

            impl<'a> SubAssign<&'a BigInt> for BigInt
            {
                #[inline] fn sub_assign(&mut self, other: &BigInt)
                {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n - other;
                }
            }

            forward_val_assign!(impl SubAssign for BigInt, sub_assign);

            promote_all_scalars!(impl Sub for BigInt, sub);
            promote_all_scalars_assign!(impl SubAssign for BigInt, sub_assign);
            forward_all_scalar_binop_to_val_val!(impl Sub<u32> for BigInt, sub);
            forward_all_scalar_binop_to_val_val!(impl Sub<u64> for BigInt, sub);
            forward_all_scalar_binop_to_val_val!(impl Sub<u128> for BigInt, sub);

            impl Sub<u32> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: u32) -> BigInt {
                    match self.sign {
                        NoSign => BigInt::from_biguint(Minus, From::from(other)),
                        Minus => BigInt::from_biguint(Minus, self.data + other),
                        Plus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Greater => BigInt::from_biguint(Plus, self.data - other),
                            Less => BigInt::from_biguint(Minus, other - self.data),
                        },
                    }
                }
            }

            impl SubAssign<u32> for BigInt
            {
                #[inline]
                fn sub_assign(&mut self, other: u32) {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n - other;
                }
            }

            impl Sub<BigInt> for u32
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    -(other - self)
                }
            }

            impl Sub<BigInt> for u64
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    -(other - self)
                }
            }
            
            impl Sub<BigInt> for u128
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    -(other - self)
                }
            }

            impl Sub<u64> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: u64) -> BigInt {
                    match self.sign {
                        NoSign => BigInt::from_biguint(Minus, From::from(other)),
                        Minus => BigInt::from_biguint(Minus, self.data + other),
                        Plus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Greater => BigInt::from_biguint(Plus, self.data - other),
                            Less => BigInt::from_biguint(Minus, other - self.data),
                        },
                    }
                }
            }
            
            impl SubAssign<u64> for BigInt
            {
                #[inline]
                fn sub_assign(&mut self, other: u64) {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n - other;
                }
            }
            
            impl Sub<u128> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: u128) -> BigInt {
                    match self.sign {
                        NoSign => BigInt::from_biguint(Minus, From::from(other)),
                        Minus => BigInt::from_biguint(Minus, self.data + other),
                        Plus => match self.data.cmp(&From::from(other)) {
                            Equal => Zero::zero(),
                            Greater => BigInt::from_biguint(Plus, self.data - other),
                            Less => BigInt::from_biguint(Minus, other - self.data),
                        },
                    }
                }
            }
            
            impl SubAssign<u128> for BigInt
            {
                #[inline]
                fn sub_assign(&mut self, other: u128) {
                    let n = mem::replace(self, BigInt::zero());
                    *self = n - other;
                }
            }

            forward_all_scalar_binop_to_val_val!(impl Sub<i32> for BigInt, sub);
            forward_all_scalar_binop_to_val_val!(impl Sub<i64> for BigInt, sub);
            forward_all_scalar_binop_to_val_val!(impl Sub<i128> for BigInt, sub);

            impl Sub<i32> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: i32) -> BigInt {
                    if other >= 0 {
                        self - other as u32
                    } else {
                        self + i32_abs_as_u32(other)
                    }
                }
            }

            impl SubAssign<i32> for BigInt
            {
                #[inline]
                fn sub_assign(&mut self, other: i32) {
                    if other >= 0 {
                        *self -= other as u32;
                    } else {
                        *self += i32_abs_as_u32(other);
                    }
                }
            }

            impl Sub<BigInt> for i32
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u32 - other
                    } else {
                        -other - i32_abs_as_u32(self)
                    }
                }
            }

            impl Sub<i64> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: i64) -> BigInt {
                    if other >= 0 {
                        self - other as u64
                    } else {
                        self + i64_abs_as_u64(other)
                    }
                }
            }

            impl SubAssign<i64> for BigInt
            {
                #[inline]
                fn sub_assign(&mut self, other: i64) {
                    if other >= 0 {
                        *self -= other as u64;
                    } else {
                        *self += i64_abs_as_u64(other);
                    }
                }
            }

            impl Sub<BigInt> for i64
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u64 - other
                    } else {
                        -other - i64_abs_as_u64(self)
                    }
                }
            }
            
            impl Sub<i128> for BigInt
            {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: i128) -> BigInt {
                    if other >= 0 {
                        self - other as u128
                    } else {
                        self + i128_abs_as_u128(other)
                    }
                }
            }
            
            impl SubAssign<i128> for BigInt {
                #[inline]
                fn sub_assign(&mut self, other: i128) {
                    if other >= 0 {
                        *self -= other as u128;
                    } else {
                        *self += i128_abs_as_u128(other);
                    }
                }
            }
            
            impl Sub<BigInt> for i128 {
                type Output = BigInt;

                #[inline]
                fn sub(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u128 - other
                    } else {
                        -other - i128_abs_as_u128(self)
                    }
                }
            }

            forward_all_binop_to_ref_ref!(impl Mul for BigInt, mul);

            impl<'a, 'b> Mul<&'b BigInt> for &'a BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: &BigInt) -> BigInt {
                    BigInt::from_biguint(self.sign * other.sign, &self.data * &other.data)
                }
            }

            impl<'a> MulAssign<&'a BigInt> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: &BigInt) {
                    *self = &*self * other;
                }
            }
            forward_val_assign!(impl MulAssign for BigInt, mul_assign);

            promote_all_scalars!(impl Mul for BigInt, mul);
            promote_all_scalars_assign!(impl MulAssign for BigInt, mul_assign);
            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u32> for BigInt, mul);
            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u64> for BigInt, mul);
            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u128> for BigInt, mul);

            impl Mul<u32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: u32) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data * other)
                }
            }

            impl MulAssign<u32> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: u32) {
                    self.data *= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Mul<u64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: u64) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data * other)
                }
            }

            impl MulAssign<u64> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: u64) {
                    self.data *= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }
            
            impl Mul<u128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: u128) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data * other)
                }
            }
            
            impl MulAssign<u128> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: u128) {
                    self.data *= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i32> for BigInt, mul);
            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i64> for BigInt, mul);
            forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i128> for BigInt, mul);

            impl Mul<i32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: i32) -> BigInt {
                    if other >= 0 {
                        self * other as u32
                    } else {
                        -(self * i32_abs_as_u32(other))
                    }
                }
            }

            impl MulAssign<i32> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: i32) {
                    if other >= 0 {
                        *self *= other as u32;
                    } else {
                        self.sign = -self.sign;
                        *self *= i32_abs_as_u32(other);
                    }
                }
            }

            impl Mul<i64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: i64) -> BigInt {
                    if other >= 0 {
                        self * other as u64
                    } else {
                        -(self * i64_abs_as_u64(other))
                    }
                }
            }

            impl MulAssign<i64> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: i64) {
                    if other >= 0 {
                        *self *= other as u64;
                    } else {
                        self.sign = -self.sign;
                        *self *= i64_abs_as_u64(other);
                    }
                }
            }
            
            impl Mul<i128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn mul(self, other: i128) -> BigInt {
                    if other >= 0 {
                        self * other as u128
                    } else {
                        -(self * i128_abs_as_u128(other))
                    }
                }
            }
            
            impl MulAssign<i128> for BigInt {
                #[inline]
                fn mul_assign(&mut self, other: i128) {
                    if other >= 0 {
                        *self *= other as u128;
                    } else {
                        self.sign = -self.sign;
                        *self *= i128_abs_as_u128(other);
                    }
                }
            }

            forward_all_binop_to_ref_ref!(impl Div for BigInt, div);

            impl<'a, 'b> Div<&'b BigInt> for &'a BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: &BigInt) -> BigInt {
                    let (q, _) = self.div_rem(other);
                    q
                }
            }

            impl<'a> DivAssign<&'a BigInt> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: &BigInt) {
                    *self = &*self / other;
                }
            }
            forward_val_assign!(impl DivAssign for BigInt, div_assign);

            promote_all_scalars!(impl Div for BigInt, div);
            promote_all_scalars_assign!(impl DivAssign for BigInt, div_assign);
            forward_all_scalar_binop_to_val_val!(impl Div<u32> for BigInt, div);
            forward_all_scalar_binop_to_val_val!(impl Div<u64> for BigInt, div);
            forward_all_scalar_binop_to_val_val!(impl Div<u128> for BigInt, div);

            impl Div<u32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: u32) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data / other)
                }
            }

            impl DivAssign<u32> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: u32) {
                    self.data /= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Div<BigInt> for u32 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(other.sign, self / other.data)
                }
            }

            impl Div<u64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: u64) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data / other)
                }
            }

            impl DivAssign<u64> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: u64) {
                    self.data /= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Div<BigInt> for u64 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(other.sign, self / other.data)
                }
            }

            
            impl Div<u128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: u128) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data / other)
                }
            }
            impl DivAssign<u128> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: u128) {
                    self.data /= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            
            impl Div<BigInt> for u128 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(other.sign, self / other.data)
                }
            }

            forward_all_scalar_binop_to_val_val!(impl Div<i32> for BigInt, div);
            forward_all_scalar_binop_to_val_val!(impl Div<i64> for BigInt, div);
            
            forward_all_scalar_binop_to_val_val!(impl Div<i128> for BigInt, div);

            impl Div<i32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: i32) -> BigInt {
                    if other >= 0 {
                        self / other as u32
                    } else {
                        -(self / i32_abs_as_u32(other))
                    }
                }
            }

            impl DivAssign<i32> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: i32) {
                    if other >= 0 {
                        *self /= other as u32;
                    } else {
                        self.sign = -self.sign;
                        *self /= i32_abs_as_u32(other);
                    }
                }
            }

            impl Div<BigInt> for i32 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u32 / other
                    } else {
                        -(i32_abs_as_u32(self) / other)
                    }
                }
            }

            impl Div<i64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: i64) -> BigInt {
                    if other >= 0 {
                        self / other as u64
                    } else {
                        -(self / i64_abs_as_u64(other))
                    }
                }
            }

            impl DivAssign<i64> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: i64) {
                    if other >= 0 {
                        *self /= other as u64;
                    } else {
                        self.sign = -self.sign;
                        *self /= i64_abs_as_u64(other);
                    }
                }
            }

            impl Div<BigInt> for i64 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u64 / other
                    } else {
                        -(i64_abs_as_u64(self) / other)
                    }
                }
            }

            
            impl Div<i128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn div(self, other: i128) -> BigInt {
                    if other >= 0 {
                        self / other as u128
                    } else {
                        -(self / i128_abs_as_u128(other))
                    }
                }
            }

            
            impl DivAssign<i128> for BigInt {
                #[inline]
                fn div_assign(&mut self, other: i128) {
                    if other >= 0 {
                        *self /= other as u128;
                    } else {
                        self.sign = -self.sign;
                        *self /= i128_abs_as_u128(other);
                    }
                }
            }

            
            impl Div<BigInt> for i128 {
                type Output = BigInt;

                #[inline]
                fn div(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u128 / other
                    } else {
                        -(i128_abs_as_u128(self) / other)
                    }
                }
            }

            forward_all_binop_to_ref_ref!(impl Rem for BigInt, rem);

            impl<'a, 'b> Rem<&'b BigInt> for &'a BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: &BigInt) -> BigInt {
                    let (_, r) = self.div_rem(other);
                    r
                }
            }

            impl<'a> RemAssign<&'a BigInt> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: &BigInt) {
                    *self = &*self % other;
                }
            }
            forward_val_assign!(impl RemAssign for BigInt, rem_assign);

            promote_all_scalars!(impl Rem for BigInt, rem);
            promote_all_scalars_assign!(impl RemAssign for BigInt, rem_assign);
            forward_all_scalar_binop_to_val_val!(impl Rem<u32> for BigInt, rem);
            forward_all_scalar_binop_to_val_val!(impl Rem<u64> for BigInt, rem);
            
            forward_all_scalar_binop_to_val_val!(impl Rem<u128> for BigInt, rem);

            impl Rem<u32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: u32) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data % other)
                }
            }

            impl RemAssign<u32> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: u32) {
                    self.data %= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Rem<BigInt> for u32 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(Plus, self % other.data)
                }
            }

            impl Rem<u64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: u64) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data % other)
                }
            }

            impl RemAssign<u64> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: u64) {
                    self.data %= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            impl Rem<BigInt> for u64 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(Plus, self % other.data)
                }
            }

            
            impl Rem<u128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: u128) -> BigInt {
                    BigInt::from_biguint(self.sign, self.data % other)
                }
            }

            
            impl RemAssign<u128> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: u128) {
                    self.data %= other;
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
            }

            
            impl Rem<BigInt> for u128 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    BigInt::from_biguint(Plus, self % other.data)
                }
            }

            forward_all_scalar_binop_to_val_val!(impl Rem<i32> for BigInt, rem);
            forward_all_scalar_binop_to_val_val!(impl Rem<i64> for BigInt, rem);
            
            forward_all_scalar_binop_to_val_val!(impl Rem<i128> for BigInt, rem);

            impl Rem<i32> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: i32) -> BigInt {
                    if other >= 0 {
                        self % other as u32
                    } else {
                        self % i32_abs_as_u32(other)
                    }
                }
            }

            impl RemAssign<i32> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: i32) {
                    if other >= 0 {
                        *self %= other as u32;
                    } else {
                        *self %= i32_abs_as_u32(other);
                    }
                }
            }

            impl Rem<BigInt> for i32 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u32 % other
                    } else {
                        -(i32_abs_as_u32(self) % other)
                    }
                }
            }

            impl Rem<i64> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: i64) -> BigInt {
                    if other >= 0 {
                        self % other as u64
                    } else {
                        self % i64_abs_as_u64(other)
                    }
                }
            }

            impl RemAssign<i64> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: i64) {
                    if other >= 0 {
                        *self %= other as u64;
                    } else {
                        *self %= i64_abs_as_u64(other);
                    }
                }
            }

            impl Rem<BigInt> for i64 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u64 % other
                    } else {
                        -(i64_abs_as_u64(self) % other)
                    }
                }
            }

            
            impl Rem<i128> for BigInt {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: i128) -> BigInt {
                    if other >= 0 {
                        self % other as u128
                    } else {
                        self % i128_abs_as_u128(other)
                    }
                }
            }
            
            impl RemAssign<i128> for BigInt {
                #[inline]
                fn rem_assign(&mut self, other: i128) {
                    if other >= 0 {
                        *self %= other as u128;
                    } else {
                        *self %= i128_abs_as_u128(other);
                    }
                }
            }
            
            impl Rem<BigInt> for i128 {
                type Output = BigInt;

                #[inline]
                fn rem(self, other: BigInt) -> BigInt {
                    if self >= 0 {
                        self as u128 % other
                    } else {
                        -(i128_abs_as_u128(self) % other)
                    }
                }
            }

            impl Neg for BigInt {
                type Output = BigInt;

                #[inline]
                fn neg(mut self) -> BigInt {
                    self.sign = -self.sign;
                    self
                }
            }

            impl<'a> Neg for &'a BigInt {
                type Output = BigInt;

                #[inline]
                fn neg(self) -> BigInt {
                    -self.clone()
                }
            }

            impl CheckedAdd for BigInt {
                #[inline]
                fn checked_add(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self.add(v))
                }
            }

            impl CheckedSub for BigInt {
                #[inline]
                fn checked_sub(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self.sub(v))
                }
            }

            impl CheckedMul for BigInt {
                #[inline]
                fn checked_mul(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self.mul(v))
                }
            }

            impl CheckedDiv for BigInt {
                #[inline]
                fn checked_div(&self, v: &BigInt) -> Option<BigInt> {
                    if v.is_zero() {
                        return None;
                    }
                    Some(self.div(v))
                }
            }

            impl Integer for BigInt {
                #[inline]
                fn div_rem(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // r.sign == self.sign
                    let (d_ui, r_ui) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from_biguint(self.sign, d_ui);
                    let r = BigInt::from_biguint(self.sign, r_ui);
                    if other.is_negative() {
                        (-d, r)
                    } else {
                        (d, r)
                    }
                }

                #[inline]
                fn div_floor(&self, other: &BigInt) -> BigInt {
                    let (d, _) = self.div_mod_floor(other);
                    d
                }

                #[inline]
                fn mod_floor(&self, other: &BigInt) -> BigInt {
                    let (_, m) = self.div_mod_floor(other);
                    m
                }

                fn div_mod_floor(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // m.sign == other.sign
                    let (d_ui, m_ui) = self.data.div_rem(&other.data);
                    let d = BigInt::from_biguint(Plus, d_ui);
                    let m = BigInt::from_biguint(Plus, m_ui);
                    let one: BigInt = One::one();
                    match (self.sign, other.sign) {
                        (_, NoSign) => panic!(),
                        (Plus, Plus) | (NoSign, Plus) => (d, m),
                        (Plus, Minus) | (NoSign, Minus) => {
                            if m.is_zero() {
                                (-d, Zero::zero())
                            } else {
                                (-d - one, m + other)
                            }
                        }
                        (Minus, Plus) => {
                            if m.is_zero() {
                                (-d, Zero::zero())
                            } else {
                                (-d - one, other - m)
                            }
                        }
                        (Minus, Minus) => (d, -m),
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                ///
                /// The result is always positive.
                #[inline]
                fn gcd(&self, other: &BigInt) -> BigInt {
                    BigInt::from_biguint(Plus, self.data.gcd(&other.data))
                }

                /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                #[inline]
                fn lcm(&self, other: &BigInt) -> BigInt {
                    BigInt::from_biguint(Plus, self.data.lcm(&other.data))
                }

                /// Deprecated, use `is_multiple_of` instead.
                #[inline]
                fn divides(&self, other: &BigInt) -> bool {
                    self.is_multiple_of(other)
                }

                /// Returns `true` if the number is a multiple of `other`.
                #[inline]
                fn is_multiple_of(&self, other: &BigInt) -> bool {
                    self.data.is_multiple_of(&other.data)
                }

                /// Returns `true` if the number is divisible by `2`.
                #[inline]
                fn is_even(&self) -> bool {
                    self.data.is_even()
                }

                /// Returns `true` if the number is not divisible by `2`.
                #[inline]
                fn is_odd(&self) -> bool {
                    self.data.is_odd()
                }
            }

            impl Roots for BigInt {
                fn nth_root(&self, n: u32) -> Self {
                    assert!(
                        !(self.is_negative() && n.is_even()),
                        "root of degree {} is imaginary",
                        n
                    );

                    BigInt::from_biguint(self.sign, self.data.nth_root(n))
                }

                fn sqrt(&self) -> Self {
                    assert!(!self.is_negative(), "square root is imaginary");

                    BigInt::from_biguint(self.sign, self.data.sqrt())
                }

                fn cbrt(&self) -> Self {
                    BigInt::from_biguint(self.sign, self.data.cbrt())
                }
            }

            impl ToPrimitive for BigInt {
                #[inline]
                fn to_i64(&self) -> Option<i64> {
                    match self.sign {
                        Plus => self.data.to_i64(),
                        NoSign => Some(0),
                        Minus => self.data.to_u64().and_then(|n| {
                            let m: u64 = 1 << 63;
                            if n < m {
                                Some(-(n as i64))
                            } else if n == m {
                                Some(i64::MIN)
                            } else {
                                None
                            }
                        }),
                    }
                }

                #[inline]
                
                fn to_i128(&self) -> Option<i128> {
                    match self.sign {
                        Plus => self.data.to_i128(),
                        NoSign => Some(0),
                        Minus => self.data.to_u128().and_then(|n| {
                            let m: u128 = 1 << 127;
                            if n < m {
                                Some(-(n as i128))
                            } else if n == m {
                                Some(i128::MIN)
                            } else {
                                None
                            }
                        }),
                    }
                }

                #[inline]
                fn to_u64(&self) -> Option<u64> {
                    match self.sign {
                        Plus => self.data.to_u64(),
                        NoSign => Some(0),
                        Minus => None,
                    }
                }

                #[inline]
                
                fn to_u128(&self) -> Option<u128> {
                    match self.sign {
                        Plus => self.data.to_u128(),
                        NoSign => Some(0),
                        Minus => None,
                    }
                }

                #[inline]
                fn to_f32(&self) -> Option<f32> {
                    self.data
                        .to_f32()
                        .map(|n| if self.sign == Minus { -n } else { n })
                }

                #[inline]
                fn to_f64(&self) -> Option<f64> {
                    self.data
                        .to_f64()
                        .map(|n| if self.sign == Minus { -n } else { n })
                }
            }

            impl FromPrimitive for BigInt {
                #[inline]
                fn from_i64(n: i64) -> Option<BigInt> {
                    Some(BigInt::from(n))
                }

                #[inline]
                
                fn from_i128(n: i128) -> Option<BigInt> {
                    Some(BigInt::from(n))
                }

                #[inline]
                fn from_u64(n: u64) -> Option<BigInt> {
                    Some(BigInt::from(n))
                }

                #[inline]
                
                fn from_u128(n: u128) -> Option<BigInt> {
                    Some(BigInt::from(n))
                }

                #[inline]
                fn from_f64(n: f64) -> Option<BigInt> {
                    if n >= 0.0 {
                        BigUint::from_f64(n).map(|x| BigInt::from_biguint(Plus, x))
                    } else {
                        BigUint::from_f64(-n).map(|x| BigInt::from_biguint(Minus, x))
                    }
                }
            }

            impl From<i64> for BigInt {
                #[inline]
                fn from(n: i64) -> Self {
                    if n >= 0 {
                        BigInt::from(n as u64)
                    } else {
                        let u = u64::MAX - (n as u64) + 1;
                        BigInt {
                            sign: Minus,
                            data: BigUint::from(u),
                        }
                    }
                }
            }

            
            impl From<i128> for BigInt {
                #[inline]
                fn from(n: i128) -> Self {
                    if n >= 0 {
                        BigInt::from(n as u128)
                    } else {
                        let u = u128::MAX - (n as u128) + 1;
                        BigInt {
                            sign: Minus,
                            data: BigUint::from(u),
                        }
                    }
                }
            }

            impl_bigint_from_int!(i8);
            impl_bigint_from_int!(i16);
            impl_bigint_from_int!(i32);
            impl_bigint_from_int!(isize);

            impl From<u64> for BigInt {
                #[inline]
                fn from(n: u64) -> Self {
                    if n > 0 {
                        BigInt {
                            sign: Plus,
                            data: BigUint::from(n),
                        }
                    } else {
                        BigInt::zero()
                    }
                }
            }

            
            impl From<u128> for BigInt {
                #[inline]
                fn from(n: u128) -> Self {
                    if n > 0 {
                        BigInt {
                            sign: Plus,
                            data: BigUint::from(n),
                        }
                    } else {
                        BigInt::zero()
                    }
                }
            }

            impl_bigint_from_uint!(u8);
            impl_bigint_from_uint!(u16);
            impl_bigint_from_uint!(u32);
            impl_bigint_from_uint!(usize);

            impl From<BigUint> for BigInt {
                #[inline]
                fn from(n: BigUint) -> Self {
                    if n.is_zero() {
                        BigInt::zero()
                    } else {
                        BigInt {
                            sign: Plus,
                            data: n,
                        }
                    }
                }
            }

            impl IntDigits for BigInt {
                #[inline]
                fn digits(&self) -> &[BigDigit] {
                    self.data.digits()
                }
                #[inline]
                fn digits_mut(&mut self) -> &mut Vec<BigDigit> {
                    self.data.digits_mut()
                }
                #[inline]
                fn normalize(&mut self) {
                    self.data.normalize();
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
                #[inline]
                fn capacity(&self) -> usize {
                    self.data.capacity()
                }
                #[inline]
                fn len(&self) -> usize {
                    self.data.len()
                }
            }
            /// A generic trait for converting a value to a `BigInt`.
            pub trait ToBigInt
            {
                /// Converts the value of `self` to a `BigInt`.
                fn to_bigint(&self) -> Option<BigInt>;
            }

            impl ToBigInt for BigInt {
                #[inline]
                fn to_bigint(&self) -> Option<BigInt> {
                    Some(self.clone())
                }
            }

            impl ToBigInt for BigUint {
                #[inline]
                fn to_bigint(&self) -> Option<BigInt> {
                    if self.is_zero() {
                        Some(Zero::zero())
                    } else {
                        Some(BigInt {
                            sign: Plus,
                            data: self.clone(),
                        })
                    }
                }
            }

            impl biguint::ToBigUint for BigInt
            {
                #[inline] fn to_biguint(&self) -> Option<BigUint>
                {
                    match self.sign() {
                        Plus => Some(self.data.clone()),
                        NoSign => Some(Zero::zero()),
                        Minus => None,
                    }
                }
            }

            impl_to_bigint!(isize, FromPrimitive::from_isize);
            impl_to_bigint!(i8, FromPrimitive::from_i8);
            impl_to_bigint!(i16, FromPrimitive::from_i16);
            impl_to_bigint!(i32, FromPrimitive::from_i32);
            impl_to_bigint!(i64, FromPrimitive::from_i64);
            impl_to_bigint!(i128, FromPrimitive::from_i128);

            impl_to_bigint!(usize, FromPrimitive::from_usize);
            impl_to_bigint!(u8, FromPrimitive::from_u8);
            impl_to_bigint!(u16, FromPrimitive::from_u16);
            impl_to_bigint!(u32, FromPrimitive::from_u32);
            impl_to_bigint!(u64, FromPrimitive::from_u64);
            impl_to_bigint!(u128, FromPrimitive::from_u128);

            impl_to_bigint!(f32, FromPrimitive::from_f32);
            impl_to_bigint!(f64, FromPrimitive::from_f64);

            impl BigInt
            {
                /// Creates and initializes a BigInt.
                #[inline] pub fn new(sign: Sign, digits: Vec<u32>) -> BigInt
                { BigInt::from_biguint(sign, BigUint::new(digits)) }
                /// Creates and initializes a `BigInt`.
                #[inline] pub fn from_biguint(mut sign: Sign, mut data: BigUint) -> BigInt
                {
                    if sign == NoSign {
                        data.assign_from_slice(&[]);
                    } else if data.is_zero() {
                        sign = NoSign;
                    }

                    BigInt {
                        sign: sign,
                        data: data,
                    }
                }
                /// Creates and initializes a `BigInt`.
                #[inline] pub fn from_slice(sign: Sign, slice: &[u32]) -> BigInt
                { BigInt::from_biguint(sign, BigUint::from_slice(slice)) }
                /// Reinitializes a `BigInt`.
                #[inline] pub fn assign_from_slice(&mut self, sign: Sign, slice: &[u32])
                {
                    if sign == NoSign {
                        self.data.assign_from_slice(&[]);
                        self.sign = NoSign;
                    } else {
                        self.data.assign_from_slice(slice);
                        self.sign = match self.data.is_zero() {
                            true => NoSign,
                            false => sign,
                        }
                    }
                }
                /// Creates and initializes a `BigInt`.
                #[inline] pub fn from_bytes_be(sign: Sign, bytes: &[u8]) -> BigInt 
                { BigInt::from_biguint(sign, BigUint::from_bytes_be(bytes)) }
                /// Creates and initializes a `BigInt`.
                #[inline] pub fn from_bytes_le(sign: Sign, bytes: &[u8]) -> BigInt
                { BigInt::from_biguint(sign, BigUint::from_bytes_le(bytes)) }
                /// Creates and inits a `BigInt` from an array of bytes in two's complement binary representation.
                #[inline] pub fn from_signed_bytes_be(digits: &[u8]) -> BigInt
                {
                    let sign = match digits.first() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::zero(),
                    };

                    if sign == Sign::Minus {
                        // two's-complement the content to retrieve the magnitude
                        let mut digits = Vec::from(digits);
                        twos_complement_be(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(&*digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(digits))
                    }
                }
                /// Creates and initializes a `BigInt` from an array of bytes in two's complement.
                #[inline] pub fn from_signed_bytes_le(digits: &[u8]) -> BigInt
                {
                    let sign = match digits.last() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::zero(),
                    };

                    if sign == Sign::Minus {
                        // two's-complement the content to retrieve the magnitude
                        let mut digits = Vec::from(digits);
                        twos_complement_le(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(&*digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(digits))
                    }
                }
                /// Creates and initializes a `BigInt`.
                #[inline] pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigInt>
                {
                    str::from_utf8(buf)
                    .ok()
                    .and_then(|s| BigInt::from_str_radix(s, radix).ok())
                }
                /// Creates and initializes a `BigInt`.
                pub fn from_radix_be(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt>
                { BigUint::from_radix_be(buf, radix).map(|u| BigInt::from_biguint(sign, u)) }
                /// Creates and initializes a `BigInt`.
                pub fn from_radix_le(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt>
                { BigUint::from_radix_le(buf, radix).map(|u| BigInt::from_biguint(sign, u)) }
                /// Returns the sign and the byte representation of the `BigInt` in big-endian byte order.
                #[inline] pub fn to_bytes_be(&self) -> (Sign, Vec<u8>) { (self.sign, self.data.to_bytes_be()) }
                /// Returns the sign and the byte representation of the `BigInt` in little-endian byte order.
                #[inline] pub fn to_bytes_le(&self) -> (Sign, Vec<u8>) { (self.sign, self.data.to_bytes_le()) }
                /// Returns the sign and the `u32` digits representation
                /// of the `BigInt` ordered least significant digit first.
                #[inline] pub fn to_u32_digits(&self) -> (Sign, Vec<u32>) { (self.sign, self.data.to_u32_digits()) }
                /// Returns the two's-complement byte representation of the `BigInt` in big-endian byte order.
                #[inline] pub fn to_signed_bytes_be(&self) -> Vec<u8>
                {
                    let mut bytes = self.data.to_bytes_be();
                    let first_byte = bytes.first().cloned().unwrap_or(0);
                    if first_byte > 0x7f
                        && !(first_byte == 0x80
                            && bytes.iter().skip(1).all(Zero::is_zero)
                            && self.sign == Sign::Minus)
                    {
                        // msb used by magnitude, extend by 1 byte
                        bytes.insert(0, 0);
                    }
                    if self.sign == Sign::Minus {
                        twos_complement_be(&mut bytes);
                    }
                    bytes
                }
                /// Returns the two's-complement byte representation of the `BigInt` in little-endian byte order.
                #[inline] pub fn to_signed_bytes_le(&self) -> Vec<u8>
                {
                    let mut bytes = self.data.to_bytes_le();
                    let last_byte = bytes.last().cloned().unwrap_or(0);
                    if last_byte > 0x7f
                        && !(last_byte == 0x80
                            && bytes.iter().rev().skip(1).all(Zero::is_zero)
                            && self.sign == Sign::Minus)
                    {
                        // msb used by magnitude, extend by 1 byte
                        bytes.push(0);
                    }
                    if self.sign == Sign::Minus {
                        twos_complement_le(&mut bytes);
                    }
                    bytes
                }
                /// Returns the integer formatted as a string in the given radix.
                #[inline] pub fn to_str_radix(&self, radix: u32) -> String
                {
                    let mut v = to_str_radix_reversed(&self.data, radix);

                    if self.is_negative() { v.push(b'-'); }

                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }
                /// Returns the integer in the requested base in big-endian digit order.
                #[inline] pub fn to_radix_be(&self, radix: u32) -> (Sign, Vec<u8>)
                { (self.sign, self.data.to_radix_be(radix)) }
                /// Returns the integer in the requested base in little-endian digit order.
                #[inline] pub fn to_radix_le(&self, radix: u32) -> (Sign, Vec<u8>)
                { (self.sign, self.data.to_radix_le(radix)) }
                /// Returns the sign of the `BigInt` as a `Sign`.
                #[inline] pub fn sign(&self) -> Sign { self.sign }
                /// Determines the fewest bits necessary to express the `BigInt`, not including the sign.
                #[inline] pub fn bits(&self) -> usize { self.data.bits() }
                /// Converts this `BigInt` into a `BigUint`, if it's not negative.
                #[inline] pub fn to_biguint(&self) -> Option<BigUint>
                {
                    match self.sign
                    {
                        Plus => Some(self.data.clone()),
                        NoSign => Some(Zero::zero()),
                        Minus => None,
                    }
                }

                #[inline] pub fn checked_add(&self, v: &BigInt) -> Option<BigInt> { Some(self.add(v)) }

                #[inline] pub fn checked_sub(&self, v: &BigInt) -> Option<BigInt> { Some(self.sub(v)) }

                #[inline] pub fn checked_mul(&self, v: &BigInt) -> Option<BigInt> { Some(self.mul(v)) }

                #[inline] pub fn checked_div(&self, v: &BigInt) -> Option<BigInt>
                {
                    if v.is_zero() { return None; }
                    Some(self.div(v))
                }
                /// Returns `(self ^ exponent) mod modulus`.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self
                {
                    assert!(
                        !exponent.is_negative(),
                        "negative exponentiation is not supported!"
                    );
                    assert!(!modulus.is_zero(), "divide by zero!");

                    let result = self.data.modpow(&exponent.data, &modulus.data);
                    if result.is_zero() {
                        return BigInt::zero();
                    }
                    
                    let (sign, mag) = match (
                        self.is_negative() && exponent.is_odd(),
                        modulus.is_negative(),
                    ) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    BigInt::from_biguint(sign, mag)
                }
                /// Returns the truncated principal square root of `self`.
                pub fn sqrt(&self) -> Self { Roots::sqrt(self) }
                /// Returns the truncated principal cube root of `self`.
                pub fn cbrt(&self) -> Self { Roots::cbrt(self) }
                /// Returns the truncated principal `n`th root of `self`.
                pub fn nth_root(&self, n: u32) -> Self { Roots::nth_root(self, n) }
            }

            impl_sum_iter_type!(BigInt);
            impl_product_iter_type!(BigInt);
            /// Perform in-place two's complement of the given binary representation, in little-endian byte order.
            #[inline] fn twos_complement_le(digits: &mut [u8]) { twos_complement(digits) }
            /// Perform in-place two's complement of the given binary representation in big-endian byte order.
            #[inline] fn twos_complement_be(digits: &mut [u8]) { twos_complement(digits.iter_mut().rev()) }
            /// Perform inplace two complement of the given digit iterator starting from the least significant byte.
            #[inline] fn twos_complement<'a, I>(digits: I) where
            I: IntoIterator<Item = &'a mut u8>
            {
                let mut carry = true;
                for d in digits {
                    *d = d.not();
                    if carry {
                        *d = d.wrapping_add(1);
                        carry = d.is_zero();
                    }
                }
            }
        }

        pub mod bigrand
        {
            //! Randomization of big integers

            use ::
            {
                rand::
                {
                    distributions::uniform::{ SampleUniform, UniformSampler },
                    prelude::{ * },
                },
                num::
                {
                    integer::Integer,
                    big::{ BigInt, BigUint, Sign::*, bigdigit::BigDigit },
                    traits::Zero,
                },
                *,
            };

            /*
            use rand::distributions::uniform::{SampleUniform, UniformSampler};
            use rand::prelude::*;
            use rand::AsByteSliceMut;

            use BigInt;
            use BigUint;
            use Sign::*;

            use big_digit::BigDigit;
            use bigint::{into_magnitude, magnitude};

            use integer::Integer;
            use traits::Zero;
            */
            /// A trait for sampling random big integers.
            pub trait RandBigInt 
            {
                /// Generate a random `BigUint` of the given bit size.
                fn gen_biguint(&mut self, bit_size: usize) -> BigUint;
                /// Generate a random BigInt of the given bit size.
                fn gen_bigint(&mut self, bit_size: usize) -> BigInt;
                /// Generate a random `BigUint` less than the given bound. Fails
                /// when the bound is zero.
                fn gen_biguint_below(&mut self, bound: &BigUint) -> BigUint;
                /// Generate a random `BigUint` within the given range. The lower
                /// bound is inclusive; the upper bound is exclusive. Fails when
                /// the upper bound is not greater than the lower bound.
                fn gen_biguint_range(&mut self, lbound: &BigUint, ubound: &BigUint) -> BigUint;
                /// Generate a random `BigInt` within the given range. The lower
                /// bound is inclusive; the upper bound is exclusive. Fails when
                /// the upper bound is not greater than the lower bound.
                fn gen_bigint_range(&mut self, lbound: &BigInt, ubound: &BigInt) -> BigInt;
            }

            impl<R: Rng + ?Sized> RandBigInt for R 
            {
                fn gen_biguint(&mut self, bit_size: usize) -> BigUint {
                    use super::big_digit::BITS;
                    let (digits, rem) = bit_size.div_rem(&BITS);
                    let mut data = vec![BigDigit::default(); digits + (rem > 0) as usize];
                    self.fill_bytes(data[..].as_byte_slice_mut());
                    data.to_le();
                    if rem > 0 {
                        data[digits] >>= BITS - rem;
                    }
                    BigUint::new(data)
                }

                fn gen_bigint(&mut self, bit_size: usize) -> BigInt {
                    loop {
                        let biguint = self.gen_biguint(bit_size);
                        let sign = if biguint.is_zero() {
                            if self.gen() {
                                continue;
                            } else {
                                NoSign
                            }
                        } else if self.gen() {
                            Plus
                        } else {
                            Minus
                        };
                        return BigInt::from_biguint(sign, biguint);
                    }
                }

                fn gen_biguint_below(&mut self, bound: &BigUint) -> BigUint {
                    assert!(!bound.is_zero());
                    let bits = bound.bits();
                    loop {
                        let n = self.gen_biguint(bits);
                        if n < *bound {
                            return n;
                        }
                    }
                }

                fn gen_biguint_range(&mut self, lbound: &BigUint, ubound: &BigUint) -> BigUint {
                    assert!(*lbound < *ubound);
                    if lbound.is_zero() {
                        self.gen_biguint_below(ubound)
                    } else {
                        lbound + self.gen_biguint_below(&(ubound - lbound))
                    }
                }

                fn gen_bigint_range(&mut self, lbound: &BigInt, ubound: &BigInt) -> BigInt {
                    assert!(*lbound < *ubound);
                    if lbound.is_zero() {
                        BigInt::from(self.gen_biguint_below(magnitude(&ubound)))
                    } else if ubound.is_zero() {
                        lbound + BigInt::from(self.gen_biguint_below(magnitude(&lbound)))
                    } else {
                        let delta = ubound - lbound;
                        lbound + BigInt::from(self.gen_biguint_below(magnitude(&delta)))
                    }
                }
            }
            /// The back-end implementing rand's `UniformSampler` for `BigUint`.
            #[derive(Clone, Debug)]
            pub struct UniformBigUint
            {
                base: BigUint,
                len: BigUint,
            }

            impl UniformSampler for UniformBigUint
            {
                type X = BigUint;

                #[inline]
                fn new(low: Self::X, high: Self::X) -> Self {
                    assert!(low < high);
                    UniformBigUint {
                        len: high - &low,
                        base: low,
                    }
                }

                #[inline]
                fn new_inclusive(low: Self::X, high: Self::X) -> Self {
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }

                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                    &self.base + rng.gen_biguint_below(&self.len)
                }

                #[inline]
                fn sample_single<R: Rng + ?Sized>(low: Self::X, high: Self::X, rng: &mut R) -> Self::X {
                    rng.gen_biguint_range(&low, &high)
                }
            }

            impl SampleUniform for BigUint
            {
                type Sampler = UniformBigUint;
            }
            /// The back-end implementing rand's `UniformSampler` for `BigInt`.
            #[derive(Clone, Debug)]
            pub struct UniformBigInt 
            {
                base: BigInt,
                len: BigUint,
            }

            impl UniformSampler for UniformBigInt
            {
                type X = BigInt;

                #[inline]
                fn new(low: Self::X, high: Self::X) -> Self {
                    assert!(low < high);
                    UniformBigInt {
                        len: into_magnitude(high - &low),
                        base: low,
                    }
                }

                #[inline]
                fn new_inclusive(low: Self::X, high: Self::X) -> Self {
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }

                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                    &self.base + BigInt::from(rng.gen_biguint_below(&self.len))
                }

                #[inline]
                fn sample_single<R: Rng + ?Sized>(low: Self::X, high: Self::X, rng: &mut R) -> Self::X {
                    rng.gen_bigint_range(&low, &high)
                }
            }

            impl SampleUniform for BigInt
            {
                type Sampler = UniformBigInt;
            }
            /// A random distribution for `BigUint` and `BigInt` values of a particular bit size.
            #[derive(Clone, Copy, Debug)]
            pub struct RandomBits
            {
                bits: usize,
            }

            impl RandomBits
            {
                #[inline] pub fn new(bits: usize) -> RandomBits { RandomBits { bits } }
            }

            impl Distribution<BigUint> for RandomBits
            {
                #[inline] fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigUint { rng.gen_biguint(self.bits) }
            }

            impl Distribution<BigInt> for RandomBits
            {
                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigInt {
                    rng.gen_bigint(self.bits)
                }
            }
        }
        
        pub mod biguint
        {

        }

        pub mod bigdigit 
        {
            /// A `BigDigit` is a `BigUint`'s composing element.
            pub type BigDigit = u32;

            /// A `DoubleBigDigit` is the internal type used to do the computations.  Its
            /// size is the double of the size of `BigDigit`.
            pub type DoubleBigDigit = u64;

            /// A `SignedDoubleBigDigit` is the signed version of `DoubleBigDigit`.
            pub type SignedDoubleBigDigit = i64;

            // `DoubleBigDigit` size dependent
            pub const BITS: usize = 32;

            const LO_MASK: DoubleBigDigit = (-1i32 as DoubleBigDigit) >> BITS;

            #[inline]
            fn get_hi(n: DoubleBigDigit) -> BigDigit {
                (n >> BITS) as BigDigit
            }
            #[inline]
            fn get_lo(n: DoubleBigDigit) -> BigDigit {
                (n & LO_MASK) as BigDigit
            }

            /// Split one `DoubleBigDigit` into two `BigDigit`s.
            #[inline]
            pub fn from_doublebigdigit(n: DoubleBigDigit) -> (BigDigit, BigDigit) {
                (get_hi(n), get_lo(n))
            }

            /// Join two `BigDigit`s into one `DoubleBigDigit`
            #[inline]
            pub fn to_doublebigdigit(hi: BigDigit, lo: BigDigit) -> DoubleBigDigit {
                DoubleBigDigit::from(lo) | (DoubleBigDigit::from(hi) << BITS)
            }
        } 
    }

    pub mod fractional
    {
        use ::
        {
            num::
            {
                big::{ BigInt },
                rational::{ BigRational },
                traits::{ FromPrimitive, pow },
            },
            *,
        };
        /*
        use std::fs::File;
        use std::io::Read;
        */
        pub fn frac_from_whole_and_dec( whole:BigInt, decimal:BigInt, dec_len:usize ) -> BigRational
        {
            let denom = pow( BigInt::from_u8( 10 ).unwrap(), dec_len );
            BigRational::new( whole, 1.into() ) + BigRational::new( decimal, denom )
        }
    }

    pub mod integer
    {
        //! Integer trait and functions.
        use ::
        {
            num::
            {
                traits::{ Num, Signed, Zero },
            },
            ops::{ Add },
            *,
        };

        macro_rules! impl_integer_for_isize
        {
            ($T:ty, $test_mod:ident) =>
            {
                impl Integer for $T {
                    /// Floored integer division
                    #[inline]
                    fn div_floor(&self, other: &Self) -> Self {
                        // Algorithm from [Daan Leijen. _Division and Modulus for Computer Scientists_,
                        // December 2001](http://research.microsoft.com/pubs/151917/divmodnote-letter.pdf)
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            d - 1
                        } else {
                            d
                        }
                    }

                    /// Floored integer modulo
                    #[inline]
                    fn mod_floor(&self, other: &Self) -> Self {
                        // Algorithm from [Daan Leijen. _Division and Modulus for Computer Scientists_,
                        // December 2001](http://research.microsoft.com/pubs/151917/divmodnote-letter.pdf)
                        let r = *self % *other;
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            r + *other
                        } else {
                            r
                        }
                    }

                    /// Calculates `div_floor` and `mod_floor` simultaneously
                    #[inline]
                    fn div_mod_floor(&self, other: &Self) -> (Self, Self) {
                        // Algorithm from [Daan Leijen. _Division and Modulus for Computer Scientists_,
                        // December 2001](http://research.microsoft.com/pubs/151917/divmodnote-letter.pdf)
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            (d - 1, r + *other)
                        } else {
                            (d, r)
                        }
                    }

                    #[inline]
                    fn div_ceil(&self, other: &Self) -> Self {
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other > 0) || (r < 0 && *other < 0) {
                            d + 1
                        } else {
                            d
                        }
                    }

                    /// Calculates the Greatest Common Divisor (GCD) of the number and
                    /// `other`. The result is always positive.
                    #[inline]
                    fn gcd(&self, other: &Self) -> Self {
                        // Use Stein's algorithm
                        let mut m = *self;
                        let mut n = *other;
                        if m == 0 || n == 0 {
                            return (m | n).abs();
                        }

                        // find common factors of 2
                        let shift = (m | n).trailing_zeros();

                        // The algorithm needs positive numbers, but the minimum value
                        // can't be represented as a positive one.
                        // It's also a power of two, so the gcd can be
                        // calculated by bitshifting in that case

                        // Assuming two's complement, the number created by the shift
                        // is positive for all numbers except gcd = abs(min value)
                        // The call to .abs() causes a panic in debug mode
                        if m == Self::min_value() || n == Self::min_value() {
                            return (1 << shift).abs();
                        }

                        // guaranteed to be positive now, rest like unsigned algorithm
                        m = m.abs();
                        n = n.abs();

                        // divide n and m by 2 until odd
                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        while m != n {
                            if m > n {
                                m -= n;
                                m >>= m.trailing_zeros();
                            } else {
                                n -= m;
                                n >>= n.trailing_zeros();
                            }
                        }
                        m << shift
                    }

                    #[inline]
                    fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self) {
                        let egcd = self.extended_gcd(other);
                        // should not have to recalculate abs
                        let lcm = if egcd.gcd.is_zero() {
                            Self::zero()
                        } else {
                            (*self * (*other / egcd.gcd)).abs()
                        };
                        (egcd, lcm)
                    }

                    /// Calculates the Lowest Common Multiple (LCM) of the number and
                    /// `other`.
                    #[inline]
                    fn lcm(&self, other: &Self) -> Self {
                        self.gcd_lcm(other).1
                    }

                    /// Calculates the Greatest Common Divisor (GCD) and
                    /// Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline]
                    fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                        if self.is_zero() && other.is_zero() {
                            return (Self::zero(), Self::zero());
                        }
                        let gcd = self.gcd(other);
                        // should not have to recalculate abs
                        let lcm = (*self * (*other / gcd)).abs();
                        (gcd, lcm)
                    }

                    /// Deprecated, use `is_multiple_of` instead.
                    #[inline]
                    fn divides(&self, other: &Self) -> bool {
                        self.is_multiple_of(other)
                    }

                    /// Returns `true` if the number is a multiple of `other`.
                    #[inline]
                    fn is_multiple_of(&self, other: &Self) -> bool {
                        *self % *other == 0
                    }

                    /// Returns `true` if the number is divisible by `2`
                    #[inline]
                    fn is_even(&self) -> bool {
                        (*self) & 1 == 0
                    }

                    /// Returns `true` if the number is not divisible by `2`
                    #[inline]
                    fn is_odd(&self) -> bool {
                        !self.is_even()
                    }

                    /// Simultaneous truncated integer division and modulus.
                    #[inline]
                    fn div_rem(&self, other: &Self) -> (Self, Self) {
                        (*self / *other, *self % *other)
                    }
                }
            };
        }

        macro_rules! impl_integer_for_usize
        {
            ($T:ty, $test_mod:ident) => {
                impl Integer for $T {
                    /// Unsigned integer division. Returns the same result as `div` (`/`).
                    #[inline]
                    fn div_floor(&self, other: &Self) -> Self {
                        *self / *other
                    }

                    /// Unsigned integer modulo operation. Returns the same result as `rem` (`%`).
                    #[inline]
                    fn mod_floor(&self, other: &Self) -> Self {
                        *self % *other
                    }

                    #[inline]
                    fn div_ceil(&self, other: &Self) -> Self {
                        *self / *other + (0 != *self % *other) as Self
                    }

                    /// Calculates the Greatest Common Divisor (GCD) of the number and `other`
                    #[inline]
                    fn gcd(&self, other: &Self) -> Self {
                        // Use Stein's algorithm
                        let mut m = *self;
                        let mut n = *other;
                        if m == 0 || n == 0 {
                            return m | n;
                        }

                        // find common factors of 2
                        let shift = (m | n).trailing_zeros();

                        // divide n and m by 2 until odd
                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        while m != n {
                            if m > n {
                                m -= n;
                                m >>= m.trailing_zeros();
                            } else {
                                n -= m;
                                n >>= n.trailing_zeros();
                            }
                        }
                        m << shift
                    }

                    #[inline]
                    fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self) {
                        let egcd = self.extended_gcd(other);
                        // should not have to recalculate abs
                        let lcm = if egcd.gcd.is_zero() {
                            Self::zero()
                        } else {
                            *self * (*other / egcd.gcd)
                        };
                        (egcd, lcm)
                    }

                    /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline]
                    fn lcm(&self, other: &Self) -> Self {
                        self.gcd_lcm(other).1
                    }

                    /// Calculates the Greatest Common Divisor (GCD) and
                    /// Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline]
                    fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                        if self.is_zero() && other.is_zero() {
                            return (Self::zero(), Self::zero());
                        }
                        let gcd = self.gcd(other);
                        let lcm = *self * (*other / gcd);
                        (gcd, lcm)
                    }

                    /// Deprecated, use `is_multiple_of` instead.
                    #[inline]
                    fn divides(&self, other: &Self) -> bool {
                        self.is_multiple_of(other)
                    }

                    /// Returns `true` if the number is a multiple of `other`.
                    #[inline]
                    fn is_multiple_of(&self, other: &Self) -> bool {
                        *self % *other == 0
                    }

                    /// Returns `true` if the number is divisible by `2`.
                    #[inline]
                    fn is_even(&self) -> bool {
                        *self % 2 == 0
                    }

                    /// Returns `true` if the number is not divisible by `2`.
                    #[inline]
                    fn is_odd(&self) -> bool {
                        !self.is_even()
                    }

                    /// Simultaneous truncated integer division and modulus.
                    #[inline]
                    fn div_rem(&self, other: &Self) -> (Self, Self) {
                        (*self / *other, *self % *other)
                    }
                }
            };
        }

        pub mod roots
        {
            use ::
            {
                num::
                {
                    traits::{ checked_pow, PrimInt },
                },
                *,
            };
            /*
            use Integer;
            */
            macro_rules! signed_roots
            {
                ($T:ty, $U:ty) =>
                {
                    impl Roots for $T
                    {
                        #[inline] fn nth_root(&self, n: u32) -> Self
                        {
                            if *self >= 0 {
                                (*self as $U).nth_root(n) as Self
                            } else {
                                assert!(n.is_odd(), "even roots of a negative are imaginary");
                                -((self.wrapping_neg() as $U).nth_root(n) as Self)
                            }
                        }

                        #[inline] fn sqrt(&self) -> Self
                        {
                            assert!(*self >= 0, "the square root of a negative is imaginary");
                            (*self as $U).sqrt() as Self
                        }

                        #[inline] fn cbrt(&self) -> Self
                        {
                            if *self >= 0 {
                                (*self as $U).cbrt() as Self
                            } else {
                                -((self.wrapping_neg() as $U).cbrt() as Self)
                            }
                        }
                    }
                };
            }
            /// Provides methods to compute an integer's square root, cube root, and arbitrary `n`th root.
            pub trait Roots: Integer
            {
                /// Returns the truncated principal `n`th root of an integer
                fn nth_root(&self, n: u32) -> Self;
                /// Returns the truncated principal square root of an integer -- `⌊√x⌋`
                #[inline] fn sqrt(&self) -> Self { self.nth_root(2) }
                /// Returns the truncated principal cube root of an integer -- `if x >= 0 { ⌊∛x⌋ } else { ⌈∛x⌉ }`
                #[inline] fn cbrt(&self) -> Self { self.nth_root(3) }
            }
            /// Returns the truncated principal square root of an integer.
            #[inline] pub fn sqrt<T: Roots>(x: T) -> T { x.sqrt() }
            /// Returns the truncated principal cube root of an integer.
            #[inline] pub fn cbrt<T: Roots>(x: T) -> T { x.cbrt() }
            /// Returns the truncated principal `n`th root of an integer.
            #[inline] pub fn nth_root<T: Roots>(x: T, n: u32) -> T { x.nth_root(n) }

            signed_roots!(i8, u8);
            signed_roots!(i16, u16);
            signed_roots!(i32, u32);
            signed_roots!(i64, u64);
            signed_roots!(i128, u128);
            signed_roots!(isize, usize);

            #[inline] fn fixpoint<T, F>(mut x: T, f: F) -> T where
            T: Integer + Copy,
            F: Fn(T) -> T
            {
                let mut xn = f(x);
                while x < xn
                {
                    x = xn;
                    xn = f(x);
                }

                while x > xn
                {
                    x = xn;
                    xn = f(x);
                }
                x
            }

            #[inline] fn bits<T>() -> u32 { 8 * mem::size_of::<T>() as u32 }

            #[inline] fn log2<T: PrimInt>(x: T) -> u32
            {
                debug_assert!(x > T::zero());
                bits::<T>() - 1 - x.leading_zeros()
            }

            macro_rules! unsigned_roots
            {
                ($T:ident) =>
                {
                    impl Roots for $T
                    {
                        #[inline] fn nth_root(&self, n: u32) -> Self
                        {
                            fn go(a: $T, n: u32) -> $T
                            {
                                match n {
                                    0 => panic!("can't find a root of degree 0!"),
                                    1 => return a,
                                    2 => return a.sqrt(),
                                    3 => return a.cbrt(),
                                    _ => (),
                                }

                                // The root of values less than 2ⁿ can only be 0 or 1.
                                if bits::<$T>() <= n || a < (1 << n) {
                                    return (a > 0) as $T;
                                }

                                if bits::<$T>() > 64 {
                                    // 128-bit division is slow, so do a bitwise `nth_root` until it's small enough.
                                    return if a <= core::u64::MAX as $T {
                                        (a as u64).nth_root(n) as $T
                                    } else {
                                        let lo = (a >> n).nth_root(n) << 1;
                                        let hi = lo + 1;
                                        // 128-bit `checked_mul` also involves division, but we can't always
                                        // compute `hiⁿ` without risking overflow.  Try to avoid it though...
                                        if hi.next_power_of_two().trailing_zeros() * n >= bits::<$T>() {
                                            match checked_pow(hi, n as usize) {
                                                Some(x) if x <= a => hi,
                                                _ => lo,
                                            }
                                        } else {
                                            if hi.pow(n) <= a {
                                                hi
                                            } else {
                                                lo
                                            }
                                        }
                                    };
                                }

                                #[cfg(feature = "std")]
                                #[inline]
                                fn guess(x: $T, n: u32) -> $T {
                                    // for smaller inputs, `f64` doesn't justify its cost.
                                    if bits::<$T>() <= 32 || x <= core::u32::MAX as $T {
                                        1 << ((log2(x) + n - 1) / n)
                                    } else {
                                        ((x as f64).ln() / f64::from(n)).exp() as $T
                                    }
                                }

                                #[cfg(not(feature = "std"))]
                                #[inline]
                                fn guess(x: $T, n: u32) -> $T {
                                    1 << ((log2(x) + n - 1) / n)
                                }

                                // https://en.wikipedia.org/wiki/Nth_root_algorithm
                                let n1 = n - 1;
                                let next = |x: $T| {
                                    let y = match checked_pow(x, n1 as usize) {
                                        Some(ax) => a / ax,
                                        None => 0,
                                    };
                                    (y + x * n1 as $T) / n as $T
                                };
                                fixpoint(guess(a, n), next)
                            }
                            go(*self, n)
                        }

                        #[inline] fn sqrt(&self) -> Self
                        {
                            fn go(a: $T) -> $T
                            {
                                if bits::<$T>() > 64
                                {
                                    return if a <= core::u64::MAX as $T
                                    {
                                        (a as u64).sqrt() as $T
                                    } else {
                                        let lo = (a >> 2u32).sqrt() << 1;
                                        let hi = lo + 1;
                                        if hi * hi <= a {
                                            hi
                                        } else {
                                            lo
                                        }
                                    };
                                }

                                if a < 4 {
                                    return (a > 0) as $T;
                                }

                                #[cfg(feature = "std")]
                                #[inline]
                                fn guess(x: $T) -> $T {
                                    (x as f64).sqrt() as $T
                                }

                                #[cfg(not(feature = "std"))]
                                #[inline]
                                fn guess(x: $T) -> $T {
                                    1 << ((log2(x) + 1) / 2)
                                }

                                // https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method
                                let next = |x: $T| (a / x + x) >> 1;
                                fixpoint(guess(a), next)
                            }
                            go(*self)
                        }

                        #[inline] fn cbrt(&self) -> Self
                        {
                            fn go(a: $T) -> $T {
                                if bits::<$T>() > 64 {
                                    // 128-bit division is slow, so do a bitwise `cbrt` until it's small enough.
                                    return if a <= core::u64::MAX as $T {
                                        (a as u64).cbrt() as $T
                                    } else {
                                        let lo = (a >> 3u32).cbrt() << 1;
                                        let hi = lo + 1;
                                        if hi * hi * hi <= a {
                                            hi
                                        } else {
                                            lo
                                        }
                                    };
                                }

                                if bits::<$T>() <= 32 {
                                    // Implementation based on Hacker's Delight `icbrt2`
                                    let mut x = a;
                                    let mut y2 = 0;
                                    let mut y = 0;
                                    let smax = bits::<$T>() / 3;
                                    for s in (0..smax + 1).rev() {
                                        let s = s * 3;
                                        y2 *= 4;
                                        y *= 2;
                                        let b = 3 * (y2 + y) + 1;
                                        if x >> s >= b {
                                            x -= b << s;
                                            y2 += 2 * y + 1;
                                            y += 1;
                                        }
                                    }
                                    return y;
                                }

                                if a < 8 {
                                    return (a > 0) as $T;
                                }
                                if a <= core::u32::MAX as $T {
                                    return (a as u32).cbrt() as $T;
                                }

                                #[cfg(feature = "std")]
                                #[inline]
                                fn guess(x: $T) -> $T {
                                    (x as f64).cbrt() as $T
                                }

                                #[cfg(not(feature = "std"))]
                                #[inline]
                                fn guess(x: $T) -> $T {
                                    1 << ((log2(x) + 2) / 3)
                                }

                                // https://en.wikipedia.org/wiki/Cube_root#Numerical_methods
                                let next = |x: $T| (a / (x * x) + x * 2) / 3;
                                fixpoint(guess(a), next)
                            }
                            go(*self)
                        }
                    }
                };
            }

            unsigned_roots!(u8);
            unsigned_roots!(u16);
            unsigned_roots!(u32);
            unsigned_roots!(u64);
            unsigned_roots!(u128);
            unsigned_roots!(usize);
        }
        pub use self::roots::{cbrt, nth_root, Roots, sqrt};

        pub trait Integer: Sized + Num + PartialOrd + Ord + Eq
        {
            /// Floored integer division.
            fn div_floor(&self, other: &Self) -> Self;
            /// Floored integer modulo.
            fn mod_floor(&self, other: &Self) -> Self;
            /// Ceiled integer division.
            fn div_ceil(&self, other: &Self) -> Self
            {
                let (q, r) = self.div_mod_floor(other);
                
                if r.is_zero() { q }
                else { q + Self::one() }
            }
            /// Greatest Common Divisor (GCD).
            fn gcd(&self, other: &Self) -> Self;
            /// Lowest Common Multiple (LCM).
            fn lcm(&self, other: &Self) -> Self;
            /// Greatest Common Divisor (GCD) and Lowest Common Multiple (LCM) together.
            #[inline] fn gcd_lcm(&self, other: &Self) -> (Self, Self) { (self.gcd(other), self.lcm(other)) }
            /// Greatest common divisor and Bézout coefficients.
            #[inline] fn extended_gcd(&self, other: &Self) -> ExtendedGcd<Self> where
            Self: Clone
            {
                let mut s = (Self::zero(), Self::one());
                let mut t = (Self::one(), Self::zero());
                let mut r = (other.clone(), self.clone());

                while !r.0.is_zero() {
                    let q = r.1.clone() / r.0.clone();
                    let f = |mut r: (Self, Self)| {
                        mem::swap(&mut r.0, &mut r.1);
                        r.0 = r.0 - q.clone() * r.1.clone();
                        r
                    };
                    r = f(r);
                    s = f(s);
                    t = f(t);
                }

                if r.1 >= Self::zero() {
                    ExtendedGcd {
                        gcd: r.1,
                        x: s.1,
                        y: t.1,
                        _hidden: (),
                    }
                } else {
                    ExtendedGcd {
                        gcd: Self::zero() - r.1,
                        x: Self::zero() - s.1,
                        y: Self::zero() - t.1,
                        _hidden: (),
                    }
                }
            }
            /// Greatest common divisor, least common multiple, and Bézout coefficients.
            #[inline] fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self) where
            Self: Clone + Signed
            { (self.extended_gcd(other), self.lcm(other)) }
            /// Deprecated, use `is_multiple_of` instead.
            fn divides(&self, other: &Self) -> bool;
            /// Returns `true` if `self` is a multiple of `other`.
            fn is_multiple_of(&self, other: &Self) -> bool;
            /// Returns `true` if the number is even.
            fn is_even(&self) -> bool;
            /// Returns `true` if the number is odd.
            fn is_odd(&self) -> bool;
            /// Simultaneous truncated integer division and modulus.
            fn div_rem(&self, other: &Self) -> (Self, Self);
            /// Simultaneous floored integer division and modulus.
            fn div_mod_floor(&self, other: &Self) -> (Self, Self) { (self.div_floor(other), self.mod_floor(other)) }
            /// Rounds up to nearest multiple of argument.
            #[inline] fn next_multiple_of(&self, other: &Self) -> Self where
            Self: Clone
            {
                let m = self.mod_floor(other);
                self.clone() + if m.is_zero() { Self::zero() } else { other.clone() - m }
            }
            /// Rounds down to nearest multiple of argument.
            #[inline] fn prev_multiple_of(&self, other: &Self) -> Self where
            Self: Clone
            { self.clone() - self.mod_floor(other) }
        }
        /// Greatest common divisor and Bézout coefficients.
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct ExtendedGcd<A>
        {
            pub gcd: A,
            pub x: A,
            pub y: A,
            _hidden: (),
        }
        /// Simultaneous integer division and modulus
        #[inline] pub fn div_rem<T: Integer>(x: T, y: T) -> (T, T) { x.div_rem(&y) }
        /// Floored integer division
        #[inline] pub fn div_floor<T: Integer>(x: T, y: T) -> T { x.div_floor(&y) }
        /// Floored integer modulus
        #[inline] pub fn mod_floor<T: Integer>(x: T, y: T) -> T { x.mod_floor(&y) }
        /// Simultaneous floored integer division and modulus
        #[inline] pub fn div_mod_floor<T: Integer>(x: T, y: T) -> (T, T) { x.div_mod_floor(&y) }
        /// Ceiled integer division
        #[inline] pub fn div_ceil<T: Integer>(x: T, y: T) -> T { x.div_ceil(&y) }
        /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
        #[inline(always)] pub fn gcd<T: Integer>(x: T, y: T) -> T { x.gcd(&y) }
        /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
        #[inline(always)] pub fn lcm<T: Integer>(x: T, y: T) -> T { x.lcm(&y) }
        /// Calculates the GCD and LCM of the number and `other`.
        #[inline(always)] pub fn gcd_lcm<T: Integer>(x: T, y: T) -> (T, T) { x.gcd_lcm(&y) }

        impl_integer_for_isize!(i8, test_integer_i8);
        impl_integer_for_isize!(i16, test_integer_i16);
        impl_integer_for_isize!(i32, test_integer_i32);
        impl_integer_for_isize!(i64, test_integer_i64);
        impl_integer_for_isize!(isize, test_integer_isize);
        impl_integer_for_isize!(i128, test_integer_i128);
        impl_integer_for_usize!(u8, test_integer_u8);
        impl_integer_for_usize!(u16, test_integer_u16);
        impl_integer_for_usize!(u32, test_integer_u32);
        impl_integer_for_usize!(u64, test_integer_u64);
        impl_integer_for_usize!(usize, test_integer_usize);
        impl_integer_for_usize!(u128, test_integer_u128);
        /// An iterator over binomial coefficients.
        pub struct IterBinomial<T>
        {
            a: T,
            n: T,
            k: T,
        }

        impl<T> IterBinomial<T> where
        T: Integer
        {
            /// For a given n, iterate over all binomial coefficients binomial(n, k), for k=0...n.
            pub fn new(n: T) -> IterBinomial<T>
            {
                IterBinomial
                {
                    k: T::zero(),
                    a: T::one(),
                    n: n,
                }
            }
        }

        impl<T> Iterator for IterBinomial<T> where
        T: Integer + Clone
        {
            type Item = T;
            fn next(&mut self) -> Option<T>
            {
                if self.k > self.n { return None; }

                self.a = if !self.k.is_zero()
                {
                    multiply_and_divide
                    (
                        self.a.clone(),
                        self.n.clone() - self.k.clone() + T::one(),
                        self.k.clone(),
                    )
                }
                else { T::one() };

                self.k = self.k.clone() + T::one();
                Some(self.a.clone())
            }
        }
        /// Calculate r * a / b, avoiding overflows and fractions.
        fn multiply_and_divide<T: Integer + Clone>(r: T, a: T, b: T) -> T
        {
            let g = gcd(r.clone(), b.clone());
            r / g.clone() * (a / (b / g))
        }
        /// Calculate the binomial coefficient.
        pub fn binomial<T: Integer + Clone>(mut n: T, k: T) -> T
        {
            if k > n { return T::zero(); }
            
            if k > n.clone() - k.clone() { return binomial(n.clone(), n - k); }

            let mut r = T::one();
            let mut d = T::one();
            
            loop
            {
                if d > k { break; }

                r = multiply_and_divide(r, n.clone(), d.clone());
                n = n - T::one();
                d = d + T::one();
            }

            r
        }
        /// Calculate the multinomial coefficient.
        pub fn multinomial<T: Integer + Clone>(k: &[T]) -> T where
        for<'a> T: Add<&'a T, Output = T>
        {
            let mut r = T::one();
            let mut p = T::zero();
            
            for i in k
            {
                p = p + i;
                r = r * binomial(p.clone(), i.clone());
            }

            r
        }
    }
    
    pub mod rational
    {
        //! Rational numbers
        use ::
        {
            error::{ Error },
            fmt::{ self, Binary, Display, Formatter, LowerExp, LowerHex, Octal, UpperExp, UpperHex },
            hash::{ Hash, Hasher },
            num::
            {
                integer::{ Integer },
                big::{ BigInt, BigUint, Sign, ToBigInt },
                traits::
                {
                    float::{ FloatCore },
                    Bounded, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub,/* ConstOne, ConstZero, */FromPrimitive, Inv,
                    Num, NumCast, One, Pow, Signed, ToPrimitive, Unsigned, Zero,
                },
            },
            ops::{ Add, Div, Mul, Neg, Rem, ShlAssign, Sub },
            str::{ FromStr },
            *,
        };
        
        macro_rules! forward_ref_ref_binop
        {
            (impl $imp:ident, $method:ident) =>
            {
                impl<'a, 'b, T: Clone + Integer> $imp<&'b Ratio<T>> for &'a Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other:&'b Ratio<T>) -> Ratio<T> {self.clone().$method(other.clone())}
                }

                impl<'a, 'b, T: Clone + Integer> $imp<&'b T> for &'a Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: &'b T) -> Ratio<T> {self.clone().$method(other.clone())}
                }
            };
        }

        macro_rules! forward_ref_val_binop
        {
            (impl $imp:ident, $method:ident) =>
            {
                impl<'a, T> $imp<Ratio<T>> for &'a Ratio<T> where
                T: Clone + Integer
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: Ratio<T>) -> Ratio<T> { self.clone().$method(other) }
                }
                
                impl<'a, T> $imp<T> for &'a Ratio<T> where
                T: Clone + Integer
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: T) -> Ratio<T> { self.clone().$method(other) }
                }
            };
        }

        macro_rules! forward_val_ref_binop
        {
            (impl $imp:ident, $method:ident) =>
            {
                impl<'a, T> $imp<&'a Ratio<T>> for Ratio<T> where
                T: Clone + Integer
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: &Ratio<T>) -> Ratio<T> { self.$method(other.clone()) }
                }

                impl<'a, T> $imp<&'a T> for Ratio<T> where
                T: Clone + Integer
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: &T) -> Ratio<T> { self.$method(other.clone()) }
                }
            };
        }

        macro_rules! forward_all_binop
        {
            (impl $imp:ident, $method:ident) =>
            {
                forward_ref_ref_binop!(impl $imp, $method);
                forward_ref_val_binop!(impl $imp, $method);
                forward_val_ref_binop!(impl $imp, $method);
            };
        }

        macro_rules! arith_impl
        {
            (impl $imp:ident, $method:ident) =>
            {
                forward_all_binop!(impl $imp, $method);
                
                impl<T: Clone + Integer> $imp<Ratio<T>> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: Ratio<T>) -> Ratio<T>
                    {
                        if self.denom == rhs.denom { return Ratio::new(self.numer.$method(rhs.numer), rhs.denom); }

                        let lcm = self.denom.lcm(&rhs.denom);
                        let lhs_numer = self.numer * (lcm.clone() / self.denom);
                        let rhs_numer = rhs.numer * (lcm.clone() / rhs.denom);
                        Ratio::new(lhs_numer.$method(rhs_numer), lcm)
                    }
                }
                
                impl<T: Clone + Integer> $imp<T> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: T) -> Ratio<T>
                    { Ratio::new(self.numer.$method(self.denom.clone() * rhs), self.denom) }
                }
            };
        }
        
        macro_rules! checked_arith_impl
        {
            (impl $imp:ident, $method:ident) =>
            {
                impl<T: Clone + Integer + CheckedMul + $imp> $imp for Ratio<T>
                {
                    #[inline] fn $method(&self, rhs: &Ratio<T>) -> Option<Ratio<T>>
                    {
                        let gcd = self.denom.clone().gcd(&rhs.denom);
                        let lcm = (self.denom.clone() / gcd.clone()).checked_mul(&rhs.denom)?;
                        let lhs_numer = (lcm.clone() / self.denom.clone()).checked_mul(&self.numer)?;
                        let rhs_numer = (lcm.clone() / rhs.denom.clone()).checked_mul(&rhs.numer)?;
                        Some(Ratio::new(lhs_numer.$method(&rhs_numer)?, lcm))
                    }
                }
            };
        }
        
        macro_rules! impl_formatting
        {
            ($fmt_trait:ident, $prefix:expr, $fmt_str:expr, $fmt_alt:expr) =>
            {
                impl<T: $fmt_trait + Clone + Integer> $fmt_trait for Ratio<T> {
                    #[cfg(feature = "std")]
                    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                        let pre_pad = if self.denom.is_one() {
                            format!($fmt_str, self.numer)
                        } else {
                            if f.alternate() {
                                format!(concat!($fmt_str, "/", $fmt_alt), self.numer, self.denom)
                            } else {
                                format!(concat!($fmt_str, "/", $fmt_str), self.numer, self.denom)
                            }
                        };
                        if let Some(pre_pad) = pre_pad.strip_prefix("-") {
                            f.pad_integral(false, $prefix, pre_pad)
                        } else {
                            f.pad_integral(true, $prefix, &pre_pad)
                        }
                    }
                    #[cfg(not(feature = "std"))]
                    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                        let plus = if f.sign_plus() && self.numer >= T::zero() {
                            "+"
                        } else {
                            ""
                        };
                        if self.denom.is_one() {
                            if f.alternate() {
                                write!(f, concat!("{}", $fmt_alt), plus, self.numer)
                            } else {
                                write!(f, concat!("{}", $fmt_str), plus, self.numer)
                            }
                        } else {
                            if f.alternate() {
                                write!(
                                    f,
                                    concat!("{}", $fmt_alt, "/", $fmt_alt),
                                    plus, self.numer, self.denom
                                )
                            } else {
                                write!(
                                    f,
                                    concat!("{}", $fmt_str, "/", $fmt_str),
                                    plus, self.numer, self.denom
                                )
                            }
                        }
                    }
                }
            };
        }

        macro_rules! from_primitive_integer
        {
            ($typ:ty, $approx:ident) =>
            {
                impl FromPrimitive for Ratio<$typ> {
                    fn from_i64(n: i64) -> Option<Self> {
                        <$typ as FromPrimitive>::from_i64(n).map(Ratio::from_integer)
                    }

                    fn from_i128(n: i128) -> Option<Self> {
                        <$typ as FromPrimitive>::from_i128(n).map(Ratio::from_integer)
                    }

                    fn from_u64(n: u64) -> Option<Self> {
                        <$typ as FromPrimitive>::from_u64(n).map(Ratio::from_integer)
                    }

                    fn from_u128(n: u128) -> Option<Self> {
                        <$typ as FromPrimitive>::from_u128(n).map(Ratio::from_integer)
                    }

                    fn from_f32(n: f32) -> Option<Self> {
                        $approx(n, 10e-20, 30)
                    }

                    fn from_f64(n: f64) -> Option<Self> {
                        $approx(n, 10e-20, 30)
                    }
                }
            };
        }

        pub mod power
        {
            use ::
            {
                num::
                {
                    integer::{ Integer },
                    rational::{ Ratio },
                    traits::{ One, Pow },
                },
                *,
            };

            macro_rules! pow_unsigned_impl
            {
                (@ $exp:ty) =>
                {
                    type Output = Ratio<T>;
                    #[inline]
                    fn pow(self, expon: $exp) -> Ratio<T> {
                        Ratio::new_raw(self.numer.pow(expon), self.denom.pow(expon))
                    }
                };

                ($exp:ty) =>
                {
                    impl<T: Clone + Integer + Pow<$exp, Output = T>> Pow<$exp> for Ratio<T> 
                    {
                        pow_unsigned_impl!(@ $exp);
                    }

                    impl<'a, T: Clone + Integer> Pow<$exp> for &'a Ratio<T> where
                    &'a T: Pow<$exp, Output = T>
                    {
                        pow_unsigned_impl!(@ $exp);
                    }

                    impl<'b, T: Clone + Integer + Pow<$exp, Output = T>> Pow<&'b $exp> for Ratio<T>
                    {                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }

                    impl<'a, 'b, T: Clone + Integer> Pow<&'b $exp> for &'a Ratio<T> where
                    &'a T: Pow<$exp, Output = T>
                    {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                };
            }

            macro_rules! pow_signed_impl
            {
                (@ &'b BigInt, BigUint) => {
                    type Output = Ratio<T>;
                    #[inline]
                    fn pow(self, expon: &'b BigInt) -> Ratio<T> {
                        match expon.sign() {
                            Sign::NoSign => One::one(),
                            Sign::Minus => {
                                Pow::pow(self, expon.magnitude()).into_recip()
                            }
                            Sign::Plus => Pow::pow(self, expon.magnitude()),
                        }
                    }
                };
                (@ $exp:ty, $unsigned:ty) => {
                    type Output = Ratio<T>;
                    #[inline]
                    fn pow(self, expon: $exp) -> Ratio<T> {
                        match expon.cmp(&0) {
                            cmp::Ordering::Equal => One::one(),
                            cmp::Ordering::Less => {
                                let expon = expon.wrapping_abs() as $unsigned;
                                Pow::pow(self, expon).into_recip()
                            }
                            cmp::Ordering::Greater => Pow::pow(self, expon as $unsigned),
                        }
                    }
                };
                ($exp:ty, $unsigned:ty) => {
                    impl<T: Clone + Integer + Pow<$unsigned, Output = T>> Pow<$exp> for Ratio<T> {
                        pow_signed_impl!(@ $exp, $unsigned);
                    }
                    impl<'a, T: Clone + Integer> Pow<$exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$unsigned, Output = T>,
                    {
                        pow_signed_impl!(@ $exp, $unsigned);
                    }
                    impl<'b, T: Clone + Integer + Pow<$unsigned, Output = T>> Pow<&'b $exp> for Ratio<T> {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                    impl<'a, 'b, T: Clone + Integer> Pow<&'b $exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$unsigned, Output = T>,
                    {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                };
            }
            
            mod bigint
            {
                use ::
                {
                    
                    num::
                    {
                        big::{ BigInt, BigUint, Sign },
                        rational::{ * },
                        traits::{ One, Pow },
                    },
                    *,
                };

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigUint> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> { Pow::pow(self, &expon) }
                }

                impl<'a, T: Clone + Integer> Pow<BigUint> for &'a Ratio<T> where
                &'a T: for<'b> Pow<&'b BigUint, Output = T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> { Pow::pow(self, &expon) }
                }
                
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigUint> for Ratio<T>
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }

                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigUint> for &'a Ratio<T> where
                &'a T: Pow<&'b BigUint, Output = T>
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigInt> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> { Pow::pow(self, &expon) }
                }

                impl<'a, T: Clone + Integer> Pow<BigInt> for &'a Ratio<T> where
                &'a T: for<'b> Pow<&'b BigUint, Output = T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> { Pow::pow(self, &expon) }
                }
                
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigInt> for Ratio<T>
                {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }

                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigInt> for &'a Ratio<T> where
                &'a T: Pow<&'b BigUint, Output = T>
                { pow_signed_impl!(@ &'b BigInt, BigUint); }
            }

            pow_unsigned_impl!(u8);
            pow_unsigned_impl!(u16);
            pow_unsigned_impl!(u32);
            pow_unsigned_impl!(u64);
            pow_unsigned_impl!(u128);
            pow_unsigned_impl!(usize);
            pow_signed_impl!(i8, u8);
            pow_signed_impl!(i16, u16);
            pow_signed_impl!(i32, u32);
            pow_signed_impl!(i64, u64);
            pow_signed_impl!(i128, u128);
            pow_signed_impl!(isize, usize);
        }

        pub mod iter_sum_product
        {
            use ::
            {
                *,
            };
            /*
            use crate::Ratio;
            use core::iter::{Product, Sum};
            use num_integer::Integer;
            use num_traits::{One, Zero};
            */
            impl<T: Integer + Clone> Sum for Ratio<T>
            {
                fn sum<I>(iter: I) -> Self where
                I: Iterator<Item = Ratio<T>> 
                { iter.fold(Self::zero(), |sum, num| sum + num) }
            }

            impl<'a, T: Integer + Clone> Sum<&'a Ratio<T>> for Ratio<T>
            {
                fn sum<I>(iter: I) -> Self where
                I: Iterator<Item = &'a Ratio<T>>
                { iter.fold(Self::zero(), |sum, num| sum + num) }
            }

            impl<T: Integer + Clone> Product for Ratio<T>
            {
                fn product<I>(iter: I) -> Self where
                I: Iterator<Item = Ratio<T>>
                {
                    iter.fold(Self::one(), |prod, num| prod * num)
                }
            }

            impl<'a, T: Integer + Clone> Product<&'a Ratio<T>> for Ratio<T>
            {
                fn product<I>(iter: I) -> Self where
                I: Iterator<Item = &'a Ratio<T>>
                { iter.fold(Self::one(), |prod, num| prod * num) }
            }
        }

        pub mod opassign
        {
            
            use ::
            {
                *,
            };
            /*
            use core::ops::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};

            use crate::Ratio;
            use num_integer::Integer;
            use num_traits::NumAssign; */

            macro_rules! forward_op_assign
            {
                (impl $imp:ident, $method:ident) =>
                {
                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a Ratio<T>> for Ratio<T>
                    {
                        #[inline] fn $method(&mut self, other: &Ratio<T>) { self.$method(other.clone()) }
                    }

                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a T> for Ratio<T>
                    {
                        #[inline] fn $method(&mut self, other: &T) { self.$method(other.clone()) }
                    }
                };
            }

            impl<T: Clone + Integer + NumAssign> AddAssign for Ratio<T>
            {
                fn add_assign(&mut self, other: Ratio<T>)
                {
                    if self.denom == other.denom { self.numer += other.numer } 
                    
                    else
                    {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer + rhs_numer;
                        self.denom = lcm;
                    }

                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> DivAssign for Ratio<T>
            {
                /// (a/b) / (c/d) = (a/gcd_ac)*(d/gcd_bd) / ((c/gcd_ac)*(b/gcd_bd))
                fn div_assign(&mut self, other: Ratio<T>)
                {
                    let gcd_ac = self.numer.gcd(&other.numer);
                    let gcd_bd = self.denom.gcd(&other.denom);
                    self.numer /= gcd_ac.clone();
                    self.numer *= other.denom / gcd_bd.clone();
                    self.denom /= gcd_bd;
                    self.denom *= other.numer / gcd_ac;
                    self.reduce(); // TODO: remove this line. see #8.
                }
            }
            
            impl<T: Clone + Integer + NumAssign> MulAssign for Ratio<T>
            {
                /// a/b * c/d = (a/gcd_ad)*(c/gcd_bc) / ((d/gcd_ad)*(b/gcd_bc))
                fn mul_assign(&mut self, other: Ratio<T>)
                {
                    let gcd_ad = self.numer.gcd(&other.denom);
                    let gcd_bc = self.denom.gcd(&other.numer);
                    self.numer /= gcd_ad.clone();
                    self.numer *= other.numer / gcd_bc.clone();
                    self.denom /= gcd_bc;
                    self.denom *= other.denom / gcd_ad;
                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> RemAssign for Ratio<T>
            {
                fn rem_assign(&mut self, other: Ratio<T>)
                {
                    if self.denom == other.denom { self.numer %= other.numer }
                    else
                    {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer % rhs_numer;
                        self.denom = lcm;
                    }

                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> SubAssign for Ratio<T>
            {
                fn sub_assign(&mut self, other: Ratio<T>)
                {
                    if self.denom == other.denom { self.numer -= other.numer }                    
                    else
                    {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer - rhs_numer;
                        self.denom = lcm;
                    }

                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> AddAssign<T> for Ratio<T>
            {
                /// a/b + c/1 = (a*1 + b*c) / (b*1) = (a + b*c) / b
                fn add_assign(&mut self, other: T)
                {
                    self.numer += self.denom.clone() * other;
                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> DivAssign<T> for Ratio<T>
            {
                fn div_assign(&mut self, other: T)
                {
                    let gcd = self.numer.gcd(&other);
                    self.numer /= gcd.clone();
                    self.denom *= other / gcd;
                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> MulAssign<T> for Ratio<T>
            {
                fn mul_assign(&mut self, other: T)
                {
                    let gcd = self.denom.gcd(&other);
                    self.denom /= gcd.clone();
                    self.numer *= other / gcd;
                    self.reduce();
                }
            }

            
            impl<T: Clone + Integer + NumAssign> RemAssign<T> for Ratio<T>
            {
                /// a/b % c/1 = (a*1 % b*c) / (b*1) = (a % b*c) / b
                fn rem_assign(&mut self, other: T)
                {
                    self.numer %= self.denom.clone() * other;
                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> SubAssign<T> for Ratio<T>
            {
                /// a/b - c/1 = (a*1 - b*c) / (b*1) = (a - b*c) / b
                fn sub_assign(&mut self, other: T)
                {
                    self.numer -= self.denom.clone() * other;
                    self.reduce();
                }
            }

            forward_op_assign!(impl AddAssign, add_assign);
            forward_op_assign!(impl DivAssign, div_assign);
            forward_op_assign!(impl MulAssign, mul_assign);
            forward_op_assign!(impl RemAssign, rem_assign);
            forward_op_assign!(impl SubAssign, sub_assign);
        }
        /// Represents the ratio between two numbers.
        #[derive(Copy, Clone, Debug)]
        pub struct Ratio<T>
        {
            /// Numerator.
            numer: T,
            /// Denominator.
            denom: T,
        }
        /// Alias for a `Ratio` of machine-sized integers.
        pub type Rational = Ratio<isize>;
        /// Alias for a `Ratio` of 32-bit-sized integers.
        pub type Rational32 = Ratio<i32>;
        /// Alias for a `Ratio` of 64-bit-sized integers.
        pub type Rational64 = Ratio<i64>;
        /// Alias for arbitrary precision rationals.
        pub type BigRational = Ratio<BigInt>;
        /// These method are `const`.
        impl<T> Ratio<T>
        {
            /// Creates a `Ratio` without checking for `denom == 0` or reducing.
            #[inline] pub const fn new_raw(numer: T, denom: T) -> Ratio<T> { Ratio { numer, denom } }
            /// Deconstructs a `Ratio` into its numerator and denominator.
            #[inline] pub fn into_raw(self) -> (T, T) { (self.numer, self.denom) }
            /// Gets an immutable reference to the numerator.
            #[inline] pub const fn numer(&self) -> &T { &self.numer }
            /// Gets an immutable reference to the denominator.
            #[inline] pub const fn denom(&self) -> &T { &self.denom }
        }
        
        impl<T: Clone + Integer> Ratio<T>
        {
            /// Creates a new `Ratio`.
            #[inline] pub fn new(numer: T, denom: T) -> Ratio<T>
            {
                let mut ret = Ratio::new_raw(numer, denom);
                ret.reduce();
                ret
            }
            /// Creates a `Ratio` representing the integer `t`.
            #[inline] pub fn from_integer(t: T) -> Ratio<T> { Ratio::new_raw(t, One::one()) }
            /// Converts to an integer, rounding towards zero.
            #[inline] pub fn to_integer(&self) -> T { self.trunc().numer }
            /// Returns true if the rational number is an integer (denominator is 1).
            #[inline] pub fn is_integer(&self) -> bool { self.denom.is_one() }
            /// Puts self into lowest terms, with `denom` > 0.
            fn reduce(&mut self)
            {
                if self.denom.is_zero() {
                    panic!("denominator == 0");
                }
                if self.numer.is_zero() {
                    self.denom.set_one();
                    return;
                }
                if self.numer == self.denom {
                    self.set_one();
                    return;
                }
                let g: T = self.numer.gcd(&self.denom);
                
                #[inline] fn replace_with<T: Zero>(x: &mut T, f: impl FnOnce(T) -> T)
                {
                    let y = replace(x, T::zero());
                    *x = f(y);
                }
                
                replace_with(&mut self.numer, |x| x / g.clone());
                replace_with(&mut self.denom, |x| x / g);
                
                if self.denom < T::zero()
                {
                    replace_with(&mut self.numer, |x| T::zero() - x);
                    replace_with(&mut self.denom, |x| T::zero() - x);
                }
            }
            /// Returns a reduced copy of self.
            pub fn reduced(&self) -> Ratio<T>
            {
                let mut ret = self.clone();
                ret.reduce();
                ret
            }
            /// Returns the reciprocal.
            #[inline] pub fn recip(&self) -> Ratio<T> { self.clone().into_recip() }

            #[inline] fn into_recip(self) -> Ratio<T>
            {
                match self.numer.cmp(&T::zero())
                {
                    cmp::Ordering::Equal => panic!("division by zero"),
                    cmp::Ordering::Greater => Ratio::new_raw(self.denom, self.numer),
                    cmp::Ordering::Less => Ratio::new_raw(T::zero() - self.denom, T::zero() - self.numer),
                }
            }
            /// Rounds towards minus infinity.
            #[inline] pub fn floor(&self) -> Ratio<T>
            {
                if *self < Zero::zero()
                {
                    let one: T = One::one();
                    Ratio::from_integer( ( self.numer.clone() - self.denom.clone() + one ) / self.denom.clone() )
                }

                else { Ratio::from_integer(self.numer.clone() / self.denom.clone()) }
            }
            /// Rounds towards plus infinity.
            #[inline] pub fn ceil(&self) -> Ratio<T>
            {
                if *self < Zero::zero()
                {
                    Ratio::from_integer(self.numer.clone() / self.denom.clone())
                }
                else
                {
                    let one: T = One::one();
                    Ratio::from_integer( (self.numer.clone() + self.denom.clone() - one) / self.denom.clone() )
                }
            }
            /// Rounds to the nearest integer. Rounds half-way cases away from zero.
            #[inline] pub fn round(&self) -> Ratio<T>
            {
                let zero: Ratio<T> = Zero::zero();
                let one: T = One::one();
                let two: T = one.clone() + one.clone();
                let mut fractional = self.fract();
                
                if fractional < zero { fractional = zero - fractional };

                let half_or_larger = if fractional.denom.is_even() { fractional.numer >= fractional.denom / two }
                else { fractional.numer >= (fractional.denom / two) + one };

                if half_or_larger
                {
                    let one: Ratio<T> = One::one();
                    
                    if *self >= Zero::zero() { self.trunc() + one }
                    else { self.trunc() - one }
                }

                else { self.trunc() }
            }
            /// Rounds towards zero.
            #[inline] pub fn trunc( &self ) -> Ratio<T> 
            { Ratio::from_integer( self.numer.clone()/self.denom.clone() ) }
            /// Returns the fractional part of a number, with division rounded towards zero.
            #[inline] pub fn fract(&self) -> Ratio<T>
            { Ratio::new_raw(self.numer.clone() % self.denom.clone(), self.denom.clone()) }
            /// Raises the `Ratio` to the power of an exponent.
            #[inline] pub fn pow(&self, expon: i32) -> Ratio<T> where
            for<'a> &'a T: Pow<u32, Output = T>
            { Pow::pow(self, expon) }
        }
        
        impl Ratio<BigInt>
        {
            /// Converts a float into a rational number.
            pub fn from_float<T: FloatCore>(f: T) -> Option<BigRational>
            {
                if !f.is_finite() { return None; }

                let (mantissa, exponent, sign) = f.integer_decode();
                let bigint_sign = if sign == 1 { Sign::Plus } else { Sign::Minus };
                
                if exponent < 0
                {
                    let one: BigInt = One::one();
                    let denom: BigInt = one << ((-exponent) as usize);
                    let numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    Some(Ratio::new(BigInt::from_biguint(bigint_sign, numer), denom))
                }
                
                else
                {
                    let mut numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    numer <<= exponent as usize;

                    Some(Ratio::from_integer(BigInt::from_biguint
                    (
                        bigint_sign,
                        numer,
                    )))
                }
            }
        }

        impl<T: Clone + Integer> Default for Ratio<T>
        {
            fn default() -> Self { Ratio::zero() }
        }
        
        impl<T> From<T> for Ratio<T> where
        T: Clone + Integer
        {
            fn from(x: T) -> Ratio<T> { Ratio::from_integer(x) }
        }
        
        impl<T> From<(T, T)> for Ratio<T> where
        T: Clone + Integer
        {
            fn from(pair: (T, T)) -> Ratio<T> { Ratio::new(pair.0, pair.1) }
        }
        
        impl<T: Clone + Integer> Ord for Ratio<T>
        {
            #[inline] fn cmp(&self, other: &Self) -> cmp::Ordering
            {
                if self.denom == other.denom
                {
                    let ord = self.numer.cmp(&other.numer);

                    return if self.denom < T::zero() { ord.reverse() }
                    else { ord };
                }
                
                if self.numer == other.numer
                {
                    if self.numer.is_zero() { return cmp::Ordering::Equal; }

                    let ord = self.denom.cmp(&other.denom);

                    return if self.numer < T::zero() { ord } else { ord.reverse() };
                }
                
                let (self_int, self_rem) = self.numer.div_mod_floor(&self.denom);
                let (other_int, other_rem) = other.numer.div_mod_floor(&other.denom);
                
                match self_int.cmp(&other_int)
                {
                    cmp::Ordering::Greater => cmp::Ordering::Greater,
                    cmp::Ordering::Less => cmp::Ordering::Less,
                    cmp::Ordering::Equal =>
                    {
                        match (self_rem.is_zero(), other_rem.is_zero())
                        {
                            (true, true) => cmp::Ordering::Equal,
                            (true, false) => cmp::Ordering::Less,
                            (false, true) => cmp::Ordering::Greater,
                            (false, false) =>
                            {
                                let self_recip = Ratio::new_raw(self.denom.clone(), self_rem);
                                let other_recip = Ratio::new_raw(other.denom.clone(), other_rem);
                                self_recip.cmp(&other_recip).reverse()
                            }
                        }
                    }
                }
            }
        }

        impl<T: Clone + Integer> PartialOrd for Ratio<T>
        {
            #[inline] fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> { Some(self.cmp(other)) }
        }

        impl<T: Clone + Integer> PartialEq for Ratio<T>
        {
            #[inline] fn eq(&self, other: &Self) -> bool { self.cmp(other) == cmp::Ordering::Equal }
        }

        impl<T: Clone + Integer> Eq for Ratio<T> {}
        
        impl<T: Clone + Integer + Hash> Hash for Ratio<T>
        {
            fn hash<H: Hasher>(&self, state: &mut H)
            {
                recurse(&self.numer, &self.denom, state);
                
                fn recurse<T: Integer + Hash, H: Hasher>(numer: &T, denom: &T, state: &mut H)
                {
                    if !denom.is_zero()
                    {
                        let (int, rem) = numer.div_mod_floor(denom);
                        int.hash(state);
                        recurse(denom, &rem, state);
                    }

                    else { denom.hash(state); }
                }
            }
        }
        
        forward_all_binop!(impl Mul, mul);
        
        impl<T> Mul<Ratio<T>> for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            /// a/b * c/d = (a/gcd_ad)*(c/gcd_bc) / ((d/gcd_ad)*(b/gcd_bc))
            #[inline] fn mul(self, rhs: Ratio<T>) -> Ratio<T>
            {
                let gcd_ad = self.numer.gcd(&rhs.denom);
                let gcd_bc = self.denom.gcd(&rhs.numer);
                Ratio::new
                (
                    self.numer / gcd_ad.clone() * (rhs.numer / gcd_bc.clone()),
                    self.denom / gcd_bc * (rhs.denom / gcd_ad),
                )
            }
        }
        
        impl<T> Mul<T> for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            /// a/b * c/1 = (a*c) / (b*1) = (a*c) / b
            #[inline] fn mul(self, rhs: T) -> Ratio<T>
            {
                let gcd = self.denom.gcd(&rhs);
                Ratio::new(self.numer * (rhs / gcd.clone()), self.denom / gcd)
            }
        }

        forward_all_binop!(impl Div, div);
        
        impl<T> Div<Ratio<T>> for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            /// (a/b) / (c/d) = (a/gcd_ac)*(d/gcd_bd) / ((c/gcd_ac)*(b/gcd_bd))
            #[inline] fn div(self, rhs: Ratio<T>) -> Ratio<T>
            {
                let gcd_ac = self.numer.gcd(&rhs.numer);
                let gcd_bd = self.denom.gcd(&rhs.denom);
                Ratio::new
                (
                    self.numer / gcd_ac.clone() * (rhs.denom / gcd_bd.clone()),
                    self.denom / gcd_bd * (rhs.numer / gcd_ac),
                )
            }
        }
        
        impl<T> Div<T> for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            /// (a/b) / (c/1) = (a*1) / (b*c) = a / (b*c)
            #[inline] fn div(self, rhs: T) -> Ratio<T>
            {
                let gcd = self.numer.gcd(&rhs);
                Ratio::new(self.numer / gcd.clone(), self.denom * (rhs / gcd))
            }
        }

        arith_impl!(impl Add, add);
        arith_impl!(impl Sub, sub);
        arith_impl!(impl Rem, rem);
        
        impl<T> CheckedMul for Ratio<T> where
        T: Clone + Integer + CheckedMul
        {
            /// a/b * c/d = (a*c)/(b*d)
            #[inline] fn checked_mul(&self, rhs: &Ratio<T>) -> Option<Ratio<T>>
            {
                let gcd_ad = self.numer.gcd(&rhs.denom);
                let gcd_bc = self.denom.gcd(&rhs.numer);
                Some(Ratio::new
                (
                    (self.numer.clone() / gcd_ad.clone()).checked_mul(&(rhs.numer.clone() / gcd_bc.clone()))?,
                    (self.denom.clone() / gcd_bc).checked_mul(&(rhs.denom.clone() / gcd_ad))?,
                ))
            }
        }
        
        impl<T> CheckedDiv for Ratio<T> where
        T: Clone + Integer + CheckedMul
        {
            /// (a/b) / (c/d) = (a*d)/(b*c)
            #[inline] fn checked_div(&self, rhs: &Ratio<T>) -> Option<Ratio<T>>
            {
                if rhs.is_zero() { return None; }

                let (numer, denom) = if self.denom == rhs.denom {
                    (self.numer.clone(), rhs.numer.clone())
                } else if self.numer == rhs.numer {
                    (rhs.denom.clone(), self.denom.clone())
                } else {
                    let gcd_ac = self.numer.gcd(&rhs.numer);
                    let gcd_bd = self.denom.gcd(&rhs.denom);
                    (
                        (self.numer.clone() / gcd_ac.clone())
                            .checked_mul(&(rhs.denom.clone() / gcd_bd.clone()))?,
                        (self.denom.clone() / gcd_bd).checked_mul(&(rhs.numer.clone() / gcd_ac))?,
                    )
                };
                
                if denom.is_zero() {
                    None
                } else if numer.is_zero() {
                    Some(Self::zero())
                } else if numer == denom {
                    Some(Self::one())
                } else {
                    let g = numer.gcd(&denom);
                    let numer = numer / g.clone();
                    let denom = denom / g;
                    let raw = if denom < T::zero() {
                        // We need to keep denom positive, but 2's-complement MIN may
                        // overflow negation -- instead we can check multiplying -1.
                        let n1 = T::zero() - T::one();
                        Ratio::new_raw(numer.checked_mul(&n1)?, denom.checked_mul(&n1)?)
                    } else {
                        Ratio::new_raw(numer, denom)
                    };
                    Some(raw)
                }
            }
        }
        
        checked_arith_impl!(impl CheckedAdd, checked_add);
        checked_arith_impl!(impl CheckedSub, checked_sub);

        impl<T> Neg for Ratio<T> where
        T: Clone + Integer + Neg<Output = T>
        {
            type Output = Ratio<T>;
            #[inline] fn neg(self) -> Ratio<T> { Ratio::new_raw(-self.numer, self.denom) }
        }

        impl<'a, T> Neg for &'a Ratio<T> where
        T: Clone + Integer + Neg<Output = T>
        {
            type Output = Ratio<T>;
            #[inline] fn neg(self) -> Ratio<T> { -self.clone() }
        }

        impl<T> Inv for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            #[inline] fn inv(self) -> Ratio<T> { self.recip() }
        }

        impl<'a, T> Inv for &'a Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
            #[inline] fn inv(self) -> Ratio<T> { self.recip() }
        }
        
        impl<T: ConstZero + ConstOne> Ratio<T>
        {
            /// A constant `Ratio` 0/1.
            pub const ZERO: Self = Self::new_raw(T::ZERO, T::ONE);
        }

        impl<T: Clone + Integer + ConstZero + ConstOne> ConstZero for Ratio<T>
        {
            const ZERO: Self = Self::ZERO;
        }

        impl<T: Clone + Integer> Zero for Ratio<T>
        {
            #[inline] fn zero() -> Ratio<T> { Ratio::new_raw(Zero::zero(), One::one()) }

            #[inline] fn is_zero(&self) -> bool { self.numer.is_zero() }

            #[inline] fn set_zero(&mut self)
            {
                self.numer.set_zero();
                self.denom.set_one();
            }
        }

        impl<T: ConstOne> Ratio<T>
        {
            /// A constant `Ratio` 1/1.
            pub const ONE: Self = Self::new_raw(T::ONE, T::ONE);
        }

        impl<T: Clone + Integer + ConstOne> ConstOne for Ratio<T>
        {
            const ONE: Self = Self::ONE;
        }

        impl<T: Clone + Integer> One for Ratio<T>
        {
            #[inline] fn one() -> Ratio<T> { Ratio::new_raw(One::one(), One::one()) }

            #[inline] fn is_one(&self) -> bool { self.numer == self.denom }

            #[inline] fn set_one(&mut self)
            {
                self.numer.set_one();
                self.denom.set_one();
            }
        }

        impl<T: Clone + Integer> Num for Ratio<T>
        {
            type FromStrRadixErr = ParseRatioError;
            /// Parses `numer/denom` where the numbers are in base `radix`.
            fn from_str_radix(s: &str, radix: u32) -> Result<Ratio<T>, ParseRatioError>
            {
                if s.splitn(2, '/').count() == 2
                {
                    let mut parts = s.splitn(2, '/').map(|ss|
                    {
                        T::from_str_radix(ss, radix).map_err(|_| ParseRatioError {
                            kind: RatioErrorKind::ParseError,
                        })
                    });

                    let numer: T = parts.next().unwrap()?;
                    let denom: T = parts.next().unwrap()?;
                    if denom.is_zero() {
                        Err(ParseRatioError {
                            kind: RatioErrorKind::ZeroDenominator,
                        })
                    }
                    else { Ok(Ratio::new(numer, denom)) }
                } 
                
                else
                {
                    Err(ParseRatioError {
                        kind: RatioErrorKind::ParseError,
                    })
                }
            }
        }

        impl<T: Clone + Integer + Signed> Signed for Ratio<T>
        {
            #[inline]
            fn abs(&self) -> Ratio<T> {
                if self.is_negative() {
                    -self.clone()
                } else {
                    self.clone()
                }
            }

            #[inline]
            fn abs_sub(&self, other: &Ratio<T>) -> Ratio<T> {
                if *self <= *other {
                    Zero::zero()
                } else {
                    self - other
                }
            }

            #[inline]
            fn signum(&self) -> Ratio<T> {
                if self.is_positive() {
                    Self::one()
                } else if self.is_zero() {
                    Self::zero()
                } else {
                    -Self::one()
                }
            }

            #[inline]
            fn is_positive(&self) -> bool {
                (self.numer.is_positive() && self.denom.is_positive())
                    || (self.numer.is_negative() && self.denom.is_negative())
            }

            #[inline]
            fn is_negative(&self) -> bool {
                (self.numer.is_negative() && self.denom.is_positive())
                    || (self.numer.is_positive() && self.denom.is_negative())
            }
        }

        impl_formatting!(Display, "", "{}", "{:#}");
        impl_formatting!(Octal, "0o", "{:o}", "{:#o}");
        impl_formatting!(Binary, "0b", "{:b}", "{:#b}");
        impl_formatting!(LowerHex, "0x", "{:x}", "{:#x}");
        impl_formatting!(UpperHex, "0x", "{:X}", "{:#X}");
        impl_formatting!(LowerExp, "", "{:e}", "{:#e}");
        impl_formatting!(UpperExp, "", "{:E}", "{:#E}");

        impl<T: FromStr + Clone + Integer> FromStr for Ratio<T>
        {
            type Err = ParseRatioError;
            /// Parses `numer/denom` or just `numer`.
            fn from_str(s: &str) -> Result<Ratio<T>, ParseRatioError>
            {
                let mut split = s.splitn(2, '/');

                let n = split.next().ok_or(ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;
                let num = FromStr::from_str(n).map_err(|_| ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;

                let d = split.next().unwrap_or("1");
                let den = FromStr::from_str(d).map_err(|_| ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;

                if Zero::is_zero(&den) {
                    Err(ParseRatioError {
                        kind: RatioErrorKind::ZeroDenominator,
                    })
                } else {
                    Ok(Ratio::new(num, den))
                }
            }
        }

        impl<T> From<Ratio<T>> for (T, T)
        {
            fn from(val: Ratio<T>) -> Self { (val.numer, val.denom) }
        }
        
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct ParseRatioError
        {
            kind: RatioErrorKind,
        }

        #[derive(Copy, Clone, Debug, PartialEq)]
        enum RatioErrorKind
        {
            ParseError,
            ZeroDenominator,
        }

        impl fmt::Display for ParseRatioError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.kind.description().fmt(f) }
        }
        
        impl Error for ParseRatioError
        {
            #[allow(deprecated)] fn description(&self) -> &str { self.kind.description() }
        }

        impl RatioErrorKind
        {
            fn description(&self) -> &'static str
            {
                match *self
                {
                    RatioErrorKind::ParseError => "failed to parse integer",
                    RatioErrorKind::ZeroDenominator => "zero value denominator",
                }
            }
        }
        
        impl FromPrimitive for Ratio<BigInt>
        {
            fn from_i64(n: i64) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_i128(n: i128) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_u64(n: u64) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_u128(n: u128) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_f32(n: f32) -> Option<Self> {
                Ratio::from_float(n)
            }

            fn from_f64(n: f64) -> Option<Self> {
                Ratio::from_float(n)
            }
        }

        from_primitive_integer!(i8, approximate_float);
        from_primitive_integer!(i16, approximate_float);
        from_primitive_integer!(i32, approximate_float);
        from_primitive_integer!(i64, approximate_float);
        from_primitive_integer!(i128, approximate_float);
        from_primitive_integer!(isize, approximate_float);
        from_primitive_integer!(u8, approximate_float_unsigned);
        from_primitive_integer!(u16, approximate_float_unsigned);
        from_primitive_integer!(u32, approximate_float_unsigned);
        from_primitive_integer!(u64, approximate_float_unsigned);
        from_primitive_integer!(u128, approximate_float_unsigned);
        from_primitive_integer!(usize, approximate_float_unsigned);

        impl<T: Integer + Signed + Bounded + NumCast + Clone> Ratio<T>
        {
            pub fn approximate_float<F: FloatCore + NumCast>(f: F) -> Option<Ratio<T>>
            {
                let epsilon = <F as NumCast>::from(10e-20).expect("Can't convert 10e-20");
                approximate_float(f, epsilon, 30)
            }
        }

        impl<T: Integer + Unsigned + Bounded + NumCast + Clone> Ratio<T>
        {
            pub fn approximate_float_unsigned<F: FloatCore + NumCast>(f: F) -> Option<Ratio<T>>
            {
                let epsilon = <F as NumCast>::from(10e-20).expect("Can't convert 10e-20");
                approximate_float_unsigned(f, epsilon, 30)
            }
        }

        fn approximate_float<T, F>(val: F, max_error: F, max_iterations: usize) -> Option<Ratio<T>> where
        T: Integer + Signed + Bounded + NumCast + Clone,
        F: FloatCore + NumCast
        {
            let negative = val.is_sign_negative();
            let abs_val = val.abs();
            let r = approximate_float_unsigned(abs_val, max_error, max_iterations)?;
            Some(if negative { r.neg() } else { r })
        }
        /// Continued fractions algorithm
        /// https://web.archive.org/web/20200629111319/http://mathforum.org:80/dr.math/faq/faq.fractions.html#decfrac
        fn approximate_float_unsigned<T, F>(val: F, max_error: F, max_iterations: usize) -> Option<Ratio<T>> where
        T: Integer + Bounded + NumCast + Clone,
        F: FloatCore + NumCast
        {
            if val < F::zero() || val.is_nan() { return None; }

            let mut q = val;
            let mut n0 = T::zero();
            let mut d0 = T::one();
            let mut n1 = T::one();
            let mut d1 = T::zero();

            let t_max = T::max_value();
            let t_max_f = <F as NumCast>::from(t_max.clone())?;

            // 1/epsilon > T::MAX
            let epsilon = t_max_f.recip();

            // Overflow
            if q > t_max_f {
                return None;
            }

            for _ in 0..max_iterations {
                let a = match <T as NumCast>::from(q) {
                    None => break,
                    Some(a) => a,
                };

                let a_f = match <F as NumCast>::from(a.clone()) {
                    None => break,
                    Some(a_f) => a_f,
                };
                let f = q - a_f;

                // Prevent overflow
                if !a.is_zero()
                    && (n1 > t_max.clone() / a.clone()
                        || d1 > t_max.clone() / a.clone()
                        || a.clone() * n1.clone() > t_max.clone() - n0.clone()
                        || a.clone() * d1.clone() > t_max.clone() - d0.clone())
                {
                    break;
                }

                let n = a.clone() * n1.clone() + n0.clone();
                let d = a.clone() * d1.clone() + d0.clone();

                n0 = n1;
                d0 = d1;
                n1 = n.clone();
                d1 = d.clone();

                // Simplify fraction. Doing so here instead of at the end
                // allows us to get closer to the target value without overflows
                let g = Integer::gcd(&n1, &d1);
                if !g.is_zero() {
                    n1 = n1 / g.clone();
                    d1 = d1 / g.clone();
                }

                // Close enough?
                let (n_f, d_f) = match (<F as NumCast>::from(n), <F as NumCast>::from(d)) {
                    (Some(n_f), Some(d_f)) => (n_f, d_f),
                    _ => break,
                };
                if (n_f / d_f - val).abs() < max_error {
                    break;
                }

                // Prevent division by ~0
                if f < epsilon {
                    break;
                }
                q = f.recip();
            }

            // Overflow
            if d1.is_zero() {
                return None;
            }

            Some(Ratio::new(n1, d1))
        }
        
        impl<T: Clone + Integer + ToPrimitive + ToBigInt> ToPrimitive for Ratio<T>
        {
            fn to_i64(&self) -> Option<i64> { self.to_integer().to_i64() }

            fn to_i128(&self) -> Option<i128> { self.to_integer().to_i128() }

            fn to_u64(&self) -> Option<u64> { self.to_integer().to_u64() }

            fn to_u128(&self) -> Option<u128> { self.to_integer().to_u128() }

            fn to_f64(&self) -> Option<f64>
            {
                let float = match (self.numer.to_i64(), self.denom.to_i64()) {
                    (Some(numer), Some(denom)) => ratio_to_f64(
                        <i128 as From<_>>::from(numer),
                        <i128 as From<_>>::from(denom),
                    ),
                    _ => {
                        let numer: BigInt = self.numer.to_bigint()?;
                        let denom: BigInt = self.denom.to_bigint()?;
                        ratio_to_f64(numer, denom)
                    }
                };
                if float.is_nan() {
                    None
                } else {
                    Some(float)
                }
            }
        }

        trait Bits
        {
            fn bits(&self) -> u64;
        }
        
        impl Bits for BigInt
        {
            fn bits(&self) -> u64 { self.bits() }
        }

        impl Bits for i128
        {
            fn bits(&self) -> u64 { (128 - self.wrapping_abs().leading_zeros()).into() }
        }

        /// Converts a ratio of `T` to an f64.
        fn ratio_to_f64<T: Bits + Clone + Integer + Signed + ShlAssign<usize> + ToPrimitive>(
        numer: T,
        denom: T
        ) -> f64
        {
            use ::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, MIN_EXP, RADIX};

            assert_eq!
            (
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );

            // Inclusive upper and lower bounds to the range of exactly-representable ints in an f64.
            const MAX_EXACT_INT: i64 = 1i64 << MANTISSA_DIGITS;
            const MIN_EXACT_INT: i64 = -MAX_EXACT_INT;

            let flo_sign = numer.signum().to_f64().unwrap() / denom.signum().to_f64().unwrap();
            if !flo_sign.is_normal() {
                return flo_sign;
            }

            // Fast track: both sides can losslessly be converted to f64s. In this case, letting the
            // FPU do the job is faster and easier. In any other case, converting to f64s may lead
            // to an inexact result: https://stackoverflow.com/questions/56641441/.
            if let (Some(n), Some(d)) = (numer.to_i64(), denom.to_i64()) {
                let exact = MIN_EXACT_INT..=MAX_EXACT_INT;
                if exact.contains(&n) && exact.contains(&d) {
                    return n.to_f64().unwrap() / d.to_f64().unwrap();
                }
            }

            // Otherwise, the goal is to obtain a quotient with at least 55 bits. 53 of these bits will
            // be used as the mantissa of the resulting float, and the remaining two are for rounding.
            // There's an error of up to 1 on the number of resulting bits, so we may get either 55 or
            // 56 bits.
            let mut numer = numer.abs();
            let mut denom = denom.abs();
            let (is_diff_positive, absolute_diff) = match numer.bits().checked_sub(denom.bits()) {
                Some(diff) => (true, diff),
                None => (false, denom.bits() - numer.bits()),
            };

            // Filter out overflows and underflows. After this step, the signed difference fits in an
            // isize.
            if is_diff_positive && absolute_diff > MAX_EXP as u64 {
                return INFINITY * flo_sign;
            }
            if !is_diff_positive && absolute_diff > -MIN_EXP as u64 + MANTISSA_DIGITS as u64 + 1 {
                return 0.0 * flo_sign;
            }
            let diff = if is_diff_positive {
                absolute_diff.to_isize().unwrap()
            } else {
                -absolute_diff.to_isize().unwrap()
            };

            // Shift is chosen so that the quotient will have 55 or 56 bits. The exception is if the
            // quotient is going to be subnormal, in which case it may have fewer bits.
            let shift: isize = diff.max(MIN_EXP as isize) - MANTISSA_DIGITS as isize - 2;
            if shift >= 0 {
                denom <<= shift as usize
            } else {
                numer <<= -shift as usize
            };

            let (quotient, remainder) = numer.div_rem(&denom);

            // This is guaranteed to fit since we've set up quotient to be at most 56 bits.
            let mut quotient = quotient.to_u64().unwrap();
            let n_rounding_bits = {
                let quotient_bits = 64 - quotient.leading_zeros() as isize;
                let subnormal_bits = MIN_EXP as isize - shift;
                quotient_bits.max(subnormal_bits) - MANTISSA_DIGITS as isize
            } as usize;
            debug_assert!(n_rounding_bits == 2 || n_rounding_bits == 3);
            let rounding_bit_mask = (1u64 << n_rounding_bits) - 1;

            // Round to 53 bits with round-to-even. For rounding, we need to take into account both
            // our rounding bits and the division's remainder.
            let ls_bit = quotient & (1u64 << n_rounding_bits) != 0;
            let ms_rounding_bit = quotient & (1u64 << (n_rounding_bits - 1)) != 0;
            let ls_rounding_bits = quotient & (rounding_bit_mask >> 1) != 0;
            if ms_rounding_bit && (ls_bit || ls_rounding_bits || !remainder.is_zero()) {
                quotient += 1u64 << n_rounding_bits;
            }
            quotient &= !rounding_bit_mask;

            // The quotient is guaranteed to be exactly representable as it's now 53 bits + 2 or 3
            // trailing zeros, so there is no risk of a rounding error here.
            let q_float = quotient as f64 * flo_sign;
            ldexp(q_float, shift as i32)
        }

        /// Multiply `x` by 2 to the power of `exp`.
        fn ldexp(x: f64, exp: i32) -> f64 
        {
            use ::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, RADIX};

            assert_eq!
            (
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );

            const EXPONENT_MASK: u64 = 0x7ff << 52;
            const MAX_UNSIGNED_EXPONENT: i32 = 0x7fe;
            const MIN_SUBNORMAL_POWER: i32 = MANTISSA_DIGITS as i32;

            if x.is_zero() || x.is_infinite() || x.is_nan() {
                return x;
            }

            // Filter out obvious over / underflows to make sure the resulting exponent fits in an isize.
            if exp > 3 * MAX_EXP {
                return INFINITY * x.signum();
            } else if exp < -3 * MAX_EXP {
                return 0.0 * x.signum();
            }

            // curr_exp is the x's *biased* exponent, and is in the [-54, MAX_UNSIGNED_EXPONENT] range.
            let (bits, curr_exp) = if !x.is_normal() {
                // If x is subnormal, we make it normal by multiplying by 2^53. This causes no loss of
                // precision or rounding.
                let normal_x = x * 2f64.powi(MIN_SUBNORMAL_POWER);
                let bits = normal_x.to_bits();
                // This cast is safe because the exponent is at most 0x7fe, which fits in an i32.
                (
                    bits,
                    ((bits & EXPONENT_MASK) >> 52) as i32 - MIN_SUBNORMAL_POWER,
                )
            } else {
                let bits = x.to_bits();
                let curr_exp = (bits & EXPONENT_MASK) >> 52;
                // This cast is safe because the exponent is at most 0x7fe, which fits in an i32.
                (bits, curr_exp as i32)
            };

            // The addition can't overflow because exponent is between 0 and 0x7fe, and exp is between
            // -2*MAX_EXP and 2*MAX_EXP.
            let new_exp = curr_exp + exp;

            if new_exp > MAX_UNSIGNED_EXPONENT {
                INFINITY * x.signum()
            } else if new_exp > 0 {
                // Normal case: exponent is not too large nor subnormal.
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits)
            } else if new_exp >= -(MANTISSA_DIGITS as i32) {
                // Result is subnormal but may not be zero.
                // In this case, we increase the exponent by 54 to make it normal, then multiply the end
                // result by 2^-53. This results in a single multiplication with no prior rounding error,
                // so there is no risk of double rounding.
                let new_exp = new_exp + MIN_SUBNORMAL_POWER;
                debug_assert!(new_exp >= 0);
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits) * 2f64.powi(-MIN_SUBNORMAL_POWER)
            } else {
                // Result is zero.
                return 0.0 * x.signum();
            }
        }
    } 
    pub use self::rational::{ BigRational };

    pub mod traits
    {
        pub use ::
        {
            num_traits::{ * },
        };
    }
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

        /// Parse command line to tokens.
        /// pub fn parse_line( ... ) -> LineInfo
        pub fn parse( line:&str ) -> LineInfo
        {
            let mut result = Vec::new();
            if is::arithmetic( line )
            {
                for x in line.split( ' ' )
            {
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
            for ( i, c ) in line.chars().enumerate()
            {
                if skip_next {
                    skip_next = false;
                    continue;
                }

                if has_backslash && sep.is_empty() && ( c == '>' || c == '<' )
            {
                    sep_made = String::from( "'" );
                    token.push( c );
                    has_backslash = false;
                    continue;
                }

                if has_backslash && sep == "\"" && c != '\"' {
                    // constant with bash: "\"" --> "; "\a" --> \a
                    token.push( '\\' );
                    token.push( c );
                    has_backslash = false;
                    continue;
                }

                if has_backslash {
                    if new_round && sep.is_empty() && ( c == '|' || c == '$' ) && token.is_empty()
            {
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

                // for cases like: echo $( foo bar )
                if c == '(' && sep.is_empty()
            {
                    if !has_dollar && token.is_empty()
            {
                        // temp solution for cmd like `( ls )`, `( ls -lh )`
                        parens_left_ignored = true;
                        continue;
                    }
                    met_parenthesis = true;
                }
                if c == ')' {
                    if parens_left_ignored && !has_dollar {
                        // temp solution for cmd like `( ls )`, `( ls -lh )`
                        if i == count_chars - 1 ||
                                ( i + 1 < count_chars &&
                                line.chars().nth( i + 1 ).unwrap() == ' ' )
            {
                            continue;
                        }
                    }
                    if sep.is_empty()
            {
                        met_parenthesis = false;
                    }
                }

                if c == '\\' {
                    if sep == "'" || !sep_second.is_empty()
            {
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
                        // handle inline comments
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
                        if sep.is_empty() && !sep_made.is_empty()
            {
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
                    } else if !met_parenthesis && sep_second.is_empty() && sep.is_empty()
            {
                        if sep.is_empty() && !sep_made.is_empty()
            {
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
                        if sep.is_empty() && !sep_made.is_empty()
            {
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

                    if sep.is_empty()
            {
                        if sep_second.is_empty()
            {
                            if sep.is_empty() && !sep_made.is_empty()
            {
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
                        if sep.is_empty() && !sep_made.is_empty()
            {
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
                        // do not use continue here!
                    }

                    if sep != c.to_string() && met_parenthesis {
                        token.push( c );
                        continue;
                    }
                    if sep.is_empty() && !sep_second.is_empty() && sep_second != c.to_string()
            {
                        token.push( c );
                        continue;
                    }

                    if sep.is_empty()
            {
                        let is_an_env = regex::contains( &token, r"^[a-zA-Z0-9_]+=.*$" );
                        if !is_an_env && ( c == '\'' || c == '"' )
            {
                            sep = c.to_string();
                            continue;
                        }

                        token.push( c );
                        if sep_second.is_empty()
            {
                            sep_second = c.to_string();
                        } else if sep_second == c.to_string()
            {
                            sep_second = String::new();
                        }
                        continue;
                    } else if sep == c.to_string()
            {
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
                if sep.is_empty() && !sep_made.is_empty()
            {
                    result.push( ( sep_made.clone(), token ) );
                } else {
                    result.push( ( sep.clone(), token ) );
                }
            }

            let mut is_line_complete = true;
            if !result.is_empty()
            {
                let token_last = result[result.len() - 1].clone();
                if token_last.0.is_empty() && token_last.1 == "|" {
                    is_line_complete = false;
                }
            }

            if !sep.is_empty()
            {
                is_line_complete = semi_ok;
            }
            if has_backslash {
                is_line_complete = false;
            }

            LineInfo { tokens: result, is_complete: is_line_complete }
        }
        //  pub fn line_to_cmds( ... ) -> Vec<String>
        /// Parse command line for multiple commands.
        pub fn to_cmds( line:&str ) -> Vec<String>
        {
            let mut result = Vec::new();
            let mut sep = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let len = line.chars().count();

            for ( i, c ) in line.chars().enumerate()
            {
                if has_backslash
                {
                    token.push( '\\' );
                    token.push( c );
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
                    if sep.is_empty()
            {
                        break;
                    } else {
                        token.push( c );
                        continue;
                    }
                }

                if c == '\'' || c == '"' || c == '`'
                {
                    if sep.is_empty()
                    {
                        sep.push( c );
                        token.push( c );
                        continue;
                    }
                    
                    else if sep == c.to_string()
                    {
                        token.push( c );
                        sep = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push( c );
                        continue;
                    }
                }

                if c == '&' || c == '|'
                {
                    if sep.is_empty()
                    {
                        if i + 1 == len
                        {
                            token.push( c );
                            continue;
                        }
                        
                        else
                        {
                            let c_next = match line.chars().nth( i + 1 )
                            {
                                Some( x ) => x,
                                None =>
                                {
                                    println!( "chars nth error - should never happen" );
                                    continue;
                                }
                            };

                            if c_next != c
                            {
                                token.push( c );
                                continue;
                            }
                        }
                    }

                    if sep.is_empty()
                    {
                        sep.push( c );
                        continue;
                    }
                    
                    else if c.to_string() == sep
                    {
                        let _token = token.trim().to_string();
                        
                        if !_token.is_empty()
            { result.push( _token ); }

                        token = String::new();
                        result.push( format!( "{}{}", sep, sep ) );
                        sep = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push( c );
                        continue;
                    }
                }

                if c == ';'
                {
                    if sep.is_empty()
                    {
                        let _token = token.trim().to_string();
                        
                        if !_token.is_empty()
            { result.push( _token ); }

                        result.push( String::from( ";" ) );
                        token = String::new();
                        continue;
                    }
                    
                    else
                    {
                        token.push( c );
                        continue;
                    }
                }

                token.push( c );
            }

            if !token.is_empty()
            { result.push( token.trim().to_string() ); }

            result
        }
    }

    pub mod over
    {
        //! Module containing parsing functions.
        use ::
        {
            num::
            {
                traits::{ ToPrimitive, Zero },
            },
            *,
        };
        /*
        use super::char_stream::CharStream;
        use super::error::ParseErrorKind::*;
        use super::error::{parse_err, ParseError};
        use super::util::*;
        use super::{ParseResult, MAX_DEPTH};
        use crate::arr::{self, Arr};
        use crate::obj::Obj;
        use crate::tup::Tup;
        use crate::types::Type;
        use crate::value::Value;
        use num_bigint::BigInt;
        use num_rational::BigRational;
        use num_traits::{ToPrimitive, Zero};
        use std::collections::{HashMap, HashSet, VecDeque};
        use std::ops::Deref;
        use std::path::Path;
        */
        pub mod chars
        {
            //! Character stream used for parsing.
            use ::
            {
                *,
            };
            /*
            use std::cell::RefCell;
            use std::fs::File;
            use std::io;
            use std::io::Read;
            use std::iter::Peekable;
            use std::mem;
            use std::rc::Rc;
            use std::str::Chars; */

            #[derive( Clone, Debug )]
            struct Inner {
                file: Option<String>,
                contents: String,
                stream: Peekable<Chars<'static>>,
                line: usize,
                col: usize,
            }

            #[derive( Clone, Debug )]
            pub struct CharStream {
                inner: Rc<RefCell<Inner>>,
            }

            impl CharStream {
                pub fn from_file( path: &str ) -> io::Result<CharStream> {
                    let mut file = File::open( path )?;

                    let len = file.metadata()?.len();
                    let mut contents = String::with_capacity( len as usize );

                    file.read_to_string( &mut contents )?;

                    Self::from_string_impl( Some( String::from( path ) ), contents )
                }

                pub fn from_string( contents: String ) -> io::Result<CharStream> {
                    Self::from_string_impl( None, contents )
                }

                fn from_string_impl( file: Option<String>, contents: String ) -> io::Result<CharStream> {
                    let chars: Chars = unsafe { mem::transmute( contents.chars() ) };
                    let stream = chars.peekable();

                    Ok( CharStream {
                        inner: Rc::new( RefCell::new( Inner {
                            file,
                            contents,
                            stream,
                            line: 1,
                            col: 1,
                        } ) ),
                    } )
                }

                pub fn peek( &self ) -> Option<char> {
                    let mut inner = self.inner.borrow_mut();
                    let opt = inner.stream.peek();

                    match opt {
                        Some( ch ) => Some( *ch ),
                        None => None,
                    }
                }

                pub fn file( &self ) -> Option<String> {
                    let inner = self.inner.borrow();
                    inner.file.clone()
                }

                pub fn line( &self ) -> usize {
                    let inner = self.inner.borrow();
                    inner.line
                }

                pub fn col( &self ) -> usize {
                    let inner = self.inner.borrow();
                    inner.col
                }

                fn set_line( &mut self, value: usize ) {
                    let mut inner = self.inner.borrow_mut();
                    inner.line = value;
                }

                fn set_col( &mut self, value: usize ) {
                    let mut inner = self.inner.borrow_mut();
                    inner.col = value;
                }
            }

            impl Iterator for CharStream {
                type Item = char;

                fn next( &mut self ) -> Option<Self::Item> {
                    let opt = {
                        let mut inner = self.inner.borrow_mut();
                        inner.stream.next()
                    };

                    match opt {
                        Some( ch ) => {
                            if ch == '\n' {
                                let line = self.line();
                                self.set_line( line + 1 );
                                self.set_col( 1 );
                            } else {
                                let col = self.col();
                                self.set_col( col + 1 );
                            }
                            Some( ch )
                        }
                        None => None,
                    }
                }
            }

            pub fn format_char( ch: char ) -> String {
                match ch {
                    '\n' => String::from( "\\n" ),
                    ch => format!( "{}", ch ),
                }
            }
        } 

        pub mod format
        {
            //! Module containing functions for formatting output of objects.
            use ::
            {
                *,
            };
            /*
            
            use crate::types::Type::*;
            use crate::arr::Arr;
            use crate::obj::Obj;
            use crate::tup::Tup;
            use crate::value::Value;
            use crate::INDENT_STEP;
            use num_bigint::BigInt;
            use num_rational::BigRational;
            use num_traits::One; */

            // Returns a `String` with the given amount of spaces.
            fn indent( amount: usize ) -> String {
                " ".repeat( amount )
            }

            fn get_char_map( ch: char ) -> Option<&'static str> {
                match ch {
                    '\\' => Some( "\\\\" ),
                    '\"' => Some( "\\\"" ),
                    '\'' => Some( "\\\'" ),
                    '$' => Some( "\\$" ),
                    '\n' => Some( "\\n" ),
                    '\r' => Some( "\\r" ),
                    '\t' => Some( "\\t" ),
                    _ => None,
                }
            }

            fn replace_all( s: &str ) -> String
            {
                let mut string = String::with_capacity( s.len() );

                for ch in s.chars()
                {
                    if let Some( s ) = get_char_map( ch ) {
                        string.push_str( s );
                    } else {
                        string.push( ch );
                    }
                }
                string
            }

            /// Trait for formatting a .over representation of an object.
            pub trait Format {
                fn format( &self, full: bool, indent_amt: usize ) -> String;
            }

            impl Format for BigRational {
                fn format( &self, _full: bool, _indent_amt: usize ) -> String {
                    let frac_fmt = format!( "{}", *self );

                    if *self.denom() == BigInt::one() {
                        format!( "{}.0", frac_fmt )
                    } else {
                        frac_fmt
                    }
                }
            }

            impl Format for char {
                fn format( &self, _full: bool, _indent_amt: usize ) -> String {
                    if let Some( s ) = get_char_map( *self ) {
                        format!( "\'{}\'", s )
                    } else {
                        format!( "\'{}\'", *self )
                    }
                }
            }

            impl Format for String {
                fn format( &self, _full: bool, _indent_amt: usize ) -> String {
                    format!( "\"{}\"", replace_all( self ) )
                }
            }

            impl Format for Value {
                fn format( &self, _full: bool, indent_amt: usize ) -> String {
                    match *self {
                        Value::Null => String::from( "null" ),

                        Value::Bool( ref inner ) => {
                            if *inner {
                                String::from( "true" )
                            } else {
                                String::from( "false" )
                            }
                        }

                        Value::Int( ref inner ) => format!( "{}", inner ),

                        Value::Frac( ref inner ) => inner.format( true, indent_amt ),
                        Value::Char( ref inner ) => inner.format( true, indent_amt ),
                        Value::Str( ref inner ) => inner.format( true, indent_amt ),
                        Value::Arr( ref inner ) => inner.format( true, indent_amt ),
                        Value::Tup( ref inner ) => inner.format( true, indent_amt ),
                        Value::Obj( ref inner ) => inner.format( true, indent_amt ),
                    }
                }
            }

            impl Format for Arr {
                fn format( &self, full: bool, indent_amt: usize ) -> String {
                    match self.len() {
                        0 => {
                            if full {
                                String::from( "[]" )
                            } else {
                                String::new()
                            }
                        }
                        1 => {
                            let f = self.get( 0 ).unwrap().format( true, indent_amt );
                            if full {
                                format!( "[{}]", f )
                            } else {
                                f
                            }
                        }
                        _ => {
                            let mut s = if full {
                                String::from( "[\n" )
                            } else {
                                String::new()
                            };

                            self.with_each( |value| {
                                s.push_str( &format!( 
                                    "{}{}\n",
                                    indent( indent_amt ),
                                    value.format( true, indent_amt + INDENT_STEP )
                                ) )
                            } );

                            if full {
                                let actual_indent_amt = if indent_amt == 0 {
                                    0
                                } else {
                                    indent_amt - INDENT_STEP
                                };
                                s.push_str( &format!( "{}]", indent( actual_indent_amt ) ) );
                            }
                            s
                        }
                    }
                }
            }

            impl Format for Tup {
                fn format( &self, full: bool, indent_amt: usize ) -> String {
                    match self.len() {
                        0 => {
                            if full {
                                String::from( "()" )
                            } else {
                                String::new()
                            }
                        }
                        1 => {
                            let f = self.get( 0 ).unwrap().format( true, indent_amt );
                            if full {
                                format!( "( {} )", f )
                            } else {
                                f
                            }
                        }
                        _ => {
                            let mut s = if full {
                                String::from( "( \n" )
                            } else {
                                String::new()
                            };

                            self.with_each( |value| {
                                s.push_str( &format!( 
                                    "{}{}\n",
                                    indent( indent_amt ),
                                    value.format( true, indent_amt + INDENT_STEP )
                                ) )
                            } );

                            if full {
                                s.push_str( &format!( "{} )", indent( indent_amt - INDENT_STEP ) ) );
                            }
                            s
                        }
                    }
                }
            }

            impl Format for Obj
            {
                fn format( &self, full: bool, indent_amt: usize ) -> String
                {
                    if self.is_empty() && !self.has_parent()
                    {
                        if full { String::from( "{}" ) }
                        
                        else { String::new() }
                    } 

                    else
                    {
                        let mut s = if full { String::from( "{\n" ) } 
                        else { String::new() };

                        if let Some( parent ) = self.get_parent()
                        {
                            s.push_str( &format!
                            ( 
                                "{}^: {}\n",
                                indent( indent_amt ),
                                parent.format( true, indent_amt + INDENT_STEP )
                            ));
                        }

                        self.with_each( |field, value|
                        {
                            s.push_str( &format!
                            ( 
                                "{}{}: {}\n",
                                indent( indent_amt ),
                                field,
                                value.format( true, indent_amt + INDENT_STEP )
                            ));
                        });

                        if full { s.push_str( &format!( "{}}}", indent( indent_amt - INDENT_STEP ) ) ); }

                        s
                    }
                }
            }
        }

        type ObjMap = HashMap<String, Value>;
        type GlobalMap = HashMap<String, Value>;
        type IncludedMap = ( HashMap<String, Value>, HashSet<String> );

        lazy_static! {
            static ref OBJ_SENTINEL: Obj = Obj::from_map_unchecked( HashMap::new() );
            static ref STR_SENTINEL: Obj = Obj::from_map_unchecked( HashMap::new() );
            static ref ARR_SENTINEL: Obj = Obj::from_map_unchecked( HashMap::new() );
            static ref TUP_SENTINEL: Obj = Obj::from_map_unchecked( HashMap::new() );
        }

        /// Parses given file as an `Obj`.
        pub fn parse_obj_file( path: &str ) -> ParseResult<Obj> {
            let stream = CharStream::from_file( path )?;
            parse_obj_stream( stream, &mut ( HashMap::new(), HashSet::new() ) )
        }

        // Parses given file as an `Obj`, keeping track of already encountered includes.
        fn parse_obj_file_includes( path: &str, included: &mut IncludedMap ) -> ParseResult<Obj> {
            let stream = CharStream::from_file( path )?;
            parse_obj_stream( stream, included )
        }

        /// Parses given &str as an `Obj`.
        pub fn parse_obj_str( contents: &str ) -> ParseResult<Obj> {
            let contents = String::from( contents );
            let stream = CharStream::from_string( contents )?;
            parse_obj_stream( stream, &mut ( HashMap::new(), HashSet::new() ) )
        }

        // Parses an Obj given a character stream.
        #[inline]
        fn parse_obj_stream( mut stream: CharStream, mut included: &mut IncludedMap ) -> ParseResult<Obj> {
            let mut obj: ObjMap = HashMap::new();

            // Go to the first non-whitespace character, or return if there is none.
            if !find_char( stream.clone() ) {
                return Ok( Obj::from_map_unchecked( obj ) );
            }

            let mut globals: GlobalMap = HashMap::new();
            let mut parent = None;

            // Parse all field/value pairs for this Obj.
            while parse_field_value_pair( 
                &mut stream,
                &mut obj,
                &mut globals,
                &mut included,
                &mut parent,
                1,
                None,
            )? {}

            Ok( match parent {
                Some( parent ) => Obj::from_map_with_parent_unchecked( obj, parent ),
                None => Obj::from_map_unchecked( obj ),
            } )
        }

        // Parses a sub-Obj in a file. It *must* start with { and end with }.
        fn parse_obj( 
            mut stream: &mut CharStream,
            globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err( stream.file(), MaxDepth( stream.line(), stream.col() ) );
            }

            // We must already be at a '{'.
            let ch = stream.next().unwrap();
            assert_eq!( ch, '{' );

            // Go to the first non-whitespace character, or error if there is none.
            if !find_char( stream.clone() ) {
                return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
            }

            let mut obj: ObjMap = HashMap::new();
            let mut parent = None;

            // Parse field/value pairs.
            while parse_field_value_pair( 
                &mut stream,
                &mut obj,
                globals,
                &mut included,
                &mut parent,
                depth,
                Some( '}' ),
            )? {}

            let obj = match parent {
                Some( parent ) => Obj::from_map_with_parent_unchecked( obj, parent ),
                None => Obj::from_map_unchecked( obj ),
            };
            Ok( obj.into() )
        }

        // Parses a field/value pair.
        #[inline]
        fn parse_field_value_pair( 
            mut stream: &mut CharStream,
            obj: &mut ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            parent: &mut Option<Obj>,
            depth: usize,
            cur_brace: Option<char>,
        ) -> ParseResult<bool> {
            // Check if we're at an end delimiter instead of a field.
            let peek = stream.peek().unwrap();
            if peek == '}' && cur_brace.is_some() {
                let _ = stream.next();
                return Ok( false );
            } else if is_end_delimiter( peek ) {
                return parse_err( 
                    stream.file(),
                    InvalidClosingBracket( cur_brace, peek, stream.line(), stream.col() ),
                );
            }

            // Get the field line/col.
            let ( field_line, field_col ) = ( stream.line(), stream.col() );

            // Parse field.
            let ( field, is_global, is_parent ) = parse_field( stream.clone(), field_line, field_col )?;

            if !is_global && !is_parent && obj.contains_key( &field ) {
                return parse_err( stream.file(), DuplicateField( field, field_line, field_col ) );
            } else if is_parent && parent.is_some() {
                return parse_err( 
                    stream.file(),
                    DuplicateField( "^".into(), field_line, field_col ),
                );
            }

            // Deal with extra whitespace between field and value.
            if !find_char( stream.clone() ) {
                return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
            }

            // At a non-whitespace character, parse value.
            let ( value_line, value_col ) = ( stream.line(), stream.col() );
            let value = parse_value( 
                &mut stream,
                obj,
                &mut globals,
                &mut included,
                value_line,
                value_col,
                depth,
                cur_brace,
                true,
            )?;

            // Add value either to the globals map or to the current Obj.
            if is_global {
                if globals.contains_key( &field ) {
                    return parse_err( stream.file(), DuplicateGlobal( field, field_line, field_col ) );
                }
                globals.insert( field, value );
            } else if is_parent {
                let par = value
                    .get_obj()
                    .map_err( |e| ParseError::from_over( &e, stream.file(), value_line, value_col ) )?;
                *parent = Some( par );
            } else {
                obj.insert( field, value );
            }

            // Go to the next non-whitespace character.
            if !find_char( stream.clone() ) {
                match cur_brace {
                    Some( _ ) => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
                    None => return Ok( false ),
                }
            }

            Ok( true )
        }

        // Parses an Arr given a file.
        fn parse_arr_file( path: &str, mut included: &mut IncludedMap ) -> ParseResult<Arr> {
            let mut stream = CharStream::from_file( path )?;

            let obj: ObjMap = HashMap::new();
            let mut globals: GlobalMap = HashMap::new();

            let mut vec = Vec::new();
            let mut tcur = Type::Any;
            let mut has_any = true;

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char( stream.clone() ) {
                    break;
                }

                // At a non-whitespace character, parse value.
                let ( value_line, value_col ) = ( stream.line(), stream.col() );
                let value = parse_value( 
                    &mut stream,
                    &obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    1,
                    None,
                    true,
                )?;

                let tnew = value.get_type();

                if has_any {
                    match Type::most_specific( &tcur, &tnew ) {
                        Some( ( t, any ) ) => {
                            tcur = t;
                            has_any = any;
                        }
                        None => {
                            return parse_err( 
                                stream.file(),
                                ExpectedType( tcur, tnew, value_line, value_col ),
                            );
                        }
                    }
                } else if tcur != tnew {
                    return parse_err( 
                        stream.file(),
                        ExpectedType( tcur, tnew, value_line, value_col ),
                    );
                }

                vec.push( value );
            }

            let arr = Arr::from_vec_unchecked( vec, tcur );

            Ok( arr )
        }

        // Parses a sub-Arr in a file. It *must* start with [ and end with ].
        fn parse_arr( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err( stream.file(), MaxDepth( stream.line(), stream.col() ) );
            }

            // We must already be at a '['.
            let ch = stream.next().unwrap();
            assert_eq!( ch, '[' );

            let mut vec = Vec::new();
            let mut tcur = Type::Any;
            let mut has_any = true;

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char( stream.clone() ) {
                    return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
                }

                let peek = stream.peek().unwrap();
                if peek == ']' {
                    let _ = stream.next();
                    break;
                } else if is_end_delimiter( peek ) {
                    return parse_err( 
                        stream.file(),
                        InvalidClosingBracket( Some( ']' ), peek, stream.line(), stream.col() ),
                    );
                }

                // At a non-whitespace character, parse value.
                let ( value_line, value_col ) = ( stream.line(), stream.col() );
                let value = parse_value( 
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    depth,
                    Some( ']' ),
                    true,
                )?;

                let tnew = value.get_type();

                if has_any {
                    match Type::most_specific( &tcur, &tnew ) {
                        Some( ( t, any ) ) => {
                            tcur = t;
                            has_any = any;
                        }
                        None => {
                            return parse_err( 
                                stream.file(),
                                ExpectedType( tcur, tnew, value_line, value_col ),
                            );
                        }
                    }
                } else if tcur != tnew {
                    return parse_err( 
                        stream.file(),
                        ExpectedType( tcur, tnew, value_line, value_col ),
                    );
                }

                vec.push( value );
            }

            let arr = Arr::from_vec_unchecked( vec, tcur );

            Ok( arr.into() )
        }

        // Parses a Tup given a file.
        fn parse_tup_file( path: &str, mut included: &mut IncludedMap ) -> ParseResult<Tup> {
            let mut stream = CharStream::from_file( path )?;

            let mut vec: Vec<Value> = Vec::new();
            let obj: ObjMap = HashMap::new();
            let mut globals: GlobalMap = HashMap::new();

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char( stream.clone() ) {
                    break;
                }

                // At a non-whitespace character, parse value.
                let ( value_line, value_col ) = ( stream.line(), stream.col() );
                let value = parse_value( 
                    &mut stream,
                    &obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    1,
                    None,
                    true,
                )?;

                vec.push( value );
            }

            Ok( vec.into() )
        }

        // Parses a sub-Tup in a file. It *must* start with ( and end with ).
        fn parse_tup( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err( stream.file(), MaxDepth( stream.line(), stream.col() ) );
            }

            // We must already be at a '('.
            let ch = stream.next().unwrap();
            assert_eq!( ch, '(' );

            let mut vec = Vec::new();

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char( stream.clone() ) {
                    return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
                }

                let peek = stream.peek().unwrap();
                if peek == ')' {
                    let _ = stream.next();
                    break;
                } else if is_end_delimiter( peek ) {
                    return parse_err( 
                        stream.file(),
                        InvalidClosingBracket( Some( ')' ), peek, stream.line(), stream.col() ),
                    );
                }

                // At a non-whitespace character, parse value.
                let ( value_line, value_col ) = ( stream.line(), stream.col() );
                let value = parse_value( 
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    depth,
                    Some( ')' ),
                    true,
                )?;

                vec.push( value );
            }

            let tup = Tup::from_vec( vec );

            Ok( tup.into() )
        }

        // Gets the next field in the char stream.
        // Returns Option<( field_name, is_global, is_parent )>.
        fn parse_field( 
            mut stream: CharStream,
            line: usize,
            col: usize,
        ) -> ParseResult<( String, bool, bool )> {
            let mut field = String::new();
            let mut first = true;
            let mut is_global = false;

            let ch = stream.peek().unwrap();
            if ch == '@' {
                let ch = stream.next().unwrap();
                is_global = true;
                field.push( ch );
            }

            while let Some( ch ) = stream.next() {
                match ch {
                    ':' if !first => {
                        break;
                    }
                    ch if Obj::is_valid_field_char( ch, first ) => field.push( ch ),
                    ch => {
                        return parse_err( 
                            stream.file(),
                            InvalidFieldChar( ch, stream.line(), stream.col() - 1 ),
                        );
                    }
                }

                first = false;
            }

            // Check for invalid field names.
            match field.as_str() {
                _field_str if is_reserved( _field_str ) => {
                    parse_err( stream.file(), InvalidFieldName( field.clone(), line, col ) )
                }
                "^" => Ok( ( field.clone(), false, true ) ),
                bad if bad.starts_with( '^' ) => {
                    parse_err( stream.file(), InvalidFieldName( field.clone(), line, col ) )
                }
                _ => Ok( ( field.clone(), is_global, false ) ),
            }
        }

        // Gets the next value in the char stream.
        fn parse_value( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            line: usize,
            col: usize,
            depth: usize,
            cur_brace: Option<char>,
            is_first: bool,
        ) -> ParseResult<Value> {
            // Peek to determine what kind of value we'll be parsing.
            let res = match stream.peek().unwrap() {
                '"' => parse_str( &mut stream )?,
                '\'' => parse_char( &mut stream )?,
                '{' => parse_obj( &mut stream, &mut globals, included, depth + 1 )?,
                '[' => parse_arr( &mut stream, obj, &mut globals, included, depth + 1 )?,
                '(' => parse_tup( &mut stream, obj, &mut globals, included, depth + 1 )?,
                '@' => parse_variable( 
                    &mut stream,
                    obj,
                    globals,
                    included,
                    line,
                    col,
                    depth,
                    cur_brace,
                )?,
                '<' => parse_include( &mut stream, obj, &mut globals, &mut included, depth + 1 )?,
                ch @ '+' | ch @ '-' => {
                    parse_unary_op( &mut stream, obj, globals, included, depth, cur_brace, ch )?
                }
                ch if is_numeric_char( ch ) => parse_numeric( &mut stream, line, col )?,
                ch if Obj::is_valid_field_char( ch, true ) => parse_variable( 
                    &mut stream,
                    obj,
                    globals,
                    included,
                    line,
                    col,
                    depth,
                    cur_brace,
                )?,
                ch => {
                    return parse_err( stream.file(), InvalidValueChar( ch, line, col ) );
                }
            };

            // Process operations if this is the first value.
            if is_first {
                let mut val_deque: VecDeque<( Value, usize, usize )> = VecDeque::new();
                let mut op_deque: VecDeque<char> = VecDeque::new();
                val_deque.push_back( ( res, line, col ) );

                loop {
                    match stream.peek() {
                        Some( ch ) if is_operator( ch ) => {
                            let _ = stream.next();
                            if stream.peek().is_none() {
                                return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
                            }

                            let ( line2, col2 ) = ( stream.line(), stream.col() );

                            // Parse another value.
                            let val2 = parse_value( 
                                &mut stream,
                                obj,
                                &mut globals,
                                &mut included,
                                line2,
                                col2,
                                depth,
                                cur_brace,
                                false,
                            )?;

                            if is_priority_operator( ch ) {
                                let ( val1, line1, col1 ) = val_deque.pop_back().unwrap();
                                let res = binary_op_on_values( stream, val1, val2, ch, line2, col2 )?;
                                val_deque.push_back( ( res, line1, col1 ) );
                            } else {
                                val_deque.push_back( ( val2, line2, col2 ) );
                                op_deque.push_back( ch );
                            }
                        }
                        _ => break,
                    }
                }

                // Check for valid characters after the value.
                check_value_end( stream, cur_brace )?;

                let ( mut val1, _, _ ) = val_deque.pop_front().unwrap();
                while !op_deque.is_empty() {
                    let ( val2, line2, col2 ) = val_deque.pop_front().unwrap();
                    val1 = binary_op_on_values( 
                        stream,
                        val1,
                        val2,
                        op_deque.pop_front().unwrap(),
                        line2,
                        col2,
                    )?;
                }
                Ok( val1 )
            } else {
                Ok( res )
            }
        }

        fn parse_unary_op( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
            cur_brace: Option<char>,
            ch: char,
        ) -> ParseResult<Value> {
            let _ = stream.next();
            let line = stream.line();
            let col = stream.col();

            let res = match stream.peek() {
                Some( _ ) => parse_value( 
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    line,
                    col,
                    depth + 1,
                    cur_brace,
                    false,
                )?,
                None => return parse_err( stream.file(), UnexpectedEnd( line ) ),
            };
            unary_op_on_value( stream, res, ch, line, col )
        }

        // Gets the next numeric ( either Int or Frac ) in the character stream.
        fn parse_numeric( stream: &mut CharStream, line: usize, col: usize ) -> ParseResult<Value> {
            let mut s1 = String::new();
            let mut s2 = String::new();
            let mut dec = false;
            let mut under = false;

            while let Some( ch ) = stream.peek() {
                match ch {
                    ch if is_value_end_char( ch ) => break,
                    ch if is_digit( ch ) => {
                        if !dec {
                            s1.push( ch );
                        } else {
                            s2.push( ch );
                        }
                    }
                    '.' | ',' => {
                        if !dec {
                            dec = true;
                        } else {
                            return parse_err( 
                                stream.file(),
                                InvalidValueChar( ch, stream.line(), stream.col() ),
                            );
                        }
                    }
                    '_' => {
                        if !under {
                            under = true;
                        } else {
                            return parse_err( 
                                stream.file(),
                                InvalidValueChar( ch, stream.line(), stream.col() ),
                            );
                        }
                    }
                    _ => {
                        return parse_err( 
                            stream.file(),
                            InvalidValueChar( ch, stream.line(), stream.col() ),
                        );
                    }
                }

                if ch != '_' {
                    under = false;
                }

                let _ = stream.next();
            }

            if dec {
                // Parse a Frac from a number with a decimal.
                if s1.is_empty() && s2.is_empty() {
                    return parse_err( stream.file(), InvalidNumeric( line, col ) );
                }

                let whole: BigInt = if s1.is_empty() {
                    0u8.into()
                } else {
                    s1.parse()?
                };

                // Remove trailing zeros.
                let s2 = s2.trim_end_matches( '0' );

                let ( decimal, dec_len ): ( BigInt, usize ) = if s2.is_empty() {
                    ( 0u8.into(), 1 )
                } else {
                    ( s2.parse()?, s2.len() )
                };

                let f = frac_from_whole_and_dec( whole, decimal, dec_len );
                Ok( f.into() )
            } else {
                // Parse an Int.
                if s1.is_empty() {
                    return parse_err( stream.file(), InvalidNumeric( line, col ) );
                }

                let i: BigInt = s1.parse()?;
                Ok( i.into() )
            }
        }

        // Parses a variable name and gets a value from the corresponding variable.
        fn parse_variable( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            line: usize,
            col: usize,
            depth: usize,
            cur_brace: Option<char>,
        ) -> ParseResult<Value> {
            let mut var = String::new();
            let mut is_global = false;
            let mut dot = false;
            let mut dot_global = false;

            let ch = stream.peek().unwrap();
            if ch == '@' {
                let ch = stream.next().unwrap();
                is_global = true;
                var.push( ch );
            }

            while let Some( ch ) = stream.peek() {
                match ch {
                    '.' => {
                        let _ = stream.next();
                        match stream.peek() {
                            Some( '@' ) => dot_global = true,
                            Some( ch ) if Obj::is_valid_field_char( ch, true ) || is_numeric_char( ch ) => (),
                            Some( ch ) => {
                                return parse_err( 
                                    stream.file(),
                                    InvalidValueChar( ch, stream.line(), stream.col() ),
                                );
                            }
                            None => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
                        }

                        dot = true;
                        break;
                    }
                    ch if is_value_end_char( ch ) => break,
                    ch if Obj::is_valid_field_char( ch, false ) => {
                        let _ = stream.next();
                        var.push( ch );
                    }
                    ch => {
                        return parse_err( 
                            stream.file(),
                            InvalidValueChar( ch, stream.line(), stream.col() ),
                        );
                    }
                }
            }

            let mut value = match var.as_str() {
                "null" => Value::Null,
                "true" => Value::Bool( true ),
                "false" => Value::Bool( false ),

                "Obj" => Value::Obj( OBJ_SENTINEL.clone() ),
                "Str" => Value::Obj( STR_SENTINEL.clone() ),
                "Arr" => Value::Obj( ARR_SENTINEL.clone() ),
                "Tup" => Value::Obj( TUP_SENTINEL.clone() ),

                var @ "@" => return parse_err( stream.file(), InvalidValue( var.into(), line, col ) ),
                var if is_global => {
                    // Global variable, get value from globals map.
                    match globals.get( var ) {
                        Some( value ) => value.clone(),
                        None => {
                            let var = String::from( var );
                            return parse_err( stream.file(), GlobalNotFound( var, line, col ) );
                        }
                    }
                }
                var => {
                    // Regular variable, get value from the current Obj.
                    match obj.get( var ) {
                        Some( value ) => value.clone(),
                        None => {
                            let var = String::from( var );
                            return parse_err( stream.file(), VariableNotFound( var, line, col ) );
                        }
                    }
                }
            };

            if dot {
                value = match value {
                    Value::Arr( arr ) => {
                        let ( line, col ) = ( stream.line(), stream.col() );
                        let value = parse_value( 
                            &mut stream,
                            obj,
                            &mut globals,
                            &mut included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                            false,
                        )?;

                        match value {
                            Value::Int( int ) => match int.to_usize() {
                                Some( index ) => arr
                                    .get( index )
                                    .map_err( |e| ParseError::from_over( &e, stream.file(), line, col ) )?,
                                None => return parse_err( stream.file(), InvalidIndex( int, line, col ) ),
                            },
                            _ => {
                                return parse_err( 
                                    stream.file(),
                                    ExpectedType( Type::Int, value.get_type(), line, col ),
                                );
                            }
                        }
                    }
                    Value::Tup( tup ) => {
                        let ( line, col ) = ( stream.line(), stream.col() );
                        let value = parse_value( 
                            &mut stream,
                            obj,
                            &mut globals,
                            &mut included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                            false,
                        )?;

                        match value {
                            Value::Int( int ) => match int.to_usize() {
                                Some( index ) => tup
                                    .get( index )
                                    .map_err( |e| ParseError::from_over( &e, stream.file(), line, col ) )?,
                                None => return parse_err( stream.file(), InvalidIndex( int, line, col ) ),
                            },
                            _ => {
                                return parse_err( 
                                    stream.file(),
                                    ExpectedType( Type::Int, value.get_type(), line, col ),
                                );
                            }
                        }
                    }
                    Value::Obj( obj ) => {
                        let ( line, col ) = ( stream.line(), stream.col() );

                        if dot_global {
                            return parse_err( stream.file(), InvalidValueChar( '@', line, col ) );
                        }

                        parse_variable( 
                            &mut stream,
                            obj.map_ref(),
                            globals,
                            included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                        )?
                    }
                    _ => return parse_err( stream.file(), InvalidDot( value.get_type(), line, col ) ),
                }
            }

            Ok( value )
        }

        // Gets the next Char in the character stream.
        // Assumes the Char starts and ends with single quote marks.
        // '\', '\n', '\r', and '\t' must be escaped with '\'.
        // ''' do not need to be escaped, although they can be.
        fn parse_char( stream: &mut CharStream ) -> ParseResult<Value> {
            let ch = stream.next().unwrap();
            assert_eq!( ch, '\'' );

            let ( escape, mut ch ) = match stream.next() {
                Some( '\\' ) => ( true, '\0' ),
                Some( ch ) if ch == '\n' || ch == '\r' || ch == '\t' => {
                    return parse_err( 
                        stream.file(),
                        InvalidValueChar( ch, stream.line(), stream.col() - 1 ),
                    );
                }
                Some( ch ) => ( false, ch ),
                None => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
            };

            if escape {
                ch = match stream.next() {
                    Some( ch ) => match get::escape_char( ch ) {
                        Some( ch ) => ch,
                        None => {
                            return parse_err( 
                                stream.file(),
                                InvalidEscapeChar( ch, stream.line(), stream.col() - 1 ),
                            );
                        }
                    },
                    None => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
                }
            }

            match stream.next() {
                Some( '\'' ) => (),
                Some( ch ) => {
                    return parse_err( 
                        stream.file(),
                        InvalidValueChar( ch, stream.line(), stream.col() - 1 ),
                    );
                }
                None => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
            }

            Ok( ch.into() )
        }

        fn parse_str_file( path: &str ) -> ParseResult<String>
        {
            let s = read_file_str( path )?.replace( "\r\n", "\n" );
            Ok( s )
        }

        /// Gets the next Str in the character stream.
        fn parse_str( stream: &mut CharStream ) -> ParseResult<Value> {
            let ch = stream.next().unwrap();
            assert_eq!( ch, '"' );

            let mut s = String::new();
            let mut escape = false;

            loop {
                match stream.next() {
                    Some( ch ) => {
                        if escape {
                            match get::escape_char( ch ) {
                                Some( ch ) => s.push( ch ),
                                None => {
                                    return parse_err( 
                                        stream.file(),
                                        InvalidEscapeChar( ch, stream.line(), stream.col() - 1 ),
                                    );
                                }
                            }
                            escape = false;
                        } else {
                            match ch {
                                '"' => break,
                                '\\' => escape = true,
                                _ => s.push( ch ),
                            }
                        }
                    }
                    None => return parse_err( stream.file(), UnexpectedEnd( stream.line() ) ),
                }
            }

            // Replace \r\n line endings with \n for consistency in internal handling.
            let s = s.replace( "\r\n", "\n" );

            Ok( s.into() )
        }

        fn parse_include( 
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            enum IncludeType {
                Obj,
                Str,
                Arr,
                Tup,
            }

            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err( stream.file(), MaxDepth( stream.line(), stream.col() ) );
            }

            let ch = stream.next().unwrap();
            assert_eq!( ch, '<' );

            // Go to the next non-whitespace character, or error if there is none.
            if !find_char( stream.clone() ) {
                return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
            }

            let ( mut line, mut col ) = ( stream.line(), stream.col() );
            let mut value = parse_value( 
                &mut stream,
                obj,
                &mut globals,
                &mut included,
                line,
                col,
                depth,
                Some( '>' ),
                true,
            )?;

            let mut include_type = IncludeType::Obj; // Default include type if no token is present.
            let mut parse_again = true; // True if an include token was found.
            match value {
                Value::Obj( ref obj ) if obj.ptr_eq( &OBJ_SENTINEL ) => include_type = IncludeType::Obj,
                Value::Obj( ref obj ) if obj.ptr_eq( &STR_SENTINEL ) => include_type = IncludeType::Str,
                Value::Obj( ref obj ) if obj.ptr_eq( &ARR_SENTINEL ) => include_type = IncludeType::Arr,
                Value::Obj( ref obj ) if obj.ptr_eq( &TUP_SENTINEL ) => include_type = IncludeType::Tup,
                Value::Str( _ ) => parse_again = false,
                _ => {
                    return parse_err( 
                        stream.file(),
                        InvalidIncludeToken( value.get_type(), line, col ),
                    );
                }
            }

            if parse_again {
                // Go to the next non-whitespace character, or error if there is none.
                if !find_char( stream.clone() ) {
                    return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
                }

                line = stream.line();
                col = stream.col();
                value = parse_value( 
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    line,
                    col,
                    depth,
                    Some( '>' ),
                    true,
                )?;
            }

            // Go to the next non-whitespace character, or error if there is none.
            if !find_char( stream.clone() ) {
                return parse_err( stream.file(), UnexpectedEnd( stream.line() ) );
            }

            match stream.next().unwrap() {
                '>' => (),
                ch => {
                    return parse_err( 
                        stream.file(),
                        InvalidClosingBracket( Some( '>' ), ch, stream.line(), stream.col() - 1 ),
                    );
                }
            }

            // Get the full path of the include file.
            let include_file = match value {
                Value::Str( s ) => s,
                _ => {
                    return parse_err( 
                        stream.file(),
                        ExpectedType( Type::Str, value.get_type(), line, col ),
                    );
                }
            };

            let pathbuf = match stream.file().as_ref() {
                Some( file ) => Path::new( file )
                    .parent()
                    .unwrap()
                    .join( Path::new( &include_file ) ),
                None => Path::new( &include_file ).to_path_buf(),
            };
            let path = pathbuf.as_path();
            if !path.is_file() {
                return parse_err( stream.file(), InvalidIncludePath( include_file, line, col ) );
            }

            // Get the include file as a path relative to the current working directory.
            let path_str = match path.to_str() {
                Some( path ) => path,
                None => return parse_err( stream.file(), InvalidIncludePath( include_file, line, col ) ),
            };

            // Get the include file as an absolute path.
            let path = match path.canonicalize() {
                Ok( path ) => path,
                Err( _ ) => return parse_err( stream.file(), InvalidIncludePath( include_file, line, col ) ),
            };
            let full_path_str = match path.to_str() {
                Some( path ) => path,
                None => return parse_err( stream.file(), InvalidIncludePath( include_file, line, col ) ),
            };

            // Prevent cyclic includes by temporarily storing the current file path.
            let storing = if let Some( file ) = stream.file() {
                let full_file = String::from( Path::new( &file ).canonicalize().unwrap().to_str().unwrap() );
                included.1.insert( full_file.clone() );
                Some( full_file )
            } else {
                None
            };
            if included.1.contains( full_path_str ) {
                return parse_err( stream.file(), CyclicInclude( include_file, line, col ) );
            }

            // Get either the tracked value or parse it if it's our first time seeing the include.
            let value = if included.0.contains_key( full_path_str ) {
                let value = &included.0[full_path_str];
                value.clone()
            } else {
                let value: Value = match include_type {
                    IncludeType::Obj => parse_obj_file_includes( path_str, included )?.into(),
                    IncludeType::Str => parse_str_file( path_str )?.into(),
                    IncludeType::Arr => parse_arr_file( path_str, included )?.into(),
                    IncludeType::Tup => parse_tup_file( path_str, included )?.into(),
                };
                // Use full path as included key.
                included.0.insert( full_path_str.into(), value.clone() );
                value
            };

            // Remove the stored file path.
            if let Some( file ) = storing {
                included.1.remove( &file );
            }

            Ok( value )
        }

        // Tries to perform a unary operation on a single value.
        fn unary_op_on_value( 
            stream: &CharStream,
            val: Value,
            op: char,
            line: usize,
            col: usize,
        ) -> ParseResult<Value> {

            let t = val.get_type();

            Ok( match op {
                '+' => match t {
                    Int | Frac => val,
                    _ => return parse_err( stream.file(), UnaryOperatorError( t, op, line, col ) ),
                },
                '-' => match t {
                    Int => ( -val.get_int().unwrap() ).into(),
                    Frac => ( -val.get_frac().unwrap() ).into(),
                    _ => return parse_err( stream.file(), UnaryOperatorError( t, op, line, col ) ),
                },
                _ => return parse_err( stream.file(), UnaryOperatorError( t, op, line, col ) ),
            } )
        }

        // Tries to perform an operation on two values.
        fn binary_op_on_values( 
            stream: &CharStream,
            mut val1: Value,
            mut val2: Value,
            op: char,
            line: usize,
            col: usize,
        ) -> ParseResult<Value> {
            use crate::types::Type::*;

            let ( mut type1, mut type2 ) = ( val1.get_type(), val2.get_type() );

            // If one value is an Int and the other is a Frac, promote the Int.
            if type1 == Int && type2 == Frac {
                val1 = Value::Frac( BigRational::new( val1.get_int().unwrap(), 1.into() ) );
                type1 = Frac;
            } else if type1 == Frac && type2 == Int {
                val2 = Value::Frac( BigRational::new( val2.get_int().unwrap(), 1.into() ) );
                type2 = Frac;
            }

            Ok( match op {
                '+' => {
                    match type1 {
                        Int if type2 == Int => ( val1.get_int().unwrap() + val2.get_int().unwrap() ).into(),
                        Frac if type2 == Frac => {
                            ( val1.get_frac().unwrap() + val2.get_frac().unwrap() ).into()
                        }
                        Char if type2 == Char => {
                            let mut s = String::with_capacity( 2 );
                            s.push( val1.get_char().unwrap() );
                            s.push( val2.get_char().unwrap() );
                            s.into()
                        }
                        Char if type2 == Str => {
                            let str2 = val2.get_str().unwrap();
                            let mut s = String::with_capacity( 1 + str2.len() );
                            s.push( val1.get_char().unwrap() );
                            s.push_str( &str2 );
                            s.into()
                        }
                        Str if type2 == Char => {
                            let str1 = val1.get_str().unwrap();
                            let mut s = String::with_capacity( str1.len() + 1 );
                            s.push_str( &str1 );
                            s.push( val2.get_char().unwrap() );
                            s.into()
                        }
                        Str if type2 == Str => {
                            let str1 = val1.get_str().unwrap();
                            let str2 = val2.get_str().unwrap();
                            let mut s = String::with_capacity( str1.len() + str2.len() );
                            s.push_str( &str1 );
                            s.push_str( &str2 );
                            s.into()
                        }
                        Arr( _ ) => {
                            match Type::most_specific( &type1, &type2 ) {
                                Some( ( t, _ ) ) => {
                                    let ( arr1, arr2 ) = ( val1.get_arr().unwrap(), val2.get_arr().unwrap() );
                                    let ( mut vec1, mut vec2 ) =
                                        ( arr1.vec_ref().clone(), arr2.vec_ref().clone() );

                                    let mut vec = Vec::with_capacity( vec1.len() + vec2.len() );
                                    vec.append( &mut vec1 );
                                    vec.append( &mut vec2 );

                                    // Get the inner type.
                                    let arr = if let Arr( ref t ) = t {
                                        // Because we know the type already, we can safely use `_unchecked`.
                                        arr::Arr::from_vec_unchecked( vec, t.deref().clone() )
                                    } else {
                                        panic!( "Logic error" )
                                    };

                                    arr.into()
                                }
                                None => {
                                    return parse_err( 
                                        stream.file(),
                                        BinaryOperatorError( type1, type2, op, line, col ),
                                    );
                                }
                            }
                        }
                        _ => {
                            return parse_err( 
                                stream.file(),
                                BinaryOperatorError( type1, type2, op, line, col ),
                            );
                        }
                    }
                }
                '-' => match type1 {
                    Int if type2 == Int => ( val1.get_int().unwrap() - val2.get_int().unwrap() ).into(),
                    Frac if type2 == Frac => ( val1.get_frac().unwrap() - val2.get_frac().unwrap() ).into(),
                    _ => {
                        return parse_err( 
                            stream.file(),
                            BinaryOperatorError( type1, type2, op, line, col ),
                        );
                    }
                },
                '*' => match type1 {
                    Int if type2 == Int => ( val1.get_int().unwrap() * val2.get_int().unwrap() ).into(),
                    Frac if type2 == Frac => ( val1.get_frac().unwrap() * val2.get_frac().unwrap() ).into(),
                    _ => {
                        return parse_err( 
                            stream.file(),
                            BinaryOperatorError( type1, type2, op, line, col ),
                        );
                    }
                },
                '/' => match type1 {
                    Int if type2 == Int => {
                        let ( int1, int2 ) = ( val1.get_int().unwrap(), val2.get_int().unwrap() );
                        if int2.is_zero() {
                            return parse_err( stream.file(), InvalidNumeric( line, col ) );
                        }
                        BigRational::new( int1, int2 ).into()
                    }
                    Frac if type2 == Frac => {
                        let ( frac1, frac2 ) = ( val1.get_frac().unwrap(), val2.get_frac().unwrap() );
                        if frac2.is_zero() {
                            return parse_err( stream.file(), InvalidNumeric( line, col ) );
                        }
                        ( frac1 / frac2 ).into()
                    }
                    _ => {
                        return parse_err( 
                            stream.file(),
                            BinaryOperatorError( type1, type2, op, line, col ),
                        );
                    }
                },
                '%' => match type1 {
                    Int if type2 == Int => {
                        let int2 = val2.get_int().unwrap();
                        if int2.is_zero() {
                            return parse_err( stream.file(), InvalidNumeric( line, col ) );
                        }
                        ( val1.get_int().unwrap() % int2 ).into()
                    }
                    _ => {
                        return parse_err( 
                            stream.file(),
                            BinaryOperatorError( type1, type2, op, line, col ),
                        );
                    }
                },
                _ => {
                    return parse_err( 
                        stream.file(),
                        BinaryOperatorError( type1, type2, op, line, col ),
                    );
                }
            } )
        }

        // Finds the next non-whitespace character, ignoring comments, and update stream position.
        // Returns true if such a character was found or false if we got to the end of the stream.
        fn find_char( mut stream: CharStream ) -> bool {
            while let Some( ch ) = stream.peek() {
                match ch {
                    '#' => {
                        // Comment found; eat the rest of the line.
                        loop {
                            let ch = stream.next();
                            if ch.is_none() {
                                return false;
                            }
                            if ch.unwrap() == '\n' {
                                break;
                            }
                        }
                    }
                    ch if ch.is_whitespace() => {
                        let _ = stream.next();
                    }
                    _ => return true,
                }
            }

            false
        }

        // Helper function to make sure values are followed by a correct end delimiter.
        fn check_value_end( stream: &CharStream, cur_brace: Option<char> ) -> ParseResult<()> {
            match stream.peek() {
                Some( ch ) => match ch {
                    ch if is_value_end_char( ch ) => {
                        if is_end_delimiter( ch ) && Some( ch ) != cur_brace {
                            parse_err( 
                                stream.file(),
                                InvalidClosingBracket( cur_brace, ch, stream.line(), stream.col() ),
                            )
                        } else {
                            Ok( () )
                        }
                    }
                    ch => parse_err( 
                        stream.file(),
                        InvalidValueChar( ch, stream.line(), stream.col() ),
                    ),
                },
                None => Ok( () ),
            }
        }

    }
}
/// Cross-platform path manipulation.
pub mod path
{
    pub use std::path::{ * };

    use ::
    {
        *,
    };

    pub fn current_dir() -> String
    {
        let _current_dir = match env::current_dir()
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                log!( "cicada: PROMPT: env current_dir error: {}", e );
                return String::new();
            }
        };

        let current_dir = match _current_dir.to_str()
        {
            Some( x ) => x,
            None =>
            {
                log!( "cicada: PROMPT: to_str error" );
                return String::new();
            }
        };

        current_dir.to_string()
    }
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

    use crate::arr;
    use crate::error::OverError;
    use crate::obj;
    use crate::parse::format::Format;
    use crate::tup;
    use crate::types::Type;
    use crate::{OverResult, INDENT_STEP};
    use num_bigint::BigInt;
    use num_rational::BigRational;
    use num_traits::ToPrimitive;
    use std::fmt;
    */

    macro_rules! get_fn
    {
        ( $doc:expr, $name:tt, $type:ty, $variant:ident ) =>
        {
            #[doc=$doc]
            pub fn $name( &self ) -> OverResult<$type>
            {
                if let Value::$variant( ref inner ) = *self {
                    Ok( inner.clone() )
                } else {
                    Err( OverError::TypeMismatch( Type::$variant, self.get_type() ) )
                }
            }
        }
    }
    
    macro_rules! impl_eq
    {
        ( $valtype:ident, $type:ty ) =>
        {
            impl PartialEq<$type> for Value
            {
                fn eq( &self, other: &$type ) -> bool {
                    match *self {
                        Value::$valtype( ref value ) => value == other,
                        _ => false,
                    }
                }
            }

            impl PartialEq<Value> for $type
            {
                fn eq( &self, other: &Value ) -> bool {
                    match *other {
                        Value::$valtype( ref value ) => value == self,
                        _ => false,
                    }
                }
            }
        };
    }
    
    macro_rules! impl_eq_int
    {
        ( $type:ty, $fn:tt ) =>
        {
            impl PartialEq<$type> for Value
            {
                fn eq( &self, other: &$type ) -> bool {
                    match *self {
                        Value::Int( ref value ) => match value.$fn() {
                            Some( value ) => value == *other,
                            None => false,
                        },
                        _ => false,
                    }
                }
            }

            impl PartialEq<Value> for $type {
                fn eq( &self, other: &Value ) -> bool {
                    match *other {
                        Value::Int( ref value ) => match value.$fn() {
                            Some( value ) => value == *self,
                            None => false,
                        },
                        _ => false,
                    }
                }
            }
        };
    }
    
    macro_rules! impl_from
    {
        ( $type:ty, $fn:tt ) =>
        {
            impl From<$type> for Value
            {
                fn from( inner: $type ) -> Self { Value::$fn( inner.into() ) }
            }
        };
    }
    
    pub type Token = ( String, String );
    pub type Tokens = Vec<Token>;
    pub type Redirection = ( String, String, String );

    #[derive( Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash )]
    pub struct WaitStatus( i32, i32, i32 );

    impl WaitStatus
    {
        pub fn from_exited( pid: i32, status: i32 ) -> Self { WaitStatus( pid, 0, status ) }

        pub fn from_signaled( pid: i32, sig: i32 ) -> Self { WaitStatus( pid, 1, sig ) }

        pub fn from_stopped( pid: i32, sig: i32 ) -> Self { WaitStatus( pid, 2, sig ) }

        pub fn from_continuted( pid: i32 ) -> Self { WaitStatus( pid, 3, 0 ) }

        pub fn from_others() -> Self { WaitStatus( 0, 9, 9 ) }

        pub fn from_error( errno: i32 ) -> Self { WaitStatus( 0, 255, errno ) }

        pub fn empty() -> Self { WaitStatus( 0, 0, 0 ) }

        pub fn is_error( &self ) -> bool { self.1 == 255 }

        pub fn is_others( &self ) -> bool { self.1 == 9 }

        pub fn is_signaled( &self ) -> bool { self.1 == 1 }

        pub fn get_errno( &self ) -> nix::Error { nix::Error::from_raw( self.2 ) }

        pub fn is_exited( &self ) -> bool { self.0 != 0 && self.1 == 0 }

        pub fn is_stopped( &self ) -> bool { self.1 == 2 }

        pub fn is_continued( &self ) -> bool { self.1 == 3 }

        pub fn get_pid( &self ) -> i32 { self.0 }

        fn _get_signaled_status( &self ) -> i32 { self.2 + 128 }

        pub fn get_signal( &self ) -> i32 { self.2 }

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
                format!( "unknown: {}", self.2 )
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
        fn fmt( &self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
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
        pub tokens: Tokens,
        pub is_complete: bool,
    }

    impl LineInfo
    {
        pub fn new( tokens: Tokens ) -> Self { LineInfo { tokens, is_complete: true } }
    }
    /// command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    /// Command {
    ///     tokens: [( "", "ls" ), ( "", "-G" ), ( "\'", "foo bar" )],
    ///     redirects_to: [
    ///         ( "2", ">", "&1" ),
    ///         ( "1", ">", "/dev/null" ),
    ///     ],
    ///     redirect_from: Some( ( "<", "one-file" ) ),
    /// }
    ///
    #[derive( Debug )]
    pub struct Command
    {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }

    impl Command
    {
        pub fn from_tokens( tokens: Tokens ) -> Result<Command, String>
        {
            let mut tokens_new = tokens.clone();
            let mut redirects_from_type = String::new();
            let mut redirects_from_value = String::new();
            let mut has_redirect_from = tokens_new.iter().any( |x| x.1 == "<" || x.1 == "<<<" );

            let mut len = tokens_new.len();
            while has_redirect_from {
                if let Some( idx ) = tokens_new.iter().position( |x| x.1 == "<" ) {
                    redirects_from_type = "<".to_string();
                    tokens_new.remove( idx );
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove( idx ).1;
                        len -= 1;
                    }
                }
                if let Some( idx ) = tokens_new.iter().position( |x| x.1 == "<<<" ) {
                    redirects_from_type = "<<<".to_string();
                    tokens_new.remove( idx );
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove( idx ).1;
                        len -= 1;
                    }
                }

                has_redirect_from = tokens_new.iter().any( |x| x.1 == "<" || x.1 == "<<<" );
            }

            let tokens_final;
            let redirects_to;
            match tokens_to_redirections( &tokens_new ) {
                Ok( ( _tokens, _redirects_to ) ) => {
                    tokens_final = _tokens;
                    redirects_to = _redirects_to;
                }
                Err( e ) => {
                    return Err( e );
                }
            }

            let redirect_from = if redirects_from_type.is_empty() {
                None
            } else {
                Some( ( redirects_from_type, redirects_from_value ) )
            };

            Ok( Command{
                tokens: tokens_final,
                redirects_to,
                redirect_from,
            } )
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

        pub fn is_builtin( &self ) -> bool { is::builtin( &self.tokens[0].1 ) }
    }

    #[derive( Debug )]
    pub struct CommandLine
    {
        pub line: String,
        pub commands: Vec<Command>,
        pub envs: HashMap<String, String>,
        pub background: bool,
    }

    impl CommandLine
    {
        pub fn from_line( line: &str, sh: &mut shell::Shell ) -> Result<CommandLine, String>
        {
            let linfo = parsers::line::parse( line );
            let mut tokens = linfo.tokens;
            shell::do_expansion( sh, &mut tokens );
            let envs = drain_env_tokens( &mut tokens );

            let mut background = false;
            let len = tokens.len();
            if len > 1 && tokens[len - 1].1 == "&" {
                background = true;
                tokens.pop();
            }

            let mut commands = Vec::new();
            for sub_tokens in split_tokens_by_pipes( &tokens ) {
                match Command::from_tokens( sub_tokens ) {
                    Ok( c ) => {
                        commands.push( c );
                    }
                    Err( e ) => {
                        return Err( e );
                    }
                }
            }

            Ok( CommandLine{
                line: line.to_string(),
                commands,
                envs,
                background,
            } )
        }

        pub fn is_empty( &self ) -> bool {
            self.commands.is_empty()
        }

        pub fn with_pipeline( &self ) -> bool {
            self.commands.len() > 1
        }

        pub fn is_single_and_builtin( &self ) -> bool {
            self.commands.len() == 1 && self.commands[0].is_builtin()
        }
    }

    #[derive( Debug, Clone, Default )]
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
        pub fn all_members_stopped( &self ) -> bool
        {
            for pid in &self.pids
            {
                if !self.pids_stopped.contains( pid ) { return false; }
            }

            true
        }

        pub fn all_members_running( &self ) -> bool { self.pids_stopped.is_empty() }
    }
    
    #[derive( Clone, Debug, Default )]
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

        pub fn from_status( gid: i32, status: i32 ) -> CommandResult
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
    /// Enum of possible types for `Value`s.
    #[derive( Clone, Debug )]
    pub enum Type
    {
        /// A type used to indicate an empty Arr.
        Any,
        /// Null value.
        Null,

        /// A boolean type.
        Bool,
        /// A signed integer type.
        Int,
        /// A fractional type.
        Frac,
        /// A character type.
        Char,
        /// A string type.
        Str,
        /// An array type, containing the type of its sub-elements.
        Arr( Box<Type> ),
        /// A tuple type, containing the types of its sub-elements.
        Tup( Vec<Type> ),
        /// An object type.
        Obj,
    }

    impl Type
    {
        /// Returns true if this type is strictly the same as `other`.
        pub fn is( &self, other: &Type ) -> bool
        {
            use self::Type::*;
            match *self
            {
                Any => {
                    if let Any = *other {
                        true
                    } else {
                        false
                    }
                }

                Null => {
                    if let Null = *other {
                        true
                    } else {
                        false
                    }
                }
                Bool => {
                    if let Bool = *other {
                        true
                    } else {
                        false
                    }
                }
                Int => {
                    if let Int = *other {
                        true
                    } else {
                        false
                    }
                }
                Frac => {
                    if let Frac = *other {
                        true
                    } else {
                        false
                    }
                }
                Char => {
                    if let Char = *other {
                        true
                    } else {
                        false
                    }
                }
                Str => {
                    if let Str = *other {
                        true
                    } else {
                        false
                    }
                }
                Obj => {
                    if let Obj = *other {
                        true
                    } else {
                        false
                    }
                }

                Arr( ref t1 ) => {
                    if let Arr( ref t2 ) = *other {
                        t1.is( t2 )
                    } else {
                        false
                    }
                }

                Tup( ref tvec1 ) => {
                    if let Tup( ref tvec2 ) = *other {
                        if tvec1.len() != tvec2.len() {
                            return false;
                        }
                        tvec1.iter().zip( tvec2.iter() ).all( |( t1, t2 )| t1.is( t2 ) )
                    } else {
                        false
                    }
                }
            }
        }
        /// Returns true if this `Type` contains `Any`.
        pub fn has_any( &self ) -> bool
        {
            match *self
            {
                Type::Any => true,
                Type::Arr( ref t ) => Self::has_any( t ),
                Type::Tup( ref tvec ) => tvec.iter().any( |t| Self::has_any( t ) ),
                _ => false,
            }
        }
        /// Returns a type with the most specificity that can be applied to the two input types
        /// as well as `true` if the returned type is not maximally specific, that is, it contains `Any`.
        pub fn most_specific( type1: &Type, type2: &Type ) -> Option<( Type, bool )>
        {
            use self::Type::*;

            if let Any = *type2 {
                return Some( ( type1.clone(), type1.has_any() ) );
            }

            match *type1 {
                Any => Some( ( type2.clone(), type2.has_any() ) ),

                Arr( ref t1 ) => {
                    if let Arr( ref t2 ) = *type2 {
                        Self::most_specific( t1, t2 ).map( |( t, any )| ( Arr( Box::new( t ) ), any ) )
                    } else {
                        None
                    }
                }

                Tup( ref tvec1 ) => {
                    if let Tup( ref tvec2 ) = *type2 {
                        if tvec1.len() == tvec2.len() {
                            let mut has_any = false;

                            let tvec: Option<Vec<Type>> = tvec1
                                .iter()
                                .zip( tvec2.iter() )
                                .map( |( t1, t2 )| {
                                    Self::most_specific( t1, t2 ).map( |( t, any )| {
                                        if !has_any && any {
                                            has_any = any;
                                        }
                                        t
                                    } )
                                } )
                                .collect();

                            tvec.map( |tvec| ( Tup( tvec ), has_any ) )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }

                ref t => {
                    if t == type2 {
                        Some( ( t.clone(), false ) )
                    } else {
                        None
                    }
                }
            }
        }
    }
    /// Two types are considered equal if one of them is Any or they have the same variant.
    impl PartialEq for Type
    {
        fn eq( &self, other: &Self ) -> bool
        {
            use self::Type::*;
            
            if let Any = *other { return true; }

            match *self
            {
                Any => true,
                Arr( ref box1 ) =>
                {
                    if let Arr( ref box2 ) = *other {
                        box1 == box2
                    } else {
                        false
                    }
                }

                Tup( ref tvec1 ) =>
                {
                    if let Tup( ref tvec2 ) = *other {
                        tvec1 == tvec2
                    } else {
                        false
                    }
                }

                _ => self.is( other ),
            }
        }
    }

    impl Eq for Type {}

    impl fmt::Display for Type
    {
        fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result
        {
            use self::Type::*;

            match *self
            {
                Any => write!( f, "Any" ),
                Null => write!( f, "Null" ),
                Bool => write!( f, "Bool" ),
                Int => write!( f, "Int" ),
                Frac => write!( f, "Frac" ),
                Char => write!( f, "Char" ),
                Str => write!( f, "Str" ),
                Arr( ref boxxy ) => write!( f, "Arr( {} )", boxxy ),
                Tup( ref tvec ) => write!
                (
                    f,
                    "Tup( {} )",
                    match tvec.get( 0 )
                    {
                        Some( t1 ) => tvec
                        .iter()
                        .skip( 1 )
                        .fold( format!( "{}", t1 ), |s, t| format!( "{}, {}", s, t ) ),
                        None => String::from( "" ),
                    }
                ),
                Obj => write!( f, "Obj" ),
            }
        }
    }
    /// Enum of possible values and their inner types.
    #[derive( Clone, Debug, PartialEq )]
    pub enum Value
    {
        /// A null value.
        Null,
        /// A boolean value.
        Bool( bool ),
        /// A signed integer value.
        Int( BigInt ),
        /// A fractional value.
        Frac( BigRational ),
        /// A character value.
        Char( char ),
        /// A string value.
        Str( String ),
        /// An array value.
        Arr( arr::Arr ),
        /// A tuple value.
        Tup( tup::Tup ),
        /// An object value.
        Obj( obj::Obj ),
    }

    impl Value
    {
        /// Returns true if this `Value` is null.
        pub fn is_null( &self ) -> bool
        {
            if let Value::Null = *self { true } else { false }
        }
        /// Returns the `Type` of this `Value`.
        pub fn get_type( &self ) -> Type
        {
            use self::Value::*;
            match *self
            {
                Null => Type::Null,
                Bool( _ ) => Type::Bool,
                Int( _ ) => Type::Int,
                Frac( _ ) => Type::Frac,
                Char( _ ) => Type::Char,
                Str( _ ) => Type::Str,
                Arr( ref arr ) => Type::Arr( Box::new( arr.inner_type() ) ),
                Tup( ref tup ) => Type::Tup( tup.inner_type_vec() ),
                Obj( _ ) => Type::Obj,
            }
        }

        get_fn!
        ( 
            "Returns the `bool` contained in this `Value`.",
            get_bool,
            bool,
            Bool
        );
        get_fn!
        ( 
            "Returns the `BigInt` contained in this `Value`.",
            get_int,
            BigInt,
            Int
        );
        /// Returns the `BigRational` contained in this `Value`.
        pub fn get_frac( &self ) -> OverResult<BigRational>
        {
            match *self
            {
                Value::Frac( ref inner ) => Ok( inner.clone() ),
                Value::Int( ref inner ) => Ok( frac!( inner.clone(), 1 ) ),
                _ => Err( OverError::TypeMismatch( Type::Frac, self.get_type() ) ),
            }
        }

        get_fn!
        ( 
            "Returns the `char` contained in this `Value`.",
            get_char,
            char,
            Char
        );
        get_fn!
        ( 
            "Returns the `String` contained in this `Value`.",
            get_str,
            String,
            Str
        );
        get_fn!
        ( 
            "Returns the `Obj` contained in this `Value`.",
            get_obj,
            obj::Obj,
            Obj
        );
        /// Returns the `Arr` contained in this `Value`.
        pub fn get_arr( &self ) -> OverResult<arr::Arr>
        {
            if let Value::Arr( ref inner ) = *self { Ok( inner.clone() ) }
            else
            {
                Err( OverError::TypeMismatch
                (
                    Type::Arr( Box::new( Type::Any ) ),
                    self.get_type(),
                ))
            }
        }
        /// Returns the `Tup` contained in this `Value`.
        pub fn get_tup( &self ) -> OverResult<tup::Tup>
        {
            if let Value::Tup( ref inner ) = *self { Ok( inner.clone() ) }
            else { Err( OverError::TypeMismatch( Type::Tup( vec![] ), self.get_type() ) ) }
        }
    }

    impl fmt::Display for Value
    {
        fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result
        { write!( f, "{}", self.format( true, INDENT_STEP ) ) }
    }
    
    impl<'a> PartialEq<&'a str> for Value
    {
        fn eq( &self, other: &&str ) -> bool
        {
            match *self
            {
                Value::Str( ref value ) => value == &other.replace( "\r\n", "\n" ),
                _ => false,
            }
        }
    }

    impl<'a> PartialEq<Value> for &'a str
    {
        fn eq( &self, other: &Value ) -> bool
        {
            match *other
            {
                Value::Str( ref value ) => value == &self.replace( "\r\n", "\n" ),
                _ => false,
            }
        }
    }

    impl PartialEq<String> for Value
    {
        fn eq( &self, other: &String ) -> bool { &other.as_str() == self }
    }

    impl PartialEq<Value> for String
    {
        fn eq( &self, other: &Value ) -> bool { &self.as_str() == other }
    }

    impl<'a> From<&'a str> for Value
    {
        fn from( inner: &str ) -> Self { Value::Str( inner.into() ) }
    }

    impl_eq!( Bool, bool );
    impl_eq!( Int, BigInt );
    impl_eq!( Frac, BigRational );
    impl_eq!( Char, char );
    impl_eq!( Arr, arr::Arr );
    impl_eq!( Tup, tup::Tup );
    impl_eq!( Obj, obj::Obj );
    impl_eq_int!( usize, to_usize );
    impl_eq_int!( u8, to_u8 );
    impl_eq_int!( u16, to_u16 );
    impl_eq_int!( u32, to_u32 );
    impl_eq_int!( u64, to_u64 );
    impl_eq_int!( i8, to_i8 );
    impl_eq_int!( i16, to_i16 );
    impl_eq_int!( i32, to_i32 );
    impl_eq_int!( i64, to_i64 );
    impl_from!( bool, Bool );
    impl_from!( usize, Int );
    impl_from!( u8, Int );
    impl_from!( u16, Int );
    impl_from!( u32, Int );
    impl_from!( u64, Int );
    impl_from!( i8, Int );
    impl_from!( i16, Int );
    impl_from!( i32, Int );
    impl_from!( i64, Int );
    impl_from!( BigInt, Int );
    impl_from!( BigRational, Frac );
    impl_from!( char, Char );
    impl_from!( String, Str );
    impl_from!( arr::Arr, Arr );
    impl_from!( tup::Tup, Tup );
    impl_from!( obj::Obj, Obj );

    fn split_tokens_by_pipes( tokens: &[Token] ) -> Vec<Tokens>
    {
        let mut cmd = Vec::new();
        let mut cmds = Vec::new();
        for token in tokens {
            let sep = &token.0;
            let value = &token.1;
            if sep.is_empty() && value == "|" {
                if cmd.is_empty() {
                    return Vec::new();
                }
                cmds.push( cmd.clone() );
                cmd = Vec::new();
            } else {
                cmd.push( token.clone() );
            }
        }
        if cmd.is_empty() {
            return Vec::new();
        }
        cmds.push( cmd.clone() );
        cmds
    }

    fn drain_env_tokens( tokens: &mut Tokens ) -> HashMap<String, String>
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new( r"^( [a-zA-Z0-9_]+ )=( .* )$" ).unwrap();
        
        for ( sep, text ) in tokens.iter()
        {
            if !sep.is_empty() || !regex::contains( text, r"^( [a-zA-Z0-9_]+ )=( .* )$" ) { break; }

            for cap in re.captures_iter( text )
            {
                let name = cap[1].to_string();
                let value = parsers::parser_line::unquote( &cap[2] );
                envs.insert( name, value );
            }

            n += 1;
        }

        if n > 0 { tokens.drain( 0..n ); }

        envs
    }
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

    pub fn close( fd: i32 ) { unsafe { libc::close( fd ); } }

    pub fn dup( fd: i32 ) -> i32 { unsafe { libc::dup( fd ) } }

    pub fn dup2( src: i32, dst: i32 ) { unsafe { libc::dup2( src, dst ); } }
    
    pub fn fork() -> Result<ForkResult> { unsafe{ nix_fork() } }    

    pub fn pipe() -> ::result::Result<( RawFd, RawFd ), Error>
    {
        unsafe
        {
            let mut fds = mem::MaybeUninit::<[c_int; 2]>::uninit();
            let res = libc::pipe( fds.as_mut_ptr() as *mut c_int );
            Error::result( res )?;
            Ok( ( fds.assume_init()[0], fds.assume_init()[1] ) )
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

    pub fn find_first_group( ptn:&str, text:&str ) -> Option<String>
    {
        let re = match regex::Regex::new( ptn )
        {
            Ok( x ) => x,
            Err( _ ) => return None,
        };

        match re.captures( text )
        {
            Some( caps ) =>
            {
                if let Some( x ) = caps.get( 1 )
            { return Some( x.as_str().to_owned() ); }
            }

            None => { return None; }
        }

        None
    }

    pub fn contains( text:&str, ptn:&str ) -> bool
    {
        let re = match regex::Regex::new( ptn )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println!( "Regex new error: {:?}", e );
                return false;
            }
        };

        re.is_match( text )
    }

    pub fn replace_all( text:&str, ptn:&str, ptn_to:&str ) -> String
    {
        let re = regex::Regex::new( ptn ).unwrap();
        let result = re.replace_all( text, ptn_to );
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
    pub fn run_script( ... ) -> i32 */
    pub fn run( sh:&mut shell::Shell, args:&Vec<String> ) -> i32
    {
        let src_file = &args[1];
        let full_src_file: String;
        if src_file.contains( '/' )
            {
            full_src_file = src_file.clone();
        } else {
            let full_path = libs::path::find_file_in_path( src_file, false );
            if full_path.is_empty()
            {
                // not in PATH and not in current work directory
                if !Path::new( src_file ).exists()
            {
                    println_stderr!( "cicada: {}: no such file", src_file );
                    return 1;
                }
                full_src_file = format!( "./{}", src_file );
            } else {
                full_src_file = full_path.clone();
            }
        }

        if !Path::new( &full_src_file ).exists()
            {
            println_stderr!( "cicada: {}: no such file", src_file );
            return 1;
        }
        if Path::new( &full_src_file ).is_dir()
            {
            println_stderr!( "cicada: {}: is a directory", src_file );
            return 1;
        }

        let mut file;
        match File::open( &full_src_file )
            {
            Ok( x ) => file = x,
            Err( e ) => {
                println_stderr!( "cicada: {}: failed to open file - {:?}", &full_src_file, e.kind() );
                return 1;
            }
        }
        let mut text = String::new();
        match file.read_to_string( &mut text )
            {
            Ok( _ ) => {}
            Err( e ) => {
                match e.kind()
            {
                    ErrorKind::InvalidData => {
                        println_stderr!( "cicada: {}: not a valid script file", &full_src_file );
                    }
                    _ => {
                        println_stderr!( "cicada: {}: error: {:?}", &full_src_file, e );
                    }
                }
                return 1;
            }
        }

        if text.contains( "\\\n" )
            {
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
        for line in text.clone().lines()
            {
            if re_func_head.is_match( line.trim() )
            {
                enter_func = true;
                let cap = re_func_head.captures( line.trim() ).unwrap();
                func_name = cap[1].to_string();
                func_body = String::new();
                continue;
            }
            if re_func_tail.is_match( line.trim() )
            {
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
        if let Some( last ) = cr_list.last()
            {
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
    use ::
    {
        collections::{HashMap, HashSet},
        *,
    };
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
    #[derive( Debug, Clone )]
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
        pub is_login:bool,
        pub exit_on_error:bool,
        pub has_terminal:bool,
        pub session_id: String,
    }

    impl Shell
    {
        pub fn new() -> Shell {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = tools::get_current_dir();
            let has_terminal = proc_has_terminal();
            let ( session_id, _ ) = uuid.split_at( 13 );
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

        pub fn insert_job( &mut self, gid: i32, pid: i32, cmd:&str, status:&str, bg:bool )
            {
            let mut i = 1;
            loop {
                let mut indexed_job_missing = false;
                if let Some( x ) = self.jobs.get_mut( &i )
            {
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

        pub fn get_job_by_id( &self, job_id: i32 ) -> Option<&types::Job> {
            self.jobs.get( &job_id )
        }

        pub fn mark_job_member_continued( &mut self, pid: i32,
                                        gid: i32 ) -> Option<&types::Job> {
            if self.jobs.is_empty()
            {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i )
            {
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

        pub fn mark_job_member_stopped( &mut self, pid: i32,
                                    gid: i32 ) -> Option<&types::Job> {
            if self.jobs.is_empty()
            {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i )
            {
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

        pub fn get_job_by_gid( &self, gid: i32 ) -> Option<&types::Job> {
            if self.jobs.is_empty()
            {
                return None;
            }

            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get( &i )
            {
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

        pub fn mark_job_as_running( &mut self, gid: i32, bg:bool )
            {
            if self.jobs.is_empty()
            {
                return;
            }

            let mut i = 1;
            loop {
                if let Some( job ) = self.jobs.get_mut( &i )
            {
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

        pub fn mark_job_as_stopped( &mut self, gid: i32 )
            {
            if self.jobs.is_empty()
            {
                return;
            }

            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get_mut( &i )
            {
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

        pub fn remove_pid_from_job( &mut self, gid: i32, pid: i32 ) -> Option<types::Job> {
            if self.jobs.is_empty()
            {
                return None;
            }

            let mut empty_pids = false;
            let mut i = 1;
            loop {
                if let Some( x ) = self.jobs.get_mut( &i )
            {
                    if x.gid == gid {
                        if let Ok( i_pid ) = x.pids.binary_search( &pid )
            {
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

        /// Update existing *ENV Variable* if such name exists in ENVs,
        /// otherwise, we define a local *Shell Variable*, which would not
        /// be exported into child processes.
        pub fn set_env( &mut self, name:&str, value:&str )
            {
            if env::var( name ).is_ok()
            {
                env::set_var( name, value );
            } else {
                self.envs.insert( name.to_string(), value.to_string() );
            }
        }

        /// get *Shell Variable*, or *ENV Variable*.
        pub fn get_env( &self, name:&str ) -> Option<String> {
            match self.envs.get( name )
            {
                Some( x ) => Some( x.to_string() ),
                None => {
                    match env::var( name )
            {
                        Ok( x ) => Some( x ),
                        Err( _ ) => None,
                    }
                }
            }
        }

        /// Remove environment variable, function from the environment of
        /// the currently running process
        pub fn remove_env( &mut self, name:&str ) -> bool {
            // function names can contain the `-` char.
            let ptn_env = Regex::new( r"^[a-zA-Z_][a-zA-Z0-9_-]*$" ).unwrap();
            if !ptn_env.is_match( name )
            {
                return false;
            }

            env::remove_var( name );
            self.envs.remove( name );
            self.remove_func( name );
            true
        }

        pub fn remove_path( &mut self, path:&str )
            {
            if let Ok( paths ) = env::var( "PATH" )
            {
                let mut paths_new: Vec<&str> = paths.split( ":" ).collect();
                paths_new.retain( |&x| x != path );
                env::set_var( "PATH", paths_new.join( ":" ).as_str() );
            }
        }

        fn remove_func( &mut self, name:&str )
            {
            self.funcs.remove( name );
        }

        pub fn set_func( &mut self, name:&str, value:&str )
            {
            self.funcs.insert( name.to_string(), value.to_string() );
        }

        pub fn get_func( &self, name:&str ) -> Option<String> {
            self.funcs.get( name ).map( |x| x.to_string() )
        }

        pub fn get_alias_list( &self ) -> Vec<( String, String )> {
            let mut result = Vec::new();
            for ( name, value ) in &self.aliases {
                result.push( ( name.clone(), value.clone() ) );
            }
            result
        }

        pub fn add_alias( &mut self, name:&str, value:&str )
            {
            self.aliases.insert( name.to_string(), value.to_string() );
        }

        pub fn is_alias( &self, name:&str ) -> bool {
            self.aliases.contains_key( name )
        }

        pub fn remove_alias( &mut self, name:&str ) -> bool {
            let opt = self.aliases.remove( name );
            opt.is_some()
        }

        pub fn get_alias_content( &self, name:&str ) -> Option<String> {
            let result = match self.aliases.get( name )
            {
                Some( x ) => x.to_string(),
                None => String::new(),
            };
            if result.is_empty()
            {
                None
            } else {
                Some( result )
            }
        }
    }

    pub unsafe fn give_terminal_to( gid: i32 ) -> bool
    {
        let mut mask: libc::sigset_t = mem::zeroed();
        let mut old_mask: libc::sigset_t = mem::zeroed();

        libc::sigemptyset( &mut mask );
        libc::sigaddset( &mut mask, libc::SIGTSTP );
        libc::sigaddset( &mut mask, libc::SIGTTIN );
        libc::sigaddset( &mut mask, libc::SIGTTOU );
        libc::sigaddset( &mut mask, libc::SIGCHLD );

        let rcode = libc::pthread_sigmask( libc::SIG_BLOCK, &mask, &mut old_mask );
        if rcode != 0 {
            log!( "failed to call pthread_sigmask" );
        }
        let rcode = libc::tcsetpgrp( 1, gid );
        let given;
        if rcode == -1 {
            given = false;
            let e = errno();
            let code = e.0;
            log!( "error in give_terminal_to()
            {}: {}", code, e );
        } else {
            given = true;
        }
        let rcode = libc::pthread_sigmask( libc::SIG_SETMASK, &old_mask, &mut mask );
        if rcode != 0 {
            log!( "failed to call pthread_sigmask" );
        }
        given
    }

    fn env_in_token( token:&str ) -> bool
    {
        if regex::contains( token, r"\$\{?[\$\?]\}?" )
            {
            return true;
        }

        let ptn_env_name = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let ptn_env = format!( r"\$\{{?{}\}}?", ptn_env_name );
        if !regex::contains( token, &ptn_env )
            {
            return false;
        }

        // do not expand env in a command substitution, e.g.:
        // - echo $( echo '$HOME' )
        // - VERSION=$( foobar -h | grep 'version: v' | awk '{print $NF}' )
        let ptn_cmd_sub1 = format!( r"^{}=`.*`$", ptn_env_name );
        let ptn_cmd_sub2 = format!( r"^{}=\$\( .*\ )$", ptn_env_name );
        if regex::contains( token, &ptn_cmd_sub1 )
            || regex::contains( token, &ptn_cmd_sub2 )
            || regex::contains( token, r"^\$\( .+\ )$" )
        {
            return false;
        }

        // for cmd-line like `alias foo='echo $PWD'`
        let ptn_env = format!( r"='.*\$\{{?{}\}}?.*'$", ptn_env_name );
        !regex::contains( token, &ptn_env )
    }

    fn do_command_substitution_for_dollar( sh:&mut Shell, tokens:&mut types::Tokens )
            {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();

        for ( sep, token ) in tokens.iter()
            {
            if sep == "'" || sep == "\\" || !should_do_dollar_command_extension( token )
            {
                idx += 1;
                continue;
            }

            let mut line = token.to_string();
            loop {
                if !should_do_dollar_command_extension( &line )
            {
                    break;
                }

                let ptn_cmd = r"\$\( ( .+ )\ )";
                let cmd = match regex::find_first_group( ptn_cmd, &line )
            {
                    Some( x ) => x,
                    None => {
                        println_stderr!( "cicada: calculator: no first group" );
                        return;
                    }
                };

                let cmd_result = match CommandLine::from_line( &cmd, sh )
            {
                    Ok( c ) => {
                        log!( "run subcmd dollar: {:?}", &cmd );
                        let ( term_given, cr ) = core::run_pipeline( sh, &c, true, true, false );
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid( 0 );
                                give_terminal_to( gid );
                            }
                        }

                        cr
                    }
                    Err( e ) => {
                        println_stderr!( "cicada: {}", e );
                        continue;
                    }
                };

                let output_txt = cmd_result.stdout.trim();

                let ptn = r"( ?P<head>[^\$]* )\$\( .+\ )( ?P<tail>.* )";
                let re;
                if let Ok( x ) = Regex::new( ptn )
            {
                    re = x;
                } else {
                    return;
                }

                let to = format!( "${{head}}{}${{tail}}", output_txt );
                let line_ = line.clone();
                let result = re.replace( &line_, to.as_str() );
                line = result.to_string();
            }

            buff.insert( idx, line.clone() );
            idx += 1;
        }

        for ( i, text ) in buff.iter()
            {
            tokens[*i].1 = text.to_string();
        }
    }

    fn do_command_substitution_for_dot( sh:&mut Shell, tokens:&mut types::Tokens )
            {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();
        for ( sep, token ) in tokens.iter()
            {
            let new_token: String;
            if sep == "`" {
                log!( "run subcmd dot1: {:?}", token );
                let cr = match CommandLine::from_line( token, sh )
            {
                    Ok( c ) => {
                        let ( term_given, _cr ) = core::run_pipeline( sh, &c, true, true, false );
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid( 0 );
                                give_terminal_to( gid );
                            }
                        }

                        _cr
                    }
                    Err( e ) => {
                        println_stderr!( "cicada: {}", e );
                        continue;
                    }
                };

                new_token = cr.stdout.trim().to_string();
            } else if sep == "\"" || sep.is_empty()
            {
                let re;
                if let Ok( x ) = Regex::new( r"^( [^`]* )`( [^`]+ )`( .* )$" )
            {
                    re = x;
                } else {
                    println_stderr!( "cicada: re new error" );
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
                loop {
                    if !re.is_match( &_token )
            {
                        if !_token.is_empty()
            {
                            _item = format!( "{}{}", _item, _token );
                        }
                        break;
                    }
                    for cap in re.captures_iter( &_token )
            {
                        _head = cap[1].to_string();
                        _tail = cap[3].to_string();
                        log!( "run subcmd dot2: {:?}", &cap[2] );

                        let cr = match CommandLine::from_line( &cap[2], sh )
            {
                            Ok( c ) => {
                                let ( term_given, _cr ) = core::run_pipeline( sh, &c, true, true, false );
                                if term_given {
                                    unsafe {
                                        let gid = libc::getpgid( 0 );
                                        give_terminal_to( gid );
                                    }
                                }

                                _cr
                            }
                            Err( e ) => {
                                println_stderr!( "cicada: {}", e );
                                continue;
                            }
                        };

                        _output = cr.stdout.trim().to_string();
                    }
                    _item = format!( "{}{}{}", _item, _head, _output );
                    if _tail.is_empty()
            {
                        break;
                    }
                    _token = _tail.clone();
                }
                new_token = _item;
            } else {
                idx += 1;
                continue;
            }

            buff.insert( idx, new_token.clone() );
            idx += 1;
        }

        for ( i, text ) in buff.iter()
            {
            tokens[*i].1 = text.to_string();
        }
    }

    fn do_command_substitution( sh:&mut Shell, tokens:&mut types::Tokens )
            {
        do_command_substitution_for_dot( sh, tokens );
        do_command_substitution_for_dollar( sh, tokens );
    }

    pub fn do_expansion( sh:&mut Shell, tokens:&mut types::Tokens )
            {
        let line = parsers::parser_line::tokens_to_line( tokens );
        if tools::is_arithmetic( &line )
            {
            return;
        }

        if tokens.len() >= 2 && tokens[0].1 == "export" && tokens[1].1.starts_with( "PROMPT=" )
            {
            return;
        }

        expand_alias( sh, tokens );
        expand_home( tokens );
        expand_env( sh, tokens );
        expand_brace( tokens );
        expand_glob( tokens );
        do_command_substitution( sh, tokens );
        expand_brace_range( tokens );
    }

    pub fn trim_multiline_prompts( line:&str ) -> String {
        // remove sub-prompts from multiple line mode
        // 1. assuming '\n' char cannot be typed manually?
        // 2. `>>` is defined as `src/prompt/multilines.rs`
        let line_new = regex::replace_all( line, r"\\\n>> ", "" );
        let line_new = regex::replace_all( &line_new, r"\| *\n>> ", "| " );
        regex::replace_all( &line_new, r"( ?P<NEWLINE>\n )>> ", "$NEWLINE" )
    }

    fn proc_has_terminal() -> bool {
        unsafe {
            let tgid = libc::tcgetpgrp( 0 );
            let pgid = libc::getpgid( 0 );
            tgid == pgid
        }
    }
}
/// Send and receive signals to processes
pub mod signals
{
    pub use nix::sys::signal::{ * };
    use ::
    {
        collections::{ HashMap, HashSet },
        error::{ errno, set_errno },
        iter::{ FromIterator },
        nix::
        {
            sys::wait::{ WaitPidFlag as WF, WaitStatus as WS, waitpid },
            unistd::{ Pid },
        },
        sync::{ Mutex },

        *,
    };

    macro_rules! impl_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) =>
        {
            impl ops::$tr for SignalSet
            {
                type Output = SignalSet;
                fn $tr_meth( self, rhs: SignalSet ) -> SignalSet { self.$method( rhs ) }
            }
        }
    }

    macro_rules! impl_mut_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) =>
        {
            impl ops::$tr for SignalSet 
            {
                fn $tr_meth( &mut self, rhs: SignalSet )
            { *self = self.$method( rhs ); }
            }
        }
    }

    macro_rules! impl_unary_op
    {
        ( $tr:ident , $tr_meth:ident , $method:ident ) =>
        {
            impl ops::$tr for SignalSet
            {
                type Output = SignalSet;
                fn $tr_meth( self ) -> SignalSet { self.$method() }
            }
        }
    }

    lazy_static!
    {
        static ref REAP_MAP: Mutex<HashMap<i32, i32>> = Mutex::new( HashMap::new() );
        static ref STOP_MAP: Mutex<HashSet<i32>> = Mutex::new( HashSet::new() );
        static ref CONT_MAP: Mutex<HashSet<i32>> = Mutex::new( HashSet::new() );
        static ref KILL_MAP: Mutex<HashMap<i32, i32>> = Mutex::new( HashMap::new() );
    }
    
    pub const NUM_SIGNALS: u8 = 6;
    /// Signal received through a terminal device
    #[derive( Copy, Clone, Debug, Eq, PartialEq )]
    pub enum Signal 
    {
        /// Break signal ( `CTRL_BREAK_EVENT` ); Windows only
        Break,
        /// Continue signal ( `SIGCONT` ); Unix only
        Continue,
        /// Interrupt signal ( `SIGINT` on Unix, `CTRL_C_EVENT` on Windows )
        Interrupt,
        /// Terminal window resize ( `SIGWINCH` on Unix,
        /// `WINDOW_BUFFER_SIZE_EVENT` on Windows )
        ///
        /// When this signal is received, it will be translated into an
        /// `Event::Resize( _ )` value containing the new size of the terminal.
        Resize,
        /// Suspend signal ( `SIGTSTP` ); Unix only
        Suspend,
        /// Quit signal ( `SIGQUIT` ); Unix only
        Quit
    }
    
    impl Signal
    {
        fn as_bit( &self ) -> u8 { 1 << ( *self as u8 ) }

        fn all_bits() -> u8 { ( 1 << NUM_SIGNALS ) - 1 }
    }

    impl ops::BitOr for Signal
    {
        type Output = SignalSet;

        fn bitor( self, rhs: Signal ) -> SignalSet
        {
            let mut set = SignalSet::new();

            set.insert( self );
            set.insert( rhs );
            set
        }
    }

    impl ops::Not for Signal
    {
        type Output = SignalSet;

        fn not( self ) -> SignalSet { !SignalSet::from( self ) }
    }
    /// Represents a set of `Signal` values
    #[derive( Copy, Clone, Default, Eq, PartialEq )]
    pub struct SignalSet( u8 );

    impl SignalSet
    {
        /// Returns an empty `SignalSet`.
        pub fn new() -> SignalSet { SignalSet( 0 ) }
        /// Returns a `SignalSet` containing all available signals.
        pub fn all() -> SignalSet { SignalSet( Signal::all_bits() ) }
        /// Returns whether this set contains the given `Signal`.
        pub fn contains( &self, sig: Signal ) -> bool { self.0 & sig.as_bit() != 0 }
        /// Returns whether this set contains all signals present in another set.
        pub fn contains_all( &self, other: SignalSet ) -> bool { self.0 & other.0 == other.0 }
        /// Returns whether this set contains any signals present in another set.
        pub fn intersects( &self, other: SignalSet ) -> bool { self.0 & other.0 != 0 }
        /// Returns whether this set contains any signals.
        pub fn is_empty( &self ) -> bool { self.0 == 0 }
        /// Inserts a `Signal` into this set.
        pub fn insert( &mut self, sig: Signal )
            { self.0 |= sig.as_bit(); }
        /// Removes a `Signal` from this set.
        pub fn remove( &mut self, sig: Signal )
            { self.0 &= !sig.as_bit(); }
        /// Sets whether this set contains the given `Signal`.
        pub fn set( &mut self, sig: Signal, set:bool )
        {
            if set { self.insert( sig ); }
            else { self.remove( sig ); }
        }
        /// Returns the difference of two sets.
        pub fn difference( &self, other: SignalSet ) -> SignalSet { SignalSet( self.0 & !other.0 ) }
        /// Returns the symmetric difference of two sets.
        pub fn symmetric_difference( &self, other: SignalSet ) -> SignalSet { SignalSet( self.0 ^ other.0 ) }
        /// Returns the intersection of two sets.
        pub fn intersection( &self, other: SignalSet ) -> SignalSet { SignalSet( self.0 & other.0 ) }
        /// Returns the union of two sets.
        pub fn union( &self, other: SignalSet ) -> SignalSet { SignalSet( self.0 | other.0 ) }
        /// Returns the inverse of the set.
        pub fn inverse( &self ) -> SignalSet { SignalSet( !self.0 & Signal::all_bits() ) }
    }

    impl fmt::Debug for SignalSet
    {
        fn fmt( &self, f:&mut fmt::Formatter ) -> Displayed
        {
            const SIGNALS:&[Signal] = &
            [
                Signal::Break,
                Signal::Continue,
                Signal::Interrupt,
                Signal::Resize,
                Signal::Suspend,
                Signal::Quit,
            ];

            let mut first = true;

            f.write_str( "SignalSet( " )?;

            for &sig in SIGNALS
            {
                if self.contains( sig )
                {
                    if !first { f.write_str( " | " )?; }

                    write!( f, "{:?}", sig )?;
                    first = false;
                }
            }

            f.write_str( " )" )
        }
    }

    impl From<Signal> for SignalSet
    {
        fn from( sig: Signal ) -> SignalSet
        {
            let mut set = SignalSet::new();
            set.insert( sig );
            set
        }
    }

    impl Extend<Signal> for SignalSet
    {
        fn extend<I: IntoIterator<Item=Signal>>( &mut self, iter: I )
        {
            for sig in iter
            {
                self.insert( sig );
            }
        }
    }

    impl FromIterator<Signal> for SignalSet
    {
        fn from_iter<I: IntoIterator<Item=Signal>>( iter: I ) -> SignalSet
        {
            let mut set = SignalSet::new();
            set.extend( iter );
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

    pub fn killed_map_insert( pid: i32, sig: i32 )
            { if let Ok( mut m ) = KILL_MAP.try_lock()
            { m.insert( pid, sig ); } }

    pub fn killed_map_pop( pid: i32 ) -> Option<i32>
    {
        if let Ok( mut m ) = KILL_MAP.try_lock()
            { m.remove( &pid ) }
        else { None }
    }

    pub fn insert_cont_map( pid: i32 )
            { if let Ok( mut m ) = CONT_MAP.try_lock()
            { m.insert( pid ); } }

    pub fn pop_cont_map( pid: i32 ) -> bool
    {
        match CONT_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => false,
        }
    }

    pub fn insert_stopped_map( pid: i32 )
            { if let Ok( mut m ) = STOP_MAP.try_lock()
            { m.insert( pid ); } }

    pub fn pop_stopped_map( pid: i32 ) -> bool
    {
        match STOP_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => false,
        }
    }

    pub fn insert_reap_map( p:i32, s:i32 )
            { if let Ok( mut m ) = REAP_MAP.try_lock()
            { m.insert( p, s ); } }

    pub fn pop_reap_map( pid: i32 ) -> Option<i32>
    {
        match REAP_MAP.try_lock()
        {
            Ok( mut m ) => m.remove( &pid ),
            Err( _ ) => None,
        }
    }

    pub fn block_signals()
    {
        let mut sigset = SigSet::empty();
        sigset.add( SIGCHLD );
        match sigprocmask( SigmaskHow::SIG_BLOCK, Some( &sigset ), None )
        {
            Ok( _ ) => {},
            Err( e ) =>
            {
                log!( "sigprocmask block error: {:?}", e );
            }
        }
    }

    pub fn unblock_signals()
    {
        let mut sigset = SigSet::empty();
        sigset.add( SIGCHLD );
        match sigprocmask( SigmaskHow::SIG_UNBLOCK, Some( &sigset ), None )
        {
            Ok( _ ) => {},
            Err( e ) => { log!( "sigprocmask unblock error: {:?}", e ); }
        }
    }
    
    pub extern "C" fn handle_sigchld( _sig: i32 )
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

                Ok( _others ) => { log!( "sigchld others: {:?}", _others ); }

                Err( e ) =>
                {
                    if e == nix::Error::ECHILD { break; }
                    log!( "chld waitpid error: {:?}", e );
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
            let sigset = SigSet::empty();
            let handler = SigHandler::Handler( handle_sigchld );
            let flags = SaFlags::SA_RESTART;
            let sa = SigAction::new( handler, flags, sigset );

            match signal::sigaction( signal::SIGCHLD, &sa )
            {
                Ok( _ ) => {},
                Err( e ) => { log!( "sigaction error: {:?}", e ); }
            }
        }
    }
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
    use ::
    {
        borrow::Cow::{self, Borrowed, Owned},
        fs::{ File },
        *,
    };
    /*
    use std::borrow::Cow::{self, Borrowed, Owned};
    use std::collections::{vec_deque, VecDeque};
    use std::fmt;
    use std::io;
    use std::iter::{repeat, Skip};
    use std::mem::swap;
    use std::ops::{Deref, DerefMut, Range};
    use std::sync::MutexGuard;
    use std::time::{Duration, Instant};

    use crate::chars::{is_ctrl, unctrl, ESCAPE, RUBOUT};
    use crate::reader::{START_INVISIBLE, END_INVISIBLE};
    use crate::terminal::{CursorMode, Size, Terminal, TerminalWriter};
    use crate::util::{
        backward_char, forward_char, backward_search_char, forward_search_char,
        filter_visible, is_combining_mark, is_wide, RangeArgument,
    };

    */
    /*
    pub( crate ) fn display_str<'a>( s:&'a str, style: Display ) -> Cow<'a, str> */
    pub fn display<'a>( s:&'a str, style: Display ) -> Cow<'a, str>
    {
        if s.chars().all( |ch| display( ch, style ) == DisplaySequence::Char( ch ) )
            { Borrowed( s ) }
        else { Owned( s.chars().flat_map( |ch| display( ch, style ) ).collect() ) }
    }

    /*
    pub fn write_file_str( ... ) -> io::Result<()> */
    /// Writes a string to a file.
    pub fn write_file( fname: &str, contents: &str ) -> io::Result<()>
    {
        // Open a file in write-only mode
        let mut file = File::create( fname )?;

        file.write_all( contents.as_bytes() )?;

        Ok( () )
    }
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
        fmt::{ columns },
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
    pub const GREEN:&str = "\x01\x1b[0;32m\x02";
    /// CLI completions
    pub mod completers
    {
        use ::
        {
            borrow::Cow::{ self, Borrowed, Owned },
            fs::{ read_dir },
            path::{ is_separator, MAIN_SEPARATOR, Path },
            regex::{ contains },
            sync::{ Arc },
            terminal::{ Terminals, Prompter },
            *,
        };
        /**/
        pub mod dots
        {

        }
        /**/
        pub mod env
        {

        }
        /**/
        pub mod make
        {

        }
        /**/
        pub mod path
        {

        }
        /**/
        pub mod ssh
        {

        }
        /**/
        pub mod utils
        {

        }
        /// Represents a single possible completion
        #[derive( Clone, Debug )]
        pub struct Completion
        {
            /// Whole completion text
            pub completion: String,
            /// Listing display string; `None` if matches completion
            pub display:Option<String>,
            /// Completion suffix; replaces append character
            pub suffix: Suffix,
        }
        /// Specifies an optional suffix to override the default value
        #[derive( Copy, Clone, Debug, Eq, PartialEq )]
        pub enum Suffix
        {
            /// Use the default suffix
            Default,
            /// Use no suffix
            None,
            /// Use the given suffix
            Some( char ),
        }

        impl Completion
        {
            /// Returns a simple `Completion` value, with display string matching
            /// completion and using the default completion suffix.
            pub fn simple( s: String ) -> Completion
            {
                Completion
                {
                    completion: s,
                    display: None,
                    suffix: Suffix::default(),
                }
            }
            /// Returns the full completion string, including suffix, 
            /// using the given default suffix if one is not assigned to this completion.
            pub fn completion( &self, def_suffix:Option<char> ) -> Cow<str>
            {
                let mut s = Borrowed( &self.completion[..] );

                if let Some( suffix ) = self.suffix.with_default( def_suffix )
            { s.to_mut().push( suffix ); }

                s
            }
            /// Returns the display string, including suffix
            pub fn display( &self ) -> Cow<str>
            {
                let mut s = Borrowed( self.display_str() );

                if let Suffix::Some( suffix ) = self.suffix { s.to_mut().push( suffix ); }

                s
            }
            /// Returns the number of characters displayed
            pub fn display_chars( &self ) -> usize
            {
                let n = self.display_str().chars().count();
                n + if self.suffix.is_some()
            { 1 } else { 0 }
            }

            fn display_str( &self ) -> &str
            {
                match self.display
                {
                    Some( ref dis ) => dis,
                    None => &self.completion
                }
            }
        }

        impl Suffix
        {
            /// Returns whether the `Suffix` value is the `Default` variant.
            pub fn is_default( &self ) -> bool
            {
                match *self
                {
                    Suffix::Default => true,
                    _ => false
                }
            }
            /// Returns whether the `Suffix` value is the `Some( _ )` variant.
            pub fn is_some( &self ) -> bool
            {
                match *self
                {
                    Suffix::Some( _ ) => true,
                    _ => false
                }
            }
            /// Returns whether the `Suffix` value is the `None` variant.
            pub fn is_none( &self ) -> bool
            {
                match *self
                {
                    Suffix::None => true,
                    _ => false
                }
            }
            /// Returns an `Option<char>`, using the given value in place of `Default`.
            pub fn with_default( self, default:Option<char> ) -> Option<char>
            {
                match self
                {
                    Suffix::None => None,
                    Suffix::Some( ch ) => Some( ch ),
                    Suffix::Default => default
                }
            }
        }

        impl Default for Suffix
        {
            fn default() -> Suffix { Suffix::Default }
        }
        /// Performs completion for `Prompter` when triggered by a user input sequence
        pub trait Completer<Term: Terminals>: Send + Sync
        {
            /// Returns the set of possible completions for the prefix `word`.
            fn complete( &self, word:&str, prompter:&Prompter<Term>,
                start:usize, end: usize ) -> Option<Vec<Completion>>;

            /// Returns the starting position of the word under the cursor.
            ///
            /// The default implementation uses `Prompter::word_break_chars()` to
            /// detect the start of a word.
            fn word_start( &self, line:&str, end:usize, prompter:&Prompter<Term> ) -> usize {
                word_break_start( &line[..end], prompter.word_break_chars() )
            }

            /// Quotes a possible completion for insertion into input.
            ///
            /// The default implementation returns the word, as is.
            fn quote<'a>( &self, word:&'a str ) -> Cow<'a, str> { Borrowed( word ) }

            /// Unquotes a piece of user input before searching for completions.
            ///
            /// The default implementation returns the word, as is.
            fn unquote<'a>( &self, word:&'a str ) -> Cow<'a, str> { Borrowed( word ) }
        }
        /// `Completer` type that performs no completion.
        pub struct DummyCompleter;

        impl<Term: Terminals> Completer<Term> for DummyCompleter
        {
            fn complete( &self, _word:&str, _reader:&Prompter<Term>, _start:usize, _end:usize ) ->
            Option<Vec<Completion>> { None }
        }
        /// Performs completion by searching for filenames matching the word prefix.
        pub struct PathCompleter;

        impl<Term: Terminals> Completer<Term> for PathCompleter
        {
            fn complete( &self, word:&str, _reader:&Prompter<Term>, _start:usize, _end: usize )
            -> Option<Vec<Completion>>
            { Some( complete_path( word ) ) }

            fn word_start( &self, line:&str, end:usize, _reader:&Prompter<Term> ) -> usize
            { escaped_word_start( &line[..end] ) }

            fn quote<'a>( &self, word:&'a str ) -> Cow<'a, str> { escape( word ) }

            fn unquote<'a>( &self, word:&'a str ) -> Cow<'a, str> { unescape( word ) }
        }
        /// Returns a sorted list of paths whose prefix matches the given path.
        pub fn complete_path( path:&str ) -> Vec<Completion>
        {
            let ( base_dir, fname ) = split_path( path );
            let mut res = Vec::new();
            let lookup_dir = base_dir.unwrap_or( "." );

            if let Ok( list ) = read_dir( lookup_dir )
            {
                for ent in list
                {
                    if let Ok( ent ) = ent
                    {
                        let ent_name = ent.file_name();
                        
                        if let Ok( path ) = ent_name.into_string()
                        {
                            if path.starts_with( fname )
                            {
                                let ( name, display ) = if let Some( dir ) = base_dir
                                {
                                    ( format!( "{}{}{}", dir, MAIN_SEPARATOR, path ),
                                        Some( path ) )
                                }
                                else { ( path, None ) };

                                let is_dir = ent.metadata().ok().map_or( false, |m| m.is_dir() );

                                let suffix = if is_dir { Suffix::Some( MAIN_SEPARATOR ) }                                
                                else { Suffix::Default };

                                res.push( Completion
                                {
                                    completion: name,
                                    display: display,
                                    suffix: suffix,
                                } );
                            }
                        }
                    }
                }
            }

            res.sort_by( |a, b| a.display_str().cmp( b.display_str() ) );
            res
        }
        /// Returns the start position of the word that ends at the end of the string.
        pub fn word_break_start( s:&str, word_break:&str ) -> usize
        {
            let mut start = s.len();

            for ( idx, ch ) in s.char_indices().rev()
            {
                if word_break.contains( ch )
            { break; }
                start = idx;
            }

            start
        }
        /// Returns the start position of a word with non-word characters escaped by backslash ( `\\` ).
        pub fn escaped_word_start( s:&str ) -> usize
        {
            let mut chars = s.char_indices().rev();
            let mut start = s.len();

            while let Some( ( idx, ch ) ) = chars.next()
            {
                if needs_escape( ch )
                {
                    let n = 
                    {
                        let mut n = 0;

                        loop
                        {
                            let mut clone = chars.clone();
                            let ch = match clone.next()
                            {
                                Some( ( _, ch ) ) => ch,
                                None => break
                            };

                            if ch == '\\'
                            {
                                chars = clone;
                                n += 1;
                            }
                            
                            else { break; }
                        }

                        n
                    };

                    if n % 2 == 0 { break; }
                }

                start = idx;
            }

            start
        }
        /// Escapes a word by prefixing a backslash ( `\\` ) to non-word characters.
        pub fn escape( s:&str ) -> Cow<str>
        {
            let n = s.chars().filter( |&ch| needs_escape( ch ) ).count();

            if n == 0 { Borrowed( s ) }
            else
            {
                let mut res = String::with_capacity( s.len() + n );

                for ch in s.chars()
                {
                    if needs_escape( ch )
            { res.push( '\\' ); }

                    res.push( ch );
                }

                Owned( res )
            }
        }
        /// Unescapes a word by removing the backslash ( `\\` ) from escaped characters.
        pub fn unescape( s:&str ) -> Cow<str>
        {
            if s.contains( '\\' )
            {
                let mut res = String::with_capacity( s.len() );
                let mut chars = s.chars();

                while let Some( ch ) = chars.next()
                {
                    if ch == '\\'
                    {
                        if let Some( ch ) = chars.next()
            { res.push( ch ); }
                    }

                    else { res.push( ch ); }
                }

                Owned( res )
            }

            else { Borrowed( s ) }
        }

        fn needs_escape( ch: char ) -> bool
        {
            match ch
            {
                ' ' | '\t' | '\n' | '\\' => true,
                _ => false
            }
        }

        fn split_path( path:&str ) -> ( Option<&str>, &str )
        {
            match path.rfind( is_separator )
            {
                Some( pos ) => ( Some( &path[..pos] ), &path[pos + 1..] ),
                None => ( None, path )
            }
        }
        /**/
        pub struct CicadaCompleter
        {
            pub sh: Arc<shell::Shell>,
        }

        fn for_make( line:&str ) -> bool { contains( line, r"^ *make " ) }

        fn for_env( line:&str ) -> bool { contains( line, r" *\$[_a-zA-Z0-9]*$" ) }

        fn for_ssh( line:&str ) -> bool { contains( line, r"^ *( ssh|scp ).* +[^ \./]+ *$" ) }

        fn for_cd( line:&str ) -> bool { contains( line, r"^ *cd +" ) }

        fn for_bin( line:&str ) -> bool
        {
            let ptn = r"( ^ *( sudo|which|nohup )? *[a-zA-Z0-9_\.-]+$ )|( ^.+\| *( sudo|which|nohup )? *[a-zA-Z0-9_\.-]+$ )";
            contains( line, ptn )
        }

        fn for_dots( line:&str ) -> bool
        {
            let args = parsers::line::line_to_plain_tokens( line );
            let len = args.len();

            if len == 0 { return false; }
            
            let dir = tools::get_user_completer_dir();
            let dot_file = format!( "{}/{}.yaml", dir, args[0] );
            Path::new( dot_file.as_str() ).exists()
        }

        impl<Term: Terminals> Completer<Term> for CicadaCompleter 
        {
            fn complete( &self, word:&str, reader:&Prompter<Term>, start:usize, end: usize ) -> 
            Option<Vec<Completion>>
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
                    let cpl = Arc::new( path::BinCompleter
                    {
                        sh: self.sh.clone(),
                    } );
                    completions = cpl.complete( word, reader, start, _end );
                }
                
                else if for_env( line )
                {
                    let cpl = Arc::new( env::EnvCompleter
                    {
                        sh: self.sh.clone(),
                    } );
                    
                    completions = cpl.complete( word, reader, start, _end );
                }
                
                else if for_cd( line )
                {
                    let cpl = Arc::new( path::CdCompleter );
                    return cpl.complete( word, reader, start, _end );
                }

                else { completions = None; }

                if let Some( x ) = completions
                {
                    if !x.is_empty()
            { return Some( x ); }
                }
                
                let cpl = Arc::new( path::PathCompleter );
                cpl.complete( word, reader, start, _end )
            }

            fn word_start( &self, line:&str, end:usize, _reader:&Prompter<Term> ) -> usize
            {
                escaped_word_start( &line[..end] )
            }
        }
    }
    
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
            static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new( HashSet::new() );
            static ref ALIASES: Mutex<HashSet<String>> = Mutex::new( HashSet::new() );
        }
        /// Represents a style to be applied to a text range.
        #[derive( Debug, Clone, PartialEq, Eq )]
        pub enum Style
        {
            /// A style using raw ANSI color codes
            AnsiColor( String ),
            /// The default terminal style
            Default,
        }
        /// A trait for providing style information for a line of text.
        pub trait Highlighter
        {
            /// Takes the current line buffer and returns a list of styled ranges.
            fn highlight( &self, line:&str ) -> Vec<( Range<usize>, Style )>;
        }
        /// Initialize the available commands cache by scanning PATH directories
        pub fn init_command_cache()
        {
            let commands = scan_available_commands();
            if let Ok( mut cache ) = AVAILABLE_COMMANDS.lock()
            {
                *cache = commands;
            }
        }
        /// Update aliases in the highlighter's cache
        pub fn update_aliases( sh:&shell::Shell )
        {
            if let Ok( mut aliases ) = ALIASES.lock()
            {
                aliases.clear();

                for alias_name in sh.aliases.keys()
                {
                    aliases.insert( alias_name.clone() );
                }
            }
        }

        fn scan_available_commands() -> HashSet<String>
        {
            let mut commands = HashSet::new();

            if let Ok( path_var ) = env::var( "PATH" )
            {
                for path in path_var.split( ':' )
                {
                    if path.is_empty()
            { continue; }

                    let dir_path = Path::new( path );

                    if !dir_path.is_dir()
            { continue; }

                    if let Ok( entries ) = fs::read_dir( dir_path )
                    {
                        for entry in entries.filter_map( Result::ok )
                        {
                            if let Ok( file_type ) = entry.file_type()
                            {
                                if file_type.is_file() || file_type.is_symlink()
                                {
                                    if let Ok( metadata ) = entry.metadata()
                                    {
                                        if metadata.permissions().mode() & 0o111 != 0
                                        {
                                            if let Some( name ) = entry.file_name().to_str()
                                            { commands.insert( name.to_string() ); }
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
        
        #[derive( Clone )]
        pub struct CicadaHighlighter;
        
        impl Highlighter for CicadaHighlighter
        {
            fn highlight( &self, line:&str ) -> Vec<( Range<usize>, Style )>
            {
                let mut styles = Vec::new();

                if line.is_empty()
            { return styles; }

                let line_info = parsers::line::parse( line );
                
                if line_info.tokens.is_empty()
                {
                    styles.push( ( 0..line.len(), Style::Default ) );
                    return styles;
                }

                let mut current_byte_idx = 0;
                let mut is_start_of_segment = true;

                for token in &line_info.tokens {
                    // Find the range in the original line for this token
                    match find_token_range_heuristic( line, current_byte_idx, token )
            {
                        Some( token_range ) => {
                            // Style potential whitespace before the token
                            if token_range.start > current_byte_idx {
                                styles.push( ( current_byte_idx..token_range.start, Style::Default ) );
                            }

                            let ( _sep, word ) = token;
                            let mut current_token_style = Style::Default;

                            if is_start_of_segment && !word.is_empty()
            {
                                if is_command( word )
            {
                                    current_token_style = Style::AnsiColor( GREEN.to_string() );
                                }
                                // Only the first non-empty token in a segment can be a command
                                is_start_of_segment = false;
                            }

                            styles.push( ( token_range.clone(), current_token_style ) );

                            // Check if this token marks the end of a command segment
                            if ["|", "&&", "||", ";"].contains( &word.as_str() )
            {
                                is_start_of_segment = true;
                            }

                            current_byte_idx = token_range.end;
                        }
                        None => {
                            // If we can't map a token, style the rest of the line as default and stop.
                            if current_byte_idx < line.len()
            {
                            styles.push( ( current_byte_idx..line.len(), Style::Default ) );
                            }
                            current_byte_idx = line.len(); // Mark as done
                            break; // Stop processing further tokens
                        }
                    }
                }

                // Style any remaining characters after the last processed token
                if current_byte_idx < line.len()
            {
                    styles.push( ( current_byte_idx..line.len(), Style::Default ) );
                }

                styles
            }
        }

        pub fn create_highlighter() -> Arc<CicadaHighlighter> { Arc::new( CicadaHighlighter ) }
    }
    pub use self::highlighter::{ Highlighter };
    
    pub mod main
    {
        use ::
        {
            env::{ var },
            terminal::
            {
                preset::{ apply_preset_item, apply_pyenv },
            },
            *,
        };
        
        pub const DEFAULT_PROMPT:&str = "${COLOR_STATUS}$USER${RESET}@${COLOR_STATUS}$HOSTNAME${RESET}: ${COLOR_STATUS}$CWD${RESET}$ ";
        
        pub fn get_prompt_string() -> String
        {
            if let Ok( x ) = var( "PROMPT" )
            { return x; }
            DEFAULT_PROMPT.to_string()
        }

        fn apply_prompt_item( sh:&shell::Shell, result:&mut String, token:&str )
        {
            if let Some( x ) = sh.get_env( token )
            {
                result.push_str( &x );
                return;
            }

            apply_preset_item( sh, result, token );
        }

        fn apply_command( result:&mut String, token:&str, prefix:&str, suffix:&str )
        {
            let cr = now::run( token );
            let output = cr.stdout.trim();
            
            if !output.is_empty()
            {
                result.push_str( prefix );
                result.push_str( output );
                result.push_str( suffix );
            }
        }

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
                        apply_command( &mut prompt, &token, &prefix, &suffix );
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
                        apply_prompt_item( sh, &mut prompt, &token );
                        token.clear();
                        met_dollar = false;
                        met_brace = false;
                        continue;
                    } 
                    else if c == '$'
                    {
                        if token.is_empty()
                        {
                            prompt.push( '$' );
                            met_dollar = true;
                            continue;
                        }
                        else
                        {
                            apply_prompt_item( sh, &mut prompt, &token );
                            token.clear();
                            continue;
                        }
                    }
                    
                    else if met_paren
                    {
                        if is_prefix_char( c ) { prefix.push( c ); }

                        else if is_suffix_char( c ) { suffix.push( c ); }

                        else { token.push( c ); }
                        continue;
                    }

                    else if is_prompt_item_char( c, &token )
                    {
                        token.push( c );
                        continue;
                    }

                    else if token.is_empty()
                    {
                        prompt.push( '$' );
                        prompt.push( c );
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
                    apply_prompt_item( sh, &mut prompt, &token );
                    token.clear();
                }
                prompt.push( c );
                met_dollar = false;
            }

            if !token.is_empty()
            {
                apply_prompt_item( sh, &mut prompt, &token );
                met_dollar = false;
            }

            if met_dollar { prompt.push( '$' ); }

            if prompt.trim().is_empty() { return format!( "cicada-{} >> ", "25.4.15" ); }

            prompt
        }
    }
    
    pub mod preset
    {
        use ::
        {
            terminal::{ color },
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
        pub fn apply_pyenv( prompt:&mut String )
        {
            if let Ok( x ) = env::var( "VIRTUAL_ENV" )
            {
                if !x.is_empty()
                {
                    let _tokens: Vec<&str> = x.split( '/' ).collect();
                    let env_name = match _tokens.last()
                    {
                        Some( x ) => x,
                        None => {
                            log!( "prompt token last error" );
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
            match token.to_ascii_lowercase().as_ref()
            {
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
        fn apply_seq( prompt:&mut String ) { prompt.push_str( color::SEQ ); }
        fn apply_end_seq( prompt:&mut String ) { prompt.push_str( color::END_SEQ ); }
        fn apply_esc( prompt:&mut String ) { prompt.push_str( color::ESC ); }
        fn apply_underlined( prompt:&mut String ) { prompt.push_str( color::UNDERLINED ); }
        fn apply_user( prompt:&mut String )
        {
            let username = get::username();
            prompt.push_str( &username );
        }
        fn apply_black( prompt:&mut String ) { prompt.push_str( color::BLACK ); }
        fn apply_black_b( prompt:&mut String ) { prompt.push_str( color::BLACK_B ); }
        fn apply_black_bg( prompt:&mut String ) { prompt.push_str( color::BLACK_BG ); }
        fn apply_blue( prompt:&mut String ) { prompt.push_str( color::BLUE ); }
        fn apply_blue_b( prompt:&mut String ) { prompt.push_str( color::BLUE_B ); }
        fn apply_blue_bg( prompt:&mut String ) { prompt.push_str( color::BLUE_BG ); }
        fn apply_bold( prompt:&mut String ) { prompt.push_str( color::BOLD ); }
        fn apply_green( prompt:&mut String ) { prompt.push_str( color::GREEN ); }
        fn apply_green_b( prompt:&mut String ) { prompt.push_str( color::GREEN_B ); }
        fn apply_green_bg( prompt:&mut String ) { prompt.push_str( color::GREEN_BG ); }
        fn apply_red( prompt:&mut String ) { prompt.push_str( color::RED ); }
        fn apply_red_b( prompt:&mut String ) { prompt.push_str( color::RED_B ); }
        fn apply_red_bg( prompt:&mut String ) { prompt.push_str( color::RED_BG ); }
        fn apply_white( prompt:&mut String ) { prompt.push_str( color::WHITE ); }
        fn apply_white_b( prompt:&mut String ) { prompt.push_str( color::WHITE_B ); }
        fn apply_white_bg( prompt:&mut String ) { prompt.push_str( color::WHITE_BG ); }
        fn apply_hidden( prompt:&mut String ) { prompt.push_str( color::HIDDEN ); }
        fn apply_reset( prompt:&mut String ) { prompt.push_str( color::RESET ); }
        fn apply_reverse( prompt:&mut String ) { prompt.push_str( color::REVERSE ); }
        fn apply_dim( prompt:&mut String ) { prompt.push_str( color::DIM ); }
        fn apply_blink( prompt:&mut String ) { prompt.push_str( color::BLINK ); }
        fn apply_reset_underlined( prompt:&mut String ) { prompt.push_str( color::RESET_UNDERLINED ); }
        fn apply_reset_dim( prompt:&mut String ) { prompt.push_str( color::RESET_DIM ); }
        fn apply_reset_reverse( prompt:&mut String ) { prompt.push_str( color::RESET_REVERSE ); }
        fn apply_reset_hidden( prompt:&mut String ) { prompt.push_str( color::RESET_HIDDEN ); }
        fn apply_reset_blink( prompt:&mut String ) { prompt.push_str( color::RESET_BLINK ); }
        fn apply_reset_bold( prompt:&mut String ) { prompt.push_str( color::RESET_BOLD ); }
        fn apply_default( prompt:&mut String ) { prompt.push_str( color::DEFAULT ); }
        fn apply_default_bg( prompt:&mut String ) { prompt.push_str( color::DEFAULT_BG ); }
        fn apply_cyan( prompt:&mut String ) { prompt.push_str( color::CYAN ); }
        fn apply_cyan_l( prompt:&mut String ) { prompt.push_str( color::CYAN_L ); }
        fn apply_cyan_bg( prompt:&mut String ) { prompt.push_str( color::CYAN_BG ); }
        fn apply_cyan_l_bg( prompt:&mut String ) { prompt.push_str( color::CYAN_L_BG ); }
        fn apply_red_l( prompt:&mut String ) { prompt.push_str( color::RED_L ); }
        fn apply_red_l_bg( prompt:&mut String ) { prompt.push_str( color::RED_L_BG ); }
        fn apply_green_l( prompt:&mut String ) { prompt.push_str( color::GREEN_L ); }
        fn apply_green_l_bg( prompt:&mut String ) { prompt.push_str( color::GREEN_L_BG ); }
        fn apply_gray_l( prompt:&mut String ) { prompt.push_str( color::GRAY_L ); }
        fn apply_gray_l_bg( prompt:&mut String ) { prompt.push_str( color::GRAY_L_BG ); }
        fn apply_gray_d( prompt:&mut String ) { prompt.push_str( color::GRAY_D ); }
        fn apply_gray_d_bg( prompt:&mut String ) { prompt.push_str( color::GRAY_D_BG ); }
        fn apply_magenta( prompt:&mut String ) { prompt.push_str( color::MAGENTA ); }
        fn apply_magenta_bg( prompt:&mut String ) { prompt.push_str( color::MAGENTA_BG ); }
        fn apply_magenta_l( prompt:&mut String ) { prompt.push_str( color::MAGENTA_L ); }
        fn apply_magenta_l_bg( prompt:&mut String ) { prompt.push_str( color::MAGENTA_L_BG ); }
        fn apply_yellow( prompt:&mut String ) { prompt.push_str( color::YELLOW ); }
        fn apply_yellow_bg( prompt:&mut String ) { prompt.push_str( color::YELLOW_BG ); }
        fn apply_yellow_l( prompt:&mut String ) { prompt.push_str( color::YELLOW_L ); }
        fn apply_yellow_l_bg( prompt:&mut String ) { prompt.push_str( color::YELLOW_L_BG ); }
        fn apply_blue_l( prompt:&mut String ) { prompt.push_str( color::BLUE_L ); }
        fn apply_blue_l_bg( prompt:&mut String ) { prompt.push_str( color::BLUE_L_BG ); }        
        fn _find_git_root() -> String
        {
            let current_dir = libs::path::current_dir();
            let dir_git = format!( "{}/.git", current_dir );
            if Path::new( &dir_git ).exists()
            {
                return current_dir;
            }

            let mut _dir = current_dir.clone();
            while Path::new( &_dir ).parent().is_some()
            {
                match Path::new( &_dir ).parent()
            {
                    Some( p ) => {
                        _dir = p.to_string_lossy().to_string();
                        let dir_git = format!( "{}/.git", _dir );
                        if Path::new( &dir_git ).exists()
            {
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
        fn apply_color_status( sh:&shell::Shell, prompt:&mut String )
        {
            if sh.previous_status == 0 { prompt.push_str( color::GREEN_B ); }
            else { prompt.push_str( color::RED_B ); }
        }
        fn apply_gitbr( prompt:&mut String )
        {
            let git_root = _find_git_root();
            if git_root.is_empty()
            {
                return;
            }

            let file_head = format!( "{}/.git/HEAD", git_root );
            if !Path::new( &file_head ).exists()
            {
                return;
            }

            let mut file;
            match File::open( &file_head )
            {
                Ok( x ) => file = x,
                Err( e ) =>
                {
                    println!( "cicada: .git/HEAD err: {:?}", e );
                    return;
                }
            }

            let mut text = String::new();
            match file.read_to_string( &mut text )
            {
                Ok( _ ) => {}
                Err( e ) =>
                {
                    println!( "cicada: read_to_string error: {:?}", e );
                    return;
                }
            }

            if let Some( branch ) = regex::find_first_group( r"^[a-z]+: ?[a-z]+/[a-z]+/( .+ )$", text.trim() )
            {
                apply_blue_b( prompt );
                if let Ok( x ) = env::var( "CICADA_GITBR_PREFIX" ) { prompt.push_str( &x ); }

                let _len_default: i32 = 32;
                let mut len_max = if let Ok( x ) = env::var( "CICADA_GITBR_MAX_LEN" )
                {
                    match x.parse::<i32>()
                    {
                        Ok( n ) => n,
                        Err( _ ) => _len_default,
                    }
                } 
                else { _len_default };

                if len_max <= 0 { len_max = _len_default; }

                if branch.len() as i32 <= len_max { prompt.push_str( &branch ); }
                else
                {
                    let len = branch.len() as i32;
                    let offset = ( len - len_max + 2 ) as usize;
                    let branch_short = format!( "..{}", &branch[offset..] );
                    prompt.push_str( &branch_short );
                }

                if let Ok( x ) = env::var( "CICADA_GITBR_SUFFIX" ) { prompt.push_str( &x ); }

                apply_reset( prompt );
            }
        }
        fn apply_cwd( prompt:&mut String )
        {
            let _current_dir = match env::current_dir()
            {
                Ok( x ) => x,
                Err( e ) => {
                    println_stderr!( "cicada: PROMPT: env current_dir error: {}", e );
                    return;
                }
            };
            let current_dir = match _current_dir.to_str()
            {
                Some( x ) => x,
                None => {
                    println_stderr!( "cicada: PROMPT: to_str error" );
                    return;
                }
            };
            let _tokens: Vec<&str> = current_dir.split( '/' ).collect();

            let last = match _tokens.last()
            {
                Some( x ) => x,
                None => {
                    log!( "cicada: PROMPT: token last error" );
                    return;
                }
            };

            let home = tools::get_user_home();
            let pwd = if last.is_empty()
            {
                "/"
            } else if current_dir == home {
                "~"
            } else {
                last
            };
            prompt.push_str( pwd );
        }
        fn apply_hostname( prompt:&mut String )
        {
            let hostname = tools::get_hostname();
            prompt.push_str( &hostname );
        }
        fn apply_newline( prompt:&mut String ) { prompt.push( '\n' ); }
    }

    pub mod prompt
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

        use self::main::get_prompt_string;
        use self::main::render_prompt;
        pub use self::multilines::EnterFunction;
        */
        /// Provides access to the current state of input while a `read_line` call is in progress.
        pub struct Prompter<'a, 'b: 'a, Term: 'b + Terminals>
        {
            pub read:&'a mut ReadLock<'b, Term>,
            write: WriteLock<'b, Term>,
        }

        impl<'a, 'b: 'a, Term: 'b + Terminals> Prompter<'a, 'b, Term> 
        {
            pub fn new( read:&'a mut ReadLock<'b, Term>, write: WriteLock<'b, Term> ) -> Prompter<'a, 'b, Term>
            {
                Prompter{read, write}
            }

            /// Returns a `Writer` instance using the currently held write lock.
            pub fn writer_append<'c>( &'c mut self ) -> io::Result<Writer<'c, 'b, Term>> {
                Writer::with_ref( &mut self.write, false )
            }
            /// Returns a `Writer` instance using the currently held write lock.
            pub fn writer_erase<'c>( &'c mut self ) -> io::Result<Writer<'c, 'b, Term>> {
                Writer::with_ref( &mut self.write, true )
            }
            /// Resets input state at the start of `read_line`
            fn reset_input( &mut self )
            {
                self.read.reset_data();
                self.write.reset_data();
            }

            pub fn start_read_line( &mut self ) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.is_prompt_drawn = true;
                self.write.update_size()?;
                self.write.draw_prompt()
            }

            pub fn end_read_line( &mut self ) -> io::Result<()> {
                self.write.expire_blink()?;

                if self.read.overwrite_mode {
                    self.write.set_cursor_mode( CursorMode::Normal )?;
                }
                if self.write.is_prompt_drawn {
                    self.write.move_to_end()?;
                    self.write.write_str( "\n" )?;
                    self.write.is_prompt_drawn = false;
                }

                self.reset_input();
                self.read.state = InputState::Inactive;

                Ok( () )
            }

            pub fn handle_input( &mut self, ch: char ) -> io::Result<Option<ReadResult>> {
                self.write.expire_blink()?;

                match self.read.state {
                    InputState::Inactive => panic!( "input received in inactive state" ),
                    InputState::NewSequence => {
                        if ch == EOF && self.write.buffer.is_empty()
            {
                            self.write.write_str( "\n" )?;
                            self.write.is_prompt_drawn = false;
                            return Ok( Some( ReadResult::Eof ) );
                        } else {
                            self.read.sequence.push( ch );
                            self.execute_sequence()?;

                            if self.read.input_accepted {
                                let s = replace( &mut self.write.buffer, String::new() );
                                return Ok( Some( ReadResult::Input( s ) ) );
                            }
                        }
                    }
                    InputState::ContinueSequence{expiry: _} => {
                        self.read.sequence.push( ch );

                        self.execute_sequence()?;

                        if self.read.input_accepted {
                            let s = replace( &mut self.write.buffer, String::new() );
                            return Ok( Some( ReadResult::Input( s ) ) );
                        }
                    }
                    InputState::Number => {
                        if let Some( digit ) = ch.to_digit( 10 )
            {
                            self.write.input_arg.input( digit as i32 );

                            if self.write.input_arg.is_out_of_bounds()
            {
                                self.read.state = InputState::NewSequence;
                                self.write.input_arg = Digit::None;
                                self.write.explicit_arg = false;
                                self.write.redraw_prompt( PromptType::Normal )?;
                            } else {
                                self.write.redraw_prompt( PromptType::Number )?;
                            }
                        } else {
                            self.read.state = InputState::NewSequence;
                            self.write.redraw_prompt( PromptType::Normal )?;
                            self.read.macro_buffer.insert( 0, ch );
                        }
                    }
                    InputState::CharSearch{n, backward} => {
                        if n != 0 {
                            if backward {
                                self.write.backward_search_char( n, ch )?;
                            } else {
                                self.write.forward_search_char( n, ch )?;
                            }
                        }
                        self.read.state = InputState::NewSequence;
                    }
                    InputState::TextSearch => {
                        if ch == DELETE {
                            {
                                let write = &mut *self.write;
                                write.search_buffer.pop();
                                write.last_search.clone_from( &write.search_buffer );
                            }
                            self.write.search_history_update()?;
                        } else if self.is_abort( ch )
            {
                            self.abort_search_history()?;
                        } else if is_ctrl( ch )
            {
                            self.end_search_history()?;
                            self.read.macro_buffer.insert( 0, ch );
                        } else {
                            {
                                let write = &mut *self.write;
                                write.search_buffer.push( ch );
                                write.last_search.clone_from( &write.search_buffer );
                            }
                            self.write.search_history_update()?;
                        }
                    }
                    InputState::CompleteIntro => {
                        match ch {
                            'y' | 'Y' | ' ' => {
                                self.write.write_str( "\n" )?;
                                self.show_completions_page( 0 )?;
                            }
                            '\r' | '\n' => {
                                self.write.write_str( "\n" )?;
                                self.show_completions_line( 0 )?;
                            }
                            'q' | 'Q' |
                            'n' | 'N' | DELETE => {
                                self.write.write_str( "\n" )?;
                                self.end_page_completions()?;
                            }
                            _ => ()
                        }
                    }
                    InputState::CompleteMore( offset ) => {
                        match ch {
                            'y' | 'Y' | ' ' => {
                                self.write.clear_prompt()?;
                                self.show_completions_page( offset )?;
                            }
                            '\r' | '\n' => {
                                self.write.clear_prompt()?;
                                self.show_completions_line( offset )?;
                            }
                            'q' | 'Q' |
                            'n' | 'N' | DELETE => {
                                self.write.clear_prompt()?;
                                self.end_page_completions()?;
                            }
                            _ => ()
                        }
                    }
                    InputState::QuotedInsert( n ) => {
                        if n != 0 {
                            self.insert( n, ch )?;
                        }
                        self.read.state = InputState::NewSequence;
                    }
                }

                Ok( None )
            }
            /// Returns the current buffer.
            pub fn buffer( &self ) -> &str {
                &self.write.buffer
            }
            /// Returns the "backup" buffer.
            ///
            /// When the user is currently editing a history entry, the backup buffer
            /// contains the original user input.
            pub fn backup_buffer( &self ) -> &str {
                &self.write.backup_buffer
            }
            /// Returns the command `Category` of the most recently executed command.
            ///
            /// Some commands may use this to influence behavior of repeated commands.
            pub fn last_command_category( &self ) -> Category {
                self.read.last_cmd
            }
            /// Returns the set of characters that indicate a word break.
            pub fn word_break_chars( &self ) -> &str {
                &self.read.word_break
            }
            /// Sets the buffer to the given value.
            ///
            /// The cursor is moved to the end of the buffer.
            pub fn set_buffer( &mut self, buf:&str ) -> io::Result<()> {
                self.write.set_buffer( buf )
            }
            /// Returns the current position of the cursor.
            pub fn cursor( &self ) -> usize {
                self.write.cursor
            }
            /// Sets the cursor to the given position within the buffer.
            pub fn set_cursor( &mut self, pos: usize ) -> io::Result<()> {
                self.write.set_cursor( pos )
            }
            /// Sets the prompt that will be displayed when `read_line` is called.
            pub fn set_prompt( &mut self, prompt:&str ) -> io::Result<()> {
                self.write.set_prompt( prompt )
            }
            /// Returns the size of the terminal at the last draw operation.
            pub fn screen_size( &self ) -> Size {
                self.write.screen_size
            }
            /// Returns whether a numerical argument was explicitly supplied by the user.
            pub fn explicit_arg( &self ) -> bool {
                self.write.explicit_arg
            }
            /// Returns the current input sequence.
            pub fn sequence( &self ) -> &str {
                &self.read.sequence
            }
            /// Returns an iterator over bound sequences
            pub fn bindings( &self ) -> BindingIter {
                self.read.bindings()
            }
            /// Returns an iterator over variable values.
            pub fn variables( &self ) -> VariableIter {
                self.read.variables()
            }
            /// Returns an iterator over history entries
            pub fn history( &self ) -> HistoryIter {
                self.write.history()
            }
            /// Returns the index into history currently being edited.
            pub fn history_index( &self ) -> Option<usize> {
                self.write.history_index
            }
            /// Returns the current number of history entries.
            pub fn history_len( &self ) -> usize {
                self.write.history.len()
            }

            fn next_history( &mut self, n: usize ) -> io::Result<()> {
                self.write.next_history( n )
            }

            fn prev_history( &mut self, n: usize ) -> io::Result<()> {
                self.write.prev_history( n )
            }
            /// Selects the history entry currently being edited by the user.
            pub fn select_history_entry( &mut self, new:Option<usize> ) -> io::Result<()> {
                self.write.select_history_entry( new )
            }
            /// Returns the current set of completions.
            pub fn completions( &self ) -> Option<&[Completion]> {
                self.read.completions.as_ref().map( |v| &v[..] )
            }
            /// Sets the current set of completions.
            pub fn set_completions( &mut self, completions:Option<Vec<Completion>> )
            {
                self.read.completions = completions;
            }
            /// Attempts to execute the current sequence.
            fn execute_sequence( &mut self ) -> io::Result<()> {
                match self.find_binding( &self.read.sequence )
            {
                    FindResult::Found( cmd ) => {
                        let ch = self.read.sequence.chars().last().unwrap();
                        let n = self.write.input_arg.to_i32();

                        self.read.state = InputState::NewSequence;
                        self.execute_command( cmd, n, ch )?;
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
                    FindResult::Undecided( _ ) => {
                        let expiry = self.keyseq_expiry();
                        self.read.state = InputState::ContinueSequence{expiry};
                    }
                }

                Ok( () )
            }

            fn force_execute_sequence( &mut self ) -> io::Result<()> {
                self.read.state = InputState::NewSequence;

                match self.find_binding( &self.read.sequence )
            {
                    FindResult::Found( cmd ) |
                    FindResult::Undecided( cmd ) => {
                        let ch = self.read.sequence.chars().last().unwrap();
                        let n = self.write.input_arg.to_i32();

                        self.execute_command( cmd, n, ch )?;
                        self.read.sequence.clear();
                    }
                    FindResult::NotFound => {
                        self.insert_first_char()?;
                    }
                    FindResult::Incomplete => unreachable!(),
                }

                Ok( () )
            }
            /// Execute the command `SelfInsert` on the first character in the input sequence, if it is printable.
            fn insert_first_char( &mut self ) -> io::Result<()> {
                let ( first, rest ) = {
                    let mut chars = self.read.sequence.chars();

                    ( chars.next().unwrap(), chars.as_str().to_owned() )
                };

                self.read.sequence.clear();

                if is_printable( first )
            {
                    let n = self.write.input_arg.to_i32();
                    self.execute_command( Command::SelfInsert, n, first )?;
                }

                if !rest.is_empty()
            {
                    self.read.queue_input( &rest );
                }

                Ok( () )
            }

            fn find_binding( &self, seq:&str ) -> FindResult<Command> {
                self.read.bindings.find( seq ).cloned()
            }

            fn get_function( &self, name:&str ) -> Option<&Arc<dyn Function<Term>>> {
                self.read.functions.get( name )
            }

            fn is_abort( &self, ch: char ) -> bool {
                let mut buf = [0; 4];
                let s = ch.encode_utf8( &mut buf );

                self.find_binding( &s ) == FindResult::Found( Command::Abort )
            }

            fn execute_command( &mut self, cmd: Command, n: i32, ch: char ) -> io::Result<()> {
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
                        } else if is_printable( ch )
            {
                            self.execute_command( SelfInsert, n, ch )?;
                        }
                    }
                    InsertCompletions => {
                        if self.read.completions.is_none()
            {
                            self.build_completions();
                        }

                        if let Some( completions ) = self.read.completions.take()
            {
                            self.insert_completions( &completions )?;
                            self.read.completions = Some( completions );
                        }
                    }
                    PossibleCompletions => {
                        if self.read.completions.is_none()
            {
                            self.build_completions();
                        }

                        if let Some( completions ) = self.read.completions.take()
            {
                            self.show_completions( &completions )?;
                            self.read.completions = Some( completions );
                        }
                    }
                    MenuComplete => {
                        if self.read.completions.is_none()
            {
                            self.build_completions();
                        }

                        if n > 0 {
                            self.next_completion( n as usize )?;
                        } else {
                            self.prev_completion( ( -n ) as usize )?;
                        }
                    }
                    MenuCompleteBackward => {
                        if self.read.completions.is_none()
            {
                            self.build_completions();
                        }

                        if n > 0 {
                            self.prev_completion( n as usize )?;
                        } else {
                            self.next_completion( ( -n ) as usize )?;
                        }
                    }
                    DigitArgument => {
                        self.read.state = InputState::Number;
                        self.write.set_digit_from_char( ch );
                        self.write.redraw_prompt( PromptType::Number )?;
                    }
                    SelfInsert => {
                        if n > 0 {
                            let n = n as usize;

                            if self.read.overwrite_mode {
                                self.overwrite( n, ch )?;
                            } else {
                                self.insert( n, ch )?;
                            }

                            if self.read.blink_matching_paren {
                                if let Some( open ) = get_open_paren( ch )
            {
                                    if let Some( pos ) = find_matching_paren( 
                                            &self.write.buffer[..self.write.cursor],
                                            &self.read.string_chars, open, ch )
            {
                                        self.blink( pos )?;
                                    }
                                }
                            }
                        }
                    }
                    TabInsert => {
                        if n > 0 {
                            self.insert( n as usize, '\t' )?;
                        }
                    }
                    InsertComment => {
                        if self.explicit_arg() &&
                                self.write.buffer.starts_with( &self.read.comment_begin[..] )
            {
                            self.write.move_to( 0 )?;
                            let n = self.read.comment_begin.len();

                            self.delete_range( ..n )?;
                            self.accept_input()?;
                        } else {
                            self.write.move_to( 0 )?;
                            let s = self.read.comment_begin.clone();
                            self.insert_str( &s )?;
                            self.accept_input()?;
                        }
                    }
                    BackwardChar => {
                        if n > 0 {
                            self.write.backward_char( n as usize )?;
                        } else if n < 0 {
                            self.write.forward_char( ( -n ) as usize )?;
                        }
                    }
                    ForwardChar => {
                        if n > 0 {
                            self.write.forward_char( n as usize )?;
                        } else if n < 0 {
                            self.write.backward_char( ( -n ) as usize )?;
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
                                n: ( -n ) as usize,
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
                                n: ( -n ) as usize,
                                backward: false,
                            };
                        }
                    }
                    BackwardWord => {
                        if n > 0 {
                            self.backward_word( n as usize )?;
                        } else if n < 0 {
                            self.forward_word( ( -n ) as usize )?;
                        }
                    }
                    ForwardWord => {
                        if n > 0 {
                            let pos = forward_word( n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            self.write.move_to( pos )?;
                        } else if n < 0 {
                            let pos = forward_word( ( -n ) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            self.write.move_to( pos )?;
                        }
                    }
                    BackwardKillLine => {
                        let r = ..self.write.cursor;
                        self.kill_range( r )?;
                    }
                    KillLine => {
                        let r = self.write.cursor..;
                        self.kill_range( r )?;
                    }
                    BackwardKillWord => {
                        if n > 0 {
                            let pos = backward_word( n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            let r = pos..self.write.cursor;
                            self.kill_range( r )?;
                        } else if n < 0 {
                            let pos = forward_word( ( -n ) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            let r = self.write.cursor..pos;
                            self.kill_range( r )?;
                        }
                    }
                    KillWord => {
                        if n > 0 {
                            let pos = forward_word( n as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            let r = self.write.cursor..pos;
                            self.kill_range( r )?;
                        } else if n < 0 {
                            let pos = backward_word( ( -n ) as usize,
                                &self.write.buffer, self.write.cursor, &self.read.word_break );
                            let r = pos..self.write.cursor;
                            self.kill_range( r )?;
                        }
                    }
                    UnixWordRubout => {
                        if n > 0 {
                            let pos = backward_word( n as usize,
                                &self.write.buffer, self.write.cursor, " \t\n" );
                            let r = pos..self.write.cursor;
                            self.kill_range( r )?;
                        } else if n < 0 {
                            let pos = forward_word( ( -n ) as usize,
                                &self.write.buffer, self.write.cursor, " \t\n" );
                            let r = self.write.cursor..pos;
                            self.kill_range( r )?;
                        }
                    }
                    ClearScreen => {
                        self.write.clear_screen()?;
                    }
                    BeginningOfLine => self.write.move_to( 0 )?,
                    EndOfLine => self.write.move_to_end()?,
                    BackwardDeleteChar => {
                        if n > 0 {
                            if self.read.overwrite_mode {
                                self.overwrite_back( n as usize )?;
                            } else {
                                let pos = backward_char( n as usize,
                                    &self.write.buffer, self.write.cursor );
                                let r = pos..self.write.cursor;
                                self.delete_range( r )?;
                            }
                        } else if n < 0 {
                            let pos = char::forward( ( -n ) as usize,
                                &self.write.buffer, self.write.cursor );
                            let r = self.write.cursor..pos;
                            self.delete_range( r )?;
                        }
                    }
                    DeleteChar => {
                        if n > 0 {
                            let pos = char::forward( n as usize,
                                &self.write.buffer, self.write.cursor );
                            let r = self.write.cursor..pos;
                            self.delete_range( r )?;
                        } else if n < 0 {
                            let pos = backward_char( n as usize,
                                &self.write.buffer, self.write.cursor );
                            let r = pos..self.write.cursor;
                            self.delete_range( r )?;
                        }
                    }
                    TransposeChars => {
                        if n != 0 && self.write.cursor != 0 {
                            let ( src, dest );

                            if !self.explicit_arg() && self.write.cursor == self.write.buffer.len()
            {
                                let end = char::backward( 1, &self.write.buffer, self.write.cursor );
                                let start = char::backward( 1, &self.write.buffer, end );

                                src = start..end;
                                dest = end..self.write.cursor;
                            } else {
                                let start = char::backward( 1, &self.write.buffer, self.write.cursor );
                                let end = self.write.cursor;

                                src = start..end;

                                dest = if n < 0 {
                                    let back = char::backward( ( -n ) as usize, &self.write.buffer, start );
                                    back..start
                                } else {
                                    let fwd = char::forward( n as usize + 1, &self.write.buffer, start );
                                    end..fwd
                                };
                            }

                            self.transpose_range( src, dest )?;
                        }
                    }
                    TransposeWords => {
                        if n != 0 {
                            if let Some( first ) = first_word( &self.write.buffer[..self.write.cursor], &self.read.word_break )
            {
                                let start = word_start( &self.write.buffer, self.write.cursor, &self.read.word_break );

                                if first != start {
                                    let ( src, dest );

                                    if !self.explicit_arg() && start == self.write.buffer.len()
            {
                                        let dest_start = backward_word( 1, &self.write.buffer, start, &self.read.word_break );
                                        let dest_end = word_end( &self.write.buffer, dest_start, &self.read.word_break );

                                        let src_start = backward_word( 1, &self.write.buffer, dest_start, &self.read.word_break );
                                        let src_end = word_end( &self.write.buffer, src_start, &self.read.word_break );

                                        src = src_start..src_end;
                                        dest = dest_start..dest_end;
                                    } else {
                                        let src_start = backward_word( 1, &self.write.buffer, start, &self.read.word_break );
                                        let src_end = word_end( &self.write.buffer, src_start, &self.read.word_break );

                                        src = src_start..src_end;

                                        dest = if n < 0 {
                                            back_n_words( ( -n ) as usize, &self.write.buffer, src_start, &self.read.word_break )
                                        } else {
                                            forward_n_words( n as usize, &self.write.buffer, src_start, &self.read.word_break )
                                        };
                                    }

                                    self.transpose_range( src, dest )?;
                                }
                            }
                        }
                    }
                    BeginningOfHistory => {
                        self.select_history_entry( Some( 0 ) )?;
                    }
                    EndOfHistory => {
                        self.select_history_entry( None )?;
                    }
                    NextHistory => {
                        if n > 0 {
                            self.next_history( n as usize )?;
                        } else if n < 0 {
                            self.prev_history( ( -n ) as usize )?;
                        }
                    }
                    PreviousHistory => {
                        if n > 0 {
                            self.prev_history( n as usize )?;
                        } else if n < 0 {
                            self.next_history( ( -n ) as usize )?;
                        }
                    }
                    ForwardSearchHistory => {
                        self.read.state = InputState::TextSearch;
                        if self.read.last_cmd == Category::IncrementalSearch {
                            self.write.continue_search_history( false )?;
                        } else {
                            self.write.start_search_history( false )?;
                        }
                    }
                    ReverseSearchHistory => {
                        self.read.state = InputState::TextSearch;
                        if self.read.last_cmd == Category::IncrementalSearch {
                            self.write.continue_search_history( true )?;
                        } else {
                            self.write.start_search_history( true )?;
                        }
                    }
                    HistorySearchForward => {
                        if self.read.last_cmd == Category::Search {
                            self.write.continue_history_search( false )?;
                        } else {
                            self.write.start_history_search( false )?;
                        }
                    }
                    HistorySearchBackward => {
                        if self.read.last_cmd == Category::Search {
                            self.write.continue_history_search( true )?;
                        } else {
                            self.write.start_history_search( true )?;
                        }
                    }
                    QuotedInsert => {
                        self.read.state = InputState::QuotedInsert( 
                            if n >= 0 { n as usize } else { 0 } );
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

                        self.write.set_cursor_mode( mode )?;
                    }
                    Yank => {
                        self.yank()?;
                    }
                    YankPop => {
                        self.yank_pop()?;
                    }
                    Custom( ref name ) => {
                        if let Some( fun ) = self.get_function( name ).cloned()
            {
                            fun.execute( self, n, ch )?;

                            category = fun.category();
                        }
                    }
                    Macro( ref seq ) => {
                        self.read.queue_input( seq );
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

                Ok( () )
            }
            /// Accepts the current input buffer as user input.
            pub fn accept_input( &mut self ) -> io::Result<()> 
            {
                self.write.move_to_end()?;
                self.write.write_str( "\n" )?;
                self.read.input_accepted = true;
                self.write.is_prompt_drawn = false;
                Ok( () )
            }
            /// Moves the cursor to the given position, waits for 500 milliseconds ( or until next user input ), 
            /// then restores the original cursor position.
            pub fn blink( &mut self, pos: usize ) -> io::Result<()> {
                self.write.blink( pos )?;

                self.read.max_wait_duration = Some( BLINK_DURATION );

                Ok( () )
            }

            fn check_expire_blink( &mut self, now: Instant ) -> io::Result<()> {
                if self.write.check_expire_blink( now )? {
                    self.read.max_wait_duration = None;
                }

                Ok( () )
            }

            fn check_expire_sequence( &mut self, now: Instant ) -> io::Result<()> {
                if let InputState::ContinueSequence{expiry: Some( expiry )} = self.read.state {
                    if now >= expiry {
                        self.read.max_wait_duration = None;
                        self.force_execute_sequence()?;
                    }
                }

                Ok( () )
            }

            fn keyseq_expiry( &mut self ) -> Option<Instant> {
                if let Some( t ) = self.read.keyseq_timeout {
                    self.read.max_wait_duration = Some( t );
                    Some( Instant::now() + t )
                } else {
                    None
                }
            }

            pub fn check_expire_timeout( &mut self ) -> io::Result<()> {
                let now = Instant::now();

                self.check_expire_blink( now )?;
                self.check_expire_sequence( now )
            }

            fn expire_blink( &mut self ) -> io::Result<()> {
                self.read.max_wait_duration = None;
                self.write.expire_blink()
            }

            fn build_completions( &mut self )
            {
                let compl = self.read.completer.clone();
                let end = self.write.cursor;
                let start = compl.word_start( &self.write.buffer, end, self );

                if start > end {
                    panic!( "Completer::word_start returned invalid index; \
                        start > end ( {} > {} )", start, end );
                }

                let unquoted = compl.unquote( &self.write.buffer[start..end] ).into_owned();

                let completions = compl.complete( &unquoted, self, start, end );
                let n_completions = completions.as_ref().map_or( 0, |c| c.len() );

                self.read.completions = completions;
                self.read.completion_index = n_completions;
                self.read.completion_start = start;
                self.read.completion_prefix = end;
            }

            fn complete_word( &mut self ) -> io::Result<()> {
                if let Some( completions ) = self.read.completions.take()
            {
                    if completions.len() == 1 {
                        self.substitute_completion( &completions[0] )?;
                    } else {
                        self.show_completions( &completions )?;
                        self.read.completions = Some( completions );
                    }
                } else {
                    self.build_completions();
                    let completions = self.read.completions.take().unwrap_or_default();

                    if completions.len() == 1 { self.substitute_completion( &completions[0] )?; }
                    
                    else if !completions.is_empty()
                    {
                        let start = self.read.completion_start;
                        let end = self.write.cursor;
                        let pfx = get::longest_common_prefix
                        ( completions.iter().map( |compl| &compl.completion[..] ) ).unwrap_or_default();

                        self.replace_str_forward( start..end, &pfx )?;
                        self.read.completions = Some( completions );
                    }
                }

                Ok( () )
            }

            fn substitute_completion( &mut self, compl:&Completion ) -> io::Result<()> {
                let mut s = self.read.completer.quote( &compl.completion );

                if let Some( suffix ) = compl.suffix.with_default( self.read.completion_append_character )
            {
                    s.to_mut().push( suffix );
                }

                let start = self.read.completion_start;
                let end = self.write.cursor;
                self.replace_str_forward( start..end, &s )
            }


            fn insert_completions( &mut self, completions:&[Completion] ) -> io::Result<()> {
                let mut words = String::new();

                for compl in completions {
                    words.push_str( &self.read.completer.unquote( &compl.completion ) );
                    words.push( ' ' );
                }

                let start = self.read.completion_start;
                let end = self.write.cursor;

                self.replace_str_forward( start..end, &words )
            }

            fn show_completions( &mut self, completions:&[Completion] ) -> io::Result<()> {
                if completions.is_empty()
            {
                    return Ok( () );
                }

                let eff_width = self.write.screen_size.columns
                    .min( self.read.completion_display_width );

                let completions = completions.iter()
                    .map( |compl| display_str( &compl.display(), Display::default() ).into_owned() )
                    .collect::<Vec<_>>();

                let cols = columns( &completions, eff_width, self.read.print_completions_horizontally );
                let table = Table::new( &completions, cols.as_ref().map( |c| &c[..] ),
                    self.read.print_completions_horizontally );

                self.write.write_str( "\n" )?;

                let n_completions = completions.len();

                if self.read.page_completions &&
                        n_completions >= self.read.completion_query_items {
                    self.start_page_completions( n_completions )
                } else {
                    self.show_list_completions( table )?;
                    self.write.draw_prompt()
                }
            }

            fn start_page_completions( &mut self, n_completions: usize ) -> io::Result<()> {
                self.read.state = InputState::CompleteIntro;
                self.write.redraw_prompt( PromptType::CompleteIntro( n_completions ) )
            }

            fn end_page_completions( &mut self ) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.prompt_type = PromptType::Normal;
                self.write.draw_prompt()
            }

            fn is_paging_completions( &self ) -> bool {
                match self.read.state {
                    InputState::CompleteMore( _ ) => true,
                    _ => false
                }
            }

            fn show_completions_page( &mut self, offset: usize ) -> io::Result<()> {
                if let Some( compl ) = self.read.completions.take()
            {
                    let width = self.write.screen_size.columns
                        .min( self.read.completion_display_width );
                    let n_lines = self.write.screen_size.lines - 1;

                    let completions = compl.iter()
                        .map( |compl| display_str( &compl.display(), Display::default() ).into_owned() )
                        .collect::<Vec<_>>();

                    let cols = columns( &completions, width, self.read.print_completions_horizontally );
                    let mut table = Table::new( &completions, cols.as_ref().map( |c| &c[..] ),
                        self.read.print_completions_horizontally );

                    for row in table.by_ref().skip( offset ).take( n_lines )
            {
                        self.show_completion_line( row )?;
                    }

                    if table.has_more()
            {
                        self.read.completions = Some( compl );
                        self.read.state = InputState::CompleteMore( offset + n_lines );
                        self.write.prompt_type = PromptType::CompleteMore;
                        self.write.draw_prompt()?;
                    } else {
                        self.end_page_completions()?;
                    }
                }

                Ok( () )
            }

            fn show_completions_line( &mut self, offset: usize ) -> io::Result<()> {
                if let Some( compl ) = self.read.completions.take()
            {
                    let width = self.write.screen_size.columns
                        .min( self.read.completion_display_width );
                    let completions = compl.iter()
                        .map( |compl| display_str( &compl.display(), Display::default() ).into_owned() )
                        .collect::<Vec<_>>();

                    let cols = columns( &completions, width, self.read.print_completions_horizontally );
                    let mut table = Table::new( &completions, cols.as_ref().map( |c| &c[..] ),
                        self.read.print_completions_horizontally );

                    if let Some( row ) = table.by_ref().skip( offset ).next()
            {
                        self.show_completion_line( row )?;
                    }

                    if table.has_more()
            {
                        self.read.completions = Some( compl );
                        self.read.state = InputState::CompleteMore( offset + 1 );
                        self.write.prompt_type = PromptType::CompleteMore;
                        self.write.draw_prompt()?;
                    } else {
                        self.end_page_completions()?;
                    }
                }

                Ok( () )
            }

            fn show_completion_line<S: AsRef<str>>( &mut self, line: Line<S> ) -> io::Result<()> {
                let mut space = 0;

                for ( width, name ) in line {
                    self.write.move_right( space )?;
                    self.write.write_str( name )?;
                    space = width - name.chars().count();
                }

                self.write.write_str( "\n" )
            }

            fn show_list_completions<S: AsRef<str>>( &mut self, table: Table<S> ) -> io::Result<()> {
                for line in table {
                    let mut space = 0;

                    for ( width, name ) in line {
                        self.write.move_right( space )?;
                        self.write.write_str( name )?;
                        space = width - name.chars().count();
                    }
                    self.write.write_str( "\n" )?;
                }

                Ok( () )
            }

            fn next_completion( &mut self, n: usize ) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or( 0, |c| c.len() );
                let max = len + 1;

                let old = self.read.completion_index;
                let new = ( old + n ) % max;

                if old != new {
                    self.set_completion( new )?;
                }

                Ok( () )
            }

            fn prev_completion( &mut self, n: usize ) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or( 0, |c| c.len() );
                let max = len + 1;

                let old = self.read.completion_index;
                let new = if n <= old {
                    max - old - n
                } else {
                    old - n
                };

                self.set_completion( new )
            }

            fn set_completion( &mut self, new: usize ) -> io::Result<()> {
                let len = self.read.completions.as_ref().map_or( 0, |c| c.len() );
                let old = self.read.completion_index;

                if old != new {
                    self.read.completion_index = new;

                    if new == len {
                        let start = self.read.completion_prefix;
                        let end = self.write.cursor;

                        self.delete_range( start..end )?;
                    } else {
                        let start = self.read.completion_start;
                        let end = self.write.cursor;
                        let s = self.read.completions.as_ref().unwrap()[new]
                            .completion( self.read.completion_append_character ).into_owned();

                        self.replace_str_forward( start..end, &s )?;
                    }
                }

                Ok( () )
            }

            fn abort_search_history( &mut self ) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.read.last_cmd = Category::Other;
                self.write.abort_search_history()
            }

            fn end_search_history( &mut self ) -> io::Result<()> {
                self.read.state = InputState::NewSequence;
                self.write.end_search_history()
            }

            pub fn handle_resize( &mut self, size:Size ) -> io::Result<()> {
                self.expire_blink()?;

                if self.is_paging_completions()
            {
                    self.end_page_completions()?;
                }

                self.write.screen_size = size;

                let p = self.write.prompt_type;
                self.write.redraw_prompt( p )
            }

            pub fn handle_signal( &mut self, signal: Signal ) -> io::Result<()> {
                self.expire_blink()?;

                match signal {
                    Signal::Continue => {
                        self.write.draw_prompt()?;
                    }
                    Signal::Interrupt => {
                        self.read.macro_buffer.clear();
                        self.write.move_to_end()?;

                        if self.read.echo_control_characters {
                            self.write.write_str( "^C" )?;
                        }

                        self.write.write_str( "\n" )?;
                        self.reset_input();
                        self.write.draw_prompt()?;
                    }
                    _ => ()
                }

                Ok( () )
            }

            fn backward_word( &mut self, n: usize ) -> io::Result<()> {
                let pos = backward_word( n,
                    &self.write.buffer, self.write.cursor, &self.read.word_break );
                self.write.move_to( pos )
            }

            fn forward_word( &mut self, n: usize ) -> io::Result<()> {
                let pos = forward_word( n,
                    &self.write.buffer, self.write.cursor, &self.read.word_break );
                self.write.move_to( pos )
            }
            /// Deletes a range of text from the input buffer.
            pub fn delete_range<R: RangeArgument<usize>>( &mut self, range: R ) -> io::Result<()> {
                self.write.delete_range( range )
            }
            /// Deletes a range from the buffer and adds the removed text to the kill ring.
            pub fn kill_range<R: RangeArgument<usize>>( &mut self, range: R ) -> io::Result<()> {
                let start = range.start().cloned().unwrap_or( 0 );
                let end = range.end().cloned().unwrap_or_else( || self.write.buffer.len() );
                let len = end - start;

                if len != 0 {
                    let buf = self.write.buffer[start..end].to_owned();

                    if self.read.last_cmd != Category::Kill {
                        self.push_kill_ring( buf );
                    } else if end == self.write.cursor {
                        self.prepend_kill_ring( buf );
                    } else {
                        self.append_kill_ring( buf );
                    }

                    self.delete_range( start..end )?;
                }

                Ok( () )
            }

            fn push_kill_ring( &mut self, s: String )
            {
                if self.read.kill_ring.len() == self.read.kill_ring.capacity()
            {
                    self.read.kill_ring.pop_back();
                }
                self.read.kill_ring.push_front( s );
            }

            fn rotate_kill_ring( &mut self )
            {
                if let Some( kill ) = self.read.kill_ring.pop_front()
            {
                    self.read.kill_ring.push_back( kill );
                }
            }

            fn append_kill_ring( &mut self, s: String )
            {
                if let Some( kill ) = self.read.kill_ring.front_mut()
            {
                    kill.push_str( &s );
                    return;
                }
                self.push_kill_ring( s );
            }

            fn prepend_kill_ring( &mut self, s: String )
            {
                if let Some( kill ) = self.read.kill_ring.front_mut()
            {
                    kill.insert_str( 0, &s );
                    return;
                }
                self.push_kill_ring( s );
            }
            /// Transposes two regions of the buffer, `src` and `dest`.
            pub fn transpose_range( &mut self, src: Range<usize>, dest: Range<usize> )
                    -> io::Result<()> {
                self.write.transpose_range( src, dest )
            }
            /// Insert text from the front of the kill ring at the current cursor position.
            pub fn yank( &mut self ) -> io::Result<()> {
                if let Some( kill ) = self.read.kill_ring.front().cloned()
            {
                    let start = self.write.cursor;
                    self.read.last_yank = Some( ( start, start + kill.len() ) );

                    self.insert_str( &kill )?;
                }

                Ok( () )
            }
            /// Rotates the kill ring and replaces yanked text with the new front.
            pub fn yank_pop( &mut self ) -> io::Result<()> {
                if let Some( ( start, end ) ) = self.read.last_yank {
                    self.rotate_kill_ring();

                    if let Some( kill ) = self.read.kill_ring.front().cloned()
            {
                        self.read.last_yank = Some( ( start, start + kill.len() ) );

                        self.write.move_to( start )?;
                        self.replace_str_forward( start..end, &kill )?;
                    }
                }

                Ok( () )
            }
            /// Overwrite `n` characters; assumes `n >= 1`
            fn overwrite( &mut self, n:usize, ch: char ) -> io::Result<()> {
                let start = self.write.cursor;
                let end = char::forward( n, &self.write.buffer, start );

                {
                    let over = &self.write.buffer[start..end];
                    let n_chars = over.chars().count();

                    if n > n_chars {
                        self.read.overwritten_append += n - n_chars;
                    }

                    if !over.is_empty()
            {
                        self.read.overwritten_chars.push_str( &over );
                    }
                }

                let s = char::repeat( ch, n );
                self.replace_str_forward( start..end, &s )
            }

            fn overwrite_back( &mut self, mut n: usize ) -> io::Result<()>
            {
                if self.read.overwritten_append != 0 
                {
                    let n_del = n.min( self.read.overwritten_append );
                    let pos = char::backward( n_del, &self.write.buffer, self.write.cursor );
                    let r = pos..self.write.cursor;
                    self.delete_range( r )?;
                    self.read.overwritten_append -= n_del;
                    n -= n_del;
                }

                if n != 0 && !self.read.overwritten_chars.is_empty()
                {
                    let n_repl = n.min( self.read.overwritten_chars.chars().count() );
                    let pos = char::backward( n_repl, &self.write.buffer, self.write.cursor );
                    let over_pos = char::backward
                    ( 
                        n_repl,
                        &self.read.overwritten_chars,
                        self.read.overwritten_chars.len()
                   );
                    let over = self.read.overwritten_chars.drain( over_pos.. ).collect::<String>();
                    let r = pos..self.write.cursor;
                    self.replace_str_backward( r, &over )?;
                    n -= n_repl;
                }

                if n != 0 { self.write.backward_char( n )?; }

                Ok( () )
            }
            /// Insert a given character at the current cursor position `n` times.
            pub fn insert( &mut self, n:usize, ch: char ) -> io::Result<()>
            {
                if n != 0
                {
                    let s = char::repeat( ch, n );
                    self.insert_str( &s )?;
                }

                Ok( () )
            }
            /// Insert a string at the current cursor position.
            pub fn insert_str( &mut self, s:&str ) -> io::Result<()> {
                self.write.insert_str( s )
            }
            /// Replaces a range in the buffer and redraws.
            pub fn replace_str_backward<R: RangeArgument<usize>>( &mut self,
                    range: R, s:&str ) -> io::Result<()> {
                self.replace_str_impl( range, s )?;
                let len = self.write.buffer.len();
                self.write.move_from( len )
            }
            /// Replaces a range in the buffer and redraws.
            pub fn replace_str_forward<R: RangeArgument<usize>>( &mut self,
                    range: R, s:&str ) -> io::Result<()> {
                self.replace_str_impl( range, s )?;
                self.write.cursor += s.len();
                let len = self.write.buffer.len();
                self.write.move_from( len )
            }
            /// Replaces a range in the buffer and redraws.
            fn replace_str_impl<R: RangeArgument<usize>>( &mut self,
                    range: R, s:&str ) -> io::Result<()> {
                let start = range.start().cloned().unwrap_or( 0 );
                let end = range.end().cloned().unwrap_or_else( || self.write.buffer.len() );
                self.write.move_to( start )?;

                let _ = self.write.buffer.drain( start..end );
                let cursor = self.write.cursor;
                self.write.buffer.insert_str( cursor, s );

                self.write.draw_buffer( cursor )?;
                self.write.clear_to_screen_end()
            }
        }

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
            let mut prompt = render_prompt( sh, &ps );
            if let Some( ( w, _h ) ) = libs::term_size::dimensions()
            {
                if get_prompt_len( &prompt ) > ( w / 2 ) as i32
                    && !regex::contains( &ps, r#"( ?i )\$\{?newline.\}?"# )
                {
                    prompt.push_str( "\n$ " );
                }
            } else {
                log!( "ERROR: Failed to get term size" );
            }
            prompt
        }
    
    }
    pub use self::prompt::{ Prompter };

    pub mod size
    {
        use ::
        {
            libc::{ c_int, c_ulong, winsize, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO },
            mem::{ zeroed },
            *,
        };
        
        #[cfg( any( target_os = "linux", target_os = "android" ) )] static TIOCGWINSZ: c_ulong = 0x5413;
        #[cfg( any
        ( 
            target_os = "macos",
            target_os = "ios",
            target_os = "dragonfly",
            target_os = "freebsd",
            target_os = "netbsd",
            target_os = "openbsd"
       ) )] static TIOCGWINSZ: c_ulong = 0x40087468;
        #[cfg( target_os = "solaris" )] static TIOCGWINSZ: c_ulong = 0x5468;

        extern "C"
        {
            fn ioctl( fd: c_int, request: c_ulong, ... ) -> c_int;
        }
        /// Runs the ioctl command.
        unsafe fn get_dimensions_any() -> winsize
        {
            let mut window: winsize = zeroed();
            let mut result = ioctl( STDOUT_FILENO, TIOCGWINSZ, &mut window );

            if result == -1
            {
                window = zeroed();
                result = ioctl( STDIN_FILENO, TIOCGWINSZ, &mut window );
                if result == -1
                {
                    window = zeroed();
                    result = ioctl( STDERR_FILENO, TIOCGWINSZ, &mut window );
                    if result == -1 { return zeroed(); }
                }
            }
            window
        }
        /// Query the current processes's stdout, stdin, and error stderr in that order, 
        // in the attempt to dtermine terminal width.
        pub fn dimensions() -> Option<( usize, usize )>
        {
            let w = unsafe { get_dimensions_any() };

            if w.ws_col == 0 || w.ws_row == 0 { None }
            else { Some( ( w.ws_col as usize, w.ws_row as usize ) ) }
        }
        /// Represents the size of a terminal window.
        #[derive( Copy, Clone, Debug, Eq, PartialEq )]
        pub struct Size
        {
            /// Number of lines in the terminal
            pub lines:usize,
            /// Number of columns in the terminal
            pub columns:usize,
        }

        impl Size
        {
            /// Returns the total number of cells in a terminal of the given size.
            #[inline] pub fn area( &self ) -> usize
            { self.checked_area().unwrap_or_else( || panic!( "overflow in Size::area {:?}", self ) ) }
            /// Returns the total number of cells in a terminal of the given size.
            #[inline] pub fn checked_area( &self ) -> Option<usize> { self.lines.checked_mul( self.columns ) }
        }
    }

    pub mod color
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
        fn name( &self ) -> &str;

        /// Acquires a lock on terminal read operations and returns a value holding
        /// that lock and granting access to such operations.
        fn lock_read<'a>( &'a self ) -> Box<dyn TerminalReader<Self> + 'a>;
        /// Acquires a lock on terminal write operations and returns a value holding
        /// that lock and granting access to such operations.
        fn lock_write<'a>( &'a self ) -> Box<dyn TerminalWriter<Self> + 'a>;
    }
    /// The main interface to input reading and other terminal operations
    pub struct Interface<Term: Terminals>
    {
        term: Arc<Term>,
        read: Arc<Mutex<Read<Term>>>,
        write: Arc<Mutex<Write>>,
        highlighter:Option<Arc<dyn Highlighter + Send + Sync>>,
    }

    impl Interface<DefaultTerminal> 
    {
        /// Creates a new `Interface` with the given application name.
        pub fn new<T>( application:T ) -> io::Result<Interface<DefaultTerminal>>
                where T: Into<Cow<'static, str>> {
            let term = DefaultTerminal::new()?;
            Interface::with_term( application, term )
        }
    }

    impl<Term: Terminals> Interface<Term>
    {
        /// Creates a new `Interface` instance with a particular terminal implementation.
        pub fn with_term<T>( application:T, term: Term ) -> io::Result<Interface<Term>> where 
        T:Into<Cow<'static, str>>
        {
            let size = term.lock_write().size()?;
            let read = Read::new( &term, application.into() );

            Ok( Interface
            {
                term: Arc::new( term ),
                read: Arc::new( Mutex::new( read ) ),
                write: Arc::new( Mutex::new( Write::new( size ) ) ),
                highlighter: None,
            } )
        }
        /// Acquires the read lock and returns a `Reader` instance.
        pub fn lock_reader( &self ) -> Reader<Term> {
            Reader::new( self, self.lock_read() )
        }
        /// Acquires the write lock and returns a `Writer` instance.
        pub fn lock_writer_append( &self ) -> io::Result<Writer<Term>> {
            Writer::with_lock( self.lock_write()?, false )
        }
        /// Acquires the write lock and returns a `Writer` instance.
        pub fn lock_writer_erase( &self ) -> io::Result<Writer<Term>> {
            Writer::with_lock( self.lock_write()?, true )
        }

        fn lock_read( &self ) -> ReadLock<Term> {
            ReadLock::new( 
                self.term.lock_read(),
                self.read.lock().expect( "Interface::lock_read" ) )
        }

        pub fn lock_write( &self )
                -> io::Result<WriteLock<Term>> {
            let guard = self.write.lock().unwrap();
            let term_writer = self.term.lock_write();

            Ok( WriteLock::new( term_writer, guard, self.highlighter.clone() ) )
        }

        pub fn lock_write_data( &self ) -> MutexGuard<Write> {
            self.write.lock().expect( "Interface::lock_write_data" )
        }
    }
    /// ## Locking
    /// The following methods internally acquire the read lock.
    impl<Term: Terminals> Interface<Term> 
    {
        /// Interactively reads a line from the terminal device.
        pub fn read_line( &self ) -> io::Result<ReadResult> {
            self.lock_reader().read_line()
        }
        /// Performs one step of the interactive `read_line` loop.
        pub fn read_line_step( &self, timeout:Option<Duration> )
                -> io::Result<Option<ReadResult>> {
            self.lock_reader().read_line_step( timeout )
        }
        /// Cancels an in-progress `read_line` operation.
        pub fn cancel_read_line( &self ) -> io::Result<()> {
            self.lock_reader().cancel_read_line()
        }
        /// Returns a clone of the current completer instance.
        pub fn completer( &self ) -> Arc<dyn Completer<Term>> {
            self.lock_reader().completer().clone()
        }
        /// Replaces the current completer, returning the previous instance.
        pub fn set_completer( &self, completer: Arc<dyn Completer<Term>> )
                -> Arc<dyn Completer<Term>> {
            self.lock_reader().set_completer( completer )
        }
        /// Returns the value of the named variable or `None` if no such variable exists.
        pub fn get_variable( &self, name:&str ) -> Option<Variable> {
            self.lock_reader().get_variable( name )
        }
        /// Sets the value of the named variable and returns the previous value.
        pub fn set_variable( &self, name:&str, value:&str ) -> Option<Variable> {
            self.lock_reader().set_variable( name, value )
        }
        /// Returns whether the given `Signal` is ignored.
        pub fn ignore_signal( &self, signal: Signal ) -> bool {
            self.lock_reader().ignore_signal( signal )
        }
        /// Sets whether the given `Signal` will be ignored.
        pub fn set_ignore_signal( &self, signal: Signal, set:bool )
            {
            self.lock_reader().set_ignore_signal( signal, set )
        }
        /// Returns whether the given `Signal` is reported.
        pub fn report_signal( &self, signal: Signal ) -> bool {
            self.lock_reader().report_signal( signal )
        }
        /// Sets whether the given `Signal` is reported.
        pub fn set_report_signal( &self, signal: Signal, set:bool )
            {
            self.lock_reader().set_report_signal( signal, set )
        }
        /// Binds a sequence to a command.
        pub fn bind_sequence<T>( &self, seq:T, cmd: Command ) -> Option<Command> where 
        T: Into<Cow<'static, str>> {
            self.lock_reader().bind_sequence( seq, cmd )
        }

        /// Binds a sequence to a command, if and only if the given sequence is not already bound to a command.
        pub fn bind_sequence_if_unbound<T>( &self, seq:T, cmd: Command ) -> bool where
        T: Into<Cow<'static, str>> {
            self.lock_reader().bind_sequence_if_unbound( seq, cmd )
        }
        /// Removes a binding for the given sequence.
        pub fn unbind_sequence( &self, seq:&str ) -> Option<Command> {
            self.lock_reader().unbind_sequence( seq )
        }
        /// Defines a named function to which sequences may be bound.
        pub fn define_function<T>( &self, name:T, cmd: Arc<dyn Function<Term>> )
                -> Option<Arc<dyn Function<Term>>> where T: Into<Cow<'static, str>> {
            self.lock_reader().define_function( name, cmd )
        }
        /// Removes a function defined with the given name.
        pub fn remove_function( &self, name:&str ) -> Option<Arc<dyn Function<Term>>> {
            self.lock_reader().remove_function( name )
        }
        /// Evaluates a series of configuration directives.
        pub fn evaluate_directives( &self, dirs: Vec<Directive> )
            {
            self.lock_reader().evaluate_directives( &self.term, dirs )
        }
        /// Evaluates a single configuration directive.
        pub fn evaluate_directive( &self, dir: Directive )
            {
            self.lock_reader().evaluate_directive( &self.term, dir )
        }
    }
    /// ## Locking
    /// The following methods internally acquire the write lock.
    impl<Term: Terminals> Interface<Term>
    {
        /// Returns the current input buffer.
        pub fn buffer( &self ) -> String {
            self.lock_write().map( |lock| lock.buffer.to_owned() ).unwrap_or_default()
        }
        /// Returns the current number of history entries.
        pub fn history_len( &self ) -> usize {
            self.lock_write().map( |lock| lock.history_len() ).unwrap_or( 0 )
        }
        /// Returns the maximum number of history entries.
        pub fn history_size( &self ) -> usize {
            self.lock_write().map( |lock| lock.history_size() ).unwrap_or( 0 )
        }
        /// Save history entries to the specified file.
        pub fn save_history<P: AsRef<Path>>( &self, path:P ) -> io::Result<()> {
            let path = path.as_ref();
            let mut w = self.lock_write()?;

            if !path.exists() || w.history_size() == !0 {
                self.append_history( path, &w )?;
            } else {
                self.rewrite_history( path, &w )?;
            }

            w.reset_new_history();
            Ok( () )
        }

        fn append_history<P: AsRef<Path>>( &self, path: P, w:&WriteLock<Term> )
                -> io::Result<()> {
            let file = OpenOptions::new()
                .append( true )
                .create( true )
                .open( path.as_ref() )?;

            self.append_history_to( &file, w )
        }

        fn append_history_to( &self, file:&File, w:&WriteLock<Term> ) -> io::Result<()> {
            let mut wtr = BufWriter::new( file );

            for entry in w.new_history()
            {
                wtr.write_all( entry.as_bytes() )?;
                wtr.write_all( b"\n" )?;
            }

            wtr.flush()
        }

        fn rewrite_history<P: AsRef<Path>>( &self, path: P, w:&WriteLock<Term> )
                -> io::Result<()> {
            fn nth_line( s:&str, n: usize ) -> Option<usize> {
                let start = s.as_ptr() as usize;

                s.lines().nth( n )
                    .map( |s| s.as_ptr() as usize - start )
            }

            let mut file = OpenOptions::new()
                .create( true )
                .read( true )
                .write( true )
                .open( path.as_ref() )?;

            let mut hist = String::new();

            file.read_to_string( &mut hist )?;

            let n_lines = hist.lines().count();
            let n = n_lines.saturating_sub( 
                w.history_size() - w.new_history_entries() );

            if n != 0 {
                if let Some( pos ) = nth_line( &hist, n )
            {
                    file.seek( SeekFrom::Start( 0 ) )?;
                    file.write_all( hist[pos..].as_bytes() )?;

                    let n = file.seek( SeekFrom::Current( 0 ) )?;
                    file.set_len( n )?;
                }
            }

            self.append_history_to( &file, w )
        }
        /// Load history entries from the specified file.
        pub fn load_history<P: AsRef<Path>>( &self, path:P ) -> io::Result<()>
        {
            let mut writer = self.lock_write()?;

            let file = File::open( &path )?;
            let rdr = BufReader::new( file );

            for line in rdr.lines()
            {
                writer.add_history( line? );
            }

            writer.reset_new_history();

            Ok( () )
        }
        /// Writes formatted text to the terminal display.
        pub fn write_fmt( &self, args:fmt::Arguments ) -> io::Result<()> {
            let s = args.to_string();
            self.write_str( &s )
        }

        fn write_str( &self, line:&str ) -> io::Result<()> {
            self.lock_writer_erase()?.write_str( line )
        }
    }
    /// ## Locking
    /// The following methods internally acquire both the read and write locks.
    impl<Term: Terminals> Interface<Term> 
    {
        /// Sets the prompt that will be displayed when `read_line` is called.
        pub fn set_prompt( &self, prompt:&str ) -> io::Result<()> {
            self.lock_reader().set_prompt( prompt )
        }
        /// Sets the input buffer to the given string.
        pub fn set_buffer( &self, buf:&str ) -> io::Result<()> {
            self.lock_reader().set_buffer( buf )
        }
        /// Sets the cursor position in the input buffer.
        pub fn set_cursor( &self, pos: usize ) -> io::Result<()> {
            self.lock_reader().set_cursor( pos )
        }
        /// Adds a line to history.
        pub fn add_history( &self, line: String )
            {
            self.lock_reader().add_history( line );
        }
        /// Adds a line to history, unless it is identical to the most recent entry.
        pub fn add_history_unique( &self, line: String )
            {
            self.lock_reader().add_history_unique( line );
        }
        /// Removes all history entries.
        pub fn clear_history( &self )
            {
            self.lock_reader().clear_history();
        }
        /// Removes the history entry at the given index.
        pub fn remove_history( &self, idx: usize )
            {
            self.lock_reader().remove_history( idx );
        }
        /// Sets the maximum number of history entries.
        pub fn set_history_size( &self, n: usize )
            {
            self.lock_reader().set_history_size( n );
        }
        /// Truncates history to the only the most recent `n` entries.
        pub fn truncate_history( &self, n: usize )
            {
            self.lock_reader().truncate_history( n );
        }
        /// Sets the syntax highlighter for the input line.
        pub fn set_highlighter( &mut self, highlighter: Arc<dyn Highlighter + Send + Sync> )
            {
            self.highlighter = Some( highlighter );
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

        #[derive( Debug, PartialEq, Eq )]
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
                    Ok( dt ) => dt,
                    Err( _ ) => OffsetDateTime::now_utc(),
                };
                DateTime { odt }
            }

            pub fn from_timestamp( ts: f64 ) -> Self
            {
                let dummy_now = Self::now();
                let offset_seconds = dummy_now.odt.offset().whole_minutes() * 60;
                let ts_nano = ( ts + offset_seconds as f64 ) * 1000000000.0;
                let odt: OffsetDateTime = match OffsetDateTime::from_unix_timestamp_nanos( ts_nano as i128 )
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
            fn fmt( &self, f:&mut fmt::Formatter<'_> ) -> Displayed
            {
                write!( f, "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:03}",
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
        io::
        { 
            reader::{ ReadResult },
        },
        sync::{ Arc },
        terminal::{ completers, highlighter, Interface, prompt },
        *,
    };

    env::initialize_pathing();

    let mut sh = shell::Shell::new();
    let args: Vec<String> = env::args().collect();

    if is::login( &args )
    {
        fs::load_rc_files( &mut sh );
        sh.is_login = true;
    }
    
    highlighter::init_command_cache();
    highlighter::update_aliases( &sh );

    if is::script( &args )
    {
        log!( "run script: {:?} ", &args );
        let status = scripts::run( &mut sh, &args );
        process::exit( status );
    }

    if is::command_string( &args )
    {
        let line = env::args_to_command_line();
        log!( "run with -c args: {}", &line );
        now::run_command_line( &mut sh, &line, false, false );
        process::exit( sh.previous_status );
    }

    if is::non_tty()
    {
        now::run_procedures_for_non_tty( &mut sh );
        return;
    }

    let mut rl;
    match Interface::new( "cicada" )
    {
        Ok( x ) => rl = x,
        Err( e ) =>
        {
            println!( "cicada: lineread error: {}", e );
            return;
        }
    }

    rl.define_function( "enter-function", Arc::new( ffi::EnterFunction ) );
    rl.bind_sequence( "\r", Command::from_str( "enter-function" ) );

    let highlighter = highlighter::create_highlighter();
    rl.set_highlighter( highlighter );

    history::init( &mut rl );
    rl.set_completer( Arc::new( completers::CicadaCompleter 
    {
        sh: Arc::new( sh.clone() ),
    } ) );

    let sig_handler_enabled = is::signal_handler_enabled();
    
    if sig_handler_enabled
    {
        signals::setup_sigchld_handler();
        signals::block_signals();
    }

    loop
    {
        let prompt = prompt::get_prompt( &sh );
        match rl.set_prompt( &prompt )
        {
            Ok( _ ) => {}
            Err( e ) => { println_stderr!( "cicada: prompt error: {}", e ); }
        }

        if sig_handler_enabled { signals::unblock_signals(); }

        match rl.read_line()
        {
            Ok( ReadResult::Input( line ) ) =>
            {
                if sig_handler_enabled { signals::block_signals(); }

                let line = shell::trim_multiline_prompts( &line );
                if line.trim() == "" {
                    jobc::try_wait_bg_jobs( &mut sh, true, sig_handler_enabled );
                    continue;
                }
                sh.cmd = line.clone();

                let tsb = time::c::DateTime::now().unix_timestamp();
                let mut line = line.clone();
                tools::extend_bangbang( &sh, &mut line );

                let mut status = 0;
                let cr_list = now::run_command_line( &mut sh, &line, true, false );
                if let Some( last ) = cr_list.last()
            {
                    status = last.status;
                }
                let tse = time::c::DateTime::now().unix_timestamp();

                if !sh.cmd.starts_with( ' ' ) && line != sh.previous_cmd {
                    history::add( &sh, &mut rl, &line, status, tsb, tse );
                    sh.previous_cmd = line.clone();
                }

                if tools::is_shell_altering_command( &line ) 
                {
                    rl.set_completer( Arc::new( completers::CicadaCompleter {
                        sh: Arc::new( sh.clone() ),
                    } ) );

                    // Update aliases in the highlighter when they might have changed
                    highlight::update_aliases( &sh );
                }

                jobc::try_wait_bg_jobs( &mut sh, true, sig_handler_enabled );
                continue;
            }
            Ok( ReadResult::Eof ) => {
                if let Ok( x ) = env::var( "NO_EXIT_ON_CTRL_D" )
            {
                    if x == "1" {
                        println!();
                    }
                } else {
                    println!( "exit" );
                    break;
                }
            }
            Ok( ReadResult::Signal( s ) ) => {
                println_stderr!( "readline signal: {:?}", s );
            }

            Err( e ) =>
            {
                println_stderr!( "readline error: {}", e );
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
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 21291
