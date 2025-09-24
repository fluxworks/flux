/*!
*/
#![feature
(

)]

#![allow
(
    dead_code,
    static_mut_refs,
    unused_attributes,
    unused_variables,
)]

extern crate regex;

/*
pub mod io
{
    pub use std::io::{ * };
}

pub mod _
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */

}
*/
pub static mut LINES:Vec<String> = Vec::new();
pub static mut SOURCE:String = String::new();

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod io
{
    pub use std::io::{ * };
}

pub unsafe fn domain() -> Result<(), ::io::Error>
{
    use ::io::prelude::*;
    use regex::RegexSet;

    let mut sourced = String::new();
    let mut f = ::fs::File::open( SOURCE.as_str() )?;    
    f.read_to_string( &mut sourced )?;

    let lines:Vec<&str> = sourced.lines().collect();
    
    let set = RegexSet::new
    (&[
        r#"^#\[path = \".+\"\]$"#,
        r#"^#\[path = \".+\"\] mod (.)+;$"#,
        r#"^#\[path = \".+\"\] pub mod (.)+;$"#,
        r#"^#\[path = \".+\"\] unsafe mod (.)+;$"#,        
        r#"^#\[path = \".+\"\] pub unsafe mod (.)+;$"#,
        r#"^mod (.)+;$"#,
        r#"^pub mod (.)+;$"#,
        r#"^unsafe mod (.)+;$"#,        
        r#"^pub unsafe mod (.)+;$"#,
    ]).unwrap();
    /*
    // Iterate over and collect all of the matches. Each match corresponds to the ID of the matching pattern.
    let matches: Vec<_> = set.matches("foobar").into_iter().collect();
    assert_eq!(matches, vec![0, 2, 3, 4, 6]);
    // You can also test whether a particular regex matched:
    let matches = set.matches("foobar");
    assert!(!matches.matched(5));
    assert!(matches.matched(6));
    */

    for line in lines
    {
        match line.len()
        {
            0 => 
            {

            }

            _=>
            {
                let matches: Vec<_> = set.matches("foobar").into_iter().collect();
                println!( r#"{:?}"#, matches );
                /*
                println!( r#"{}"#, line );
                
                match true
                {
                    true if line.starts_with( "unsafe" ) =>
                    {
                        //
                    }
                    
                    _=>
                    {
                        //
                    }
                }
                
                LINES.push( line.to_string() ); */
            }
        }
    }
    
    Ok(())
}

pub fn main() -> Result<(), ::io::Error>
{
    unsafe
    {
        SOURCE = "example.rs".to_string();
        Ok( domain()? )
    }
}
