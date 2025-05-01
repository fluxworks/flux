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
    unused_attributes,
    unused_imports,
    unused_unsafe,
    unused_variables,
    unreachable_code,    
 )]
/*
External Macros
#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static; */
/*
External Crates */
extern crate regex as re;
extern crate nix; 
/*
    extern crate fnv;
    extern crate libc;
    extern crate memchr;
    extern crate smallvec;
    extern crate time as timing;
    extern crate unicode_normalization;
    extern crate unicode_width; 
*/

extern crate libc;

#[macro_use] pub mod macros
{
    #[macro_export] macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            use ::io::Write;
            match writeln!( &mut ::io::stderr(), $fmt )
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            use ::io::Write;
            match writeln!( &mut ::io::stderr(), $fmt, $($arg)* )
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }

}

pub mod borrow
{
    //! A module for working with borrowed data.
    pub use std::borrow::{ * };
}

pub mod collections
{
    //! Collection types.
    pub use std::collections::{ * };
}

pub mod convert
{
    //! Traits for conversions between types.
    pub use std::convert::{ * };
}

pub mod env
{
    //! Inspection and manipulation of the process's environment.
    pub use std::env::{ * };
    use ::
    {
        path::{ Path },
        *,
    };

    pub fn initialize_paths()
    {
        let mut directories: Vec<String> = vec![];
        for directory in
        [
            "/usr/local/sbin",
            "/usr/local/bin",
            "/usr/sbin",
            "/usr/bin",
            "/sbin",
            "/bin",
        ]
        { 
            if Path::new( directory ).exists() { directories.push( directory.to_string() ); }
        }

        if let Ok( environment ) = var("PATH")
        {
            for directory in environment.split(":") 
            {
                println!( r#"{}"#, directory );

                if !directories.contains( &directory.to_string() ) 
                {
                    directories.push( directory.to_string() );
                }
            }
        }

        let directories = directories.join(":");
        set_var( "PATH", directories );
    }

    
}

pub mod error
{
    //! Error Handling With Results.
    pub use std::error::{ * };
    /*
    uuid::error.rs */
    pub mod uuid
    {
        use ::
        {
            str::{ from_utf8, from_utf8_unchecked },
            *,
        };
        /// A general error that can occur when working with UUIDs.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct Error(pub ErrorKind);

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum ErrorKind
        {
            /// Invalid character in the [`Uuid`] string.
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            Char { character: char, index: usize },
            /// A simple [`Uuid`] didn't contain 32 characters.
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            SimpleLength { len: usize },
            /// A byte array didn't contain 16 bytes
            ByteLength { len: usize },
            /// A hyphenated [`Uuid`] didn't contain 5 groups
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            GroupCount { count: usize },
            /// A hyphenated [`Uuid`] had a group that wasn't the right length
            ///
            /// [`Uuid`]: ../struct.Uuid.html
            GroupLength {
                group: usize,
                len: usize,
                index: usize,
            },
            /// The input was not a valid UTF8 string
            InvalidUTF8,
            /// The UUID is nil.
            Nil,
            /// Some other error occurred.
            Other,
        }

        /// A string that is guaranteed to fail to parse to a [`Uuid`].
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct InvalidUuid<'a>(pub &'a [u8]);

        impl<'a> InvalidUuid<'a>
        {
            /// Converts the lightweight error type into detailed diagnostics.
            pub fn into_err(self) -> Error
            {
                let input_str = match from_utf8(self.0)
                {
                    Ok(s) => s,
                    Err(_) => return Error(ErrorKind::InvalidUTF8),
                };

                let (uuid_str, offset, simple) = match input_str.as_bytes()
                {
                    [b'{', s @ .., b'}'] => (s, 1, false),
                    [b'u', b'r', b'n', b':', b'u', b'u', b'i', b'd', b':', s @ ..] => { (s, "urn:uuid:".len(), false) }
                    s => (s, 0, true),
                };

                let mut hyphen_count = 0;
                let mut group_bounds = [0; 4];
                let uuid_str = unsafe { from_utf8_unchecked(uuid_str) };

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
                    else if !byte.is_ascii_hexdigit()
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
                        len: input_str.len(),
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
                    ErrorKind::Char
                    {
                        character, index, ..
                    } =>
                    {
                        write!(f, "invalid character: expected an optional prefix of `urn:uuid:` followed by [0-9a-fA-F-], found `{}` at {}", character, index)
                    }
                    
                    ErrorKind::SimpleLength { len } =>
                    {
                        write!
                        (
                            f,
                            "invalid length: expected length 32 for simple format, found {}",
                            len
                        )
                    }

                    ErrorKind::ByteLength { len } => { write!(f, "invalid length: expected 16 bytes, found {}", len) }
                    ErrorKind::GroupCount { count } => { write!(f, "invalid group count: expected 5, found {}", count) }
                    ErrorKind::GroupLength { group, len, .. } =>
                    {
                        let expected = [8, 4, 4, 4, 12][group];
                        write!(
                            f,
                            "invalid group length in group {}: expected {}, found {}",
                            group, expected, len
                        )
                    }
                    ErrorKind::InvalidUTF8 => write!(f, "non-UTF8 input"),
                    ErrorKind::Nil => write!(f, "the UUID is nil"),
                    ErrorKind::Other => write!(f, "failed to parse a UUID"),
                }
            }
        }
        
        impl ::error::Error for self::Error {}
    }
}

pub mod fmt
{
    //! Utilities for formatting and printing strings.
    //! Adapters for alternative string formats for UUID
    pub use std::fmt::{ * };
    use ::
    {
        borrow::{ Borrow },
        primitive::{ Uuid, Variant },
        str::{ FromStr },
        string::{ String, ToString },
        *,
    };
    // rust-uuid v1.4::fmt.rs
    

    macro_rules! impl_fmt_traits
    {
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

    macro_rules! impl_fmt_from
    {
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

    impl std::fmt::Debug for Uuid
    {
        #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
    }

    impl fmt::Display for Uuid
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::LowerHex::fmt(self, f) }
    }
    
    impl From<Uuid> for String
    {
        fn from(uuid: Uuid) -> Self { uuid.to_string() }
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
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            fmt::LowerHex::fmt(self.as_hyphenated(), f)
        }
    }

    impl fmt::UpperHex for Uuid
    {
        #[inline] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            fmt::UpperHex::fmt(self.as_hyphenated(), f)
        }
    }

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

        // SAFETY: `buf` is guaranteed to be at least `LEN` bytes
        // SAFETY: The encoded buffer is ASCII encoded
        unsafe {
            ptr::write(dst.cast(), format_simple(src, upper));
            str::from_utf8_unchecked_mut(buf)
        }
    }

    #[inline]
    fn encode_hyphenated<'b>(src: &[u8; 16], buffer: &'b mut [u8], upper: bool) -> &'b mut str {
        let buf = &mut buffer[..Hyphenated::LENGTH];
        let dst = buf.as_mut_ptr();

        // SAFETY: `buf` is guaranteed to be at least `LEN` bytes
        // SAFETY: The encoded buffer is ASCII encoded
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

        // SAFETY: `buf` is guaranteed to be at least `LEN` bytes
        // SAFETY: The encoded buffer is ASCII encoded
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

        // SAFETY: `buf` is guaranteed to be at least `LEN` bytes
        // SAFETY: The encoded buffer is ASCII encoded
        unsafe {
            let dst = buf.as_mut_ptr().add(9);

            ptr::write(dst.cast(), format_hyphenated(src, upper));
            str::from_utf8_unchecked_mut(buf)
        }
    }

    impl Hyphenated {
        /// The length of a hyphenated [`Uuid`] string.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        pub const LENGTH: usize = 36;

        /// Creates a [`Hyphenated`] from a [`Uuid`].
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        /// [`Hyphenated`]: struct.Hyphenated.html
        pub const fn from_uuid(uuid: Uuid) -> Self {
            Hyphenated(uuid)
        }

        /// Writes the [`Uuid`] as a lower-case hyphenated string to
        /// `buffer`, and returns the subslice of the buffer that contains the
        /// encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936DA01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.hyphenated()
        ///             .encode_lower(&mut Uuid::encode_buffer()),
        ///         "936da01f-9abd-4d9d-80c7-02af85c822a8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 40];
        ///     uuid.hyphenated().encode_lower(&mut buf);
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"936da01f-9abd-4d9d-80c7-02af85c822a8!!!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_hyphenated(self.0.as_bytes(), buffer, false)
        }

        /// Writes the [`Uuid`] as an upper-case hyphenated string to
        /// `buffer`, and returns the subslice of the buffer that contains the
        /// encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936da01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.hyphenated()
        ///             .encode_upper(&mut Uuid::encode_buffer()),
        ///         "936DA01F-9ABD-4D9D-80C7-02AF85C822A8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 40];
        ///     uuid.hyphenated().encode_upper(&mut buf);
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"936DA01F-9ABD-4D9D-80C7-02AF85C822A8!!!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_hyphenated(self.0.as_bytes(), buffer, true)
        }

        /// Get a reference to the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let hyphenated = Uuid::nil().hyphenated();
        /// assert_eq!(*hyphenated.as_uuid(), Uuid::nil());
        /// ```
        pub const fn as_uuid(&self) -> &Uuid {
            &self.0
        }

        /// Consumes the [`Hyphenated`], returning the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let hyphenated = Uuid::nil().hyphenated();
        /// assert_eq!(hyphenated.into_uuid(), Uuid::nil());
        /// ```
        pub const fn into_uuid(self) -> Uuid {
            self.0
        }
    }

    impl Braced {
        /// The length of a braced [`Uuid`] string.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        pub const LENGTH: usize = 38;

        /// Creates a [`Braced`] from a [`Uuid`].
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        /// [`Braced`]: struct.Braced.html
        pub const fn from_uuid(uuid: Uuid) -> Self {
            Braced(uuid)
        }

        /// Writes the [`Uuid`] as a lower-case hyphenated string surrounded by
        /// braces to `buffer`, and returns the subslice of the buffer that contains
        /// the encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936DA01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.braced()
        ///             .encode_lower(&mut Uuid::encode_buffer()),
        ///         "{936da01f-9abd-4d9d-80c7-02af85c822a8}"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 40];
        ///     uuid.braced().encode_lower(&mut buf);
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"{936da01f-9abd-4d9d-80c7-02af85c822a8}!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_braced(self.0.as_bytes(), buffer, false)
        }

        /// Writes the [`Uuid`] as an upper-case hyphenated string surrounded by
        /// braces to `buffer`, and returns the subslice of the buffer that contains
        /// the encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936da01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.braced()
        ///             .encode_upper(&mut Uuid::encode_buffer()),
        ///         "{936DA01F-9ABD-4D9D-80C7-02AF85C822A8}"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 40];
        ///     uuid.braced().encode_upper(&mut buf);
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"{936DA01F-9ABD-4D9D-80C7-02AF85C822A8}!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_braced(self.0.as_bytes(), buffer, true)
        }

        /// Get a reference to the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let braced = Uuid::nil().braced();
        /// assert_eq!(*braced.as_uuid(), Uuid::nil());
        /// ```
        pub const fn as_uuid(&self) -> &Uuid {
            &self.0
        }

        /// Consumes the [`Braced`], returning the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let braced = Uuid::nil().braced();
        /// assert_eq!(braced.into_uuid(), Uuid::nil());
        /// ```
        pub const fn into_uuid(self) -> Uuid {
            self.0
        }
    }

    impl Simple {
        /// The length of a simple [`Uuid`] string.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        pub const LENGTH: usize = 32;

        /// Creates a [`Simple`] from a [`Uuid`].
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        /// [`Simple`]: struct.Simple.html
        pub const fn from_uuid(uuid: Uuid) -> Self {
            Simple(uuid)
        }

        /// Writes the [`Uuid`] as a lower-case simple string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936DA01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.simple().encode_lower(&mut Uuid::encode_buffer()),
        ///         "936da01f9abd4d9d80c702af85c822a8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 36];
        ///     assert_eq!(
        ///         uuid.simple().encode_lower(&mut buf),
        ///         "936da01f9abd4d9d80c702af85c822a8"
        ///     );
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"936da01f9abd4d9d80c702af85c822a8!!!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_simple(self.0.as_bytes(), buffer, false)
        }

        /// Writes the [`Uuid`] as an upper-case simple string to `buffer`,
        /// and returns the subslice of the buffer that contains the encoded UUID.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936da01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.simple().encode_upper(&mut Uuid::encode_buffer()),
        ///         "936DA01F9ABD4D9D80C702AF85C822A8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 36];
        ///     assert_eq!(
        ///         uuid.simple().encode_upper(&mut buf),
        ///         "936DA01F9ABD4D9D80C702AF85C822A8"
        ///     );
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"936DA01F9ABD4D9D80C702AF85C822A8!!!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_simple(self.0.as_bytes(), buffer, true)
        }

        /// Get a reference to the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let simple = Uuid::nil().simple();
        /// assert_eq!(*simple.as_uuid(), Uuid::nil());
        /// ```
        pub const fn as_uuid(&self) -> &Uuid {
            &self.0
        }

        /// Consumes the [`Simple`], returning the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let simple = Uuid::nil().simple();
        /// assert_eq!(simple.into_uuid(), Uuid::nil());
        /// ```
        pub const fn into_uuid(self) -> Uuid {
            self.0
        }
    }

    impl Urn {
        /// The length of a URN [`Uuid`] string.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        pub const LENGTH: usize = 45;

        /// Creates a [`Urn`] from a [`Uuid`].
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        /// [`Urn`]: struct.Urn.html
        pub const fn from_uuid(uuid: Uuid) -> Self {
            Urn(uuid)
        }

        /// Writes the [`Uuid`] as a lower-case URN string to
        /// `buffer`, and returns the subslice of the buffer that contains the
        /// encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936DA01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.urn().encode_lower(&mut Uuid::encode_buffer()),
        ///         "urn:uuid:936da01f-9abd-4d9d-80c7-02af85c822a8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 49];
        ///     uuid.urn().encode_lower(&mut buf);
        ///     assert_eq!(
        ///         uuid.urn().encode_lower(&mut buf),
        ///         "urn:uuid:936da01f-9abd-4d9d-80c7-02af85c822a8"
        ///     );
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"urn:uuid:936da01f-9abd-4d9d-80c7-02af85c822a8!!!!" as &[_]
        ///     );
        ///     
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_lower<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_urn(self.0.as_bytes(), buffer, false)
        }

        /// Writes the [`Uuid`] as an upper-case URN string to
        /// `buffer`, and returns the subslice of the buffer that contains the
        /// encoded UUID.
        ///
        /// This is slightly more efficient than using the formatting
        /// infrastructure as it avoids virtual calls, and may avoid
        /// double buffering.
        ///
        /// [`Uuid`]: ../struct.Uuid.html
        ///
        /// # Panics
        ///
        /// Panics if the buffer is not large enough: it must have length at least
        /// [`LENGTH`]. [`Uuid::encode_buffer`] can be used to get a
        /// sufficiently-large temporary buffer.
        ///
        /// [`LENGTH`]: #associatedconstant.LENGTH
        /// [`Uuid::encode_buffer`]: ../struct.Uuid.html#method.encode_buffer
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// fn main() -> Result<(), uuid::Error> {
        ///     let uuid = Uuid::parse_str("936da01f9abd4d9d80c702af85c822a8")?;
        ///
        ///     // the encoded portion is returned
        ///     assert_eq!(
        ///         uuid.urn().encode_upper(&mut Uuid::encode_buffer()),
        ///         "urn:uuid:936DA01F-9ABD-4D9D-80C7-02AF85C822A8"
        ///     );
        ///
        ///     // the buffer is mutated directly, and trailing contents remains
        ///     let mut buf = [b'!'; 49];
        ///     assert_eq!(
        ///         uuid.urn().encode_upper(&mut buf),
        ///         "urn:uuid:936DA01F-9ABD-4D9D-80C7-02AF85C822A8"
        ///     );
        ///     assert_eq!(
        ///         &buf as &[_],
        ///         b"urn:uuid:936DA01F-9ABD-4D9D-80C7-02AF85C822A8!!!!" as &[_]
        ///     );
        ///
        ///     Ok(())
        /// }
        /// ```
        /// */
        #[inline]
        pub fn encode_upper<'buf>(&self, buffer: &'buf mut [u8]) -> &'buf mut str {
            encode_urn(self.0.as_bytes(), buffer, true)
        }

        /// Get a reference to the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let urn = Uuid::nil().urn();
        /// assert_eq!(*urn.as_uuid(), Uuid::nil());
        /// ```
        pub const fn as_uuid(&self) -> &Uuid {
            &self.0
        }

        /// Consumes the [`Urn`], returning the underlying [`Uuid`].
        ///
        /// # Examples
        ///
        /// ```rust
        /// use uuid::Uuid;
        ///
        /// let urn = Uuid::nil().urn();
        /// assert_eq!(urn.into_uuid(), Uuid::nil());
        /// ```
        pub const fn into_uuid(self) -> Uuid {
            self.0
        }
    }

    impl FromStr for Hyphenated {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            crate::parser::parse_hyphenated(s.as_bytes())
                .map(|b| Hyphenated(Uuid(b)))
                .map_err(|invalid| invalid.into_err())
        }
    }

    impl FromStr for Simple {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            crate::parser::parse_simple(s.as_bytes())
                .map(|b| Simple(Uuid(b)))
                .map_err(|invalid| invalid.into_err())
        }
    }

    impl FromStr for Urn {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            crate::parser::parse_urn(s.as_bytes())
                .map(|b| Urn(Uuid(b)))
                .map_err(|invalid| invalid.into_err())
        }
    }

    impl FromStr for Braced {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            crate::parser::parse_braced(s.as_bytes())
                .map(|b| Braced(Uuid(b)))
                .map_err(|invalid| invalid.into_err())
        }
    }
    impl_fmt_traits! {
        Hyphenated<>,
        Simple<>,
        Urn<>,
        Braced<>
    }
}

pub mod get
{
    //! State Retrieval
    use ::
    {
        env::{ current_dir },
        path::{ PathBuf },
        *,
    };
    /*
    pub fn get_current_dir(...) -> String  */
    pub fn current_directory() -> String 
    {
        let mut current = PathBuf::new();
        
        match current_dir() 
        {
            Ok( directory ) => current = directory,
            Err( code ) => { println_stderr!("env current_dir() failed: {}", code ); }
        }

        let mut directory = "";
        
        match current.to_str()
        {
            Some( current ) => directory = current,
            None => { println_stderr!("current_dir to str failed."); }
        }

        directory.to_string()
    }
}

pub mod hash
{
    //! Generic hashing support.
    pub use std::hash::{ * };
}

pub mod io
{
    //! Traits, helpers, and type definitions for core I/O functionality.
    pub use std::io::{ * };
}

pub mod is
{
    //! State Testing
    use ::
    {
        *,
    };
    /*
    pub fn is_builtin(...) -> bool */
    pub fn builtin( s:&str ) -> bool
    {
        let builtins = 
        [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg",
            "history", "jobs", "read", "source", "ulimit", "unalias", "vox",
            "minfd", "set", "unset", "unpath",
        ];
        builtins.contains(&s)
    }
}

pub mod path
{
    //! Cross-platform path manipulation.
    pub use std::path::{ * };
}

pub mod primitive
{
    pub use std::prelude::v1::{ * };
    pub use std::primitive::{ * };

    use ::
    {
        collections::{HashMap, HashSet},
        convert::{ TryFrom, TryInto },
        hash::{ Hash },
        regex::{ contains, Regex },
        *,
    };
    /*
    use crate::parsers;
    use crate::parsers::parser_line::tokens_to_redirections;
    use crate::shell;
    use crate::libs;
    use crate::tools;
    */
    pub type Token = (String, String);
    pub type Tokens = Vec<Token>;
    pub type Redirection = (String, String, String);

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
                true if self.is_exited()    => { return "Exited".to_string(); }
                true if self.is_stopped()   => { return "Stopped".to_string(); }
                true if self.is_continued() => { return "Continued".to_string(); }
                true if self.is_signaled()  => { return "Signaled".to_string(); }
                true if self.is_others()    => { return "Others".to_string(); }
                true if self.is_error()     => { return "Error".to_string(); }
                unknown                     => { return format!("unknown: {}", self.2); }
            }
        }

        pub fn get_status(&self) -> i32
        {
            if self.is_exited() { self.2 } 
            else { self._get_signaled_status() }
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
    /**
    command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    ...
    Command 
    {
        tokens: [("", "ls"), ("", "-G"), ("\'", "foo bar")],
        redirects_to:
        [
            ("2", ">", "&1"),
            ("1", ">", "/dev/null"),
        ],
        redirect_from: Some(("<", "one-file")),
    }
    */
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
        /*
        pub fn from_tokens( tokens: Tokens ) -> Result<Command, String>
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

            Ok(Command
            {
                tokens: tokens_final,
                redirects_to,
                redirect_from,
            })
        }
        */

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

        pub fn is_builtin(&self) -> bool { is::builtin(&self.tokens[0].1) }
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
    
    #[derive( Clone, Debug, Default )]
    pub struct CommandOptions
    {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }

    pub fn split_tokens_by_pipes( tokens: &[Token] ) -> Vec<Tokens>
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
                cmds.push( cmd.clone() );
                cmd = Vec::new();
            } else { cmd.push(token.clone()); }
        }
        
        if cmd.is_empty() { return Vec::new(); }
        
        cmds.push(cmd.clone());
        cmds
    }
    /*
    fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String>
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter()
        {
            if !sep.is_empty() || !contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") { break; }

            for cap in re.captures_iter(text)
            {
                let name = cap[1].to_string();
                let value = parsers::line::unquote(&cap[2]);
                envs.insert(name, value);
            }

            n += 1;
        }

        if n > 0 { tokens.drain(0..n); }
        
        envs
    }
    */

    impl CommandLine
    {
        /*
        pub fn from_line( line: &str, sh: &mut shell::Shell ) -> Result<CommandLine, String>
        {
            let linfo = parsers::line::parse( line );
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

            Ok(CommandLine
            {
                line: line.to_string(),
                commands,
                envs,
                background,
            })
        }
        */

        pub fn is_empty(&self) -> bool { self.commands.is_empty() }

        pub fn with_pipeline(&self) -> bool { self.commands.len() > 1 }

        pub fn is_single_and_builtin(&self) -> bool { self.commands.len() == 1 && self.commands[0].is_builtin() }
    }
    /// A 128-bit (16 byte) buffer containing the UUID.
    pub type Bytes = [ u8; 16 ];
    /// The version of the UUID, denoting the generating algorithm.
    #[repr(u8)] #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq)]
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
    #[repr(u8)] #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Variant
    {
        /// Reserved by the NCS for backward compatibility.
        NCS = 0u8,
        /// As described in the RFC 9562 Specification (default).
        /// (for backward compatibility it is not yet renamed)
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
        ([ 0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8, ]);
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
            let d4: &[u8; 8] = TryInto::try_into(&self.as_bytes()[8..16]).unwrap();
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
        #[inline] pub const fn as_bytes(&self) -> &Bytes { &self.0 }
        /// Consumes self and returns the underlying byte value of the UUID.
        #[inline] pub const fn into_bytes(self) -> Bytes { self.0 }
        /// Returns the bytes of the UUID in little-endian order.
        pub const fn to_bytes_le(&self) -> Bytes
        {
            [
                self.0[3], self.0[2], self.0[1], self.0[0], self.0[5], self.0[4], self.0[7], self.0[6],
                self.0[8], self.0[9], self.0[10], self.0[11], self.0[12], self.0[13], self.0[14], self.0[15],
            ]
        }
        /// Tests if the UUID is nil (all zeros).
        pub const fn is_nil(&self) -> bool { self.as_u128() == u128::MIN }
        /// Tests if the UUID is max (all ones).
        pub const fn is_max(&self) -> bool { self.as_u128() == u128::MAX }
        /// A buffer that can be used for `encode_...` calls, that is
        /// guaranteed to be long enough for any of the format adapters.
        pub const fn encode_buffer() -> [u8; fmt::Urn::LENGTH] { [0; fmt::Urn::LENGTH] }
        /// Return the timestamp in a version-agnostic [`Timestamp`].
        pub const fn get_timestamp(&self) -> Option<Timestamp>
        {
            match self.get_version()
            {
                Some(Version::Mac) =>
                {
                    let (ticks, counter) = timestamp::decode_gregorian_timestamp(self);
                    Some(Timestamp::from_gregorian(ticks, counter))
                }
                Some(Version::SortMac) =>
                {
                    let (ticks, counter) = timestamp::decode_sorted_gregorian_timestamp(self);
                    Some(Timestamp::from_gregorian(ticks, counter))
                }
                Some(Version::SortRand) =>
                {
                    let millis = timestamp::decode_unix_timestamp_millis(self);
                    let seconds = millis / 1000;
                    let nanos = ((millis % 1000) * 1_000_000) as u32;
                    Some(Timestamp::from_unix_time(seconds, nanos, 0, 0))
                }
                _ => None,
            }
        }
        /// If the UUID is the correct version (v1, or v6) this will return the
        /// node value as a 6-byte array.
        pub const fn get_node_id(&self) -> Option<[u8; 6]>
        {
            match self.get_version()
            {
                Some(Version::Mac) | Some(Version::SortMac) =>
                {
                    let mut node_id = [0; 6];
                    node_id[0] = self.0[10];
                    node_id[1] = self.0[11];
                    node_id[2] = self.0[12];
                    node_id[3] = self.0[13];
                    node_id[4] = self.0[14];
                    node_id[5] = self.0[15];
                    Some(node_id)
                }
                _ => None,
            }
        }
    }

    impl Hash for Uuid
    {
        fn hash<H: Hasher>(&self, state: &mut H) { state.write(&self.0); }
    }

    impl Default for Uuid
    {
        #[inline] fn default() -> Self { Uuid::nil() }
    }

    impl AsRef<Uuid> for Uuid
    {
        #[inline] fn as_ref(&self) -> &Uuid { self }
    }

    impl AsRef<[u8]> for Uuid
    {
        #[inline] fn as_ref(&self) -> &[u8] { &self.0 }
    }
    
    impl From<Uuid> for Vec<u8>
    {
        fn from(value: Uuid) -> Self { value.0.to_vec() }
    }
    
    impl TryFrom<Vec<u8>> for Uuid
    {
        type Error = Error;
        fn try_from( value:Vec<u8> ) -> Result<Self, Self::Error> { Uuid::from_slice(&value) }
    }
    
    #[repr(transparent)] #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub struct Hyphenated(Uuid);
    /// Format a [`Uuid`] as a simple string, like `67e5504410b1426f9247bb680e5fe0c8`.
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Simple(Uuid);
    /// Format a [`Uuid`] as a URN string, like `urn:uuid:67e55044-10b1-426f-9247-bb680e5fe0c8`.
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Urn(Uuid);
    /// Format a [`Uuid`] as a braced hyphenated string, like `{67e55044-10b1-426f-9247-bb680e5fe0c8}`.
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Braced(Uuid);
}

pub mod regex
{
    //! Regular Expression Implementation
    pub use ::re::{ * };

    pub fn find_first_group(ptn: &str, text: &str) -> Option<String>
    {
        let re = match Regex::new(ptn)
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

    pub fn contains(text: &str, ptn: &str) -> bool
    {
        let re = match Regex::new(ptn)
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
        let re = Regex::new(ptn).unwrap();
        let result = re.replace_all(text, ptn_to);
        result.to_string()
    }
}

pub mod shell
{
    //! Simple Shell Interface
    use ::
    {
        collections::{ HashMap },
        primitive::{ Uuid },
        *,
    };

    #[derive( Clone, Debug )]
    pub struct Shell
    {
        pub jobs: HashMap<i32, primitive::Job>,
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
            let current_dir = get::current_directory();
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
    }
}

pub mod str
{
    pub use std::str::{ * };
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod vec
{
    pub use std::vec::{ * };
}

pub unsafe fn domain()
{
    unsafe
    {   
        env::initialize_paths();
        let mut shell = ::shell::Shell::new();

        println!(r#"::"#);
        /*
        loop
        {

        }*/
        println!(r#"::"#);
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
// 2149
