#![allow
(
    non_camel_case_types,
    stable_features,
    unknown_lints,
    unused_assignments,
    unused_imports,
    unused_macros,
    unused_variables,
)]
/**/
#[macro_use] extern crate lazy_static;
/**/
extern crate fnv;
//extern crate libc;
extern crate memchr;
extern crate nix;
extern crate num_integer;
extern crate num_traits;
extern crate regex as re;
extern crate time as timed;
extern crate unicode_normalization;
extern crate unicode_width;

#[macro_use] pub mod macros
{
    use ::
    {
        *,
    };    
    /// OVER::map    
    #[macro_export] macro_rules! map
    {
        { } => { ::collections::HashMap::new() };
        
        { $( $key:expr => $value:expr ),+ , } => { map!{ $( $key => $value),+ } };
        
        { $( $key:expr => $value:expr ),* } =>
        {
            {
                let mut _map = ::collections::HashMap::new();
                $( let _ = _map.insert($key, $value); )*
                _map
            }
        }
    }
    /// Given an int, creates and returns a `BigInt`.
    #[macro_export] macro_rules! int
    {
        ($int:expr) => 
        {{
            use num::big::BigInt;

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
    /// Given a list of elements, converts each element to a `Value` and returns an `Arr` containing a
    /// vector of the values.
    #[macro_export] macro_rules! arr
    {
        [] =>
        {
            ::database::arrays::Arr::from_vec(vec![]).unwrap()
        };

        [ $( $elem:expr ),+ , ] => { try_arr![ $( $elem ),+ ].unwrap() };

        [ $( $elem:expr ),+ ] => { try_arr![ $( $elem ),+ ].unwrap() };
    }
    /// Given a list of elements, converts each to a `Value` and returns an `Arr` containing a vector of the values.
    #[macro_export] macro_rules! try_arr
    {
        [ $( $elem:expr ),+ , ] => { try_arr![ $( $elem ),+ ] };

        [ $( $elem:expr ),+ ] =>
        {
            {
                $crate::arrays::Arr::from_vec(vec![ $( $elem.into() ),+ ])
            }
        };
    }
    /// Given a list of elements, converts each element to `Value`s and returns a `Tup` containing a
    /// vector of the values.
    #[macro_export] macro_rules! tup
    {
        ( $( $elem:expr ),* , ) => { tup!( $( $elem ),* ) };

        ( $( $elem:expr ),* ) =>
        {
            {
                ::database::tuples::Tup::from_vec(vec![ $( $elem.into() ),+ ])            
            }
        };
    }
    /// Given a list of field/value pairs, returns an `Obj` containing each pair.
    #[macro_export] macro_rules! obj
    {
        {} => { ::database::objects::Obj::from_map_unchecked( ::collections::HashMap::new() ) };
        { $( $field:expr => $inner:expr ),+ , } => { try_obj!{ $( $field => $inner ),+ }.unwrap() };
        { $( $field:expr => $inner:expr ),+ } => { try_obj!{ $( $field => $inner ),+ }.unwrap() };
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
                use ::database::objects::Obj;
                let mut _map = ::collections::HashMap::new();
                let mut _parent: Option<::database::values::Value> = None;
                $(
                    if $field == "^" { _parent = Some($inner.into()); }
                    else { _map.insert($field.into(), $inner.into()); }
                )*

                match _parent
                {
                    Some(parent) => match parent.get_obj()
                    {
                        Ok(parent) => Obj::from_map_with_parent(_map, parent),
                        e @ Err(_) => e,
                    }

                    None => Obj::from_map(_map),
                }
            }
        };
    }
}

pub mod alloc
{
    pub use std::alloc::{ * };
}

pub mod arch
{
    pub use std::arch::{ x86_64::{ * }, * };
}

pub mod borrow
{
    pub use std::borrow::{ * };
}

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod cell
{
   pub use std::cell::{ * };
}

pub mod char
{
    //! Character stream used for parsing.
    pub use std::char::{ * };
    use ::
    {
        cell::{ RefCell },
        fs::{ File },
        io::{ self, Read },
        iter::{ Peekable },
        rc::{ Rc },
        str::{ Chars },
        *,
    };

    #[derive(Clone, Debug)]
    struct Inner
    {
        file: Option<String>,
        contents: String,
        stream: Peekable<Chars<'static>>,
        line: usize,
        col: usize,
    }

    #[derive(Clone, Debug)]
    pub struct CharStream
    {
        inner: Rc<RefCell<Inner>>,
    }

    impl CharStream
    {
        pub fn from_file(path: &str) -> io::Result<CharStream>
        {
            let mut file = File::open(path)?;

            let len = file.metadata()?.len();
            let mut contents = String::with_capacity(len as usize);

            file.read_to_string(&mut contents)?;

            Self::from_string_impl(Some(String::from(path)), contents)
        }

        pub fn from_string(contents: String) -> io::Result<CharStream> {
            Self::from_string_impl(None, contents)
        }

        fn from_string_impl(file: Option<String>, contents: String) -> io::Result<CharStream>
        {
            let chars: Chars = unsafe { mem::transmute(contents.chars()) };
            let stream = chars.peekable();

            Ok(CharStream
            {
                inner: Rc::new(RefCell::new(Inner
                {
                    file,
                    contents,
                    stream,
                    line: 1,
                    col: 1,
                })),
            })
        }

        pub fn peek(&self) -> Option<char>
        {
            let mut inner = self.inner.borrow_mut();
            let opt = inner.stream.peek();
            match opt
            {
                Some(ch) => Some(*ch),
                None => None,
            }
        }

        pub fn file(&self) -> Option<String>
        {
            let inner = self.inner.borrow();
            inner.file.clone()
        }

        pub fn line(&self) -> usize
        {
            let inner = self.inner.borrow();
            inner.line
        }

        pub fn col(&self) -> usize
        {
            let inner = self.inner.borrow();
            inner.col
        }

        fn set_line(&mut self, value: usize)
        {
            let mut inner = self.inner.borrow_mut();
            inner.line = value;
        }

        fn set_col(&mut self, value: usize)
        {
            let mut inner = self.inner.borrow_mut();
            inner.col = value;
        }
    }

    impl Iterator for CharStream
    {
        type Item = char;

        fn next(&mut self) -> Option<Self::Item>
        {
            let opt =
            {
                let mut inner = self.inner.borrow_mut();
                inner.stream.next()
            };

            match opt
            {
                Some(ch) =>
                {
                    if ch == '\n' {
                        let line = self.line();
                        self.set_line(line + 1);
                        self.set_col(1);
                    } else {
                        let col = self.col();
                        self.set_col(col + 1);
                    }
                    Some(ch)
                }
                None => None,
            }
        }
    }
    /*
    pub fn format_char(...) -> String */
    pub fn format( ch:char ) -> String
    {
        match ch
        {
            '\n' => String::from("\\n"),
            ch => format!("{}", ch),
        }
    }
    /*
    pub fn get_escape_char(...) -> Option<char> */
    /// If `ch` preceded by a backslash together form an escape character, then return this char.
    pub fn get_escape(ch: char) -> Option<char>
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

pub mod cmp
{
    pub use std::cmp::{ * };
}

pub mod collections
{
    pub use std::collections::{ * };
}
/*
terminfo*/
pub mod common
{
    use ::
    {
        *,
    };
    
    extern "C" {}

    mod error
    {
        use ::
        {
            *,
        };
        
        #[derive(Debug)]
        pub enum Error 
        {
            /// IO error.
            Io(io::Error),
            /// Database not found.
            NotFound,
            /// Parsing error.
            Parse,
            /// Expansion error.
            Expand(Expand),
        }

        #[derive(Eq, PartialEq, Copy, Clone, Debug)]
        pub enum Expand 
        {
            /// The expansion string is invalid.
            Invalid,
            /// There was a type mismatch while expanding.
            TypeMismatch,
            /// The stack underflowed while expanding.
            StackUnderflow,
        }

        pub type Result<T> = ::result::Result<T, Error>;

        impl From<io::Error> for Error 
        
        {
            fn from(value: io::Error) -> Self 
            {
                Error::Io(value)
            }
        }

        impl From<Expand> for Error 
        
        {
            fn from(value: Expand) -> Self 
            {
                Error::Expand(value)
            }
        }

        impl fmt::Display for Error 
        
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> ::result::Result<(), fmt::Error> 
            {
                match *self 
                {
                    Error::Io(ref err) => err.fmt(f),

                    Error::NotFound => f.write_str("Capability database not found."),

                    Error::Parse => f.write_str("Failed to parse capability database."),

                    Error::Expand(ref err) => match *err
                    {
                        Expand::Invalid => f.write_str("The expansion string is invalid."),
                        Expand::StackUnderflow => f.write_str("Not enough elements on the stack."),
                        Expand::TypeMismatch => f.write_str("Type mismatch."),
                    },
                }
            }
        }

        impl error::Error for Error {}
    }
    pub use self::error::{Error, Result};
    /// String capability expansion.
    #[macro_use] pub mod expand
    {
        use ::
        {
            common::
            { 
                error::{ self },
                parser::expansion::{ * },
            },
            io::{ BufWriter, Write },
            *,
        };
        /// Expand a parametrized string.
        #[macro_export] macro_rules! expand 
        {
            ($value:expr) => (
                $crate::expand!($value;)
            );

            ($value:expr => $context:expr) => (
                $crate::expand!($value => $context;)
            );

            ($value:expr; $($item:expr),*) => (
                $crate::expand!($value => &mut ::default::Default::default(); $($item),*)
            );

            ($value:expr => $context:expr; $($item:expr),*) => ({
                let mut output = ::vec::Vec::new();

                $crate::expand!(&mut output, $value => $context; $($item),*).map(|()| output)
            });

            ($output:expr, $value:expr) => (
                $crate::expand!($output, $value;)
            );

            ($output:expr, $value:expr => $context:expr) => (
                $crate::expand!($output, $value => $context;)
            );

            ($output:expr, $value:expr; $($item:expr),*) => (
                $crate::expand!($output, $value => &mut ::default::Default::default(); $($item),*)
            );

            ($output:expr, $value:expr => $context:expr; $($item:expr),*) => ({
                use $crate::Expand;
                $value.expand($output, &[$($item.into()),*], $context)
            })
        }

        macro_rules! from 
        {
            (number $ty:ty) => 
            {
                impl From<$ty> for Parameter 
                {
                    fn from(value: $ty) -> Self 
                    {
                        Parameter::Number(value as i32)
                    }
                }
            };

            (string ref $ty:ty) => 
            {
                impl<'a> From<&'a $ty> for Parameter 
                {
                    fn from(value: &'a $ty) -> Self 
                    {
                        Parameter::String(value.into())
                    }
                }
            };

            (string $ty:ty) => 
            {
                impl From<$ty> for Parameter 
                {
                    fn from(value: $ty) -> Self 
                    {
                        Parameter::String(value.into())
                    }
                }
            };
        }
        /// Trait for items that can be expanded.
        pub trait Expand 
        
        {
            fn expand<W: Write>(
                &self,
                output: W,
                parameters: &[Parameter],
                context: &mut Context,
            ) -> error::Result<()>;
        }
        /// An expansion parameter.
        #[derive(Eq, PartialEq, Clone, Debug)]
        pub enum Parameter 
        {
            /// A number.
            Number(i32),
            /// An ASCII string.
            String(Vec<u8>),
        }

        impl Default for Parameter 
        
        {
            fn default() -> Self 
            {
                Parameter::Number(0)
            }
        }

        from!(number bool);
        from!(number u8);
        from!(number i8);
        from!(number u16);
        from!(number i16);
        from!(number u32);
        from!(number i32);

        from!(string String);
        from!(string ref str);
        from!(string Vec<u8>);
        from!(string ref [u8]);

        /// The expansion context.
        #[derive(Eq, PartialEq, Default, Debug)]
        pub struct Context 
        {
            pub fixed: [Parameter; 26],
            pub dynamic: [Parameter; 26],
        }

        impl Expand for [u8] 
        
        {
            fn expand<W: Write>(
                &self,
                output: W,
                parameters: &[Parameter],
                context: &mut Context,
            ) -> error::Result<()> 
            {
                let mut output = BufWriter::new(output);
                let mut input = self;
                let mut params: [Parameter; 9] = Default::default();
                let mut stack = Vec::new();
                let mut conditional = false;
                let mut incremented = false;

                for (dest, source) in params.iter_mut().zip(parameters.iter()) 
                {
                    *dest = source.clone();
                }

                macro_rules! next 
                {
                    () => {
                        match parse(input) 
                        {
                            Ok((rest, item)) => 
                            {
                                input = rest;
                                item
                            }

                            Err(_) => return Err(error::Expand::Invalid.into()),
                        }
                    };
                }

                'main: while !input.is_empty() 
                {
                    match next!() {
                        Item::Conditional(Conditional::If) => 
                        {
                            conditional = true;
                        }

                        Item::Conditional(Conditional::End) if conditional => 
                        {
                            conditional = false;
                        }

                        Item::Conditional(Conditional::Then) if conditional => match stack.pop() 
                        {
                            Some(Parameter::Number(0)) => 
                            {
                                let mut level = 0;

                                while !input.is_empty() 
                                {
                                    match next!() {
                                        Item::Conditional(Conditional::End)
                                        | Item::Conditional(Conditional::Else)
                                            if level == 0 =>
                                        {
                                            continue 'main
                                        }

                                        Item::Conditional(Conditional::If) => level += 1,

                                        Item::Conditional(Conditional::End) => level -= 1,

                                        _ => (),
                                    }
                                }

                                return Err(error::Expand::Invalid.into());
                            }

                            Some(_) => (),

                            None => return Err(error::Expand::StackUnderflow.into()),
                        },

                        Item::Conditional(Conditional::Else) if conditional => 
                        {
                            let mut level = 0;

                            while !input.is_empty() 
                            {
                                match next!() {
                                    Item::Conditional(Conditional::End) if level == 0 => continue 'main,

                                    Item::Conditional(Conditional::If) => level += 1,

                                    Item::Conditional(Conditional::End) => level -= 1,

                                    _ => (),
                                }
                            }

                            return Err(error::Expand::Invalid.into());
                        }

                        Item::Conditional(..) => return Err(error::Expand::Invalid.into()),

                        Item::String(value) => output.write_all(value)?,

                        Item::Constant(Constant::Character(ch)) => 
                        {
                            stack.push(Parameter::Number(ch as i32));
                        }

                        Item::Constant(Constant::Integer(value)) => 
                        {
                            stack.push(Parameter::Number(value));
                        }

                        Item::Variable(Variable::Length) => match stack.pop() 
                        {
                            Some(Parameter::String(ref value)) => 
                            {
                                stack.push(Parameter::Number(value.len() as i32));
                            }

                            Some(_) => 
                            {
                                return Err(error::Expand::TypeMismatch.into());
                            }

                            None => 
                            {
                                return Err(error::Expand::StackUnderflow.into());
                            }
                        },

                        Item::Variable(Variable::Push(index)) => 
                        {
                            stack.push(params[index as usize].clone());
                        }

                        Item::Variable(Variable::Set(dynamic, index)) => 
                        {
                            if let Some(value) = stack.pop() 
                            {
                                if dynamic {
                                    context.dynamic[index as usize] = value.clone();
                                } else {
                                    context.fixed[index as usize] = value.clone();
                                }
                            } else {
                                return Err(error::Expand::StackUnderflow.into());
                            }
                        }

                        Item::Variable(Variable::Get(dynamic, index)) => 
                        {
                            if dynamic {
                                stack.push(context.dynamic[index as usize].clone());
                            } else {
                                stack.push(context.fixed[index as usize].clone());
                            }
                        }

                        Item::Operation(Operation::Increment) if !incremented => 
                        {
                            incremented = true;

                            if let (&Parameter::Number(x), &Parameter::Number(y)) = (&params[0], &params[1])
                            {
                                params[0] = Parameter::Number(x + 1);
                                params[1] = Parameter::Number(y + 1);
                            } else 
                            {
                                return Err(error::Expand::TypeMismatch.into());
                            }
                        }

                        Item::Operation(Operation::Increment) => (),

                        Item::Operation(Operation::Binary(operation)) => match (stack.pop(), stack.pop()) 
                        {
                            (Some(Parameter::Number(y)), Some(Parameter::Number(x))) => 
                            {
                                stack.push(Parameter::Number(match operation 
                                {
                                    Binary::Add => x + y,
                                    Binary::Subtract => x - y,
                                    Binary::Multiply => x * y,
                                    Binary::Divide => 
                                    {
                                        if y != 0 
                                        {
                                            x / y
                                        } else 
                                        {
                                            0
                                        }
                                    }
                                    Binary::Remainder => 
                                    {
                                        if y != 0 
                                        {
                                            x % y
                                        } else 
                                        {
                                            0
                                        }
                                    }

                                    Binary::AND => x & y,
                                    Binary::OR => x | y,
                                    Binary::XOR => x ^ y,

                                    Binary::And => (x != 0 && y != 0) as i32,
                                    Binary::Or => (x != 0 || y != 0) as i32,

                                    Binary::Equal => (x == y) as i32,
                                    Binary::Greater => (x > y) as i32,
                                    Binary::Lesser => (x < y) as i32,
                                }))
                            }

                            (Some(_), Some(_)) => return Err(error::Expand::TypeMismatch.into()),

                            _ => return Err(error::Expand::StackUnderflow.into()),
                        },

                        Item::Operation(Operation::Unary(operation)) => match stack.pop() 
                        {
                            Some(Parameter::Number(x)) => stack.push(Parameter::Number(match operation 
                            {
                                Unary::Not => (x != 0) as i32,
                                Unary::NOT => !x,
                            })),

                            Some(_) => return Err(error::Expand::TypeMismatch.into()),

                            _ => return Err(error::Expand::StackUnderflow.into()),
                        },

                        Item::Print(p) => 
                        {
                            /// Calculate the length of a formatted number.
                            fn length(value: i32, p: &Print) -> usize 
                            {
                                let digits = match p.format {
                                    Format::Dec => (value as f32).abs().log(10.0).floor() as usize + 1,

                                    Format::Oct => (value as f32).abs().log(8.0).floor() as usize + 1,

                                    Format::Hex | Format::HEX => 
                                    {
                                        (value as f32).abs().log(16.0).floor() as usize + 1
                                    }

                                    _ => unreachable!(),
                                };

                                let mut length = digits;

                                // Add the minimum number of digits.
                                if p.flags.precision > digits 
                                {
                                    length += p.flags.precision - digits;
                                }

                                // Add the sign if present.
                                if p.format == Format::Dec && (value < 0 || p.flags.sign) 
                                {
                                    length += 1;
                                }

                                // Add the alternate representation.
                                if p.flags.alternate 
                                {
                                    match p.format 
                                    {
                                        Format::Hex | Format::HEX => length += 2,

                                        Format::Oct => length += 1,

                                        _ => (),
                                    }
                                }

                                length
                            }

                            macro_rules! w 
                            {
                                ($value:expr) => (
                                    output.write_all($value)?
                                );

                                ($($item:tt)*) => (
                                    write!(output, $($item)*)?
                                );
                            }

                            macro_rules! f 
                            {
                                (by $length:expr) => (
                                    for _ in 0 .. p.flags.width - $length 
                                    {
                                        output.write_all(if p.flags.space { b" " } else { b"0" })?;
                                    }
                                );

                                (before by $length:expr) => (
                                    if !p.flags.left && p.flags.width > $length 
                                    {
                                        f!(by $length);
                                    }
                                );

                                (after by $length:expr) => (
                                    if p.flags.left && p.flags.width > $length 
                                    {
                                        f!(by $length);
                                    }
                                );

                                (before $value:expr) => (
                                    f!(before by length($value, &p));
                                );

                                (after $value:expr) => (
                                    f!(after by length($value, &p));
                                );
                            }

                            match (p.format, stack.pop()) 
                            {
                                (Format::Str, Some(Parameter::String(ref value))) => 
                                {
                                    let mut value = &value[..];

                                    if p.flags.precision > 0 && p.flags.precision < value.len() 
                                    {
                                        value = &value[..p.flags.precision];
                                    }

                                    f!(before by value.len());
                                    w!(value);
                                    f!(after by value.len());
                                }

                                (Format::Chr, Some(Parameter::Number(value))) => 
                                {
                                    w!("{}", value as u8 as char)
                                }

                                (Format::Uni, Some(Parameter::Number(value))) => w!(
                                    "{}",
                                    char::from_u32(value as u32).ok_or(error::Expand::TypeMismatch)?
                                ),

                                (Format::Dec, Some(Parameter::Number(value))) => 
                                {
                                    f!(before value);

                                    if p.flags.sign && value >= 0 
                                    {
                                        w!(b"+");
                                    }

                                    w!("{:.1$}", value, p.flags.precision);

                                    f!(after value);
                                }

                                (Format::Oct, Some(Parameter::Number(value))) => 
                                {
                                    f!(before value);

                                    if p.flags.alternate 
                                    {
                                        w!(b"0");
                                    }

                                    w!("{:.1$o}", value, p.flags.precision);

                                    f!(after value);
                                }

                                (Format::Hex, Some(Parameter::Number(value))) => 
                                {
                                    f!(before value);

                                    if p.flags.alternate 
                                    {
                                        w!(b"0x");
                                    }

                                    w!("{:.1$x}", value, p.flags.precision);

                                    f!(after value);
                                }

                                (Format::HEX, Some(Parameter::Number(value))) => 
                                {
                                    f!(before value);

                                    if p.flags.alternate 
                                    {
                                        w!(b"0X");
                                    }

                                    w!("{:.1$X}", value, p.flags.precision);

                                    f!(after value);
                                }

                                (_, Some(_)) => return Err(error::Expand::TypeMismatch.into()),

                                (_, None) => return Err(error::Expand::StackUnderflow.into()),
                            }
                        }
                    }
                }

                Ok(())
            }
        }
    }
    pub use self::expand::Expand;
    /// Standard terminal capabilities.
    pub mod capability
    {
        //! Standard capabilities.
        use ::
        {
            borrow::{ Cow },
            common::
            {
                error::{ self },
                expand::{ Context, Expand, Parameter },
            },
            io::{ Write },
            *,
        };

        macro_rules! from 
        {
            (number $ty:ty) => 
            {
                impl From<$ty> for Value 
                {
                    fn from(value: $ty) -> Self 
                    {
                        Value::Number(value as i32)
                    }
                }
            };

            (string ref $ty:ty) => 
            {
                impl<'a> From<&'a $ty> for Value 
                {
                    fn from(value: &'a $ty) -> Self 
                    {
                        Value::String(value.into())
                    }
                }
            };

            (string $ty:ty) => 
            {
                impl From<$ty> for Value 
                {
                    fn from(value: $ty) -> Self 
                    {
                        Value::String(value.into())
                    }
                }
            };
        }

        macro_rules! define 
        {
            (boolean $ident:ident => $capability:expr) => 
            (
                #[derive(Eq, PartialEq, Copy, Clone, Debug)]
                pub struct $ident(pub bool);

                impl<'a> Capability<'a> for $ident {
                    #[inline] fn name() -> &'static str {
                        $capability
                    }

                    #[inline] fn from(value: Option<&Value>) -> Option<Self> 
                    {
                        if let Some(&Value::True) = value 
                        {
                            Some($ident(true))
                        }
                        else 
                        {
                            Some($ident(false))
                        }
                    }

                    #[inline] fn into(self) -> Option<Value> 
                    {
                        if self.0 
                        {
                            Some(Value::True)
                        }
                        else 
                        {
                            None
                        }
                    }
                }

                impl From<$ident> for bool 
                {
                    fn from(cap: $ident) -> Self 
                    {
                        cap.0
                    }
                }
            );

            (number $ident:ident => $capability:expr) => 
            (
                #[derive(Eq, PartialEq, Copy, Clone, Debug)]
                pub struct $ident(pub i32);

                impl<'a> Capability<'a> for $ident 
                {
                    #[inline] fn name() -> &'static str 
                    {
                        $capability
                    }

                    #[inline] fn from(value: Option<&Value>) -> Option<Self> 
                    {
                        if let Some(&Value::Number(value)) = value 
                        {
                            Some($ident(value))
                        }
                        else 
                        {
                            None
                        }
                    }

                    #[inline] fn into(self) -> Option<Value> 
                    {
                        Some(Value::Number(self.0))
                    }
                }

                impl From<$ident> for i32 
                {
                    fn from(cap: $ident) -> Self 
                    {
                        cap.0
                    }
                }
            );

            (string define $ident:ident => $capability:expr) => 
            (
                #[derive(Eq, PartialEq, Clone, Debug)]
                pub struct $ident<'a>(Cow<'a, [u8]>);

                impl<'a> Capability<'a> for $ident<'a> 
                {
                    #[inline] fn name() -> &'static str 
                    {
                        $capability
                    }

                    #[inline] fn from(value: Option<&'a Value>) -> Option<$ident<'a>> 
                    {
                        if let Some(&Value::String(ref value)) = value 
                        {
                            Some($ident(Cow::Borrowed(value)))
                        }
                        else { None }
                    }

                    #[inline] fn into(self) -> Option<Value> 
                    {
                        Some(Value::String(match self.0 
                        {
                            Cow::Borrowed(value) => value.into(),
                            Cow::Owned(value) => value,
                        }))
                    }
                }

                impl<'a, T: AsRef<&'a [u8]>> From<T> for $ident<'a> 
                {
                    #[inline] fn from(value: T) -> Self 
                    {
                        $ident(Cow::Borrowed(value.as_ref()))
                    }
                }

                impl<'a> AsRef<[u8]> for $ident<'a> 
                {
                    #[inline] fn as_ref(&self) -> &[u8] 
                    {
                        &self.0
                    }
                }

                impl<'a> $ident<'a> 
                {
                    /// Begin expanding the capability.
                    #[inline] pub fn expand(&self) -> Expansion<$ident> 
                    {
                        Expansion
                        {
                            string:  self,
                            params:  Default::default(),
                            context: None,
                        }
                    }
                }
            );

            (string $ident:ident => $capability:expr) => 
            (
                define!(string define $ident => $capability);
            );

            (string $ident:ident => $capability:expr; $($rest:tt)+) => 
            (
                define!(string define $ident => $capability);
                define!(string parameters $ident; $($rest)+);
                define!(string builder $ident; 0, $($rest)+, );
            );

            (string parameters $ident:ident; $($name:ident : $ty:ty),+) => 
            (
                impl<'a> Expansion<'a, $ident<'a>> 
                {
                    /// Pass all expansion parameters at once.
                    #[inline] pub fn parameters(mut self, $($name: $ty),*) -> Self 
                    {
                        let mut index = 0;

                        $({
                            self.params[index]  = $name.into();
                            index              += 1;
                        })*;

                        self
                    }
                }
            );

            (string builder $ident:ident; $index:expr, ) => ();

            (string builder $ident:ident; $index:expr, $name:ident : u8, $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : u8);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : i8, $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : i8);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : u16, $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : u16);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : i16 $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : i16);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : u32, $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : u32);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : i32, $($rest:tt)*) => 
            (
                define!(string builder direct $ident; $index, $name : i32);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder $ident:ident; $index:expr, $name:ident : $ty:ty, $($rest:tt)*) => 
            (
                define!(string builder into $ident; $index, $name : $ty);
                define!(string builder $ident; $index + 1, $($rest)*);
            );

            (string builder direct $ident:ident; $index:expr, $name:ident : $ty:ty) => 
            (
                impl<'a> Expansion<'a, $ident<'a>> 
                {
                    /// Set the given parameter.
                    #[inline] pub fn $name(mut self, value: $ty) -> Self 
                    {
                        self.params[$index] = value.into();
                        self
                    }
                }
            );

            (string builder into $ident:ident; $index:expr, $name:ident : $ty:ty) => 
            (
                impl<'a> Expansion<'a, $ident<'a>> 
                {
                    /// Set the given parameter.
                    #[inline] pub fn $name<T: Into<$ty>>(mut self, value: T) -> Self 
                    {
                        self.params[$index] = value.into().into();
                        self
                    }
                }
            );
        }
        /// A trait for any object that will represent a terminal capability.
        pub trait Capability<'a>: Sized 
        {
            /// Returns the name of the capability in its long form.
            fn name() -> &'static str;
            /// Parse the capability from its raw value.
            fn from(value: Option<&'a Value>) -> Option<Self>;
            /// Convert the capability into its raw value.
            fn into(self) -> Option<Value>;
        }
        /// Possible value types for capabilities.
        #[derive(Eq, PartialEq, Clone, Debug)]
        pub enum Value 
        {
            /// A boolean.
            True,
            /// A number.
            Number(i32),
            /// An ASCII string requiring expansion.
            String(Vec<u8>),
        }
        /// Expansion helper struct.
        #[derive(Debug)]
        pub struct Expansion<'a, T: 'a + AsRef<[u8]>> 
        {
            string: &'a T,
            params: [Parameter; 9],
            context: Option<&'a mut Context>,
        }

        impl<'a, T: AsRef<[u8]>> Expansion<'a, T> 
        {
            /// Expand using the given context.
            pub fn with<'c: 'a>(mut self, context: &'c mut Context) -> Self 
            {
                self.context = Some(context);
                self
            }
            /// Expand to the given output.
            pub fn to<W: Write>(self, output: W) -> error::Result<()> 
            {
                self.string.as_ref().expand(
                    output,
                    &self.params,
                    self.context.unwrap_or(&mut Default::default()),
                )
            }
            /// Expand into a vector.
            pub fn to_vec(self) -> error::Result<Vec<u8>> 
            {
                let mut result = Vec::with_capacity(self.string.as_ref().len());
                self.to(&mut result)?;
                Ok(result)
            }
        }

        impl From<()> for Value 
        
        {
            fn from(_: ()) -> Self {
                Value::True
            }
        }

        from!(number u8);
        from!(number i8);
        from!(number u16);
        from!(number i16);
        from!(number u32);
        from!(number i32);

        from!(string String);
        from!(string ref str);
        from!(string Vec<u8>);
        from!(string ref [u8]);

        define!(boolean AutoLeftMargin => "auto_left_margin");
        define!(boolean AutoRightMargin => "auto_right_margin");
        define!(boolean NoEscCtlc => "no_esc_ctlc");
        define!(boolean CeolStandoutGlitch => "ceol_standout_glitch");
        define!(boolean EatNewlineGlitch => "eat_newline_glitch");
        define!(boolean EraseOverstrike => "erase_overstrike");
        define!(boolean GenericType => "generic_type");
        define!(boolean HardCopy => "hard_copy");
        define!(boolean HasMetaKey => "has_meta_key");
        define!(boolean HasStatusLine => "has_status_line");
        define!(boolean InsertNullGlitch => "insert_null_glitch");
        define!(boolean MemoryAbove => "memory_above");
        define!(boolean MemoryBelow => "memory_below");
        define!(boolean MoveInsertMode => "move_insert_mode");
        define!(boolean MoveStandoutMode => "move_standout_mode");
        define!(boolean OverStrike => "over_strike");
        define!(boolean StatusLineEscOk => "status_line_esc_ok");
        define!(boolean DestTabsMagicSmso => "dest_tabs_magic_smso");
        define!(boolean TildeGlitch => "tilde_glitch");
        define!(boolean TransparentUnderline => "transparent_underline");
        define!(boolean XonXoff => "xon_xoff");
        define!(boolean NeedsXonXoff => "needs_xon_xoff");
        define!(boolean PrtrSilent => "prtr_silent");
        define!(boolean HardCursor => "hard_cursor");
        define!(boolean NonRevRmcup => "non_rev_rmcup");
        define!(boolean NoPadChar => "no_pad_char");
        define!(boolean NonDestScrollRegion => "non_dest_scroll_region");
        define!(boolean CanChange => "can_change");
        define!(boolean BackColorErase => "back_color_erase");
        define!(boolean HueLightnessSaturation => "hue_lightness_saturation");
        define!(boolean ColAddrGlitch => "col_addr_glitch");
        define!(boolean CrCancelsMicroMode => "cr_cancels_micro_mode");
        define!(boolean HasPrintWheel => "has_print_wheel");
        define!(boolean RowAddrGlitch => "row_addr_glitch");
        define!(boolean SemiAutoRightMargin => "semi_auto_right_margin");
        define!(boolean CpiChangesRes => "cpi_changes_res");
        define!(boolean LpiChangesRes => "lpi_changes_res");
        define!(boolean BackspacesWithBs => "backspaces_with_bs");
        define!(boolean CrtNoScrolling => "crt_no_scrolling");
        define!(boolean NoCorrectlyWorkingCr => "no_correctly_working_cr");
        define!(boolean GnuHasMetaKey => "gnu_has_meta_key");
        define!(boolean LinefeedIsNewline => "linefeed_is_newline");
        define!(boolean HasHardwareTabs => "has_hardware_tabs");
        define!(boolean ReturnDoesClrEol => "return_does_clr_eol");

        define!(number Columns => "columns");
        define!(number InitTabs => "init_tabs");
        define!(number Lines => "lines");
        define!(number LinesOfMemory => "lines_of_memory");
        define!(number MagicCookieGlitch => "magic_cookie_glitch");
        define!(number PaddingBaudRate => "padding_baud_rate");
        define!(number VirtualTerminal => "virtual_terminal");
        define!(number WidthStatusLine => "width_status_line");
        define!(number NumLabels => "num_labels");
        define!(number LabelHeight => "label_height");
        define!(number LabelWidth => "label_width");
        define!(number MaxAttributes => "max_attributes");
        define!(number MaximumWindows => "maximum_windows");
        define!(number MaxColors => "max_colors");
        define!(number MaxPairs => "max_pairs");
        define!(number NoColorVideo => "no_color_video");
        define!(number BufferCapacity => "buffer_capacity");
        define!(number DotVertSpacing => "dot_vert_spacing");
        define!(number DotHorzSpacing => "dot_horz_spacing");
        define!(number MaxMicroAddress => "max_micro_address");
        define!(number MaxMicroJump => "max_micro_jump");
        define!(number MicroColSize => "micro_col_size");
        define!(number MicroLineSize => "micro_line_size");
        define!(number NumberOfPins => "number_of_pins");
        define!(number OutputResChar => "output_res_char");
        define!(number OutputResLine => "output_res_line");
        define!(number OutputResHorzInch => "output_res_horz_inch");
        define!(number OutputResVertInch => "output_res_vert_inch");
        define!(number PrintRate => "print_rate");
        define!(number WideCharSize => "wide_char_size");
        define!(number Buttons => "buttons");
        define!(number BitImageEntwining => "bit_image_entwining");
        define!(number BitImageType => "bit_image_type");
        define!(number MagicCookieGlitchUl => "magic_cookie_glitch_ul");
        define!(number CarriageReturnDelay => "carriage_return_delay");
        define!(number NewLineDelay => "new_line_delay");
        define!(number BackspaceDelay => "backspace_delay");
        define!(number HorizontalTabDelay => "horizontal_tab_delay");
        define!(number NumberOfFunctionKeys => "number_of_function_keys");

        define!(string BackTab => "back_tab");
        define!(string Bell => "bell");
        define!(string CarriageReturn => "carriage_return");
        define!(string ClearAllTabs => "clear_all_tabs");
        define!(string ClearScreen => "clear_screen");
        define!(string ClrEol => "clr_eol");
        define!(string ClrEos => "clr_eos");
        define!(string CommandCharacter => "command_character");
        define!(string CursorDown => "cursor_down");
        define!(string CursorHome => "cursor_home");
        define!(string CursorInvisible => "cursor_invisible");
        define!(string CursorLeft => "cursor_left");
        define!(string CursorMemAddress => "cursor_mem_address");
        define!(string CursorNormal => "cursor_normal");
        define!(string CursorRight => "cursor_right");
        define!(string CursorToLl => "cursor_to_ll");
        define!(string CursorUp => "cursor_up");
        define!(string CursorVisible => "cursor_visible");
        define!(string DeleteCharacter => "delete_character");
        define!(string DeleteLine => "delete_line");
        define!(string DisStatusLine => "dis_status_line");
        define!(string DownHalfLine => "down_half_line");
        define!(string EnterAltCharsetMode => "enter_alt_charset_mode");
        define!(string EnterBlinkMode => "enter_blink_mode");
        define!(string EnterBoldMode => "enter_bold_mode");
        define!(string EnterCaMode => "enter_ca_mode");
        define!(string EnterDeleteMode => "enter_delete_mode");
        define!(string EnterDimMode => "enter_dim_mode");
        define!(string EnterInsertMode => "enter_insert_mode");
        define!(string EnterSecureMode => "enter_secure_mode");
        define!(string EnterProtectedMode => "enter_protected_mode");
        define!(string EnterReverseMode => "enter_reverse_mode");
        define!(string EnterStandoutMode => "enter_standout_mode");
        define!(string EnterUnderlineMode => "enter_underline_mode");
        define!(string ExitAltCharsetMode => "exit_alt_charset_mode");
        define!(string ExitAttributeMode => "exit_attribute_mode");
        define!(string ExitCaMode => "exit_ca_mode");
        define!(string ExitDeleteMode => "exit_delete_mode");
        define!(string ExitInsertMode => "exit_insert_mode");
        define!(string ExitStandoutMode => "exit_standout_mode");
        define!(string ExitUnderlineMode => "exit_underline_mode");
        define!(string FlashScreen => "flash_screen");
        define!(string FormFeed => "form_feed");
        define!(string FromStatusLine => "from_status_line");
        define!(string Init1String => "init_1string");
        define!(string Init2String => "init_2string");
        define!(string Init3String => "init_3string");
        define!(string InitFile => "init_file");
        define!(string InsertCharacter => "insert_character");
        define!(string InsertLine => "insert_line");
        define!(string InsertPadding => "insert_padding");
        define!(string KeyBackspace => "key_backspace");
        define!(string KeyCATab => "key_catab");
        define!(string KeyClear => "key_clear");
        define!(string KeyCTab => "key_ctab");
        define!(string KeyDc => "key_dc");
        define!(string KeyDl => "key_dl");
        define!(string KeyDown => "key_down");
        define!(string KeyEic => "key_eic");
        define!(string KeyEol => "key_eol");
        define!(string KeyEos => "key_eos");
        define!(string KeyF0 => "key_f0");
        define!(string KeyF1 => "key_f1");
        define!(string KeyF10 => "key_f10");
        define!(string KeyF2 => "key_f2");
        define!(string KeyF3 => "key_f3");
        define!(string KeyF4 => "key_f4");
        define!(string KeyF5 => "key_f5");
        define!(string KeyF6 => "key_f6");
        define!(string KeyF7 => "key_f7");
        define!(string KeyF8 => "key_f8");
        define!(string KeyF9 => "key_f9");
        define!(string KeyHome => "key_home");
        define!(string KeyIc => "key_ic");
        define!(string KeyIl => "key_il");
        define!(string KeyLeft => "key_left");
        define!(string KeyLl => "key_ll");
        define!(string KeyNPage => "key_npage");
        define!(string KeyPPage => "key_ppage");
        define!(string KeyRight => "key_right");
        define!(string KeySf => "key_sf");
        define!(string KeySr => "key_sr");
        define!(string KeySTab => "key_stab");
        define!(string KeyUp => "key_up");
        define!(string KeypadLocal => "keypad_local");
        define!(string KeypadXmit => "keypad_xmit");
        define!(string LabF0 => "lab_f0");
        define!(string LabF1 => "lab_f1");
        define!(string LabF10 => "lab_f10");
        define!(string LabF2 => "lab_f2");
        define!(string LabF3 => "lab_f3");
        define!(string LabF4 => "lab_f4");
        define!(string LabF5 => "lab_f5");
        define!(string LabF6 => "lab_f6");
        define!(string LabF7 => "lab_f7");
        define!(string LabF8 => "lab_f8");
        define!(string LabF9 => "lab_f9");
        define!(string MetaOff => "meta_off");
        define!(string MetaOn => "meta_on");
        define!(string Newline => "newline");
        define!(string PadChar => "pad_char");
        define!(string PKeyKey => "pkey_key");
        define!(string PKeyLocal => "pkey_local");
        define!(string PKeyXmit => "pkey_xmit");
        define!(string PrintScreen => "print_screen");
        define!(string PrtrOff => "prtr_off");
        define!(string PrtrOn => "prtr_on");
        define!(string RepeatChar => "repeat_char");
        define!(string Reset1String => "reset_1string");
        define!(string Reset2String => "reset_2string");
        define!(string Reset3String => "reset_3string");
        define!(string ResetFile => "reset_file");
        define!(string RestoreCursor => "restore_cursor");
        define!(string SaveCursor => "save_cursor");
        define!(string ScrollForward => "scroll_forward");
        define!(string ScrollReverse => "scroll_reverse");
        define!(string SetTab => "set_tab");
        define!(string SetWindow => "set_window");
        define!(string Tab => "tab");
        define!(string ToStatusLine => "to_status_line");
        define!(string UnderlineChar => "underline_char");
        define!(string UpHalfLine => "up_half_line");
        define!(string InitProg => "init_prog");
        define!(string KeyA1 => "key_a1");
        define!(string KeyA3 => "key_a3");
        define!(string KeyB2 => "key_b2");
        define!(string KeyC1 => "key_c1");
        define!(string KeyC3 => "key_c3");
        define!(string PrtrNon => "prtr_non");
        define!(string CharPadding => "char_padding");
        define!(string AcsChars => "acs_chars");
        define!(string PlabNorm => "plab_norm");
        define!(string KeyBTab => "key_btab");
        define!(string EnterXonMode => "enter_xon_mode");
        define!(string ExitXonMode => "exit_xon_mode");
        define!(string EnterAmMode => "enter_am_mode");
        define!(string ExitAmMode => "exit_am_mode");
        define!(string XonCharacter => "xon_character");
        define!(string XoffCharacter => "xoff_character");
        define!(string EnaAcs => "ena_acs");
        define!(string LabelOn => "label_on");
        define!(string LabelOff => "label_off");
        define!(string KeyBeg => "key_beg");
        define!(string KeyCancel => "key_cancel");
        define!(string KeyClose => "key_close");
        define!(string KeyCommand => "key_command");
        define!(string KeyCopy => "key_copy");
        define!(string KeyCreate => "key_create");
        define!(string KeyEnd => "key_end");
        define!(string KeyEnter => "key_enter");
        define!(string KeyExit => "key_exit");
        define!(string KeyFind => "key_find");
        define!(string KeyHelp => "key_help");
        define!(string KeyMark => "key_mark");
        define!(string KeyMessage => "key_message");
        define!(string KeyMove => "key_move");
        define!(string KeyNext => "key_next");
        define!(string KeyOpen => "key_open");
        define!(string KeyOptions => "key_options");
        define!(string KeyPrevious => "key_previous");
        define!(string KeyPrint => "key_print");
        define!(string KeyRedo => "key_redo");
        define!(string KeyReference => "key_reference");
        define!(string KeyRefresh => "key_refresh");
        define!(string KeyReplace => "key_replace");
        define!(string KeyRestart => "key_restart");
        define!(string KeyResume => "key_resume");
        define!(string KeySave => "key_save");
        define!(string KeySuspend => "key_suspend");
        define!(string KeyUndo => "key_undo");
        define!(string KeySBeg => "key_sbeg");
        define!(string KeySCancel => "key_scancel");
        define!(string KeySCommand => "key_scommand");
        define!(string KeySCopy => "key_scopy");
        define!(string KeySCreate => "key_screate");
        define!(string KeySDc => "key_sdc");
        define!(string KeySDl => "key_sdl");
        define!(string KeySelect => "key_select");
        define!(string KeySEnd => "key_send");
        define!(string KeySEol => "key_seol");
        define!(string KeySExit => "key_sexit");
        define!(string KeySFind => "key_sfind");
        define!(string KeySHelp => "key_shelp");
        define!(string KeySHome => "key_shome");
        define!(string KeySIc => "key_sic");
        define!(string KeySLeft => "key_sleft");
        define!(string KeySMessage => "key_smessage");
        define!(string KeySMove => "key_smove");
        define!(string KeySNext => "key_snext");
        define!(string KeySOptions => "key_soptions");
        define!(string KeySPrevious => "key_sprevious");
        define!(string KeySPrint => "key_sprint");
        define!(string KeySRedo => "key_sredo");
        define!(string KeySReplace => "key_sreplace");
        define!(string KeySRight => "key_sright");
        define!(string KeySRsume => "key_srsume");
        define!(string KeySSave => "key_ssave");
        define!(string KeySSuspend => "key_ssuspend");
        define!(string KeySUndo => "key_sundo");
        define!(string ReqForInput => "req_for_input");
        define!(string KeyF11 => "key_f11");
        define!(string KeyF12 => "key_f12");
        define!(string KeyF13 => "key_f13");
        define!(string KeyF14 => "key_f14");
        define!(string KeyF15 => "key_f15");
        define!(string KeyF16 => "key_f16");
        define!(string KeyF17 => "key_f17");
        define!(string KeyF18 => "key_f18");
        define!(string KeyF19 => "key_f19");
        define!(string KeyF20 => "key_f20");
        define!(string KeyF21 => "key_f21");
        define!(string KeyF22 => "key_f22");
        define!(string KeyF23 => "key_f23");
        define!(string KeyF24 => "key_f24");
        define!(string KeyF25 => "key_f25");
        define!(string KeyF26 => "key_f26");
        define!(string KeyF27 => "key_f27");
        define!(string KeyF28 => "key_f28");
        define!(string KeyF29 => "key_f29");
        define!(string KeyF30 => "key_f30");
        define!(string KeyF31 => "key_f31");
        define!(string KeyF32 => "key_f32");
        define!(string KeyF33 => "key_f33");
        define!(string KeyF34 => "key_f34");
        define!(string KeyF35 => "key_f35");
        define!(string KeyF36 => "key_f36");
        define!(string KeyF37 => "key_f37");
        define!(string KeyF38 => "key_f38");
        define!(string KeyF39 => "key_f39");
        define!(string KeyF40 => "key_f40");
        define!(string KeyF41 => "key_f41");
        define!(string KeyF42 => "key_f42");
        define!(string KeyF43 => "key_f43");
        define!(string KeyF44 => "key_f44");
        define!(string KeyF45 => "key_f45");
        define!(string KeyF46 => "key_f46");
        define!(string KeyF47 => "key_f47");
        define!(string KeyF48 => "key_f48");
        define!(string KeyF49 => "key_f49");
        define!(string KeyF50 => "key_f50");
        define!(string KeyF51 => "key_f51");
        define!(string KeyF52 => "key_f52");
        define!(string KeyF53 => "key_f53");
        define!(string KeyF54 => "key_f54");
        define!(string KeyF55 => "key_f55");
        define!(string KeyF56 => "key_f56");
        define!(string KeyF57 => "key_f57");
        define!(string KeyF58 => "key_f58");
        define!(string KeyF59 => "key_f59");
        define!(string KeyF60 => "key_f60");
        define!(string KeyF61 => "key_f61");
        define!(string KeyF62 => "key_f62");
        define!(string KeyF63 => "key_f63");
        define!(string ClrBol => "clr_bol");
        define!(string ClearMargins => "clear_margins");
        define!(string SetLeftMargin => "set_left_margin");
        define!(string SetRightMargin => "set_right_margin");
        define!(string LabelFormat => "label_format");
        define!(string SetClock => "set_clock");
        define!(string DisplayClock => "display_clock");
        define!(string RemoveClock => "remove_clock");
        define!(string CreateWindow => "create_window");
        define!(string GotoWindow => "goto_window");
        define!(string Hangup => "hangup");
        define!(string DialPhone => "dial_phone");
        define!(string QuickDial => "quick_dial");
        define!(string Tone => "tone");
        define!(string Pulse => "pulse");
        define!(string FlashHook => "flash_hook");
        define!(string FixedPause => "fixed_pause");
        define!(string WaitTone => "wait_tone");
        define!(string User0 => "user0");
        define!(string User1 => "user1");
        define!(string User2 => "user2");
        define!(string User3 => "user3");
        define!(string User4 => "user4");
        define!(string User5 => "user5");
        define!(string User6 => "user6");
        define!(string User7 => "user7");
        define!(string User8 => "user8");
        define!(string User9 => "user9");
        define!(string OrigPair => "orig_pair");
        define!(string OrigColors => "orig_colors");
        define!(string InitializeColor => "initialize_color");
        define!(string InitializePair => "initialize_pair");
        define!(string SetColorPair => "set_color_pair");
        define!(string ChangeCharPitch => "change_char_pitch");
        define!(string ChangeLinePitch => "change_line_pitch");
        define!(string ChangeResHorz => "change_res_horz");
        define!(string ChangeResVert => "change_res_vert");
        define!(string DefineChar => "define_char");
        define!(string EnterDoublewideMode => "enter_doublewide_mode");
        define!(string EnterDraftQuality => "enter_draft_quality");
        define!(string EnterItalicsMode => "enter_italics_mode");
        define!(string EnterLeftwardMode => "enter_leftward_mode");
        define!(string EnterMicroMode => "enter_micro_mode");
        define!(string EnterNearLetterQuality => "enter_near_letter_quality");
        define!(string EnterNormalQuality => "enter_normal_quality");
        define!(string EnterShadowMode => "enter_shadow_mode");
        define!(string EnterSubscriptMode => "enter_subscript_mode");
        define!(string EnterSuperscriptMode => "enter_superscript_mode");
        define!(string EnterUpwardMode => "enter_upward_mode");
        define!(string ExitDoublewideMode => "exit_doublewide_mode");
        define!(string ExitItalicsMode => "exit_italics_mode");
        define!(string ExitLeftwardMode => "exit_leftward_mode");
        define!(string ExitMicroMode => "exit_micro_mode");
        define!(string ExitShadowMode => "exit_shadow_mode");
        define!(string ExitSubscriptMode => "exit_subscript_mode");
        define!(string ExitSuperscriptMode => "exit_superscript_mode");
        define!(string ExitUpwardMode => "exit_upward_mode");
        define!(string MicroColumnAddress => "micro_column_address");
        define!(string MicroDown => "micro_down");
        define!(string MicroLeft => "micro_left");
        define!(string MicroRight => "micro_right");
        define!(string MicroRowAddress => "micro_row_address");
        define!(string MicroUp => "micro_up");
        define!(string OrderOfPins => "order_of_pins");
        define!(string SelectCharSet => "select_char_set");
        define!(string SetBottomMargin => "set_bottom_margin");
        define!(string SetBottomMarginParm => "set_bottom_margin_parm");
        define!(string SetLeftMarginParm => "set_left_margin_parm");
        define!(string SetRightMarginParm => "set_right_margin_parm");
        define!(string SetTopMargin => "set_top_margin");
        define!(string SetTopMarginParm => "set_top_margin_parm");
        define!(string StartBitImage => "start_bit_image");
        define!(string StartCharSetDef => "start_char_set_def");
        define!(string StopBitImage => "stop_bit_image");
        define!(string StopCharSetDef => "stop_char_set_def");
        define!(string SubscriptCharacters => "subscript_characters");
        define!(string SuperscriptCharacters => "superscript_characters");
        define!(string TheseCauseCr => "these_cause_cr");
        define!(string ZeroMotion => "zero_motion");
        define!(string CharSetNames => "char_set_names");
        define!(string KeyMouse => "key_mouse");
        define!(string MouseInfo => "mouse_info");
        define!(string ReqMousePos => "req_mouse_pos");
        define!(string GetMouse => "get_mouse");
        define!(string PkeyPlab => "pkey_plab");
        define!(string DeviceType => "device_type");
        define!(string CodeSetInit => "code_set_init");
        define!(string Set0DesSeq => "set0_des_seq");
        define!(string Set1DesSeq => "set1_des_seq");
        define!(string Set2DesSeq => "set2_des_seq");
        define!(string Set3DesSeq => "set3_des_seq");
        define!(string SetLrMargin => "set_lr_margin");
        define!(string SetTbMargin => "set_tb_margin");
        define!(string BitImageRepeat => "bit_image_repeat");
        define!(string BitImageNewline => "bit_image_newline");
        define!(string BitImageCarriageReturn => "bit_image_carriage_return");
        define!(string ColorNames => "color_names");
        define!(string DefineBitImageRegion => "define_bit_image_region");
        define!(string EndBitImageRegion => "end_bit_image_region");
        define!(string SetColorBand => "set_color_band");
        define!(string SetPageLength => "set_page_length");
        define!(string DisplayPcChar => "display_pc_char");
        define!(string EnterPcCharsetMode => "enter_pc_charset_mode");
        define!(string ExitPcCharsetMode => "exit_pc_charset_mode");
        define!(string EnterScancodeMode => "enter_scancode_mode");
        define!(string ExitScancodeMode => "exit_scancode_mode");
        define!(string PcTermOptions => "pc_term_options");
        define!(string ScancodeEscape => "scancode_escape");
        define!(string AltScancodeEsc => "alt_scancode_esc");
        define!(string EnterHorizontalHlMode => "enter_horizontal_hl_mode");
        define!(string EnterLeftHlMode => "enter_left_hl_mode");
        define!(string EnterLowHlMode => "enter_low_hl_mode");
        define!(string EnterRightHlMode => "enter_right_hl_mode");
        define!(string EnterTopHlMode => "enter_top_hl_mode");
        define!(string EnterVerticalHlMode => "enter_vertical_hl_mode");
        define!(string SetAAttributes => "set_a_attributes");
        define!(string SetPglenInch => "set_pglen_inch");
        define!(string TermcapInit2 => "termcap_init2");
        define!(string TermcapReset => "termcap_reset");
        define!(string LinefeedIfNotLf => "linefeed_if_not_lf");
        define!(string BackspaceIfNotBs => "backspace_if_not_bs");
        define!(string OtherNonFunctionKeys => "other_non_function_keys");
        define!(string ArrowKeyMap => "arrow_key_map");
        define!(string AcsULcorner => "acs_ulcorner");
        define!(string AcsLLcorner => "acs_llcorner");
        define!(string AcsURcorner => "acs_urcorner");
        define!(string AcsLRcorner => "acs_lrcorner");
        define!(string AcsLTee => "acs_ltee");
        define!(string AcsRTee => "acs_rtee");
        define!(string AcsBTee => "acs_btee");
        define!(string AcsTTee => "acs_ttee");
        define!(string AcsHLine => "acs_hline");
        define!(string AcsVLine => "acs_vline");
        define!(string AcsPlus => "acs_plus");
        define!(string MemoryLock => "memory_lock");
        define!(string MemoryUnlock => "memory_unlock");
        define!(string BoxChars1 => "box_chars_1");

        define!(string ChangeScrollRegion => "change_scroll_region";
            top:    u32,
            bottom: u32);

        define!(string ColumnAddress => "column_address";
            x: u32);

        define!(string CursorAddress => "cursor_address";
            y: u32,
            x: u32);

        define!(string EraseChars => "erase_chars";
            count: u32);

        define!(string ParmDch => "parm_dch";
            count: u32);

        define!(string ParmDeleteLine => "parm_delete_line";
            count: u32);

        define!(string ParmDownCursor => "parm_down_cursor";
            count: u32);

        define!(string ParmIch => "parm_ich";
            count: u32);

        define!(string ParmIndex => "parm_index";
            count: u32);

        define!(string ParmInsertLine => "parm_insert_line";
            count: u32);

        define!(string ParmLeftCursor => "parm_left_cursor";
            count: u32);

        define!(string ParmRightCursor => "parm_right_cursor";
            count: u32);

        define!(string ParmRindex => "parm_rindex";
            count: u32);

        define!(string ParmUpCursor => "parm_up_cursor";
            count: u32);

        define!(string ParmDownMicro => "parm_down_micro";
            count: u32);

        define!(string ParmLeftMicro => "parm_left_micro";
            count: u32);

        define!(string ParmRightMicro => "parm_right_micro";
            count: u32);

        define!(string ParmUpMicro => "parm_up_micro";
            count: u32);

        define!(string RowAddress => "row_address";
            y: u32);

        define!(string SetAttributes => "set_attributes";
            standout:    bool,
            underline:   bool,
            reverse:     bool,
            blink:       bool,
            dim:         bool,
            bold:        bool,
            invisible:   bool,
            protected:   bool,
            alt_charset: bool);

        define!(string SetAForeground => "set_a_foreground";
            color: u8);

        define!(string SetABackground => "set_a_background";
            color: u8);

        define!(string SetForeground => "set_foreground";
            color: u8);

        define!(string SetBackground => "set_background";
            color: u8);


        define!(boolean XTermTitle => "XT");
        define!(boolean BrightAttribute => "AX");
        define!(boolean XTermMouse => "XM");
        
        define!(boolean TrueColor => "Tc");

        define!(string SetClipboard => "Ms";
            selection: String,
            content:   Vec<u8>);

        define!(string SetCursorStyle => "Ss";
            kind: u8);

        define!(string ResetCursorStyle => "Se");
        
        define!(string SetTrueColorForeground => "8f";
            r: u8,
            g: u8,
            b: u8);

        define!(string SetTrueColorBackground => "8b";
            r: u8,
            g: u8,
            b: u8);

        define!(string ResetCursorColor => "Cr");

        define!(string SetCursorColor => "Cs";
            color: String);

    }
    pub use self::capability::{Capability, Value};

    mod database
    {
        use ::
        {
            collections::{ HashMap },
            common::
            {
                capability::{ Capability, Value },
                error::{ self, Error },
                names,
                parser::compiled,
            },
            fnv::{ FnvHasher },
            fs::{ self, File },
            hash::{ BuildHasherDefault},
            io::{ Read },
            path::{ Path, PathBuf },
            *,
        };
        /// A capability database.
        #[derive(Eq, PartialEq, Clone, Debug)]
        pub struct Database 
        {
            name: String,
            aliases: Vec<String>,
            description: String,
            inner: HashMap<String, Value, BuildHasherDefault<FnvHasher>>,
        }
        /// Builder for a new `Database`.
        #[derive(Default, Debug)]
        pub struct Builder 
        {
            name: Option<String>,
            aliases: Vec<String>,
            description: Option<String>,
            inner: HashMap<String, Value, BuildHasherDefault<FnvHasher>>,
        }

        impl Builder 
        {
            /// Build the database.
            pub fn build(self) -> Result<Database, ()> 
            {
                Ok(Database 
                {
                    name: self.name.ok_or(())?,
                    aliases: self.aliases,
                    description: self.description.unwrap_or_default(),
                    inner: self.inner,
                })
            }
            /// Set the terminal name.
            pub fn name<T: Into<String>>(&mut self, name: T) -> &mut Self 
            {
                self.name = Some(name.into());
                self
            }
            /// Set the terminal aliases.
            pub fn aliases<T, I>(&mut self, iter: I) -> &mut Self where
                T: Into<String>,
                I: IntoIterator<Item = T>,
            {
                self.aliases = iter.into_iter().map(|a| a.into()).collect();
                self
            }
            /// Set the terminal description.
            pub fn description<T: Into<String>>(&mut self, description: T) -> &mut Self 
            {
                self.description = Some(description.into());
                self
            }
            /// Set a capability.
            pub fn set<'a, C: Capability<'a>>(&'a mut self, value: C) -> &mut Self 
            {
                if !self.inner.contains_key(C::name()) 
                {
                    if let Some(value) = C::into(value) 
                    {
                        self.inner.insert(C::name().into(), value);
                    }
                }

                self
            }
            /// Set a raw capability.
            pub fn raw<S: AsRef<str>, V: Into<Value>>(&mut self, name: S, value: V) -> &mut Self 
            {
                /*
                let name = name.as_ref();
                let name = names::ALIASES.get(name).copied().unwrap_or(name);

                if !self.inner.contains_key(name) 
                {
                    self.inner.insert(name.into(), value.into());
                }
                */
                self
            }
        }

        impl Database 
        {
            pub fn new() -> Builder 
            {
                Builder::default()
            }
            /// Load a database from the current environment.
            pub fn from_env() -> error::Result<Self> 
            {
                if let Ok(name) = env::var("TERM") 
                {
                    Self::from_name(name)
                }
                else 
                {
                    Err(Error::NotFound)
                }
            }
            /// Load a database for the given name.
            pub fn from_name<N: AsRef<str>>(name: N) -> error::Result<Self> 
            {
                let name = name.as_ref();
                let first = name.chars().next().ok_or(Error::NotFound)?;

                // See https://manpages.debian.org/buster/ncurses-bin/terminfo.5.en.html#Fetching_Compiled_Descriptions
                let mut search = Vec::<PathBuf>::new();

                #[allow(deprecated)]
                if let Some(dir) = env::var_os("TERMINFO") 
                {
                    search.push(dir.into());
                } else if let Some(mut home) = std::env::home_dir() 
                {
                    home.push(".terminfo");
                    search.push(home);
                }

                if let Ok(dirs) = env::var("TERMINFO_DIRS") 
                {
                    for dir in dirs.split(':') {
                        search.push(dir.into());
                    }
                }

                // handle non-FHS systems like Termux
                if let Ok(prefix) = env::var("PREFIX") 
                {
                    let path = Path::new(&prefix);
                    search.push(path.join("etc/terminfo"));
                    search.push(path.join("lib/terminfo"));
                    search.push(path.join("share/terminfo"));
                }

                search.push("/etc/terminfo".into());
                search.push("/lib/terminfo".into());
                search.push("/usr/share/terminfo".into());
                search.push("/usr/local/share/terminfo".into());
                search.push("/usr/local/share/site-terminfo".into());
                search.push("/boot/system/data/terminfo".into());

                for path in search 
                {
                    if fs::metadata(&path).is_err() 
                    {
                        continue;
                    }

                    // Check standard location.
                    {
                        let mut path = path.clone();
                        path.push(first.to_string());
                        path.push(name);

                        if fs::metadata(&path).is_ok() 
                        {
                            return Self::from_path(path);
                        }
                    }

                    // Check non-standard location.
                    {
                        let mut path = path.clone();
                        path.push(format!("{:x}", first as usize));
                        path.push(name);

                        if fs::metadata(&path).is_ok() 
                        {
                            return Self::from_path(path);
                        }
                    }
                }

                Err(Error::NotFound)
            }
            /// Load a database from the given path.
            pub fn from_path<P: AsRef<Path>>(path: P) -> error::Result<Self> 
            {
                let mut file = File::open(path)?;
                let mut buffer = Vec::new();
                file.read_to_end(&mut buffer)?;

                Self::from_buffer(buffer)
            }
            /// Load a database from a buffer.
            pub fn from_buffer<T: AsRef<[u8]>>(buffer: T) -> error::Result<Self> 
            {
                if let Ok((_, database)) = compiled::parse(buffer.as_ref()) 
                {
                    Ok(database.into())
                } else 
                {
                    Err(Error::Parse)
                }
            }
            /// The terminal name.
            pub fn name(&self) -> &str 
            
            {
                &self.name
            }
            /// The terminal aliases.
            pub fn aliases(&self) -> &[String] 
            
            {
                &self.aliases
            }
            /// The terminal description.
            pub fn description(&self) -> &str 
            
            {
                &self.description
            }
            /// Get a capability.
            pub fn get<'a, C: Capability<'a>>(&'a self) -> Option<C> 
            {
                C::from(self.inner.get(C::name()))
            }
            /// Get a capability by name.
            pub fn raw<S: AsRef<str>>(&self, name: S) -> Option<&Value> 
            {
                /*
                let name = name.as_ref();
                let name = names::ALIASES.get(name).copied().unwrap_or(name);
                self.inner.get(name)
                */
                None
            }
        }
    }
    pub use self::database::Database;
    /// Constants to deal with name differences across terminfo and termcap.
    pub mod names
    {
        use ::
        {
            *,
        };
    }
    /// Parsers for various formats.
    mod parser
    {
        use ::
        {
            *,
        };

        #[macro_use] mod util
        {
            use ::
            {
                borrow::Cow,
                parsers::nom::
                {
                    branch::alt,
                    character::streaming::char,
                    character::{streaming::line_ending as eol},
                    combinator::eof,
                    IResult,
                },
                *,
            };
            
            const NONE: u8 = 0b000000;
            const PRINT: u8 = 0b000001;
            const SPACE: u8 = 0b000010;
            const CONTROL: u8 = 0b000100;
            const PIPE: u8 = 0b001000;
            const COMMA: u8 = 0b010000;
            const EOL: u8 = 0b100000;

            // Ugly table of DOOM, gotta run and gun.
            
            static ASCII: [u8; 256] = 
            [
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, SPACE, EOL, NONE, NONE, EOL, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                PRINT | SPACE, PRINT, PRINT, PRINT | CONTROL, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT | COMMA | CONTROL, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT | CONTROL, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
                PRINT, PRINT, PRINT, PRINT, PRINT | PIPE, PRINT, PRINT, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
                NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
            ];

            #[inline( always )] pub fn is_ws(ch: u8) -> bool 
            {
                unsafe { ASCII.get_unchecked(ch as usize) & SPACE == SPACE }
            }

            #[inline( always )] pub fn is_eol(ch: u8) -> bool 
            {
                unsafe { ASCII.get_unchecked(ch as usize) & EOL == EOL }
            }

            #[inline( always )] pub fn is_printable_no_pipe(ch: u8) -> bool 
            {
                unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | PIPE) == PRINT }
            }

            #[inline( always )] pub fn is_printable_no_comma(ch: u8) -> bool 
            {
                unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | COMMA) == PRINT }
            }

            #[inline( always )] pub fn is_printable_no_control(ch: u8) -> bool 
            {
                unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | CONTROL) == PRINT }
            }

            pub fn ws(input: &[u8]) -> IResult<&[u8], char> 
            {
                alt((char(' '), char('\t')))(input)
            }

            pub fn end(input: &[u8]) -> IResult<&[u8], &[u8]> 
            {
                alt((eof, eol))(input)
            }

            #[inline] pub fn number(i: &[u8]) -> i32 
            {
                let mut n: i32 = 0;

                for &ch in i {
                    let d = (ch as i32).wrapping_sub(b'0' as i32);

                    if d <= 9 {
                        n = n.saturating_mul(10).saturating_add(d);
                    }
                }

                n
            }

            pub fn unescape(i: &[u8]) -> Cow<[u8]> 
            {
                fn escape<I: Iterator<Item = u8>>(output: &mut Vec<u8>, iter: &mut I) 
                {
                    match iter.next() {
                        None => (),

                        Some(b'a') => output.push(0x07),

                        Some(b'b') => output.push(0x08),

                        Some(b'E') | Some(b'e') => output.push(0x1B),

                        Some(b'f') => output.push(0x0C),

                        Some(b'l') | Some(b'n') => output.push(b'\n'),

                        Some(b'r') => output.push(b'\r'),

                        Some(b's') => output.push(b' '),

                        Some(b't') => output.push(b'\t'),

                        Some(b'^') => output.push(b'^'),

                        Some(b'\\') => output.push(b'\\'),

                        Some(b',') => output.push(b','),

                        Some(b':') => output.push(b':'),

                        Some(b'0') => output.push(0x00),

                        Some(a) if is::digit(a) => match (iter.next(), iter.next()) {
                            (Some(b), Some(c)) if is::digit(b) && is::digit(c) => {
                                if let Ok(number) =
                                    u8::from_str_radix(unsafe { str::from_utf8_unchecked(&[a, b, c]) }, 8)
                                {
                                    output.push(number);
                                } else {
                                    output.extend(&[a, b, c]);
                                }
                            }

                            (Some(b), None) => output.extend(&[b'\\', a, b]),

                            (None, None) => output.extend(&[b'\\', a]),

                            _ => unreachable!(),
                        },

                        Some(ch) => output.extend(&[b'\\', ch]),
                    }
                }

                fn control<I: Iterator<Item = u8>>(output: &mut Vec<u8>, iter: &mut I) {
                    match iter.next() {
                        None => (),

                        Some(ch) if ch.is_ascii_uppercase() => output.push(ch - b'A' + 1),

                        Some(ch) if ch.is_ascii_lowercase() => output.push(ch - b'a' + 1),

                        Some(ch) => output.extend(&[b'^', ch]),
                    }
                }

                let mut chars = i.iter().cloned();
                let mut offset = 0;

                while let Some(ch) = chars.next() {
                    if ch == b'\\' || ch == b'^' {
                        let mut output = i[..offset].to_vec();

                        match ch {
                            b'\\' => escape(&mut output, &mut chars),

                            b'^' => control(&mut output, &mut chars),

                            _ => unreachable!(),
                        }

                        while let Some(ch) = chars.next() {
                            match ch {
                                b'\\' => escape(&mut output, &mut chars),

                                b'^' => control(&mut output, &mut chars),

                                ch => output.push(ch),
                            }
                        }

                        return Cow::Owned(output);
                    }

                    offset += 1;
                }

                Cow::Borrowed(i)
            }
        }

        pub mod compiled
        {
            use ::
            {
                parsers::nom::
                {
                    branch::alt,
                    bytes::streaming::{tag, take, take_until},
                    combinator::{complete, cond, map, map_opt, map_parser, opt},
                    multi::count,
                    number::streaming::{le_i16, le_i32},
                    IResult,
                },
                *,
            };

            use ::common::capability::Value;
            use ::common::names;

            #[derive(Eq, PartialEq, Clone, Debug)]
            pub struct Database<'a> {
                names: &'a [u8],
                standard: Standard<'a>,
                extended: Option<Extended<'a>>,
            }

            impl<'a> From<Database<'a>> for ::common::Database
            {
                fn from(source: Database<'a>) -> Self 
                {
                    let mut database = ::common::Database::new();
                    /*
                    let mut names = source
                    .names
                    .split(|&c| c == b'|')
                    .map(|s| unsafe { str::from_utf8_unchecked(s) })
                    .map(|s| s.trim())
                    .collect::<Vec<_>>();
                    
                    database.name(names.remove(0));
                    names.pop().map(|name| database.description(name));
                    database.aliases(names);

                    for (index, _) in source.standard.booleans.iter().enumerate().filter(|&(_, &value)| value) {
                        if let Some(&name) = names::BOOLEAN.get(&(index as u16)) {
                            database.raw(name, Value::True);
                        }
                    }

                    for (index, &value) in source.standard.numbers.iter().enumerate().filter(|&(_, &n)| n >= 0)
                    {
                        if let Some(&name) = names::NUMBER.get(&(index as u16)) {
                            database.raw(name, Value::Number(value));
                        }
                    }

                    for (index, &offset) in source.standard.strings.iter().enumerate().filter(|&(_, &n)| n >= 0)
                    {
                        if let Some(&name) = names::STRING.get(&(index as u16)) {
                            let string = &source.standard.table[offset as usize..];
                            let edge = string.iter().position(|&c| c == 0).unwrap();

                            database.raw(name, Value::String(Vec::from(&string[..edge])));
                        }
                    }

                    if let Some(extended) = source.extended {
                        let names = extended
                            .table
                            .split(|&c| c == 0)
                            .skip(extended.strings.iter().cloned().filter(|&n| n >= 0).count())
                            .map(|s| unsafe { str::from_utf8_unchecked(s) })
                            .collect::<Vec<_>>();

                        for (index, _) in extended.booleans.iter().enumerate().filter(|&(_, &value)| value) {
                            database.raw(names[index], Value::True);
                        }

                        for (index, &value) in extended.numbers.iter().enumerate().filter(|&(_, &n)| n >= 0) {
                            database.raw(names[extended.booleans.len() + index], Value::Number(value));
                        }

                        for (index, &offset) in extended.strings.iter().enumerate().filter(|&(_, &n)| n >= 0) {
                            let string = &extended.table[offset as usize..];
                            let edge = string.iter().position(|&c| c == 0).unwrap();

                            database.raw(
                                names[extended.booleans.len() + extended.numbers.len() + index],
                                Value::String(Vec::from(&string[..edge])),
                            );
                        }
                    }
                    */
                    database.build().unwrap()
                }
            }

            #[derive(Eq, PartialEq, Clone, Debug)]
            pub struct Standard<'a> {
                booleans: Vec<bool>,
                numbers: Vec<i32>,
                strings: Vec<i32>,
                table: &'a [u8],
            }

            #[derive(Eq, PartialEq, Clone, Debug)]
            pub struct Extended<'a> {
                booleans: Vec<bool>,
                numbers: Vec<i32>,
                strings: Vec<i32>,
                names: Vec<i32>,
                table: &'a [u8],
            }

            fn bit_size(magic: &[u8]) -> usize {
                match magic[1] {
                    0x01 => 16,
                    0x02 => 32,

                    _ => unreachable!("unknown magic number"),
                }
            }

            pub fn parse(input: &[u8]) -> IResult<&[u8], Database> {
                let (input, magic) = alt((tag([0x1A, 0x01]), tag([0x1E, 0x02])))(input)?;

                let (input, name_size) = size(input)?;
                let (input, bool_count) = size(input)?;
                let (input, num_count) = size(input)?;
                let (input, string_count) = size(input)?;
                let (input, table_size) = size(input)?;

                let (input, names) = map_parser(take(name_size), take_until("\x00"))(input)?;

                let (input, booleans) = count(boolean, bool_count)(input)?;

                let (input, _) = cond((name_size + bool_count) % 2 != 0, take(1_usize))(input)?;

                let (input, numbers) = count(|input| capability(input, bit_size(magic)), num_count)(input)?;

                let (input, strings) = count(|input| capability(input, 16), string_count)(input)?;

                let (input, table) = take(table_size)(input)?;

                let (input, extended) = opt(complete(|input| {
                    let (input, _) = cond(table_size % 2 != 0, take(1_usize))(input)?;

                    let (input, ext_bool_count) = size(input)?;
                    let (input, ext_num_count) = size(input)?;
                    let (input, ext_string_count) = size(input)?;
                    let (input, _ext_offset_count) = size(input)?;
                    let (input, ext_table_size) = size(input)?;

                    let (input, booleans) = count(boolean, ext_bool_count)(input)?;

                    let (input, _) = cond(ext_bool_count % 2 != 0, take(1_usize))(input)?;

                    let (input, numbers) =
                        count(|input| capability(input, bit_size(magic)), ext_num_count)(input)?;

                    let (input, strings) = count(|input| capability(input, 16), ext_string_count)(input)?;

                    let (input, names) = count(
                        |input| capability(input, 16),
                        ext_bool_count + ext_num_count + ext_string_count,
                    )(input)?;

                    let (input, table) = take(ext_table_size)(input)?;

                    Ok((input, Extended { booleans, numbers, strings, names, table }))
                }))(input)?;

                Ok((
                    input,
                    Database { names, standard: Standard { booleans, numbers, strings, table }, extended },
                ))
            }

            fn boolean(input: &[u8]) -> IResult<&[u8], bool> {
                alt((map(tag([0]), |_| false), map(tag([1]), |_| true)))(input)
            }

            fn size(input: &[u8]) -> IResult<&[u8], usize> {
                map_opt(le_i16, |n| match n {
                    -1 => Some(0),
                    n if n >= 0 => Some(n as usize),
                    _ => None,
                })(input)
            }

            fn capability(input: &[u8], bits: usize) -> IResult<&[u8], i32> {
                alt((
                    map_opt(
                        cond(bits == 16, map_opt(le_i16, |n| if n >= -2 { Some(n as i32) } else { None })),
                        |o| o,
                    ),
                    map_opt(cond(bits == 32, map_opt(le_i32, |n| if n >= -2 { Some(n) } else { None })), |o| o),
                ))(input)
            }
        }

        pub mod expansion
        {
            use ::
            {
                parsers::nom::
                {
                    branch::alt,
                    bytes::complete,
                    bytes::streaming::{tag, take, take_while},
                    character::streaming::one_of,
                    combinator::{map, opt, value},
                    error::{make_error, ErrorKind},
                    IResult, Err
                },
                *,
            }; use super::util::number;

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Item<'a> 
            {
                String(&'a [u8]),
                Constant(Constant),
                Variable(Variable),
                Operation(Operation),
                Conditional(Conditional),
                Print(Print),
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Constant 
            {
                Character(u8),
                Integer(i32),
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Variable 
            {
                Length,
                Push(u8),
                Set(bool, u8),
                Get(bool, u8),
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Operation 
            {
                Increment,
                Unary(Unary),
                Binary(Binary),
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Unary 
            {
                Not,
                NOT,
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Binary 
            {
                Add,
                Subtract,
                Multiply,
                Divide,
                Remainder,

                AND,
                OR,
                XOR,

                And,
                Or,

                Equal,
                Greater,
                Lesser,
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Conditional 
            {
                If,
                Then,
                Else,
                End,
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub struct Print 
            {
                pub flags: Flags,
                pub format: Format,
            }

            #[derive( Eq, PartialEq, Copy, Clone, Debug )]
            pub enum Format 
            {
                Chr,
                Uni,
                Str,
                Dec,
                Oct,
                Hex,
                HEX,
            }

            #[derive(Eq, PartialEq, Copy, Clone, Default, Debug)]
            pub struct Flags 
            {
                pub width: usize,
                pub precision: usize,

                pub alternate: bool,
                pub left: bool,
                pub sign: bool,
                pub space: bool,
            }

            pub fn parse(input: &[u8]) -> IResult<&[u8], Item> 
            {
                alt((expansion, string))(input)
            }

            fn string(input: &[u8]) -> IResult<&[u8], Item> 
            {
                map(complete::take_till(|b| b == b'%'), Item::String)(input)
            }

            fn expansion(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, _) = tag("%")(input)?;
                let (input, item) = alt((percent, constant, variable, operation, conditional, print))(input)?;

                Ok((input, item))
            }

            fn percent(input: &[u8]) -> IResult<&[u8], Item> 
            {
                value(Item::String(b"%"), tag("%"))(input)
            }

            fn constant(input: &[u8]) -> IResult<&[u8], Item> 
            {
                alt((constant_char, constant_integer))(input)
            }

            fn constant_char(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, _) = tag("'")(input)?;
                let (input, ch) = take(1_usize)(input)?;
                let (input, _) = tag("'")(input)?;

                Ok((input, Item::Constant(Constant::Character(ch[0]))))
            }

            fn constant_integer(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, _) = tag("{")(input)?;
                let (input, digit) = take_while(is::digit)(input)?;
                let (input, _) = tag("}")(input)?;

                Ok((input, Item::Constant(Constant::Integer(number(digit)))))
            }

            fn variable(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, c) = take(1_usize)(input)?;
                match c {
                    b"l" => Ok((input, Item::Variable(Variable::Length))),

                    b"p" => map(one_of("123456789"), |n| Item::Variable(Variable::Push(n as u8 - b'1')))(input),

                    b"P" => alt((
                        map(one_of("abcdefghijklmnopqrstuvwxyz"), |n| {
                            Item::Variable(Variable::Set(true, n as u8 - b'a'))
                        }),
                        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |n| {
                            Item::Variable(Variable::Set(false, n as u8 - b'A'))
                        }),
                    ))(input),

                    b"g" => alt((
                        map(one_of("abcdefghijklmnopqrstuvwxyz"), |n| {
                            Item::Variable(Variable::Get(true, n as u8 - b'a'))
                        }),
                        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |n| {
                            Item::Variable(Variable::Get(false, n as u8 - b'A'))
                        }),
                    ))(input),

                    _ => Err( Err::Error(make_error(input, ErrorKind::Switch))),
                }
            }

            fn operation(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, c) = take(1_usize)(input)?;
                match c {
                    b"+" => Ok((input, Item::Operation(Operation::Binary(Binary::Add)))),
                    b"-" => Ok((input, Item::Operation(Operation::Binary(Binary::Subtract)))),
                    b"*" => Ok((input, Item::Operation(Operation::Binary(Binary::Multiply)))),
                    b"/" => Ok((input, Item::Operation(Operation::Binary(Binary::Divide)))),
                    b"m" => Ok((input, Item::Operation(Operation::Binary(Binary::Remainder)))),
                    b"i" => Ok((input, Item::Operation(Operation::Increment))),

                    b"&" => Ok((input, Item::Operation(Operation::Binary(Binary::AND)))),
                    b"|" => Ok((input, Item::Operation(Operation::Binary(Binary::OR)))),
                    b"^" => Ok((input, Item::Operation(Operation::Binary(Binary::XOR)))),
                    b"~" => Ok((input, Item::Operation(Operation::Unary(Unary::NOT)))),

                    b"A" => Ok((input, Item::Operation(Operation::Binary(Binary::And)))),
                    b"O" => Ok((input, Item::Operation(Operation::Binary(Binary::Or)))),
                    b"!" => Ok((input, Item::Operation(Operation::Unary(Unary::Not)))),

                    b"=" => Ok((input, Item::Operation(Operation::Binary(Binary::Equal)))),
                    b">" => Ok((input, Item::Operation(Operation::Binary(Binary::Greater)))),
                    b"<" => Ok((input, Item::Operation(Operation::Binary(Binary::Lesser)))),

                    _ => Err( Err::Error(make_error(input, ErrorKind::Switch))),
                }
            }

            fn conditional(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, c) = take(1_usize)(input)?;
                match c {
                    b"?" => Ok((input, Item::Conditional(Conditional::If))),
                    b"t" => Ok((input, Item::Conditional(Conditional::Then))),
                    b"e" => Ok((input, Item::Conditional(Conditional::Else))),
                    b";" => Ok((input, Item::Conditional(Conditional::End))),

                    _ => Err( Err::Error(make_error(input, ErrorKind::Switch))),
                }
            }

            fn print(input: &[u8]) -> IResult<&[u8], Item> 
            {
                let (input, _) = opt(tag(":"))(input)?;

                let (input, flags) = take_while(is_flag)(input)?;
                let (input, width) = opt(take_while(is::digit))(input)?;
                let (input, precision) = opt(|input| {
                    let (input, _) = tag(".")(input)?;
                    let (input, amount) = take_while(is::digit)(input)?;

                    Ok((input, amount))
                })(input)?;

                let (input, format) = one_of("doxXsc")(input)?;

                Ok((
                    input,
                    Item::Print(Print {
                        flags: Flags {
                            width: number(width.unwrap_or(b"0")) as usize,
                            precision: number(precision.unwrap_or(b"0")) as usize,

                            alternate: flags.contains(&b'#'),
                            left: flags.contains(&b'-'),
                            sign: flags.contains(&b'+'),
                            space: flags.contains(&b' '),
                        },

                        format: match format {
                            'd' => Format::Dec,
                            'o' => Format::Oct,
                            'x' => Format::Hex,
                            'X' => Format::HEX,
                            's' => Format::Str,
                            'c' => Format::Chr,
                            'u' => Format::Uni,
                            _ => unreachable!(),
                        },
                    }),
                ))
            }

            fn is_flag(i: u8) -> bool 
            {
                i == b' ' || i == b'-' || i == b'+' || i == b'#'
            }
        }

        pub mod source
        {
            use ::
            {
                borrow::{ Cow },
                common::
                {
                    parser::
                    {
                        util::{ unescape, end, is_eol, is_ws, ws, is_printable_no_comma, is_printable_no_control, is_printable_no_pipe },
                    },
                },
                parsers::nom::
                {
                    branch::alt,
                    bytes::streaming::{ tag, take, take_until, take_while },
                    character::{ streaming::line_ending as eol },
                    combinator::{ complete, map, map_res, opt },
                    error::{ make_error, ErrorKind },
                    sequence::terminated,
                    IResult, Err,
                },
                *,
            };

            #[derive(Eq, PartialEq, Clone, Debug)]
            pub enum Item<'a> {
                Comment(&'a str),

                Definition { name: &'a str, aliases: Vec<&'a str>, description: &'a str },

                True(&'a str),
                Number(&'a str, i16),
                String(&'a str, Cow<'a, [u8]>),
                Disable(&'a str),
            }

            pub fn parse(input: &[u8]) -> IResult<&[u8], Item> {
                alt((comment, definition, disable, entry))(input)
            }

            fn comment(input: &[u8]) -> IResult<&[u8], Item> {
                let (input, _) = tag("#")(input)?;
                let (input, content) = map_res(terminated(take_until("\n"), tag("\n")), str::from_utf8)(input)?;
                let (input, _) = opt(complete(take_while(is_eol)))(input)?;

                Ok((input, Item::Comment(content.trim())))
            }

            fn definition(input: &[u8]) -> IResult<&[u8], Item> {
                let (input, name) =
                    map(take_while(is_printable_no_pipe), |n| unsafe { str::from_utf8_unchecked(n) })(input)?;

                let (input, _) = tag("|")(input)?;

                let (input, content) =
                    map(take_while(is_printable_no_comma), |n| unsafe { str::from_utf8_unchecked(n) })(input)?;

                let (input, _) = tag(",")(input)?;

                let (input, _) = take_while(is_ws)(input)?;

                let (input, _) = eol(input)?;
                let (input, _) = opt(complete(take_while(is_eol)))(input)?;

                Ok((input, {
                    let mut aliases = content.split(|c| c == '|').map(|n| n.trim()).collect::<Vec<_>>();

                    Item::Definition { name, description: aliases.pop().unwrap(), aliases }
                }))
            }

            fn disable(input: &[u8]) -> IResult<&[u8], Item> {
                let (input, _) = ws(input)?;
                let (input, _) = take_while(is_ws)(input)?;
                let (input, _) = tag("@")(input)?;

                let (input, name) =
                    map(take_while(is_printable_no_control), |n| unsafe { str::from_utf8_unchecked(n) })(
                        input,
                    )?;

                let (input, _) = tag(",")(input)?;
                let (input, _) = take_while(is_ws)(input)?;
                let (input, _) = end(input)?;
                let (input, _) = opt(complete(take_while(is_eol)))(input)?;

                Ok((input, Item::Disable(name)))
            }

            fn entry(input: &[u8]) -> IResult<&[u8], Item> {
                let (input, _) = ws(input)?;
                let (input, _) = take_while(is_ws)(input)?;

                let (input, name) =
                    map(take_while(is_printable_no_control), |n| unsafe { str::from_utf8_unchecked(n) })(
                        input,
                    )?;

                let (input, c) = take(1_usize)(input)?;
                let (input, value) = match c {
                    b"," => (input, Item::True(name)),

                    b"#" => {
                        let (input, value) =
                            map(take_while(is::digit), |n| unsafe { str::from_utf8_unchecked(n) })(input)?;

                        let (input, _) = tag(",")(input)?;

                        (input, Item::Number(name, value.parse().unwrap()))
                    }

                    b"=" => {
                        let (input, value) = take_while(is_printable_no_comma)(input)?;

                        let (input, _) = tag(",")(input)?;

                        (input, Item::String(name, unescape(value)))
                    }

                    _ => Err( Err::Error(make_error(input, ErrorKind::Switch)))?,
                };

                let (input, _) = take_while(is_ws)(input)?;
                let (input, _) = end(input)?;
                let (input, _) = opt(complete(take_while(is_eol)))(input)?;

                Ok((input, value))
            }
        }       
    }
}

pub mod convert
{
    pub use std::convert::{ * };
}
/*
mortal*/
pub mod core
{
    use ::
    {
        *,
    };
    
}
/*
over | the best data format*/
pub mod database
{
    //! OVER: the best data format.
    //extern crate num_bigint;
    //extern crate num_rational;
    use ::
    {
        *,
    };
    /// Result type for this crate.
    pub type OverResult<T> = Result<T, OverError>;

    // Indent step in .over files.
    const INDENT_STEP: usize = 4; 

    pub mod arrays
    {
        //! `Arr` module | An array container which can hold an arbitrary number of elements of a single type.
        use ::
        {
            database::
            {
                parses::format::Format,
                types::Type,
                values::Value,
                OverError, OverResult, INDENT_STEP,
            },
            slice::Iter,
            sync::Arc,
            *,
        };
        
        #[derive(Clone, Debug)]
        struct ArrInner {
            vec: Vec<Value>,
            inner_t: Type,
        }

        /// `Arr` struct.
        #[derive(Clone, Debug)]
        pub struct Arr {
            inner: Arc<ArrInner>,
        }

        impl Arr {
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
            pub fn from_vec_unchecked(vec: Vec<Value>, inner_t: Type) -> Arr {
                Arr {
                    inner: Arc::new(ArrInner { vec, inner_t }),
                }
            }

            /// Returns a reference to the inner vec of this `Arr`.
            pub fn vec_ref(&self) -> &Vec<Value> 
            {
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
            pub fn is_empty(&self) -> bool
            {
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
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {write!(f, "{}", self.format(true, INDENT_STEP))
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
    } pub use self::arrays::Arr;

    pub mod error
    {
        //! Error module.
        use ::
        {
            database::
            {
                parses::error::ParseError,
                types::Type,
            },
            error::{ Error },
            *,
        };
        /// The fabulous OVER error type.
        #[derive(Debug, PartialEq, Eq)]
        pub enum OverError
        {
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

        impl fmt::Display for OverError 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {use self::OverError::*;

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

        impl Error for OverError 
        {
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

        impl From<io::Error> for OverError 
        {
            fn from(e: io::Error) -> Self {
                OverError::IoError(format!("{}", e))
            }
        }

        impl From<ParseError> for OverError 
        {
            fn from(e: ParseError) -> Self {
                OverError::ParseError(format!("{}", e))
            }
        }

    } pub use self::error::OverError;

    pub mod objects
    {
        //! `Obj` module | A hashmap of keys to values, where values can be any type, including other objects.
        use ::
        {
            collections::
            {
                hash_map::{Iter, Keys, Values},
                HashMap,
            },
            database::
            {
                arrays::Arr,
                error::OverError,
                parses::{ self, format::Format },
                tuples::Tup,
                types::Type,
                values::Value,
                OverResult, INDENT_STEP,
            },
            num::
            {
                big::{ BigInt },
                rational::{ BigRational },
                traits::{ Zero },
            },
            str::{ FromStr },
            sync::
            {
                atomic::{AtomicUsize, Ordering}, Arc,
            },
            *,
        };

        lazy_static! {
            static ref CUR_ID: AtomicUsize = AtomicUsize::new(0);
        }

        fn get_id() -> usize {
            CUR_ID.fetch_add(1, Ordering::Relaxed)
        }

        #[derive(Clone, Debug)]
        struct ObjInner {
            map: HashMap<String, Value>,
            parent: Option<Obj>,
            id: usize,
        }

        /// `Obj` struct.
        #[derive(Clone, Debug)]
        pub struct Obj {
            inner: Arc<ObjInner>,
        }

        macro_rules! get_fn {
            ( $doc:expr, $name:tt, $type:ty ) => {
                #[doc=$doc]
                pub fn $name(&self, field: &str) -> OverResult<$type> {
                    match self.get(field) {
                        Some(value) => {
                            match value.$name() {
                                Ok(result) => Ok(result),
                                e @ Err(_) => e,
                            }
                        }
                        None => Err(OverError::FieldNotFound(field.into())),
                    }
                }
            }
        }

        impl Obj {
            /// Returns a new `Obj` created from the given `HashMap`.
            pub fn from_map(obj_map: HashMap<String, Value>) -> OverResult<Obj> {
                for field in obj_map.keys() {
                    if !Self::is_valid_field(field) {
                        return Err(OverError::InvalidFieldName((*field).clone()));
                    }
                }
                let id = get_id();

                Ok(Obj {
                    inner: Arc::new(ObjInner {
                        map: obj_map,
                        parent: None,
                        id,
                    }),
                })
            }
            /// Returns a new `Obj` created from the given `HashMap` with given `parent`.
            pub fn from_map_with_parent(obj_map: HashMap<String, Value>, parent: Obj) -> OverResult<Obj> {
                for field in obj_map.keys() {
                    if !Self::is_valid_field(field) {
                        return Err(OverError::InvalidFieldName(field.clone()));
                    }
                }
                let id = get_id();

                Ok(Obj {
                    inner: Arc::new(ObjInner {
                        map: obj_map,
                        parent: Some(parent),
                        id,
                    }),
                })
            }
            /// Returns a new `Obj` created from the given `HashMap`.
            pub fn from_map_unchecked(obj_map: HashMap<String, Value>) -> Obj {
                let id = get_id();

                Obj {
                    inner: Arc::new(ObjInner {
                        map: obj_map,
                        parent: None,
                        id,
                    }),
                }
            }
            /// Returns a new `Obj` created from the given `HashMap` with given `parent`.
            pub fn from_map_with_parent_unchecked(obj_map: HashMap<String, Value>, parent: Obj) -> Obj {
                let id = get_id();

                Obj {
                    inner: Arc::new(ObjInner {
                        map: obj_map,
                        parent: Some(parent),
                        id,
                    }),
                }
            }
            /// Returns the ID of this `Obj`.
            pub fn id(&self) -> usize {
                self.inner.id
            }
            /// Returns a reference to the inner map of this `Obj`.
            pub fn map_ref(&self) -> &HashMap<String, Value> 
            {
                &self.inner.map
            }
            /// Returns a new `Obj` loaded from a file.
            pub fn from_file(path: &str) -> OverResult<Obj> {
                Ok(parses::load_from_file(path)?)
            }
            /// Writes this `Obj` to given file in `.over` representation.
            pub fn write_to_file(&self, path: &str) -> OverResult<()> {
                str::write_file(path, &self.write_str())?;
                Ok(())
            }
            /// Writes this `Obj` to a `String`.
            pub fn write_str(&self) -> String {
                self.format(false, 0)
            }
            /// Iterates over each `(String, Value)` pair in `self`, applying `f`.
            pub fn with_each<F>(&self, mut f: F)
            where
                F: FnMut(&String, &Value),
            {
                for (field, value) in &self.inner.map {
                    f(field, value)
                }
            }
            /// Returns the number of fields for this `Obj` (parent fields not included).
            pub fn len(&self) -> usize {
                self.inner.map.len()
            }
            /// Returns whether this `Obj` is empty.
            pub fn is_empty(&self) -> bool
            {
                self.inner.map.is_empty()
            }
            /// Returns whether `self` and `other` point to the same data.
            pub fn ptr_eq(&self, other: &Self) -> bool {
                Arc::ptr_eq(&self.inner, &other.inner)
            }
            /// Returns true if this `Obj` contains `field`.
            pub fn contains(&self, field: &str) -> bool
            {
                self.inner.map.contains_key(field)
            }
            /// Gets the `Value` associated with `field`.
            pub fn get(&self, field: &str) -> Option<Value> {
                match self.inner.map.get(field) {
                    Some(value) => Some(value.clone()),
                    None => match self.inner.parent {
                        Some(ref parent) => parent.get(field),
                        None => None,
                    },
                }
            }
            /// Gets the `Value` associated with `field` and the `Obj` where it was found.
            pub fn get_with_source(&self, field: &str) -> Option<(Value, Obj)> {
                match self.inner.map.get(field) {
                    Some(value) => Some((value.clone(), self.clone())),
                    None => match self.inner.parent {
                        Some(ref parent) => parent.get_with_source(field),
                        None => None,
                    },
                }
            }

            get_fn!(
                "Returns the `bool` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Bool`.",
                get_bool,
                bool
            );
            get_fn!(
                "Returns the `BigInt` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Int`.",
                get_int,
                BigInt
            );
            get_fn!(
                "Returns the `BigRational` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Frac`.",
                get_frac,
                BigRational
            );
            get_fn!(
                "Returns the `char` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Char`.",
                get_char,
                char
            );
            get_fn!(
                "Returns the `String` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Str`.",
                get_str,
                String
            );
            get_fn!(
                "Returns the `Arr` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Arr`.",
                get_arr,
                Arr
            );
            get_fn!(
                "Returns the `Tup` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Tup`.",
                get_tup,
                Tup
            );
            get_fn!(
                "Returns the `Obj` found at `field`. \
                Returns an error if the field was not found \
                or if the `Value` at `field` is not `Obj`.",
                get_obj,
                Obj
            );

            /// Returns whether this `Obj` has a parent.
            pub fn has_parent(&self) -> bool
            {
                self.inner.parent.is_some()
            }

            /// Returns the parent for this `Obj`.
            pub fn get_parent(&self) -> Option<Obj> {
                match self.inner.parent {
                    Some(ref parent) => Some(parent.clone()),
                    None => None,
                }
            }

            /// Returns true if `field` is a valid field name for an `Obj`.
            pub fn is_valid_field(field: &str) -> bool {
                let mut first = true;

                for ch in field.chars() {
                    if first {
                        if !Self::is_valid_field_char(ch, true) {
                            return false;
                        }
                        first = false;
                    } else if !Self::is_valid_field_char(ch, false) {
                        return false;
                    }
                }

                true
            }

            /// Returns true if the given char is valid for a field, depending on whether it is the first char or not.
            pub fn is_valid_field_char(ch: char, first: bool) -> bool
            {
                match ch {
                    ch if ch.is_alphabetic() => true,
                    ch if is::char_digit(ch) => !first,
                    '_' => true,
                    '^' => first,
                    _ => false,
                }
            }

            /// An iterator visiting all fields (keys) in arbitrary order.
            pub fn keys(&self) -> Keys<String, Value> {
                self.map_ref().keys()
            }

            /// An iterator visiting all values in arbitrary order.
            pub fn values(&self) -> Values<String, Value> {
                self.map_ref().values()
            }

            /// An iterator visiting all field-value pairs in arbitrary order.
            pub fn iter(&self) -> Iter<String, Value> {
                self.map_ref().iter()
            }
        }

        impl Default for Obj 
        {
            fn default() -> Self {
                Self::from_map_unchecked(map! {})
            }
        }

        impl fmt::Display for Obj 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {write!(f, "{}", self.format(true, INDENT_STEP))
            }
        }

        impl FromStr for Obj
        {
            type Err = OverError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(parses::load_from_str(s)?)
            }
        }

        /// For two Objs to be equal, the following two checks must pass:
        /// 1. If either Obj has a parent, then both must have parents and the parents must be equal.
        /// 2. The two Objs must have all the same fields pointing to the same values.
        impl PartialEq for Obj 
        {
            fn eq(&self, other: &Self) -> bool {
                let inner = &self.inner;
                let other_inner = &other.inner;

                // Check parent equality.
                if inner.parent.is_some() && other_inner.parent.is_some() {
                    let parent = self.get_parent().unwrap();
                    let other_parent = other.get_parent().unwrap();
                    if parent != other_parent {
                        return false;
                    }
                } else if !(inner.parent.is_none() && other_inner.parent.is_none()) {
                    return false;
                }

                // Check HashMap equality.
                inner.map == other_inner.map
            }
        }
    } pub use self::objects::Obj;

    pub mod tuples
    {
        use ::
        {
            database::
            {
                parses::format::Format,
                types::Type,
                values::Value,
                OverError, OverResult, INDENT_STEP,
            },
            slice::Iter,
            sync::Arc,
            *,
        };
        
        #[derive(Clone, Debug)]
        struct TupInner {
            vec: Vec<Value>,
            inner_tvec: Vec<Type>,
        }

        /// `Tup` struct.
        #[derive(Clone, Debug)]
        pub struct Tup {
            inner: Arc<TupInner>,
        }

        impl Tup {
            /// Returns a new `Tup` from the given vector of `Value`s.
            pub fn from_vec(values: Vec<Value>) -> Tup {
                let tvec: Vec<Type> = values.iter().map(|val| val.get_type()).collect();

                Tup {
                    inner: Arc::new(TupInner {
                        vec: values,
                        inner_tvec: tvec,
                    }),
                }
            }

            /// Returns a reference to the inner vec of this `Tup`.
            pub fn vec_ref(&self) -> &Vec<Value> 
            {
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
                    Err(OverError::TupOutOfBounds(index))
                } else {
                    Ok(self.inner.vec[index].clone())
                }
            }

            /// Returns the type vector of this `Tup`.
            pub fn inner_type_vec(&self) -> Vec<Type> {
                self.inner.inner_tvec.clone()
            }

            /// Returns the length of this `Tup`.
            pub fn len(&self) -> usize {
                self.inner.vec.len()
            }

            /// Returns whether this `Tup` is empty.
            pub fn is_empty(&self) -> bool
            {
                self.inner.vec.is_empty()
            }

            /// Returns whether `self` and `other` point to the same data.
            pub fn ptr_eq(&self, other: &Self) -> bool {
                Arc::ptr_eq(&self.inner, &other.inner)
            }

            /// Returns an iterator over the Tup.
            pub fn iter(&self) -> Iter<Value> {
                self.vec_ref().iter()
            }
        }

        impl Default for Tup 
        {
            fn default() -> Self {
                Self::from_vec(vec![])
            }
        }

        impl fmt::Display for Tup 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {write!(f, "{}", self.format(true, INDENT_STEP))
            }
        }

        impl From<Vec<Value>> for Tup 
        {
            fn from(vec: Vec<Value>) -> Self {
                Self::from_vec(vec)
            }
        }

        impl PartialEq for Tup 
        {
            fn eq(&self, other: &Self) -> bool {
                // Quickly return false if the types don't match.
                if self.inner.inner_tvec != other.inner.inner_tvec {
                    return false;
                }

                self.inner.vec == other.inner.vec
            }
        }
    } pub use self::tuples::Tup;

    pub mod types
    {
        //! Module for types.
        use ::
        {
            *,
        };
        /// Enum of possible types for `Value`s.
        #[derive(Clone, Debug)]
        pub enum Type {
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
            Arr(Box<Type>),
            /// A tuple type, containing the types of its sub-elements.
            Tup(Vec<Type>),
            /// An object type.
            Obj,
        }

        impl Type {
            /// Returns true if this type is strictly the same as `other`.
            pub fn is(&self, other: &Type) -> bool {
                use self::Type::*;

                match *self {
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

                    Arr(ref t1) => {
                        if let Arr(ref t2) = *other {
                            t1.is(t2)
                        } else {
                            false
                        }
                    }

                    Tup(ref tvec1) => {
                        if let Tup(ref tvec2) = *other {
                            if tvec1.len() != tvec2.len() {
                                return false;
                            }
                            tvec1.iter().zip(tvec2.iter()).all(|(t1, t2)| t1.is(t2))
                        } else {
                            false
                        }
                    }
                }
            }
            /// Returns true if this `Type` contains `Any`.
            pub fn has_any(&self) -> bool {
                match *self {
                    Type::Any => true,
                    Type::Arr(ref t) => Self::has_any(t),
                    Type::Tup(ref tvec) => tvec.iter().any(|t| Self::has_any(t)),
                    _ => false,
                }
            }
            /// Returns a type with the most specificity that can be applied to the two input types as well
            /// as `true` if the returned type is not maximally specific, that is, it contains `Any`.
            pub fn most_specific(type1: &Type, type2: &Type) -> Option<(Type, bool)> {
                use self::Type::*;

                if let Any = *type2 {
                    return Some((type1.clone(), type1.has_any()));
                }

                match *type1 {
                    Any => Some((type2.clone(), type2.has_any())),

                    Arr(ref t1) => {
                        if let Arr(ref t2) = *type2 {
                            Self::most_specific(t1, t2).map(|(t, any)| (Arr(Box::new(t)), any))
                        } else {
                            None
                        }
                    }

                    Tup(ref tvec1) => {
                        if let Tup(ref tvec2) = *type2 {
                            if tvec1.len() == tvec2.len() {
                                let mut has_any = false;

                                let tvec: Option<Vec<Type>> = tvec1
                                    .iter()
                                    .zip(tvec2.iter())
                                    .map(|(t1, t2)| {
                                        Self::most_specific(t1, t2).map(|(t, any)| {
                                            if !has_any && any {
                                                has_any = any;
                                            }
                                            t
                                        })
                                    })
                                    .collect();

                                tvec.map(|tvec| (Tup(tvec), has_any))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }

                    ref t => {
                        if t == type2 {
                            Some((t.clone(), false))
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
            fn eq(&self, other: &Self) -> bool {
                use self::Type::*;

                // If either is Any, always return `true`.
                if let Any = *other {
                    return true;
                }

                match *self {
                    Any => true,
                    Arr(ref box1) => {
                        if let Arr(ref box2) = *other {
                            box1 == box2
                        } else {
                            false
                        }
                    }
                    Tup(ref tvec1) => {
                        if let Tup(ref tvec2) = *other {
                            tvec1 == tvec2
                        } else {
                            false
                        }
                    }
                    _ => self.is(other),
                }
            }
        }
        impl Eq for Type {}

        impl fmt::Display for Type 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {use self::Type::*;

                match *self {
                    Any => write!(f, "Any"),
                    Null => write!(f, "Null"),
                    Bool => write!(f, "Bool"),
                    Int => write!(f, "Int"),
                    Frac => write!(f, "Frac"),
                    Char => write!(f, "Char"),
                    Str => write!(f, "Str"),
                    Arr(ref boxxy) => write!(f, "Arr({})", boxxy),
                    Tup(ref tvec) => write!(
                        f,
                        "Tup({})",
                        match tvec.get(0) {
                            Some(t1) => tvec
                                .iter()
                                .skip(1)
                                .fold(format!("{}", t1), |s, t| format!("{}, {}", s, t)),
                            None => String::from(""),
                        }
                    ),
                    Obj => write!(f, "Obj"),
                }
            }
        }
    }

    pub mod values
    {
        //! Module for values.
        use ::
        {
            database::
            {
                arrays,
                error::OverError,
                objects,
                parses::{ format::Format },
                tuples,
                types::Type,
                OverResult, INDENT_STEP,
            },
            num::
            {
                big::{ BigInt },
                rational::{ BigRational },
                traits::{ ToPrimitive },
            },
            *,
        };

        macro_rules! get_fn 
        {
            ( $doc:expr, $name:tt, $type:ty, $variant:ident ) => {
                #[doc=$doc]
                pub fn $name(&self) -> OverResult<$type> {
                    if let Value::$variant(ref inner) = *self {
                        Ok(inner.clone())
                    } else {
                        Err(OverError::TypeMismatch(Type::$variant, self.get_type()))
                    }
                }
            }
        }

        macro_rules! impl_from 
        {
            ($type:ty, $fn:tt) => {
                impl From<$type> for Value {
                    fn from(inner: $type) -> Self {
                        Value::$fn(inner.into())
                    }
                }
            };
        }

        macro_rules! impl_eq 
        {
            ($valtype:ident, $type:ty) => {
                impl PartialEq<$type> for Value {
                    fn eq(&self, other: &$type) -> bool {
                        match *self {
                            Value::$valtype(ref value) => value == other,
                            _ => false,
                        }
                    }
                }

                impl PartialEq<Value> for $type {
                    fn eq(&self, other: &Value) -> bool {
                        match *other {
                            Value::$valtype(ref value) => value == self,
                            _ => false,
                        }
                    }
                }
            };
        }

        macro_rules! impl_eq_int 
        {
            ($type:ty, $fn:tt) => {
                impl PartialEq<$type> for Value {
                    fn eq(&self, other: &$type) -> bool {
                        match *self {
                            Value::Int(ref value) => match value.$fn() {
                                Some(value) => value == *other,
                                None => false,
                            },
                            _ => false,
                        }
                    }
                }

                impl PartialEq<Value> for $type {
                    fn eq(&self, other: &Value) -> bool {
                        match *other {
                            Value::Int(ref value) => match value.$fn() {
                                Some(value) => value == *self,
                                None => false,
                            },
                            _ => false,
                        }
                    }
                }
            };
        }
        /// Enum of possible values and their inner types.
        #[derive(Clone, Debug, PartialEq)]
        pub enum Value
        {
            /// A null value.
            Null,

            // Copy values.
            /// A boolean value.
            Bool(bool),
            /// A signed integer value.
            Int(BigInt),
            /// A fractional value.
            Frac(BigRational),
            /// A character value.
            Char(char),
            /// A string value.
            Str(String),

            // Reference values.
            /// An array value.
            Arr(arrays::Arr),
            /// A tuple value.
            Tup(tuples::Tup),
            /// An object value.
            Obj(objects::Obj),
        }

        impl Value 
        {
            /// Returns true if this `Value` is null.
            pub fn is_null(&self) -> bool {
                if let Value::Null = *self {
                    true
                } else {
                    false
                }
            }

            /// Returns the `Type` of this `Value`.
            pub fn get_type(&self) -> Type {
                use self::Value::*;

                match *self {
                    Null => Type::Null,
                    Bool(_) => Type::Bool,
                    Int(_) => Type::Int,
                    Frac(_) => Type::Frac,
                    Char(_) => Type::Char,
                    Str(_) => Type::Str,
                    Arr(ref arr) => Type::Arr(Box::new(arr.inner_type())),
                    Tup(ref tup) => Type::Tup(tup.inner_type_vec()),
                    Obj(_) => Type::Obj,
                }
            }

            get_fn!(
                "Returns the `bool` contained in this `Value`. \
                Returns an error if this `Value` is not `Bool`.",
                get_bool,
                bool,
                Bool
            );
            get_fn!(
                "Returns the `BigInt` contained in this `Value`. \
                Returns an error if this `Value` is not `Int`.",
                get_int,
                BigInt,
                Int
            );
            /// Returns the `BigRational` contained in this `Value`.
            /// Returns an error if this `Value` is not `Frac`.
            pub fn get_frac(&self) -> OverResult<BigRational> {
                match *self {
                    Value::Frac(ref inner) => Ok(inner.clone()),
                    Value::Int(ref inner) => Ok(frac!(inner.clone(), 1)),
                    _ => Err(OverError::TypeMismatch(Type::Frac, self.get_type())),
                }
            }
            get_fn!(
                "Returns the `char` contained in this `Value`. \
                Returns an error if this `Value` is not `Char`.",
                get_char,
                char,
                Char
            );
            get_fn!(
                "Returns the `String` contained in this `Value`. \
                Returns an error if this `Value` is not `Str`.",
                get_str,
                String,
                Str
            );
            get_fn!(
                "Returns the `Obj` contained in this `Value`. \
                Returns an error if this `Value` is not `Obj`.",
                get_obj,
                objects::Obj,
                Obj
            );

            /// Returns the `Arr` contained in this `Value`.
            /// Returns an error if this `Value` is not `Arr`.
            pub fn get_arr(&self) -> OverResult<arrays::Arr> {
                if let Value::Arr(ref inner) = *self {
                    Ok(inner.clone())
                } else {
                    Err(OverError::TypeMismatch(
                        Type::Arr(Box::new(Type::Any)),
                        self.get_type(),
                    ))
                }
            }

            /// Returns the `Tup` contained in this `Value`.
            /// Returns an error if this `Value` is not `Tup`.
            pub fn get_tup(&self) -> OverResult<tuples::Tup> {
                if let Value::Tup(ref inner) = *self {
                    Ok(inner.clone())
                } else {
                    Err(OverError::TypeMismatch(Type::Tup(vec![]), self.get_type()))
                }
            }
        }

        impl fmt::Display for Value 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {write!(f, "{}", self.format(true, INDENT_STEP))
            }
        }
        
        impl_eq!(Bool, bool);
        impl_eq!(Int, BigInt);
        impl_eq!(Frac, BigRational);
        impl_eq!(Char, char);
        impl_eq!(Arr, arrays::Arr);
        impl_eq!(Tup, tuples::Tup);
        impl_eq!(Obj, objects::Obj);

        impl<'a> PartialEq<&'a str> for Value 
        {
            fn eq(&self, other: &&str) -> bool {
                match *self {
                    Value::Str(ref value) => value == &other.replace("\r\n", "\n"),
                    _ => false,
                }
            }
        }

        impl<'a> PartialEq<Value> for &'a str 
        {
            fn eq(&self, other: &Value) -> bool {
                match *other {
                    Value::Str(ref value) => value == &self.replace("\r\n", "\n"),
                    _ => false,
                }
            }
        }

        impl PartialEq<String> for Value 
        {
            fn eq(&self, other: &String) -> bool {
                &other.as_str() == self
            }
        }

        impl PartialEq<Value> for String 
        {
            fn eq(&self, other: &Value) -> bool
            {
                &self.as_str() == other
            }
        }
        
        impl_eq_int!(usize, to_usize);
        impl_eq_int!(u8, to_u8);
        impl_eq_int!(u16, to_u16);
        impl_eq_int!(u32, to_u32);
        impl_eq_int!(u64, to_u64);
        impl_eq_int!(i8, to_i8);
        impl_eq_int!(i16, to_i16);
        impl_eq_int!(i32, to_i32);
        impl_eq_int!(i64, to_i64);

        impl_from!(bool, Bool);

        impl_from!(usize, Int);
        impl_from!(u8, Int);
        impl_from!(u16, Int);
        impl_from!(u32, Int);
        impl_from!(u64, Int);
        impl_from!(i8, Int);
        impl_from!(i16, Int);
        impl_from!(i32, Int);
        impl_from!(i64, Int);
        impl_from!(BigInt, Int);
    
        impl_from!(BigRational, Frac);
        impl_from!(char, Char);
        impl_from!(String, Str);

        impl<'a> From<&'a str> for Value 
        {
            fn from(inner: &str) -> Self {
                Value::Str(inner.into())
            }
        }

        impl_from!(arrays::Arr, Arr);
        impl_from!(tuples::Tup, Tup);
        impl_from!(objects::Obj, Obj);
    }

    pub mod parses
    {
        //! Functions for loading/writing Objs.
        use ::
        {
            *,
        };

        pub mod error
        {
            //! Module for parse errors.
            use ::
            {
                database::
                {
                    types::{ Type },
                    OverError,
                },
                error::{ Error },
                num::
                {
                    big::{ BigInt, ParseBigIntError },
                    ParseIntError,
                },
                *,
            };
            
            use super::
            {
                ParseResult,
                MAX_DEPTH,
            };

            pub fn parse_err<T>(file: Option<String>, kind: ParseErrorKind) -> ParseResult<T>
            { Err(ParseError { file, kind }) }
            /// Error kind.
            #[derive(Debug)]
            pub enum ParseErrorKind
            {
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
            pub struct ParseError
            {
                /// The file this error occurred in.
                pub file: Option<String>,
                /// Error kind.
                pub kind: ParseErrorKind,
            }

            impl fmt::Display for ParseError
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
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
                            ::char::format(*ch),
                            line,
                            col
                        ),
                        InvalidFieldChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid character '{}' for field at line {}, column {}",
                            ::char::format(*ch),
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
                            ::char::format(*ch),
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

            impl Error for ParseError
            {
                fn description(&self) -> &str
                {
                    use self::ParseErrorKind::*;

                    match (*self).kind
                    {
                        BinaryOperatorError(_, _, _, _, _) | UnaryOperatorError(_, _, _, _) =>
                        {
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

            impl ParseError
            {
                /// Convert an `OverError` to a `ParseError` given line and column numbers.
                pub fn from_over(e: &OverError, file: Option<String>, line: usize, col: usize) -> Self
                {
                    ParseError
                    {
                        file,
                        kind: ParseErrorKind::OverError(format!("{} at line {}, col {}", e, line, col)),
                    }
                }
            }

            impl From<io::Error> for ParseError
            {
                fn from(e: io::Error) -> Self
                {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::IoError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseIntError> for ParseError
            {
                fn from(e: ParseIntError) -> Self
                {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseBigIntError> for ParseError
            {
                fn from(e: ParseBigIntError) -> Self
                {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }
        } use self::error::ParseError;

        pub mod format
        {
            //! Module containing functions for formatting output of objects.
            use ::
            {
                database::{ arrays::Arr, objects::Obj, tuples::Tup, values::Value, INDENT_STEP },
                num::
                {
                    big::BigInt,
                    rational::BigRational,
                    traits::One,
                },
                *,
            };
            
            fn indent(amount: usize) -> String { " ".repeat(amount) }

            fn get_char_map(ch: char) -> Option<&'static str>
            {
                match ch
                {
                    '\\' => Some("\\\\"),
                    '\"' => Some("\\\""),
                    '\'' => Some("\\\'"),
                    '$' => Some("\\$"),
                    '\n' => Some("\\n"),
                    '\r' => Some("\\r"),
                    '\t' => Some("\\t"),
                    _ => None,
                }
            }

            fn replace_all(s: &str) -> String
            {
                let mut string = String::with_capacity(s.len());

                for ch in s.chars()
                {
                    if let Some(s) = get_char_map(ch) {
                        string.push_str(s);
                    } else {
                        string.push(ch);
                    }
                }

                string
            }
            /// Trait for formatting a .over representation of an object.
            pub trait Format
            {
                fn format(&self, full: bool, indent_amt: usize) -> String;
            }

            impl Format for BigRational
            {
                fn format(&self, _full: bool, _indent_amt: usize) -> String
                {
                    let frac_fmt = format!("{}", *self);

                    if *self.denom() == BigInt::one() { format!("{}.0", frac_fmt) } else { frac_fmt }
                }
            }

            impl Format for char
            {
                fn format(&self, _full: bool, _indent_amt: usize) -> String
                {
                    if let Some(s) = get_char_map(*self) {
                        format!("\'{}\'", s)
                    } else {
                        format!("\'{}\'", *self)
                    }
                }
            }

            impl Format for String
            {
                fn format(&self, _full: bool, _indent_amt: usize) -> String { format!("\"{}\"", replace_all(self)) }
            }

            impl Format for Value
            {
                fn format(&self, _full: bool, indent_amt: usize) -> String
                {
                    match *self
                    {
                        Value::Null => String::from("null"),

                        Value::Bool(ref inner) => {
                            if *inner {
                                String::from("true")
                            } else {
                                String::from("false")
                            }
                        }

                        Value::Int(ref inner) => format!("{}", inner),

                        Value::Frac(ref inner) => inner.format(true, indent_amt),
                        Value::Char(ref inner) => inner.format(true, indent_amt),
                        Value::Str(ref inner) => inner.format(true, indent_amt),
                        Value::Arr(ref inner) => inner.format(true, indent_amt),
                        Value::Tup(ref inner) => inner.format(true, indent_amt),
                        Value::Obj(ref inner) => inner.format(true, indent_amt),
                    }
                }
            }

            impl Format for Arr
            {
                fn format(&self, full: bool, indent_amt: usize) -> String
                {
                    match self.len()
                    {
                        0 =>
                        {
                            if full {
                                String::from("[]")
                            } else {
                                String::new()
                            }
                        }

                        1 =>
                        {
                            let f = self.get(0).unwrap().format(true, indent_amt);
                            if full {
                                format!("[{}]", f)
                            } else {
                                f
                            }
                        }

                        _ =>
                        {
                            let mut s = if full {
                                String::from("[\n")
                            } else {
                                String::new()
                            };

                            self.with_each(|value| {
                                s.push_str(&format!(
                                    "{}{}\n",
                                    indent(indent_amt),
                                    value.format(true, indent_amt + INDENT_STEP)
                                ))
                            });

                            if full {
                                let actual_indent_amt = if indent_amt == 0 {
                                    0
                                } else {
                                    indent_amt - INDENT_STEP
                                };
                                s.push_str(&format!("{}]", indent(actual_indent_amt)));
                            }
                            s
                        }
                    }
                }
            }

            impl Format for Tup
            {
                fn format(&self, full: bool, indent_amt: usize) -> String
                {
                    match self.len()
                    {
                        0 =>
                        {
                            if full {
                                String::from("()")
                            } else {
                                String::new()
                            }
                        }

                        1 =>
                        {
                            let f = self.get(0).unwrap().format(true, indent_amt);
                            if full {
                                format!("({})", f)
                            } else {
                                f
                            }
                        }

                        _ =>
                        {
                            let mut s = if full {
                                String::from("(\n")
                            } else {
                                String::new()
                            };

                            self.with_each(|value| {
                                s.push_str(&format!(
                                    "{}{}\n",
                                    indent(indent_amt),
                                    value.format(true, indent_amt + INDENT_STEP)
                                ))
                            });

                            if full {
                                s.push_str(&format!("{})", indent(indent_amt - INDENT_STEP)));
                            }
                            s
                        }
                    }
                }
            }

            impl Format for Obj
            {
                fn format(&self, full: bool, indent_amt: usize) -> String
                {
                    if self.is_empty() && !self.has_parent()
                    {
                        if full { String::from("{}") } else { String::new() }
                    }
                    
                    else
                    {
                        let mut s = if full { String::from("{\n") }

                        else { String::new() };

                        if let Some(parent) = self.get_parent()
                        {
                            s.push_str(&format!
                            (
                                "{}^: {}\n",
                                indent(indent_amt),
                                parent.format(true, indent_amt + INDENT_STEP)
                            ));
                        }

                        self.with_each(|field, value|
                        {
                            s.push_str(&format!
                            (
                                "{}{}: {}\n",
                                indent(indent_amt),
                                field,
                                value.format(true, indent_amt + INDENT_STEP)
                            ));
                        });

                        if full { s.push_str(&format!("{}}}", indent(indent_amt - INDENT_STEP))); }
                        s
                    }
                }
            }
        }

        pub mod parser
        {
            //! Module containing parsing functions.
            use num::fractional::frac_from_whole_and_dec;
            use ::
            {
                char::{ CharStream },
                collections::{ HashMap, HashSet, VecDeque },
                database::
                {
                    arrays::{self, Arr},
                    objects::Obj,
                    tuples::Tup,
                    types::Type,
                    values::Value,
                },
                num::
                {
                    big::{ BigInt },
                    rational::{ BigRational },
                    traits::{ ToPrimitive, Zero },
                },
                ops::{ Deref },
                path::{ Path },
                *,
            };

            use super::
            {
                error::ParseErrorKind::*,
                error::{parse_err, ParseError},
                ParseResult, MAX_DEPTH,
            };
            
            type ObjMap = HashMap<String, Value>;
            type GlobalMap = HashMap<String, Value>;
            type IncludedMap = (HashMap<String, Value>, HashSet<String>);

            lazy_static!
            {
                static ref OBJ_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
                static ref STR_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
                static ref ARR_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
                static ref TUP_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
            }
            /// Parses given file as an `Obj`.
            pub fn parse_obj_file(path: &str) -> ParseResult<Obj> 
            {
                let stream = CharStream::from_file(path)?;
                parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
            }
            /// Parses given file as an `Obj`, keeping track of already encountered includes.
            pub fn parse_obj_file_includes(path: &str, included: &mut IncludedMap) -> ParseResult<Obj> 
            {
                let stream = CharStream::from_file(path)?;
                parse_obj_stream(stream, included)
            }
            /// Parses given &str as an `Obj`.
            pub fn parse_obj_str(contents: &str) -> ParseResult<Obj> 
            {
                let contents = String::from(contents);
                let stream = CharStream::from_string(contents)?;
                parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
            }
            /// Parses an Obj given a character stream.
            #[inline] fn parse_obj_stream(mut stream: CharStream, mut included: &mut IncludedMap) -> ParseResult<Obj> 
            {
                let mut obj: ObjMap = HashMap::new();

                // Go to the first non-whitespace character, or return if there is none.
                if !find_char(stream.clone()) 
                {
                    return Ok(Obj::from_map_unchecked(obj));
                }

                let mut globals: GlobalMap = HashMap::new();
                let mut parent = None;

                // Parse all field/value pairs for this Obj.
                while parse_field_value_pair
                (
                    &mut stream,
                    &mut obj,
                    &mut globals,
                    &mut included,
                    &mut parent,
                    1,
                    None,
                )? {}

                Ok(match parent 
                {
                    Some(parent) => Obj::from_map_with_parent_unchecked(obj, parent),
                    None => Obj::from_map_unchecked(obj),
                })
            }
            /// Parses a sub-Obj in a file. It *must* start with { and end with }.
            pub fn parse_obj
            (
                mut stream: &mut CharStream,
                globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                depth: usize,
            ) -> ParseResult<Value> 
            {
                // Check depth.
                if depth > MAX_DEPTH 
                {
                    return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
                }

                // We must already be at a '{'.
                let ch = stream.next().unwrap();
                assert_eq!(ch, '{');

                // Go to the first non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) 
                {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                let mut obj: ObjMap = HashMap::new();
                let mut parent = None;

                // Parse field/value pairs.
                while parse_field_value_pair
                (
                    &mut stream,
                    &mut obj,
                    globals,
                    &mut included,
                    &mut parent,
                    depth,
                    Some('}'),
                )? {}

                let obj = match parent 
                {
                    Some(parent) => Obj::from_map_with_parent_unchecked(obj, parent),
                    None => Obj::from_map_unchecked(obj),
                };
                Ok(obj.into())
            }
            /// Parses a field/value pair.
            #[inline] pub fn parse_field_value_pair
            (
                mut stream: &mut CharStream,
                obj: &mut ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                parent: &mut Option<Obj>,
                depth: usize,
                cur_brace: Option<char>,
            ) -> ParseResult<bool>
            {
                // Check if we're at an end delimiter instead of a field.
                let peek = stream.peek().unwrap();
                if peek == '}' && cur_brace.is_some() 
                {
                    let _ = stream.next();
                    return Ok(false);
                } else if is::end_delimiter(peek) 
                {
                    return parse_err
                    (
                        stream.file(),
                        InvalidClosingBracket(cur_brace, peek, stream.line(), stream.col()),
                    );
                }

                // Get the field line/col.
                let (field_line, field_col) = (stream.line(), stream.col());

                // Parse field.
                let (field, is_global, is_parent) = parse_field(stream.clone(), field_line, field_col)?;

                if !is_global && !is_parent && obj.contains_key(&field) 
                {
                    return parse_err(stream.file(), DuplicateField(field, field_line, field_col));
                } else if is_parent && parent.is_some() 
                {
                    return parse_err(
                        stream.file(),
                        DuplicateField("^".into(), field_line, field_col),
                    );
                }

                // Deal with extra whitespace between field and value.
                if !find_char(stream.clone()) 
                {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                // At a non-whitespace character, parse value.
                let (value_line, value_col) = (stream.line(), stream.col());
                let value = parse_value
                (
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
                if is_global 
                {
                    if globals.contains_key(&field) 
                    {
                        return parse_err(stream.file(), DuplicateGlobal(field, field_line, field_col));
                    }
                    globals.insert(field, value);
                } else if is_parent 
                {
                    let par = value
                        .get_obj()
                        .map_err(|e| ParseError::from_over(&e, stream.file(), value_line, value_col))?;
                    *parent = Some(par);
                } else 
                {
                    obj.insert(field, value);
                }

                // Go to the next non-whitespace character.
                if !find_char(stream.clone()) 
                {
                    match cur_brace 
                    {
                        Some(_) => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                        None => return Ok(false),
                    }
                }

                Ok(true)
            }
            /// Parses an Arr given a file.
            fn parse_arr_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Arr> 
            {
                let mut stream = CharStream::from_file(path)?;

                let obj: ObjMap = HashMap::new();
                let mut globals: GlobalMap = HashMap::new();

                let mut vec = Vec::new();
                let mut tcur = Type::Any;
                let mut has_any = true;

                loop 
                {
                    // Go to the first non-whitespace character, or error if there is none.
                    if !find_char(stream.clone()) 
                    {
                        break;
                    }

                    // At a non-whitespace character, parse value.
                    let (value_line, value_col) = (stream.line(), stream.col());
                    let value = parse_value
                    (
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

                    if has_any 
                    {
                        match Type::most_specific(&tcur, &tnew) 
                        {
                            Some((t, any)) => 
                            {
                                tcur = t;
                                has_any = any;
                            }
                            None => 
                            {
                                return parse_err
                                (
                                    stream.file(),
                                    ExpectedType(tcur, tnew, value_line, value_col),
                                );
                            }
                        }
                    } else if tcur != tnew 
                    {
                        return parse_err
                        (
                            stream.file(),
                            ExpectedType(tcur, tnew, value_line, value_col),
                        );
                    }

                    vec.push(value);
                }

                let arr = Arr::from_vec_unchecked(vec, tcur);

                Ok(arr)
            }
            /// Parses a sub-Arr in a file. It *must* start with [ and end with ].
            pub fn parse_arr
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                depth: usize,
            ) -> ParseResult<Value>
            {
                // Check depth.
                if depth > MAX_DEPTH {
                    return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
                }

                // We must already be at a '['.
                let ch = stream.next().unwrap();
                assert_eq!(ch, '[');

                let mut vec = Vec::new();
                let mut tcur = Type::Any;
                let mut has_any = true;

                loop {
                    // Go to the first non-whitespace character, or error if there is none.
                    if !find_char(stream.clone()) {
                        return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                    }

                    let peek = stream.peek().unwrap();
                    if peek == ']' {
                        let _ = stream.next();
                        break;
                    } else if is::end_delimiter(peek) {
                        return parse_err(
                            stream.file(),
                            InvalidClosingBracket(Some(']'), peek, stream.line(), stream.col()),
                        );
                    }

                    // At a non-whitespace character, parse value.
                    let (value_line, value_col) = (stream.line(), stream.col());
                    let value = parse_value(
                        &mut stream,
                        obj,
                        &mut globals,
                        &mut included,
                        value_line,
                        value_col,
                        depth,
                        Some(']'),
                        true,
                    )?;

                    let tnew = value.get_type();

                    if has_any {
                        match Type::most_specific(&tcur, &tnew) {
                            Some((t, any)) => {
                                tcur = t;
                                has_any = any;
                            }
                            None => {
                                return parse_err(
                                    stream.file(),
                                    ExpectedType(tcur, tnew, value_line, value_col),
                                );
                            }
                        }
                    } else if tcur != tnew {
                        return parse_err(
                            stream.file(),
                            ExpectedType(tcur, tnew, value_line, value_col),
                        );
                    }

                    vec.push(value);
                }

                let arr = Arr::from_vec_unchecked(vec, tcur);

                Ok(arr.into())
            }
            /// Parses a Tup given a file.
            pub fn parse_tup_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Tup>
            {
                let mut stream = CharStream::from_file(path)?;

                let mut vec: Vec<Value> = Vec::new();
                let obj: ObjMap = HashMap::new();
                let mut globals: GlobalMap = HashMap::new();

                loop {
                    // Go to the first non-whitespace character, or error if there is none.
                    if !find_char(stream.clone()) {
                        break;
                    }

                    // At a non-whitespace character, parse value.
                    let (value_line, value_col) = (stream.line(), stream.col());
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

                    vec.push(value);
                }

                Ok(vec.into())
            }
            /// Parses a sub-Tup in a file. It *must* start with ( and end with ).
            pub fn parse_tup
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                depth: usize,
            ) -> ParseResult<Value>
            {
                // Check depth.
                if depth > MAX_DEPTH {
                    return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
                }

                // We must already be at a '('.
                let ch = stream.next().unwrap();
                assert_eq!(ch, '(');

                let mut vec = Vec::new();

                loop {
                    // Go to the first non-whitespace character, or error if there is none.
                    if !find_char(stream.clone()) {
                        return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                    }

                    let peek = stream.peek().unwrap();
                    if peek == ')' {
                        let _ = stream.next();
                        break;
                    } else if is::end_delimiter(peek) {
                        return parse_err(
                            stream.file(),
                            InvalidClosingBracket(Some(')'), peek, stream.line(), stream.col()),
                        );
                    }

                    // At a non-whitespace character, parse value.
                    let (value_line, value_col) = (stream.line(), stream.col());
                    let value = parse_value(
                        &mut stream,
                        obj,
                        &mut globals,
                        &mut included,
                        value_line,
                        value_col,
                        depth,
                        Some(')'),
                        true,
                    )?;

                    vec.push(value);
                }

                let tup = Tup::from_vec(vec);

                Ok(tup.into())
            }
            /// Gets the next field in the char stream.
            pub fn parse_field
            (
                mut stream: CharStream,
                line: usize,
                col: usize,
            ) -> ParseResult<(String, bool, bool)>
            {
                let mut field = String::new();
                let mut first = true;
                let mut is_global = false;

                let ch = stream.peek().unwrap();
                if ch == '@' {
                    let ch = stream.next().unwrap();
                    is_global = true;
                    field.push(ch);
                }

                while let Some(ch) = stream.next() {
                    match ch {
                        ':' if !first => {
                            break;
                        }
                        ch if Obj::is_valid_field_char(ch, first) => field.push(ch),
                        ch => {
                            return parse_err(
                                stream.file(),
                                InvalidFieldChar(ch, stream.line(), stream.col() - 1),
                            );
                        }
                    }

                    first = false;
                }

                // Check for invalid field names.
                match field.as_str() {
                    _field_str if is::reserved(_field_str) => {
                        parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
                    }
                    "^" => Ok((field.clone(), false, true)),
                    bad if bad.starts_with('^') => {
                        parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
                    }
                    _ => Ok((field.clone(), is_global, false)),
                }
            }
            /// Gets the next value in the char stream.
            pub fn parse_value
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                line: usize,
                col: usize,
                depth: usize,
                cur_brace: Option<char>,
                is_first: bool,
            ) -> ParseResult<Value>
            {
                // Peek to determine what kind of value we'll be parsing.
                let res = match stream.peek().unwrap() {
                    '"' => parse_str(&mut stream)?,
                    '\'' => parse_char(&mut stream)?,
                    '{' => parse_obj(&mut stream, &mut globals, included, depth + 1)?,
                    '[' => parse_arr(&mut stream, obj, &mut globals, included, depth + 1)?,
                    '(' => parse_tup(&mut stream, obj, &mut globals, included, depth + 1)?,
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
                    '<' => parse_include(&mut stream, obj, &mut globals, &mut included, depth + 1)?,
                    ch @ '+' | ch @ '-' => {
                        parse_unary_op(&mut stream, obj, globals, included, depth, cur_brace, ch)?
                    }
                    ch if is::numeric_char(ch) => parse_numeric(&mut stream, line, col)?,
                    ch if Obj::is_valid_field_char(ch, true) => parse_variable(
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
                        return parse_err(stream.file(), InvalidValueChar(ch, line, col));
                    }
                };

                // Process operations if this is the first value.
                if is_first {
                    let mut val_deque: VecDeque<(Value, usize, usize)> = VecDeque::new();
                    let mut op_deque: VecDeque<char> = VecDeque::new();
                    val_deque.push_back((res, line, col));

                    loop {
                        match stream.peek() {
                            Some(ch) if is::operator(ch) => {
                                let _ = stream.next();
                                if stream.peek().is_none() {
                                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                                }

                                let (line2, col2) = (stream.line(), stream.col());

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

                                if is::priority_operator(ch) {
                                    let (val1, line1, col1) = val_deque.pop_back().unwrap();
                                    let res = binary_op_on_values(stream, val1, val2, ch, line2, col2)?;
                                    val_deque.push_back((res, line1, col1));
                                } else {
                                    val_deque.push_back((val2, line2, col2));
                                    op_deque.push_back(ch);
                                }
                            }
                            _ => break,
                        }
                    }

                    // Check for valid characters after the value.
                    check_value_end(stream, cur_brace)?;

                    let (mut val1, _, _) = val_deque.pop_front().unwrap();
                    while !op_deque.is_empty() {
                        let (val2, line2, col2) = val_deque.pop_front().unwrap();
                        val1 = binary_op_on_values(
                            stream,
                            val1,
                            val2,
                            op_deque.pop_front().unwrap(),
                            line2,
                            col2,
                        )?;
                    }
                    Ok(val1)
                } else {
                    Ok(res)
                }
            }

            fn parse_unary_op
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                depth: usize,
                cur_brace: Option<char>,
                ch: char,
            ) -> ParseResult<Value>
            {
                let _ = stream.next();
                let line = stream.line();
                let col = stream.col();

                let res = match stream.peek() {
                    Some(_) => parse_value(
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
                    None => return parse_err(stream.file(), UnexpectedEnd(line)),
                };
                unary_op_on_value(stream, res, ch, line, col)
            }
            /// Gets the next numeric (either Int or Frac) in the character stream.
            fn parse_numeric(stream: &mut CharStream, line: usize, col: usize) -> ParseResult<Value>
            {
                let mut s1 = String::new();
                let mut s2 = String::new();
                let mut dec = false;
                let mut under = false;

                while let Some(ch) = stream.peek() {
                    match ch {
                        ch if is::value_end_char(ch) => break,
                        ch if is::char_digit(ch) => {
                            if !dec {
                                s1.push(ch);
                            } else {
                                s2.push(ch);
                            }
                        }
                        '.' | ',' => {
                            if !dec {
                                dec = true;
                            } else {
                                return parse_err(
                                    stream.file(),
                                    InvalidValueChar(ch, stream.line(), stream.col()),
                                );
                            }
                        }
                        '_' => {
                            if !under {
                                under = true;
                            } else {
                                return parse_err(
                                    stream.file(),
                                    InvalidValueChar(ch, stream.line(), stream.col()),
                                );
                            }
                        }
                        _ => {
                            return parse_err(
                                stream.file(),
                                InvalidValueChar(ch, stream.line(), stream.col()),
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
                        return parse_err(stream.file(), InvalidNumeric(line, col));
                    }

                    let whole: BigInt = if s1.is_empty() {
                        0u8.into()
                    } else {
                        s1.parse()?
                    };

                    // Remove trailing zeros.
                    let s2 = s2.trim_end_matches('0');

                    let (decimal, dec_len): (BigInt, usize) = if s2.is_empty() {
                        (0u8.into(), 1)
                    } else {
                        (s2.parse()?, s2.len())
                    };

                    let f = frac_from_whole_and_dec(whole, decimal, dec_len);
                    Ok(f.into())
                } else {
                    // Parse an Int.
                    if s1.is_empty() {
                        return parse_err(stream.file(), InvalidNumeric(line, col));
                    }

                    let i: BigInt = s1.parse()?;
                    Ok(i.into())
                }
            }
            /// Parses a variable name and gets a value from the corresponding variable.
            fn parse_variable
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                line: usize,
                col: usize,
                depth: usize,
                cur_brace: Option<char>,
            ) -> ParseResult<Value>
            {
                let mut var = String::new();
                let mut is_global = false;
                let mut dot = false;
                let mut dot_global = false;

                let ch = stream.peek().unwrap();
                if ch == '@' {
                    let ch = stream.next().unwrap();
                    is_global = true;
                    var.push(ch);
                }

                while let Some(ch) = stream.peek() {
                    match ch {
                        '.' => {
                            let _ = stream.next();
                            match stream.peek() {
                                Some('@') => dot_global = true,
                                Some(ch) if Obj::is_valid_field_char(ch, true) || is::numeric_char(ch) => (),
                                Some(ch) => {
                                    return parse_err(
                                        stream.file(),
                                        InvalidValueChar(ch, stream.line(), stream.col()),
                                    );
                                }
                                None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                            }

                            dot = true;
                            break;
                        }
                        ch if is::value_end_char(ch) => break,
                        ch if Obj::is_valid_field_char(ch, false) => {
                            let _ = stream.next();
                            var.push(ch);
                        }
                        ch => {
                            return parse_err(
                                stream.file(),
                                InvalidValueChar(ch, stream.line(), stream.col()),
                            );
                        }
                    }
                }

                let mut value = match var.as_str() {
                    "null" => Value::Null,
                    "true" => Value::Bool(true),
                    "false" => Value::Bool(false),

                    "Obj" => Value::Obj(OBJ_SENTINEL.clone()),
                    "Str" => Value::Obj(STR_SENTINEL.clone()),
                    "Arr" => Value::Obj(ARR_SENTINEL.clone()),
                    "Tup" => Value::Obj(TUP_SENTINEL.clone()),

                    var @ "@" => return parse_err(stream.file(), InvalidValue(var.into(), line, col)),
                    var if is_global => {
                        // Global variable, get value from globals map.
                        match globals.get(var) {
                            Some(value) => value.clone(),
                            None => {
                                let var = String::from(var);
                                return parse_err(stream.file(), GlobalNotFound(var, line, col));
                            }
                        }
                    }
                    var => {
                        // Regular variable, get value from the current Obj.
                        match obj.get(var) {
                            Some(value) => value.clone(),
                            None => {
                                let var = String::from(var);
                                return parse_err(stream.file(), VariableNotFound(var, line, col));
                            }
                        }
                    }
                };

                if dot {
                    value = match value {
                        Value::Arr(arr) => {
                            let (line, col) = (stream.line(), stream.col());
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
                                Value::Int(int) => match int.to_usize() {
                                    Some(index) => arr
                                        .get(index)
                                        .map_err(|e| ParseError::from_over(&e, stream.file(), line, col))?,
                                    None => return parse_err(stream.file(), InvalidIndex(int, line, col)),
                                },
                                _ => {
                                    return parse_err(
                                        stream.file(),
                                        ExpectedType(Type::Int, value.get_type(), line, col),
                                    );
                                }
                            }
                        }
                        Value::Tup(tup) => {
                            let (line, col) = (stream.line(), stream.col());
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
                                Value::Int(int) => match int.to_usize() {
                                    Some(index) => tup
                                        .get(index)
                                        .map_err(|e| ParseError::from_over(&e, stream.file(), line, col))?,
                                    None => return parse_err(stream.file(), InvalidIndex(int, line, col)),
                                },
                                _ => {
                                    return parse_err(
                                        stream.file(),
                                        ExpectedType(Type::Int, value.get_type(), line, col),
                                    );
                                }
                            }
                        }
                        Value::Obj(obj) => {
                            let (line, col) = (stream.line(), stream.col());

                            if dot_global {
                                return parse_err(stream.file(), InvalidValueChar('@', line, col));
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
                        _ => return parse_err(stream.file(), InvalidDot(value.get_type(), line, col)),
                    }
                }

                Ok(value)
            }
            /// Gets the next Char in the character stream.
            pub fn parse_char(stream: &mut CharStream) -> ParseResult<Value>
            {
                let ch = stream.next().unwrap();
                assert_eq!(ch, '\'');

                let (escape, mut ch) = match stream.next() {
                    Some('\\') => (true, '\0'),
                    Some(ch) if ch == '\n' || ch == '\r' || ch == '\t' => {
                        return parse_err(
                            stream.file(),
                            InvalidValueChar(ch, stream.line(), stream.col() - 1),
                        );
                    }
                    Some(ch) => (false, ch),
                    None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                };

                if escape {
                    ch = match stream.next() {
                        Some(ch) => match char::get_escape(ch) {
                            Some(ch) => ch,
                            None => {
                                return parse_err(
                                    stream.file(),
                                    InvalidEscapeChar(ch, stream.line(), stream.col() - 1),
                                );
                            }
                        },
                        None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                    }
                }

                match stream.next() {
                    Some('\'') => (),
                    Some(ch) => {
                        return parse_err(
                            stream.file(),
                            InvalidValueChar(ch, stream.line(), stream.col() - 1),
                        );
                    }
                    None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                }

                Ok(ch.into())
            }

            fn parse_str_file(path: &str) -> ParseResult<String>
            {
                let s = str::read_file(path)?.replace("\r\n", "\n");
                Ok(s)
            }
            /// Gets the next Str in the character stream.
            pub fn parse_str(stream: &mut CharStream) -> ParseResult<Value>
            {
                let ch = stream.next().unwrap();
                assert_eq!(ch, '"');

                let mut s = String::new();
                let mut escape = false;

                loop {
                    match stream.next() {
                        Some(ch) => {
                            if escape {
                                match char::get_escape(ch) {
                                    Some(ch) => s.push(ch),
                                    None => {
                                        return parse_err(
                                            stream.file(),
                                            InvalidEscapeChar(ch, stream.line(), stream.col() - 1),
                                        );
                                    }
                                }
                                escape = false;
                            } else {
                                match ch {
                                    '"' => break,
                                    '\\' => escape = true,
                                    _ => s.push(ch),
                                }
                            }
                        }
                        None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                    }
                }

                // Replace \r\n line endings with \n for consistency in internal handling.
                let s = s.replace("\r\n", "\n");

                Ok(s.into())
            }

            pub fn parse_include
            (
                mut stream: &mut CharStream,
                obj: &ObjMap,
                mut globals: &mut GlobalMap,
                mut included: &mut IncludedMap,
                depth: usize,
            ) -> ParseResult<Value>
            {
                enum IncludeType {
                    Obj,
                    Str,
                    Arr,
                    Tup,
                }

                // Check depth.
                if depth > MAX_DEPTH {
                    return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
                }

                let ch = stream.next().unwrap();
                assert_eq!(ch, '<');

                // Go to the next non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                let (mut line, mut col) = (stream.line(), stream.col());
                let mut value = parse_value(
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    line,
                    col,
                    depth,
                    Some('>'),
                    true,
                )?;

                let mut include_type = IncludeType::Obj; // Default include type if no token is present.
                let mut parse_again = true; // True if an include token was found.
                match value {
                    Value::Obj(ref obj) if obj.ptr_eq(&OBJ_SENTINEL) => include_type = IncludeType::Obj,
                    Value::Obj(ref obj) if obj.ptr_eq(&STR_SENTINEL) => include_type = IncludeType::Str,
                    Value::Obj(ref obj) if obj.ptr_eq(&ARR_SENTINEL) => include_type = IncludeType::Arr,
                    Value::Obj(ref obj) if obj.ptr_eq(&TUP_SENTINEL) => include_type = IncludeType::Tup,
                    Value::Str(_) => parse_again = false,
                    _ => {
                        return parse_err(
                            stream.file(),
                            InvalidIncludeToken(value.get_type(), line, col),
                        );
                    }
                }

                if parse_again {
                    // Go to the next non-whitespace character, or error if there is none.
                    if !find_char(stream.clone()) {
                        return parse_err(stream.file(), UnexpectedEnd(stream.line()));
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
                        Some('>'),
                        true,
                    )?;
                }

                // Go to the next non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                match stream.next().unwrap() {
                    '>' => (),
                    ch => {
                        return parse_err(
                            stream.file(),
                            InvalidClosingBracket(Some('>'), ch, stream.line(), stream.col() - 1),
                        );
                    }
                }

                // Get the full path of the include file.
                let include_file = match value {
                    Value::Str(s) => s,
                    _ => {
                        return parse_err(
                            stream.file(),
                            ExpectedType(Type::Str, value.get_type(), line, col),
                        );
                    }
                };

                let pathbuf = match stream.file().as_ref() {
                    Some(file) => Path::new(file)
                        .parent()
                        .unwrap()
                        .join(Path::new(&include_file)),
                    None => Path::new(&include_file).to_path_buf(),
                };
                let path = pathbuf.as_path();
                if !path.is_file() {
                    return parse_err(stream.file(), InvalidIncludePath(include_file, line, col));
                }

                // Get the include file as a path relative to the current working directory.
                let path_str = match path.to_str() {
                    Some(path) => path,
                    None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
                };

                // Get the include file as an absolute path.
                let path = match path.canonicalize() {
                    Ok(path) => path,
                    Err(_) => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
                };
                let full_path_str = match path.to_str() {
                    Some(path) => path,
                    None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
                };

                // Prevent cyclic includes by temporarily storing the current file path.
                let storing = if let Some(file) = stream.file() {
                    let full_file = String::from(Path::new(&file).canonicalize().unwrap().to_str().unwrap());
                    included.1.insert(full_file.clone());
                    Some(full_file)
                } else {
                    None
                };
                if included.1.contains(full_path_str) {
                    return parse_err(stream.file(), CyclicInclude(include_file, line, col));
                }

                // Get either the tracked value or parse it if it's our first time seeing the include.
                let value = if included.0.contains_key(full_path_str) {
                    let value = &included.0[full_path_str];
                    value.clone()
                } else {
                    let value: Value = match include_type {
                        IncludeType::Obj => parse_obj_file_includes(path_str, included)?.into(),
                        IncludeType::Str => parse_str_file(path_str)?.into(),
                        IncludeType::Arr => parse_arr_file(path_str, included)?.into(),
                        IncludeType::Tup => parse_tup_file(path_str, included)?.into(),
                    };
                    // Use full path as included key.
                    included.0.insert(full_path_str.into(), value.clone());
                    value
                };

                // Remove the stored file path.
                if let Some(file) = storing {
                    included.1.remove(&file);
                }

                Ok(value)
            }
            /// Tries to perform a unary operation on a single value.
            pub fn unary_op_on_value
            (
                stream: &CharStream,
                val: Value,
                op: char,
                line: usize,
                col: usize,
            ) -> ParseResult<Value>
            {
                use crate::database::types::Type::*;

                let t = val.get_type();

                Ok(match op {
                    '+' => match t {
                        Int | Frac => val,
                        _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
                    },
                    '-' => match t {
                        Int => (-val.get_int().unwrap()).into(),
                        Frac => (-val.get_frac().unwrap()).into(),
                        _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
                    },
                    _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
                })
            }
            /// Tries to perform an operation on two values.
            pub fn binary_op_on_values
            (
                stream: &CharStream,
                mut val1: Value,
                mut val2: Value,
                op: char,
                line: usize,
                col: usize,
            ) -> ParseResult<Value>
            {
                use ::database::types::Type::*;

                let (mut type1, mut type2) = (val1.get_type(), val2.get_type());
                
                if type1 == Int && type2 == Frac 
                {
                    val1 = Value::Frac(BigRational::new(val1.get_int().unwrap(), 1.into()));
                    type1 = Frac;
                }
                
                else if type1 == Frac && type2 == Int 
                {
                    val2 = Value::Frac(BigRational::new(val2.get_int().unwrap(), 1.into()));
                    type2 = Frac;
                }

                Ok(match op 
                {
                    '+' => 
                    {
                        match type1 
                        {
                            Int if type2 == Int => (val1.get_int().unwrap() + val2.get_int().unwrap()).into(),
                            Frac if type2 == Frac => 
                            {
                                (val1.get_frac().unwrap() + val2.get_frac().unwrap()).into()
                            }
                            Char if type2 == Char => 
                            {
                                let mut s = String::with_capacity(2);
                                s.push(val1.get_char().unwrap());
                                s.push(val2.get_char().unwrap());
                                s.into()
                            }
                            Char if type2 == Str => 
                            {
                                let str2 = val2.get_str().unwrap();
                                let mut s = String::with_capacity(1 + str2.len());
                                s.push(val1.get_char().unwrap());
                                s.push_str(&str2);
                                s.into()
                            }
                            Str if type2 == Char => 
                            {
                                let str1 = val1.get_str().unwrap();
                                let mut s = String::with_capacity(str1.len() + 1);
                                s.push_str(&str1);
                                s.push(val2.get_char().unwrap());
                                s.into()
                            }
                            Str if type2 == Str => 
                            {
                                let str1 = val1.get_str().unwrap();
                                let str2 = val2.get_str().unwrap();
                                let mut s = String::with_capacity(str1.len() + str2.len());
                                s.push_str(&str1);
                                s.push_str(&str2);
                                s.into()
                            }
                            Arr(_) => 
                            {
                                match Type::most_specific(&type1, &type2) 
                                {
                                    Some((t, _)) => 
                                    {
                                        let (arr1, arr2) = (val1.get_arr().unwrap(), val2.get_arr().unwrap());
                                        let (mut vec1, mut vec2) =
                                            (arr1.vec_ref().clone(), arr2.vec_ref().clone());

                                        let mut vec = Vec::with_capacity(vec1.len() + vec2.len());
                                        vec.append(&mut vec1);
                                        vec.append(&mut vec2);

                                        // Get the inner type.
                                        let arr = if let Arr(ref t) = t 
                                        {
                                            // Because we know the type already, we can safely use `_unchecked`.
                                            arrays::Arr::from_vec_unchecked(vec, t.deref().clone())
                                        } else 
                                        {
                                            panic!("Logic error")
                                        };

                                        arr.into()
                                    }
                                    None => {
                                        return parse_err
                                        (
                                            stream.file(),
                                            BinaryOperatorError(type1, type2, op, line, col),
                                        );
                                    }
                                }
                            }
                            _ => {
                                return parse_err
                                (
                                    stream.file(),
                                    BinaryOperatorError(type1, type2, op, line, col),
                                );
                            }
                        }
                    }
                    '-' => match type1 
                    {
                        Int if type2 == Int => (val1.get_int().unwrap() - val2.get_int().unwrap()).into(),
                        Frac if type2 == Frac => (val1.get_frac().unwrap() - val2.get_frac().unwrap()).into(),
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    }
                    ,
                    '*' => match type1 
                    {
                        Int if type2 == Int => (val1.get_int().unwrap() * val2.get_int().unwrap()).into(),
                        Frac if type2 == Frac => (val1.get_frac().unwrap() * val2.get_frac().unwrap()).into(),
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    }
                    ,
                    '/' => match type1 
                    {
                        Int if type2 == Int => {
                            let (int1, int2) = (val1.get_int().unwrap(), val2.get_int().unwrap());
                            if int2.is_zero() {
                                return parse_err(stream.file(), InvalidNumeric(line, col));
                            }
                            BigRational::new(int1, int2).into()
                        }
                        Frac if type2 == Frac => {
                            let (frac1, frac2) = (val1.get_frac().unwrap(), val2.get_frac().unwrap());
                            if frac2.is_zero() {
                                return parse_err(stream.file(), InvalidNumeric(line, col));
                            }
                            (frac1 / frac2).into()
                        }
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    }
                    ,
                    '%' => match type1 
                    {
                        Int if type2 == Int => {
                            let int2 = val2.get_int().unwrap();
                            if int2.is_zero() {
                                return parse_err(stream.file(), InvalidNumeric(line, col));
                            }
                            (val1.get_int().unwrap() % int2).into()
                        }
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    },
                    _ => 
                    {
                        return parse_err(
                            stream.file(),
                            BinaryOperatorError(type1, type2, op, line, col),
                        );
                    }
                })
            }
            /// Finds the next non-whitespace character, ignoring comments, and update stream position.
            pub fn find_char(mut stream: CharStream) -> bool
            {
                while let Some(ch) = stream.peek() 
                {
                    match ch 
                    {
                        '#' => 
                        {
                            // Comment found; eat the rest of the line.
                            loop {
                                let ch = stream.next();
                                if ch.is_none() 
                                {
                                    return false;
                                }
                                if ch.unwrap() == '\n' 
                                {
                                    break;
                                }
                            }
                        }

                        ch if ch.is_whitespace() => 
                        {
                            let _ = stream.next();
                        }

                        _ => return true,
                    }
                }

                false
            }
            /// Helper function to make sure values are followed by a correct end delimiter.
            pub fn check_value_end(stream: &CharStream, cur_brace: Option<char>) -> ParseResult<()>
            {
                match stream.peek() 
                {
                    Some(ch) => match ch 
                    {
                        ch if is::value_end_char(ch) => 
                        {
                            if is::end_delimiter(ch) && Some(ch) != cur_brace 
                            {
                                parse_err(
                                    stream.file(),
                                    InvalidClosingBracket(cur_brace, ch, stream.line(), stream.col()),
                                )
                            } else 
                            {
                                Ok(())
                            }
                        }

                        ch => parse_err
                        (
                            stream.file(),
                            InvalidValueChar(ch, stream.line(), stream.col()),
                        ),
                    },
                    None => Ok(()),
                }
            }
        }
        
        use super::Obj;

        pub type ParseResult<T> = Result<T, ParseError>;
        pub const MAX_DEPTH: usize = 64;
        /// Load an `Obj` from a file.
        pub fn load_from_file(path: &str) -> ParseResult<Obj> { parser::parse_obj_file(path) }
        /// Load an `Obj` from a &str.
        pub fn load_from_str(contents: &str) -> ParseResult<Obj> { parser::parse_obj_str(contents) }
    }
}

pub mod default
{
    pub use std::default::{ * };
}

pub mod env
{
    pub use std::env::{ * };
}

pub mod error
{
    pub use std::error::{ * };

    pub mod no
    {
        use ::
        {
            *,
        };
    }
}

pub mod fmt
{
    pub use std::fmt::{ * };
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

pub mod is
{
    use ::
    {
        *,
    };
    /*
    pub fn is_alphabetic(...) -> bool */
    /// Tests if byte is ASCII alphabetic: A-Z, a-z
    #[inline] pub fn alphabetic(chr: u8) -> bool { (chr >= 0x41 && chr <= 0x5A) || (chr >= 0x61 && chr <= 0x7A) }
    /*
    pub fn is_digit(...) -> bool */
    /// Tests if byte is ASCII digit: 0-9
    #[inline] pub fn digit(chr: u8) -> bool { chr >= 0x30 && chr <= 0x39 }
    /*
    pub fn is_hex_digit(...) -> bool */
    /// Tests if byte is ASCII hex digit: 0-9, A-F, a-f
    #[inline] pub fn hex_digit(chr: u8) -> bool 
    { (chr >= 0x30 && chr <= 0x39) || (chr >= 0x41 && chr <= 0x46) || (chr >= 0x61 && chr <= 0x66) }
    /*
    pub fn is_oct_digit(...) -> bool */
    /// Tests if byte is ASCII octal digit: 0-7
    #[inline] pub fn oct_digit(chr: u8) -> bool { chr >= 0x30 && chr <= 0x37 }
    /*
    pub fn is_alphanumeric(...) -> bool */
    /// Tests if byte is ASCII alphanumeric: A-Z, a-z, 0-9
    #[inline] pub fn alphanumeric(chr: u8) -> bool { alphabetic(chr) || digit(chr) }
    /*
    pub fn is_space(...) -> bool */
    /// Tests if byte is ASCII space or tab
    #[inline] pub fn space(chr: u8) -> bool { chr == b' ' || chr == b'\t' }
    /*
    pub fn is_newline(...) -> bool */
    /// Tests if byte is ASCII newline: \n
    #[inline] pub fn newline(chr: u8) -> bool { chr == b'\n' }
    /*
    pub fn is_value_end_char(...) -> bool */
    /// Returns true if this character signifies the legal end of a value.
    pub fn value_end_char(ch: char) -> bool {
        whitespace(ch) || end_delimiter(ch) || operator(ch)
    }
    /*
    pub fn is_whitespace(...) -> bool */
    /// Returns true if the character is either whitespace or '#' (start of a comment).
    pub fn whitespace(ch: char) -> bool {
        ch.is_whitespace() || ch == '#'
    }
    /*
    pub fn is_end_delimiter(...) -> bool */
    pub fn end_delimiter(ch: char) -> bool {
        match ch {
            ')' | ']' | '}' | '>' => true,
            _ => false,
        }
    }
    /*
    pub fn is_numeric_char(...) -> bool */
    pub fn numeric_char(ch: char) -> bool {
        match ch {
            _ch if is::char_digit(_ch) => true,
            '.' | ',' => true,
            _ => false,
        }
    }
    /*
    pub fn is_priority_operator(...) -> bool */
    pub fn priority_operator(ch: char) -> bool {
        match ch {
            '*' | '/' | '%' => true,
            _ => false,
        }
    }
    /*
    pub fn is_operator(...) -> bool */
    pub fn operator(ch: char) -> bool
    {
        match ch {
            '+' | '-' | '*' | '/' | '%' => true,
            _ => false,
        }
    }
    /*
    pub fn is_reserved(...) -> bool */
    pub fn reserved(field: &str) -> bool {
        match field {
            "@" | "null" | "true" | "false" | "Obj" | "Str" | "Arr" | "Tup" => true,
            _ => false,
        }
    }
    /*
    pub fn is_digit(...) -> bool */
    /// Returns true if `ch` is an ASCII decimal digit.
    pub fn char_digit(ch: char) -> bool
    {
        match ch
        {
            '0'..='9' => true,
            _ => false,
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
}

pub mod num
{
    pub use std::num::{ * };
    pub mod big
    {
        //! Big Integer Types for Rust
        use ::
        {
            *,
        };

        #[macro_use] pub mod macros
        {
            use ::
            {
                *,
            };

            macro_rules! cfg_32 {
                ($($any:tt)+) => {
                    #[cfg(not(target_pointer_width = "64"))] $($any)+
                }
            }

            macro_rules! cfg_32_or_test {
                ($($any:tt)+) => {
                    #[cfg(any(not(target_pointer_width = "64"), test))] $($any)+
                }
            }

            macro_rules! cfg_64 {
                ($($any:tt)+) => {
                    #[cfg(target_pointer_width = "64")] $($any)+
                }
            }

            macro_rules! cfg_digit {
                ($item32:item $item64:item) => {
                    cfg_32!($item32);
                    cfg_64!($item64);
                };
            }

            macro_rules! cfg_digit_expr {
                ($expr32:expr, $expr64:expr) => {
                    cfg_32!($expr32);
                    cfg_64!($expr64);
                };
            }

            macro_rules! forward_val_val_binop {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            // forward to val-ref
                            $imp::$method(self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_val_val_binop_commutative {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            // forward to val-ref, with the larger capacity as val
                            if self.capacity() >= other.capacity() {
                                $imp::$method(self, &other)
                            } else {
                                $imp::$method(other, &self)
                            }
                        }
                    }
                };
            }

            macro_rules! forward_ref_val_binop {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            // forward to ref-ref
                            $imp::$method(self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_val_binop_commutative {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            // reverse, forward to val-ref
                            $imp::$method(other, self)
                        }
                    }
                };
            }

            macro_rules! forward_val_ref_binop {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<&$res> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            // forward to ref-ref
                            $imp::$method(&self, other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_ref_binop {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<&$res> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            // forward to val-ref
                            $imp::$method(self.clone(), other)
                        }
                    }
                };
            }

            macro_rules! forward_ref_ref_binop_commutative {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<&$res> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            // forward to val-ref, choosing the larger to clone
                            if self.len() >= other.len() {
                                $imp::$method(self.clone(), other)
                            } else {
                                $imp::$method(other.clone(), self)
                            }
                        }
                    }
                };
            }

            macro_rules! forward_val_assign {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    impl $imp<$res> for $res {
                        #[inline]
                        fn $method(&mut self, other: $res) {
                            self.$method(&other);
                        }
                    }
                };
            }

            macro_rules! forward_val_assign_scalar {
                (impl $imp:ident for $res:ty, $scalar:ty, $method:ident) => {
                    impl $imp<$res> for $scalar {
                        #[inline]
                        fn $method(&mut self, other: $res) {
                            self.$method(&other);
                        }
                    }
                };
            }

            /// use this if val_val_binop is already implemented and the reversed order is required
            macro_rules! forward_scalar_val_val_binop_commutative {
                (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) => {
                    impl $imp<$res> for $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(other, self)
                        }
                    }
                };
            }

            // Forward scalar to ref-val, when reusing storage is not helpful
            macro_rules! forward_scalar_val_val_binop_to_ref_val {
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

            macro_rules! forward_scalar_ref_ref_binop_to_ref_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl $imp<&$scalar> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self, *other)
                        }
                    }

                    impl $imp<&$res> for &$scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(*self, other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_val_ref_binop_to_ref_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl $imp<&$scalar> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(&self, *other)
                        }
                    }

                    impl $imp<$res> for &$scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(*self, &other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_val_ref_binop_to_val_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl $imp<&$scalar> for $res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self, *other)
                        }
                    }

                    impl $imp<$res> for &$scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $res) -> $res {
                            $imp::$method(*self, other)
                        }
                    }
                };
            }

            macro_rules! forward_scalar_ref_val_binop_to_val_val {
                (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) => {
                    impl $imp<$scalar> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: $scalar) -> $res {
                            $imp::$method(self.clone(), other)
                        }
                    }

                    impl $imp<&$res> for $scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(self, other.clone())
                        }
                    }
                };
            }

            macro_rules! forward_scalar_ref_ref_binop_to_val_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    impl $imp<&$scalar> for &$res {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$scalar) -> $res {
                            $imp::$method(self.clone(), *other)
                        }
                    }

                    impl $imp<&$res> for &$scalar {
                        type Output = $res;

                        #[inline]
                        fn $method(self, other: &$res) -> $res {
                            $imp::$method(*self, other.clone())
                        }
                    }
                };
            }

            macro_rules! promote_scalars {
                (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
                    $(
                        forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);

                        impl $imp<$scalar> for $res {
                            type Output = $res;

                            #[allow(clippy::cast_lossless)]
                            #[inline]
                            fn $method(self, other: $scalar) -> $res {
                                $imp::$method(self, other as $promo)
                            }
                        }

                        impl $imp<$res> for $scalar {
                            type Output = $res;

                            #[allow(clippy::cast_lossless)]
                            #[inline]
                            fn $method(self, other: $res) -> $res {
                                $imp::$method(self as $promo, other)
                            }
                        }
                    )*
                }
            }
            macro_rules! promote_scalars_assign {
                (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
                    $(
                        impl $imp<$scalar> for $res {
                            #[allow(clippy::cast_lossless)]
                            #[inline]
                            fn $method(&mut self, other: $scalar) {
                                self.$method(other as $promo);
                            }
                        }
                    )*
                }
            }

            macro_rules! promote_unsigned_scalars {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars!(impl $imp<u32> for $res, $method, u8, u16);
                    promote_scalars!(impl $imp<UsizePromotion> for $res, $method, usize);
                }
            }

            macro_rules! promote_unsigned_scalars_assign {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars_assign!(impl $imp<u32> for $res, $method, u8, u16);
                    promote_scalars_assign!(impl $imp<UsizePromotion> for $res, $method, usize);
                }
            }

            macro_rules! promote_signed_scalars {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars!(impl $imp<i32> for $res, $method, i8, i16);
                    promote_scalars!(impl $imp<IsizePromotion> for $res, $method, isize);
                }
            }

            macro_rules! promote_signed_scalars_assign {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_scalars_assign!(impl $imp<i32> for $res, $method, i8, i16);
                    promote_scalars_assign!(impl $imp<IsizePromotion> for $res, $method, isize);
                }
            }

            // Forward everything to ref-ref, when reusing storage is not helpful
            macro_rules! forward_all_binop_to_ref_ref {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop!(impl $imp for $res, $method);
                    forward_val_ref_binop!(impl $imp for $res, $method);
                    forward_ref_val_binop!(impl $imp for $res, $method);
                };
            }

            // Forward everything to val-ref, so LHS storage can be reused
            macro_rules! forward_all_binop_to_val_ref {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop!(impl $imp for $res, $method);
                    forward_ref_val_binop!(impl $imp for $res, $method);
                    forward_ref_ref_binop!(impl $imp for $res, $method);
                };
            }

            // Forward everything to val-ref, commutatively, so either LHS or RHS storage can be reused
            macro_rules! forward_all_binop_to_val_ref_commutative {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    forward_val_val_binop_commutative!(impl $imp for $res, $method);
                    forward_ref_val_binop_commutative!(impl $imp for $res, $method);
                    forward_ref_ref_binop_commutative!(impl $imp for $res, $method);
                };
            }

            macro_rules! forward_all_scalar_binop_to_ref_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_val_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_val_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! forward_all_scalar_binop_to_val_val {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_val_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                    forward_scalar_ref_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! forward_all_scalar_binop_to_val_val_commutative {
                (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
                    forward_scalar_val_val_binop_commutative!(impl $imp<$scalar> for $res, $method);
                    forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
                }
            }

            macro_rules! promote_all_scalars {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_unsigned_scalars!(impl $imp for $res, $method);
                    promote_signed_scalars!(impl $imp for $res, $method);
                }
            }

            macro_rules! promote_all_scalars_assign {
                (impl $imp:ident for $res:ty, $method:ident) => {
                    promote_unsigned_scalars_assign!(impl $imp for $res, $method);
                    promote_signed_scalars_assign!(impl $imp for $res, $method);
                }
            }

            macro_rules! impl_sum_iter_type {
                ($res:ty) => {
                    impl<T> Sum<T> for $res
                    where
                        $res: Add<T, Output = $res>,
                    {
                        fn sum<I>(iter: I) -> Self
                        where
                            I: Iterator<Item = T>,
                        {
                            iter.fold(Self::ZERO, <$res>::add)
                        }
                    }
                };
            }

            macro_rules! impl_product_iter_type {
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
                cmp::Ordering::{self, Equal},
                default::Default,
                num::integers::{Integer, Roots},
                num::traits::{ConstZero, Num, One, Pow, Signed, Zero},
                ops::{Neg, Not},
                string::String,
                vec::Vec,
                *,
            };

            use self::Sign::{Minus, NoSign, Plus};

            use super::big_digit::BigDigit;
            use super::biguint::to_str_radix_reversed;
            use super::biguint::{BigUint, IntDigits, U32Digits, U64Digits};

            pub mod addition
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    iter::Sum,
                    num::
                    {
                        big::{ IsizePromotion, UsizePromotion, },
                        traits::{ CheckedAdd },
                    },
                    ops::{Add, AddAssign},
                    *,
                };

                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::{Minus, NoSign, Plus};
                use super::{BigInt, UnsignedAbs};
                
                macro_rules! bigint_add
                {
                    ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) =>
                    {
                        match ($a.sign, $b.sign)
                        {
                            (_, NoSign) => $a_owned,
                            (NoSign, _) => $b_owned,
                            // same sign => keep the sign with the sum of magnitudes
                            (Plus, Plus) | (Minus, Minus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                            // opposite signs => keep the sign of the larger with the difference of magnitudes
                            (Plus, Minus) | (Minus, Plus) => match $a.data.cmp(&$b.data) {
                                Less => BigInt::from_biguint($b.sign, $b_data - $a_data),
                                Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                                Equal => BigInt::ZERO,
                            },
                        }
                    };
                }

                impl Add<&BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: &BigInt) -> BigInt {
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

                impl Add<BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: BigInt) -> BigInt {
                        bigint_add!(self, self.clone(), &self.data, other, other, other.data)
                    }
                }

                impl Add<&BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: &BigInt) -> BigInt {
                        bigint_add!(self, self, self.data, other, other.clone(), &other.data)
                    }
                }

                impl Add<BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: BigInt) -> BigInt {
                        bigint_add!(self, self, self.data, other, other, other.data)
                    }
                }

                impl AddAssign<&BigInt> for BigInt {
                    #[inline] fn add_assign(&mut self, other: &BigInt) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }
                forward_val_assign!(impl AddAssign for BigInt, add_assign);

                promote_all_scalars!(impl Add for BigInt, add);
                promote_all_scalars_assign!(impl AddAssign for BigInt, add_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u32> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u64> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u128> for BigInt, add);

                impl Add<u32> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u32) -> BigInt {
                        match self.sign {
                            NoSign => From::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }

                impl AddAssign<u32> for BigInt {
                    #[inline] fn add_assign(&mut self, other: u32) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }

                impl Add<u64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u64) -> BigInt {
                        match self.sign {
                            NoSign => From::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }

                impl AddAssign<u64> for BigInt {
                    #[inline] fn add_assign(&mut self, other: u64) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }

                impl Add<u128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u128) -> BigInt {
                        match self.sign {
                            NoSign => BigInt::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }
                impl AddAssign<u128> for BigInt {
                    #[inline] fn add_assign(&mut self, other: u128) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }

                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i32> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i64> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i128> for BigInt, add);

                impl Add<i32> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i32> for BigInt {
                    #[inline] fn add_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }

                impl Add<i64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i64> for BigInt {
                    #[inline] fn add_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }

                impl Add<i128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i128> for BigInt {
                    #[inline] fn add_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }

                impl CheckedAdd for BigInt {
                    #[inline] fn checked_add(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.add(v))
                    }
                }

                impl_sum_iter_type!(BigInt);
            }

            pub mod division
            {
                use ::
                {
                    num::
                    {
                        big::{ IsizePromotion, UsizePromotion },
                        integers::{ Integer },
                        traits::{ CheckedDiv, CheckedEuclid, Euclid, Signed, ToPrimitive, Zero },
                    },
                    ops::{Div, DivAssign, Rem, RemAssign},
                    *,
                };

                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::NoSign;
                use super::{BigInt, UnsignedAbs};

                forward_all_binop_to_ref_ref!(impl Div for BigInt, div);

                impl Div<&BigInt> for &BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: &BigInt) -> BigInt {
                        let (q, _) = self.div_rem(other);
                        q
                    }
                }

                impl DivAssign<&BigInt> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: &BigInt) {
                        *self = &*self / other;
                    }
                }
                forward_val_assign!(impl DivAssign for BigInt, div_assign);

                promote_all_scalars!(impl Div for BigInt, div);
                promote_all_scalars_assign!(impl DivAssign for BigInt, div_assign);
                forward_all_scalar_binop_to_val_val!(impl Div<u32> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u64> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u128> for BigInt, div);

                impl Div<u32> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }

                impl DivAssign<u32> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: u32) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Div<BigInt> for u32 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }

                impl Div<u64> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }

                impl DivAssign<u64> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: u64) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Div<BigInt> for u64 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }

                impl Div<u128> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }

                impl DivAssign<u128> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: u128) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Div<BigInt> for u128 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }

                forward_all_scalar_binop_to_val_val!(impl Div<i32> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<i64> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<i128> for BigInt, div);

                impl Div<i32> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }

                impl DivAssign<i32> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }

                impl Div<BigInt> for i32 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }

                impl Div<i64> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }

                impl DivAssign<i64> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }

                impl Div<BigInt> for i64 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }

                impl Div<i128> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }

                impl DivAssign<i128> for BigInt 
                {
                    #[inline] fn div_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }

                impl Div<BigInt> for i128 
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }

                forward_all_binop_to_ref_ref!(impl Rem for BigInt, rem);

                impl Rem<&BigInt> for &BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: &BigInt) -> BigInt {
                        if let Some(other) = other.to_u32() {
                            self % other
                        } else if let Some(other) = other.to_i32() {
                            self % other
                        } else {
                            let (_, r) = self.div_rem(other);
                            r
                        }
                    }
                }

                impl RemAssign<&BigInt> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: &BigInt) {
                        *self = &*self % other;
                    }
                }
                forward_val_assign!(impl RemAssign for BigInt, rem_assign);

                promote_all_scalars!(impl Rem for BigInt, rem);
                promote_all_scalars_assign!(impl RemAssign for BigInt, rem_assign);
                forward_all_scalar_binop_to_val_val!(impl Rem<u32> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u64> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u128> for BigInt, rem);

                impl Rem<u32> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }

                impl RemAssign<u32> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: u32) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Rem<BigInt> for u32 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }

                impl Rem<u64> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }

                impl RemAssign<u64> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: u64) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Rem<BigInt> for u64 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }

                impl Rem<u128> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }

                impl RemAssign<u128> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: u128) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Rem<BigInt> for u128 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }

                forward_all_scalar_binop_to_val_val!(impl Rem<i32> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<i64> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<i128> for BigInt, rem);

                impl Rem<i32> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i32) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }

                impl RemAssign<i32> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: i32) {
                        *self %= other.unsigned_abs();
                    }
                }

                impl Rem<BigInt> for i32 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }

                impl Rem<i64> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i64) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }

                impl RemAssign<i64> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: i64) {
                        *self %= other.unsigned_abs();
                    }
                }

                impl Rem<BigInt> for i64 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }

                impl Rem<i128> for BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i128) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }

                impl RemAssign<i128> for BigInt 
                {
                    #[inline] fn rem_assign(&mut self, other: i128) {
                        *self %= other.unsigned_abs();
                    }
                }

                impl Rem<BigInt> for i128 
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }

                impl CheckedDiv for BigInt 
                {
                    #[inline] fn checked_div(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div(v))
                    }
                }

                impl CheckedEuclid for BigInt 
                {
                    #[inline] fn checked_div_euclid(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div_euclid(v))
                    }

                    #[inline] fn checked_rem_euclid(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.rem_euclid(v))
                    }
                    /*
                    fn checked_div_rem_euclid(&self, v: &Self) -> Option<(Self, Self)> {
                        Some(self.div_rem_euclid(v))
                    }
                    */
                }

                impl Euclid for BigInt 
                {
                    #[inline] fn div_euclid(&self, v: &BigInt) -> BigInt {
                        let (q, r) = self.div_rem(v);
                        if r.is_negative() {
                            if v.is_positive() {
                                q - 1
                            } else {
                                q + 1
                            }
                        } else {
                            q
                        }
                    }

                    #[inline] fn rem_euclid(&self, v: &BigInt) -> BigInt {
                        let r = self % v;
                        if r.is_negative() {
                            if v.is_positive() {
                                r + v
                            } else {
                                r - v
                            }
                        } else {
                            r
                        }
                    }

                    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
                        let (q, r) = self.div_rem(v);
                        if r.is_negative() {
                            if v.is_positive() {
                                (q - 1, r + v)
                            } else {
                                (q + 1, r - v)
                            }
                        } else {
                            (q, r)
                        }
                    }
                }
            }

            pub mod multiplication
            {
                use ::
                {
                    iter::Product,
                    num::
                    {
                        big::{ IsizePromotion, UsizePromotion },
                        traits::{ CheckedMul, One, Zero },
                    },
                    ops::{Mul, MulAssign},
                    *,
                };

                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::{self, Minus, NoSign, Plus};
                use super::{BigInt, UnsignedAbs};

                impl Mul<Sign> for Sign {
                    type Output = Sign;

                    #[inline] fn mul(self, other: Sign) -> Sign {
                        match (self, other) {
                            (NoSign, _) | (_, NoSign) => NoSign,
                            (Plus, Plus) | (Minus, Minus) => Plus,
                            (Plus, Minus) | (Minus, Plus) => Minus,
                        }
                    }
                }

                macro_rules! impl_mul {
                    ($(impl Mul<$Other:ty> for $Self:ty;)*) => {$(
                        impl Mul<$Other> for $Self {
                            type Output = BigInt;

                            #[inline]
                            fn mul(self, other: $Other) -> BigInt {
                                // automatically match value/ref
                                let BigInt { data: x, .. } = self;
                                let BigInt { data: y, .. } = other;
                                BigInt::from_biguint(self.sign * other.sign, x * y)
                            }
                        }
                    )*}
                }
                impl_mul! {
                    impl Mul<BigInt> for BigInt;
                    impl Mul<BigInt> for &BigInt;
                    impl Mul<&BigInt> for BigInt;
                    impl Mul<&BigInt> for &BigInt;
                }

                macro_rules! impl_mul_assign {
                    ($(impl MulAssign<$Other:ty> for BigInt;)*) => {$(
                        impl MulAssign<$Other> for BigInt {
                            #[inline]
                            fn mul_assign(&mut self, other: $Other) {
                                // automatically match value/ref
                                let BigInt { data: y, .. } = other;
                                self.data *= y;
                                if self.data.is_zero() {
                                    self.sign = NoSign;
                                } else {
                                    self.sign = self.sign * other.sign;
                                }
                            }
                        }
                    )*}
                }
                impl_mul_assign! {
                    impl MulAssign<BigInt> for BigInt;
                    impl MulAssign<&BigInt> for BigInt;
                }

                promote_all_scalars!(impl Mul for BigInt, mul);
                promote_all_scalars_assign!(impl MulAssign for BigInt, mul_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u32> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u64> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u128> for BigInt, mul);

                impl Mul<u32> for BigInt {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }

                impl MulAssign<u32> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: u32) {
                        self.data *= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Mul<u64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }

                impl MulAssign<u64> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: u64) {
                        self.data *= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }

                impl Mul<u128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }

                impl MulAssign<u128> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: u128) {
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

                    #[inline] fn mul(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }

                impl MulAssign<i32> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }

                impl Mul<i64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }

                impl MulAssign<i64> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }

                impl Mul<i128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }

                impl MulAssign<i128> for BigInt {
                    #[inline] fn mul_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }

                impl CheckedMul for BigInt {
                    #[inline] fn checked_mul(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.mul(v))
                    }
                }

                impl_product_iter_type!(BigInt);
            }

            pub mod subtraction
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    num::
                    {
                        big::{ IsizePromotion, UsizePromotion },
                        traits::{ CheckedSub },
                    },
                    ops::{ Sub, SubAssign },
                    *,
                };

                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::{Minus, NoSign, Plus};
                use super::{BigInt, UnsignedAbs};

                // We want to forward to BigUint::sub, but it's not clear how that will go until
                // we compare both sign and magnitude.  So we duplicate this body for every
                // val/ref combination, deferring that decision to BigUint's own forwarding.
                macro_rules! bigint_sub {
                    ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) => {
                        match ($a.sign, $b.sign) {
                            (_, NoSign) => $a_owned,
                            (NoSign, _) => -$b_owned,
                            // opposite signs => keep the sign of the left with the sum of magnitudes
                            (Plus, Minus) | (Minus, Plus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                            // same sign => keep or toggle the sign of the left with the difference of magnitudes
                            (Plus, Plus) | (Minus, Minus) => match $a.data.cmp(&$b.data) {
                                Less => BigInt::from_biguint(-$a.sign, $b_data - $a_data),
                                Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                                Equal => BigInt::ZERO,
                            },
                        }
                    };
                }

                impl Sub<&BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: &BigInt) -> BigInt {
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

                impl Sub<BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        bigint_sub!(self, self.clone(), &self.data, other, other, other.data)
                    }
                }

                impl Sub<&BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: &BigInt) -> BigInt {
                        bigint_sub!(self, self, self.data, other, other.clone(), &other.data)
                    }
                }

                impl Sub<BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        bigint_sub!(self, self, self.data, other, other, other.data)
                    }
                }

                impl SubAssign<&BigInt> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: &BigInt) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }
                forward_val_assign!(impl SubAssign for BigInt, sub_assign);

                promote_all_scalars!(impl Sub for BigInt, sub);
                promote_all_scalars_assign!(impl SubAssign for BigInt, sub_assign);
                forward_all_scalar_binop_to_val_val!(impl Sub<u32> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u64> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u128> for BigInt, sub);

                impl Sub<u32> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u32) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }
                impl SubAssign<u32> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: u32) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }

                impl Sub<BigInt> for u32 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }

                impl Sub<BigInt> for u64 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }

                impl Sub<BigInt> for u128 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }

                impl Sub<u64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u64) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }

                impl SubAssign<u64> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: u64) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }

                impl Sub<u128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u128) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }

                impl SubAssign<u128> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: u128) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }

                forward_all_scalar_binop_to_val_val!(impl Sub<i32> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<i64> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<i128> for BigInt, sub);

                impl Sub<i32> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }
                impl SubAssign<i32> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }

                impl Sub<BigInt> for i32 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }

                impl Sub<i64> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }
                impl SubAssign<i64> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }

                impl Sub<BigInt> for i64 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }

                impl Sub<i128> for BigInt {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }

                impl SubAssign<i128> for BigInt {
                    #[inline] fn sub_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }

                impl Sub<BigInt> for i128 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }

                impl CheckedSub for BigInt {
                    #[inline] fn checked_sub(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.sub(v))
                    }
                }
            }

            pub mod bits
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    num::
                    {
                        big::
                        {
                            big_digit::{self, BigDigit, DoubleBigDigit},
                            biguint::IntDigits,
                        },
                        traits::{ ToPrimitive, Zero },
                    },
                    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign},
                    vec::{ Vec },
                    *,
                };

                use super::BigInt;
                use super::Sign::{Minus, NoSign, Plus};
                // Negation in two's complement.
                #[inline]
                fn negate_carry(a: BigDigit, acc: &mut DoubleBigDigit) -> BigDigit {
                    *acc += DoubleBigDigit::from(!a);
                    let lo = *acc as BigDigit;
                    *acc >>= big_digit::BITS;
                    lo
                }

                // + 1 & -ff = ...0 01 & ...f 01 = ...0 01 = + 1
                // +ff & - 1 = ...0 ff & ...f ff = ...0 ff = +ff
                // answer is pos, has length of a
                fn bitand_pos_neg(a: &mut [BigDigit], b: &[BigDigit]) {
                    let mut carry_b = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai &= twos_b;
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                }

                // - 1 & +ff = ...f ff & ...0 ff = ...0 ff = +ff
                // -ff & + 1 = ...f 01 & ...0 01 = ...0 01 = + 1
                // answer is pos, has length of b
                fn bitand_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
                    let mut carry_a = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = twos_a & bi;
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => a.truncate(b.len()),
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().cloned());
                        }
                    }
                }

                // - 1 & -ff = ...f ff & ...f 01 = ...f 01 = - ff
                // -ff & - 1 = ...f 01 & ...f ff = ...f 01 = - ff
                // -ff & -fe = ...f 01 & ...f 02 = ...f 00 = -100
                // answer is neg, has length of longest with a possible carry
                fn bitand_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
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
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                *ai = negate_carry(twos_a, &mut carry_and);
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_and)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                    if carry_and != 0 {
                        a.push(1);
                    }
                }

                forward_val_val_binop!(impl BitAnd for BigInt, bitand);
                forward_ref_val_binop!(impl BitAnd for BigInt, bitand);

                // do not use forward_ref_ref_binop_commutative! for bitand so that we can
                // clone as needed, avoiding over-allocation
                impl BitAnd<&BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn bitand(self, other: &BigInt) -> BigInt {
                        match (self.sign, other.sign) {
                            (NoSign, _) | (_, NoSign) => BigInt::ZERO,
                            (Plus, Plus) => BigInt::from(&self.data & &other.data),
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

                impl BitAnd<&BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn bitand(mut self, other: &BigInt) -> BigInt {
                        self &= other;
                        self
                    }
                }

                forward_val_assign!(impl BitAndAssign for BigInt, bitand_assign);

                impl BitAndAssign<&BigInt> for BigInt {
                    fn bitand_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (NoSign, _) => {}
                            (_, NoSign) => self.set_zero(),
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

                // + 1 | -ff = ...0 01 | ...f 01 = ...f 01 = -ff
                // +ff | - 1 = ...0 ff | ...f ff = ...f ff = - 1
                // answer is neg, has length of b
                fn bitor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
                    let mut carry_b = 1;
                    let mut carry_or = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(*ai | twos_b, &mut carry_or);
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            a.truncate(b.len());
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_or)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                    // for carry_or to be non-zero, we would need twos_b == 0
                    debug_assert!(carry_or == 0);
                }

                // - 1 | +ff = ...f ff | ...0 ff = ...f ff = - 1
                // -ff | + 1 = ...f 01 | ...0 01 = ...f 01 = -ff
                // answer is neg, has length of a
                fn bitor_neg_pos(a: &mut [BigDigit], b: &[BigDigit]) {
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
                    // for carry_or to be non-zero, we would need twos_a == 0
                    debug_assert!(carry_or == 0);
                }

                // - 1 | -ff = ...f ff | ...f 01 = ...f ff = -1
                // -ff | - 1 = ...f 01 | ...f ff = ...f ff = -1
                // answer is neg, has length of shortest
                fn bitor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
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
                    // for carry_or to be non-zero, we would need twos_a == 0 or twos_b == 0
                    debug_assert!(carry_or == 0);
                }

                forward_val_val_binop!(impl BitOr for BigInt, bitor);
                forward_ref_val_binop!(impl BitOr for BigInt, bitor);

                // do not use forward_ref_ref_binop_commutative! for bitor so that we can
                // clone as needed, avoiding over-allocation
                impl BitOr<&BigInt> for &BigInt {
                    type Output = BigInt;

                    #[inline] fn bitor(self, other: &BigInt) -> BigInt {
                        match (self.sign, other.sign) {
                            (NoSign, _) => other.clone(),
                            (_, NoSign) => self.clone(),
                            (Plus, Plus) => BigInt::from(&self.data | &other.data),
                            (Plus, Minus) => other.clone() | self,
                            (Minus, Plus) => self.clone() | other,
                            (Minus, Minus) => {
                                // forward to val-ref, choosing the smaller to clone
                                if self.len() <= other.len() {
                                    self.clone() | other
                                } else {
                                    other.clone() | self
                                }
                            }
                        }
                    }
                }

                impl BitOr<&BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn bitor(mut self, other: &BigInt) -> BigInt {
                        self |= other;
                        self
                    }
                }

                forward_val_assign!(impl BitOrAssign for BigInt, bitor_assign);

                impl BitOrAssign<&BigInt> for BigInt {
                    fn bitor_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (_, NoSign) => {}
                            (NoSign, _) => self.clone_from(other),
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

                // + 1 ^ -ff = ...0 01 ^ ...f 01 = ...f 00 = -100
                // +ff ^ - 1 = ...0 ff ^ ...f ff = ...f 00 = -100
                // answer is neg, has length of longest with a possible carry
                fn bitxor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
                    let mut carry_b = 1;
                    let mut carry_xor = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_b = !0;
                                *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                            }
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_xor)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                    if carry_xor != 0 {
                        a.push(1);
                    }
                }

                // - 1 ^ +ff = ...f ff ^ ...0 ff = ...f 00 = -100
                // -ff ^ + 1 = ...f 01 ^ ...0 01 = ...f 00 = -100
                // answer is neg, has length of longest with a possible carry
                fn bitxor_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
                    let mut carry_a = 1;
                    let mut carry_xor = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a ^ bi, &mut carry_xor);
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                *ai = negate_carry(twos_a, &mut carry_xor);
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_a = !0;
                                negate_carry(twos_a ^ bi, &mut carry_xor)
                            }));
                        }
                    }
                    if carry_xor != 0 {
                        a.push(1);
                    }
                }

                // - 1 ^ -ff = ...f ff ^ ...f 01 = ...0 fe = +fe
                // -ff & - 1 = ...f 01 ^ ...f ff = ...0 fe = +fe
                // answer is pos, has length of longest
                fn bitxor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) {
                    let mut carry_a = 1;
                    let mut carry_b = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = twos_a ^ twos_b;
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                let twos_b = !0;
                                *ai = twos_a ^ twos_b;
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_a = !0;
                                let twos_b = negate_carry(bi, &mut carry_b);
                                twos_a ^ twos_b
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                }

                forward_all_binop_to_val_ref_commutative!(impl BitXor for BigInt, bitxor);

                impl BitXor<&BigInt> for BigInt {
                    type Output = BigInt;

                    #[inline] fn bitxor(mut self, other: &BigInt) -> BigInt {
                        self ^= other;
                        self
                    }
                }

                forward_val_assign!(impl BitXorAssign for BigInt, bitxor_assign);

                impl BitXorAssign<&BigInt> for BigInt {
                    fn bitxor_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (_, NoSign) => {}
                            (NoSign, _) => self.clone_from(other),
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

                pub(super) fn set_negative_bit(x: &mut BigInt, bit: u64, value: bool) {
                    debug_assert_eq!(x.sign, Minus);
                    let data = &mut x.data;

                    let bits_per_digit = u64::from(big_digit::BITS);
                    if bit >= bits_per_digit * data.len() as u64 {
                        if !value {
                            data.set_bit(bit, true);
                        }
                    } else {
                        // If the Uint number is
                        //   ... 0  x 1 0 ... 0
                        // then the two's complement is
                        //   ... 1 !x 1 0 ... 0
                        //            |-- bit at position 'trailing_zeros'
                        // where !x is obtained from x by flipping each bit
                        let trailing_zeros = data.trailing_zeros().unwrap();
                        if bit > trailing_zeros {
                            data.set_bit(bit, !value);
                        } else if bit == trailing_zeros && !value {
                            // Clearing the bit at position `trailing_zeros` is dealt with by doing
                            // similarly to what `bitand_neg_pos` does, except we start at digit
                            // `bit_index`. All digits below `bit_index` are guaranteed to be zero,
                            // so initially we have `carry_in` = `carry_out` = 1. Furthermore, we
                            // stop traversing the digits when there are no more carries.
                            let bit_index = (bit / bits_per_digit).to_usize().unwrap();
                            let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                            let mut digit_iter = data.digits_mut().iter_mut().skip(bit_index);
                            let mut carry_in = 1;
                            let mut carry_out = 1;

                            let digit = digit_iter.next().unwrap();
                            let twos_in = negate_carry(*digit, &mut carry_in);
                            let twos_out = twos_in & !bit_mask;
                            *digit = negate_carry(twos_out, &mut carry_out);

                            for digit in digit_iter {
                                if carry_in == 0 && carry_out == 0 {
                                    // Exit the loop since no more digits can change
                                    break;
                                }
                                let twos = negate_carry(*digit, &mut carry_in);
                                *digit = negate_carry(twos, &mut carry_out);
                            }

                            if carry_out != 0 {
                                // All digits have been traversed and there is a carry
                                debug_assert_eq!(carry_in, 0);
                                data.digits_mut().push(1);
                            }
                        } else if bit < trailing_zeros && value {
                            // Flip each bit from position 'bit' to 'trailing_zeros', both inclusive
                            //       ... 1 !x 1 0 ... 0 ... 0
                            //                        |-- bit at position 'bit'
                            //                |-- bit at position 'trailing_zeros'
                            // bit_mask:      1 1 ... 1 0 .. 0
                            // This is done by xor'ing with the bit_mask
                            let index_lo = (bit / bits_per_digit).to_usize().unwrap();
                            let index_hi = (trailing_zeros / bits_per_digit).to_usize().unwrap();
                            let bit_mask_lo = big_digit::MAX << (bit % bits_per_digit);
                            let bit_mask_hi =
                                big_digit::MAX >> (bits_per_digit - 1 - (trailing_zeros % bits_per_digit));
                            let digits = data.digits_mut();

                            if index_lo == index_hi {
                                digits[index_lo] ^= bit_mask_lo & bit_mask_hi;
                            } else {
                                digits[index_lo] = bit_mask_lo;
                                for digit in &mut digits[index_lo + 1..index_hi] {
                                    *digit = big_digit::MAX;
                                }
                                digits[index_hi] ^= bit_mask_hi;
                            }
                        } else {
                            // We end up here in two cases:
                            //   bit == trailing_zeros && value: Bit is already set
                            //   bit < trailing_zeros && !value: Bit is already cleared
                        }
                    }
                }
            }

            pub mod convert
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    convert::TryFrom,
                    num::
                    {
                        big::{ BigUint, ParseBigIntError, ToBigUint, TryFromBigIntError },
                        traits::{ FromPrimitive, Num, One, ToPrimitive, Zero },
                    },
                    str::{self, FromStr},
                    vec::{ Vec },
                    *,
                };

                use super::Sign::{self, Minus, NoSign, Plus};
                use super::{BigInt, ToBigInt};

                impl FromStr for BigInt
                {
                    type Err = ParseBigIntError;

                    #[inline] fn from_str(s: &str) -> Result<BigInt, ParseBigIntError> {
                        BigInt::from_str_radix(s, 10)
                    }
                }

                impl Num for BigInt {
                    type FromStrRadixErr = ParseBigIntError;

                    /// Creates and initializes a [`BigInt`].
                    #[inline] fn from_str_radix(mut s: &str, radix: u32) -> Result<BigInt, ParseBigIntError> {
                        let sign = if let Some(tail) = s.strip_prefix('-') {
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

                impl ToPrimitive for BigInt {
                    #[inline] fn to_i64(&self) -> Option<i64> {
                        match self.sign {
                            Plus => self.data.to_i64(),
                            NoSign => Some(0),
                            Minus => {
                                let n = self.data.to_u64()?;
                                let m: u64 = 1 << 63;
                                match n.cmp(&m) {
                                    Less => Some(-(n as i64)),
                                    Equal => Some(i64::MIN),
                                    Greater => None,
                                }
                            }
                        }
                    }

                    #[inline] fn to_i128(&self) -> Option<i128> {
                        match self.sign {
                            Plus => self.data.to_i128(),
                            NoSign => Some(0),
                            Minus => {
                                let n = self.data.to_u128()?;
                                let m: u128 = 1 << 127;
                                match n.cmp(&m) {
                                    Less => Some(-(n as i128)),
                                    Equal => Some(i128::MIN),
                                    Greater => None,
                                }
                            }
                        }
                    }

                    #[inline] fn to_u64(&self) -> Option<u64> {
                        match self.sign {
                            Plus => self.data.to_u64(),
                            NoSign => Some(0),
                            Minus => None,
                        }
                    }

                    #[inline] fn to_u128(&self) -> Option<u128> {
                        match self.sign {
                            Plus => self.data.to_u128(),
                            NoSign => Some(0),
                            Minus => None,
                        }
                    }

                    #[inline] fn to_f32(&self) -> Option<f32> {
                        let n = self.data.to_f32()?;
                        Some(if self.sign == Minus { -n } else { n })
                    }

                    #[inline] fn to_f64(&self) -> Option<f64> {
                        let n = self.data.to_f64()?;
                        Some(if self.sign == Minus { -n } else { n })
                    }
                }

                macro_rules! impl_try_from_bigint {
                    ($T:ty, $to_ty:path) => {
                        impl TryFrom<&BigInt> for $T {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: &BigInt) -> Result<$T, TryFromBigIntError<()>> {
                                $to_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }

                        impl TryFrom<BigInt> for $T {
                            type Error = TryFromBigIntError<BigInt>;

                            #[inline]
                            fn try_from(value: BigInt) -> Result<$T, TryFromBigIntError<BigInt>> {
                                <$T>::try_from(&value).map_err(|_| TryFromBigIntError::new(value))
                            }
                        }
                    };
                }

                impl_try_from_bigint!(u8, ToPrimitive::to_u8);
                impl_try_from_bigint!(u16, ToPrimitive::to_u16);
                impl_try_from_bigint!(u32, ToPrimitive::to_u32);
                impl_try_from_bigint!(u64, ToPrimitive::to_u64);
                impl_try_from_bigint!(usize, ToPrimitive::to_usize);
                impl_try_from_bigint!(u128, ToPrimitive::to_u128);

                impl_try_from_bigint!(i8, ToPrimitive::to_i8);
                impl_try_from_bigint!(i16, ToPrimitive::to_i16);
                impl_try_from_bigint!(i32, ToPrimitive::to_i32);
                impl_try_from_bigint!(i64, ToPrimitive::to_i64);
                impl_try_from_bigint!(isize, ToPrimitive::to_isize);
                impl_try_from_bigint!(i128, ToPrimitive::to_i128);

                impl FromPrimitive for BigInt {
                    #[inline] fn from_i64(n: i64) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }

                    #[inline] fn from_i128(n: i128) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }

                    #[inline] fn from_u64(n: u64) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }

                    #[inline] fn from_u128(n: u128) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }

                    #[inline] fn from_f64(n: f64) -> Option<BigInt> {
                        if n >= 0.0 {
                            BigUint::from_f64(n).map(BigInt::from)
                        } else {
                            let x = BigUint::from_f64(-n)?;
                            Some(-BigInt::from(x))
                        }
                    }
                }

                impl From<i64> for BigInt {
                    #[inline] fn from(n: i64) -> Self {
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
                    #[inline] fn from(n: i128) -> Self {
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

                macro_rules! impl_bigint_from_int {
                    ($T:ty) => {
                        impl From<$T> for BigInt {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigInt::from(n as i64)
                            }
                        }
                    };
                }

                impl_bigint_from_int!(i8);
                impl_bigint_from_int!(i16);
                impl_bigint_from_int!(i32);
                impl_bigint_from_int!(isize);

                impl From<u64> for BigInt {
                    #[inline] fn from(n: u64) -> Self {
                        if n > 0 {
                            BigInt {
                                sign: Plus,
                                data: BigUint::from(n),
                            }
                        } else {
                            Self::ZERO
                        }
                    }
                }

                impl From<u128> for BigInt {
                    #[inline] fn from(n: u128) -> Self {
                        if n > 0 {
                            BigInt {
                                sign: Plus,
                                data: BigUint::from(n),
                            }
                        } else {
                            Self::ZERO
                        }
                    }
                }

                macro_rules! impl_bigint_from_uint {
                    ($T:ty) => {
                        impl From<$T> for BigInt {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigInt::from(n as u64)
                            }
                        }
                    };
                }

                impl_bigint_from_uint!(u8);
                impl_bigint_from_uint!(u16);
                impl_bigint_from_uint!(u32);
                impl_bigint_from_uint!(usize);

                impl From<BigUint> for BigInt {
                    #[inline] fn from(n: BigUint) -> Self {
                        if n.is_zero() {
                            Self::ZERO
                        } else {
                            BigInt {
                                sign: Plus,
                                data: n,
                            }
                        }
                    }
                }

                impl ToBigInt for BigInt {
                    #[inline] fn to_bigint(&self) -> Option<BigInt> {
                        Some(self.clone())
                    }
                }

                impl ToBigInt for BigUint {
                    #[inline] fn to_bigint(&self) -> Option<BigInt> {
                        if self.is_zero() {
                            Some(BigInt::ZERO)
                        } else {
                            Some(BigInt {
                                sign: Plus,
                                data: self.clone(),
                            })
                        }
                    }
                }

                impl ToBigUint for BigInt {
                    #[inline] fn to_biguint(&self) -> Option<BigUint> {
                        match self.sign() {
                            Plus => Some(self.data.clone()),
                            NoSign => Some(BigUint::ZERO),
                            Minus => None,
                        }
                    }
                }

                impl TryFrom<&BigInt> for BigUint {
                    type Error = TryFromBigIntError<()>;

                    #[inline] fn try_from(value: &BigInt) -> Result<BigUint, TryFromBigIntError<()>> {
                        value
                            .to_biguint()
                            .ok_or_else(|| TryFromBigIntError::new(()))
                    }
                }

                impl TryFrom<BigInt> for BigUint {
                    type Error = TryFromBigIntError<BigInt>;

                    #[inline] fn try_from(value: BigInt) -> Result<BigUint, TryFromBigIntError<BigInt>> {
                        if value.sign() == Sign::Minus {
                            Err(TryFromBigIntError::new(value))
                        } else {
                            Ok(value.data)
                        }
                    }
                }

                macro_rules! impl_to_bigint {
                    ($T:ty, $from_ty:path) => {
                        impl ToBigInt for $T {
                            #[inline]
                            fn to_bigint(&self) -> Option<BigInt> {
                                $from_ty(*self)
                            }
                        }
                    };
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

                impl From<bool> for BigInt {
                    fn from(x: bool) -> Self {
                        if x {
                            One::one()
                        } else {
                            Self::ZERO
                        }
                    }
                }

                #[inline]
                pub(super) fn from_signed_bytes_be(digits: &[u8]) -> BigInt {
                    let sign = match digits.first() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::ZERO,
                    };

                    if sign == Sign::Minus {
                        // two's-complement the content to retrieve the magnitude
                        let mut digits = Vec::from(digits);
                        twos_complement_be(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(&digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(digits))
                    }
                }

                #[inline]
                pub(super) fn from_signed_bytes_le(digits: &[u8]) -> BigInt {
                    let sign = match digits.last() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::ZERO,
                    };

                    if sign == Sign::Minus {
                        // two's-complement the content to retrieve the magnitude
                        let mut digits = Vec::from(digits);
                        twos_complement_le(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(&digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(digits))
                    }
                }

                #[inline]
                pub(super) fn to_signed_bytes_be(x: &BigInt) -> Vec<u8> {
                    let mut bytes = x.data.to_bytes_be();
                    let first_byte = bytes.first().cloned().unwrap_or(0);
                    if first_byte > 0x7f
                        && !(first_byte == 0x80 && bytes.iter().skip(1).all(Zero::is_zero) && x.sign == Sign::Minus)
                    {
                        // msb used by magnitude, extend by 1 byte
                        bytes.insert(0, 0);
                    }
                    if x.sign == Sign::Minus {
                        twos_complement_be(&mut bytes);
                    }
                    bytes
                }

                #[inline]
                pub(super) fn to_signed_bytes_le(x: &BigInt) -> Vec<u8> {
                    let mut bytes = x.data.to_bytes_le();
                    let last_byte = bytes.last().cloned().unwrap_or(0);
                    if last_byte > 0x7f
                        && !(last_byte == 0x80
                            && bytes.iter().rev().skip(1).all(Zero::is_zero)
                            && x.sign == Sign::Minus)
                    {
                        // msb used by magnitude, extend by 1 byte
                        bytes.push(0);
                    }
                    if x.sign == Sign::Minus {
                        twos_complement_le(&mut bytes);
                    }
                    bytes
                }

                /// Perform in-place two's complement of the given binary representation,
                /// in little-endian byte order.
                #[inline]
                fn twos_complement_le(digits: &mut [u8]) {
                    twos_complement(digits)
                }

                /// Perform in-place two's complement of the given binary representation
                /// in big-endian byte order.
                #[inline]
                fn twos_complement_be(digits: &mut [u8]) {
                    twos_complement(digits.iter_mut().rev())
                }

                /// Perform in-place two's complement of the given digit iterator
                /// starting from the least significant byte.
                #[inline]
                fn twos_complement<'a, I>(digits: I)
                where
                    I: IntoIterator<Item = &'a mut u8>,
                {
                    let mut carry = true;
                    for d in digits {
                        *d = !*d;
                        if carry {
                            *d = d.wrapping_add(1);
                            carry = d.is_zero();
                        }
                    }
                }
            }

            pub mod power
            {
                use ::
                {
                    num::
                    {
                        big::{ BigUint },
                        integers::{ Integer },
                        traits::{ Pow, Signed, Zero },
                    },
                    *,
                };

                use super::BigInt;
                use super::Sign::{self, Minus, Plus};
                /// Help function for pow
                #[inline] fn powsign<T: Integer>(sign: Sign, other: &T) -> Sign
                {
                    if other.is_zero() {
                        Plus
                    } else if sign != Minus || other.is_odd() {
                        sign
                    } else {
                        -sign
                    }
                }

                macro_rules! pow_impl {
                    ($T:ty) => {
                        impl Pow<$T> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: $T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, &rhs), self.data.pow(rhs))
                            }
                        }

                        impl Pow<&$T> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: &$T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, rhs), self.data.pow(rhs))
                            }
                        }

                        impl Pow<$T> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: $T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, &rhs), Pow::pow(&self.data, rhs))
                            }
                        }

                        impl Pow<&$T> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: &$T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, rhs), Pow::pow(&self.data, rhs))
                            }
                        }
                    };
                }

                pow_impl!(u8);
                pow_impl!(u16);
                pow_impl!(u32);
                pow_impl!(u64);
                pow_impl!(usize);
                pow_impl!(u128);
                pow_impl!(BigUint);

                pub(super) fn modpow(x: &BigInt, exponent: &BigInt, modulus: &BigInt) -> BigInt {
                    assert!(
                        !exponent.is_negative(),
                        "negative exponentiation is not supported!"
                    );
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    let result = x.data.modpow(&exponent.data, &modulus.data);
                    if result.is_zero() {
                        return BigInt::ZERO;
                    }

                    // The sign of the result follows the modulus, like `mod_floor`.
                    let (sign, mag) = match (x.is_negative() && exponent.is_odd(), modulus.is_negative()) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    BigInt::from_biguint(sign, mag)
                }
            }

            pub mod shift
            {
                use ::
                {
                    ops::{Shl, ShlAssign, Shr, ShrAssign},
                    num::traits::{PrimInt, Signed, Zero},
                    *,
                };

                use super::BigInt;
                use super::Sign::NoSign; 

                macro_rules! impl_shift {
                    (@ref $Shx:ident :: $shx:ident, $ShxAssign:ident :: $shx_assign:ident, $rhs:ty) => {
                        impl $Shx<&$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigInt {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $Shx<&$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigInt {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $ShxAssign<&$rhs> for BigInt {
                            #[inline]
                            fn $shx_assign(&mut self, rhs: &$rhs) {
                                $ShxAssign::$shx_assign(self, *rhs);
                            }
                        }
                    };
                    ($($rhs:ty),+) => {$(
                        impl Shl<$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigInt {
                                BigInt::from_biguint(self.sign, self.data << rhs)
                            }
                        }
                        impl Shl<$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigInt {
                                BigInt::from_biguint(self.sign, &self.data << rhs)
                            }
                        }
                        impl ShlAssign<$rhs> for BigInt {
                            #[inline]
                            fn shl_assign(&mut self, rhs: $rhs) {
                                self.data <<= rhs
                            }
                        }
                        impl_shift! { @ref Shl::shl, ShlAssign::shl_assign, $rhs }

                        impl Shr<$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigInt {
                                let round_down = shr_round_down(&self, rhs);
                                let data = self.data >> rhs;
                                let data = if round_down { data + 1u8 } else { data };
                                BigInt::from_biguint(self.sign, data)
                            }
                        }
                        impl Shr<$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigInt {
                                let round_down = shr_round_down(self, rhs);
                                let data = &self.data >> rhs;
                                let data = if round_down { data + 1u8 } else { data };
                                BigInt::from_biguint(self.sign, data)
                            }
                        }
                        impl ShrAssign<$rhs> for BigInt {
                            #[inline]
                            fn shr_assign(&mut self, rhs: $rhs) {
                                let round_down = shr_round_down(self, rhs);
                                self.data >>= rhs;
                                if round_down {
                                    self.data += 1u8;
                                } else if self.data.is_zero() {
                                    self.sign = NoSign;
                                }
                            }
                        }
                        impl_shift! { @ref Shr::shr, ShrAssign::shr_assign, $rhs }
                    )*};
                }

                impl_shift! { u8, u16, u32, u64, u128, usize }
                impl_shift! { i8, i16, i32, i64, i128, isize }

                // Negative values need a rounding adjustment if there are any ones in the
                // bits that are getting shifted out.
                fn shr_round_down<T: PrimInt>(i: &BigInt, shift: T) -> bool {
                    if i.is_negative() {
                        let zeros = i.trailing_zeros().expect("negative values are non-zero");
                        shift > T::zero() && shift.to_u64().map(|shift| zeros < shift).unwrap_or(true)
                    } else {
                        false
                    }
                }
            }
            /// A `Sign` is a [`BigInt`]'s composing element.
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

                /// Negate `Sign` value.
                #[inline]
                fn neg(self) -> Sign {
                    match self {
                        Minus => Plus,
                        NoSign => NoSign,
                        Plus => Minus,
                    }
                }
            }
            /// A big signed integer type.
            pub struct BigInt
            {
                sign: Sign,
                data: BigUint,
            }
            
            impl Clone for BigInt 
            {
                #[inline]
                fn clone(&self) -> Self {
                    BigInt {
                        sign: self.sign,
                        data: self.data.clone(),
                    }
                }

                #[inline]
                fn clone_from(&mut self, other: &Self) {
                    self.sign = other.sign;
                    self.data.clone_from(&other.data);
                }
            }

            impl hash::Hash for BigInt 
            {
                #[inline]
                fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    self.sign.hash(state);
                    if self.sign != NoSign {
                        self.data.hash(state);
                    }
                }
            }

            impl PartialEq for BigInt 
            {
                #[inline]
                fn eq(&self, other: &BigInt) -> bool {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    debug_assert!((other.sign != NoSign) ^ other.data.is_zero());
                    self.sign == other.sign && (self.sign == NoSign || self.data == other.data)
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
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    debug_assert!((other.sign != NoSign) ^ other.data.is_zero());
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
                    Self::ZERO
                }
            }

            impl fmt::Debug for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Display::fmt(self, f)
                }
            }

            impl fmt::Display for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "", &self.data.to_str_radix(10))
                }
            }

            impl fmt::Binary for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0b", &self.data.to_str_radix(2))
                }
            }

            impl fmt::Octal for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0o", &self.data.to_str_radix(8))
                }
            }

            impl fmt::LowerHex for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0x", &self.data.to_str_radix(16))
                }
            }

            impl fmt::UpperHex for BigInt 
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let mut s = self.data.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(!self.is_negative(), "0x", &s)
                }
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

            impl Not for &BigInt 
            {
                type Output = BigInt;

                fn not(self) -> BigInt {
                    match self.sign {
                        NoSign => -BigInt::one(),
                        Plus => -BigInt::from(&self.data + 1u32),
                        Minus => BigInt::from(&self.data - 1u32),
                    }
                }
            }

            impl Zero for BigInt 
            {
                #[inline]
                fn zero() -> BigInt {
                    Self::ZERO
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

            impl ConstZero for BigInt 
            {
                // forward to the inherent const
                const ZERO: Self = Self::ZERO;
            }

            impl One for BigInt 
            {
                #[inline]
                fn one() -> BigInt {
                    BigInt {
                        sign: Plus,
                        data: BigUint::one(),
                    }
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
                        Minus => BigInt::from(self.data.clone()),
                    }
                }

                #[inline]
                fn abs_sub(&self, other: &BigInt) -> BigInt {
                    if *self <= *other {
                        Self::ZERO
                    } else {
                        self - other
                    }
                }

                #[inline]
                fn signum(&self) -> BigInt {
                    match self.sign {
                        Plus => BigInt::one(),
                        Minus => -BigInt::one(),
                        NoSign => Self::ZERO,
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

            trait UnsignedAbs 
            {
                type Unsigned;

                fn checked_uabs(self) -> CheckedUnsignedAbs<Self::Unsigned>;
            }

            enum CheckedUnsignedAbs<T> 
            {
                Positive(T),
                Negative(T),
            }
            use self::CheckedUnsignedAbs::{Negative, Positive};

            macro_rules! impl_unsigned_abs
            {
                ($Signed:ty, $Unsigned:ty) => {
                    impl UnsignedAbs for $Signed {
                        type Unsigned = $Unsigned;

                        #[inline]
                        fn checked_uabs(self) -> CheckedUnsignedAbs<Self::Unsigned> {
                            if self >= 0 {
                                Positive(self as $Unsigned)
                            } else {
                                Negative(self.wrapping_neg() as $Unsigned)
                            }
                        }
                    }
                };
            }

            impl_unsigned_abs!(i8, u8);
            impl_unsigned_abs!(i16, u16);
            impl_unsigned_abs!(i32, u32);
            impl_unsigned_abs!(i64, u64);
            impl_unsigned_abs!(i128, u128);
            impl_unsigned_abs!(isize, usize);

            impl Neg for BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn neg(mut self) -> BigInt {
                    self.sign = -self.sign;
                    self
                }
            }

            impl Neg for &BigInt 
            {
                type Output = BigInt;

                #[inline]
                fn neg(self) -> BigInt {
                    -self.clone()
                }
            }

            impl Integer for BigInt 
            {
                #[inline]
                fn div_rem(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // r.sign == self.sign
                    let (d_ui, r_ui) = self.data.div_rem(&other.data);
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
                    let (d_ui, m) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => d,
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                -d
                            } else {
                                -d - 1u32
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                #[inline]
                fn mod_floor(&self, other: &BigInt) -> BigInt {
                    // m.sign == other.sign
                    let m_ui = self.data.mod_floor(&other.data);
                    let m = BigInt::from_biguint(other.sign, m_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => m,
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                m
                            } else {
                                other - m
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                fn div_mod_floor(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // m.sign == other.sign
                    let (d_ui, m_ui) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    let m = BigInt::from_biguint(other.sign, m_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => (d, m),
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                (-d, m)
                            } else {
                                (-d - 1u32, other - m)
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                #[inline]
                fn div_ceil(&self, other: &Self) -> Self {
                    let (d_ui, m) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    match (self.sign, other.sign) {
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => -d,
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => {
                            if m.is_zero() {
                                d
                            } else {
                                d + 1u32
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                #[inline]
                fn gcd(&self, other: &BigInt) -> BigInt {
                    BigInt::from(self.data.gcd(&other.data))
                }

                /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                #[inline]
                fn lcm(&self, other: &BigInt) -> BigInt {
                    BigInt::from(self.data.lcm(&other.data))
                }

                /// Calculates the Greatest Common Divisor (GCD) and
                /// Lowest Common Multiple (LCM) together.
                #[inline]
                fn gcd_lcm(&self, other: &BigInt) -> (BigInt, BigInt) {
                    let (gcd, lcm) = self.data.gcd_lcm(&other.data);
                    (BigInt::from(gcd), BigInt::from(lcm))
                }

                /// Greatest common divisor, least common multiple, and Bézout coefficients.
                #[inline]
                fn extended_gcd_lcm(&self, other: &BigInt) -> (num_integer::ExtendedGcd<BigInt>, BigInt) {
                    let egcd = self.extended_gcd(other);
                    let lcm = if egcd.gcd.is_zero() {
                        Self::ZERO
                    } else {
                        BigInt::from(&self.data / &egcd.gcd.data * &other.data)
                    };
                    (egcd, lcm)
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

                /// Rounds up to nearest multiple of argument.
                #[inline]
                fn next_multiple_of(&self, other: &Self) -> Self {
                    let m = self.mod_floor(other);
                    if m.is_zero() {
                        self.clone()
                    } else {
                        self + (other - m)
                    }
                }
                /// Rounds down to nearest multiple of argument.
                #[inline]
                fn prev_multiple_of(&self, other: &Self) -> Self {
                    self - self.mod_floor(other)
                }
                /*
                fn dec(&mut self) {
                    *self -= 1u32;
                }

                fn inc(&mut self) {
                    *self += 1u32;
                }
                */
            }

            impl Roots for BigInt 
            {
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

            impl IntDigits for BigInt 
            {
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
            /// A generic trait for converting a value to a [`BigInt`].
            pub trait ToBigInt 
            {
                /// Converts the value of `self` to a [`BigInt`].
                fn to_bigint(&self) -> Option<BigInt>;
            }

            impl BigInt 
            {
                /// A constant `BigInt` with value 0, useful for static initialization.
                pub const ZERO: Self = BigInt {
                    sign: NoSign,
                    data: BigUint::ZERO,
                };

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn new(sign: Sign, digits: Vec<u32>) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::new(digits))
                }

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_biguint(mut sign: Sign, mut data: BigUint) -> BigInt {
                    if sign == NoSign {
                        data.assign_from_slice(&[]);
                    } else if data.is_zero() {
                        sign = NoSign;
                    }

                    BigInt { sign, data }
                }

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_slice(sign: Sign, slice: &[u32]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_slice(slice))
                }

                /// Reinitializes a [`BigInt`].
                #[inline] pub fn assign_from_slice(&mut self, sign: Sign, slice: &[u32]) {
                    if sign == NoSign {
                        self.set_zero();
                    } else {
                        self.data.assign_from_slice(slice);
                        self.sign = if self.data.is_zero() { NoSign } else { sign };
                    }
                }

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_bytes_be(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_be(bytes))
                }

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_bytes_le(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_le(bytes))
                }

                /// Creates and initializes a [`BigInt`] from an array of bytes in
                /// two's complement binary representation.
                #[inline] pub fn from_signed_bytes_be(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_be(digits)
                }

                /// Creates and initializes a [`BigInt`] from an array of bytes in two's complement.
                #[inline] pub fn from_signed_bytes_le(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_le(digits)
                }

                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigInt> {
                    let s = str::from_utf8(buf).ok()?;
                    BigInt::from_str_radix(s, radix).ok()
                }

                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                pub fn from_radix_be(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_be(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }

                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                pub fn from_radix_le(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_le(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }

                /// Returns the sign and the byte representation of the [`BigInt`] in big-endian byte order.
                #[inline] pub fn to_bytes_be(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_be())
                }

                /// Returns the sign and the byte representation of the [`BigInt`] in little-endian byte order.
                #[inline] pub fn to_bytes_le(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_le())
                }

                /// Returns the sign and the `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn to_u32_digits(&self) -> (Sign, Vec<u32>) {
                    (self.sign, self.data.to_u32_digits())
                }

                /// Returns the sign and the `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn to_u64_digits(&self) -> (Sign, Vec<u64>) {
                    (self.sign, self.data.to_u64_digits())
                }

                /// Returns an iterator of `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    self.data.iter_u32_digits()
                }

                /// Returns an iterator of `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    self.data.iter_u64_digits()
                }

                /// Returns the two's-complement byte representation of the [`BigInt`] in big-endian byte order.
                #[inline] pub fn to_signed_bytes_be(&self) -> Vec<u8> {
                    convert::to_signed_bytes_be(self)
                }

                /// Returns the two's-complement byte representation of the [`BigInt`] in little-endian byte order.
                #[inline] pub fn to_signed_bytes_le(&self) -> Vec<u8> {
                    convert::to_signed_bytes_le(self)
                }

                /// Returns the integer formatted as a string in the given radix.
                #[inline] pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(&self.data, radix);

                    if self.is_negative() {
                        v.push(b'-');
                    }

                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }

                /// Returns the integer in the requested base in big-endian digit order.
                #[inline] pub fn to_radix_be(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_be(radix))
                }

                /// Returns the integer in the requested base in little-endian digit order.
                #[inline] pub fn to_radix_le(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_le(radix))
                }

                /// Returns the sign of the [`BigInt`] as a [`Sign`].
                #[inline] pub fn sign(&self) -> Sign {
                    self.sign
                }

                /// Returns the magnitude of the [`BigInt`] as a [`BigUint`].
                #[inline] pub fn magnitude(&self) -> &BigUint {
                    &self.data
                }

                /// Convert this [`BigInt`] into its [`Sign`] and [`BigUint`] magnitude,
                /// the reverse of [`BigInt::from_biguint()`].
                #[inline] pub fn into_parts(self) -> (Sign, BigUint) 
                {
                    (self.sign, self.data)
                }

                /// Determines the fewest bits necessary to express the [`BigInt`],
                /// not including the sign.
                #[inline] pub fn bits(&self) -> u64 
                {
                    self.data.bits()
                }

                /// Converts this [`BigInt`] into a [`BigUint`], if it's not negative.
                #[inline] pub fn to_biguint(&self) -> Option<BigUint> 
                {
                    match self.sign {
                        Plus => Some(self.data.clone()),
                        NoSign => Some(BigUint::ZERO),
                        Minus => None,
                    }
                }

                #[inline] pub fn checked_add(&self, v: &BigInt) -> Option<BigInt> 
                {
                    Some(self + v)
                }

                #[inline] pub fn checked_sub(&self, v: &BigInt) -> Option<BigInt> 
                {
                    Some(self - v)
                }

                #[inline] pub fn checked_mul(&self, v: &BigInt) -> Option<BigInt> 
                {
                    Some(self * v)
                }

                #[inline] pub fn checked_div(&self, v: &BigInt) -> Option<BigInt> 
                {
                    if v.is_zero() {
                        return None;
                    }
                    Some(self / v)
                }

                /// Returns `self ^ exponent`.
                pub fn pow(&self, exponent: u32) -> Self 
                {
                    Pow::pow(self, exponent)
                }
                /// Returns `(self ^ exponent) mod modulus`.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self 
                {
                    power::modpow(self, exponent, modulus)
                }
                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                pub fn modinv(&self, modulus: &Self) -> Option<Self> 
                {
                    let result = self.data.modinv(&modulus.data)?;
                    // The sign of the result follows the modulus, like `mod_floor`.
                    let (sign, mag) = match (self.is_negative(), modulus.is_negative()) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    Some(BigInt::from_biguint(sign, mag))
                }
                /// Returns the truncated principal square root of `self`
                pub fn sqrt(&self) -> Self 
                {
                    Roots::sqrt(self)
                }
                /// Returns the truncated principal cube root of `self`
                pub fn cbrt(&self) -> Self 
                {
                    Roots::cbrt(self)
                }
                /// Returns the truncated principal `n`th root of `self`
                pub fn nth_root(&self, n: u32) -> Self 
                {
                    Roots::nth_root(self, n)
                }
                /// Returns the number of least-significant bits that are zero,
                /// or `None` if the entire number is zero.
                pub fn trailing_zeros(&self) -> Option<u64> 
                {
                    self.data.trailing_zeros()
                }

                /// Returns whether the bit in position `bit` is set,
                /// using the two's complement for negative numbers
                pub fn bit(&self, bit: u64) -> bool 
                {
                    if self.is_negative() {
                        // Let the binary representation of a number be
                        //   ... 0  x 1 0 ... 0
                        // Then the two's complement is
                        //   ... 1 !x 1 0 ... 0
                        // where !x is obtained from x by flipping each bit
                        if bit >= u64::from(crate::big_digit::BITS) * self.len() as u64 {
                            true
                        } else {
                            let trailing_zeros = self.data.trailing_zeros().unwrap();
                            match Ord::cmp(&bit, &trailing_zeros) {
                                Ordering::Less => false,
                                Ordering::Equal => true,
                                Ordering::Greater => !self.data.bit(bit),
                            }
                        }
                    } else {
                        self.data.bit(bit)
                    }
                }

                /// Sets or clears the bit in the given position,
                /// using the two's complement for negative numbers
                pub fn set_bit(&mut self, bit: u64, value: bool) 
                {
                    match self.sign {
                        Sign::Plus => self.data.set_bit(bit, value),
                        Sign::Minus => bits::set_negative_bit(self, bit, value),
                        Sign::NoSign => {
                            if value {
                                self.data.set_bit(bit, true);
                                self.sign = Sign::Plus;
                            } else {
                                // Clearing a bit for zero is a no-op
                            }
                        }
                    }
                    // The top bit may have been cleared, so normalize
                    self.normalize();
                }
            }

            impl ::num::traits::FromBytes for BigInt {
                type Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_be(bytes)
                }

                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_le(bytes)
                }
            }

            impl ::num::traits::ToBytes for BigInt {
                type Bytes = Vec<u8>;

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_signed_bytes_be()
                }

                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_signed_bytes_le()
                }
            }
        }

        pub mod biguint
        {
            use ::
            {
                cmp::{ Ordering },
                default::Default,
                num::
                {
                    big::
                    {
                        big_digit::{self, BigDigit},
                    },
                    integers::{Integer, Roots},
                    traits::{ConstZero, Num, One, Pow, ToPrimitive, Unsigned, Zero},
                },
                string::{ String },
                vec::{ Vec },
                *,
            };
            
            pub mod addition
            {
                use ::
                {
                    iter::{ Sum },
                    ops::{ Add, AddAssign },
                    num::
                    {
                        big::
                        {
                            big_digit::{self, BigDigit},
                            UsizePromotion,
                        },
                        traits::{ CheckedAdd },
                    },
                    *,
                };

                use super::{BigUint, IntDigits};
                
                #[inline] fn adc(carry: u8, a: u64, b: u64, out: &mut u64) -> u8
                {
                    unsafe { ::arch::_addcarry_u64(carry, a, b, out) }
                }
                /// Two argument addition of raw slices, `a += b`, returning the carry.
                ///
                /// This is used when the data `Vec` might need to resize to push a non-zero carry, so we perform
                /// the addition first hoping that it will fit.
                ///
                /// The caller _must_ ensure that `a` is at least as long as `b`.
                #[inline]
                pub(super) fn __add2(a: &mut [BigDigit], b: &[BigDigit]) -> BigDigit {
                    debug_assert!(a.len() >= b.len());

                    let mut carry = 0;
                    let (a_lo, a_hi) = a.split_at_mut(b.len());

                    for (a, b) in a_lo.iter_mut().zip(b) {
                        carry = adc(carry, *a, *b, a);
                    }

                    if carry != 0 {
                        for a in a_hi {
                            carry = adc(carry, *a, 0, a);
                            if carry == 0 {
                                break;
                            }
                        }
                    }

                    carry as BigDigit
                }

                /// Two argument addition of raw slices:
                /// a += b
                ///
                /// The caller _must_ ensure that a is big enough to store the result - typically this means
                /// resizing a to max(a.len(), b.len()) + 1, to fit a possible carry.
                pub(super) fn add2(a: &mut [BigDigit], b: &[BigDigit]) {
                    let carry = __add2(a, b);

                    debug_assert!(carry == 0);
                }

                forward_all_binop_to_val_ref_commutative!(impl Add for BigUint, add);
                forward_val_assign!(impl AddAssign for BigUint, add_assign);

                impl Add<&BigUint> for BigUint {
                    type Output = BigUint;

                    fn add(mut self, other: &BigUint) -> BigUint {
                        self += other;
                        self
                    }
                }
                impl AddAssign<&BigUint> for BigUint {
                    #[inline] fn add_assign(&mut self, other: &BigUint) {
                        let self_len = self.data.len();
                        let carry = if self_len < other.data.len() {
                            let lo_carry = __add2(&mut self.data[..], &other.data[..self_len]);
                            self.data.extend_from_slice(&other.data[self_len..]);
                            __add2(&mut self.data[self_len..], &[lo_carry])
                        } else {
                            __add2(&mut self.data[..], &other.data[..])
                        };
                        if carry != 0 {
                            self.data.push(carry);
                        }
                    }
                }

                promote_unsigned_scalars!(impl Add for BigUint, add);
                promote_unsigned_scalars_assign!(impl AddAssign for BigUint, add_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u32> for BigUint, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u64> for BigUint, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u128> for BigUint, add);

                impl Add<u32> for BigUint {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u32) -> BigUint {
                        self += other;
                        self
                    }
                }

                impl AddAssign<u32> for BigUint {
                    #[inline] fn add_assign(&mut self, other: u32) {
                        if other != 0 {
                            if self.data.is_empty() {
                                self.data.push(0);
                            }

                            let carry = __add2(&mut self.data, &[other as BigDigit]);
                            if carry != 0 {
                                self.data.push(carry);
                            }
                        }
                    }
                }

                impl Add<u64> for BigUint {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u64) -> BigUint {
                        self += other;
                        self
                    }
                }

                impl AddAssign<u64> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn add_assign(&mut self, other: u64) {
                            let (hi, lo) = big_digit::from_doublebigdigit(other);
                            if hi == 0 {
                                *self += lo;
                            } else {
                                while self.data.len() < 2 {
                                    self.data.push(0);
                                }

                                let carry = __add2(&mut self.data, &[lo, hi]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }

                        #[inline]
                        fn add_assign(&mut self, other: u64) {
                            if other != 0 {
                                if self.data.is_empty() {
                                    self.data.push(0);
                                }

                                let carry = __add2(&mut self.data, &[other as BigDigit]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                    );
                }

                impl Add<u128> for BigUint {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u128) -> BigUint {
                        self += other;
                        self
                    }
                }

                impl AddAssign<u128> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn add_assign(&mut self, other: u128) {
                            if other <= u128::from(u64::MAX) {
                                *self += other as u64
                            } else {
                                let (a, b, c, d) = super::u32_from_u128(other);
                                let carry = if a > 0 {
                                    while self.data.len() < 4 {
                                        self.data.push(0);
                                    }
                                    __add2(&mut self.data, &[d, c, b, a])
                                } else {
                                    debug_assert!(b > 0);
                                    while self.data.len() < 3 {
                                        self.data.push(0);
                                    }
                                    __add2(&mut self.data, &[d, c, b])
                                };

                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }

                        #[inline]
                        fn add_assign(&mut self, other: u128) {
                            let (hi, lo) = big_digit::from_doublebigdigit(other);
                            if hi == 0 {
                                *self += lo;
                            } else {
                                while self.data.len() < 2 {
                                    self.data.push(0);
                                }

                                let carry = __add2(&mut self.data, &[lo, hi]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                    );
                }

                impl CheckedAdd for BigUint {
                    #[inline] fn checked_add(&self, v: &BigUint) -> Option<BigUint> {
                        Some(self.add(v))
                    }
                }

                impl_sum_iter_type!(BigUint);
            }

            pub mod division
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    num::
                    {
                        big::{ big_digit::{ self, BigDigit, DoubleBigDigit, HALF, HALF_BITS }, UsizePromotion },
                        integers::{ Integer },
                        traits::{ CheckedDiv, CheckedEuclid, Euclid, One, ToPrimitive, Zero },
                    },
                    ops::{Div, DivAssign, Rem, RemAssign},
                    *,
                };

                use super::addition::__add2;
                use super::{cmp_slice, BigUint};
                
                pub(super) const FAST_DIV_WIDE: bool = true;

                /// Divide a two digit numerator by a one digit divisor, returns quotient and remainder.
                #[inline] fn div_wide(hi: BigDigit, lo: BigDigit, divisor: BigDigit) -> (BigDigit, BigDigit)
                {
                    
                    debug_assert!(hi < divisor);
                    
                    unsafe {
                        let (div, rem);

                        macro_rules! div {
                                () => {
                                    "div {0:r}"
                                };
                            }

                        arch::asm!
                        (
                            div!(),
                            in(reg) divisor,
                            inout("dx") hi => rem,
                            inout("ax") lo => div,
                            options(pure, nomem, nostack),
                        );

                        (div, rem)
                    }
                }

                /// For small divisors, we can divide without promoting to `DoubleBigDigit` by
                /// using half-size pieces of digit, like long-division.
                #[inline]
                fn div_half(rem: BigDigit, digit: BigDigit, divisor: BigDigit) -> (BigDigit, BigDigit)
                {
                    debug_assert!(rem < divisor && divisor <= HALF);
                    let (hi, rem) = ((rem << HALF_BITS) | (digit >> HALF_BITS)).div_rem(&divisor);
                    let (lo, rem) = ((rem << HALF_BITS) | (digit & HALF)).div_rem(&divisor);
                    ((hi << HALF_BITS) | lo, rem)
                }

                #[inline]
                pub(super) fn div_rem_digit(mut a: BigUint, b: BigDigit) -> (BigUint, BigDigit) {
                    if b == 0 {
                        panic!("attempt to divide by zero")
                    }

                    let mut rem = 0;

                    if !FAST_DIV_WIDE && b <= big_digit::HALF {
                        for d in a.data.iter_mut().rev() {
                            let (q, r) = div_half(rem, *d, b);
                            *d = q;
                            rem = r;
                        }
                    } else {
                        for d in a.data.iter_mut().rev() {
                            let (q, r) = div_wide(rem, *d, b);
                            *d = q;
                            rem = r;
                        }
                    }

                    (a.normalized(), rem)
                }

                #[inline]
                fn rem_digit(a: &BigUint, b: BigDigit) -> BigDigit {
                    if b == 0 {
                        panic!("attempt to divide by zero")
                    }

                    let mut rem = 0;

                    if !FAST_DIV_WIDE && b <= big_digit::HALF {
                        for &digit in a.data.iter().rev() {
                            let (_, r) = div_half(rem, digit, b);
                            rem = r;
                        }
                    } else {
                        for &digit in a.data.iter().rev() {
                            let (_, r) = div_wide(rem, digit, b);
                            rem = r;
                        }
                    }

                    rem
                }

                /// Subtract a multiple.
                /// a -= b * c
                /// Returns a borrow (if a < b then borrow > 0).
                fn sub_mul_digit_same_len(a: &mut [BigDigit], b: &[BigDigit], c: BigDigit) -> BigDigit {
                    debug_assert!(a.len() == b.len());

                    // carry is between -big_digit::MAX and 0, so to avoid overflow we store
                    // offset_carry = carry + big_digit::MAX
                    let mut offset_carry = big_digit::MAX;

                    for (x, y) in a.iter_mut().zip(b) {
                        // We want to calculate sum = x - y * c + carry.
                        // sum >= -(big_digit::MAX * big_digit::MAX) - big_digit::MAX
                        // sum <= big_digit::MAX
                        // Offsetting sum by (big_digit::MAX << big_digit::BITS) puts it in DoubleBigDigit range.
                        let offset_sum = big_digit::to_doublebigdigit(big_digit::MAX, *x)
                            - big_digit::MAX as DoubleBigDigit
                            + offset_carry as DoubleBigDigit
                            - *y as DoubleBigDigit * c as DoubleBigDigit;

                        let (new_offset_carry, new_x) = big_digit::from_doublebigdigit(offset_sum);
                        offset_carry = new_offset_carry;
                        *x = new_x;
                    }

                    // Return the borrow.
                    big_digit::MAX - offset_carry
                }

                fn div_rem(mut u: BigUint, mut d: BigUint) -> (BigUint, BigUint) {
                    if d.is_zero() {
                        panic!("attempt to divide by zero")
                    }
                    if u.is_zero() {
                        return (BigUint::ZERO, BigUint::ZERO);
                    }

                    if d.data.len() == 1 {
                        if d.data == [1] {
                            return (u, BigUint::ZERO);
                        }
                        let (div, rem) = div_rem_digit(u, d.data[0]);
                        // reuse d
                        d.data.clear();
                        d += rem;
                        return (div, d);
                    }

                    // Required or the q_len calculation below can underflow:
                    match u.cmp(&d) {
                        Less => return (BigUint::ZERO, u),
                        Equal => {
                            u.set_one();
                            return (u, BigUint::ZERO);
                        }
                        Greater => {} // Do nothing
                    }

                    // This algorithm is from Knuth, TAOCP vol 2 section 4.3, algorithm D:
                    //
                    // First, normalize the arguments so the highest bit in the highest digit of the divisor is
                    // set: the main loop uses the highest digit of the divisor for generating guesses, so we
                    // want it to be the largest number we can efficiently divide by.
                    //
                    let shift = d.data.last().unwrap().leading_zeros() as usize;

                    if shift == 0 {
                        // no need to clone d
                        div_rem_core(u, &d.data)
                    } else {
                        let (q, r) = div_rem_core(u << shift, &(d << shift).data);
                        // renormalize the remainder
                        (q, r >> shift)
                    }
                }

                pub(super) fn div_rem_ref(u: &BigUint, d: &BigUint) -> (BigUint, BigUint) {
                    if d.is_zero() {
                        panic!("attempt to divide by zero")
                    }
                    if u.is_zero() {
                        return (BigUint::ZERO, BigUint::ZERO);
                    }

                    if d.data.len() == 1 {
                        if d.data == [1] {
                            return (u.clone(), BigUint::ZERO);
                        }

                        let (div, rem) = div_rem_digit(u.clone(), d.data[0]);
                        return (div, rem.into());
                    }

                    // Required or the q_len calculation below can underflow:
                    match u.cmp(d) {
                        Less => return (BigUint::ZERO, u.clone()),
                        Equal => return (One::one(), BigUint::ZERO),
                        Greater => {} // Do nothing
                    }

                    // This algorithm is from Knuth, TAOCP vol 2 section 4.3, algorithm D:
                    //
                    // First, normalize the arguments so the highest bit in the highest digit of the divisor is
                    // set: the main loop uses the highest digit of the divisor for generating guesses, so we
                    // want it to be the largest number we can efficiently divide by.
                    //
                    let shift = d.data.last().unwrap().leading_zeros() as usize;

                    if shift == 0 {
                        // no need to clone d
                        div_rem_core(u.clone(), &d.data)
                    } else {
                        let (q, r) = div_rem_core(u << shift, &(d << shift).data);
                        // renormalize the remainder
                        (q, r >> shift)
                    }
                }

                /// An implementation of the base division algorithm.
                /// Knuth, TAOCP vol 2 section 4.3.1, algorithm D, with an improvement from exercises 19-21.
                fn div_rem_core(mut a: BigUint, b: &[BigDigit]) -> (BigUint, BigUint) {
                    debug_assert!(a.data.len() >= b.len() && b.len() > 1);
                    debug_assert!(b.last().unwrap().leading_zeros() == 0);

                    // The algorithm works by incrementally calculating "guesses", q0, for the next digit of the
                    // quotient. Once we have any number q0 such that (q0 << j) * b <= a, we can set
                    //
                    //     q += q0 << j
                    //     a -= (q0 << j) * b
                    //
                    // and then iterate until a < b. Then, (q, a) will be our desired quotient and remainder.
                    //
                    // q0, our guess, is calculated by dividing the last three digits of a by the last two digits of
                    // b - this will give us a guess that is close to the actual quotient, but is possibly greater.
                    // It can only be greater by 1 and only in rare cases, with probability at most
                    // 2^-(big_digit::BITS-1) for random a, see TAOCP 4.3.1 exercise 21.
                    //
                    // If the quotient turns out to be too large, we adjust it by 1:
                    // q -= 1 << j
                    // a += b << j

                    // a0 stores an additional extra most significant digit of the dividend, not stored in a.
                    let mut a0 = 0;

                    // [b1, b0] are the two most significant digits of the divisor. They never change.
                    let b0 = b[b.len() - 1];
                    let b1 = b[b.len() - 2];

                    let q_len = a.data.len() - b.len() + 1;
                    let mut q = BigUint {
                        data: vec![0; q_len],
                    };

                    for j in (0..q_len).rev() {
                        debug_assert!(a.data.len() == b.len() + j);

                        let a1 = *a.data.last().unwrap();
                        let a2 = a.data[a.data.len() - 2];

                        // The first q0 estimate is [a1,a0] / b0. It will never be too small, it may be too large
                        // by at most 2.
                        let (mut q0, mut r) = if a0 < b0 {
                            let (q0, r) = div_wide(a0, a1, b0);
                            (q0, r as DoubleBigDigit)
                        } else {
                            debug_assert!(a0 == b0);
                            // Avoid overflowing q0, we know the quotient fits in BigDigit.
                            // [a1,a0] = b0 * (1<<BITS - 1) + (a0 + a1)
                            (big_digit::MAX, a0 as DoubleBigDigit + a1 as DoubleBigDigit)
                        };

                        // r = [a1,a0] - q0 * b0
                        //
                        // Now we want to compute a more precise estimate [a2,a1,a0] / [b1,b0] which can only be
                        // less or equal to the current q0.
                        //
                        // q0 is too large if:
                        // [a2,a1,a0] < q0 * [b1,b0]
                        // (r << BITS) + a2 < q0 * b1
                        while r <= big_digit::MAX as DoubleBigDigit
                            && big_digit::to_doublebigdigit(r as BigDigit, a2)
                                < q0 as DoubleBigDigit * b1 as DoubleBigDigit
                        {
                            q0 -= 1;
                            r += b0 as DoubleBigDigit;
                        }

                        // q0 is now either the correct quotient digit, or in rare cases 1 too large.
                        // Subtract (q0 << j) from a. This may overflow, in which case we will have to correct.

                        let mut borrow = sub_mul_digit_same_len(&mut a.data[j..], b, q0);
                        if borrow > a0 {
                            // q0 is too large. We need to add back one multiple of b.
                            q0 -= 1;
                            borrow -= __add2(&mut a.data[j..], b);
                        }
                        // The top digit of a, stored in a0, has now been zeroed.
                        debug_assert!(borrow == a0);

                        q.data[j] = q0;

                        // Pop off the next top digit of a.
                        a0 = a.data.pop().unwrap();
                    }

                    a.data.push(a0);
                    a.normalize();

                    debug_assert_eq!(cmp_slice(&a.data, b), Less);

                    (q.normalized(), a)
                }

                forward_val_ref_binop!(impl Div for BigUint, div);
                forward_ref_val_binop!(impl Div for BigUint, div);
                forward_val_assign!(impl DivAssign for BigUint, div_assign);

                impl Div<BigUint> for BigUint {
                    type Output = BigUint;

                    #[inline] fn div(self, other: BigUint) -> BigUint {
                        let (q, _) = div_rem(self, other);
                        q
                    }
                }

                impl Div<&BigUint> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn div(self, other: &BigUint) -> BigUint {
                        let (q, _) = self.div_rem(other);
                        q
                    }
                }
                impl DivAssign<&BigUint> for BigUint {
                    #[inline] fn div_assign(&mut self, other: &BigUint) {
                        *self = &*self / other;
                    }
                }

                promote_unsigned_scalars!(impl Div for BigUint, div);
                promote_unsigned_scalars_assign!(impl DivAssign for BigUint, div_assign);
                forward_all_scalar_binop_to_val_val!(impl Div<u32> for BigUint, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u64> for BigUint, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u128> for BigUint, div);

                impl Div<u32> for BigUint {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u32) -> BigUint {
                        let (q, _) = div_rem_digit(self, other as BigDigit);
                        q
                    }
                }
                impl DivAssign<u32> for BigUint {
                    #[inline] fn div_assign(&mut self, other: u32) {
                        *self = &*self / other;
                    }
                }

                impl Div<BigUint> for u32 {
                    type Output = BigUint;

                    #[inline] fn div(self, other: BigUint) -> BigUint {
                        match other.data.len() {
                            0 => panic!("attempt to divide by zero"),
                            1 => From::from(self as BigDigit / other.data[0]),
                            _ => BigUint::ZERO,
                        }
                    }
                }

                impl Div<u64> for BigUint {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u64) -> BigUint {
                        let (q, _) = div_rem(self, From::from(other));
                        q
                    }
                }
                impl DivAssign<u64> for BigUint {
                    #[inline] fn div_assign(&mut self, other: u64) {
                        // a vec of size 0 does not allocate, so this is fairly cheap
                        let temp = mem::replace(self, Self::ZERO);
                        *self = temp / other;
                    }
                }

                impl Div<BigUint> for u64 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / u64::from(other.data[0])),
                                2 => From::from(self / big_digit::to_doublebigdigit(other.data[1], other.data[0])),
                                _ => BigUint::ZERO,
                            }
                        }

                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / other.data[0]),
                                _ => BigUint::ZERO,
                            }
                        }
                    );
                }

                impl Div<u128> for BigUint {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u128) -> BigUint {
                        let (q, _) = div_rem(self, From::from(other));
                        q
                    }
                }

                impl DivAssign<u128> for BigUint {
                    #[inline] fn div_assign(&mut self, other: u128) {
                        *self = &*self / other;
                    }
                }

                impl Div<BigUint> for u128 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            use super::u32_to_u128;
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / u128::from(other.data[0])),
                                2 => From::from(
                                    self / u128::from(big_digit::to_doublebigdigit(other.data[1], other.data[0])),
                                ),
                                3 => From::from(self / u32_to_u128(0, other.data[2], other.data[1], other.data[0])),
                                4 => From::from(
                                    self / u32_to_u128(other.data[3], other.data[2], other.data[1], other.data[0]),
                                ),
                                _ => BigUint::ZERO,
                            }
                        }

                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / other.data[0] as u128),
                                2 => From::from(self / big_digit::to_doublebigdigit(other.data[1], other.data[0])),
                                _ => BigUint::ZERO,
                            }
                        }
                    );
                }

                forward_val_ref_binop!(impl Rem for BigUint, rem);
                forward_ref_val_binop!(impl Rem for BigUint, rem);
                forward_val_assign!(impl RemAssign for BigUint, rem_assign);

                impl Rem<BigUint> for BigUint {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: BigUint) -> BigUint {
                        if let Some(other) = other.to_u32() {
                            &self % other
                        } else {
                            let (_, r) = div_rem(self, other);
                            r
                        }
                    }
                }

                impl Rem<&BigUint> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: &BigUint) -> BigUint {
                        if let Some(other) = other.to_u32() {
                            self % other
                        } else {
                            let (_, r) = self.div_rem(other);
                            r
                        }
                    }
                }
                impl RemAssign<&BigUint> for BigUint {
                    #[inline] fn rem_assign(&mut self, other: &BigUint) {
                        *self = &*self % other;
                    }
                }

                promote_unsigned_scalars!(impl Rem for BigUint, rem);
                promote_unsigned_scalars_assign!(impl RemAssign for BigUint, rem_assign);
                forward_all_scalar_binop_to_ref_val!(impl Rem<u32> for BigUint, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u64> for BigUint, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u128> for BigUint, rem);

                impl Rem<u32> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u32) -> BigUint {
                        rem_digit(self, other as BigDigit).into()
                    }
                }
                impl RemAssign<u32> for BigUint {
                    #[inline] fn rem_assign(&mut self, other: u32) {
                        *self = &*self % other;
                    }
                }

                impl Rem<&BigUint> for u32 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: &BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }

                macro_rules! impl_rem_assign_scalar {
                    ($scalar:ty, $to_scalar:ident) => {
                        forward_val_assign_scalar!(impl RemAssign for BigUint, $scalar, rem_assign);
                        impl RemAssign<&BigUint> for $scalar {
                            #[inline]
                            fn rem_assign(&mut self, other: &BigUint) {
                                *self = match other.$to_scalar() {
                                    None => *self,
                                    Some(0) => panic!("attempt to divide by zero"),
                                    Some(v) => *self % v
                                };
                            }
                        }
                    }
                }

                // we can scalar %= BigUint for any scalar, including signed types
                impl_rem_assign_scalar!(u128, to_u128);
                impl_rem_assign_scalar!(usize, to_usize);
                impl_rem_assign_scalar!(u64, to_u64);
                impl_rem_assign_scalar!(u32, to_u32);
                impl_rem_assign_scalar!(u16, to_u16);
                impl_rem_assign_scalar!(u8, to_u8);
                impl_rem_assign_scalar!(i128, to_i128);
                impl_rem_assign_scalar!(isize, to_isize);
                impl_rem_assign_scalar!(i64, to_i64);
                impl_rem_assign_scalar!(i32, to_i32);
                impl_rem_assign_scalar!(i16, to_i16);
                impl_rem_assign_scalar!(i8, to_i8);

                impl Rem<u64> for BigUint {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u64) -> BigUint {
                        let (_, r) = div_rem(self, From::from(other));
                        r
                    }
                }
                impl RemAssign<u64> for BigUint {
                    #[inline] fn rem_assign(&mut self, other: u64) {
                        *self = &*self % other;
                    }
                }

                impl Rem<BigUint> for u64 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }

                impl Rem<u128> for BigUint {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u128) -> BigUint {
                        let (_, r) = div_rem(self, From::from(other));
                        r
                    }
                }

                impl RemAssign<u128> for BigUint {
                    #[inline] fn rem_assign(&mut self, other: u128) {
                        *self = &*self % other;
                    }
                }

                impl Rem<BigUint> for u128 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }

                impl CheckedDiv for BigUint {
                    #[inline] fn checked_div(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div(v))
                    }
                }

                impl CheckedEuclid for BigUint {
                    #[inline] fn checked_div_euclid(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div_euclid(v))
                    }

                    #[inline] fn checked_rem_euclid(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.rem_euclid(v))
                    }

                    fn checked_div_rem_euclid(&self, v: &Self) -> Option<(Self, Self)> {
                        Some(self.div_rem_euclid(v))
                    }
                }

                impl Euclid for BigUint {
                    #[inline] fn div_euclid(&self, v: &BigUint) -> BigUint {
                        // trivially same as regular division
                        self / v
                    }

                    #[inline] fn rem_euclid(&self, v: &BigUint) -> BigUint {
                        // trivially same as regular remainder
                        self % v
                    }

                    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
                        // trivially same as regular division and remainder
                        self.div_rem(v)
                    }
                }
            }

            pub mod multiplication
            {
                use ::
                {
                    cmp::Ordering,
                    iter::Product,
                    num::
                    {
                        big::
                        {
                            big_digit::{self, BigDigit, DoubleBigDigit},
                            Sign::{self, Minus, NoSign, Plus},
                            BigInt, UsizePromotion,
                        },
                        traits::{ CheckedMul, FromPrimitive, One, Zero },
                    },
                    ops::{ Mul, MulAssign },
                    *,
                };

                use super::addition::{__add2, add2};
                use super::subtraction::sub2;
                use super::{biguint_from_vec, cmp_slice, BigUint, IntDigits};
                
                #[inline]
                pub(super) fn mac_with_carry(
                    a: BigDigit,
                    b: BigDigit,
                    c: BigDigit,
                    acc: &mut DoubleBigDigit,
                ) -> BigDigit {
                    *acc += DoubleBigDigit::from(a);
                    *acc += DoubleBigDigit::from(b) * DoubleBigDigit::from(c);
                    let lo = *acc as BigDigit;
                    *acc >>= big_digit::BITS;
                    lo
                }

                #[inline]
                fn mul_with_carry(a: BigDigit, b: BigDigit, acc: &mut DoubleBigDigit) -> BigDigit {
                    *acc += DoubleBigDigit::from(a) * DoubleBigDigit::from(b);
                    let lo = *acc as BigDigit;
                    *acc >>= big_digit::BITS;
                    lo
                }

                /// Three argument multiply accumulate:
                /// acc += b * c
                fn mac_digit(acc: &mut [BigDigit], b: &[BigDigit], c: BigDigit) {
                    if c == 0 {
                        return;
                    }

                    let mut carry = 0;
                    let (a_lo, a_hi) = acc.split_at_mut(b.len());

                    for (a, &b) in a_lo.iter_mut().zip(b) {
                        *a = mac_with_carry(*a, b, c, &mut carry);
                    }

                    let (carry_hi, carry_lo) = big_digit::from_doublebigdigit(carry);

                    let final_carry = if carry_hi == 0 {
                        __add2(a_hi, &[carry_lo])
                    } else {
                        __add2(a_hi, &[carry_hi, carry_lo])
                    };
                    assert_eq!(final_carry, 0, "carry overflow during multiplication!");
                }

                fn bigint_from_slice(slice: &[BigDigit]) -> BigInt {
                    BigInt::from(biguint_from_vec(slice.to_vec()))
                }

                /// Three argument multiply accumulate:
                /// acc += b * c
                
                fn mac3(mut acc: &mut [BigDigit], mut b: &[BigDigit], mut c: &[BigDigit]) {
                    // Least-significant zeros have no effect on the output.
                    if let Some(&0) = b.first() {
                        if let Some(nz) = b.iter().position(|&d| d != 0) {
                            b = &b[nz..];
                            acc = &mut acc[nz..];
                        } else {
                            return;
                        }
                    }
                    if let Some(&0) = c.first() {
                        if let Some(nz) = c.iter().position(|&d| d != 0) {
                            c = &c[nz..];
                            acc = &mut acc[nz..];
                        } else {
                            return;
                        }
                    }

                    let acc = acc;
                    let (x, y) = if b.len() < c.len() { (b, c) } else { (c, b) };

                    // We use four algorithms for different input sizes.
                    //
                    // - For small inputs, long multiplication is fastest.
                    // - If y is at least least twice as long as x, split using Half-Karatsuba.
                    // - Next we use Karatsuba multiplication (Toom-2), which we have optimized
                    //   to avoid unnecessary allocations for intermediate values.
                    // - For the largest inputs we use Toom-3, which better optimizes the
                    //   number of operations, but uses more temporary allocations.
                    //
                    // The thresholds are somewhat arbitrary, chosen by evaluating the results
                    // of `cargo bench --bench bigint multiply`.

                    if x.len() <= 32 {
                        // Long multiplication:
                        for (i, xi) in x.iter().enumerate() {
                            mac_digit(&mut acc[i..], y, *xi);
                        }
                    } else if x.len() * 2 <= y.len() {
                        // Karatsuba Multiplication for factors with significant length disparity.
                        //
                        // The Half-Karatsuba Multiplication Algorithm is a specialized case of
                        // the normal Karatsuba multiplication algorithm, designed for the scenario
                        // where y has at least twice as many base digits as x.
                        //
                        // In this case y (the longer input) is split into high2 and low2,
                        // at m2 (half the length of y) and x (the shorter input),
                        // is used directly without splitting.
                        //
                        // The algorithm then proceeds as follows:
                        //
                        // 1. Compute the product z0 = x * low2.
                        // 2. Compute the product temp = x * high2.
                        // 3. Adjust the weight of temp by adding m2 (* NBASE ^ m2)
                        // 4. Add temp and z0 to obtain the final result.
                        //
                        // Proof:
                        //
                        // The algorithm can be derived from the original Karatsuba algorithm by
                        // simplifying the formula when the shorter factor x is not split into
                        // high and low parts, as shown below.
                        //
                        // Original Karatsuba formula:
                        //
                        //     result = (z2 * NBASE ^ (m2 × 2)) + ((z1 - z2 - z0) * NBASE ^ m2) + z0
                        //
                        // Substitutions:
                        //
                        //     low1 = x
                        //     high1 = 0
                        //
                        // Applying substitutions:
                        //
                        //     z0 = (low1 * low2)
                        //        = (x * low2)
                        //
                        //     z1 = ((low1 + high1) * (low2 + high2))
                        //        = ((x + 0) * (low2 + high2))
                        //        = (x * low2) + (x * high2)
                        //
                        //     z2 = (high1 * high2)
                        //        = (0 * high2)
                        //        = 0
                        //
                        // Simplified using the above substitutions:
                        //
                        //     result = (z2 * NBASE ^ (m2 × 2)) + ((z1 - z2 - z0) * NBASE ^ m2) + z0
                        //            = (0 * NBASE ^ (m2 × 2)) + ((z1 - 0 - z0) * NBASE ^ m2) + z0
                        //            = ((z1 - z0) * NBASE ^ m2) + z0
                        //            = ((z1 - z0) * NBASE ^ m2) + z0
                        //            = (x * high2) * NBASE ^ m2 + z0
                        let m2 = y.len() / 2;
                        let (low2, high2) = y.split_at(m2);

                        // (x * high2) * NBASE ^ m2 + z0
                        mac3(acc, x, low2);
                        mac3(&mut acc[m2..], x, high2);
                    } else if x.len() <= 256 {
                        // Karatsuba multiplication:
                        //
                        // The idea is that we break x and y up into two smaller numbers that each have about half
                        // as many digits, like so (note that multiplying by b is just a shift):
                        //
                        // x = x0 + x1 * b
                        // y = y0 + y1 * b
                        //
                        // With some algebra, we can compute x * y with three smaller products, where the inputs to
                        // each of the smaller products have only about half as many digits as x and y:
                        //
                        // x * y = (x0 + x1 * b) * (y0 + y1 * b)
                        //
                        // x * y = x0 * y0
                        //       + x0 * y1 * b
                        //       + x1 * y0 * b
                        //       + x1 * y1 * b^2
                        //
                        // Let p0 = x0 * y0 and p2 = x1 * y1:
                        //
                        // x * y = p0
                        //       + (x0 * y1 + x1 * y0) * b
                        //       + p2 * b^2
                        //
                        // The real trick is that middle term:
                        //
                        //         x0 * y1 + x1 * y0
                        //
                        //       = x0 * y1 + x1 * y0 - p0 + p0 - p2 + p2
                        //
                        //       = x0 * y1 + x1 * y0 - x0 * y0 - x1 * y1 + p0 + p2
                        //
                        // Now we complete the square:
                        //
                        //       = -(x0 * y0 - x0 * y1 - x1 * y0 + x1 * y1) + p0 + p2
                        //
                        //       = -((x1 - x0) * (y1 - y0)) + p0 + p2
                        //
                        // Let p1 = (x1 - x0) * (y1 - y0), and substitute back into our original formula:
                        //
                        // x * y = p0
                        //       + (p0 + p2 - p1) * b
                        //       + p2 * b^2
                        //
                        // Where the three intermediate products are:
                        //
                        // p0 = x0 * y0
                        // p1 = (x1 - x0) * (y1 - y0)
                        // p2 = x1 * y1
                        //
                        // In doing the computation, we take great care to avoid unnecessary temporary variables
                        // (since creating a BigUint requires a heap allocation): thus, we rearrange the formula a
                        // bit so we can use the same temporary variable for all the intermediate products:
                        //
                        // x * y = p2 * b^2 + p2 * b
                        //       + p0 * b + p0
                        //       - p1 * b
                        //
                        // The other trick we use is instead of doing explicit shifts, we slice acc at the
                        // appropriate offset when doing the add.

                        // When x is smaller than y, it's significantly faster to pick b such that x is split in
                        // half, not y:
                        let b = x.len() / 2;
                        let (x0, x1) = x.split_at(b);
                        let (y0, y1) = y.split_at(b);

                        // We reuse the same BigUint for all the intermediate multiplies and have to size p
                        // appropriately here: x1.len() >= x0.len and y1.len() >= y0.len():
                        let len = x1.len() + y1.len() + 1;
                        let mut p = BigUint { data: vec![0; len] };

                        // p2 = x1 * y1
                        mac3(&mut p.data, x1, y1);

                        // Not required, but the adds go faster if we drop any unneeded 0s from the end:
                        p.normalize();

                        add2(&mut acc[b..], &p.data);
                        add2(&mut acc[b * 2..], &p.data);

                        // Zero out p before the next multiply:
                        p.data.truncate(0);
                        p.data.resize(len, 0);

                        // p0 = x0 * y0
                        mac3(&mut p.data, x0, y0);
                        p.normalize();

                        add2(acc, &p.data);
                        add2(&mut acc[b..], &p.data);

                        // p1 = (x1 - x0) * (y1 - y0)
                        // We do this one last, since it may be negative and acc can't ever be negative:
                        let (j0_sign, j0) = sub_sign(x1, x0);
                        let (j1_sign, j1) = sub_sign(y1, y0);

                        match j0_sign * j1_sign {
                            Plus => {
                                p.data.truncate(0);
                                p.data.resize(len, 0);

                                mac3(&mut p.data, &j0.data, &j1.data);
                                p.normalize();

                                sub2(&mut acc[b..], &p.data);
                            }
                            Minus => {
                                mac3(&mut acc[b..], &j0.data, &j1.data);
                            }
                            NoSign => (),
                        }
                    } else {
                        // Toom-3 multiplication:
                        //
                        // Toom-3 is like Karatsuba above, but dividing the inputs into three parts.
                        // Both are instances of Toom-Cook, using `k=3` and `k=2` respectively.
                        //
                        // The general idea is to treat the large integers digits as
                        // polynomials of a certain degree and determine the coefficients/digits
                        // of the product of the two via interpolation of the polynomial product.
                        let i = y.len() / 3 + 1;

                        let x0_len = Ord::min(x.len(), i);
                        let x1_len = Ord::min(x.len() - x0_len, i);

                        let y0_len = i;
                        let y1_len = Ord::min(y.len() - y0_len, i);

                        // Break x and y into three parts, representating an order two polynomial.
                        // t is chosen to be the size of a digit so we can use faster shifts
                        // in place of multiplications.
                        //
                        // x(t) = x2*t^2 + x1*t + x0
                        let x0 = bigint_from_slice(&x[..x0_len]);
                        let x1 = bigint_from_slice(&x[x0_len..x0_len + x1_len]);
                        let x2 = bigint_from_slice(&x[x0_len + x1_len..]);

                        // y(t) = y2*t^2 + y1*t + y0
                        let y0 = bigint_from_slice(&y[..y0_len]);
                        let y1 = bigint_from_slice(&y[y0_len..y0_len + y1_len]);
                        let y2 = bigint_from_slice(&y[y0_len + y1_len..]);

                        // Let w(t) = x(t) * y(t)
                        //
                        // This gives us the following order-4 polynomial.
                        //
                        // w(t) = w4*t^4 + w3*t^3 + w2*t^2 + w1*t + w0
                        //
                        // We need to find the coefficients w4, w3, w2, w1 and w0. Instead
                        // of simply multiplying the x and y in total, we can evaluate w
                        // at 5 points. An n-degree polynomial is uniquely identified by (n + 1)
                        // points.
                        //
                        // It is arbitrary as to what points we evaluate w at but we use the
                        // following.
                        //
                        // w(t) at t = 0, 1, -1, -2 and inf
                        //
                        // The values for w(t) in terms of x(t)*y(t) at these points are:
                        //
                        // let a = w(0)   = x0 * y0
                        // let b = w(1)   = (x2 + x1 + x0) * (y2 + y1 + y0)
                        // let c = w(-1)  = (x2 - x1 + x0) * (y2 - y1 + y0)
                        // let d = w(-2)  = (4*x2 - 2*x1 + x0) * (4*y2 - 2*y1 + y0)
                        // let e = w(inf) = x2 * y2 as t -> inf

                        // x0 + x2, avoiding temporaries
                        let p = &x0 + &x2;

                        // y0 + y2, avoiding temporaries
                        let q = &y0 + &y2;

                        // x2 - x1 + x0, avoiding temporaries
                        let p2 = &p - &x1;

                        // y2 - y1 + y0, avoiding temporaries
                        let q2 = &q - &y1;

                        // w(0)
                        let r0 = &x0 * &y0;

                        // w(inf)
                        let r4 = &x2 * &y2;

                        // w(1)
                        let r1 = (p + x1) * (q + y1);

                        // w(-1)
                        let r2 = &p2 * &q2;

                        // w(-2)
                        let r3 = ((p2 + x2) * 2 - x0) * ((q2 + y2) * 2 - y0);

                        // Evaluating these points gives us the following system of linear equations.
                        //
                        //  0  0  0  0  1 | a
                        //  1  1  1  1  1 | b
                        //  1 -1  1 -1  1 | c
                        // 16 -8  4 -2  1 | d
                        //  1  0  0  0  0 | e
                        //
                        // The solved equation (after gaussian elimination or similar)
                        // in terms of its coefficients:
                        //
                        // w0 = w(0)
                        // w1 = w(0)/2 + w(1)/3 - w(-1) + w(-2)/6 - 2*w(inf)
                        // w2 = -w(0) + w(1)/2 + w(-1)/2 - w(inf)
                        // w3 = -w(0)/2 + w(1)/6 + w(-1)/2 - w(-2)/6 + 2*w(inf)
                        // w4 = w(inf)
                        //
                        // This particular sequence is given by Bodrato and is an interpolation
                        // of the above equations.
                        let mut comp3: BigInt = (r3 - &r1) / 3u32;
                        let mut comp1: BigInt = (r1 - &r2) >> 1;
                        let mut comp2: BigInt = r2 - &r0;
                        comp3 = ((&comp2 - comp3) >> 1) + (&r4 << 1);
                        comp2 += &comp1 - &r4;
                        comp1 -= &comp3;

                        // Recomposition. The coefficients of the polynomial are now known.
                        //
                        // Evaluate at w(t) where t is our given base to get the result.
                        //
                        //     let bits = u64::from(big_digit::BITS) * i as u64;
                        //     let result = r0
                        //         + (comp1 << bits)
                        //         + (comp2 << (2 * bits))
                        //         + (comp3 << (3 * bits))
                        //         + (r4 << (4 * bits));
                        //     let result_pos = result.to_biguint().unwrap();
                        //     add2(&mut acc[..], &result_pos.data);
                        //
                        // But with less intermediate copying:
                        for (j, result) in [&r0, &comp1, &comp2, &comp3, &r4].iter().enumerate().rev() {
                            match result.sign() {
                                Plus => add2(&mut acc[i * j..], result.digits()),
                                Minus => sub2(&mut acc[i * j..], result.digits()),
                                NoSign => {}
                            }
                        }
                    }
                }

                fn mul3(x: &[BigDigit], y: &[BigDigit]) -> BigUint {
                    let len = x.len() + y.len() + 1;
                    let mut prod = BigUint { data: vec![0; len] };

                    mac3(&mut prod.data, x, y);
                    prod.normalized()
                }

                fn scalar_mul(a: &mut BigUint, b: BigDigit) {
                    match b {
                        0 => a.set_zero(),
                        1 => {}
                        _ => {
                            if b.is_power_of_two() {
                                *a <<= b.trailing_zeros();
                            } else {
                                let mut carry = 0;
                                for a in a.data.iter_mut() {
                                    *a = mul_with_carry(*a, b, &mut carry);
                                }
                                if carry != 0 {
                                    a.data.push(carry as BigDigit);
                                }
                            }
                        }
                    }
                }

                fn sub_sign(mut a: &[BigDigit], mut b: &[BigDigit]) -> (Sign, BigUint) {
                    // Normalize:
                    if let Some(&0) = a.last() {
                        a = &a[..a.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1)];
                    }
                    if let Some(&0) = b.last() {
                        b = &b[..b.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1)];
                    }

                    match cmp_slice(a, b) {
                        Ordering::Greater => {
                            let mut a = a.to_vec();
                            sub2(&mut a, b);
                            (Plus, biguint_from_vec(a))
                        }
                        Ordering::Less => {
                            let mut b = b.to_vec();
                            sub2(&mut b, a);
                            (Minus, biguint_from_vec(b))
                        }
                        Ordering::Equal => (NoSign, BigUint::ZERO),
                    }
                }

                macro_rules! impl_mul {
                    ($(impl Mul<$Other:ty> for $Self:ty;)*) => {$(
                        impl Mul<$Other> for $Self {
                            type Output = BigUint;

                            #[inline]
                            fn mul(self, other: $Other) -> BigUint {
                                match (&*self.data, &*other.data) {
                                    // multiply by zero
                                    (&[], _) | (_, &[]) => BigUint::ZERO,
                                    // multiply by a scalar
                                    (_, &[digit]) => self * digit,
                                    (&[digit], _) => other * digit,
                                    // full multiplication
                                    (x, y) => mul3(x, y),
                                }
                            }
                        }
                    )*}
                }
                impl_mul! {
                    impl Mul<BigUint> for BigUint;
                    impl Mul<BigUint> for &BigUint;
                    impl Mul<&BigUint> for BigUint;
                    impl Mul<&BigUint> for &BigUint;
                }

                macro_rules! impl_mul_assign {
                    ($(impl MulAssign<$Other:ty> for BigUint;)*) => {$(
                        impl MulAssign<$Other> for BigUint {
                            #[inline]
                            fn mul_assign(&mut self, other: $Other) {
                                match (&*self.data, &*other.data) {
                                    // multiply by zero
                                    (&[], _) => {},
                                    (_, &[]) => self.set_zero(),
                                    // multiply by a scalar
                                    (_, &[digit]) => *self *= digit,
                                    (&[digit], _) => *self = other * digit,
                                    // full multiplication
                                    (x, y) => *self = mul3(x, y),
                                }
                            }
                        }
                    )*}
                }
                impl_mul_assign! {
                    impl MulAssign<BigUint> for BigUint;
                    impl MulAssign<&BigUint> for BigUint;
                }

                promote_unsigned_scalars!(impl Mul for BigUint, mul);
                promote_unsigned_scalars_assign!(impl MulAssign for BigUint, mul_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u32> for BigUint, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u64> for BigUint, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u128> for BigUint, mul);

                impl Mul<u32> for BigUint {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u32) -> BigUint {
                        self *= other;
                        self
                    }
                }
                impl MulAssign<u32> for BigUint {
                    #[inline] fn mul_assign(&mut self, other: u32) {
                        scalar_mul(self, other as BigDigit);
                    }
                }

                impl Mul<u64> for BigUint {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u64) -> BigUint {
                        self *= other;
                        self
                    }
                }
                impl MulAssign<u64> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn mul_assign(&mut self, other: u64) {
                            if let Some(other) = BigDigit::from_u64(other) {
                                scalar_mul(self, other);
                            } else {
                                let (hi, lo) = big_digit::from_doublebigdigit(other);
                                *self = mul3(&self.data, &[lo, hi]);
                            }
                        }

                        #[inline]
                        fn mul_assign(&mut self, other: u64) {
                            scalar_mul(self, other);
                        }
                    );
                }

                impl Mul<u128> for BigUint {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u128) -> BigUint {
                        self *= other;
                        self
                    }
                }

                impl MulAssign<u128> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn mul_assign(&mut self, other: u128) {
                            if let Some(other) = BigDigit::from_u128(other) {
                                scalar_mul(self, other);
                            } else {
                                *self = match super::u32_from_u128(other) {
                                    (0, 0, c, d) => mul3(&self.data, &[d, c]),
                                    (0, b, c, d) => mul3(&self.data, &[d, c, b]),
                                    (a, b, c, d) => mul3(&self.data, &[d, c, b, a]),
                                };
                            }
                        }

                        #[inline]
                        fn mul_assign(&mut self, other: u128) {
                            if let Some(other) = BigDigit::from_u128(other) {
                                scalar_mul(self, other);
                            } else {
                                let (hi, lo) = big_digit::from_doublebigdigit(other);
                                *self = mul3(&self.data, &[lo, hi]);
                            }
                        }
                    );
                }

                impl CheckedMul for BigUint {
                    #[inline] fn checked_mul(&self, v: &BigUint) -> Option<BigUint> {
                        Some(self.mul(v))
                    }
                }

                impl_product_iter_type!(BigUint);
            }

            pub mod subtraction
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    num::
                    {
                        big::{ UsizePromotion, big_digit::{self, BigDigit} },
                        traits::{ CheckedSub },
                    },
                    ops::{Sub, SubAssign},
                    *,
                };

                use super::BigUint;
                
                #[inline] fn sbb(borrow: u8, a: u64, b: u64, out: &mut u64) -> u8 {
                    // Safety: There are absolutely no safety concerns with calling `_subborrow_u64`.
                    // It's just unsafe for API consistency with other intrinsics.
                    unsafe { arch::_subborrow_u64(borrow, a, b, out) }
                }

                pub(super) fn sub2(a: &mut [BigDigit], b: &[BigDigit]) {
                    let mut borrow = 0;

                    let len = Ord::min(a.len(), b.len());
                    let (a_lo, a_hi) = a.split_at_mut(len);
                    let (b_lo, b_hi) = b.split_at(len);

                    for (a, b) in a_lo.iter_mut().zip(b_lo) {
                        borrow = sbb(borrow, *a, *b, a);
                    }

                    if borrow != 0 {
                        for a in a_hi {
                            borrow = sbb(borrow, *a, 0, a);
                            if borrow == 0 {
                                break;
                            }
                        }
                    }

                    // note: we're _required_ to fail on underflow
                    assert!(
                        borrow == 0 && b_hi.iter().all(|x| *x == 0),
                        "Cannot subtract b from a because b is larger than a."
                    );
                }

                // Only for the Sub impl. `a` and `b` must have same length.
                #[inline]
                fn __sub2rev(a: &[BigDigit], b: &mut [BigDigit]) -> u8 {
                    debug_assert!(b.len() == a.len());

                    let mut borrow = 0;

                    for (ai, bi) in a.iter().zip(b) {
                        borrow = sbb(borrow, *ai, *bi, bi);
                    }

                    borrow
                }

                fn sub2rev(a: &[BigDigit], b: &mut [BigDigit]) {
                    debug_assert!(b.len() >= a.len());

                    let len = Ord::min(a.len(), b.len());
                    let (a_lo, a_hi) = a.split_at(len);
                    let (b_lo, b_hi) = b.split_at_mut(len);

                    let borrow = __sub2rev(a_lo, b_lo);

                    assert!(a_hi.is_empty());

                    // note: we're _required_ to fail on underflow
                    assert!(
                        borrow == 0 && b_hi.iter().all(|x| *x == 0),
                        "Cannot subtract b from a because b is larger than a."
                    );
                }

                forward_val_val_binop!(impl Sub for BigUint, sub);
                forward_ref_ref_binop!(impl Sub for BigUint, sub);
                forward_val_assign!(impl SubAssign for BigUint, sub_assign);

                impl Sub<&BigUint> for BigUint {
                    type Output = BigUint;

                    fn sub(mut self, other: &BigUint) -> BigUint {
                        self -= other;
                        self
                    }
                }
                impl SubAssign<&BigUint> for BigUint {
                    fn sub_assign(&mut self, other: &BigUint) {
                        sub2(&mut self.data[..], &other.data[..]);
                        self.normalize();
                    }
                }

                impl Sub<BigUint> for &BigUint {
                    type Output = BigUint;

                    fn sub(self, mut other: BigUint) -> BigUint {
                        let other_len = other.data.len();
                        if other_len < self.data.len() {
                            let lo_borrow = __sub2rev(&self.data[..other_len], &mut other.data);
                            other.data.extend_from_slice(&self.data[other_len..]);
                            if lo_borrow != 0 {
                                sub2(&mut other.data[other_len..], &[1])
                            }
                        } else {
                            sub2rev(&self.data[..], &mut other.data[..]);
                        }
                        other.normalized()
                    }
                }

                promote_unsigned_scalars!(impl Sub for BigUint, sub);
                promote_unsigned_scalars_assign!(impl SubAssign for BigUint, sub_assign);
                forward_all_scalar_binop_to_val_val!(impl Sub<u32> for BigUint, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u64> for BigUint, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u128> for BigUint, sub);

                impl Sub<u32> for BigUint {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u32) -> BigUint {
                        self -= other;
                        self
                    }
                }

                impl SubAssign<u32> for BigUint {
                    fn sub_assign(&mut self, other: u32) {
                        sub2(&mut self.data[..], &[other as BigDigit]);
                        self.normalize();
                    }
                }

                impl Sub<BigUint> for u32 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.len() == 0 {
                                other.data.push(self);
                            } else {
                                sub2rev(&[self], &mut other.data[..]);
                            }
                            other.normalized()
                        }

                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.is_empty() {
                                other.data.push(self as BigDigit);
                            } else {
                                sub2rev(&[self as BigDigit], &mut other.data[..]);
                            }
                            other.normalized()
                        }
                    );
                }

                impl Sub<u64> for BigUint {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u64) -> BigUint {
                        self -= other;
                        self
                    }
                }

                impl SubAssign<u64> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn sub_assign(&mut self, other: u64) {
                            let (hi, lo) = big_digit::from_doublebigdigit(other);
                            sub2(&mut self.data[..], &[lo, hi]);
                            self.normalize();
                        }

                        #[inline]
                        fn sub_assign(&mut self, other: u64) {
                            sub2(&mut self.data[..], &[other as BigDigit]);
                            self.normalize();
                        }
                    );
                }

                impl Sub<BigUint> for u64 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 2 {
                                other.data.push(0);
                            }

                            let (hi, lo) = big_digit::from_doublebigdigit(self);
                            sub2rev(&[lo, hi], &mut other.data[..]);
                            other.normalized()
                        }

                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.is_empty() {
                                other.data.push(self);
                            } else {
                                sub2rev(&[self], &mut other.data[..]);
                            }
                            other.normalized()
                        }
                    );
                }

                impl Sub<u128> for BigUint {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u128) -> BigUint {
                        self -= other;
                        self
                    }
                }

                impl SubAssign<u128> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn sub_assign(&mut self, other: u128) {
                            let (a, b, c, d) = super::u32_from_u128(other);
                            sub2(&mut self.data[..], &[d, c, b, a]);
                            self.normalize();
                        }

                        #[inline]
                        fn sub_assign(&mut self, other: u128) {
                            let (hi, lo) = big_digit::from_doublebigdigit(other);
                            sub2(&mut self.data[..], &[lo, hi]);
                            self.normalize();
                        }
                    );
                }

                impl Sub<BigUint> for u128 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 4 {
                                other.data.push(0);
                            }

                            let (a, b, c, d) = super::u32_from_u128(self);
                            sub2rev(&[d, c, b, a], &mut other.data[..]);
                            other.normalized()
                        }

                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 2 {
                                other.data.push(0);
                            }

                            let (hi, lo) = big_digit::from_doublebigdigit(self);
                            sub2rev(&[lo, hi], &mut other.data[..]);
                            other.normalized()
                        }
                    );
                }

                impl CheckedSub for BigUint {
                    #[inline] fn checked_sub(&self, v: &BigUint) -> Option<BigUint> {
                        match self.cmp(v) {
                            Less => None,
                            Equal => Some(Self::ZERO),
                            Greater => Some(self.sub(v)),
                        }
                    }
                }
            }

            pub mod bits
            {
                use ::
                {
                    ops::{ BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign },
                    *,
                };

                use super::{BigUint, IntDigits};

                forward_val_val_binop!(impl BitAnd for BigUint, bitand);
                forward_ref_val_binop!(impl BitAnd for BigUint, bitand);

                // do not use forward_ref_ref_binop_commutative! for bitand so that we can
                // clone the smaller value rather than the larger, avoiding over-allocation
                impl BitAnd<&BigUint> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn bitand(self, other: &BigUint) -> BigUint {
                        // forward to val-ref, choosing the smaller to clone
                        if self.data.len() <= other.data.len() {
                            self.clone() & other
                        } else {
                            other.clone() & self
                        }
                    }
                }

                forward_val_assign!(impl BitAndAssign for BigUint, bitand_assign);

                impl BitAnd<&BigUint> for BigUint {
                    type Output = BigUint;

                    #[inline] fn bitand(mut self, other: &BigUint) -> BigUint {
                        self &= other;
                        self
                    }
                }
                impl BitAndAssign<&BigUint> for BigUint {
                    #[inline] fn bitand_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai &= bi;
                        }
                        self.data.truncate(other.data.len());
                        self.normalize();
                    }
                }

                forward_all_binop_to_val_ref_commutative!(impl BitOr for BigUint, bitor);
                forward_val_assign!(impl BitOrAssign for BigUint, bitor_assign);

                impl BitOr<&BigUint> for BigUint {
                    type Output = BigUint;

                    fn bitor(mut self, other: &BigUint) -> BigUint {
                        self |= other;
                        self
                    }
                }
                impl BitOrAssign<&BigUint> for BigUint {
                    #[inline] fn bitor_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai |= bi;
                        }
                        if other.data.len() > self.data.len() {
                            let extra = &other.data[self.data.len()..];
                            self.data.extend(extra.iter().cloned());
                        }
                    }
                }

                forward_all_binop_to_val_ref_commutative!(impl BitXor for BigUint, bitxor);
                forward_val_assign!(impl BitXorAssign for BigUint, bitxor_assign);

                impl BitXor<&BigUint> for BigUint {
                    type Output = BigUint;

                    fn bitxor(mut self, other: &BigUint) -> BigUint {
                        self ^= other;
                        self
                    }
                }
                impl BitXorAssign<&BigUint> for BigUint {
                    #[inline] fn bitxor_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai ^= bi;
                        }
                        if other.data.len() > self.data.len() {
                            let extra = &other.data[self.data.len()..];
                            self.data.extend(extra.iter().cloned());
                        }
                        self.normalize();
                    }
                }
            }

            pub mod convert
            {
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    convert::TryFrom,
                    num::
                    {
                        big::
                        {
                            big_digit::{self, BigDigit},
                            ParseBigIntError,
                            TryFromBigIntError,
                        },
                        integers::{ Integer, Roots },
                        traits::{ float::FloatCore, FromPrimitive, Num, One, PrimInt, ToPrimitive, Zero },
                    },
                    str::{ FromStr },
                    vec::{ Vec },
                    *,
                };
                
                use super::{biguint_from_vec, BigUint, ToBigUint};
                use super::addition::add2;
                use super::division::{div_rem_digit, FAST_DIV_WIDE};
                use super::multiplication::mac_with_carry;
                /// Find last set bit | fls(0) == 0, fls(u32::MAX) == 32
                fn fls<T: PrimInt>(v: T) -> u8 {
                    mem::size_of::<T>() as u8 * 8 - v.leading_zeros() as u8
                }

                fn ilog2<T: PrimInt>(v: T) -> u8 {
                    fls(v) - 1
                }

                impl FromStr for BigUint {
                    type Err = ParseBigIntError;

                    #[inline] fn from_str(s: &str) -> Result<BigUint, ParseBigIntError> {
                        BigUint::from_str_radix(s, 10)
                    }
                }

                // Convert from a power of two radix (bits == ilog2(radix)) where bits evenly divides
                // BigDigit::BITS
                pub(super) fn from_bitwise_digits_le(v: &[u8], bits: u8) -> BigUint {
                    debug_assert!(!v.is_empty() && bits <= 8 && big_digit::BITS % bits == 0);
                    debug_assert!(v.iter().all(|&c| BigDigit::from(c) < (1 << bits)));

                    let digits_per_big_digit = big_digit::BITS / bits;

                    let data = v
                        .chunks(digits_per_big_digit.into())
                        .map(|chunk| {
                            chunk
                                .iter()
                                .rev()
                                .fold(0, |acc, &c| (acc << bits) | BigDigit::from(c))
                        })
                        .collect();

                    biguint_from_vec(data)
                }

                // Convert from a power of two radix (bits == ilog2(radix)) where bits doesn't evenly divide
                // BigDigit::BITS
                fn from_inexact_bitwise_digits_le(v: &[u8], bits: u8) -> BigUint {
                    debug_assert!(!v.is_empty() && bits <= 8 && big_digit::BITS % bits != 0);
                    debug_assert!(v.iter().all(|&c| BigDigit::from(c) < (1 << bits)));

                    let total_bits = (v.len() as u64).saturating_mul(bits.into());
                    let big_digits = Integer::div_ceil(&total_bits, &big_digit::BITS.into())
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut data = Vec::with_capacity(big_digits);

                    let mut d = 0;
                    let mut dbits = 0; // number of bits we currently have in d

                    // walk v accumululating bits in d; whenever we accumulate big_digit::BITS in d, spit out a
                    // big_digit:
                    for &c in v {
                        d |= BigDigit::from(c) << dbits;
                        dbits += bits;

                        if dbits >= big_digit::BITS {
                            data.push(d);
                            dbits -= big_digit::BITS;
                            // if dbits was > big_digit::BITS, we dropped some of the bits in c (they couldn't fit
                            // in d) - grab the bits we lost here:
                            d = BigDigit::from(c) >> (bits - dbits);
                        }
                    }

                    if dbits > 0 {
                        debug_assert!(dbits < big_digit::BITS);
                        data.push(d as BigDigit);
                    }

                    biguint_from_vec(data)
                }

                // Read little-endian radix digits
                fn from_radix_digits_be(v: &[u8], radix: u32) -> BigUint {
                    debug_assert!(!v.is_empty() && !radix.is_power_of_two());
                    debug_assert!(v.iter().all(|&c| u32::from(c) < radix));

                    // Estimate how big the result will be, so we can pre-allocate it.
                    #[cfg(feature = "std")]
                    let big_digits = {
                        let radix_log2 = f64::from(radix).log2();
                        let bits = radix_log2 * v.len() as f64;
                        (bits / big_digit::BITS as f64).ceil()
                    };
                    #[cfg(not(feature = "std"))]
                    let big_digits = {
                        let radix_log2 = ilog2(radix.next_power_of_two()) as usize;
                        let bits = radix_log2 * v.len();
                        (bits / big_digit::BITS as usize) + 1
                    };

                    let mut data = Vec::with_capacity(big_digits.to_usize().unwrap_or(0));

                    let (base, power) = get_radix_base(radix);
                    let radix = radix as BigDigit;

                    let r = v.len() % power;
                    let i = if r == 0 { power } else { r };
                    let (head, tail) = v.split_at(i);

                    let first = head
                        .iter()
                        .fold(0, |acc, &d| acc * radix + BigDigit::from(d));
                    data.push(first);

                    debug_assert!(tail.len() % power == 0);
                    for chunk in tail.chunks(power) {
                        if data.last() != Some(&0) {
                            data.push(0);
                        }

                        let mut carry = 0;
                        for d in data.iter_mut() {
                            *d = mac_with_carry(0, *d, base, &mut carry);
                        }
                        debug_assert!(carry == 0);

                        let n = chunk
                            .iter()
                            .fold(0, |acc, &d| acc * radix + BigDigit::from(d));
                        add2(&mut data, &[n]);
                    }

                    biguint_from_vec(data)
                }

                pub(super) fn from_radix_be(buf: &[u8], radix: u32) -> Option<BigUint> {
                    assert!(
                        2 <= radix && radix <= 256,
                        "The radix must be within 2...256"
                    );

                    if buf.is_empty() {
                        return Some(BigUint::ZERO);
                    }

                    if radix != 256 && buf.iter().any(|&b| b >= radix as u8) {
                        return None;
                    }

                    let res = if radix.is_power_of_two() {
                        // Powers of two can use bitwise masks and shifting instead of multiplication
                        let bits = ilog2(radix);
                        let mut v = Vec::from(buf);
                        v.reverse();
                        if big_digit::BITS % bits == 0 {
                            from_bitwise_digits_le(&v, bits)
                        } else {
                            from_inexact_bitwise_digits_le(&v, bits)
                        }
                    } else {
                        from_radix_digits_be(buf, radix)
                    };

                    Some(res)
                }

                pub(super) fn from_radix_le(buf: &[u8], radix: u32) -> Option<BigUint> {
                    assert!(
                        2 <= radix && radix <= 256,
                        "The radix must be within 2...256"
                    );

                    if buf.is_empty() {
                        return Some(BigUint::ZERO);
                    }

                    if radix != 256 && buf.iter().any(|&b| b >= radix as u8) {
                        return None;
                    }

                    let res = if radix.is_power_of_two() {
                        // Powers of two can use bitwise masks and shifting instead of multiplication
                        let bits = ilog2(radix);
                        if big_digit::BITS % bits == 0 {
                            from_bitwise_digits_le(buf, bits)
                        } else {
                            from_inexact_bitwise_digits_le(buf, bits)
                        }
                    } else {
                        let mut v = Vec::from(buf);
                        v.reverse();
                        from_radix_digits_be(&v, radix)
                    };

                    Some(res)
                }

                impl Num for BigUint {
                    type FromStrRadixErr = ParseBigIntError;

                    /// Creates and initializes a `BigUint`.
                    fn from_str_radix(s: &str, radix: u32) -> Result<BigUint, ParseBigIntError> {
                        assert!(2 <= radix && radix <= 36, "The radix must be within 2...36");
                        let mut s = s;
                        if let Some(tail) = s.strip_prefix('+') {
                            if !tail.starts_with('+') {
                                s = tail
                            }
                        }

                        if s.is_empty() {
                            return Err(ParseBigIntError::empty());
                        }

                        if s.starts_with('_') {
                            // Must lead with a real digit!
                            return Err(ParseBigIntError::invalid());
                        }

                        // First normalize all characters to plain digit values
                        let mut v = Vec::with_capacity(s.len());
                        for b in s.bytes() {
                            let d = match b {
                                b'0'..=b'9' => b - b'0',
                                b'a'..=b'z' => b - b'a' + 10,
                                b'A'..=b'Z' => b - b'A' + 10,
                                b'_' => continue,
                                _ => u8::MAX,
                            };
                            if d < radix as u8 {
                                v.push(d);
                            } else {
                                return Err(ParseBigIntError::invalid());
                            }
                        }

                        let res = if radix.is_power_of_two() {
                            // Powers of two can use bitwise masks and shifting instead of multiplication
                            let bits = ilog2(radix);
                            v.reverse();
                            if big_digit::BITS % bits == 0 {
                                from_bitwise_digits_le(&v, bits)
                            } else {
                                from_inexact_bitwise_digits_le(&v, bits)
                            }
                        } else {
                            from_radix_digits_be(&v, radix)
                        };
                        Ok(res)
                    }
                }

                fn high_bits_to_u64(v: &BigUint) -> u64 {
                    match v.data.len() {
                        0 => 0,
                        1 => {
                            // XXX Conversion is useless if already 64-bit.
                            #[allow(clippy::useless_conversion)]
                            let v0 = u64::from(v.data[0]);
                            v0
                        }
                        _ => {
                            let mut bits = v.bits();
                            let mut ret = 0u64;
                            let mut ret_bits = 0;

                            for d in v.data.iter().rev() {
                                let digit_bits = (bits - 1) % u64::from(big_digit::BITS) + 1;
                                let bits_want = Ord::min(64 - ret_bits, digit_bits);

                                if bits_want != 0 {
                                    if bits_want != 64 {
                                        ret <<= bits_want;
                                    }
                                    // XXX Conversion is useless if already 64-bit.
                                    #[allow(clippy::useless_conversion)]
                                    let d0 = u64::from(*d) >> (digit_bits - bits_want);
                                    ret |= d0;
                                }

                                // Implement round-to-odd: If any lower bits are 1, set LSB to 1
                                // so that rounding again to floating point value using
                                // nearest-ties-to-even is correct.
                                //
                                // See: https://en.wikipedia.org/wiki/Rounding#Rounding_to_prepare_for_shorter_precision

                                if digit_bits - bits_want != 0 {
                                    // XXX Conversion is useless if already 64-bit.
                                    #[allow(clippy::useless_conversion)]
                                    let masked = u64::from(*d) << (64 - (digit_bits - bits_want) as u32);
                                    ret |= (masked != 0) as u64;
                                }

                                ret_bits += bits_want;
                                bits -= bits_want;
                            }

                            ret
                        }
                    }
                }

                impl ToPrimitive for BigUint {
                    #[inline] fn to_i64(&self) -> Option<i64> {
                        self.to_u64().as_ref().and_then(u64::to_i64)
                    }

                    #[inline] fn to_i128(&self) -> Option<i128> {
                        self.to_u128().as_ref().and_then(u128::to_i128)
                    }

                    #[allow(clippy::useless_conversion)]
                    #[inline] fn to_u64(&self) -> Option<u64> {
                        let mut ret: u64 = 0;
                        let mut bits = 0;

                        for i in self.data.iter() {
                            if bits >= 64 {
                                return None;
                            }

                            // XXX Conversion is useless if already 64-bit.
                            ret += u64::from(*i) << bits;
                            bits += big_digit::BITS;
                        }

                        Some(ret)
                    }

                    #[inline] fn to_u128(&self) -> Option<u128> {
                        let mut ret: u128 = 0;
                        let mut bits = 0;

                        for i in self.data.iter() {
                            if bits >= 128 {
                                return None;
                            }

                            ret |= u128::from(*i) << bits;
                            bits += big_digit::BITS;
                        }

                        Some(ret)
                    }

                    #[inline] fn to_f32(&self) -> Option<f32> {
                        let mantissa = high_bits_to_u64(self);
                        let exponent = self.bits() - u64::from(fls(mantissa));

                        if exponent > f32::MAX_EXP as u64 {
                            Some(f32::INFINITY)
                        } else {
                            Some((mantissa as f32) * 2.0f32.powi(exponent as i32))
                        }
                    }

                    #[inline] fn to_f64(&self) -> Option<f64> {
                        let mantissa = high_bits_to_u64(self);
                        let exponent = self.bits() - u64::from(fls(mantissa));

                        if exponent > f64::MAX_EXP as u64 {
                            Some(f64::INFINITY)
                        } else {
                            Some((mantissa as f64) * 2.0f64.powi(exponent as i32))
                        }
                    }
                }

                macro_rules! impl_try_from_biguint {
                    ($T:ty, $to_ty:path) => {
                        impl TryFrom<&BigUint> for $T {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: &BigUint) -> Result<$T, TryFromBigIntError<()>> {
                                $to_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }

                        impl TryFrom<BigUint> for $T {
                            type Error = TryFromBigIntError<BigUint>;

                            #[inline]
                            fn try_from(value: BigUint) -> Result<$T, TryFromBigIntError<BigUint>> {
                                <$T>::try_from(&value).map_err(|_| TryFromBigIntError::new(value))
                            }
                        }
                    };
                }

                impl_try_from_biguint!(u8, ToPrimitive::to_u8);
                impl_try_from_biguint!(u16, ToPrimitive::to_u16);
                impl_try_from_biguint!(u32, ToPrimitive::to_u32);
                impl_try_from_biguint!(u64, ToPrimitive::to_u64);
                impl_try_from_biguint!(usize, ToPrimitive::to_usize);
                impl_try_from_biguint!(u128, ToPrimitive::to_u128);

                impl_try_from_biguint!(i8, ToPrimitive::to_i8);
                impl_try_from_biguint!(i16, ToPrimitive::to_i16);
                impl_try_from_biguint!(i32, ToPrimitive::to_i32);
                impl_try_from_biguint!(i64, ToPrimitive::to_i64);
                impl_try_from_biguint!(isize, ToPrimitive::to_isize);
                impl_try_from_biguint!(i128, ToPrimitive::to_i128);

                impl FromPrimitive for BigUint {
                    #[inline] fn from_i64(n: i64) -> Option<BigUint> {
                        if n >= 0 {
                            Some(BigUint::from(n as u64))
                        } else {
                            None
                        }
                    }

                    #[inline] fn from_i128(n: i128) -> Option<BigUint> {
                        if n >= 0 {
                            Some(BigUint::from(n as u128))
                        } else {
                            None
                        }
                    }

                    #[inline] fn from_u64(n: u64) -> Option<BigUint> {
                        Some(BigUint::from(n))
                    }

                    #[inline] fn from_u128(n: u128) -> Option<BigUint> {
                        Some(BigUint::from(n))
                    }

                    #[inline] fn from_f64(mut n: f64) -> Option<BigUint> {
                        // handle NAN, INFINITY, NEG_INFINITY
                        if !n.is_finite() {
                            return None;
                        }

                        // match the rounding of casting from float to int
                        n = n.trunc();

                        // handle 0.x, -0.x
                        if n.is_zero() {
                            return Some(Self::ZERO);
                        }

                        let (mantissa, exponent, sign) = FloatCore::integer_decode(n);

                        if sign == -1 {
                            return None;
                        }

                        let mut ret = BigUint::from(mantissa);
                        match exponent.cmp(&0) {
                            Greater => ret <<= exponent as usize,
                            Equal => {}
                            Less => ret >>= (-exponent) as usize,
                        }
                        Some(ret)
                    }
                }

                impl From<u64> for BigUint {
                    #[inline] fn from(mut n: u64) -> Self {
                        let mut ret: BigUint = Self::ZERO;

                        while n != 0 {
                            ret.data.push(n as BigDigit);
                            // don't overflow if BITS is 64:
                            n = (n >> 1) >> (big_digit::BITS - 1);
                        }

                        ret
                    }
                }

                impl From<u128> for BigUint {
                    #[inline] fn from(mut n: u128) -> Self {
                        let mut ret: BigUint = Self::ZERO;

                        while n != 0 {
                            ret.data.push(n as BigDigit);
                            n >>= big_digit::BITS;
                        }

                        ret
                    }
                }

                macro_rules! impl_biguint_from_uint {
                    ($T:ty) => {
                        impl From<$T> for BigUint {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigUint::from(n as u64)
                            }
                        }
                    };
                }

                impl_biguint_from_uint!(u8);
                impl_biguint_from_uint!(u16);
                impl_biguint_from_uint!(u32);
                impl_biguint_from_uint!(usize);

                macro_rules! impl_biguint_try_from_int {
                    ($T:ty, $from_ty:path) => {
                        impl TryFrom<$T> for BigUint {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: $T) -> Result<BigUint, TryFromBigIntError<()>> {
                                $from_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }
                    };
                }

                impl_biguint_try_from_int!(i8, FromPrimitive::from_i8);
                impl_biguint_try_from_int!(i16, FromPrimitive::from_i16);
                impl_biguint_try_from_int!(i32, FromPrimitive::from_i32);
                impl_biguint_try_from_int!(i64, FromPrimitive::from_i64);
                impl_biguint_try_from_int!(isize, FromPrimitive::from_isize);
                impl_biguint_try_from_int!(i128, FromPrimitive::from_i128);

                impl ToBigUint for BigUint {
                    #[inline] fn to_biguint(&self) -> Option<BigUint> {
                        Some(self.clone())
                    }
                }

                macro_rules! impl_to_biguint {
                    ($T:ty, $from_ty:path) => {
                        impl ToBigUint for $T {
                            #[inline]
                            fn to_biguint(&self) -> Option<BigUint> {
                                $from_ty(*self)
                            }
                        }
                    };
                }

                impl_to_biguint!(isize, FromPrimitive::from_isize);
                impl_to_biguint!(i8, FromPrimitive::from_i8);
                impl_to_biguint!(i16, FromPrimitive::from_i16);
                impl_to_biguint!(i32, FromPrimitive::from_i32);
                impl_to_biguint!(i64, FromPrimitive::from_i64);
                impl_to_biguint!(i128, FromPrimitive::from_i128);

                impl_to_biguint!(usize, FromPrimitive::from_usize);
                impl_to_biguint!(u8, FromPrimitive::from_u8);
                impl_to_biguint!(u16, FromPrimitive::from_u16);
                impl_to_biguint!(u32, FromPrimitive::from_u32);
                impl_to_biguint!(u64, FromPrimitive::from_u64);
                impl_to_biguint!(u128, FromPrimitive::from_u128);

                impl_to_biguint!(f32, FromPrimitive::from_f32);
                impl_to_biguint!(f64, FromPrimitive::from_f64);

                impl From<bool> for BigUint {
                    fn from(x: bool) -> Self {
                        if x {
                            One::one()
                        } else {
                            Self::ZERO
                        }
                    }
                }

                // Extract bitwise digits that evenly divide BigDigit
                pub(super) fn to_bitwise_digits_le(u: &BigUint, bits: u8) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && bits <= 8 && big_digit::BITS % bits == 0);

                    let last_i = u.data.len() - 1;
                    let mask: BigDigit = (1 << bits) - 1;
                    let digits_per_big_digit = big_digit::BITS / bits;
                    let digits = Integer::div_ceil(&u.bits(), &u64::from(bits))
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut res = Vec::with_capacity(digits);

                    for mut r in u.data[..last_i].iter().cloned() {
                        for _ in 0..digits_per_big_digit {
                            res.push((r & mask) as u8);
                            r >>= bits;
                        }
                    }

                    let mut r = u.data[last_i];
                    while r != 0 {
                        res.push((r & mask) as u8);
                        r >>= bits;
                    }

                    res
                }

                // Extract bitwise digits that don't evenly divide BigDigit
                fn to_inexact_bitwise_digits_le(u: &BigUint, bits: u8) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && bits <= 8 && big_digit::BITS % bits != 0);

                    let mask: BigDigit = (1 << bits) - 1;
                    let digits = Integer::div_ceil(&u.bits(), &u64::from(bits))
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut res = Vec::with_capacity(digits);

                    let mut r = 0;
                    let mut rbits = 0;

                    for c in &u.data {
                        r |= *c << rbits;
                        rbits += big_digit::BITS;

                        while rbits >= bits {
                            res.push((r & mask) as u8);
                            r >>= bits;

                            // r had more bits than it could fit - grab the bits we lost
                            if rbits > big_digit::BITS {
                                r = *c >> (big_digit::BITS - (rbits - bits));
                            }

                            rbits -= bits;
                        }
                    }

                    if rbits != 0 {
                        res.push(r as u8);
                    }

                    while let Some(&0) = res.last() {
                        res.pop();
                    }

                    res
                }

                // Extract little-endian radix digits
                #[inline(always)] // forced inline to get const-prop for radix=10
                pub(super) fn to_radix_digits_le(u: &BigUint, radix: u32) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && !radix.is_power_of_two());

                    #[cfg(feature = "std")]
                    let radix_digits = {
                        let radix_log2 = f64::from(radix).log2();
                        ((u.bits() as f64) / radix_log2).ceil()
                    };
                    #[cfg(not(feature = "std"))]
                    let radix_digits = {
                        let radix_log2 = ilog2(radix) as usize;
                        ((u.bits() as usize) / radix_log2) + 1
                    };

                    // Estimate how big the result will be, so we can pre-allocate it.
                    let mut res = Vec::with_capacity(radix_digits.to_usize().unwrap_or(0));

                    let mut digits = u.clone();

                    // X86 DIV can quickly divide by a full digit, otherwise we choose a divisor
                    // that's suitable for `div_half` to avoid slow `DoubleBigDigit` division.
                    let (base, power) = if FAST_DIV_WIDE {
                        get_radix_base(radix)
                    } else {
                        get_half_radix_base(radix)
                    };
                    let radix = radix as BigDigit;

                    // For very large numbers, the O(n²) loop of repeated `div_rem_digit` dominates the
                    // performance. We can mitigate this by dividing into chunks of a larger base first.
                    // The threshold for this was chosen by anecdotal performance measurements to
                    // approximate where this starts to make a noticeable difference.
                    if digits.data.len() >= 64 {
                        let mut big_base = BigUint::from(base);
                        let mut big_power = 1usize;

                        // Choose a target base length near √n.
                        let target_len = digits.data.len().sqrt();
                        while big_base.data.len() < target_len {
                            big_base = &big_base * &big_base;
                            big_power *= 2;
                        }

                        // This outer loop will run approximately √n times.
                        while digits > big_base {
                            // This is still the dominating factor, with n digits divided by √n digits.
                            let (q, mut big_r) = digits.div_rem(&big_base);
                            digits = q;

                            // This inner loop now has O(√n²)=O(n) behavior altogether.
                            for _ in 0..big_power {
                                let (q, mut r) = div_rem_digit(big_r, base);
                                big_r = q;
                                for _ in 0..power {
                                    res.push((r % radix) as u8);
                                    r /= radix;
                                }
                            }
                        }
                    }

                    while digits.data.len() > 1 {
                        let (q, mut r) = div_rem_digit(digits, base);
                        for _ in 0..power {
                            res.push((r % radix) as u8);
                            r /= radix;
                        }
                        digits = q;
                    }

                    let mut r = digits.data[0];
                    while r != 0 {
                        res.push((r % radix) as u8);
                        r /= radix;
                    }

                    res
                }

                pub(super) fn to_radix_le(u: &BigUint, radix: u32) -> Vec<u8> {
                    if u.is_zero() {
                        vec![0]
                    } else if radix.is_power_of_two() {
                        // Powers of two can use bitwise masks and shifting instead of division
                        let bits = ilog2(radix);
                        if big_digit::BITS % bits == 0 {
                            to_bitwise_digits_le(u, bits)
                        } else {
                            to_inexact_bitwise_digits_le(u, bits)
                        }
                    } else if radix == 10 {
                        // 10 is so common that it's worth separating out for const-propagation.
                        // Optimizers can often turn constant division into a faster multiplication.
                        to_radix_digits_le(u, 10)
                    } else {
                        to_radix_digits_le(u, radix)
                    }
                }

                pub fn to_str_radix_reversed(u: &BigUint, radix: u32) -> Vec<u8> {
                    assert!(2 <= radix && radix <= 36, "The radix must be within 2...36");

                    if u.is_zero() {
                        return vec![b'0'];
                    }

                    let mut res = to_radix_le(u, radix);

                    // Now convert everything to ASCII digits.
                    for r in &mut res {
                        debug_assert!(u32::from(*r) < radix);
                        if *r < 10 {
                            *r += b'0';
                        } else {
                            *r += b'a' - 10;
                        }
                    }
                    res
                }

                /// Returns the greatest power of the radix for the `BigDigit` bit size
                #[inline]
                fn get_radix_base(radix: u32) -> (BigDigit, usize) {
                    static BASES: [(BigDigit, usize); 257] = generate_radix_bases(big_digit::MAX);
                    debug_assert!(!radix.is_power_of_two());
                    debug_assert!((3..256).contains(&radix));
                    BASES[radix as usize]
                }

                /// Returns the greatest power of the radix for half the `BigDigit` bit size
                #[inline]
                fn get_half_radix_base(radix: u32) -> (BigDigit, usize) {
                    static BASES: [(BigDigit, usize); 257] = generate_radix_bases(big_digit::HALF);
                    debug_assert!(!radix.is_power_of_two());
                    debug_assert!((3..256).contains(&radix));
                    BASES[radix as usize]
                }

                /// Generate tables of the greatest power of each radix that is less that the given maximum. These
                /// are returned from `get_radix_base` to batch the multiplication/division of radix conversions on
                /// full `BigUint` values, operating on primitive integers as much as possible.
                ///
                /// e.g. BASES_16[3] = (59049, 10) // 3¹⁰ fits in u16, but 3¹¹ is too big
                ///      BASES_32[3] = (3486784401, 20)
                ///      BASES_64[3] = (12157665459056928801, 40)
                ///
                /// Powers of two are not included, just zeroed, as they're implemented with shifts.
                const fn generate_radix_bases(max: BigDigit) -> [(BigDigit, usize); 257] {
                    let mut bases = [(0, 0); 257];

                    let mut radix: BigDigit = 3;
                    while radix < 256 {
                        if !radix.is_power_of_two() {
                            let mut power = 1;
                            let mut base = radix;

                            while let Some(b) = base.checked_mul(radix) {
                                if b > max {
                                    break;
                                }
                                base = b;
                                power += 1;
                            }
                            bases[radix as usize] = (base, power)
                        }
                        radix += 1;
                    }

                    bases
                }
            }

            pub mod iter
            {
                use ::
                {
                    iter::{ FusedIterator },
                    *,
                };
                
                /// An iterator of `u32` digits representation of a `BigUint` or `BigInt`, ordered least significant digit first.
                pub struct U32Digits<'a>
                {
                    data: &'a [u64],
                    next_is_lo: bool,
                    last_hi_is_zero: bool,
                }
                
                
                impl<'a> U32Digits<'a> {
                    #[inline]
                    pub(super) fn new(data: &'a [u64]) -> Self {
                        let last_hi_is_zero = data
                            .last()
                            .map(|&last| {
                                let last_hi = (last >> 32) as u32;
                                last_hi == 0
                            })
                            .unwrap_or(false);
                        U32Digits {
                            data,
                            next_is_lo: true,
                            last_hi_is_zero,
                        }
                    }
                }

                impl Iterator for U32Digits<'_> {
                    type Item = u32;
                    #[inline] fn next(&mut self) -> Option<u32> {
                        match self.data.split_first() {
                            Some((&first, data)) => {
                                let next_is_lo = self.next_is_lo;
                                self.next_is_lo = !next_is_lo;
                                if next_is_lo {
                                    Some(first as u32)
                                } else {
                                    self.data = data;
                                    if data.is_empty() && self.last_hi_is_zero {
                                        self.last_hi_is_zero = false;
                                        None
                                    } else {
                                        Some((first >> 32) as u32)
                                    }
                                }
                            }
                            None => None,
                        }
                    }

                    #[inline] fn size_hint(&self) -> (usize, Option<usize>) {
                        let len = self.len();
                        (len, Some(len))
                    }

                    #[inline] fn last(self) -> Option<u32> {
                        self.data.last().map(|&last| {
                            if self.last_hi_is_zero {
                                last as u32
                            } else {
                                (last >> 32) as u32
                            }
                        })
                    }

                    #[inline] fn count(self) -> usize {
                        self.len()
                    }
                }

                impl DoubleEndedIterator for U32Digits<'_> {
                    fn next_back(&mut self) -> Option<Self::Item> {
                        match self.data.split_last() {
                            Some((&last, data)) => {
                                let last_is_lo = self.last_hi_is_zero;
                                self.last_hi_is_zero = !last_is_lo;
                                if last_is_lo {
                                    self.data = data;
                                    if data.is_empty() && !self.next_is_lo {
                                        self.next_is_lo = true;
                                        None
                                    } else {
                                        Some(last as u32)
                                    }
                                } else {
                                    Some((last >> 32) as u32)
                                }
                            }
                            None => None,
                        }
                    }
                }

                impl ExactSizeIterator for U32Digits<'_> {
                    #[inline] fn len(&self) -> usize {
                        self.data.len() * 2
                            - usize::from(self.last_hi_is_zero)
                            - usize::from(!self.next_is_lo)
                    }
                }

                impl FusedIterator for U32Digits<'_> {}
                
                /// An iterator of `u64` digits representation of 
                /// a `BigUint` or `BigInt`, ordered least significant digit first.
                pub struct U64Digits<'a> {
                    it: ::slice::Iter<'a, u64>,
                }

                
                impl<'a> U64Digits<'a> {
                    #[inline]
                    pub(super) fn new(data: &'a [u64]) -> Self {
                        Self { it: data.iter() }
                    }
                }

                impl Iterator for U64Digits<'_> {
                    type Item = u64;
                    #[inline] fn next(&mut self) -> Option<u64> {
                        self.it.next().cloned()
                    }

                    #[inline] fn size_hint(&self) -> (usize, Option<usize>) {
                        self.it.size_hint()
                    }

                    #[inline] fn nth(&mut self, n: usize) -> Option<u64> {
                        self.it.nth(n).cloned()
                    }

                    #[inline] fn last(self) -> Option<u64> {
                        self.it.last().cloned()
                    }

                    #[inline] fn count(self) -> usize {
                        self.it.count()
                    }
                }

                impl DoubleEndedIterator for U64Digits<'_> {
                    fn next_back(&mut self) -> Option<Self::Item> {
                        self.it.next_back().cloned()
                    }
                }
                    

                impl ExactSizeIterator for U64Digits<'_> {
                    #[inline] fn len(&self) -> usize {
                        self.it.len()
                    }
                }

                impl FusedIterator for U64Digits<'_> {}
            }

            pub mod monty
            {
                use ::
                {
                    vec::{ Vec },
                    ops::{ Shl },
                    num::
                    {
                        big::{ biguint::BigUint, big_digit::{ self, BigDigit, DoubleBigDigit } },
                        traits::{ One },
                    }, 
                    *,
                };

                struct MontyReducer {
                    n0inv: BigDigit,
                }

                // k0 = -m**-1 mod 2**BITS. Algorithm from: Dumas, J.G. "On Newton–Raphson
                // Iteration for Multiplicative Inverses Modulo Prime Powers".
                fn inv_mod_alt(b: BigDigit) -> BigDigit {
                    assert_ne!(b & 1, 0);

                    let mut k0 = BigDigit::wrapping_sub(2, b);
                    let mut t = b - 1;
                    let mut i = 1;
                    while i < big_digit::BITS {
                        t = t.wrapping_mul(t);
                        k0 = k0.wrapping_mul(t + 1);

                        i <<= 1;
                    }
                    debug_assert_eq!(k0.wrapping_mul(b), 1);
                    k0.wrapping_neg()
                }

                impl MontyReducer {
                    fn new(n: &BigUint) -> Self {
                        let n0inv = inv_mod_alt(n.data[0]);
                        MontyReducer { n0inv }
                    }
                }

                /// Computes z mod m = x * y * 2 ** (-n*_W) mod m, assuming k = -1/m mod 2**_W
                fn montgomery(x: &BigUint, y: &BigUint, m: &BigUint, k: BigDigit, n: usize) -> BigUint {
                    // This code assumes x, y, m are all the same length, n.
                    // (required by addMulVVW and the for loop).
                    // It also assumes that x, y are already reduced mod m,
                    // or else the result will not be properly reduced.
                    assert!(
                        x.data.len() == n && y.data.len() == n && m.data.len() == n,
                        "{:?} {:?} {:?} {}",
                        x,
                        y,
                        m,
                        n
                    );

                    let mut z = BigUint::ZERO;
                    z.data.resize(n * 2, 0);

                    let mut c: BigDigit = 0;
                    for i in 0..n {
                        let c2 = add_mul_vvw(&mut z.data[i..n + i], &x.data, y.data[i]);
                        let t = z.data[i].wrapping_mul(k);
                        let c3 = add_mul_vvw(&mut z.data[i..n + i], &m.data, t);
                        let cx = c.wrapping_add(c2);
                        let cy = cx.wrapping_add(c3);
                        z.data[n + i] = cy;
                        if cx < c2 || cy < c3 {
                            c = 1;
                        } else {
                            c = 0;
                        }
                    }

                    if c == 0 {
                        z.data = z.data[n..].to_vec();
                    } else {
                        {
                            let (first, second) = z.data.split_at_mut(n);
                            sub_vv(first, second, &m.data);
                        }
                        z.data = z.data[..n].to_vec();
                    }

                    z
                }

                #[inline(always)]
                fn add_mul_vvw(z: &mut [BigDigit], x: &[BigDigit], y: BigDigit) -> BigDigit {
                    let mut c = 0;
                    for (zi, xi) in z.iter_mut().zip(x.iter()) {
                        let (z1, z0) = mul_add_www(*xi, y, *zi);
                        let (c_, zi_) = add_ww(z0, c, 0);
                        *zi = zi_;
                        c = c_ + z1;
                    }

                    c
                }

                /// The resulting carry c is either 0 or 1.
                #[inline(always)]
                fn sub_vv(z: &mut [BigDigit], x: &[BigDigit], y: &[BigDigit]) -> BigDigit {
                    let mut c = 0;
                    for (i, (xi, yi)) in x.iter().zip(y.iter()).enumerate().take(z.len()) {
                        let zi = xi.wrapping_sub(*yi).wrapping_sub(c);
                        z[i] = zi;
                        // see "Hacker's Delight", section 2-12 (overflow detection)
                        c = ((yi & !xi) | ((yi | !xi) & zi)) >> (big_digit::BITS - 1)
                    }

                    c
                }

                /// z1<<_W + z0 = x+y+c, with c == 0 or 1
                #[inline(always)]
                fn add_ww(x: BigDigit, y: BigDigit, c: BigDigit) -> (BigDigit, BigDigit) {
                    let yc = y.wrapping_add(c);
                    let z0 = x.wrapping_add(yc);
                    let z1 = if z0 < x || yc < y { 1 } else { 0 };

                    (z1, z0)
                }

                /// z1 << _W + z0 = x * y + c
                #[inline(always)]
                fn mul_add_www(x: BigDigit, y: BigDigit, c: BigDigit) -> (BigDigit, BigDigit) {
                    let z = x as DoubleBigDigit * y as DoubleBigDigit + c as DoubleBigDigit;
                    ((z >> big_digit::BITS) as BigDigit, z as BigDigit)
                }

                /// Calculates x ** y mod m using a fixed, 4-bit window.
                
                pub(super) fn monty_modpow(x: &BigUint, y: &BigUint, m: &BigUint) -> BigUint {
                    assert!(m.data[0] & 1 == 1);
                    let mr = MontyReducer::new(m);
                    let num_words = m.data.len();

                    let mut x = x.clone();

                    // We want the lengths of x and m to be equal.
                    // It is OK if x >= m as long as len(x) == len(m).
                    if x.data.len() > num_words {
                        x %= m;
                        // Note: now len(x) <= numWords, not guaranteed ==.
                    }
                    if x.data.len() < num_words {
                        x.data.resize(num_words, 0);
                    }

                    // rr = 2**(2*_W*len(m)) mod m
                    let mut rr = BigUint::one();
                    rr = (rr.shl(2 * num_words as u64 * u64::from(big_digit::BITS))) % m;
                    if rr.data.len() < num_words {
                        rr.data.resize(num_words, 0);
                    }
                    // one = 1, with equal length to that of m
                    let mut one = BigUint::one();
                    one.data.resize(num_words, 0);

                    let n = 4;
                    // powers[i] contains x^i
                    let mut powers = Vec::with_capacity(1 << n);
                    powers.push(montgomery(&one, &rr, m, mr.n0inv, num_words));
                    powers.push(montgomery(&x, &rr, m, mr.n0inv, num_words));
                    for i in 2..1 << n {
                        let r = montgomery(&powers[i - 1], &powers[1], m, mr.n0inv, num_words);
                        powers.push(r);
                    }

                    // initialize z = 1 (Montgomery 1)
                    let mut z = powers[0].clone();
                    z.data.resize(num_words, 0);
                    let mut zz = BigUint::ZERO;
                    zz.data.resize(num_words, 0);

                    // same windowed exponent, but with Montgomery multiplications
                    for i in (0..y.data.len()).rev() {
                        let mut yi = y.data[i];
                        let mut j = 0;
                        while j < big_digit::BITS {
                            if i != y.data.len() - 1 || j != 0 {
                                zz = montgomery(&z, &z, m, mr.n0inv, num_words);
                                z = montgomery(&zz, &zz, m, mr.n0inv, num_words);
                                zz = montgomery(&z, &z, m, mr.n0inv, num_words);
                                z = montgomery(&zz, &zz, m, mr.n0inv, num_words);
                            }
                            zz = montgomery(
                                &z,
                                &powers[(yi >> (big_digit::BITS - n)) as usize],
                                m,
                                mr.n0inv,
                                num_words,
                            );
                            mem::swap(&mut z, &mut zz);
                            yi <<= n;
                            j += n;
                        }
                    }

                    // convert to regular number
                    zz = montgomery(&z, &one, m, mr.n0inv, num_words);

                    zz.normalize();
                    // One last reduction, just in case.
                    // See golang.org/issue/13907.
                    if zz >= *m {
                        // Common case is m has high bit set; in that case,
                        // since zz is the same length as m, there can be just
                        // one multiple of m to remove. Just subtract.
                        // We think that the subtract should be sufficient in general,
                        // so do that unconditionally, but double-check,
                        // in case our beliefs are wrong.
                        // The div is not expected to be reached.
                        zz -= m;
                        if zz >= *m {
                            zz %= m;
                        }
                    }

                    zz.normalize();
                    zz
                }
            }

            pub mod power
            {
                use ::
                {
                    num::
                    {
                        big::{ big_digit::{self, BigDigit} },
                        integers::{ Integer },
                        traits::{ One, Pow, ToPrimitive, Zero },
                    },
                    *,
                };

                use super::monty::monty_modpow;
                use super::BigUint;
                
                impl Pow<&BigUint> for BigUint 
                {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: &BigUint) -> BigUint {
                        if self.is_one() || exp.is_zero() {
                            BigUint::one()
                        } else if self.is_zero() {
                            Self::ZERO
                        } else if let Some(exp) = exp.to_u64() {
                            self.pow(exp)
                        } else if let Some(exp) = exp.to_u128() {
                            self.pow(exp)
                        } else {
                            // At this point, `self >= 2` and `exp >= 2¹²⁸`. The smallest possible result given
                            // `2.pow(2¹²⁸)` would require far more memory than 64-bit targets can address!
                            panic!("memory overflow")
                        }
                    }
                }

                impl Pow<BigUint> for BigUint {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: BigUint) -> BigUint {
                        Pow::pow(self, &exp)
                    }
                }

                impl Pow<&BigUint> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: &BigUint) -> BigUint {
                        if self.is_one() || exp.is_zero() {
                            BigUint::one()
                        } else if self.is_zero() {
                            BigUint::ZERO
                        } else {
                            self.clone().pow(exp)
                        }
                    }
                }

                impl Pow<BigUint> for &BigUint {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: BigUint) -> BigUint {
                        Pow::pow(self, &exp)
                    }
                }

                macro_rules! pow_impl {
                    ($T:ty) => {
                        impl Pow<$T> for BigUint {
                            type Output = BigUint;

                            fn pow(self, mut exp: $T) -> BigUint {
                                if exp == 0 {
                                    return BigUint::one();
                                }
                                let mut base = self;

                                while exp & 1 == 0 {
                                    base = &base * &base;
                                    exp >>= 1;
                                }

                                if exp == 1 {
                                    return base;
                                }

                                let mut acc = base.clone();
                                while exp > 1 {
                                    exp >>= 1;
                                    base = &base * &base;
                                    if exp & 1 == 1 {
                                        acc *= &base;
                                    }
                                }
                                acc
                            }
                        }

                        impl Pow<&$T> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: &$T) -> BigUint {
                                Pow::pow(self, *exp)
                            }
                        }

                        impl Pow<$T> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: $T) -> BigUint {
                                if exp == 0 {
                                    return BigUint::one();
                                }
                                Pow::pow(self.clone(), exp)
                            }
                        }

                        impl Pow<&$T> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: &$T) -> BigUint {
                                Pow::pow(self, *exp)
                            }
                        }
                    };
                }

                pow_impl!(u8);
                pow_impl!(u16);
                pow_impl!(u32);
                pow_impl!(u64);
                pow_impl!(usize);
                pow_impl!(u128);

                pub(super) fn modpow(x: &BigUint, exponent: &BigUint, modulus: &BigUint) -> BigUint {
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    if modulus.is_odd() {
                        // For an odd modulus, we can use Montgomery multiplication in base 2^32.
                        monty_modpow(x, exponent, modulus)
                    } else {
                        // Otherwise do basically the same as `num::pow`, but with a modulus.
                        plain_modpow(x, &exponent.data, modulus)
                    }
                }

                fn plain_modpow(base: &BigUint, exp_data: &[BigDigit], modulus: &BigUint) -> BigUint {
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    let i = match exp_data.iter().position(|&r| r != 0) {
                        None => return BigUint::one(),
                        Some(i) => i,
                    };

                    let mut base = base % modulus;
                    for _ in 0..i {
                        for _ in 0..big_digit::BITS {
                            base = &base * &base % modulus;
                        }
                    }

                    let mut r = exp_data[i];
                    let mut b = 0u8;
                    while r.is_even() {
                        base = &base * &base % modulus;
                        r >>= 1;
                        b += 1;
                    }

                    let mut exp_iter = exp_data[i + 1..].iter();
                    if exp_iter.len() == 0 && r.is_one() {
                        return base;
                    }

                    let mut acc = base.clone();
                    r >>= 1;
                    b += 1;

                    {
                        let mut unit = |exp_is_odd| {
                            base = &base * &base % modulus;
                            if exp_is_odd {
                                acc *= &base;
                                acc %= modulus;
                            }
                        };

                        if let Some(&last) = exp_iter.next_back() {
                            // consume exp_data[i]
                            for _ in b..big_digit::BITS {
                                unit(r.is_odd());
                                r >>= 1;
                            }

                            // consume all other digits before the last
                            for &r in exp_iter {
                                let mut r = r;
                                for _ in 0..big_digit::BITS {
                                    unit(r.is_odd());
                                    r >>= 1;
                                }
                            }
                            r = last;
                        }

                        debug_assert_ne!(r, 0);
                        while !r.is_zero() {
                            unit(r.is_odd());
                            r >>= 1;
                        }
                    }
                    acc
                }
            }

            pub mod shift
            {
                use ::
                {
                    borrow::{ Cow },
                    num::
                    {
                        big::{ big_digit::{ self }, },
                        traits::{ PrimInt, Zero },
                    },
                    ops::{ Shl, ShlAssign, Shr, ShrAssign },
                    vec::{ Vec },
                    *,
                };

                use super::{biguint_from_vec, BigUint};
                
                #[inline]
                fn biguint_shl<T: PrimInt>(n: Cow<'_, BigUint>, shift: T) -> BigUint {
                    if shift < T::zero() {
                        panic!("attempt to shift left with negative");
                    }
                    if n.is_zero() {
                        return n.into_owned();
                    }
                    let bits = T::from(big_digit::BITS).unwrap();
                    let digits = (shift / bits).to_usize().expect("capacity overflow");
                    let shift = (shift % bits).to_u8().unwrap();
                    biguint_shl2(n, digits, shift)
                }

                fn biguint_shl2(n: Cow<'_, BigUint>, digits: usize, shift: u8) -> BigUint {
                    let mut data = match digits {
                        0 => n.into_owned().data,
                        _ => {
                            let len = digits.saturating_add(n.data.len() + 1);
                            let mut data = Vec::with_capacity(len);
                            data.resize(digits, 0);
                            data.extend(n.data.iter());
                            data
                        }
                    };

                    if shift > 0 {
                        let mut carry = 0;
                        let carry_shift = big_digit::BITS - shift;
                        for elem in data[digits..].iter_mut() {
                            let new_carry = *elem >> carry_shift;
                            *elem = (*elem << shift) | carry;
                            carry = new_carry;
                        }
                        if carry != 0 {
                            data.push(carry);
                        }
                    }

                    biguint_from_vec(data)
                }

                #[inline]
                fn biguint_shr<T: PrimInt>(n: Cow<'_, BigUint>, shift: T) -> BigUint {
                    if shift < T::zero() {
                        panic!("attempt to shift right with negative");
                    }
                    if n.is_zero() {
                        return n.into_owned();
                    }
                    let bits = T::from(big_digit::BITS).unwrap();
                    let digits = (shift / bits).to_usize().unwrap_or(usize::MAX);
                    let shift = (shift % bits).to_u8().unwrap();
                    biguint_shr2(n, digits, shift)
                }

                fn biguint_shr2(n: Cow<'_, BigUint>, digits: usize, shift: u8) -> BigUint {
                    if digits >= n.data.len() {
                        let mut n = n.into_owned();
                        n.set_zero();
                        return n;
                    }
                    let mut data = match n {
                        Cow::Borrowed(n) => n.data[digits..].to_vec(),
                        Cow::Owned(mut n) => {
                            n.data.drain(..digits);
                            n.data
                        }
                    };

                    if shift > 0 {
                        let mut borrow = 0;
                        let borrow_shift = big_digit::BITS - shift;
                        for elem in data.iter_mut().rev() {
                            let new_borrow = *elem << borrow_shift;
                            *elem = (*elem >> shift) | borrow;
                            borrow = new_borrow;
                        }
                    }

                    biguint_from_vec(data)
                }

                macro_rules! impl_shift {
                    (@ref $Shx:ident :: $shx:ident, $ShxAssign:ident :: $shx_assign:ident, $rhs:ty) => {
                        impl $Shx<&$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigUint {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $Shx<&$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigUint {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $ShxAssign<&$rhs> for BigUint {
                            #[inline]
                            fn $shx_assign(&mut self, rhs: &$rhs) {
                                $ShxAssign::$shx_assign(self, *rhs);
                            }
                        }
                    };
                    ($($rhs:ty),+) => {$(
                        impl Shl<$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigUint {
                                biguint_shl(Cow::Owned(self), rhs)
                            }
                        }
                        impl Shl<$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigUint {
                                biguint_shl(Cow::Borrowed(self), rhs)
                            }
                        }
                        impl ShlAssign<$rhs> for BigUint {
                            #[inline]
                            fn shl_assign(&mut self, rhs: $rhs) {
                                let n = mem::replace(self, Self::ZERO);
                                *self = n << rhs;
                            }
                        }
                        impl_shift! { @ref Shl::shl, ShlAssign::shl_assign, $rhs }

                        impl Shr<$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigUint {
                                biguint_shr(Cow::Owned(self), rhs)
                            }
                        }
                        impl Shr<$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigUint {
                                biguint_shr(Cow::Borrowed(self), rhs)
                            }
                        }
                        impl ShrAssign<$rhs> for BigUint {
                            #[inline]
                            fn shr_assign(&mut self, rhs: $rhs) {
                                let n = mem::replace(self, Self::ZERO);
                                *self = n >> rhs;
                            }
                        }
                        impl_shift! { @ref Shr::shr, ShrAssign::shr_assign, $rhs }
                    )*};
                }

                impl_shift! { u8, u16, u32, u64, u128, usize }
                impl_shift! { i8, i16, i32, i64, i128, isize }
            }

            pub use self::convert::to_str_radix_reversed;
            pub use self::iter::{U32Digits, U64Digits};

            /// A big unsigned integer type.
            pub struct BigUint {
                data: Vec<BigDigit>,
            }

            // Note: derived `Clone` doesn't specialize `clone_from`,
            // but we want to keep the allocation in `data`.
            impl Clone for BigUint {
                #[inline]
                fn clone(&self) -> Self {
                    BigUint {
                        data: self.data.clone(),
                    }
                }

                #[inline]
                fn clone_from(&mut self, other: &Self) {
                    self.data.clone_from(&other.data);
                }
            }

            impl hash::Hash for BigUint {
                #[inline]
                fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!(self.data.last() != Some(&0));
                    self.data.hash(state);
                }
            }

            impl PartialEq for BigUint {
                #[inline]
                fn eq(&self, other: &BigUint) -> bool {
                    debug_assert!(self.data.last() != Some(&0));
                    debug_assert!(other.data.last() != Some(&0));
                    self.data == other.data
                }
            }
            impl Eq for BigUint {}

            impl PartialOrd for BigUint {
                #[inline]
                fn partial_cmp(&self, other: &BigUint) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for BigUint {
                #[inline]
                fn cmp(&self, other: &BigUint) -> Ordering {
                    cmp_slice(&self.data[..], &other.data[..])
                }
            }

            #[inline] fn cmp_slice(a: &[BigDigit], b: &[BigDigit]) -> Ordering {
                debug_assert!(a.last() != Some(&0));
                debug_assert!(b.last() != Some(&0));

                match Ord::cmp(&a.len(), &b.len()) {
                    Ordering::Equal => Iterator::cmp(a.iter().rev(), b.iter().rev()),
                    other => other,
                }
            }

            impl Default for BigUint {
                #[inline]
                fn default() -> BigUint {
                    Self::ZERO
                }
            }

            impl fmt::Debug for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Display::fmt(self, f)
                }
            }

            impl fmt::Display for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "", &self.to_str_radix(10))
                }
            }

            impl fmt::LowerHex for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0x", &self.to_str_radix(16))
                }
            }

            impl fmt::UpperHex for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let mut s = self.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(true, "0x", &s)
                }
            }

            impl fmt::Binary for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0b", &self.to_str_radix(2))
                }
            }

            impl fmt::Octal for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0o", &self.to_str_radix(8))
                }
            }

            impl Zero for BigUint {
                #[inline]
                fn zero() -> BigUint {
                    Self::ZERO
                }

                #[inline]
                fn set_zero(&mut self) {
                    self.data.clear();
                }

                #[inline]
                fn is_zero(&self) -> bool {
                    self.data.is_empty()
                }
            }

            impl ConstZero for BigUint {
                // forward to the inherent const
                const ZERO: Self = Self::ZERO; // BigUint { data: Vec::new() };
            }

            impl One for BigUint {
                #[inline]
                fn one() -> BigUint {
                    BigUint { data: vec![1] }
                }

                #[inline]
                fn set_one(&mut self) {
                    self.data.clear();
                    self.data.push(1);
                }

                #[inline]
                fn is_one(&self) -> bool {
                    self.data[..] == [1]
                }
            }

            impl Unsigned for BigUint {}

            impl Integer for BigUint {
                #[inline]
                fn div_rem(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }

                #[inline]
                fn div_floor(&self, other: &BigUint) -> BigUint {
                    let (d, _) = division::div_rem_ref(self, other);
                    d
                }

                #[inline]
                fn mod_floor(&self, other: &BigUint) -> BigUint {
                    let (_, m) = division::div_rem_ref(self, other);
                    m
                }

                #[inline]
                fn div_mod_floor(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }

                #[inline]
                fn div_ceil(&self, other: &BigUint) -> BigUint {
                    let (d, m) = division::div_rem_ref(self, other);
                    if m.is_zero() {
                        d
                    } else {
                        d + 1u32
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                ///
                /// The result is always positive.
                #[inline]
                fn gcd(&self, other: &Self) -> Self {
                    #[inline] fn twos(x: &BigUint) -> u64 {
                        x.trailing_zeros().unwrap_or(0)
                    }

                    // Stein's algorithm
                    if self.is_zero() {
                        return other.clone();
                    }
                    if other.is_zero() {
                        return self.clone();
                    }
                    let mut m = self.clone();
                    let mut n = other.clone();

                    // find common factors of 2
                    let shift = cmp::min(twos(&n), twos(&m));

                    // divide m and n by 2 until odd
                    // m inside loop
                    n >>= twos(&n);

                    while !m.is_zero() {
                        m >>= twos(&m);
                        if n > m {
                            mem::swap(&mut n, &mut m)
                        }
                        m -= &n;
                    }

                    n << shift
                }

                /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                #[inline]
                fn lcm(&self, other: &BigUint) -> BigUint {
                    if self.is_zero() && other.is_zero() {
                        Self::ZERO
                    } else {
                        self / self.gcd(other) * other
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) and
                /// Lowest Common Multiple (LCM) together.
                #[inline]
                fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                    let gcd = self.gcd(other);
                    let lcm = if gcd.is_zero() {
                        Self::ZERO
                    } else {
                        self / &gcd * other
                    };
                    (gcd, lcm)
                }

                /// Deprecated, use `is_multiple_of` instead.
                #[inline]
                fn divides(&self, other: &BigUint) -> bool {
                    self.is_multiple_of(other)
                }

                /// Returns `true` if the number is a multiple of `other`.
                #[inline]
                fn is_multiple_of(&self, other: &BigUint) -> bool {
                    if other.is_zero() {
                        return self.is_zero();
                    }
                    (self % other).is_zero()
                }

                /// Returns `true` if the number is divisible by `2`.
                #[inline]
                fn is_even(&self) -> bool {
                    // Considering only the last digit.
                    match self.data.first() {
                        Some(x) => x.is_even(),
                        None => true,
                    }
                }

                /// Returns `true` if the number is not divisible by `2`.
                #[inline]
                fn is_odd(&self) -> bool {
                    !self.is_even()
                }

                /// Rounds up to nearest multiple of argument.
                #[inline]
                fn next_multiple_of(&self, other: &Self) -> Self {
                    let m = self.mod_floor(other);
                    if m.is_zero() {
                        self.clone()
                    } else {
                        self + (other - m)
                    }
                }
                /// Rounds down to nearest multiple of argument.
                #[inline]
                fn prev_multiple_of(&self, other: &Self) -> Self {
                    self - self.mod_floor(other)
                }

                fn dec(&mut self) {
                    *self -= 1u32;
                }

                fn inc(&mut self) {
                    *self += 1u32;
                }
            }

            #[inline] fn fixpoint<F>(mut x: BigUint, max_bits: u64, f: F) -> BigUint
            where
                F: Fn(&BigUint) -> BigUint,
            {
                let mut xn = f(&x);

                // If the value increased, then the initial guess must have been low.
                // Repeat until we reverse course.
                while x < xn {
                    // Sometimes an increase will go way too far, especially with large
                    // powers, and then take a long time to walk back.  We know an upper
                    // bound based on bit size, so saturate on that.
                    x = if xn.bits() > max_bits {
                        BigUint::one() << max_bits
                    } else {
                        xn
                    };
                    xn = f(&x);
                }

                // Now keep repeating while the estimate is decreasing.
                while x > xn {
                    x = xn;
                    xn = f(&x);
                }
                x
            }

            impl Roots for BigUint {
                // nth_root, sqrt and cbrt use Newton's method to compute
                // principal root of a given degree for a given integer.

                // Reference:
                // Brent & Zimmermann, Modern Computer Arithmetic, v0.5.9, Algorithm 1.14
                fn nth_root(&self, n: u32) -> Self {
                    assert!(n > 0, "root degree n must be at least 1");

                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    match n {
                        // Optimize for small n
                        1 => return self.clone(),
                        2 => return self.sqrt(),
                        3 => return self.cbrt(),
                        _ => (),
                    }

                    // The root of non-zero values less than 2ⁿ can only be 1.
                    let bits = self.bits();
                    let n64 = u64::from(n);
                    if bits <= n64 {
                        return BigUint::one();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.nth_root(n).into();
                    }

                    let max_bits = bits / n64 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64((f.ln() / f64::from(n)).exp()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2ⁿᵏ), its nth root ≈ (ⁿ√x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = Integer::div_ceil(&extra_bits, &n64);
                            let scale = root_scale * n64;
                            if scale < bits && bits - scale > n64 {
                                (self >> scale).nth_root(n) << root_scale
                            } else {
                                BigUint::one() << max_bits
                            }
                        }
                    };

                    #[cfg(not(feature = "std"))]
                    let guess = BigUint::one() << max_bits;

                    let n_min_1 = n - 1;
                    fixpoint(guess, max_bits, move |s| {
                        let q = self / s.pow(n_min_1);
                        let t = n_min_1 * s + q;
                        t / n
                    })
                }

                // Reference:
                // Brent & Zimmermann, Modern Computer Arithmetic, v0.5.9, Algorithm 1.13
                fn sqrt(&self) -> Self {
                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.sqrt().into();
                    }

                    let bits = self.bits();
                    let max_bits = bits / 2 + 1;
                    
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64(f.sqrt()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2²ᵏ), its sqrt ≈ (√x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = (extra_bits + 1) / 2;
                            let scale = root_scale * 2;
                            (self >> scale).sqrt() << root_scale
                        }
                    };

                    fixpoint(guess, max_bits, move |s| {
                        let q = self / s;
                        let t = s + q;
                        t >> 1
                    })
                }

                fn cbrt(&self) -> Self {
                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.cbrt().into();
                    }

                    let bits = self.bits();
                    let max_bits = bits / 3 + 1;
                    
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64(f.cbrt()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2³ᵏ), its cbrt ≈ (∛x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = (extra_bits + 2) / 3;
                            let scale = root_scale * 3;
                            (self >> scale).cbrt() << root_scale
                        }
                    };
                    

                    fixpoint(guess, max_bits, move |s| {
                        let q = self / (s * s);
                        let t = (s << 1) + q;
                        t / 3u32
                    })
                }
            }

            /// A generic trait for converting a value to a [`BigUint`].
            pub trait ToBigUint {
                /// Converts the value of `self` to a [`BigUint`].
                fn to_biguint(&self) -> Option<BigUint>;
            }

            /// Creates and initializes a [`BigUint`].
            ///
            /// The digits are in little-endian base matching `BigDigit`.
            #[inline] pub fn biguint_from_vec(digits: Vec<BigDigit>) -> BigUint {
                BigUint { data: digits }.normalized()
            }

            impl BigUint {
                /// A constant `BigUint` with value 0, useful for static initialization.
                pub const ZERO: Self = BigUint { data: Vec::new() };

                /// Creates and initializes a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline] pub fn new(digits: Vec<u32>) -> BigUint {
                    let mut big = Self::ZERO;

                    cfg_digit_expr!(
                        {
                            big.data = digits;
                            big.normalize();
                        },
                        big.assign_from_slice(&digits)
                    );

                    big
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline] pub fn from_slice(slice: &[u32]) -> BigUint {
                    let mut big = Self::ZERO;
                    big.assign_from_slice(slice);
                    big
                }

                /// Assign a value to a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline] pub fn assign_from_slice(&mut self, slice: &[u32]) {
                    self.data.clear();

                    cfg_digit_expr!(
                        self.data.extend_from_slice(slice),
                        self.data.extend(slice.chunks(2).map(u32_chunk_to_u64))
                    );

                    self.normalize();
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The bytes are in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from_bytes_be(b"A"),
                ///            BigUint::parse_bytes(b"65", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"AA"),
                ///            BigUint::parse_bytes(b"16705", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"AB"),
                ///            BigUint::parse_bytes(b"16706", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"Hello world!"),
                ///            BigUint::parse_bytes(b"22405534230753963835153736737", 10).unwrap());
                /// ```
                #[inline] pub fn from_bytes_be(bytes: &[u8]) -> BigUint {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        let mut v = bytes.to_vec();
                        v.reverse();
                        BigUint::from_bytes_le(&v)
                    }
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The bytes are in little-endian byte order.
                #[inline] pub fn from_bytes_le(bytes: &[u8]) -> BigUint {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        convert::from_bitwise_digits_le(bytes, 8)
                    }
                }

                /// Creates and initializes a [`BigUint`]. The input slice must contain
                /// ascii/utf8 characters in [0-9a-zA-Z].
                /// `radix` must be in the range `2...36`.
                ///
                /// The function `from_str_radix` from the `Num` trait provides the same logic
                /// for `&str` buffers.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint, ToBigUint};
                ///
                /// assert_eq!(BigUint::parse_bytes(b"1234", 10), ToBigUint::to_biguint(&1234));
                /// assert_eq!(BigUint::parse_bytes(b"ABCD", 16), ToBigUint::to_biguint(&0xABCD));
                /// assert_eq!(BigUint::parse_bytes(b"G", 16), None);
                /// ```
                #[inline] pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigUint> {
                    let s = str::from_utf8(buf).ok()?;
                    BigUint::from_str_radix(s, radix).ok()
                }

                /// Creates and initializes a [`BigUint`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in big-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint};
                ///
                /// let inbase190 = &[15, 33, 125, 12, 14];
                /// let a = BigUint::from_radix_be(inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), inbase190);
                /// ```
                pub fn from_radix_be(buf: &[u8], radix: u32) -> Option<BigUint> {
                    convert::from_radix_be(buf, radix)
                }

                /// Creates and initializes a [`BigUint`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in little-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint};
                ///
                /// let inbase190 = &[14, 12, 125, 33, 15];
                /// let a = BigUint::from_radix_be(inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), inbase190);
                /// ```
                pub fn from_radix_le(buf: &[u8], radix: u32) -> Option<BigUint> {
                    convert::from_radix_le(buf, radix)
                }

                /// Returns the byte representation of the [`BigUint`] in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"1125", 10).unwrap();
                /// assert_eq!(i.to_bytes_be(), vec![4, 101]);
                /// ```
                #[inline] pub fn to_bytes_be(&self) -> Vec<u8> {
                    let mut v = self.to_bytes_le();
                    v.reverse();
                    v
                }

                /// Returns the byte representation of the [`BigUint`] in little-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"1125", 10).unwrap();
                /// assert_eq!(i.to_bytes_le(), vec![101, 4]);
                /// ```
                #[inline] pub fn to_bytes_le(&self) -> Vec<u8> {
                    if self.is_zero() {
                        vec![0]
                    } else {
                        convert::to_bitwise_digits_le(self, 8)
                    }
                }

                /// Returns the `u32` digits representation of the [`BigUint`] ordered least significant digit
                /// first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).to_u32_digits(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).to_u32_digits(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).to_u32_digits(), vec![0, 1]);
                /// assert_eq!(BigUint::from(112500000000u64).to_u32_digits(), vec![830850304, 26]);
                /// ```
                #[inline] pub fn to_u32_digits(&self) -> Vec<u32> {
                    self.iter_u32_digits().collect()
                }

                /// Returns the `u64` digits representation of the [`BigUint`] ordered least significant digit
                /// first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).to_u64_digits(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).to_u64_digits(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).to_u64_digits(), vec![4294967296]);
                /// assert_eq!(BigUint::from(112500000000u64).to_u64_digits(), vec![112500000000]);
                /// assert_eq!(BigUint::from(1u128 << 64).to_u64_digits(), vec![0, 1]);
                /// ```
                #[inline] pub fn to_u64_digits(&self) -> Vec<u64> {
                    self.iter_u64_digits().collect()
                }

                /// Returns an iterator of `u32` digits representation of the [`BigUint`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).iter_u32_digits().collect::<Vec<u32>>(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).iter_u32_digits().collect::<Vec<u32>>(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).iter_u32_digits().collect::<Vec<u32>>(), vec![0, 1]);
                /// assert_eq!(BigUint::from(112500000000u64).iter_u32_digits().collect::<Vec<u32>>(), vec![830850304, 26]);
                /// ```
                #[inline] pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    U32Digits::new(self.data.as_slice())
                }

                /// Returns an iterator of `u64` digits representation of the [`BigUint`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).iter_u64_digits().collect::<Vec<u64>>(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967296]);
                /// assert_eq!(BigUint::from(112500000000u64).iter_u64_digits().collect::<Vec<u64>>(), vec![112500000000]);
                /// assert_eq!(BigUint::from(1u128 << 64).iter_u64_digits().collect::<Vec<u64>>(), vec![0, 1]);
                /// ```
                #[inline] pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    U64Digits::new(self.data.as_slice())
                }

                /// Returns the integer formatted as a string in the given radix.
                /// `radix` must be in the range `2...36`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"ff", 16).unwrap();
                /// assert_eq!(i.to_str_radix(16), "ff");
                /// ```
                #[inline] pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(self, radix);
                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }

                /// Returns the integer in the requested base in big-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based `u8` number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(0xFFFFu64).to_radix_be(159),
                ///            vec![2, 94, 27]);
                /// // 0xFFFF = 65535 = 2*(159^2) + 94*159 + 27
                /// ```
                #[inline] pub fn to_radix_be(&self, radix: u32) -> Vec<u8> {
                    let mut v = convert::to_radix_le(self, radix);
                    v.reverse();
                    v
                }

                /// Returns the integer in the requested base in little-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based u8 number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(0xFFFFu64).to_radix_le(159),
                ///            vec![27, 94, 2]);
                /// // 0xFFFF = 65535 = 27 + 94*159 + 2*(159^2)
                /// ```
                #[inline] pub fn to_radix_le(&self, radix: u32) -> Vec<u8> {
                    convert::to_radix_le(self, radix)
                }

                /// Determines the fewest bits necessary to express the [`BigUint`].
                #[inline] pub fn bits(&self) -> u64 {
                    if self.is_zero() {
                        return 0;
                    }
                    let zeros: u64 = self.data.last().unwrap().leading_zeros().into();
                    self.data.len() as u64 * u64::from(big_digit::BITS) - zeros
                }

                /// Strips off trailing zero bigdigits - comparisons require the last element in the vector to
                /// be nonzero.
                #[inline]
                fn normalize(&mut self) {
                    if let Some(&0) = self.data.last() {
                        let len = self.data.iter().rposition(|&d| d != 0).map_or(0, |i| i + 1);
                        self.data.truncate(len);
                    }
                    if self.data.len() < self.data.capacity() / 4 {
                        self.data.shrink_to_fit();
                    }
                }

                /// Returns a normalized [`BigUint`].
                #[inline]
                fn normalized(mut self) -> BigUint {
                    self.normalize();
                    self
                }

                /// Returns `self ^ exponent`.
                pub fn pow(&self, exponent: u32) -> Self {
                    Pow::pow(self, exponent)
                }

                /// Returns `(self ^ exponent) % modulus`.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self {
                    power::modpow(self, exponent, modulus)
                }

                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                pub fn modinv(&self, modulus: &Self) -> Option<Self> {
                    // Based on the inverse pseudocode listed here:
                    // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
                    // TODO: consider Binary or Lehmer's GCD algorithms for optimization.

                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );
                    if modulus.is_one() {
                        return Some(Self::zero());
                    }

                    let mut r0; // = modulus.clone();
                    let mut r1 = self % modulus;
                    let mut t0; // = Self::zero();
                    let mut t1; // = Self::one();

                    // Lift and simplify the first iteration to avoid some initial allocations.
                    if r1.is_zero() {
                        return None;
                    } else if r1.is_one() {
                        return Some(r1);
                    } else {
                        let (q, r2) = modulus.div_rem(&r1);
                        if r2.is_zero() {
                            return None;
                        }
                        r0 = r1;
                        r1 = r2;
                        t0 = Self::one();
                        t1 = modulus - q;
                    }

                    while !r1.is_zero() {
                        let (q, r2) = r0.div_rem(&r1);
                        r0 = r1;
                        r1 = r2;

                        // let t2 = (t0 - q * t1) % modulus;
                        let qt1 = q * &t1 % modulus;
                        let t2 = if t0 < qt1 {
                            t0 + (modulus - qt1)
                        } else {
                            t0 - qt1
                        };
                        t0 = t1;
                        t1 = t2;
                    }

                    if r0.is_one() {
                        Some(t0)
                    } else {
                        None
                    }
                }

                /// Returns the truncated principal square root of `self` --
                /// see [Roots::sqrt](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#method.sqrt)
                pub fn sqrt(&self) -> Self {
                    Roots::sqrt(self)
                }

                /// Returns the truncated principal cube root of `self` --
                /// see [Roots::cbrt](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#method.cbrt).
                pub fn cbrt(&self) -> Self {
                    Roots::cbrt(self)
                }

                /// Returns the truncated principal `n`th root of `self` --
                /// see [Roots::nth_root](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#tymethod.nth_root).
                pub fn nth_root(&self, n: u32) -> Self {
                    Roots::nth_root(self, n)
                }

                /// Returns the number of least-significant bits that are zero,
                /// or `None` if the entire number is zero.
                pub fn trailing_zeros(&self) -> Option<u64> {
                    let i = self.data.iter().position(|&digit| digit != 0)?;
                    let zeros: u64 = self.data[i].trailing_zeros().into();
                    Some(i as u64 * u64::from(big_digit::BITS) + zeros)
                }

                /// Returns the number of least-significant bits that are ones.
                pub fn trailing_ones(&self) -> u64 {
                    if let Some(i) = self.data.iter().position(|&digit| !digit != 0) {
                        let ones: u64 = self.data[i].trailing_ones().into();
                        i as u64 * u64::from(big_digit::BITS) + ones
                    } else {
                        self.data.len() as u64 * u64::from(big_digit::BITS)
                    }
                }

                /// Returns the number of one bits.
                pub fn count_ones(&self) -> u64 {
                    self.data.iter().map(|&d| u64::from(d.count_ones())).sum()
                }

                /// Returns whether the bit in the given position is set
                pub fn bit(&self, bit: u64) -> bool {
                    let bits_per_digit = u64::from(big_digit::BITS);
                    if let Some(digit_index) = (bit / bits_per_digit).to_usize() {
                        if let Some(digit) = self.data.get(digit_index) {
                            let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                            return (digit & bit_mask) != 0;
                        }
                    }
                    false
                }

                /// Sets or clears the bit in the given position
                ///
                /// Note that setting a bit greater than the current bit length, a reallocation may be needed
                /// to store the new digits
                pub fn set_bit(&mut self, bit: u64, value: bool) {
                    // Note: we're saturating `digit_index` and `new_len` -- any such case is guaranteed to
                    // fail allocation, and that's more consistent than adding our own overflow panics.
                    let bits_per_digit = u64::from(big_digit::BITS);
                    let digit_index = (bit / bits_per_digit).to_usize().unwrap_or(usize::MAX);
                    let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                    if value {
                        if digit_index >= self.data.len() {
                            let new_len = digit_index.saturating_add(1);
                            self.data.resize(new_len, 0);
                        }
                        self.data[digit_index] |= bit_mask;
                    } else if digit_index < self.data.len() {
                        self.data[digit_index] &= !bit_mask;
                        // the top bit may have been cleared, so normalize
                        self.normalize();
                    }
                }
            }

            impl ::num::traits::FromBytes for BigUint {
                type Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_be(bytes)
                }

                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_le(bytes)
                }
            }

            impl ::num::traits::ToBytes for BigUint {
                type Bytes = Vec<u8>;

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_bytes_be()
                }

                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_bytes_le()
                }
            }

            pub trait IntDigits {
                fn digits(&self) -> &[BigDigit];
                fn digits_mut(&mut self) -> &mut Vec<BigDigit>;
                fn normalize(&mut self);
                fn capacity(&self) -> usize;
                fn len(&self) -> usize;
            }

            impl IntDigits for BigUint {
                #[inline]
                fn digits(&self) -> &[BigDigit] {
                    &self.data
                }
                #[inline]
                fn digits_mut(&mut self) -> &mut Vec<BigDigit> {
                    &mut self.data
                }
                #[inline]
                fn normalize(&mut self) {
                    self.normalize();
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

            /// Convert a `u32` chunk (len is either 1 or 2) to a single `u64` digit
            #[inline] fn u32_chunk_to_u64(chunk: &[u32]) -> u64 {
                // raw could have odd length
                let mut digit = chunk[0] as u64;
                if let Some(&hi) = chunk.get(1) {
                    digit |= (hi as u64) << 32;
                }
                digit
            }
        }

        pub use self::biguint::BigUint;
        pub use self::biguint::ToBigUint;
        pub use self::biguint::U32Digits;
        pub use self::biguint::U64Digits;
        pub use self::bigint::BigInt;
        pub use self::bigint::Sign;
        pub use self::bigint::ToBigInt;

        type UsizePromotion = u64;
        type IsizePromotion = i64;

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct ParseBigIntError
        {
            kind: BigIntErrorKind,
        }

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum BigIntErrorKind
        {
            Empty,
            InvalidDigit,
        }

        impl ParseBigIntError
        {
            fn __description(&self) -> &str 
            {
                use self::BigIntErrorKind::*;
                match self.kind 
                {
                    Empty => "cannot parse integer from empty string",
                    InvalidDigit => "invalid digit found in string",
                }
            }

            fn empty() -> Self 
            {
                ParseBigIntError 
                {
                    kind: BigIntErrorKind::Empty,
                }
            }

            fn invalid() -> Self 
            {
                ParseBigIntError 
                {
                    kind: BigIntErrorKind::InvalidDigit,
                }
            }
        }

        impl fmt::Display for ParseBigIntError 
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            {
                self.__description().fmt(f)
            }
        }
        
        impl ::error::Error for ParseBigIntError
        {
            fn description(&self) -> &str {
                self.__description()
            }
        }
        /// The error type returned when a checked conversion regarding big integer fails.
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub struct TryFromBigIntError<T>
        {
            original: T,
        }

        impl<T> TryFromBigIntError<T>
        {
            fn new(original: T) -> Self {
                TryFromBigIntError { original }
            }

            fn __description(&self) -> &str {
                "out of range conversion regarding big integer attempted"
            }

            /// Extract the original value, if available. The value will be available
            /// if the type before conversion was either [`BigInt`] or [`BigUint`].
            pub fn into_original(self) -> T {
                self.original
            }
        }
        
        impl<T> ::error::Error for TryFromBigIntError<T> where
        T: fmt::Debug
        {
            fn description(&self) -> &str {
                self.__description()
            }
        }

        impl<T> fmt::Display for TryFromBigIntError<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            {
                self.__description().fmt(f)
            }
        }

        mod big_digit
        {
            /// A [`BigDigit`] is a [`BigUint`]'s composing element.
            pub type BigDigit = u64;
            /// A [`DoubleBigDigit`] is the internal type used to do the computations.
            pub type DoubleBigDigit = u128;

            pub const BITS: u8 = BigDigit::BITS as u8;
            pub const HALF_BITS: u8 = BITS / 2;
            pub const HALF: BigDigit = (1 << HALF_BITS) - 1;
            pub const MAX: BigDigit = BigDigit::MAX;
            pub const LO_MASK: DoubleBigDigit = MAX as DoubleBigDigit;

            #[inline] fn get_hi(n: DoubleBigDigit) -> BigDigit { (n >> BITS) as BigDigit }

            #[inline] fn get_lo(n: DoubleBigDigit) -> BigDigit { (n & LO_MASK) as BigDigit }
            /// Split one [`DoubleBigDigit`] into two [`BigDigit`]s.
            #[inline] pub fn from_doublebigdigit(n: DoubleBigDigit) -> (BigDigit, BigDigit)
            { (get_hi(n), get_lo(n)) }
            /// Join two [`BigDigit`]s into one [`DoubleBigDigit`].
            #[inline] pub fn to_doublebigdigit(hi: BigDigit, lo: BigDigit) -> DoubleBigDigit
            { DoubleBigDigit::from(lo) | (DoubleBigDigit::from(hi) << BITS) }
        }
    }

    pub mod fractional
    {
        use ::
        {
            num::traits::{ pow },
            *,
        };

        pub fn frac_from_whole_and_dec(whole: BigInt, decimal: BigInt, dec_len: usize) -> BigRational
        {
            let denom = pow(BigInt::from_u8(10).unwrap(), dec_len);
            BigRational::new(whole, 1.into()) + BigRational::new(decimal, denom)
        }
    }

    pub mod integers
    {
        pub use ::num_integer::{ * };
    }

    pub mod rational
    {
        //! Rational numbers
        use ::
        {
            error::{ Error },
            fmt::{ self, Binary, Display, Formatter, LowerExp, LowerHex, Octal, UpperExp, UpperHex },
            hash::{Hash, Hasher},
            num::
            {
                big::{ BigInt, BigUint, Sign, ToBigInt },
                integers::{ Integer },
                traits::
                { 
                    float::FloatCore, Bounded, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, ConstOne, ConstZero, 
                    FromPrimitive, Inv, Num, NumCast, One, Pow, Signed, ToPrimitive, Unsigned, Zero,
                },
            },
            ops::{Add, Div, Mul, Neg, Rem, ShlAssign, Sub},
            str::FromStr,
            *,
        };

        mod pow
        {
            use ::
            {
                num::
                {
                    integers::{ Integer },
                    traits::{ One, Pow },
                },
                *,
            };

            use super::Ratio; 

            macro_rules! pow_unsigned_impl {
                (@ $exp:ty) => {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: $exp) -> Ratio<T> {
                        Ratio::new_raw(self.numer.pow(expon), self.denom.pow(expon))
                    }
                };
                ($exp:ty) => {
                    impl<T: Clone + Integer + Pow<$exp, Output = T>> Pow<$exp> for Ratio<T> {
                        pow_unsigned_impl!(@ $exp);
                    }
                    impl<'a, T: Clone + Integer> Pow<$exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$exp, Output = T>,
                    {
                        pow_unsigned_impl!(@ $exp);
                    }
                    impl<'b, T: Clone + Integer + Pow<$exp, Output = T>> Pow<&'b $exp> for Ratio<T> {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                    impl<'a, 'b, T: Clone + Integer> Pow<&'b $exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$exp, Output = T>,
                    {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                };
            }
            pow_unsigned_impl!(u8);
            pow_unsigned_impl!(u16);
            pow_unsigned_impl!(u32);
            pow_unsigned_impl!(u64);
            pow_unsigned_impl!(u128);
            pow_unsigned_impl!(usize);

            macro_rules! pow_signed_impl {
                (@ &'b BigInt, BigUint) => {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: &'b BigInt) -> Ratio<T> {
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
                    #[inline] fn pow(self, expon: $exp) -> Ratio<T> {
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
            pow_signed_impl!(i8, u8);
            pow_signed_impl!(i16, u16);
            pow_signed_impl!(i32, u32);
            pow_signed_impl!(i64, u64);
            pow_signed_impl!(i128, u128);
            pow_signed_impl!(isize, usize);
            
            mod bigint
            {
                use super::*;
                use ::num::big::{BigInt, BigUint, Sign};

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigUint> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'a, T: Clone + Integer> Pow<BigUint> for &'a Ratio<T> where
                    &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigUint> for Ratio<T> {
                    pow_unsigned_impl!(@ &'b BigUint);
                }
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigUint> for &'a Ratio<T> where
                    &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigInt> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'a, T: Clone + Integer> Pow<BigInt> for &'a Ratio<T> where
                    &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigInt> for Ratio<T> {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigInt> for &'a Ratio<T> where
                    &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
            }
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
            #[inline] pub fn from_integer(t: T) -> Ratio<T>
            {
                Ratio::new_raw(t, One::one())
            }
            /// Converts to an integer, rounding towards zero.
            #[inline] pub fn to_integer(&self) -> T
            {
                self.trunc().numer
            }
            /// Returns true if the rational number is an integer (denominator is 1).
            #[inline] pub fn is_integer(&self) -> bool
            {
                self.denom.is_one()
            }
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

                // FIXME(#5992): assignment operator overloads
                // T: Clone + Integer != T: Clone + NumAssign

                #[inline]
                fn replace_with<T: Zero>(x: &mut T, f: impl FnOnce(T) -> T) {
                    let y = ::mem::replace(x, T::zero());
                    *x = f(y);
                }

                // self.numer /= g;
                replace_with(&mut self.numer, |x| x / g.clone());

                // self.denom /= g;
                replace_with(&mut self.denom, |x| x / g);

                // keep denom positive!
                if self.denom < T::zero() {
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
            #[inline] pub fn recip(&self) -> Ratio<T>
            {
                self.clone().into_recip()
            }
            #[inline] fn into_recip(self) -> Ratio<T> 
            {
                match self.numer.cmp(&T::zero()) {
                    cmp::Ordering::Equal => panic!("division by zero"),
                    cmp::Ordering::Greater => Ratio::new_raw(self.denom, self.numer),
                    cmp::Ordering::Less => Ratio::new_raw(T::zero() - self.denom, T::zero() - self.numer),
                }
            }
            /// Rounds towards minus infinity.
            #[inline] pub fn floor(&self) -> Ratio<T>
            {
                if *self < Zero::zero() {
                    let one: T = One::one();
                    Ratio::from_integer(
                        (self.numer.clone() - self.denom.clone() + one) / self.denom.clone(),
                    )
                } else {
                    Ratio::from_integer(self.numer.clone() / self.denom.clone())
                }
            }
            /// Rounds towards plus infinity.
            #[inline] pub fn ceil(&self) -> Ratio<T>
            {
                if *self < Zero::zero() {
                    Ratio::from_integer(self.numer.clone() / self.denom.clone())
                } else {
                    let one: T = One::one();
                    Ratio::from_integer(
                        (self.numer.clone() + self.denom.clone() - one) / self.denom.clone(),
                    )
                }
            }
            /// Rounds to the nearest integer. Rounds half-way cases away from zero.
            #[inline] pub fn round(&self) -> Ratio<T>
            {
                let zero: Ratio<T> = Zero::zero();
                let one: T = One::one();
                let two: T = one.clone() + one.clone();
                
                let mut fractional = self.fract();
                if fractional < zero {
                    fractional = zero - fractional
                };
                
                let half_or_larger = if fractional.denom.is_even() {
                    fractional.numer >= fractional.denom / two
                } else {
                    fractional.numer >= (fractional.denom / two) + one
                };

                if half_or_larger {
                    let one: Ratio<T> = One::one();
                    if *self >= Zero::zero() {
                        self.trunc() + one
                    } else {
                        self.trunc() - one
                    }
                } else {
                    self.trunc()
                }
            }
            /// Rounds towards zero.
            #[inline] pub fn trunc(&self) -> Ratio<T>
            { Ratio::from_integer(self.numer.clone() / self.denom.clone()) }
            /// Returns the fractional part of a number, with division rounded towards zero.
            #[inline] pub fn fract(&self) -> Ratio<T> 
            { Ratio::new_raw(self.numer.clone() % self.denom.clone(), self.denom.clone()) }
            /// Raises the `Ratio` to the power of an exponent.
            #[inline] pub fn pow(&self, expon: i32) -> Ratio<T> where
            for<'a> &'a T: Pow<u32, Output = T>,
            {
                Pow::pow(self, expon)
            }
        }
        
        impl Ratio<BigInt>
        {
            /// Converts a float into a rational number.
            pub fn from_float<T: FloatCore>(f: T) -> Option<BigRational>
            {
                if !f.is_finite() { return None; }

                let (mantissa, exponent, sign) = f.integer_decode();
                let bigint_sign = if sign == 1 { Sign::Plus } else { Sign::Minus };
                if exponent < 0 {
                    let one: BigInt = One::one();
                    let denom: BigInt = one << ((-exponent) as usize);
                    let numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    Some(Ratio::new(BigInt::from_biguint(bigint_sign, numer), denom))
                } else {
                    let mut numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    numer <<= exponent as usize;
                    Some(Ratio::from_integer(BigInt::from_biguint(
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
        T: Clone + Integer,
        {
            fn from(x: T) -> Ratio<T> { Ratio::from_integer(x) }
        }
        
        impl<T> From<(T, T)> for Ratio<T> where
        T: Clone + Integer,
        {
            fn from(pair: (T, T)) -> Ratio<T> { Ratio::new(pair.0, pair.1) }
        }

        impl<T: Clone + Integer> Ord for Ratio<T>
        {
            #[inline] fn cmp(&self, other: &Self) -> cmp::Ordering
            {
                if self.denom == other.denom {
                    let ord = self.numer.cmp(&other.numer);
                    return if self.denom < T::zero() {
                        ord.reverse()
                    } else {
                        ord
                    };
                }
                
                if self.numer == other.numer {
                    if self.numer.is_zero() {
                        return cmp::Ordering::Equal;
                    }
                    let ord = self.denom.cmp(&other.denom);
                    return if self.numer < T::zero() {
                        ord
                    } else {
                        ord.reverse()
                    };
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
            #[inline] fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering>
            {
                Some(self.cmp(other))
            }
        }

        impl<T: Clone + Integer> PartialEq for Ratio<T>
        {
            #[inline] fn eq(&self, other: &Self) -> bool
            {
                self.cmp(other) == cmp::Ordering::Equal
            }
        }

        impl<T: Clone + Integer> Eq for Ratio<T> {}
        
        impl<T: Clone + Integer + Hash> Hash for Ratio<T>
        {
            fn hash<H: Hasher>(&self, state: &mut H)
            {
                recurse(&self.numer, &self.denom, state);

                fn recurse<T: Integer + Hash, H: Hasher>(numer: &T, denom: &T, state: &mut H) 
                {
                    if !denom.is_zero() {
                        let (int, rem) = numer.div_mod_floor(denom);
                        int.hash(state);
                        recurse(denom, &rem, state);
                    } else {
                        denom.hash(state);
                    }
                }
            }
        }

        mod iter_sum_product
        {
            use ::
            {
                iter::{Product, Sum},
                num::
                {
                    integers::{ Integer },
                    traits::{ One, Zero },
                },
                *,
            }; use super::Ratio; 

            impl<T: Integer + Clone> Sum for Ratio<T> {
                fn sum<I>(iter: I) -> Self
                where
                    I: Iterator<Item = Ratio<T>>,
                {
                    iter.fold(Self::zero(), |sum, num| sum + num)
                }
            }

            impl<'a, T: Integer + Clone> Sum<&'a Ratio<T>> for Ratio<T> {
                fn sum<I>(iter: I) -> Self
                where
                    I: Iterator<Item = &'a Ratio<T>>,
                {
                    iter.fold(Self::zero(), |sum, num| sum + num)
                }
            }

            impl<T: Integer + Clone> Product for Ratio<T> {
                fn product<I>(iter: I) -> Self
                where
                    I: Iterator<Item = Ratio<T>>,
                {
                    iter.fold(Self::one(), |prod, num| prod * num)
                }
            }

            impl<'a, T: Integer + Clone> Product<&'a Ratio<T>> for Ratio<T> {
                fn product<I>(iter: I) -> Self
                where
                    I: Iterator<Item = &'a Ratio<T>>,
                {
                    iter.fold(Self::one(), |prod, num| prod * num)
                }
            }
        }

        mod opassign
        {
            use ::ops::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};

            use super::Ratio;
            use ::num::integers::Integer;
            use ::num::traits::NumAssign;

            impl<T: Clone + Integer + NumAssign> AddAssign for Ratio<T> {
                fn add_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer += other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer + rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }

            // (a/b) / (c/d) = (a/gcd_ac)*(d/gcd_bd) / ((c/gcd_ac)*(b/gcd_bd))
            impl<T: Clone + Integer + NumAssign> DivAssign for Ratio<T> {
                fn div_assign(&mut self, other: Ratio<T>) {
                    let gcd_ac = self.numer.gcd(&other.numer);
                    let gcd_bd = self.denom.gcd(&other.denom);
                    self.numer /= gcd_ac.clone();
                    self.numer *= other.denom / gcd_bd.clone();
                    self.denom /= gcd_bd;
                    self.denom *= other.numer / gcd_ac;
                    self.reduce(); // TODO: remove this line. see #8.
                }
            }

            // a/b * c/d = (a/gcd_ad)*(c/gcd_bc) / ((d/gcd_ad)*(b/gcd_bc))
            impl<T: Clone + Integer + NumAssign> MulAssign for Ratio<T> {
                fn mul_assign(&mut self, other: Ratio<T>) {
                    let gcd_ad = self.numer.gcd(&other.denom);
                    let gcd_bc = self.denom.gcd(&other.numer);
                    self.numer /= gcd_ad.clone();
                    self.numer *= other.numer / gcd_bc.clone();
                    self.denom /= gcd_bc;
                    self.denom *= other.denom / gcd_ad;
                    self.reduce(); // TODO: remove this line. see #8.
                }
            }

            impl<T: Clone + Integer + NumAssign> RemAssign for Ratio<T> {
                fn rem_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer %= other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer % rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> SubAssign for Ratio<T> {
                fn sub_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer -= other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer - rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }

            // a/b + c/1 = (a*1 + b*c) / (b*1) = (a + b*c) / b
            impl<T: Clone + Integer + NumAssign> AddAssign<T> for Ratio<T> {
                fn add_assign(&mut self, other: T) {
                    self.numer += self.denom.clone() * other;
                    self.reduce();
                }
            }

            impl<T: Clone + Integer + NumAssign> DivAssign<T> for Ratio<T> {
                fn div_assign(&mut self, other: T) {
                    let gcd = self.numer.gcd(&other);
                    self.numer /= gcd.clone();
                    self.denom *= other / gcd;
                    self.reduce(); // TODO: remove this line. see #8.
                }
            }

            impl<T: Clone + Integer + NumAssign> MulAssign<T> for Ratio<T> {
                fn mul_assign(&mut self, other: T) {
                    let gcd = self.denom.gcd(&other);
                    self.denom /= gcd.clone();
                    self.numer *= other / gcd;
                    self.reduce(); // TODO: remove this line. see #8.
                }
            }

            // a/b % c/1 = (a*1 % b*c) / (b*1) = (a % b*c) / b
            impl<T: Clone + Integer + NumAssign> RemAssign<T> for Ratio<T> {
                fn rem_assign(&mut self, other: T) {
                    self.numer %= self.denom.clone() * other;
                    self.reduce();
                }
            }

            // a/b - c/1 = (a*1 - b*c) / (b*1) = (a - b*c) / b
            impl<T: Clone + Integer + NumAssign> SubAssign<T> for Ratio<T> {
                fn sub_assign(&mut self, other: T) {
                    self.numer -= self.denom.clone() * other;
                    self.reduce();
                }
            }

            macro_rules! forward_op_assign {
                (impl $imp:ident, $method:ident) => {
                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a Ratio<T>> for Ratio<T> {
                        #[inline]
                        fn $method(&mut self, other: &Ratio<T>) {
                            self.$method(other.clone())
                        }
                    }
                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a T> for Ratio<T> {
                        #[inline]
                        fn $method(&mut self, other: &T) {
                            self.$method(other.clone())
                        }
                    }
                };
            }

            forward_op_assign!(impl AddAssign, add_assign);
            forward_op_assign!(impl DivAssign, div_assign);
            forward_op_assign!(impl MulAssign, mul_assign);
            forward_op_assign!(impl RemAssign, rem_assign);
            forward_op_assign!(impl SubAssign, sub_assign);
        }

        macro_rules! forward_ref_ref_binop 
        {
            (impl $imp:ident, $method:ident) => {
                impl<'a, 'b, T: Clone + Integer> $imp<&'b Ratio<T>> for &'a Ratio<T> {
                    type Output = Ratio<T>;

                    #[inline] fn $method(self, other: &'b Ratio<T>) -> Ratio<T> {
                        self.clone().$method(other.clone())
                    }
                }
                impl<'a, 'b, T: Clone + Integer> $imp<&'b T> for &'a Ratio<T> {
                    type Output = Ratio<T>;

                    #[inline] fn $method(self, other: &'b T) -> Ratio<T> {
                        self.clone().$method(other.clone())
                    }
                }
            };
        }

        macro_rules! forward_ref_val_binop 
        {
            (impl $imp:ident, $method:ident) =>
            {
                impl<'a, T> $imp<Ratio<T>> for &'a Ratio<T> where
                T: Clone + Integer,
                {
                    type Output = Ratio<T>;

                    #[inline] fn $method(self, other: Ratio<T>) -> Ratio<T> {
                        self.clone().$method(other)
                    }
                }

                impl<'a, T> $imp<T> for &'a Ratio<T> where
                T: Clone + Integer,
                {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, other: T) -> Ratio<T> { self.clone().$method(other) }
                }
            };
        }

        macro_rules! forward_val_ref_binop 
        {
            (impl $imp:ident, $method:ident) => {
                impl<'a, T> $imp<&'a Ratio<T>> for Ratio<T> where
                    T: Clone + Integer,
                {
                    type Output = Ratio<T>;

                    #[inline] fn $method(self, other: &Ratio<T>) -> Ratio<T> {
                        self.$method(other.clone())
                    }
                }
                impl<'a, T> $imp<&'a T> for Ratio<T> where
                    T: Clone + Integer,
                {
                    type Output = Ratio<T>;

                    #[inline] fn $method(self, other: &T) -> Ratio<T> {
                        self.$method(other.clone())
                    }
                }
            };
        }

        macro_rules! forward_all_binop 
        {
            (impl $imp:ident, $method:ident) => {
                forward_ref_ref_binop!(impl $imp, $method);
                forward_ref_val_binop!(impl $imp, $method);
                forward_val_ref_binop!(impl $imp, $method);
            };
        }
        
        forward_all_binop!(impl Mul, mul);
        
        impl<T> Mul<Ratio<T>> for Ratio<T> where
        T: Clone + Integer
        {
            type Output = Ratio<T>;
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
        T: Clone + Integer,
        {
            type Output = Ratio<T>;
            #[inline] fn div(self, rhs: T) -> Ratio<T>
            {
                let gcd = self.numer.gcd(&rhs);
                Ratio::new(self.numer / gcd.clone(), self.denom * (rhs / gcd))
            }
        }

        macro_rules! arith_impl 
        {
            (impl $imp:ident, $method:ident) => {
                forward_all_binop!(impl $imp, $method);
                // Abstracts a/b `op` c/d = (a*lcm/b `op` c*lcm/d)/lcm where lcm = lcm(b,d)
                impl<T: Clone + Integer> $imp<Ratio<T>> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: Ratio<T>) -> Ratio<T> {
                        if self.denom == rhs.denom {
                            return Ratio::new(self.numer.$method(rhs.numer), rhs.denom);
                        }
                        let lcm = self.denom.lcm(&rhs.denom);
                        let lhs_numer = self.numer * (lcm.clone() / self.denom);
                        let rhs_numer = rhs.numer * (lcm.clone() / rhs.denom);
                        Ratio::new(lhs_numer.$method(rhs_numer), lcm)
                    }
                }
                // Abstracts the a/b `op` c/1 = (a*1 `op` b*c) / (b*1) = (a `op` b*c) / b pattern
                impl<T: Clone + Integer> $imp<T> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: T) -> Ratio<T> {
                        Ratio::new(self.numer.$method(self.denom.clone() * rhs), self.denom)
                    }
                }
            };
        }

        arith_impl!(impl Add, add);
        arith_impl!(impl Sub, sub);
        arith_impl!(impl Rem, rem);
        
        impl<T> CheckedMul for Ratio<T> where
        T: Clone + Integer + CheckedMul,
        {
            #[inline] fn checked_mul(&self, rhs: &Ratio<T>) -> Option<Ratio<T>>
            {
                let gcd_ad = self.numer.gcd(&rhs.denom);
                let gcd_bc = self.denom.gcd(&rhs.numer);
                Some(Ratio::new(
                    (self.numer.clone() / gcd_ad.clone())
                        .checked_mul(&(rhs.numer.clone() / gcd_bc.clone()))?,
                    (self.denom.clone() / gcd_bc).checked_mul(&(rhs.denom.clone() / gcd_ad))?,
                ))
            }
        }
        
        impl<T> CheckedDiv for Ratio<T> where
        T: Clone + Integer + CheckedMul,
        {
            #[inline] fn checked_div(&self, rhs: &Ratio<T>) -> Option<Ratio<T>> 
            {
                if rhs.is_zero() {
                    return None;
                }
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
                
                if denom.is_zero() 
                {
                    None
                } 
                else if numer.is_zero() 
                {
                    Some(Self::zero())
                }
                else if numer == denom 
                {
                    Some(Self::one())
                }
                else
                {
                    let g = numer.gcd(&denom);
                    let numer = numer / g.clone();
                    let denom = denom / g;
                    let raw = if denom < T::zero() 
                    { 
                        let n1 = T::zero() - T::one();
                        Ratio::new_raw(numer.checked_mul(&n1)?, denom.checked_mul(&n1)?)
                    } else 
                    {
                        Ratio::new_raw(numer, denom)
                    };
                    Some(raw)
                }
            }
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
        T: Clone + Integer,
        {
            type Output = Ratio<T>;
            #[inline] fn inv(self) -> Ratio<T> 
            {
                self.recip()
            }
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
            #[inline] fn zero() -> Ratio<T> 
            {
                Ratio::new_raw(Zero::zero(), One::one())
            }

            #[inline] fn is_zero(&self) -> bool
            {
                self.numer.is_zero()
            }

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
            #[inline] fn one() -> Ratio<T>
            {
                Ratio::new_raw(One::one(), One::one())
            }

            #[inline] fn is_one(&self) -> bool
            {
                self.numer == self.denom
            }

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
            fn from_str_radix(s: &str, radix: u32) -> Result<Ratio<T>, ParseRatioError> {
                if s.splitn(2, '/').count() == 2 {
                    let mut parts = s.splitn(2, '/').map(|ss| {
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
                    } else {
                        Ok(Ratio::new(numer, denom))
                    }
                } else {
                    Err(ParseRatioError {
                        kind: RatioErrorKind::ParseError,
                    })
                }
            }
        }

        impl<T: Clone + Integer + Signed> Signed for Ratio<T>
        {
            #[inline] fn abs(&self) -> Ratio<T> {
                if self.is_negative() {
                    -self.clone()
                } else {
                    self.clone()
                }
            }

            #[inline] fn abs_sub(&self, other: &Ratio<T>) -> Ratio<T> {
                if *self <= *other {
                    Zero::zero()
                } else {
                    self - other
                }
            }

            #[inline] fn signum(&self) -> Ratio<T> {
                if self.is_positive() {
                    Self::one()
                } else if self.is_zero() {
                    Self::zero()
                } else {
                    -Self::one()
                }
            }

            #[inline] fn is_positive(&self) -> bool {
                (self.numer.is_positive() && self.denom.is_positive())
                    || (self.numer.is_negative() && self.denom.is_negative())
            }

            #[inline] fn is_negative(&self) -> bool {
                (self.numer.is_negative() && self.denom.is_positive())
                    || (self.numer.is_positive() && self.denom.is_negative())
            }
        }
        
        macro_rules! impl_formatting 
        {
            ($fmt_trait:ident, $prefix:expr, $fmt_str:expr, $fmt_alt:expr) => {
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
            fn from_str(s: &str) -> Result<Ratio<T>, ParseRatioError> {
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
            fn from(val: Ratio<T>) -> Self 
            {
                (val.numer, val.denom)
            }
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
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                self.kind.description().fmt(f)
            }
        }
        
        impl Error for ParseRatioError 
        {
            #[allow(deprecated)]
            fn description(&self) -> &str 
            {
                self.kind.description()
            }
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
            fn from_i64(n: i64) -> Option<Self> 
            {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_i128(n: i128) -> Option<Self> 
            {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_u64(n: u64) -> Option<Self> 
            {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_u128(n: u128) -> Option<Self> 
            {
                Some(Ratio::from_integer(n.into()))
            }

            fn from_f32(n: f32) -> Option<Self> 
            {
                Ratio::from_float(n)
            }

            fn from_f64(n: f64) -> Option<Self> 
            {
                Ratio::from_float(n)
            }
        }

        macro_rules! from_primitive_integer 
        {
            ($typ:ty, $approx:ident) => 
            {
                impl FromPrimitive for Ratio<$typ> 
                {
                    fn from_i64(n: i64) -> Option<Self> 
                    {
                        <$typ as FromPrimitive>::from_i64(n).map(Ratio::from_integer)
                    }

                    fn from_i128(n: i128) -> Option<Self> 
                    {
                        <$typ as FromPrimitive>::from_i128(n).map(Ratio::from_integer)
                    }

                    fn from_u64(n: u64) -> Option<Self> 
                    {
                        <$typ as FromPrimitive>::from_u64(n).map(Ratio::from_integer)
                    }

                    fn from_u128(n: u128) -> Option<Self> 
                    {
                        <$typ as FromPrimitive>::from_u128(n).map(Ratio::from_integer)
                    }

                    fn from_f32(n: f32) -> Option<Self> 
                    {
                        $approx(n, 10e-20, 30)
                    }

                    fn from_f64(n: f64) -> Option<Self> 
                    {
                        $approx(n, 10e-20, 30)
                    }
                }
            };
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
        F: FloatCore + NumCast,
        {
            let negative = val.is_sign_negative();
            let abs_val = val.abs();

            let r = approximate_float_unsigned(abs_val, max_error, max_iterations)?;
            
            Some(if negative { r.neg() } else { r })
        }
        
        fn approximate_float_unsigned<T, F>(val: F, max_error: F, max_iterations: usize) -> Option<Ratio<T>> where
        T: Integer + Bounded + NumCast + Clone,
        F: FloatCore + NumCast,
        {
            if val < F::zero() || val.is_nan() 
            {
                return None;
            }

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
            if q > t_max_f 
            {
                return None;
            }

            for _ in 0..max_iterations 
            {
                let a = match <T as NumCast>::from(q) {
                    None => break,
                    Some(a) => a,
                };

                let a_f = match <F as NumCast>::from(a.clone()) 
                {
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
                if !g.is_zero() 
                {
                    n1 = n1 / g.clone();
                    d1 = d1 / g.clone();
                }

                // Close enough?
                let (n_f, d_f) = match (<F as NumCast>::from(n), <F as NumCast>::from(d)) 
                {
                    (Some(n_f), Some(d_f)) => (n_f, d_f),
                    _ => break,
                };
                if (n_f / d_f - val).abs() < max_error 
                {
                    break;
                }

                // Prevent division by ~0
                if f < epsilon 
                {
                    break;
                }
                q = f.recip();
            }

            // Overflow
            if d1.is_zero() 
            {
                return None;
            }

            Some(Ratio::new(n1, d1))
        }
        
        impl<T: Clone + Integer + ToPrimitive + ToBigInt> ToPrimitive for Ratio<T>
        {
            fn to_i64(&self) -> Option<i64> 
            {
                self.to_integer().to_i64()
            }

            fn to_i128(&self) -> Option<i128> 
            {
                self.to_integer().to_i128()
            }

            fn to_u64(&self) -> Option<u64> 
            {
                self.to_integer().to_u64()
            }

            fn to_u128(&self) -> Option<u128> 
            {
                self.to_integer().to_u128()
            }

            fn to_f64(&self) -> Option<f64> 
            {
                let float = match (self.numer.to_i64(), self.denom.to_i64()) 
                {
                    (Some(numer), Some(denom)) => ratio_to_f64
                    (
                        <i128 as From<_>>::from(numer),
                        <i128 as From<_>>::from(denom),
                    ),
                    _ => 
                    {
                        let numer: BigInt = self.numer.to_bigint()?;
                        let denom: BigInt = self.denom.to_bigint()?;
                        ratio_to_f64(numer, denom)
                    }
                };
                if float.is_nan() 
                {
                    None
                } else 
                {
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
            fn bits(&self) -> u64 
            {
                self.bits()
            }
        }

        impl Bits for i128 
        {
            fn bits(&self) -> u64 
            {
                (128 - self.wrapping_abs().leading_zeros()).into()
            }
        }
        /// Converts a ratio of `T` to an f64.
        fn ratio_to_f64<T: Bits + Clone + Integer + Signed + ShlAssign<usize> + ToPrimitive>
        (
            numer: T,
            denom: T
        ) -> f64 
        {
            use std::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, MIN_EXP, RADIX};

            assert_eq!(
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );
            
            const MAX_EXACT_INT: i64 = 1i64 << MANTISSA_DIGITS;
            const MIN_EXACT_INT: i64 = -MAX_EXACT_INT;

            let flo_sign = numer.signum().to_f64().unwrap() / denom.signum().to_f64().unwrap();
            if !flo_sign.is_normal() {
                return flo_sign;
            }
            
            if let (Some(n), Some(d)) = (numer.to_i64(), denom.to_i64()) {
                let exact = MIN_EXACT_INT..=MAX_EXACT_INT;
                if exact.contains(&n) && exact.contains(&d) {
                    return n.to_f64().unwrap() / d.to_f64().unwrap();
                }
            }
            
            let mut numer = numer.abs();
            let mut denom = denom.abs();
            let (is_diff_positive, absolute_diff) = match numer.bits().checked_sub(denom.bits()) {
                Some(diff) => (true, diff),
                None => (false, denom.bits() - numer.bits()),
            };
            
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
            
            let shift: isize = diff.max(MIN_EXP as isize) - MANTISSA_DIGITS as isize - 2;
            if shift >= 0 {
                denom <<= shift as usize
            } else {
                numer <<= -shift as usize
            };

            let (quotient, remainder) = numer.div_rem(&denom);
            
            let mut quotient = quotient.to_u64().unwrap();
            let n_rounding_bits = {
                let quotient_bits = 64 - quotient.leading_zeros() as isize;
                let subnormal_bits = MIN_EXP as isize - shift;
                quotient_bits.max(subnormal_bits) - MANTISSA_DIGITS as isize
            } as usize;
            debug_assert!(n_rounding_bits == 2 || n_rounding_bits == 3);
            let rounding_bit_mask = (1u64 << n_rounding_bits) - 1;
            
            let ls_bit = quotient & (1u64 << n_rounding_bits) != 0;
            let ms_rounding_bit = quotient & (1u64 << (n_rounding_bits - 1)) != 0;
            let ls_rounding_bits = quotient & (rounding_bit_mask >> 1) != 0;
            if ms_rounding_bit && (ls_bit || ls_rounding_bits || !remainder.is_zero()) {
                quotient += 1u64 << n_rounding_bits;
            }
            quotient &= !rounding_bit_mask;
            
            let q_float = quotient as f64 * flo_sign;
            ldexp(q_float, shift as i32)
        }
        /// Multiply `x` by 2 to the power of `exp`. Returns an accurate result even if `2^exp` is not representable.
        fn ldexp(x: f64, exp: i32) -> f64
        {
            use std::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, RADIX};

            assert_eq!(
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );

            const EXPONENT_MASK: u64 = 0x7ff << 52;
            const MAX_UNSIGNED_EXPONENT: i32 = 0x7fe;
            const MIN_SUBNORMAL_POWER: i32 = MANTISSA_DIGITS as i32;

            if x.is_zero() || x.is_infinite() || x.is_nan() {
                return x;
            }
            
            if exp > 3 * MAX_EXP {
                return INFINITY * x.signum();
            } else if exp < -3 * MAX_EXP {
                return 0.0 * x.signum();
            }
            
            let (bits, curr_exp) = if !x.is_normal() {
                let normal_x = x * 2f64.powi(MIN_SUBNORMAL_POWER);
                let bits = normal_x.to_bits();
                (
                    bits,
                    ((bits & EXPONENT_MASK) >> 52) as i32 - MIN_SUBNORMAL_POWER,
                )
            } else {
                let bits = x.to_bits();
                let curr_exp = (bits & EXPONENT_MASK) >> 52;
                (bits, curr_exp as i32)
            };
            
            let new_exp = curr_exp + exp;

            if new_exp > MAX_UNSIGNED_EXPONENT {
                INFINITY * x.signum()
            } else if new_exp > 0 {
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits)
            } else if new_exp >= -(MANTISSA_DIGITS as i32) {
                let new_exp = new_exp + MIN_SUBNORMAL_POWER;
                debug_assert!(new_exp >= 0);
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits) * 2f64.powi(-MIN_SUBNORMAL_POWER)
            } else {
                return 0.0 * x.signum();
            }
        }
    }

    pub mod traits
    {
        pub use num_traits::{ * };
        
        macro_rules! zero_impl
        {
            ($t:ty, $v:expr) =>
            {
                impl Zero for $t
                {
                    #[inline] fn zero() -> $t
                    {
                        $v
                    }
                    
                    #[inline] fn is_zero(&self) -> bool
                    {
                        *self == $v
                    }
                }

                impl ConstZero for $t
                {
                    const ZERO: Self = $v;
                }
            };
        }

        macro_rules! one_impl 
        {
            ($t:ty, $v:expr) => 
            {
                impl One for $t 
                {
                    #[inline] fn one() -> $t 
                    {
                        $v
                    }
                    #[inline] fn is_one(&self) -> bool 
                    {
                        *self == $v
                    }
                }

                impl ConstOne for $t 
                {
                    const ONE: Self = $v;
                }
            };
        }
        /// Defines an associated constant representing the additive identity element for `Self`.
        pub trait ConstZero: Zero
        {
            /// The additive identity element of `Self`, `0`.
            const ZERO: Self;
        }

        zero_impl!(usize, 0);
        zero_impl!(u8, 0);
        zero_impl!(u16, 0);
        zero_impl!(u32, 0);
        zero_impl!(u64, 0);
        zero_impl!(u128, 0);

        zero_impl!(isize, 0);
        zero_impl!(i8, 0);
        zero_impl!(i16, 0);
        zero_impl!(i32, 0);
        zero_impl!(i64, 0);
        zero_impl!(i128, 0);

        zero_impl!(f32, 0.0);
        zero_impl!(f64, 0.0);

        impl<T: Zero> Zero for Wrapping<T> where
        Wrapping<T>: Add<Output = Wrapping<T>>,
        {
            fn is_zero(&self) -> bool { self.0.is_zero() }
            fn set_zero(&mut self) { self.0.set_zero(); }
            fn zero() -> Self { Wrapping(T::zero()) }
        }

        impl<T: ConstZero> ConstZero for Wrapping<T> where
        Wrapping<T>: Add<Output = Wrapping<T>>,
        {
            const ZERO: Self = Wrapping(T::ZERO);
        }
        /// Defines an associated constant representing the multiplicative identity element for `Self`.
        pub trait ConstOne: One
        {
            /// The multiplicative identity element of `Self`, `1`.
            const ONE: Self;
        }

        one_impl!(usize, 1);
        one_impl!(u8, 1);
        one_impl!(u16, 1);
        one_impl!(u32, 1);
        one_impl!(u64, 1);
        one_impl!(u128, 1);

        one_impl!(isize, 1);
        one_impl!(i8, 1);
        one_impl!(i16, 1);
        one_impl!(i32, 1);
        one_impl!(i64, 1);
        one_impl!(i128, 1);

        one_impl!(f32, 1.0);
        one_impl!(f64, 1.0);

        impl<T: One> One for Wrapping<T> where
        Wrapping<T>: Mul<Output = Wrapping<T>>,
        {
            fn set_one(&mut self)
            {
                self.0.set_one();
            }

            fn one() -> Self {
                Wrapping(T::one())
            }
        }

        impl<T: ConstOne> ConstOne for Wrapping<T> where
        Wrapping<T>: Mul<Output = Wrapping<T>>,
        {
            const ONE: Self = Wrapping(T::ONE);
        }
    }
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod option
{
    pub use std::option::{ * };
}

pub mod parsers
{
    /*
    nom 7.1.3*/
    pub mod nom
    {
        //! # nom, eating data byte by byte
        #[macro_use] pub mod macros
        {
            use ::
            {
                *,
            };

            #[macro_export] macro_rules! succ 
            (
                (0, $submac:ident ! ($($rest:tt)*)) => ($submac!(1, $($rest)*));
                (1, $submac:ident ! ($($rest:tt)*)) => ($submac!(2, $($rest)*));
                (2, $submac:ident ! ($($rest:tt)*)) => ($submac!(3, $($rest)*));
                (3, $submac:ident ! ($($rest:tt)*)) => ($submac!(4, $($rest)*));
                (4, $submac:ident ! ($($rest:tt)*)) => ($submac!(5, $($rest)*));
                (5, $submac:ident ! ($($rest:tt)*)) => ($submac!(6, $($rest)*));
                (6, $submac:ident ! ($($rest:tt)*)) => ($submac!(7, $($rest)*));
                (7, $submac:ident ! ($($rest:tt)*)) => ($submac!(8, $($rest)*));
                (8, $submac:ident ! ($($rest:tt)*)) => ($submac!(9, $($rest)*));
                (9, $submac:ident ! ($($rest:tt)*)) => ($submac!(10, $($rest)*));
                (10, $submac:ident ! ($($rest:tt)*)) => ($submac!(11, $($rest)*));
                (11, $submac:ident ! ($($rest:tt)*)) => ($submac!(12, $($rest)*));
                (12, $submac:ident ! ($($rest:tt)*)) => ($submac!(13, $($rest)*));
                (13, $submac:ident ! ($($rest:tt)*)) => ($submac!(14, $($rest)*));
                (14, $submac:ident ! ($($rest:tt)*)) => ($submac!(15, $($rest)*));
                (15, $submac:ident ! ($($rest:tt)*)) => ($submac!(16, $($rest)*));
                (16, $submac:ident ! ($($rest:tt)*)) => ($submac!(17, $($rest)*));
                (17, $submac:ident ! ($($rest:tt)*)) => ($submac!(18, $($rest)*));
                (18, $submac:ident ! ($($rest:tt)*)) => ($submac!(19, $($rest)*));
                (19, $submac:ident ! ($($rest:tt)*)) => ($submac!(20, $($rest)*));
                (20, $submac:ident ! ($($rest:tt)*)) => ($submac!(21, $($rest)*));
            );
        }

        #[macro_use] pub mod error
        {
            //! Error management
            use ::
            {
                collections::{ HashMap },
                fmt::{ self, Write},
                hash::{ Hash, Hasher },
                parsers::nom::
                {
                    HexDisplay,
                    internal::Parser,
                    traits::Offset,
                },
                *,
            };
            /// Creates a parse error from a `nom::ErrorKind` and the position in the input
            #[macro_export(local_inner_macros)]
            macro_rules! error_position
            (
                ($input:expr, $code:expr) => 
                ({
                    ::parsers::nom::error::make_error($input, $code)
                });
            );
            /// Creates a parse error from a `nom::ErrorKind`, 
            /// the position in the input and the next error in the parsing tree.
            #[macro_export(local_inner_macros)] macro_rules! error_node_position
            (
                ($input:expr, $code:expr, $next:expr) => 
                ({
                    ::parsers::nom::error::append_error($input, $code, $next)
                });
            );
            /// This trait must be implemented by the error type of a nom parser.
            pub trait ParseError<I>: Sized 
            {
                /// Creates an error from the input position and an [ErrorKind]
                fn from_error_kind(input: I, kind: ErrorKind) -> Self;
                /// Combines an existing error with a new one created from the input position and an [ErrorKind].
                fn append(input: I, kind: ErrorKind, other: Self) -> Self;
                /// Creates an error from an input position and an expected character
                fn from_char(input: I, _: char) -> Self { Self::from_error_kind(input, ErrorKind::Char) }
                /// Combines two existing errors.
                fn or(self, other: Self) -> Self { other }
            }
            /// This trait is required by the `context` combinator to add a static string to an existing error.
            pub trait ContextError<I>: Sized 
            {
                /// Creates a new error from an input position, a static string and an existing error.
                fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self { other }
            }
            /// Required by the `map_res` combinator to integrate error types from external functions.
            pub trait FromExternalError<I, E>
            {
                /// Creates a new error from an input position, 
                /// an [ErrorKind] indicating the wrapping parser, and an external error
                fn from_external_error(input: I, kind: ErrorKind, e: E) -> Self;
            }
            /// default error type, only contains the error' location and code
            #[derive(Debug, PartialEq)]
            pub struct Error<I>
            {
                /// position of the error in the input data
                pub input: I,
                /// nom error code
                pub code: ErrorKind,
            }

            impl<I> Error<I>
            {
                /// creates a new basic error
                pub fn new(input: I, code: ErrorKind) -> Error<I> { Error { input, code } }
            }

            impl<I> ParseError<I> for Error<I>
            {
                fn from_error_kind(input: I, kind: ErrorKind) -> Self { Error { input, code: kind } }

                fn append(_: I, _: ErrorKind, other: Self) -> Self { other }
            }

            impl<I> ContextError<I> for Error<I> {}

            impl<I, E> FromExternalError<I, E> for Error<I>
            {
                /// Create a new error from an input position and an external error
                fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self
                {
                    Error { input, code: kind }
                }
            }
            /// The Display implementation allows the ::error::Error implementation
            impl<I: fmt::Display> fmt::Display for Error<I>
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {
                    write!(f, "error {:?} at: {}", self.code, self.input)
                }
            }
            
            impl<I: fmt::Debug + fmt::Display> ::error::Error for Error<I> {}
            
            impl<I> ParseError<I> for (I, ErrorKind)
            {
                fn from_error_kind(input: I, kind: ErrorKind) -> Self { (input, kind) }
                fn append(_: I, _: ErrorKind, other: Self) -> Self { other }
            }

            impl<I> ContextError<I> for (I, ErrorKind) {}

            impl<I, E> FromExternalError<I, E> for (I, ErrorKind)
            {
                fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self { (input, kind) }
            }

            impl<I> ParseError<I> for ()
            {
                fn from_error_kind(_: I, _: ErrorKind) -> Self {}
                fn append(_: I, _: ErrorKind, _: Self) -> Self {}
            }

            impl<I> ContextError<I> for () {}

            impl<I, E> FromExternalError<I, E> for ()
            {
                fn from_external_error(_input: I, _kind: ErrorKind, _e: E) -> Self {}
            }
            /// Creates an error from the input position and an [ErrorKind]
            pub fn make_error<I, E: ParseError<I>>(input: I, kind: ErrorKind) -> E
            {
                E::from_error_kind(input, kind)
            }
            /// Combines an existing error with a new one created from the input position and an [ErrorKind].
            pub fn append_error<I, E: ParseError<I>>(input: I, kind: ErrorKind, other: E) -> E
            {
                E::append(input, kind, other)
            }
            /// This error type accumulates errors and their position when backtracking through a parse tree.
            #[derive(Clone, Debug, PartialEq)]
            pub struct VerboseError<I> {
            /// List of errors accumulated by `VerboseError`, containing the affected
            /// part of input data, and some context
            pub errors: ::vec::Vec<(I, VerboseErrorKind)>,
            }
            /// Error context for `VerboseError`
            #[derive(Clone, Debug, PartialEq)]
            pub enum VerboseErrorKind
            {
                /// Static string added by the `context` function
                Context(&'static str),
                /// Indicates which character was expected by the `char` function
                Char(char),
                /// Error kind given by various nom parsers
                Nom(ErrorKind),
            }

            impl<I> ParseError<I> for VerboseError<I>
            {
                fn from_error_kind(input: I, kind: ErrorKind) -> Self
                {
                    VerboseError
                    {
                        errors: vec![(input, VerboseErrorKind::Nom(kind))],
                    }
                }

                fn append(input: I, kind: ErrorKind, mut other: Self) -> Self
                {
                    other.errors.push((input, VerboseErrorKind::Nom(kind)));
                    other
                }

                fn from_char(input: I, c: char) -> Self
                {
                    VerboseError
                    {
                        errors: vec![(input, VerboseErrorKind::Char(c))],
                    }
                }
            }

            impl<I> ContextError<I> for VerboseError<I>
            {
                fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self
                {
                    other.errors.push((input, VerboseErrorKind::Context(ctx)));
                    other
                }
            }

            impl<I, E> FromExternalError<I, E> for VerboseError<I>
            {
                /// Create a new error from an input position and an external error
                fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self { Self::from_error_kind(input, kind) }
            }

            impl<I: fmt::Display> fmt::Display for VerboseError<I>
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {
                    writeln!(f, "Parse error:")?;
                    for (input, error) in &self.errors
                    {
                        match error
                        {
                            VerboseErrorKind::Nom(e) => writeln!(f, "{:?} at: {}", e, input)?,
                            VerboseErrorKind::Char(c) => writeln!(f, "expected '{}' at: {}", c, input)?,
                            VerboseErrorKind::Context(s) => writeln!(f, "in section '{}', at: {}", s, input)?,
                        }
                    }

                    Ok(())
                }
            }
            
            impl<I: fmt::Debug + fmt::Display> ::error::Error for VerboseError<I> {}

            use super::internal::{Err, IResult};
            /// Create a new error from an input position, a static string and an existing error.
            pub fn context<I: Clone, E: ContextError<I>, F, O>( context: &'static str, mut f: F ) 
            -> impl FnMut(I) -> IResult<I, O, E> where
            F: Parser<I, O, E>
            {
                move |i: I| match f.parse(i.clone())
                {
                    Ok(o) => Ok(o),
                    Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                    Err(Err::Error(e)) => Err(Err::Error(E::add_context(i, context, e))),
                    Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(i, context, e))),
                }
            }
            /// Transforms a `VerboseError` into a trace with input position information
            pub fn convert_error<I: ::ops::Deref<Target = str>>( input: I, e: VerboseError<I> ) 
            -> ::string::String 
            {
                let mut result = ::string::String::new();

                for (i, (substring, kind)) in e.errors.iter().enumerate()
                {
                    let offset = input.offset(substring);

                    if input.is_empty()
                    {
                        match kind
                        {
                            VerboseErrorKind::Char(c) => { write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c) }
                            VerboseErrorKind::Context(s) => write!(&mut result, "{}: in {}, got empty input\n\n", i, s),
                            VerboseErrorKind::Nom(e) => write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e),
                        }
                    }
                    
                    else
                    {
                        let prefix = &input.as_bytes()[..offset];
                        let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;
                        let line_begin = prefix
                        .iter()
                        .rev()
                        .position(|&b| b == b'\n')
                        .map(|pos| offset - pos)
                        .unwrap_or(0);
                        
                        let line = input[line_begin..]
                        .lines()
                        .next()
                        .unwrap_or(&input[line_begin..])
                        .trim_end();
                        
                        let column_number = line.offset(substring) + 1;

                        match kind 
                        {
                            VerboseErrorKind::Char(c) =>
                            {
                                if let Some(actual) = substring.chars().next()
                                {
                                    write!
                                    (
                                        &mut result,
                                        "{i}: at line {line_number}:\n\
                                        {line}\n\
                                        {caret:>column$}\n\
                                        expected '{expected}', found {actual}\n\n",
                                        i = i,
                                        line_number = line_number,
                                        line = line,
                                        caret = '^',
                                        column = column_number,
                                        expected = c,
                                        actual = actual,
                                    )
                                }

                                else
                                {
                                    write!
                                    (
                                        &mut result,
                                        "{i}: at line {line_number}:\n\
                                        {line}\n\
                                        {caret:>column$}\n\
                                        expected '{expected}', got end of input\n\n",
                                        i = i,
                                        line_number = line_number,
                                        line = line,
                                        caret = '^',
                                        column = column_number,
                                        expected = c,
                                    )
                                }
                            }

                            VerboseErrorKind::Context(s) => write!
                            (
                                &mut result,
                                "{i}: at line {line_number}, in {context}:\n\
                                    {line}\n\
                                    {caret:>column$}\n\n",
                                i = i,
                                line_number = line_number,
                                context = s,
                                line = line,
                                caret = '^',
                                column = column_number,
                            ),

                            VerboseErrorKind::Nom(e) => write!
                            (
                                &mut result,
                                "{i}: at line {line_number}, in {nom_err:?}:\n\
                                    {line}\n\
                                    {caret:>column$}\n\n",
                                i = i,
                                line_number = line_number,
                                nom_err = e,
                                line = line,
                                caret = '^',
                                column = column_number,
                            ),
                        }
                    }.unwrap();
                }

                result
            }
            /// Indicates which parser returned an error
            #[derive( Debug,PartialEq,Eq,Hash,Clone,Copy )]
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
            /// Converts an ErrorKind to a number
            pub fn error_to_u32(e: &ErrorKind) -> u32 
            {
                match *e
                {
                    ErrorKind::Tag                       => 1,
                    ErrorKind::MapRes                    => 2,
                    ErrorKind::MapOpt                    => 3,
                    ErrorKind::Alt                       => 4,
                    ErrorKind::IsNot                     => 5,
                    ErrorKind::IsA                       => 6,
                    ErrorKind::SeparatedList             => 7,
                    ErrorKind::SeparatedNonEmptyList     => 8,
                    ErrorKind::Many1                     => 9,
                    ErrorKind::Count                     => 10,
                    ErrorKind::TakeUntil                 => 12,
                    ErrorKind::LengthValue               => 15,
                    ErrorKind::TagClosure                => 16,
                    ErrorKind::Alpha                     => 17,
                    ErrorKind::Digit                     => 18,
                    ErrorKind::AlphaNumeric              => 19,
                    ErrorKind::Space                     => 20,
                    ErrorKind::MultiSpace                => 21,
                    ErrorKind::LengthValueFn             => 22,
                    ErrorKind::Eof                       => 23,
                    ErrorKind::Switch                    => 27,
                    ErrorKind::TagBits                   => 28,
                    ErrorKind::OneOf                     => 29,
                    ErrorKind::NoneOf                    => 30,
                    ErrorKind::Char                      => 40,
                    ErrorKind::CrLf                      => 41,
                    ErrorKind::RegexpMatch               => 42,
                    ErrorKind::RegexpMatches             => 43,
                    ErrorKind::RegexpFind                => 44,
                    ErrorKind::RegexpCapture             => 45,
                    ErrorKind::RegexpCaptures            => 46,
                    ErrorKind::TakeWhile1                => 47,
                    ErrorKind::Complete                  => 48,
                    ErrorKind::Fix                       => 49,
                    ErrorKind::Escaped                   => 50,
                    ErrorKind::EscapedTransform          => 51,
                    ErrorKind::NonEmpty                  => 56,
                    ErrorKind::ManyMN                    => 57,
                    ErrorKind::HexDigit                  => 59,
                    ErrorKind::OctDigit                  => 61,
                    ErrorKind::Many0                     => 62,
                    ErrorKind::Not                       => 63,
                    ErrorKind::Permutation               => 64,
                    ErrorKind::ManyTill                  => 65,
                    ErrorKind::Verify                    => 66,
                    ErrorKind::TakeTill1                 => 67,
                    ErrorKind::TakeWhileMN               => 69,
                    ErrorKind::TooLarge                  => 70,
                    ErrorKind::Many0Count                => 71,
                    ErrorKind::Many1Count                => 72,
                    ErrorKind::Float                     => 73,
                    ErrorKind::Satisfy                   => 74,
                    ErrorKind::Fail                      => 75,
                }
            }

            impl ErrorKind 
            {
                /// Converts an ErrorKind to a text description
                pub fn description(&self) -> &str
                {
                    match *self
                    {
                        ErrorKind::Tag                       => "Tag",
                        ErrorKind::MapRes                    => "Map on Result",
                        ErrorKind::MapOpt                    => "Map on Option",
                        ErrorKind::Alt                       => "Alternative",
                        ErrorKind::IsNot                     => "IsNot",
                        ErrorKind::IsA                       => "IsA",
                        ErrorKind::SeparatedList             => "Separated list",
                        ErrorKind::SeparatedNonEmptyList     => "Separated non empty list",
                        ErrorKind::Many0                     => "Many0",
                        ErrorKind::Many1                     => "Many1",
                        ErrorKind::Count                     => "Count",
                        ErrorKind::TakeUntil                 => "Take until",
                        ErrorKind::LengthValue               => "Length followed by value",
                        ErrorKind::TagClosure                => "Tag closure",
                        ErrorKind::Alpha                     => "Alphabetic",
                        ErrorKind::Digit                     => "Digit",
                        ErrorKind::AlphaNumeric              => "AlphaNumeric",
                        ErrorKind::Space                     => "Space",
                        ErrorKind::MultiSpace                => "Multiple spaces",
                        ErrorKind::LengthValueFn             => "LengthValueFn",
                        ErrorKind::Eof                       => "End of file",
                        ErrorKind::Switch                    => "Switch",
                        ErrorKind::TagBits                   => "Tag on bitstream",
                        ErrorKind::OneOf                     => "OneOf",
                        ErrorKind::NoneOf                    => "NoneOf",
                        ErrorKind::Char                      => "Char",
                        ErrorKind::CrLf                      => "CrLf",
                        ErrorKind::RegexpMatch               => "RegexpMatch",
                        ErrorKind::RegexpMatches             => "RegexpMatches",
                        ErrorKind::RegexpFind                => "RegexpFind",
                        ErrorKind::RegexpCapture             => "RegexpCapture",
                        ErrorKind::RegexpCaptures            => "RegexpCaptures",
                        ErrorKind::TakeWhile1                => "TakeWhile1",
                        ErrorKind::Complete                  => "Complete",
                        ErrorKind::Fix                       => "Fix",
                        ErrorKind::Escaped                   => "Escaped",
                        ErrorKind::EscapedTransform          => "EscapedTransform",
                        ErrorKind::NonEmpty                  => "NonEmpty",
                        ErrorKind::ManyMN                    => "Many(m, n)",
                        ErrorKind::HexDigit                  => "Hexadecimal Digit",
                        ErrorKind::OctDigit                  => "Octal digit",
                        ErrorKind::Not                       => "Negation",
                        ErrorKind::Permutation               => "Permutation",
                        ErrorKind::ManyTill                  => "ManyTill",
                        ErrorKind::Verify                    => "predicate verification",
                        ErrorKind::TakeTill1                 => "TakeTill1",
                        ErrorKind::TakeWhileMN               => "TakeWhileMN",
                        ErrorKind::TooLarge                  => "Needed data size is too large",
                        ErrorKind::Many0Count                => "Count occurrence of >=0 patterns",
                        ErrorKind::Many1Count                => "Count occurrence of >=1 patterns",
                        ErrorKind::Float                     => "Float",
                        ErrorKind::Satisfy                   => "Satisfy",
                        ErrorKind::Fail                      => "Fail",
                    }
                }
            }
            /// Prints a message and the input if the parser fails.
            pub fn dbg_dmp<'a, F, O, E: ::fmt::Debug>( f:F, context:&'static str ) 
            -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O, E> where
            F: Fn(&'a [u8]) -> IResult<&'a [u8], O, E>,
            { 
                move |i: &'a [u8]| match f(i) 
                {
                    Err(e) => {
                    println!("{}: Error({:?}) at:\n{}", context, e, i.to_hex(8));
                    Err(e)
                    }
                    a => a,
                }
            }
        }

        pub mod branch
        {
            //! Choice combinators
            use ::
            {
                *,
            };
            use super::error::{ ErrorKind, ParseError };
            use super::internal::{Err, IResult, Parser};
            
            macro_rules! alt_trait
            (
                ($first:ident $second:ident $($id: ident)+) => (
                    alt_trait!(__impl $first $second; $($id)+);
                );
                (__impl $($current:ident)*; $head:ident $($id: ident)+) => (
                    alt_trait_impl!($($current)*);

                    alt_trait!(__impl $($current)* $head; $($id)+);
                );
                (__impl $($current:ident)*; $head:ident) => (
                    alt_trait_impl!($($current)*);
                    alt_trait_impl!($($current)* $head);
                );
            );

            macro_rules! alt_trait_impl
            (
                ($($id:ident)+) => (
                    impl<
                    Input: Clone, Output, Error: ParseError<Input>,
                    $($id: Parser<Input, Output, Error>),+
                    > Alt<Input, Output, Error> for ( $($id),+ ) {

                    fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
                        match self.0.parse(input.clone()) {
                        Err(Err::Error(e)) => alt_trait_inner!(1, self, input, e, $($id)+),
                        res => res,
                        }
                    }
                    }
                );
            );

            macro_rules! alt_trait_inner
            (
                ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => (
                    match $self.$it.parse($input.clone()) {
                    Err(Err::Error(e)) => {
                        let err = $err.or(e);
                        succ!($it, alt_trait_inner!($self, $input, err, $($id)+))
                    }
                    res => res,
                    }
                );
                ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident) => (
                    Err(Err::Error(Error::append($input, ErrorKind::Alt, $err)))
                );
            );
            
            macro_rules! permutation_trait
            (
                (
                    $name1:ident $ty1:ident $item1:ident
                    $name2:ident $ty2:ident $item2:ident
                    $($name3:ident $ty3:ident $item3:ident)*
                ) => (
                    permutation_trait!(__impl $name1 $ty1 $item1, $name2 $ty2 $item2; $($name3 $ty3 $item3)*);
                );
                (
                    __impl $($name:ident $ty:ident $item:ident),+;
                    $name1:ident $ty1:ident $item1:ident $($name2:ident $ty2:ident $item2:ident)*
                ) => (
                    permutation_trait_impl!($($name $ty $item),+);
                    permutation_trait!(__impl $($name $ty $item),+ , $name1 $ty1 $item1; $($name2 $ty2 $item2)*);
                );
                (__impl $($name:ident $ty:ident $item:ident),+;) => (
                    permutation_trait_impl!($($name $ty $item),+);
                );
            );

            macro_rules! permutation_trait_impl
            (
                ($($name:ident $ty:ident $item:ident),+) => (
                    impl<
                    Input: Clone, $($ty),+ , Error: ParseError<Input>,
                    $($name: Parser<Input, $ty, Error>),+
                    > Permutation<Input, ( $($ty),+ ), Error> for ( $($name),+ )
                    {

                        fn permutation(&mut self, mut input: Input) -> IResult<Input, ( $($ty),+ ), Error>
                        {
                            let mut res = ($(Option::<$ty>::None),+);

                            loop
                            {
                                let mut err: Option<Error> = None;
                                permutation_trait_inner!(0, self, input, res, err, $($name)+);
                                
                                if let Some(err) = err
                                {
                                    return Err(Err::Error(Error::append(input, ErrorKind::Permutation, err)));
                                }
                                
                                match res
                                {
                                    ($(Some($item)),+) => return Ok((input, ($($item),+))),
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }
                );
            );

            macro_rules! permutation_trait_inner
            (
                ($it:tt, $self:expr, $input:ident, $res:expr, $err:expr, $head:ident $($id:ident)*) => (
                    if $res.$it.is_none() {
                    match $self.$it.parse($input.clone()) {
                        Ok((i, o)) => {
                        $input = i;
                        $res.$it = Some(o);
                        continue;
                        }
                        Err(Err::Error(e)) => {
                        $err = Some(match $err {
                            Some(err) => err.or(e),
                            None => e,
                        });
                        }
                        Err(e) => return Err(e),
                    };
                    }
                    succ!($it, permutation_trait_inner!($self, $input, $res, $err, $($id)*));
                );
                ($it:tt, $self:expr, $input:ident, $res:expr, $err:expr,) => ();
            );
            /// Helper trait for the [alt()] combinator.
            pub trait Alt<I, O, E> 
            {
                /// Tests each parser in the tuple and returns the result of the first one that succeeds
                fn choice(&mut self, input: I) -> IResult<I, O, E>;
            }
            /// Tests a list of parsers one by one until one succeeds.
            pub fn alt<I: Clone, O, E: ParseError<I>, List: Alt<I, O, E>>(
            mut l: List,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            {
                move |i: I| l.choice(i)
            }
            /// Helper trait for the [permutation()] combinator.
            pub trait Permutation<I, O, E>
            {
                /// Tries to apply all parsers in the tuple in various orders until all of them succeed
                fn permutation(&mut self, input: I) -> IResult<I, O, E>;
            }
            /// Applies a list of parsers in any order.
            pub fn permutation<I: Clone, O, E: ParseError<I>, List: Permutation<I, O, E>>( mut l: List ) -> 
            impl FnMut(I) -> IResult<I, O, E>
            { move |i: I| l.permutation(i) }

            alt_trait!(A B C D E F G H I J K L M N O P Q R S T U);
            
            impl<Input, Output, Error: ParseError<Input>, A: Parser<Input, Output, Error>>
            Alt<Input, Output, Error> for (A,)
            {
                fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> { self.0.parse(input) }
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
        
        pub mod combinator
        {
            use ::
            {
                boxed::{ Box },
                borrow::{ Borrow },
                convert::{ Into },
                fmt::{ Debug },
                mem::{ transmute },
                ops::{Range, RangeFrom, RangeTo},
                parsers::nom::
                {
                    error::{ErrorKind, FromExternalError, ParseError},
                    internal::*,
                    traits::{AsChar, Compare, CompareResult, Offset, Slice, InputIter, InputLength, InputTakeAtPosition, ParseTo},
                },
                *,
            };
            /// Return the remaining input.
            #[inline] pub fn rest<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
            T: Slice<RangeFrom<usize>>,
            T: InputLength,
            {
                Ok((input.slice(input.input_len()..), input))
            }
            /// Return the length of the remaining input.
            #[inline] pub fn rest_len<T, E: ParseError<T>>(input: T) -> IResult<T, usize, E> where
            T: InputLength,
            {
                let len = input.input_len();
                Ok((input, len))
            }
            /// Maps a function on the result of a parser.
            pub fn map<I, O1, O2, E, F, G>(mut parser: F, mut f: G) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: FnMut(O1) -> O2,
            {
                move |input: I| {
                    let (input, o1) = parser.parse(input)?;
                    Ok((input, f(o1)))
                }
            }
            /// Applies a function returning a `Result` over the result of a parser.
            pub fn map_res<I: Clone, O1, O2, E: FromExternalError<I, E2>, E2, F, G>(
            mut parser: F,
            mut f: G,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: FnMut(O1) -> Result<O2, E2>,
            {
                move |input: I| {
                    let i = input.clone();
                    let (input, o1) = parser.parse(input)?;
                    match f(o1) {
                    Ok(o2) => Ok((input, o2)),
                    Err(e) => Err(Err::Error(E::from_external_error(i, ErrorKind::MapRes, e))),
                    }
                }
            }
            /// Applies a function returning an `Option` over the result of a parser.
            pub fn map_opt<I: Clone, O1, O2, E: ParseError<I>, F, G>(
            mut parser: F,
            mut f: G,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: FnMut(O1) -> Option<O2>,
            {
                move |input: I| {
                    let i = input.clone();
                    let (input, o1) = parser.parse(input)?;
                    match f(o1) {
                    Some(o2) => Ok((input, o2)),
                    None => Err(Err::Error(E::from_error_kind(i, ErrorKind::MapOpt))),
                    }
                }
            }
            /// Applies a parser over the result of another one.
            pub fn map_parser<I, O1, O2, E: ParseError<I>, F, G>(
            mut parser: F,
            mut applied_parser: G,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: Parser<O1, O2, E>,
            {
                move |input: I| {
                    let (input, o1) = parser.parse(input)?;
                    let (_, o2) = applied_parser.parse(o1)?;
                    Ok((input, o2))
                }
            }
            /// Creates a new parser from the output of the first parser,
            /// then apply that parser over the rest of the input.
            pub fn flat_map<I, O1, O2, E: ParseError<I>, F, G, H>(
            mut parser: F,
            mut applied_parser: G,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: FnMut(O1) -> H,
            H: Parser<I, O2, E>,
            {
                move |input: I| {
                    let (input, o1) = parser.parse(input)?;
                    applied_parser(o1).parse(input)
                }
            }
            /// Optional parser, will return `None` on [`Err::Error`].
            pub fn opt<I: Clone, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Option<O>, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match f.parse(input) {
                    Ok((i, o)) => Ok((i, Some(o))),
                    Err(Err::Error(_)) => Ok((i, None)),
                    Err(e) => Err(e),
                    }
                }
            }
            /// Calls the parser if the condition is met.
            pub fn cond<I, O, E: ParseError<I>, F>(
            b: bool,
            mut f: F,
            ) -> impl FnMut(I) -> IResult<I, Option<O>, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    if b {
                    match f.parse(input) {
                        Ok((i, o)) => Ok((i, Some(o))),
                        Err(e) => Err(e),
                    }
                    } else {
                    Ok((input, None))
                    }
                }
            }
            /// Tries to apply its parser without consuming the input.
            pub fn peek<I: Clone, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, O, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match f.parse(input) {
                    Ok((_, o)) => Ok((i, o)),
                    Err(e) => Err(e),
                    }
                }
            }
            /// returns its input if it is at the end of input data.
            pub fn eof<I: InputLength + Clone, E: ParseError<I>>( input:I ) -> IResult<I, I, E>
            {
                if input.input_len() == 0 {
                    let clone = input.clone();
                    Ok((input, clone))
                } else {
                    Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
                }
            }
            /// Transforms Incomplete into `Error`.
            pub fn complete<I: Clone, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, O, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match f.parse(input) {
                    Err(Err::Incomplete(_)) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Complete))),
                    rest => rest,
                    }
                }
            }
            /// Succeeds if all the input has been consumed by its child parser.
            pub fn all_consuming<I, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, O, E> where
            I: InputLength,
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let (input, res) = f.parse(input)?;
                    if input.input_len() == 0 {
                    Ok((input, res))
                    } else {
                    Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
                    }
                }
            }
            /// Returns the result of the child parser if it satisfies a verification function.
            pub fn verify<I: Clone, O1, O2, E: ParseError<I>, F, G>(
            mut first: F,
            second: G,
            ) -> impl FnMut(I) -> IResult<I, O1, E> where
            F: Parser<I, O1, E>,
            G: Fn(&O2) -> bool,
            O1: Borrow<O2>,
            O2: ?Sized,
            {
                move |input: I| {
                    let i = input.clone();
                    let (input, o) = first.parse(input)?;

                    if second(o.borrow()) {
                    Ok((input, o))
                    } else {
                    Err(Err::Error(E::from_error_kind(i, ErrorKind::Verify)))
                    }
                }
            }
            /// Returns the provided value if the child parser succeeds.
            pub fn value<I, O1: Clone, O2, E: ParseError<I>, F>(
            val: O1,
            mut parser: F,
            ) -> impl FnMut(I) -> IResult<I, O1, E> where
            F: Parser<I, O2, E>,
            {
                move |input: I| parser.parse(input).map(|(i, _)| (i, val.clone()))
            }
            /// Succeeds if the child parser returns an error.
            pub fn not<I: Clone, O, E: ParseError<I>, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, (), E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match parser.parse(input) {
                    Ok(_) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Not))),
                    Err(Err::Error(_)) => Ok((i, ())),
                    Err(e) => Err(e),
                    }
                }
            }
            /// If the child parser was successful, return the consumed input as produced value.
            pub fn recognize<I: Clone + Offset + Slice<RangeTo<usize>>, O, E: ParseError<I>, F>(
            mut parser: F,
            ) -> impl FnMut(I) -> IResult<I, I, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match parser.parse(i) {
                    Ok((i, _)) => {
                        let index = input.offset(&i);
                        Ok((i, input.slice(..index)))
                    }
                    Err(e) => Err(e),
                    }
                }
            }
            /// if the child parser was successful, return the consumed input with the output as a tuple.
            pub fn consumed<I, O, F, E>(mut parser: F) -> impl FnMut(I) -> IResult<I, (I, O), E> where
            I: Clone + Offset + Slice<RangeTo<usize>>,
            E: ParseError<I>,
            F: Parser<I, O, E>,
            {
                move |input: I| {
                    let i = input.clone();
                    match parser.parse(i) {
                    Ok((remaining, result)) => {
                        let index = input.offset(&remaining);
                        let consumed = input.slice(..index);
                        Ok((remaining, (consumed, result)))
                    }
                    Err(e) => Err(e),
                    }
                }
            }
            /// Transforms an [`Err::Error`] (recoverable) to [`Err::Failure`] (unrecoverable)
            pub fn cut<I, O, E: ParseError<I>, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O, E> where
            F: Parser<I, O, E>,
            {
                move |input: I| match parser.parse(input) {
                    Err(Err::Error(e)) => Err(Err::Failure(e)),
                    rest => rest,
                }
            }
            /// automatically converts the child parser's result to another type
            pub fn into<I, O1, O2, E1, E2, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O2, E2> where
            O1: Into<O2>,
            E1: Into<E2>,
            E1: ParseError<I>,
            E2: ParseError<I>,
            F: Parser<I, O1, E1>,
            {
                //map(parser, Into::into)
                move |input: I| match parser.parse(input) {
                    Ok((i, o)) => Ok((i, o.into())),
                    Err(Err::Error(e)) => Err(Err::Error(e.into())),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.into())),
                    Err(Err::Incomplete(e)) => Err(Err::Incomplete(e)),
                }
            }
            /// Creates an iterator from input data and a parser.
            pub fn iterator<Input, Output, Error, F>(input: Input, f: F) -> ParserIterator<Input, Error, F> where
            F: Parser<Input, Output, Error>,
            Error: ParseError<Input>,
            {
                ParserIterator {
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
                state: Option<State<E>>,
            }

            impl<I: Clone, E, F> ParserIterator<I, E, F> 
            {
                /// Returns the remaining input if parsing was successful, or the error if we encountered an error.
                pub fn finish(mut self) -> IResult<I, (), E> {
                    match self.state.take().unwrap() {
                    State::Running | State::Done => Ok((self.input, ())),
                    State::Failure(e) => Err(Err::Failure(e)),
                    State::Incomplete(i) => Err(Err::Incomplete(i)),
                    }
                }
            }

            impl<'a, Input, Output, Error, F> ::iter::Iterator for &'a mut ParserIterator<Input, Error, F> where
            F: FnMut(Input) -> IResult<Input, Output, Error>,
            Input: Clone,
            {
                type Item = Output;

                fn next(&mut self) -> Option<Self::Item> {
                    if let State::Running = self.state.take().unwrap() {
                    let input = self.input.clone();

                    match (self.iterator)(input) {
                        Ok((i, o)) => {
                        self.input = i;
                        self.state = Some(State::Running);
                        Some(o)
                        }
                        Err(Err::Error(_)) => {
                        self.state = Some(State::Done);
                        None
                        }
                        Err(Err::Failure(e)) => {
                        self.state = Some(State::Failure(e));
                        None
                        }
                        Err(Err::Incomplete(i)) => {
                        self.state = Some(State::Incomplete(i));
                        None
                        }
                    }
                    } else {
                    None
                    }
                }
            }

            enum State<E>
            {
                Running,
                Done,
                Failure(E),
                Incomplete(Needed),
            }
            /// a parser which always succeeds with given value without consuming any input.
            pub fn success<I, O: Clone, E: ParseError<I>>(val: O) -> impl Fn(I) -> IResult<I, O, E> 
            {
                move |input: I| Ok((input, val.clone()))
            }
            /// A parser which always fails.
            pub fn fail<I, O, E: ParseError<I>>(i: I) -> IResult<I, O, E>
            {
                Err(Err::Error(E::from_error_kind(i, ErrorKind::Fail)))
            }
        }

        pub mod internal
        {
            //! Basic types to build the parsers
            use ::
            {
                borrow::{ ToOwned },
                boxed::{ Box },
                marker::{ PhantomData, Sized },
                num::NonZeroUsize,
                parsers::nom::
                {
                    error::{ self, ErrorKind, ParseError }
                },
                string::{ String },
                vec::{ Vec },
                *,
            };
            /// Holds the result of parsing functions.
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
                    match self {
                    Ok(res) => Ok(res),
                    Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
                    Err(Err::Incomplete(_)) => {
                        panic!("Cannot call `finish()` on `Err(Err::Incomplete(_))`: this result means that the parser does not have enough data to decide, you should gather more data and try to reapply  the parser instead")
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
                    match NonZeroUsize::new(s) {
                    Some(sz) => Needed::Size(sz),
                    None => Needed::Unknown,
                    }
                }
                /// Indicates if we know how many bytes we need
                pub fn is_known(&self) -> bool { *self != Needed::Unknown }
                /// Maps a `Needed` to `Needed` by applying a function to a contained `Size` value.
                #[inline] pub fn map<F: Fn(NonZeroUsize) -> usize>(self, f: F) -> Needed
                {
                    match self 
                    {
                        Needed::Unknown => Needed::Unknown,
                        Needed::Size(n) => Needed::new(f(n)),
                    }
                }
            }
            /// The `Err` enum indicates the parser was not successful.
            #[derive(Debug, Clone, PartialEq)]
            pub enum Err<E>
            {
                /// There was not enough data
                Incomplete(Needed),
                /// The parser had an error (recoverable)
                Error(E),
                /// The parser had an unrecoverable error: we got to the right
                /// branch and we know other branches won't work, so backtrack
                /// as fast as possible
                Failure(E),
            }

            impl<E> Err<E> 
            {
                /// Tests if the result is Incomplete
                pub fn is_incomplete(&self) -> bool {
                    if let Err::Incomplete(_) = self {
                    true
                    } else {
                    false
                    }
                }

                /// Applies the given function to the inner error
                pub fn map<E2, F>(self, f: F) -> Err<E2> where
                    F: FnOnce(E) -> E2,
                {
                    match self {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure(t) => Err::Failure(f(t)),
                    Err::Error(t) => Err::Error(f(t)),
                    }
                }
                /// Automatically converts between errors if the underlying type supports it
                pub fn convert<F>(e: Err<F>) -> Self
                where
                    E: From<F>,
                {
                    e.map( ::convert::Into::into )
                }
            }

            impl<T> Err<(T, ErrorKind)>
            {
                /// Maps `Err<(T, ErrorKind)>` to `Err<(U, ErrorKind)>` with the given `F: T -> U`
                pub fn map_input<U, F>(self, f: F) -> Err<(U, ErrorKind)> where
                    F: FnOnce(T) -> U,
                {
                    match self {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure((input, k)) => Err::Failure((f(input), k)),
                    Err::Error((input, k)) => Err::Error((f(input), k)),
                    }
                }
            }

            impl<T> Err<error::Error<T>> 
            {
                /// Maps `Err<error::Error<T>>` to `Err<error::Error<U>>` with the given `F: T -> U`
                pub fn map_input<U, F>(self, f: F) -> Err<error::Error<U>> where
                    F: FnOnce(T) -> U,
                {
                    match self {
                    Err::Incomplete(n) => Err::Incomplete(n),
                    Err::Failure(error::Error { input, code }) => Err::Failure(error::Error {
                        input: f(input),
                        code,
                    }),
                    Err::Error(error::Error { input, code }) => Err::Error(error::Error {
                        input: f(input),
                        code,
                    }),
                    }
                }
            }
            
            impl Err<(&[u8], ErrorKind)>
            {
                /// Obtaining ownership
                pub fn to_owned(self) -> Err<(Vec<u8>, ErrorKind)> {
                    self.map_input(ToOwned::to_owned)
                }
            }
            
            impl Err<(&str, ErrorKind)>
            {
                /// Obtaining ownership
                pub fn to_owned(self) -> Err<(String, ErrorKind)> {
                    self.map_input(ToOwned::to_owned)
                }
            }
            
            impl Err<error::Error<&[u8]>>
            {
                /// Obtaining ownership
                pub fn to_owned(self) -> Err<error::Error<Vec<u8>>> {
                    self.map_input(ToOwned::to_owned)
                }
            }
            
            impl Err<error::Error<&str>>
            {
                /// Obtaining ownership
                pub fn to_owned(self) -> Err<error::Error<String>> {
                    self.map_input(ToOwned::to_owned)
                }
            }

            impl<E: Eq> Eq for Err<E> {}

            impl<E> fmt::Display for Err<E> where
            E: fmt::Debug,
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {
                    match self {
                    Err::Incomplete(Needed::Size(u)) => write!(f, "Parsing requires {} bytes/chars", u),
                    Err::Incomplete(Needed::Unknown) => write!(f, "Parsing requires more data"),
                    Err::Failure(c) => write!(f, "Parsing Failure: {:?}", c),
                    Err::Error(c) => write!(f, "Parsing Error: {:?}", c),
                    }
                }
            }
            
            use ::error::Error;
            
            impl<E> Error for Err<E> where
            E: fmt::Debug,
            {
                fn source(&self) -> Option<&(dyn Error + 'static)> {
                    None // no underlying error
                }
            }
            /// All nom parsers implement this trait
            pub trait Parser<I, O, E>
            {
                /// A parser takes in input type, 
                /// and returns a `Result` containing either the remaining input and the output value, or an error.
                fn parse(&mut self, input: I) -> IResult<I, O, E>;
                /// Maps a function over the result of a parser
                fn map<G, O2>(self, g: G) -> Map<Self, G, O> where
                    G: Fn(O) -> O2,
                    Self: Sized,
                {
                    Map {
                    f: self,
                    g,
                    phantom: PhantomData,
                    }
                }

                /// Creates a second parser from the output of the first one, then apply over the rest of the input
                fn flat_map<G, H, O2>(self, g: G) -> FlatMap<Self, G, O> where
                    G: FnMut(O) -> H,
                    H: Parser<I, O2, E>,
                    Self: Sized,
                {
                    FlatMap {
                    f: self,
                    g,
                    phantom: PhantomData,
                    }
                }

                /// Applies a second parser over the output of the first one
                fn and_then<G, O2>(self, g: G) -> AndThen<Self, G, O> where
                    G: Parser<O, O2, E>,
                    Self: Sized,
                {
                    AndThen {
                    f: self,
                    g,
                    phantom: PhantomData,
                    }
                }

                /// Applies a second parser after the first one, return their results as a tuple
                fn and<G, O2>(self, g: G) -> And<Self, G> where
                    G: Parser<I, O2, E>,
                    Self: Sized,
                {
                    And { f: self, g }
                }

                /// Applies a second parser over the input if the first one failed
                fn or<G>(self, g: G) -> Or<Self, G> where
                    G: Parser<I, O, E>,
                    Self: Sized,
                {
                    Or { f: self, g }
                }

                /// automatically converts the parser's output and error values to another type, as long as they
                /// implement the `From` trait
                fn into<O2: From<O>, E2: From<E>>(self) -> Into<Self, O, O2, E, E2> where
                    Self: Sized,
                {
                    Into {
                    f: self,
                    phantom_out1: PhantomData,
                    phantom_err1: PhantomData,
                    phantom_out2: PhantomData,
                    phantom_err2: PhantomData,
                    }
                }
            }

            impl<'a, I, O, E, F> Parser<I, O, E> for F
            where
            F: FnMut(I) -> IResult<I, O, E> + 'a,
            {
                fn parse(&mut self, i: I) -> IResult<I, O, E> {
                    self(i)
                }
            }
            
            impl<'a, I, O, E> Parser<I, O, E> for Box<dyn Parser<I, O, E> + 'a> 
            {
                fn parse(&mut self, input: I) -> IResult<I, O, E> {
                    (**self).parse(input)
                }
            }
            /// Implementation of `Parser::map`            
            pub struct Map<F, G, O1>
            {
                f: F,
                g: G,
                phantom: PhantomData<O1>,
            }

            impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Fn(O1) -> O2> Parser<I, O2, E> for Map<F, G, O1>
            {
                fn parse(&mut self, i: I) -> IResult<I, O2, E> {
                    match self.f.parse(i) {
                    Err(e) => Err(e),
                    Ok((i, o)) => Ok((i, (self.g)(o))),
                    }
                }
            }
            /// Implementation of `Parser::flat_map`
            pub struct FlatMap<F, G, O1> 
            {
                f: F,
                g: G,
                phantom: PhantomData<O1>,
            }

            impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Fn(O1) -> H, H: Parser<I, O2, E>> Parser<I, O2, E>
            for FlatMap<F, G, O1>
            {
                fn parse(&mut self, i: I) -> IResult<I, O2, E> {
                    let (i, o1) = self.f.parse(i)?;
                    (self.g)(o1).parse(i)
                }
            }
            /// Implementation of `Parser::and_then`            
            pub struct AndThen<F, G, O1> 
            {
                f: F,
                g: G,
                phantom: PhantomData<O1>,
            }

            impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Parser<O1, O2, E>> Parser<I, O2, E>
            for AndThen<F, G, O1>
            {
                fn parse(&mut self, i: I) -> IResult<I, O2, E> {
                    let (i, o1) = self.f.parse(i)?;
                    let (_, o2) = self.g.parse(o1)?;
                    Ok((i, o2))
                }
            }
            /// Implementation of `Parser::and`            
            pub struct And<F, G> 
            {
                f: F,
                g: G,
            }

            impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Parser<I, O2, E>> Parser<I, (O1, O2), E>
            for And<F, G>
            {
                fn parse(&mut self, i: I) -> IResult<I, (O1, O2), E> {
                    let (i, o1) = self.f.parse(i)?;
                    let (i, o2) = self.g.parse(i)?;
                    Ok((i, (o1, o2)))
                }
            }
            /// Implementation of `Parser::or`            
            pub struct Or<F, G> 
            {
                f: F,
                g: G,
            }

            impl<'a, I: Clone, O, E: ParseError<I>, F: Parser<I, O, E>, G: Parser<I, O, E>>
            Parser<I, O, E> for Or<F, G>
            {
                fn parse(&mut self, i: I) -> IResult<I, O, E> {
                    match self.f.parse(i.clone()) {
                    Err(Err::Error(e1)) => match self.g.parse(i) {
                        Err(Err::Error(e2)) => Err(Err::Error(e1.or(e2))),
                        res => res,
                    },
                    res => res,
                    }
                }
            }
            /// Implementation of `Parser::into`
            pub struct Into<F, O1, O2: From<O1>, E1, E2: From<E1>> 
            {
                f: F,
                phantom_out1: PhantomData<O1>,
                phantom_err1: PhantomData<E1>,
                phantom_out2: PhantomData<O2>,
                phantom_err2: PhantomData<E2>,
            }

            impl<
                'a,
                I: Clone,
                O1,
                O2: From<O1>,
                E1,
                E2: ParseError<I> + From<E1>,
                F: Parser<I, O1, E1>,
            > Parser<I, O2, E2> for Into<F, O1, O2, E1, E2>
            {
                fn parse(&mut self, i: I) -> IResult<I, O2, E2> 
                {
                    match self.f.parse(i) {
                    Ok((i, o)) => Ok((i, o.into())),
                    Err(Err::Error(e)) => Err(Err::Error(e.into())),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.into())),
                    Err(Err::Incomplete(e)) => Err(Err::Incomplete(e)),
                    }
                }
            }

        } pub use self::internal::*;
        
        pub mod multi
        {
            use ::
            {
                mem::{ size_of },
                num::{ NonZeroUsize },
                parsers::nom::
                {
                    error::{ ErrorKind, ParseError },
                    internal::{ Err, IResult, Needed, Parser },
                    traits::{ InputLength, InputTake, ToUsize },
                },
                vec::{ Vec },
                *,
            };
            /// Don't pre-allocate more than 64KiB when calling `Vec::with_capacity`.
            const MAX_INITIAL_CAPACITY_BYTES: usize = 65536;
            /// Repeats the embedded parser, gathering the results in a `Vec`.
            pub fn many0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |mut i: I| {
                    let mut acc = Vec::with_capacity(4);
                    loop {
                    let len = i.input_len();
                    match f.parse(i.clone()) {
                        Err(Err::Error(_)) => return Ok((i, acc)),
                        Err(e) => return Err(e),
                        Ok((i1, o)) => {
                        // infinite loop check: the parser must always consume
                        if i1.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                        }

                        i = i1;
                        acc.push(o);
                        }
                    }
                    }
                }
            }
            /// Runs the embedded parser, gathering the results in a `Vec`.
            pub fn many1<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |mut i: I| match f.parse(i.clone()) {
                    Err(Err::Error(err)) => Err(Err::Error(E::append(i, ErrorKind::Many1, err))),
                    Err(e) => Err(e),
                    Ok((i1, o)) => {
                    let mut acc = Vec::with_capacity(4);
                    acc.push(o);
                    i = i1;

                    loop {
                        let len = i.input_len();
                        match f.parse(i.clone()) {
                        Err(Err::Error(_)) => return Ok((i, acc)),
                        Err(e) => return Err(e),
                        Ok((i1, o)) => {
                            // infinite loop check: the parser must always consume
                            if i1.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                            }

                            i = i1;
                            acc.push(o);
                        }
                        }
                    }
                    }
                }
            }
            /// Applies the parser `f` until the parser `g` produces a result.
            pub fn many_till<I, O, P, E, F, G>(
            mut f: F,
            mut g: G,
            ) -> impl FnMut(I) -> IResult<I, (Vec<O>, P), E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: Parser<I, P, E>,
            E: ParseError<I>,
            {
                move |mut i: I| {
                    let mut res = Vec::new();
                    loop {
                    let len = i.input_len();
                    match g.parse(i.clone()) {
                        Ok((i1, o)) => return Ok((i1, (res, o))),
                        Err(Err::Error(_)) => {
                        match f.parse(i.clone()) {
                            Err(Err::Error(err)) => return Err(Err::Error(E::append(i, ErrorKind::ManyTill, err))),
                            Err(e) => return Err(e),
                            Ok((i1, o)) => {
                            // infinite loop check: the parser must always consume
                            if i1.input_len() == len {
                                return Err(Err::Error(E::from_error_kind(i1, ErrorKind::ManyTill)));
                            }

                            res.push(o);
                            i = i1;
                            }
                        }
                        }
                        Err(e) => return Err(e),
                    }
                    }
                }
            }
            /// Alternates between two parsers to produce a list of elements.
            pub fn separated_list0<I, O, O2, E, F, G>(
            mut sep: G,
            mut f: F,
            ) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: Parser<I, O2, E>,
            E: ParseError<I>,
            {
                move |mut i: I| {
                    let mut res = Vec::new();

                    match f.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i1, o)) => {
                        res.push(o);
                        i = i1;
                    }
                    }

                    loop {
                    let len = i.input_len();
                    match sep.parse(i.clone()) {
                        Err(Err::Error(_)) => return Ok((i, res)),
                        Err(e) => return Err(e),
                        Ok((i1, _)) => {
                        // infinite loop check: the parser must always consume
                        if i1.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(i1, ErrorKind::SeparatedList)));
                        }

                        match f.parse(i1.clone()) {
                            Err(Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                            res.push(o);
                            i = i2;
                            }
                        }
                        }
                    }
                    }
                }
            }
            /// Alternates between two parsers to produce a list of elements until [`Err::Error`].
            pub fn separated_list1<I, O, O2, E, F, G>(
            mut sep: G,
            mut f: F,
            ) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: Parser<I, O2, E>,
            E: ParseError<I>,
            {
                move |mut i: I| {
                    let mut res = Vec::new();

                    // Parse the first element
                    match f.parse(i.clone()) {
                    Err(e) => return Err(e),
                    Ok((i1, o)) => {
                        res.push(o);
                        i = i1;
                    }
                    }

                    loop {
                    let len = i.input_len();
                    match sep.parse(i.clone()) {
                        Err(Err::Error(_)) => return Ok((i, res)),
                        Err(e) => return Err(e),
                        Ok((i1, _)) => {
                        // infinite loop check: the parser must always consume
                        if i1.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(i1, ErrorKind::SeparatedList)));
                        }

                        match f.parse(i1.clone()) {
                            Err(Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                            res.push(o);
                            i = i2;
                            }
                        }
                        }
                    }
                    }
                }
            }
            /// Repeats the embedded parser `m..=n` times.
            pub fn many_m_n<I, O, E, F>(
            min: usize,
            max: usize,
            mut parse: F,
            ) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |mut input: I| {
                    if min > max {
                    return Err(Err::Failure(E::from_error_kind(input, ErrorKind::ManyMN)));
                    }

                    let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES / size_of::<O>().max(1);
                    let mut res = Vec::with_capacity(min.min(max_initial_capacity));
                    for count in 0..max {
                    let len = input.input_len();
                    match parse.parse(input.clone()) {
                        Ok((tail, value)) => {
                        // infinite loop check: the parser must always consume
                        if tail.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(input, ErrorKind::ManyMN)));
                        }

                        res.push(value);
                        input = tail;
                        }
                        Err(Err::Error(e)) => {
                        if count < min {
                            return Err(Err::Error(E::append(input, ErrorKind::ManyMN, e)));
                        } else {
                            return Ok((input, res));
                        }
                        }
                        Err(e) => {
                        return Err(e);
                        }
                    }
                    }

                    Ok((input, res))
                }
            }
            /// Repeats the embedded parser, counting the results.
            pub fn many0_count<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, usize, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let mut input = i;
                    let mut count = 0;

                    loop {
                    let input_ = input.clone();
                    let len = input.input_len();
                    match f.parse(input_) {
                        Ok((i, _)) => {
                        // infinite loop check: the parser must always consume
                        if i.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(input, ErrorKind::Many0Count)));
                        }

                        input = i;
                        count += 1;
                        }

                        Err(Err::Error(_)) => return Ok((input, count)),

                        Err(e) => return Err(e),
                    }
                    }
                }
            }
            /// Runs the embedded parser, counting the results.
            pub fn many1_count<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, usize, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let i_ = i.clone();
                    match f.parse(i_) {
                    Err(Err::Error(_)) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1Count))),
                    Err(i) => Err(i),
                    Ok((i1, _)) => {
                        let mut count = 1;
                        let mut input = i1;

                        loop {
                        let len = input.input_len();
                        let input_ = input.clone();
                        match f.parse(input_) {
                            Err(Err::Error(_)) => return Ok((input, count)),
                            Err(e) => return Err(e),
                            Ok((i, _)) => {
                            // infinite loop check: the parser must always consume
                            if i.input_len() == len {
                                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1Count)));
                            }

                            count += 1;
                            input = i;
                            }
                        }
                        }
                    }
                    }
                }
            }
            /// Runs the embedded parser `count` times, gathering the results in a `Vec`.
            pub fn count<I, O, E, F>(mut f: F, count: usize) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone + PartialEq,
            F: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let mut input = i.clone();
                    let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES / size_of::<O>().max(1);
                    let mut res = Vec::with_capacity(count.min(max_initial_capacity));

                    for _ in 0..count {
                    let input_ = input.clone();
                    match f.parse(input_) {
                        Ok((i, o)) => {
                        res.push(o);
                        input = i;
                        }
                        Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                        }
                        Err(e) => {
                        return Err(e);
                        }
                    }
                    }

                    Ok((input, res))
                }
            }
            /// Runs the embedded parser repeatedly, filling the given slice with results.
            pub fn fill<'a, I, O, E, F>(f: F, buf: &'a mut [O]) -> impl FnMut(I) -> IResult<I, (), E> + 'a where
            I: Clone + PartialEq,
            F: Fn(I) -> IResult<I, O, E> + 'a,
            E: ParseError<I>,
            {
                move |i: I| {
                    let mut input = i.clone();

                    for elem in buf.iter_mut() {
                    let input_ = input.clone();
                    match f(input_) {
                        Ok((i, o)) => {
                        *elem = o;
                        input = i;
                        }
                        Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                        }
                        Err(e) => {
                        return Err(e);
                        }
                    }
                    }

                    Ok((input, ()))
                }
            }
            /// Repeats the embedded parser, calling `g` to gather the results.
            pub fn fold_many0<I, O, E, F, G, H, R>(
            mut f: F,
            mut init: H,
            mut g: G,
            ) -> impl FnMut(I) -> IResult<I, R, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: FnMut(R, O) -> R,
            H: FnMut() -> R,
            E: ParseError<I>,
            {
                move |i: I| {
                    let mut res = init();
                    let mut input = i;

                    loop {
                    let i_ = input.clone();
                    let len = input.input_len();
                    match f.parse(i_) {
                        Ok((i, o)) => {
                        // infinite loop check: the parser must always consume
                        if i.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(input, ErrorKind::Many0)));
                        }

                        res = g(res, o);
                        input = i;
                        }
                        Err(Err::Error(_)) => {
                        return Ok((input, res));
                        }
                        Err(e) => {
                        return Err(e);
                        }
                    }
                    }
                }
            }
            /// Repeats the embedded parser, calling `g` to gather the results.
            pub fn fold_many1<I, O, E, F, G, H, R>(
            mut f: F,
            mut init: H,
            mut g: G,
            ) -> impl FnMut(I) -> IResult<I, R, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: FnMut(R, O) -> R,
            H: FnMut() -> R,
            E: ParseError<I>,
            {
                move |i: I| {
                    let _i = i.clone();
                    let init = init();
                    match f.parse(_i) {
                    Err(Err::Error(_)) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1))),
                    Err(e) => Err(e),
                    Ok((i1, o1)) => {
                        let mut acc = g(init, o1);
                        let mut input = i1;

                        loop {
                        let _input = input.clone();
                        let len = input.input_len();
                        match f.parse(_input) {
                            Err(Err::Error(_)) => {
                            break;
                            }
                            Err(e) => return Err(e),
                            Ok((i, o)) => {
                            // infinite loop check: the parser must always consume
                            if i.input_len() == len {
                                return Err(Err::Failure(E::from_error_kind(i, ErrorKind::Many1)));
                            }

                            acc = g(acc, o);
                            input = i;
                            }
                        }
                        }

                        Ok((input, acc))
                    }
                    }
                }
            }
            /// Repeats the embedded parser `m..=n` times, calling `g` to gather the results.
            pub fn fold_many_m_n<I, O, E, F, G, H, R>(
            min: usize,
            max: usize,
            mut parse: F,
            mut init: H,
            mut fold: G,
            ) -> impl FnMut(I) -> IResult<I, R, E> where
            I: Clone + InputLength,
            F: Parser<I, O, E>,
            G: FnMut(R, O) -> R,
            H: FnMut() -> R,
            E: ParseError<I>,
            {
                move |mut input: I| {
                    if min > max {
                    return Err(Err::Failure(E::from_error_kind(input, ErrorKind::ManyMN)));
                    }

                    let mut acc = init();
                    for count in 0..max {
                    let len = input.input_len();
                    match parse.parse(input.clone()) {
                        Ok((tail, value)) => {
                        if tail.input_len() == len {
                            return Err(Err::Error(E::from_error_kind(tail, ErrorKind::ManyMN)));
                        }

                        acc = fold(acc, value);
                        input = tail;
                        }
                        Err(Err::Error(err)) => {
                        if count < min {
                            return Err(Err::Error(E::append(input, ErrorKind::ManyMN, err)));
                        } else {
                            break;
                        }
                        }
                        Err(e) => return Err(e),
                    }
                    }

                    Ok((input, acc))
                }
            }
            /// Gets a number from the parser and returns a subslice of the input of that size.
            pub fn length_data<I, N, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, I, E> where
            I: InputLength + InputTake,
            N: ToUsize,
            F: Parser<I, N, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let (i, length) = f.parse(i)?;

                    let length: usize = length.to_usize();

                    if let Some(needed) = length
                    .checked_sub(i.input_len())
                    .and_then(NonZeroUsize::new)
                    {
                    Err(Err::Incomplete(Needed::Size(needed)))
                    } else {
                    Ok(i.take_split(length))
                    }
                }
            }
            /// Gets a number from the first parser, takes a subslice of the input of that size, 
            /// then applies the second parser on that subslice.
            pub fn length_value<I, O, N, E, F, G>(mut f: F, mut g: G) -> impl FnMut(I) -> IResult<I, O, E> where
            I: Clone + InputLength + InputTake,
            N: ToUsize,
            F: Parser<I, N, E>,
            G: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let (i, length) = f.parse(i)?;

                    let length: usize = length.to_usize();

                    if let Some(needed) = length
                    .checked_sub(i.input_len())
                    .and_then(NonZeroUsize::new)
                    {
                    Err(Err::Incomplete(Needed::Size(needed)))
                    } else {
                    let (rest, i) = i.take_split(length);
                    match g.parse(i.clone()) {
                        Err(Err::Incomplete(_)) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Complete))),
                        Err(e) => Err(e),
                        Ok((_, o)) => Ok((rest, o)),
                    }
                    }
                }
            }
            /// Gets a number from the first parser, then applies the second parser that many times.
            pub fn length_count<I, O, N, E, F, G>(mut f: F, mut g: G) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
            I: Clone,
            N: ToUsize,
            F: Parser<I, N, E>,
            G: Parser<I, O, E>,
            E: ParseError<I>,
            {
                move |i: I| {
                    let (i, count) = f.parse(i)?;
                    let mut input = i.clone();
                    let mut res = Vec::new();

                    for _ in 0..count.to_usize() {
                    let input_ = input.clone();
                    match g.parse(input_) {
                        Ok((i, o)) => {
                        res.push(o);
                        input = i;
                        }
                        Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                        }
                        Err(e) => {
                        return Err(e);
                        }
                    }
                    }

                    Ok((input, res))
                }
            }
        }
        
        pub mod sequence
        {
            //! Combinators applying parsers in sequence
            use ::
            {                
                parsers::nom::
                {
                    error::ParseError,
                    internal::{IResult, Parser},
                },
                *,
            };            

            macro_rules! tuple_trait
            (
                ($name1:ident $ty1:ident, $name2: ident $ty2:ident, $($name:ident $ty:ident),*) => (
                    tuple_trait!(__impl $name1 $ty1, $name2 $ty2; $($name $ty),*);
                );
                (__impl $($name:ident $ty: ident),+; $name1:ident $ty1:ident, $($name2:ident $ty2:ident),*) => (
                    tuple_trait_impl!($($name $ty),+);
                    tuple_trait!(__impl $($name $ty),+ , $name1 $ty1; $($name2 $ty2),*);
                );
                (__impl $($name:ident $ty: ident),+; $name1:ident $ty1:ident) => (
                    tuple_trait_impl!($($name $ty),+);
                    tuple_trait_impl!($($name $ty),+, $name1 $ty1);
                );
            );

            macro_rules! tuple_trait_impl
            (
                ($($name:ident $ty: ident),+) => (
                    impl<
                    Input: Clone, $($ty),+ , Error: ParseError<Input>,
                    $($name: Parser<Input, $ty, Error>),+
                    > Tuple<Input, ( $($ty),+ ), Error> for ( $($name),+ ) {

                    fn parse(&mut self, input: Input) -> IResult<Input, ( $($ty),+ ), Error> {
                        tuple_trait_inner!(0, self, input, (), $($name)+)

                    }
                    }
                );
            );

            macro_rules! tuple_trait_inner
            (
                ($it:tt, $self:expr, $input:expr, (), $head:ident $($id:ident)+) => ({
                    let (i, o) = $self.$it.parse($input.clone())?;

                    succ!($it, tuple_trait_inner!($self, i, ( o ), $($id)+))
                });
                ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident $($id:ident)+) => ({
                    let (i, o) = $self.$it.parse($input.clone())?;

                    succ!($it, tuple_trait_inner!($self, i, ($($parsed)* , o), $($id)+))
                });
                ($it:tt, $self:expr, $input:expr, ($($parsed:tt)*), $head:ident) => ({
                    let (i, o) = $self.$it.parse($input.clone())?;

                    Ok((i, ($($parsed)* , o)))
                });
            );
            /// Gets an object from the first parser, then gets another object from the second parser.
            pub fn pair<I, O1, O2, E: ParseError<I>, F, G>(
            mut first: F,
            mut second: G,
            ) -> impl FnMut(I) -> IResult<I, (O1, O2), E> where
            F: Parser<I, O1, E>,
            G: Parser<I, O2, E>,
            {
                move |input: I| {
                    let (input, o1) = first.parse(input)?;
                    second.parse(input).map(|(i, o2)| (i, (o1, o2)))
                }
            }
            /// Matches an object from the first parser and discards it,
            /// then gets an object from the second parser.
            pub fn preceded<I, O1, O2, E: ParseError<I>, F, G>(
            mut first: F,
            mut second: G,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: Parser<I, O2, E>,
            {
                move |input: I| {
                    let (input, _) = first.parse(input)?;
                    second.parse(input)
                }
            }
            /// Gets an object from the first parser,
            /// then matches an object from the second parser and discards it.
            pub fn terminated<I, O1, O2, E: ParseError<I>, F, G>(
            mut first: F,
            mut second: G,
            ) -> impl FnMut(I) -> IResult<I, O1, E> where
            F: Parser<I, O1, E>,
            G: Parser<I, O2, E>,
            {
                move |input: I| {
                    let (input, o1) = first.parse(input)?;
                    second.parse(input).map(|(i, _)| (i, o1))
                }
            }
            /// Gets an object from the first parser,
            /// then matches an object from the sep_parser and discards it,
            /// then gets another object from the second parser.
            pub fn separated_pair<I, O1, O2, O3, E: ParseError<I>, F, G, H>(
            mut first: F,
            mut sep: G,
            mut second: H,
            ) -> impl FnMut(I) -> IResult<I, (O1, O3), E> where
            F: Parser<I, O1, E>,
            G: Parser<I, O2, E>,
            H: Parser<I, O3, E>,
            {
                move |input: I| {
                    let (input, o1) = first.parse(input)?;
                    let (input, _) = sep.parse(input)?;
                    second.parse(input).map(|(i, o2)| (i, (o1, o2)))
                }
            }
            /// Matches an object from the first parser and discards it,
            /// then gets an object from the second parser,
            /// and finally matches an object from the third parser and discards it.
            pub fn delimited<I, O1, O2, O3, E: ParseError<I>, F, G, H>(
            mut first: F,
            mut second: G,
            mut third: H,
            ) -> impl FnMut(I) -> IResult<I, O2, E> where
            F: Parser<I, O1, E>,
            G: Parser<I, O2, E>,
            H: Parser<I, O3, E>,
            {
                move |input: I| {
                    let (input, _) = first.parse(input)?;
                    let (input, o2) = second.parse(input)?;
                    third.parse(input).map(|(i, _)| (i, o2))
                }
            }
            /// Helper trait for the tuple combinator.
            pub trait Tuple<I, O, E>
            {
                /// Parses the input and returns a tuple of results of each parser.
                fn parse(&mut self, input: I) -> IResult<I, O, E>;
            }

            impl<Input, Output, Error: ParseError<Input>, F: Parser<Input, Output, Error>>
            Tuple<Input, (Output,), Error> for (F,)
            {
                fn parse(&mut self, input: Input) -> IResult<Input, (Output,), Error>
                {
                    self.0.parse(input).map(|(i, o)| (i, (o,)))
                }
            }

            tuple_trait!(FnA A, FnB B, FnC C, FnD D, FnE E, FnF F, FnG G, FnH H, FnI I, FnJ J, FnK K, FnL L,
            FnM M, FnN N, FnO O, FnP P, FnQ Q, FnR R, FnS S, FnT T, FnU U);
            
            impl<I, E: ParseError<I>> Tuple<I, (), E> for () 
            {
                fn parse(&mut self, input: I) -> IResult<I, (), E> {
                    Ok((input, ()))
                }
            }
            /// Applies a tuple of parsers one by one and returns their results as a tuple.
            pub fn tuple<I, O, E: ParseError<I>, List: Tuple<I, O, E>>(
            mut l: List,
            ) -> impl FnMut(I) -> IResult<I, O, E> 
            {
                move |i: I| l.parse(i)
            }
        }
        
        pub mod traits
        {
            //! Traits input types have to implement to work with nom combinators            
            use ::
            {
                iter::{Copied, Enumerate},
                ops::{Range, RangeFrom, RangeFull, RangeTo},
                parsers::nom::
                {
                    error::{ self, ErrorKind, ParseError},
                    internal::{Err, IResult, Needed},
                },
                slice::Iter,
                str::{ Chars, CharIndices, FromStr, from_utf8, },
                string::{ String },
                vec::{ Vec },
                *,
            };
            
            macro_rules! as_bytes_array_impls 
            {
                ($($N:expr)+) => {
                    $(
                    impl<'a> AsBytes for &'a [u8; $N] 
                    {
                        #[inline(always)]
                        fn as_bytes(&self) -> &[u8] 
                        {
                        *self
                        }
                    }

                    impl AsBytes for [u8; $N] 
                    {
                        #[inline(always)]
                        fn as_bytes(&self) -> &[u8] 
                        {
                        self
                        }
                    }
                    )+
                };
            }
            
            macro_rules! impl_fn_slice
            {
                ( $ty:ty ) => {
                    fn slice(&self, range: $ty) -> Self {
                    &self[range]
                    }
                };
            }

            macro_rules! slice_range_impl
            {
                ( [ $for_type:ident ], $ty:ty ) => {
                    impl<'a, $for_type> Slice<$ty> for &'a [$for_type] {
                    impl_fn_slice!($ty);
                    }
                };
                ( $for_type:ty, $ty:ty ) => {
                    impl<'a> Slice<$ty> for &'a $for_type {
                    impl_fn_slice!($ty);
                    }
                };
            }

            macro_rules! slice_ranges_impl
            {
                ( [ $for_type:ident ] ) => {
                    slice_range_impl! {[$for_type], Range<usize>}
                    slice_range_impl! {[$for_type], RangeTo<usize>}
                    slice_range_impl! {[$for_type], RangeFrom<usize>}
                    slice_range_impl! {[$for_type], RangeFull}
                };
                ( $for_type:ty ) => {
                    slice_range_impl! {$for_type, Range<usize>}
                    slice_range_impl! {$for_type, RangeTo<usize>}
                    slice_range_impl! {$for_type, RangeFrom<usize>}
                    slice_range_impl! {$for_type, RangeFull}
                };
            }

            macro_rules! array_impls
            {
                ($($N:expr)+) => {
                    $(
                    impl InputLength for [u8; $N] {
                        #[inline]
                        fn input_len(&self) -> usize {
                        self.len()
                        }
                    }

                    impl<'a> InputLength for &'a [u8; $N] {
                        #[inline]
                        fn input_len(&self) -> usize {
                        self.len()
                        }
                    }

                    impl<'a> InputIter for &'a [u8; $N] {
                        type Item = u8;
                        type Iter = Enumerate<Self::IterElem>;
                        type IterElem = Copied<Iter<'a, u8>>;

                        fn iter_indices(&self) -> Self::Iter {
                        (&self[..]).iter_indices()
                        }

                        fn iter_elements(&self) -> Self::IterElem {
                        (&self[..]).iter_elements()
                        }

                        fn position<P>(&self, predicate: P) -> Option<usize>
                        where P: Fn(Self::Item) -> bool {
                        (&self[..]).position(predicate)
                        }

                        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
                        (&self[..]).slice_index(count)
                        }
                    }

                    impl<'a> Compare<[u8; $N]> for &'a [u8] {
                        #[inline(always)]
                        fn compare(&self, t: [u8; $N]) -> CompareResult {
                        self.compare(&t[..])
                        }

                        #[inline(always)]
                        fn compare_no_case(&self, t: [u8;$N]) -> CompareResult {
                        self.compare_no_case(&t[..])
                        }
                    }

                    impl<'a,'b> Compare<&'b [u8; $N]> for &'a [u8] {
                        #[inline(always)]
                        fn compare(&self, t: &'b [u8; $N]) -> CompareResult {
                        self.compare(&t[..])
                        }

                        #[inline(always)]
                        fn compare_no_case(&self, t: &'b [u8;$N]) -> CompareResult {
                        self.compare_no_case(&t[..])
                        }
                    }

                    impl FindToken<u8> for [u8; $N] {
                        fn find_token(&self, token: u8) -> bool {
                        memchr::memchr(token, &self[..]).is_some()
                        }
                    }

                    impl<'a> FindToken<&'a u8> for [u8; $N] {
                        fn find_token(&self, token: &u8) -> bool {
                        self.find_token(*token)
                        }
                    }
                    )+
                };
            }
            /// Abstract method to calculate the input length
            pub trait InputLength 
            {
                /// Calculates the input length, as indicated by its name,
                /// and the name of the trait itself
                fn input_len(&self) -> usize;
            }

            impl<'a, T> InputLength for &'a [T] 
            {
                #[inline] fn input_len(&self) -> usize 
                {
                    self.len()
                }
            }

            impl<'a> InputLength for &'a str 
            {
                #[inline] fn input_len(&self) -> usize 
                {
                    self.len()
                }
            }

            impl<'a> InputLength for (&'a [u8], usize) 
            {
                #[inline] fn input_len(&self) -> usize 
                {
                    //println!("bit input length for ({:?}, {}):", self.0, self.1);
                    //println!("-> {}", self.0.len() * 8 - self.1);
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
                fn offset(&self, second: &Self) -> usize {
                    let fst = self.as_ptr();
                    let snd = second.as_ptr();

                    snd as usize - fst as usize
                }
            }

            impl<'a> Offset for &'a [u8]
            {
                fn offset(&self, second: &Self) -> usize {
                    let fst = self.as_ptr();
                    let snd = second.as_ptr();

                    snd as usize - fst as usize
                }
            }

            impl Offset for str
            {
                fn offset(&self, second: &Self) -> usize {
                    let fst = self.as_ptr();
                    let snd = second.as_ptr();

                    snd as usize - fst as usize
                }
            }

            impl<'a> Offset for &'a str
            {
                fn offset(&self, second: &Self) -> usize {
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

            impl<'a> AsBytes for &'a str
            {
                #[inline(always)]
                fn as_bytes(&self) -> &[u8] {
                    (*self).as_bytes()
                }
            }

            impl AsBytes for str 
            {
                #[inline(always)]
                fn as_bytes(&self) -> &[u8]  { self.as_ref() }
            }

            impl<'a> AsBytes for &'a [u8] 
            {
                #[inline(always)]
                fn as_bytes(&self) -> &[u8] 
                {
                    *self
                }
            }

            impl AsBytes for [u8] 
            {
                #[inline(always)]
                fn as_bytes(&self) -> &[u8] 
                {
                    self
                }
            }

            as_bytes_array_impls!
            {
                0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
            }
            /// Transforms common types to a char for basic token parsing
            pub trait AsChar 
            {
                /// makes a char from self
                fn as_char(self) -> char;

                /// Tests that self is an alphabetic character
                ///
                /// Warning: for `&str` it recognizes alphabetic
                /// characters outside of the 52 ASCII letters
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
                #[inline] fn as_char(self) -> char {
                    self as char
                }
                #[inline] fn is_alpha(self) -> bool {
                    (self >= 0x41 && self <= 0x5A) || (self >= 0x61 && self <= 0x7A)
                }
                #[inline] fn is_alphanum(self) -> bool {
                    self.is_alpha() || self.is_dec_digit()
                }
                #[inline] fn is_dec_digit(self) -> bool {
                    self >= 0x30 && self <= 0x39
                }
                #[inline] fn is_hex_digit(self) -> bool {
                    (self >= 0x30 && self <= 0x39)
                    || (self >= 0x41 && self <= 0x46)
                    || (self >= 0x61 && self <= 0x66)
                }
                #[inline] fn is_oct_digit(self) -> bool {
                    self >= 0x30 && self <= 0x37
                }
                #[inline] fn len(self) -> usize {
                    1
                }
            }

            impl<'a> AsChar for &'a u8
            {
                #[inline] fn as_char(self) -> char {
                    *self as char
                }
                #[inline] fn is_alpha(self) -> bool {
                    (*self >= 0x41 && *self <= 0x5A) || (*self >= 0x61 && *self <= 0x7A)
                }
                #[inline] fn is_alphanum(self) -> bool {
                    self.is_alpha() || self.is_dec_digit()
                }
                #[inline] fn is_dec_digit(self) -> bool {
                    *self >= 0x30 && *self <= 0x39
                }
                #[inline] fn is_hex_digit(self) -> bool {
                    (*self >= 0x30 && *self <= 0x39)
                    || (*self >= 0x41 && *self <= 0x46)
                    || (*self >= 0x61 && *self <= 0x66)
                }
                #[inline] fn is_oct_digit(self) -> bool {
                    *self >= 0x30 && *self <= 0x37
                }
                #[inline] fn len(self) -> usize {
                    1
                }
            }

            impl AsChar for char
            {
                #[inline] fn as_char(self) -> char {
                    self
                }
                #[inline] fn is_alpha(self) -> bool {
                    self.is_ascii_alphabetic()
                }
                #[inline] fn is_alphanum(self) -> bool {
                    self.is_alpha() || self.is_dec_digit()
                }
                #[inline] fn is_dec_digit(self) -> bool {
                    self.is_ascii_digit()
                }
                #[inline] fn is_hex_digit(self) -> bool {
                    self.is_ascii_hexdigit()
                }
                #[inline] fn is_oct_digit(self) -> bool {
                    self.is_digit(8)
                }
                #[inline] fn len(self) -> usize {
                    self.len_utf8()
                }
            }

            impl<'a> AsChar for &'a char
            {
                #[inline] fn as_char(self) -> char 
                {
                    *self
                }

                #[inline] fn is_alpha(self) -> bool 
                {
                    self.is_ascii_alphabetic()
                }

                #[inline] fn is_alphanum(self) -> bool 
                {
                    self.is_alpha() || self.is_dec_digit()
                }

                #[inline] fn is_dec_digit(self) -> bool 
                {
                    self.is_ascii_digit()
                }

                #[inline] fn is_hex_digit(self) -> bool 
                {
                    self.is_ascii_hexdigit()
                }

                #[inline] fn is_oct_digit(self) -> bool 
                {
                    self.is_digit(8)
                }

                #[inline] fn len(self) -> usize 
                {
                    self.len_utf8()
                }

            }
            /// Abstracts common iteration operations on the input type
            pub trait InputIter 
            {
                /// The current input type is a sequence of that `Item` type.
                ///
                /// Example: `u8` for `&[u8]` or `char` for `&str`
                type Item;
                /// An iterator over the input type, producing the item and its position
                /// for use with [Slice]. If we're iterating over `&str`, the position
                /// corresponds to the byte index of the character
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

                #[inline] fn iter_indices(&self) -> Self::Iter {
                    self.iter_elements().enumerate()
                }
                #[inline] fn iter_elements(&self) -> Self::IterElem {
                    self.iter().copied()
                }
                #[inline] fn position<P>(&self, predicate: P) -> Option<usize> where
                    P: Fn(Self::Item) -> bool,
                {
                    self.iter().position(|b| predicate(*b))
                }
                #[inline] fn slice_index(&self, count: usize) -> Result<usize, Needed> {
                    if self.len() >= count {
                    Ok(count)
                    } else {
                    Err(Needed::new(count - self.len()))
                    }
                }
            }

            impl<'a> InputTake for &'a [u8]
            {
                #[inline] fn take(&self, count: usize) -> Self {
                    &self[0..count]
                }
                #[inline] fn take_split(&self, count: usize) -> (Self, Self) {
                    let (prefix, suffix) = self.split_at(count);
                    (suffix, prefix)
                }
            }

            impl<'a> InputIter for &'a str
            {
                type Item = char;
                type Iter = CharIndices<'a>;
                type IterElem = Chars<'a>;
                #[inline] fn iter_indices(&self) -> Self::Iter {
                    self.char_indices()
                }
                #[inline] fn iter_elements(&self) -> Self::IterElem {
                    self.chars()
                }
                fn position<P>(&self, predicate: P) -> Option<usize> where
                    P: Fn(Self::Item) -> bool,
                {
                    for (o, c) in self.char_indices() {
                    if predicate(c) {
                        return Some(o);
                    }
                    }
                    None
                }
                #[inline] fn slice_index(&self, count: usize) -> Result<usize, Needed> {
                    let mut cnt = 0;
                    for (index, _) in self.char_indices() {
                    if cnt == count {
                        return Ok(index);
                    }
                    cnt += 1;
                    }
                    if cnt == count {
                    return Ok(self.len());
                    }
                    Err(Needed::Unknown)
                }
            }

            impl<'a> InputTake for &'a str
            {
                #[inline] fn take(&self, count: usize) -> Self {
                    &self[..count]
                }
                
                #[inline] fn take_split(&self, count: usize) -> (Self, Self) {
                    let (prefix, suffix) = self.split_at(count);
                    (suffix, prefix)
                }
            }
            /// Dummy trait used for default implementations.
            pub trait UnspecializedInput {}
            /// Methods to take as much input as possible 
            /// until the provided function returns true for the current element.
            pub trait InputTakeAtPosition: Sized 
            {
                /// The current input type is a sequence of that `Item` type.
                type Item;

                /// Looks for the first element of the input type for which the condition returns true,
                /// and returns the input up to this position.
                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true
                /// and returns the input up to this position.
                fn split_at_position1<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true,
                /// and returns the input up to this position.
                fn split_at_position_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true
                /// and returns the input up to this position.
                fn split_at_position1_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool;
            }

            impl<T: InputLength + InputIter + InputTake + Clone + UnspecializedInput> InputTakeAtPosition
            for T
            {
                type Item = <T as InputIter>::Item;

                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.position(predicate) {
                    Some(n) => Ok(self.take_split(n)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position1<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.position(predicate) {
                    Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
                    Some(n) => Ok(self.take_split(n)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.split_at_position(predicate) {
                    Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
                    res => res,
                    }
                }

                fn split_at_position1_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.split_at_position1(predicate, e) {
                    Err(Err::Incomplete(_)) => {
                        if self.input_len() == 0 {
                        Err(Err::Error(E::from_error_kind(self.clone(), e)))
                        } else {
                        Ok(self.take_split(self.input_len()))
                        }
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
                    match self.iter().position(|c| predicate(*c)) {
                    Some(i) => Ok(self.take_split(i)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position1<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.iter().position(|c| predicate(*c)) {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    Some(i) => Ok(self.take_split(i)),
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.iter().position(|c| predicate(*c)) {
                    Some(i) => Ok(self.take_split(i)),
                    None => Ok(self.take_split(self.input_len())),
                    }
                }

                fn split_at_position1_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.iter().position(|c| predicate(*c)) {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    Some(i) => Ok(self.take_split(i)),
                    None => {
                        if self.is_empty() {
                        Err(Err::Error(E::from_error_kind(self, e)))
                        } else {
                        Ok(self.take_split(self.input_len()))
                        }
                    }
                    }
                }
            }

            impl<'a> InputTakeAtPosition for &'a str 
            {
                type Item = char;

                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.find(predicate) {
                    // find() returns a byte index that is already in the slice at a char boundary
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position1<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.find(predicate) {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    // find() returns a byte index that is already in the slice at a char boundary
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => Err(Err::Incomplete(Needed::new(1))),
                    }
                }

                fn split_at_position_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.find(predicate) {
                    // find() returns a byte index that is already in the slice at a char boundary
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    // the end of slice is a char boundary
                    None => unsafe {
                        Ok((
                        self.get_unchecked(self.len()..),
                        self.get_unchecked(..self.len()),
                        ))
                    },
                    }
                }

                fn split_at_position1_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E> where
                    P: Fn(Self::Item) -> bool,
                {
                    match self.find(predicate) {
                    Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                    // find() returns a byte index that is already in the slice at a char boundary
                    Some(i) => unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) },
                    None => {
                        if self.is_empty() {
                        Err(Err::Error(E::from_error_kind(self, e)))
                        } else {
                        // the end of slice is a char boundary
                        unsafe {
                            Ok((
                            self.get_unchecked(self.len()..),
                            self.get_unchecked(..self.len()),
                            ))
                        }
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
                Error,
            }
            /// Abstracts comparison operations
            pub trait Compare<T> 
            {
                /// Compares self to another value for equality
                fn compare(&self, t: T) -> CompareResult;
                /// Compares self to another value for equality
                /// independently of the case.
                fn compare_no_case(&self, t: T) -> CompareResult;
            }

            fn lowercase_byte(c: u8) -> u8 
            {
                match c {
                    b'A'..=b'Z' => c - b'A' + b'a',
                    _ => c,
                }
            }

            impl<'a, 'b> Compare<&'b [u8]> for &'a [u8] 
            {
                #[inline(always)]
                fn compare(&self, t: &'b [u8]) -> CompareResult {
                    let pos = self.iter().zip(t.iter()).position(|(a, b)| a != b);

                    match pos {
                    Some(_) => CompareResult::Error,
                    None => {
                        if self.len() >= t.len() {
                        CompareResult::Ok
                        } else {
                        CompareResult::Incomplete
                        }
                    }
                    }
                }

                #[inline(always)]
                fn compare_no_case(&self, t: &'b [u8]) -> CompareResult {
                    if self
                    .iter()
                    .zip(t)
                    .any(|(a, b)| lowercase_byte(*a) != lowercase_byte(*b))
                    {
                    CompareResult::Error
                    } else if self.len() < t.len() {
                    CompareResult::Incomplete
                    } else {
                    CompareResult::Ok
                    }
                }
            }

            impl<
                T: InputLength + InputIter<Item = u8> + InputTake + UnspecializedInput,
                O: InputLength + InputIter<Item = u8> + InputTake,
            > Compare<O> for T
            {
                #[inline(always)]
                fn compare(&self, t: O) -> CompareResult {
                    let pos = self
                    .iter_elements()
                    .zip(t.iter_elements())
                    .position(|(a, b)| a != b);

                    match pos {
                    Some(_) => CompareResult::Error,
                    None => {
                        if self.input_len() >= t.input_len() {
                        CompareResult::Ok
                        } else {
                        CompareResult::Incomplete
                        }
                    }
                    }
                }

                #[inline(always)]
                fn compare_no_case(&self, t: O) -> CompareResult {
                    if self
                    .iter_elements()
                    .zip(t.iter_elements())
                    .any(|(a, b)| lowercase_byte(a) != lowercase_byte(b))
                    {
                    CompareResult::Error
                    } else if self.input_len() < t.input_len() {
                    CompareResult::Incomplete
                    } else {
                    CompareResult::Ok
                    }
                }
            }

            impl<'a, 'b> Compare<&'b str> for &'a [u8] 
            {
                #[inline(always)]
                fn compare(&self, t: &'b str) -> CompareResult {
                    self.compare(AsBytes::as_bytes(t))
                }
                #[inline(always)]
                fn compare_no_case(&self, t: &'b str) -> CompareResult {
                    self.compare_no_case(AsBytes::as_bytes(t))
                }
            }

            impl<'a, 'b> Compare<&'b str> for &'a str 
            {
                #[inline(always)]
                fn compare(&self, t: &'b str) -> CompareResult {
                    self.as_bytes().compare(t.as_bytes())
                }
                
                #[inline(always)]
                fn compare_no_case(&self, t: &'b str) -> CompareResult {
                    let pos = self
                    .chars()
                    .zip(t.chars())
                    .position(|(a, b)| a.to_lowercase().ne(b.to_lowercase()));

                    match pos {
                    Some(_) => CompareResult::Error,
                    None => {
                        if self.len() >= t.len() {
                        CompareResult::Ok
                        } else {
                        CompareResult::Incomplete
                        }
                    }
                    }
                }
            }

            impl<'a, 'b> Compare<&'b [u8]> for &'a str 
            {
                #[inline(always)]
                fn compare(&self, t: &'b [u8]) -> CompareResult {
                    AsBytes::as_bytes(self).compare(t)
                }
                #[inline(always)]
                fn compare_no_case(&self, t: &'b [u8]) -> CompareResult {
                    AsBytes::as_bytes(self).compare_no_case(t)
                }
            }
            /// Look for a token in self
            pub trait FindToken<T> 
            {
                /// Returns true if self contains the token
                fn find_token(&self, token: T) -> bool;
            }

            impl<'a> FindToken<u8> for &'a [u8] 
            {
                fn find_token(&self, token: u8) -> bool {
                    memchr::memchr(token, self).is_some()
                }
            }

            impl<'a> FindToken<u8> for &'a str 
            {
                fn find_token(&self, token: u8) -> bool {
                    self.as_bytes().find_token(token)
                }
            }

            impl<'a, 'b> FindToken<&'a u8> for &'b [u8] 
            {
                fn find_token(&self, token: &u8) -> bool {
                    self.find_token(*token)
                }
            }

            impl<'a, 'b> FindToken<&'a u8> for &'b str 
            {
                fn find_token(&self, token: &u8) -> bool {
                    self.as_bytes().find_token(token)
                }
            }

            impl<'a> FindToken<char> for &'a [u8] 
            {
                fn find_token(&self, token: char) -> bool {
                    self.iter().any(|i| *i == token as u8)
                }
            }

            impl<'a> FindToken<char> for &'a str 
            {
                fn find_token(&self, token: char) -> bool {
                    self.chars().any(|i| i == token)
                }
            }

            impl<'a> FindToken<char> for &'a [char] 
            {
                fn find_token(&self, token: char) -> bool {
                    self.iter().any(|i| *i == token)
                }
            }

            impl<'a, 'b> FindToken<&'a char> for &'b [char] 
            {
                fn find_token(&self, token: &char) -> bool {
                    self.find_token(*token)
                }
            }
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
                    if substr.len() > self.len() {
                    return None;
                    }

                    let (&substr_first, substr_rest) = match substr.split_first() 
                    {
                        Some(split) => split,
                        None => return Some(0),
                    };

                    if substr_rest.is_empty() {
                    return memchr::memchr(substr_first, self);
                    }

                    let mut offset = 0;
                    let haystack = &self[..self.len() - substr_rest.len()];

                    while let Some(position) = memchr::memchr(substr_first, &haystack[offset..]) {
                    offset += position;
                    let next_offset = offset + 1;
                    if &self[next_offset..][..substr_rest.len()] == substr_rest {
                        return Some(offset);
                    }

                    offset = next_offset;
                    }

                    None
                }
            }

            impl<'a, 'b> FindSubstring<&'b str> for &'a [u8]
            {
                fn find_substring(&self, substr: &'b str) -> Option<usize> {
                    self.find_substring(AsBytes::as_bytes(substr))
                }
            }

            impl<'a, 'b> FindSubstring<&'b str> for &'a str 
            {
                fn find_substring(&self, substr: &'b str) -> Option<usize> {
                    self.find(substr)
                }
            }
            /// Used to integrate `str`'s `parse()` method
            pub trait ParseTo<R> 
            {
                /// Succeeds if `parse()` succeeded. The byte slice implementation
                /// will first convert it to a `&str`, then apply the `parse()` function
                fn parse_to(&self) -> Option<R>;
            }

            impl<'a, R: FromStr> ParseTo<R> for &'a [u8]
            {
                fn parse_to(&self) -> Option<R> {
                    from_utf8(self).ok().and_then(|s| s.parse().ok())
                }
            }

            impl<'a, R: FromStr> ParseTo<R> for &'a str
            {
                fn parse_to(&self) -> Option<R> {
                    self.parse().ok()
                }
            }
            /// Slicing operations using ranges.
            pub trait Slice<R>
            {
                /// Slices self according to the range argument
                fn slice(&self, range: R) -> Self;
            }

            slice_ranges_impl! {str}
            slice_ranges_impl! {[T]}

            array_impls!
            {
                0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
                17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
            }
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

                #[inline] fn new_builder(&self) -> Vec<u8> {
                    Vec::new()
                }
                #[inline] fn extend_into(&self, acc: &mut Vec<u8>) {
                    acc.extend(self.iter().cloned());
                }
            }
            
            impl ExtendInto for &[u8] 
            {
                type Item = u8;
                type Extender = Vec<u8>;

                #[inline] fn new_builder(&self) -> Vec<u8> {
                    Vec::new()
                }
                #[inline] fn extend_into(&self, acc: &mut Vec<u8>) {
                    acc.extend_from_slice(self);
                }
            }
            
            impl ExtendInto for str 
            {
                type Item = char;
                type Extender = String;

                #[inline] fn new_builder(&self) -> String {
                    String::new()
                }
                #[inline] fn extend_into(&self, acc: &mut String) {
                    acc.push_str(self);
                }
            }
            
            impl ExtendInto for &str 
            {
                type Item = char;
                type Extender = String;

                #[inline] fn new_builder(&self) -> String {
                    String::new()
                }
                #[inline] fn extend_into(&self, acc: &mut String) {
                    acc.push_str(self);
                }
            }
            
            impl ExtendInto for char 
            {
                type Item = char;
                type Extender = String;

                #[inline] fn new_builder(&self) -> String {
                    String::new()
                }
                #[inline] fn extend_into(&self, acc: &mut String) {
                    acc.push(*self);
                }
            }
            /// Helper trait to convert numbers to usize.
            pub trait ToUsize 
            {
                /// converts self to usize
                fn to_usize(&self) -> usize;
            }

            impl ToUsize for u8 
            {
                #[inline] fn to_usize(&self) -> usize {
                    *self as usize
                }
            }

            impl ToUsize for u16 
            {
                #[inline] fn to_usize(&self) -> usize {
                    *self as usize
                }
            }

            impl ToUsize for usize
            {
                #[inline] fn to_usize(&self) -> usize {
                    *self
                }
            }
            
            impl ToUsize for u32
            {
                #[inline] fn to_usize(&self) -> usize {
                    *self as usize
                }
            }
            
            impl ToUsize for u64
            {
                #[inline] fn to_usize(&self) -> usize {
                    *self as usize
                }
            }
            /// Equivalent From implementation to avoid orphan rules in bits parsers
            pub trait ErrorConvert<E>
            {
                /// Transform to another error type
                fn convert(self) -> E;
            }

            impl<I> ErrorConvert<(I, ErrorKind)> for ((I, usize), ErrorKind) 
            {
                fn convert(self) -> (I, ErrorKind) {
                    ((self.0).0, self.1)
                }
            }

            impl<I> ErrorConvert<((I, usize), ErrorKind)> for (I, ErrorKind)
            {
                fn convert(self) -> ((I, usize), ErrorKind) {
                    ((self.0, 0), self.1)
                }
            }
            
            impl<I> ErrorConvert<error::Error<I>> for error::Error<(I, usize)> 
            {
                fn convert(self) -> error::Error<I> {
                    error::Error {
                    input: self.input.0,
                    code: self.code,
                    }
                }
            }

            impl<I> ErrorConvert<error::Error<(I, usize)>> for error::Error<I> 
            {
                fn convert(self) -> error::Error<(I, usize)> {
                    error::Error {
                    input: (self.input, 0),
                    code: self.code,
                    }
                }
            }
            
            impl<I> ErrorConvert<error::VerboseError<I>> for error::VerboseError<(I, usize)> 
            {
                fn convert(self) -> error::VerboseError<I> {
                    error::VerboseError {
                    errors: self.errors.into_iter().map(|(i, e)| (i.0, e)).collect(),
                    }
                }
            }
            
            impl<I> ErrorConvert<error::VerboseError<(I, usize)>> for error::VerboseError<I>
            {
                fn convert(self) -> error::VerboseError<(I, usize)> {
                    error::VerboseError {
                    errors: self.errors.into_iter().map(|(i, e)| ((i, 0), e)).collect(),
                    }
                }
            }

            impl ErrorConvert<()> for ()
            {
                fn convert(self) {}
            }
            /// Helper trait to show a byte slice as a hex dump
            pub trait HexDisplay
            {
                /// Converts the value of `self` to a hex dump, returning the owned
                /// `String`.
                fn to_hex(&self, chunk_size: usize) -> String;

                /// Converts the value of `self` to a hex dump beginning at `from` address, returning the owned
                /// `String`.
                fn to_hex_from(&self, chunk_size: usize, from: usize) -> String;
            }
            
            static CHARS: &[u8] = b"0123456789abcdef";
            
            impl HexDisplay for [u8]
            {
                #[allow(unused_variables)]
                fn to_hex(&self, chunk_size: usize) -> String {
                    self.to_hex_from(chunk_size, 0)
                }

                #[allow(unused_variables)]
                fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
                    let mut v = Vec::with_capacity(self.len() * 3);
                    let mut i = from;
                    for chunk in self.chunks(chunk_size) {
                    let s = format!("{:08x}", i);
                    for &ch in s.as_bytes().iter() {
                        v.push(ch);
                    }
                    v.push(b'\t');

                    i += chunk_size;

                    for &byte in chunk {
                        v.push(CHARS[(byte >> 4) as usize]);
                        v.push(CHARS[(byte & 0xf) as usize]);
                        v.push(b' ');
                    }
                    if chunk_size > chunk.len() {
                        for j in 0..(chunk_size - chunk.len()) {
                        v.push(b' ');
                        v.push(b' ');
                        v.push(b' ');
                        }
                    }
                    v.push(b'\t');

                    for &byte in chunk {
                        if (byte >= 32 && byte <= 126) || byte >= 128 {
                        v.push(byte);
                        } else {
                        v.push(b'.');
                        }
                    }
                    v.push(b'\n');
                    }

                    String::from_utf8_lossy(&v[..]).into_owned()
                }
            }
            
            impl HexDisplay for str
            {
                #[allow(unused_variables)]
                fn to_hex(&self, chunk_size: usize) -> String {
                    self.to_hex_from(chunk_size, 0)
                }

                #[allow(unused_variables)]
                fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
                    self.as_bytes().to_hex_from(chunk_size, from)
                }
            }
        } pub use self::traits::*;
        
        pub mod bits
        {
            use ::
            {
                ops::RangeFrom,
                parsers::nom::
                {
                    error::{ErrorKind, ParseError},
                    internal::{Err, IResult, Needed, Parser},
                    traits::{ErrorConvert, Slice},
                },
                *,
            };

            pub mod complete
            {
                //! Bit level parsers
                use ::
                {
                    ops::{AddAssign, Div, RangeFrom, Shl, Shr},
                    parsers::nom::
                    {
                        error::{ErrorKind, ParseError},
                        internal::{Err, IResult},
                        traits::{InputIter, InputLength, Slice, ToUsize},
                    },
                    *
                };
                /// Generates a parser taking `count` bits
                pub fn take<I, O, C, E: ParseError<(I, usize)>>(
                count: C,
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                C: ToUsize,
                O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
                {
                    let count = count.to_usize();
                    move |(input, bit_offset): (I, usize)| {
                        if count == 0 {
                        Ok(((input, bit_offset), 0u8.into()))
                        } else {
                        let cnt = (count + bit_offset).div(8);
                        if input.input_len() * 8 < count + bit_offset {
                            Err(Err::Error(E::from_error_kind(
                            (input, bit_offset),
                            ErrorKind::Eof,
                            )))
                        } else {
                            let mut acc: O = 0_u8.into();
                            let mut offset: usize = bit_offset;
                            let mut remaining: usize = count;
                            let mut end_offset: usize = 0;

                            for byte in input.iter_elements().take(cnt + 1) {
                            if remaining == 0 {
                                break;
                            }
                            let val: O = if offset == 0 {
                                byte.into()
                            } else {
                                ((byte << offset) as u8 >> offset).into()
                            };

                            if remaining < 8 - offset {
                                acc += val >> (8 - offset - remaining);
                                end_offset = remaining + offset;
                                break;
                            } else {
                                acc += val << (remaining - (8 - offset));
                                remaining -= 8 - offset;
                                offset = 0;
                            }
                            }
                            Ok(((input.slice(cnt..), end_offset), acc))
                        }
                        }
                    }
                }
                /// Generates a parser taking `count` bits and comparing them to `pattern`
                pub fn tag<I, O, C, E: ParseError<(I, usize)>>(
                pattern: O,
                count: C,
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength + Clone,
                C: ToUsize,
                O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O> + PartialEq,
                {
                    let count = count.to_usize();
                    move |input: (I, usize)| {
                        let inp = input.clone();

                        take(count)(input).and_then(|(i, o)| {
                        if pattern == o {
                            Ok((i, o))
                        } else {
                            Err(Err::Error(error_position!(inp, ErrorKind::TagBits)))
                        }
                        })
                    }
                }
                /// Parses one specific bit as a bool.
                pub fn bool<I, E: ParseError<(I, usize)>>(input: (I, usize)) -> IResult<(I, usize), bool, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let (res, bit): (_, u32) = take(1usize)(input)?;
                    Ok((res, bit != 0))
                }
            }

            pub mod streaming
            {
                use ::
                {
                    ops::{AddAssign, Div, RangeFrom, Shl, Shr},
                    parsers::nom::
                    {
                        error::{ErrorKind, ParseError},
                        internal::{Err, IResult, Needed},
                        traits::{InputIter, InputLength, Slice, ToUsize},
                    },
                    *
                };
                /// Generates a parser taking `count` bits
                pub fn take<I, O, C, E: ParseError<(I, usize)>>(
                count: C,
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                C: ToUsize,
                O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
                {
                    let count = count.to_usize();
                    move |(input, bit_offset): (I, usize)| {
                        if count == 0 {
                        Ok(((input, bit_offset), 0u8.into()))
                        } else {
                        let cnt = (count + bit_offset).div(8);
                        if input.input_len() * 8 < count + bit_offset {
                            Err(Err::Incomplete(Needed::new(count as usize)))
                        } else {
                            let mut acc: O = 0_u8.into();
                            let mut offset: usize = bit_offset;
                            let mut remaining: usize = count;
                            let mut end_offset: usize = 0;

                            for byte in input.iter_elements().take(cnt + 1) {
                            if remaining == 0 {
                                break;
                            }
                            let val: O = if offset == 0 {
                                byte.into()
                            } else {
                                ((byte << offset) as u8 >> offset).into()
                            };

                            if remaining < 8 - offset {
                                acc += val >> (8 - offset - remaining);
                                end_offset = remaining + offset;
                                break;
                            } else {
                                acc += val << (remaining - (8 - offset));
                                remaining -= 8 - offset;
                                offset = 0;
                            }
                            }
                            Ok(((input.slice(cnt..), end_offset), acc))
                        }
                        }
                    }
                }
                /// Generates a parser taking `count` bits and comparing them to `pattern`
                pub fn tag<I, O, C, E: ParseError<(I, usize)>>(
                pattern: O,
                count: C,
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength + Clone,
                C: ToUsize,
                O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O> + PartialEq,
                {
                    let count = count.to_usize();
                    move |input: (I, usize)| {
                        let inp = input.clone();

                        take(count)(input).and_then(|(i, o)| {
                        if pattern == o {
                            Ok((i, o))
                        } else {
                            Err(Err::Error(error_position!(inp, ErrorKind::TagBits)))
                        }
                        })
                    }
                }
                /// Parses one specific bit as a bool.
                pub fn bool<I, E: ParseError<(I, usize)>>(input: (I, usize)) -> IResult<(I, usize), bool, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let (res, bit): (_, u32) = take(1usize)(input)?;
                    Ok((res, bit != 0))
                }
            }
            /// Converts a byte-level input to a bit-level input, for consumption by a parser that uses bits.
            pub fn bits<I, O, E1, E2, P>(mut parser: P) -> impl FnMut(I) -> IResult<I, O, E2> where
            E1: ParseError<(I, usize)> + ErrorConvert<E2>,
            E2: ParseError<I>,
            I: Slice<RangeFrom<usize>>,
            P: Parser<(I, usize), O, E1>,
            {
                move |input: I| match parser.parse((input, 0)) {
                    Ok(((rest, offset), result)) => {
                    let remaining_bytes_index = offset / 8 + if offset % 8 == 0 { 0 } else { 1 };
                    Ok((rest.slice(remaining_bytes_index..), result))
                    }
                    Err(Err::Incomplete(n)) => Err(Err::Incomplete(n.map(|u| u.get() / 8 + 1))),
                    Err(Err::Error(e)) => Err(Err::Error(e.convert())),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
                }
            }
            /// Counterpart to `bits`, `bytes` transforms its bit stream input into a byte slice for the underlying
            /// parser, allowing byte-slice parsers to work on bit streams.
            pub fn bytes<I, O, E1, E2, P>(mut parser: P) -> impl FnMut((I, usize)) -> IResult<(I, usize), O, E2> where
            E1: ParseError<I> + ErrorConvert<E2>,
            E2: ParseError<(I, usize)>,
            I: Slice<RangeFrom<usize>> + Clone,
            P: Parser<I, O, E1>,
            {
                move |(input, offset): (I, usize)| {
                    let inner = if offset % 8 != 0 {
                    input.slice((1 + offset / 8)..)
                    } else {
                    input.slice((offset / 8)..)
                    };
                    let i = (input, offset);
                    match parser.parse(inner) {
                    Ok((rest, res)) => Ok(((rest, 0), res)),
                    Err(Err::Incomplete(Needed::Unknown)) => Err(Err::Incomplete(Needed::Unknown)),
                    Err(Err::Incomplete(Needed::Size(sz))) => Err(match sz.get().checked_mul(8) {
                        Some(v) => Err::Incomplete(Needed::new(v)),
                        None => Err::Failure(E2::from_error_kind(i, ErrorKind::TooLarge)),
                    }),
                    Err(Err::Error(e)) => Err(Err::Error(e.convert())),
                    Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
                    }
                }
            }
        }
    
        pub mod bytes
        {
            //! Parsers recognizing bytes streams
            use ::
            {
                *,
            };

            pub mod complete
            {
                use ::
                {
                    ops::RangeFrom,
                    parsers::nom::
                    {
                        error::{ ErrorKind, ParseError },
                        internal::{ Err, IResult, Parser },
                        traits::
                        {
                            AsChar, Compare, CompareResult, ExtendInto, FindSubstring, FindToken, InputIter, InputLength, InputTake,
                            InputTakeAtPosition, Offset, Slice, ToUsize,
                        },
                    },
                    result::Result::*,
                    *
                };

                /// Recognizes a pattern.
                pub fn tag<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + Compare<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let tag_len = tag.input_len();
                        let t = tag.clone();
                        let res: IResult<_, _, Error> = match i.compare(t) {
                        CompareResult::Ok => Ok(i.take_split(tag_len)),
                        _ => {
                            let e: ErrorKind = ErrorKind::Tag;
                            Err(Err::Error(Error::from_error_kind(i, e)))
                        }
                        };
                        res
                    }
                }
                /// Recognizes a case insensitive pattern.
                pub fn tag_no_case<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + Compare<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let tag_len = tag.input_len();
                        let t = tag.clone();

                        let res: IResult<_, _, Error> = match (i).compare_no_case(t) {
                        CompareResult::Ok => Ok(i.take_split(tag_len)),
                        _ => {
                            let e: ErrorKind = ErrorKind::Tag;
                            Err(Err::Error(Error::from_error_kind(i, e)))
                        }
                        };
                        res
                    }
                }
                /// Parse till certain characters are met.
                pub fn is_not<T, Input, Error: ParseError<Input>>(
                arr: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                T: FindToken<<Input as InputTakeAtPosition>::Item>,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::IsNot;
                        i.split_at_position1_complete(|c| arr.find_token(c), e)
                    }
                }
                /// Returns the longest slice of the matches the pattern.
                pub fn is_a<T, Input, Error: ParseError<Input>>(
                arr: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                T: FindToken<<Input as InputTakeAtPosition>::Item>,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::IsA;
                        i.split_at_position1_complete(|c| !arr.find_token(c), e)
                    }
                }
                /// Returns the longest input slice (if any) that matches the predicate.
                pub fn take_while<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position_complete(|c| !cond(c))
                }
                /// Returns the longest (at least 1) input slice that matches the predicate.
                pub fn take_while1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::TakeWhile1;
                        i.split_at_position1_complete(|c| !cond(c), e)
                    }
                }
                /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
                pub fn take_while_m_n<F, Input, Error: ParseError<Input>>(
                m: usize,
                n: usize,
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputIter + InputLength + Slice<RangeFrom<usize>>,
                F: Fn(<Input as InputIter>::Item) -> bool,
                {
                    move |i: Input| {
                        let input = i;

                        match input.position(|c| !cond(c)) {
                        Some(idx) => {
                            if idx >= m {
                            if idx <= n {
                                let res: IResult<_, _, Error> = if let Ok(index) = input.slice_index(idx) {
                                Ok(input.take_split(index))
                                } else {
                                Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::TakeWhileMN,
                                )))
                                };
                                res
                            } else {
                                let res: IResult<_, _, Error> = if let Ok(index) = input.slice_index(n) {
                                Ok(input.take_split(index))
                                } else {
                                Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::TakeWhileMN,
                                )))
                                };
                                res
                            }
                            } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                            }
                        }
                        None => {
                            let len = input.input_len();
                            if len >= n {
                            match input.slice_index(n) {
                                Ok(index) => Ok(input.take_split(index)),
                                Err(_needed) => Err(Err::Error(Error::from_error_kind(
                                input,
                                ErrorKind::TakeWhileMN,
                                ))),
                            }
                            } else if len >= m && len <= n {
                            let res: IResult<_, _, Error> = Ok((input.slice(len..), input));
                            res
                            } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                            }
                        }
                        }
                    }
                }
                /// Returns the longest input slice (if any) till a predicate is met.
                pub fn take_till<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position_complete(|c| cond(c))
                }
                /// Returns the longest (at least 1) input slice till a predicate is met.
                pub fn take_till1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::TakeTill1;
                        i.split_at_position1_complete(|c| cond(c), e)
                    }
                }
                /// Returns an input slice containing the first N input elements (Input[..N]).
                pub fn take<C, Input, Error: ParseError<Input>>(
                count: C,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputIter + InputTake,
                C: ToUsize,
                {
                    let c = count.to_usize();
                    move |i: Input| match i.slice_index(c) {
                        Err(_needed) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::Eof))),
                        Ok(index) => Ok(i.take_split(index)),
                    }
                }
                /// Returns the input slice up to the first occurrence of the pattern.
                pub fn take_until<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + FindSubstring<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let t = tag.clone();
                        let res: IResult<_, _, Error> = match i.find_substring(t) {
                        None => Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil))),
                        Some(index) => Ok(i.take_split(index)),
                        };
                        res
                    }
                }
                /// Returns the non empty input slice up to the first occurrence of the pattern.
                pub fn take_until1<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + FindSubstring<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let t = tag.clone();
                        let res: IResult<_, _, Error> = match i.find_substring(t) {
                        None => Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil))),
                        Some(0) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil))),
                        Some(index) => Ok(i.take_split(index)),
                        };
                        res
                    }
                }
                /// Matches a byte string with escaped characters.
                pub fn escaped<'a, Input: 'a, Error, F, G, O1, O2>(
                mut normal: F,
                control_char: char,
                mut escapable: G,
                ) -> impl FnMut(Input) -> IResult<Input, Input, Error> where
                Input: Clone
                    + Offset
                    + InputLength
                    + InputTake
                    + InputTakeAtPosition
                    + Slice<RangeFrom<usize>>
                    + InputIter,
                <Input as InputIter>::Item: AsChar,
                F: Parser<Input, O1, Error>,
                G: Parser<Input, O2, Error>,
                Error: ParseError<Input>,
                {
                    move |input: Input|
                    {
                        let mut i = input.clone();

                        while i.input_len() > 0 {
                        let current_len = i.input_len();

                        match normal.parse(i.clone()) {
                            Ok((i2, _)) => {
                            if i2.input_len() == 0 {
                                return Ok((input.slice(input.input_len()..), input));
                            } else if i2.input_len() == current_len {
                                let index = input.offset(&i2);
                                return Ok(input.take_split(index));
                            } else {
                                i = i2;
                            }
                            }
                            Err(Err::Error(_)) => {
                            if i.iter_elements().next().unwrap().as_char() == control_char {
                                let next = control_char.len_utf8();
                                if next >= i.input_len() {
                                return Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::Escaped,
                                )));
                                } else {
                                match escapable.parse(i.slice(next..)) {
                                    Ok((i2, _)) => {
                                    if i2.input_len() == 0 {
                                        return Ok((input.slice(input.input_len()..), input));
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
                                return Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::Escaped,
                                )));
                                }
                                return Ok(input.take_split(index));
                            }
                            }
                            Err(e) => {
                            return Err(e);
                            }
                        }
                        }

                        Ok((input.slice(input.input_len()..), input))
                    }
                }
                /// Matches a byte string with escaped characters.
                pub fn escaped_transform<Input, Error, F, G, O1, O2, ExtendItem, Output>(
                mut normal: F,
                control_char: char,
                mut transform: G,
                ) -> impl FnMut(Input) -> IResult<Input, Output, Error> where
                Input: Clone
                    + Offset
                    + InputLength
                    + InputTake
                    + InputTakeAtPosition
                    + Slice<RangeFrom<usize>>
                    + InputIter,
                Input: ExtendInto<Item = ExtendItem, Extender = Output>,
                O1: ExtendInto<Item = ExtendItem, Extender = Output>,
                O2: ExtendInto<Item = ExtendItem, Extender = Output>,
                <Input as InputIter>::Item: AsChar,
                F: Parser<Input, O1, Error>,
                G: Parser<Input, O2, Error>,
                Error: ParseError<Input>,
                {
                    move | input:Input |
                    {
                        let mut index = 0;
                        let mut res = input.new_builder();

                        let i = input.clone();

                        while index < i.input_len() {
                        let current_len = i.input_len();
                        let remainder = i.slice(index..);
                        match normal.parse(remainder.clone()) {
                            Ok((i2, o)) => {
                            o.extend_into(&mut res);
                            if i2.input_len() == 0 {
                                return Ok((i.slice(i.input_len()..), res));
                            } else if i2.input_len() == current_len {
                                return Ok((remainder, res));
                            } else {
                                index = input.offset(&i2);
                            }
                            }
                            Err(Err::Error(_)) => {
                            if remainder.iter_elements().next().unwrap().as_char() == control_char {
                                let next = index + control_char.len_utf8();
                                let input_len = input.input_len();

                                if next >= input_len {
                                return Err(Err::Error(Error::from_error_kind(
                                    remainder,
                                    ErrorKind::EscapedTransform,
                                )));
                                } else {
                                match transform.parse(i.slice(next..)) {
                                    Ok((i2, o)) => {
                                    o.extend_into(&mut res);
                                    if i2.input_len() == 0 {
                                        return Ok((i.slice(i.input_len()..), res));
                                    } else {
                                        index = input.offset(&i2);
                                    }
                                    }
                                    Err(e) => return Err(e),
                                }
                                }
                            } else {
                                if index == 0 {
                                return Err(Err::Error(Error::from_error_kind(
                                    remainder,
                                    ErrorKind::EscapedTransform,
                                )));
                                }
                                return Ok((remainder, res));
                            }
                            }
                            Err(e) => return Err(e),
                        }
                        }
                        Ok((input.slice(index..), res))
                    }
                }
            }

            pub mod streaming
            {
                use ::
                {
                    ops::RangeFrom,
                    parsers::nom::
                    {
                        error::{ ErrorKind, ParseError },
                        internal::{Err, IResult, Needed, Parser},
                        traits::
                        {
                            AsChar, Compare, CompareResult, ExtendInto, FindSubstring, FindToken, InputIter, InputLength, InputTake,
                            InputTakeAtPosition, Offset, Slice, ToUsize,
                        },
                    },
                    result::Result::*,
                    *
                };
                /// Recognizes a pattern.
                pub fn tag<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputLength + Compare<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let tag_len = tag.input_len();
                        let t = tag.clone();

                        let res: IResult<_, _, Error> = match i.compare(t) {
                        CompareResult::Ok => Ok(i.take_split(tag_len)),
                        CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(tag_len - i.input_len()))),
                        CompareResult::Error => {
                            let e: ErrorKind = ErrorKind::Tag;
                            Err(Err::Error(Error::from_error_kind(i, e)))
                        }
                        };
                        res
                    }
                }
                /// Recognizes a case insensitive pattern.
                pub fn tag_no_case<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputLength + Compare<T>,
                T: InputLength + Clone,
                {
                    move |i: Input| {
                        let tag_len = tag.input_len();
                        let t = tag.clone();

                        let res: IResult<_, _, Error> = match (i).compare_no_case(t) {
                        CompareResult::Ok => Ok(i.take_split(tag_len)),
                        CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(tag_len - i.input_len()))),
                        CompareResult::Error => {
                            let e: ErrorKind = ErrorKind::Tag;
                            Err(Err::Error(Error::from_error_kind(i, e)))
                        }
                        };
                        res
                    }
                }
                /// Parse till certain characters are met.
                pub fn is_not<T, Input, Error: ParseError<Input>>(
                arr: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                T: FindToken<<Input as InputTakeAtPosition>::Item>,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::IsNot;
                        i.split_at_position1(|c| arr.find_token(c), e)
                    }
                }
                /// Returns the longest slice of the matches the pattern.
                pub fn is_a<T, Input, Error: ParseError<Input>>(
                arr: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                T: FindToken<<Input as InputTakeAtPosition>::Item>,
                {
                    move |i: Input| 
                    {
                        let e: ErrorKind = ErrorKind::IsA;
                        i.split_at_position1(|c| !arr.find_token(c), e)
                    }
                }
                /// Returns the longest input slice (if any) that matches the predicate.
                pub fn take_while<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position(|c| !cond(c))
                }
                /// Returns the longest (at least 1) input slice that matches the predicate.
                pub fn take_while1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::TakeWhile1;
                        i.split_at_position1(|c| !cond(c), e)
                    }
                }
                /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
                pub fn take_while_m_n<F, Input, Error: ParseError<Input>>(
                m: usize,
                n: usize,
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputIter + InputLength,
                F: Fn(<Input as InputIter>::Item) -> bool,
                {
                    move |i: Input| {
                        let input = i;

                        match input.position(|c| !cond(c)) {
                        Some(idx) => {
                            if idx >= m {
                            if idx <= n {
                                let res: IResult<_, _, Error> = if let Ok(index) = input.slice_index(idx) {
                                Ok(input.take_split(index))
                                } else {
                                Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::TakeWhileMN,
                                )))
                                };
                                res
                            } else {
                                let res: IResult<_, _, Error> = if let Ok(index) = input.slice_index(n) {
                                Ok(input.take_split(index))
                                } else {
                                Err(Err::Error(Error::from_error_kind(
                                    input,
                                    ErrorKind::TakeWhileMN,
                                )))
                                };
                                res
                            }
                            } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                            }
                        }
                        None => {
                            let len = input.input_len();
                            if len >= n {
                            match input.slice_index(n) {
                                Ok(index) => Ok(input.take_split(index)),
                                Err(_needed) => Err(Err::Error(Error::from_error_kind(
                                input,
                                ErrorKind::TakeWhileMN,
                                ))),
                            }
                            } else {
                            let needed = if m > len { m - len } else { 1 };
                            Err(Err::Incomplete(Needed::new(needed)))
                            }
                        }
                        }
                    }
                }
                /// Returns the longest input slice (if any) till a predicate is met.
                pub fn take_till<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position(|c| cond(c))
                }
                /// Returns the longest (at least 1) input slice till a predicate is met.
                pub fn take_till1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| {
                        let e: ErrorKind = ErrorKind::TakeTill1;
                        i.split_at_position1(|c| cond(c), e)
                    }
                }
                /// Returns an input slice containing the first N input elements (Input[..N]).
                pub fn take<C, Input, Error: ParseError<Input>>(
                count: C,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputIter + InputTake + InputLength,
                C: ToUsize,
                {
                    let c = count.to_usize();
                    move |i: Input| match i.slice_index(c) {
                        Err(i) => Err(Err::Incomplete(i)),
                        Ok(index) => Ok(i.take_split(index)),
                    }
                }
                /// Returns the input slice up to the first occurrence of the pattern.
                pub fn take_until<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputLength + FindSubstring<T>,
                T: Clone,
                {
                    move |i: Input| {
                        let t = tag.clone();

                        let res: IResult<_, _, Error> = match i.find_substring(t) {
                        None => Err(Err::Incomplete(Needed::Unknown)),
                        Some(index) => Ok(i.take_split(index)),
                        };
                        res
                    }
                }
                /// Returns the non empty input slice up to the first occurrence of the pattern.
                pub fn take_until1<T, Input, Error: ParseError<Input>>(
                tag: T,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error> where
                Input: InputTake + InputLength + FindSubstring<T>,
                T: Clone,
                {
                    move |i: Input| {
                        let t = tag.clone();

                        let res: IResult<_, _, Error> = match i.find_substring(t) {
                        None => Err(Err::Incomplete(Needed::Unknown)),
                        Some(0) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil))),
                        Some(index) => Ok(i.take_split(index)),
                        };
                        res
                    }
                }
                /// Matches a byte string with escaped characters.
                pub fn escaped<Input, Error, F, G, O1, O2>(
                mut normal: F,
                control_char: char,
                mut escapable: G,
                ) -> impl FnMut(Input) -> IResult<Input, Input, Error> where
                Input: Clone
                    + Offset
                    + InputLength
                    + InputTake
                    + InputTakeAtPosition
                    + Slice<RangeFrom<usize>>
                    + InputIter,
                <Input as InputIter>::Item: AsChar,
                F: Parser<Input, O1, Error>,
                G: Parser<Input, O2, Error>,
                Error: ParseError<Input>,
                {
                    move | input:Input |
                    {
                        let mut i = input.clone();

                        while i.input_len() > 0 {
                        let current_len = i.input_len();

                        match normal.parse(i.clone()) {
                            Ok((i2, _)) => {
                            if i2.input_len() == 0 {
                                return Err(Err::Incomplete(Needed::Unknown));
                            } else if i2.input_len() == current_len {
                                let index = input.offset(&i2);
                                return Ok(input.take_split(index));
                            } else {
                                i = i2;
                            }
                            }
                            Err(Err::Error(_)) => {
                            if i.iter_elements().next().unwrap().as_char() == control_char {
                                let next = control_char.len_utf8();
                                if next >= i.input_len() {
                                return Err(Err::Incomplete(Needed::new(1)));
                                } else {
                                match escapable.parse(i.slice(next..)) {
                                    Ok((i2, _)) => {
                                    if i2.input_len() == 0 {
                                        return Err(Err::Incomplete(Needed::Unknown));
                                    } else {
                                        i = i2;
                                    }
                                    }
                                    Err(e) => return Err(e),
                                }
                                }
                            } else {
                                let index = input.offset(&i);
                                return Ok(input.take_split(index));
                            }
                            }
                            Err(e) => {
                            return Err(e);
                            }
                        }
                        }

                        Err(Err::Incomplete(Needed::Unknown))
                    }
                }
                /// Matches a byte string with escaped characters.
                pub fn escaped_transform<Input, Error, F, G, O1, O2, ExtendItem, Output>(
                mut normal: F,
                control_char: char,
                mut transform: G,
                ) -> impl FnMut(Input) -> IResult<Input, Output, Error> where
                Input: Clone
                    + Offset
                    + InputLength
                    + InputTake
                    + InputTakeAtPosition
                    + Slice<RangeFrom<usize>>
                    + InputIter,
                Input: ExtendInto<Item = ExtendItem, Extender = Output>,
                O1: ExtendInto<Item = ExtendItem, Extender = Output>,
                O2:  ExtendInto<Item = ExtendItem, Extender = Output>,
                <Input as InputIter>::Item: AsChar,
                F: Parser<Input, O1, Error>,
                G: Parser<Input, O2, Error>,
                Error: ParseError<Input>,
                {
                    move | input:Input |
                    {
                        let mut index = 0;
                        let mut res = input.new_builder();

                        let i = input.clone();

                        while index < i.input_len() {
                        let current_len = i.input_len();
                        let remainder = i.slice(index..);
                        match normal.parse(remainder.clone()) {
                            Ok((i2, o)) => {
                            o.extend_into(&mut res);
                            if i2.input_len() == 0 {
                                return Err(Err::Incomplete(Needed::Unknown));
                            } else if i2.input_len() == current_len {
                                return Ok((remainder, res));
                            } else {
                                index = input.offset(&i2);
                            }
                            }
                            Err(Err::Error(_)) => {
                            // unwrap() should be safe here since index < $i.input_len()
                            if remainder.iter_elements().next().unwrap().as_char() == control_char {
                                let next = index + control_char.len_utf8();
                                let input_len = input.input_len();

                                if next >= input_len {
                                return Err(Err::Incomplete(Needed::Unknown));
                                } else {
                                match transform.parse(i.slice(next..)) {
                                    Ok((i2, o)) => {
                                    o.extend_into(&mut res);
                                    if i2.input_len() == 0 {
                                        return Err(Err::Incomplete(Needed::Unknown));
                                    } else {
                                        index = input.offset(&i2);
                                    }
                                    }
                                    Err(e) => return Err(e),
                                }
                                }
                            } else {
                                return Ok((remainder, res));
                            }
                            }
                            Err(e) => return Err(e),
                        }
                        }
                        Err(Err::Incomplete(Needed::Unknown))
                    }
                }
            }

        } pub use self::bytes::*;
        
        pub mod character
        {
            use ::
            {
                *,
            };

            pub mod complete
            {
                //! Character specific parsers and combinators, complete input version.
                //! Functions recognizing specific characters.
                use ::
                {
                    ops::{ Range, RangeFrom, RangeTo },
                    parsers::nom::
                    {
                        branch::alt,
                        bytes::complete::tag,
                        combinator::{ opt, value },
                        error::{ ErrorKind, ParseError },
                        internal::{Err, IResult},
                        traits::{ AsChar, Compare, CompareResult, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice, },
                    },
                    *
                };
                
                macro_rules! ints 
                {
                    ($($t:tt)+) => {
                        $(
                        /// will parse a number in text form to a number
                        ///
                        /// *Complete version*: can parse until the end of input.
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E>
                            where
                            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
                            <T as InputIter>::Item: AsChar,
                            T: for <'a> Compare<&'a[u8]>,
                            {
                                let (i, sign) = sign(input.clone())?;

                                if i.input_len() == 0 {
                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                }

                                let mut value: $t = 0;
                                if sign {
                                    for (pos, c) in i.iter_indices() {
                                        match c.as_char().to_digit(10) {
                                            None => {
                                                if pos == 0 {
                                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                                } else {
                                                    return Ok((i.slice(pos..), value));
                                                }
                                            },
                                            Some(d) => match value.checked_mul(10).and_then(|v| v.checked_add(d as $t)) {
                                                None => return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))),
                                                Some(v) => value = v,
                                            }
                                        }
                                    }
                                } else {
                                    for (pos, c) in i.iter_indices() {
                                        match c.as_char().to_digit(10) {
                                            None => {
                                                if pos == 0 {
                                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                                } else {
                                                    return Ok((i.slice(pos..), value));
                                                }
                                            },
                                            Some(d) => match value.checked_mul(10).and_then(|v| v.checked_sub(d as $t)) {
                                                None => return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))),
                                                Some(v) => value = v,
                                            }
                                        }
                                    }
                                }

                                Ok((i.slice(i.input_len()..), value))
                            }
                        )+
                    }
                }
                
                macro_rules! uints
                {
                    ($($t:tt)+) => {
                        $(
                        /// will parse a number in text form to a number
                        ///
                        /// *Complete version*: can parse until the end of input.
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E>
                            where
                            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
                            <T as InputIter>::Item: AsChar,
                            {
                                let i = input;

                                if i.input_len() == 0 {
                                    return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
                                }

                                let mut value: $t = 0;
                                for (pos, c) in i.iter_indices() {
                                    match c.as_char().to_digit(10) {
                                        None => {
                                            if pos == 0 {
                                                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
                                            } else {
                                                return Ok((i.slice(pos..), value));
                                            }
                                        },
                                        Some(d) => match value.checked_mul(10).and_then(|v| v.checked_add(d as $t)) {
                                            None => return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit))),
                                            Some(v) => value = v,
                                        }
                                    }
                                }

                                Ok((i.slice(i.input_len()..), value))
                            }
                        )+
                    }
                }
                /// Recognizes one character.
                pub fn char<I, Error: ParseError<I>>(c: char) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                {
                    move |i: I| match (i).iter_elements().next().map(|t| {
                        let b = t.as_char() == c;
                        (&c, b)
                    }) {
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                        _ => Err(Err::Error(Error::from_char(i, c))),
                    }
                }
                /// Recognizes one character and checks that it satisfies a predicate
                pub fn satisfy<F, I, Error: ParseError<I>>(cond: F) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                F: Fn(char) -> bool,
                {
                    move |i: I| match (i).iter_elements().next().map(|t| {
                        let c = t.as_char();
                        let b = cond(c);
                        (c, b)
                    }) {
                        Some((c, true)) => Ok((i.slice(c.len()..), c)),
                        _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::Satisfy))),
                    }
                }
                /// Recognizes one of the provided characters.
                pub fn one_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar + Copy,
                T: FindToken<<I as InputIter>::Item>,
                {
                    move |i: I| match (i).iter_elements().next().map(|c| (c, list.find_token(c))) {
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                        _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
                    }
                }
                /// Recognizes a character that is not in the provided characters.
                pub fn none_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar + Copy,
                T: FindToken<<I as InputIter>::Item>,
                {
                    move |i: I| match (i).iter_elements().next().map(|c| (c, !list.find_token(c))) {
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                        _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::NoneOf))),
                    }
                }
                /// Recognizes the string "\r\n".
                pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>>,
                T: InputIter,
                T: Compare<&'static str>,
                {
                    match input.compare("\r\n") {
                        //FIXME: is this the right index?
                        CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                        _ => {
                        let e: ErrorKind = ErrorKind::CrLf;
                        Err(Err::Error(E::from_error_kind(input, e)))
                        }
                    }
                }
                /// Recognizes a string of any char except '\r\n' or '\n'.
                pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: InputIter + InputLength,
                T: Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                <T as InputIter>::Item: AsChar,
                {
                    match input.position(|item| {
                        let c = item.as_char();
                        c == '\r' || c == '\n'
                    }) {
                        None => Ok((input.slice(input.input_len()..), input)),
                        Some(index) => {
                        let mut it = input.slice(index..).iter_elements();
                        let nth = it.next().unwrap().as_char();
                        if nth == '\r' {
                            let sliced = input.slice(index..);
                            let comp = sliced.compare("\r\n");
                            match comp {
                            //FIXME: calculate the right index
                            CompareResult::Ok => Ok((input.slice(index..), input.slice(..index))),
                            _ => {
                                let e: ErrorKind = ErrorKind::Tag;
                                Err(Err::Error(E::from_error_kind(input, e)))
                            }
                            }
                        } else {
                            Ok((input.slice(index..), input.slice(..index)))
                        }
                        }
                    }
                }
                /// Recognizes an end of line (both '\n' and '\r\n').
                pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: InputIter + InputLength,
                T: Compare<&'static str>,
                {
                    match input.compare("\n") {
                        CompareResult::Ok => Ok((input.slice(1..), input.slice(0..1))),
                        CompareResult::Incomplete => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                        CompareResult::Error => {
                        match input.compare("\r\n") {
                            //FIXME: is this the right index?
                            CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                            _ => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                        }
                        }
                    }
                }
                /// Matches a newline character '\n'.
                pub fn newline<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                {
                    char('\n')(input)
                }
                /// Matches a tab character '\t'.
                pub fn tab<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                {
                    char('\t')(input)
                }

                /// Matches one byte as a character. Note that the input type will
                /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
                pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E> where
                T: InputIter + InputLength + Slice<RangeFrom<usize>>,
                <T as InputIter>::Item: AsChar,
                {
                    let mut it = input.iter_indices();
                    match it.next() {
                        None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
                        Some((_, c)) => match it.next() {
                        None => Ok((input.slice(input.input_len()..), c.as_char())),
                        Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
                        },
                    }
                }
                /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_alpha())
                }
                /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_alpha(), ErrorKind::Alpha)
                }
                /// Recognizes zero or more ASCII numerical characters: 0-9
                pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_dec_digit())
                }
                /// Recognizes one or more ASCII numerical characters: 0-9
                pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_dec_digit(), ErrorKind::Digit)
                }
                /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_hex_digit())
                }
                /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_hex_digit(), ErrorKind::HexDigit)
                }
                /// Recognizes zero or more octal characters: 0-7
                pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_oct_digit())
                }
                /// Recognizes one or more octal characters: 0-7
                pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_oct_digit(), ErrorKind::OctDigit)
                }
                /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
                pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_alphanum())
                }
                /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
                pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric)
                }
                /// Recognizes zero or more spaces and tabs.
                pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position_complete(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    })
                }
                /// Recognizes one or more spaces and tabs.
                pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position1_complete(
                        |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                        },
                        ErrorKind::Space,
                    )
                }
                /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position_complete(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    })
                }
                /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position1_complete(
                        |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                        },
                        ErrorKind::MultiSpace,
                    )
                }

                pub fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E> where
                T: Clone + InputTake,
                T: for<'a> Compare<&'a [u8]>,
                {
                    let (i, opt_sign) = opt(alt((
                        value(false, tag(&b"-"[..])),
                        value(true, tag(&b"+"[..])),
                    )))(input)?;
                    let sign = opt_sign.unwrap_or(true);

                    Ok((i, sign))
                }

                ints! { i8 i16 i32 i64 i128 }

                uints! { u8 u16 u32 u64 u128 }
            }

            pub mod streaming
            {
                use ::
                {
                    ops::{Range, RangeFrom, RangeTo},
                    parsers::nom::
                    {
                       branch::alt,
                       bytes::streaming::tag,
                       combinator::{ opt, value },
                       error::ErrorKind,
                       error::ParseError,
                       internal::{Err, IResult, Needed},
                       traits::{ AsChar, Compare, CompareResult, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice, },
                    },
                    *
                };
                
                macro_rules! ints 
                {
                    ($($t:tt)+) => {
                        $(
                        /// will parse a number in text form to a number
                        ///
                        /// *Complete version*: can parse until the end of input.
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E>
                            where
                            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
                            <T as InputIter>::Item: AsChar,
                            T: for <'a> Compare<&'a[u8]>,
                            {
                            let (i, sign) = sign(input.clone())?;

                                if i.input_len() == 0 {
                                    return Err(Err::Incomplete(Needed::new(1)));
                                }

                                let mut value: $t = 0;
                                if sign {
                                    for (pos, c) in i.iter_indices() {
                                        match c.as_char().to_digit(10) {
                                            None => {
                                                if pos == 0 {
                                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                                } else {
                                                    return Ok((i.slice(pos..), value));
                                                }
                                            },
                                            Some(d) => match value.checked_mul(10).and_then(|v| v.checked_add(d as $t)) {
                                                None => return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))),
                                                Some(v) => value = v,
                                            }
                                        }
                                    }
                                } else {
                                    for (pos, c) in i.iter_indices() {
                                        match c.as_char().to_digit(10) {
                                            None => {
                                                if pos == 0 {
                                                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
                                                } else {
                                                    return Ok((i.slice(pos..), value));
                                                }
                                            },
                                            Some(d) => match value.checked_mul(10).and_then(|v| v.checked_sub(d as $t)) {
                                                None => return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit))),
                                                Some(v) => value = v,
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
                    ($($t:tt)+) => {
                        $(
                        /// will parse a number in text form to a number
                        ///
                        /// *Complete version*: can parse until the end of input.
                        pub fn $t<T, E: ParseError<T>>(input: T) -> IResult<T, $t, E>
                            where
                            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
                            <T as InputIter>::Item: AsChar,
                            {
                                let i = input;

                                if i.input_len() == 0 {
                                    return Err(Err::Incomplete(Needed::new(1)));
                                }

                                let mut value: $t = 0;
                                for (pos, c) in i.iter_indices() {
                                    match c.as_char().to_digit(10) {
                                        None => {
                                            if pos == 0 {
                                                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
                                            } else {
                                                return Ok((i.slice(pos..), value));
                                            }
                                        },
                                        Some(d) => match value.checked_mul(10).and_then(|v| v.checked_add(d as $t)) {
                                            None => return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit))),
                                            Some(v) => value = v,
                                        }
                                    }
                                }

                                Err(Err::Incomplete(Needed::new(1)))
                            }
                        )+
                    }
                }
                /// Recognizes one character.
                pub fn char<I, Error: ParseError<I>>(c: char) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter + InputLength,
                <I as InputIter>::Item: AsChar,
                {
                    move |i: I| match (i).iter_elements().next().map(|t| {
                        let b = t.as_char() == c;
                        (&c, b)
                    }) {
                        None => Err(Err::Incomplete(Needed::new(c.len() - i.input_len()))),
                        Some((_, false)) => Err(Err::Error(Error::from_char(i, c))),
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                    }
                }
                /// Recognizes one character and checks that it satisfies a predicate.
                pub fn satisfy<F, I, Error: ParseError<I>>(cond: F) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                F: Fn(char) -> bool,
                {
                    move |i: I| match (i).iter_elements().next().map(|t| {
                        let c = t.as_char();
                        let b = cond(c);
                        (c, b)
                    }) {
                        None => Err(Err::Incomplete(Needed::Unknown)),
                        Some((_, false)) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::Satisfy))),
                        Some((c, true)) => Ok((i.slice(c.len()..), c)),
                    }
                }
                /// Recognizes one of the provided characters.
                pub fn one_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar + Copy,
                T: FindToken<<I as InputIter>::Item>,
                {
                    move |i: I| match (i).iter_elements().next().map(|c| (c, list.find_token(c))) {
                        None => Err(Err::Incomplete(Needed::new(1))),
                        Some((_, false)) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                    }
                }
                /// Recognizes a character that is not in the provided characters.
                pub fn none_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar + Copy,
                T: FindToken<<I as InputIter>::Item>,
                {
                    move |i: I| match (i).iter_elements().next().map(|c| (c, !list.find_token(c))) {
                        None => Err(Err::Incomplete(Needed::new(1))),
                        Some((_, false)) => Err(Err::Error(Error::from_error_kind(i, ErrorKind::NoneOf))),
                        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                    }
                }
                /// Recognizes the string "\r\n".
                pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: InputIter,
                T: Compare<&'static str>,
                {
                    match input.compare("\r\n") {
                        CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                        CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                        CompareResult::Error => {
                        let e: ErrorKind = ErrorKind::CrLf;
                        Err(Err::Error(E::from_error_kind(input, e)))
                        }
                    }
                }
                /// Recognizes a string of any char except '\r\n' or '\n'.
                pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: InputIter + InputLength,
                T: Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                <T as InputIter>::Item: AsChar,
                {
                    match input.position(|item| {
                        let c = item.as_char();
                        c == '\r' || c == '\n'
                    }) {
                        None => Err(Err::Incomplete(Needed::Unknown)),
                        Some(index) => {
                        let mut it = input.slice(index..).iter_elements();
                        let nth = it.next().unwrap().as_char();
                        if nth == '\r' {
                            let sliced = input.slice(index..);
                            let comp = sliced.compare("\r\n");
                            match comp {
                            //FIXME: calculate the right index
                            CompareResult::Incomplete => Err(Err::Incomplete(Needed::Unknown)),
                            CompareResult::Error => {
                                let e: ErrorKind = ErrorKind::Tag;
                                Err(Err::Error(E::from_error_kind(input, e)))
                            }
                            CompareResult::Ok => Ok((input.slice(index..), input.slice(..index))),
                            }
                        } else {
                            Ok((input.slice(index..), input.slice(..index)))
                        }
                        }
                    }
                }
                /// Recognizes an end of line (both '\n' and '\r\n').
                pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: InputIter + InputLength,
                T: Compare<&'static str>,
                {
                    match input.compare("\n") {
                        CompareResult::Ok => Ok((input.slice(1..), input.slice(0..1))),
                        CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(1))),
                        CompareResult::Error => {
                        match input.compare("\r\n") {
                            //FIXME: is this the right index?
                            CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                            CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                            CompareResult::Error => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                        }
                        }
                    }
                }
                /// Matches a newline character '\\n'.
                pub fn newline<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter + InputLength,
                <I as InputIter>::Item: AsChar,
                {
                    char('\n')(input)
                }
                /// Matches a tab character '\t'.
                pub fn tab<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error> where
                I: Slice<RangeFrom<usize>> + InputIter + InputLength,
                <I as InputIter>::Item: AsChar,
                {
                    char('\t')(input)
                }
                /// Matches one byte as a character. Note that the input type will
                /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
                pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E> where
                T: InputIter + InputLength + Slice<RangeFrom<usize>>,
                <T as InputIter>::Item: AsChar,
                {
                    let mut it = input.iter_indices();
                    match it.next() {
                        None => Err(Err::Incomplete(Needed::new(1))),
                        Some((_, c)) => match it.next() {
                        None => Ok((input.slice(input.input_len()..), c.as_char())),
                        Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
                        },
                    }
                }
                /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_alpha())
                }
                /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_alpha(), ErrorKind::Alpha)
                }
                /// Recognizes zero or more ASCII numerical characters: 0-9.
                pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_dec_digit())
                }
                /// Recognizes one or more ASCII numerical characters: 0-9.
                pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_dec_digit(), ErrorKind::Digit)
                }
                /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_hex_digit())
                }
                /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
                pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_hex_digit(), ErrorKind::HexDigit)
                }
                /// Recognizes zero or more octal characters: 0-7.
                pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_oct_digit())
                }
                /// Recognizes one or more octal characters: 0-7.
                pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_oct_digit(), ErrorKind::OctDigit)
                }
                /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
                pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_alphanum())
                }
                /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
                pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric)
                }
                /// Recognizes zero or more spaces and tabs.
                pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    })
                }
                /// Recognizes one or more spaces and tabs.
                pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position1(
                        |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                        },
                        ErrorKind::Space,
                    )
                }
                /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    })
                }
                /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position1(
                        |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                        },
                        ErrorKind::MultiSpace,
                    )
                }

                pub fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E> where
                T: Clone + InputTake + InputLength,
                T: for<'a> Compare<&'a [u8]>,
                {
                    let (i, opt_sign) = opt(alt((
                        value(false, tag(&b"-"[..])),
                        value(true, tag(&b"+"[..])),
                    )))(input)?;
                    let sign = opt_sign.unwrap_or(true);

                    Ok((i, sign))
                }

                ints! { i8 i16 i32 i64 i128 }
                uints! { u8 u16 u32 u64 u128 }
            }

        }
        
        pub mod number
        {
            use ::
            {
                parsers::nom::
                {

                },
                *,
            };

            pub mod complete
            {
                use ::
                {
                    ops::{ Range, RangeFrom, RangeTo },
                    parsers::nom::
                    {
                        branch::alt,
                        bytes::complete::{ is_a, tag, tag_no_case },
                        character::complete::{char, digit1, sign},
                        combinator::{cut, map, opt, recognize},
                        error::{ ErrorKind, make_error, ParseError },
                        internal::*,
                        number::{ Endianness },
                        sequence::{pair, tuple},
                        traits::{ AsBytes, AsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, ParseTo, Slice },
                    },
                    *
                };
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn be_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 2 bytes integer.
                #[inline] pub fn be_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 2;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u16;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u16;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 3 byte integer.
                #[inline] pub fn be_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 3;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u32;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u32;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 4 bytes integer.
                #[inline] pub fn be_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 4;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u32;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u32;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 8 bytes integer.
                #[inline] pub fn be_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 8;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u64;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u64;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 16 bytes integer.
                #[inline] pub fn be_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 16;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u128;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u128;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a signed 1 byte integer.
                #[inline] pub fn be_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a big endian signed 2 bytes integer.
                #[inline] pub fn be_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a big endian signed 3 bytes integer.
                #[inline] pub fn be_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    // Same as the unsigned version but we need to sign-extend manually here
                    be_u24
                        .map(|x| {
                        if x & 0x80_00_00 != 0 {
                            (x | 0xff_00_00_00) as i32
                        } else {
                            x as i32
                        }
                        })
                        .parse(input)
                }
                /// Recognizes a big endian signed 4 bytes integer.
                #[inline] pub fn be_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a big endian signed 8 bytes integer.
                #[inline] pub fn be_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a big endian signed 16 bytes integer.
                #[inline] pub fn be_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn le_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 2 bytes integer.
                #[inline] pub fn le_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 2;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u16;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u16) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 3 byte integer.
                #[inline] pub fn le_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 3;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u32;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u32) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 4 bytes integer.
                #[inline] pub fn le_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 4;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u32;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u32) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 8 bytes integer.
                #[inline] pub fn le_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 8;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u64;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u64) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 16 bytes integer.
                #[inline] pub fn le_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 16;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let mut res = 0u128;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u128) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a signed 1 byte integer.
                #[inline] pub fn le_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a little endian signed 2 bytes integer.
                #[inline] pub fn le_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a little endian signed 3 bytes integer.
                #[inline] pub fn le_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u24
                    .map(|x| {
                    if x & 0x80_00_00 != 0 {
                        (x | 0xff_00_00_00) as i32
                    } else {
                        x as i32
                    }
                    })
                    .parse(input)
                }
                /// Recognizes a little endian signed 4 bytes integer.
                #[inline] pub fn le_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                le_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a little endian signed 8 bytes integer.
                #[inline] pub fn le_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a little endian signed 16 bytes integer.
                #[inline] pub fn le_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer
                #[inline] pub fn u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Error(make_error(input, ErrorKind::Eof)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes an unsigned 2 bytes integer
                #[inline] pub fn u16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u16,
                        Endianness::Little => le_u16,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u16,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u16,
                    }
                }
                /// Recognizes an unsigned 3 byte integer
                #[inline] pub fn u24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u24,
                        Endianness::Little => le_u24,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u24,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u24,
                    }
                }
                /// Recognizes an unsigned 4 byte integer
                #[inline] pub fn u32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u32,
                        Endianness::Little => le_u32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u32,
                    }
                }
                /// Recognizes an unsigned 8 byte integer
                #[inline] pub fn u64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u64,
                        Endianness::Little => le_u64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u64,
                    }
                }
                /// Recognizes an unsigned 16 byte integer.
                #[inline] pub fn u128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u128,
                        Endianness::Little => le_u128,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u128,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u128,
                    }
                }
                /// Recognizes a signed 1 byte integer
                #[inline] pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    u8.map(|x| x as i8).parse(i)
                }
                /// Recognizes a signed 2 byte integer
                #[inline] pub fn i16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i16,
                        Endianness::Little => le_i16,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i16,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i16,
                    }
                }
                /// Recognizes a signed 3 byte integer.
                #[inline] pub fn i24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i24,
                        Endianness::Little => le_i24,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i24,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i24,
                    }
                }
                /// Recognizes a signed 4 byte integer.
                #[inline] pub fn i32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i32,
                        Endianness::Little => le_i32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i32,
                    }
                }
                /// Recognizes a signed 8 byte integer
                #[inline] pub fn i64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i64,
                        Endianness::Little => le_i64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i64,
                    }
                }
                /// Recognizes a signed 16 byte integer
                #[inline] pub fn i128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i128,
                        Endianness::Little => le_i128,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i128,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i128,
                    }
                }
                /// Recognizes a big endian 4 bytes floating point number.
                #[inline] pub fn be_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a big endian 8 bytes floating point number.
                #[inline] pub fn be_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 4 bytes floating point number.
                #[inline] pub fn le_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 8 bytes floating point number.
                #[inline] pub fn le_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a 4 byte floating point number
                #[inline] pub fn f32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_f32,
                        Endianness::Little => le_f32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_f32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_f32,
                    }
                }
                /// Recognizes an 8 byte floating point number
                #[inline] pub fn f64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_f64,
                        Endianness::Little => le_f64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_f64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_f64,
                    }
                }
                /// Recognizes a hex-encoded integer.
                #[inline] pub fn hex_u32<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], u32, E>
                {
                    let (i, o) = is_a(&b"0123456789abcdefABCDEF"[..])(input)?;
                    
                    let (parsed, remaining) = if o.len() <= 8 {
                        (o, i)
                    } else {
                        (&input[..8], &input[8..])
                    };

                    let res = parsed
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
                pub fn recognize_float<T, E:ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter,
                <T as InputIter>::Item: AsChar,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    recognize(
                        tuple((
                        opt(alt((char('+'), char('-')))),
                        alt((
                            map(tuple((digit1, opt(pair(char('.'), opt(digit1))))), |_| ()),
                            map(tuple((char('.'), digit1)), |_| ())
                        )),
                        opt(tuple((
                            alt((char('e'), char('E'))),
                            opt(alt((char('+'), char('-')))),
                            cut(digit1)
                        )))
                        ))
                    )(input)
                }
                
                pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter + InputTake + Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    alt((
                        |i: T| {
                        recognize_float::<_, E>(i.clone()).map_err(|e| match e {
                            ::parsers::nom::Err::Error(_) => ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)),
                            ::parsers::nom::Err::Failure(_) => ::parsers::nom::Err::Failure(E::from_error_kind(i, ErrorKind::Float)),
                            ::parsers::nom::Err::Incomplete(needed) => ::parsers::nom::Err::Incomplete(needed),
                        })
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("nan")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("inf")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("infinity")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                    ))(input)
                }
                /// Recognizes a floating point number in text format
                pub fn recognize_float_parts<T, E: ParseError<T>>(input: T) -> IResult<T, (bool, T, T, i32), E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
                T: Clone + Offset,
                T: InputIter + InputTake,
                <T as InputIter>::Item: AsChar + Copy,
                T: InputTakeAtPosition + InputLength,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: for<'a> Compare<&'a [u8]>,
                T: AsBytes,
                {
                    let (i, sign) = sign(input.clone())?;

                    //let (i, zeroes) = take_while(|c: <T as InputTakeAtPosition>::Item| c.as_char() == '0')(i)?;
                    let (i, zeroes) = match i.as_bytes().iter().position(|c| *c != b'0') {
                        Some(index) => i.take_split(index),
                        None => i.take_split(i.input_len()),
                    };
                    //let (i, mut integer) = digit0(i)?;
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
                        integer = zeroes.slice(zeroes.input_len() - 1..);
                    }

                    let (i, opt_dot) = opt(tag(&b"."[..]))(i)?;
                    let (i, fraction) = if opt_dot.is_none() {
                        let i2 = i.clone();
                        (i2, i.slice(..0))
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

                        let position = position.unwrap_or(i.input_len());

                        let index = if zero_count == 0 {
                        position
                        } else if zero_count == position {
                        position - zero_count + 1
                        } else {
                        position - zero_count
                        };

                        (i.slice(position..), i.slice(..index))
                    };

                    if integer.input_len() == 0 && fraction.input_len() == 0 {
                        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Float)));
                    }

                    let i2 = i.clone();
                    let (i, e) = match i.as_bytes().iter().next() {
                        Some(b'e') => (i.slice(1..), true),
                        Some(b'E') => (i.slice(1..), true),
                        _ => (i, false),
                    };

                    let (i, exp) = if e {
                        cut( ::parsers::nom::character::complete::i32)(i)?
                    } else {
                        (i2, 0)
                    };

                    Ok((i, (sign, integer, fraction, exp)))
                }
                /// Recognizes floating point number in text format and returns a f32.
                pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
                T: Clone + Offset + ParseTo<f32> + Compare<&'static str>,
                T: InputIter + InputLength + InputTake,
                <T as InputIter>::Item: AsChar + Copy,
                <T as InputIter>::IterElem: Clone,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: AsBytes,
                T: for<'a> Compare<&'a [u8]>,
                {
                    let (i, s) = recognize_float_or_exceptions(input)?;
                    match s.parse_to() {
                        Some(f) => Ok((i, f)),
                        None => Err(::parsers::nom::Err::Error(E::from_error_kind(
                        i,
                        ErrorKind::Float,
                        ))),
                    }
                }
                /// Recognizes floating point number in text format and returns a f64.
                pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
                T: Clone + Offset + ParseTo<f64> + Compare<&'static str>,
                T: InputIter + InputLength + InputTake,
                <T as InputIter>::Item: AsChar + Copy,
                <T as InputIter>::IterElem: Clone,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: AsBytes,
                T: for<'a> Compare<&'a [u8]>,
                {
                    let (i, s) = recognize_float_or_exceptions(input)?;
                    match s.parse_to() {
                        Some(f) => Ok((i, f)),
                        None => Err( ::parsers::nom::Err::Error(E::from_error_kind(
                        i,
                        ErrorKind::Float,
                        ))),
                    }
                }

            }

            pub mod streaming
            {
                //! Parsers recognizing numbers, streaming version
                use ::
                {
                    ops::{RangeFrom, RangeTo},
                    parsers::nom::
                    {
                        branch::alt,
                        bytes::streaming::{ is_a, tag, tag_no_case },
                        character::streaming::{char, digit1, sign},
                        combinator::{cut, map, opt, recognize},
                        error::{ErrorKind, ParseError},
                        internal::*,
                        number::{ Endianness },
                        sequence::{pair, tuple},
                        traits::{ AsBytes, AsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, ParseTo, Slice },

                    },
                    *
                };
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn be_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(1)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 2 bytes integer.
                #[inline] pub fn be_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 2;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u16;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u16;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 3 byte integer.
                #[inline] pub fn be_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 3;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u32;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u32;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 4 bytes integer.
                #[inline] pub fn be_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 4;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u32;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u32;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 8 bytes integer.
                #[inline] pub fn be_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 8;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u64;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u64;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a big endian unsigned 16 bytes integer.
                #[inline] pub fn be_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 16;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u128;
                        for byte in input.iter_elements().take(bound) {
                        res = (res << 8) + byte as u128;
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a signed 1 byte integer.
                #[inline] pub fn be_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a big endian signed 2 bytes integer.
                #[inline] pub fn be_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a big endian signed 3 bytes integer.
                #[inline] pub fn be_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u24
                        .map(|x| {
                        if x & 0x80_00_00 != 0 {
                            (x | 0xff_00_00_00) as i32
                        } else {
                            x as i32
                        }
                        })
                        .parse(input)
                }
                /// Recognizes a big endian signed 4 bytes integer.
                #[inline] pub fn be_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a big endian signed 8 bytes integer.
                #[inline] pub fn be_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a big endian signed 16 bytes integer.
                #[inline] pub fn be_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn le_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(1)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 2 bytes integer.
                #[inline] pub fn le_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 2;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u16;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u16) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 3 bytes integer.
                #[inline] pub fn le_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 3;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u32;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u32) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 4 bytes integer.
                #[inline] pub fn le_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 4;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u32;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u32) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 8 bytes integer.
                #[inline] pub fn le_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 8;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u64;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u64) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a little endian unsigned 16 bytes integer.
                #[inline] pub fn le_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 16;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(bound - input.input_len())))
                    } else {
                        let mut res = 0u128;
                        for (index, byte) in input.iter_indices().take(bound) {
                        res += (byte as u128) << (8 * index);
                        }

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes a signed 1 byte integer.
                #[inline] pub fn le_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a little endian signed 2 bytes integer.
                #[inline] pub fn le_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a little endian signed 3 bytes integer.
                #[inline] pub fn le_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u24
                        .map(|x| {
                        if x & 0x80_00_00 != 0 {
                            (x | 0xff_00_00_00) as i32
                        } else {
                            x as i32
                        }
                        })
                        .parse(input)
                }
                /// Recognizes a little endian signed 4 bytes integer.
                #[inline] pub fn le_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a little endian signed 8 bytes integer.
                #[inline] pub fn le_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a little endian signed 16 bytes integer.
                #[inline] pub fn le_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer
                #[inline] pub fn u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    let bound: usize = 1;
                    if input.input_len() < bound {
                        Err(Err::Incomplete(Needed::new(1)))
                    } else {
                        let res = input.iter_elements().next().unwrap();

                        Ok((input.slice(bound..), res))
                    }
                }
                /// Recognizes an unsigned 2 bytes integer
                #[inline] pub fn u16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u16,
                        Endianness::Little => le_u16,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u16,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u16,
                    }
                }
                /// Recognizes an unsigned 3 byte integer
                #[inline] pub fn u24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u24,
                        Endianness::Little => le_u24,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u24,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u24,
                    }
                }
                /// Recognizes an unsigned 4 byte integer.
                #[inline] pub fn u32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u32,
                        Endianness::Little => le_u32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u32,
                    }
                }
                /// Recognizes an unsigned 8 byte integer
                #[inline] pub fn u64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u64,
                        Endianness::Little => le_u64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u64,
                    }
                }
                /// Recognizes an unsigned 16 byte integer
                #[inline] pub fn u128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_u128,
                        Endianness::Little => le_u128,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_u128,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_u128,
                    }
                }
                /// Recognizes a signed 1 byte integer
                #[inline] pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    u8.map(|x| x as i8).parse(i)
                }
                /// Recognizes a signed 2 byte integer
                #[inline] pub fn i16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i16, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i16,
                        Endianness::Little => le_i16,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i16,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i16,
                    }
                }
                /// Recognizes a signed 3 byte integer.
                #[inline] pub fn i24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i24,
                        Endianness::Little => le_i24,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i24,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i24,
                    }
                }
                /// Recognizes a signed 4 byte integer
                #[inline] pub fn i32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i32,
                        Endianness::Little => le_i32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i32,
                    }
                }
                /// Recognizes a signed 8 byte integer
                #[inline] pub fn i64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i64,
                        Endianness::Little => le_i64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i64,
                    }
                }
                /// Recognizes a signed 16 byte integer.
                #[inline] pub fn i128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i128, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_i128,
                        Endianness::Little => le_i128,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_i128,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_i128,
                    }
                }
                /// Recognizes a big endian 4 bytes floating point number.
                #[inline] pub fn be_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a big endian 8 bytes floating point number.
                #[inline] pub fn be_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 4 bytes floating point number.
                #[inline] pub fn le_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 8 bytes floating point number.
                #[inline] pub fn le_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a 4 byte floating point number
                #[inline] pub fn f32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f32, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_f32,
                        Endianness::Little => le_f32,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_f32,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_f32,
                    }
                }
                /// Recognizes an 8 byte floating point number
                #[inline] pub fn f64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f64, E> where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match endian {
                        Endianness::Big => be_f64,
                        Endianness::Little => le_f64,
                        #[cfg(target_endian = "big")]
                        Endianness::Native => be_f64,
                        #[cfg(target_endian = "little")]
                        Endianness::Native => le_f64,
                    }
                }
                /// Recognizes a hex-encoded integer.
                #[inline] pub fn hex_u32<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], u32, E>
                {
                    let (i, o) = is_a(&b"0123456789abcdefABCDEF"[..])(input)?;
                    
                    let (parsed, remaining) = if o.len() <= 8 {
                        (o, i)
                    } else {
                        (&input[..8], &input[8..])
                    };

                    let res = parsed
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
                /// Recognizes a floating point number in text format and returns the corresponding part of the input.
                pub fn recognize_float<T, E:ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter,
                <T as InputIter>::Item: AsChar,
                T: InputTakeAtPosition + InputLength,
                <T as InputTakeAtPosition>::Item: AsChar
                {
                    recognize(
                        tuple((
                        opt(alt((char('+'), char('-')))),
                        alt((
                            map(tuple((digit1, opt(pair(char('.'), opt(digit1))))), |_| ()),
                            map(tuple((char('.'), digit1)), |_| ())
                        )),
                        opt(tuple((
                            alt((char('e'), char('E'))),
                            opt(alt((char('+'), char('-')))),
                            cut(digit1)
                        )))
                        ))
                    )(input)
                }
                
                pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(input: T) -> IResult<T, T, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter + InputTake + InputLength + Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    alt((
                        |i: T| {
                        recognize_float::<_, E>(i.clone()).map_err(|e| match e {
                            ::parsers::nom::Err::Error(_) => ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)),
                            ::parsers::nom::Err::Failure(_) => ::parsers::nom::Err::Failure(E::from_error_kind(i, ErrorKind::Float)),
                            ::parsers::nom::Err::Incomplete(needed) => ::parsers::nom::Err::Incomplete(needed),
                        })
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("nan")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("inf")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                        |i: T| {
                        tag_no_case::<_, _, E>("infinity")(i.clone())
                            .map_err(|_| ::parsers::nom::Err::Error(E::from_error_kind(i, ErrorKind::Float)))
                        },
                    ))(input)
                }
                /// Recognizes a floating point number in text format
                pub fn recognize_float_parts<T, E: ParseError<T>>(input: T) -> IResult<T, (bool, T, T, i32), E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter + ParseTo<i32>,
                <T as InputIter>::Item: AsChar,
                T: InputTakeAtPosition + InputTake + InputLength,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: for<'a> Compare<&'a [u8]>,
                T: AsBytes,
                {
                    let (i, sign) = sign(input.clone())?;

                    //let (i, zeroes) = take_while(|c: <T as InputTakeAtPosition>::Item| c.as_char() == '0')(i)?;
                    let (i, zeroes) = match i.as_bytes().iter().position(|c| *c != b'0') {
                        Some(index) => i.take_split(index),
                        None => i.take_split(i.input_len()),
                    };

                    //let (i, mut integer) = digit0(i)?;
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
                        integer = zeroes.slice(zeroes.input_len() - 1..);
                    }

                    let (i, opt_dot) = opt(tag(&b"."[..]))(i)?;
                    let (i, fraction) = if opt_dot.is_none() {
                        let i2 = i.clone();
                        (i2, i.slice(..0))
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

                        let position = match position {
                        Some(p) => p,
                        None => return Err(Err::Incomplete(Needed::new(1))),
                        };

                        let index = if zero_count == 0 {
                        position
                        } else if zero_count == position {
                        position - zero_count + 1
                        } else {
                        position - zero_count
                        };

                        (i.slice(position..), i.slice(..index))
                    };

                    if integer.input_len() == 0 && fraction.input_len() == 0 {
                        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Float)));
                    }

                    let i2 = i.clone();
                    let (i, e) = match i.as_bytes().iter().next() {
                        Some(b'e') => (i.slice(1..), true),
                        Some(b'E') => (i.slice(1..), true),
                        _ => (i, false),
                    };

                    let (i, exp) = if e {
                        cut( ::parsers::nom::character::streaming::i32)(i)?
                    } else {
                        (i2, 0)
                    };

                    Ok((i, (sign, integer, fraction, exp)))
                }
                /// Recognizes floating point number in text format and returns a f32.
                pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter + InputLength + InputTake + ParseTo<f32> + Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                <T as InputIter>::IterElem: Clone,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: AsBytes,
                T: for<'a> Compare<&'a [u8]>,
                { 
                    let (i, s) = recognize_float_or_exceptions(input)?;
                    match s.parse_to() {
                        Some(f) => Ok((i, f)),
                        None => Err(::parsers::nom::Err::Error(E::from_error_kind(
                        i,
                        ErrorKind::Float,
                        ))),
                    }
                }
                /// Recognizes floating point number in text format and returns a f64.
                pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E> where
                T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
                T: Clone + Offset,
                T: InputIter + InputLength + InputTake + ParseTo<f64> + Compare<&'static str>,
                <T as InputIter>::Item: AsChar,
                <T as InputIter>::IterElem: Clone,
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                T: AsBytes,
                T: for<'a> Compare<&'a [u8]>,
                {
                    let (i, s) = recognize_float_or_exceptions(input)?;
                    match s.parse_to() {
                        Some(f) => Ok((i, f)),
                        None => Err( parsers::nom::Err::Error(E::from_error_kind(
                        i,
                        ErrorKind::Float,
                        ))),
                    }
                }
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
        }
    }
}

pub mod path
{
    pub use std::path::{ * };
}

pub mod rc
{
    pub use std::rc::{ * };
}

pub mod result
{
    pub use std::result::{ * };
}

pub mod slice
{
    pub use std::slice::{ * };
}

pub mod str
{
    pub use std::str::{ * };    
    use ::
    {
        fs::{ File },
        io::{ self, Read, Write },
        *,
    };
    /*
    pub fn write_file_str(...) -> io::Result<()> */
    /// Writes a strand to a file.
    pub fn write_file(fname: &str, contents: &str) -> io::Result<()>
    {
        let mut file = File::create(fname)?;
        file.write_all(contents.as_bytes())?;
        Ok(())
    }
    /*
    pub fn read_file_str(...) -> io::Result<String> */
    /// Reads a file and returns its contents in a string.
    pub fn read_file(fname: &str) -> io::Result<String>
    {
        // Open a file in read-only mode
        let mut file = File::open(fname)?;

        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents)?;

        Ok(contents)
    }
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod sync
{
    pub use std::sync::{ * };
}
/*
linefeed*/
pub mod system
{
    use ::
    {
        *,
    };
    
}

pub mod vec
{
    pub use std::vec::{ * };
}

fn main()
{
    
}
// #\[stable\(feature = ".+", since = ".+"\)\]
// #\[unstable\(feature = ".+", issue = ".+"\)\]
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 24003
