#![allow
(
    non_camel_case_types,
    stable_features,
    unknown_lints,
    unused_imports,
    unused_macros,
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
extern crate phf;
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
        use ::
        {
            *,
        };
    } pub use self::arrays::Arr;

    pub mod error
    {
        use ::
        {
            *,
        };
    } pub use self::error::OverError;

    pub mod objects
    {
        use ::
        {
            *,
        };
    } pub use self::objects::Obj;

    pub mod tuples
    {
        use ::
        {
            *,
        };
    } pub use self::tuples::Tup;

    pub mod types
    {
        use ::
        {
            *,
        };
    }

    pub mod values
    {
        use ::
        {
            *,
        };
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
                        ch if is::digit(ch) => {
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
                use crate::database::types::Type::*;

                let (mut type1, mut type2) = (val1.get_type(), val2.get_type());

                // If one value is an Int and the other is a Frac, promote the Int.
                if type1 == Int && type2 == Frac {
                    val1 = Value::Frac(BigRational::new(val1.get_int().unwrap(), 1.into()));
                    type1 = Frac;
                } else if type1 == Frac && type2 == Int {
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
                    '-' => match type1 {
                        Int if type2 == Int => (val1.get_int().unwrap() - val2.get_int().unwrap()).into(),
                        Frac if type2 == Frac => (val1.get_frac().unwrap() - val2.get_frac().unwrap()).into(),
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    },
                    '*' => match type1 {
                        Int if type2 == Int => (val1.get_int().unwrap() * val2.get_int().unwrap()).into(),
                        Frac if type2 == Frac => (val1.get_frac().unwrap() * val2.get_frac().unwrap()).into(),
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    },
                    '/' => match type1 {
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
                    },
                    '%' => match type1 {
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
                    _ => {
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
                        ch if is_value_end_char(ch) => 
                        {
                            if is_end_delimiter(ch) && Some(ch) != cur_brace 
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
            _ch if is_digit(_ch) => true,
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
        use ::
        {
            *,
        };
    }

    pub mod fractional
    {
        use ::
        {
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
        use ::
        {
            *,
        };
    }

    pub mod traits
    {
        pub use ::num_traits::{ * };
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
            /// The Display implementation allows the std::error::Error implementation
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
            
            impl<I: fmt::Debug + fmt::Display> std::error::Error for VerboseError<I> {}

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
                pub fn map<E2, F>(self, f: F) -> Err<E2>
                where
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
                pub fn map_input<U, F>(self, f: F) -> Err<(U, ErrorKind)>
                where
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
                pub fn map_input<U, F>(self, f: F) -> Err<error::Error<U>>
                where
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
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                fn map<G, O2>(self, g: G) -> Map<Self, G, O>
                where
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
                fn flat_map<G, H, O2>(self, g: G) -> FlatMap<Self, G, O>
                where
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
                fn and_then<G, O2>(self, g: G) -> AndThen<Self, G, O>
                where
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
                fn and<G, O2>(self, g: G) -> And<Self, G>
                where
                    G: Parser<I, O2, E>,
                    Self: Sized,
                {
                    And { f: self, g }
                }

                /// Applies a second parser over the input if the first one failed
                fn or<G>(self, g: G) -> Or<Self, G>
                where
                    G: Parser<I, O, E>,
                    Self: Sized,
                {
                    Or { f: self, g }
                }

                /// automatically converts the parser's output and error values to another type, as long as they
                /// implement the `From` trait
                fn into<O2: From<O>, E2: From<E>>(self) -> Into<Self, O, O2, E, E2>
                where
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
                fn position<P>(&self, predicate: P) -> Option<usize>
                where
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
                #[inline] fn position<P>(&self, predicate: P) -> Option<usize>
                where
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
                fn position<P>(&self, predicate: P) -> Option<usize>
                where
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
                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
                where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true
                /// and returns the input up to this position.
                fn split_at_position1<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E>
                where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true,
                /// and returns the input up to this position.
                fn split_at_position_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                ) -> IResult<Self, Self, E>
                where
                    P: Fn(Self::Item) -> bool;

                /// Looks for the first element of the input type for which the condition returns true
                /// and returns the input up to this position.
                fn split_at_position1_complete<P, E: ParseError<Self>>(
                    &self,
                    predicate: P,
                    e: ErrorKind,
                ) -> IResult<Self, Self, E>
                where
                    P: Fn(Self::Item) -> bool;
            }

            impl<T: InputLength + InputIter + InputTake + Clone + UnspecializedInput> InputTakeAtPosition
            for T
            {
                type Item = <T as InputIter>::Item;

                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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

                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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

                fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> IResult<Self, Self, E>
                where
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
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
                where
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
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
                where
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
                pub fn bool<I, E: ParseError<(I, usize)>>(input: (I, usize)) -> IResult<(I, usize), bool, E>
                where
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
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
                where
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
                ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
                where
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
                pub fn bool<I, E: ParseError<(I, usize)>>(input: (I, usize)) -> IResult<(I, usize), bool, E>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position_complete(|c| !cond(c))
                }
                /// Returns the longest (at least 1) input slice that matches the predicate.
                pub fn take_while1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position_complete(|c| cond(c))
                }
                /// Returns the longest (at least 1) input slice till a predicate is met.
                pub fn take_till1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl FnMut(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl FnMut(Input) -> IResult<Input, Output, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position(|c| !cond(c))
                }
                /// Returns the longest (at least 1) input slice that matches the predicate.
                pub fn take_while1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
                Input: InputTakeAtPosition,
                F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
                {
                    move |i: Input| i.split_at_position(|c| cond(c))
                }
                /// Returns the longest (at least 1) input slice till a predicate is met.
                pub fn take_till1<F, Input, Error: ParseError<Input>>(
                cond: F,
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl Fn(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl FnMut(Input) -> IResult<Input, Input, Error>
                where
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
                ) -> impl FnMut(Input) -> IResult<Input, Output, Error>
                where
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
                pub fn char<I, Error: ParseError<I>>(c: char) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn satisfy<F, I, Error: ParseError<I>>(cond: F) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn one_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn none_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn newline<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error>
                where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                {
                    char('\n')(input)
                }
                /// Matches a tab character '\t'.
                pub fn tab<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error>
                where
                I: Slice<RangeFrom<usize>> + InputIter,
                <I as InputIter>::Item: AsChar,
                {
                    char('\t')(input)
                }

                /// Matches one byte as a character. Note that the input type will
                /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
                pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
                where
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
                pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_alpha())
                }
                /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_alpha(), ErrorKind::Alpha)
                }
                /// Recognizes zero or more ASCII numerical characters: 0-9
                pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_dec_digit())
                }
                /// Recognizes one or more ASCII numerical characters: 0-9
                pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_dec_digit(), ErrorKind::Digit)
                }
                /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_hex_digit())
                }
                /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_hex_digit(), ErrorKind::HexDigit)
                }
                /// Recognizes zero or more octal characters: 0-7
                pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_oct_digit())
                }
                /// Recognizes one or more octal characters: 0-7
                pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_oct_digit(), ErrorKind::OctDigit)
                }
                /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
                pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position_complete(|item| !item.is_alphanum())
                }
                /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
                pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1_complete(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric)
                }
                /// Recognizes zero or more spaces and tabs.
                pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position_complete(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    })
                }
                /// Recognizes one or more spaces and tabs.
                pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position_complete(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    })
                }
                /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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

                pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E>
                where
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
                pub fn char<I, Error: ParseError<I>>(c: char) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn satisfy<F, I, Error: ParseError<I>>(cond: F) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn one_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn none_of<I, T, Error: ParseError<I>>(list: T) -> impl Fn(I) -> IResult<I, char, Error>
                where
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
                pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn newline<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error>
                where
                I: Slice<RangeFrom<usize>> + InputIter + InputLength,
                <I as InputIter>::Item: AsChar,
                {
                    char('\n')(input)
                }
                /// Matches a tab character '\t'.
                pub fn tab<I, Error: ParseError<I>>( input:I ) -> IResult<I, char, Error>
                where
                I: Slice<RangeFrom<usize>> + InputIter + InputLength,
                <I as InputIter>::Item: AsChar,
                {
                    char('\t')(input)
                }
                /// Matches one byte as a character. Note that the input type will
                /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
                pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
                where
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
                pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_alpha())
                }
                /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z.
                pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_alpha(), ErrorKind::Alpha)
                }
                /// Recognizes zero or more ASCII numerical characters: 0-9.
                pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_dec_digit())
                }
                /// Recognizes one or more ASCII numerical characters: 0-9.
                pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_dec_digit(), ErrorKind::Digit)
                }
                /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
                pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_hex_digit())
                }
                /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f.
                pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_hex_digit(), ErrorKind::HexDigit)
                }
                /// Recognizes zero or more octal characters: 0-7.
                pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_oct_digit())
                }
                /// Recognizes one or more octal characters: 0-7.
                pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_oct_digit(), ErrorKind::OctDigit)
                }
                /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
                pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position(|item| !item.is_alphanum())
                }
                /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z.
                pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar,
                {
                    input.split_at_position1(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric)
                }
                /// Recognizes zero or more spaces and tabs.
                pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    })
                }
                /// Recognizes one or more spaces and tabs.
                pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
                T: InputTakeAtPosition,
                <T as InputTakeAtPosition>::Item: AsChar + Clone,
                {
                    input.split_at_position(|item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    })
                }
                /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
                pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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

                pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E>
                where
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
                #[inline] pub fn be_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn be_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn be_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn be_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn be_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn be_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn be_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a big endian signed 2 bytes integer.
                #[inline] pub fn be_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a big endian signed 3 bytes integer.
                #[inline] pub fn be_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn be_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a big endian signed 8 bytes integer.
                #[inline] pub fn be_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a big endian signed 16 bytes integer.
                #[inline] pub fn be_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn le_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn le_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn le_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn le_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn le_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn le_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn le_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a little endian signed 2 bytes integer.
                #[inline] pub fn le_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a little endian signed 3 bytes integer.
                #[inline] pub fn le_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn le_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                le_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a little endian signed 8 bytes integer.
                #[inline] pub fn le_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a little endian signed 16 bytes integer.
                #[inline] pub fn le_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer
                #[inline] pub fn u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn u16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn u24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn u32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn u64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn u128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    u8.map(|x| x as i8).parse(i)
                }
                /// Recognizes a signed 2 byte integer
                #[inline] pub fn i16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i16, E>
                where
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
                #[inline] pub fn i24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn i32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn i64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i64, E>
                where
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
                #[inline] pub fn i128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i128, E>
                where
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
                #[inline] pub fn be_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a big endian 8 bytes floating point number.
                #[inline] pub fn be_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 4 bytes floating point number.
                #[inline] pub fn le_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 8 bytes floating point number.
                #[inline] pub fn le_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a 4 byte floating point number
                #[inline] pub fn f32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f32, E>
                where
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
                #[inline] pub fn f64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f64, E>
                where
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
                pub fn recognize_float<T, E:ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                
                pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn recognize_float_parts<T, E: ParseError<T>>(input: T) -> IResult<T, (bool, T, T, i32), E>
                where
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
                pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E>
                where
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
                pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E>
                where
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
                #[inline] pub fn be_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn be_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn be_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn be_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn be_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn be_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn be_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a big endian signed 2 bytes integer.
                #[inline] pub fn be_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a big endian signed 3 bytes integer.
                #[inline] pub fn be_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn be_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a big endian signed 8 bytes integer.
                #[inline] pub fn be_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a big endian signed 16 bytes integer.
                #[inline] pub fn be_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    be_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer.
                #[inline] pub fn le_u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn le_u16<I, E: ParseError<I>>( input:I ) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn le_u24<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn le_u32<I, E: ParseError<I>>( input:I ) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn le_u64<I, E: ParseError<I>>( input:I ) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn le_u128<I, E: ParseError<I>>( input:I ) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn le_i8<I, E: ParseError<I>>( input:I ) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u8.map(|x| x as i8).parse(input)
                }
                /// Recognizes a little endian signed 2 bytes integer.
                #[inline] pub fn le_i16<I, E: ParseError<I>>( input:I ) -> IResult<I, i16, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u16.map(|x| x as i16).parse(input)
                }
                /// Recognizes a little endian signed 3 bytes integer.
                #[inline] pub fn le_i24<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn le_i32<I, E: ParseError<I>>( input:I ) -> IResult<I, i32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u32.map(|x| x as i32).parse(input)
                }
                /// Recognizes a little endian signed 8 bytes integer.
                #[inline] pub fn le_i64<I, E: ParseError<I>>( input:I ) -> IResult<I, i64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u64.map(|x| x as i64).parse(input)
                }
                /// Recognizes a little endian signed 16 bytes integer.
                #[inline] pub fn le_i128<I, E: ParseError<I>>( input:I ) -> IResult<I, i128, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    le_u128.map(|x| x as i128).parse(input)
                }
                /// Recognizes an unsigned 1 byte integer
                #[inline] pub fn u8<I, E: ParseError<I>>( input:I ) -> IResult<I, u8, E>
                where
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
                #[inline] pub fn u16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u16, E>
                where
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
                #[inline] pub fn u24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn u32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u32, E>
                where
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
                #[inline] pub fn u64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u64, E>
                where
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
                #[inline] pub fn u128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, u128, E>
                where
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
                #[inline] pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    u8.map(|x| x as i8).parse(i)
                }
                /// Recognizes a signed 2 byte integer
                #[inline] pub fn i16<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i16, E>
                where
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
                #[inline] pub fn i24<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn i32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i32, E>
                where
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
                #[inline] pub fn i64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i64, E>
                where
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
                #[inline] pub fn i128<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, i128, E>
                where
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
                #[inline] pub fn be_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a big endian 8 bytes floating point number.
                #[inline] pub fn be_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match be_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 4 bytes floating point number.
                #[inline] pub fn le_f32<I, E: ParseError<I>>( input:I ) -> IResult<I, f32, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u32(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f32::from_bits(o))),
                    }
                }
                /// Recognizes a little endian 8 bytes floating point number.
                #[inline] pub fn le_f64<I, E: ParseError<I>>( input:I ) -> IResult<I, f64, E>
                where
                I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
                {
                    match le_u64(input) {
                        Err(e) => Err(e),
                        Ok((i, o)) => Ok((i, f64::from_bits(o))),
                    }
                }
                /// Recognizes a 4 byte floating point number
                #[inline] pub fn f32<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f32, E>
                where
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
                #[inline] pub fn f64<I, E: ParseError<I>>(endian: Endianness) -> fn(I) -> IResult<I, f64, E>
                where
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
                pub fn recognize_float<T, E:ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                
                pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
                where
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
                pub fn recognize_float_parts<T, E: ParseError<T>>(input: T) -> IResult<T, (bool, T, T, i32), E>
                where
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
                pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E>
                where
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
                pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E>
                where
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
        io::{ self, Write },
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
// 12620
