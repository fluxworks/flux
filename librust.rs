#![allow
(
    bad_style,
    improper_ctypes,
    non_camel_case_types,
    overflowing_literals,
    renamed_and_removed_lints,
    unknown_lints,
    unused_macros,
    unused_macro_rules,
)]

#[macro_use] pub mod macros
{
    /// A macro for defining #[cfg] if-else statements.
    #[macro_export] macro_rules! cfg_if
    {
        ($( if #[cfg($($meta:meta),*)] { $($it:item)* } ) else * else { $($it2:item)* }) =>
        {
            cfg_if!
            {
                @__items
                () ;
                $( ( ($($meta),*) ($($it)*) ), )*
                ( () ($($it2)*) ),
            }
        };
        
        (
            if #[cfg($($i_met:meta),*)] { $($i_it:item)* }
            $(
                else if #[cfg($($e_met:meta),*)] { $($e_it:item)* }
            )*
        ) =>
        {
            cfg_if! {
                @__items
                () ;
                ( ($($i_met),*) ($($i_it)*) ),
                $( ( ($($e_met),*) ($($e_it)*) ), )*
                ( () () ),
            }
        };
        
        (@__items ($($not:meta,)*) ; ) => {};
        
        (@__items ($($not:meta,)*) ; ( ($($m:meta),*) ($($it:item)*) ), $($rest:tt)*) =>
        {
            cfg_if! { @__apply cfg(all($($m,)* not(any($($not),*)))), $($it)* }            
            cfg_if! { @__items ($($not,)* $($m,)*) ; $($rest)* }
        };
        
        (@__apply $m:meta, $($it:item)*) => { $(#[$m] $it)* };
    }
    /// Create an internal crate prelude with `core` reexports and common types.
    #[macro_export] macro_rules! prelude
    {
        () =>
        {
            /// Frequently-used types that are available on all platforms.
            mod prelude 
            {
                #[allow(unused_imports)]
                pub(crate) use ::core::clone::Clone;
                #[allow(unused_imports)]
                pub(crate) use ::core::marker::{Copy, Send, Sync};
                #[allow(unused_imports)]
                pub(crate) use ::core::option::Option;
                #[allow(unused_imports)]
                pub(crate) use ::core::{fmt, hash, iter, mem};                
                #[allow(unused_imports)]
                pub(crate) use crate::
                {
                    c_char, c_double, c_float, c_int, c_long, c_longlong, c_short, c_uchar, c_uint, c_ulong, 
                    c_ulonglong, c_ushort, c_void, intptr_t, size_t, ssize_t, uintptr_t,
                };
            }
        };
    }
    /// Implement `Clone` and `Copy` for a struct, as well as `Debug`, `Eq`, `Hash`, and `PartialEq` 
    /// if the `extra_traits` feature is enabled.
    #[macro_export] macro_rules! s
    {
        ($(
            $(#[$attr:meta])*
            pub $t:ident $i:ident { $($field:tt)* }
        )*) => 
        ($(
            s!(it: $(#[$attr])* pub $t $i { $($field)* });
        )*);

        (it: $(#[$attr:meta])* pub union $i:ident { $($field:tt)* }) =>
        (
            compile_error!("unions cannot derive extra traits, use s_no_extra_traits instead");
        );

        (it: $(#[$attr:meta])* pub struct $i:ident { $($field:tt)* }) =>
        (
            __item! {
                #[repr(C)]
                #[cfg_attr(
                    feature = "extra_traits",
                    ::core::prelude::v1::derive(Debug, Eq, Hash, PartialEq)
                )]
                #[::core::prelude::v1::derive(::core::clone::Clone, ::core::marker::Copy)]
                #[allow(deprecated)]
                $(#[$attr])*
                pub struct $i { $($field)* }
            }
        );
    }
    /// Implement `Clone` and `Copy` for a tuple struct, as well as `Debug`, `Eq`, `Hash`, and `PartialEq`
    /// if the `extra_traits` feature is enabled.
    #[macro_export] macro_rules! s_paren
    {
        ($(
            $(#[$attr:meta])*
            pub struct $i:ident ( $($field:tt)* );
        )*) =>
        ($(
            __item! {
                #[cfg_attr(
                    feature = "extra_traits",
                    ::core::prelude::v1::derive(Debug, Eq, Hash, PartialEq)
                )]
                #[::core::prelude::v1::derive(::core::clone::Clone, ::core::marker::Copy)]
                $(#[$attr])*
                pub struct $i ( $($field)* );
            }
        )*);
    }
    /// Implement `Clone` and `Copy` for a struct with no `extra_traits` feature.
    #[macro_export] macro_rules! s_no_extra_traits
    {
        ($(
            $(#[$attr:meta])*
            pub $t:ident $i:ident { $($field:tt)* }
        )*) => 
        ($(
            s_no_extra_traits!(it: $(#[$attr])* pub $t $i { $($field)* });
        )*);

        (it: $(#[$attr:meta])* pub union $i:ident { $($field:tt)* }) => 
        (
            __item! {
                #[repr(C)]
                #[::core::prelude::v1::derive(::core::clone::Clone, ::core::marker::Copy)]
                $(#[$attr])*
                pub union $i { $($field)* }
            }

            #[cfg(feature = "extra_traits")]
            impl ::core::fmt::Debug for $i {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(::core::stringify!($i)).finish_non_exhaustive()
                }
            }
        );

        (it: $(#[$attr:meta])* pub struct $i:ident { $($field:tt)* }) => 
        (
            __item! 
            {
                #[repr(C)]
                #[::core::prelude::v1::derive(::core::clone::Clone, ::core::marker::Copy)]
                #[cfg_attr(feature = "extra_traits", ::core::prelude::v1::derive(Debug))]
                $(#[$attr])*
                pub struct $i { $($field)* }
            }
        );
    }
    /// Specify that an enum should have no traits that aren't specified in the macro invocation.
    #[macro_export] macro_rules! missing
    {
        ($(
            $(#[$attr:meta])*
            pub enum $i:ident {}
        )*) =>
        ($(
            $(#[$attr])*
            #[allow(missing_copy_implementations)]
            pub enum $i { }
        )*);
    }
    /// Implement `Clone` and `Copy` for an enum, as well as `Debug`, `Eq`, `Hash`, and `PartialEq`
    /// if the `extra_traits` feature is enabled.
    #[macro_export] macro_rules! e
    {
        ($(
            $(#[$attr:meta])*
            pub enum $i:ident { $($field:tt)* }
        )*) => 
        ($(
            __item! {
                #[cfg_attr(
                    feature = "extra_traits",
                    ::core::prelude::v1::derive(Debug, Eq, Hash, PartialEq)
                )]
                #[::core::prelude::v1::derive(::core::clone::Clone, ::core::marker::Copy)]
                $(#[$attr])*
                pub enum $i { $($field)* }
            }
        )*);
    }
    /// Represent a C enum as Rust constants and a type.
    #[macro_export] macro_rules! c_enum
    {
        (
            $(#[repr($repr:ty)])?
            enum $ty_name:ident {
                $($variant:ident $(= $value:literal)?,)+
            }
        ) =>
        {
            pub type $ty_name = c_enum!(@ty $($repr)?);
            c_enum!(@one; $ty_name; 0; $($variant $(= $value)?,)+);
        };
        
        (@one; $_ty_name:ident; $_idx:expr;) => {};
        (
            @one; $ty_name:ident; $default_val:expr;
            $variant:ident $(= $value:literal)?,
            $($tail:tt)*
        ) =>
        {
            pub const $variant: $ty_name =
            {
                #[allow(unused_variables)]
                let r = $default_val;
                $(let r = $value;)?
                r
            };
            
            c_enum!(@one; $ty_name; $variant + 1; $($tail)*);
        };
        
        (@ty $repr:ty) => { $repr };
        (@ty) => { $crate::c_uint };
    }
    // Conditionally mark some functions as 'const'.
    cfg_if!
    {
        if #[cfg(libc_const_extern_fn)]
        {
            /// Define an `unsafe` function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! f
            {
                ($(
                    $(#[$attr:meta])*
                    pub $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) =>
                ($(
                    #[inline]
                    $(#[$attr])*
                    pub $($constness)* unsafe extern "C" fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
            /// Define a safe function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! safe_f
            {
                ($(
                    $(#[$attr:meta])*
                    pub $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) =>
                ($(
                    #[inline]
                    $(#[$attr])*
                    pub $($constness)* extern "C" fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
            /// A nonpublic function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! const_fn
            {
                ($(
                    $(#[$attr:meta])*
                    $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) =>
                ($(
                    #[inline]
                    $(#[$attr])*
                    $($constness)* fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
        }

        else
        {
            /// Define an `unsafe` function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! f
            {
                ($(
                    $(#[$attr:meta])*
                    pub $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) =>
                ($(
                    #[inline]
                    $(#[$attr])*
                    pub unsafe extern "C" fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
            /// Define a safe function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! safe_f
            {
                ($(
                    $(#[$attr:meta])*
                    pub $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) =>
                ($(
                    #[inline]
                    $(#[$attr])*
                    pub extern "C" fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
            /// A nonpublic function that is const as long as `libc_const_extern_fn` is enabled.
            macro_rules! const_fn
            {
                ($(
                    $(#[$attr:meta])*
                    $({$constness:ident})* fn $i:ident($($arg:ident: $argty:ty),* $(,)*) -> $ret:ty
                        $body:block
                )*) => 
                ($(
                    #[inline]
                    $(#[$attr])*
                    fn $i($($arg: $argty),*) -> $ret
                        $body
                )*)
            }
        }
    }

    #[macro_export] macro_rules! __item
    {
        ($i:item) => { $i };
    }
}

pub mod boxed
{
    //! The `Box<T>` type for heap allocation.
    use ::
    {
        *,
    };
    /*
    use core::borrow::{Borrow, BorrowMut};
    #[cfg(not(no_global_oom_handling))]
    use core::clone::CloneToUninit;
    use core::cmp::Ordering;
    use core::error::{self, Error};
    use core::fmt;
    use core::future::Future;
    use core::hash::{Hash, Hasher};
    use core::marker::{PointerLike, Tuple, Unsize};
    use core::mem::{self, SizedTypeProperties};
    use core::ops::{
        AsyncFn, AsyncFnMut, AsyncFnOnce, CoerceUnsized, Coroutine, CoroutineState, Deref, DerefMut,
        DerefPure, DispatchFromDyn, LegacyReceiver,
    };
    use core::pin::{Pin, PinCoerceUnsized};
    use core::ptr::{self, NonNull, Unique};
    use core::task::{Context, Poll};

    #[cfg(not(no_global_oom_handling))]
    use crate::alloc::handle_alloc_error;
    use crate::alloc::{AllocError, Allocator, Global, Layout};
    use crate::raw_vec::RawVec;
    #[cfg(not(no_global_oom_handling))]
    use crate::str::from_boxed_utf8_unchecked;
    */
    /// Conversion related impls for `Box<_>` (`From`, `downcast`, etc)
    mod convert
    {
        use ::
        {
            *,
        };
        /*
        use core::any::Any;
        use core::error::Error;
        use core::mem;
        use core::pin::Pin;
        #[cfg(not(no_global_oom_handling))]
        use core::{fmt, ptr};

        use crate::alloc::Allocator;
        #[cfg(not(no_global_oom_handling))]
        use crate::borrow::Cow;
        use crate::boxed::Box;
        #[cfg(not(no_global_oom_handling))]
        use crate::raw_vec::RawVec;
        #[cfg(not(no_global_oom_handling))]
        use crate::str::from_boxed_utf8_unchecked;
        #[cfg(not(no_global_oom_handling))]
        use crate::string::String;
        #[cfg(not(no_global_oom_handling))]
        use crate::vec::Vec;
        */
        #[cfg(not(no_global_oom_handling))]
        impl<T> From<T> for Box<T>
        {
            /// Converts a `T` into a `Box<T>`
            fn from(t: T) -> Self { Box::new(t) }
        }
        
        impl<T: ?Sized, A: Allocator> From<Box<T, A>> for Pin<Box<T, A>> where
        A: 'static,
        {
            /// Converts a `Box<T>` into a `Pin<Box<T>>`.
            fn from(boxed: Box<T, A>) -> Self { Box::into_pin(boxed) }
        }
        /// Specialization trait used for `From<&[T]>`.
        #[cfg(not(no_global_oom_handling))]
        trait BoxFromSlice<T> 
        {
            fn from_slice(slice: &[T]) -> Self;
        }

        #[cfg(not(no_global_oom_handling))]
        impl<T: Clone> BoxFromSlice<T> for Box<[T]> 
        {
            #[inline] default fn from_slice(slice: &[T]) -> Self 
            {
                slice.to_vec().into_boxed_slice()
            }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<T: Copy> BoxFromSlice<T> for Box<[T]> 
        {
            #[inline] fn from_slice(slice: &[T]) -> Self 
            {
                unsafe 
                {
                    let len = slice.len();
                    let buf = RawVec::with_capacity(len);
                    ptr::copy_nonoverlapping(slice.as_ptr(), buf.ptr(), len);
                    buf.into_box(slice.len()).assume_init()
                }
            }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<T: Clone> From<&[T]> for Box<[T]>
        {
            /// Converts a `&[T]` into a `Box<[T]>`
            #[inline] fn from(slice: &[T]) -> Box<[T]> { <Self as BoxFromSlice<T>>::from_slice(slice) }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<T: Clone> From<&mut [T]> for Box<[T]> 
        {
            /// Converts a `&mut [T]` into a `Box<[T]>`
            #[inline] fn from(slice: &mut [T]) -> Box<[T]> { Self::from(&*slice) }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<T: Clone> From<Cow<'_, [T]>> for Box<[T]> 
        {
            /// Converts a `Cow<'_, [T]>` into a `Box<[T]>`
            #[inline] fn from(cow: Cow<'_, [T]>) -> Box<[T]> 
            {
                match cow 
                {
                    Cow::Borrowed(slice) => Box::from(slice),
                    Cow::Owned(slice) => Box::from(slice),
                }
            }
        }

        #[cfg(not(no_global_oom_handling))]
        impl From<&str> for Box<str> 
        {
            /// Converts a `&str` into a `Box<str>`
            #[inline] fn from(s: &str) -> Box<str> 
            {
                unsafe { from_boxed_utf8_unchecked(Box::from(s.as_bytes())) }
            }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl From<&mut str> for Box<str> 
        {
            /// Converts a `&mut str` into a `Box<str>`
            #[inline] fn from(s: &mut str) -> Box<str> { Self::from(&*s) }
        }

        #[cfg(not(no_global_oom_handling))]
        impl From<Cow<'_, str>> for Box<str>
        {
            /// Converts a `Cow<'_, str>` into a `Box<str>`
            #[inline] fn from(cow: Cow<'_, str>) -> Box<str> 
            {
                match cow 
                {
                    Cow::Borrowed(s) => Box::from(s),
                    Cow::Owned(s) => Box::from(s),
                }
            }
        }
        
        impl<A: Allocator> From<Box<str, A>> for Box<[u8], A>
        {
            /// Converts a `Box<str>` into a `Box<[u8]>`
            #[inline] fn from(s: Box<str, A>) -> Self 
            {
                let (raw, alloc) = Box::into_raw_with_allocator(s);
                unsafe { Box::from_raw_in(raw as *mut [u8], alloc) }
            }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<T, const N: usize> From<[T; N]> for Box<[T]>
        {
            /// Converts a `[T; N]` into a `Box<[T]>`
            fn from(array: [T; N]) -> Box<[T]> { Box::new(array) }
        }
        /// Casts a boxed slice to a boxed array.
        unsafe fn boxed_slice_as_array_unchecked<T, A: Allocator, const N: usize>
        (
            boxed_slice: Box<[T], A>,
        ) -> Box<[T; N], A> 
        {
            unsafe
            {
                debug_assert_eq!(boxed_slice.len(), N);
                
                let (ptr, alloc) = Box::into_raw_with_allocator(boxed_slice);
                
                Box::from_raw_in(ptr as *mut [T; N], alloc)
            }
        }
        
        impl<T, const N: usize> TryFrom<Box<[T]>> for Box<[T; N]>
        {
            type Error = Box<[T]>;
            /// Attempts to convert a `Box<[T]>` into a `Box<[T; N]>`.
            fn try_from(boxed_slice: Box<[T]>) -> Result<Self, Self::Error> 
            {
                if boxed_slice.len() == N { Ok(unsafe { boxed_slice_as_array_unchecked(boxed_slice) }) } 
                else { Err(boxed_slice) }
            }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<T, const N: usize> TryFrom<Vec<T>> for Box<[T; N]> 
        {
            type Error = Vec<T>;
            /// Attempts to convert a `Vec<T>` into a `Box<[T; N]>`.
            fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> 
            {
                if vec.len() == N
                {
                    let boxed_slice = vec.into_boxed_slice();
                    Ok(unsafe { boxed_slice_as_array_unchecked(boxed_slice) })
                } else { Err(vec) }
            }
        }

        impl<A: Allocator> Box<dyn Any, A> 
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self> 
            {
                if self.is::<T>() { unsafe { Ok(self.downcast_unchecked::<T>()) } } else { Err(self) }
            }
            /// Downcasts the box to a concrete type.
            #[inline] pub unsafe fn downcast_unchecked<T: Any>(self) -> Box<T, A> 
            {
                unsafe 
                {
                    debug_assert!(self.is::<T>());
                    let (raw, alloc): (*mut dyn Any, _) = Box::into_raw_with_allocator(self);
                    Box::from_raw_in(raw as *mut T, alloc)
                }
            }
        }

        impl<A: Allocator> Box<dyn Any + Send, A> 
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self>
            { if self.is::<T>() { unsafe { Ok(self.downcast_unchecked::<T>()) } } else { Err(self) } }
            /// Downcasts the box to a concrete type.
            #[inline] pub unsafe fn downcast_unchecked<T: Any>(self) -> Box<T, A> 
            {
                unsafe 
                {
                    debug_assert!(self.is::<T>());
                    let (raw, alloc): (*mut (dyn Any + Send), _) = Box::into_raw_with_allocator(self);
                    Box::from_raw_in(raw as *mut T, alloc)
                }
            }
        }

        impl<A: Allocator> Box<dyn Any + Send + Sync, A>
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self> 
            {
                if self.is::<T>() { unsafe { Ok(self.downcast_unchecked::<T>()) } } else { Err(self) }
            }
            /// Downcasts the box to a concrete type.
            #[inline] pub unsafe fn downcast_unchecked<T: Any>(self) -> Box<T, A> 
            {
                unsafe 
                {
                    debug_assert!(self.is::<T>());
                    let (raw, alloc): (*mut (dyn Any + Send + Sync), _) = Box::into_raw_with_allocator(self);
                    Box::from_raw_in(raw as *mut T, alloc)
                }
            }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<'a, E: Error + 'a> From<E> for Box<dyn Error + 'a> 
        {
            /// Converts a type of [`Error`] into a box of dyn [`Error`].
            fn from(err: E) -> Box<dyn Error + 'a> { Box::new(err) }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<'a, E: Error + Send + Sync + 'a> From<E> for Box<dyn Error + Send + Sync + 'a> 
        {
            /// Converts a type of [`Error`] + [`Send`] + [`Sync`] into a box of dyn [`Error`] + [`Send`] + [`Sync`].
            fn from(err: E) -> Box<dyn Error + Send + Sync + 'a> { Box::new(err) }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<'a> From<String> for Box<dyn Error + Send + Sync + 'a> 
        {
            /// Converts a [`String`] into a box of dyn [`Error`] + [`Send`] + [`Sync`].
            #[inline] fn from(err: String) -> Box<dyn Error + Send + Sync + 'a> 
            {
                struct StringError(String);

                impl Error for StringError {
                    #[allow(deprecated)]
                    fn description(&self) -> &str {
                        &self.0
                    }
                }

                impl fmt::Display for StringError {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        fmt::Display::fmt(&self.0, f)
                    }
                }
                
                impl fmt::Debug for StringError {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        fmt::Debug::fmt(&self.0, f)
                    }
                }

                Box::new(StringError(err))
            }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<'a> From<String> for Box<dyn Error + 'a> 
        {
            /// Converts a [`String`] into a box of dyn [`Error`].
            fn from(str_err: String) -> Box<dyn Error + 'a> 
            {
                let err1: Box<dyn Error + Send + Sync> = From::from(str_err);
                let err2: Box<dyn Error> = err1;
                err2
            }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<'a> From<&str> for Box<dyn Error + Send + Sync + 'a> 
        {
            /// Converts a [`str`] into a box of dyn [`Error`] + [`Send`] + [`Sync`].
            #[inline] fn from(err: &str) -> Box<dyn Error + Send + Sync + 'a> 
            {
                From::from(String::from(err))
            }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<'a> From<&str> for Box<dyn Error + 'a> 
        {
            /// Converts a [`str`] into a box of dyn [`Error`].
            fn from(err: &str) -> Box<dyn Error + 'a> { From::from(String::from(err)) }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl<'a, 'b> From<Cow<'b, str>> for Box<dyn Error + Send + Sync + 'a> 
        {
            /// Converts a [`Cow`] into a box of dyn [`Error`] + [`Send`] + [`Sync`].
            fn from(err: Cow<'b, str>) -> Box<dyn Error + Send + Sync + 'a> { From::from(String::from(err)) }
        }

        #[cfg(not(no_global_oom_handling))]
        impl<'a, 'b> From<Cow<'b, str>> for Box<dyn Error + 'a> 
        {
            /// Converts a [`Cow`] into a box of dyn [`Error`].
            fn from(err: Cow<'b, str>) -> Box<dyn Error + 'a> { From::from(String::from(err)) }
        }

        impl dyn Error 
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Error + 'static>(self: Box<Self>) -> Result<Box<T>, Box<dyn Error>> 
            {
                if self.is::<T>() 
                {
                    unsafe 
                    {
                        let raw: *mut dyn Error = Box::into_raw(self);
                        Ok(Box::from_raw(raw as *mut T))
                    }
                }

                else { Err(self) }
            }
        }

        impl dyn Error + Send 
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Error + 'static>(self: Box<Self>) -> Result<Box<T>, Box<dyn Error + Send>> 
            {
                let err: Box<dyn Error> = self;
                <dyn Error>::downcast(err).map_err(|s| unsafe 
                {
                    // Reapply the `Send` marker.
                    mem::transmute::<Box<dyn Error>, Box<dyn Error + Send>>(s)
                })
            }
        }

        impl dyn Error + Send + Sync 
        {
            /// Attempts to downcast the box to a concrete type.
            #[inline] pub fn downcast<T: Error + 'static>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
            {
                let err: Box<dyn Error> = self;
                <dyn Error>::downcast(err).map_err(|s| unsafe 
                {
                    // Reapply the `Send + Sync` markers.
                    mem::transmute::<Box<dyn Error>, Box<dyn Error + Send + Sync>>(s)
                })
            }
        }
    }
    /// Iterator related impls for `Box<_>`.
    mod iter
    {
        use ::
        {
            *,
        };
    }
    /// [`ThinBox`] implementation.
    mod thin
    {
        use ::
        {
            *,
        };
    }
}

pub mod c
{
    pub mod primitives
    {
        //! This module contains type aliases for C's platform-specific types and fixed-width integer types.
        use ::
        {
            *,
        };

        pub type c_schar = i8;
        pub type c_uchar = u8;
        pub type c_short = i16;
        pub type c_ushort = u16;
        pub type c_longlong = i64;
        pub type c_ulonglong = u64;
        pub type c_float = f32;
        pub type c_double = f64;

        cfg_if!
        {
            if #[cfg
            (
                all
                (
                    not(windows),
                    not
                    (
                        any
                        (
                            target_os = "macos",
                            target_os = "ios",
                            target_os = "tvos",
                            target_os = "watchos",
                            target_os = "visionos",
                        )
                    ),
                    not(target_os = "vita"),
                    any
                    (
                        target_arch = "aarch64",
                        target_arch = "arm",
                        target_arch = "csky",
                        target_arch = "hexagon",
                        target_arch = "msp430",
                        target_arch = "powerpc",
                        target_arch = "powerpc64",
                        target_arch = "riscv32",
                        target_arch = "riscv64",
                        target_arch = "s390x",
                        target_arch = "xtensa",
                    )
                )
            )] 
            {
                pub type c_char = u8;
            } else 
            {
                pub type c_char = i8;
            }
        }

        cfg_if!
        {
            if #[cfg(any(target_arch = "avr", target_arch = "msp430"))]
            {
                pub type c_int = i16;
                pub type c_uint = u16;
            }
            else
            {
                pub type c_int = i32;
                pub type c_uint = u32;
            }
        }

        cfg_if!
        {
            if #[cfg(all(target_pointer_width = "64", not(windows)))]
            {
                pub type c_long = i64;
                pub type c_ulong = u64;
            }
            else
            {
                pub type c_long = i32;
                pub type c_ulong = u32;
            }
        }

        cfg_if!
        {
            if #[cfg
            (
                all
                (
                    target_arch = "aarch64",
                    not
                    (
                        any
                        (
                            target_os = "windows",
                            target_os = "macos",
                            target_os = "ios",
                            target_os = "tvos",
                            target_os = "watchos"
                        )
                    )
                )
            )]
            {
                /// Introduces partial support for FFI with __int128 and equivalent types.
                const _SIZE_128: usize = 16;
                const _ALIGN_128: usize = 16;
                /// C `__int128` (a GCC extension that's part of many ABIs)
                pub type __int128 = i128;
                /// C `unsigned __int128` (a GCC extension that's part of many ABIs)
                pub type __uint128 = u128;
                /// C __int128_t (alternate name for [__int128][])
                pub type __int128_t = i128;
                /// C __uint128_t (alternate name for [__uint128][])
                pub type __uint128_t = u128;
                
                
            }

            else if #[cfg(all
            (
                target_arch = "aarch64",
                any
                (
                    target_os = "macos",
                    target_os = "ios",
                    target_os = "tvos",
                    target_os = "watchos"
                )
            ))]
            {
                /// C `__int128_t`
                pub type __int128_t = i128;
                /// C `__uint128_t`
                pub type __uint128_t = u128;
            }
        }

        pub use ::ffi::c_void;
    } pub use self::primitives::{ * };
}

pub mod ffi
{
    //! Utilities related to FFI bindings.
    use ::
    {
        *,
    };

    pub enum c_void 
    {
        __variant1,
        __variant2,
    }

    impl fmt::Debug for c_void
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            f.debug_struct("c_void").finish()
        }
    }

    // Link the MSVC default lib
    #[cfg(all(windows, target_env = "msvc"))]
    #[link
    (
        name = "/defaultlib:msvcrt",
        modifiers = "+verbatim",
        cfg(not(target_feature = "crt-static"))
    )] 
    #[link
    (
        name = "/defaultlib:libcmt", modifiers = "+verbatim", cfg(target_feature = "crt-static")
    )]
    unsafe extern "C" {}

    pub mod c_str
    {
        //! [`CString`] and its related types.
        use ::
        {
            borrow::{ Borrow, Cow, ToOwned },
            ffi::{ CStr, c_char },
            num::{ NonZero },
            rc::{ Rc },
            slice::{ memchr },
            str::{ self, FromStr, Utf8Error },
            string::{ String },
            vec::{ Vec },
            *,
        };
        
        #[cfg(target_has_atomic = "ptr")] use ::sync::Arc;
        /*
        use core::borrow::Borrow;
        use core::ffi::{CStr, c_char};
        use core::num::NonZero;
        use core::slice::memchr;
        use core::str::{self, FromStr, Utf8Error};
        use core::{fmt, mem, ops, ptr, slice};

        use crate::borrow::{Cow, ToOwned};
        use crate::boxed::Box;
        use crate::rc::Rc;
        use crate::string::String;
        #[cfg(target_has_atomic = "ptr")]
        use crate::sync::Arc;
        use crate::vec::Vec; */
        /// Represents an owned, C-compatible, nul-terminated string with no nul bytes in the middle.
        #[derive( Clone, Eq, Hash, Ord, PartialEq, PartialOrd )]
        pub struct CString
        {
            inner: Box<[u8]>,
        }
        /// An error indicating that an interior nul byte was found.
        #[derive( Clone, Debug, Eq, PartialEq )]
        pub struct NulError( pub usize, pub Vec<u8> );

        #[derive( Clone, Debug, Eq, PartialEq )]
        pub enum FromBytesWithNulErrorKind
        {
            InteriorNul(usize),
            NotNulTerminated,
        }
        /// An error indicating that a nul byte was not in the expected position.
        #[derive( Clone, Debug, Eq, PartialEq )]
        pub struct FromVecWithNulError
        {
            error_kind: FromBytesWithNulErrorKind,
            bytes: Vec<u8>,
        }
        
        impl FromVecWithNulError
        {
            /// Returns a slice of [`u8`]s bytes that were attempted to convert to a [`CString`].
            #[must_use]
            pub fn as_bytes(&self) -> &[u8]
            {
                &self.bytes[..]
            }
            /// Returns the bytes that were attempted to convert to a [`CString`].
            #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_bytes(self) -> Vec<u8>
            {
                self.bytes
            }
        }
        /// An error indicating invalid UTF-8 when converting a [`CString`] into a [`String`].
        #[derive( Clone, Debug, Eq, PartialEq )]
        pub struct IntoStringError
        {
            inner: CString,
            error: Utf8Error,
        }

        impl CString
        {
            /// Creates a new C-compatible string from a container of bytes.
            pub fn new<T: Into<Vec<u8>>>(t: T) -> Result<CString, NulError>
            {
                trait SpecNewImpl
                {
                    fn spec_new_impl(self) -> Result<CString, NulError>;
                }

                impl<T: Into<Vec<u8>>> SpecNewImpl for T
                {
                    default fn spec_new_impl(self) -> Result<CString, NulError>
                    {
                        let bytes: Vec<u8> = self.into();
                        match memchr::memchr(0, &bytes)
                        {
                            Some(i) => Err(NulError(i, bytes)),
                            None => Ok(unsafe { CString::_from_vec_unchecked(bytes) }),
                        }
                    }
                }
                
                #[inline( always )] fn spec_new_impl_bytes(bytes: &[u8]) -> Result<CString, NulError>
                {
                    let capacity = bytes.len().checked_add(1).unwrap();
                    let mut buffer = Vec::with_capacity(capacity);
                    buffer.extend(bytes);
                    
                    match memchr::memchr(0, bytes)
                    {
                        Some(i) => Err(NulError(i, buffer)),
                        None => Ok(unsafe { CString::_from_vec_unchecked(buffer) }),
                    }
                }

                impl SpecNewImpl for &'_ [u8]
                {
                    fn spec_new_impl(self) -> Result<CString, NulError> { spec_new_impl_bytes(self) }
                }

                impl SpecNewImpl for &'_ str
                {
                    fn spec_new_impl(self) -> Result<CString, NulError> { spec_new_impl_bytes(self.as_bytes()) }
                }

                impl SpecNewImpl for &'_ mut [u8]
                {
                    fn spec_new_impl(self) -> Result<CString, NulError> { spec_new_impl_bytes(self) }
                }

                t.spec_new_impl()
            }
            /// Creates a C-compatible string by consuming a byte vector, without checking for interior 0 bytes.
            #[must_use]
            pub unsafe fn from_vec_unchecked(v: Vec<u8>) -> Self
            {
                debug_assert!(memchr::memchr(0, &v).is_none());
                unsafe { Self::_from_vec_unchecked(v) }
            }

            unsafe fn _from_vec_unchecked(mut v: Vec<u8>) -> Self
            {
                v.reserve_exact(1);
                v.push(0);
                Self { inner: v.into_boxed_slice() }
            }
            /// Retakes ownership of a `CString` that was transferred to C via [`CString::into_raw`].
            #[must_use = "call `drop(from_raw(ptr))` if you intend to drop the `CString`"]
            pub unsafe fn from_raw(ptr: *mut c_char) -> CString
            {
                unsafe 
                {
                    unsafe extern "C"
                    {
                        /// Provided by libc or compiler_builtins.
                        fn strlen(s: *const c_char) -> usize;
                    }

                    let len = strlen(ptr) + 1;
                    let slice = ::slice::from_raw_parts_mut(ptr, len);
                    CString { inner: Box::from_raw(slice as *mut [c_char] as *mut [u8]) }
                }
            }
            /// Consumes the `CString` and transfers ownership of the string to a C caller.
            #[inline] #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_raw(self) -> *mut c_char { Box::into_raw(self.into_inner()) as *mut c_char }
            /// Converts the `CString` into a [`String`] if it contains valid UTF-8 data.
            pub fn into_string(self) -> Result<String, IntoStringError>
            {
                String::from_utf8(self.into_bytes()).map_err(|e| IntoStringError
                {
                    error: e.utf8_error(),
                    inner: unsafe { Self::_from_vec_unchecked(e.into_bytes()) },
                })
            }
            /// Consumes the `CString` and returns the underlying byte buffer.
            #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_bytes(self) -> Vec<u8>
            {
                let mut vec = self.into_inner().into_vec();
                let _nul = vec.pop();
                debug_assert_eq!(_nul, Some(0u8));
                vec
            }
            /// Same as `CString::into_bytes()` except that the returned vector includes the trailing nul terminator.
            #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_bytes_with_nul(self) -> Vec<u8> { self.into_inner().into_vec() }
            /// Returns the contents of this `CString` as a slice of bytes.
            #[inline] #[must_use]
            pub fn as_bytes(&self) -> &[u8] { unsafe { self.inner.get_unchecked(..self.inner.len() - 1) } }
            /// Same as [`CString::as_bytes()`] except that the returned slice includes the trailing nul terminator.
            #[inline] #[must_use]
            pub fn as_bytes_with_nul(&self) -> &[u8] { &self.inner }
            /// Extracts a [`CStr`] slice containing the entire string.
            #[inline] #[must_use]
            pub fn as_c_str(&self) -> &CStr
            {
                unsafe { CStr::from_bytes_with_nul_unchecked(self.as_bytes_with_nul()) }
            }
            /// Converts this `CString` into a boxed [`CStr`].
            #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_boxed_c_str(self) -> Box<CStr> 
            {
                unsafe { Box::from_raw(Box::into_raw(self.into_inner()) as *mut CStr) }
            }
            /// Bypass "move out of struct which implements [`Drop`] trait" restriction.
            #[inline] fn into_inner(self) -> Box<[u8]>
            {
                let this = mem::ManuallyDrop::new(self);
                unsafe { ptr::read(&this.inner) }
            }
            /// Converts a Vec[u8] to a [`CString`] without checking the invariants on the given [`Vec`].
            #[must_use]
            pub unsafe fn from_vec_with_nul_unchecked(v: Vec<u8>) -> Self
            {
                debug_assert!(memchr::memchr(0, &v).unwrap() + 1 == v.len());
                unsafe { Self::_from_vec_with_nul_unchecked(v) }
            }

            unsafe fn _from_vec_with_nul_unchecked(v: Vec<u8>) -> Self
            {
                Self { inner: v.into_boxed_slice() }
            }
            /// Attempts to convert a Vec[u8] to a [`CString`].
            pub fn from_vec_with_nul(v: Vec<u8>) -> Result<Self, FromVecWithNulError>
            {
                let nul_pos = memchr::memchr(0, &v);
                
                match nul_pos 
                {
                    Some(nul_pos) if nul_pos + 1 == v.len() => {
                        // SAFETY: We know there is only one nul byte, at the end
                        // of the vec.
                        Ok(unsafe { Self::_from_vec_with_nul_unchecked(v) })
                    }
                    Some(nul_pos) => Err(FromVecWithNulError {
                        error_kind: FromBytesWithNulErrorKind::InteriorNul(nul_pos),
                        bytes: v,
                    }),
                    None => Err(FromVecWithNulError {
                        error_kind: FromBytesWithNulErrorKind::NotNulTerminated,
                        bytes: v,
                    }),
                }
            }
        }
        
        impl Drop for CString
        {
            #[inline] fn drop(&mut self)
            {
                unsafe 
                {
                    *self.inner.get_unchecked_mut(0) = 0;
                }
            }
        }
        
        impl ops::Deref for CString
        {
            type Target = CStr;
            #[inline] fn deref(&self) -> &CStr {
                self.as_c_str()
            }
        }
        /// Delegates to the [`CStr`] implementation of [`fmt::Debug`], showing invalid UTF-8 as hex escapes.
        impl fmt::Debug for CString
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            { fmt::Debug::fmt(self.as_c_str(), f) }
        }
        
        impl From<CString> for Vec<u8>
        {
            /// Converts a [`CString`] into a <code>[Vec]<[u8]></code>.
            #[inline] fn from(s: CString) -> Vec<u8> { s.into_bytes() }
        }
        
        impl Default for CString
        {
            /// Creates an empty `CString`.
            fn default() -> CString
            {
                let a: &CStr = Default::default();
                a.to_owned()
            }
        }
        
        impl Borrow<CStr> for CString
        {
            #[inline] fn borrow(&self) -> &CStr
            {
                self
            }
        }
        
        impl<'a> From<Cow<'a, CStr>> for CString
        {
            /// Converts a `Cow<'a, CStr>` into a `CString`, by copying the contents if they are borrowed.
            #[inline] fn from(s: Cow<'a, CStr>) -> Self 
            { s.into_owned() }
        }
        
        impl From<&CStr> for Box<CStr>
        {
            /// Converts a `&CStr` into a `Box<CStr>`, by copying the contents into a newly allocated [`Box`].
            fn from(s: &CStr) -> Box<CStr>
            {
                let boxed: Box<[u8]> = Box::from(s.to_bytes_with_nul());
                unsafe { Box::from_raw(Box::into_raw(boxed) as *mut CStr) }
            }
        }
        
        impl From<&mut CStr> for Box<CStr>
        {
            /// Converts a `&mut CStr` into a `Box<CStr>`, by copying the contents into a newly allocated [`Box`].
            fn from(s: &mut CStr) -> Box<CStr> { Self::from(&*s) }
        }
        
        impl From<Cow<'_, CStr>> for Box<CStr>
        {
            /// Converts a `Cow<'a, CStr>` into a `Box<CStr>`, by copying the contents if they are borrowed.
            #[inline] fn from(cow: Cow<'_, CStr>) -> Box<CStr>
            {
                match cow 
                {
                    Cow::Borrowed(s) => Box::from(s),
                    Cow::Owned(s) => Box::from(s),
                }
            }
        }
        
        impl From<Box<CStr>> for CString
        {
            /// Converts a <code>[Box]<[CStr]></code> into a [`CString`] without copying or allocating.
            #[inline] fn from(s: Box<CStr>) -> CString
            {
                let raw = Box::into_raw(s) as *mut [u8];
                CString { inner: unsafe { Box::from_raw(raw) } }
            }
        }
        
        impl From<Vec<NonZero<u8>>> for CString
        {
            /// Converts a <code>[Vec]<[NonZero]<[u8]>></code> into a [`CString`] without copying nor checking for inner nul bytes.
            #[inline] fn from(v: Vec<NonZero<u8>>) -> CString
            {
                unsafe 
                {
                    let v: Vec<u8> = 
                    {
                        let (ptr, len, cap): (*mut NonZero<u8>, _, _) = Vec::into_raw_parts(v);
                        Vec::from_raw_parts(ptr.cast::<u8>(), len, cap)
                    };
                    
                    Self::_from_vec_unchecked(v)
                }
            }
        }
        
        impl FromStr for CString
        {
            type Err = NulError;
            /// Converts a string `s` into a [`CString`].
            #[inline] fn from_str(s: &str) -> Result<Self, Self::Err> { Self::new(s) }
        }
        
        impl TryFrom<CString> for String
        {
            type Error = IntoStringError;
            /// Converts a [`CString`] into a [`String`] if it contains valid UTF-8 data.
            #[inline] fn try_from(value: CString) -> Result<Self, Self::Error> { value.into_string() }
        }
        
        impl Clone for Box<CStr>
        {
            #[inline] fn clone(&self) -> Self { (**self).into() }
        }
        
        impl From<CString> for Box<CStr>
        {
            /// Converts a [`CString`] into a <code>[Box]<[CStr]></code> without copying or allocating.
            #[inline] fn from(s: CString) -> Box<CStr> { s.into_boxed_c_str() }
        }
        
        impl<'a> From<CString> for Cow<'a, CStr>
        {
            /// Converts a [`CString`] into an owned [`Cow`] without copying or allocating.
            #[inline] fn from(s: CString) -> Cow<'a, CStr> { Cow::Owned(s) }
        }
        
        impl<'a> From<&'a CStr> for Cow<'a, CStr> 
        {
            /// Converts a [`CStr`] into a borrowed [`Cow`] without copying or allocating.
            #[inline] fn from(s: &'a CStr) -> Cow<'a, CStr> { Cow::Borrowed(s) }
        }
        
        impl<'a> From<&'a CString> for Cow<'a, CStr>
        {
            /// Converts a `&`[`CString`] into a borrowed [`Cow`] without copying or allocating.
            #[inline] fn from(s: &'a CString) -> Cow<'a, CStr> { Cow::Borrowed(s.as_c_str()) }
        }

        #[cfg(target_has_atomic = "ptr")]
        impl From<CString> for Arc<CStr>
        {
            /// Converts a [`CString`] into an Arc<CStr> by moving the [`CString`] data into a new [`Arc`] buffer.
            #[inline] fn from(s: CString) -> Arc<CStr>
            {
                let arc: Arc<[u8]> = Arc::from(s.into_inner());
                unsafe { Arc::from_raw(Arc::into_raw(arc) as *const CStr) }
            }
        }

        #[cfg(target_has_atomic = "ptr")]
        impl From<&CStr> for Arc<CStr> 
        {
            /// Converts a `&CStr` into a `Arc<CStr>`, by copying the contents into a newly allocated [`Arc`].
            #[inline] fn from(s: &CStr) -> Arc<CStr> 
            {
                let arc: Arc<[u8]> = Arc::from(s.to_bytes_with_nul());
                unsafe { Arc::from_raw(Arc::into_raw(arc) as *const CStr) }
            }
        }

        #[cfg(target_has_atomic = "ptr")]
        impl From<&mut CStr> for Arc<CStr> 
        {
            /// Converts a `&mut CStr` into a `Arc<CStr>`, by copying the contents into a newly allocated [`Arc`].
            #[inline] fn from(s: &mut CStr) -> Arc<CStr> { Arc::from(&*s) }
        }
        
        impl From<CString> for Rc<CStr>
        {
            /// Converts a [`CString`] into an Rc<CStr> by moving the [`CString`] data into a new [`Rc`] buffer.
            #[inline] fn from(s: CString) -> Rc<CStr>
            {
                let rc: Rc<[u8]> = Rc::from(s.into_inner());
                unsafe { Rc::from_raw(Rc::into_raw(rc) as *const CStr) }
            }
        }
        
        impl From<&CStr> for Rc<CStr>
        {
            /// Converts a `&CStr` into a `Rc<CStr>`, by copying the contents into a newly allocated [`Rc`].
            #[inline] fn from(s: &CStr) -> Rc<CStr>
            {
                let rc: Rc<[u8]> = Rc::from(s.to_bytes_with_nul());
                unsafe { Rc::from_raw(Rc::into_raw(rc) as *const CStr) }
            }
        }
        
        impl From<&mut CStr> for Rc<CStr> 
        {
            /// Converts a `&mut CStr` into a `Rc<CStr>`, by copying the contents into a newly allocated [`Rc`].
            #[inline] fn from(s: &mut CStr) -> Rc<CStr> { Rc::from(&*s) }
        }

        #[cfg(not(no_global_oom_handling))]        
        impl Default for Rc<CStr> 
        {
            /// Creates an empty CStr inside an Rc
            #[inline] fn default() -> Self 
            {
                let rc = Rc::<[u8]>::from(*b"\0");
                // `[u8]` has the same layout as `CStr`, and it is `NUL` terminated.
                unsafe { Rc::from_raw(Rc::into_raw(rc) as *const CStr) }
            }
        }
        
        impl Default for Box<CStr> 
        {
            fn default() -> Box<CStr> 
            {
                unsafe 
                { 
                    let boxed: Box<[u8]> = Box::from([0]);
                    Box::from_raw(Box::into_raw(boxed) as *mut CStr) 
                }
            }
        }

        impl NulError 
        {
            /// Returns the position of the nul byte in the slice that caused [`CString::new`] to fail.
            #[must_use]
            pub fn nul_position(&self) -> usize { self.0 }
            /// Consumes this error, returning the underlying vector of bytes 
            /// which generated the error in the first place.
            #[must_use = "`self` will be dropped if the result is not used"]
            pub fn into_vec(self) -> Vec<u8> { self.1 }
        }
        
        impl fmt::Display for NulError 
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            { write!(f, "nul byte found in provided data at position: {}", self.0) }
        }
        
        impl fmt::Display for FromVecWithNulError 
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            {
                match self.error_kind 
                {
                    FromBytesWithNulErrorKind::InteriorNul(pos) => 
                    { write!(f, "data provided contains an interior nul byte at pos {pos}") }

                    FromBytesWithNulErrorKind::NotNulTerminated => 
                    { write!(f, "data provided is not nul terminated") }
                }
            }
        }

        impl IntoStringError
        {
            /// Consumes this error, returning original [`CString`] which generated the error.
            #[must_use = "`self` will be dropped if the result is not used"]            
            pub fn into_cstring(self) -> CString { self.inner }
            /// Access the underlying UTF-8 error that was the cause of this error.
            #[must_use]
            pub fn utf8_error(&self) -> Utf8Error { self.error }
        }

        impl IntoStringError 
        {
            fn description(&self) -> &str { "C string contained non-utf8 bytes" }
        }
        
        impl fmt::Display for IntoStringError 
        {
            #[allow(deprecated, deprecated_in_future)]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.description().fmt(f) }
        }
        
        impl ToOwned for CStr 
        {
            type Owned = CString;
            fn to_owned(&self) -> CString { CString { inner: self.to_bytes_with_nul().into() } }
            fn clone_into(&self, target: &mut CString)
            {
                let mut b = mem::take(&mut target.inner).into_vec();
                self.to_bytes_with_nul().clone_into(&mut b);
                target.inner = b.into_boxed_slice();
            }
        }
        
        impl From<&CStr> for CString 
        {
            /// Converts a <code>&[CStr]</code> into a [`CString`] by copying the contents into a new allocation.
            fn from(s: &CStr) -> CString { s.to_owned() }
        }
        
        impl ops::Index<ops::RangeFull> for CString
        {
            type Output = CStr;
            #[inline] fn index(&self, _index: ops::RangeFull) -> &CStr { self }
        }
        
        impl AsRef<CStr> for CString 
        {
            #[inline] fn as_ref(&self) -> &CStr { self }
        }

        impl CStr 
        {
            /// Converts a `CStr` into a Cow<str>.
            #[must_use = "this returns the result of the operation, without modifying the original"]            
            pub fn to_string_lossy(&self) -> Cow<'_, str> { String::from_utf8_lossy(self.to_bytes()) }
            /// Converts a Box<CStr> into a [`CString`] without copying or allocating.
            #[must_use = "`self` will be dropped if the result is not used"]            
            pub fn into_c_string(self: Box<Self>) -> CString { CString::from(self) }
        }
        
        impl error::Error for NulError 
        {
            #[allow(deprecated)] fn description(&self) -> &str { "nul byte found in data" }
        }
        
        impl error::Error for FromVecWithNulError {}
        
        impl error::Error for IntoStringError 
        {
            #[allow(deprecated)] fn description(&self) -> &str 
            {
                "C string contained non-utf8 bytes"
            }

            fn source(&self) -> Option<&(dyn core::error::Error + 'static)> 
            {
                Some(&self.error)
            }
        }
    } pub use self::c_str::{ CString, FromVecWithNulError, IntoStringError, NulError};
}

pub fn main()
{

}
// 1562
