#![allow
(
    unknown_lints,
    unused_imports,
)]
// #![feature(tool_lints)]
extern crate errno;
extern crate exec;
extern crate glob;
//extern crate libc;
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

#[macro_use] pub mod macros
{

}

pub mod borrow
{
    pub use std::borrow::{ * };
}

pub mod builtins
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod calculator
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod cmp
{
    pub use std::cmp::{ * };
}

pub mod completers
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod error
{
    pub use std::error::{ * };
}

pub mod execute
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod fmt
{
    pub use std::fmt::{ * };
}

pub mod highlight
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod hint
{
    pub use std::hint::{ * };
}

pub mod history
{
    /*!
    */
    use ::
    {
        *,
    };
} 

pub mod libc
{
    /*!
    */
    use ::
    {
        *,
    };

    pub mod jobs
    {
        /*!
        */
        use ::
        {
            *,
        };
    }

    pub mod time
    {
        /*!
        */
        use ::
        {
            borrow::Borrow,
            cmp::Ordering,
            error::Error,
            hint::assert_unchecked,
            num::{IntErrorKind, NonZero},
            str::FromStr,
            *,
        };
        /// Declare and implement `Per` for all relevant types. Identity implementations are automatic.
        macro_rules! impl_per 
        {
            ($($t:ident ($str:literal) per {$(
                $larger:ident : [$default_output:ty]

                $($int_output:ty)|+ = $int_value:expr;
                $($float_output:ty)|+ = $float_value:expr;
            )+})*) => {$(
                #[doc = concat!("A unit of time representing exactly one ", $str, ".")]
                #[derive(Debug, Clone, Copy)]
                pub struct $t;

                impl $t {
                    #[doc = concat!("Obtain the number of times `", stringify!($t), "` can fit into `T`.")]
                    #[doc = concat!("If `T` is smaller than `", stringify!($t), "`, the code will fail to")]
                    /// compile. The return type is the smallest unsigned integer type that can represent
                    /// the value.
                    ///
                    /// Valid calls:
                    ///
                    $(#[doc = concat!(
                        "  - `", stringify!($t), "::per(", stringify!($larger), ")` (returns `",
                        stringify!($default_output), "`)"
                    )])+
                    #[inline]
                    pub const fn per<T>(_larger: T) -> <T as DefaultOutput<Self>>::Output
                    where
                        T: MultipleOf<Self, T::Output> + DefaultOutput<Self> + Copy,
                    {
                        T::VALUE
                    }

                    #[doc = concat!("Obtain the number of times `", stringify!($t), "` can fit into `T`.")]
                    #[doc = concat!("If `T` is smaller than `", stringify!($t), "`, the code will fail to")]
                    /// compile. The return type is any primitive numeric type that can represent the value.
                    ///
                    /// Valid calls:
                    ///
                    $(#[doc = concat!(
                        "  - `", stringify!($t), "::per(", stringify!($larger), ")` (returns ",
                        stringify_outputs!($($int_output),+ , $($float_output),+), ")"
                    )])+
                    #[inline]
                    pub const fn per_t<Output>(larger: impl MultipleOf<Self, Output> + Copy) -> Output {
                        multiple_of_value(larger)
                    }
                }

                $(
                    $(impl MultipleOf<$t, $int_output> for $larger {
                        const VALUE: $int_output = $int_value;
                    })+

                    $(impl MultipleOf<$t, $float_output> for $larger {
                        const VALUE: $float_output = $float_value;
                    })+

                    impl DefaultOutput<$t> for $larger {
                        type Output = $default_output;
                    }
                )+
            )*};
        }
        
        /// Output the given tokens if the type is signed, otherwise output nothing.
        macro_rules! if_signed
        {
            (true $($x:tt)*) => { $($x)*};
            (false $($x:tt)*) => {};
        }

        /// Output the given tokens if the type is unsigned, otherwise output nothing.
        macro_rules! if_unsigned
        {
            (true $($x:tt)*) => {};
            (false $($x:tt)*) => { $($x)* };
        }
        /// `Option::unwrap_unchecked`, but usable in `const` contexts.
        macro_rules! unsafe_unwrap_unchecked
        {
            ($e:expr) => 
            {{
                let opt = $e;
                debug_assert!(opt.is_some());
                match $e
                {
                    Some(value) => value,
                    None => ::hint::unreachable_unchecked(),
                }
            }};
        }
        /// Implement a ranged integer type.
        macro_rules! impl_ranged
        {
            ($(
                $type:ident
                {
                    mod_name: $mod_name:ident
                    internal: $internal:ident
                    signed: $is_signed:ident
                    unsigned: $unsigned_type:ident
                    optional: $optional_type:ident
                    from: [$($from:ident($from_internal:ident))+]
                    $(manual: [$($skips:ident)+])?
                }
            )*) =>
            {$(
                #[repr(transparent)] #[derive(Clone, Copy, Eq, Ord, Hash)]
                pub struct $type<const MIN: $internal, const MAX: $internal>
                (
                    Unsafe<$internal>,
                );
                
                #[repr(transparent)] #[derive(Clone, Copy, Eq, Hash)]
                pub struct $optional_type<const MIN: $internal, const MAX: $internal>
                (
                    $internal,
                );

                impl $type<0, 0>
                {
                    #[inline( always )] pub const fn exact<const VALUE: $internal>() -> $type<VALUE, VALUE>
                    {
                        unsafe { $type::new_unchecked(VALUE) }
                    }
                }

                if_unsigned! { $is_signed impl $type<1, { $internal::MAX }>
                {
                    /// Creates a ranged integer from a non-zero value.
                    #[inline( always )] pub const fn from_nonzero(value: NonZero<$internal>) -> Self
                    {
                        unsafe { Self::new_unchecked(value.get()) }
                    }
                    /// Creates a non-zero value from a ranged integer.
                    #[inline( always )] pub const fn to_nonzero(self) -> NonZero<$internal>
                    {
                        unsafe { NonZero::new_unchecked(self.get()) }
                    }
                }}

                impl<const MIN: $internal, const MAX: $internal> $type<MIN, MAX>
                {
                    /// The smallest value that can be represented by this type.
                    pub const MIN: Self = Self::new_static::<MIN>();
                    /// The largest value that can be represented by this type.
                    pub const MAX: Self = Self::new_static::<MAX>();
                    /// Creates a ranged integer without checking the value.
                    #[track_caller] #[inline( always )] pub const unsafe fn new_unchecked(value: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            assert_unchecked(MIN <= value && value <= MAX);
                            Self(Unsafe::new(value))
                        }
                    }
                    /// Returns the value as a primitive type.
                    #[inline( always )] pub const fn get(self) -> $internal
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            assert_unchecked(MIN <= *self.0.get() && *self.0.get() <= MAX);
                            *self.0.get()
                        }
                    }
                    /// Returns the value as a primitive type.
                    #[inline( always )] pub const fn get_without_hint(self) -> $internal
                    {
                        const { assert!(MIN <= MAX); }
                        *self.0.get()
                    }

                    #[track_caller] #[inline(always)] pub const fn get_ref(&self) -> &$internal
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            let value = self.0.get();
                            assert_unchecked(MIN <= *value && *value <= MAX)
                            value
                        }
                    }
                    /// Creates a ranged integer if the given value is in the range `MIN..=MAX`.
                    #[inline( always )] pub const fn new(value: $internal) -> Option<Self>
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            if value < MIN || value > MAX { None }
                            else
                            {
                                Some( Self::new_unchecked(value) )
                            }
                        }
                    }
                    /// Creates a ranged integer with a statically known value. **Fails to compile** if the value is not in range.
                    #[inline( always )] pub const fn new_static<const VALUE: $internal>() -> Self
                    {
                        unsafe
                        {
                            const
                            {
                                assert!(MIN <= VALUE);
                                assert!(VALUE <= MAX);
                            }
                            Self::new_unchecked(VALUE)
                        }
                    }
                    /// Creates a ranged integer with the given value, saturating if it is out of range.
                    #[inline] pub const fn new_saturating(value: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            if value < MIN { Self::MIN }
                            else if value > MAX { Self::MAX }
                            else { Self::new_unchecked(value) }
                        }
                    }
                    /// Emit a hint to the compiler that the value is in range.
                    #[inline( always )] pub const fn emit_range_hint(self)
                    {
                        unsafe
                        { 
                            const { assert!(MIN <= MAX); }
                            let value = self.0.get();
                            assert_unchecked(MIN <= *value && *value <= MAX)
                        };
                    }
                    /// Expand the range that the value may be in. **Fails to compile** if the new range is not a superset of the current range.
                    #[inline( always )] pub const fn expand<const NEW_MIN: $internal, const NEW_MAX: $internal>( self ) -> $type<NEW_MIN, NEW_MAX>
                    {
                        unsafe
                        {
                            const
                            {
                                assert!(MIN <= MAX);
                                assert!(NEW_MIN <= NEW_MAX);
                                assert!(NEW_MIN <= MIN);
                                assert!(NEW_MAX >= MAX);
                            }
                            $type::new_unchecked(self.get())
                        }
                    }
                    /// Attempt to narrow the range that the value may be in.
                    #[inline( always )] pub const fn narrow
                    <
                        const NEW_MIN: $internal,
                        const NEW_MAX: $internal,
                    >(self) -> Option<$type<NEW_MIN, NEW_MAX>>
                    {
                        const 
                        {
                            assert!(MIN <= MAX);
                            assert!(NEW_MIN <= NEW_MAX);
                            assert!(NEW_MIN >= MIN);
                            assert!(NEW_MAX <= MAX);
                        }
                        $type::<NEW_MIN, NEW_MAX>::new(self.get())
                    }
                    /// Converts a string slice in a given base to an integer.
                    #[inline] pub fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>
                    {
                        const { assert!(MIN <= MAX); }
                        match $internal::from_str_radix(src, radix)
                        {
                            Ok(value) if value > MAX => { Err(ParseIntError { kind: IntErrorKind::PosOverflow }) }
                            Ok(value) if value < MIN => { Err(ParseIntError { kind: IntErrorKind::NegOverflow }) }
                            Ok(value) => Ok(unsafe { Self::new_unchecked(value) }),
                            Err(e) => Err(ParseIntError { kind: e.kind().clone() }),
                        }
                    }
                    /// Checked integer addition. Computes `self + rhs`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_add(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_add(rhs)))
                    }
                    /// Unchecked integer addition. Computes `self + rhs`, assuming that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_add(self, rhs: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_add(rhs)))
                        }
                    }
                    /// Checked integer addition. Computes `self - rhs`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_sub(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_sub(rhs)))
                    }
                    /// Unchecked integer subtraction. Computes `self - rhs`, assuming that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_sub(self, rhs: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_sub(rhs)))
                        }
                    }
                    /// Checked integer addition. Computes `self * rhs`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_mul(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_mul(rhs)))
                    }
                    /// Unchecked integer multiplication. Computes `self * rhs`, assuming that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_mul(self, rhs: $internal) -> Self
                    {
                        unsafe 
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_mul(rhs)))
                        }
                    }
                    /// Checked integer addition. Computes `self / rhs`, returning `None` if `rhs == 0` or if the resulting value is out of range.
                    #[inline] pub const fn checked_div(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_div(rhs)))
                    }
                    /// Unchecked integer division. Computes `self / rhs`, assuming that `rhs != 0` and that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_div(self, rhs: $internal) -> Self 
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_div(rhs)))
                        }
                    }
                    /// Checked Euclidean division. Computes `self.div_euclid(rhs)`, returning `None` if `rhs == 0` or if the resulting value is out of range.
                    #[inline] pub const fn checked_div_euclid(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_div_euclid(rhs)))
                    }
                    /// Unchecked Euclidean division. Computes `self.div_euclid(rhs)`, assuming that `rhs != 0` and that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_div_euclid(self, rhs: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked
                            (
                                unsafe_unwrap_unchecked!(self.get().checked_div_euclid(rhs))
                            )
                        }
                    }

                    if_unsigned!
                    (
                        $is_signed
                        /// Remainder. Computes `self % rhs`, statically guaranteeing that the returned value is in range.
                        #[track_caller]
                        #[inline] pub const fn rem<const RHS_VALUE: $internal>
                        (
                            self,
                            rhs: $type<RHS_VALUE, RHS_VALUE>,
                        ) -> $type<0, RHS_VALUE>
                        {
                            unsafe
                            {
                                const { assert!(MIN <= MAX); }
                                $type::new_unchecked(self.get() % rhs.get())
                            }
                        }
                    );
                    /// Checked integer remainder. Computes `self % rhs`, returning `None` if `rhs == 0` or if the resulting value is out of range.
                    #[inline] pub const fn checked_rem(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_rem(rhs)))
                    }
                    /// Unchecked remainder. Computes `self % rhs`, assuming that `rhs != 0` and that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_rem(self, rhs: $internal) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_rem(rhs)))
                        }
                    }
                    /// Checked Euclidean remainder. Computes `self.rem_euclid(rhs)`, returning `None` if `rhs == 0` or if the resulting value is out of range.
                    #[inline] pub const fn checked_rem_euclid(self, rhs: $internal) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_rem_euclid(rhs)))
                    }
                    /// Unchecked Euclidean remainder. Computes `self.rem_euclid(rhs)`, assuming that `rhs != 0` and that the result is in range.
                    #[track_caller] #[inline(always)]  pub const unsafe fn unchecked_rem_euclid(self, rhs: $internal) -> Self
                    {
                        
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(
                                unsafe_unwrap_unchecked!(self.get().checked_rem_euclid(rhs))
                            )
                        }
                    }
                    /// Checked negation. Computes `-self`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_neg(self) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_neg()))
                    }
                    /// Unchecked negation. Computes `-self`, assuming that `-self` is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_neg(self) -> Self
                    {                        
                        unsafe
                        { 
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_neg()))
                        }
                    }
                    /// Negation. Computes `self.neg()`, **failing to compile** if the result is not guaranteed to be in range.
                    #[inline( always )] pub const fn neg(self) -> Self
                    {
                        unsafe
                        {
                            const
                            {
                                assert!(MIN <= MAX);
                                if_signed!
                                {
                                    $is_signed
                                    assert!(MIN != $internal::MIN);
                                    assert!(-MIN <= MAX);
                                    assert!(-MAX >= MIN);
                                }

                                if_unsigned!
                                { 
                                    $is_signed
                                    assert!(MAX == 0);
                                }
                            }
                            self.unchecked_neg() 
                        }
                    }
                    /// Checked shift left. Computes `self << rhs`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_shl(self, rhs: u32) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_shl(rhs)))
                    }
                    /// Unchecked shift left. Computes `self << rhs`, assuming that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_shl(self, rhs: u32) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_shl(rhs)))
                        }
                    }
                    /// Checked shift right. Computes `self >> rhs`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_shr(self, rhs: u32) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_shr(rhs)))
                    }
                    /// Unchecked shift right. Computes `self >> rhs`, assuming that the result is in range.
                    #[track_caller] #[inline(always)] pub const unsafe fn unchecked_shr(self, rhs: u32) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_shr(rhs)))
                        }
                    }

                    if_signed!
                    (
                        $is_signed
                        /// Checked absolute value. Computes `self.abs()`, returning `None` if the resulting value is out of range.
                        #[inline] pub const fn checked_abs(self) -> Option<Self>
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new(const_try_opt!(self.get().checked_abs()))
                        }
                        /// Unchecked absolute value. Computes `self.abs()`, assuming that the result is in range.
                        #[track_caller] #[inline(always)] pub const unsafe fn unchecked_abs(self) -> Self
                        {
                            unsafe
                            {
                                const { assert!(MIN <= MAX); }
                                Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_abs()))
                            }
                        }
                        /// Absolute value. Computes `self.abs()`, **failing to compile** if the result is not guaranteed to be in range.
                        #[inline( always )] pub const fn abs(self) -> Self
                        {
                            unsafe 
                            {
                                const 
                                {
                                    assert!(MIN <= MAX);
                                    assert!(MIN != $internal::MIN);
                                    assert!(-MIN <= MAX);
                                }
                                self.unchecked_abs() 
                            }
                        }
                    );
                    /// Checked exponentiation. Computes `self.pow(exp)`, returning `None` if the resulting value is out of range.
                    #[inline] pub const fn checked_pow(self, exp: u32) -> Option<Self>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(const_try_opt!(self.get().checked_pow(exp)))
                    }
                    /// Unchecked exponentiation. Computes `self.pow(exp)`, assuming that the result is in range.
                    #[track_caller] #[inline( always )] pub const unsafe fn unchecked_pow(self, exp: u32) -> Self
                    {
                        unsafe
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_unchecked(unsafe_unwrap_unchecked!(self.get().checked_pow(exp)))
                        }
                    }
                    /// Saturating integer addition. Computes `self + rhs`, saturating at the numeric bounds.
                    #[inline] pub const fn saturating_add(self, rhs: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new_saturating(self.get().saturating_add(rhs))
                    }
                    /// Saturating integer subtraction. Computes `self - rhs`, saturating at the numeric bounds.
                    #[inline] pub const fn saturating_sub(self, rhs: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new_saturating(self.get().saturating_sub(rhs))
                    }

                    if_signed!
                    (
                        $is_signed
                        /// Saturating integer negation. Computes `self - rhs`, saturating at the numeric bounds.
                        #[inline] pub const fn saturating_neg(self) -> Self
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_saturating(self.get().saturating_neg())
                        }
                    );

                    if_signed!
                    (
                        $is_signed
                        /// Saturating absolute value. Computes `self.abs()`, saturating at the numeric bounds.
                        #[inline]
                        pub const fn saturating_abs(self) -> Self
                        {
                            const { assert!(MIN <= MAX); }
                            Self::new_saturating(self.get().saturating_abs())
                        }
                    );
                    /// Saturating integer multiplication. Computes `self * rhs`, saturating at the numeric bounds.
                    #[inline] pub const fn saturating_mul(self, rhs: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new_saturating(self.get().saturating_mul(rhs))
                    }
                    /// Saturating integer exponentiation. Computes `self.pow(exp)`, saturating at the numeric bounds.
                    #[inline] pub const fn saturating_pow(self, exp: u32) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new_saturating(self.get().saturating_pow(exp))
                    }

                    if_signed!
                    { 
                        $is_signed
                        /// Returns `true` if the number is positive and `false` if the number is zero or negative.
                        #[inline] pub const fn is_positive(self) -> bool
                        {
                            const { assert!(MIN <= MAX); }
                            self.get().is_positive()
                        }

                        /// Returns `true` if the number is negative and `false` if the number is zero or positive.
                        #[inline] pub const fn is_negative(self) -> bool
                        {
                            const { assert!(MIN <= MAX); }
                            self.get().is_negative()
                        }
                    }
                    /// Compute the `rem_euclid` of this type with its unsigned type equivalent
                    #[track_caller] #[inline] const fn rem_euclid_unsigned( rhs: $internal, range_len: $unsigned_type ) -> $unsigned_type
                    {
                        if rhs >= 0 { (rhs as $unsigned_type) % range_len }
                        else
                        {
                            let rhs_abs = ($internal::wrapping_sub(0, rhs)) as $unsigned_type;
                            
                            ((($unsigned_type::MAX / range_len) * range_len) - (rhs_abs)) % range_len
                        }
                    }
                    /// Wrapping integer addition. Computes `self + rhs`, wrapping around the numeric bounds.
                    #[inline] pub const fn wrapping_add(self, rhs: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        
                        if MIN == $internal::MIN && MAX == $internal::MAX
                        {
                            return unsafe { Self::new_unchecked(self.get().wrapping_add(rhs)) }
                        }

                        let inner = self.get();
                        
                        let range_len = MAX.abs_diff(MIN) + 1;
                        
                        let offset = Self::rem_euclid_unsigned(rhs, range_len);

                        let greater_vals = MAX.abs_diff(inner);
                        
                        if offset <= greater_vals {
                            unsafe { Self::new_unchecked(
                                ((inner as $unsigned_type).wrapping_add(offset)) as $internal
                            ) }
                        }
                        
                        else {
                            unsafe
                            { 
                                Self::new_unchecked
                                (
                                    ((MIN as $unsigned_type).wrapping_add(
                                        offset - (greater_vals + 1)
                                    )) as $internal
                                )
                            }
                        }
                    }
                    /// Wrapping integer subtraction. Computes `self - rhs`, wrapping around the numeric bounds.
                    #[inline] pub const fn wrapping_sub(self, rhs: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        
                        if MIN == $internal::MIN && MAX == $internal::MAX
                        {
                            return unsafe { Self::new_unchecked(self.get().wrapping_sub(rhs)) }
                        }

                        let inner = self.get();
                        
                        let range_len = MAX.abs_diff(MIN) + 1;
                        
                        let offset = Self::rem_euclid_unsigned(rhs, range_len);

                        let lesser_vals = MIN.abs_diff(inner);
                        
                        if offset <= lesser_vals 
                        {
                            unsafe
                            {
                                Self::new_unchecked
                                (
                                    ((inner as $unsigned_type).wrapping_sub(offset)) as $internal
                                )
                            }
                        }
                        
                        else
                        {
                            unsafe
                            { 
                                Self::new_unchecked
                                (
                                    ((MAX as $unsigned_type).wrapping_sub
                                    (
                                        offset - (lesser_vals + 1)
                                    )) as $internal
                                )
                            }
                        }
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> $optional_type<MIN, MAX>
                {
                    /// The value used as the niche. Must not be in the range `MIN..=MAX`.
                    const NICHE: $internal = match (MIN, MAX)
                    {
                        ($internal::MIN, $internal::MAX) => panic!("type has no niche"),
                        ($internal::MIN, _) => $internal::MAX,
                        (_, _) => $internal::MIN,
                    };
                    /// An optional ranged value that is not present.
                    pub const None: Self = Self(Self::NICHE);
                    /// Creates an optional ranged value that is present.
                    #[inline(always)] pub const fn Some(value: $type<MIN, MAX>) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self(value.get())
                    }
                    /// Returns the value as the standard library's [`Option`] type.
                    #[inline(always)] pub const fn get(self) -> Option<$type<MIN, MAX>>
                    {
                        const { assert!(MIN <= MAX); }
                        if self.0 == Self::NICHE { None }
                        else { Some(unsafe { $type::new_unchecked(self.0) }) }
                    }
                    /// Creates an optional ranged integer without checking the value.
                    #[track_caller] #[inline( always )] pub const unsafe fn some_unchecked(value: $internal) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        unsafe { assert_unchecked(MIN <= value && value <= MAX) };
                        Self(value)
                    }
                    /// Obtain the inner value of the struct. This is useful for comparisons.
                    #[inline(always)] pub const fn inner(self) -> $internal
                    {
                        const { assert!(MIN <= MAX); }
                        self.0
                    }
                    /// Obtain the value of the struct as an `Option` of the primitive type.
                    #[inline( always )] pub const fn get_primitive(self) -> Option<$internal>
                    {
                        const { assert!(MIN <= MAX); }
                        Some(const_try_opt!(self.get()).get())
                    }
                    /// Obtain the value of the struct as an `Option` of the primitive type.
                    #[inline( always )] pub const fn get_primitive_without_hint(self) -> Option<$internal>
                    {
                        const { assert!(MIN <= MAX); }
                        Some(const_try_opt!(self.get()).get_without_hint())
                    }
                    /// Returns `true` if the value is the niche value.
                    #[inline( always )] pub const fn is_none(&self) -> bool
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().is_none()
                    }
                    /// Returns `true` if the value is not the niche value.
                    #[inline( always )] pub const fn is_some(&self) -> bool
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().is_some()
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::Debug for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::Debug for $optional_type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::Display for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> Default for $optional_type<MIN, MAX>
                {
                    #[inline(always)] fn default() -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::None
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> AsRef<$internal> for $type<MIN, MAX>
                {
                    #[inline(always)] fn as_ref(&self) -> &$internal
                    {
                        const { assert!(MIN <= MAX); }
                        &self.get_ref()
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> Borrow<$internal> for $type<MIN, MAX>
                {
                    #[inline(always)] fn borrow(&self) -> &$internal
                    {
                        const { assert!(MIN <= MAX); }
                        &self.get_ref()
                    }
                }

                impl
                <
                    const MIN_A: $internal,
                    const MAX_A: $internal,
                    const MIN_B: $internal,
                    const MAX_B: $internal,
                > PartialEq<$type<MIN_B, MAX_B>> for $type<MIN_A, MAX_A>
                {
                    #[inline(always)] fn eq(&self, other: &$type<MIN_B, MAX_B>) -> bool
                    {
                        const
                        {
                            assert!(MIN_A <= MAX_A);
                            assert!(MIN_B <= MAX_B);
                        }

                        self.get() == other.get()
                    }
                }

                impl
                <
                    const MIN_A: $internal,
                    const MAX_A: $internal,
                    const MIN_B: $internal,
                    const MAX_B: $internal,
                > PartialEq<$optional_type<MIN_B, MAX_B>> for $optional_type<MIN_A, MAX_A>
                {
                    #[inline(always)] fn eq(&self, other: &$optional_type<MIN_B, MAX_B>) -> bool
                    {
                        const
                        {
                            assert!(MIN_A <= MAX_A);
                            assert!(MIN_B <= MAX_B);
                        }

                        self.inner() == other.inner()
                    }
                }

                impl
                <
                    const MIN_A: $internal,
                    const MAX_A: $internal,
                    const MIN_B: $internal,
                    const MAX_B: $internal,
                > PartialOrd<$type<MIN_B, MAX_B>> for $type<MIN_A, MAX_A>
                {
                    #[inline(always)] fn partial_cmp(&self, other: &$type<MIN_B, MAX_B>) -> Option<Ordering>
                    {
                        const 
                        {
                            assert!(MIN_A <= MAX_A);
                            assert!(MIN_B <= MAX_B);
                        }

                        self.get().partial_cmp(&other.get())
                    }
                }

                impl
                <
                    const MIN_A: $internal,
                    const MAX_A: $internal,
                    const MIN_B: $internal,
                    const MAX_B: $internal,
                > PartialOrd<$optional_type<MIN_B, MAX_B>> for $optional_type<MIN_A, MAX_A>
                {
                    #[inline] fn partial_cmp(&self, other: &$optional_type<MIN_B, MAX_B>) -> Option<Ordering>
                    {
                        const
                        {
                            assert!(MIN_A <= MAX_A);
                            assert!(MIN_B <= MAX_B);
                        }

                        if self.is_none() && other.is_none() { Some(Ordering::Equal) }
                        else if self.is_none() { Some(Ordering::Less) }
                        else if other.is_none() { Some(Ordering::Greater) }
                        else { self.inner().partial_cmp(&other.inner()) }
                    }
                }

                impl
                <
                    const MIN: $internal,
                    const MAX: $internal,
                > Ord for $optional_type<MIN, MAX>
                {
                    #[inline] fn cmp(&self, other: &Self) -> Ordering
                    {
                        const { assert!(MIN <= MAX); }

                        if self.is_none() && other.is_none() { Ordering::Equal }
                        else if self.is_none() { Ordering::Less }
                        else if other.is_none() { Ordering::Greater }
                        else { self.inner().cmp(&other.inner()) }
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::Binary for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::LowerHex for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::UpperHex for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::LowerExp for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::UpperExp for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> fmt::Octal for $type<MIN, MAX>
                {
                    #[inline(always)] fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                    {
                        const { assert!(MIN <= MAX); }
                        self.get().fmt(f)
                    }
                }

                if_unsigned!
                {
                    $is_signed
                    impl From<NonZero<$internal>> for $type<1, { $internal::MAX }>
                    {
                        #[inline(always)] fn from(value: NonZero<$internal>) -> Self { Self::from_nonzero(value) }
                    }

                    impl From<$type<1, { $internal::MAX }>> for NonZero<$internal>
                    {
                        #[inline(always)] fn from(value: $type<1, { $internal::MAX }>) -> Self { value.to_nonzero() }
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> From<$type<MIN, MAX>> for $internal
                {
                    #[inline(always)] fn from(value: $type<MIN, MAX>) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        value.get()
                    }
                }

                impl
                <
                    const MIN: $internal,
                    const MAX: $internal,
                > From<$type<MIN, MAX>> for $optional_type<MIN, MAX>
                {
                    #[inline(always)] fn from(value: $type<MIN, MAX>) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        Self::Some(value)
                    }
                }

                impl
                <
                    const MIN: $internal,
                    const MAX: $internal,
                > From<Option<$type<MIN, MAX>>> for $optional_type<MIN, MAX>
                {
                    #[inline(always)] fn from(value: Option<$type<MIN, MAX>>) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        match value
                        {
                            Some(value) => Self::Some(value),
                            None => Self::None,
                        }
                    }
                }

                impl
                <
                    const MIN: $internal,
                    const MAX: $internal,
                > From<$optional_type<MIN, MAX>> for Option<$type<MIN, MAX>>
                {
                    #[inline(always)] fn from(value: $optional_type<MIN, MAX>) -> Self
                    {
                        const { assert!(MIN <= MAX); }
                        value.get()
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> TryFrom<$internal> for $type<MIN, MAX>
                {
                    type Error = TryFromIntError;
                    #[inline] fn try_from(value: $internal) -> Result<Self, Self::Error>
                    {
                        const { assert!(MIN <= MAX); }
                        Self::new(value).ok_or(TryFromIntError)
                    }
                }

                impl<const MIN: $internal, const MAX: $internal> FromStr for $type<MIN, MAX>
                {
                    type Err = ParseIntError;
                    #[inline] fn from_str(s: &str) -> Result<Self, Self::Err>
                    {
                        const { assert!(MIN <= MAX); }
                        
                        let value = s.parse::<$internal>().map_err(|e| ParseIntError
                        {
                            kind: e.kind().clone()
                        })?;

                        if value < MIN { Err(ParseIntError { kind: IntErrorKind::NegOverflow }) }
                        else if value > MAX { Err(ParseIntError { kind: IntErrorKind::PosOverflow }) } 
                        else { Ok(unsafe { Self::new_unchecked(value) }) }
                    }
                }

                $(impl
                <
                    const MIN_SRC: $from_internal,
                    const MAX_SRC: $from_internal,
                    const MIN_DST: $internal,
                    const MAX_DST: $internal,
                > From<$from<MIN_SRC, MAX_SRC>> for $type<MIN_DST, MAX_DST>
                {
                    #[inline( always )] fn from(value: $from<MIN_SRC, MAX_SRC>) -> Self 
                    {
                        const {
                            assert!(MIN_SRC <= MAX_SRC, "source range is invalid");
                            assert!(MIN_DST <= MAX_DST, "target range is invalid");

                            match ($from_internal::MIN == 0, $internal::MIN == 0) {
                                // unsigned -> unsigned
                                (true, true) => {
                                    assert!(
                                        MIN_SRC as u128 >= MIN_DST as u128,
                                        "minimum value cannot be represented in the target range"
                                    );
                                    assert!(
                                        MAX_SRC as u128 <= MAX_DST as u128,
                                        "maximum value cannot be represented in the target range"
                                    );
                                }
                                // signed -> signed
                                (false, false) => {
                                    assert!(
                                        MIN_SRC as i128 >= MIN_DST as i128,
                                        "minimum value cannot be represented in the target range"
                                    );
                                    assert!(
                                        MAX_SRC as i128 <= MAX_DST as i128,
                                        "maximum value cannot be represented in the target range"
                                    );
                                }
                                // unsigned -> signed
                                (true, false) => {
                                    assert!(
                                        MIN_DST < 0 || MIN_SRC as u128 >= MIN_DST as u128,
                                        "minimum value cannot be represented in the target range"
                                    );
                                    assert!(
                                        MAX_DST >= 0
                                            && MAX_SRC as u128 <= i128::MAX as u128
                                            && MAX_SRC as i128 <= MAX_DST as i128,
                                        "maximum value cannot be represented in the target range"
                                    );
                                }
                                // signed -> unsigned
                                (false, true) => {
                                    assert!(
                                        MIN_SRC >= 0 && MIN_SRC as u128 >= MIN_DST as u128,
                                        "minimum value cannot be represented in the target range"
                                    );
                                    assert!(
                                        MAX_SRC >= 0 && MAX_SRC as u128 <= MAX_DST as u128,
                                        "maximum value cannot be represented in the target range"
                                    );
                                }
                            }
                        }

                        // Safety: The source range is a subset of the destination range.
                        unsafe { $type::new_unchecked(value.get() as $internal) }
                    }
                })+

                #[cfg(feature = "num")]
                impl<const MIN: $internal, const MAX: $internal> num_traits::Bounded for $type<MIN, MAX> 
                {
                    #[inline(always)]
                    fn min_value() -> Self {
                        const { assert!(MIN <= MAX); }
                        Self::MIN
                    }

                    #[inline(always)]
                    fn max_value() -> Self {
                        const { assert!(MIN <= MAX); }
                        Self::MAX
                    }
                }
            )*};
        }

        pub struct Date
        {
            pub year: i32,
            pub ordinal: u16,
        }

        pub struct Time
        {
            pub hour: u8,
            pub minute: u8,
            pub second: u8,
            pub nanosecond: u32,
        }
        /// A value that is safe to use, but is unsafe to construct or mutate.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct Unsafe<T>(T);

        impl<T: ::fmt::Debug> ::fmt::Debug for Unsafe<T>
        {
            #[inline] fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result { self.0.fmt(f) }
        }

        impl<T> Unsafe<T>
        {
            /// Create a new `Unsafe`, asserting that all invariants are upheld.
            #[inline(always)] pub const unsafe fn new(value: T) -> Self { Self(value) }
            /// Get a reference to the inner value.
            #[inline(always)] pub const fn get(&self) -> &T { &self.0 }
        }

        impl<T> ::ops::Deref for Unsafe<T>
        {
            type Target = T;
            #[inline(always)] fn deref(&self) -> &Self::Target { &self.0 }
        }
        
        impl_ranged!
        {
            RangedU8
            {
                mod_name: ranged_u8
                internal: u8
                signed: false
                unsigned: u8
                optional: OptionRangedU8
                from:
                [
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedU16
            {
                mod_name: ranged_u16
                internal: u16
                signed: false
                unsigned: u16
                optional: OptionRangedU16
                from:
                [
                    RangedU8(u8)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedU32
            {
                mod_name: ranged_u32
                internal: u32
                signed: false
                unsigned: u32
                optional: OptionRangedU32
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }
            
            RangedU64
            {
                mod_name: ranged_u64
                internal: u64
                signed: false
                unsigned: u64
                optional: OptionRangedU64
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedU128
            {
                mod_name: ranged_u128
                internal: u128
                signed: false
                unsigned: u128
                optional: OptionRangedU128
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedUsize
            {
                mod_name: ranged_usize
                internal: usize
                signed: false
                unsigned: usize
                optional: OptionRangedUsize
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
                manual: [rand_09]
            }

            RangedI8
            {
                mod_name: ranged_i8
                internal: i8
                signed: true
                unsigned: u8
                optional: OptionRangedI8
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedI16
            {
                mod_name: ranged_i16
                internal: i16
                signed: true
                unsigned: u16
                optional: OptionRangedI16
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedI32
            {
                mod_name: ranged_i32
                internal: i32
                signed: true
                unsigned: u32
                optional: OptionRangedI32
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI64(i64)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedI64
            {
                mod_name: ranged_i64
                internal: i64
                signed: true
                unsigned: u64
                optional: OptionRangedI64
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI128(i128)
                    RangedIsize(isize)
                ]
            }

            RangedI128
            {
                mod_name: ranged_i128
                internal: i128
                signed: true
                unsigned: u128
                optional: OptionRangedI128
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedIsize(isize)
                ]
            }

            RangedIsize
            {
                mod_name: ranged_isize
                internal: isize
                signed: true
                unsigned: usize
                optional: OptionRangedIsize
                from:
                [
                    RangedU8(u8)
                    RangedU16(u16)
                    RangedU32(u32)
                    RangedU64(u64)
                    RangedU128(u128)
                    RangedUsize(usize)
                    RangedI8(i8)
                    RangedI16(i16)
                    RangedI32(i32)
                    RangedI64(i64)
                    RangedI128(i128)
                ]
                manual: [rand_09]
            }
        }

        impl_per! 
        {
            Nanosecond ("nanosecond") per 
            {
                Nanosecond: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Microsecond: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 1_000; f32|f64 = 1_000.;
                Millisecond: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 1_000_000; f32|f64 = 1_000_000.;
                Second:
                    [u32] u32|u64|u128|usize|i32|i64|i128|isize = 1_000_000_000; f32|f64 = 1_000_000_000.;
                Minute: [u64] u64|u128|i64|i128 = 60_000_000_000; f32|f64 = 60_000_000_000.;
                Hour: [u64] u64|u128|i64|i128 = 3_600_000_000_000; f32|f64 = 3_600_000_000_000.;
                Day: [u64] u64|u128|i64|i128 = 86_400_000_000_000; f32|f64 = 86_400_000_000_000.;
                Week: [u64] u64|u128|i64|i128 = 604_800_000_000_000; f32|f64 = 604_800_000_000_000.;
            }
            Microsecond ("microsecond") per {
                Microsecond: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Millisecond: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 1_000; f32|f64 = 1_000.;
                Second: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 1_000_000; f32|f64 = 1_000_000.;
                Minute: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 60_000_000; f32|f64 = 60_000_000.;
                Hour: [u32] u32|u64|u128|i64|i128 = 3_600_000_000; f32|f64 = 3_600_000_000.;
                Day: [u64] u64|u128|i64|i128 = 86_400_000_000; f32|f64 = 86_400_000_000.;
                Week: [u64] u64|u128|i64|i128 = 604_800_000_000; f32|f64 = 604_800_000_000.;
            }
            Millisecond ("millisecond") per {
                Millisecond: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Second: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 1_000; f32|f64 = 1_000.;
                Minute: [u16] u16|u32|u64|u128|usize|i32|i64|i128|isize = 60_000; f32|f64 = 60_000.;
                Hour: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 3_600_000; f32|f64 = 3_600_000.;
                Day: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 86_400_000; f32|f64 = 86_400_000.;
                Week: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 604_800_000; f32|f64 = 604_800_000.;
            }
            Second ("second") per {
                Second: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Minute: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 60; f32|f64 = 60.;
                Hour: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 3_600; f32|f64 = 3_600.;
                Day: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 86_400; f32|f64 = 86_400.;
                Week: [u32] u32|u64|u128|usize|i32|i64|i128|isize = 604_800; f32|f64 = 604_800.;
            }
            Minute ("minute") per {
                Minute: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Hour: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 60; f32|f64 = 60.;
                Day: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 1_440; f32|f64 = 1_440.;
                Week: [u16] u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 10_080; f32|f64 = 10_080.;
            }
            Hour ("hour") per {
                Hour: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Day: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 24; f32|f64 = 24.;
                Week: [u8] u8|u16|u32|u64|u128|usize|i16|i32|i64|i128|isize = 168; f32|f64 = 168.;
            }
            Day ("day") per {
                Day: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
                Week: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 7; f32|f64 = 7.;
            }
            Week ("week") per {
                Week: [u8] u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize = 1; f32|f64 = 1.;
            }
        }
        /// The type of the `hours` field of `UtcOffset`.
        type Hours = RangedI8<-25, 25>;
        /// Type of padding to ensure a minimum width.
        #[non_exhaustive] #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Padding 
        {
            /// A space character (` `) should be used as padding.
            Space,
            /// A zero character (`0`) should be used as padding.
            Zero,
            /// There is no padding.
            None,
        }
        /// Hour of the day.
        #[non_exhaustive] #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct Hour
        {
            /// The padding to obtain the minimum width.
            pub padding: Padding,
            /// Is the hour displayed using a 12 or 24-hour clock?
            pub is_12_hour_clock: bool,
        }
        /// Minute within the hour.
        #[non_exhaustive] #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct Minute
        {
            /// The padding to obtain the minimum width.
            pub padding: Padding,
        }
        /// Second within the minute.
        #[non_exhaustive] #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct Second
        {
            /// The padding to obtain the minimum width.
            pub padding: Padding,
        }
        /// The type of the `minutes` field of `UtcOffset`.
        type Minutes = RangedI8<{ -(Minute::per_t::<i8>(Hour) - 1) }, { Minute::per_t::<i8>(Hour) - 1 }>;
        /// The type of the `seconds` field of `UtcOffset`.
        type Seconds = RangedI8<{ -(Second::per_t::<i8>(Minute) - 1) }, { Second::per_t::<i8>(Minute) - 1 }>;
        /// The type capable of storing the range of whole seconds that a `UtcOffset` can encompass.
        type WholeSeconds = RangedI32
        <
            {
                Hours::MIN.get() as i32 * Second::per_t::<i32>(Hour) + Minutes::MIN.get() as i32 * Second::per_t::<i32>(Minute) + Seconds::MIN.get() as i32
            },
            {
                Hours::MAX.get() as i32 * Second::per_t::<i32>(Hour) + Minutes::MAX.get() as i32 * Second::per_t::<i32>(Minute) + Seconds::MAX.get() as i32
            },
        >;

        /// Combined date and time.
        #[repr( C )] #[derive( Clone, Copy, Eq )]
        pub struct PrimitiveDateTime 
        {
            time: Time,
            date: Date,
        }
        /// An offset from UTC.
        #[repr(C)] #[derive(Clone, Copy, Eq)]
        pub struct UtcOffset
        {
            seconds: Seconds,
            minutes: Minutes,
            hours: Hours
        }
        /// A [`PrimitiveDateTime`] with a [`UtcOffset`].
        #[derive(Debug, Clone, Copy, Eq)]
        pub struct OffsetDateTime
        {
            /// The [`PrimitiveDateTime`], which is _always_ UTC.
            pub utc_datetime: PrimitiveDateTime,
            /// The [`UtcOffset`], which will be added to the [`PrimitiveDateTime`] as necessary.
            pub offset: UtcOffset,
        }
    }
}

pub mod libs
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod num
{
    pub use std::num::{ * };
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod parsers
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod prompt
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod rcfile
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod scripting
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod shell
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod signals
{
    /*!
    */
    use ::
    {
        *,
    };
}

pub mod str
{
    pub use std::str::{ * };
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod types
{
    /*!
    */
    use ::
    {
        *,
    };
}


pub unsafe fn domain()
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);

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
                
                Ok(ReadResult::Eof) =>
                {
                    if let Ok(x) = env::var("NO_EXIT_ON_CTRL_D")
                    {
                        if x == "1" { println!(); }
                    }
                    
                    else
                    {
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
}

fn main()
{
    unsafe
    {
        domain();
    }   
}
